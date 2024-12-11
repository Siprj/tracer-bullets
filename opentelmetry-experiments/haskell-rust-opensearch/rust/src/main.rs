use http_body_util::{combinators::BoxBody, BodyExt, Full};
use hyper::{body::Incoming, service::service_fn, Request, Response, StatusCode};
use hyper_util::rt::{TokioExecutor, TokioIo};
use log::{error, warn, Level};
use opentelemetry::{
    global, trace::{FutureExt, Span, SpanKind, TraceContextExt, Tracer}, Context, KeyValue
};
use opentelemetry_appender_log::OpenTelemetryLogBridge;
use opentelemetry_http::{Bytes, HeaderExtractor};
use opentelemetry_otlp::{WithExportConfig, WithTonicConfig};
use opentelemetry_sdk::{logs::LoggerProvider, propagation::TraceContextPropagator};
use opentelemetry_sdk::{trace::{self, RandomIdGenerator, Sampler}, Resource};
use opentelemetry_semantic_conventions::{self, resource::SERVICE_NAME, trace::HTTP_RESPONSE_STATUS_CODE};
use opentelemetry_stdout::SpanExporter;
use tonic::metadata::MetadataMap;
use std::{convert::Infallible, net::SocketAddr, time::Duration};
use tokio::net::TcpListener;

// Utility function to extract the context from the incoming request headers
fn extract_context_from_request(req: &Request<Incoming>) -> Context {
    global::get_text_map_propagator(|propagator| {
        propagator.extract(&HeaderExtractor(req.headers()))
    })
}

// Separate async function for the handle endpoint
async fn handle_health_check(
    _req: Request<Incoming>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, Infallible> {
    let tracer = global::tracer("rust-example/server");
    let mut span = tracer
        .span_builder("health_check")
        .with_kind(SpanKind::Internal)
        .start(&tracer);
    error!("Health check was called");
    span.add_event("Health check accessed", vec![]);

    let res = Response::new(
        Full::new(Bytes::from_static(b"Server is up and running!"))
            .map_err(|err| match err {})
            .boxed(),
    );

    Ok(res)
}

// Separate async function for the echo endpoint
async fn handle_echo(
    req: Request<Incoming>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, Infallible> {
    let tracer = global::tracer("rust-example/server");
    let mut span = tracer
        .span_builder("echo")
        .with_kind(SpanKind::Internal)
        .start(&tracer);
    warn!("echoing stuff back");
    span.add_event("Echoing back the request", vec![]);

    let res = Response::new(req.into_body().boxed());

    Ok(res)
}

async fn router(
    req: Request<Incoming>,
) -> Result<Response<BoxBody<Bytes, hyper::Error>>, Infallible> {
    // Extract the context from the incoming request headers
    let parent_cx = extract_context_from_request(&req);
    let response = {
        // Create a span parenting the remote client span.
        let tracer = global::tracer("example/server");
        let mut span = tracer
            .span_builder("router")
            .with_kind(SpanKind::Server)
            .start_with_context(&tracer, &parent_cx);

        span.add_event("dispatching request", vec![]);

        warn!("*****************adsfasdfasdf****************");
        let cx = Context::default().with_span(span);
        match (req.method(), req.uri().path()) {
            (&hyper::Method::GET, "/health") => handle_health_check(req).with_context(cx).await,
            (&hyper::Method::GET, "/echo") => handle_echo(req).with_context(cx).await,
            _ => {
                cx.span()
                    .set_attribute(KeyValue::new(HTTP_RESPONSE_STATUS_CODE, 404));
                let mut not_found = Response::new(BoxBody::default());
                *not_found.status_mut() = StatusCode::NOT_FOUND;
                Ok(not_found)
            }
        }
    };

    response
}

fn init_tracer() {
    global::set_text_map_propagator(TraceContextPropagator::new());

    let mut map = MetadataMap::with_capacity(3);

    map.insert("x-host", "rust-example.com".parse().unwrap());

    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .with_endpoint("http://localhost:4317")
        .with_timeout(Duration::from_secs(3))
        .with_metadata(map.clone())
        .build().unwrap();

    let tracer_provider = opentelemetry_sdk::trace::TracerProvider::builder()
        .with_batch_exporter(exporter, opentelemetry_sdk::runtime::Tokio)
        .with_simple_exporter(SpanExporter::default())
        .with_config(
            trace::Config::default()
                .with_sampler(Sampler::AlwaysOn)
                .with_id_generator(RandomIdGenerator::default())
                .with_max_events_per_span(64)
                .with_max_attributes_per_span(16)
                .with_max_events_per_span(16)
                .with_resource(Resource::new(vec![KeyValue::new("service.name", "rust-example")])),
        ).build();
    global::set_tracer_provider(tracer_provider);


    let exporter = opentelemetry_otlp::LogExporter::builder()
        .with_tonic()
        .with_endpoint("http://localhost:4317")
        .with_timeout(Duration::from_secs(3))
        .with_metadata(map)
        .build().unwrap();

    let logger_provider = LoggerProvider::builder()
        .with_resource(Resource::new([KeyValue::new(
            SERVICE_NAME,
            "rust-example",
        )]))
        .with_batch_exporter(exporter, opentelemetry_sdk::runtime::Tokio)
        .with_simple_exporter(opentelemetry_stdout::LogExporter::default())
        .build();

    // Setup Log Appender for the log crate.
    let otel_log_appender = OpenTelemetryLogBridge::new(&logger_provider);
    log::set_boxed_logger(Box::new(otel_log_appender)).unwrap();
    log::set_max_level(Level::Info.to_level_filter());
}

#[tokio::main]
async fn main() {
    println!("running rust example");
    use hyper_util::server::conn::auto::Builder;

    init_tracer();
    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    let listener = TcpListener::bind(addr).await.unwrap();

    while let Ok((stream, _addr)) = listener.accept().await {
        if let Err(err) = Builder::new(TokioExecutor::new())
            .serve_connection(TokioIo::new(stream), service_fn(router))
            .await
        {
            eprintln!("{err}");
        }
    }
}
