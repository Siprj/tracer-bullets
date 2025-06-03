use std::{thread::sleep, time::{Duration, Instant, SystemTime}};

use axum_otel_metrics::HttpMetricsLayerBuilder;
use log::Level;
use opentelemetry::{global, KeyValue};
use opentelemetry_appender_log::OpenTelemetryLogBridge;
use opentelemetry_otlp::{MetricExporter, WithExportConfig, WithTonicConfig};
use opentelemetry_sdk::{logs::LoggerProvider, metrics::{PeriodicReader, SdkMeterProvider}, propagation::TraceContextPropagator, runtime::Tokio, trace::{RandomIdGenerator, Sampler}, Resource};

use axum::{
    http::StatusCode,
    response::{Html, IntoResponse},
    routing::{get, post},
    Json, Router,
};
use opentelemetry_semantic_conventions::resource::SERVICE_NAME;
use opentelemetry_stdout::SpanExporter;
use serde::{Deserialize, Serialize};
use tonic::metadata::MetadataMap;

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
        .with_sampler(Sampler::AlwaysOn)
        .with_id_generator(RandomIdGenerator::default())
        .with_max_events_per_span(64)
        .with_max_attributes_per_span(16)
        .with_max_events_per_span(16)
        .with_resource(Resource::new(vec![KeyValue::new(SERVICE_NAME, "rust-example")]))
        .build();
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

    let exporter = MetricExporter::builder().with_tonic().build().unwrap();
    let reader = PeriodicReader::builder(exporter, Tokio).with_interval(Duration::from_secs(10)).build();

    let meter_provider = SdkMeterProvider::builder()
        .with_reader(reader)
        .with_resource(Resource::new([KeyValue::new(
            SERVICE_NAME,
            "rust-example",
        )]))
        .build();
    global::set_meter_provider(meter_provider);
}


#[tokio::main]
async fn main() {
    init_tracer();
    let metrics = HttpMetricsLayerBuilder::<PeriodicReader>::new()
        .with_service_name(env!("CARGO_PKG_NAME").to_string())
        .with_service_version(env!("CARGO_PKG_VERSION").to_string())
        .build();

    // build our application with a route
    let app = Router::new()
        // `GET /` goes to `root`
        .route("/", get(root))
        // `POST /users` goes to `create_user`
        .route("/users", post(create_user))
        .layer(metrics);

    // run our app with hyper
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8000")
        .await
        .unwrap();
    tracing::debug!("listening on {}", listener.local_addr().unwrap());
    axum::serve(listener, app).await.unwrap();
}

// basic handler that responds with a static string
async fn root() -> Html<&'static str> {
    let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs();

    if now % 10 > 5 {
        sleep(Duration::from_secs_f64(0.5));
    } else {
        sleep(Duration::from_secs_f64(1.0));
    }

    Html("<h1>Hello, World!</h1>")
}

async fn create_user(
    // this argument tells axum to parse the request body
    // as JSON into a `CreateUser` type
    Json(payload): Json<CreateUser>,
) -> impl IntoResponse {
    // insert your application logic here
    let user = User {
        id: 1337,
        username: payload.username,
    };

    let d = rand::random::<f64>() * 5.0;
    sleep(Duration::from_secs_f64(7.0 + d));
    //sleep(Duration::from_secs_f64(12.0));

    // this will be converted into a JSON response
    // with a status code of `201 Created`
    (StatusCode::CREATED, Json(user))
}

// the input to our `create_user` handler
#[derive(Deserialize)]
struct CreateUser {
    username: String,
}

// the output to our `create_user` handler
#[derive(Serialize)]
struct User {
    id: u64,
    username: String,
}
