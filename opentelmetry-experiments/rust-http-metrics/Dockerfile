FROM rust:1.82-alpine3.20 AS build-rust
WORKDIR /usr/src/app
COPY Cargo.toml Cargo.lock ./
RUN apk add musl-dev && \
    mkdir src && echo "fn main() {}" > src/main.rs && \
    cargo build --release && \
    rm -Rvf src
COPY ./src ./src
RUN touch src/main.rs && cargo build --release

FROM alpine:3.20 AS rust-example
WORKDIR /usr/src/app
COPY --from=build-rust /usr/src/app/target/release/rust-http-metrics ./
CMD ["./rust-http-metrics"]
