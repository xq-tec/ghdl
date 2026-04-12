use std::io;

use opentelemetry::global;
use opentelemetry::trace::TracerProvider;
use opentelemetry::KeyValue;
use opentelemetry_appender_tracing::layer::OpenTelemetryTracingBridge;
use opentelemetry_sdk::logs::SdkLoggerProvider;
use opentelemetry_sdk::trace::SdkTracerProvider;
use opentelemetry_sdk::Resource;
use tracing_opentelemetry::OpenTelemetryLayer;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

fn resource() -> Resource {
    Resource::builder()
        .with_service_name("ghdl-adapter")
        .with_attributes([KeyValue::new(
            "service.version",
            env!("CARGO_PKG_VERSION"),
        )])
        .build()
}

fn init_tracer_provider() -> SdkTracerProvider {
    let exporter = opentelemetry_otlp::SpanExporter::builder()
        .with_tonic()
        .build()
        .expect("failed to create span exporter");

    SdkTracerProvider::builder()
        .with_resource(resource())
        .with_batch_exporter(exporter)
        .build()
}

fn init_logger_provider() -> SdkLoggerProvider {
    let exporter = opentelemetry_otlp::LogExporter::builder()
        .with_tonic()
        .build()
        .expect("failed to create log exporter");

    SdkLoggerProvider::builder()
        .with_resource(resource())
        .with_batch_exporter(exporter)
        .build()
}

/// Initializes the OpenTelemetry tracing subscriber.
///
/// Sets up a composite subscriber with:
/// - An `fmt` layer for stderr output (local debugging)
/// - An `EnvFilter` for level filtering (defaults to `info`)
/// - An OpenTelemetry trace layer (spans via OTLP/gRPC)
/// - An OpenTelemetry log layer (events via OTLP/gRPC)
pub(crate) fn init_logging() {
    let tracer_provider = init_tracer_provider();
    let logger_provider = init_logger_provider();

    global::set_tracer_provider(tracer_provider.clone());

    let tracer = tracer_provider.tracer("ghdl-adapter");
    let otel_trace_layer = OpenTelemetryLayer::new(tracer);
    let otel_log_layer = OpenTelemetryTracingBridge::new(&logger_provider);

    tracing_subscriber::registry()
        .with(
            tracing_subscriber::fmt::layer()
                .with_target(true)
                .with_writer(io::stderr),
        )
        .with(tracing_subscriber::EnvFilter::new("info"))
        .with(otel_trace_layer)
        .with(otel_log_layer)
        .init();
}
