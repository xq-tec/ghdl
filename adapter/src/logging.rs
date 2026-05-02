use std::io;

use opentelemetry::KeyValue;
use opentelemetry::trace::TracerProvider as _;
use opentelemetry_appender_tracing::layer::OpenTelemetryTracingBridge;
use opentelemetry_sdk::Resource;
use opentelemetry_sdk::logs::SdkLoggerProvider;
use opentelemetry_sdk::trace::SdkTracerProvider;
use tracing_core::LevelFilter;
use tracing_opentelemetry::OpenTelemetryLayer;
use tracing_subscriber::layer::SubscriberExt as _;
use tracing_subscriber::util::SubscriberInitExt as _;

/// Initializes logging for the adapter.
///
/// The `GHDL_LOG_LEVEL` environment variable is used to set the logging level and filters.
/// The default log destination is *stderr*;
/// this can be overridden by setting the `GHDL_LOG_OTLP` environment variable to select OpenTelemetry.
pub(crate) fn init_logging() {
    let mut log_target = tracing_subscriber::EnvFilter::builder()
        .with_default_directive(LevelFilter::WARN.into())
        .with_env_var("GHDL_LOG_LEVEL")
        .from_env_lossy();

    if let Ok(otlp_config) = std::env::var("GHDL_LOG_OTLP") {
        if let Ok(directive) = otlp_config.parse::<tracing_subscriber::filter::Directive>() {
            log_target = log_target.add_directive(directive);
        }

        let tracer_provider = init_tracer_provider();
        let logger_provider = init_logger_provider();

        opentelemetry::global::set_tracer_provider(tracer_provider.clone());

        let tracer = tracer_provider.tracer("ghdl-adapter");
        let otel_trace_layer = OpenTelemetryLayer::new(tracer);
        let otel_log_layer = OpenTelemetryTracingBridge::new(&logger_provider);
        tracing_subscriber::registry()
            .with(log_target)
            .with(otel_trace_layer)
            .with(otel_log_layer)
            .init();
    } else {
        let stderr_layer = tracing_subscriber::fmt::layer()
            .with_target(true)
            .with_writer(io::stderr);
        tracing_subscriber::registry()
            .with(log_target)
            .with(stderr_layer)
            .init();
    }
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

fn resource() -> Resource {
    Resource::builder()
        .with_service_name("ghdl-adapter")
        .with_attributes([KeyValue::new("service.version", env!("CARGO_PKG_VERSION"))])
        .build()
}
