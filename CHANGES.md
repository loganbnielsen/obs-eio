# Changes

## Unreleased

- **API change**: `Obs_trace.t` is now a private record, so callers can inspect
  parsed/generated contexts but cannot construct W3C-invalid ones directly.
- `Obs_trace.of_traceparent` now rejects all-zero trace IDs and span IDs, as
  required by the W3C trace context spec.
- `Obs_trace.generate` and `Obs_trace.child_span` now avoid the same reserved
  all-zero trace ID and span ID values.

## 0.1.0

- Initial standalone OPAM package: core observability handle, traceparent helpers,
  structured span logs, and metric declaration/emission.
