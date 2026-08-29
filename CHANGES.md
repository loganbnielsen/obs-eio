# Changes

## Unreleased

- `Obs_trace.of_traceparent` now rejects all-zero trace IDs and span IDs, as
  required by the W3C trace context spec.

## 0.1.0

- Initial standalone OPAM package: core observability handle, traceparent helpers,
  structured span logs, and metric declaration/emission.
