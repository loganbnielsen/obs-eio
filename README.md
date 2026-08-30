# obs-eio

A small Eio-native observability event model for OCaml 5 — distributed tracing,
structured logging, and metrics in a single capability-passed handle, with swappable
backends. Not a replacement for every OCaml observability package: it is a compact core
that backend packages adapt to real exporters.

- [`obs-loki-eio`](https://github.com/loganbnielsen/obs-loki-eio) — Loki HTTP push backend
- [`obs-prometheus-eio`](https://github.com/loganbnielsen/obs-prometheus-eio) — Prometheus text-exposition backend

Extracted from the [Sun](https://github.com/loganbnielsen/sun) platform, where it
continues to be used as an external dependency.

## Build

```bash
eval $(opam env)
dune build
```

## Test

```bash
dune runtest
```

No external infrastructure required — `obs-eio` itself has no network or backend
dependencies to test against.

## Public API

### `Obs_trace`

Deliberately minimal: carries just enough to serialize/parse the W3C `traceparent`
header and derive child spans. No baggage, no tracestate, no vendor extensions — wrap
`t` in your own type if you need those.

```ocaml
type t = {
  trace_id    : int64 * int64;
  span_id     : int64;
  trace_flags : char;
}

val generate         : unit -> t               (* new root context *)
val child_span       : t -> t                  (* same trace_id, new span_id *)
val traceparent_header   : string              (* "traceparent" *)
val to_traceparent   : t -> string             (* W3C "00-{32hex}-{16hex}-{02hex}" *)
val of_traceparent   : string -> t option
val extract_from_headers : (string * string) list -> t option
val inject_to_headers    : t -> (string * string) list -> (string * string) list
```

`generate`'s randomness is a self-seeded, mutex-protected PRNG state private to this
module — no caller `Random.self_init ()` call needed, no risk of every process sharing
the stdlib `Random` module's fixed default seed, and safe to call concurrently from
multiple OCaml 5 domains. Not cryptographically strong; fine for correlation and
collision-avoidance, not for anything security-sensitive.

`of_traceparent` is intentionally lenient: it parses the W3C `traceparent` wire shape
but does not reject the all-zero trace/span IDs that the spec reserves as invalid.

### `Obs_eio` — metric emitter types

```ocaml
type counter_fn   = ?labels:(string * string) list -> int   -> unit
type gauge_fn     = ?labels:(string * string) list -> float -> unit
type histogram_fn = ?labels:(string * string) list -> float -> unit
```

### `Obs_eio`

```ocaml
type level = Debug | Info | Warn | Error

type log_entry = {
  timestamp_ns : int64;  (* monotonic, same clock as span_event.start_ns/end_ns *)
  level   : level;
  message : string;
  fields  : (string * string) list;
}

type span_event = {
  trace_ctx   : Obs_trace.t;
  name        : string;
  service     : string;
  start_ns    : int64;
  end_ns      : int64;
  status      : [ `Ok | `Error of string ];
  log_entries : log_entry list;
  context     : (string * string) list;
}

type metric_event = {
  name    : string;
  help    : string;
  kind    : [ `Counter of int | `Gauge of float | `Histogram of float ];
  labels  : (string * string) list;  (* call-site labels *)
  context : (string * string) list;
  service : string;
}
(* [context] is frozen at [register_*] time — see the note after the value
   signatures below. Unlike [span_event.context], it does NOT track later
   [with_context] calls on a different derived handle. *)

type metric_declaration = {
  declaration_name        : string;
  declaration_help        : string;
  declaration_kind        : [ `Counter | `Gauge | `Histogram ];
  declaration_label_names : string list;
  declaration_service     : string;
}
(* Registration-time metadata, delivered to [backend.declare_metric] once per
   [register_*] call, before the first [emit_metric] for that family — possibly
   before any. A backend that wants a metric visible (e.g. at its zero value) as
   soon as it's registered rather than only after its first observation uses this;
   a log/trace-only backend ignores it. *)

type backend = {
  emit_span      : span_event   -> unit;
  emit_metric    : metric_event -> unit;
  declare_metric : metric_declaration  -> unit;
}
(* A caller-supplied backend may raise; callers of [with_span], [log_standalone], and the
   [register_*] emitters/declarations never see that exception — it is caught and
   logged to stderr, so a broken backend cannot crash application code. *)

val noop    : backend   (* drops everything *)
val stdout  : backend   (* pretty-prints to stdout *)
val compose : backend -> backend -> backend
(* [compose a b] fans out to two backends. Each backend's emit/declare calls are
   isolated: if one raises, the other still receives the event. *)

val create
  :  service:string
  -> mono_clock:_ Eio.Time.Mono.t
  -> backend:backend
  -> t

val with_context : t -> (string * string) list -> t

val with_span : t -> ?parent:Obs_trace.t -> string -> (span -> 'a) -> 'a

val log   : span -> level -> ?fields:(string * string) list -> string -> unit
(* Raises [Invalid_argument] once [span]'s [with_span] callback has returned —
   see Design Notes below on [span]'s lifetime. *)
val log_standalone : t -> ?parent:Obs_trace.t -> level -> ?fields:(string * string) list -> string -> unit
val current_trace_context : span -> Obs_trace.t

type label_name = private string   (* validated Prometheus label name *)
val label_name          : string -> label_name
val label_name_to_string : label_name -> string
(* Exists for backend packages that need their own validated, typed label-name
   selectors (e.g. obs-loki-eio's stream-label selection) — the register_*
   functions below validate their own `label_names:string list` internally
   and don't use this type directly. *)

val register_counter   : t -> name:string -> help:string -> label_names:string list -> counter_fn
val register_gauge     : t -> name:string -> help:string -> label_names:string list -> gauge_fn
val register_histogram : t -> name:string -> help:string -> label_names:string list -> histogram_fn
val register_counter_and_histogram
  :  t
  -> counter_name:string -> counter_help:string -> counter_labels:string list
  -> histogram_name:string -> histogram_help:string -> histogram_labels:string list
  -> counter_fn * histogram_fn
```

`register_counter`'s emitter raises `Invalid_argument` on a negative delta — Prometheus
counters are monotonic. `register_histogram` raises `Invalid_argument` if `label_names`
includes `"le"`, since the Prometheus backend synthesizes an `"le"` label per bucket
sample. Bucket boundaries are backend-defined (`obs-prometheus-eio`'s `default_bounds`);
there is no per-metric override. Follow Prometheus convention and observe histogram
values in the metric's base unit (seconds for a duration, not milliseconds) unless a
specific backend says otherwise.

**`metric_event.context` is frozen at registration, not per-call.** A `register_*`
emitter closure captures the `t` it was registered from once; a later
`with_context ot extra` on a *derived* handle never reaches an emitter already
registered against the original `ot` — there is no way to pass per-call context to an
emitter, only per-call `labels`. Register from the most-specific `t` you want reflected
forever in a given metric family. This is unlike `span_event.context`, which reflects
whatever `t` `with_span` is called with each time.

**There is no ambient/current-span mechanism** — nesting is entirely manual. If code
inside a `with_span` callback calls another function that itself calls `with_span`
without explicitly passing `?parent:(Obs_eio.current_trace_context sp)`, that inner span starts
a brand new root trace rather than becoming a child of the outer one. Thread the parent
context through by hand:

```ocaml
Obs_eio.with_span ot "outer" (fun sp ->
  let parent = Obs_eio.current_trace_context sp in
  do_inner_work ~parent ...)
(* and inside do_inner_work: *)
let do_inner_work ~parent ... =
  Obs_eio.with_span ot ~parent "inner" (fun _sp -> ...)
```

## Example Usage

```ocaml
let ot = Obs_eio.create ~service:"checkout-api"
           ~mono_clock:env#mono_clock ~backend:Obs_eio.stdout () in
let ot = Obs_eio.with_context ot [("env", "prod"); ("region", "us-east-1")] in

(* Register metrics once at startup *)
let requests_total = Obs_eio.register_counter ot
  ~name:"http_requests_total"
  ~help:"Total HTTP requests handled"
  ~label_names:["route"; "status"] in

(* Per-request handler *)
let handle_request req =
  let parent = Obs_trace.extract_from_headers req.headers in
  (* Unlike requests_total above, with_span picks up whatever context [ot]
     carries at the moment it's called — so deriving a per-request [ot]
     here (before with_span, not inside it) does reach this span's logs. *)
  let ot = Obs_eio.with_context ot [("request_id", req.id)] in
  Obs_eio.with_span ot ?parent "handle_request" (fun sp ->
    Obs_eio.log sp Obs_eio.Info ~fields:[("route", req.route)] "handling request";
    (* ... business logic ... *)
    requests_total ~labels:[("route", req.route); ("status", "200")] 1;
    (* propagate trace to a downstream call *)
    let headers = Obs_trace.inject_to_headers (Obs_eio.current_trace_context sp) [] in
    ignore (ot, headers))
in
ignore handle_request
```

## Design Notes

- **Immutable context**: `with_context` returns a new `t`. The original is unchanged, so it is safe to pass the same `ot` to multiple concurrent fibers and derive per-fiber scoped copies.
- **Monotonic time**: `span_event.start_ns` and `end_ns` use `Mtime.to_uint64_ns` on `Eio.Time.Mono.now` — unaffected by NTP corrections.
- **W3C traceparent-compatible tracing**: `Obs_trace.t` carries fields compatible with the W3C `traceparent` propagation format — with lenient parsing of all-zero IDs — not OpenTelemetry's full data model, OTLP, baggage, or tracestate (see Out of Scope). `extract_from_headers` / `inject_to_headers` connect producers, brokers, and consumers into a single distributed trace.
- **Pre-registered metrics**: `register_counter` / `register_gauge` / `register_histogram` return typed emitter closures, after synchronously delivering a `metric_declaration` to the backend's `declare_metric` — so a backend can make a metric visible (e.g. at its zero value) as soon as it's registered, not only after its first observation.
- **Backend composition**: `compose a b` fans out to two backends — use for e.g. `compose prometheus_backend loki_backend`.
- **Backend failure isolation**: a caller-supplied backend may raise; `with_span`, `log_standalone`, and the `register_*` emitters/declarations catch it and log to stderr rather than propagate it, so a broken backend cannot crash application code. `compose` isolates each sibling the same way, so one broken backend cannot also block delivery to the other. The one exception is `Eio.Cancel.Cancelled`, which is never caught this way — it always propagates, since a backend may do real blocking Eio I/O (see the `backend` type's doc) and a cancellation firing mid-call has to unwind the caller's structured concurrency correctly rather than being logged and ignored. If an application exception from `with_span`'s body would otherwise be lost because closing the span races a `Cancelled` from the backend, that original exception is logged to stderr before `Cancelled` wins — unless the body's own exception was `Cancelled` too, the ordinary case of one cancellation reaching both the body and the backend's I/O, where nothing is lost and nothing is logged. See `with_span`'s doc.
- **Span lifetime**: a `span` value is only valid for the extent of the `with_span` callback it was handed to — `log` raises `Invalid_argument` once that callback has returned, so stashing a `span` in a ref or a fiber/domain that outlives the callback fails loudly instead of silently discarding log entries. Open a new `with_span` inside that fiber/domain instead of reusing the outer one.
- **Single-domain `t`**: `t` is safe to share across fibers within one domain. `Obs_trace`'s ID generator is safe across domains (mutex-protected); a `span`'s log buffer is not meant to be written from more than one domain.

## Out of Scope

- Backend implementations — see `obs-loki-eio`, `obs-prometheus-eio`, and any future backend package
- Grafana dashboard templates
- Baggage, tracestate, and other W3C extensions beyond `traceparent` — wrap `Obs_trace.t` in your own type if you need them
- Sampling decisions — `trace_flags` is set to `\x01` (sampled) by default; a sampling API is deferred
- Detecting a metric re-registered under the same name with a different label set — each `t`'s `register_*` calls are independent closures with no cross-registration registry at this layer (a backend such as `obs-prometheus-eio` may detect some of this itself)
