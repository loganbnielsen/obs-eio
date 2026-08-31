(** Observability handle — distributed tracing, structured logging, and metrics
    in a single capability-passed value.

    Create once at service startup, then pass [ot] into request/message
    handlers. Use [with_context] to derive a scoped copy with per-request or
    per-message fields; the original is unchanged and safe to share across
    fibers within one domain (see the note on [span] below for the domain
    boundary this library does not cross).

    {[
      let ot = Obs_eio.create ~service:"checkout-api"
                 ~mono_clock:env#mono_clock ~backend:Obs_eio.stdout () in
      let ot = Obs_eio.with_context ot [("env", "prod")] in

      (* Register once, at startup, from the handle whose context you want
         every emission from this counter to carry forever — see
         [metric_event.context]'s doc on why that context is frozen here,
         unlike a span's. *)
      let requests_total = Obs_eio.register_counter ot
        ~name:"http_requests_total"
        ~help:"Total HTTP requests handled"
        ~label_names:["route"; "status"] in

      let handle_request req =
        let parent = Obs_trace.extract_from_headers req.headers in
        (* Unlike the counter above, with_span picks up whatever context [ot]
           carries at the moment it's called — so a per-request derived [ot]
           here does reach this span's logs. *)
        let ot = Obs_eio.with_context ot [("request_id", req.id)] in
        Obs_eio.with_span ot ?parent "handle_request" (fun sp ->
          Obs_eio.log sp Obs_eio.Info ~fields:[("route", req.route)] "handling request";
          requests_total ~labels:[("route", req.route); ("status", "200")] 1)
      in
      ignore handle_request
    ]} *)

(* ------------------------------------------------------------------ *)
(* Backend                                                             *)
(* ------------------------------------------------------------------ *)

type level = Debug | Info | Warn | Error

type log_entry = {
  timestamp_ns : int64;
  (** Monotonic nanoseconds from the same clock as [span_event.start_ns]/
      [end_ns] — the moment [log] was called, not when the span closed. A
      backend that needs a wall-clock timestamp per entry can derive one
      from [span_event.end_ns] and this field (e.g. by offsetting backward
      from a wall-clock read taken at close time), since this layer never
      touches the wall clock itself. *)
  level   : level;
  message : string;
  fields  : (string * string) list;
}

type span_event = {
  trace_ctx : Obs_trace.t;
  name      : string;
  service   : string;
  start_ns  : int64;  (** monotonic nanoseconds from [Eio.Time.Mono.now] *)
  end_ns    : int64;
  status    : [ `Ok | `Error of string ];
  (** [`Error msg] carries only [Printexc.to_string exn] — no backtrace.
      Deliberately minimal for v1: a backend wanting stack context would
      need its own field for [Printexc.get_backtrace ()], which isn't
      threaded through here. *)
  log_entries : log_entry list;
  (** Structured log entries from [log] calls within this span.
      Entries appear in call order. *)
  context   : (string * string) list;
  (** Ambient context from [with_context] at the time the span was opened.
      Backends use this for stream labels (Loki) or resource attributes (OTLP). *)
}

type metric_event = {
  name    : string;
  help    : string;
  kind    : [ `Counter of int | `Gauge of float | `Histogram of float ];
  labels  : (string * string) list;  (** call-site labels *)
  context : (string * string) list;
  (** Ambient context from [with_context] — frozen at [register_*] time, unlike
      [span_event.context]. The [t] a [register_*] emitter closure was built
      from is fixed forever; a later [with_context ot extra] on a *different*
      derived handle never reaches an emitter already registered against
      [ot]. Register from the most-specific [t] you'll ever want reflected in
      a given metric family, or re-register (a new family, since [name] is
      the same) from the more-specific handle if you need per-call context on
      a metric — there is no way to pass per-call context to an emitter, only
      per-call [labels]. *)
  service : string;
}

type metric_kind = [ `Counter | `Gauge | `Histogram ]

type metric_declaration = {
  declaration_name        : string;
  declaration_help        : string;
  declaration_kind        : metric_kind;
  declaration_label_names : string list;
  declaration_service     : string;
}
(** Registration-time metadata for one metric family, delivered to
    [backend.declare_metric] once per [register_*] call — before the first
    [emit_metric] for that family, possibly before any. A backend that wants
    a metric visible (e.g. at its zero value) as soon as it is registered,
    rather than only after its first observation, uses this; a backend that
    only cares about observed values (e.g. a log/trace-only backend) ignores
    it. *)

type backend = {
  emit_span      : span_event   -> unit;
  emit_metric    : metric_event -> unit;
  declare_metric : metric_declaration  -> unit;
}
(** A caller-supplied backend may raise; callers of [with_span], [log_standalone],
    and the [register_*] emitters/declarations never see ordinary backend
    exceptions — they are sent to [create]'s [on_backend_error] handler, so a
    broken backend cannot crash application code. If that handler itself
    raises, the failure falls back to stderr. [Eio.Cancel.Cancelled],
    [Out_of_memory], [Stack_overflow], and [Sys.Break] are never caught this
    way: cancellation must unwind Eio structured concurrency, and fatal runtime
    failures must not be turned into telemetry backend errors.

    Every field is called synchronously, inline, on the calling fiber — there
    is no queue, no background fiber, no offload of any kind at this layer.
    A slow [emit_span]/[emit_metric]/[declare_metric] (e.g. one that blocks
    on a network call) stalls whatever request path it's instrumenting for
    exactly as long as it takes to return. A backend that talks to the
    network (as [obs-loki-eio] and [obs-prometheus-eio]'s [push] do) owns
    the decision of whether to do that work inline, with a bounded timeout,
    or hand it off to a queue/background fiber itself — this layer does
    neither for you. *)

type backend_op =
  | Emit_span of { name : string }
  | Emit_metric of { name : string; kind : metric_kind }
  | Declare_metric of { name : string; kind : metric_kind }
(** Backend operation that failed. Passed to [on_backend_error]. *)

exception Multiple_backend_errors of exn list
(** Raised by [compose] when both sibling backends fail for the same operation.
    The containing [Obs_eio.t] catches it and passes it to [on_backend_error]
    like any other ordinary backend failure. *)

val noop    : backend
(** Drops all events. Use in tests and CI. *)

val stdout  : backend
(** Pretty-prints spans and metrics to stdout. Use for local development. *)

val compose : backend -> backend -> backend
(** Fan-out to two backends, e.g. [compose prometheus_backend loki_backend].
    Each backend's [emit_span]/[emit_metric]/[declare_metric] is called
    independently: if one raises, the other backend still receives the event.
    If both raise, [Multiple_backend_errors] preserves both exceptions for [create]'s
    [on_backend_error] handler. Cancellation and fatal runtime exceptions are
    never caught here (see [backend]'s doc) and propagate immediately, skipping
    the other backend, exactly as an uncaught exception from any other Eio
    operation would. *)

(* ------------------------------------------------------------------ *)
(* Handle                                                              *)
(* ------------------------------------------------------------------ *)

type t
(** Immutable and safe to share across fibers in one domain. Using it can
    still reach protected mutable state: backend closures may close over their
    own state, metric declarations are tracked inside [t], and [Obs_trace]'s
    ID generator is a mutex-protected process-global. Not tied to any
    particular span or request. *)

type span
(** A [span] value is only valid for the extent of the [with_span] callback
    it was handed to — [log] raises [Invalid_argument] once that callback has
    returned. Do not stash a [span] in a ref, or capture it in a fiber or
    domain that outlives the callback (e.g. via [Eio.Fiber.fork] or
    [Eio.Domain_manager.run]); a call to [log] from such a fiber will
    correctly raise rather than silently discard the entry, but the safe
    pattern is to open a new [with_span] inside that fiber/domain instead of
    reusing the outer one. *)

val create
  :  service:string
  -> mono_clock:_ Eio.Time.Mono.t
  -> ?on_backend_error:(backend_op -> exn -> unit)
  -> backend:backend
  -> unit
  -> t
(** [create ~service ~mono_clock ~backend ()] creates an observability handle.
    [mono_clock] is used for span duration measurement only — pass [env#mono_clock].
    Wall clock is never used for span timestamps.

    [on_backend_error], if supplied, is called synchronously when a backend
    raises during span/metric/declaration delivery. If it raises, the error is
    reported to stderr instead; cancellation and fatal runtime exceptions still
    propagate. *)

(* ------------------------------------------------------------------ *)
(* Context                                                             *)
(* ------------------------------------------------------------------ *)

val with_context : t -> (string * string) list -> t
(** [with_context ot fields] returns a new handle with [fields] merged into the
    ambient context. Fields in [fields] override existing keys with the same name.
    The original [ot] is unchanged — safe to pass to concurrent fibers. *)

(* ------------------------------------------------------------------ *)
(* Spans & logging                                                     *)
(* ------------------------------------------------------------------ *)

val with_span : t -> ?parent:Obs_trace.t -> string -> (span -> 'a) -> 'a
(** [with_span ot ?parent name f] opens a span, runs [f span], then closes it.
    If [f] raises, the span closes with [Error] status and the exception propagates.
    [parent] is typically from [Obs_trace.extract_from_headers] on an incoming
    request — linking this span to the upstream trace. See [span]'s doc for
    the lifetime [span] values are valid for.

    If the backend's own [emit_span] call raises [Eio.Cancel.Cancelled] while
    closing the span (see [backend]'s doc), that takes priority over the
    normal outcome: a successful [f]'s return value is discarded (cancellation
    dominates, same as any other Eio operation racing a cancellation). If [f]
    itself raised a different exception first, that original exception is
    logged to stderr before [Cancelled] propagates in its place — so it is
    never silently lost, even though it isn't what the caller ultimately
    sees. If [f] itself raised [Cancelled] too (the ordinary case of one
    cancellation reaching both the span body and the backend's own I/O),
    nothing is logged — there is nothing to lose, both exceptions are the
    same cancellation.

    There is no ambient/current-span mechanism: nesting is entirely manual.
    If code inside [f] calls another function that itself calls [with_span]
    without explicitly passing [?parent:(Obs_eio.current_trace_context sp)], that
    inner span starts a brand new root trace rather than becoming a child of
    the outer one — every intermediate call in between has to thread the
    parent context through by hand. This is a deliberate minimalism choice,
    not an oversight:
    {[
      Obs_eio.with_span ot "outer" (fun sp ->
        let parent = Obs_eio.current_trace_context sp in
        do_inner_work ~parent ...)
      (* and inside do_inner_work: *)
      let do_inner_work ~parent ... =
        Obs_eio.with_span ot ~parent "inner" (fun _sp -> ...)
    ]} *)

val log : span -> level -> ?fields:(string * string) list -> string -> unit
(** [log span level ?fields message] records a structured log entry inside an
    active span. Entries are buffered and included in [span_event.log_entries]
    when the span closes. The span's trace_id and span_id are attached
    automatically. Raises [Invalid_argument] if [span]'s [with_span] callback
    has already returned — see [span]'s doc. *)

val log_standalone : t -> ?parent:Obs_trace.t -> level -> ?fields:(string * string) list -> string -> unit
(** [log_standalone ot ?parent level ?fields message] logs without an explicit calling
    convention for an existing span. Equivalent to
    [with_span ot ?parent "log" (fun sp -> log sp level ?fields message)] —
    still opens (and immediately closes) a span internally, since this
    library has no logging primitive independent of spans; pass [?parent] to
    correlate the resulting span with an ambient trace instead of starting a
    new root trace for every log call. *)

val current_trace_context : span -> Obs_trace.t
(** [current_trace_context span] returns the active [Obs_trace.t] for an open span.
    Use with [Obs_trace.inject_to_headers] to propagate the trace into an
    outgoing request, linking the downstream span to this trace. *)

(* ------------------------------------------------------------------ *)
(* Metrics                                                             *)
(* ------------------------------------------------------------------ *)

val metric_name : string -> string
(** [metric_name name] validates [name] against Prometheus metric naming
    rules and returns it unchanged. Raises [Invalid_argument] on invalid
    names. [:] is accepted (the grammar allows it) but is conventionally
    reserved for recording rules (e.g. ["job:http_requests:rate5m"]), not
    direct instrumentation — avoid it in a name you register here. *)

type label_name = private string
(** Validated Prometheus label name. Construct with [label_name]. The
    [register_*] functions below validate their own [label_names:string list]
    internally and don't use this type directly — it exists for backend
    packages that need their own validated, typed label-name selectors (e.g.
    [obs-loki-eio]'s stream-label selection). *)

val label_name : string -> (label_name, string) result
(** [label_name name] validates [name] against Prometheus label naming rules.
    [Error _] on invalid names, including a name starting with ["__"] — that
    prefix is reserved for Prometheus's own internal use (e.g. [__name__]). *)

val label_name_exn : string -> label_name
(** Like [label_name], but raises [Invalid_argument] instead of returning
    [Error]. Intended for static label-name literals (a source-code
    constant), not runtime data — same convention as
    [Kafka_service.topic_name_exn] in the [kafka-eio-service] package. *)

val label_name_to_string : label_name -> string
(** [label_name_to_string name] returns the validated label name as a string. *)

type counter_fn   = ?labels:(string * string) list -> int   -> unit
type gauge_fn     = ?labels:(string * string) list -> float -> unit
type histogram_fn = ?labels:(string * string) list -> float -> unit
(** Typed emitter functions returned by [register_*]. *)

val register_counter
  :  t
  -> name:string
  -> help:string
  -> label_names:string list
  -> counter_fn
(** Register a counter metric family — synchronously delivers a
    [metric_declaration] to the backend's [declare_metric] before returning, then
    returns an emitter function. Call it once at startup, then call the
    returned function per event. [label_names] must be unique and each must
    pass the same rules as {!val-label_name} (so, e.g., a ["__"]-prefixed name is
    rejected here too); emitted labels must match the declared names
    exactly. The emitter raises [Invalid_argument] on a negative delta —
    Prometheus counters are monotonic.
    {[
      let reqs = Obs_eio.register_counter ot ~name:"http_requests_total"
                   ~help:"Total HTTP requests" ~label_names:["method";"status"] in
      reqs ~labels:[("method","POST");("status","200")] 1
    ]} *)

val register_gauge
  :  t
  -> name:string
  -> help:string
  -> label_names:string list
  -> gauge_fn
(** Register a gauge metric family — a value that can go up or down (e.g. a
    queue depth or an in-flight request count), unlike a counter. Same
    registration/declaration behavior as [register_counter], minus the
    negative-value rejection: a gauge's emitter simply replaces the current
    value, so negative deltas make no sense here to reject. No value is
    rejected here — negative, [nan], and infinite are all accepted as-is. *)

val register_histogram
  :  t
  -> name:string
  -> help:string
  -> label_names:string list
  -> histogram_fn
(** Register a histogram metric family — for distributions of observed
    values (e.g. request latency), sorted into backend-defined buckets on
    emission. Follow Prometheus convention and observe values in the metric's
    base unit (seconds for a duration, not milliseconds) unless a specific
    backend documents otherwise; nothing at this layer enforces a unit. The
    emitter raises [Invalid_argument] on a negative observation — matching
    [register_counter]'s negative-delta rejection, since a real distribution
    (e.g. a duration) has no negative values; a negative input here means the
    caller has a bug (e.g. a clock going backwards), not a legitimate
    observation. [nan] and [infinity] are NOT rejected at this layer — only
    [neg_infinity] is, as a side effect of the negative check above ([<] on
    floats: [neg_infinity < 0.0] is [true], but [nan < 0.0] and
    [infinity < 0.0] are both [false]), not a deliberate special case for it.

    [label_names] must not include ["le"] — raises [Invalid_argument] if it
    does, since the Prometheus backend synthesizes an ["le"] label per
    bucket sample and a caller-declared one would collide with it.
    Bucket boundaries are backend-defined (e.g. [Obs_prometheus]'s
    [default_bounds]); there is currently no per-metric override. *)

val register_counter_and_histogram
  :  t
  -> counter_name:string
  -> counter_help:string
  -> counter_labels:string list
  -> histogram_name:string
  -> histogram_help:string
  -> histogram_labels:string list
  -> counter_fn * histogram_fn
(** Register a counter and histogram metric family together — for when a
    caller always wants both a count and a duration metric for the same
    operation (e.g. "requests handled" plus "request duration").

    If the counter half succeeds (and so has already reached the backend's
    [declare_metric]) but the histogram half then raises — an invalid name
    or label — the counter's declaration is not retracted. In practice this
    only matters if a caller passes a bad histogram name/labels while a good
    counter name/labels, which is a startup-time programmer error that will
    raise immediately and be visible; it isn't a state a running service
    would drift into later. *)
