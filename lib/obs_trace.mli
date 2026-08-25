(** W3C traceparent-compatible trace context propagation.

    Deliberately minimal: carries just enough to serialize/parse the W3C
    [traceparent] header and derive child spans. No baggage, no tracestate,
    no vendor extensions — add a wrapper type around [t] if a consumer needs
    those; this module isn't the place for them (see the README's Out of
    Scope section for the reasoning). *)

type t = {
  trace_id    : int64 * int64;  (** 128-bit trace identifier *)
  span_id     : int64;          (** 64-bit span identifier *)
  trace_flags : char;           (** bit 0 = sampled *)
}

val generate   : unit -> t
(** Create a new root context with a fresh random trace_id and span_id, from
    a self-seeded PRNG state private to this module — no caller-side
    [Random.self_init ()] needed. Not cryptographically strong; sufficient
    for correlation and collision-avoidance, not for anything security-
    sensitive. Safe to call concurrently from multiple domains: the shared
    PRNG state is mutex-protected. *)

val child_span : t -> t
(** Derive a child span: inherits trace_id, generates a new span_id. *)

val traceparent_header : string
(** Canonical W3C traceparent header name. *)

val to_traceparent : t -> string
(** Serialize to W3C traceparent: ["00-{32hex}-{16hex}-{02hex}"] *)

val of_traceparent : string -> t option
(** Parse a W3C traceparent header value. Returns [None] if malformed.
    Does not reject the all-zero trace-id or span-id that the W3C spec
    reserves as invalid — a malformed-but-parseable upstream header is
    accepted rather than treated as absent. Follows the spec's forward-
    compatibility rule: version ["00"] must be exactly the 4 documented
    fields, but any other version (except the reserved-invalid ["ff"]) may
    carry additional trailing fields for that future version's own use —
    those are ignored, since trace-id/span-id/flags still live at the same
    fixed positions regardless of version. *)

val extract_from_headers : (string * string) list -> t option
(** Look up {!traceparent_header} in a header list case-insensitively and parse it. *)

val inject_to_headers : t -> (string * string) list -> (string * string) list
(** Set or replace {!traceparent_header} in a header list case-insensitively. *)
