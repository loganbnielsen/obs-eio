(** Unit tests for obs-eio. No broker required. *)

let noop_declare = (fun (_ : Obs_eio.metric_decl) -> ())

let capture_stdout f =
  let old_stdout = Unix.dup Unix.stdout in
  let read_fd, write_fd = Unix.pipe () in
  Unix.dup2 write_fd Unix.stdout;
  Unix.close write_fd;
  let restored = ref false in
  let restore () =
    if not !restored then begin
      Unix.dup2 old_stdout Unix.stdout;
      Unix.close old_stdout;
      restored := true
    end
  in
  match f () with
  | result ->
    flush Stdlib.stdout;
    restore ();
    let ic = Unix.in_channel_of_descr read_fd in
    let buf = Buffer.create 128 in
    (try
       while true do
         Buffer.add_string buf (input_line ic);
         Buffer.add_char buf '\n'
       done
     with End_of_file -> ());
    close_in ic;
    (result, Buffer.contents buf)
  | exception exn ->
    restore ();
    Unix.close read_fd;
    raise exn

let capture_stderr f =
  let old_stderr = Unix.dup Unix.stderr in
  let read_fd, write_fd = Unix.pipe () in
  Unix.dup2 write_fd Unix.stderr;
  Unix.close write_fd;
  let restored = ref false in
  let restore () =
    if not !restored then begin
      Unix.dup2 old_stderr Unix.stderr;
      Unix.close old_stderr;
      restored := true
    end
  in
  match f () with
  | result ->
    flush Stdlib.stderr;
    restore ();
    let ic = Unix.in_channel_of_descr read_fd in
    let buf = Buffer.create 128 in
    (try
       while true do
         Buffer.add_string buf (input_line ic);
         Buffer.add_char buf '\n'
       done
     with End_of_file -> ());
    close_in ic;
    (result, Buffer.contents buf)
  | exception exn ->
    restore ();
    Unix.close read_fd;
    raise exn

(* ------------------------------------------------------------------ *)
(* Obs_trace                                                           *)
(* ------------------------------------------------------------------ *)

let test_traceparent_roundtrip () =
  let ctx = Obs_trace.generate () in
  match Obs_trace.of_traceparent (Obs_trace.to_traceparent ctx) with
  | None -> Alcotest.fail "of_traceparent returned None on valid traceparent"
  | Some ctx2 ->
    Alcotest.(check bool) "trace_id preserved" true (ctx.trace_id = ctx2.trace_id);
    Alcotest.(check bool) "span_id preserved"  true (ctx.span_id  = ctx2.span_id);
    Alcotest.(check char) "flags preserved"    ctx.trace_flags ctx2.trace_flags

let test_child_span () =
  let root  = Obs_trace.generate () in
  let child = Obs_trace.child_span root in
  Alcotest.(check bool) "same trace_id"     true  (root.trace_id = child.trace_id);
  Alcotest.(check bool) "different span_id" false (root.span_id  = child.span_id)

let test_of_traceparent_malformed () =
  Alcotest.(check bool) "garbage" true (Obs_trace.of_traceparent "garbage" = None);
  Alcotest.(check bool) "reserved invalid version \"ff\"" true
    (Obs_trace.of_traceparent
       "ff-00000000000000000000000000000001-0000000000000001-01" = None);
  Alcotest.(check bool) "version \"00\" with an extra trailing field" true
    (Obs_trace.of_traceparent
       "00-00000000000000000000000000000001-0000000000000001-01-extra" = None);
  Alcotest.(check bool) "reserved invalid version \"FF\" (uppercase)" true
    (Obs_trace.of_traceparent
       "FF-00000000000000000000000000000001-0000000000000001-01" = None);
  (* Int64.of_string/int_of_string treat '_' as a digit-group separator and
     would otherwise silently drop it instead of rejecting the header. *)
  Alcotest.(check bool) "underscore in span-id hex is rejected, not silently dropped"
    true (Obs_trace.of_traceparent
            "00-00000000000000000000000000000001-123456789abcde_f-01" = None)

let test_of_traceparent_forward_compatible_version () =
  (* A future version ("01" here) may append trailing fields of its own;
     trace-id/span-id/flags still parse from the same fixed positions. *)
  match Obs_trace.of_traceparent
          "01-00000000000000000000000000000001-0000000000000001-01-00f067aa0ba902b7" with
  | None -> Alcotest.fail "expected Some: a non-00, non-ff version with trailing fields is valid"
  | Some ctx ->
    Alcotest.(check bool) "trace_id parsed" true (ctx.trace_id = (0L, 1L));
    Alcotest.(check bool) "span_id parsed"  true (ctx.span_id = 1L)

let test_of_traceparent_accepts_all_zero_reserved_id () =
  (* Documented leniency: the W3C-reserved all-zero trace-id/span-id parses
     successfully rather than being treated as malformed. *)
  match Obs_trace.of_traceparent
          "00-00000000000000000000000000000000-0000000000000000-01" with
  | None -> Alcotest.fail "expected Some, all-zero ids are parsed leniently"
  | Some ctx ->
    Alcotest.(check bool) "trace_id is all zero" true (ctx.trace_id = (0L, 0L));
    Alcotest.(check bool) "span_id is all zero"  true (ctx.span_id = 0L)

let test_inject_extract_headers () =
  let ctx     = Obs_trace.generate () in
  let headers = Obs_trace.inject_to_headers ctx [("content-type", "application/json")] in
  Alcotest.(check bool) "traceparent present"
    true (List.mem_assoc Obs_trace.traceparent_header headers);
  Alcotest.(check bool) "original header preserved"
    true (List.assoc_opt "content-type" headers = Some "application/json");
  match Obs_trace.extract_from_headers headers with
  | None      -> Alcotest.fail "extract returned None"
  | Some ctx2 ->
    Alcotest.(check bool) "trace_id round-trips" true (ctx.trace_id = ctx2.trace_id);
    Alcotest.(check bool) "span_id round-trips"  true (ctx.span_id  = ctx2.span_id)

let test_inject_replaces_existing () =
  let ctx1    = Obs_trace.generate () in
  let ctx2    = Obs_trace.generate () in
  let headers = Obs_trace.inject_to_headers ctx1 [] in
  let headers = Obs_trace.inject_to_headers ctx2 headers in
  Alcotest.(check int) "exactly one traceparent"
    1
    (List.length
       (List.filter (fun (k, _) -> k = Obs_trace.traceparent_header) headers));
  match Obs_trace.extract_from_headers headers with
  | None      -> Alcotest.fail "extract returned None"
  | Some ctx3 -> Alcotest.(check bool) "latest wins" true (ctx2.span_id = ctx3.span_id)

let test_extract_headers_case_insensitive () =
  let ctx     = Obs_trace.generate () in
  let headers = [("TraceParent", Obs_trace.to_traceparent ctx)] in
  match Obs_trace.extract_from_headers headers with
  | None      -> Alcotest.fail "extract returned None"
  | Some ctx2 ->
    Alcotest.(check bool) "trace_id round-trips" true (ctx.trace_id = ctx2.trace_id);
    Alcotest.(check bool) "span_id round-trips"  true (ctx.span_id  = ctx2.span_id)

let test_inject_replaces_existing_case_insensitive () =
  let ctx1    = Obs_trace.generate () in
  let ctx2    = Obs_trace.generate () in
  let headers = [
    ("TraceParent", Obs_trace.to_traceparent ctx1);
    ("content-type", "application/json");
  ] in
  let headers = Obs_trace.inject_to_headers ctx2 headers in
  Alcotest.(check int) "exactly one traceparent variant"
    1 (List.length (List.filter
         (fun (k, _) -> String.lowercase_ascii k = Obs_trace.traceparent_header)
         headers));
  Alcotest.(check bool) "canonical traceparent present"
    true (List.mem_assoc Obs_trace.traceparent_header headers);
  Alcotest.(check bool) "original header preserved"
    true (List.assoc_opt "content-type" headers = Some "application/json");
  match Obs_trace.extract_from_headers headers with
  | None      -> Alcotest.fail "extract returned None"
  | Some ctx3 -> Alcotest.(check bool) "latest wins" true (ctx2.span_id = ctx3.span_id)

let test_generate_is_domain_safe () =
  let n_domains = 4 and per_domain = 200 in
  let domains = List.init n_domains (fun _ ->
    Domain.spawn (fun () ->
      List.init per_domain (fun _ -> (Obs_trace.generate ()).span_id)))
  in
  let all_ids = List.concat_map Domain.join domains in
  Alcotest.(check int) "no crash or lost work under concurrent domains"
    (n_domains * per_domain) (List.length all_ids);
  let module I64Set = Set.Make (Int64) in
  let unique = I64Set.cardinal (I64Set.of_list all_ids) in
  Alcotest.(check bool) "ids stay unique across domains (no torn/corrupted state)"
    true (unique >= (n_domains * per_domain) - 1)

let test_generate_uses_full_64_bits () =
  (* Random.State.int64 rng_state Int64.max_int would make the top bit of
     every id always 0 (never in [0x8000000000000000, 0xffffffffffffffff]).
     Generate enough ids that seeing at least one with the top bit set is
     overwhelmingly likely if the fix is in place, and impossible if it
     regresses back to the old biased generator. *)
  let top_bit_set id = Int64.compare id 0L < 0 in
  let ids = List.init 200 (fun _ -> (Obs_trace.generate ()).span_id) in
  Alcotest.(check bool) "at least one span_id has the top bit set"
    true (List.exists top_bit_set ids)

(* ------------------------------------------------------------------ *)
(* Context                                                             *)
(* ------------------------------------------------------------------ *)

let test_with_context_merges () =
  Eio_main.run @@ fun env ->
  let last_ctx = ref [] in
  let backend = {
    Obs_eio.emit_span   = (fun _ -> ());
    emit_metric      = (fun e -> last_ctx := e.Obs_eio.context);
    declare_metric   = noop_declare;
  } in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend in
  let ot = Obs_eio.with_context ot [("env", "prod"); ("region", "us-east-1")] in
  let ot = Obs_eio.with_context ot [("env", "staging")] in  (* override env *)
  let emit = Obs_eio.register_counter ot ~name:"n" ~help:"" ~label_names:[] in
  emit 1;
  Alcotest.(check string) "env overridden to staging"
    "staging" (List.assoc "env" !last_ctx);
  Alcotest.(check bool) "region preserved"
    true (List.mem_assoc "region" !last_ctx)

let test_with_context_immutable () =
  Eio_main.run @@ fun env ->
  let ctxs = ref [] in
  let backend = {
    Obs_eio.emit_span   = (fun _ -> ());
    emit_metric      = (fun e -> ctxs := e.Obs_eio.context :: !ctxs);
    declare_metric   = noop_declare;
  } in
  let ot  = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend in
  let ot1 = Obs_eio.with_context ot  [("req", "a")] in
  let ot2 = Obs_eio.with_context ot  [("req", "b")] in
  let emit1 = Obs_eio.register_counter ot1 ~name:"n" ~help:"" ~label_names:[] in
  let emit2 = Obs_eio.register_counter ot2 ~name:"n" ~help:"" ~label_names:[] in
  emit1 1; emit2 1;
  (match !ctxs with
   | [ctx_b; ctx_a] ->
     Alcotest.(check string) "ot1 context unaffected"  "a" (List.assoc "req" ctx_a);
     Alcotest.(check string) "ot2 context independent" "b" (List.assoc "req" ctx_b)
   | _ -> Alcotest.fail "expected exactly 2 metric events")

(* ------------------------------------------------------------------ *)
(* Spans                                                               *)
(* ------------------------------------------------------------------ *)

let test_span_context_reflects_with_context () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let backend = {
    Obs_eio.emit_span   = (fun e -> spans := e :: !spans);
    emit_metric      = (fun _ -> ());
    declare_metric   = noop_declare;
  } in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend in
  let ot = Obs_eio.with_context ot [("request_id", "r-1")] in
  Obs_eio.with_span ot "op" (fun _sp -> ());
  Alcotest.(check string) "span picked up the context at call time"
    "r-1" (List.assoc "request_id" (List.hd !spans).Obs_eio.context)

let test_span_emitted () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let backend = {
    Obs_eio.emit_span   = (fun e -> spans := e :: !spans);
    emit_metric      = (fun _ -> ());
    declare_metric   = noop_declare;
  } in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend in
  Obs_eio.with_span ot "do_work" (fun _sp -> ());
  Alcotest.(check int)  "one span emitted"  1       (List.length !spans);
  Alcotest.(check string) "span name"  "do_work"    (List.hd !spans).Obs_eio.name;
  Alcotest.(check string) "service"    "svc"        (List.hd !spans).Obs_eio.service;
  Alcotest.(check bool)   "end >= start" true
    ((List.hd !spans).Obs_eio.end_ns >= (List.hd !spans).Obs_eio.start_ns)

let test_span_ok_status () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun e -> spans := e :: !spans);
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  Obs_eio.with_span ot "op" (fun _sp -> ());
  Alcotest.(check bool) "ok status"
    true (match (List.hd !spans).Obs_eio.status with `Ok -> true | _ -> false)

let test_span_error_status_on_exception () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun e -> spans := e :: !spans);
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  (try Obs_eio.with_span ot "boom" (fun _sp -> raise Exit) with Exit -> ());
  Alcotest.(check bool) "error status on exception"
    true (match (List.hd !spans).Obs_eio.status with `Error _ -> true | _ -> false)

let test_log_appends_to_span () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun e -> spans := e :: !spans);
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  Obs_eio.with_span ot "op" (fun sp ->
    Obs_eio.log sp Obs_eio.Info ~fields:[("key", "val")] "first";
    Obs_eio.log sp Obs_eio.Warn "second");
  let span = List.hd !spans in
  let entries = span.Obs_eio.log_entries in
  Alcotest.(check int) "two log entries" 2 (List.length entries);
  (match entries with
   | [first; second] ->
     Alcotest.(check bool) "first level" true (first.Obs_eio.level = Obs_eio.Info);
     Alcotest.(check string) "first message" "first" first.Obs_eio.message;
     Alcotest.(check bool) "extra field present"
       true (List.exists (fun (k, v) -> k = "key" && v = "val") first.Obs_eio.fields);
     Alcotest.(check bool) "second level" true (second.Obs_eio.level = Obs_eio.Warn);
     Alcotest.(check string) "second message" "second" second.Obs_eio.message
   | _ -> Alcotest.fail "expected exactly two log entries")

let test_log_order_preserved () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun e -> spans := e :: !spans);
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  Obs_eio.with_span ot "op" (fun sp ->
    Obs_eio.log sp Obs_eio.Info "first";
    Obs_eio.log sp Obs_eio.Info "second");
  let msgs = (List.hd !spans).Obs_eio.log_entries
    |> List.map (fun entry -> entry.Obs_eio.message) in
  Alcotest.(check (list string)) "call order preserved" ["first"; "second"] msgs

let test_log_after_close_raises () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  let escaped = ref None in
  Obs_eio.with_span ot "op" (fun sp -> escaped := Some sp);
  match !escaped with
  | None -> Alcotest.fail "span was not captured"
  | Some sp ->
    (match Obs_eio.log sp Obs_eio.Info "too late" with
     | ()                        -> Alcotest.fail "log on a closed span should raise"
     | exception Invalid_argument _ -> ())

let test_current_trace_ctx_child_of_parent () =
  Eio_main.run @@ fun env ->
  let ot     = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  let parent = Obs_trace.generate () in
  Obs_eio.with_span ot ~parent "child" (fun sp ->
    let ctx = Obs_eio.current_trace_ctx sp in
    Alcotest.(check bool) "inherits trace_id"
      true (parent.Obs_trace.trace_id = ctx.Obs_trace.trace_id);
    Alcotest.(check bool) "new span_id"
      true (parent.Obs_trace.span_id <> ctx.Obs_trace.span_id))

let test_with_span_no_parent_generates_root () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun e -> spans := e :: !spans);
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  Obs_eio.with_span ot "root" (fun _sp -> ());
  let tp = Obs_trace.to_traceparent (List.hd !spans).Obs_eio.trace_ctx in
  Alcotest.(check bool) "valid traceparent"
    true (Obs_trace.of_traceparent tp <> None)

let test_log_t_uses_parent () =
  Eio_main.run @@ fun env ->
  let spans = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun e -> spans := e :: !spans);
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  let parent = Obs_trace.generate () in
  Obs_eio.log_t ot ~parent Obs_eio.Info "standalone log";
  Alcotest.(check bool) "log_t's span shares the parent trace_id"
    true (parent.Obs_trace.trace_id = (List.hd !spans).Obs_eio.trace_ctx.Obs_trace.trace_id)

(* ------------------------------------------------------------------ *)
(* Metrics                                                             *)
(* ------------------------------------------------------------------ *)

let test_counter_emits_event () =
  Eio_main.run @@ fun env ->
  let metrics = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun e -> metrics := e :: !metrics);
               declare_metric = noop_declare } in
  let c = Obs_eio.register_counter ot ~name:"reqs" ~help:"desc" ~label_names:["method"] in
  c ~labels:[("method", "POST")] 1;
  Alcotest.(check int)    "one event"   1     (List.length !metrics);
  Alcotest.(check string) "name"        "reqs" (List.hd !metrics).Obs_eio.name;
  Alcotest.(check string) "service"     "svc"  (List.hd !metrics).Obs_eio.service;
  Alcotest.(check bool)   "is counter"  true
    (match (List.hd !metrics).Obs_eio.kind with `Counter 1 -> true | _ -> false);
  Alcotest.(check string) "label value" "POST"
    (List.assoc "method" (List.hd !metrics).Obs_eio.labels)

let test_gauge_accepts_negative_value () =
  Eio_main.run @@ fun env ->
  let metrics = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun e -> metrics := e :: !metrics);
               declare_metric = noop_declare } in
  let g = Obs_eio.register_gauge ot ~name:"delta" ~help:"desc" ~label_names:[] in
  (* Must not raise: unlike a counter, a gauge legitimately goes negative. *)
  g (-5.0);
  Alcotest.(check bool) "is gauge with the negative value"
    true (match (List.hd !metrics).Obs_eio.kind with `Gauge (-5.0) -> true | _ -> false)

let test_histogram_rejects_negative_observation () =
  Eio_main.run @@ fun env ->
  let emitted = ref 0 in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ()); emit_metric = (fun _ -> incr emitted);
               declare_metric = noop_declare } in
  let h = Obs_eio.register_histogram ot ~name:"latency" ~help:"desc" ~label_names:[] in
  (match h (-0.5) with
   | ()                        -> Alcotest.fail "negative observation should raise"
   | exception Invalid_argument _ -> ());
  Alcotest.(check int) "backend not called for negative observation" 0 !emitted

let test_histogram_emits_event () =
  Eio_main.run @@ fun env ->
  let metrics = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun e -> metrics := e :: !metrics);
               declare_metric = noop_declare } in
  let h = Obs_eio.register_histogram ot ~name:"latency_ms" ~help:"desc" ~label_names:[] in
  h 42.5;
  Alcotest.(check bool) "is histogram"
    true (match (List.hd !metrics).Obs_eio.kind with `Histogram 42.5 -> true | _ -> false)

let test_counter_and_histogram_helper_emits_both () =
  Eio_main.run @@ fun env ->
  let metrics = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun e -> metrics := e :: !metrics);
               declare_metric = noop_declare } in
  let c, h =
    Obs_eio.register_counter_and_histogram ot
      ~counter_name:"items_total"
      ~counter_help:"Total items"
      ~counter_labels:["status"]
      ~histogram_name:"item_duration_seconds"
      ~histogram_help:"Item duration"
      ~histogram_labels:[]
  in
  c ~labels:[("status", "ok")] 1;
  h 0.25;
  let names = List.map (fun e -> e.Obs_eio.name) (List.rev !metrics) in
  Alcotest.(check (list string)) "emits both metrics"
    ["items_total"; "item_duration_seconds"] names

let check_invalid_arg label f =
  match f () with
  | () -> Alcotest.fail (label ^ " should raise Invalid_argument")
  | exception Invalid_argument _ -> ()

let test_emit_accepts_declared_labels_any_order () =
  Eio_main.run @@ fun env ->
  let metrics = ref [] in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun e -> metrics := e :: !metrics);
               declare_metric = noop_declare } in
  let g =
    Obs_eio.register_gauge ot
      ~name:"http_inflight_requests" ~help:"desc"
      ~label_names:["method"; "status"]
  in
  g ~labels:[("status", "200"); ("method", "GET")] 3.0;
  match !metrics with
  | [metric] ->
    Alcotest.(check string) "method" "GET" (List.assoc "method" metric.Obs_eio.labels);
    Alcotest.(check string) "status" "200" (List.assoc "status" metric.Obs_eio.labels)
  | _ -> Alcotest.fail "expected exactly one metric event"

let test_emit_rejects_missing_extra_duplicate_labels () =
  Eio_main.run @@ fun env ->
  let emitted = ref 0 in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun _ -> incr emitted);
               declare_metric = noop_declare } in
  let c =
    Obs_eio.register_counter ot
      ~name:"http_requests_total" ~help:"desc"
      ~label_names:["method"; "status"]
  in
  let g =
    Obs_eio.register_gauge ot
      ~name:"queue_depth" ~help:"desc"
      ~label_names:["queue"]
  in
  let h =
    Obs_eio.register_histogram ot
      ~name:"request_duration_seconds" ~help:"desc"
      ~label_names:["route"; "status"]
  in
  check_invalid_arg "missing label" (fun () ->
    c ~labels:[("method", "GET")] 1);
  check_invalid_arg "extra label" (fun () ->
    g ~labels:[("queue", "jobs"); ("host", "api-1")] 1.0);
  check_invalid_arg "duplicate label" (fun () ->
    h ~labels:[("route", "/"); ("route", "/health"); ("status", "200")] 0.1);
  Alcotest.(check int) "backend not called for invalid labels" 0 !emitted

let test_metric_name_validation () =
  Alcotest.(check string) "valid metric name"
    "http_requests_total" (Obs_eio.metric_name "http_requests_total");
  (* Colons are valid per the Prometheus name grammar, but conventionally
     reserved for recording rules (e.g. "job:http_requests:rate5m"), not
     direct instrumentation — see register_counter's mli doc. *)
  Alcotest.(check string) "colon allowed in metric name"
    "job:http_requests_total" (Obs_eio.metric_name "job:http_requests_total");
  check_invalid_arg "empty metric name" (fun () ->
    ignore (Obs_eio.metric_name ""));
  check_invalid_arg "metric name starting with digit" (fun () ->
    ignore (Obs_eio.metric_name "1_total"));
  check_invalid_arg "metric name containing dash" (fun () ->
    ignore (Obs_eio.metric_name "http-requests-total"))

let test_label_name_validation () =
  Alcotest.(check string) "valid label name"
    "http_status" (Obs_eio.label_name_to_string (Obs_eio.label_name "http_status"));
  check_invalid_arg "empty label name" (fun () ->
    ignore (Obs_eio.label_name ""));
  check_invalid_arg "label name starting with digit" (fun () ->
    ignore (Obs_eio.label_name "1status"));
  check_invalid_arg "label name containing colon" (fun () ->
    ignore (Obs_eio.label_name "http:status"))

let test_register_rejects_invalid_metric_name () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  check_invalid_arg "invalid counter name" (fun () ->
    let _emit : Obs_eio.counter_fn =
      Obs_eio.register_counter ot ~name:"bad-name" ~help:"desc" ~label_names:[]
    in
    ());
  check_invalid_arg "invalid gauge name" (fun () ->
    let _emit : Obs_eio.gauge_fn =
      Obs_eio.register_gauge ot ~name:"9bad" ~help:"desc" ~label_names:[]
    in
    ());
  check_invalid_arg "invalid histogram name" (fun () ->
    let _emit : Obs_eio.histogram_fn =
      Obs_eio.register_histogram ot ~name:"bad.name" ~help:"desc" ~label_names:[]
    in
    ())

let test_register_rejects_invalid_label_name () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  check_invalid_arg "invalid counter label" (fun () ->
    let _emit : Obs_eio.counter_fn =
      Obs_eio.register_counter ot
        ~name:"requests_total" ~help:"desc" ~label_names:["bad-label"]
    in
    ());
  check_invalid_arg "invalid gauge label" (fun () ->
    let _emit : Obs_eio.gauge_fn =
      Obs_eio.register_gauge ot
        ~name:"queue_depth" ~help:"desc" ~label_names:["9host"]
    in
    ());
  check_invalid_arg "invalid histogram label" (fun () ->
    let _emit : Obs_eio.histogram_fn =
      Obs_eio.register_histogram ot
        ~name:"request_duration_seconds" ~help:"desc"
        ~label_names:["bad.label"]
    in
    ())

let test_register_rejects_duplicate_label_names () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  check_invalid_arg "duplicate registered label" (fun () ->
    let _emit : Obs_eio.counter_fn =
      Obs_eio.register_counter ot
        ~name:"requests_total" ~help:"desc"
        ~label_names:["method"; "method"]
    in
    ())

let test_register_counter_rejects_negative_delta () =
  Eio_main.run @@ fun env ->
  let emitted = ref 0 in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ()); emit_metric = (fun _ -> incr emitted);
               declare_metric = noop_declare } in
  let c = Obs_eio.register_counter ot ~name:"reqs_total" ~help:"desc" ~label_names:[] in
  check_invalid_arg "negative counter delta" (fun () -> c (-1));
  Alcotest.(check int) "backend not called for negative delta" 0 !emitted

let test_register_histogram_rejects_le_label () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  check_invalid_arg "\"le\" as a declared histogram label" (fun () ->
    let _emit : Obs_eio.histogram_fn =
      Obs_eio.register_histogram ot
        ~name:"request_duration_seconds" ~help:"desc"
        ~label_names:["le"]
    in
    ())

let test_register_declares_before_first_emit () =
  Eio_main.run @@ fun env ->
  let declared = ref [] and emitted = ref 0 in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ());
               emit_metric = (fun _ -> incr emitted);
               declare_metric = (fun d -> declared := d :: !declared) } in
  let c =
    Obs_eio.register_counter ot
      ~name:"reqs_total" ~help:"Total requests" ~label_names:["route"]
  in
  Alcotest.(check int) "declared exactly once, before any emit"
    1 (List.length !declared);
  Alcotest.(check int) "not emitted yet" 0 !emitted;
  (match !declared with
   | [d] ->
     Alcotest.(check string) "declared name" "reqs_total" d.Obs_eio.decl_name;
     Alcotest.(check string) "declared help" "Total requests" d.Obs_eio.decl_help;
     Alcotest.(check string) "declared service" "svc" d.Obs_eio.decl_service;
     Alcotest.(check bool)   "declared kind is Counter"
       true (d.Obs_eio.decl_kind = `Counter);
     Alcotest.(check (list string)) "declared label names" ["route"] d.Obs_eio.decl_label_names
   | _ -> Alcotest.fail "expected exactly one declaration");
  c ~labels:[("route", "/")] 1;
  Alcotest.(check int) "emitted once after use" 1 !emitted

let contains_substring haystack needle =
  let h = String.length haystack and n = String.length needle in
  let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
  n = 0 || go 0

let test_stdout_backend_runs_and_prints () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.stdout in
  let ((), output) = capture_stdout (fun () ->
    Obs_eio.with_span ot "op" (fun sp ->
      Obs_eio.log sp Obs_eio.Info ~fields:[("key", "val")] "hello");
    let c = Obs_eio.register_counter ot ~name:"n" ~help:"" ~label_names:[] in
    c 1)
  in
  Alcotest.(check bool) "printed a SPAN line" true (contains_substring output "SPAN");
  Alcotest.(check bool) "printed a METRIC line" true (contains_substring output "METRIC")

let test_noop_compiles_and_runs () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock ~backend:Obs_eio.noop in
  Obs_eio.with_span ot "op" (fun sp ->
    Obs_eio.log sp Obs_eio.Info "hello";
    let c = Obs_eio.register_counter ot ~name:"n" ~help:"" ~label_names:[] in
    c 1)

(* ------------------------------------------------------------------ *)
(* compose                                                             *)
(* ------------------------------------------------------------------ *)

let test_compose_fans_out () =
  Eio_main.run @@ fun env ->
  let spans_a = ref 0 and spans_b = ref 0 in
  let backend_a = { Obs_eio.emit_span = (fun _ -> incr spans_a); emit_metric = (fun _ -> ());
                     declare_metric = noop_declare } in
  let backend_b = { Obs_eio.emit_span = (fun _ -> incr spans_b); emit_metric = (fun _ -> ());
                     declare_metric = noop_declare } in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:(Obs_eio.compose backend_a backend_b) in
  Obs_eio.with_span ot "op" (fun _sp -> ());
  Alcotest.(check int) "backend_a received span" 1 !spans_a;
  Alcotest.(check int) "backend_b received span" 1 !spans_b

let test_compose_isolates_a_raising_sibling () =
  Eio_main.run @@ fun env ->
  let spans_b = ref 0 and metrics_b = ref 0 and declares_b = ref 0 in
  let backend_a =
    { Obs_eio.emit_span = (fun _ -> failwith "boom span");
      emit_metric = (fun _ -> failwith "boom metric");
      declare_metric = (fun _ -> failwith "boom declare") } in
  let backend_b =
    { Obs_eio.emit_span = (fun _ -> incr spans_b);
      emit_metric = (fun _ -> incr metrics_b);
      declare_metric = (fun _ -> incr declares_b) } in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:(Obs_eio.compose backend_a backend_b) in
  Obs_eio.with_span ot "op" (fun _sp -> ());
  let c = Obs_eio.register_counter ot ~name:"n" ~help:"" ~label_names:[] in
  c 1;
  Alcotest.(check int) "sibling still received span despite a's exception" 1 !spans_b;
  Alcotest.(check int) "sibling still received metric despite a's exception" 1 !metrics_b;
  Alcotest.(check int) "sibling still received declare despite a's exception" 1 !declares_b

let test_with_span_survives_raising_backend () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> failwith "boom"); emit_metric = (fun _ -> ());
               declare_metric = noop_declare } in
  (* Must not raise: a broken backend cannot crash application code. *)
  Obs_eio.with_span ot "op" (fun _sp -> ())

let test_register_counter_survives_raising_backend () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ()); emit_metric = (fun _ -> failwith "boom");
               declare_metric = noop_declare } in
  let c = Obs_eio.register_counter ot ~name:"n" ~help:"" ~label_names:[] in
  (* Must not raise. *)
  c 1

let test_register_survives_raising_declare () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> ()); emit_metric = (fun _ -> ());
               declare_metric = (fun _ -> failwith "boom") } in
  (* Must not raise: registration itself must survive a broken backend too. *)
  let c = Obs_eio.register_counter ot ~name:"n" ~help:"" ~label_names:[] in
  c 1

let test_with_span_propagates_cancelled () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> raise (Eio.Cancel.Cancelled Exit));
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  (* Unlike an ordinary exception, Eio.Cancel.Cancelled from a backend must
     NOT be swallowed — a cancellation is not "the backend is buggy", it's
     the caller's own structured concurrency unwinding. *)
  match Obs_eio.with_span ot "op" (fun _sp -> ()) with
  | ()                              -> Alcotest.fail "Cancelled should have propagated, not been swallowed"
  | exception Eio.Cancel.Cancelled _ -> ()

let test_with_span_logs_original_exception_lost_to_cancelled () =
  Eio_main.run @@ fun env ->
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:{ Obs_eio.emit_span = (fun _ -> raise (Eio.Cancel.Cancelled Exit));
               emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  (* Double fault: the application body fails with an ordinary exception,
     and closing the span (emit_span) then also raises Cancelled instead of
     returning. Cancelled must win (it's what the caller sees), but the
     original application failure must not vanish with no trace at all — it
     has to be logged before Cancelled takes over. *)
  let (result, stderr_output) =
    capture_stderr (fun () ->
      match Obs_eio.with_span ot "op" (fun _sp -> failwith "original failure") with
      | ()                                -> `Wrong_return
      | exception Eio.Cancel.Cancelled _ -> `Cancelled_propagated
      | exception (Failure _)             -> `Wrong_original_exn_leaked)
  in
  Alcotest.(check bool) "Cancelled is what the caller sees"
    true (result = `Cancelled_propagated);
  Alcotest.(check bool) "the original failure was logged, not silently dropped"
    true (contains_substring stderr_output "original failure")

let test_compose_propagates_cancelled_and_skips_sibling () =
  Eio_main.run @@ fun env ->
  let spans_b = ref 0 in
  let backend_a = { Obs_eio.emit_span = (fun _ -> raise (Eio.Cancel.Cancelled Exit));
                     emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  let backend_b = { Obs_eio.emit_span = (fun _ -> incr spans_b);
                     emit_metric = (fun _ -> ()); declare_metric = noop_declare } in
  let ot = Obs_eio.create ~service:"svc" ~mono_clock:env#mono_clock
    ~backend:(Obs_eio.compose backend_a backend_b) in
  (match Obs_eio.with_span ot "op" (fun _sp -> ()) with
   | ()                              -> Alcotest.fail "Cancelled should have propagated"
   | exception Eio.Cancel.Cancelled _ -> ());
  Alcotest.(check int) "sibling backend was skipped, not called" 0 !spans_b

(* ------------------------------------------------------------------ *)
(* Test runner                                                         *)
(* ------------------------------------------------------------------ *)

let () =
  let open Alcotest in
  run "obs_eio" [
    "trace_context", [
      test_case "traceparent round-trips"         `Quick test_traceparent_roundtrip;
      test_case "child_span inherits trace_id"    `Quick test_child_span;
      test_case "malformed traceparent → None"    `Quick test_of_traceparent_malformed;
      test_case "forward-compatible version with trailing fields" `Quick test_of_traceparent_forward_compatible_version;
      test_case "all-zero reserved id parses leniently" `Quick test_of_traceparent_accepts_all_zero_reserved_id;
      test_case "inject/extract headers"          `Quick test_inject_extract_headers;
      test_case "inject replaces existing header" `Quick test_inject_replaces_existing;
      test_case "extract mixed-case header"       `Quick test_extract_headers_case_insensitive;
      test_case "inject replaces mixed-case header" `Quick
        test_inject_replaces_existing_case_insensitive;
      test_case "generated ids use the full 64 bits" `Quick test_generate_uses_full_64_bits;
      test_case "generate is safe across domains" `Quick test_generate_is_domain_safe;
    ];
    "context", [
      test_case "with_context merges and overrides" `Quick test_with_context_merges;
      test_case "with_context is immutable"         `Quick test_with_context_immutable;
    ];
    "spans", [
      test_case "span picks up context at call time" `Quick test_span_context_reflects_with_context;
      test_case "span emitted on close"           `Quick test_span_emitted;
      test_case "ok status on normal return"      `Quick test_span_ok_status;
      test_case "error status on exception"       `Quick test_span_error_status_on_exception;
      test_case "log appends entries to span"     `Quick test_log_appends_to_span;
      test_case "log order preserved"             `Quick test_log_order_preserved;
      test_case "log after close raises"          `Quick test_log_after_close_raises;
      test_case "current_trace_ctx child of parent" `Quick test_current_trace_ctx_child_of_parent;
      test_case "root span has valid traceparent" `Quick test_with_span_no_parent_generates_root;
      test_case "log_t uses ?parent"               `Quick test_log_t_uses_parent;
    ];
    "metrics", [
      test_case "counter emits metric event"   `Quick test_counter_emits_event;
      test_case "gauge accepts negative value" `Quick test_gauge_accepts_negative_value;
      test_case "histogram emits metric event" `Quick test_histogram_emits_event;
      test_case "histogram rejects negative observation" `Quick test_histogram_rejects_negative_observation;
      test_case "counter and histogram helper emits both" `Quick test_counter_and_histogram_helper_emits_both;
      test_case "emit accepts declared labels in any order" `Quick test_emit_accepts_declared_labels_any_order;
      test_case "emit rejects missing extra duplicate labels" `Quick test_emit_rejects_missing_extra_duplicate_labels;
      test_case "metric name validation"       `Quick test_metric_name_validation;
      test_case "label name validation"        `Quick test_label_name_validation;
      test_case "register rejects invalid metric names" `Quick test_register_rejects_invalid_metric_name;
      test_case "register rejects invalid label names" `Quick test_register_rejects_invalid_label_name;
      test_case "register rejects duplicate label names" `Quick test_register_rejects_duplicate_label_names;
      test_case "register_counter rejects negative delta" `Quick test_register_counter_rejects_negative_delta;
      test_case "register_histogram rejects \"le\" label" `Quick test_register_histogram_rejects_le_label;
      test_case "register declares before first emit" `Quick test_register_declares_before_first_emit;
      test_case "noop backend runs silently"   `Quick test_noop_compiles_and_runs;
      test_case "stdout backend runs and prints" `Quick test_stdout_backend_runs_and_prints;
    ];
    "compose", [
      test_case "compose fans out to both backends" `Quick test_compose_fans_out;
      test_case "compose isolates a raising sibling backend" `Quick test_compose_isolates_a_raising_sibling;
    ];
    "backend_failure_isolation", [
      test_case "with_span survives a raising backend"        `Quick test_with_span_survives_raising_backend;
      test_case "register_counter survives a raising backend" `Quick test_register_counter_survives_raising_backend;
      test_case "register survives a raising declare_metric"  `Quick test_register_survives_raising_declare;
      test_case "with_span propagates Cancelled instead of swallowing it" `Quick test_with_span_propagates_cancelled;
      test_case "with_span logs an application exception lost to a racing Cancelled" `Quick test_with_span_logs_original_exception_lost_to_cancelled;
      test_case "compose propagates Cancelled and skips the sibling"      `Quick test_compose_propagates_cancelled_and_skips_sibling;
    ];
  ]
