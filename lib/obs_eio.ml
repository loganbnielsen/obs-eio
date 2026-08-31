(* ------------------------------------------------------------------ *)
(* Types                                                               *)
(* ------------------------------------------------------------------ *)

type counter_fn   = ?labels:(string * string) list -> int   -> unit
type gauge_fn     = ?labels:(string * string) list -> float -> unit
type histogram_fn = ?labels:(string * string) list -> float -> unit

type level = Debug | Info | Warn | Error

type log_entry = {
  timestamp_ns : int64;
  level   : level;
  message : string;
  fields  : (string * string) list;
}

type span_event = {
  trace_ctx : Obs_trace.t;
  name      : string;
  service   : string;
  start_ns  : int64;
  end_ns    : int64;
  status    : [ `Ok | `Error of string ];
  log_entries : log_entry list;
  context   : (string * string) list;
}

type metric_event = {
  name    : string;
  help    : string;
  kind    : [ `Counter of int | `Gauge of float | `Histogram of float ];
  labels  : (string * string) list;
  context : (string * string) list;
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

type backend = {
  emit_span      : span_event   -> unit;
  emit_metric    : metric_event -> unit;
  declare_metric : metric_declaration  -> unit;
}

type backend_op =
  | Emit_span of { name : string }
  | Emit_metric of { name : string; kind : metric_kind }
  | Declare_metric of { name : string; kind : metric_kind }

type label_name = string

(* ------------------------------------------------------------------ *)
(* Handle and span                                                     *)
(* ------------------------------------------------------------------ *)

type t = {
  service             : string;
  get_time            : unit -> Mtime.t;  (* closure over mono_clock *)
  backend             : backend;
  context             : (string * string) list;
  on_backend_error    : backend_op -> exn -> unit;
  metric_declarations : (string, metric_declaration) Hashtbl.t;
  metric_mutex        : Mutex.t;
}

type span = {
  sp_ctx     : Obs_trace.t;
  (* Accumulates log entries in reverse call order. *)
  sp_log_buf : log_entry list ref;
  (* Set once [with_span]'s callback returns; [log] rejects a closed span
     rather than silently buffering into an entry nothing will ever read. *)
  sp_closed  : bool ref;
  (* Same monotonic clock as start_ns/end_ns, captured from [t] at
     [with_span] time so [log] can stamp each entry individually instead of
     every entry in a span sharing one timestamp. *)
  sp_now     : unit -> int64;
}

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let now_ns t = Mtime.to_uint64_ns (t.get_time ())

let level_string = function
  | Debug -> "debug" | Info -> "info" | Warn -> "warn" | Error -> "error"

let log_entry_fields entry =
  [("log.level", level_string entry.level); ("log.msg", entry.message)]
  @ entry.fields

let log_entries_fields entries =
  List.concat_map log_entry_fields entries

(* ------------------------------------------------------------------ *)
(* Built-in backends                                                   *)
(* ------------------------------------------------------------------ *)

let noop = {
  emit_span      = (fun _ -> ());
  emit_metric    = (fun _ -> ());
  declare_metric = (fun _ -> ());
}

let pp_kv pairs =
  String.concat " " (List.map (fun (k, v) -> k ^ "=" ^ String.escaped v) pairs)

let stdout =
  let pp_trace ctx =
    let (hi, lo) = ctx.Obs_trace.trace_id in
    Printf.sprintf "trace=%016Lx%016Lx span=%016Lx" hi lo ctx.Obs_trace.span_id
  in
  { emit_span = (fun e ->
      let dur_ms = Int64.(to_float (sub e.end_ns e.start_ns)) /. 1e6 in
      let status = match e.status with `Ok -> "ok" | `Error s -> "error:" ^ s in
      let fields = log_entries_fields e.log_entries in
      Printf.printf "SPAN  svc=%s name=%s %s status=%s dur=%.2fms%s\n%!"
        e.service e.name (pp_trace e.trace_ctx) status dur_ms
        (if fields = [] then "" else " | " ^ pp_kv fields));
    emit_metric = (fun e ->
      let kind = match e.kind with
        | `Counter n   -> Printf.sprintf "counter=%d" n
        | `Gauge f     -> Printf.sprintf "gauge=%g" f
        | `Histogram f -> Printf.sprintf "hist=%g" f
      in
      Printf.printf "METRIC svc=%s name=%s %s%s%s\n%!"
        e.service e.name kind
        (if e.labels  = [] then "" else " labels={" ^ pp_kv e.labels ^ "}")
        (if e.context = [] then "" else " ctx={"    ^ pp_kv e.context ^ "}"));
    declare_metric = (fun _ -> ());
  }

let string_of_metric_kind = function
  | `Counter -> "counter"
  | `Gauge -> "gauge"
  | `Histogram -> "histogram"

let string_of_backend_op = function
  | Emit_span { name } -> Printf.sprintf "emit_span name=%S" name
  | Emit_metric { name; kind } ->
    Printf.sprintf "emit_metric name=%S kind=%s" name (string_of_metric_kind kind)
  | Declare_metric { name; kind } ->
    Printf.sprintf "declare_metric name=%S kind=%s" name (string_of_metric_kind kind)

let default_backend_error op exn =
  Printf.eprintf "Obs_eio: backend_error op=%s exn=%S\n%!"
    (string_of_backend_op op) (Printexc.to_string exn)

let report_backend_error t op exn =
  try t.on_backend_error op exn with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
  | handler_exn ->
    Printf.eprintf
      "Obs_eio: backend_error_handler_raised op=%s handler_exn=%S original_exn=%S\n%!"
      (string_of_backend_op op)
      (Printexc.to_string handler_exn)
      (Printexc.to_string exn)

(* Eio cancellation is control flow owned by the caller's switch, not a
   backend failure. Keep it propagating through both catch layers. *)
let safe_call ~on_error f =
  try f () with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
  | exn -> on_error exn

let compose_call first_error f =
  try f () with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | (Out_of_memory | Stack_overflow | Sys.Break) as exn -> raise exn
  | exn ->
    if !first_error = None then first_error := Some exn

let compose a b = {
  emit_span      = (fun e ->
    let first_error = ref None in
    compose_call first_error (fun () -> a.emit_span e);
    compose_call first_error (fun () -> b.emit_span e);
    match !first_error with Some exn -> raise exn | None -> ());
  emit_metric    = (fun e ->
    let first_error = ref None in
    compose_call first_error (fun () -> a.emit_metric e);
    compose_call first_error (fun () -> b.emit_metric e);
    match !first_error with Some exn -> raise exn | None -> ());
  declare_metric = (fun d ->
    let first_error = ref None in
    compose_call first_error (fun () -> a.declare_metric d);
    compose_call first_error (fun () -> b.declare_metric d);
    match !first_error with Some exn -> raise exn | None -> ());
}

(* ------------------------------------------------------------------ *)
(* Create                                                              *)
(* ------------------------------------------------------------------ *)

let create ~service ~mono_clock ?(on_backend_error = default_backend_error) ~backend () = {
  service;
  get_time = (fun () -> Eio.Time.Mono.now mono_clock);
  backend;
  context = [];
  on_backend_error;
  metric_declarations = Hashtbl.create 8;
  metric_mutex = Mutex.create ();
}

(* ------------------------------------------------------------------ *)
(* Context                                                             *)
(* ------------------------------------------------------------------ *)

let with_context t extra =
  (* Merge: new keys override existing; unlisted keys are preserved. *)
  let merged = List.fold_left (fun acc (k, v) ->
    (k, v) :: List.filter (fun (k2, _) -> k2 <> k) acc
  ) t.context extra in
  { t with context = merged }

(* ------------------------------------------------------------------ *)
(* Spans                                                               *)
(* ------------------------------------------------------------------ *)

let with_span t ?parent name f =
  let sp_ctx = match parent with
    | None   -> Obs_trace.generate ()
    | Some p -> Obs_trace.child_span p
  in
  let sp_start = now_ns t in
  let sp = { sp_ctx; sp_log_buf = ref []; sp_closed = ref false; sp_now = (fun () -> now_ns t) } in
  (* Not Fun.protect: its ~finally wraps any raised exception, including
     Cancelled, in Fun.Finally_raised, which would defeat safe_call's
     Cancelled re-raise. This match still emits on every exit path. *)
  let emit status =
    let end_ns = now_ns t in
    let log_entries = List.rev !(sp.sp_log_buf) in
    sp.sp_closed := true;
    safe_call
      ~on_error:(report_backend_error t (Emit_span { name }))
      (fun () ->
      t.backend.emit_span {
        trace_ctx = sp_ctx; name; service = t.service;
        start_ns = sp_start; end_ns;
        status;
        log_entries;
        context = t.context;
      })
  in
  match f sp with
  | v -> emit `Ok; v
  | exception exn ->
    let msg = Printexc.to_string exn in
    (* emit can itself raise only Cancelled (see safe_call); if it does,
       log the original [exn] before letting Cancelled win so it isn't
       silently lost. Skip logging when [exn] is itself Cancelled — that's
       an ordinary double-cancellation, not a masked application bug. *)
    (match emit (`Error msg) with
     | ()               -> raise exn
     | exception emit_exn ->
       (match exn with
        | Eio.Cancel.Cancelled _ -> ()
        | _ ->
          Printf.eprintf
            "Obs_eio: with_span %S: application exception %s was about to \
             close the span, but emit_span itself raised %s first\n%!"
            name msg (Printexc.to_string emit_exn));
       raise emit_exn)

let log sp level ?(fields = []) message =
  if !(sp.sp_closed) then
    invalid_arg
      "Obs_eio.log: span is already closed — a span value must not be used, or \
       escape to another fiber/domain, after its with_span callback returns";
  let timestamp_ns = sp.sp_now () in
  sp.sp_log_buf := { timestamp_ns; level; message; fields } :: !(sp.sp_log_buf)

let log_standalone t ?parent level ?(fields = []) message =
  with_span t ?parent "log" (fun sp -> log sp level ~fields message)

let current_trace_context sp = sp.sp_ctx

(* ------------------------------------------------------------------ *)
(* Metrics                                                             *)
(* ------------------------------------------------------------------ *)

let is_metric_initial_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | ':' -> true
  | _ -> false

let is_label_initial_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_metric_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | ':' -> true
  | _ -> false

let is_label_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

let validate_name ~kind ~is_initial ~is_char name =
  if name = "" then
    invalid_arg ("Obs_eio." ^ kind ^ ": name must not be empty");
  if not (is_initial name.[0]) then
    invalid_arg
      (Printf.sprintf "Obs_eio.%s: invalid Prometheus name %S" kind name);
  String.iter (fun c ->
    if not (is_char c) then
      invalid_arg
        (Printf.sprintf "Obs_eio.%s: invalid Prometheus name %S" kind name)
  ) name;
  name

let metric_name name =
  validate_name ~kind:"metric_name"
    ~is_initial:is_metric_initial_char
    ~is_char:is_metric_char
    name

let label_name_exn name =
  let name =
    validate_name ~kind:"label_name"
      ~is_initial:is_label_initial_char
      ~is_char:is_label_char
      name
  in
  if String.length name >= 2 && name.[0] = '_' && name.[1] = '_' then
    invalid_arg
      (Printf.sprintf
         "Obs_eio.label_name: %S starts with \"__\", reserved for Prometheus's \
          own internal use (e.g. __name__)" name);
  name

let label_name name =
  try Ok (label_name_exn name) with Invalid_argument msg -> Error msg

let label_name_to_string name = name

let duplicate_name names =
  let rec loop seen = function
    | [] -> None
    | name :: rest ->
      if List.mem name seen then Some name else loop (name :: seen) rest
  in
  loop [] names

let validate_label_names label_names =
  List.iter (fun name -> ignore (label_name_exn name)) label_names;
  (match duplicate_name label_names with
   | None -> ()
   | Some name ->
     invalid_arg
       (Printf.sprintf "Obs_eio.register_metric: duplicate label name %S" name));
  label_names

let validate_metric_labels ~name ~label_names labels =
  let emitted_label_names = List.map fst labels in
  match duplicate_name emitted_label_names with
  | Some label ->
    invalid_arg
      (Printf.sprintf "Obs_eio.emit_metric %S: duplicate label %S" name label)
  | None ->
    let missing =
      List.filter (fun label -> not (List.mem_assoc label labels)) label_names
    in
    let extra =
      List.filter
        (fun label -> not (List.mem label label_names))
        emitted_label_names
    in
    match missing, extra with
    | [], [] -> ()
    | label :: _, _ ->
      invalid_arg
        (Printf.sprintf "Obs_eio.emit_metric %S: missing label %S" name label)
    | [], label :: _ ->
      invalid_arg
        (Printf.sprintf "Obs_eio.emit_metric %S: extra label %S" name label)

let metric_event_kind (event : metric_event) =
  match event.kind with
  | `Counter _ -> `Counter
  | `Gauge _ -> `Gauge
  | `Histogram _ -> `Histogram

let emit_metric t event =
  let kind = metric_event_kind event in
  safe_call
    ~on_error:(report_backend_error t (Emit_metric { name = event.name; kind }))
    (fun () -> t.backend.emit_metric event)

let sorted_label_names names = List.sort String.compare names

let validate_declaration t (d : metric_declaration) =
  Mutex.lock t.metric_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.metric_mutex) (fun () ->
    match Hashtbl.find_opt t.metric_declarations d.declaration_name with
    | None ->
      Hashtbl.add t.metric_declarations d.declaration_name
        { d with declaration_label_names = sorted_label_names d.declaration_label_names }
    | Some existing ->
      let label_names = sorted_label_names d.declaration_label_names in
      if existing.declaration_kind <> d.declaration_kind then
        invalid_arg
          (Printf.sprintf
             "Obs_eio.register_metric %S: already registered as %s, got %s"
             d.declaration_name
             (string_of_metric_kind existing.declaration_kind)
             (string_of_metric_kind d.declaration_kind));
      if existing.declaration_label_names <> label_names then
        invalid_arg
          (Printf.sprintf
             "Obs_eio.register_metric %S: conflicting label names"
             d.declaration_name);
      if existing.declaration_help <> d.declaration_help then
        invalid_arg
          (Printf.sprintf
             "Obs_eio.register_metric %S: conflicting help text"
             d.declaration_name))

let declare_metric t ~name ~help ~kind ~label_names =
  let declaration =
    { declaration_name = name; declaration_help = help; declaration_kind = kind;
      declaration_label_names = label_names; declaration_service = t.service }
  in
  validate_declaration t declaration;
  safe_call
    ~on_error:(report_backend_error t (Declare_metric { name; kind }))
    (fun () -> t.backend.declare_metric declaration)

let register_counter t ~name ~help ~label_names : counter_fn =
  let name = metric_name name in
  let label_names = validate_label_names label_names in
  declare_metric t ~name ~help ~kind:`Counter ~label_names;
  fun ?(labels = []) value ->
    validate_metric_labels ~name ~label_names labels;
    if value < 0 then
      invalid_arg
        (Printf.sprintf "Obs_eio.emit_metric %S: counter delta must be >= 0, got %d" name value);
    emit_metric t {
      name; help; kind = `Counter value; labels; context = t.context; service = t.service;
    }

let register_gauge t ~name ~help ~label_names : gauge_fn =
  let name = metric_name name in
  let label_names = validate_label_names label_names in
  declare_metric t ~name ~help ~kind:`Gauge ~label_names;
  fun ?(labels = []) value ->
    validate_metric_labels ~name ~label_names labels;
    emit_metric t {
      name; help; kind = `Gauge value; labels; context = t.context; service = t.service;
    }

let register_histogram t ~name ~help ~label_names : histogram_fn =
  let name = metric_name name in
  if List.mem "le" label_names then
    invalid_arg
      (Printf.sprintf
         "Obs_eio.register_histogram %S: label name \"le\" is reserved for the \
          Prometheus bucket boundary" name);
  let label_names = validate_label_names label_names in
  declare_metric t ~name ~help ~kind:`Histogram ~label_names;
  fun ?(labels = []) value ->
    validate_metric_labels ~name ~label_names labels;
    if value < 0.0 then
      invalid_arg
        (Printf.sprintf "Obs_eio.emit_metric %S: histogram observation must be >= 0, got %g" name value);
    emit_metric t {
      name; help; kind = `Histogram value; labels; context = t.context; service = t.service;
    }

let register_counter_and_histogram t
    ~counter_name ~counter_help ~counter_labels
    ~histogram_name ~histogram_help ~histogram_labels =
  let counter =
    register_counter t
      ~name:counter_name
      ~help:counter_help
      ~label_names:counter_labels
  in
  let histogram =
    register_histogram t
      ~name:histogram_name
      ~help:histogram_help
      ~label_names:histogram_labels
  in
  (counter, histogram)
