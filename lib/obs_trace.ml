type t = {
  trace_id    : int64 * int64;
  span_id     : int64;
  trace_flags : char;
}

(* Self-seeded so trace/span IDs don't collide across process restarts —
   unlike the global Random module, this needs no Random.self_init () call
   from the caller (easy to forget, and silently falls back to a fixed seed).
   [bits64] (not [int64 rng_state Int64.max_int]) so every bit is uniformly
   random — [int64 _ Int64.max_int] can never set the top bit, silently
   halving the ID space. Mutex-protected: Random.State.t mutation is not
   domain-safe, and this state is process-wide, reachable from any domain
   that calls [generate]/[child_span]. *)
let rng_state = Random.State.make_self_init ()
let rng_mutex = Mutex.create ()

let random_int64 () =
  Mutex.lock rng_mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock rng_mutex)
    (fun () -> Random.State.bits64 rng_state)

let generate () = {
  trace_id    = (random_int64 (), random_int64 ());
  span_id     = random_int64 ();
  trace_flags = '\x01';  (* sampled *)
}

let child_span t = { t with span_id = random_int64 () }

let traceparent_header = "traceparent"

let header_name_equal a b =
  String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)

let assoc_header_opt name headers =
  List.find_map
    (fun (k, v) -> if header_name_equal k name then Some v else None)
    headers

let to_traceparent t =
  let (hi, lo) = t.trace_id in
  Printf.sprintf "00-%016Lx%016Lx-%016Lx-%02x"
    hi lo t.span_id (Char.code t.trace_flags)

let of_traceparent s =
  match String.split_on_char '-' s with
  (* W3C forward compatibility: version "00" must be exactly these 4 fields
     (extra fields are malformed for the current version); any other
     version (except the reserved-invalid "ff") may have additional
     trailing fields for a future version's own use, which are ignored —
     trace-id/parent-id/flags still live at these fixed positions. *)
  | version :: trace_hex :: span_hex :: flags_hex :: rest
    when version <> "ff"
      && String.length version = 2
      && String.length trace_hex = 32
      && String.length span_hex  = 16
      && String.length flags_hex = 2
      && (version <> "00" || rest = []) ->
    (try
       let _  = int_of_string ("0x" ^ version) in
       let hi = Int64.of_string ("0x" ^ String.sub trace_hex  0 16) in
       let lo = Int64.of_string ("0x" ^ String.sub trace_hex 16 16) in
       let si = Int64.of_string ("0x" ^ span_hex) in
       let fl = Char.chr (int_of_string ("0x" ^ flags_hex)) in
       Some { trace_id = (hi, lo); span_id = si; trace_flags = fl }
     with _ -> None)
  | _ -> None

let extract_from_headers headers =
  match assoc_header_opt traceparent_header headers with
  | None   -> None
  | Some v -> of_traceparent v

let inject_to_headers ctx headers =
  let tp      = to_traceparent ctx in
  let without =
    List.filter (fun (k, _) -> not (header_name_equal k traceparent_header)) headers
  in
  (traceparent_header, tp) :: without
