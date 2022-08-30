let channel = "aeron:udp?endpoint=localhost:20121"
let stream_id = 1001l

open Aeroon.Bindings
open Ctypes
open C.Functions

let pr = Printf.printf
let u64_to_i = Unsigned.UInt64.to_int

(* keep global refrences to callbacks to avoid their GC; this seems to
      eliminate the warning in:

   https://github.com/ocamllabs/ocaml-ctypes/blob/9048ac78b885cc3debeeb020c56ea91f459a4d33/src/ctypes-foreign/ctypes_ffi.ml#L270
*)

(*
type callback = [
  | `EH of (unit -> int -> string -> unit)
  | `ONP of (unit -> unit -> string -> int32 -> int32 -> int64 -> unit)
  | `OAC of n_available_counte
  | `OUC of On_unavailable_counter.t
  | `ONS of On_new_subscription.t
  | `OC of On_close_client.t
]

let kept_alive : callback list ref = ref []
let keep_alive (a : callback) =
  kept_alive := a :: !kept_alive
 *)

let _ =
  let ctx =
    let p_context = Alloc.ptr_context () in
    let err = context_init p_context in
    assert (err = 0);
    !@p_context
  in

  let p_client = Alloc.ptr_client () in
  let err = init p_client ctx in
  assert (err = 0);
  let client = !@p_client in

  let err = start client in
  assert (err = 0);

  pr "dir=%s\n%!" (context_get_dir ctx);

  pr "timeout=%d\n%!" (u64_to_i (context_get_driver_timeout_ms ctx));

  pr "keepalive_ns=%d\n%!" (u64_to_i (context_get_keepalive_interval_ns ctx));

  pr "linger_duration_ns=%d\n%!"
    (u64_to_i (context_get_resource_linger_duration_ns ctx));

  pr "pre_touch_mappped_memory=%b\n%!" (context_get_pre_touch_mapped_memory ctx);

  pr "use_conductor_agent_invoker=%b\n%!"
    (context_get_use_conductor_agent_invoker ctx);

  let error_handler _ code msg =
    Printf.printf "error code=%d msg=%s\n%!" code msg
  in

  let _ = context_set_error_handler ctx error_handler null in

  (* keep_alive (`EH error_handler); *)
  let on_publication _ _ channel stream_id session_id correlation_id =
    Printf.printf
      "new publication channel=%s stream_id=%ld session_id=%ld \
       correlation_id=%Ld\n\
       %!"
      channel stream_id session_id correlation_id
  in

  let _ = context_set_on_new_publication ctx on_publication null in

  (* keep_alive (`ONP on_publication); *)
  let on_exclusive_publication _ _ channel stream_id session_id correlation_id =
    Printf.printf
      "new exclusive publication channel=%s stream_id=%ld session_id=%ld \
       correlation_id=%Ld\n\
       %!"
      channel stream_id session_id correlation_id
  in
  let _ =
    context_set_on_new_exclusive_publication ctx on_exclusive_publication null
  in

  (* keep_alive (`ONP on_exclusive_publication); *)
  let on_subscription _ _ channel stream_id correlation_id =
    Printf.printf
      "new subscription channel=%s stream_id=%ld correlation_id=%Ld\n%!" channel
      stream_id correlation_id
  in

  let _ = context_set_on_new_subscription ctx on_subscription null in

  (* keep_alive (`ONS on_subscription); *)
  let on_available_counter _ _ registration_id counter_id =
    Printf.printf "available counter registration_id=%Ld counter_id=%ld\n%!"
      registration_id counter_id
  in

  let _ = context_set_on_available_counter ctx on_available_counter null in

  (* keep_alive (`OAC on_available_counter); *)
  let on_unavailable_counter _ _ registration_id counter_id =
    Printf.printf "unavailable counter registration_id=%Ld counter_id=%ld\n%!"
      registration_id counter_id
  in

  let _ = context_set_on_unavailable_counter ctx on_unavailable_counter null in

  (* keep_alive (`OUC on_unavailable_counter); *)
  let on_close _ = Printf.printf "close client\n%!" in

  let _ = context_set_on_close_client ctx on_close null in

  (* keep_alive (`OC on_close); *)
  let async =
    let p_async = Alloc.ptr_async_add_publication () in
    let err = async_add_publication p_async client channel stream_id in
    assert (err = 0);
    !@p_async
  in

  let publication =
    let p_publication = Alloc.ptr_publication () in
    let rec poll () =
      match async_add_publication_poll p_publication async with
      | 1 -> !@p_publication
      | 0 -> poll ()
      | -1 -> failwith "failed async publication"
      | _ -> assert false
    in
    poll ()
  in

  let _ = publication_close publication None null in

  print_endline "close";
  let _ = close client in
  print_endline "context_close";
  let _ = context_close ctx in

  Unix.sleep 1;

  ()
