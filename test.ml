let uri = "aeron:udp?endpoint=localhost:20121"
let stream_id = 1001l

open Aeroon
open Ctypes

let pr = Printf.printf
let u64_to_i = Unsigned.UInt64.to_int

type callback = [
  | `EH of Error_Handler.t
  | `OAC of On_available_counter.t
  | `OUC of On_unavailable_counter.t
  | `ONP of On_new_publication.t
  | `ONS of On_new_subscription.t
  | `OC of On_close_client.t
]

(* keep global refrences to callbacks to avoid their GC; this seems to
   eliminate the warning in:

https://github.com/ocamllabs/ocaml-ctypes/blob/9048ac78b885cc3debeeb020c56ea91f459a4d33/src/ctypes-foreign/ctypes_ffi.ml#L270
 *)
let kept_alive : callback list ref = ref []
let keep_alive (a : callback) =
  kept_alive := a :: !kept_alive

let _ =
  let ctx =
    let p_context = alloc_ptr_context () in
    let err = context_init p_context in
    assert (err = 0);
    !@ p_context
  in

  let p_client = alloc_ptr_client () in
  let err = init p_client ctx in
  assert (err = 0);
  let client = !@ p_client in

  let err = start client in
  assert (err = 0);

  pr "dir=%s\n%!" (context_get_dir ctx);

  pr "timeout=%d\n%!"
    (u64_to_i (context_get_driver_timeout_ms ctx));

  pr "keepalive_ns=%d\n%!"
    (u64_to_i (context_get_keepalive_interval_ns ctx));

  pr "linger_duration_ns=%d\n%!"
    (u64_to_i (context_get_resource_linger_duration_ns ctx));

  pr "pre_touch_mappped_memory=%b\n%!"
    (context_get_pre_touch_mapped_memory ctx);

  pr "use_conductor_agent_invoker=%b\n%!"
    (context_get_use_conductor_agent_invoker ctx);

  let error_handler = Error_Handler.of_fun (
    fun _ code msg ->
      Printf.printf "error code=%d msg=%s\n%!" code msg
  ) in

  let _ = context_set_error_handler ctx error_handler null in
  keep_alive (`EH error_handler);

  let on_publication = On_new_publication.of_fun (
    fun _ _ channel stream_id session_id correlation_id ->
      Printf.printf "new publication \
                     channel=%s \
                     stream_id=%ld \
                     session_id=%ld \
                     correlation_id=%Ld\n%!"
        channel stream_id session_id correlation_id
  ) in

  let _ = context_set_on_new_publication ctx on_publication null in
  keep_alive (`ONP on_publication);

  let on_exclusive_publication = On_new_publication.of_fun (
    fun _ _ channel stream_id session_id correlation_id ->
      Printf.printf "new exclusive publication \
                     channel=%s \
                     stream_id=%ld \
                     session_id=%ld \
                     correlation_id=%Ld\n%!"
        channel stream_id session_id correlation_id
  ) in
  let _ = context_set_on_new_exclusive_publication ctx on_exclusive_publication null in
  keep_alive (`ONP on_exclusive_publication);

  let on_subscription = On_new_subscription.of_fun (
    fun _ _ channel stream_id correlation_id ->
      Printf.printf "new subscription \
                     channel=%s \
                     stream_id=%ld \
                     correlation_id=%Ld\n%!"
        channel stream_id correlation_id
  ) in

  let _ = context_set_on_new_subscription ctx on_subscription null in
  keep_alive (`ONS on_subscription);

  let pr_on_available_counter =
    fun _ _ registration_id counter_id ->
      Printf.printf "available counter \
                     registration_id=%Ld \
                     counter_id=%ld\n%!"
        registration_id counter_id
  in

  let on_available_counter =
    On_available_counter.of_fun pr_on_available_counter
  in

  let _ = context_set_on_available_counter ctx on_available_counter null in
  keep_alive (`OAC on_available_counter);

  let pr_unavailable_counter =
    fun _ _ registration_id counter_id ->
      Printf.printf "unavailable counter \
                     registration_id=%Ld \
                     counter_id=%ld\n%!"
        registration_id counter_id
  in

  let on_unavailable_counter =
    On_unavailable_counter.of_fun pr_unavailable_counter
  in

  let _ = context_set_on_unavailable_counter ctx on_unavailable_counter null in
  keep_alive (`OUC on_unavailable_counter);

  let on_close = On_close_client.of_fun (
    fun _ ->
      Printf.printf "close client\n%!"
  ) in

  let _ = context_set_on_close_client ctx on_close null in
  keep_alive (`OC on_close);

  let p_async_add_publication = alloc_ptr_client_registering_resource () in
  let err = async_add_publication p_async_add_publication client uri stream_id in
  assert (err = 0);

  let async_add_publication = !@ p_async_add_publication in
  let p_publication = alloc_ptr_publication () in

  let rec poll () =
    match async_add_publication_poll p_publication async_add_publication with
    | 1 -> !@ p_publication
    | 0 -> poll ()
    | -1 -> failwith "failed async publication"
    | _ -> assert false
  in
  let publication = poll () in

  Unix.sleep 100;

  let _ = publication_close publication None null in

  print_endline "close";
  let _ = close client in
  print_endline "context_close";
  let _ = context_close ctx in

  ()
