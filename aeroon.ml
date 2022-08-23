open Ctypes
open Foreign

type clientd = unit ptr
let clientd = ptr void

let abstract_1 name =
  abstract ~name ~size:1 ~alignment:1

let subscription : [`Subscription] abstract typ =
  abstract_1 "aeron_subscription_stct"

let image : [`Image ] abstract typ =
  abstract_1 "aeron_image_stct"

let dyfnp = dynamic_funptr ~runtime_lock:true ~thread_registration:true

module On_available_image =
  (val (
     dyfnp (
       clientd                (* clientd      *)
       @-> (ptr subscription) (* subscription *)
       @-> (ptr image)        (* image        *)
       @-> returning void
     )
   )
  )

module On_unavailable_image = On_available_image

let counters_reader : [`Counters_reader] abstract typ =
  abstract_1 "aeron_counters_reader_stct"

let context : [`Context] abstract typ =
  abstract ~name:"aeron_context_stct" ~size:1 ~alignment:1

let context_get_dir =
  foreign "aeron_context_get_dir"
    ((ptr context) @-> returning string)

(* Note: [set_dir new_dir] seems to return [None] on success *)
let context_set_dir =
  foreign "aeron_context_set_dir"
    ((ptr context) @-> string @-> returning string_opt)

(* Note: [set_driver_timeout_msg timeout] seems to return a
   meaningless error code (always 0, even for negative values) *)
let context_set_driver_timeout_ms =
  foreign "aeron_context_set_driver_timeout_ms"
    ((ptr context) @-> uint64_t @-> returning int)

let context_get_driver_timeout_ms =
  foreign "aeron_context_get_driver_timeout_ms"
    ((ptr context) @-> returning uint64_t)

let context_set_keepalive_interval_ns =
  foreign "aeron_context_set_keepalive_interval_ns"
    ((ptr context) @-> uint64_t @-> returning int)

let context_get_keepalive_interval_ns =
  foreign "aeron_context_get_keepalive_interval_ns"
    ((ptr context) @-> returning uint64_t)

let context_set_resource_linger_duration_ns =
  foreign "aeron_context_set_resource_linger_duration_ns"
    ((ptr context) @-> uint64_t @-> returning int)

let context_get_resource_linger_duration_ns =
  foreign "aeron_context_get_resource_linger_duration_ns"
    ((ptr context) @-> returning uint64_t)

let context_set_pre_touch_mapped_memory =
  foreign "aeron_context_set_pre_touch_mapped_memory"
    ((ptr context) @-> bool @-> returning int)

let context_get_pre_touch_mapped_memory =
  foreign "aeron_context_get_pre_touch_mapped_memory"
    ((ptr context) @-> returning bool)

module Error_Handler =
  (val (
     dyfnp (
       clientd
       @-> int
       @-> string
       @-> returning void
     )
   )
  )

let context_set_error_handler =
  foreign "aeron_context_set_error_handler"
    ((ptr context) @-> Error_Handler.t @-> clientd @-> returning int)

let client_registering_resource : [`Registering_resource] abstract typ =
  abstract_1 "aeron_client_registering_resource_stct"

module On_new_pubsub =
  (val (
     dyfnp (
       clientd                               (* clientd        *)
       @-> (ptr client_registering_resource) (* async          *)
       @-> string                            (* channel        *)
       @-> int32_t                           (* stream_id      *)
       @-> int64_t                           (* correlation_id *)
       @-> returning void
     )
   )
  )

let context_set_on_new_publication =
  foreign "aeron_context_set_on_new_publication"
    ((ptr context) @-> On_new_pubsub.t @-> clientd @-> returning int)

let context_set_on_new_exclusive_publication =
  foreign "aeron_context_set_on_new_exclusive_publication"
    ((ptr context) @-> On_new_pubsub.t @-> clientd @-> returning int)

let context_set_on_new_subscription =
  foreign "aeron_context_set_on_new_subscription"
    ((ptr context) @-> On_new_pubsub.t @-> clientd @-> returning int)

module On_available_counter =
  (val (
     dyfnp (
       clientd                   (* clientd         *)
       @-> (ptr counters_reader) (* counters_reader *)
       @-> int64_t               (* registration_id *)
       @-> int32_t               (* counter_id      *)
       @-> returning void

     )
   )
  )

module On_unavailable_counter = On_available_counter

let context_set_on_available_counter =
  foreign "aeron_context_set_on_available_counter"
    ((ptr context) @-> On_available_counter.t @-> clientd @-> returning int)

let context_set_on_unavailable_counter =
  foreign "aeron_context_set_on_unavailable_counter"
    ((ptr context) @-> On_unavailable_counter.t @-> clientd @-> returning int)

module On_close_client =
  (val (
     dyfnp (
       clientd
       @-> returning void
     )
   )
  )

let context_set_on_close_client =
  foreign "aeron_context_set_on_close_client"
    ((ptr context) @-> On_close_client.t @-> clientd @-> returning int)

let context_set_use_conductor_agent_invoker =
  foreign "aeron_context_set_use_conductor_agent_invoker"
    ((ptr context) @-> bool @-> returning int)

let context_get_use_conductor_agent_invoker =
  foreign "aeron_context_get_use_conductor_agent_invoker"
    ((ptr context) @-> returning bool)

let context_init =
  foreign "aeron_context_init"
    (ptr (ptr context) @-> returning int)

let context_close =
  foreign "aeron_context_close"
    ((ptr context) @-> returning int)

let client : [`Client] abstract typ =
  abstract ~name:"aeron_stct" ~size:1 ~alignment:1

let init =
  foreign "aeron_init"
    ((ptr (ptr client)) @-> (ptr context) @-> returning int)

let start =
  foreign "aeron_start"
    ((ptr client) @-> returning int)

let main_do_work =
  foreign "aeron_main_do_work"
    ((ptr client) @-> returning int)

let main_idle_strategy =
  foreign "aeron_main_idle_strategy"
    ((ptr client) @-> int @-> returning void)

let close =
  foreign "aeron_close"
    ((ptr client) @-> returning int)

let is_closed =
  foreign "aeron_is_closed"
    ((ptr client) @-> returning bool)

module Stream_out =
  (val (
     dyfnp (
       string
       @-> returning void
     )
   )
  )

let print_counters =
  foreign "aeron_print_counters"
    ((ptr client) @-> Stream_out.t @-> returning void)

let get_context =
  foreign "aeron_context"
    ((ptr client) @-> returning (ptr context))

let client_id =
  foreign "aeron_client_id"
    ((ptr client) @-> returning int64_t)

let next_correlation_id =
  foreign "aeron_next_correlation_id"
    ((ptr client) @-> returning int64_t)

let async_add_publication =
  foreign "aeron_async_add_publication"
    ((ptr (ptr client_registering_resource))
     @-> (ptr client)
     @-> string
     @-> int32_t
     @-> returning int
    )

let publication : [`Publication] abstract typ =
  abstract_1 "aeron_publication_stct"

let async_add_publication_poll =
  foreign "aeron_async_add_publication_poll"
    ((ptr (ptr publication))
     @-> (ptr client_registering_resource)
     @-> returning int
   )

let errcode =
  foreign "aeron_errcode"
    (void @-> returning int)

let errmsg =
  foreign "aeron_errmsg"
    (void @-> returning string)

let alloc_ptr_context () =
  allocate (ptr context) (from_voidp context null)

let alloc_ptr_client () =
  allocate (ptr client) (from_voidp client null)

let alloc_ptr_client_registering_resource () =
  allocate (ptr client_registering_resource)
    (from_voidp client_registering_resource null)

let alloc_ptr_publication () =
  allocate (ptr publication) (from_voidp publication null)

let pr = Printf.printf
let u64_to_i = Unsigned.UInt64.to_int

type callback = [
  | `EH of Error_Handler.t
  | `OAC of On_available_counter.t
  | `OUC of On_unavailable_counter.t
  | `OPS of On_new_pubsub.t
  | `OC of On_close_client.t
]

let kept_alive = ref []
let keep_alive (a : callback) =
  kept_alive := a :: !kept_alive

let _ =
  let wh = Hashtbl.create 100 in
  ignore wh;

  let ctx =
    let p_context = alloc_ptr_context () in
    let err = context_init p_context in
    assert (err = 0); (* otherwise (-1) -> failed or (_) -> assert false *)
    !@ p_context
  in

  pr "dir=%s\n" (context_get_dir ctx);

  pr "timeout=%d\n"
    (u64_to_i (context_get_driver_timeout_ms ctx));

  pr "keepalive_ns=%d\n"
    (u64_to_i (context_get_keepalive_interval_ns ctx));

  pr "linger_duration_ns=%d\n"
    (u64_to_i (context_get_resource_linger_duration_ns ctx));

  pr "pre_touch_mappped_memory=%b\n"
    (context_get_pre_touch_mapped_memory ctx);

  let error_handler = Error_Handler.of_fun (
    fun _ code msg ->
      Printf.printf "error code=%d msg=%s\n%!" code msg
  ) in

  let _ = context_set_error_handler ctx error_handler null in
  keep_alive (`EH error_handler);

  let on_publication = On_new_pubsub.of_fun (
    fun _ _ channel stream_id correlation_id ->
      Printf.printf "new publication \
                     channel=%s \
                     stream_id=%ld \
                     correlation_id=%Ld\n%!"
        channel stream_id correlation_id
  ) in

  let _ = context_set_on_new_publication ctx on_publication null in
  keep_alive (`OPS on_publication);

  let on_exclusive_publication = On_new_pubsub.of_fun (
    fun _ _ channel stream_id correlation_id ->
      Printf.printf "new exclusive publication \
                     channel=%s \
                     stream_id=%ld \
                     correlation_id=%Ld\n%!"
        channel stream_id correlation_id
  ) in
  let _ = context_set_on_new_exclusive_publication ctx on_exclusive_publication null in
  keep_alive (`OPS on_exclusive_publication);

  let on_subscription = On_new_pubsub.of_fun (
    fun _ _ channel stream_id correlation_id ->
      Printf.printf "new subscription \
                     channel=%s \
                     stream_id=%ld \
                     correlation_id=%Ld\n%!"
        channel stream_id correlation_id
  ) in

  let _ = context_set_on_new_subscription ctx on_subscription null in
  keep_alive (`OPS on_subscription);

  let on_available_counter =
    On_available_counter.of_fun (
      fun _ _ registration_id counter_id ->
        Printf.printf "available counter \
                       registration_id=%Ld \
                       counter_id=%ld\n%!"
          registration_id counter_id
    )
  in

  let _ = context_set_on_available_counter ctx on_available_counter null in
  keep_alive (`OAC on_available_counter);

  let on_unavailable_counter =
    On_unavailable_counter.of_fun (
      fun _ _ registration_id counter_id ->
        Printf.printf "unavailable counter \
                       registration_id=%Ld \
                       counter_id=%ld\n%!"
          registration_id counter_id
    )
  in

  let _ = context_set_on_unavailable_counter ctx on_unavailable_counter null in
  keep_alive (`OUC on_unavailable_counter);

  let on_close = On_close_client.of_fun (
    fun _ ->
      Printf.printf "close client\n%!"
  ) in

  let _ = context_set_on_close_client ctx on_close null in
  keep_alive (`OC on_close);

  Printf.printf "use_conductor_agent_invoker=%b\n"
    (context_get_use_conductor_agent_invoker ctx);

  let p_client = alloc_ptr_client () in
  let err = init p_client ctx in
  assert (err = 0);
  let client = !@ p_client in

  let err = start client in
  assert (err = 0);

  let p_async_add_publication = alloc_ptr_client_registering_resource () in
  let url = "aeron:udp?endpoint=localhost:20121" in
  let err = async_add_publication p_async_add_publication client url 1001l in
  assert (err = 0);
  let async_add_publication = !@ p_async_add_publication in
  let p_publication = alloc_ptr_publication () in

  let rec wait () =
    let err = async_add_publication_poll p_publication async_add_publication in
    pr "pub=%d\n%!" err;
    match err with
    | 0 -> wait ()
    | 1 -> ()
    | -1 -> failwith "failed async publication"
    | _ -> assert false
  in
  wait ();

  let _ = close client in
  let _ = context_close ctx in

  ()
