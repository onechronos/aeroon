open Ctypes
open Foreign

type clientd = unit ptr
let clientd = ptr void

let abstract_1 name =
  abstract ~name ~size:1 ~alignment:1

let dyfnp = dynamic_funptr (* ~runtime_lock:true *) ~thread_registration:true

let subscription : [`Subscription] abstract typ =
  abstract_1 "aeron_subscription_stct"

let image : [`Image ] abstract typ =
  abstract_1 "aeron_image_stct"

module On_available_image =
  (val (dyfnp (
     clientd                (* clientd      *)
     @-> (ptr subscription) (* subscription *)
     @-> (ptr image)        (* image        *)
     @-> returning void
   )))

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
  (val (dyfnp (
     clientd
     @-> int
     @-> string
     @-> returning void
   )))

let context_set_error_handler =
  foreign "aeron_context_set_error_handler"
    ((ptr context) @-> Error_Handler.t @-> clientd @-> returning int)

let client_registering_resource : [`Registering_resource] abstract typ =
  abstract_1 "aeron_client_registering_resource_stct"

module On_new_publication =
  (val (dyfnp (
     clientd                               (* clientd        *)
     @-> (ptr client_registering_resource) (* async          *)
     @-> string                            (* channel        *)
     @-> int32_t                           (* stream_id      *)
     @-> int32_t                           (* session_id     *)
     @-> int64_t                           (* correlation_id *)
     @-> returning void
   )))


module On_new_subscription =
  (val (dyfnp (
     clientd                               (* clientd        *)
     @-> (ptr client_registering_resource) (* async          *)
     @-> string                            (* channel        *)
     @-> int32_t                           (* stream_id      *)
     @-> int64_t                           (* correlation_id *)
     @-> returning void
   )))

let context_set_on_new_publication =
  foreign "aeron_context_set_on_new_publication"
    ((ptr context) @-> On_new_publication.t @-> clientd @-> returning int)

let context_set_on_new_exclusive_publication =
  foreign "aeron_context_set_on_new_exclusive_publication"
    ((ptr context) @-> On_new_publication.t @-> clientd @-> returning int)

let context_set_on_new_subscription =
  foreign "aeron_context_set_on_new_subscription"
    ((ptr context) @-> On_new_subscription.t @-> clientd @-> returning int)

module On_available_counter =
  (val (dyfnp (
     clientd                   (* clientd         *)
     @-> (ptr counters_reader) (* counters_reader *)
     @-> int64_t               (* registration_id *)
     @-> int32_t               (* counter_id      *)
     @-> returning void
   )))

module On_unavailable_counter = On_available_counter

let context_set_on_available_counter =
  foreign "aeron_context_set_on_available_counter"
    ((ptr context) @-> On_available_counter.t @-> clientd @-> returning int)

let context_set_on_unavailable_counter =
  foreign "aeron_context_set_on_unavailable_counter"
    ((ptr context) @-> On_unavailable_counter.t @-> clientd @-> returning int)

module On_close_client =
  (val (dyfnp (
     clientd
     @-> returning void
   )))

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
  (val (dyfnp (
     string
     @-> returning void
   )))

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

module Notification =
  (val (dyfnp (
     clientd (* clientd *)
     @-> returning void
   )))

let publication_close =
  foreign "aeron_publication_close"
    ((ptr publication)
    @-> Notification.t_opt
    @-> clientd
    @-> returning int
   )

let async_add_exclusive_publication =
  foreign "aeron_async_add_exclusive_publication"
    ((ptr (ptr client_registering_resource))
     @-> (ptr client)
     @-> string
     @-> int32_t
     @-> returning int
    )

let exclusive_publication : [`Exclusive_publication] abstract typ =
  abstract_1 "aeron_exclusive_publication_stct"

let async_add_exclusive_publication_poll =
  foreign "aeron_async_add_exclusive_publication_poll"
    ((ptr (ptr exclusive_publication))
     @-> (ptr client_registering_resource)
     @-> returning int
   )

let async_add_subscription =
  foreign "aeron_async_add_subscription"
    ((ptr (ptr client_registering_resource)) (* async                        *)
    @-> (ptr client)                         (* client                       *)
    @-> string                               (* uri                          *)
    @-> int32_t                              (* stream_id                    *)
    @-> On_available_image.t_opt             (* on_available_image_handler   *)
    @-> clientd                              (* on_available_image_clientd   *)
    @-> On_unavailable_image.t_opt           (* on_unavailable_image_handler *)
    @-> clientd                              (* on_available_image_clientd   *)
    @-> returning int
   )

let subscription : [`Subscription] abstract typ =
  abstract_1 "aeron_subscription_stct"

let async_add_subscription_poll =
  foreign "aeron_async_add_subscription_poll"
    ((ptr (ptr subscription))
     @-> (ptr client_registering_resource)
     @-> returning int
   )

let counters_reader =
  foreign "aeron_counters_reader"
    ((ptr client) @-> returning (ptr counters_reader))

let async_add_counter =
  foreign "aeron_async_add_counter"
    (ptr (ptr client_registering_resource)
      @-> (ptr client)  (* client              *)
      @-> int32_t       (* type_id             *)
      @-> string        (* key_buffer          *)
      @-> size_t        (* key_buffer_length   *)
      @-> string        (* label_buffer        *)
      @-> size_t        (* label_buffer_length *)
      @-> returning int
    )

let counter : [`Counter] abstract typ =
  abstract_1 "aeron_counter_stct"

let async_add_counter_poll =
  foreign "aeron_async_add_counter_poll"
    (ptr (ptr counter)
     @-> (ptr client_registering_resource)
     @-> returning int
    )

let subscription_channel_status =
  foreign "aeron_subscription_channel_status"
    ((ptr subscription) @-> returning int64_t)

let header : [`Header] abstract typ =
  abstract_1 "aeron_header_stct"

module Fragment_handler =
  (val (dyfnp (
     clientd            (* clientd *)
     @-> (ptr uint8_t)  (* buffer  *)
     @-> size_t         (* length  *)
     @-> (ptr header)   (* header  *)
     @-> returning void
   )))

let fragment_assembler : [`Fragment_assembler] abstract typ =
  abstract_1 "aeron_fragment_assembler_stct"

let fragment_assembler_create =
  foreign "aeron_fragment_assembler_create"
    (ptr (ptr fragment_assembler)
     @-> Fragment_handler.t (* delegate *)
     @-> clientd            (* delegate_clientd *)
     @-> returning int
    )

let fragment_assembler_handler : Fragment_handler.fn =
  foreign "aeron_fragment_assembler_handler"
    (clientd             (* clientd *)
     @-> (ptr uint8_t)   (* buffer  *)
     @-> size_t          (* length  *)
     @-> (ptr header)    (* header  *)
     @-> returning void
    )

let subscription_poll =
  foreign "aeron_subscription_poll"
    ((ptr subscription)
    @-> Fragment_handler.t (* handler *)
    @-> (ptr fragment_assembler) (* fragment_assembler *)
    @-> size_t             (* fragment_limit *)
    @-> returning int
   )

module Reserved_value_supplier =
  (val (dyfnp (
     clientd               (* clientd *)
     @-> ptr uint8_t       (* buffer *)
     @-> size_t            (* frame_length *)
     @-> returning int64_t
   )))


let exclusive_publication_offer =
  foreign "aeron_exclusive_publication_offer"
    ((ptr exclusive_publication)       (* exclusive_publication   *)
     @-> (ptr uint8_t)                 (* buffer                  *)
     @-> size_t                        (* length                  *)
     @-> Reserved_value_supplier.t_opt (* reserved_value_supplier *)
     @-> clientd                       (* clientd                 *)
     @-> returning int64_t
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

let alloc_ptr_exclusive_publication () =
  allocate (ptr exclusive_publication) (from_voidp exclusive_publication null)

let alloc_ptr_subscription () =
  allocate (ptr subscription) (from_voidp subscription null)

let alloc_ptr_fragment_assembler () =
  allocate (ptr fragment_assembler) (from_voidp fragment_assembler null)
