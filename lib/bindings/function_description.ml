open Ctypes
module T = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let funptr = Foreign.funptr (* ~runtime_lock:true *) ~thread_registration:true

  let funptr_opt =
    Foreign.funptr_opt (* ~runtime_lock:true *) ~thread_registration:true

  let version_full = foreign "aeron_version_full" (void @-> returning string)
  let version_major = foreign "aeron_version_major" (void @-> returning int)
  let version_minor = foreign "aeron_version_minor" (void @-> returning int)
  let version_patch = foreign "aeron_version_patch" (void @-> returning int)
  let nano_clock = foreign "aeron_nano_clock" (void @-> returning int64_t)
  let epoch_clock = foreign "aeron_epoch_clock" (void @-> returning int64_t)

  let on_available_image =
    funptr_opt
      Ctypes_static.(
        T.clientd (* clientd      *)
        @-> ptr T.subscription (* subscription *)
        @-> ptr T.image (* image        *)
        @-> returning void)

  let on_unavailable_image = on_available_image

  let context_get_dir =
    foreign "aeron_context_get_dir" (ptr T.context @-> returning string_opt)

  (* Note: [set_dir new_dir] seems to return [None] on success *)
  let context_set_dir =
    foreign "aeron_context_set_dir"
      (ptr T.context @-> string_opt @-> returning string_opt)

  (* Note: [set_driver_timeout_msg timeout] seems to return a
     meaningless error code (always 0, even for negative values) *)
  let context_set_driver_timeout_ms =
    foreign "aeron_context_set_driver_timeout_ms"
      (ptr T.context @-> uint64_t @-> returning int)

  let context_get_driver_timeout_ms =
    foreign "aeron_context_get_driver_timeout_ms"
      (ptr T.context @-> returning uint64_t)

  let context_set_keepalive_interval_ns =
    foreign "aeron_context_set_keepalive_interval_ns"
      (ptr T.context @-> uint64_t @-> returning int)

  let context_get_keepalive_interval_ns =
    foreign "aeron_context_get_keepalive_interval_ns"
      (ptr T.context @-> returning uint64_t)

  let context_set_resource_linger_duration_ns =
    foreign "aeron_context_set_resource_linger_duration_ns"
      (ptr T.context @-> uint64_t @-> returning int)

  let context_get_resource_linger_duration_ns =
    foreign "aeron_context_get_resource_linger_duration_ns"
      (ptr T.context @-> returning uint64_t)

  let context_set_pre_touch_mapped_memory =
    foreign "aeron_context_set_pre_touch_mapped_memory"
      (ptr T.context @-> bool @-> returning int)

  let context_get_pre_touch_mapped_memory =
    foreign "aeron_context_get_pre_touch_mapped_memory"
      (ptr T.context @-> returning bool)

  let agent_on_start =
    funptr
      Ctypes_static.(
        ptr void (* state     *) @-> string (* role name *) @-> returning int)

  (* declared but not defined
     let contex_set_agent_on_start =
       foreign "aeron_context_set_agent_on_start_function"
         ((ptr T.context) @-> agent_on_start @-> (ptr void) @-> returning int)
  *)

  let error_handler =
    funptr Ctypes_static.(T.clientd @-> int @-> string @-> returning void)

  let context_set_error_handler =
    foreign "aeron_context_set_error_handler"
      (ptr T.context @-> error_handler @-> T.clientd @-> returning int)

  let on_new_publication =
    funptr
      Ctypes_static.(
        T.clientd (* clientd        *)
        @-> ptr T.async_add_publication (* async          *)
        @-> string (* channel        *) @-> int32_t (* stream_id      *)
        @-> int32_t (* session_id     *) @-> int64_t (* correlation_id *)
        @-> returning void)

  let on_new_subscription =
    funptr
      Ctypes_static.(
        T.clientd (* clientd        *)
        @-> ptr T.async_add_subscription (* async          *)
        @-> string (* channel        *) @-> int32_t (* stream_id      *)
        @-> int64_t (* correlation_id *) @-> returning void)

  let context_set_on_new_publication =
    foreign "aeron_context_set_on_new_publication"
      (ptr T.context @-> on_new_publication @-> T.clientd @-> returning int)

  let context_set_on_new_exclusive_publication =
    foreign "aeron_context_set_on_new_exclusive_publication"
      (ptr T.context @-> on_new_publication @-> T.clientd @-> returning int)

  let context_set_on_new_subscription =
    foreign "aeron_context_set_on_new_subscription"
      (ptr T.context @-> on_new_subscription @-> T.clientd @-> returning int)

  let subscription_is_connected =
    foreign "aeron_subscription_is_connected"
      (ptr T.subscription @-> returning bool)

  let on_available_counter =
    funptr
      Ctypes_static.(
        T.clientd (* clientd         *)
        @-> ptr T.counters_reader (* counters_reader *)
        @-> int64_t (* registration_id *) @-> int32_t (* counter_id      *)
        @-> returning void)

  let on_unavailable_counter = on_available_counter

  let context_set_on_available_counter =
    foreign "aeron_context_set_on_available_counter"
      (ptr T.context @-> on_available_counter @-> T.clientd @-> returning int)

  let context_set_on_unavailable_counter =
    foreign "aeron_context_set_on_unavailable_counter"
      (ptr T.context @-> on_unavailable_counter @-> T.clientd @-> returning int)

  let on_close_client = funptr Ctypes_static.(T.clientd @-> returning void)

  let context_set_on_close_client =
    foreign "aeron_context_set_on_close_client"
      (ptr T.context @-> on_close_client @-> T.clientd @-> returning int)

  let context_set_use_conductor_agent_invoker =
    foreign "aeron_context_set_use_conductor_agent_invoker"
      (ptr T.context @-> bool @-> returning int)

  let context_get_use_conductor_agent_invoker =
    foreign "aeron_context_get_use_conductor_agent_invoker"
      (ptr T.context @-> returning bool)

  let context_init =
    foreign "aeron_context_init" (ptr (ptr T.context) @-> returning int)

  let context_close =
    foreign "aeron_context_close" (ptr T.context @-> returning int)

  let init =
    foreign "aeron_init" (ptr (ptr T.client) @-> ptr T.context @-> returning int)

  let start = foreign "aeron_start" (ptr T.client @-> returning int)

  let main_do_work =
    foreign "aeron_main_do_work" (ptr T.client @-> returning int)

  let main_idle_strategy =
    foreign "aeron_main_idle_strategy" (ptr T.client @-> int @-> returning void)

  let idle_strategy_busy_spinning_idle =
    foreign "aeron_idle_strategy_busy_spinning_idle"
      (ptr void @-> int @-> returning void)

  let close = foreign "aeron_close" (ptr T.client @-> returning int)
  let is_closed = foreign "aeron_is_closed" (ptr T.client @-> returning bool)
  let stream_out = funptr Ctypes_static.(string @-> returning void)

  let print_counters =
    foreign "aeron_print_counters"
      (ptr T.client @-> stream_out @-> returning void)

  let get_context =
    foreign "aeron_context" (ptr T.client @-> returning (ptr T.context))

  let client_id = foreign "aeron_client_id" (ptr T.client @-> returning int64_t)

  let next_correlation_id =
    foreign "aeron_next_correlation_id" (ptr T.client @-> returning int64_t)

  let async_add_publication =
    foreign "aeron_async_add_publication"
      (ptr (ptr T.async_add_publication)
      @-> ptr T.client @-> string @-> int32_t @-> returning int)

  let async_add_publication_poll =
    foreign "aeron_async_add_publication_poll"
      (ptr (ptr T.publication) @-> ptr T.async_add_publication @-> returning int)

  let notification =
    funptr_opt Ctypes_static.(T.clientd (* clientd *) @-> returning void)

  let publication_close =
    foreign "aeron_publication_close"
      (ptr T.publication @-> notification @-> T.clientd @-> returning int)

  let exclusive_publication_close =
    foreign "aeron_exclusive_publication_close"
      (ptr T.exclusive_publication
      @-> notification @-> T.clientd @-> returning int)

  let subscription_close =
    foreign "aeron_subscription_close"
      (ptr T.subscription @-> notification @-> T.clientd @-> returning int)

  let counter_close =
    foreign "aeron_counter_close"
      (ptr T.counter @-> notification (* on_close_complete         *)
     @-> T.clientd (* on_close_complete_clientd *) @-> returning int)

  let async_add_exclusive_publication =
    foreign "aeron_async_add_exclusive_publication"
      (ptr (ptr T.async_add_exclusive_publication)
      @-> ptr T.client @-> string @-> int32_t @-> returning int)

  let async_add_exclusive_publication_poll =
    foreign "aeron_async_add_exclusive_publication_poll"
      (ptr (ptr T.exclusive_publication)
      @-> ptr T.async_add_exclusive_publication
      @-> returning int)

  let async_add_subscription =
    foreign "aeron_async_add_subscription"
      (ptr (ptr T.async_add_subscription) (* async                        *)
      @-> ptr T.client (* client                       *)
      @-> string (* uri                          *) @-> int32_t
      (* stream_id                    *) @-> on_available_image
      (* on_available_image_handler   *) @-> T.clientd
      (* on_available_image_clientd   *) @-> on_unavailable_image
      (* on_unavailable_image_handler *) @-> T.clientd
      (* on_available_image_clientd   *) @-> returning int)

  let async_add_subscription_poll =
    foreign "aeron_async_add_subscription_poll"
      (ptr (ptr T.subscription)
      @-> ptr T.async_add_subscription
      @-> returning int)

  let counters_reader =
    foreign "aeron_counters_reader"
      (ptr T.client @-> returning (ptr T.counters_reader))

  let async_add_counter =
    foreign "aeron_async_add_counter"
      (ptr (ptr T.async_add_counter)
      @-> ptr T.client (* client              *)
      @-> int32_t (* type_id             *) @-> string (* key_buffer          *)
      @-> size_t (* key_buffer_length   *) @-> string (* label_buffer        *)
      @-> size_t (* label_buffer_length *) @-> returning int)

  let async_add_counter_poll =
    foreign "aeron_async_add_counter_poll"
      (ptr (ptr T.counter) @-> ptr T.async_add_counter @-> returning int)

  let subscription_channel_status =
    foreign "aeron_subscription_channel_status"
      (ptr T.subscription @-> returning int64_t)

  (* NOTE: aeronc.h refers to buffer as *uint8_t, but we use *char here
     for use of conversion using Ctypes.string_from_ptr *)
  let fragment_handler =
    funptr
      Ctypes_static.(
        T.clientd (* clientd *) @-> ptr char (* buffer  *)
        @-> size_t (* length  *) @-> ptr T.header (* header  *)
        @-> returning void)

  let fragment_assembler_create =
    foreign "aeron_fragment_assembler_create"
      (ptr (ptr T.fragment_assembler)
      @-> fragment_handler (* delegate *) @-> T.clientd (* delegate_clientd *)
      @-> returning int)

  let fragment_assembler_delete =
    foreign "aeron_fragment_assembler_delete"
      (ptr T.fragment_assembler @-> returning int)

  let image_fragment_assembler_create =
    foreign "aeron_image_fragment_assembler_create"
      (ptr (ptr T.image_fragment_assembler)
      @-> fragment_handler (* delegate *) @-> T.clientd (* delegate_clientd *)
      @-> returning int)

  let image_fragment_assembler_delete =
    foreign "aeron_image_fragment_assembler_delete"
      (ptr T.image_fragment_assembler @-> returning int)

  let fragment_assembler_handler =
    foreign "aeron_fragment_assembler_handler"
      (T.clientd (* clientd *) @-> ptr char (* buffer  *)
      @-> size_t (* length  *) @-> ptr T.header
      (* header  *) @-> returning void)

  let image_fragment_assembler_handler =
    foreign "aeron_image_fragment_assembler_handler"
      (T.clientd (* clientd *) @-> ptr char (* buffer  *)
      @-> size_t (* length  *) @-> ptr T.header
      (* header  *) @-> returning void)

  let subscription_poll =
    foreign "aeron_subscription_poll"
      (ptr T.subscription @-> fragment_handler (* handler *)
      @-> ptr T.fragment_assembler (* fragment_assembler *)
      @-> size_t (* fragment_limit *) @-> returning int)

  let reserved_value_supplier =
    funptr_opt
      Ctypes_static.(
        T.clientd (* clientd *) @-> ptr uint8_t (* buffer *)
        @-> size_t (* frame_length *) @-> returning int64_t)

  let publication_offer =
    foreign "aeron_publication_offer"
      (ptr T.publication (* publication             *)
      @-> string (* buffer                  *) @-> size_t
      (* length                  *) @-> reserved_value_supplier
      (* reserved_value_supplier *) @-> T.clientd
      (* clientd                 *) @-> returning int64_t)

  let exclusive_publication_offer =
    foreign "aeron_exclusive_publication_offer"
      (ptr T.exclusive_publication (* exclusive_publication   *)
      @-> string (* buffer                  *) @-> size_t
      (* length                  *) @-> reserved_value_supplier
      (* reserved_value_supplier *) @-> T.clientd
      (* clientd                 *) @-> returning int64_t)

  let errcode = foreign "aeron_errcode" (void @-> returning int)
  let errmsg = foreign "aeron_errmsg" (void @-> returning string)

  let subscription_image_at_index =
    foreign "aeron_subscription_image_at_index"
      (ptr T.subscription (* subscription *)
      @-> size_t (* index        *)
      @-> returning (ptr T.image))

  let subscription_image_retain =
    foreign "aeron_subscription_image_retain"
      (ptr T.subscription (* subscription *)
      @-> ptr T.image (* image        *)
      @-> returning int)

  let subscription_image_release =
    foreign "aeron_subscription_image_release"
      (ptr T.subscription (* subscription *)
      @-> ptr T.image (* image        *)
      @-> returning int)

  let counters_reader_foreach_counter_fn =
    funptr
      Ctypes_static.(
        int64_t (* value        *) @-> int32_t (* id           *)
        @-> int32_t (* type_id      *)
        @-> ptr char (* key          *)
        @-> size_t (* key_length   *)
        @-> ptr char (* label        *)
        @-> size_t (* label_length *) @-> T.clientd (* clientd      *)
        @-> returning void)

  let counters_reader_foreach_counter =
    foreign "aeron_counters_reader_foreach_counter"
      (ptr T.counters_reader (* counters_reader *)
      @-> counters_reader_foreach_counter_fn (* func            *) @-> T.clientd
      (* clientd         *) @-> returning void)

  let counters_reader_max_counter_id =
    foreign "aeron_counters_reader_max_counter_id"
      (ptr T.counters_reader @-> returning int32_t)

  let counters_reader_addr =
    foreign "aeron_counters_reader_addr"
      (ptr T.counters_reader (* counters_reader *)
      @-> int32_t (* counter_id      *) @-> returning int64_t)

  let counters_reader_registration_id =
    foreign "aeron_counters_reader_counter_registration_id"
      (ptr T.counters_reader (* counters_reader *)
      @-> int32_t (* counter_id      *)
      @-> ptr int64_t (* registration_id *)
      @-> returning int)

  (* note that the clientd appears to be intended to be an
     image_fragment_assembler, so using the more strict typing here *)
  let image_poll =
    foreign "aeron_image_poll"
      (ptr T.image (* image          *)
      @-> fragment_handler (* handler        *)
      @-> ptr T.image_fragment_assembler (* clientd        *)
      @-> size_t (* fragment_limit *) @-> returning int)
end
