open Ctypes
open Foreign

type clientd = unit ptr
let clientd = ptr void

let abstract_1 name =
  abstract ~name ~size:1 ~alignment:1

module Error_Handler =
  (val (
     dynamic_funptr (
       clientd @->
         int @->
         string @->
         returning void
     )
   )
  )

let client_registering_resource : [`Registering_resource] abstract typ =
  abstract_1 "aeron_client_registering_resource_stct"

let async_add_publication  = client_registering_resource
let async_add_subscription = client_registering_resource

module On_new_publication =
  (val (
     dynamic_funptr (
       clientd @->                       (* clientd        *)
         (ptr async_add_publication) @-> (* async          *)
         string @->                      (* channel        *)
         int32_t @->                     (* stream_id      *)
         int64_t @->                     (* correlation_id *)
         returning void
     )
   )
  )

module On_new_subscription = On_new_publication

let subscription : [`Subscription] abstract typ =
  abstract_1 "aeron_subscription_stct"

let image : [`Image ] abstract typ =
  abstract_1 "aeron_image_stct"

module On_available_image =
  (val (
     dynamic_funptr (
       clientd @->              (* clientd      *)
         (ptr subscription) @-> (* subscription *)
         (ptr image) @->        (* image        *)
         returning void
     )
   )
  )

module On_unavailable_image = On_available_image

let counters_reader : [`Counters_reader] abstract typ =
  abstract_1 "aeron_counters_reader_stct"

module On_available_counter =
  (val (
     dynamic_funptr (
       clientd @->                 (* clientd         *)
         (ptr counters_reader) @-> (* counters_reader *)
         int64_t @->               (* registration_id *)
         int32_t @->               (* counter_id      *)
         returning void
     )
   )
  )

module On_unavailable_counter = On_available_counter

module On_close_client =
  (val (
     dynamic_funptr (
       clientd @->
         returning void
     )
   )
  )

module Context = struct
  let t : [`Context] abstract typ =
    abstract ~name:"aeron_context_stct" ~size:1 ~alignment:1

  let alloc () =
    allocate (ptr t) (from_voidp t null)

  let init =
    foreign "aeron_context_init"
      (ptr (ptr t) @-> returning int)

  let get_dir =
    foreign "aeron_context_get_dir"
      ((ptr t) @-> returning string)

  (* Note: [set_dir new_dir] seems to return [None] on success *)
  let set_dir =
    foreign "aeron_context_set_dir"
      ((ptr t) @-> string @-> returning string_opt)

  (* Note: [set_driver_timeout_msg timeout] seems to return a
     meaningless error code (always 0, even for negative values) *)
  let set_driver_timeout_ms =
    foreign "aeron_context_set_driver_timeout_ms"
      ((ptr t) @-> uint64_t @-> returning int)

  let get_driver_timeout_ms =
    foreign "aeron_context_get_driver_timeout_ms"
      ((ptr t) @-> returning uint64_t)

  let set_keepalive_interval_ns =
    foreign "aeron_context_set_keepalive_interval_ns"
      ((ptr t) @-> uint64_t @-> returning int)

  let get_keepalive_interval_ns =
    foreign "aeron_context_get_keepalive_interval_ns"
      ((ptr t) @-> returning uint64_t)

  let set_resource_linger_duration_ns =
    foreign "aeron_context_set_resource_linger_duration_ns"
      ((ptr t) @-> uint64_t @-> returning int)

  let get_resource_linger_duration_ns =
    foreign "aeron_context_get_resource_linger_duration_ns"
      ((ptr t) @-> returning uint64_t)

  let set_pre_touch_mapped_memory =
    foreign "aeron_context_set_pre_touch_mapped_memory"
      ((ptr t) @-> bool @-> returning int)

  let get_pre_touch_mapped_memory =
    foreign "aeron_context_get_pre_touch_mapped_memory"
      ((ptr t) @-> returning bool)

  let set_error_handler =
    foreign "aeron_context_set_error_handler"
      ((ptr t) @-> Error_Handler.t @-> clientd @-> returning int)

  let set_on_new_publication =
    foreign "aeron_context_set_on_new_publication"
      ((ptr t) @-> On_new_publication.t @-> clientd @-> returning int)

  let set_on_new_exclusive_publication =
    foreign "aeron_context_set_on_new_exclusive_publication"
      ((ptr t) @-> On_new_publication.t @-> clientd @-> returning int)

  let set_on_new_subscription =
    foreign "aeron_context_set_on_new_subscription"
      ((ptr t) @-> On_new_subscription.t @-> clientd @-> returning int)

  let set_on_available_counter =
    foreign "aeron_context_set_on_available_counter"
      ((ptr t) @-> On_available_counter.t @-> clientd @-> returning int)

  let set_on_unavailable_counter =
    foreign "aeron_context_set_on_unavailable_counter"
      ((ptr t) @-> On_unavailable_counter.t @-> clientd @-> returning int)

  let set_on_close_client =
    foreign "aeron_context_set_on_close_client"
      ((ptr t) @-> On_close_client.t @-> clientd @-> returning int)

end

let pr = Printf.printf
let u64_to_i = Unsigned.UInt64.to_int

let _ =
  let p_context = Context.alloc () in
  let err = Context.init p_context in
  match err with
  | -1 -> failwith "context_init"
  | 0 -> (
      let ctx = !@ p_context in
      pr "dir=%s\n" (Context.get_dir ctx);

      pr "timeout=%d\n"
        (u64_to_i (Context.get_driver_timeout_ms ctx));

      pr "keepalive_ns=%d\n"
        (u64_to_i (Context.get_keepalive_interval_ns ctx));

      pr "linger_duration_ns=%d\n"
        (u64_to_i (Context.get_resource_linger_duration_ns ctx));

      pr "pre_touch_mappped_memory=%b\n"
        (Context.get_pre_touch_mapped_memory ctx);

      let _ = Context.set_error_handler ctx (
        Error_Handler.of_fun (
          fun _ code msg ->
            Printf.printf "error code=%d msg=%s\n%!" code msg
        )
      ) null in

      let _ = Context.set_on_new_publication ctx (
        On_new_publication.of_fun (
          fun _ _ channel stream_id correlation_id ->
            Printf.printf "new publication \
                           channel=%s \
                           stream_id=%ld \
                           correlation_id=%Ld\n%!"
              channel stream_id correlation_id
        )
      ) null in

      let _ = Context.set_on_new_exclusive_publication ctx (
        On_new_publication.of_fun (
          fun _ _ channel stream_id correlation_id ->
            Printf.printf "new exclusive publication \
                           channel=%s \
                           stream_id=%ld \
                           correlation_id=%Ld\n%!"
              channel stream_id correlation_id
        )
      ) null in

      let _ = Context.set_on_new_subscription ctx (
        On_new_subscription.of_fun (
          fun _ _ channel stream_id correlation_id ->
            Printf.printf "new subscription \
                           channel=%s \
                           stream_id=%ld \
                           correlation_id=%Ld\n%!"
              channel stream_id correlation_id
        )
      ) null in

      let _ = Context.set_on_available_counter ctx (
        On_available_counter.of_fun (
          fun _ _ registration_id counter_id ->
            Printf.printf "available counter \
                           registration_id=%Ld \
                           counter_id=%ld\n%!"
              registration_id counter_id
        )
      ) null in

      let _ = Context.set_on_unavailable_counter ctx (
        On_unavailable_counter.of_fun (
          fun _ _ registration_id counter_id ->
            Printf.printf "unavailable counter \
                           registration_id=%Ld \
                           counter_id=%ld\n%!"
              registration_id counter_id
        )
      ) null in

      let _ = Context.set_on_close_client ctx (
        On_close_client.of_fun (
          fun _ ->
            Printf.printf "close client\n%!"
        )
      ) null in
      ()

      (*
      let dir2 = Context.set_dir p "/tmp/z" in
      (match dir2 with
      | None -> pr "dir2=None\n";
      | Some d2 -> pr "dir2=%s\n" d2
      );
      let dir = Context.get_dir p in
      pr "dir=%s\n" dir;
      let err = Context.set_driver_timeout_ms p (Unsigned.UInt64.of_int (0)) in
      pr "err=%d\n" err;
      *)


    )
  | _ -> assert false

