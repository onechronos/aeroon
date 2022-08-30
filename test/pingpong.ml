(** round-trip benchmark over Aeron *)

let ping = "aeron:udp?endpoint=localhost:20123", 1002l
let pong = "aeron:udp?endpoint=localhost:20124", 1003l
(*
let number_of_messages = 10_000_000
let number_of_warm_up_messages = 100_000
*)
let fragment_count_limit = 10

open Ctypes
open Aeron
open C.Functions

let s_to_i = Unsigned.Size_t.to_int
let i_to_s = Unsigned.Size_t.of_int

let create_subscription client (channel, stream_id) =
  let async =
    let p_async = Alloc.ptr_async_add_subscription () in
    let err = async_add_subscription p_async client channel stream_id None null None null in
    assert (err >= 0);
    !@ p_async
  in

  let subscription =
    let p_subscription = Alloc.ptr_subscription () in
    let rec poll () =
      match async_add_subscription_poll p_subscription async with
      | 1 -> !@ p_subscription
      | 0 -> poll ()
      | _ -> assert false
    in
    poll ()
  in

  let rec wait_until_connected () =
    if not (subscription_is_connected subscription) then
      wait_until_connected ()
    else
      ()
  in
  wait_until_connected ();
  subscription

let create_exclusive_publication client (channel, stream_id) =
  let async =
    let p_async = Alloc.ptr_async_add_exclusive_publication () in
    let err = async_add_exclusive_publication p_async client channel stream_id in
    assert (err >= 0);
    !@ p_async
  in

  let exclusive_publication =
    let p_exclusive_publication = Alloc.ptr_exclusive_publication () in
    let rec poll () =
      match async_add_exclusive_publication_poll p_exclusive_publication async with
      | 1 -> !@ p_exclusive_publication
      | 0 -> poll ()
      | _ -> assert false
    in
    poll ()
  in
  exclusive_publication

let context_and_client () =
  let ctx =
    let p_context = Alloc.ptr_context () in
    let err = context_init p_context in
    assert (err = 0);
    !@ p_context
  in

  let client =
    let p_client = Alloc.ptr_client () in
    let err = init p_client ctx in
    assert (err = 0);
    !@ p_client
  in

  let err = start client in
  assert (err = 0);
  ctx, client

let cache = ref []

let pong =
  let rec offer_until_success publication buffer length =
    let result = exclusive_publication_offer publication buffer length None null in
    if result < 0L then (
      idle_strategy_busy_spinning_idle null 0;
      offer_until_success publication buffer length
    )
    else
      ()
  in

  let ping_poll_handler publication _ buffer length _ =
    let msg = string_from_ptr buffer ~length:(s_to_i length) in
    offer_until_success publication msg length;
  in


  fun () ->
    let _context, client = context_and_client () in
    let subscription = create_subscription client ping in

    let publication = create_exclusive_publication client pong in
    let pph = ping_poll_handler publication in
    cache := pph :: !cache;

    let image = subscription_image_at_index subscription (i_to_s 0) in

    let fragment_assembler =
      let p_fragment_assembler = Alloc.ptr_image_fragment_assembler () in
      let res = image_fragment_assembler_create p_fragment_assembler
          pph null in
      assert (res = 0);
      !@ p_fragment_assembler
    in

    let fragment_count_limit = i_to_s fragment_count_limit in
    let rec loop () =
      let fragments_read = image_poll image image_fragment_assembler_handler
          fragment_assembler fragment_count_limit in
      if fragments_read < 0 then (
        print_endline (errmsg ());
        exit 1
      )
      else (
        idle_strategy_busy_spinning_idle null fragments_read;
        loop ()
      )
    in
    loop ()

let _ =
  pong ()
