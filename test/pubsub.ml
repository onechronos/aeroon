open Aeron
open Ctypes
open C.Functions

let channel = "aeron:udp?endpoint=localhost:20121"
let stream_id = 1001l


let s_of_i  = Unsigned.Size_t.of_int
let s_to_i  = Unsigned.Size_t.to_int

let pr = Printf.printf

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


let subscribe () =
  let ctx, client = context_and_client () in

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

  let fragment_handler =
    fun _ buf size _header ->
      let message = string_from_ptr buf ~length:(s_to_i size) in
      Printf.printf "received: %s\n%!" message
  in

  let fragment_assembler =
    let p_fragment_assembler = Alloc.ptr_fragment_assembler () in
    let err = fragment_assembler_create p_fragment_assembler fragment_handler null in
    assert (err = 0);
    !@ p_fragment_assembler
  in

  let rec poll () =
    let num_fragments_read =
      subscription_poll
        subscription
        fragment_assembler_handler
        fragment_assembler
        (s_of_i 10)
    in
    if num_fragments_read < 0 then
      pr "subscribe error: %s\n%!" (errmsg ())
    else (
      Printf.printf "num_fragments_received = %d\n%!" num_fragments_read;
      Unix.sleep 1;
      (* main_idle_strategy client 100; *)
      poll ()
    )
  in

  poll ();

  print_endline "close";
  let _ = close client in
  print_endline "context_close";
  let _ = context_close ctx in
  ()

let publish () =
  let ctx, client = context_and_client () in

  let async =
    let p_async = Alloc.ptr_async_add_publication () in
    let err = async_add_publication p_async client channel stream_id in
    assert (err >= 0);
    !@ p_async
  in

  let publication =
    let p_publication = Alloc.ptr_publication () in
    let rec poll () =
      match async_add_publication_poll p_publication async with
      | 1 -> !@ p_publication
      | 0 -> poll ()
      | _ -> assert false
    in
    poll ()
  in

  let msg = "This is a test of the emergency broadcast system. This is \
             only a test." in

  let rec pub n i =
    if i < n then
      let msg = Printf.sprintf "[%d] %s" i msg in
      let buffer_size = s_of_i (String.length msg) in
      let status = publication_offer publication
          msg buffer_size None null in
      pr "status=%Ld\n%!" status;
      Unix.sleep 1;
      pub n (i + 1)
    else
      ()
  in
  pub 100 0;

  print_endline "close";
  let _ = close client in
  print_endline "context_close";
  let _ = context_close ctx in
  ()


let _ =
  let role = Sys.argv.(1) in
  match role with
  | "sub" -> subscribe ()
  | "pub" -> publish ()
  | _ ->
    Printf.printf "usage: %s <sub|pub>\n%!" Sys.argv.(1);
    exit 1
