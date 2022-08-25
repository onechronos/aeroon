open Aeroon
open Ctypes

let channel = "aeron:udp?endpoint=localhost:20121"
let stream_id = 1001l


let s_of_i  = Unsigned.Size_t.of_int
let i_to_s  = Unsigned.Size_t.to_int

let u8_of_i = Unsigned.UInt8.of_int
let u8_to_i = Unsigned.UInt8.to_int

let pr = Printf.printf

let context_and_client () =
  let ctx =
    let p_context = alloc_ptr_context () in
    let err = context_init p_context in
    assert (err = 0);
    !@ p_context
  in

  let client =
    let p_client = alloc_ptr_client () in
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
    let p_async = alloc_ptr_client_registering_resource () in
    let err = async_add_subscription p_async client channel stream_id None null None null in
    assert (err >= 0);
    !@ p_async
  in

  let subscription =
    let p_subscription = alloc_ptr_subscription () in
    let rec poll () =
      match async_add_subscription_poll p_subscription async with
      | 1 -> !@ p_subscription
      | 0 -> poll ()
      | _ -> assert false
    in
    poll ()
  in

  let fragment_handler = Fragment_handler.of_fun (
    fun _ buf size header ->
      Printf.printf "received fragment of size %d\n%!" (i_to_s size);
  ) in

  let fragment_assembler =
    let p_fragment_assembler = alloc_ptr_fragment_assembler () in
    let err = fragment_assembler_create p_fragment_assembler fragment_handler null in
    assert (err = 0);
    !@ p_fragment_assembler
  in

  let fragment_assembler_handler_fn =
    Fragment_handler.of_fun fragment_assembler_handler in

  let rec poll () =
    let num_fragments_read =
      subscription_poll
        subscription
        fragment_assembler_handler_fn
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
    let p_async = alloc_ptr_client_registering_resource () in
    let err = async_add_exclusive_publication p_async client channel stream_id in
    assert (err >= 0);
    !@ p_async
  in

  let exclusive_publication =
    let p_exclusive_publication = alloc_ptr_exclusive_publication () in
    let rec poll () =
      match async_add_exclusive_publication_poll p_exclusive_publication async with
      | 1 -> !@ p_exclusive_publication
      | 0 -> poll ()
      | _ -> assert false
    in
    poll ()
  in

  let buffer = CArray.of_list uint8_t (List.map u8_of_i [1;2;3;4;5]) in
  let buffer_size = s_of_i (CArray.length buffer) in

  let rec pub n i =
    if i < n then
      let status = exclusive_publication_offer exclusive_publication
          (CArray.start buffer) buffer_size None null in
      pr "status=%Ld\n%!" status;
      Unix.sleep 1;
      pub n (i + 1)
    else
      ()
  in
  pub 10 0;

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
