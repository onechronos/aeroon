open Aeroon.Bindings
open Ctypes
module F = C.Functions

let[@inline] ( let@ ) f x = f x
let channel = "aeron:udp?endpoint=localhost:20121"
let stream_id = 1001l
let s_of_i = Unsigned.Size_t.of_int
let s_to_i = Unsigned.Size_t.to_int
let pr = Printf.printf

let context_and_client () =
  let ctx =
    let p_context = Alloc.ptr_context () in
    let err = F.context_init p_context in
    assert (err = 0);
    !@p_context
  in

  let client =
    let p_client = Alloc.ptr_client () in
    let err = F.init p_client ctx in
    assert (err = 0);
    !@p_client
  in

  let err = F.start client in
  assert (err = 0);
  ctx, client

let subscribe () =
  let ctx, client = context_and_client () in

  let async =
    let p_async = Alloc.ptr_async_add_subscription () in
    let err =
      F.async_add_subscription p_async client channel stream_id None null None
        null
    in
    assert (err >= 0);
    !@p_async
  in

  let subscription =
    let p_subscription = Alloc.ptr_subscription () in
    let rec poll () =
      match F.async_add_subscription_poll p_subscription async with
      | 1 -> !@p_subscription
      | 0 -> poll ()
      | _ -> assert false
    in
    poll ()
  in

  let fragment_handler _ buf size _header =
    let message = string_from_ptr buf ~length:(s_to_i size) in
    Printf.printf "received: %s\n%!" message
  in

  let fragment_assembler =
    let p_fragment_assembler = Alloc.ptr_fragment_assembler () in
    let err =
      F.fragment_assembler_create p_fragment_assembler fragment_handler null
    in
    assert (err = 0);
    !@p_fragment_assembler
  in

  let rec poll () =
    let num_fragments_read =
      F.subscription_poll subscription F.fragment_assembler_handler
        fragment_assembler (s_of_i 10)
    in
    if num_fragments_read < 0 then
      pr "subscribe error: %s\n%!" (F.errmsg ())
    else (
      F.main_idle_strategy client num_fragments_read;
      poll ()
    )
  in

  poll ();

  print_endline "close";
  let _ = F.close client in
  print_endline "context_close";
  let _ = F.context_close ctx in
  ()

let publish () =
  let@ ctx = Aeroon.Context.with_ in
  let@ client = Aeroon.Client.with_ ctx in
  Aeroon.Client.start client;

  let publication =
    Aeroon.Client.add_publication client ~uri:channel ~stream_id
  in

  let msg =
    "This is a test of the emergency broadcast system. This is only a test."
  in

  let rec pub n i =
    if i < n then (
      let msg = Printf.sprintf "[%d] %s" i msg in
      let buffer_size = s_of_i (String.length msg) in
      let status = F.publication_offer publication msg buffer_size None null in
      pr "status=%Ld\n%!" status;
      Unix.sleep 1;
      pub n (i + 1)
    ) else
      ()
  in
  pub 100 0;
  ()

let _ =
  let role = Sys.argv.(1) in
  match role with
  | "sub" -> subscribe ()
  | "pub" -> publish ()
  | _ ->
    Printf.printf "usage: %s <sub|pub>\n%!" Sys.argv.(1);
    exit 1
