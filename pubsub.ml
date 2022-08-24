open Aeroon
open Ctypes

let uri = "aeron:udp?endpoint=localhost:20121"

let s_of_i = Unsigned.Size_t.of_int
let i_to_s = Unsigned.Size_t.to_int

let subscribe () =
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

  let async =
    let p_async = alloc_ptr_client_registering_resource () in
    let err = async_add_subscription p_async client uri 5l None null None null in
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
      Printf.printf "received fragment of size %d\n" (i_to_s size);
  ) in

  let num_fragments_received = subscription_poll
      subscription fragment_handler null (s_of_i 1) in

  Printf.printf "num_fragments_received = %d\n" num_fragments_received;

  print_endline "close";
  let _ = close client in
  print_endline "context_close";
  let _ = context_close ctx in
  ()

let publish () =
  ()


let _ =
  let role = Sys.argv.(1) in
  match role with
  | "sub" -> subscribe ()
  | "pub" -> publish ()
  | _ ->
    Printf.printf "usage: %s <sub|pub>\n%!" Sys.argv.(1);
    exit 1
