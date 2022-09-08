open Aeron.Raw

let uri = "aeron:udp?endpoint=localhost:20121"

let stream_id = 1001

let context_and_client () =
  match context_init () with
  | None -> failwith "context_init"
  | Some ctx ->
    (match init ctx with
    | None -> failwith "init"
    | Some client ->
      assert (start client);
      ctx, client)

let cleanup ctx client =
  ignore (close client);
  context_close ctx

let subscribe () =
  let ctx, client = context_and_client () in
  match async_add_subscription client uri stream_id None None with
  | None -> failwith "failed to get sub async"
  | Some async ->
    let subscription =
      let rec poll () =
        match async_add_subscription_poll async with
        | Error -> failwith "async_add_subscription_poll error"
        | TryAgain ->
          Unix.sleep 1;
          poll ()
        | Ok subscription -> subscription
      in
      poll ()
    in

    let fragment_handler msg = Printf.printf "received: %s\n%!" msg in

    let fragment_assembler =
      match fragment_assembler_create fragment_handler with
      | Some fragment_assembler -> fragment_assembler
      | None -> failwith "failed to create fragment assembler"
    in

    let rec poll : unit -> unit =
     fun () ->
      match subscription_poll subscription fragment_assembler 10 with
      | Some num_frags_read ->
        if num_frags_read > 0 then
          Printf.printf "num fragments read: %d\n" num_frags_read;
        idle_strategy_sleeping_idle 1_000_000 num_frags_read;

        poll ()
      | None ->
        Printf.printf "subscription_poll error: [%d]%s\n" (errcode ())
          (errmsg ());
        exit 1
    in
    poll ();
    cleanup ctx client

let publish () =
  let ctx, client = context_and_client () in

  let publication =
    match async_add_publication client uri stream_id with
    | None -> failwith "async_add_publication"
    | Some async ->
      let rec poll () =
        match async_add_publication_poll async with
        | Ok pub -> pub
        | TryAgain -> poll ()
        | Error -> failwith "pub error"
      in
      poll ()
  in

  let rec connect () =
    if not (publication_is_connected publication) then (
      print_endline "publication not connected; trying again";
      Unix.sleep 1;
      connect ()
    )
  in
  connect ();

  let msg =
    "This is a test of the emergency broadcast system. This is only a test."
  in

  let string_of_publication_error = function
    | Not_connected -> "not connected"
    | Back_pressured -> "back pressured"
    | Admin_action -> "admin action"
    | Closed -> "closed"
    | Max_position_exceeded -> "max position exceeded"
    | Error -> "error"
  in

  let rec pub n i =
    if i < n then (
      let msg = Printf.sprintf "[%d] %s" i msg in
      match publication_offer publication msg with
      | Ok _position ->
        idle_strategy_sleeping_idle 0 0;
        pub n (i + 1)
      | Error Admin_action ->
        (* try again *)
        pub n i
      | Error code ->
        Printf.printf "publication offer error=%s\n"
          (string_of_publication_error code);
        exit 1
    )
  in
  pub 1_000_000 0;
  cleanup ctx client

let _ =
  let role = Sys.argv.(1) in
  match role with
  | "sub" -> subscribe ()
  | "pub" -> publish ()
  | _ ->
    Printf.printf "usage: %s <sub|pub>\n%!" Sys.argv.(1);
    exit 1
