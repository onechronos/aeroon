open Aeron.Raw

let string_of_publication_error = function
  | Not_connected -> "not connected"
  | Back_pressured -> "back pressured"
  | Admin_action -> "admin action"
  | Closed -> "closed"
  | Max_position_exceeded -> "max position exceeded"
  | Error -> "error"

let run () =
  let major = version_major () in
  let minor = version_minor () in
  let patch = version_patch () in
  let full = version_full () in
  Printf.printf "version: %d.%d.%d %s\n%!" major minor patch full;

  let ctx, client =
    match context_init () with
    | None -> failwith "context_init"
    | Some ctx ->
      (match init ctx with
      | None -> failwith "init"
      | Some client ->
        assert (start client);
        ctx, client)
  in
  print_endline "started!";

  let uri = "aeron:udp?endpoint=localhost:20121" in
  let stream_id = 1001 in

  let publication =
    match async_add_publication client uri stream_id with
    | None -> failwith "async_add_publication"
    | Some async ->
      let rec poll () =
        match async_add_publication_poll async with
        | Ok pub -> pub
        | TryAgain ->
          print_endline "pub try again";
          Unix.sleep 1;
          poll ()
        | Error -> failwith "pub error"
      in
      let pub = poll () in
      (*
      let rec connect () =
        if not (publication_is_connected pub) then (
          print_endline "publication not connected; trying again";
          Unix.sleep 1;
          connect ()
        ) else
          pub
      in
      connect () *)
      pub
  in

  let exclusive_publication =
    match async_add_exclusive_publication client uri stream_id with
    | None -> failwith "async_add_exclusive_publication"
    | Some async ->
      let rec poll () =
        match async_add_exclusive_publication_poll async with
        | Ok x_pub -> x_pub
        | TryAgain ->
          print_endline "x-pub try again";
          Unix.sleep 1;
          poll ()
        | Error -> failwith "x-pub error"
      in
      let x_pub = poll () in
      (*
      let rec connect () =
        if not (exclusive_publication_is_connected x_pub) then (
          print_endline "exclusive publication not connected; trying again";
          Unix.sleep 1;
          connect ()
        ) else
          x_pub
      in 
      connect ()
      *)
      x_pub
  in

  let subscription =
    match async_add_subscription client uri stream_id with
    | Some async ->
      print_endline "got sub async";
      let rec poll () =
        match async_add_subscription_poll async with
        | Error -> failwith "async_add_subscription_poll error"
        | TryAgain ->
          print_endline "sub try again";
          Unix.sleep 1;
          poll ()
        | Ok subscription -> subscription
      in
      poll ()
    | None -> failwith "failed to get sub async"
  in

  let msg =
    "This is a test of the emergency broadcast system. This is only a test."
  in
  (match publication_offer publication msg with
  | Ok position -> Printf.printf "publication offer position=%d\n%!" position
  | Error code ->
    Printf.printf "publication offer error=%s\n%!"
      (string_of_publication_error code));

  (match exclusive_publication_offer exclusive_publication msg with
  | Ok position ->
    Printf.printf "exclusive publication offer position=%d\n%!" position
  | Error code ->
    Printf.printf "exclusive publication offer error=%s\n%!"
      (string_of_publication_error code));

  let fragment_handler msg = print_endline ("fragment handler: " ^ msg) in

  let fragment_assembler =
    match fragment_assembler_create fragment_handler with
    | Some fragment_assembler -> fragment_assembler
    | None -> failwith "failed to create fragment assembler"
  in

  let rec poll () =
    match subscription_poll subscription fragment_assembler 10 with
    | Some 0 ->
      print_endline "subscription poll: read no fragments; trying again";
      idle_strategy_sleeping_idle 0 0;
      poll ()
    | Some num_frags_read ->
      Printf.printf "num fragments read: %d\n%!" num_frags_read
    | None ->
      let err_msg =
        Printf.sprintf "subscription_poll error: [%d]%s" (errcode ())
          (errmsg ())
      in
      failwith err_msg
  in
  print_endline "starting subscription poll";
  poll ();

  let image =
    match subscription_image_at_index subscription 0 with
    | None -> failwith "no image at index 0"
    | Some image -> image
  in

  let image_fragment_assembler =
    match image_fragment_assembler_create fragment_handler with
    | Some image_fragment_assembler -> image_fragment_assembler
    | None -> failwith "failed to create image fragment assembler"
  in

  (match image_poll image image_fragment_assembler 10 with
  | None -> failwith "failed to poll image"
  | Some n -> Printf.printf "polled %d fragments\n" n);

  let is_client_closed = ref false in
  let is_context_closed = ref false in

  Gc.finalise
    (fun pub ->
      if !is_client_closed || !is_context_closed then
        print_endline "client or context already closed"
      else if publication_is_closed pub then
        print_endline "publication already closed"
      else
        Printf.printf "publication close %s\n%!"
          (match publication_close pub with
          | true -> "success"
          | false -> "failure"))
    publication;

  Gc.finalise
    (fun c ->
      print_endline "finalizing context";
      ignore (context_close c);
      is_context_closed := true)
    ctx;

  Gc.finalise
    (fun c ->
      print_endline "finalizing client";
      ignore (close c);
      is_client_closed := true)
    client;

  ()

let _ =
  run ();
  Gc.compact ();
  print_endline "post compact"
