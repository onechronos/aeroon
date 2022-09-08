open Aeron.Raw

let string_of_publication_error = function
  | Not_connected -> "not connected"
  | Back_pressured -> "back pressured"
  | Admin_action -> "admin action"
  | Closed -> "closed"
  | Max_position_exceeded -> "max position exceeded"
  | Error -> "error"

let () =
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
          poll ()
        | Error -> failwith "pub error"
      in
      poll ()
  in
  let publication_closed () = print_endline "publication closed" in
  Callback.register "pc" publication_closed;

  let exclusive_publication =
    match async_add_exclusive_publication client uri stream_id with
    | None -> failwith "async_add_exclusive_publication"
    | Some async ->
      let rec poll () =
        match async_add_exclusive_publication_poll async with
        | Ok x_pub -> x_pub
        | TryAgain ->
          print_endline "x-pub try again";
          poll ()
        | Error -> failwith "x-pub error"
      in
      poll ()
  in

  let subscription =
    let on_available_image _sub _image = print_endline "on_available_image" in
    Callback.register "oai" on_available_image;

    let on_unavailable_image _sub _image =
      print_endline "on_unavailable_image"
    in
    Callback.register "ouai" on_unavailable_image;

    match
      async_add_subscription client uri stream_id (Some on_available_image)
        (Some on_unavailable_image)
      (* async_add_subscription client uri stream_id None None *)
    with
    | Some async ->
      Unix.sleep 1;
      print_endline "got sub async";
      let rec poll () =
        match async_add_subscription_poll async with
        | Error -> failwith "async_add_subscription_poll error"
        | TryAgain ->
          print_endline "sub try again";
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
  | Ok position -> Printf.printf "publication offer position=%d\n" position
  | Error code ->
    Printf.printf "publication offer error=%s\n"
      (string_of_publication_error code));

  (match exclusive_publication_offer exclusive_publication msg with
  | Ok position ->
    Printf.printf "exclusive publication offer position=%d\n" position
  | Error code ->
    Printf.printf "exclusive publication offer error=%s\n"
      (string_of_publication_error code));

  let fragment_handler msg = print_endline ("fragment handler: " ^ msg) in
  Callback.register "fh" fragment_handler;

  let fragment_assembler =
    match fragment_assembler_create fragment_handler with
    | Some fragment_assembler -> fragment_assembler
    | None -> failwith "failed to create fragment assembler"
  in

  let image_fragment_assembler =
    match image_fragment_assembler_create fragment_handler with
    | Some image_fragment_assembler -> image_fragment_assembler
    | None -> failwith "failed to create image fragment assembler"
  in

  let rec poll () =
    match subscription_poll subscription fragment_assembler 10 with
    | Some num_frags_read when num_frags_read = 0 -> poll ()
    | Some num_frags_read ->
      Printf.printf "num fragments read: %d\n" num_frags_read
    | None ->
      let err_msg =
        Printf.sprintf "subscription_poll error: [%d]%s" (errcode ())
          (errmsg ())
      in
      failwith err_msg
  in
  poll ();

  let image =
    match subscription_image_at_index subscription 0 with
    | None -> failwith "no image at index 0"
    | Some image -> image
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
        Printf.printf "publication close %s\n"
          (match publication_close pub (Some publication_closed) with
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

  Gc.compact ();
  print_endline "post compact"
