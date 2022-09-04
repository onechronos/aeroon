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

  let ctx = context_init () in
  let client = init ctx in
  start client;
  print_endline "started!";

  let uri = "aeron:udp?endpoint=localhost:20121" in
  let stream_id = 1001 in

  let publication =
    let async = async_add_publication client uri stream_id in
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

  let exclusive_publication =
    let async = async_add_exclusive_publication client uri stream_id in
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
  ignore subscription;

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

  Gc.finalise
    (fun c ->
      print_endline "finalizing context";
      context_close c)
    ctx;
  Gc.finalise
    (fun c ->
      print_endline "finalizing client";
      close c)
    client;

  Gc.compact ();
  print_endline "post compact"
