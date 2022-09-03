open Aeron.Raw

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

  let async = async_add_publication client uri stream_id in

  (match async_add_publication_poll async with
  | Ok _pub -> print_endline "got publication"
  | TryAgain -> print_endline "try again"
  | Error -> print_endline "error");

  let async = async_add_exclusive_publication client uri stream_id in

  (match async_add_exclusive_publication_poll async with
  | Ok _pub -> print_endline "got exclusive_publication"
  | TryAgain -> print_endline "try again"
  | Error -> print_endline "error");

  let on_available_image _sub _image = print_endline "on_available_image" in
  Callback.register "oai" on_available_image;

  let on_unavailable_image _sub _image = print_endline "on_unavailable_image" in
  Callback.register "ouai" on_unavailable_image;

  (match
     async_add_subscription client uri stream_id (Some on_available_image)
       (Some on_unavailable_image)
   with
  | Some _async -> print_endline "got sub async"
  | None -> print_endline "failed to get sub async");

  Unix.sleep 2;

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
