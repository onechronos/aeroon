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
