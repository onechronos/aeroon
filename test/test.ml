open Aeron.Raw

let () =
  let major = version_major () in
  let minor = version_minor () in
  let patch = version_patch () in
  let full = version_full () in
  Printf.printf "version: %d.%d.%d %s\n%!" major minor patch full;

  let _ctx = context_init () in
  (*
  let _client = client_init _ctx in
  client_start _client;
  *)
  Gc.compact ();
  context_del _ctx;
  print_endline "post compact"
