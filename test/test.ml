open Aeron.Raw

let _ =
  let major = version_major () in
  let minor = version_minor () in
  let patch = version_patch () in
  let full = version_full () in
  Printf.printf "version: %d.%d.%d %s\n" major minor patch full;

  let _ctx = context_init () in
  Gc.compact ();
  print_endline "post compact"
