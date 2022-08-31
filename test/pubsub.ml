open Aeroon.Bindings
module F = C.Functions

let[@inline] ( let@ ) f x = f x
let channel = "aeron:udp?endpoint=localhost:20121"
let stream_id = 1001l
let pr = Printf.printf

let subscribe () =
  let@ ctx = Aeroon.Context.with_ in
  let@ client = Aeroon.Client.with_ ctx in
  Aeroon.Client.start client;

  let sub = Aeroon.Client.add_subscription client ~uri:channel ~stream_id in

  let n_iterations = ref 0 in
  while true do
    let msg_l = Aeroon.Subscription.poll sub in
    pr "got %d fragments\n%!" (List.length msg_l);
    List.iteri (pr "  fragment[%d]: %s\n%!") msg_l;

    incr n_iterations;
    if !n_iterations mod 100 = 0 then Gc.compact ()
  done;
  ()

let env_N = try Sys.getenv "N" |> int_of_string with _ -> 100
let env_SLEEP = try Sys.getenv "SLEEP" |> float_of_string with _ -> 1.

let publish () =
  let@ ctx = Aeroon.Context.with_ in
  let@ client = Aeroon.Client.with_ ctx in
  Aeroon.Client.start client;

  pr "emit messages every %fs, %d iterations\n%!" env_SLEEP env_N;

  let publication =
    Aeroon.Client.add_publication client ~uri:channel ~stream_id
  in

  let msg =
    "This is a test of the emergency broadcast system. This is only a test."
  in

  let rec pub n i =
    if i < n then (
      let msg = Printf.sprintf "[%d] %s" i msg in
      let pos = Aeroon.Publication.offer' publication msg in
      pr "published (pos=%Ld) %S\n%!" pos msg;

      (* emit a batch of messages in bulk *)
      for _j = 1 to 5 do
        Aeroon.Publication.offer publication (Printf.sprintf "bulk %d" _j)
      done;

      if n mod 100 = 0 then Gc.compact ();

      Unix.sleepf env_SLEEP;
      pub n (i + 1)
    ) else
      ()
  in
  pub env_N 0;
  ()

let _ =
  let role = Sys.argv.(1) in
  match role with
  | "sub" -> subscribe ()
  | "pub" -> publish ()
  | _ ->
    Printf.printf "usage: %s <sub|pub>\n%!" Sys.argv.(0);
    exit 1
