open Aeron.Cooked

let canal = { uri = "aeron:udp?endpoint=localhost:20121"; stream_id = 1001 }

let context_and_client () =
  match Context.create () with
  | None -> failwith "Context.create"
  | Some ctx ->
    (match Client.create ctx with
    | None -> failwith "Client.create"
    | Some client ->
      assert (Client.start client);
      ctx, client)

let cleanup ctx client =
  ignore (Client.close client);
  ignore (Context.close ctx)

let subscribe mbox () =
  let ctx, client = context_and_client () in
  let subscription =
    match Subscription.create client canal with
    | Ok sub -> sub
    | Error msg -> failwith msg
  in
  let fragment_handler = CCBlockingQueue.push mbox in

  let fragment_assembler =
    match Subscription.create_assembler fragment_handler with
    | Some fragment_assembler -> fragment_assembler
    | None -> failwith "failed to create fragment assembler"
  in

  let rec poll : unit -> unit =
   fun () ->
    match Subscription.poll subscription fragment_assembler 10 with
    | Some num_frags_read ->
      if num_frags_read > 0 then
        Printf.printf "num fragments read: %d\n" num_frags_read;
      IdleStrategy.sleeping 1_000_000 num_frags_read;

      poll ()
    | None -> failwith "Subscription.poll"
  in
  poll ();
  cleanup ctx client

let publish () =
  let ctx, client = context_and_client () in

  let publication =
    match Publication.create client canal with
    | Error msg -> failwith msg
    | Ok pub -> pub
  in

  let rec connect () =
    if not (Publication.is_connected publication) then (
      print_endline "publication not connected; trying again";
      Unix.sleep 1;
      connect ()
    )
  in
  connect ();

  let msg =
    "This is a test of the emergency broadcast system. This is only a test."
  in

  let rec pub n i =
    if i < n then (
      let msg = Printf.sprintf "[%d] %s" i msg in
      match Publication.offer publication msg with
      | Ok _position ->
        IdleStrategy.sleeping 0 0;
        pub n (i + 1)
      | Error Admin_action ->
        (* try again *)
        pub n i
      | Error code ->
        Printf.printf "Publication offer error=%s\n"
          (Publication_error.to_string code);
        exit 1
    )
  in
  pub 1_000_000 0;
  cleanup ctx client

let rec dequeue mbox =
  let msg = CCBlockingQueue.take mbox in
  print_endline msg;
  dequeue mbox

let main () =
  let role = Sys.argv.(1) in
  match role with
  | "sub" ->
    let mbox = CCBlockingQueue.create 1 in
    let _sub_thread = CCThread.spawn (subscribe mbox) in
    dequeue mbox
  | "pub" -> publish ()
  | _ ->
    Printf.printf "usage: %s <sub|pub>\n%!" Sys.argv.(1);
    exit 1

let _ = main ()
