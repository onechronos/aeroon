(** round-trip benchmark over Aeron *)

open Aeron.Cooked

let ping_canal =
  { uri = "aeron:udp?endpoint=localhost:20123"; stream_id = 1002 }

let pong_canal =
  { uri = "aeron:udp?endpoint=localhost:20124"; stream_id = 1003 }

let num_measured_messages = 10_000_000

let num_warm_up_messages = 100_000

let fragment_count_limit = 10

let use_image = true

type pong_pub_mode =
  [ `TryClaim
  | `XPub
  | `Pub
  ]

let pong_pub_mode : pong_pub_mode = `XPub

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

let create_subscription client canal =
  let subscription =
    match Subscription.create client canal with
    | Error msg -> failwith msg
    | Ok sub -> sub
  in
  let rec wait_until_connected () =
    if not (Subscription.is_connected subscription) then
      wait_until_connected ()
    else
      ()
  in
  wait_until_connected ();
  subscription

let create_exclusive_publication client canal =
  match ExclusivePublication.create client canal with
  | Error msg -> failwith msg
  | Ok pub -> pub

let create_publication client canal =
  match Publication.create client canal with
  | Error msg -> failwith msg
  | Ok pub -> pub

let () = ignore create_publication

let () = ignore create_exclusive_publication

open Utils

let ping =
  let durations = ref [] in
  let n = ref 0 in

  let pong_measuring_handler buffer =
    let end_ns = Clock.nano () in
    let start_ns = int_of_string buffer in
    let duration_ns = end_ns - start_ns in
    if !n mod 100_000 = 0 then Printf.printf "n=%d\n%!" !n;
    if !n >= num_warm_up_messages then durations := duration_ns :: !durations;
    incr n
  in

  let num_messages = num_measured_messages + num_warm_up_messages in

  let send_ping_and_recv_pong publication image_fragment_assembler image =
    let rec loop () =
      if !n < num_messages then (
        let now_ns = Clock.nano () in
        let msg = string_of_int now_ns in
        let position = send msg in
        recv position;
        loop ()
      )
    and send msg =
      match ExclusivePublication.offer publication msg with
      | Ok position -> position
      | Error Admin_action ->
        print_endline "admin action; trying again";
        IdleStrategy.busy_spinning 0 0;
        send msg
      | Error code ->
        print_endline (Publication_error.to_string code);
        exit 1
    and recv position =
      if Image.position image < position then
        poll ()
      else
        ()
    and poll () =
      match Image.poll image image_fragment_assembler fragment_count_limit with
      | None -> failwith "Image.poll"
      | Some 0 ->
        IdleStrategy.busy_spinning 0 0;
        poll ()
      | Some _ -> ()
    in

    loop ();
    stats !durations
  in

  fun () ->
    let _context, client = context_and_client () in

    let publication = create_exclusive_publication client ping_canal in
    print_endline "have publication";

    let subscription = create_subscription client pong_canal in
    print_endline "have subscription";

    let image_fragment_assembler =
      match Image.create_assembler pong_measuring_handler with
      | Some image_fragment_assembler -> image_fragment_assembler
      | None -> failwith "Image.create_assembler"
    in

    let image =
      match Subscription.image_at_index subscription 0 with
      | None -> failwith "Subscription.image_at_index"
      | Some image -> image
    in
    print_endline "starting send/recv";

    Unix.sleep 1;
    send_ping_and_recv_pong publication image_fragment_assembler image

let pong () =
  let context, client = context_and_client () in
  let subscription = create_subscription client ping_canal in
  print_endline "have subscription";

  let n = ref 0 in
  let send =
    match pong_pub_mode with
    | `TryClaim ->
      let exclusive_publication =
        create_exclusive_publication client pong_canal
      in
      print_endline "try-claim exclusive publication";

      fun msg ->
        assert (ExclusivePublication.try_claim exclusive_publication msg);
        incr n;
        if !n mod 1000 = 0 then Printf.printf "n=%d\n%!" !n
    | `XPub ->
      let exclusive_publication =
        create_exclusive_publication client pong_canal
      in
      print_endline "exclusive publication";

      let rec offer msg =
        match ExclusivePublication.offer exclusive_publication msg with
        | Ok 0 | Error Admin_action ->
          IdleStrategy.busy_spinning 0 0;
          offer msg
        | Ok position ->
          assert (position > 0);
          ()
        | Error code ->
          print_endline (Publication_error.to_string code);
          exit 1
      in

      offer
    | `Pub ->
      let publication = create_publication client pong_canal in
      print_endline "publication";

      let rec offer msg =
        match Publication.offer publication msg with
        | Ok 0 | Error Admin_action ->
          IdleStrategy.busy_spinning 0 0;
          offer msg
        | Ok position ->
          assert (position > 0);
          ()
        | Error code ->
          print_endline (Publication_error.to_string code);
          exit 1
      in

      offer
  in

  if use_image then (
    let image =
      match Subscription.image_at_index subscription 0 with
      | None -> failwith "Subscription.image_at_index"
      | Some image -> image
    in

    let image_fragment_assembler =
      match Image.create_assembler send with
      | None -> failwith "Image.create_assembler"
      | Some fragment_assembler -> fragment_assembler
    in

    let rec loop : unit -> unit =
     fun () ->
      match Image.poll image image_fragment_assembler fragment_count_limit with
      | None -> failwith "poll"
      | Some fragments_read ->
        IdleStrategy.busy_spinning 0 fragments_read;
        loop ()
    in
    print_endline "looping";

    loop ()
  ) else (
    let fragment_assembler =
      match Subscription.create_assembler send with
      | None -> failwith "Subscription.create_assembler"
      | Some fragment_assembler -> fragment_assembler
    in

    let rec loop : unit -> unit =
     fun () ->
      match
        Subscription.poll subscription fragment_assembler fragment_count_limit
      with
      | None -> failwith "poll"
      | Some fragments_read ->
        IdleStrategy.busy_spinning 0 fragments_read;
        loop ()
    in
    print_endline "looping";

    loop ()
  );
  cleanup context client

let _ =
  (match Sys.argv.(1) with
  | "ping" -> ping ()
  | "pong" -> pong ()
  | _ ->
    Printf.printf "usage: %s (ping|pong)\n%!" Sys.argv.(0);
    exit 1);
  Gc.compact ()
