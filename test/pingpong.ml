(** round-trip benchmark over Aeron *)

let ping_canal = "aeron:udp?endpoint=localhost:20123", 1002

let pong_canal = "aeron:udp?endpoint=localhost:20124", 1003

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

open Aeron.Raw

let context_and_client () =
  match context_init () with
  | None -> failwith "context_init"
  | Some ctx ->
    (match init ctx with
    | None -> failwith "init"
    | Some client ->
      assert (start client);
      ctx, client)

let cleanup ctx client =
  ignore (close client);
  ignore (context_close ctx)

let create_subscription client (uri, stream_id) =
  match async_add_subscription client uri stream_id with
  | None -> failwith "failed to get sub async"
  | Some async ->
    let rec poll () =
      match async_add_subscription_poll async with
      | Error -> failwith "async_add_subscription_poll error"
      | TryAgain ->
        Unix.sleepf 0.1;
        poll ()
      | Ok subscription -> subscription
    in
    let subscription = poll () in

    let rec wait_until_connected () =
      if not (subscription_is_connected subscription) then
        wait_until_connected ()
      else
        ()
    in
    wait_until_connected ();
    subscription

let create_exclusive_publication client (uri, stream_id) =
  match async_add_exclusive_publication client uri stream_id with
  | None -> failwith "async_add_exclusive_publication"
  | Some async ->
    let rec poll () =
      match async_add_exclusive_publication_poll async with
      | Ok x_pub -> x_pub
      | TryAgain ->
        Unix.sleepf 0.1;
        poll ()
      | Error -> failwith "x-pub error"
    in
    poll ()

let create_publication client (uri, stream_id) =
  match async_add_publication client uri stream_id with
  | None -> failwith "async_add_publication"
  | Some async ->
    let rec poll () =
      match async_add_publication_poll async with
      | Ok pub -> pub
      | TryAgain ->
        Unix.sleepf 0.1;
        poll ()
      | Error -> failwith "pub error"
    in
    poll ()

let () = ignore create_publication

let () = ignore create_exclusive_publication

let string_of_publication_error = function
  | Not_connected -> "not connected"
  | Back_pressured -> "back pressured"
  | Admin_action -> "admin action"
  | Closed -> "closed"
  | Max_position_exceeded -> "max position exceeded"
  | Error -> "error"

open Utils

let ping =
  let durations = ref [] in
  let n = ref 0 in

  let pong_measuring_handler buffer =
    let end_ns = nano_clock () in
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
        let now_ns = nano_clock () in
        let msg = string_of_int now_ns in
        let position = send msg in
        recv position;
        loop ()
      )
    and send msg =
      match exclusive_publication_offer publication msg with
      | Ok position -> position
      | Error Admin_action ->
        print_endline "admin action; trying again";
        idle_strategy_busy_spinning_idle 0 0;
        send msg
      | Error code ->
        print_endline (string_of_publication_error code);
        exit 1
    and recv position =
      if image_position image < position then
        poll ()
      else
        ()
    and poll () =
      match image_poll image image_fragment_assembler fragment_count_limit with
      | None -> failwith "failed to poll image"
      | Some 0 ->
        idle_strategy_busy_spinning_idle 0 0;
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
      match image_fragment_assembler_create pong_measuring_handler with
      | Some image_fragment_assembler -> image_fragment_assembler
      | None -> failwith "failed to create image fragment assembler"
    in

    let image =
      match subscription_image_at_index subscription 0 with
      | None -> failwith "subscription_image_at_index"
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
      print_endline "try-claim exclusive_publication";

      fun msg ->
        assert (exclusive_publication_try_claim exclusive_publication msg);
        incr n;
        if !n mod 1000 = 0 then Printf.printf "n=%d\n%!" !n
    | `XPub ->
      let exclusive_publication =
        create_exclusive_publication client pong_canal
      in
      print_endline "x-pub exclusive_publication";

      let rec offer msg =
        match exclusive_publication_offer exclusive_publication msg with
        | Ok 0 | Error Admin_action ->
          idle_strategy_busy_spinning_idle 0 0;
          offer msg
        | Ok position ->
          assert (position > 0);
          ()
        | Error code ->
          print_endline (string_of_publication_error code);
          exit 1
      in

      offer
    | `Pub ->
      let publication = create_publication client pong_canal in
      print_endline "pub publication";

      let rec offer msg =
        match publication_offer publication msg with
        | Ok 0 | Error Admin_action ->
          idle_strategy_busy_spinning_idle 0 0;
          offer msg
        | Ok position ->
          assert (position > 0);
          ()
        | Error code ->
          print_endline (string_of_publication_error code);
          exit 1
      in

      offer
  in

  if use_image then (
    let image =
      match subscription_image_at_index subscription 0 with
      | None -> failwith "subscription_image_at_index"
      | Some image -> image
    in

    let image_fragment_assembler =
      match image_fragment_assembler_create send with
      | None -> failwith "image_fragment_assembler_create"
      | Some fragment_assembler -> fragment_assembler
    in

    let rec loop : unit -> unit =
     fun () ->
      match image_poll image image_fragment_assembler fragment_count_limit with
      | None -> failwith "poll"
      | Some fragments_read ->
        idle_strategy_busy_spinning_idle 0 fragments_read;
        loop ()
    in
    print_endline "looping";

    loop ()
  ) else (
    let fragment_assembler =
      match fragment_assembler_create send with
      | None -> failwith "fragment_assembler_create"
      | Some fragment_assembler -> fragment_assembler
    in

    let rec loop : unit -> unit =
     fun () ->
      match
        subscription_poll subscription fragment_assembler fragment_count_limit
      with
      | None -> failwith "poll"
      | Some fragments_read ->
        idle_strategy_busy_spinning_idle 0 fragments_read;
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
