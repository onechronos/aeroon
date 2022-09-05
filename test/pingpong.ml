(** round-trip benchmark over Aeron *)

let ping_canal = "aeron:udp?endpoint=localhost:20123", 1002

let pong_canal = "aeron:udp?endpoint=localhost:20124", 1003

let number_of_messages = 10_000_000

(* let number_of_warm_up_messages = 100_000 *)

open Aeron.Raw

let fragment_count_limit = 10

let context_and_client () =
  let ctx = context_init () in
  let client = init ctx in
  start client;
  ctx, client

let cleanup ctx client =
  close client;
  context_close ctx

let create_subscription client (uri, stream_id) =
  match async_add_subscription client uri stream_id None None with
  | None -> failwith "failed to get sub async"
  | Some async ->
    let rec poll () =
      match async_add_subscription_poll async with
      | Error -> failwith "async_add_subscription_poll error"
      | TryAgain ->
        Unix.sleepf 1e-6;
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

let string_of_publication_error = function
  | Not_connected -> "not connected"
  | Back_pressured -> "back pressured"
  | Admin_action -> "admin action"
  | Closed -> "closed"
  | Max_position_exceeded -> "max position exceeded"
  | Error -> "error"

let ping =
  let add_to_histogram _ = () in

  let pong_measuring_handler buffer =
    let end_ns = nano_clock () in
    let start_ns = int_of_string buffer in
    let duration_ns = end_ns - start_ns in
    add_to_histogram duration_ns
  in
  Callback.register "pmh" pong_measuring_handler;

  let image_fragment_assembler =
    match image_fragment_assembler_create pong_measuring_handler with
    | Some image_fragment_assembler -> image_fragment_assembler
    | None -> failwith "failed to create image fragment assembler"
  in

  let send_ping_and_recv_pong publication image num_messages =
    let rec loop n i =
      if i < n then (
        let now_ns = nano_clock () in
        let msg = string_of_int now_ns in
        let position = send msg in
        recv position;
        loop n (i + 1)
      )
    and send msg =
      match exclusive_publication_offer publication msg with
      | Ok position -> position
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
      | Some n when n = 0 ->
        Unix.sleep 0;
        idle_strategy_busy_spinning_idle 0 0;
        poll ()
      | Some _ -> ()
    in

    loop num_messages 0
  in

  fun () ->
    let _context, client = context_and_client () in
    let subscription = create_subscription client pong_canal in
    let publication = create_exclusive_publication client ping_canal in

    let image =
      match subscription_image_at_index subscription 0 with
      | None -> failwith "subscription_image_at_index"
      | Some image -> image
    in

    send_ping_and_recv_pong publication image number_of_messages

let pong =
  let offer_until_success publication msg =
    print_endline "ous";
    match exclusive_publication_offer publication msg with
    | Ok _ ->
      print_endline "ous: ok";
      ()
    | Error code ->
      print_endline "ous: err";
      print_endline (string_of_publication_error code);
      exit 1
  in

  fun () ->
    let context, client = context_and_client () in
    let subscription = create_subscription client ping_canal in

    let publication = create_exclusive_publication client pong_canal in
    let ping_poll_handler = offer_until_success publication in
    Callback.register "pph" ping_poll_handler;

    let image =
      match subscription_image_at_index subscription 0 with
      | None -> failwith "subscription_image_at_index"
      | Some image -> image
    in

    let image_fragment_assembler =
      match image_fragment_assembler_create ping_poll_handler with
      | None -> failwith "image_fragment_assembler_create"
      | Some fragment_assembler -> fragment_assembler
    in

    let rec loop () =
      match image_poll image image_fragment_assembler fragment_count_limit with
      | None -> failwith "image_poll"
      | Some fragments_read ->
        if fragments_read > 0 then Printf.printf "loop %d\n%!" fragments_read;
        idle_strategy_busy_spinning_idle 0 fragments_read;
        loop ()
    in
    let _ = loop () in
    cleanup context client

let _ =
  match Sys.argv.(1) with
  | "ping" -> ping ()
  | "pong" -> pong ()
  | _ ->
    Printf.printf "usage: %s (ping|pong)\n%!" Sys.argv.(0);
    exit 1
