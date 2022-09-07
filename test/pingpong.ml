(** round-trip benchmark over Aeron *)

let ping_canal = "aeron:udp?endpoint=localhost:20123", 1002

let pong_canal = "aeron:udp?endpoint=localhost:20124", 1003

let number_of_messages = 1_000_000

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
  let async = async_add_exclusive_publication client uri stream_id in
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
  let async = async_add_publication client uri stream_id in
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

let ping =
  let durations = ref [] in
  let n = ref 0 in

  let add_duration duration =
    incr n;
    durations := duration :: !durations;
    if !n mod 100 = 0 then Printf.printf "n=%d\n%!" !n
  in

  let pong_measuring_handler buffer =
    let end_ns = nano_clock () in
    let start_ns = int_of_string buffer in
    let duration_ns = end_ns - start_ns in
    add_duration duration_ns
  in

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
        idle_strategy_busy_spinning_idle 0 0;
        poll ()
      | Some _ -> ()
    in

    loop num_messages 0;
    Printf.printf "%d\n" (List.length !durations)
  in

  fun () ->
    let _context, client = context_and_client () in
    let publication = create_exclusive_publication client ping_canal in
    print_endline "have publication";

    let subscription = create_subscription client pong_canal in
    print_endline "have subscription";

    let image =
      match subscription_image_at_index subscription 0 with
      | None -> failwith "subscription_image_at_index"
      | Some image -> image
    in
    print_endline "starting send/recv";

    Unix.sleep 5;
    send_ping_and_recv_pong publication image number_of_messages

let pong () =
  let use_image = false in
  let context, client = context_and_client () in
  let subscription = create_subscription client ping_canal in
  print_endline "have subscription";

  let exclusive_publication = create_exclusive_publication client pong_canal in
  print_endline "have exclusive_publication";
  flush stdout;

  let n = ref 0 in
  let send =
    let rec loop msg =
      Printf.printf "loop length=%d\n%!" (String.length msg);
      match exclusive_publication_try_claim exclusive_publication msg with
      | Ok buffer_claim ->
        assert (buffer_claim_commit buffer_claim);

        incr n;
        if !n mod 1000 = 0 then Printf.printf "n=%d\n%!" !n
      | Error _ ->
        idle_strategy_busy_spinning_idle 0 0;
        loop msg
    in
    loop
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
        Printf.printf "%d %b\n%!" fragments_read (image_is_closed image);
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
        Printf.printf "%d %b\n%!" fragments_read
          (subscription_is_closed subscription);
        idle_strategy_busy_spinning_idle 0 fragments_read;
        loop ()
    in
    print_endline "looping";

    loop ()
  );
  cleanup context client

let _ =
  match Sys.argv.(1) with
  | "ping" -> ping ()
  | "pong" -> pong ()
  | _ ->
    Printf.printf "usage: %s (ping|pong)\n%!" Sys.argv.(0);
    exit 1
