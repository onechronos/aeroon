module A = Aeron.Cooked

module Log = (val Logs.src_log (Logs.Src.create "chat"))

open struct
  let spf = Printf.sprintf

  let ( let@ ) f x = f x

  let unwrap_ ~err_to_string = function
    | Ok x -> x
    | Error e ->
      Log.err (fun k -> k "error: %s" (err_to_string e));
      failwith "an error occurred"

  let unwrap_str_ x = unwrap_ ~err_to_string:Fun.id x

  let unwrap_pub_err_ x = unwrap_ ~err_to_string:A.Publication_error.to_string x

  let unwrap_opt_ what x = x |> Option.to_result ~none:what |> unwrap_str_

  let debug_warn_ = function
    | Ok _ -> ()
    | Error s -> Log.warn (fun k -> k "%s" s)
end

module Message = struct
  module J = Yojson.Basic

  type t = {
    nick: string;
    msg: string;
  }

  let to_string (self : t) : string =
    J.to_string (`Assoc [ "nick", `String self.nick; "msg", `String self.msg ])

  let of_string (s : string) : (t, string) result =
    try
      let open J.Util in
      let j = J.from_string s in
      let nick = member "nick" j |> to_string in
      let msg = member "msg" j |> to_string in
      Ok { nick; msg }
    with
    | Yojson.Json_error s -> Error (spf "bad json: %s" s)
    | J.Util.Type_error (s, _) -> Error (spf "could not decode message: %s" s)
end

let canal ~port () : A.canal =
  let uri = spf "aeron:udp?endpoint=localhost:%d" port in
  { A.uri; stream_id = 1 }

let thread_receive ~stop ~port ~(client : A.Client.t) () : unit =
  let canal = canal ~port () in
  let@ sub = A.Subscription.with_ client canal in
  let sub = unwrap_str_ sub in

  (* handler for incoming messages *)
  let on_msg s =
    match Message.of_string s with
    | Ok msg -> Printf.printf "%s> %s\n%!" msg.nick msg.msg
    | Error s -> Log.err (fun k -> k "received invalid message: %s" s)
  in

  let assembler =
    A.Subscription.create_assembler on_msg |> unwrap_opt_ "message assembler"
  in
  while not (Atomic.get stop) do
    let n_read =
      A.Subscription.poll sub assembler 10 |> unwrap_opt_ "poll subscription"
    in
    A.IdleStrategy.sleeping 5_000 n_read
  done

let chat_file = "/tmp/chat"

let thread_cli ~stop ~nick ~port ~(client : A.Client.t) () : unit =
  let canal = canal ~port () in
  let@ pub = A.Publication.with_ client canal in
  let pub = unwrap_str_ pub in

  (* init the CLI *)
  LNoise.catch_break true;
  LNoise.history_load ~filename:chat_file |> debug_warn_;
  LNoise.history_set ~max_length:1000 |> debug_warn_;
  let@ () =
    Fun.protect ~finally:(fun () ->
        LNoise.history_save ~filename:chat_file |> debug_warn_)
  in

  while not (Atomic.get stop) do
    match LNoise.linenoise "> " |> Option.map String.trim with
    | exception Sys.Break ->
      Log.info (fun k -> k "quitting!");
      Atomic.set stop true
    | None ->
      Log.info (fun k -> k "disconnected, quitting!");
      Atomic.set stop true
    | Some "" -> ()
    | Some msg ->
      LNoise.history_add msg |> debug_warn_;
      let msg = { Message.nick; msg } |> Message.to_string in
      Log.debug (fun k -> k "send %s" msg);
      (* publish *)
      ignore (A.Publication.offer pub msg |> unwrap_pub_err_ : int)
  done

let run ~port ~nick () : unit =
  let stop = Atomic.make false in
  Log.info (fun k -> k "running with nick %S on port %d\n%!" nick port);
  let@ ctx = A.Context.with_ in
  let ctx = unwrap_opt_ "could not create ctx" ctx in
  let@ client = A.Client.with_create_start ctx in
  let client = unwrap_opt_ "could not start client" client in

  let t_read =
    Thread.create (fun () -> thread_receive ~stop ~port ~client ()) ()
  in
  let t_cli =
    Thread.create (fun () -> thread_cli ~stop ~nick ~port ~client ()) ()
  in

  Thread.join t_read;
  Thread.join t_cli;
  ()

let () =
  let port = ref 1125 in
  let nick = ref "chatbox" in
  let debug = ref false in
  let opts =
    [
      "--nick", Arg.Set_string nick, " pick nickname";
      "-p", Arg.Set_int port, " set port";
      "-d", Arg.Set debug, " enable debug";
    ]
    |> Arg.align
  in
  Arg.parse opts (fun _ -> ()) "chat [opt]*";
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level ~all:true
    (Some
       (if !debug then
         Logs.Debug
       else
         Logs.Info));
  run ~port:!port ~nick:!nick ()
