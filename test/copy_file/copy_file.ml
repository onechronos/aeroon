(* an overly complicated "cp" *)

module A = Aeron.Cooked

module Log = (val Logs.src_log (Logs.Src.create "copy_file"))

open struct
  let spf = Printf.sprintf

  let ( let@ ) f x = f x

  let unwrap_ ~err_to_string = function
    | Ok x -> x
    | Error e ->
      let bt = Printexc.get_backtrace () in
      Log.err (fun k -> k "error: %s\n%s" (err_to_string e) bt);
      failwith "an error occurred"

  let unwrap_str_ x = unwrap_ ~err_to_string:Fun.id x

  let unwrap_pub_err_ x = unwrap_ ~err_to_string:A.Publication_error.to_string x

  let unwrap_opt_ what x = x |> Option.to_result ~none:what |> unwrap_str_
end

module Chunk = struct
  type t = string

  type view =
    | Eof
    | C of string

  let view (self : t) : view =
    if self.[0] = 'e' then
      Eof
    else
      C (String.sub self 1 (String.length self - 1))

  let eof = "e"

  let make c = "c" ^ c
end

module With_aeron = struct
  let canal ~port () : A.canal =
    let uri = spf "aeron:udp?endpoint=localhost:%d" port in
    { A.uri; stream_id = 1 }

  (* read input files *)
  let t_read ~(client : A.Client.t) ~port ~inputs () : unit =
    let canal = canal ~port () in
    let@ pub = A.Publication.with_ client canal in
    let pub = unwrap_str_ pub in

    let read1 file =
      Log.info (fun k -> k "reading file %S" file);
      let n_chunks = ref 0 in

      let@ ic = CCIO.with_in file in

      CCIO.read_chunks_iter ~size:200 ic (fun chunk ->
          incr n_chunks;
          let chunk = Chunk.make chunk in
          (* publish *)
          ignore (A.Publication.offer pub chunk |> unwrap_pub_err_ : int));

      Log.debug (fun k -> k "sent %d chunks for %S" !n_chunks file)
    in
    List.iter read1 inputs;
    (* now close *)
    Log.debug (fun k -> k "send EOF");
    ignore (A.Publication.offer pub Chunk.eof |> unwrap_pub_err_ : int);
    ()

  (* write into file *)
  let t_write ~(client : A.Client.t) ~port ~into ~n_written () : unit =
    let canal = canal ~port () in
    let@ sub = A.Subscription.with_ client canal in
    let sub = unwrap_str_ sub in

    let@ oc = CCIO.with_out into in

    let continue = ref true in
    let on_chunk c =
      match Chunk.view c with
      | Chunk.Eof ->
        Log.info (fun k -> k "received EOF chunk");
        continue := false
      | Chunk.C s ->
        ignore (Atomic.fetch_and_add n_written (String.length s) : int);
        output_string oc s
    in

    let assembler =
      A.Subscription.create_assembler on_chunk
      |> unwrap_opt_ "message assembler"
    in
    while !continue do
      let n_read =
        A.Subscription.poll sub assembler 10 |> unwrap_opt_ "poll subscription"
      in
      A.IdleStrategy.sleeping 5_000 n_read
    done;
    ()

  let copy ~inputs ~into ~port () : unit =
    let time_start = Unix.gettimeofday () in

    Log.info (fun k -> k "start client");
    let@ ctx = A.Context.with_ in
    let ctx = unwrap_opt_ "could not create ctx" ctx in
    let@ client = A.Client.with_create_start ctx in
    let client = unwrap_opt_ "could not start client" client in

    let n_written = Atomic.make 0 in

    let t_read = Thread.create (fun () -> t_read ~client ~port ~inputs ()) () in
    let t_write =
      Thread.create (fun () -> t_write ~client ~port ~into ~n_written ()) ()
    in

    Thread.join t_read;
    Thread.join t_write;

    let time_stop = Unix.gettimeofday () in
    Log.info (fun k ->
        k "wrote %d bytes in %.4fs (%.2fMB/s)" (Atomic.get n_written)
          (time_stop -. time_start)
          (float (Atomic.get n_written) /. 1e6 /. (time_stop -. time_start)));
    Log.info (fun k -> k "all done, exiting");
    ()
end

module With_zmq = struct
  let uri = "inproc:///foo"

  (* read input files *)
  let t_read ~(ctx : Zmq.Context.t) ~inputs () : unit =
    let sock = Zmq.Socket.create ctx Zmq.Socket.push in
    let@ () = Fun.protect ~finally:(fun () -> Zmq.Socket.close sock) in
    Zmq.Socket.connect sock uri;

    let read1 file =
      Log.info (fun k -> k "reading file %S" file);
      let n_chunks = ref 0 in

      let@ ic = CCIO.with_in file in

      CCIO.read_chunks_iter ~size:200 ic (fun chunk ->
          incr n_chunks;
          let chunk = Chunk.make chunk in
          (* publish *)
          Zmq.Socket.send ~block:true sock chunk);

      Log.debug (fun k -> k "sent %d chunks for %S" !n_chunks file)
    in
    List.iter read1 inputs;
    (* now close *)
    Log.debug (fun k -> k "send EOF");
    Zmq.Socket.send ~block:true sock Chunk.eof;
    ()

  (* write into file *)
  let t_write ~(ctx : Zmq.Context.t) ~into ~n_written () : unit =
    let sock = Zmq.Socket.create ctx Zmq.Socket.pull in
    let@ () = Fun.protect ~finally:(fun () -> Zmq.Socket.close sock) in
    Zmq.Socket.bind sock uri;

    let@ oc = CCIO.with_out into in

    let continue = ref true in
    let on_chunk c =
      match Chunk.view c with
      | Chunk.Eof ->
        Log.info (fun k -> k "received EOF chunk");
        continue := false
      | Chunk.C s ->
        ignore (Atomic.fetch_and_add n_written (String.length s) : int);
        output_string oc s
    in

    while !continue do
      let msg = Zmq.Socket.recv ~block:true sock in
      on_chunk msg
    done;
    ()

  let copy ~inputs ~into ~port:_ () : unit =
    let time_start = Unix.gettimeofday () in

    let ctx = Zmq.Context.create () in
    let@ () = Fun.protect ~finally:(fun () -> Zmq.Context.terminate ctx) in
    let n_written = Atomic.make 0 in

    let t_read = Thread.create (fun () -> t_read ~ctx ~inputs ()) () in
    let t_write =
      Thread.create (fun () -> t_write ~ctx ~into ~n_written ()) ()
    in

    Thread.join t_read;
    Thread.join t_write;

    let time_stop = Unix.gettimeofday () in
    Log.info (fun k ->
        k "wrote %d bytes in %.4fs (%.2fMB/s)" (Atomic.get n_written)
          (time_stop -. time_start)
          (float (Atomic.get n_written) /. 1e6 /. (time_stop -. time_start)));
    Log.info (fun k -> k "all done, exiting");
    ()
end

let () =
  Printexc.record_backtrace true;
  let debug = ref false in
  let files = ref [] in
  let port = ref 12456 in
  let zmq = ref false in
  let opts =
    [
      "-d", Arg.Set debug, " enable debug";
      "-p", Arg.Set_int port, " set port";
      "--zmq", Arg.Set zmq, " use zmq";
    ]
    |> Arg.align
  in
  Arg.parse opts (fun f -> files := f :: !files) "cp file [file+] into_file";
  Logs.set_reporter @@ Logs.format_reporter ();
  Logs.set_level ~all:true
    (Some
       (if !debug then
         Logs.Debug
       else
         Logs.Info));

  match !files with
  | into :: (_ :: _ as inputs) ->
    let inputs = List.rev inputs in
    if !zmq then
      With_zmq.copy ~inputs ~into ~port:!port ()
    else
      With_aeron.copy ~inputs ~into ~port:!port ()
  | _ -> failwith "require at least 2 arguments"
