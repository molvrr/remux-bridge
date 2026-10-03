(* A chat bridge of your own, in OCaml on Eio: every integration's chat, and every
   event, on remux's wire. A port of bridge.bend, itself a port of remux's
   byo/bridge.py.

     bridge [--port 9999]
     remux chat url ws://127.0.0.1:9999

   What to read is the [byo] table of remux's config (~/.config/remux/config.toml,
   or $REMUX_CONFIG): each key names an integration, its value what that reads.
     [byo]
     twitch = "kartths"
     youtube = "mine"        (or "<video id>")
   Tokens come from the environment; each integration's file says which.

   remux's wire (ws://127.0.0.1:<port>, docs/wire.md) gets {"line": {id, platform,
   channel, from, body}} for what is written in chat, and {"event": …} for the rest.
   Up, it acts on the engine's say and delete, and answers with a notice what went
   wrong (and what a delete did).

   Adding an integration: write <name>/logic.ml (pure) and <name>/plugin.ml, an
   Integration.S (see twitch/), and add it to integrations. *)
open Core

exception Die of int * string

let die code text = raise (Die (code, text))

(* The hub: one fiber holds every client's inbox and hands each event to all.
   ---------------------------------------------------------------------------- *)

type inbox = { events : chat_event Eio.Stream.t; mutable closed : bool }

(* The hub's mail: integrations report what Happened; each client Joins with an inbox. *)
type up = Happened of chat_event | Join of inbox

let hub (ups : up Eio.Stream.t) =
  let rec loop clients =
    match Eio.Stream.take ups with
    | Join inbox -> loop (inbox :: clients)
    | Happened e ->
        (* Pushes e to every inbox; the ones that are closed drop out. *)
        loop
          (List.filter
             (fun inbox ->
               if not inbox.closed then Eio.Stream.add inbox.events e;
               not inbox.closed)
             clients)
  in
  loop []

let join ups =
  let inbox = { events = Eio.Stream.create 256; closed = false } in
  Eio.Stream.add ups (Join inbox);
  inbox

(* A client gone: its inbox is closed, and emptied, so the hub never waits on it. *)
let leave inbox =
  inbox.closed <- true;
  let rec drain () = match Eio.Stream.take_nonblocking inbox.events with Some _ -> drain () | None -> () in
  drain ()

(* Integrations: plug starts one, and serves its posts.
   ---------------------------------------------------------------------------- *)

(* What an integration is asked: post a message, or take one down. *)
type op = Post of string | Remove of string

(* One op for an integration, and where its outcome goes. *)
type job = { op : op; reply : outcome Eio.Promise.u }

(* A running integration, as remux reaches it: by its channel. *)
type route = { channel : string; jobs : job Eio.Stream.t }

(* Starts it when the [byo] table names it: its reader, and its sender behind a
   route, which serves its posts one at a time, in order, with what it keeps. *)
let plug ~sw net (module I : Integration.S) cfg ups =
  let value = config_get cfg I.name in
  if value = "" then []
  else
    match I.setup value with
    | Error why -> die 2 ("bridge: " ^ I.name ^ ": " ^ why)
    | Ok conf ->
        let jobs = Eio.Stream.create 64 in
        let serve st { op; reply } =
          let st, o =
            match op with
            | Post body when utf8_length body > I.limit -> (st, Refused ("longer than " ^ string_of_int I.limit ^ " characters"))
            | Post body -> (
                match Net.attempt (fun () -> I.send net conf st body) with Ok r -> r | Error why -> (st, Refused why))
            | Remove id -> (
                match Net.attempt (fun () -> I.delete net conf st id) with Ok r -> r | Error why -> (st, Refused why))
          in
          Eio.Promise.resolve reply o;
          st
        in
        let rec sender st = sender (serve st (Eio.Stream.take jobs)) in
        Eio.Fiber.fork ~sw (fun () -> I.read net conf ~emit:(fun e -> Eio.Stream.add ups (Happened e)));
        Eio.Fiber.fork ~sw (fun () -> sender I.fresh);
        [ { channel = I.channel conf; jobs } ]

(* Every integration the bridge knows. One line each. *)
let integrations : (module Integration.S) list = [ (module Twitch.Plugin); (module Youtube.Plugin) ]

(* The route whose channel is channel. *)
let route routes channel = List.find_opt (fun r -> r.channel = channel) routes

(* Hands op to the integration behind route, and waits for how it ended; missing
   names what nobody answers to. *)
let ask route op missing =
  match route with
  | None -> Refused missing
  | Some r ->
      let p, reply = Eio.Promise.create () in
      Eio.Stream.add r.jobs { op; reply };
      Eio.Promise.await p

(* A client's two ways: down, what the hub pushes; up, what it asks. Either ending
   ends both. *)
let both_ways ~down ~up = Net.quietly (fun () -> Eio.Fiber.first down up)

let forever f () =
  while true do
    f ()
  done

(* remux's wire: a WebSocket server on 127.0.0.1. Lines go down as the hub pushes
   events; what comes up is read as it comes, and pings get their pong.
   ---------------------------------------------------------------------------- *)

let wire_client ~ups ~routes c =
  let inbox = join ups in
  Net.log "wire: engine connected";
  let put = Ws.text c in
  let says channel body =
    List.iter
      (fun ch ->
        let o = ask (route routes ch) (Post body) ("no chat called " ^ ch ^ " is on") in
        Option.iter put (notice_said ch o))
      (asked_targets channel (List.map (fun r -> r.channel) routes))
  in
  let asked = function
    | Ask_nothing -> ()
    | Ask_bad { about; why } -> put (notice about why false)
    | Ask_say { channel; body } -> says channel body
    | Ask_delete { channel; id } ->
        put (notice_deleted channel (ask (route routes channel) (Remove id) ("no chat called " ^ channel ^ " is on")))
  in
  let rec up () =
    match Ws.recv c with
    | Ws.Text text ->
        asked (Core.asked (Net.json_of text));
        up ()
    | Ws.Binary _ | Ws.Pong _ -> up ()
    | Ws.Closed _ -> ()
  in
  both_ways ~down:(forever (fun () -> List.iter put (frames (Eio.Stream.take inbox.events)))) ~up;
  leave inbox;
  Ws.close c;
  Net.log "wire: engine gone"

(* An upgrade within 10 s, or the connection is dropped. *)
let wire (net : Net.t) ~ups ~routes flow _addr =
  match Net.attempt (fun () -> Eio.Time.with_timeout_exn net.clock 10. (fun () -> Ws.serve ~r:(Ws.reader flow) flow)) with
  | Ok c -> wire_client ~ups ~routes c
  | Error _ -> ()

(* Start
   ----- *)

let start env o cfg =
  if o.bad_flag <> "" then
    die 2
      ("bridge: cannot use " ^ o.bad_flag
     ^ "; usage: bridge [--port 9999]; what to read is the [byo] table of the config");
  Eio.Switch.run @@ fun sw ->
  let net = Net.make env in
  let ups = Eio.Stream.create 1024 in
  let port = string_of_int o.port in
  let tcp =
    match Eio.Net.listen ~sw ~backlog:512 ~reuse_addr:true env#net (`Tcp (Eio.Net.Ipaddr.V4.loopback, o.port)) with
    | l -> l
    | exception e -> die 1 ("bridge: cannot listen on 127.0.0.1:" ^ port ^ ": " ^ Net.why e)
  in
  match List.concat_map (fun it -> plug ~sw net it cfg ups) integrations with
  | [] ->
      die 2
        "bridge: nothing to read: name an integration under [byo] in ~/.config/remux/config.toml, as twitch = \"<channel>\" or youtube = \"mine\""
  | routes ->
      let on_error e = Net.log ("bridge: " ^ Net.why e) in
      Eio.Fiber.fork ~sw (fun () -> Eio.Net.run_server ~on_error tcp (wire net ~ups ~routes));
      hub ups

let main env =
  let own = Net.getenv "REMUX_CONFIG" and home = Net.getenv "HOME" in
  let path = if own = "" then home ^ "/.config/remux/config.toml" else own in
  let text = try In_channel.with_open_bin path In_channel.input_all with Sys_error _ -> "" in
  let args = match Array.to_list Sys.argv with _ :: args -> args | [] -> [] in
  start env (opts args { port = 9999; bad_flag = "" }) (byo text)
