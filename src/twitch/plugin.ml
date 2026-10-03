(* Twitch, as an Integration: chat, subs, gifts, cheers and raids read over IRC as a
   guest; with a token, follows, redemptions, hype trains, polls, predictions, the
   stream going up or down and raids out over EventSub; and messages posted and
   deleted through Helix.
     [byo] twitch = "<channel>"
   To post, a user token with user:write:chat; to delete, with
   moderator:manage:chat_messages too. Either one that lasts until it expires:
     TWITCH_CHAT_TOKEN
   or one the bridge renews before it expires, from what `twitch token -u` printed:
     TWITCH_REFRESH_TOKEN, and the app it was issued to: TWITCH_CLIENT_ID, TWITCH_CLIENT_SECRET
   EventSub subscribes to what the token's scopes allow of: moderator:read:followers, channel:read:redemptions, channel:read:hype_train,
   channel:read:polls, channel:read:predictions (the stream and raids need none). *)
open Core
module L = Logic
module E = Eventsub

(* A refresh token and the app it was issued to. *)
type creds = { refresh : string; client_id : string; secret : string }

type conf = { channel : string; given : string; creds : creds }

(* Who posts, and where: looked up once, then kept. *)
type auth = { app : string; sender : string; broadcaster : string; scopes : string list }

(* What the sender keeps: the token in use until a time (ms; 0 for one that is not
   renewed), the refresh token to renew it with next, and who posts. *)
type keep = { token : string; next_refresh : string; until : int; auth : auth option }

let name = "twitch"
let limit = 500
let fresh = { token = ""; next_refresh = ""; until = 0; auth = None }

(* Setup
   ----- *)

let renews c = c.refresh <> "" && c.client_id <> "" && c.secret <> ""

let setup v =
  let channel = L.channel v in
  if channel = "" then Error "the value is a channel name, and it is empty"
  else
    Ok
      {
        channel;
        given = L.token (Net.getenv "TWITCH_CHAT_TOKEN");
        creds =
          {
            refresh = Net.getenv "TWITCH_REFRESH_TOKEN";
            client_id = Net.getenv "TWITCH_CLIENT_ID";
            secret = Net.getenv "TWITCH_CLIENT_SECRET";
          };
      }

let channel conf = conf.channel
let sends conf = conf.given <> "" || renews conf.creds

(* Reading: IRC over TLS, as a read-only guest. Every failure waits 5 s and dials again.
   ---------------------------------------------------------------------------- *)

let host = "irc.chat.twitch.tv"

(* One connection: hello, then every line, until it fails. A connection silent for
   6 minutes, past Twitch's pings, is gone. *)
let irc (net : Net.t) conf ~emit =
  Eio.Switch.run @@ fun sw ->
  let flow = Net.connect_tls net ~sw host 6697 in
  Eio.Flow.copy_string (L.hello conf.channel (Random.bits ())) flow;
  Net.log ("twitch: connected to #" ^ conf.channel);
  let r = Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024) in
  let rec loop () =
    let line = Eio.Time.with_timeout_exn net.clock 360. (fun () -> Eio.Buf_read.line r) in
    (match L.irc line ("twitch-" ^ string_of_int (Net.now_ms net)) with
    | L.Ping token -> Eio.Flow.copy_string (L.pong token) flow
    | L.Got e -> emit e
    | L.Other -> ());
    loop ()
  in
  loop ()

let rec dial net conf ~emit =
  ignore (Net.attempt (fun () -> irc net conf ~emit));
  Net.sleep_ms net 5000;
  dial net conf ~emit

(* Posting and deleting, through Helix.
   ---------------------------------------------------------------------------- *)

let helix token app = Net.json_headers token @ [ ("client-id", app) ]

(* The token: TWITCH_CHAT_TOKEN as it is, or a renewed one, renewed again a minute
   before it expires. *)

let renew net creds refresh now auth =
  match Net.post net L.renew_url Net.form (L.renew_form creds.client_id creds.secret refresh) with
  | Error why -> Error ("renewing TWITCH_REFRESH_TOKEN failed: " ^ why)
  | Ok { j; _ } ->
      Result.map
        (fun (r : L.renewal) ->
          let life = number r.secs 14400 in
          { token = r.r_token; next_refresh = r.r_refresh; until = now + max 0 ((life * 1000) - 60000); auth })
        (L.renewed j refresh)

(* The refresh token to renew with: the last one Twitch handed, else the one given. *)
let token net conf keep now need =
  if not (renews conf.creds) then
    if conf.given = "" then Error need else Ok { token = conf.given; next_refresh = ""; until = 0; auth = keep.auth }
  else if keep.token <> "" && now < keep.until then Ok keep
  else renew net conf.creds (or_else keep.next_refresh conf.creds.refresh) now keep.auth

(* Who posts, and where: the token's user and app (GET oauth2/validate), and the
   channel's user id, asked once and kept. *)
let who net keep channel =
  match keep.auth with
  | Some _ -> Ok keep
  | None -> (
      let me =
        match Net.get net L.validate_url [ ("authorization", "OAuth " ^ keep.token) ] with
        | Error why -> Error ("validating the Twitch token failed: " ^ why)
        | Ok { j; _ } -> L.me j
      in
      match me with
      | Error why -> Error why
      | Ok me -> (
          match Net.get net (L.users_url channel) (helix keep.token me.client) with
          | Error why -> Error ("looking up the channel failed: " ^ why)
          | Ok { status; j } ->
              let b = L.user_id j in
              if b = "" then
                Error
                  (if status = 200 then "no Twitch channel by that name"
                   else "looking up the channel failed: " ^ string_of_int status ^ " " ^ api_why j)
              else Ok { keep with auth = Some { app = me.client; sender = me.user; broadcaster = b; scopes = me.scopes } }))

(* A token, renewed if due, and who posts where: all an act needs. *)
let ready net conf keep need = Result.bind (token net conf keep (Net.now_ms net) need) (fun k -> who net k conf.channel)

(* A 401 forgets the token and who we are, so the next act renews or looks again. *)
let after status keep = if status = 401 then { keep with token = ""; until = 0; auth = None } else keep

(* An act on the platform: post (text is a body) or delete (text is an id), each
   after its own scope. *)
let act net conf keep ~delete text =
  let need =
    if delete then
      "deleting needs TWITCH_CHAT_TOKEN, or TWITCH_REFRESH_TOKEN with TWITCH_CLIENT_ID and TWITCH_CLIENT_SECRET, for a user token with moderator:manage:chat_messages"
    else
      "posting needs TWITCH_CHAT_TOKEN, or TWITCH_REFRESH_TOKEN with TWITCH_CLIENT_ID and TWITCH_CLIENT_SECRET, for a user token with user:write:chat"
  in
  match ready net conf keep need with
  | Error why -> (keep, Refused why)
  | Ok ({ auth = None; _ } as k) -> (k, Refused "nobody to act as")
  | Ok ({ auth = Some a; _ } as k) -> (
      match L.need a.scopes (if delete then "moderator:manage:chat_messages" else "user:write:chat") with
      | Some why -> (k, Refused why)
      | None ->
          let status, o =
            if delete then Net.deleted (Net.delete net (L.delete_url a.broadcaster a.sender text) (helix k.token a.app)) text
            else Net.posted (Net.post net L.messages_url (helix k.token a.app) (L.body a.broadcaster a.sender text)) L.sent
          in
          (after status k, o))

let send net conf keep body = act net conf keep ~delete:false body
let delete net conf keep id = act net conf keep ~delete:true id

(* EventSub: what IRC cannot see, over a WebSocket session, subscribed with the
   posting token (renewed on its own). A session silent for longer than its
   keepalive is gone, and dialed again.
   ---------------------------------------------------------------------------- *)

(* Subscribes the session to what the token's scopes allow, as the token's user;
   and whether anything was. *)
let subscribe net conf keep session =
  match
    ready net conf keep "EventSub needs TWITCH_CHAT_TOKEN, or TWITCH_REFRESH_TOKEN with TWITCH_CLIENT_ID and TWITCH_CLIENT_SECRET"
  with
  | Error _ -> (keep, false)
  | Ok ({ auth = None; _ } as k) -> (k, false)
  | Ok ({ auth = Some a; _ } as k) ->
      let plan = E.plan a.broadcaster a.sender a.scopes in
      (* Every one is asked for, so no short-circuiting. *)
      let ok =
        List.filter
          (fun (w : E.want) ->
            match Net.post net E.subscriptions_url (helix k.token a.app) (E.body w session) with
            | Ok { status = 202; _ } -> true
            | _ -> false)
          plan.ok
      in
      (k, ok <> [])

(* How a session ended: dial again after a while, move to a new one, or rest, as
   nothing was subscribed. *)
type ended = Retry | Move of string | Idle

(* One session at url. A fresh one is subscribed at its welcome; one moved to by a
   reconnect keeps what it had. *)
let session (net : Net.t) conf keep url fresh ~emit =
  let host, path = E.ws_url url in
  Eio.Switch.run @@ fun sw ->
  let flow = Net.connect_tls net ~sw host 443 in
  let c = Eio.Time.with_timeout_exn net.clock 10. (fun () -> Ws.connect ~r:(Ws.reader flow) ~host ~path flow) in
  let rec loop fresh wait =
    match Eio.Time.with_timeout net.clock (float_of_int wait) (fun () -> Ok (Ws.recv c)) with
    | Error `Timeout ->
        Ws.close c;
        Retry
    | Ok (Ws.Text data) -> (
        match E.es (Net.json_of data) with
        | E.Welcome { session; keepalive } ->
            let wait = keepalive + 10 in
            if not fresh then loop false wait
            else
              let k, ok = subscribe net conf !keep session in
              keep := k;
              (* A session with nothing subscribed is closed by Twitch within seconds:
                 rather than dial again at once, forever, the bridge waits a while (a
                 token may be renewed or replaced meanwhile). *)
              if ok then loop false wait
              else (
                Ws.close c;
                Idle)
        | E.Keepalive | E.Unknown | E.Note None -> loop fresh wait
        | E.Note (Some e) ->
            emit e;
            loop fresh wait
        | E.Reconnect url ->
            Ws.close c;
            Move url
        | E.Revoked _ -> loop fresh wait)
    | Ok (Ws.Binary _ | Ws.Pong _) -> loop fresh wait
    | Ok (Ws.Closed _) ->
        Ws.close c;
        Retry
  in
  loop fresh 40

let rec eventsub net conf keep url fresh ~emit =
  match Net.attempt (fun () -> session net conf keep url fresh ~emit) with
  | Ok (Move url) -> eventsub net conf keep url false ~emit
  | Ok Idle ->
      Net.sleep_ms net 300000;
      eventsub net conf keep E.base_url true ~emit
  | Ok Retry | Error _ ->
      Net.sleep_ms net 5000;
      eventsub net conf keep E.base_url true ~emit

(* Reading: IRC always; EventSub too when there is a token to subscribe with. *)
let read net conf ~emit =
  if sends conf then Eio.Fiber.both (fun () -> eventsub net conf (ref fresh) E.base_url true ~emit) (fun () -> dial net conf ~emit)
  else dial net conf ~emit
