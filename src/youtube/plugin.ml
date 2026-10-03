(* YouTube, as an Integration: chat, memberships, gifts and Super Chats streamed
   from a live chat, and messages posted through the Data API.
     [byo] youtube = "mine"            your channel's broadcast that is live now,
                                       whichever it is (read with the token)
     [byo] youtube = "<video id>"      that video's (live, public or unlisted)
     YOUTUBE_API_KEY                   to read a video id
     YOUTUBE_ACCESS_TOKEN              to post and delete, and to read mine: a token
                                       with youtube.force-ssl of the channel's owner or
                                       a moderator (mine: of the owner), or, so
     YOUTUBE_REFRESH_TOKEN             the bridge refreshes it, a refresh token and
     YOUTUBE_CLIENT_ID, _SECRET        the OAuth client it was issued to *)
open Core
module L = Logic

type creds = { access : string; refresh : string; client_id : string; secret : string }

(* Whose chat: a video's, or the token's channel's live broadcast's. *)
type target = Video of string | Mine

type conf = { target : target; value : string; key : string; creds : creds }

(* What the sender keeps: an access token until a time (ms), and the live chat it
   posts to. *)
type keep = { token : string; until : int; chat : string }

let name = "youtube"
let limit = 200
let fresh = { token = ""; until = 0; chat = "" }

(* Setup
   ----- *)

let can c = c.access <> "" || (c.refresh <> "" && c.client_id <> "" && c.secret <> "")

let needs what =
  what ^ " needs YOUTUBE_ACCESS_TOKEN, or YOUTUBE_REFRESH_TOKEN with YOUTUBE_CLIENT_ID and YOUTUBE_CLIENT_SECRET"

let setup v =
  let value = String.trim v and key = Net.getenv "YOUTUBE_API_KEY" in
  let creds =
    {
      access = Net.getenv "YOUTUBE_ACCESS_TOKEN";
      refresh = Net.getenv "YOUTUBE_REFRESH_TOKEN";
      client_id = Net.getenv "YOUTUBE_CLIENT_ID";
      secret = Net.getenv "YOUTUBE_CLIENT_SECRET";
    }
  in
  if value = "" then Error "the value is mine, or a video id, and it is empty"
  else if value = "mine" then
    if can creds then Ok { target = Mine; value; key; creds } else Error (needs "reading mine")
  else if key = "" then Error "reading a video needs YOUTUBE_API_KEY in the environment"
  else Ok { target = Video value; value; key; creds }

let channel conf = conf.value
let sends conf = can conf.creds

(* Tokens
   ------ *)

(* A refreshed token, kept until a minute before it expires. *)
let renew net creds now keep =
  match Net.post net L.token_url Net.form (L.refresh_form creds.client_id creds.secret creds.refresh) with
  | Error why -> Error ("refreshing the token failed: " ^ why)
  | Ok { j; _ } ->
      Result.map
        (fun (token, secs) -> { keep with token; until = now + max 0 ((number secs 3599 * 1000) - 60000) })
        (L.token j)

(* A token: the one given, or one refreshed a minute before the last expires. *)
let token net creds keep now =
  if creds.refresh = "" then Ok { keep with token = creds.access; until = 0 }
  else if keep.token <> "" && now < keep.until then Ok keep
  else renew net creds now keep

(* 403 and 404: the chat ended or the key is refused. *)
let refused status = status = 403 || status = 404

(* 401 forgets the token; 403 and 404 forget the chat, which may have ended. *)
let after status keep =
  if status = 401 then { keep with token = ""; until = 0 } else if refused status then { keep with chat = "" } else keep

(* Finding the live chat
   --------------------- *)

(* The request that finds it: with the key, or "" for one made with a token. *)
let lookup_url conf key = match conf.target with Video v -> L.video_url key v | Mine -> L.broadcasts_url

(* The video and live chat its answer names; "" for the chat when there is none. *)
let live conf j = match conf.target with Video v -> (v, L.chat_id j) | Mine -> L.broadcast_chat j

let none conf =
  match conf.target with
  | Video v -> v ^ " has no live chat (is it live?)"
  | Mine -> "the channel has no broadcast live"

(* Reading: the live chat, streamed.
   ---------------------------------------------------------------------------- *)

(* How a read is made: a video's with the key; mine with the token, kept in keep. *)
let auth net conf keep =
  match conf.target with
  | Video _ -> Ok ([], conf.key)
  | Mine -> (
      match token net conf.creds !keep (Net.now_ms net) with
      | Ok k ->
          keep := k;
          Ok (Net.bearer k.token, "")
      | Error why -> Error why)

let rec find net conf keep ~emit =
  match auth net conf keep with
  | Error why ->
      Net.debug ("youtube: " ^ why);
      Net.sleep_ms net 30000;
      find net conf keep ~emit
  | Ok (headers, key) -> (
      match Net.get net (lookup_url conf key) headers with
      | Error why ->
          Net.debug ("youtube: looking up the live chat failed: " ^ why);
          Net.sleep_ms net 10000;
          find net conf keep ~emit
      | Ok { status = 200; j } -> start net conf keep (live conf j) ~emit
      | Ok { status; j } ->
          Net.debug ("youtube: looking up the live chat failed: " ^ string_of_int status ^ " " ^ api_why j);
          again net conf keep status ~emit)

and again net conf keep status ~emit =
  keep := after status !keep;
  Net.sleep_ms net (if refused status then 30000 else 10000);
  find net conf keep ~emit

and start net conf keep (video, chat) ~emit =
  (* No live chat yet: nothing is live. *)
  if chat = "" then (
    Net.debug ("youtube: " ^ none conf);
    Net.sleep_ms net 15000;
    find net conf keep ~emit)
  else (
    Net.log ("youtube: connected to the chat of " ^ video);
    follow net conf keep chat "" true ~emit)

(* The chat's answers as they come, resumed from the last page token whenever the
   stream ends. A fresh stream's first answer is the chat before the bridge came:
   it is skipped. A stream silent for 5 minutes is dialed again. *)
and follow net conf keep chat page skip ~emit =
  let page = ref page and skip = ref skip and offline = ref false and rest = ref "" in
  let answer text =
    let j = Net.json_of text in
    Net.debug ("youtube: stream answer: " ^ string_of_int (List.length (j_list (j_get j "items"))) ^ " items" ^ if !skip then " (history, skipped)" else "");
    if j_get j "error" <> JNull then failwith (api_why j);
    let p = L.page j conf.value in
    if not !skip then List.iter emit p.events;
    skip := false;
    page := or_else p.next !page;
    if p.offline then (
      offline := true;
      failwith "the chat went offline")
  in
  let each piece =
    Net.debug ("youtube: stream got " ^ string_of_int (String.length piece) ^ " bytes");
    let whole, left = L.elements (!rest ^ piece) in
    rest := left;
    List.iter answer whole
  in
  let streamed =
    Result.bind (auth net conf keep) (fun (headers, key) ->
        Net.stream net (L.stream_url key chat !page) headers ~idle:300. each)
  in
  match streamed with
  | _ when !offline ->
      Net.debug "youtube: the chat went offline";
      again net conf keep 404 ~emit
  | Ok 200 ->
      Net.debug "youtube: the stream ended";
      Net.sleep_ms net 1000;
      follow net conf keep chat !page !skip ~emit
  | Error why ->
      Net.debug ("youtube: the stream failed: " ^ why);
      Net.sleep_ms net 10000;
      follow net conf keep chat !page !skip ~emit
  | Ok status ->
      Net.debug ("youtube: the stream was refused: " ^ string_of_int status);
      again net conf keep status ~emit

let read net conf ~emit = find net conf (ref fresh) ~emit

(* Posting: liveChatMessages.insert.
   ---------------------------------------------------------------------------- *)

(* The live chat: found once, then kept. *)
let chat net conf keep =
  if keep.chat <> "" then Ok keep
  else
    match Net.get net (lookup_url conf "") (Net.bearer keep.token) with
    | Error why -> Error ("looking up the live chat failed: " ^ why)
    | Ok { status; j } ->
        let _, c = live conf j in
        if c <> "" then Ok { keep with chat = c }
        else if status = 200 then Error (none conf)
        else Error ("looking up the live chat failed: " ^ string_of_int status ^ " " ^ api_why j)

let send net conf keep body =
  if not (can conf.creds) then (keep, Refused (needs "posting"))
  else
    match token net conf.creds keep (Net.now_ms net) with
    | Error why -> (keep, Refused why)
    | Ok k -> (
        match chat net conf k with
        | Error why -> ({ k with chat = "" }, Refused why)
        | Ok k ->
            let status, o = Net.posted (Net.post net L.send_url (Net.json_headers k.token) (L.body k.chat body)) L.sent in
            (after status k, o))

(* Deleting: liveChatMessages.delete. *)
let delete net conf keep id =
  if not (can conf.creds) then (keep, Refused (needs "deleting"))
  else
    match token net conf.creds keep (Net.now_ms net) with
    | Error why -> (keep, Refused why)
    | Ok k ->
        let status, o = Net.deleted (Net.delete net (L.delete_url id) (Net.bearer k.token)) id in
        (after status k, o)
