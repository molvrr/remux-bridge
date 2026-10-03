(* Twitch, pure: IRC lines in, events out; the Helix requests that post a message. *)
open Core

(* IRC
   --- *)

(* An IRCv3 tag value: \s is a space, \: a semicolon, \\ a backslash, \r and \n. *)
let unescape s =
  let b = Buffer.create (String.length s) in
  let esc = ref false in
  String.iter
    (fun c ->
      if !esc then (
        esc := false;
        Buffer.add_char b (match c with 's' -> ' ' | ':' -> ';' | 'r' -> '\r' | 'n' -> '\n' | c -> c))
      else if c = '\\' then esc := true
      else Buffer.add_char b c)
    s;
  Buffer.contents b

(* The value of key in "k1=v1;k2=v2", unescaped; "" when it is not there. *)
let tag tags key =
  String.split_on_char ';' tags
  |> List.find_map (fun kv -> let k, v = cut kv '=' in if k = key then Some (unescape v) else None)
  |> Option.value ~default:""

(* What one IRC line means to the bridge. *)
type irc = Ping of string | Got of chat_event | Other

(* A badge as a word every platform shares; "" for the rest (bits, predictions, …). *)
let badge = function
  | "broadcaster" -> "broadcaster"
  | "moderator" -> "moderator"
  | "vip" -> "vip"
  | "subscriber" | "founder" -> "member"
  | "partner" -> "verified"
  | _ -> ""

(* The badges tag ("broadcaster/1,subscriber/12"), and first for a first message. *)
let badges tags =
  let bs = String.split_on_char ',' (tag tags "badges") |> List.map (fun x -> badge (fst (cut x '/'))) |> List.filter (( <> ) "") in
  if tag tags "first-msg" = "1" then bs @ [ "first" ] else bs

let event what tags fallback channel from body =
  Got
    {
      what;
      id = or_else (tag tags "id") fallback;
      platform = "twitch";
      channel;
      from;
      body;
      badges = badges tags;
      reply = tag tags "reply-parent-msg-id";
    }

(* A chat message. With bits it is a cheer, a tip in bits; with a custom reward, a
   channel-points redemption; with a msg-id, Twitch's own kind (a highlighted
   message, a Power-up). *)
let privmsg_what tags =
  let bits = tag tags "bits" and reward = tag tags "custom-reward-id" and id = tag tags "msg-id" in
  if bits <> "" then Tip { amount = bits; currency = "bits"; micros = digits bits ^ "000000" }
  else if reward <> "" then Custom { name = "twitch.redemption"; fields = [ ("reward", reward) ] }
  else if id <> "" then Custom { name = "twitch." ^ id; fields = [] }
  else Chat

let privmsg tags nick channel body fallback =
  event (privmsg_what tags) tags fallback channel (or_else (tag tags "display-name") nick) body

(* The msg-param-* tags, without that prefix: the fields of Twitch's own kinds. *)
let params tags =
  String.split_on_char ';' tags
  |> List.filter_map (fun kv ->
         if starts_with kv ~prefix:"msg-param-" then
           let k, v = cut kv '=' in
           Some (drop k 10, unescape v)
         else None)

let is_sub = function "sub" | "resub" | "giftpaidupgrade" | "anongiftpaidupgrade" | "primepaidupgrade" -> true | _ -> false

(* A community gift arrives as one submysterygift, then a subgift per viewer that
   names it: those are left out, so the gift counts once. A gifted or Prime sub
   turned paid is a sub. What has no shared word (an announcement, a watch streak,
   a charity donation, …) is Twitch's own kind, with its fields. *)
let notice_what id tags =
  let tier = tag tags "msg-param-sub-plan" in
  if id = "" then None
  else if is_sub id then Some (Sub { months = number (tag tags "msg-param-cumulative-months") 1; tier })
  else if id = "subgift" then
    if tag tags "msg-param-community-gift-id" = "" then
      Some (Gift { count = 1; tier; to_ = tag tags "msg-param-recipient-display-name" })
    else None
  else if id = "submysterygift" then Some (Gift { count = number (tag tags "msg-param-mass-gift-count") 1; tier; to_ = "" })
  else if id = "raid" then Some (Raid { viewers = number (tag tags "msg-param-viewerCount") 0 })
  else Some (Custom { name = "twitch." ^ id; fields = params tags })

(* USERNOTICE: subs, resubs, gifts and raids, with the viewer's message if any. *)
let usernotice tags channel body fallback =
  match notice_what (tag tags "msg-id") tags with
  | None -> Other
  | Some what -> event what tags fallback channel (or_else (tag tags "display-name") (tag tags "login")) body

(* CLEARMSG: a message deleted; body is its text. *)
let clearmsg tags channel body fallback =
  event (Deleted { target = tag tags "target-msg-id" }) tags fallback channel (tag tags "login") body

(* CLEARCHAT: with a user, a ban (no ban-duration) or a timeout; without, the whole chat. *)
let clearchat tags channel user fallback =
  let what = if user = "" then Cleared else Banned { user; seconds = number (tag tags "ban-duration") 0 } in
  event what tags fallback channel "" ""

let command cmd tags nick channel body fallback =
  match cmd with
  | "PRIVMSG" -> privmsg tags nick channel body fallback
  | "USERNOTICE" -> usernotice tags channel body fallback
  | "CLEARMSG" -> clearmsg tags channel body fallback
  | "CLEARCHAT" -> clearchat tags channel body fallback
  | _ -> Other

(* ":nick!user@host COMMAND #channel :body" *)
let message tags rest fallback =
  let prefix, rest = cut (drop1 rest ':') ' ' in
  let cmd, rest = cut rest ' ' in
  let channel, body = cut rest ' ' in
  command cmd tags (fst (cut prefix '!')) (drop1 channel '#') (drop1 body ':') fallback

let irc_msg tags rest fallback =
  if starts_with rest ~prefix:"PING" then Ping (snd (cut rest ' ')) else message tags rest fallback

(* One line from Twitch, without its "\r\n". fallback is the id of an event whose
   tags name none. *)
let irc raw fallback =
  if starts_with raw ~prefix:"@" then
    let tags, rest = cut (drop1 raw '@') ' ' in
    irc_msg tags rest fallback
  else irc_msg "" raw fallback

let pong token = "PONG " ^ token ^ "\r\n"

(* A Twitch channel as IRC wants it: lowercase, without its '#'. *)
let channel s = String.lowercase_ascii (drop1 (String.trim s) '#')

(* The anonymous login: justinfan and a number are Twitch's read-only guest.
   twitch.tv/commands brings USERNOTICE, where subs, gifts and raids are. *)
let hello channel n =
  let nick = (n mod 90000) + 10000 in
  "CAP REQ :twitch.tv/tags twitch.tv/commands\r\nNICK justinfan" ^ string_of_int nick ^ "\r\nJOIN #" ^ channel ^ "\r\n"

(* Helix, with a user token
   ------------------------ *)

(* A token without the "oauth:" chat clients put before it. *)
let token t = if starts_with t ~prefix:"oauth:" then drop t 6 else t

(* Who a token is (GET id.twitch.tv/oauth2/validate). *)
type me = { client : string; user : string; scopes : string list }

(* The client, user and scopes of a validate answer, or why the token is no good. *)
let me resp =
  let client = j_str (j_get resp "client_id") and user = j_str (j_get resp "user_id") in
  if client = "" || user = "" then Error "TWITCH_CHAT_TOKEN is not a valid user token"
  else Ok { client; user; scopes = List.map j_str (j_list (j_get resp "scopes")) }

(* Why a token with scopes cannot do what needs scope; None when it can. Posting
   needs user:write:chat; deleting, moderator:manage:chat_messages. *)
let need xs scope = if List.mem scope xs then None else Some ("TWITCH_CHAT_TOKEN lacks the " ^ scope ^ " scope")

(* The id of the first user of a GET helix/users answer. *)
let user_id resp = j_str (j_get (j_first (j_list (j_get resp "data"))) "id")

let users_url channel = "https://api.twitch.tv/helix/users?login=" ^ url_esc channel
let messages_url = "https://api.twitch.tv/helix/chat/messages"
let validate_url = "https://id.twitch.tv/oauth2/validate"

let body broadcaster sender text =
  "{\"broadcaster_id\":" ^ str broadcaster ^ ",\"sender_id\":" ^ str sender ^ ",\"message\":" ^ str text ^ "}"

(* A 200 from POST helix/chat/messages: sent, or dropped with Twitch's reason. *)
let sent resp =
  let first = j_first (j_list (j_get resp "data")) in
  if j_is_true (j_get first "is_sent") then Sent (j_str (j_get first "message_id"))
  else Refused (or_else (j_str (j_get (j_get first "drop_reason") "message")) "Twitch did not send it")

(* DELETE helix/moderation/chat: a message, by id, taken down by moderator. *)
let delete_url broadcaster moderator id =
  "https://api.twitch.tv/helix/moderation/chat?broadcaster_id=" ^ url_esc broadcaster ^ "&moderator_id="
  ^ url_esc moderator ^ "&message_id=" ^ url_esc id

(* Renewal: POST id.twitch.tv/oauth2/token with a refresh token
   ------------------------------------------------------------ *)

let renew_url = "https://id.twitch.tv/oauth2/token"

let renew_form client secret refresh =
  "client_id=" ^ url_esc client ^ "&client_secret=" ^ url_esc secret ^ "&refresh_token=" ^ url_esc refresh
  ^ "&grant_type=refresh_token"

(* A renewed token, the refresh token to use next (Twitch may hand a new one), and
   its life in seconds. *)
type renewal = { r_token : string; r_refresh : string; secs : string }

(* A renewal answer; old is the refresh token that asked, kept if none comes back. *)
let renewed resp old =
  let t = j_str (j_get resp "access_token") in
  if t = "" then Error ("renewing TWITCH_REFRESH_TOKEN failed: " ^ or_else (api_why resp) "no reason given")
  else
    Ok
      {
        r_token = t;
        r_refresh = or_else (j_str (j_get resp "refresh_token")) old;
        secs = or_else (j_str (j_get resp "expires_in")) "14400";
      }
