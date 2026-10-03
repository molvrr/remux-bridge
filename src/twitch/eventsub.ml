(* Twitch EventSub, pure: what the bridge subscribes to over a WebSocket session,
   and what each message means. EventSub brings what IRC cannot see: follows,
   redemptions that ask for no text, hype trains, polls, predictions, the stream
   going up or down, and raids out. Subs, cheers and raids in stay on IRC, so
   nothing is said twice. *)
open Core

let base_url = "wss://eventsub.wss.twitch.tv/ws?keepalive_timeout_seconds=30"
let subscriptions_url = "https://api.twitch.tv/helix/eventsub/subscriptions"

(* A wss:// URL as a host and the path after it, as a WebSocket client asks. *)
let ws_url url =
  let host, path = cut (drop1 (snd (cut url '/')) '/') '/' in
  (host, "/" ^ path)

(* Subscribing
   ----------- *)

(* One subscription: its topic (EventSub's type) and version, its condition (a JSON
   object), and the scopes that allow it, any one of them; none needs no scope. *)
type want = { topic : string; version : string; condition : string; scopes : string list }

let cond_b b = "{\"broadcaster_user_id\":" ^ str b ^ "}"

(* Everything the bridge subscribes to, for the channel b, as the token's user m. *)
let wants b m =
  let w topic version condition scopes = { topic; version; condition; scopes } in
  let redemptions = [ "channel:read:redemptions"; "channel:manage:redemptions" ]
  and hype = [ "channel:read:hype_train" ]
  and polls = [ "channel:read:polls"; "channel:manage:polls" ]
  and predictions = [ "channel:read:predictions"; "channel:manage:predictions" ] in
  [
    w "channel.follow" "2"
      ("{\"broadcaster_user_id\":" ^ str b ^ ",\"moderator_user_id\":" ^ str m ^ "}")
      [ "moderator:read:followers"; "moderator:manage:followers" ];
    w "channel.channel_points_custom_reward_redemption.add" "1" (cond_b b) redemptions;
    w "channel.hype_train.begin" "1" (cond_b b) hype;
    w "channel.hype_train.progress" "1" (cond_b b) hype;
    w "channel.hype_train.end" "1" (cond_b b) hype;
    w "channel.poll.begin" "1" (cond_b b) polls;
    w "channel.poll.end" "1" (cond_b b) polls;
    w "channel.prediction.begin" "1" (cond_b b) predictions;
    w "channel.prediction.lock" "1" (cond_b b) predictions;
    w "channel.prediction.end" "1" (cond_b b) predictions;
    w "stream.online" "1" (cond_b b) [];
    w "stream.offline" "1" (cond_b b) [];
    w "channel.raid" "1" ("{\"from_broadcaster_user_id\":" ^ str b ^ "}") [];
  ]

(* Whether a token with have may make w. *)
let allowed w have = w.scopes = [] || List.exists (fun s -> List.mem s have) w.scopes

(* What to subscribe to, and what is left out for a scope the token lacks. *)
type plan = { ok : want list; skipped : string list }

let plan b m have =
  let ok, no = List.partition (fun w -> allowed w have) (wants b m) in
  { ok; skipped = List.map (fun w -> w.topic ^ " (needs " ^ String.concat " or " w.scopes ^ ")") no }

(* POST helix/eventsub/subscriptions: w, delivered to the session. *)
let body w session =
  "{\"type\":" ^ str w.topic ^ ",\"version\":" ^ str w.version ^ ",\"condition\":" ^ w.condition
  ^ ",\"transport\":{\"method\":\"websocket\",\"session_id\":" ^ str session ^ "}}"

(* Messages
   -------- *)

(* What one message on the session means.
     Welcome    the session is up: subscribe to it within 10 s; silent for longer
                than keepalive seconds, it is gone
     Reconnect  move to url; the subscriptions go along
     Revoked    a subscription Twitch ended, and why *)
type es =
  | Welcome of { session : string; keepalive : int }
  | Keepalive
  | Note of chat_event option
  | Reconnect of string
  | Revoked of { topic : string; status : string }
  | Unknown

let str2 j k1 k2 = j_str (j_get (j_get j k1) k2)

(* A list of objects as "a|b", each by key. *)
let joined xs key = String.concat "|" (List.map (fun x -> j_str (j_get x key)) xs)

(* As "a:12|b:3": each title, and its count by key. *)
let tally xs key = String.concat "|" (List.map (fun x -> j_str (j_get x "title") ^ ":" ^ j_str (j_get x key)) xs)

(* The title of the outcome whose id is id. *)
let winner xs id =
  List.find_map (fun x -> if j_str (j_get x "id") = id then Some (j_str (j_get x "title")) else None) xs
  |> Option.value ~default:""

let said what id channel from body = Some { what; id; platform = "twitch"; channel; from; body; badges = []; reply = "" }
let custom name fields id channel from body = said (Custom { name = "twitch." ^ name; fields }) id channel from body

(* A redemption that asks for text arrives on IRC with it; only the rest come from here. *)
let redemption ev id channel =
  if j_str (j_get ev "user_input") <> "" then None
  else
    custom "redemption"
      [ ("reward", str2 ev "reward" "id"); ("title", str2 ev "reward" "title"); ("cost", str2 ev "reward" "cost") ]
      id channel (j_str (j_get ev "user_name")) ""

let field ev k = (k, j_str (j_get ev k))

let hype kind ev id channel =
  custom ("hype_train." ^ kind) [ field ev "level"; field ev "total"; field ev "progress"; field ev "goal" ] id channel "" ""

let poll kind ev id channel =
  let choices = j_list (j_get ev "choices") in
  custom ("poll." ^ kind)
    [ field ev "title"; field ev "status"; ("choices", if kind = "end" then tally choices "votes" else joined choices "title") ]
    id channel "" ""

let prediction kind ev id channel =
  let outcomes = j_list (j_get ev "outcomes") in
  custom ("prediction." ^ kind)
    [
      field ev "title";
      ("outcomes", joined outcomes "title");
      ("winner", winner outcomes (j_str (j_get ev "winning_outcome_id")));
    ]
    id channel "" ""

(* A notification of type ty, as an event of the chat; None for what IRC says. *)
let note ty ev id =
  let channel = j_str (j_get ev "broadcaster_user_login") in
  let after prefix = drop ty (String.length prefix) in
  if ty = "channel.follow" then said Follow id channel (j_str (j_get ev "user_name")) ""
  else if ty = "channel.channel_points_custom_reward_redemption.add" then redemption ev id channel
  else if starts_with ty ~prefix:"channel.hype_train." then hype (after "channel.hype_train.") ev id channel
  else if starts_with ty ~prefix:"channel.poll." then poll (after "channel.poll.") ev id channel
  else if starts_with ty ~prefix:"channel.prediction." then prediction (after "channel.prediction.") ev id channel
  else if ty = "stream.online" then custom "stream.online" [ field ev "type" ] id channel "" ""
  else if ty = "stream.offline" then custom "stream.offline" [] id channel "" ""
  else if ty = "channel.raid" then
    custom "raid.out"
      [ ("to", j_str (j_get ev "to_broadcaster_user_name")); field ev "viewers" ]
      id (j_str (j_get ev "from_broadcaster_user_login")) "" ""
  else custom ty [] id channel "" ""

(* One message on the session, as parsed JSON. *)
let es msg =
  let meta = j_get msg "metadata" and payload = j_get msg "payload" in
  match j_str (j_get meta "message_type") with
  | "session_welcome" ->
      Welcome { session = str2 payload "session" "id"; keepalive = number (str2 payload "session" "keepalive_timeout_seconds") 10 }
  | "session_keepalive" -> Keepalive
  | "notification" ->
      Note (note (j_str (j_get meta "subscription_type")) (j_get payload "event") (j_str (j_get meta "message_id")))
  | "session_reconnect" -> Reconnect (str2 payload "session" "reconnect_url")
  | "revocation" -> Revoked { topic = str2 payload "subscription" "type"; status = str2 payload "subscription" "status" }
  | _ -> Unknown
