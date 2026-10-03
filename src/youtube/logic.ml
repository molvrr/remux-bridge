(* YouTube, pure: liveChat answers in, events out; the Data API requests that post. *)
open Core

let api = "https://www.googleapis.com/youtube/v3/"

(* Reading
   ------- *)

(* The live chat of a videos?part=liveStreamingDetails answer; "" when it has none. *)
let chat_id resp =
  j_str (j_get (j_get (j_first (j_list (j_get resp "items"))) "liveStreamingDetails") "activeLiveChatId")

(* The video and live chat of the first broadcast of a liveBroadcasts answer; ""
   for both when there is none. *)
let broadcast_chat resp =
  let b = j_first (j_list (j_get resp "items")) in
  (j_str (j_get b "id"), j_str (j_get (j_get b "snippet") "liveChatId"))

let details sn key field = j_str (j_get (j_get sn key) field)

let tip sn key =
  Tip
    {
      amount = details sn key "amountDisplayString";
      currency = details sn key "currency";
      micros = digits (details sn key "amountMicros");
    }

(* A user's name as every platform writes it: YouTube's handle without its @. *)
let name d = drop1 (j_str (j_get d "displayName")) '@'

(* A ban: timed out for banDurationSeconds when temporary, for good otherwise. *)
let banned d =
  Banned
    {
      user = name (j_get d "bannedUserDetails");
      seconds = (if j_str (j_get d "banType") = "temporary" then number (j_str (j_get d "banDurationSeconds")) 0 else 0);
    }

(* A snippet by its type, as what happened and what the viewer wrote. Messages,
   Super Chats and Stickers (tips), new members and milestones (subs), gifted
   memberships, deletions and bans; what has no shared word (a poll, the chat's end,
   members-only mode, a gift's receipt) is YouTube's own kind. *)
let said ty sn =
  match ty with
  | "" -> None
  | "textMessageEvent" -> Some (Chat, j_str (j_get sn "displayMessage"))
  | "superChatEvent" -> Some (tip sn "superChatDetails", details sn "superChatDetails" "userComment")
  | "superStickerEvent" -> Some (tip sn "superStickerDetails", "")
  | "newSponsorEvent" -> Some (Sub { months = 1; tier = details sn "newSponsorDetails" "memberLevelName" }, "")
  | "memberMilestoneChatEvent" ->
      let d = "memberMilestoneChatDetails" in
      Some (Sub { months = number (details sn d "memberMonth") 1; tier = details sn d "memberLevelName" }, details sn d "userComment")
  | "membershipGiftingEvent" ->
      let d = "membershipGiftingDetails" in
      Some (Gift { count = number (details sn d "giftMembershipsCount") 1; tier = details sn d "giftMembershipsLevelName"; to_ = "" }, "")
  | "messageDeletedEvent" -> Some (Deleted { target = details sn "messageDeletedDetails" "deletedMessageId" }, "")
  | "userBannedEvent" -> Some (banned (j_get sn "userBannedDetails"), "")
  | ty -> Some (Custom { name = "youtube." ^ ty; fields = [] }, j_str (j_get sn "displayMessage"))

(* The author's flags as the badges every platform shares. *)
let badges author =
  List.filter_map
    (fun (key, name) -> if j_is_true (j_get author key) then Some name else None)
    [ ("isChatOwner", "broadcaster"); ("isChatModerator", "moderator"); ("isChatSponsor", "member"); ("isVerified", "verified") ]

(* One liveChat/messages item as an event of the chat of video. *)
let event item video =
  let sn = j_get item "snippet" in
  Option.map
    (fun (what, body) ->
      let author = j_get item "authorDetails" in
      {
        what;
        id = j_str (j_get item "id");
        platform = "youtube";
        channel = video;
        from = or_else (name author) "?";
        body;
        badges = badges author;
        reply = "";
      })
    (said (j_str (j_get sn "type")) sn)

let events items video = List.filter_map (fun item -> event item video) items

(* A liveChat/messages answer: its events, the token to resume after it, and
   whether the chat went offline. *)
type page = { events : chat_event list; next : string; offline : bool }

let page resp video =
  {
    events = events (j_list (j_get resp "items")) video;
    next = j_str (j_get resp "nextPageToken");
    offline = j_str (j_get resp "offlineAt") <> "";
  }

(* Google streams an RPC's answers as one JSON array, each element as it comes:
   [{…}\n,{…}\n]. The whole elements of text, and what is left of the next. *)
let elements text =
  let n = String.length text in
  let rec between i acc =
    if i >= n then (List.rev acc, "")
    else if text.[i] = '{' then inside i (i + 1) 1 false acc
    else between (i + 1) acc (* [ , ] and white space *)
  and inside start i depth quoted acc =
    if i >= n then (List.rev acc, String.sub text start (n - start))
    else
      match (quoted, text.[i]) with
      | true, '\\' -> inside start (i + 2) depth true acc
      | true, '"' -> inside start (i + 1) depth false acc
      | true, _ -> inside start (i + 1) depth true acc
      | false, '"' -> inside start (i + 1) depth true acc
      | false, ('{' | '[') -> inside start (i + 1) (depth + 1) false acc
      | false, ('}' | ']') when depth = 1 -> between (i + 1) (String.sub text start (i + 1 - start) :: acc)
      | false, ('}' | ']') -> inside start (i + 1) (depth - 1) false acc
      | false, _ -> inside start (i + 1) depth false acc
  in
  between 0 []

(* key "" leaves the key out, for a request made with a token. *)
let with_key key = if key = "" then "" else "&key=" ^ url_esc key

let video_url key video = api ^ "videos?part=liveStreamingDetails&id=" ^ url_esc video ^ with_key key

(* The token's channel's broadcasts that are live now, of every kind (an event, or
   the channel's default stream). *)
let broadcasts_url = api ^ "liveBroadcasts?part=snippet&broadcastStatus=active&broadcastType=all"

(* liveChatMessages.streamList: the chat's answers, streamed, from page on. *)
let stream_url key chat page =
  api ^ "liveChat/messages/stream?liveChatId=" ^ url_esc chat ^ "&part=id%2Csnippet%2CauthorDetails"
  ^ (if page = "" then "" else "&pageToken=" ^ url_esc page)
  ^ with_key key

(* Posting, with a user token that has youtube.force-ssl
   ----------------------------------------------------- *)

let token_url = "https://oauth2.googleapis.com/token"

let refresh_form client secret refresh =
  "client_id=" ^ url_esc client ^ "&client_secret=" ^ url_esc secret ^ "&refresh_token=" ^ url_esc refresh
  ^ "&grant_type=refresh_token"

(* A refreshed access token and how many seconds it lives; Error with Google's reason. *)
let token resp =
  let t = j_str (j_get resp "access_token") in
  if t = "" then Error ("refreshing the token failed: " ^ or_else (j_str (j_get resp "error_description")) (api_why resp))
  else Ok (t, or_else (j_str (j_get resp "expires_in")) "3599")

let send_url = api ^ "liveChat/messages?part=snippet"

let body chat text =
  "{\"snippet\":{\"liveChatId\":" ^ str chat ^ ",\"type\":\"textMessageEvent\",\"textMessageDetails\":{\"messageText\":"
  ^ str text ^ "}}}"

(* A 200 from liveChat/messages insert. *)
let sent resp = Sent (j_str (j_get resp "id"))

(* DELETE liveChat/messages: a message, by id (the owner or a moderator). *)
let delete_url id = api ^ "liveChat/messages?id=" ^ url_esc id
