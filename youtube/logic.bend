# YouTube, pure: liveChat answers in, events out; the Data API requests that post.
import Base
import ../core.bend as C

def api() -> String:
  "https://www.googleapis.com/youtube/v3/"

# Reading
# -------

# The live chat of a videos?part=liveStreamingDetails answer; "" when it has none.
def chat_id(resp: C.J) -> String:
  C.j.str(C.j.get(C.j.get(C.j.first(C.j.list(C.j.get(resp, "items"))), "liveStreamingDetails"), "activeLiveChatId"))

# What happened, and what the viewer wrote.
type Said is Data:
  Said{what: C.What, body: String}

def details(+sn: C.J, +key: String, +field: String) -> String:
  C.j.str(C.j.get(C.j.get(sn, key), field))

def tip(+sn: C.J, +key: String) -> C.What:
  C.Tip{details(sn, key, "amountDisplayString"), details(sn, key, "currency"), C.digits(details(sn, key, "amountMicros"))}

# A ban: timed out for banDurationSeconds when temporary, for good otherwise.
def banned(+d: C.J) -> C.What:
  C.Banned{C.j.str(C.j.get(C.j.get(d, "bannedUserDetails"), "displayName")),
    Bool.pick(U32, String.eq(C.j.str(C.j.get(d, "banType")), "temporary"), C.number(C.j.str(C.j.get(d, "banDurationSeconds")), 0), 0)}

# A snippet by its type. Messages, Super Chats and Stickers (tips), new members and
# milestones (subs), gifted memberships, deletions and bans; what has no shared word
# (a poll, the chat's end, members-only mode, a gift's receipt) is YouTube's own kind.
def said(+ty: String, +sn: C.J) -> Maybe<&2, Said>:
  Bool.pick(Maybe<&2, Said>, String.is_empty(ty), None{},
  Bool.pick(Maybe<&2, Said>, String.eq(ty, "textMessageEvent"), Some{Said{C.Chat{}, C.j.str(C.j.get(sn, "displayMessage"))}},
    Bool.pick(Maybe<&2, Said>, String.eq(ty, "superChatEvent"), Some{Said{tip(sn, "superChatDetails"), details(sn, "superChatDetails", "userComment")}},
      Bool.pick(Maybe<&2, Said>, String.eq(ty, "superStickerEvent"), Some{Said{tip(sn, "superStickerDetails"), ""}},
        Bool.pick(Maybe<&2, Said>, String.eq(ty, "newSponsorEvent"), Some{Said{C.Sub{1, details(sn, "newSponsorDetails", "memberLevelName")}, ""}},
          Bool.pick(Maybe<&2, Said>, String.eq(ty, "memberMilestoneChatEvent"), Some{Said{C.Sub{C.number(details(sn, "memberMilestoneChatDetails", "memberMonth"), 1), details(sn, "memberMilestoneChatDetails", "memberLevelName")}, details(sn, "memberMilestoneChatDetails", "userComment")}},
            Bool.pick(Maybe<&2, Said>, String.eq(ty, "membershipGiftingEvent"), Some{Said{C.Gift{C.number(details(sn, "membershipGiftingDetails", "giftMembershipsCount"), 1), details(sn, "membershipGiftingDetails", "giftMembershipsLevelName"), ""}, ""}},
              Bool.pick(Maybe<&2, Said>, String.eq(ty, "messageDeletedEvent"), Some{Said{C.Deleted{details(sn, "messageDeletedDetails", "deletedMessageId")}, ""}},
                Bool.pick(Maybe<&2, Said>, String.eq(ty, "userBannedEvent"), Some{Said{banned(C.j.get(sn, "userBannedDetails")), ""}},
                  Some{Said{C.Custom{"youtube." ++ ty, []}, C.j.str(C.j.get(sn, "displayMessage"))}})))))))))

def flag(+author: C.J, key: String, +name: String, +rest: List<&2, String>) -> List<&2, String>:
  Bool.pick(List<&2, String>, C.j.is_true(C.j.get(author, key)), name <> rest, rest)

# The author's flags as the badges every platform shares.
def badges(+author: C.J) -> List<&2, String>:
  flag(author, "isChatOwner", "broadcaster", flag(author, "isChatModerator", "moderator", flag(author, "isChatSponsor", "member", flag(author, "isVerified", "verified", []))))

def event.of(m: Maybe<&2, Said>, +item: C.J, video: String) -> Maybe<&2, C.ChatEvent>:
  match m:
    case None{}:
      None{}
    case Some{s}:
      match s:
        case Said{what, body}:
          +author = C.j.get(item, "authorDetails")
          Some{C.ChatEvent{what, C.j.str(C.j.get(item, "id")), "youtube", video, C.or_else(C.j.str(C.j.get(author, "displayName")), "?"), body, badges(author), ""}}

# One liveChat/messages item as an event of the chat of video.
def event(+item: C.J, video: String) -> Maybe<&2, C.ChatEvent>:
  +sn = C.j.get(item, "snippet")
  event.of(said(C.j.str(C.j.get(sn, "type")), sn), item, video)

def keep(m: Maybe<&2, C.ChatEvent>, rest: List<&2, C.ChatEvent>) -> List<&2, C.ChatEvent>:
  match m:
    case None{}:
      rest
    case Some{e}:
      e <> rest

def events(items: List<&2, C.J>, +video: String) -> List<&2, C.ChatEvent>:
  match items:
    case Nil{}:
      []
    case Con{item, t}:
      keep(event(item, video), events(t, video))

type Page is Data:
  Page{events: List<&2, C.ChatEvent>, next: String, wait: U32}

def wait(+ms: U32) -> U32:
  Bool.pick(U32, U32.is_lt(ms, 3000), 3000, ms)

# A liveChat/messages answer: its events, the next page's token, and how long to
# wait before asking (what YouTube says, 3 s at least).
def page(+resp: C.J, +video: String) -> Page:
  Page{events(C.j.list(C.j.get(resp, "items")), video), C.j.str(C.j.get(resp, "nextPageToken")), wait(C.number(C.j.str(C.j.get(resp, "pollingIntervalMillis")), 5000))}

def video_url(key: String, video: String) -> String:
  api() ++ "videos?part=liveStreamingDetails&id=" ++ C.url.esc(video) ++ "&key=" ++ C.url.esc(key)

def page_param(+page: String) -> String:
  Bool.pick(String, String.is_empty(page), "", "&pageToken=" ++ C.url.esc(page))

def chat_url(key: String, chat: String, page: String) -> String:
  api() ++ "liveChat/messages?liveChatId=" ++ C.url.esc(chat) ++ "&part=snippet%2CauthorDetails&maxResults=200" ++ page_param(page) ++ "&key=" ++ C.url.esc(key)

# Posting, with a user token that has youtube.force-ssl
# -----------------------------------------------------

def refresh_form(client: String, secret: String, refresh: String) -> String:
  "client_id=" ++ C.url.esc(client) ++ "&client_secret=" ++ C.url.esc(secret) ++ "&refresh_token=" ++ C.url.esc(refresh) ++ "&grant_type=refresh_token"

# A refreshed access token and how many seconds it lives; Fail with Google's reason.
def token(+resp: C.J) -> Result<&2, &2, String, C.Two>:
  +t = C.j.str(C.j.get(resp, "access_token"))
  Bool.pick(Result<&2, &2, String, C.Two>, String.is_empty(t), Fail{"refreshing the token failed: " ++ C.or_else(C.j.str(C.j.get(resp, "error_description")), C.api.why(resp))},
    Done{C.Two{t, C.or_else(C.j.str(C.j.get(resp, "expires_in")), "3599")}})

def lookup_url(video: String) -> String:
  api() ++ "videos?part=liveStreamingDetails&id=" ++ C.url.esc(video)

def send_url() -> String:
  api() ++ "liveChat/messages?part=snippet"

def body(chat: String, text: String) -> String:
  "{\"snippet\":{\"liveChatId\":" ++ C.str(chat) ++ ",\"type\":\"textMessageEvent\",\"textMessageDetails\":{\"messageText\":" ++ C.str(text) ++ "}}}"

# A 200 from liveChat/messages insert.
def sent(resp: C.J) -> C.Outcome:
  C.Sent{C.j.str(C.j.get(resp, "id"))}

# DELETE liveChat/messages: a message, by id (the owner or a moderator).
def delete_url(id: String) -> String:
  api() ++ "liveChat/messages?id=" ++ C.url.esc(id)
