# Twitch EventSub, pure: what the bridge subscribes to over a WebSocket session,
# and what each message means. EventSub brings what IRC cannot see: follows,
# redemptions that ask for no text, hype trains, polls, predictions, the stream
# going up or down, and raids out. Subs, cheers and raids in stay on IRC, so
# nothing is said twice.
import Base
import ../core.bend as C

def base_url() -> String:
  "wss://eventsub.wss.twitch.tv/ws?keepalive_timeout_seconds=30"

def subscriptions_url() -> String:
  "https://api.twitch.tv/helix/eventsub/subscriptions"

# A wss:// URL as a host and the path after it, as a WebSocket client asks.
def ws_url(url: String) -> C.Two:
  +rest = C.second(C.cut(url, '/'))
  +after = C.drop1(rest, '/')
  +hp = C.cut(after, '/')
  C.Two{C.first(hp), "/" ++ C.second(hp)}

# Subscribing
# -----------

# One subscription: its topic (EventSub's type) and version, its condition (a JSON object), and the
# scopes that allow it, any one of them; none needs no scope.
type Want is Data:
  Want{topic: String, version: String, condition: String, scopes: List<&2, String>}

def cond.b(b: String) -> String:
  "{\"broadcaster_user_id\":" ++ C.str(b) ++ "}"

# Everything the bridge subscribes to, for the channel b, as the token's user m.
def wants(+b: String, m: String) -> List<&2, Want>:
  [Want{"channel.follow", "2", "{\"broadcaster_user_id\":" ++ C.str(b) ++ ",\"moderator_user_id\":" ++ C.str(m) ++ "}", ["moderator:read:followers", "moderator:manage:followers"]},
   Want{"channel.channel_points_custom_reward_redemption.add", "1", cond.b(b), ["channel:read:redemptions", "channel:manage:redemptions"]},
   Want{"channel.hype_train.begin", "1", cond.b(b), ["channel:read:hype_train"]},
   Want{"channel.hype_train.progress", "1", cond.b(b), ["channel:read:hype_train"]},
   Want{"channel.hype_train.end", "1", cond.b(b), ["channel:read:hype_train"]},
   Want{"channel.poll.begin", "1", cond.b(b), ["channel:read:polls", "channel:manage:polls"]},
   Want{"channel.poll.end", "1", cond.b(b), ["channel:read:polls", "channel:manage:polls"]},
   Want{"channel.prediction.begin", "1", cond.b(b), ["channel:read:predictions", "channel:manage:predictions"]},
   Want{"channel.prediction.lock", "1", cond.b(b), ["channel:read:predictions", "channel:manage:predictions"]},
   Want{"channel.prediction.end", "1", cond.b(b), ["channel:read:predictions", "channel:manage:predictions"]},
   Want{"stream.online", "1", cond.b(b), []},
   Want{"stream.offline", "1", cond.b(b), []},
   Want{"channel.raid", "1", "{\"from_broadcaster_user_id\":" ++ C.str(b) ++ "}", []}]

def has(xs: List<&2, String>, +want: String) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case Con{x, t}:
      Bool.or(String.eq(x, want), has(t, want))

def any_of(wanted: List<&2, String>, +have: List<&2, String>) -> Bool:
  match wanted:
    case Nil{}:
      False{}
    case Con{w, t}:
      Bool.or(has(have, w), any_of(t, have))

# Whether a token with have may make w.
def allowed(w: Want, +have: List<&2, String>) -> Bool:
  match w:
    case Want{topic, version, condition, +scopes}:
      Bool.or(List.is_empty(&2, String, scopes), any_of(scopes, have))

# What to subscribe to, and what is left out for a scope the token lacks.
type Plan is Data:
  Plan{wants: List<&2, Want>, skipped: List<&2, String>}

def plan.add(ok: Bool, w: Want, p: Plan) -> Plan:
  match w:
    case Want{+topic, +version, +condition, +scopes}:
      match p:
        case Plan{+ws, +sk}:
          Bool.pick(Plan, ok, Plan{Want{topic, version, condition, scopes} <> ws, sk}, Plan{ws, (topic ++ " (needs " ++ String.join(scopes, " or ") ++ ")") <> sk})

def plan.go(xs: List<&2, Want>, +have: List<&2, String>) -> Plan:
  match xs:
    case Nil{}:
      Plan{[], []}
    case Con{+w, t}:
      plan.add(allowed(w, have), w, plan.go(t, have))

def plan(+b: String, m: String, +have: List<&2, String>) -> Plan:
  plan.go(wants(b, m), have)

# POST helix/eventsub/subscriptions: w, delivered to the session.
def body(w: Want, session: String) -> String:
  match w:
    case Want{topic, version, condition, scopes}:
      "{\"type\":" ++ C.str(topic) ++ ",\"version\":" ++ C.str(version) ++ ",\"condition\":" ++ condition ++ ",\"transport\":{\"method\":\"websocket\",\"session_id\":" ++ C.str(session) ++ "}}"

def name(w: Want) -> String:
  match w:
    case Want{topic, version, condition, scopes}:
      topic

# Messages
# --------

# What one message on the session means.
#   Welcome    the session is up: subscribe to it within 10 s; silent for longer
#              than keepalive seconds, it is gone
#   Reconnect  move to url; the subscriptions go along
#   Revoked    a subscription Twitch ended, and why
type Es is Data:
  Welcome{session: String, keepalive: U32}
  Keepalive{}
  Note{event: Maybe<&2, C.ChatEvent>}
  Reconnect{url: String}
  Revoked{topic: String, status: String}
  Unknown{}

def str2(+j: C.J, k1: String, k2: String) -> String:
  C.j.str(C.j.get(C.j.get(j, k1), k2))

# A list of objects as "a|b", each by key.
def joined.go(xs: List<&2, C.J>, +key: String) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{x, t}:
      "|" ++ C.j.str(C.j.get(x, key)) ++ joined.go(t, key)

def joined(xs: List<&2, C.J>, +key: String) -> String:
  C.drop1(joined.go(xs, key), '|')

# As "a:12|b:3": each title, and its count by key.
def tally.go(xs: List<&2, C.J>, +key: String) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{+x, t}:
      "|" ++ C.j.str(C.j.get(x, "title")) ++ ":" ++ C.j.str(C.j.get(x, key)) ++ tally.go(t, key)

def tally(xs: List<&2, C.J>, +key: String) -> String:
  C.drop1(tally.go(xs, key), '|')

# The title of the outcome whose id is id.
def winner(xs: List<&2, C.J>, +id: String) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{+x, t}:
      Bool.pick(String, String.eq(C.j.str(C.j.get(x, "id")), id), C.j.str(C.j.get(x, "title")), winner(t, id))

def custom(+name: String, fields: List<&2, C.Two>, +id: String, +channel: String, from: String, body: String) -> Maybe<&2, C.ChatEvent>:
  Some{C.ChatEvent{C.Custom{"twitch." ++ name, fields}, id, "twitch", channel, from, body, [], ""}}

def said(what: C.What, +id: String, +channel: String, from: String, body: String) -> Maybe<&2, C.ChatEvent>:
  Some{C.ChatEvent{what, id, "twitch", channel, from, body, [], ""}}

# A redemption that asks for text arrives on IRC with it; only the rest come from here.
def redemption(+ev: C.J, +id: String, +channel: String) -> Maybe<&2, C.ChatEvent>:
  +input = C.j.str(C.j.get(ev, "user_input"))
  Bool.pick(Maybe<&2, C.ChatEvent>, Bool.not(String.is_empty(input)), None{},
    custom("redemption", [C.Two{"reward", str2(ev, "reward", "id")}, C.Two{"title", str2(ev, "reward", "title")}, C.Two{"cost", str2(ev, "reward", "cost")}], id, channel, C.j.str(C.j.get(ev, "user_name")), ""))

def hype(+kind: String, +ev: C.J, +id: String, +channel: String) -> Maybe<&2, C.ChatEvent>:
  custom("hype_train." ++ kind, [C.Two{"level", C.j.str(C.j.get(ev, "level"))}, C.Two{"total", C.j.str(C.j.get(ev, "total"))}, C.Two{"progress", C.j.str(C.j.get(ev, "progress"))}, C.Two{"goal", C.j.str(C.j.get(ev, "goal"))}], id, channel, "", "")

def poll(+kind: String, +ev: C.J, +id: String, +channel: String) -> Maybe<&2, C.ChatEvent>:
  +choices = C.j.list(C.j.get(ev, "choices"))
  custom("poll." ++ kind, [C.Two{"title", C.j.str(C.j.get(ev, "title"))}, C.Two{"status", C.j.str(C.j.get(ev, "status"))}, C.Two{"choices", Bool.pick(String, String.eq(kind, "end"), tally(choices, "votes"), joined(choices, "title"))}], id, channel, "", "")

def prediction(+kind: String, +ev: C.J, +id: String, +channel: String) -> Maybe<&2, C.ChatEvent>:
  +outcomes = C.j.list(C.j.get(ev, "outcomes"))
  custom("prediction." ++ kind, [C.Two{"title", C.j.str(C.j.get(ev, "title"))}, C.Two{"outcomes", joined(outcomes, "title")}, C.Two{"winner", winner(outcomes, C.j.str(C.j.get(ev, "winning_outcome_id")))}], id, channel, "", "")

# A notification of type ty, as an event of the chat; None for what IRC says.
def note(+ty: String, +ev: C.J, +id: String) -> Maybe<&2, C.ChatEvent>:
  +channel = C.j.str(C.j.get(ev, "broadcaster_user_login"))
  Bool.pick(Maybe<&2, C.ChatEvent>, String.eq(ty, "channel.follow"), said(C.Follow{}, id, channel, C.j.str(C.j.get(ev, "user_name")), ""),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.eq(ty, "channel.channel_points_custom_reward_redemption.add"), redemption(ev, id, channel),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.starts_with(ty, "channel.hype_train."), hype(String.drop(ty, 19n), ev, id, channel),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.starts_with(ty, "channel.poll."), poll(String.drop(ty, 13n), ev, id, channel),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.starts_with(ty, "channel.prediction."), prediction(String.drop(ty, 19n), ev, id, channel),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.eq(ty, "stream.online"), custom("stream.online", [C.Two{"type", C.j.str(C.j.get(ev, "type"))}], id, channel, "", ""),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.eq(ty, "stream.offline"), custom("stream.offline", [], id, channel, "", ""),
  Bool.pick(Maybe<&2, C.ChatEvent>, String.eq(ty, "channel.raid"), custom("raid.out", [C.Two{"to", C.j.str(C.j.get(ev, "to_broadcaster_user_name"))}, C.Two{"viewers", C.j.str(C.j.get(ev, "viewers"))}], id, C.j.str(C.j.get(ev, "from_broadcaster_user_login")), "", ""),
    custom(ty, [], id, channel, "", "")))))))))

# One message on the session, as parsed JSON.
def es(+msg: C.J) -> Es:
  +meta = C.j.get(msg, "metadata")
  +payload = C.j.get(msg, "payload")
  +ty = C.j.str(C.j.get(meta, "message_type"))
  Bool.pick(Es, String.eq(ty, "session_welcome"), Welcome{str2(payload, "session", "id"), C.number(str2(payload, "session", "keepalive_timeout_seconds"), 10)},
  Bool.pick(Es, String.eq(ty, "session_keepalive"), Keepalive{},
  Bool.pick(Es, String.eq(ty, "notification"), Note{note(C.j.str(C.j.get(meta, "subscription_type")), C.j.get(payload, "event"), C.j.str(C.j.get(meta, "message_id")))},
  Bool.pick(Es, String.eq(ty, "session_reconnect"), Reconnect{str2(payload, "session", "reconnect_url")},
  Bool.pick(Es, String.eq(ty, "revocation"), Revoked{str2(payload, "subscription", "type"), str2(payload, "subscription", "status")},
    Unknown{})))))
