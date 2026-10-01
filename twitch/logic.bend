# Twitch, pure: IRC lines in, events out; the Helix requests that post a message.
import Base
import ../core.bend as C

# IRC
# ---

def unesc.char(+c: Char) -> Char:
  Bool.pick(Char, Char.is_eq(c, 's'), ' ',
    Bool.pick(Char, Char.is_eq(c, ':'), ';',
      Bool.pick(Char, Char.is_eq(c, 'r'), '\r',
        Bool.pick(Char, Char.is_eq(c, 'n'), '\n', c))))

def unesc.step(esc: Bool, slash: Bool, h: Char, rest: Bool -> String) -> String:
  match esc:
    case True{}:
      SCon{unesc.char(h), rest(False{})}
    case False{}:
      match slash:
        case True{}:
          rest(True{})
        case False{}:
          SCon{h, rest(False{})}

# The rest of the text, given whether a backslash came just before it.
def unesc.go(s: String) -> Bool -> String:
  match s:
    case SNil{}:
      esc => ""
    case SCon{+h, t}:
      rest = unesc.go(t)
      esc => unesc.step(esc, Char.is_eq(h, '\\'), h, rest)

# An IRCv3 tag value: \s is a space, \: a semicolon, \\ a backslash, \r and \n.
def unescape(s: String) -> String:
  unesc.go(s)(False{})

def tag.pick(hit: Bool, +kv: String, rest: String) -> String:
  match hit:
    case True{}:
      unescape(C.second(C.cut(kv, '=')))
    case False{}:
      rest

def tag.go(xs: List<&2, String>, +key: String) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{+kv, t}:
      tag.pick(String.eq(C.first(C.cut(kv, '=')), key), kv, tag.go(t, key))

# The value of key in "k1=v1;k2=v2", unescaped; "" when it is not there.
def tag(tags: String, +key: String) -> String:
  tag.go(String.split(tags, ';'), key)

# What one IRC line means to the bridge.
type Irc is Data:
  Ping{token: String}
  Got{event: C.ChatEvent}
  Other{}

# A badge as a word every platform shares; "" for the rest (bits, predictions, …).
def badge(+name: String) -> String:
  Bool.pick(String, String.eq(name, "broadcaster"), "broadcaster",
    Bool.pick(String, String.eq(name, "moderator"), "moderator",
      Bool.pick(String, String.eq(name, "vip"), "vip",
        Bool.pick(String, Bool.or(String.eq(name, "subscriber"), String.eq(name, "founder")), "member",
          Bool.pick(String, String.eq(name, "partner"), "verified", "")))))

def badges.keep(+b: String, +rest: List<&2, String>) -> List<&2, String>:
  Bool.pick(List<&2, String>, String.is_empty(b), rest, b <> rest)

def badges.go(xs: List<&2, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      []
    case Con{x, t}:
      badges.keep(badge(C.first(C.cut(x, '/'))), badges.go(t))

# The badges tag ("broadcaster/1,subscriber/12"), and first for a first message.
def badges(+tags: String) -> List<&2, String>:
  +bs = badges.go(String.split(tag(tags, "badges"), ','))
  Bool.pick(List<&2, String>, String.eq(tag(tags, "first-msg"), "1"), List.append(&2, String, bs, ["first"]), bs)

def event(what: C.What, +tags: String, fallback: String, channel: String, from: String, body: String) -> Irc:
  Got{C.ChatEvent{what, C.or_else(tag(tags, "id"), fallback), "twitch", channel, from, body, badges(tags), tag(tags, "reply-parent-msg-id")}}

# A chat message. With bits it is a cheer, a tip in bits; with a custom reward, a
# channel-points redemption; with a msg-id, Twitch's own kind (a highlighted
# message, a Power-up).
def privmsg.what(+tags: String) -> C.What:
  +bits = tag(tags, "bits")
  +reward = tag(tags, "custom-reward-id")
  +id = tag(tags, "msg-id")
  Bool.pick(C.What, Bool.not(String.is_empty(bits)), C.Tip{bits, "bits", C.digits(bits) ++ "000000"},
    Bool.pick(C.What, Bool.not(String.is_empty(reward)), C.Custom{"twitch.redemption", [C.Two{"reward", reward}]},
      Bool.pick(C.What, Bool.not(String.is_empty(id)), C.Custom{"twitch." ++ id, []}, C.Chat{})))

def privmsg(+tags: String, nick: String, channel: String, body: String, fallback: String) -> Irc:
  event(privmsg.what(tags), tags, fallback, channel, C.or_else(tag(tags, "display-name"), nick), body)

def notice.of(m: Maybe<&2, C.What>, +tags: String, fallback: String, channel: String, from: String, body: String) -> Irc:
  match m:
    case None{}:
      Other{}
    case Some{what}:
      event(what, tags, fallback, channel, from, body)

def params.keep(+kv: String, +rest: List<&2, C.Two>) -> List<&2, C.Two>:
  Bool.pick(List<&2, C.Two>, String.starts_with(kv, "msg-param-"), C.Two{String.drop(C.first(C.cut(kv, '=')), 10n), unescape(C.second(C.cut(kv, '=')))} <> rest, rest)

def params.go(xs: List<&2, String>) -> List<&2, C.Two>:
  match xs:
    case Nil{}:
      []
    case Con{x, t}:
      params.keep(x, params.go(t))

# The msg-param-* tags, without that prefix: the fields of Twitch's own kinds.
def params(tags: String) -> List<&2, C.Two>:
  params.go(String.split(tags, ';'))

def is_sub(+id: String) -> Bool:
  Bool.or(String.eq(id, "sub"), Bool.or(String.eq(id, "resub"),
    Bool.or(String.eq(id, "giftpaidupgrade"), Bool.or(String.eq(id, "anongiftpaidupgrade"), String.eq(id, "primepaidupgrade")))))

# A community gift arrives as one submysterygift, then a subgift per viewer that
# names it: those are left out, so the gift counts once. A gifted or Prime sub
# turned paid is a sub. What has no shared word (an announcement, a watch streak,
# a charity donation, …) is Twitch's own kind, with its fields.
def notice.what(+id: String, +tags: String) -> Maybe<&2, C.What>:
  +tier = tag(tags, "msg-param-sub-plan")
  Bool.pick(Maybe<&2, C.What>, String.is_empty(id), None{},
  Bool.pick(Maybe<&2, C.What>, is_sub(id), Some{C.Sub{C.number(tag(tags, "msg-param-cumulative-months"), 1), tier}},
    Bool.pick(Maybe<&2, C.What>, String.eq(id, "subgift"),
      Bool.pick(Maybe<&2, C.What>, String.is_empty(tag(tags, "msg-param-community-gift-id")), Some{C.Gift{1, tier, tag(tags, "msg-param-recipient-display-name")}}, None{}),
      Bool.pick(Maybe<&2, C.What>, String.eq(id, "submysterygift"), Some{C.Gift{C.number(tag(tags, "msg-param-mass-gift-count"), 1), tier, ""}},
        Bool.pick(Maybe<&2, C.What>, String.eq(id, "raid"), Some{C.Raid{C.number(tag(tags, "msg-param-viewerCount"), 0)}},
          Some{C.Custom{"twitch." ++ id, params(tags)}})))))

# USERNOTICE: subs, resubs, gifts and raids, with the viewer's message if any.
def usernotice(+tags: String, channel: String, body: String, fallback: String) -> Irc:
  notice.of(notice.what(tag(tags, "msg-id"), tags), tags, fallback, channel, C.or_else(tag(tags, "display-name"), tag(tags, "login")), body)

# CLEARMSG: a message deleted; body is its text.
def clearmsg(+tags: String, channel: String, body: String, fallback: String) -> Irc:
  event(C.Deleted{tag(tags, "target-msg-id")}, tags, fallback, channel, tag(tags, "login"), body)

# CLEARCHAT: with a user, a ban (no ban-duration) or a timeout; without, the whole chat.
def clearchat(+tags: String, channel: String, +user: String, fallback: String) -> Irc:
  event(Bool.pick(C.What, String.is_empty(user), C.Cleared{}, C.Banned{user, C.number(tag(tags, "ban-duration"), 0)}), tags, fallback, channel, "", "")

def command(+cmd: String, +tags: String, +nick: String, +channel: String, +body: String, +fallback: String) -> Irc:
  Bool.pick(Irc, String.eq(cmd, "PRIVMSG"), privmsg(tags, nick, channel, body, fallback),
    Bool.pick(Irc, String.eq(cmd, "USERNOTICE"), usernotice(tags, channel, body, fallback),
      Bool.pick(Irc, String.eq(cmd, "CLEARMSG"), clearmsg(tags, channel, body, fallback),
        Bool.pick(Irc, String.eq(cmd, "CLEARCHAT"), clearchat(tags, channel, body, fallback), Other{}))))

# ":nick!user@host COMMAND #channel :body"
def message(+tags: String, rest: String, fallback: String) -> Irc:
  +a = C.cut(C.drop1(rest, ':'), ' ')
  +b = C.cut(C.second(a), ' ')
  +c = C.cut(C.second(b), ' ')
  command(C.first(b), tags, C.first(C.cut(C.first(a), '!')), C.drop1(C.first(c), '#'), C.drop1(C.second(c), ':'), fallback)

def irc.msg(ping: Bool, +tags: String, +rest: String, fallback: String) -> Irc:
  match ping:
    case True{}:
      Ping{C.second(C.cut(rest, ' '))}
    case False{}:
      message(tags, rest, fallback)

def irc.tags(tagged: Bool, +raw: String, fallback: String) -> Irc:
  match tagged:
    case True{}:
      +p = C.cut(C.drop1(raw, '@'), ' ')
      irc.msg(String.starts_with(C.second(p), "PING"), C.first(p), C.second(p), fallback)
    case False{}:
      irc.msg(String.starts_with(raw, "PING"), "", raw, fallback)

# One line from Twitch, without its "\r\n". fallback is the id of an event whose
# tags name none.
def irc(+raw: String, fallback: String) -> Irc:
  irc.tags(String.starts_with(raw, "@"), raw, fallback)

def pong(token: String) -> String:
  "PONG " ++ token ++ "\r\n"

# A Twitch channel as IRC wants it: lowercase, without its '#'.
def channel(s: String) -> String:
  String.to_lower(C.drop1(String.trim(s), '#'))

# The anonymous login: justinfan and a number are Twitch's read-only guest.
# twitch.tv/commands brings USERNOTICE, where subs, gifts and raids are.
def hello(+channel: String, +n: U32) -> String:
  +nick = (n % 90000 + 10000 : U32)
  "CAP REQ :twitch.tv/tags twitch.tv/commands\r\nNICK justinfan" ++ U32.show(nick) ++ "\r\nJOIN #" ++ channel ++ "\r\n"

# Helix, with a user token
# -------------------------

# A token without the "oauth:" chat clients put before it.
def token(+t: String) -> String:
  Bool.pick(String, String.starts_with(t, "oauth:"), String.drop(t, 6n), t)

# Who a token is (GET id.twitch.tv/oauth2/validate).
type Me is Data:
  Me{client: String, user: String, scopes: List<&2, String>}

def scopes(xs: List<&2, C.J>) -> List<&2, String>:
  match xs:
    case Nil{}:
      []
    case Con{x, t}:
      C.j.str(x) <> scopes(t)

# The client, user and scopes of a validate answer, or why the token is no good.
def me(+resp: C.J) -> Result<&2, &2, String, Me>:
  +client = C.j.str(C.j.get(resp, "client_id"))
  +user = C.j.str(C.j.get(resp, "user_id"))
  Bool.pick(Result<&2, &2, String, Me>, Bool.or(String.is_empty(client), String.is_empty(user)), Fail{"TWITCH_CHAT_TOKEN is not a valid user token"},
    Done{Me{client, user, scopes(C.j.list(C.j.get(resp, "scopes")))}})

def has(xs: List<&2, String>, +want: String) -> Bool:
  match xs:
    case Nil{}:
      False{}
    case Con{x, t}:
      Bool.or(String.eq(x, want), has(t, want))

# Why a token with scopes cannot do what needs scope; None when it can. Posting
# needs user:write:chat; deleting, moderator:manage:chat_messages.
def need(xs: List<&2, String>, +scope: String) -> Maybe<&2, String>:
  Bool.pick(Maybe<&2, String>, has(xs, scope), None{}, Some{"TWITCH_CHAT_TOKEN lacks the " ++ scope ++ " scope"})

# The id of the first user of a GET helix/users answer.
def user_id(resp: C.J) -> String:
  C.j.str(C.j.get(C.j.first(C.j.list(C.j.get(resp, "data"))), "id"))

def users_url(channel: String) -> String:
  "https://api.twitch.tv/helix/users?login=" ++ C.url.esc(channel)

def body(broadcaster: String, sender: String, text: String) -> String:
  "{\"broadcaster_id\":" ++ C.str(broadcaster) ++ ",\"sender_id\":" ++ C.str(sender) ++ ",\"message\":" ++ C.str(text) ++ "}"

def sent.pick(ok: Bool, +first: C.J) -> C.Outcome:
  match ok:
    case True{}:
      C.Sent{C.j.str(C.j.get(first, "message_id"))}
    case False{}:
      C.Refused{C.or_else(C.j.str(C.j.get(C.j.get(first, "drop_reason"), "message")), "Twitch did not send it")}

# A 200 from POST helix/chat/messages: sent, or dropped with Twitch's reason.
def sent(resp: C.J) -> C.Outcome:
  +first = C.j.first(C.j.list(C.j.get(resp, "data")))
  sent.pick(C.j.is_true(C.j.get(first, "is_sent")), first)

# DELETE helix/moderation/chat: a message, by id, taken down by moderator.
def delete_url(broadcaster: String, moderator: String, id: String) -> String:
  "https://api.twitch.tv/helix/moderation/chat?broadcaster_id=" ++ C.url.esc(broadcaster) ++ "&moderator_id=" ++ C.url.esc(moderator) ++ "&message_id=" ++ C.url.esc(id)

# Renewal: POST id.twitch.tv/oauth2/token with a refresh token
# ------------------------------------------------------------

def renew_url() -> String:
  "https://id.twitch.tv/oauth2/token"

def renew_form(client: String, secret: String, refresh: String) -> String:
  "client_id=" ++ C.url.esc(client) ++ "&client_secret=" ++ C.url.esc(secret) ++ "&refresh_token=" ++ C.url.esc(refresh) ++ "&grant_type=refresh_token"

# A renewed token, the refresh token to use next (Twitch may hand a new one), and
# its life in seconds.
type Renewal is Data:
  Renewal{token: String, refresh: String, secs: String}

# A renewal answer; old is the refresh token that asked, kept if none comes back.
def renewed(+resp: C.J, +old: String) -> Result<&2, &2, String, Renewal>:
  +t = C.j.str(C.j.get(resp, "access_token"))
  Bool.pick(Result<&2, &2, String, Renewal>, String.is_empty(t), Fail{"renewing TWITCH_REFRESH_TOKEN failed: " ++ C.or_else(C.api.why(resp), "no reason given")},
    Done{Renewal{t, C.or_else(C.j.str(C.j.get(resp, "refresh_token")), old), C.or_else(C.j.str(C.j.get(resp, "expires_in")), "14400")}})
