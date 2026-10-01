# The bridge's shared vocabulary: events, the Integration every platform implements,
# and the text the bridge speaks (remux's wire, the socket's NDJSON, the config).
# No IO and no packages, so PROOF.bend can prove what LAWS.bend says.
# Text is octets, one Char per byte, as Wire and Json hand it over: UTF-8 passes
# through untouched, and only ASCII is ever looked at or escaped.
import Base

# Events
# ------

# What happened, in words every platform shares. A platform says what it can:
# Twitch has subs, gifts, bits and raids; YouTube memberships, gifts and Super Chats.
#   Tip       amount as the platform shows it; micros, the amount in millionths of
#             currency as decimal digits (exact at any size), to add or rank tips
#             (bits are a currency here: 100 bits is 100000000 micros of "bits")
#   Deleted   a message taken down: target is its id
#   Banned    a viewer banned (seconds 0) or timed out
#   Cleared   the whole chat cleared
#   Custom    what only one platform has, named "<platform>.<what>", with its fields:
#             an integration reports it without a change here
type What is Data:
  Chat{}
  Sub{months: U32, tier: String}
  Gift{count: U32, tier: String, to: String}
  Tip{amount: String, currency: String, micros: String}
  Raid{viewers: U32}
  Follow{}
  Deleted{target: String}
  Banned{user: String, seconds: U32}
  Cleared{}
  Custom{name: String, fields: List<&2, Two>}

# One thing that happened in a chat. body is what the viewer wrote, "" when nothing.
# badges say who from is, in words every platform shares: broadcaster, moderator,
# vip, member, verified, and first for a first message. reply is the id of the
# message this one answers, "" when none.
type ChatEvent is Data:
  ChatEvent{what: What, id: String, platform: String, channel: String, from: String, body: String, badges: List<&2, String>, reply: String}

# The hub's mail: integrations report what Happened; each client Joins with an inbox, where
# the hub Pushes every event and a ticker Ticks.
type Down is Type:
  Push{event: ChatEvent}
  Tick{}

type Up is Type:
  Happened{event: ChatEvent}
  Join{inbox: Chan(Down)}

# How a send ended.
type Outcome is Data:
  Sent{id: String}
  Refused{why: String}

# The Integration
# ---------------

# One platform the bridge reads from and posts to. bridge.bend runs any of them;
# see twitch/twitch.bend for one. C is what setup makes of the platform's value in
# the [byo] table and the environment; S is what its sender keeps between messages.
#   name     its key in [byo], and its "platform" everywhere
#   limit    the characters one message may have
#   setup    its settings from its [byo] value, or why it cannot run
#   channel  the channel its events carry, as remux's say and delete name it
#   read     runs for good, handing each event to the hub as Happened
#   sends    whether it has what posting needs (tokens)
#   fresh    what its sender starts with
#   send     posts one message
#   delete   takes one message down, by its id
type Integration<-C: Type, -S: Type> is Type:
  Integration{name: String, limit: U32, setup: String -> IO(Result<&1, &1, String, C>), channel: C -> String, read: C -> Chan(Up) -> IO(Unit), sends: C -> Bool, fresh: S, send: C -> S -> String -> IO(S & Outcome), delete: C -> S -> String -> IO(S & Outcome)}

def Integration.name(-C: Type, -S: Type, it: Integration<C, S>) -> String:
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  name

def Integration.limit(-C: Type, -S: Type, it: Integration<C, S>) -> U32:
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  limit

def Integration.setup(-C: Type, -S: Type, it: Integration<C, S>, value: String) -> IO(Result<&1, &1, String, C>):
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  setup(value)

def Integration.channel(-C: Type, -S: Type, it: Integration<C, S>, conf: C) -> String:
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  channel(conf)

def Integration.read(-C: Type, -S: Type, it: Integration<C, S>, conf: C, hub: Chan(Up)) -> IO(Unit):
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  read(conf, hub)

def Integration.sends(-C: Type, -S: Type, it: Integration<C, S>, conf: C) -> Bool:
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  sends(conf)

def Integration.fresh(-C: Type, -S: Type, it: Integration<C, S>) -> S:
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  fresh

def Integration.delete(-C: Type, -S: Type, it: Integration<C, S>, conf: C, st: S, id: String) -> IO(S & Outcome):
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  delete(conf, st, id)

def Integration.send(-C: Type, -S: Type, it: Integration<C, S>, conf: C, st: S, body: String) -> IO(S & Outcome):
  Integration{name, limit, setup, channel, read, sends, fresh, send, delete} = it
  send(conf, st, body)

# Text
# ----

# Text split in two.
type Two is Data:
  Two{a: String, b: String}

def first(r: Two) -> String:
  match r:
    case Two{a, b}:
      a

def second(r: Two) -> String:
  match r:
    case Two{a, b}:
      b

def cut.pick(hit: Bool, h: Char, t: String, r: Two) -> Two:
  match hit:
    case True{}:
      Two{"", t}
    case False{}:
      match r:
        case Two{a, b}:
          Two{SCon{h, a}, b}

# The text before the first c and the text after it; Two{s, ""} when there is none.
def cut(s: String, +c: Char) -> Two:
  match s:
    case SNil{}:
      Two{"", ""}
    case SCon{+h, +t}:
      cut.pick(Char.is_eq(h, c), h, t, cut(t, c))

def drop1.pick(hit: Bool, h: Char, t: String) -> String:
  match hit:
    case True{}:
      t
    case False{}:
      SCon{h, t}

# s without one leading c.
def drop1(s: String, +c: Char) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+h, t}:
      drop1.pick(Char.is_eq(h, c), h, t)

# a, or b when a is empty.
def or_else(+a: String, b: String) -> String:
  Bool.pick(String, String.is_empty(a), b, a)

# The number s spells, or d.
def number(s: String, d: U32) -> U32:
  Maybe.default(&2, U32, U32.read(s), d)

def digits.go(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+h, t}:
      +n = Char.to_u32(h)
      +rest = digits.go(t)
      Bool.pick(String, Bool.and(U32.is_ge(n, 48), U32.is_le(n, 57)), SCon{h, rest}, rest)

# The digits of s, and nothing else; "0" when it has none. A number of any size,
# safe to write into JSON as it is.
def digits(s: String) -> String:
  or_else(digits.go(s), "0")

def is_quote(+c: Char) -> Bool:
  Bool.or(Char.is_eq(c, '"'), Char.is_eq(c, '\''))

def unquote.front(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+h, +t}:
      Bool.pick(String, is_quote(h), unquote.front(t), SCon{h, t})

# s without the quotes around it, as Python's strip("\"'").
def unquote(s: String) -> String:
  String.reverse(unquote.front(String.reverse(unquote.front(s))))

# Control characters as '?', for a terminal: chat text is a stranger's.
def printable(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+h, t}:
      +n = Char.to_u32(h)
      SCon{Bool.pick(Char, Bool.or(U32.is_lt(n, 32), U32.is_eq(n, 127)), '?', h), printable(t)}

# Any octet below 32, or 127: line breaks and other control characters.
def has_control(s: String) -> Bool:
  match s:
    case SNil{}:
      False{}
    case SCon{h, t}:
      +n = Char.to_u32(h)
      Bool.or(Bool.or(U32.is_lt(n, 32), U32.is_eq(n, 127)), has_control(t))

def utf8.lead(+c: Char) -> U32:
  +n = Char.to_u32(c)
  Bool.pick(U32, Bool.and(U32.is_ge(n, 128), U32.is_lt(n, 192)), 0, 1)

# Characters in UTF-8 octets: every octet but a continuation (0x80..0xBF) starts one.
def utf8.length(s: String) -> U32:
  match s:
    case SNil{}:
      0
    case SCon{h, t}:
      (utf8.lead(h) + utf8.length(t) : U32)

def url.keep(+c: Char) -> Bool:
  +n = Char.to_u32(c)
  Bool.or(Bool.and(U32.is_ge(n, 48), U32.is_le(n, 57)),
    Bool.or(Bool.and(U32.is_ge(n, 65), U32.is_le(n, 90)),
      Bool.or(Bool.and(U32.is_ge(n, 97), U32.is_le(n, 122)),
        Bool.or(Char.is_eq(c, '-'), Bool.or(Char.is_eq(c, '_'), Bool.or(Char.is_eq(c, '.'), Char.is_eq(c, '~')))))))

def hex.up(+n: U32) -> Char:
  Bool.pick(Char, U32.is_lt(n, 10), Char.from_u32((n + 48 : U32)), Char.from_u32((n + 55 : U32)))

# s percent-encoded for a URL query: all but A-Z a-z 0-9 - _ . ~ (RFC 3986 §2.3).
def url.esc(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{+h, t}:
      +n = Char.to_u32(h)
      Bool.pick(String, url.keep(h), SCon{h, ""}, SCon{'%', SCon{hex.up((n / 16 : U32)), SCon{hex.up((n % 16 : U32)), ""}}}) ++ url.esc(t)

# Lines
# -----

def lines.push(nl: Bool, +cur: String, acc: List<&2, String>) -> List<&2, String>:
  match nl:
    case True{}:
      String.reverse(drop1(cur, '\r')) <> acc
    case False{}:
      acc

def lines.fin(r: List<&2, String> & String) -> List<&2, String> & String:
  (xs, cur) = r
  (List.reverse(&2, String, xs), String.reverse(cur))

# cur is the line so far, reversed; acc the whole lines, reversed.
def lines.go(s: String, +cur: String, +acc: List<&2, String>) -> List<&2, String> & String:
  match s:
    case SNil{}:
      (acc, cur)
    case SCon{+h, t}:
      +nl = Char.is_eq(h, '\n')
      lines.go(t, Bool.pick(String, nl, "", SCon{h, cur}), lines.push(nl, cur, acc))

# The whole lines of buf, each without its "\r\n", and the partial line after them.
def lines(buf: String) -> List<&2, String> & String:
  lines.fin(lines.go(buf, "", []))

# JSON out
# --------

def hex(+n: U32) -> Char:
  Bool.pick(Char, U32.is_lt(n, 10), Char.from_u32((n + 48 : U32)), Char.from_u32((n + 87 : U32)))

def esc.char(+c: Char) -> String:
  +n = Char.to_u32(c)
  Bool.pick(String, Char.is_eq(c, '"'), "\\\"",
    Bool.pick(String, Char.is_eq(c, '\\'), "\\\\",
      Bool.pick(String, Char.is_eq(c, '\n'), "\\n",
        Bool.pick(String, Char.is_eq(c, '\r'), "\\r",
          Bool.pick(String, Char.is_eq(c, '\t'), "\\t",
            Bool.pick(String, U32.is_lt(n, 32), "\\u00" ++ SCon{hex((n / 16 : U32)), SCon{hex((n % 16 : U32)), ""}},
              SCon{c, ""}))))))

# s as the inside of a JSON string (RFC 8259 §7). Octets from 0x80 up are UTF-8
# and pass through as they are.
def esc(s: String) -> String:
  match s:
    case SNil{}:
      ""
    case SCon{h, t}:
      esc.char(h) ++ esc(t)

def str(s: String) -> String:
  "\"" ++ esc(s) ++ "\""

# One chat line as remux's wire spells it (`remux schema`, wire_line).
type Line is Data:
  Line{id: String, platform: String, channel: String, from: String, body: String}

# One wire frame: {"line": {id, platform, channel, from, body}}.
def wire(l: Line) -> String:
  match l:
    case Line{id, platform, channel, from, body}:
      "{\"line\":{\"id\":" ++ str(id) ++ ",\"platform\":" ++ str(platform) ++ ",\"channel\":" ++ str(channel) ++ ",\"from\":" ++ str(from) ++ ",\"body\":" ++ str(body) ++ "}}"

def as_line.said(+body: String, line: Line) -> Maybe<&2, Line>:
  Bool.pick(Maybe<&2, Line>, String.is_empty(body), None{}, Some{line})

# What remux's wire shows of an event: what was written in chat, a chat message or
# the message of a tip or of a platform's own kind (an announcement, a redemption).
# A deleted message's text never goes back out. remux's wire carries lines only;
# the socket carries all events.
def as_line(e: ChatEvent) -> Maybe<&2, Line>:
  match e:
    case ChatEvent{what, id, platform, channel, from, +body, badges, reply}:
      match what:
        case Chat{}:
          Some{Line{id, platform, channel, from, body}}
        case Tip{amount, currency, micros}:
          as_line.said(body, Line{id, platform, channel, from, body})
        case Custom{name, fields}:
          as_line.said(body, Line{id, platform, channel, from, body})
        case Sub{months, tier}:
          None{}
        case Gift{count, tier, to}:
          None{}
        case Raid{viewers}:
          None{}
        case Follow{}:
          None{}
        case Deleted{target}:
          None{}
        case Banned{user, seconds}:
          None{}
        case Cleared{}:
          None{}

def what.name(k: What) -> String:
  match k:
    case Chat{}:
      "chat"
    case Sub{months, tier}:
      "sub"
    case Gift{count, tier, to}:
      "gift"
    case Tip{amount, currency, micros}:
      "tip"
    case Raid{viewers}:
      "raid"
    case Follow{}:
      "follow"
    case Deleted{target}:
      "deleted"
    case Banned{user, seconds}:
      "banned"
    case Cleared{}:
      "cleared"
    case Custom{name, fields}:
      "custom"

# ["a","b"]
def strs.go(xs: List<&2, String>) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{x, t}:
      "," ++ str(x) ++ strs.go(t)

def strs(xs: List<&2, String>) -> String:
  "[" ++ drop1(strs.go(xs), ',') ++ "]"

# {"k":"v",…}
def obj.go(xs: List<&2, Two>) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{kv, t}:
      match kv:
        case Two{k, v}:
          "," ++ str(k) ++ ":" ++ str(v) ++ obj.go(t)

def obj(xs: List<&2, Two>) -> String:
  "{" ++ drop1(obj.go(xs), ',') ++ "}"

# Its own fields, each after a comma.
def what.json(k: What) -> String:
  match k:
    case Chat{}:
      ""
    case Sub{months, tier}:
      ",\"months\":" ++ U32.show(months) ++ ",\"tier\":" ++ str(tier)
    case Gift{count, tier, to}:
      ",\"count\":" ++ U32.show(count) ++ ",\"tier\":" ++ str(tier) ++ ",\"to\":" ++ str(to)
    case Tip{amount, currency, micros}:
      ",\"amount\":" ++ str(amount) ++ ",\"currency\":" ++ str(currency) ++ ",\"micros\":" ++ digits(micros)
    case Raid{viewers}:
      ",\"viewers\":" ++ U32.show(viewers)
    case Follow{}:
      ""
    case Deleted{target}:
      ",\"target\":" ++ str(target)
    case Banned{user, seconds}:
      ",\"user\":" ++ str(user) ++ ",\"seconds\":" ++ U32.show(seconds)
    case Cleared{}:
      ""
    case Custom{name, fields}:
      ",\"name\":" ++ str(name) ++ ",\"fields\":" ++ obj(fields)

# One event as the socket sends it:
#   {"event": {"type", "id", "platform", "channel", "from", "body", "badges",
#              "reply", …the fields of what}}
def event.json(e: ChatEvent) -> String:
  match e:
    case ChatEvent{+what, id, platform, channel, from, body, badges, reply}:
      "{\"event\":{\"type\":" ++ str(what.name(what)) ++ ",\"id\":" ++ str(id) ++ ",\"platform\":" ++ str(platform) ++ ",\"channel\":" ++ str(channel) ++ ",\"from\":" ++ str(from) ++ ",\"body\":" ++ str(body) ++ ",\"badges\":" ++ strs(badges) ++ ",\"reply\":" ++ str(reply) ++ what.json(what) ++ "}}"

# JSON in
# -------

# A JSON value, copyable, as the bridge reads APIs' answers.
type J is Data:
  JNull{}
  JBool{on: Bool}
  JNum{s: String}
  JStr{s: String}
  JArr{xs: List<&2, J>}
  JObj{kvs: List<&2, JKv>}

type JKv is Data:
  JKv{k: String, v: J}

def j.find(kvs: List<&2, JKv>, +key: String) -> J:
  match kvs:
    case Nil{}:
      JNull{}
    case Con{JKv{k, v}, t}:
      Bool.pick(J, String.eq(k, key), v, j.find(t, key))

# The field key of an object; JNull for anything else.
def j.get(j: J, +key: String) -> J:
  match j:
    case JObj{kvs}:
      j.find(kvs, key)
    case JNull{}:
      JNull{}
    case JBool{b}:
      JNull{}
    case JNum{s}:
      JNull{}
    case JStr{s}:
      JNull{}
    case JArr{xs}:
      JNull{}

def j.str(j: J) -> String:
  match j:
    case JStr{s}:
      s
    case JNum{s}:
      s
    case JNull{}:
      ""
    case JBool{b}:
      ""
    case JArr{xs}:
      ""
    case JObj{kvs}:
      ""

def j.list(j: J) -> List<&2, J>:
  match j:
    case JArr{xs}:
      xs
    case JNull{}:
      []
    case JBool{b}:
      []
    case JNum{s}:
      []
    case JStr{s}:
      []
    case JObj{kvs}:
      []

def j.first(xs: List<&2, J>) -> J:
  match xs:
    case Nil{}:
      JNull{}
    case Con{h, t}:
      h

def j.is_true(v: J) -> Bool:
  match v:
    case JBool{b}:
      b
    case JNull{}:
      False{}
    case JNum{s}:
      False{}
    case JStr{s}:
      False{}
    case JArr{xs}:
      False{}
    case JObj{kvs}:
      False{}

def j.is_obj(j: J) -> Bool:
  match j:
    case JObj{kvs}:
      True{}
    case JNull{}:
      False{}
    case JBool{b}:
      False{}
    case JNum{s}:
      False{}
    case JStr{s}:
      False{}
    case JArr{xs}:
      False{}

# The message of an API error: Google's {"error": {"message"}}, or Twitch's
# {"error", "message"}.
def api.why(+resp: J) -> String:
  or_else(j.str(j.get(j.get(resp, "error"), "message")), or_else(j.str(j.get(resp, "message")), j.str(j.get(resp, "error"))))

def api.failed(+status: U32, +resp: J) -> Outcome:
  Refused{U32.show(status) ++ ": " ++ or_else(api.why(resp), "no reason given")}

# The socket's requests and answers
# ---------------------------------
# A client sends one JSON object per line:
#   {"say": {"platform": "<an integration's name>" | "all", "body": "…"}}
# and is answered one line per platform:
#   {"said": {"platform": …, "id": …}}  or  {"error": {"platform": …, "message": …}}

type Req is Data:
  Say{platform: String, body: String}
  Delete{platform: String, id: String}
  Bad{why: String}

def req.check(+p: String, +body: String) -> Req:
  Bool.pick(Req, String.is_empty(p), Bad{"platform is missing"},
    Bool.pick(Req, String.is_empty(body), Bad{"body is empty"},
      Bool.pick(Req, has_control(body), Bad{"body has a line break or another control character"},
        Say{p, body})))

def req.say(+say: J) -> Req:
  Bool.pick(Req, j.is_obj(say), req.check(j.str(j.get(say, "platform")), j.str(j.get(say, "body"))),
    Bad{"expected {\"say\": {\"platform\": ..., \"body\": ...}} or {\"delete\": {\"platform\": ..., \"id\": ...}}"})

def req.delete(+del: J) -> Req:
  +p = j.str(j.get(del, "platform"))
  +id = j.str(j.get(del, "id"))
  Bool.pick(Req, String.is_empty(p), Bad{"platform is missing"},
    Bool.pick(Req, String.is_empty(id), Bad{"id is missing"}, Delete{p, id}))

# One request line, as parsed JSON: a say, or a delete.
def req(+line: J) -> Req:
  +del = j.get(line, "delete")
  Bool.pick(Req, j.is_obj(del), req.delete(del), req.say(j.get(line, "say")))

# The platforms a Say to p goes to, of those running.
def targets(+p: String, names: List<&2, String>) -> List<&2, String>:
  Bool.pick(List<&2, String>, String.eq(p, "all"), names, [p])

def said(p: String, id: String) -> String:
  "{\"said\":{\"platform\":" ++ str(p) ++ ",\"id\":" ++ str(id) ++ "}}"

def failed(p: String, why: String) -> String:
  "{\"error\":{\"platform\":" ++ str(p) ++ ",\"message\":" ++ str(why) ++ "}}"

def bad(why: String) -> String:
  "{\"error\":{\"message\":" ++ str(why) ++ "}}"

# The line a client gets for how a send to platform p ended.
def reply(p: String, o: Outcome) -> String:
  match o:
    case Sent{id}:
      said(p, id)
    case Refused{why}:
      failed(p, why)

# The WebSocket handshake
# -----------------------

def head.pick(hit: Bool, +acc: String, +s: String, r: Maybe<&2, Two>) -> Maybe<&2, Two>:
  match hit:
    case True{}:
      Some{Two{String.reverse(acc), String.drop(s, 4n)}}
    case False{}:
      r

def head.go(+s: String, +acc: String) -> Maybe<&2, Two>:
  match s:
    case SNil{}:
      None{}
    case SCon{+h, t}:
      head.pick(String.starts_with(s, "\r\n\r\n"), acc, s, head.go(t, SCon{h, acc}))

# An HTTP head once its blank line came: the head, and the bytes after it.
def head(buf: String) -> Maybe<&2, Two>:
  head.go(buf, "")

def header.pick(hit: Bool, +l: String, rest: String) -> String:
  match hit:
    case True{}:
      String.trim(second(cut(l, ':')))
    case False{}:
      rest

def header.go(xs: List<&2, String>, +name: String) -> String:
  match xs:
    case Nil{}:
      ""
    case Con{+l, t}:
      header.pick(String.eq(String.to_lower(String.trim(first(cut(l, ':')))), name), l, header.go(t, name))

# The value of the first header line called name (lowercase); "" when there is none.
def header(head: String, +name: String) -> String:
  header.go(String.lines(head), name)

# Config and flags
# ----------------

def byo.table(+l: String, in: Bool) -> Bool:
  Bool.pick(Bool, String.starts_with(l, "["), String.eq(String.trim(first(cut(drop1(l, '['), ']'))), "byo"), in)

def byo.add(hit: Bool, +l: String, +acc: List<&2, Two>) -> List<&2, Two>:
  match hit:
    case True{}:
      +kv = cut(l, '=')
      Two{String.trim(first(kv)), unquote(String.trim(second(kv)))} <> acc
    case False{}:
      acc

def byo.go(xs: List<&2, String>, +in: Bool, +acc: List<&2, Two>) -> List<&2, Two>:
  match xs:
    case Nil{}:
      List.reverse(&2, Two, acc)
    case Con{x, t}:
      +l = String.trim(first(cut(x, '#')))
      +i = byo.table(l, in)
      byo.go(t, i, byo.add(Bool.and(i, Bool.and(String.contains(l, "="), Bool.not(String.starts_with(l, "[")))), l, acc))

# Every key = value of the [byo] table of remux's config.toml, in file order. Each
# key names an integration; its value is what that integration reads (a channel, a video).
def byo(text: String) -> List<&2, Two>:
  byo.go(String.lines(text), False{}, [])

# The value of key; "" when it is not there.
def config.get(cfg: List<&2, Two>, +key: String) -> String:
  match cfg:
    case Nil{}:
      ""
    case Con{kv, t}:
      match kv:
        case Two{k, v}:
          Bool.pick(String, String.eq(k, key), v, config.get(t, key))

type Opts is Data:
  Opts{port: U32, socket: String, bad: String}

def opts.port(o: Opts, m: Maybe<&2, U32>, +v: String) -> Opts:
  match o:
    case Opts{p, so, bad}:
      match m:
        case None{}:
          Opts{p, so, or_else(bad, "--port " ++ v)}
        case Some{n}:
          Opts{n, so, bad}

def opts.set(+flag: String, +v: String, o: Opts) -> Opts:
  match o:
    case Opts{+p, +so, +bad}:
      Bool.pick(Opts, String.eq(flag, "--socket"), Opts{p, v, bad},
        Bool.pick(Opts, String.eq(flag, "--port"), opts.port(Opts{p, so, bad}, U32.read(v), v),
          Opts{p, so, or_else(bad, flag)}))

def opts.lone(+flag: String, o: Opts) -> Opts:
  match o:
    case Opts{p, so, bad}:
      Opts{p, so, or_else(bad, flag ++ " needs a value")}

# --port <n> and --socket <path>, over o. The first word it could not use is in bad.
# What to read is config, not flags: the [byo] table.
def opts(args: List<&2, String>, o: Opts) -> Opts:
  match args:
    case Nil{}:
      o
    case Con{flag, Nil{}}:
      opts.lone(flag, o)
    case Con{flag, Con{v, rest}}:
      opts(rest, opts.set(flag, v, o))

# The line a client gets for how a delete on platform p ended.
def reply.deleted(p: String, o: Outcome) -> String:
  match o:
    case Sent{id}:
      "{\"deleted\":{\"platform\":" ++ str(p) ++ ",\"id\":" ++ str(id) ++ "}}"
    case Refused{why}:
      failed(p, why)

# remux's wire, up
# ----------------
# What the engine asks of a chat bridge (docs/wire.md): say, into the chat called
# channel ("" for every chat), or delete a line by its id. The rest (heartbeats,
# open) asks for nothing.

type Asked is Data:
  AskSay{channel: String, body: String}
  AskDelete{channel: String, id: String}
  AskBad{about: String, why: String}
  AskNothing{}

def asked.say(+say: J) -> Asked:
  +channel = j.str(j.get(say, "channel"))
  +body = j.str(j.get(say, "body"))
  Bool.pick(Asked, String.is_empty(body), AskBad{channel, "a say with no body"},
    Bool.pick(Asked, has_control(body), AskBad{channel, "a say with a line break or another control character"},
      AskSay{channel, body}))

def asked.delete(+del: J) -> Asked:
  +channel = j.str(j.get(del, "channel"))
  +id = j.str(j.get(del, "id"))
  Bool.pick(Asked, Bool.or(String.is_empty(channel), String.is_empty(id)), AskBad{channel, "a delete needs an id and a channel"}, AskDelete{channel, id})

# One frame from the engine, as parsed JSON.
def asked(+frame: J) -> Asked:
  +say = j.get(frame, "say")
  +del = j.get(frame, "delete")
  Bool.pick(Asked, j.is_obj(say), asked.say(say),
    Bool.pick(Asked, j.is_obj(del), asked.delete(del), AskNothing{}))

# The chats a say into channel goes to: the one called so, or every one for "".
def asked.targets(+channel: String, channels: List<&2, String>) -> List<&2, String>:
  Bool.pick(List<&2, String>, String.is_empty(channel), channels, [channel])

# remux's wire, down: what went wrong, or right, with what the engine asked.
def notice(about: String, text: String, +fine: Bool) -> String:
  "{\"notice\":{\"about\":" ++ str(about) ++ ",\"text\":" ++ str(text) ++ ",\"fine\":" ++ Bool.pick(String, fine, "true", "false") ++ "}}"

# The notice for how a delete ended.
def notice.deleted(about: String, o: Outcome) -> String:
  match o:
    case Sent{id}:
      notice(about, "deleted " ++ id, True{})
    case Refused{why}:
      notice(about, "delete failed: " ++ why, False{})

# The notice for a say that failed; none for one that went out, as its line comes
# back down like anybody's.
def notice.said(about: String, o: Outcome) -> Maybe<&2, String>:
  match o:
    case Sent{id}:
      None{}
    case Refused{why}:
      Some{notice(about, "say failed: " ++ why, False{})}

# Whether remux's wire gets an event frame too: for all but plain chat, which the
# line carries. A tip's message comes as a line and as the event, with one id.
def is_event(e: ChatEvent) -> Bool:
  match e:
    case ChatEvent{what, id, platform, channel, from, body, badges, reply}:
      match what:
        case Chat{}:
          False{}
        case Sub{months, tier}:
          True{}
        case Gift{count, tier, to}:
          True{}
        case Tip{amount, currency, micros}:
          True{}
        case Raid{viewers}:
          True{}
        case Follow{}:
          True{}
        case Deleted{target}:
          True{}
        case Banned{user, seconds}:
          True{}
        case Cleared{}:
          True{}
        case Custom{name, fields}:
          True{}

def frames.line(m: Maybe<&2, Line>, rest: List<&2, String>) -> List<&2, String>:
  match m:
    case None{}:
      rest
    case Some{l}:
      wire(l) <> rest

# The frames an event makes on remux's wire: its line, if it has one, then the
# event itself, unless it is plain chat.
def frames(+e: ChatEvent) -> List<&2, String>:
  frames.line(as_line(e), Bool.pick(List<&2, String>, is_event(e), [event.json(e)], []))
