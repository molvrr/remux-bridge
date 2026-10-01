# Twitch, as an Integration: chat, subs, gifts, cheers and raids read over IRC as a
# guest; with a token, follows, redemptions, hype trains, polls, predictions, the
# stream going up or down and raids out over EventSub; and messages posted and
# deleted through Helix.
#   [byo] twitch = "<channel>"
# To post, a user token with user:write:chat; to delete, with
# moderator:manage:chat_messages too. Either one that lasts until it expires:
#   TWITCH_CHAT_TOKEN
# or one the bridge renews before it expires, from what `twitch token -u` printed:
#   TWITCH_REFRESH_TOKEN, and the app it was issued to: TWITCH_CLIENT_ID, TWITCH_CLIENT_SECRET
# EventSub subscribes to what the token's scopes allow and names the rest in the
# log: moderator:read:followers, channel:read:redemptions, channel:read:hype_train,
# channel:read:polls, channel:read:predictions (the stream and raids need none).
# Unproven: the parsing it relies on is in logic.bend and eventsub.bend, where
# LAWS.bend proves it.
import Base
import bend-kit-wire@0.4.2.0/wire.bend as Wire
import bend-kit-dns@0.5.0.0/dns.bend as Dns
import ../core.bend as C
import ../net.bend as N
import bend-kit-websocket@0.1.0.0/websocket.bend as Ws
import bend-kit-bytes@0.3.1.0/bytes.bend as WB
import ./logic.bend as L
import ./eventsub.bend as E

# A refresh token and the app it was issued to.
type Creds is Data:
  Creds{refresh: String, client: String, secret: String}

type Conf is Data:
  Conf{channel: String, token: String, creds: Creds}

# Who posts, and where: looked up once, then kept.
type Auth is Data:
  Auth{client: String, sender: String, broadcaster: String, scopes: List<&2, String>}

# What the sender keeps: the token in use until a time (ms of IO.now; 0 for one
# that is not renewed), the refresh token to renew it with next, and who posts.
type Keep is Data:
  Keep{token: String, refresh: String, until: Nat, auth: Maybe<&2, Auth>}

# Setup
# -----

def renews(creds: Creds) -> Bool:
  match creds:
    case Creds{+refresh, +client, +secret}:
      Bool.and(Bool.not(String.is_empty(refresh)), Bool.and(Bool.not(String.is_empty(client)), Bool.not(String.is_empty(secret))))

def setup.of(+channel: String, token: String, creds: Creds) -> Result<&1, &1, String, Conf>:
  Bool.pick(Result<&1, &1, String, Conf>, String.is_empty(channel), Fail{"the value is a channel name, and it is empty"}, Done{Conf{channel, L.token(token), creds}})

def setup(v: String) -> IO(Result<&1, &1, String, Conf>):
  do IO<Result<&1, &1, String, Conf>>:
    token : String <- N.getenv("TWITCH_CHAT_TOKEN")
    refresh : String <- N.getenv("TWITCH_REFRESH_TOKEN")
    client : String <- N.getenv("TWITCH_CLIENT_ID")
    secret : String <- N.getenv("TWITCH_CLIENT_SECRET")
    return setup.of(L.channel(v), token, Creds{refresh, client, secret})

def channel(conf: Conf) -> String:
  match conf:
    case Conf{channel, token, creds}:
      channel

def sends(conf: Conf) -> Bool:
  match conf:
    case Conf{channel, token, creds}:
      Bool.or(Bool.not(String.is_empty(token)), renews(creds))


# Reading: IRC over TLS, as a read-only guest. Every failure waits 5 s and dials again.
# ----------------------------------------------------------------------------

def host() -> String:
  "irc.chat.twitch.tv"

@unsafe
def dial(+channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  do IO<Unit>:
    ip : Maybe<&2, String> <- Dns.resolve(host())
    dial.ip(ip, channel, hub)

def retry(why: String, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  do IO<Unit>:
    N.later(5000, "twitch: " ++ why ++ "; again in 5 s")
    dial(channel, hub)

def sock(r: Socket & Result<&1, &1, U32 & String, Unit>) -> Socket:
  (s, x) = r
  s

def one(s: Socket, m: L.Irc, +hub: Chan(C.Up)) -> IO(Socket):
  match m:
    case L.Ping{token}:
      do IO<Socket>:
        r : Socket & Result<&1, &1, U32 & String, Unit> <- Wire.tls.send(s, L.pong(token))
        return sock(r)
    case L.Got{e}:
      do IO<Socket>:
        N.emit(hub, e)
        return s
    case L.Other{}:
      IO.pure(Socket, s)

def each(xs: List<&2, String>, s: Socket, +hub: Chan(C.Up)) -> IO(Socket):
  match xs:
    case Nil{}:
      IO.pure(Socket, s)
    case Con{x, t}:
      do IO<Socket>:
        now : Nat <- IO.now()
        s2 : Socket <- one(s, L.irc(x, "twitch-" ++ Nat.show(now)), hub)
        each(t, s2, hub)

@unsafe
def read.loop(s: Socket, +buf: String, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  do IO<Unit>:
    r : Socket & Result<&1, &1, U32 & String, String> <- Wire.tls.recv(s, 65536, 360000)
    got(r, buf, channel, hub)

def split(s: Socket, p: List<&2, String> & String, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  (xs, rest) = p
  do IO<Unit>:
    s2 : Socket <- each(xs, s, hub)
    read.loop(s2, rest, channel, hub)

def chunk(closed: Bool, s: Socket, +all: String, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  match closed:
    case True{}:
      do IO<Unit>:
        Wire.tls.close(s)
        retry("twitch closed the connection", channel, hub)
    case False{}:
      split(s, C.lines(all), channel, hub)

def got(r: Socket & Result<&1, &1, U32 & String, String>, +buf: String, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  (s, res) = r
  match res:
    case Fail{(code, why)}:
      do IO<Unit>:
        Wire.tls.close(s)
        retry(why, channel, hub)
    case Done{+text}:
      chunk(String.is_empty(text), s, buf ++ text, channel, hub)

def hello(s: Socket, +channel: String, +hub: Chan(C.Up), n: Result<&1, &1, U32 & String, U32>) -> IO(Unit):
  +k = N.or_u32(n, 4242)
  do IO<Unit>:
    r : Socket & Result<&1, &1, U32 & String, Unit> <- Wire.tls.send(s, L.hello(channel, k))
    N.log("twitch: reading #" ++ channel)
    read.loop(sock(r), "", channel, hub)

def tls(m: Socket & Result<&1, &1, U32 & String, Unit>, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  (s, r) = m
  match r:
    case Fail{(code, why)}:
      do IO<Unit>:
        Socket.close(s)
        retry(why, channel, hub)
    case Done{u}:
      do IO<Unit>:
        n : Result<&1, &1, U32 & String, U32> <- IO.random_u32()
        hello(s, channel, hub, n)

def tcp(r: Result<&1, &1, U32 & String, Socket>, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  match r:
    case Fail{(code, why)}:
      retry(why, channel, hub)
    case Done{s}:
      do IO<Unit>:
        m : Socket & Result<&1, &1, U32 & String, Unit> <- Wire.tls.connect(s, host(), 10000)
        tls(m, channel, hub)

def dial.ip(ip: Maybe<&2, String>, +channel: String, +hub: Chan(C.Up)) -> IO(Unit):
  match ip:
    case None{}:
      retry("cannot resolve " ++ host(), channel, hub)
    case Some{addr}:
      do IO<Unit>:
        r : Result<&1, &1, U32 & String, Socket> <- Wire.connect(addr, 6697, 10000)
        tcp(r, channel, hub)



# Posting and deleting, through Helix.
# ----------------------------------------------------------------------------

def helix(+token: String, client: String) -> Map<&2, List<&2, String>>:
  N.header(N.json.headers(token), "client-id", client)

# The token: TWITCH_CHAT_TOKEN as it is, or a renewed one, renewed again a minute
# before it expires.

def fresh(r: Result<&2, &2, String, L.Renewal>, +now: Nat, auth: Maybe<&2, Auth>) -> Result<&2, &2, String, Keep>:
  match r:
    case Fail{why}:
      Fail{why}
    case Done{renewal}:
      match renewal:
        case L.Renewal{token, refresh, secs}:
          +life = U32.to_nat(C.number(secs, 14400))
          Done{Keep{token, refresh, Nat.add(now, Nat.sub(Nat.mul(life, 1000n), 60000n)), auth}}

def renewed(r: Result<&2, &2, String, N.Ans>, +refresh: String, +now: Nat, auth: Maybe<&2, Auth>) -> Result<&2, &2, String, Keep>:
  match r:
    case Fail{why}:
      Fail{"renewing TWITCH_REFRESH_TOKEN failed: " ++ why}
    case Done{ans}:
      match ans:
        case N.Ans{status, j}:
          fresh(L.renewed(j, refresh), now, auth)

def renew(creds: Creds, +refresh: String, +now: Nat, auth: Maybe<&2, Auth>) -> IO(Result<&2, &2, String, Keep>):
  match creds:
    case Creds{r, client, secret}:
      do IO<Result<&2, &2, String, Keep>>:
        a : Result<&2, &2, String, N.Ans> <- N.post(L.renew_url(), N.header(N.headers(), "content-type", "application/x-www-form-urlencoded"), L.renew_form(client, secret, refresh))
        return renewed(a, refresh, now, auth)

def first.refresh(creds: Creds) -> String:
  match creds:
    case Creds{refresh, client, secret}:
      refresh

# The refresh token to renew with: the last one Twitch handed, else the one given.
def token(conf: Conf, keep: Keep, +now: Nat, +need: String) -> IO(Result<&2, &2, String, Keep>):
  match conf:
    case Conf{channel, +given, +creds}:
      match keep:
        case Keep{+token, +refresh, +until, +auth}:
          Bool.pick(IO(Result<&2, &2, String, Keep>), Bool.not(renews(creds)),
            IO.pure(Result<&2, &2, String, Keep>, Bool.pick(Result<&2, &2, String, Keep>, String.is_empty(given), Fail{need}, Done{Keep{given, "", 0n, auth}})),
            Bool.pick(IO(Result<&2, &2, String, Keep>), Bool.and(Bool.not(String.is_empty(token)), Nat.is_lt(now, until)),
              IO.pure(Result<&2, &2, String, Keep>, Done{Keep{token, refresh, until, auth}}),
              renew(creds, C.or_else(refresh, first.refresh(creds)), now, auth)))

# Who posts, and where: the token's user and app (GET oauth2/validate), and the
# channel's user id, asked once and kept.

def found(r: Result<&2, &2, String, N.Ans>, +client: String, +user: String, +scopes: List<&2, String>) -> Result<&2, &2, String, Auth>:
  match r:
    case Fail{why}:
      Fail{"looking up the channel failed: " ++ why}
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          +b = L.user_id(j)
          Bool.pick(Result<&2, &2, String, Auth>, String.is_empty(b),
            Fail{Bool.pick(String, U32.is_eq(status, 200), "no Twitch channel by that name", "looking up the channel failed: " ++ U32.show(status) ++ " " ++ C.api.why(j))},
            Done{Auth{client, user, b, scopes}})

def lookup(me: Result<&2, &2, String, L.Me>, +token: String, +channel: String) -> IO(Result<&2, &2, String, Auth>):
  match me:
    case Fail{why}:
      IO.pure(Result<&2, &2, String, Auth>, Fail{why})
    case Done{m}:
      match m:
        case L.Me{+client, +user, +scopes}:
          do IO<Result<&2, &2, String, Auth>>:
            r : Result<&2, &2, String, N.Ans> <- N.get(L.users_url(channel), helix(token, client))
            return found(r, client, user, scopes)

def validated(r: Result<&2, &2, String, N.Ans>) -> Result<&2, &2, String, L.Me>:
  match r:
    case Fail{why}:
      Fail{"validating the Twitch token failed: " ++ why}
    case Done{ans}:
      match ans:
        case N.Ans{status, j}:
          L.me(j)

def with.auth(a: Result<&2, &2, String, Auth>, +keep: Keep) -> Result<&2, &2, String, Keep>:
  match a:
    case Fail{why}:
      Fail{why}
    case Done{auth}:
      match keep:
        case Keep{token, refresh, until, old}:
          Done{Keep{token, refresh, until, Some{auth}}}

def who(+keep: Keep, +channel: String) -> IO(Result<&2, &2, String, Keep>):
  match keep:
    case Keep{+token, +refresh, +until, +auth}:
      Bool.pick(IO(Result<&2, &2, String, Keep>), Maybe.is_some(&2, Auth, auth), IO.pure(Result<&2, &2, String, Keep>, Done{Keep{token, refresh, until, auth}}),
        do IO<Result<&2, &2, String, Keep>>:
          v : Result<&2, &2, String, N.Ans> <- N.get("https://id.twitch.tv/oauth2/validate", N.header(N.headers(), "authorization", "OAuth " ++ token))
          a : Result<&2, &2, String, Auth> <- lookup(validated(v), token, channel)
          return with.auth(a, Keep{token, refresh, until, auth}))

def ready.who(t: Result<&2, &2, String, Keep>, +channel: String) -> IO(Result<&2, &2, String, Keep>):
  match t:
    case Fail{why}:
      IO.pure(Result<&2, &2, String, Keep>, Fail{why})
    case Done{k}:
      who(k, channel)

# A token, renewed if due, and who posts where: all an act needs.
def ready(+conf: Conf, +keep: Keep, +need: String) -> IO(Result<&2, &2, String, Keep>):
  do IO<Result<&2, &2, String, Keep>>:
    now : Nat <- IO.now()
    t : Result<&2, &2, String, Keep> <- token(conf, keep, now, need)
    ready.who(t, channel(conf))

# A 401 forgets the token and who we are, so the next act renews or looks again.
def after(+status: U32, keep: Keep) -> Keep:
  match keep:
    case Keep{+token, +refresh, +until, +auth}:
      Bool.pick(Keep, U32.is_eq(status, 401), Keep{"", refresh, 0n, None{}}, Keep{token, refresh, until, auth})

def acted(r: U32 & C.Outcome, +keep: Keep) -> Keep & C.Outcome:
  (+status, o) = r
  (after(status, keep), o)

def posted(r: Result<&2, &2, String, N.Ans>) -> U32 & C.Outcome:
  match r:
    case Fail{why}:
      (0, C.Refused{"the request failed: " ++ why})
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          (status, N.outcome.pick(U32.is_eq(status, 200), status, j, L.sent(j)))

def act.post(+delete: Bool, +keep: Keep, +text: String, +token: String, +client: String, +sender: String, +b: String) -> IO(Keep & C.Outcome):
  Bool.pick(IO(Keep & C.Outcome), delete,
    do IO<Keep & C.Outcome>:
      r : Result<&2, &2, String, N.Ans> <- N.delete(L.delete_url(b, sender, text), helix(token, client))
      return acted(N.deleted(r, text), keep),
    do IO<Keep & C.Outcome>:
      r : Result<&2, &2, String, N.Ans> <- N.post("https://api.twitch.tv/helix/chat/messages", helix(token, client), L.body(b, sender, text))
      return acted(posted(r), keep))

# Why the token may not, or the act.
def act.scoped(m: Maybe<&2, String>, +keep: Keep, go: IO(Keep & C.Outcome)) -> IO(Keep & C.Outcome):
  match m:
    case Some{why}:
      IO.pure(Keep & C.Outcome, (keep, C.Refused{why}))
    case None{}:
      go

# An act on the platform, once ready: post (text is a body) or delete (text is an
# id), each after its own scope.
def act.go(+delete: Bool, keep: Keep, +text: String) -> IO(Keep & C.Outcome):
  match keep:
    case Keep{+token, +refresh, +until, +auth}:
      match auth:
        case None{}:
          IO.pure(Keep & C.Outcome, (Keep{token, refresh, until, None{}}, C.Refused{"nobody to act as"}))
        case Some{a}:
          match a:
            case Auth{+client, +sender, +b, +scopes}:
              +k = {Keep{token, refresh, until, Some{Auth{client, sender, b, scopes}}} : Keep}
              act.scoped(L.need(scopes, Bool.pick(String, delete, "moderator:manage:chat_messages", "user:write:chat")), k,
                act.post(delete, k, text, token, client, sender, b))

def act.ready(r: Result<&2, &2, String, Keep>, +keep: Keep, +delete: Bool, +text: String) -> IO(Keep & C.Outcome):
  match r:
    case Fail{why}:
      IO.pure(Keep & C.Outcome, (keep, C.Refused{why}))
    case Done{k}:
      act.go(delete, k, text)

def act(+conf: Conf, +keep: Keep, +delete: Bool, +text: String) -> IO(Keep & C.Outcome):
  +need = Bool.pick(String, delete, "deleting needs TWITCH_CHAT_TOKEN, or TWITCH_REFRESH_TOKEN with TWITCH_CLIENT_ID and TWITCH_CLIENT_SECRET, for a user token with moderator:manage:chat_messages",
    "posting needs TWITCH_CHAT_TOKEN, or TWITCH_REFRESH_TOKEN with TWITCH_CLIENT_ID and TWITCH_CLIENT_SECRET, for a user token with user:write:chat")
  do IO<Keep & C.Outcome>:
    r : Result<&2, &2, String, Keep> <- ready(conf, keep, need)
    act.ready(r, keep, delete, text)

def send(conf: Conf, keep: Keep, body: String) -> IO(Keep & C.Outcome):
  act(conf, keep, False{}, body)

def delete(conf: Conf, keep: Keep, id: String) -> IO(Keep & C.Outcome):
  act(conf, keep, True{}, id)

# EventSub: what IRC cannot see, over a WebSocket session, subscribed with the
# posting token (renewed on its own). A session silent for longer than its
# keepalive is gone, and dialed again.
# ----------------------------------------------------------------------------

type Subbed is Data:
  Subbed{ok: List<&2, String>, no: List<&2, String>}

def es.took(r: Result<&2, &2, String, N.Ans>, +name: String, s: Subbed) -> Subbed:
  match r:
    case Fail{why}:
      match s:
        case Subbed{ok, no}:
          Subbed{ok, (name ++ ": " ++ why) <> no}
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          match s:
            case Subbed{+ok, +no}:
              Bool.pick(Subbed, U32.is_eq(status, 202), Subbed{name <> ok, no}, Subbed{ok, (name ++ ": " ++ U32.show(status) ++ " " ++ C.api.why(j)) <> no})

def es.subs(ws: List<&2, E.Want>, +session: String, +token: String, +client: String) -> IO(Subbed):
  match ws:
    case Nil{}:
      IO.pure(Subbed, Subbed{[], []})
    case Con{+w, t}:
      do IO<Subbed>:
        r : Result<&2, &2, String, N.Ans> <- N.post(E.subscriptions_url(), helix(token, client), E.body(w, session))
        rest : Subbed <- es.subs(t, session, token, client)
        return es.took(r, E.name(w), rest)

def es.log.list(prefix: String, +xs: List<&2, String>) -> IO(Unit):
  Bool.pick(IO(Unit), List.is_empty(&2, String, xs), IO.pure(Unit, Unit{}), N.log(prefix ++ String.join(xs, ", ")))

def es.report(s: Subbed, +skipped: List<&2, String>) -> IO(Unit):
  match s:
    case Subbed{+ok, no}:
      do IO<Unit>:
        N.log("twitch: eventsub on: " ++ C.or_else(String.join(ok, ", "), "nothing"))
        es.log.list("twitch: eventsub refused: ", no)
        es.log.list("twitch: eventsub skipped, the token lacks a scope: ", skipped)

def es.any(s: Subbed) -> Bool:
  match s:
    case Subbed{ok, no}:
      Bool.not(List.is_empty(&2, String, ok))

def es.reported(+s: Subbed, +skipped: List<&2, String>) -> IO(Bool):
  do IO<Bool>:
    es.report(s, skipped)
    return es.any(s)

# Whether anything was subscribed.
def es.planned(p: E.Plan, +session: String, +token: String, +client: String) -> IO(Bool):
  match p:
    case E.Plan{ws, skipped}:
      do IO<Bool>:
        s : Subbed <- es.subs(ws, session, token, client)
        es.reported(s, skipped)

def es.ready(r: Result<&2, &2, String, Keep>, +keep: Keep, +session: String) -> IO(Keep & Bool):
  match r:
    case Fail{why}:
      do IO<Keep & Bool>:
        N.log("twitch: eventsub: " ++ why)
        return (keep, False{})
    case Done{k}:
      match k:
        case Keep{+token, +refresh, +until, +auth}:
          match auth:
            case None{}:
              IO.pure(Keep & Bool, (Keep{token, refresh, until, None{}}, False{}))
            case Some{a}:
              match a:
                case Auth{+client, +sender, +b, +scopes}:
                  do IO<Keep & Bool>:
                    ok : Bool <- es.planned(E.plan(b, sender, scopes), session, token, client)
                    return (Keep{token, refresh, until, Some{Auth{client, sender, b, scopes}}}, ok)

# Subscribes the session to what the token's scopes allow, as the token's user;
# and whether anything was.
def es.subscribe(+conf: Conf, +keep: Keep, +session: String) -> IO(Keep & Bool):
  do IO<Keep & Bool>:
    r : Result<&2, &2, String, Keep> <- ready(conf, keep, "EventSub needs TWITCH_CHAT_TOKEN, or TWITCH_REFRESH_TOKEN with TWITCH_CLIENT_ID and TWITCH_CLIENT_SECRET")
    es.ready(r, keep, session)

@unsafe
def es.dial(+conf: Conf, +hub: Chan(C.Up), +keep: Keep, +url: String, +fresh: Bool) -> IO(Unit):
  +hp = E.ws_url(url)
  do IO<Unit>:
    ip : Maybe<&2, String> <- Dns.resolve(C.first(hp))
    es.ip(ip, conf, hub, keep, hp, fresh)

def es.retry(why: String, +conf: Conf, +hub: Chan(C.Up), +keep: Keep) -> IO(Unit):
  do IO<Unit>:
    N.later(5000, "twitch: eventsub: " ++ why ++ "; again in 5 s")
    es.dial(conf, hub, keep, E.base_url(), True{})

@unsafe
def es.loop(c: Ws.Conn, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +fresh: Bool, +wait: U32) -> IO(Unit):
  do IO<Unit>:
    r : Ws.Conn & Result<&1, &1, U32 & String, Ws.Incoming> <- Ws.recv(c, (wait * 1000 : U32))
    es.heard(r, conf, hub, keep, fresh, wait)

# A session with nothing subscribed is closed by Twitch within seconds: rather than
# dial again at once, forever, the bridge waits a while (a token may be renewed or
# replaced meanwhile).
def es.subscribed(r: Keep & Bool, c: Ws.Conn, +conf: Conf, +hub: Chan(C.Up), +wait: U32) -> IO(Unit):
  (+k, ok) = r
  match ok:
    case True{}:
      es.loop(c, conf, hub, k, False{}, wait)
    case False{}:
      do IO<Unit>:
        Ws.shutdown(c)
        N.later(300000, "twitch: eventsub: nothing subscribed; again in 5 min")
        es.dial(conf, hub, k, E.base_url(), True{})

# A welcome: a fresh session is subscribed; one moved to by a reconnect keeps
# what it had.
def es.welcome(fresh: Bool, c: Ws.Conn, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +session: String, +wait: U32) -> IO(Unit):
  match fresh:
    case True{}:
      do IO<Unit>:
        r : Keep & Bool <- es.subscribe(conf, keep, session)
        es.subscribed(r, c, conf, hub, wait)
    case False{}:
      es.loop(c, conf, hub, keep, False{}, wait)

def es.note(m: Maybe<&2, C.ChatEvent>, c: Ws.Conn, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +fresh: Bool, +wait: U32) -> IO(Unit):
  match m:
    case None{}:
      es.loop(c, conf, hub, keep, fresh, wait)
    case Some{e}:
      do IO<Unit>:
        N.emit(hub, e)
        es.loop(c, conf, hub, keep, fresh, wait)

def es.msg(e: E.Es, c: Ws.Conn, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +fresh: Bool, +wait: U32) -> IO(Unit):
  match e:
    case E.Welcome{session, keepalive}:
      es.welcome(fresh, c, conf, hub, keep, session, (keepalive + 10 : U32))
    case E.Keepalive{}:
      es.loop(c, conf, hub, keep, fresh, wait)
    case E.Note{m}:
      es.note(m, c, conf, hub, keep, fresh, wait)
    case E.Reconnect{url}:
      do IO<Unit>:
        Ws.shutdown(c)
        N.log("twitch: eventsub moves to a new session")
        es.dial(conf, hub, keep, url, False{})
    case E.Revoked{topic, status}:
      do IO<Unit>:
        N.log("twitch: eventsub " ++ topic ++ " ended by Twitch: " ++ status)
        es.loop(c, conf, hub, keep, fresh, wait)
    case E.Unknown{}:
      es.loop(c, conf, hub, keep, fresh, wait)

def es.quiet(+code: U32) -> Bool:
  Bool.or(U32.is_eq(code, 60), U32.is_eq(code, 110))

def es.heard(r: Ws.Conn & Result<&1, &1, U32 & String, Ws.Incoming>, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +fresh: Bool, +wait: U32) -> IO(Unit):
  (c, res) = r
  match res:
    case Fail{(+code, why)}:
      do IO<Unit>:
        Ws.shutdown(c)
        es.retry(Bool.pick(String, es.quiet(code), "Twitch went quiet past the keepalive", why), conf, hub, keep)
    case Done{m}:
      match m:
        case Ws.Text{data}:
          es.msg(E.es(N.json.of(WB.to_string(data))), c, conf, hub, keep, fresh, wait)
        case Ws.Binary{data}:
          es.loop(c, conf, hub, keep, fresh, wait)
        case Ws.Pong{data}:
          es.loop(c, conf, hub, keep, fresh, wait)
        case Ws.Closed{code, reason}:
          do IO<Unit>:
            Ws.shutdown(c)
            es.retry("Twitch closed the session (" ++ U32.show(code) ++ ")", conf, hub, keep)

def es.connected(r: Result<&1, &1, U32 & String, Ws.Conn>, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +fresh: Bool) -> IO(Unit):
  match r:
    case Fail{(code, why)}:
      es.retry(why, conf, hub, keep)
    case Done{c}:
      es.loop(c, conf, hub, keep, fresh, 40)

def es.ip(ip: Maybe<&2, String>, +conf: Conf, +hub: Chan(C.Up), +keep: Keep, +hp: C.Two, +fresh: Bool) -> IO(Unit):
  match ip:
    case None{}:
      es.retry("cannot resolve " ++ C.first(hp), conf, hub, keep)
    case Some{addr}:
      do IO<Unit>:
        r : Result<&1, &1, U32 & String, Ws.Conn> <- Ws.connect(addr, 443, C.first(hp), C.second(hp), True{}, 1048576, 10000)
        es.connected(r, conf, hub, keep, fresh)

# Reading: IRC always; EventSub too when there is a token to subscribe with.
def read.go(conf: Conf, +hub: Chan(C.Up)) -> IO(Unit):
  match conf:
    case Conf{+channel, +token, +creds}:
      +c = {Conf{channel, token, creds} : Conf}
      do IO<Unit>:
        Bool.pick(IO(Unit), sends(c), IO.spawn(Unit, es.dial(c, hub, Keep{"", "", 0n, None{}}, E.base_url(), True{})), IO.pure(Unit, Unit{}))
        dial(channel, hub)

def read(conf: Conf, hub: Chan(C.Up)) -> IO(Unit):
  read.go(conf, hub)

# The Integration
# ---------------

def it() -> C.Integration<Conf, Keep>:
  C.Integration{"twitch", 500, setup, channel, read, sends, Keep{"", "", 0n, None{}}, send, delete}
