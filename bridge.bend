# A chat bridge of your own, in Bend: every integration's chat on remux's wire, and
# every event, with posting, on a Unix socket. A port of remux's byo/bridge.py.
#
#   ./bridge [--port 9999] [--socket ~/.config/remux/bridge.sock]
#   remux chat url ws://127.0.0.1:9999
#
# What to read is the [byo] table of remux's config (~/.config/remux/config.toml,
# or $REMUX_CONFIG): each key names an integration, its value what that reads.
#   [byo]
#   twitch = "kartths"
#   youtube = "<video id>"
# Tokens come from the environment; each integration's file says which.
#
# remux's wire (ws://127.0.0.1:<port>, docs/wire.md) gets {"line": {id, platform,
# channel, from, body}} for what is written in chat, and {"event": …} for the rest.
# Up, it acts on the engine's say and delete, and answers with a notice what went
# wrong (and what a delete did).
# The socket (mode 0600) speaks NDJSON: every event as {"event": {"type": "chat" |
# "sub" | "gift" | "tip" | "raid" | "follow" | "deleted" | "banned" | "cleared" |
# "custom", …}}; {"say": {"platform": "<name>" | "all", "body": "…"}} posts and is
# answered with {"said": …} or {"error": …} per platform; {"delete": {"platform",
# "id"}} takes a message down and is answered with {"deleted": …} or {"error": …}.
#
# Adding an integration: write <name>/logic.bend (pure, with laws) and
# <name>/<name>.bend with an it() returning a C.Integration (see twitch/), import it
# below, and add one plug line to integrations().
#
# Unproven by design: this file only moves bytes. What they mean is in core.bend
# and the integrations' logic.bend, and LAWS.bend states what those do.
import Base
import bend-kit-websocket@0.1.0.0/websocket.bend as Ws
import bend-kit-wire@0.4.2.0/wire.bend as Wire
import bend-kit-bytes@0.3.1.0/bytes.bend as WB
import ./core.bend as C
import ./net.bend as N
import ./unix/unix.bend as Unix
import ./twitch/twitch.bend as Twitch
import ./youtube/youtube.bend as YouTube

# The hub: one computation holds every client's inbox and hands each event to all.
# ----------------------------------------------------------------------------

def hub.keep(ok: Bool, +c: Chan(C.Down), +rest: List<&2, Chan(C.Down)>) -> List<&2, Chan(C.Down)>:
  Bool.pick(List<&2, Chan(C.Down)>, ok, c <> rest, rest)

# Pushes e to every inbox; the ones that are closed drop out.
def hub.push(clients: List<&2, Chan(C.Down)>, +e: C.ChatEvent) -> IO(List<&2, Chan(C.Down)>):
  match clients:
    case Nil{}:
      IO.pure(List<&2, Chan(C.Down)>, [])
    case Con{+c, t}:
      do IO<List<&2, Chan(C.Down)>>:
        ok : Bool <- Chan.send(C.Down, c, C.Push{e})
        rest : List<&2, Chan(C.Down)> <- hub.push(t, e)
        return hub.keep(ok, c, rest)

@unsafe
def hub.run(+ch: Chan(C.Up), clients: List<&2, Chan(C.Down)>) -> IO(Unit):
  do IO<Unit>:
    m : Maybe<&1, C.Up> <- Chan.recv(C.Up, ch)
    hub.on(ch, clients, m)

def hub.on(+ch: Chan(C.Up), clients: List<&2, Chan(C.Down)>, m: Maybe<&1, C.Up>) -> IO(Unit):
  match m:
    case None{}:
      IO.pure(Unit, Unit{})
    case Some{up}:
      match up:
        case C.Join{inbox}:
          hub.run(ch, inbox <> clients)
        case C.Happened{e}:
          do IO<Unit>:
            kept : List<&2, Chan(C.Down)> <- hub.push(clients, e)
            hub.run(ch, kept)

# Every client's inbox gets a Tick every ms, so it reads what came up; the ticker
# stops once the inbox is closed.
@unsafe
def ticker(+ms: U32, +inbox: Chan(C.Down)) -> IO(Unit):
  do IO<Unit>:
    IO.sleep(ms)
    ok : Bool <- Chan.send(C.Down, inbox, C.Tick{})
    ticker.again(ok, ms, inbox)

def ticker.again(ok: Bool, +ms: U32, +inbox: Chan(C.Down)) -> IO(Unit):
  match ok:
    case True{}:
      ticker(ms, inbox)
    case False{}:
      IO.pure(Unit, Unit{})

# Integrations: plug starts one, and serves its posts.
# ----------------------------------------------------------------------------

# What an integration is asked: post a message, or take one down.
type Op is Data:
  Post{body: String}
  Remove{id: String}

# One op for an integration, and where its outcome goes.
type Job is Type:
  Job{op: Op, reply: Chan(C.Outcome)}

# A running integration, as the socket (by name) and remux (by channel) reach it.
type Route is Data:
  Route{name: String, channel: String, jobs: Chan(Job)}

def answer(+reply: Chan(C.Outcome), o: C.Outcome) -> IO(Unit):
  do IO<Unit>:
    ok : Bool <- Chan.send(C.Outcome, reply, o)
    return Unit{}

def served(-S: Type, +reply: Chan(C.Outcome), r: S & C.Outcome) -> IO(S):
  (st, o) = r
  do IO<S>:
    answer(reply, o)
    return st

def too_long(+limit: U32, +reply: Chan(C.Outcome), -S: Type, st: S) -> IO(S):
  do IO<S>:
    answer(reply, C.Refused{"longer than " ++ U32.show(limit) ++ " characters"})
    return st

# One op: a post checked against the integration's limit and sent, or a delete.
def serve.one(~Cf: Data, ~St: Data, ~it: C.Integration<Cf, St>, +conf: Cf, +st: St, m: Maybe<&1, Job>) -> IO(St):
  match m:
    case None{}:
      IO.pure(St, st)
    case Some{job}:
      match job:
        case Job{op, +reply}:
          match op:
            case Post{+body}:
              +limit = C.Integration.limit(Cf, St, it)
              Bool.pick(IO(St), U32.is_gt(C.utf8.length(body), limit), too_long(limit, reply, St, st),
                do IO<St>:
                  r : St & C.Outcome <- C.Integration.send(Cf, St, it, conf, st, body)
                  served(St, reply, r))
            case Remove{id}:
              do IO<St>:
                r : St & C.Outcome <- C.Integration.delete(Cf, St, it, conf, st, id)
                served(St, reply, r)

# An integration's posts, one at a time, in order, with what its sender keeps.
@unsafe
def serve(~Cf: Data, ~St: Data, ~it: C.Integration<Cf, St>, +conf: Cf, +jobs: Chan(Job), st: St) -> IO(Unit):
  do IO<Unit>:
    m : Maybe<&1, Job> <- Chan.recv(Job, jobs)
    st2 : St <- serve.one(~Cf, ~St, ~it, conf, st, m)
    serve(~Cf, ~St, ~it, conf, jobs, st2)

def plug.run(~Cf: Data, ~St: Data, ~it: C.Integration<Cf, St>, +conf: Cf, +hub: Chan(C.Up), +jobs: Chan(Job)) -> IO(List<&2, Route>):
  +name = C.Integration.name(Cf, St, it)
  do IO<List<&2, Route>>:
    N.log(name ++ ": on" ++ Bool.pick(String, C.Integration.sends(Cf, St, it, conf), ", posting", ", reading only (no tokens to post)"))
    IO.spawn(Unit, C.Integration.read(Cf, St, it, conf, hub))
    IO.spawn(Unit, serve(~Cf, ~St, ~it, conf, jobs, C.Integration.fresh(Cf, St, it)))
    return [Route{name, C.Integration.channel(Cf, St, it, conf), jobs}]

def plug.set(~Cf: Data, ~St: Data, ~it: C.Integration<Cf, St>, r: Result<&1, &1, String, Cf>, +hub: Chan(C.Up)) -> IO(List<&2, Route>):
  match r:
    case Fail{why}:
      IO.die(List<&2, Route>, 2, "bridge: " ++ C.Integration.name(Cf, St, it) ++ ": " ++ why)
    case Done{+conf}:
      do IO<List<&2, Route>>:
        jobs : Chan(Job) <- Chan.new(Job, 64)
        plug.run(~Cf, ~St, ~it, conf, hub, jobs)

# Starts it when the [byo] table names it: its reader, and its sender behind a Route.
def plug(~Cf: Data, ~St: Data, ~it: C.Integration<Cf, St>, +cfg: List<&2, C.Two>, +hub: Chan(C.Up)) -> IO(List<&2, Route>):
  +value = C.config.get(cfg, C.Integration.name(Cf, St, it))
  Bool.pick(IO(List<&2, Route>), String.is_empty(value), IO.pure(List<&2, Route>, []),
    do IO<List<&2, Route>>:
      r : Result<&1, &1, String, Cf> <- C.Integration.setup(Cf, St, it, value)
      plug.set(~Cf, ~St, ~it, r, hub))

def routes.join(a: List<&2, Route>, b: List<&2, Route>) -> List<&2, Route>:
  List.append(&2, Route, a, b)

# Every integration the bridge knows. One line each.
def integrations(+cfg: List<&2, C.Two>, +hub: Chan(C.Up)) -> IO(List<&2, Route>):
  do IO<List<&2, Route>>:
    tw : List<&2, Route> <- plug(~Twitch.Conf, ~Twitch.Keep, ~Twitch.it(), cfg, hub)
    yt : List<&2, Route> <- plug(~YouTube.Conf, ~YouTube.Keep, ~YouTube.it(), cfg, hub)
    return routes.join(tw, yt)

def names(rs: List<&2, Route>) -> List<&2, String>:
  match rs:
    case Nil{}:
      []
    case Con{r, t}:
      match r:
        case Route{name, channel, jobs}:
          name <> names(t)

def channels(rs: List<&2, Route>) -> List<&2, String>:
  match rs:
    case Nil{}:
      []
    case Con{r, t}:
      match r:
        case Route{name, channel, jobs}:
          channel <> channels(t)

# The jobs of the route called name (by_channel: whose channel is name).
def route(rs: List<&2, Route>, +by_channel: Bool, +name: String) -> Maybe<&2, Chan(Job)>:
  match rs:
    case Nil{}:
      None{}
    case Con{r, t}:
      match r:
        case Route{n, ch, jobs}:
          Bool.pick(Maybe<&2, Chan(Job)>, String.eq(Bool.pick(String, by_channel, ch, n), name), Some{jobs}, route(t, by_channel, name))

def ask.got(+reply: Chan(C.Outcome), m: Maybe<&1, C.Outcome>) -> IO(C.Outcome):
  do IO<C.Outcome>:
    Chan.close(C.Outcome, reply)
    return Maybe.default(&1, C.Outcome, m, C.Refused{"the sender is gone"})

def ask.wait(+jobs: Chan(Job), op: Op, +reply: Chan(C.Outcome)) -> IO(C.Outcome):
  do IO<C.Outcome>:
    ok : Bool <- Chan.send(Job, jobs, Job{op, reply})
    m : Maybe<&1, C.Outcome> <- Chan.recv(C.Outcome, reply)
    ask.got(reply, m)

# Hands op to the integration behind m, and waits for how it ended; missing names
# what nobody answers to.
def ask(m: Maybe<&2, Chan(Job)>, op: Op, missing: String) -> IO(C.Outcome):
  match m:
    case None{}:
      IO.pure(C.Outcome, C.Refused{missing})
    case Some{jobs}:
      do IO<C.Outcome>:
        reply : Chan(C.Outcome) <- Chan.new(C.Outcome, 1)
        ask.wait(jobs, op, reply)

# remux's wire: a WebSocket server on 127.0.0.1. Lines go down as the hub pushes
# events; once a second a Tick has it read what came up, so pings get their pong.
# ----------------------------------------------------------------------------

@unsafe
def client(c: Ws.Conn, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    m : Maybe<&1, C.Down> <- Chan.recv(C.Down, inbox)
    client.on(c, inbox, routes, m)

def client.gone(c: Ws.Conn, +inbox: Chan(C.Down)) -> IO(Unit):
  do IO<Unit>:
    Chan.close(C.Down, inbox)
    Ws.shutdown(c)
    N.log("wire: engine gone")

def client.sent(r: Ws.Conn & Result<&1, &1, U32 & String, Unit>, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  (c, res) = r
  match res:
    case Fail{e}:
      client.gone(c, inbox)
    case Done{u}:
      client(c, inbox, routes)

# A read that found nothing within its deadline is ETIMEDOUT (60 on macOS, 110 on Linux).
def quiet(+code: U32) -> Bool:
  Bool.or(U32.is_eq(code, 60), U32.is_eq(code, 110))

def client.fail(quiet: Bool, c: Ws.Conn, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  match quiet:
    case True{}:
      client(c, inbox, routes)
    case False{}:
      client.gone(c, inbox)

def client.conn(r: Ws.Conn & Result<&1, &1, U32 & String, Unit>) -> Ws.Conn:
  (c, x) = r
  c

# One frame down; a failure shows at the next read.
def client.put(c: Ws.Conn, text: String) -> IO(Ws.Conn):
  do IO<Ws.Conn>:
    r : Ws.Conn & Result<&1, &1, U32 & String, Unit> <- Ws.send(c, True{}, 1, WB.from_string(text))
    return client.conn(r)

def client.put.some(c: Ws.Conn, m: Maybe<&2, String>) -> IO(Ws.Conn):
  match m:
    case None{}:
      IO.pure(Ws.Conn, c)
    case Some{text}:
      client.put(c, text)

# A say, into each chat it names: a failure comes back as a notice.
def client.says(chs: List<&2, String>, c: Ws.Conn, +body: String, +routes: List<&2, Route>) -> IO(Ws.Conn):
  match chs:
    case Nil{}:
      IO.pure(Ws.Conn, c)
    case Con{+ch, t}:
      do IO<Ws.Conn>:
        o : C.Outcome <- ask(route(routes, True{}, ch), Post{body}, "no chat called " ++ ch ++ " is on")
        c2 : Ws.Conn <- client.put.some(c, C.notice.said(ch, o))
        client.says(t, c2, body, routes)

# What the engine asked: say, delete, or nothing.
def client.asked(a: C.Asked, c: Ws.Conn, +routes: List<&2, Route>) -> IO(Ws.Conn):
  match a:
    case C.AskNothing{}:
      IO.pure(Ws.Conn, c)
    case C.AskBad{about, why}:
      client.put(c, C.notice(about, why, False{}))
    case C.AskSay{+channel, body}:
      client.says(C.asked.targets(channel, channels(routes)), c, body, routes)
    case C.AskDelete{+channel, id}:
      do IO<Ws.Conn>:
        o : C.Outcome <- ask(route(routes, True{}, channel), Remove{id}, "no chat called " ++ channel ++ " is on")
        client.put(c, C.notice.deleted(channel, o))

def client.text(+text: String, c: Ws.Conn, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    c2 : Ws.Conn <- client.asked(C.asked(N.json.of(text)), c, routes)
    client(c2, inbox, routes)

def client.heard(r: Ws.Conn & Result<&1, &1, U32 & String, Ws.Incoming>, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  (c, res) = r
  match res:
    case Fail{(+code, why)}:
      client.fail(quiet(code), c, inbox, routes)
    case Done{m}:
      match m:
        case Ws.Text{data}:
          client.text(WB.to_string(data), c, inbox, routes)
        case Ws.Binary{data}:
          client(c, inbox, routes)
        case Ws.Pong{data}:
          client(c, inbox, routes)
        case Ws.Closed{code, reason}:
          client.gone(c, inbox)

def client.frames(xs: List<&2, String>, c: Ws.Conn, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  match xs:
    case Nil{}:
      client(c, inbox, routes)
    case Con{x, t}:
      do IO<Unit>:
        c2 : Ws.Conn <- client.put(c, x)
        client.frames(t, c2, inbox, routes)

def client.on(c: Ws.Conn, +inbox: Chan(C.Down), +routes: List<&2, Route>, m: Maybe<&1, C.Down>) -> IO(Unit):
  match m:
    case None{}:
      client.gone(c, inbox)
    case Some{down}:
      match down:
        case C.Push{e}:
          client.frames(C.frames(e), c, inbox, routes)
        case C.Tick{}:
          do IO<Unit>:
            heard : Ws.Conn & Result<&1, &1, U32 & String, Ws.Incoming> <- Ws.recv(c, 1)
            client.heard(heard, inbox, routes)

def shake.join(c: Ws.Conn, +inbox: Chan(C.Down), +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    ok : Bool <- Chan.send(C.Up, hub, C.Join{inbox})
    IO.spawn(Unit, ticker(1000, inbox))
    N.log("wire: engine connected")
    client(c, inbox, routes)

def shake.open(m: Socket & Result<&1, &1, U32 & String, Unit>, rest: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  (s, r) = m
  match r:
    case Fail{e}:
      Socket.close(s)
    case Done{u}:
      do IO<Unit>:
        inbox : Chan(C.Down) <- Chan.new(C.Down, 256)
        shake.join(Ws.Conn{s, False{}, True{}, WB.from_string(rest), Ws.Idle{}, 1048576, False{}}, inbox, hub, routes)

def shake.accepted(s: Socket, rest: String, r: Result<&1, &1, U32 & String, String>, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  match r:
    case Fail{e}:
      Socket.close(s)
    case Done{accept}:
      head = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: " ++ accept ++ "\r\n\r\n"
      do IO<Unit>:
        sent : Socket & Result<&1, &1, U32 & String, Unit> <- Wire.send(s, head)
        shake.open(sent, rest, hub, routes)

def shake.key(missing: Bool, s: Socket, key: String, rest: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  match missing:
    case True{}:
      Socket.close(s)
    case False{}:
      do IO<Unit>:
        accept : Result<&1, &1, U32 & String, String> <- Ws.accept(key)
        shake.accepted(s, rest, accept, hub, routes)

@unsafe
def shake(s: Socket, +buf: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    r : Socket & Result<&1, &1, U32 & String, String> <- Wire.recv(s, 4096, 10000)
    shake.got(r, buf, hub, routes)

def shake.more(small: Bool, s: Socket, +buf: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  match small:
    case True{}:
      shake(s, buf, hub, routes)
    case False{}:
      Socket.close(s)

def shake.have(m: Maybe<&2, C.Two>, s: Socket, +buf: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  match m:
    case None{}:
      shake.more(U32.is_lt(U32.from_nat(String.length(buf)), 16384), s, buf, hub, routes)
    case Some{two}:
      match two:
        case C.Two{head, rest}:
          +key = C.header(head, "sec-websocket-key")
          shake.key(String.is_empty(key), s, key, rest, hub, routes)

def shake.chunk(closed: Bool, s: Socket, +all: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  match closed:
    case True{}:
      Socket.close(s)
    case False{}:
      shake.have(C.head(all), s, all, hub, routes)

def shake.got(r: Socket & Result<&1, &1, U32 & String, String>, +buf: String, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  (s, res) = r
  match res:
    case Fail{e}:
      Socket.close(s)
    case Done{+chunk}:
      shake.chunk(String.is_empty(chunk), s, buf ++ chunk, hub, routes)

def accept.one(m: Listener & Result<&1, &1, U32 & String, Socket>, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Listener):
  (l, r) = m
  match r:
    case Fail{e}:
      IO.pure(Listener, l)
    case Done{s}:
      do IO<Listener>:
        IO.spawn(Unit, shake(s, "", hub, routes))
        return l

@unsafe
def accept(l: Listener, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    m : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    l2 : Listener <- accept.one(m, hub, routes)
    accept(l2, hub, routes)

# The Unix socket: NDJSON both ways. Events go down as the hub pushes them; ten
# times a second the connection reads what came up, and answers each request.
# ----------------------------------------------------------------------------

@unsafe
def nd(s: Socket, +inbox: Chan(C.Down), +buf: String, +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    m : Maybe<&1, C.Down> <- Chan.recv(C.Down, inbox)
    nd.on(m, s, inbox, buf, routes)

def nd.gone(s: Socket, +inbox: Chan(C.Down)) -> IO(Unit):
  do IO<Unit>:
    Chan.close(C.Down, inbox)
    Socket.close(s)
    N.log("socket: client gone")

def nd.sock(r: Socket & Result<&1, &1, U32 & String, Unit>) -> Socket:
  (s, x) = r
  s

def nd.write(s: Socket, line: String) -> IO(Socket):
  do IO<Socket>:
    r : Socket & Result<&1, &1, U32 & String, Unit> <- Wire.send(s, line ++ "\n")
    return nd.sock(r)

def nd.targets(ps: List<&2, String>, s: Socket, +body: String, +routes: List<&2, Route>) -> IO(Socket):
  match ps:
    case Nil{}:
      IO.pure(Socket, s)
    case Con{+p, t}:
      do IO<Socket>:
        o : C.Outcome <- ask(route(routes, False{}, p), Post{body}, "no integration called " ++ p ++ " is on")
        s2 : Socket <- nd.write(s, C.reply(p, o))
        nd.targets(t, s2, body, routes)

def nd.req(r: C.Req, s: Socket, +routes: List<&2, Route>) -> IO(Socket):
  match r:
    case C.Bad{why}:
      nd.write(s, C.bad(why))
    case C.Say{+p, body}:
      nd.targets(C.targets(p, names(routes)), s, body, routes)
    case C.Delete{+p, id}:
      do IO<Socket>:
        o : C.Outcome <- ask(route(routes, False{}, p), Remove{id}, "no integration called " ++ p ++ " is on")
        nd.write(s, C.reply.deleted(p, o))

def nd.json(obj: Bool, j: C.J, s: Socket, +routes: List<&2, Route>) -> IO(Socket):
  match obj:
    case True{}:
      nd.req(C.req(j), s, routes)
    case False{}:
      nd.write(s, C.bad("not a JSON object"))

def nd.line(blank: Bool, +line: String, s: Socket, +routes: List<&2, Route>) -> IO(Socket):
  match blank:
    case True{}:
      IO.pure(Socket, s)
    case False{}:
      +j = N.json.of(line)
      nd.json(C.j.is_obj(j), j, s, routes)

def nd.each(xs: List<&2, String>, s: Socket, +routes: List<&2, Route>) -> IO(Socket):
  match xs:
    case Nil{}:
      IO.pure(Socket, s)
    case Con{+x, t}:
      do IO<Socket>:
        s2 : Socket <- nd.line(String.is_empty(String.trim(x)), x, s, routes)
        nd.each(t, s2, routes)

def nd.split(p: List<&2, String> & String, s: Socket, +inbox: Chan(C.Down), +routes: List<&2, Route>) -> IO(Unit):
  (xs, rest) = p
  do IO<Unit>:
    s2 : Socket <- nd.each(xs, s, routes)
    nd(s2, inbox, rest, routes)

def nd.chunk(closed: Bool, s: Socket, +inbox: Chan(C.Down), +all: String, +routes: List<&2, Route>) -> IO(Unit):
  match closed:
    case True{}:
      nd.gone(s, inbox)
    case False{}:
      nd.split(C.lines(all), s, inbox, routes)

def nd.fail(quiet: Bool, s: Socket, +inbox: Chan(C.Down), +buf: String, +routes: List<&2, Route>) -> IO(Unit):
  match quiet:
    case True{}:
      nd(s, inbox, buf, routes)
    case False{}:
      nd.gone(s, inbox)

def nd.heard(r: Socket & Result<&1, &1, U32 & String, String>, +inbox: Chan(C.Down), +buf: String, +routes: List<&2, Route>) -> IO(Unit):
  (s, res) = r
  match res:
    case Fail{(+code, why)}:
      nd.fail(quiet(code), s, inbox, buf, routes)
    case Done{+chunk}:
      nd.chunk(String.is_empty(chunk), s, inbox, buf ++ chunk, routes)

def nd.sent(r: Socket & Result<&1, &1, U32 & String, Unit>, +inbox: Chan(C.Down), +buf: String, +routes: List<&2, Route>) -> IO(Unit):
  (s, res) = r
  match res:
    case Fail{e}:
      nd.gone(s, inbox)
    case Done{u}:
      nd(s, inbox, buf, routes)

def nd.on(m: Maybe<&1, C.Down>, s: Socket, +inbox: Chan(C.Down), +buf: String, +routes: List<&2, Route>) -> IO(Unit):
  match m:
    case None{}:
      nd.gone(s, inbox)
    case Some{down}:
      match down:
        case C.Push{e}:
          do IO<Unit>:
            r : Socket & Result<&1, &1, U32 & String, Unit> <- Wire.send(s, C.event.json(e) ++ "\n")
            nd.sent(r, inbox, buf, routes)
        case C.Tick{}:
          do IO<Unit>:
            r : Socket & Result<&1, &1, U32 & String, String> <- Wire.recv(s, 65536, 1)
            nd.heard(r, inbox, buf, routes)

def nd.join(s: Socket, +inbox: Chan(C.Down), +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    ok : Bool <- Chan.send(C.Up, hub, C.Join{inbox})
    IO.spawn(Unit, ticker(100, inbox))
    N.log("socket: client connected")
    nd(s, inbox, "", routes)

def nd.start(s: Socket, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    inbox : Chan(C.Down) <- Chan.new(C.Down, 256)
    nd.join(s, inbox, hub, routes)

def nd.one(m: Listener & Result<&1, &1, U32 & String, Socket>, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Listener):
  (l, r) = m
  match r:
    case Fail{e}:
      IO.pure(Listener, l)
    case Done{s}:
      do IO<Listener>:
        IO.spawn(Unit, nd.start(s, hub, routes))
        return l

@unsafe
def nd.accept(l: Listener, +hub: Chan(C.Up), +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    m : Listener & Result<&1, &1, U32 & String, Socket> <- TCP.accept(l)
    l2 : Listener <- nd.one(m, hub, routes)
    nd.accept(l2, hub, routes)

# Start
# -----

def config.path(+own: String, home: String) -> String:
  Bool.pick(String, String.is_empty(own), home ++ "/.config/remux/config.toml", own)

def config.text(r: File & Result<&1, &1, U32 & String, String>) -> IO(String):
  (f, t) = r
  do IO<String>:
    File.close(f)
    return N.env(t)

def config.read(r: Result<&1, &1, U32 & String, File>) -> IO(String):
  match r:
    case Fail{e}:
      IO.pure(String, "")
    case Done{f}:
      do IO<String>:
        t : File & Result<&1, &1, U32 & String, String> <- File.read(f, 65536)
        config.text(t)

def args.data(xs: List<&1, String>) -> List<&2, String>:
  match xs:
    case Nil{}:
      []
    case Con{h, t}:
      h <> args.data(t)

def start.go(+port: U32, +socket: String, +hub: Chan(C.Up), l: Listener, u: Listener, +routes: List<&2, Route>) -> IO(Unit):
  do IO<Unit>:
    N.log("wire: ws://127.0.0.1:" ++ U32.show(port) ++ "  (remux chat url ws://127.0.0.1:" ++ U32.show(port) ++ ")")
    N.log("socket: " ++ socket)
    IO.spawn(Unit, accept(l, hub, routes))
    IO.spawn(Unit, nd.accept(u, hub, routes))
    hub.run(hub, [])

def start.routes(+port: U32, +socket: String, +hub: Chan(C.Up), l: Listener, u: Listener, +routes: List<&2, Route>) -> IO(Unit):
  match routes:
    case Nil{}:
      IO.die(Unit, 2, "bridge: nothing to read: name an integration under [byo] in ~/.config/remux/config.toml, as twitch = \"<channel>\" or youtube = \"<video id>\"")
    case Con{r, t}:
      start.go(port, socket, hub, l, u, r <> t)

def start.listen(+port: U32, +socket: String, +cfg: List<&2, C.Two>, +hub: Chan(C.Up)) -> IO(Unit):
  do IO<Unit>:
    l : Listener <- IO.try(Listener, TCP.listen("127.0.0.1", port))
    u : Listener <- IO.try(Listener, Unix.listen(socket))
    routes : List<&2, Route> <- integrations(cfg, hub)
    start.routes(port, socket, hub, l, u, routes)

def start(o: C.Opts, +cfg: List<&2, C.Two>) -> IO(Unit):
  match o:
    case C.Opts{+port, +socket, +bad}:
      Bool.pick(IO(Unit), Bool.not(String.is_empty(bad)),
        IO.die(Unit, 2, "bridge: cannot use " ++ bad ++ "; usage: bridge [--port 9999] [--socket <path>]; what to read is the [byo] table of the config"),
        do IO<Unit>:
          hub : Chan(C.Up) <- Chan.new(C.Up, 1024)
          start.listen(port, socket, cfg, hub))

def main.go(+own: String, +home: String) -> IO(Unit):
  do IO<Unit>:
    f : Result<&1, &1, U32 & String, File> <- File.open(config.path(own, home), "r")
    text : String <- config.read(f)
    args : List<String> <- IO.args()
    start(C.opts(List.tail(&2, String, args.data(args)), C.Opts{9999, home ++ "/.config/remux/bridge.sock", ""}), C.byo(text))

def main() -> IO(Unit):
  do IO<Unit>:
    own : String <- N.getenv("REMUX_CONFIG")
    home : String <- N.getenv("HOME")
    main.go(own, home)
