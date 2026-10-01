# What every integration's wiring shares: logging, the environment, HTTP answers
# as JSON, and handing events to the hub. Unproven: it calls the network.
import Base
import bend-kit-http@0.25.0.2/http.bend as Http
import 0x584fc27920487ceab242392391418d7f/json.bend as Json
# The Bytes that Http and Json use.
import 0x49814d83de8f70993a43e1002be29ecd/bytes.bend as HB
import ./core.bend as C

def log(text: String) -> IO(Unit):
  IO.print_err(text)

def later(ms: U32, text: String) -> IO(Unit):
  do IO<Unit>:
    log(text)
    IO.sleep(ms)

# An environment variable or a file's text; "" when it failed.
def env(r: Result<&1, &1, U32 & String, String>) -> String:
  match r:
    case Fail{e}:
      ""
    case Done{x}:
      x

def or_u32(r: Result<&1, &1, U32 & String, U32>, d: U32) -> U32:
  match r:
    case Fail{e}:
      d
    case Done{x}:
      x

def getenv(name: String) -> IO(String):
  do IO<String>:
    r : Result<&1, &1, U32 & String, String> <- IO.get_env(name)
    return String.trim(env(r))

# Hands an event to the hub.
def emit(+hub: Chan(C.Up), e: C.ChatEvent) -> IO(Unit):
  do IO<Unit>:
    ok : Bool <- Chan.send(C.Up, hub, C.Happened{e})
    return Unit{}

# JSON
# ----

@unsafe
def to_j(v: Json.Val) -> C.J:
  match v:
    case Json.Null{}:
      C.JNull{}
    case Json.Flag{on}:
      C.JBool{on}
    case Json.Num{s}:
      C.JNum{HB.to_string(s)}
    case Json.Str{s}:
      C.JStr{HB.to_string(s)}
    case Json.Arr{xs}:
      C.JArr{to_j.list(xs)}
    case Json.Obj{kvs}:
      C.JObj{to_j.kvs(kvs)}

def to_j.list(xs: List<&1, Json.Val>) -> List<&2, C.J>:
  match xs:
    case Nil{}:
      []
    case Con{h, t}:
      to_j(h) <> to_j.list(t)

def to_j.kvs(kvs: List<&1, HB.Bytes & Json.Val>) -> List<&2, C.JKv>:
  match kvs:
    case Nil{}:
      []
    case Con{(k, v), t}:
      C.JKv{HB.to_string(k), to_j(v)} <> to_j.kvs(t)

def json(m: Maybe<&1, Json.Val>) -> C.J:
  match m:
    case None{}:
      C.JNull{}
    case Some{v}:
      to_j(v)

# Text (octets) as JSON; JNull when it is not JSON.
def json.of(text: String) -> C.J:
  json(Json.parse.bytes(HB.from_string(text)))

# HTTP
# ----

# An answer: its status and its body as JSON.
type Ans is Data:
  Ans{status: U32, j: C.J}

def err.text(e: Http.Err) -> String:
  match e:
    case Http.ErrUrl{}:
      "bad URL"
    case Http.ErrDns{}:
      "cannot resolve the host"
    case Http.ErrConnect{code, why}:
      "cannot connect: " ++ why
    case Http.ErrTls{code, why}:
      "TLS failed: " ++ why
    case Http.ErrRead{code, why}:
      "read failed: " ++ why
    case Http.ErrWrite{code, why}:
      "write failed: " ++ why
    case Http.ErrTimeout{}:
      "timed out"
    case Http.ErrRedirect{}:
      "too many redirects"
    case Http.ErrBad{}:
      "bad response"

def answer(r: Result<&1, &1, Http.Err, Http.Res>) -> Result<&2, &2, String, Ans>:
  match r:
    case Fail{e}:
      Fail{err.text(e)}
    case Done{res}:
      match res:
        case Http.Res{status, h, body}:
          Done{Ans{status, json(Json.parse.bytes(body))}}

def headers() -> Map<&2, List<&2, String>>:
  Http.empty()

def header(m: Map<&2, List<&2, String>>, k: String, v: String) -> Map<&2, List<&2, String>>:
  Http.set(m, k, v)

def bearer(+token: String) -> Map<&2, List<&2, String>>:
  Http.set(Http.empty(), "authorization", "Bearer " ++ token)

def json.headers(+token: String) -> Map<&2, List<&2, String>>:
  Http.set(bearer(token), "content-type", "application/json")

def get(url: String, h: Map<&2, List<&2, String>>) -> IO(Result<&2, &2, String, Ans>):
  do IO<Result<&2, &2, String, Ans>>:
    r : Result<&1, &1, Http.Err, Http.Res> <- Http.fetch("GET", url, h, HB.new(0))
    return answer(r)

# body is text (octets): JSON, or a form.
def post(url: String, h: Map<&2, List<&2, String>>, body: String) -> IO(Result<&2, &2, String, Ans>):
  do IO<Result<&2, &2, String, Ans>>:
    r : Result<&1, &1, Http.Err, Http.Res> <- Http.fetch("POST", url, h, HB.from_string(body))
    return answer(r)

def delete(url: String, h: Map<&2, List<&2, String>>) -> IO(Result<&2, &2, String, Ans>):
  do IO<Result<&2, &2, String, Ans>>:
    r : Result<&1, &1, Http.Err, Http.Res> <- Http.fetch("DELETE", url, h, HB.new(0))
    return answer(r)

# A delete's answer: 204 (or any 2xx) took it down; else the API's error.
def deleted(r: Result<&2, &2, String, Ans>, +id: String) -> U32 & C.Outcome:
  match r:
    case Fail{why}:
      (0, C.Refused{"the request failed: " ++ why})
    case Done{ans}:
      match ans:
        case Ans{+status, +j}:
          (status, Bool.pick(C.Outcome, Bool.and(U32.is_ge(status, 200), U32.is_lt(status, 300)), C.Sent{id}, C.api.failed(status, j)))

# How a send's POST ended: ok reads a 200; any other status is the API's error.
def outcome.pick(ok: Bool, +status: U32, +j: C.J, sent: C.Outcome) -> C.Outcome:
  match ok:
    case True{}:
      sent
    case False{}:
      C.api.failed(status, j)
