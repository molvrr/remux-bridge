# YouTube, as an Integration: chat, memberships, gifts and Super Chats polled from
# the live chat of a video, and messages posted through the Data API.
#   [byo] youtube = "<video id>"      (the video must be live, public or unlisted)
#   YOUTUBE_API_KEY                   to read
#   YOUTUBE_ACCESS_TOKEN              to post and delete: a token with youtube.force-ssl
#                                     of the channel's owner or a moderator, or, so
#   YOUTUBE_REFRESH_TOKEN             the bridge refreshes it, a refresh token and
#   YOUTUBE_CLIENT_ID, _SECRET        the OAuth client it was issued to
# Unproven: the parsing it relies on is in logic.bend, where LAWS.bend proves it.
import Base
import ../core.bend as C
import ../net.bend as N
import ./logic.bend as L

type Creds is Data:
  Creds{access: String, refresh: String, client: String, secret: String}

type Conf is Data:
  Conf{video: String, key: String, creds: Creds}

# What the sender keeps: an access token until a time (ms of IO.now), and the live
# chat of the video.
type Keep is Data:
  Keep{token: String, until: Nat, chat: String}

# Setup
# -----

def can(creds: Creds) -> Bool:
  match creds:
    case Creds{+access, +refresh, +client, +secret}:
      Bool.or(Bool.not(String.is_empty(access)),
        Bool.and(Bool.not(String.is_empty(refresh)), Bool.and(Bool.not(String.is_empty(client)), Bool.not(String.is_empty(secret)))))

def setup.of(+video: String, +key: String, creds: Creds) -> Result<&1, &1, String, Conf>:
  Bool.pick(Result<&1, &1, String, Conf>, String.is_empty(video), Fail{"the value is a video id, and it is empty"},
    Bool.pick(Result<&1, &1, String, Conf>, String.is_empty(key), Fail{"reading needs YOUTUBE_API_KEY in the environment"},
      Done{Conf{video, key, creds}}))

def setup(v: String) -> IO(Result<&1, &1, String, Conf>):
  do IO<Result<&1, &1, String, Conf>>:
    key : String <- N.getenv("YOUTUBE_API_KEY")
    access : String <- N.getenv("YOUTUBE_ACCESS_TOKEN")
    refresh : String <- N.getenv("YOUTUBE_REFRESH_TOKEN")
    client : String <- N.getenv("YOUTUBE_CLIENT_ID")
    secret : String <- N.getenv("YOUTUBE_CLIENT_SECRET")
    return setup.of(String.trim(v), key, Creds{access, refresh, client, secret})

def channel(conf: Conf) -> String:
  match conf:
    case Conf{video, key, creds}:
      video

def sends(conf: Conf) -> Bool:
  match conf:
    case Conf{video, key, creds}:
      can(creds)

# Reading: the live chat, polled with the API key.
# ----------------------------------------------------------------------------

# 403 and 404: the chat ended or the key is refused.
def refused(+status: U32) -> Bool:
  Bool.or(U32.is_eq(status, 403), U32.is_eq(status, 404))

@unsafe
def find(+video: String, +key: String, +hub: Chan(C.Up)) -> IO(Unit):
  do IO<Unit>:
    r : Result<&2, &2, String, N.Ans> <- N.get(L.video_url(key, video), N.headers())
    found(r, video, key, hub)

def again(+status: U32, +video: String, +key: String, +hub: Chan(C.Up)) -> IO(Unit):
  Bool.pick(IO(Unit), refused(status),
    do IO<Unit>:
      N.later(30000, "youtube: " ++ U32.show(status) ++ "; the chat ended or the key is refused; again in 30 s")
      find(video, key, hub),
    do IO<Unit>:
      N.later(10000, "youtube: " ++ U32.show(status) ++ "; again in 10 s")
      find(video, key, hub))

def emit.all(xs: List<&2, C.ChatEvent>, +hub: Chan(C.Up)) -> IO(Unit):
  match xs:
    case Nil{}:
      IO.pure(Unit, Unit{})
    case Con{e, t}:
      do IO<Unit>:
        N.emit(hub, e)
        emit.all(t, hub)

@unsafe
def poll(+video: String, +key: String, +chat: String, first: Bool, +page: String, +hub: Chan(C.Up)) -> IO(Unit):
  do IO<Unit>:
    r : Result<&2, &2, String, N.Ans> <- N.get(L.chat_url(key, chat, page), N.headers())
    polled(r, video, key, chat, first, page, hub)

# The first answer is the chat before the bridge came: it is skipped.
def next(p: L.Page, first: Bool, +video: String, +key: String, +chat: String, +page: String, +hub: Chan(C.Up)) -> IO(Unit):
  match p:
    case L.Page{xs, next, wait}:
      do IO<Unit>:
        Bool.pick(IO(Unit), first, IO.pure(Unit, Unit{}), emit.all(xs, hub))
        IO.sleep(wait)
        poll(video, key, chat, False{}, C.or_else(next, page), hub)

def polled(r: Result<&2, &2, String, N.Ans>, +video: String, +key: String, +chat: String, first: Bool, +page: String, +hub: Chan(C.Up)) -> IO(Unit):
  match r:
    case Fail{why}:
      do IO<Unit>:
        N.later(10000, "youtube: " ++ why ++ "; again in 10 s")
        poll(video, key, chat, first, page, hub)
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          Bool.pick(IO(Unit), U32.is_eq(status, 200), next(L.page(j, video), first, video, key, chat, page, hub), again(status, video, key, hub))

def start(+chat: String, +video: String, +key: String, +hub: Chan(C.Up)) -> IO(Unit):
  Bool.pick(IO(Unit), String.is_empty(chat),
    do IO<Unit>:
      N.later(15000, "youtube: " ++ video ++ " has no live chat yet (is it live?); again in 15 s")
      find(video, key, hub),
    do IO<Unit>:
      N.log("youtube: reading the chat of " ++ video)
      poll(video, key, chat, True{}, "", hub))

def found(r: Result<&2, &2, String, N.Ans>, +video: String, +key: String, +hub: Chan(C.Up)) -> IO(Unit):
  match r:
    case Fail{why}:
      do IO<Unit>:
        N.later(10000, "youtube: " ++ why ++ "; again in 10 s")
        find(video, key, hub)
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          Bool.pick(IO(Unit), U32.is_eq(status, 200), start(L.chat_id(j), video, key, hub), again(status, video, key, hub))

def read(conf: Conf, hub: Chan(C.Up)) -> IO(Unit):
  match conf:
    case Conf{video, key, creds}:
      find(video, key, hub)

# Posting: liveChatMessages.insert.
# ----------------------------------------------------------------------------

# A refreshed token kept until a minute before it expires.
def fresh(t: Result<&2, &2, String, C.Two>, +now: Nat, +chat: String) -> Result<&2, &2, String, Keep>:
  match t:
    case Fail{why}:
      Fail{why}
    case Done{two}:
      match two:
        case C.Two{token, secs}:
          +life = U32.to_nat(C.number(secs, 3599))
          Done{Keep{token, Nat.add(now, Nat.sub(Nat.mul(life, 1000n), 60000n)), chat}}

def refreshed(r: Result<&2, &2, String, N.Ans>, +now: Nat, +chat: String) -> Result<&2, &2, String, Keep>:
  match r:
    case Fail{why}:
      Fail{"refreshing the token failed: " ++ why}
    case Done{ans}:
      match ans:
        case N.Ans{status, j}:
          fresh(L.token(j), now, chat)

def renew(creds: Creds, +now: Nat, +chat: String) -> IO(Result<&2, &2, String, Keep>):
  match creds:
    case Creds{access, refresh, client, secret}:
      do IO<Result<&2, &2, String, Keep>>:
        r : Result<&2, &2, String, N.Ans> <- N.post("https://oauth2.googleapis.com/token", N.header(N.headers(), "content-type", "application/x-www-form-urlencoded"), L.refresh_form(client, secret, refresh))
        return refreshed(r, now, chat)

# A token: the one given, or one refreshed a minute before the last expires.
def token(+creds: Creds, keep: Keep, +now: Nat) -> IO(Result<&2, &2, String, Keep>):
  match creds:
    case Creds{+access, +refresh, +client, +secret}:
      match keep:
        case Keep{+token, +until, +chat}:
          Bool.pick(IO(Result<&2, &2, String, Keep>), String.is_empty(refresh), IO.pure(Result<&2, &2, String, Keep>, Done{Keep{access, 0n, chat}}),
            Bool.pick(IO(Result<&2, &2, String, Keep>), Bool.and(Bool.not(String.is_empty(token)), Nat.is_lt(now, until)), IO.pure(Result<&2, &2, String, Keep>, Done{Keep{token, until, chat}}),
              renew(Creds{access, refresh, client, secret}, now, chat)))

def chat.found(r: Result<&2, &2, String, N.Ans>, +video: String, +token: String, +until: Nat) -> Result<&2, &2, String, Keep>:
  match r:
    case Fail{why}:
      Fail{"looking up the live chat failed: " ++ why}
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          +c = L.chat_id(j)
          Bool.pick(Result<&2, &2, String, Keep>, String.is_empty(c),
            Fail{Bool.pick(String, U32.is_eq(status, 200), video ++ " has no live chat (is it live?)", "looking up the live chat failed: " ++ U32.show(status) ++ " " ++ C.api.why(j))},
            Done{Keep{token, until, c}})

# The live chat of the video: asked once, then kept.
def chat(+video: String, keep: Keep) -> IO(Result<&2, &2, String, Keep>):
  match keep:
    case Keep{+token, +until, +chat}:
      Bool.pick(IO(Result<&2, &2, String, Keep>), Bool.not(String.is_empty(chat)), IO.pure(Result<&2, &2, String, Keep>, Done{Keep{token, until, chat}}),
        do IO<Result<&2, &2, String, Keep>>:
          r : Result<&2, &2, String, N.Ans> <- N.get(L.lookup_url(video), N.bearer(token))
          return chat.found(r, video, token, until))

# 401 forgets the token; 403 and 404 forget the chat, which may have ended.
def after(+status: U32, keep: Keep) -> Keep:
  match keep:
    case Keep{+token, +until, +chat}:
      Bool.pick(Keep, U32.is_eq(status, 401), Keep{"", 0n, chat},
        Bool.pick(Keep, refused(status), Keep{token, until, ""}, Keep{token, until, chat}))

def posted(r: Result<&2, &2, String, N.Ans>, +keep: Keep) -> Keep & C.Outcome:
  match r:
    case Fail{why}:
      (keep, C.Refused{"the request failed: " ++ why})
    case Done{ans}:
      match ans:
        case N.Ans{+status, +j}:
          (after(status, keep), N.outcome.pick(U32.is_eq(status, 200), status, j, L.sent(j)))

def post(+keep: Keep, body: String) -> IO(Keep & C.Outcome):
  match keep:
    case Keep{+token, +until, +chat}:
      do IO<Keep & C.Outcome>:
        r : Result<&2, &2, String, N.Ans> <- N.post(L.send_url(), N.json.headers(token), L.body(chat, body))
        return posted(r, Keep{token, until, chat})

def forget.chat(keep: Keep) -> Keep:
  match keep:
    case Keep{token, until, chat}:
      Keep{token, until, ""}

def with.chat(c: Result<&2, &2, String, Keep>, +keep: Keep, body: String) -> IO(Keep & C.Outcome):
  match c:
    case Fail{why}:
      IO.pure(Keep & C.Outcome, (forget.chat(keep), C.Refused{why}))
    case Done{k}:
      post(k, body)

def with.token(t: Result<&2, &2, String, Keep>, +video: String, +keep: Keep, +body: String) -> IO(Keep & C.Outcome):
  match t:
    case Fail{why}:
      IO.pure(Keep & C.Outcome, (keep, C.Refused{why}))
    case Done{+k}:
      do IO<Keep & C.Outcome>:
        c : Result<&2, &2, String, Keep> <- chat(video, k)
        with.chat(c, k, body)

def send.go(+video: String, +creds: Creds, +keep: Keep, +body: String) -> IO(Keep & C.Outcome):
  Bool.pick(IO(Keep & C.Outcome), Bool.not(can(creds)),
    IO.pure(Keep & C.Outcome, (keep, C.Refused{"posting needs YOUTUBE_ACCESS_TOKEN, or YOUTUBE_REFRESH_TOKEN with YOUTUBE_CLIENT_ID and YOUTUBE_CLIENT_SECRET"})),
    do IO<Keep & C.Outcome>:
      now : Nat <- IO.now()
      t : Result<&2, &2, String, Keep> <- token(creds, keep, now)
      with.token(t, video, keep, body))

def send(conf: Conf, keep: Keep, body: String) -> IO(Keep & C.Outcome):
  match conf:
    case Conf{video, key, creds}:
      send.go(video, creds, keep, body)

# Deleting: liveChatMessages.delete.

def removed(r: U32 & C.Outcome, +keep: Keep) -> Keep & C.Outcome:
  (+status, o) = r
  (after(status, keep), o)

def remove(t: Result<&2, &2, String, Keep>, +keep: Keep, +id: String) -> IO(Keep & C.Outcome):
  match t:
    case Fail{why}:
      IO.pure(Keep & C.Outcome, (keep, C.Refused{why}))
    case Done{k}:
      match k:
        case Keep{+token, +until, +chat}:
          do IO<Keep & C.Outcome>:
            r : Result<&2, &2, String, N.Ans> <- N.delete(L.delete_url(id), N.bearer(token))
            return removed(N.deleted(r, id), Keep{token, until, chat})

def delete.go(+creds: Creds, +keep: Keep, +id: String) -> IO(Keep & C.Outcome):
  Bool.pick(IO(Keep & C.Outcome), Bool.not(can(creds)),
    IO.pure(Keep & C.Outcome, (keep, C.Refused{"deleting needs YOUTUBE_ACCESS_TOKEN, or YOUTUBE_REFRESH_TOKEN with YOUTUBE_CLIENT_ID and YOUTUBE_CLIENT_SECRET"})),
    do IO<Keep & C.Outcome>:
      now : Nat <- IO.now()
      t : Result<&2, &2, String, Keep> <- token(creds, keep, now)
      remove(t, keep, id))

def delete(conf: Conf, keep: Keep, id: String) -> IO(Keep & C.Outcome):
  match conf:
    case Conf{video, key, creds}:
      delete.go(creds, keep, id)

# The Integration
# ---------------

def it() -> C.Integration<Conf, Keep>:
  C.Integration{"youtube", 200, setup, channel, read, sends, Keep{"", 0n, ""}, send, delete}
