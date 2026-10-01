# The spec. Each law is a claim about the bridge's pure code, proven in PROOF.bend.
import Base
import ./core.bend as C
import ./twitch/logic.bend as T
import ./twitch/eventsub.bend as E
import ./youtube/logic.bend as Y

# Core: events
# ------------

# A chat message reaches remux's wire as it came, every field.
law as_line_chat:
  for id: String
  for p: String
  for ch: String
  for from: String
  for body: String
  for badges: List<&2, String>
  for reply: String
  {C.as_line(C.ChatEvent{C.Chat{}, id, p, ch, from, body, badges, reply}) == Some{C.Line{id, p, ch, from, body}} : Maybe<&2, C.Line>}

# A tip with a message shows its message; a sub does not reach remux's wire at all.
law as_line_tip:
  {C.as_line(C.ChatEvent{C.Tip{"100", "bits", "100000000"}, "c1", "twitch", "k", "Ana", "gg", [], ""}) == Some{C.Line{"c1", "twitch", "k", "Ana", "gg"}} : Maybe<&2, C.Line>}

law as_line_sub:
  {C.as_line(C.ChatEvent{C.Sub{6, "1000"}, "u1", "twitch", "k", "Ana", "six months!", [], ""}) == None{} : Maybe<&2, C.Line>}

law event_json_sub:
  {C.event.json(C.ChatEvent{C.Sub{6, "1000"}, "u1", "twitch", "kartths", "Ana", "hi", [], ""}) == "{\"event\":{\"type\":\"sub\",\"id\":\"u1\",\"platform\":\"twitch\",\"channel\":\"kartths\",\"from\":\"Ana\",\"body\":\"hi\",\"badges\":[],\"reply\":\"\",\"months\":6,\"tier\":\"1000\"}}" : String}

law event_json_gift:
  {C.event.json(C.ChatEvent{C.Gift{5, "1000", ""}, "g1", "twitch", "k", "Bob", "", [], ""}) == "{\"event\":{\"type\":\"gift\",\"id\":\"g1\",\"platform\":\"twitch\",\"channel\":\"k\",\"from\":\"Bob\",\"body\":\"\",\"badges\":[],\"reply\":\"\",\"count\":5,\"tier\":\"1000\",\"to\":\"\"}}" : String}

law event_json_raid:
  {C.event.json(C.ChatEvent{C.Raid{42}, "r1", "twitch", "k", "Cid", "", [], ""}) == "{\"event\":{\"type\":\"raid\",\"id\":\"r1\",\"platform\":\"twitch\",\"channel\":\"k\",\"from\":\"Cid\",\"body\":\"\",\"badges\":[],\"reply\":\"\",\"viewers\":42}}" : String}

# A deleted message's text never goes back out to remux, whatever it said.
law as_line_deleted:
  for target: String
  for body: String
  {C.as_line(C.ChatEvent{C.Deleted{target}, "d1", "twitch", "k", "foo", body, [], ""}) == None{} : Maybe<&2, C.Line>}

law as_line_custom:
  {C.as_line(C.ChatEvent{C.Custom{"twitch.announcement", []}, "a1", "twitch", "k", "Cid", "hello all", [], ""}) == Some{C.Line{"a1", "twitch", "k", "Cid", "hello all"}} : Maybe<&2, C.Line>}

law event_json_tip:
  {C.event.json(C.ChatEvent{C.Tip{"$5.00", "USD", "5000000"}, "s1", "youtube", "v", "Ana", "gg", ["member", "verified"], ""}) == "{\"event\":{\"type\":\"tip\",\"id\":\"s1\",\"platform\":\"youtube\",\"channel\":\"v\",\"from\":\"Ana\",\"body\":\"gg\",\"badges\":[\"member\",\"verified\"],\"reply\":\"\",\"amount\":\"$5.00\",\"currency\":\"USD\",\"micros\":5000000}}" : String}

law event_json_custom:
  {C.event.json(C.ChatEvent{C.Custom{"twitch.viewermilestone", [C.Two{"category", "watch-streak"}, C.Two{"value", "5"}]}, "w1", "twitch", "k", "Ana", "", [], "p9"}) == "{\"event\":{\"type\":\"custom\",\"id\":\"w1\",\"platform\":\"twitch\",\"channel\":\"k\",\"from\":\"Ana\",\"body\":\"\",\"badges\":[],\"reply\":\"p9\",\"name\":\"twitch.viewermilestone\",\"fields\":{\"category\":\"watch-streak\",\"value\":\"5\"}}}" : String}

law event_json_banned:
  {C.event.json(C.ChatEvent{C.Banned{"troll", 600}, "b1", "twitch", "k", "", "", [], ""}) == "{\"event\":{\"type\":\"banned\",\"id\":\"b1\",\"platform\":\"twitch\",\"channel\":\"k\",\"from\":\"\",\"body\":\"\",\"badges\":[],\"reply\":\"\",\"user\":\"troll\",\"seconds\":600}}" : String}

# Micros keep every digit, past what a U32 holds, and nothing else goes into JSON.
law digits_big:
  {(C.digits("500000000000"), C.digits("1,5\""), C.digits("")) == ("500000000000", "15", "0") : String & String & String}

# Core: JSON out
# --------------

# Escaping works piece by piece, so no text can escape its quotes by being split.
law esc_append:
  for a: String
  for +b: String
  {C.esc(a ++ b) == C.esc(a) ++ C.esc(b) : String}

law esc_control:
  {C.esc(SCon{Char.from_u32(1), ""}) == "\\u0001" : String}

# UTF-8 passes through: "á" is the octets C3 A1, and they come out as they went in.
law esc_utf8:
  {C.esc(SCon{Char.from_u32(195), SCon{Char.from_u32(161), ""}}) == SCon{Char.from_u32(195), SCon{Char.from_u32(161), ""}} : String}

law wire_line:
  {C.wire(C.Line{"id1", "twitch", "kartths", "Kart\"ths", "a\\b\nc\t"}) == "{\"line\":{\"id\":\"id1\",\"platform\":\"twitch\",\"channel\":\"kartths\",\"from\":\"Kart\\\"ths\",\"body\":\"a\\\\b\\nc\\t\"}}" : String}

# Core: text, handshake, config
# -----------------------------

law lines_split:
  {C.lines("a\r\nb\r\npar") == (["a", "b"], "par") : List<&2, String> & String}

law head_split:
  {C.head("GET / HTTP/1.1\r\nHost: x\r\n\r\nrest") == Some{C.Two{"GET / HTTP/1.1\r\nHost: x", "rest"}} : Maybe<&2, C.Two>}

law head_waits:
  {C.head("GET / HTTP/1.1\r\nHost: x\r\n") == None{} : Maybe<&2, C.Two>}

law ws_key:
  {C.header("GET / HTTP/1.1\r\nHost: x\r\nSec-WebSocket-Key: dGhlIHNhbXBsZQ==", "sec-websocket-key") == "dGhlIHNhbXBsZQ==" : String}

law url_esc:
  {C.url.esc("a b,c~") == "a%20b%2Cc~" : String}

# Every key of [byo] names an integration, in file order, whatever its name.
law byo_reads:
  {C.byo("[chat]\nurl = \"ws://127.0.0.1:9999\"\n\n[byo]\ntwitch = \"kartths\"  # me\nyoutube = 'abc123'\nkick = \"k\"\n") == [C.Two{"twitch", "kartths"}, C.Two{"youtube", "abc123"}, C.Two{"kick", "k"}] : List<&2, C.Two>}

law byo_only_its_table:
  {C.byo("[chat]\ntwitch = \"x\"\n[byo]\n") == [] : List<&2, C.Two>}

law config_get:
  {(C.config.get([C.Two{"twitch", "kartths"}, C.Two{"youtube", "v1"}], "youtube"), C.config.get([C.Two{"twitch", "kartths"}], "youtube")) == ("v1", "") : String & String}

law opts_flags:
  {C.opts(["--port", "9000", "--socket", "/tmp/b.sock"], C.Opts{9999, "s", ""}) == C.Opts{9000, "/tmp/b.sock", ""} : C.Opts}

law opts_bad_port:
  {C.opts(["--port", "x"], C.Opts{9999, "s", ""}) == C.Opts{9999, "s", "--port x"} : C.Opts}

# What to read is config, never a flag.
law opts_no_platform_flags:
  {C.opts(["--twitch", "kartths"], C.Opts{9999, "s", ""}) == C.Opts{9999, "s", "--twitch"} : C.Opts}

# Core: the socket's requests and answers
# ---------------------------------------

def say(platform: String, body: String) -> C.J:
  C.JObj{[C.JKv{"say", C.JObj{[C.JKv{"platform", C.JStr{platform}}, C.JKv{"body", C.JStr{body}}]}}]}

law req_say:
  {C.req(say("twitch", "hello chat")) == C.Say{"twitch", "hello chat"} : C.Req}

# Any name passes here: whether an integration by that name is on is the bridge's
# to answer, so a new integration needs no change to the core.
law req_any_platform:
  {C.req(say("kick", "hi")) == C.Say{"kick", "hi"} : C.Req}

law req_no_platform:
  {C.req(say("", "hi")) == C.Bad{"platform is missing"} : C.Req}

law req_empty:
  {C.req(say("twitch", "")) == C.Bad{"body is empty"} : C.Req}

# No line break reaches a platform: one request is one message.
law req_line_break:
  {C.req(say("youtube", "a\nb")) == C.Bad{"body has a line break or another control character"} : C.Req}

law req_not_say:
  {C.req(C.JObj{[C.JKv{"line", C.JNull{}}]}) == C.Bad{"expected {\"say\": {\"platform\": ..., \"body\": ...}} or {\"delete\": {\"platform\": ..., \"id\": ...}}"} : C.Req}

law req_delete:
  {C.req(C.JObj{[C.JKv{"delete", C.JObj{[C.JKv{"platform", C.JStr{"twitch"}}, C.JKv{"id", C.JStr{"m1"}}]}}]}) == C.Delete{"twitch", "m1"} : C.Req}

law req_delete_no_id:
  {C.req(C.JObj{[C.JKv{"delete", C.JObj{[C.JKv{"platform", C.JStr{"twitch"}}]}}]}) == C.Bad{"id is missing"} : C.Req}

law reply_deleted:
  {(C.reply.deleted("twitch", C.Sent{"m1"}), C.reply.deleted("youtube", C.Refused{"403: nope"})) == ("{\"deleted\":{\"platform\":\"twitch\",\"id\":\"m1\"}}", "{\"error\":{\"platform\":\"youtube\",\"message\":\"403: nope\"}}") : String & String}

# remux's wire
# ------------

def frame(key: String, kvs: List<&2, C.JKv>) -> C.J:
  C.JObj{[C.JKv{key, C.JObj{kvs}}]}

law asked_say:
  {C.asked(frame("say", [C.JKv{"body", C.JStr{"valeu!"}}, C.JKv{"channel", C.JStr{"kartths"}}])) == C.AskSay{"kartths", "valeu!"} : C.Asked}

# A say with no channel goes to every chat that is on; one with a channel, to it.
law asked_say_everywhere:
  for chs: List<&2, String>
  {C.asked.targets("", chs) == chs : List<&2, String>}

law asked_say_one:
  {C.asked.targets("kartths", ["kartths", "vid"]) == ["kartths"] : List<&2, String>}

law asked_say_line_break:
  {C.asked(frame("say", [C.JKv{"body", C.JStr{"a\nb"}}])) == C.AskBad{"", "a say with a line break or another control character"} : C.Asked}

law asked_delete:
  {C.asked(frame("delete", [C.JKv{"id", C.JStr{"m1"}}, C.JKv{"channel", C.JStr{"main"}}])) == C.AskDelete{"main", "m1"} : C.Asked}

law asked_heartbeat:
  {C.asked(C.JObj{[C.JKv{"heartbeat", C.JObj{[]}}]}) == C.AskNothing{} : C.Asked}

law notice_deleted:
  {C.notice.deleted("kartths", C.Sent{"m1"}) == "{\"notice\":{\"about\":\"kartths\",\"text\":\"deleted m1\",\"fine\":true}}" : String}

# A say that went out makes no notice: its line comes back down like anybody's.
law notice_said:
  for id: String
  {C.notice.said("kartths", C.Sent{id}) == None{} : Maybe<&2, String>}

law notice_said_failed:
  {C.notice.said("vid", C.Refused{"401: bad token"}) == Some{"{\"notice\":{\"about\":\"vid\",\"text\":\"say failed: 401: bad token\",\"fine\":false}}"} : Maybe<&2, String>}

# Plain chat is one line and no event, whatever it says.
law frames_chat:
  for body: String
  {C.frames(C.ChatEvent{C.Chat{}, "m1", "twitch", "k", "Ana", body, [], ""}) == [C.wire(C.Line{"m1", "twitch", "k", "Ana", body})] : List<&2, String>}

law frames_sub:
  {C.frames(C.ChatEvent{C.Sub{6, "1000"}, "u1", "twitch", "k", "Ana", "hi", [], ""}) == [C.event.json(C.ChatEvent{C.Sub{6, "1000"}, "u1", "twitch", "k", "Ana", "hi", [], ""})] : List<&2, String>}

# A tip with a message: its line, then its event, under one id.
law frames_tip:
  {C.frames(C.ChatEvent{C.Tip{"100", "bits", "100000000"}, "c1", "twitch", "k", "Ana", "gg", [], ""}) == [C.wire(C.Line{"c1", "twitch", "k", "Ana", "gg"}), C.event.json(C.ChatEvent{C.Tip{"100", "bits", "100000000"}, "c1", "twitch", "k", "Ana", "gg", [], ""})] : List<&2, String>}

# A deleted message's text never goes back out as a line, whatever it said.
law frames_deleted:
  for body: String
  {C.frames(C.ChatEvent{C.Deleted{"m1"}, "d1", "twitch", "k", "foo", body, [], ""}) == [C.event.json(C.ChatEvent{C.Deleted{"m1"}, "d1", "twitch", "k", "foo", body, [], ""})] : List<&2, String>}

# "all" is every integration that is on, however many.
law targets_all:
  for names: List<&2, String>
  {C.targets("all", names) == names : List<&2, String>}

law targets_one:
  {C.targets("youtube", ["twitch", "youtube"]) == ["youtube"] : List<&2, String>}

# "olá" is four octets and three characters.
law utf8_length:
  {C.utf8.length(SCon{'o', SCon{'l', SCon{Char.from_u32(195), SCon{Char.from_u32(161), ""}}}}) == 3 : U32}

law said_line:
  {C.said("twitch", "m1") == "{\"said\":{\"platform\":\"twitch\",\"id\":\"m1\"}}" : String}

law reply_refused:
  {C.reply("twitch", C.Refused{"401: Invalid OAuth token"}) == "{\"error\":{\"platform\":\"twitch\",\"message\":\"401: Invalid OAuth token\"}}" : String}

law api_failed_google:
  {C.api.failed(403, C.JObj{[C.JKv{"error", C.JObj{[C.JKv{"code", C.JNum{"403"}}, C.JKv{"message", C.JStr{"The live chat is no longer live."}}]}}]}) == C.Refused{"403: The live chat is no longer live."} : C.Outcome}

law api_failed_twitch:
  {C.api.failed(401, C.JObj{[C.JKv{"error", C.JStr{"Unauthorized"}}, C.JKv{"status", C.JNum{"401"}}, C.JKv{"message", C.JStr{"Invalid OAuth token"}}]}) == C.Refused{"401: Invalid OAuth token"} : C.Outcome}

# Twitch
# ------

# A tagged message: author and id come from the tags, and the text is passed on
# verbatim, whatever it says.
law tw_privmsg:
  for body: String
  {T.irc("@badge-info=;color=#FF0000;display-name=Kartths;id=de88-1e5d;mod=0 :kartths!kartths@kartths.tmi.twitch.tv PRIVMSG #kartths :" ++ body, "fb") == T.Got{C.ChatEvent{C.Chat{}, "de88-1e5d", "twitch", "kartths", "Kartths", body, [], ""}} : T.Irc}

# Without tags: the nick names the author, and the fallback is the id.
law tw_untagged:
  for body: String
  {T.irc(":foo!foo@foo.tmi.twitch.tv PRIVMSG #chan :" ++ body, "fb") == T.Got{C.ChatEvent{C.Chat{}, "fb", "twitch", "chan", "foo", body, [], ""}} : T.Irc}

law tw_tag_escapes:
  {T.irc("@display-name=A\\sB\\:C;id=x :a!a@a PRIVMSG #c :hi", "fb") == T.Got{C.ChatEvent{C.Chat{}, "x", "twitch", "c", "A B;C", "hi", [], ""}} : T.Irc}

law tw_ping:
  for token: String
  {T.irc("PING " ++ token, "fb") == T.Ping{token} : T.Irc}

law tw_join_ignored:
  {T.irc(":foo!foo@foo.tmi.twitch.tv JOIN #chan", "fb") == T.Other{} : T.Irc}

# A cheer is a tip in bits, with its message.
law tw_cheer:
  {T.irc("@bits=100;display-name=Ana;id=c1 :ana!ana@ana PRIVMSG #k :Cheer100 gg", "fb") == T.Got{C.ChatEvent{C.Tip{"100", "bits", "100000000"}, "c1", "twitch", "k", "Ana", "Cheer100 gg", [], ""}} : T.Irc}

# A resub: its months and tier, and the viewer's message verbatim.
law tw_resub:
  for body: String
  {T.irc("@display-name=Ana;id=u1;login=ana;msg-id=resub;msg-param-cumulative-months=6;msg-param-sub-plan=1000 :tmi.twitch.tv USERNOTICE #kartths :" ++ body, "fb") == T.Got{C.ChatEvent{C.Sub{6, "1000"}, "u1", "twitch", "kartths", "Ana", body, [], ""}} : T.Irc}

law tw_sub_silent:
  {T.irc("@display-name=Ana;id=u2;msg-id=sub;msg-param-sub-plan=Prime :tmi.twitch.tv USERNOTICE #kartths", "fb") == T.Got{C.ChatEvent{C.Sub{1, "Prime"}, "u2", "twitch", "kartths", "Ana", "", [], ""}} : T.Irc}

law tw_gift:
  {T.irc("@display-name=Bob;id=g1;msg-id=subgift;msg-param-recipient-display-name=Cid;msg-param-sub-plan=1000 :tmi.twitch.tv USERNOTICE #k", "fb") == T.Got{C.ChatEvent{C.Gift{1, "1000", "Cid"}, "g1", "twitch", "k", "Bob", "", [], ""}} : T.Irc}

# A community gift counts once: the mystery gift carries the count, and the gifts
# it hands out, which name it, are left out.
law tw_community_gift:
  {T.irc("@display-name=Bob;id=g2;msg-id=submysterygift;msg-param-mass-gift-count=5;msg-param-sub-plan=1000 :tmi.twitch.tv USERNOTICE #k", "fb") == T.Got{C.ChatEvent{C.Gift{5, "1000", ""}, "g2", "twitch", "k", "Bob", "", [], ""}} : T.Irc}

law tw_community_gift_parts:
  {T.irc("@display-name=Bob;id=g3;msg-id=subgift;msg-param-community-gift-id=123;msg-param-recipient-display-name=Cid;msg-param-sub-plan=1000 :tmi.twitch.tv USERNOTICE #k", "fb") == T.Other{} : T.Irc}

law tw_raid:
  {T.irc("@display-name=Cid;id=r1;msg-id=raid;msg-param-viewerCount=42 :tmi.twitch.tv USERNOTICE #k", "fb") == T.Got{C.ChatEvent{C.Raid{42}, "r1", "twitch", "k", "Cid", "", [], ""}} : T.Irc}

# What Twitch has no shared word for is its own kind, with its msg-param fields.
law tw_announcement:
  {T.irc("@display-name=Cid;id=a1;msg-id=announcement;msg-param-color=PRIMARY :tmi.twitch.tv USERNOTICE #k :hello all", "fb") == T.Got{C.ChatEvent{C.Custom{"twitch.announcement", [C.Two{"color", "PRIMARY"}]}, "a1", "twitch", "k", "Cid", "hello all", [], ""}} : T.Irc}

law tw_notice_without_id:
  {T.irc("@display-name=Cid;id=a2 :tmi.twitch.tv USERNOTICE #k", "fb") == T.Other{} : T.Irc}

law tw_channel:
  {T.channel(" #Kartths ") == "kartths" : String}

law tw_token_prefix:
  for t: String
  {T.token("oauth:" ++ t) == t : String}

law tw_token_plain:
  {T.token("abc123") == "abc123" : String}

law tw_me:
  {T.me(C.JObj{[C.JKv{"client_id", C.JStr{"cid"}}, C.JKv{"login", C.JStr{"kartths"}}, C.JKv{"scopes", C.JArr{[C.JStr{"chat:read"}, C.JStr{"user:write:chat"}]}}, C.JKv{"user_id", C.JStr{"42"}}]}) == Done{T.Me{"cid", "42", ["chat:read", "user:write:chat"]}} : Result<&2, &2, String, T.Me>}

law tw_me_invalid:
  {T.me(C.JObj{[C.JKv{"status", C.JNum{"401"}}, C.JKv{"message", C.JStr{"invalid access token"}}]}) == Fail{"TWITCH_CHAT_TOKEN is not a valid user token"} : Result<&2, &2, String, T.Me>}

# Each act checks its own scope: a token that may post but not moderate can post.
law tw_need:
  {(T.need(["chat:read", "user:write:chat"], "user:write:chat"), T.need(["chat:read", "user:write:chat"], "moderator:manage:chat_messages")) == (None{}, Some{"TWITCH_CHAT_TOKEN lacks the moderator:manage:chat_messages scope"}) : Maybe<&2, String> & Maybe<&2, String>}

law tw_delete_url:
  {T.delete_url("1", "2", "ab-c") == "https://api.twitch.tv/helix/moderation/chat?broadcaster_id=1&moderator_id=2&message_id=ab-c" : String}

law tw_body:
  {T.body("1", "2", "hi \"all\"") == "{\"broadcaster_id\":\"1\",\"sender_id\":\"2\",\"message\":\"hi \\\"all\\\"\"}" : String}

law tw_sent:
  {T.sent(C.JObj{[C.JKv{"data", C.JArr{[C.JObj{[C.JKv{"message_id", C.JStr{"abc"}}, C.JKv{"is_sent", C.JBool{True{}}}, C.JKv{"drop_reason", C.JNull{}}]}]}}]}) == C.Sent{"abc"} : C.Outcome}

law tw_dropped:
  {T.sent(C.JObj{[C.JKv{"data", C.JArr{[C.JObj{[C.JKv{"message_id", C.JStr{""}}, C.JKv{"is_sent", C.JBool{False{}}}, C.JKv{"drop_reason", C.JObj{[C.JKv{"code", C.JStr{"msg_duplicate"}}, C.JKv{"message", C.JStr{"Your message is identical"}}]}}]}]}}]}) == C.Refused{"Your message is identical"} : C.Outcome}

# Badges in shared words; a partner is verified; first for a first message; and the
# message it replies to.
law tw_badges_reply:
  {T.irc("@badges=broadcaster/1,subscriber/12,glhf-pledge/1,partner/1;display-name=K;first-msg=1;id=x;reply-parent-msg-id=p1 :k!k@k PRIVMSG #k :hi", "fb") == T.Got{C.ChatEvent{C.Chat{}, "x", "twitch", "k", "K", "hi", ["broadcaster", "member", "verified", "first"], "p1"}} : T.Irc}

law tw_redemption:
  {T.irc("@custom-reward-id=r-1;display-name=Ana;id=x :ana!ana@ana PRIVMSG #k :play my song", "fb") == T.Got{C.ChatEvent{C.Custom{"twitch.redemption", [C.Two{"reward", "r-1"}]}, "x", "twitch", "k", "Ana", "play my song", [], ""}} : T.Irc}

law tw_power_up:
  {T.irc("@display-name=Ana;id=x;msg-id=gigantified-emote-message :ana!ana@ana PRIVMSG #k :Kappa", "fb") == T.Got{C.ChatEvent{C.Custom{"twitch.gigantified-emote-message", []}, "x", "twitch", "k", "Ana", "Kappa", [], ""}} : T.Irc}

law tw_gift_upgrade:
  {T.irc("@display-name=Ana;id=u3;msg-id=giftpaidupgrade;msg-param-sender-login=bob :tmi.twitch.tv USERNOTICE #k", "fb") == T.Got{C.ChatEvent{C.Sub{1, ""}, "u3", "twitch", "k", "Ana", "", [], ""}} : T.Irc}

# A deleted message: its id, its author, and its text.
law tw_deleted:
  for body: String
  {T.irc("@login=foo;room-id=;target-msg-id=abc;tmi-sent-ts=1 :tmi.twitch.tv CLEARMSG #k :" ++ body, "fb") == T.Got{C.ChatEvent{C.Deleted{"abc"}, "fb", "twitch", "k", "foo", body, [], ""}} : T.Irc}

law tw_timeout:
  {T.irc("@ban-duration=600;room-id=1;target-user-id=2 :tmi.twitch.tv CLEARCHAT #k :troll", "fb") == T.Got{C.ChatEvent{C.Banned{"troll", 600}, "fb", "twitch", "k", "", "", [], ""}} : T.Irc}

law tw_ban:
  {T.irc("@room-id=1;target-user-id=2 :tmi.twitch.tv CLEARCHAT #k :troll", "fb") == T.Got{C.ChatEvent{C.Banned{"troll", 0}, "fb", "twitch", "k", "", "", [], ""}} : T.Irc}

law tw_cleared:
  {T.irc("@room-id=1 :tmi.twitch.tv CLEARCHAT #k", "fb") == T.Got{C.ChatEvent{C.Cleared{}, "fb", "twitch", "k", "", "", [], ""}} : T.Irc}

# A renewal: the new token, the refresh token Twitch handed back, and its life.
law tw_renewed:
  {T.renewed(C.JObj{[C.JKv{"access_token", C.JStr{"new"}}, C.JKv{"expires_in", C.JNum{"14124"}}, C.JKv{"refresh_token", C.JStr{"next"}}, C.JKv{"scope", C.JArr{[C.JStr{"user:write:chat"}]}}]}, "old") == Done{T.Renewal{"new", "next", "14124"}} : Result<&2, &2, String, T.Renewal>}

# When no refresh token comes back, the one that asked is kept.
law tw_renewed_keeps_refresh:
  for old: String
  {T.renewed(C.JObj{[C.JKv{"access_token", C.JStr{"new"}}, C.JKv{"expires_in", C.JNum{"14124"}}]}, old) == Done{T.Renewal{"new", old, "14124"}} : Result<&2, &2, String, T.Renewal>}

law tw_renewed_refused:
  {T.renewed(C.JObj{[C.JKv{"status", C.JNum{"400"}}, C.JKv{"message", C.JStr{"Invalid refresh token"}}]}, "old") == Fail{"renewing TWITCH_REFRESH_TOKEN failed: Invalid refresh token"} : Result<&2, &2, String, T.Renewal>}

law tw_renew_form:
  {T.renew_form("cid", "s/x", "r+1") == "client_id=cid&client_secret=s%2Fx&refresh_token=r%2B1&grant_type=refresh_token" : String}

# Twitch EventSub
# ---------------

def o(kvs: List<&2, C.JKv>) -> C.J:
  C.JObj{kvs}

def s(x: String) -> C.J:
  C.JStr{x}

def msg(ty: String, sub: String, id: String, payload: List<&2, C.JKv>) -> C.J:
  o([C.JKv{"metadata", o([C.JKv{"message_id", s(id)}, C.JKv{"message_type", s(ty)}, C.JKv{"subscription_type", s(sub)}])}, C.JKv{"payload", o(payload)}])

def note(+sub: String, id: String, event: List<&2, C.JKv>) -> C.J:
  msg("notification", sub, id, [C.JKv{"subscription", o([C.JKv{"type", s(sub)}])}, C.JKv{"event", o(event)}])

law es_welcome:
  {E.es(msg("session_welcome", "", "w1", [C.JKv{"session", o([C.JKv{"id", s("AQoQ")}, C.JKv{"status", s("connected")}, C.JKv{"keepalive_timeout_seconds", C.JNum{"30"}}, C.JKv{"reconnect_url", C.JNull{}}])}])) == E.Welcome{"AQoQ", 30} : E.Es}

law es_keepalive:
  {E.es(msg("session_keepalive", "", "k1", [])) == E.Keepalive{} : E.Es}

law es_reconnect:
  {E.es(msg("session_reconnect", "", "r1", [C.JKv{"session", o([C.JKv{"reconnect_url", s("wss://eventsub.wss.twitch.tv/ws?id=abc")}])}])) == E.Reconnect{"wss://eventsub.wss.twitch.tv/ws?id=abc"} : E.Es}

law es_revoked:
  {E.es(msg("revocation", "channel.follow", "v1", [C.JKv{"subscription", o([C.JKv{"type", s("channel.follow")}, C.JKv{"status", s("authorization_revoked")}])}])) == E.Revoked{"channel.follow", "authorization_revoked"} : E.Es}

# A follow: who followed, on which channel, by the message's own id.
law es_follow:
  {E.es(note("channel.follow", "f1", [C.JKv{"user_login", s("cool_user")}, C.JKv{"user_name", s("Cool_User")}, C.JKv{"broadcaster_user_login", s("kartths")}, C.JKv{"followed_at", s("2026-09-30T00:00:00Z")}])) == E.Note{Some{C.ChatEvent{C.Follow{}, "f1", "twitch", "kartths", "Cool_User", "", [], ""}}} : E.Es}

law es_redemption:
  {E.es(note("channel.channel_points_custom_reward_redemption.add", "p1", [C.JKv{"user_name", s("Ana")}, C.JKv{"user_input", s("")}, C.JKv{"broadcaster_user_login", s("kartths")}, C.JKv{"reward", o([C.JKv{"id", s("r-1")}, C.JKv{"title", s("Hydrate")}, C.JKv{"cost", C.JNum{"500"}}])}])) == E.Note{Some{C.ChatEvent{C.Custom{"twitch.redemption", [C.Two{"reward", "r-1"}, C.Two{"title", "Hydrate"}, C.Two{"cost", "500"}]}, "p1", "twitch", "kartths", "Ana", "", [], ""}}} : E.Es}

# A redemption that asked for text came over IRC with it: not said twice.
law es_redemption_with_text:
  {E.es(note("channel.channel_points_custom_reward_redemption.add", "p2", [C.JKv{"user_name", s("Ana")}, C.JKv{"user_input", s("play my song")}, C.JKv{"broadcaster_user_login", s("kartths")}])) == E.Note{None{}} : E.Es}

law es_poll_end:
  {E.es(note("channel.poll.end", "q1", [C.JKv{"broadcaster_user_login", s("kartths")}, C.JKv{"title", s("Pizza?")}, C.JKv{"status", s("completed")}, C.JKv{"choices", C.JArr{[o([C.JKv{"id", s("a")}, C.JKv{"title", s("Yes")}, C.JKv{"votes", C.JNum{"12"}}]), o([C.JKv{"id", s("b")}, C.JKv{"title", s("No")}, C.JKv{"votes", C.JNum{"3"}}])]}}])) == E.Note{Some{C.ChatEvent{C.Custom{"twitch.poll.end", [C.Two{"title", "Pizza?"}, C.Two{"status", "completed"}, C.Two{"choices", "Yes:12|No:3"}]}, "q1", "twitch", "kartths", "", "", [], ""}}} : E.Es}

law es_prediction_end:
  {E.es(note("channel.prediction.end", "d1", [C.JKv{"broadcaster_user_login", s("kartths")}, C.JKv{"title", s("Win?")}, C.JKv{"winning_outcome_id", s("o2")}, C.JKv{"outcomes", C.JArr{[o([C.JKv{"id", s("o1")}, C.JKv{"title", s("Yes")}]), o([C.JKv{"id", s("o2")}, C.JKv{"title", s("No")}])]}}])) == E.Note{Some{C.ChatEvent{C.Custom{"twitch.prediction.end", [C.Two{"title", "Win?"}, C.Two{"outcomes", "Yes|No"}, C.Two{"winner", "No"}]}, "d1", "twitch", "kartths", "", "", [], ""}}} : E.Es}

law es_stream_online:
  {E.es(note("stream.online", "s1", [C.JKv{"broadcaster_user_login", s("kartths")}, C.JKv{"type", s("live")}])) == E.Note{Some{C.ChatEvent{C.Custom{"twitch.stream.online", [C.Two{"type", "live"}]}, "s1", "twitch", "kartths", "", "", [], ""}}} : E.Es}

law es_raid_out:
  {E.es(note("channel.raid", "x1", [C.JKv{"from_broadcaster_user_login", s("kartths")}, C.JKv{"to_broadcaster_user_name", s("Friend")}, C.JKv{"viewers", C.JNum{"42"}}])) == E.Note{Some{C.ChatEvent{C.Custom{"twitch.raid.out", [C.Two{"to", "Friend"}, C.Two{"viewers", "42"}]}, "x1", "twitch", "kartths", "", "", [], ""}}} : E.Es}

def want.names(ws: List<&2, E.Want>) -> List<&2, String>:
  match ws:
    case Nil{}:
      []
    case Con{w, t}:
      E.name(w) <> want.names(t)

def plan.names(p: E.Plan) -> List<&2, String> & List<&2, String>:
  match p:
    case E.Plan{ws, skipped}:
      (want.names(ws), skipped)

# A token subscribes to what its scopes allow; the rest is named, with what it needs.
law es_plan:
  {plan.names(E.plan("1", "2", ["moderator:read:followers", "channel:manage:polls"])) == (["channel.follow", "channel.poll.begin", "channel.poll.end", "stream.online", "stream.offline", "channel.raid"], ["channel.channel_points_custom_reward_redemption.add (needs channel:read:redemptions or channel:manage:redemptions)", "channel.hype_train.begin (needs channel:read:hype_train)", "channel.hype_train.progress (needs channel:read:hype_train)", "channel.hype_train.end (needs channel:read:hype_train)", "channel.prediction.begin (needs channel:read:predictions or channel:manage:predictions)", "channel.prediction.lock (needs channel:read:predictions or channel:manage:predictions)", "channel.prediction.end (needs channel:read:predictions or channel:manage:predictions)"]) : List<&2, String> & List<&2, String>}

law es_body:
  {E.body(E.Want{"channel.follow", "2", "{\"broadcaster_user_id\":\"1\",\"moderator_user_id\":\"2\"}", []}, "AQoQ") == "{\"type\":\"channel.follow\",\"version\":\"2\",\"condition\":{\"broadcaster_user_id\":\"1\",\"moderator_user_id\":\"2\"},\"transport\":{\"method\":\"websocket\",\"session_id\":\"AQoQ\"}}" : String}

law es_ws_url:
  {E.ws_url(E.base_url()) == C.Two{"eventsub.wss.twitch.tv", "/ws?keepalive_timeout_seconds=30"} : C.Two}

# YouTube
# -------

def author(from: String, owner: Bool, member: Bool) -> C.J:
  C.JObj{[C.JKv{"displayName", C.JStr{from}}, C.JKv{"isChatOwner", C.JBool{owner}}, C.JKv{"isChatModerator", C.JBool{False{}}}, C.JKv{"isChatSponsor", C.JBool{member}}, C.JKv{"isVerified", C.JBool{False{}}}]}

law yt_badges:
  {Y.event(C.JObj{[C.JKv{"id", C.JStr{"m1"}}, C.JKv{"snippet", C.JObj{[C.JKv{"type", C.JStr{"textMessageEvent"}}, C.JKv{"displayMessage", C.JStr{"hi"}}]}}, C.JKv{"authorDetails", author("Ana", True{}, True{})}]}, "vid") == Some{C.ChatEvent{C.Chat{}, "m1", "youtube", "vid", "Ana", "hi", ["broadcaster", "member"], ""}} : Maybe<&2, C.ChatEvent>}

law yt_deleted:
  {Y.event(item("d1", "Mod", [C.JKv{"type", C.JStr{"messageDeletedEvent"}}, C.JKv{"messageDeletedDetails", C.JObj{[C.JKv{"deletedMessageId", C.JStr{"m9"}}]}}]), "vid") == Some{C.ChatEvent{C.Deleted{"m9"}, "d1", "youtube", "vid", "Mod", "", [], ""}} : Maybe<&2, C.ChatEvent>}

law yt_timeout:
  {Y.event(item("b1", "Mod", [C.JKv{"type", C.JStr{"userBannedEvent"}}, C.JKv{"userBannedDetails", C.JObj{[C.JKv{"bannedUserDetails", C.JObj{[C.JKv{"displayName", C.JStr{"troll"}}]}}, C.JKv{"banType", C.JStr{"temporary"}}, C.JKv{"banDurationSeconds", C.JStr{"300"}}]}}]), "vid") == Some{C.ChatEvent{C.Banned{"troll", 300}, "b1", "youtube", "vid", "Mod", "", [], ""}} : Maybe<&2, C.ChatEvent>}

law yt_ban:
  {Y.event(item("b2", "Mod", [C.JKv{"type", C.JStr{"userBannedEvent"}}, C.JKv{"userBannedDetails", C.JObj{[C.JKv{"bannedUserDetails", C.JObj{[C.JKv{"displayName", C.JStr{"troll"}}]}}, C.JKv{"banType", C.JStr{"permanent"}}]}}]), "vid") == Some{C.ChatEvent{C.Banned{"troll", 0}, "b2", "youtube", "vid", "Mod", "", [], ""}} : Maybe<&2, C.ChatEvent>}

# What YouTube has no shared word for is its own kind.
law yt_own_kind:
  {Y.event(item("p1", "Ana", [C.JKv{"type", C.JStr{"pollEvent"}}]), "vid") == Some{C.ChatEvent{C.Custom{"youtube.pollEvent", []}, "p1", "youtube", "vid", "Ana", "", [], ""}} : Maybe<&2, C.ChatEvent>}


def item(id: String, from: String, snippet: List<&2, C.JKv>) -> C.J:
  C.JObj{[C.JKv{"id", C.JStr{id}}, C.JKv{"snippet", C.JObj{snippet}}, C.JKv{"authorDetails", C.JObj{[C.JKv{"displayName", C.JStr{from}}]}}]}

# A page: its events (an item without a type is left out), the next page's
# token, and a wait of 3 s at least.
law yt_page:
  {Y.page(C.JObj{[C.JKv{"nextPageToken", C.JStr{"tok"}}, C.JKv{"pollingIntervalMillis", C.JNum{"1000"}}, C.JKv{"items", C.JArr{[item("m1", "Ana", [C.JKv{"type", C.JStr{"textMessageEvent"}}, C.JKv{"displayMessage", C.JStr{"hi"}}]), item("m2", "Bob", [])]}}]}, "vid") == Y.Page{[C.ChatEvent{C.Chat{}, "m1", "youtube", "vid", "Ana", "hi", [], ""}], "tok", 3000} : Y.Page}

law yt_superchat:
  {Y.event(item("s1", "Ana", [C.JKv{"type", C.JStr{"superChatEvent"}}, C.JKv{"superChatDetails", C.JObj{[C.JKv{"amountMicros", C.JStr{"5000000"}}, C.JKv{"amountDisplayString", C.JStr{"$5.00"}}, C.JKv{"currency", C.JStr{"USD"}}, C.JKv{"userComment", C.JStr{"love it"}}]}}]), "vid") == Some{C.ChatEvent{C.Tip{"$5.00", "USD", "5000000"}, "s1", "youtube", "vid", "Ana", "love it", [], ""}} : Maybe<&2, C.ChatEvent>}

law yt_member:
  {Y.event(item("n1", "Bob", [C.JKv{"type", C.JStr{"newSponsorEvent"}}, C.JKv{"newSponsorDetails", C.JObj{[C.JKv{"memberLevelName", C.JStr{"Fan"}}]}}]), "vid") == Some{C.ChatEvent{C.Sub{1, "Fan"}, "n1", "youtube", "vid", "Bob", "", [], ""}} : Maybe<&2, C.ChatEvent>}

law yt_milestone:
  {Y.event(item("n2", "Bob", [C.JKv{"type", C.JStr{"memberMilestoneChatEvent"}}, C.JKv{"memberMilestoneChatDetails", C.JObj{[C.JKv{"memberMonth", C.JNum{"12"}}, C.JKv{"memberLevelName", C.JStr{"Fan"}}, C.JKv{"userComment", C.JStr{"a year!"}}]}}]), "vid") == Some{C.ChatEvent{C.Sub{12, "Fan"}, "n2", "youtube", "vid", "Bob", "a year!", [], ""}} : Maybe<&2, C.ChatEvent>}

law yt_gifting:
  {Y.event(item("g1", "Cid", [C.JKv{"type", C.JStr{"membershipGiftingEvent"}}, C.JKv{"membershipGiftingDetails", C.JObj{[C.JKv{"giftMembershipsCount", C.JNum{"5"}}, C.JKv{"giftMembershipsLevelName", C.JStr{"Fan"}}]}}]), "vid") == Some{C.ChatEvent{C.Gift{5, "Fan", ""}, "g1", "youtube", "vid", "Cid", "", [], ""}} : Maybe<&2, C.ChatEvent>}

law yt_chat_id:
  {Y.chat_id(C.JObj{[C.JKv{"items", C.JArr{[C.JObj{[C.JKv{"liveStreamingDetails", C.JObj{[C.JKv{"activeLiveChatId", C.JStr{"Cg0KC"}}]}}]}]}}]}) == "Cg0KC" : String}

law yt_refresh_form:
  {Y.refresh_form("id.apps", "s/x", "1//r") == "client_id=id.apps&client_secret=s%2Fx&refresh_token=1%2F%2Fr&grant_type=refresh_token" : String}

law yt_token:
  {Y.token(C.JObj{[C.JKv{"access_token", C.JStr{"ya29.x"}}, C.JKv{"expires_in", C.JNum{"3599"}}]}) == Done{C.Two{"ya29.x", "3599"}} : Result<&2, &2, String, C.Two>}

law yt_token_refused:
  {Y.token(C.JObj{[C.JKv{"error", C.JStr{"invalid_grant"}}, C.JKv{"error_description", C.JStr{"Token has been expired or revoked."}}]}) == Fail{"refreshing the token failed: Token has been expired or revoked."} : Result<&2, &2, String, C.Two>}

law yt_delete_url:
  {Y.delete_url("LCC.x/y") == "https://www.googleapis.com/youtube/v3/liveChat/messages?id=LCC.x%2Fy" : String}

law yt_body:
  {Y.body("Cg0", "hi") == "{\"snippet\":{\"liveChatId\":\"Cg0\",\"type\":\"textMessageEvent\",\"textMessageDetails\":{\"messageText\":\"hi\"}}}" : String}
