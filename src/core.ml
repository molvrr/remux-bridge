(* The bridge's shared vocabulary: events, and the text the bridge speaks (remux's
   wire, the config). No IO. Text is octets, as OCaml strings are: UTF-8 passes
   through untouched, and only ASCII is ever looked at or escaped. *)

(* Events
   ------ *)

(* What happened, in words every platform shares. A platform says what it can:
   Twitch has subs, gifts, bits and raids; YouTube memberships, gifts and Super Chats.
     Tip       amount as the platform shows it; micros, the amount in millionths of
               currency as decimal digits (exact at any size), to add or rank tips
               (bits are a currency here: 100 bits is 100000000 micros of "bits")
     Deleted   a message taken down: target is its id
     Banned    a viewer banned (seconds 0) or timed out
     Cleared   the whole chat cleared
     Custom    what only one platform has, named "<platform>.<what>", with its fields:
               an integration reports it without a change here *)
type what =
  | Chat
  | Sub of { months : int; tier : string }
  | Gift of { count : int; tier : string; to_ : string }
  | Tip of { amount : string; currency : string; micros : string }
  | Raid of { viewers : int }
  | Follow
  | Deleted of { target : string }
  | Banned of { user : string; seconds : int }
  | Cleared
  | Custom of { name : string; fields : (string * string) list }

(* One thing that happened in a chat. body is what the viewer wrote, "" when nothing.
   badges say who from is, in words every platform shares: broadcaster, moderator,
   vip, member, verified, and first for a first message. reply is the id of the
   message this one answers, "" when none. *)
type chat_event = {
  what : what;
  id : string;
  platform : string;
  channel : string;
  from : string;
  body : string;
  badges : string list;
  reply : string;
}

(* How a send ended. *)
type outcome = Sent of string | Refused of string

(* Text
   ---- *)

(* The text before the first c and the text after it; (s, "") when there is none. *)
let cut s c =
  match String.index_opt s c with
  | None -> (s, "")
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

(* s without one leading c. *)
let drop1 s c =
  if s <> "" && s.[0] = c then String.sub s 1 (String.length s - 1) else s

(* s without its first n octets. *)
let drop s n =
  if n >= String.length s then "" else String.sub s n (String.length s - n)

let starts_with s ~prefix = String.starts_with ~prefix s

(* a, or b when a is empty. *)
let or_else a b = if a = "" then b else a

(* The number s spells, as decimal digits that fit in 32 bits. *)
let read_number s =
  let is_digit c = c >= '0' && c <= '9' in
  if s = "" || String.length s > 10 || not (String.for_all is_digit s) then None
  else
    let n = int_of_string s in
    if n > 0xFFFF_FFFF then None else Some n

(* The number s spells, or d. *)
let number s d = Option.value (read_number s) ~default:d

(* The digits of s, and nothing else; "0" when it has none. A number of any size,
   safe to write into JSON as it is. *)
let digits s = or_else (String.of_seq (Seq.filter (fun c -> c >= '0' && c <= '9') (String.to_seq s))) "0"

let is_quote c = c = '"' || c = '\''

(* s without the quotes around it, as Python's strip("\"'"). *)
let unquote s =
  let n = String.length s in
  let i = ref 0 and j = ref n in
  while !i < n && is_quote s.[!i] do incr i done;
  while !j > !i && is_quote s.[!j - 1] do decr j done;
  String.sub s !i (!j - !i)

let is_control c = Char.code c < 32 || Char.code c = 127

(* Control characters as '?', for a terminal: chat text is a stranger's. *)
let printable s = String.map (fun c -> if is_control c then '?' else c) s

(* Any octet below 32, or 127: line breaks and other control characters. *)
let has_control s = String.exists is_control s

(* Characters in UTF-8 octets: every octet but a continuation (0x80..0xBF) starts one. *)
let utf8_length s =
  String.fold_left (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1) 0 s

let url_keep = function
  | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' | '-' | '_' | '.' | '~' -> true
  | _ -> false

(* s percent-encoded for a URL query: all but A-Z a-z 0-9 - _ . ~ (RFC 3986 §2.3). *)
let url_esc s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c -> if url_keep c then Buffer.add_char b c else Printf.bprintf b "%%%02X" (Char.code c)) s;
  Buffer.contents b

(* Lines
   ----- *)

(* The whole lines of buf, each without its "\r\n", and the partial line after them. *)
let lines buf =
  let parts = String.split_on_char '\n' buf in
  let rec go acc = function
    | [] -> (List.rev acc, "")
    | [ rest ] -> (List.rev acc, rest)
    | l :: t ->
        let n = String.length l in
        go ((if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l) :: acc) t
  in
  go [] parts

(* JSON out
   -------- *)

(* s as the inside of a JSON string (RFC 8259 §7). Octets from 0x80 up are UTF-8
   and pass through as they are. *)
let esc s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 32 -> Printf.bprintf b "\\u%04x" (Char.code c)
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let str s = "\"" ^ esc s ^ "\""

(* One chat line as remux's wire spells it (`remux schema`, wire_line). *)
type line = { l_id : string; l_platform : string; l_channel : string; l_from : string; l_body : string }

(* One wire frame: {"line": {id, platform, channel, from, body}}. *)
let wire l =
  "{\"line\":{\"id\":" ^ str l.l_id ^ ",\"platform\":" ^ str l.l_platform ^ ",\"channel\":" ^ str l.l_channel
  ^ ",\"from\":" ^ str l.l_from ^ ",\"body\":" ^ str l.l_body ^ "}}"

(* What remux's wire shows of an event: what was written in chat, a chat message or
   the message of a tip or of a platform's own kind (an announcement, a redemption).
   A deleted message's text never goes back out. *)
let as_line e =
  let line = { l_id = e.id; l_platform = e.platform; l_channel = e.channel; l_from = e.from; l_body = e.body } in
  match e.what with
  | Chat -> Some line
  | Tip _ | Custom _ -> if e.body = "" then None else Some line
  | Sub _ | Gift _ | Raid _ | Follow | Deleted _ | Banned _ | Cleared -> None

let what_name = function
  | Chat -> "chat"
  | Sub _ -> "sub"
  | Gift _ -> "gift"
  | Tip _ -> "tip"
  | Raid _ -> "raid"
  | Follow -> "follow"
  | Deleted _ -> "deleted"
  | Banned _ -> "banned"
  | Cleared -> "cleared"
  | Custom _ -> "custom"

(* ["a","b"] *)
let strs xs = "[" ^ String.concat "," (List.map str xs) ^ "]"

(* {"k":"v",…} *)
let obj kvs = "{" ^ String.concat "," (List.map (fun (k, v) -> str k ^ ":" ^ str v) kvs) ^ "}"

(* Its own fields, each after a comma. *)
let what_json = function
  | Chat | Follow | Cleared -> ""
  | Sub { months; tier } -> ",\"months\":" ^ string_of_int months ^ ",\"tier\":" ^ str tier
  | Gift { count; tier; to_ } ->
      ",\"count\":" ^ string_of_int count ^ ",\"tier\":" ^ str tier ^ ",\"to\":" ^ str to_
  | Tip { amount; currency; micros } ->
      ",\"amount\":" ^ str amount ^ ",\"currency\":" ^ str currency ^ ",\"micros\":" ^ digits micros
  | Raid { viewers } -> ",\"viewers\":" ^ string_of_int viewers
  | Deleted { target } -> ",\"target\":" ^ str target
  | Banned { user; seconds } -> ",\"user\":" ^ str user ^ ",\"seconds\":" ^ string_of_int seconds
  | Custom { name; fields } -> ",\"name\":" ^ str name ^ ",\"fields\":" ^ obj fields

(* One event as remux's wire sends it:
     {"event": {"type", "id", "platform", "channel", "from", "body", "badges",
                "reply", …the fields of what}} *)
let event_json e =
  "{\"event\":{\"type\":" ^ str (what_name e.what) ^ ",\"id\":" ^ str e.id ^ ",\"platform\":" ^ str e.platform
  ^ ",\"channel\":" ^ str e.channel ^ ",\"from\":" ^ str e.from ^ ",\"body\":" ^ str e.body ^ ",\"badges\":"
  ^ strs e.badges ^ ",\"reply\":" ^ str e.reply ^ what_json e.what ^ "}}"

(* JSON in
   ------- *)

(* A JSON value, as the bridge reads APIs' answers. A number keeps its text. *)
type j = JNull | JBool of bool | JNum of string | JStr of string | JArr of j list | JObj of (string * j) list

(* The field key of an object; JNull for anything else. *)
let j_get j key = match j with JObj kvs -> Option.value (List.assoc_opt key kvs) ~default:JNull | _ -> JNull
let j_str = function JStr s | JNum s -> s | _ -> ""
let j_list = function JArr xs -> xs | _ -> []
let j_first = function x :: _ -> x | [] -> JNull
let j_is_true = function JBool b -> b | _ -> false
let j_is_obj = function JObj _ -> true | _ -> false

(* The message of an API error: Google's {"error": {"message"}}, or Twitch's
   {"error", "message"}. *)
let api_why resp =
  or_else (j_str (j_get (j_get resp "error") "message")) (or_else (j_str (j_get resp "message")) (j_str (j_get resp "error")))

let api_failed status resp = Refused (string_of_int status ^ ": " ^ or_else (api_why resp) "no reason given")

(* The WebSocket handshake
   ----------------------- *)

let find_sub s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
  go 0

(* An HTTP head once its blank line came: the head, and the bytes after it. *)
let head buf =
  match find_sub buf "\r\n\r\n" with
  | None -> None
  | Some i -> Some (String.sub buf 0 i, drop buf (i + 4))

(* The value of the first header line called name (lowercase); "" when there is none. *)
let header head name =
  String.split_on_char '\n' head
  |> List.find_map (fun l ->
         let k, v = cut l ':' in
         if String.lowercase_ascii (String.trim k) = name then Some (String.trim v) else None)
  |> Option.value ~default:""

(* Config and flags
   ---------------- *)

(* Every key = value of the [byo] table of remux's config.toml, in file order. Each
   key names an integration; its value is what that integration reads (a channel, a video). *)
let byo text =
  let _, acc =
    List.fold_left
      (fun (inside, acc) x ->
        let l = String.trim (fst (cut x '#')) in
        let table = starts_with l ~prefix:"[" in
        let inside = if table then String.trim (fst (cut (drop1 l '[') ']')) = "byo" else inside in
        if inside && String.contains l '=' && not table then
          let k, v = cut l '=' in
          (inside, (String.trim k, unquote (String.trim v)) :: acc)
        else (inside, acc))
      (false, []) (String.split_on_char '\n' text)
  in
  List.rev acc

(* The value of key; "" when it is not there. *)
let config_get cfg key = Option.value (List.assoc_opt key cfg) ~default:""

type opts = { port : int; bad_flag : string }

(* --port <n>, over o. The first word it could not use is in
   bad_flag. What to read is config, not flags: the [byo] table. *)
let rec opts args o =
  match args with
  | [] -> o
  | [ flag ] -> { o with bad_flag = or_else o.bad_flag (flag ^ " needs a value") }
  | flag :: v :: rest ->
      let o =
        if flag = "--port" then
          match read_number v with
          | None -> { o with bad_flag = or_else o.bad_flag ("--port " ^ v) }
          | Some n -> { o with port = n }
        else { o with bad_flag = or_else o.bad_flag flag }
      in
      opts rest o

(* remux's wire, up
   ----------------
   What the engine asks of a chat bridge (docs/wire.md): say, into the chat called
   channel ("" for every chat), or delete a line by its id. The rest (heartbeats,
   open) asks for nothing. *)

type asked =
  | Ask_say of { channel : string; body : string }
  | Ask_delete of { channel : string; id : string }
  | Ask_bad of { about : string; why : string }
  | Ask_nothing

let asked_say say =
  let channel = j_str (j_get say "channel") and body = j_str (j_get say "body") in
  if body = "" then Ask_bad { about = channel; why = "a say with no body" }
  else if has_control body then Ask_bad { about = channel; why = "a say with a line break or another control character" }
  else Ask_say { channel; body }

let asked_delete del =
  let channel = j_str (j_get del "channel") and id = j_str (j_get del "id") in
  if channel = "" || id = "" then Ask_bad { about = channel; why = "a delete needs an id and a channel" }
  else Ask_delete { channel; id }

(* One frame from the engine, as parsed JSON. *)
let asked frame =
  let say = j_get frame "say" and del = j_get frame "delete" in
  if j_is_obj say then asked_say say else if j_is_obj del then asked_delete del else Ask_nothing

(* The chats a say into channel goes to: the one called so, or every one for "". *)
let asked_targets channel channels = if channel = "" then channels else [ channel ]

(* remux's wire, down: what went wrong, or right, with what the engine asked. *)
let notice about text fine =
  "{\"notice\":{\"about\":" ^ str about ^ ",\"text\":" ^ str text ^ ",\"fine\":" ^ (if fine then "true" else "false") ^ "}}"

(* The notice for how a delete ended. *)
let notice_deleted about = function
  | Sent id -> notice about ("deleted " ^ id) true
  | Refused why -> notice about ("delete failed: " ^ why) false

(* The notice for a say that failed; none for one that went out, as its line comes
   back down like anybody's. *)
let notice_said about = function Sent _ -> None | Refused why -> Some (notice about ("say failed: " ^ why) false)

(* Whether remux's wire gets an event frame too: for all but plain chat, which the
   line carries. A tip's message comes as a line and as the event, with one id. *)
let is_event e = match e.what with Chat -> false | _ -> true

(* The frames an event makes on remux's wire: its line, if it has one, then the
   event itself, unless it is plain chat. *)
let frames e =
  (match as_line e with Some l -> [ wire l ] | None -> []) @ if is_event e then [ event_json e ] else []
