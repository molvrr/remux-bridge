(* WebSocket (RFC 6455), both ends: the bridge serves remux's wire, and dials
   Twitch's EventSub. Frames are read off an Eio.Buf_read, so what came in with
   the handshake is not lost. *)

type incoming = Text of string | Binary of string | Pong of string | Closed of int * string

exception Protocol of string

type conn = {
  r : Eio.Buf_read.t;
  write : string -> unit;
  shutdown : unit -> unit;
  client : bool;  (** a client masks what it sends *)
  max : int;  (** the largest message it takes *)
  lock : Eio.Mutex.t;  (** one frame at a time goes out *)
  mutable closing : bool;  (** a close frame went out *)
}

let max_message = 1048576

(* A Buf_read for a connection that will carry messages of max octets. *)
let reader ?(max = max_message) flow = Eio.Buf_read.of_flow flow ~max_size:(max + 64)

let make ~client ?(max = max_message) ~r flow =
  {
    r;
    write = (fun s -> Eio.Flow.copy_string s flow);
    shutdown = (fun () -> Eio.Flow.shutdown flow `All);
    client;
    max;
    lock = Eio.Mutex.create ();
    closing = false;
  }

let guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

(* The Sec-WebSocket-Accept that answers key. *)
let accept key = Base64.encode_string (Digestif.SHA1.to_raw_string (Digestif.SHA1.digest_string (key ^ guid)))

(* Frames
   ------ *)

let mask key data =
  if key = "" then data else String.mapi (fun i c -> Char.chr (Char.code c lxor Char.code key.[i land 3])) data

let big_endian s = String.fold_left (fun v c -> (v lsl 8) lor Char.code c) 0 s

let frame c =
  let b0 = Char.code (Eio.Buf_read.any_char c.r) in
  let b1 = Char.code (Eio.Buf_read.any_char c.r) in
  let len =
    match b1 land 0x7f with
    | 126 -> big_endian (Eio.Buf_read.take 2 c.r)
    | 127 -> big_endian (Eio.Buf_read.take 8 c.r)
    | n -> n
  in
  if len < 0 || len > c.max then raise (Protocol "a frame larger than allowed");
  let key = if b1 land 0x80 <> 0 then Eio.Buf_read.take 4 c.r else "" in
  (b0 land 0x80 <> 0, b0 land 0x0f, mask key (Eio.Buf_read.take len c.r))

let send c op data =
  let n = String.length data in
  let b = Buffer.create (n + 14) in
  let m = if c.client then 0x80 else 0 in
  Buffer.add_char b (Char.chr (0x80 lor op));
  if n < 126 then Buffer.add_char b (Char.chr (m lor n))
  else if n < 65536 then (
    Buffer.add_char b (Char.chr (m lor 126));
    Buffer.add_uint16_be b n)
  else (
    Buffer.add_char b (Char.chr (m lor 127));
    Buffer.add_int64_be b (Int64.of_int n));
  if c.client then (
    let key = Mirage_crypto_rng.generate 4 in
    Buffer.add_string b key;
    Buffer.add_string b (mask key data))
  else Buffer.add_string b data;
  Eio.Mutex.use_rw ~protect:true c.lock (fun () -> c.write (Buffer.contents b))

(* One text frame out. *)
let text c s = send c 0x1 s

(* A close frame out, once, and the connection shut. Never fails. *)
let close ?(code = 1000) c =
  (try
     if not c.closing then (
       c.closing <- true;
       let b = Bytes.create 2 in
       Bytes.set_uint16_be b 0 code;
       send c 0x8 (Bytes.to_string b))
   with _ -> ());
  try c.shutdown () with _ -> ()

(* The next message in: a ping is answered on the way, and fragments put together. *)
let recv c =
  let rec go parts op size =
    let fin, code, data = frame c in
    match (code, op) with
    | 0x9, _ ->
        send c 0xA data;
        go parts op size
    | 0xA, None -> Pong data
    | 0xA, Some _ -> go parts op size
    | 0x8, _ ->
        let status = if String.length data >= 2 then big_endian (String.sub data 0 2) else 1005 in
        let reason = if String.length data > 2 then String.sub data 2 (String.length data - 2) else "" in
        (try
           if not c.closing then (
             c.closing <- true;
             send c 0x8 (String.sub data 0 (min 2 (String.length data))))
         with _ -> ());
        Closed (status, reason)
    | (0x1 | 0x2), None when fin -> if code = 0x1 then Text data else Binary data
    | (0x1 | 0x2), None -> go [ data ] (Some code) (String.length data)
    | 0x0, Some op ->
        let size = size + String.length data in
        if size > c.max then raise (Protocol "a message larger than allowed");
        if fin then
          let msg = String.concat "" (List.rev (data :: parts)) in
          if op = 0x1 then Text msg else Binary msg
        else go (data :: parts) (Some op) size
    | _ -> raise (Protocol "a frame out of place")
  in
  go [] None 0

(* Handshakes
   ---------- *)

(* An HTTP head off r: its lines up to the blank one, 16 KiB at most. *)
let read_head r =
  let rec go acc size =
    let l = Eio.Buf_read.line r in
    let size = size + String.length l + 2 in
    if size > 16384 then raise (Protocol "an HTTP head larger than 16 KiB")
    else if l = "" then String.concat "\r\n" (List.rev acc)
    else go (l :: acc) size
  in
  go [] 0

(* The server's end: a client's upgrade, read and answered. *)
let serve ~r flow =
  let key = Core.header (read_head r) "sec-websocket-key" in
  if key = "" then raise (Protocol "no Sec-WebSocket-Key");
  Eio.Flow.copy_string
    ("HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: "
   ^ accept key ^ "\r\n\r\n")
    flow;
  make ~client:false ~r flow

(* The client's end: an upgrade to path on host, asked and checked. *)
let connect ~r ~host ~path flow =
  let key = Base64.encode_string (Mirage_crypto_rng.generate 16) in
  Eio.Flow.copy_string
    ("GET " ^ path ^ " HTTP/1.1\r\nHost: " ^ host
   ^ "\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: " ^ key
   ^ "\r\nSec-WebSocket-Version: 13\r\n\r\n")
    flow;
  let head = read_head r in
  let status = fst (Core.cut head '\r') in
  if not (List.mem "101" (String.split_on_char ' ' status)) then raise (Protocol ("the upgrade was refused: " ^ status));
  if Core.header head "sec-websocket-accept" <> accept key then raise (Protocol "a wrong Sec-WebSocket-Accept");
  make ~client:true ~r flow
