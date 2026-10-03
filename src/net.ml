(* What every integration's wiring shares: logging, the environment, HTTP answers
   as JSON, and TLS connections. *)
open Eio.Std

let log text = prerr_endline text

(* An environment variable, trimmed; "" when it is not set. *)
let getenv name = String.trim (Option.value (Sys.getenv_opt name) ~default:"")

(* Logged only with BRIDGE_DEBUG=1: why an integration is not reading. *)
let debug text = if getenv "BRIDGE_DEBUG" = "1" then log text

(* What the bridge reaches the world with. *)
type t = {
  net : [ `Generic | `Unix ] Eio.Net.ty r;
  clock : float Eio.Time.clock_ty r;
  http : Cohttp_eio.Client.t;
}

let now_ms t = int_of_float (Eio.Time.now t.clock *. 1000.)
let sleep_ms t ms = Eio.Time.sleep t.clock (float_of_int ms /. 1000.)

(* Why something failed, in a few words. *)
let why = function
  | Eio.Time.Timeout -> "timed out"
  | End_of_file -> "the connection closed"
  | Failure m -> m
  | e -> Printexc.to_string e

(* f's result, or why it failed. Cancellation is not a failure: it goes on up. *)
let attempt f = match f () with x -> Ok x | exception (Eio.Cancel.Cancelled _ as e) -> raise e | exception e -> Error (why e)

(* f, with whatever it fails with forgotten. *)
let quietly f = match f () with () -> () | exception (Eio.Cancel.Cancelled _ as e) -> raise e | exception _ -> ()

(* JSON
   ---- *)

let rec of_yojson : Yojson.Safe.t -> Core.j = function
  | `Null -> JNull
  | `Bool b -> JBool b
  | `Int i -> JNum (string_of_int i)
  | `Intlit s -> JNum s
  | `Float f -> JNum (Yojson.Safe.to_string (`Float f))
  | `String s -> JStr s
  | `List xs -> JArr (List.map of_yojson xs)
  | `Assoc kvs -> JObj (List.map (fun (k, v) -> (k, of_yojson v)) kvs)

(* Text (octets) as JSON; JNull when it is not JSON. *)
let json_of text = try of_yojson (Yojson.Safe.from_string text) with Yojson.Json_error _ -> JNull

(* TLS
   --- *)

let tls_config =
  lazy
    (let authenticator =
       match Ca_certs.authenticator () with
       | Ok a -> a
       | Error (`Msg m) -> failwith ("cannot load the system's CA certificates: " ^ m)
     in
     match Tls.Config.client ~authenticator () with Ok c -> c | Error (`Msg m) -> failwith ("TLS failed: " ^ m))

let host_name host = Result.to_option (Result.bind (Domain_name.of_string host) Domain_name.host)

(* flow, spoken over TLS to host, whose certificate is checked. *)
let tls host flow = Tls_eio.client_of_flow (Lazy.force tls_config) ?host:(host_name host) flow

(* A TCP connection to host:port, made within 10 s. *)
let connect t ~sw host port =
  Eio.Time.with_timeout_exn t.clock 10. (fun () ->
      match Eio.Net.getaddrinfo_stream ~service:(string_of_int port) t.net host with
      | [] -> failwith ("cannot resolve " ^ host)
      | addr :: _ -> Eio.Net.connect ~sw t.net addr)

(* A TLS connection to host:port, made within 10 s, and the TLS handshake in 10 more. *)
let connect_tls t ~sw host port =
  let tcp = connect t ~sw host port in
  Eio.Time.with_timeout_exn t.clock 10. (fun () -> tls host tcp)

let make (env : Eio_unix.Stdenv.base) =
  let https uri raw = tls (Option.value (Uri.host uri) ~default:"") raw in
  { net = env#net; clock = env#clock; http = Cohttp_eio.Client.make ~https:(Some https) env#net }

(* HTTP
   ---- *)

(* An answer: its status and its body as JSON. *)
type ans = { status : int; j : Core.j }

let fetch t meth url headers body =
  attempt (fun () ->
      Eio.Time.with_timeout_exn t.clock 30. @@ fun () ->
      Switch.run @@ fun sw ->
      let resp, rbody =
        Cohttp_eio.Client.call t.http ~sw ~headers:(Http.Header.of_list headers)
          ?body:(Option.map Cohttp_eio.Body.of_string body)
          meth (Uri.of_string url)
      in
      let text = Eio.Buf_read.(parse_exn take_all) rbody ~max_size:(16 * 1024 * 1024) in
      { status = Http.Status.to_int (Http.Response.status resp); j = json_of text })

let get t url headers = fetch t `GET url headers None

(* body is text (octets): JSON, or a form. *)
let post t url headers body = fetch t `POST url headers (Some body)

let delete t url headers = fetch t `DELETE url headers None

(* A streamed GET: each piece of a 200's body goes to each as it comes, and a body
   silent for idle seconds is given up. The answer's status; the body of any other
   is not read. *)
let stream t url headers ~idle each =
  attempt (fun () ->
      Switch.run @@ fun sw ->
      let resp, body =
        Eio.Time.with_timeout_exn t.clock 30. (fun () ->
            Cohttp_eio.Client.call t.http ~sw ~headers:(Http.Header.of_list headers) `GET (Uri.of_string url))
      in
      let status = Http.Status.to_int (Http.Response.status resp) in
      (if status = 200 then
         let buf = Cstruct.create 65536 in
         try
           while true do
             let n = Eio.Time.with_timeout_exn t.clock idle (fun () -> Eio.Flow.single_read body buf) in
             each (Cstruct.to_string ~len:n buf)
           done
         with End_of_file -> ());
      status)

let bearer token = [ ("authorization", "Bearer " ^ token) ]
let json_headers token = bearer token @ [ ("content-type", "application/json") ]
let form = [ ("content-type", "application/x-www-form-urlencoded") ]

(* A delete's answer: 204 (or any 2xx) took it down; else the API's error. *)
let deleted r id =
  match r with
  | Error why -> (0, Core.Refused ("the request failed: " ^ why))
  | Ok { status; j } -> (status, if status >= 200 && status < 300 then Core.Sent id else Core.api_failed status j)

(* How a send's POST ended: a 200 is read by sent; any other status is the API's error. *)
let posted r sent =
  match r with
  | Error why -> (0, Core.Refused ("the request failed: " ^ why))
  | Ok { status; j } -> (status, if status = 200 then sent j else Core.api_failed status j)
