(lang dune 3.7)

(name bridge)

(generate_opam_files true)

(package
 (name bridge)
 (synopsis "A chat bridge of your own: Twitch and YouTube chat on remux's wire")
 (depends
  (ocaml (>= 5.1))
  eio
  eio_main
  tls
  tls-eio
  ca-certs
  x509
  cohttp-eio
  http
  uri
  domain-name
  ipaddr
  mirage-crypto-rng
  yojson
  digestif
  base64))
