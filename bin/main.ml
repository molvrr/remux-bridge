let () =
  (* A write to a client that left fails with EPIPE, rather than ending the bridge. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Mirage_crypto_rng_unix.use_default ();
  Random.self_init ();
  match Eio_main.run Bridge.Server.main with
  | () -> ()
  | exception Bridge.Server.Die (code, text) ->
      prerr_endline text;
      exit code
