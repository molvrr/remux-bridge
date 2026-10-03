(* The Integration: one platform the bridge reads from and posts to. Server runs
   any of them; see twitch/plugin.ml for one. conf is what setup makes of the
   platform's value in the [byo] table and the environment; keep is what its
   sender keeps between messages.
     name     its key in [byo], and its "platform" everywhere
     limit    the characters one message may have
     setup    its settings from its [byo] value, or why it cannot run
     channel  the channel its events carry, as remux's say and delete name it
     read     runs for good, handing each event to emit
     sends    whether it has what posting needs (tokens)
     fresh    what its sender starts with
     send     posts one message
     delete   takes one message down, by its id *)
module type S = sig
  type conf
  type keep

  val name : string
  val limit : int
  val setup : string -> (conf, string) result
  val channel : conf -> string
  val read : Net.t -> conf -> emit:(Core.chat_event -> unit) -> unit
  val sends : conf -> bool
  val fresh : keep
  val send : Net.t -> conf -> keep -> string -> keep * Core.outcome
  val delete : Net.t -> conf -> keep -> string -> keep * Core.outcome
end
