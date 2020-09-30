(** Raised when it is not a person. *)
exception Not_a_person

(** Raised when it is not a command. *)
exception Not_a_command

(** [cast_vote votes st] Takes list of player's votes [votes] 
    and current game state [st].
    For each bot, calculate whether they vote "yay". Tally total 
    (including players' votes) and return true if tally is more than half of all 
    living people. *)
val cast_vote : string list -> Server_state.t -> bool

(** [mafia_kill st ?target] kills [target] if commanded to, else
    chooses a random player from the town field in [st] to kill.
    Updates [st]: the dead and killed tonight list is updated.
    Updates [player]: the status of the player is updated. *)
val mafia_kill: ?targets:Server_player.t list -> Server_state.t -> unit

(** [accuse ?targets st] is either the head of targets, or if targets is empty
      a random player *)
val accuse: ?targets:Server_player.t list -> Server_state.t -> Server_player.t

(** [print_rules ()] prints out the rules of Mafia *)
val print_rules : unit -> unit