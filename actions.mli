(** Raised when there is a there is no majority. *)
exception NoMajority

(** Raised when it is not a person. *)
exception Not_a_person

(** Raised when it is not a command. *)
exception Not_a_command

(** [cast_vote vote mafia innocents accused] Takes argument string [vote], 
    list of mafia [mafia], innocents [innocents], and accused player [accused].
    For each bot, calculate whether they vote "yay". Tally total 
    (including player's vote) and return true if tally is more than half of all 
    living people. *)
val cast_vote : string -> Player.t list-> Player.t list-> Player.t -> 
  State.t -> bool

(** [mafia_kill innocents mafias st ?target] kills [target] if commanded to, else
    chooses a random player from [innocents] to kill.
    Updates [st]: the dead and killed tonight list is updated.
    Updates [player]: the status of the player is updated. *)
val mafia_kill: Player.t list -> Player.t list -> ?target:string -> State.t  
  -> unit

(** [serial_killer_kill players target st] kills [target] if it is supplied
    by the user. Otherwise selects random person from list of living players
    and kills them. 
    Updates [st]: the killed tonight list is updated.
    Updates [players]: the (living) player list is updated *)
val serial_killer_kill: Player.t list -> Player.t list -> ?target:string -> 
  State.t -> unit

(** [vigilante_kill target st] kills [target].
    Updates [st]: the killed tonight list is updated.
    Updates [player]: the status of the player is updated. *)
val vigilante_kill: string -> State.t -> unit

(** [doctor_heal ?target st] is true if [target] is a valid target
    or false if they are the player or on the wrong team. 
    As a side effect it heals [target] if commanded to, else chooses
    a random player to heal.
    Updates [st]: If [player] is in the killed tonight list, remove [player] from 
    killed tonight list.
    Updates [player]: If [player] is [Dead] then change status of [player]. *)
val doctor_heal: ?target:string -> State.t -> unit

(** [retributionist_revive town_dead ?target st] is true if [target]
    is a valid target, or false if they are the player or a mafioso.
    As a side effect it revives [target] if commanded to, else revives 
    a random Town from [town_dead] and updates [st] and
    [player] of the revived and reviver accordingly.
    Requires: [?target] to be a valid target in [town_dead]. *)
val retributionist_revive: Player.t list -> ?target:string -> 
  State.t -> unit 

(** [neapolitan_reveal players ?target st] reveals whether [target] is 
    an innocent if commanded to, else reveals whether a random citizen from 
    [current_players] is a Townie. Updates stack of messages, which is an
    attribute of state, to be printed at the end of each night.
    Requires: [?target] to be a valid target in [current_players]. *)
val neapolitan_reveal: Player.t list -> ?target:string -> State.t -> unit 

(** [accuse living st ?target] is true if [target] is not the player, a 
    living player, and is not mafioso if the player is mafioso, and is false 
    otherwise. As a sideffect it advances the player with the most accusations 
    to a vote. The current player will accuse [target] if [target] is given 
    and all other players will also accuse someone, although the Mafia cannot 
    accuse other Mafia members. Raises: [NoMajority] if there is a tie between
    the players with the most accusations. *)
val accuse: Player.t list -> ?target:string -> State.t  -> Player.t

(** [print_rules ()] prints out the rules of Mafia *)
val print_rules : unit -> unit

(** [Investigate living_players sheriffs st suspect] is true if [suspect]
    is not the player and is living, otherwsie it is false. 
    It mutates players, mafia, dead, and killed_tonight fields of st. 
    Specfically, it makes each living sheriff (including the player, if 
    applicable) randomly choose a suspect to investigate. If condemned, 
    the suspect is summarily killed and appropriate changes are made to 
    the fields mentioned above, e.g. all killed players are added to 
    killed_tonight, deaths are tallied, etc. 
*)
val investigate : ?suspect:string -> Player.t list -> Player.t list 
  ->  State.t -> unit

(**[defense def_att accused st] adds messages to the message queue in state
   [st] depending on the user ipnput. If Def_att is "defend", the player
   is prompted for a defense message, and then the bots add messages.
   If def_att is "attack", the player is prompted for an attack message,
   then bot messages are added, and if def_attack is "pass", then only 
   messages from the bots are added.  *)
val defense : Player.t -> State.t -> string -> unit

(** [create_bot_messages st accused] creates the bot messages for [accused]
    and adds the messages to [st]. *)
val create_bot_messages: State.t -> Player.t -> unit   