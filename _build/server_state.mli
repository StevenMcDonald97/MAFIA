(** 
   Representation of dynamic game state.
   This module represents the state of a current game, including 
   the list of players, which player are mafia and which are town, 
   who has been killed, what stage of the game it is, and functions 
   the cause the state to change
*)

(** The abstract type representing the game stage. *)
(* type stage = Killing | Investigating | Protecting | Supporting 
           | Accusing | Defending | Voting | Neap *)
type stage = Killing | Accusing | Voting 



(** The abstract type of values representing the game state. *)
type t

(**[string_to_killed_tonight] is the OCaml record representation of the 
   json representatino of killed_tonight string*)
val string_to_killed_tonight: string * Yojson.Basic.json ->  Server_player.role * Server_player.t 

(**[string_to_killed_tonight] is the OCaml record representation of the
   json representation of state *)
val state_of_json: Yojson.Basic.json -> t

val json_of_state: t -> string

(** [get_vig_guilt st] gets [vig_guilt] of [st]. *)
(* val get_vig_guilt: t -> int *)

(** [change_vig_guilt st] changes [vig_guilt] of [st]. *)
(* val change_vig_guilt: t -> unit *)

(** [get_mafia st] is list of current players who are mafia. *)
val get_mafia : Server_player.t list -> Server_player.t list

(** [get_innocents st] is the list of current players who are innocents. *)
val get_town : Server_player.t list -> Server_player.t list 

(** [get_group players role lst] is a list of [players] of [role] appended to 
    [lst]. *)
val get_group : Server_player.t list -> Server_player.role -> Server_player.t list -> Server_player.t list

(** [init_state game] is a starting point for the game *)
val init_state : Server_player.t list-> Server_player.t list -> t

(** [current_players st] is the list of players in [st]*)
val current_players : t-> Server_player.t list

(** [current_mafia st] is the list of mafia in [st]*)
val current_mafia : t-> Server_player.t list

(** [current_town st] is the list of townfolk in [st]*)
val current_town : t-> Server_player.t list

(** [current_neutral st] is the list of neutrals in [st]*)
(* val current_neutral : t-> Server_player.t list *)

(**[role_helper role lst acc] is the list of players in [lst] of [role]
   appended to [acc]*)
val role_helper : Server_player.role -> Server_player.t list -> Server_player.t list -> Server_player.t list
(* 
(**[current_sheriffs st] is the list of players who are sheriffs in [st]. *)
val current_sheriffs : t -> Server_player.t list

(**[current_vigilantes st] is the list of players who are vigilantes in [st]. *)
val current_vigilantes : t -> Server_player.t list

(**[current_doctors st] is the list of players who are docotors in [st]. *)
val current_doctors : t -> Server_player.t list

(**[current_retributionists st] is the list of players who are retributionists 
   in [st]. *)
val current_retributionists : t -> Server_player.t list

(** [current_neapolitans st] is the list of players who are neapolitans in [st].
*)
val current_neapolitans : t -> Server_player.t list *)

(** [current_dead st] is the list of killed players in [st]*)
val current_dead : t-> Server_player.t list

(** [current_player st] is the current player id in [st]*)
(* val current_player : t-> Server_player.t  *)

(** [current_stage st] is the current stage in [st]*)
val current_stage : t-> stage

(** [current_night st] is the current night in [st]*)
val current_night : t-> int

(** [get_killed_tonight st] is an associative list of everyone killed 
    and who killed them during the current night*)
val get_killed_tonight : t-> (Server_player.role* Server_player.t) list

(** [add_to_killed st player] adds [player] to the dead list in [st]*)
val add_to_killed : t -> Server_player.t -> unit

(** [add_to_killed_tonight st player role] adds ([player],[role]) to 
    the killed tonight list in [st]. *)
val add_to_killed_tonight: t -> Server_player.t -> Server_player.role -> unit

(** [remove_from_killed_tonight st player] removes ([player],_) from
    the killed tonight list in [st]. *)
val remove_from_killed_tonight: t -> Server_player.t -> unit

(** [clear_killed_tonight st] clear the killed tonight list in [st]. *)
val clear_killed_tonight: t -> unit

(** [change_stage st time] changes the stage in [st] to [time]*)
val change_stage : t -> stage -> unit

(** [add_night st] adds one to the night_count in [st]*)
val add_night : t -> unit

(** [is_player player st] is true if [player] is the name of a 
    current player in state [st]*)
val is_player : string -> t -> bool

(** [is_dead player st] is true if [player] is the name of a 
    dead player in state [st]*)
val is_dead : string -> t -> bool

(**[move_player_to_killed st player] changes state to remove [player] 
   from the players, mafia, and innocent fields in [st] and add them to
   the dead field
*)
val move_player_to_killed : t -> Server_player.t -> unit

(** [revive_player_from_killed st player] changes [st] to put [player] back
    into game. *)
val revive_player_from_killed : t -> Server_player.t -> unit

(** [get_player player st] is the player *)
val get_player : t -> string -> Server_player.t

(** [get_messages st] is the message queue in [st]*)
val get_messages : t -> string list

(** [add_to_messages st message] adds [message] to the message queue in [st]*)
val add_to_messages : t -> string list-> unit

(** [clear_messages st] clears the message queue in [st]. *)
val clear_messages : t-> unit

(**[is_active player stage] is true is [player]'s role can act during 
   [stage]*)
val is_active : Server_player.t -> stage -> bool

(** [add_day st] increments the day_count field in [state] by 1*)
val add_day : t -> unit

(** [get_day st] is the current day_count in [state]*)
val get_day : t -> int

(** [reset_day st] sets the day_count field in [state] to 0*)
val reset_day : t -> unit

(** [change_accused st player] changes the accused field in [st] to [player]*)
val change_accused : t -> Server_player.t -> unit

(** [get_accused st] is the current accused in [state]*)
val get_accused : t -> Server_player.t

