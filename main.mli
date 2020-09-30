(** 
   The main entry point for the game interface.
*)

exception Invalid_command  
exception Invalid_name
exception Invalid_player


(* (** [print_grave_maf_alive st] prints out the life of people in the Graveyard,
    alive Mafia (if current player is a member of the Mafia), and a list of all
    alive players. *)
   val print_grave_maf_alive: State.t -> unit *)

(** [retrieve_name n] takes in a function [n] mapping a string to a unit, and 
    returns [n] applied to the input from read_line()
    If reaad_line() raises [End_of_file], restart the game  *)
val retrieve_name : (string -> unit) -> unit

(** [print_message st] prints every message in the message queue of [st] *)
val print_messages : State.t -> unit

(** [end_of_game st] if st satisifies one of the end of game ocnditions, i.e. 
    the mafia outnumber the innocent, the mafia are all dead, or the play is 
    dead, a message is displayed to the player and the game exits gracefully.
    Raises [exception "end of game triggered prematurely"] if the player 
    somehow is alive but everyone of their group is dead *)
val end_of_game : State.t -> unit

(**[play st] triggers the actiosn for the current stage in [st]. 
    If the player's role can perform an action in the stage, it will
    prompt the player for a command. If they cannot, it will execute the
    functionality of the stage and continue to the next stage.*)

(** [start_game name] Displays a welcome message for the player,
    tells htem who the other town members are, and if the player
    is mafia also tells them who the other mafia are*)
val start_game :string-> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit