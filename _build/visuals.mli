(** [reset ()] sets autoreset to true and erases the screen. *)
val reset : unit->unit

(** [typing_color ()] changes the typing color to cyan, for player input. *)
val typing_color : unit -> unit

(** [not_typing_color ()] changes the text color to yellow, for game output. *)
val not_typing_color : unit -> unit

(** [graveyard_alive ()] changes the text color to magenta, for the graveyard,
    alive, and mafia list. *)
val graveyard_alive : unit -> unit

(** [mafia_lst ()] changes the text color to red, for displaying mafia role. *)
val mafia_lst : unit -> unit

(** [town_lst ()] changes the text color to green, for displaying town role. *)
val town_lst : unit -> unit

(** [sk_lst ()] changes the text color to blue, for displaying serial killer role. *)
val sk_lst : unit -> unit

(** [print_grave_maf_alive st] prints out the graveyard, alive, and mafia list. *)
val print_grave_maf_alive : State.t -> unit