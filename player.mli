(** The type [side] represents whether a role is on the side of [Town], [Mafia], 
    or [Neutral]. *)
type side = Town | Mafia |Neutral

(** The abstract type of values representing a player. *)
type t

(** The type [role] represents a player's role, decomposed with what side they
    are on. *)
type role = 
  | Sheriff of side 
  | Citizen of side 
  | Mafioso of side
  | SerialKiller of side 
  | Vigilante of side
  | Doctor of side
  | Retributionist of side
  | Neapolitan of side


(** The type [status] represents whether a player is [Alive] or [Dead]. *)
type status = Alive | Dead 

(** The type [charge] represents the number of times the player can activate
    their ability. *)
type charge = Int of int | Unlimited

(** Raised when [role] is given the wrong [side]. *)
exception WrongSide of role

(** Raised when there is no such [role]. *)
exception Invalid_role

(** Raised when there is no such [player]. *)
exception Invalid_player

(** Raised when [role] is given the wrong [charge]. *)
exception WrongCharge of role

(** [get_name player] returns the [name] of [player]. *)
val get_name: t -> string

(** [get_role player] returns the [role] of [player]. *)
val get_role: t -> role

(** [get_charge player] returns the [charge] of [player]. *)
val get_charge: t -> charge

(** [charge_value charge] gets the value of [charge].
    Raises: [failwith "no value"] if charge has no numerical value. *)
val charge_value: charge -> int

(** [get_player name players] returns a player with [name] in [players].
    Raises: [Invalid_player] if no such [player] exists. *)
val get_player: string -> t list -> t

(** [is_town player] is true if [player] role has side [Town]. 
    Examples: If [role] was (Sheriff Town), (Citizen Town) then true
    If [role] was (Mafioso Mafia) then false
    Raises: [WrongSide role] if [role] has the wrong [side]. *)
val is_town: t -> bool

(** [is_neutral player] is true if [player] role has side [Neutral]. 
    Examples: If [role] was (Sheriff Town), (Citizen Town) then false
    If [role] was (Mafioso Mafia) then false
    If [role] was (SerialKiller Neutral) then true
    Raises: [WrongSide role] if [role] has the wrong [side]. *)
val is_neutral: t -> bool

(** [get_status player] returns the [status] of [player]. *)
val get_status: t -> status

(** [change_status player] changes the [status] of [player]. *)
val change_status: t -> unit

(** [change_charge player] changes the [charge] of [player].
    Raises: [WrongCharge role] if charge is already 0. *)
val change_charge: t -> unit

(** [check_role role] returns [role] if it is given the correct [side].
    Examples: If [role] was (Sheriff Town) then (Sheriff Town)
    If [role] was (Citizen Town) then (Citizen Town)
    If [role] was (Mafioso Mafia) then (Mafioso Mafia)
    Raises: [WrongSide role] if [role] isn't given the correct [side]. *)
val check_role: role -> role

(** [description role] returns the description of the abilities of a [role]. 
    Examples: If [role] was (Sheriff Town) then 
    "The Sheriff can investigate a person every night to see if that person is 
    suspicious (Mafia) or not suspicious (Town)."
*)
val description: role -> string

(** [make_player name role] returns a player with the given [name] and [role]. *)
val make_player: string -> role -> t

(** [get_rnd_name n ] is a random string representing a name. *)
val get_rnd_name: int->string

(** [get_rnd_role n] is a random role.
    Raises: [failwith "Error in rnd_role"] if [n] > numbers of [role]. *)
val get_rnd_role: int->role

(** [name_used nm_1 lst] evaluates true if [nm_1] is the name of a player 
    in [lst] and false otherwise. *)
val name_used : string -> t list -> bool

(** [make_random_player n role] is a player with role [role] and a random name. *)
val make_random_player :  int -> role -> t

(** [generate_players m_count c_count s_count sk_count v_count d_count r_count
    n_count] is a player list with [m_count] mafias, [c_count] citizens, 
    [s_count] sheriffs, [sk_count] serial killers, [v_count] vigilantes, 
    [d_count] doctors, [r_count] retributionists, [n_count] neapolitans. *)
val generate_players:  int -> int -> int -> int -> int -> int -> int -> 
  int -> t list

(* [generate_random_players count lst role] is a list with [count] number of 
    random players of role [role]
   val generate_random_players:  int -> t list -> string-> t list *)

(** [get_player_names plyrs str] is a string of the names of the players in 
    [plyrs] concatenated to [str]. *)
val get_player_names :  t list -> string -> string


(** [role_to_string rl] is a string representing role "role".
    Examples: If (Mafioso Mafia) then "Mafioso"
    If (Citizen Town) then "Citizen"
    If (Sheriff Town) then "Sheriff"
    Raises: [Invalid_role] if [rl] is not a valid [role]. *)
val role_to_string : role -> string

(** [string_to_role str] is a role defined by [str].
    Examples: If "mafioso" then (Mafioso Mafia)
    If "citizen" then (Citizen Town)
    If "sheriff" then (Sheriff Town)
    Raises: [Invalid_role] if [rl] is not a valid [role].*)
val string_to_role : string->role

(** [get_random_player plyrs] is a random player from player list [plyrs]. *)
val get_random_player : t list -> t

(** [add_to_suspects player suspect] add player [suspect] to [player]'s suspect 
    queue. *)
val add_to_suspects : t -> t -> unit

(** [is_a_suspect player suspect] is true if [suspect] is in [player]'s suspect 
    queue.*)
val is_a_suspect : t -> t-> bool

(** [add_to_innocents player suspect] adds player [suspect] to [player]'s 
    innocent queue. *)
val add_to_innocents : t -> t -> unit

(** [is_innocent player suspect] is true if [suspect] is in [player]'s innocent 
    queue. *)
val is_innocent : t -> t -> bool

(** [get_doubt player doubted] returns the doubt of [player] against [accused] *)
val get_doubt : t -> string -> int

(** [change_doubt player doubted dval] increases the doubt of [player] against 
    [accused] by d*)
val change_doubt: t ->  string -> int -> unit 

(** [set_doubt player doubted dval] sets the doubt of [player] against 
    [accused] to d*)
val set_doubt : t -> string -> int -> unit 

(** [add_doubt player doubted dval] adds [accused] to doubtlst of [player] and 
    sets the accompanying doubt value to d *)
val add_doubt : t -> string -> int -> unit
