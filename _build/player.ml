type side = Town | Mafia | Neutral 

type role = 
  | Sheriff of side
  (* in the future - store accused, track who they know is innocent or not *)
  | Citizen of side 
  | Mafioso of side
  | SerialKiller of side 
  | Vigilante of side
  | Doctor of side
  | Retributionist of side
  | Neapolitan of side

type status = Alive | Dead

type charge = Int of int | Unlimited

(** The type [doubt] represents how suspicious a player is. *)
type doubt = {
  pname:string;
  mutable d:int
}

(** The type of player with a [name], [role], [status], [suspects], [innocents],
    [attacked], [defended], [doubtlst], [charge]. *)
type t = {
  name:string; 
  role:role; 
  mutable status:status;
  mutable suspects: t list;
  mutable innocents: t list;
  mutable attacked: t list;
  mutable defended: t list;
  mutable doubtlst: doubt list;
  mutable charge: charge;
  (* mutable voted: (t * string) list *)
  (* suspects- people who have reaosn to seem guilty
      inncoents - seem innocent/ sheriff knows they're innocent
      attacked - who they've leveled attack agaings
      defended  - who they've defended
      voted - tuple list of player names and how they voted on them*)
}

exception WrongSide of role
exception Invalid_role
exception Invalid_player
exception WrongCharge of role

(** Initialzie random to use the default seed *)
let _ = Random.self_init()

let get_role player = player.role

let get_charge player = player.charge

let charge_value charge = 
  match charge with 
  | Int i -> i
  | Unlimited -> max_int

let rec get_player name players = 
  match players with 
  | [] -> raise Invalid_player
  | h::t -> if h.name = name then h else get_player name t

let is_town player = 
  let role = get_role player in
  match get_role player with
  | Sheriff Town -> true
  | Citizen Town -> true
  | Mafioso Mafia -> false
  | SerialKiller Neutral -> false
  | Vigilante Town -> true
  | Doctor Town -> true
  | Retributionist Town -> true
  | Neapolitan Town -> true
  | _ -> raise (WrongSide role)

let is_neutral player =
  let role = get_role player in
  match get_role player with
  | Sheriff Town -> false
  | Citizen Town -> false
  | Mafioso Mafia -> false
  | SerialKiller Neutral -> true
  | Vigilante Town -> false
  | Doctor Town -> false
  | Retributionist Town -> false
  | Neapolitan Town -> false
  | _ -> raise (WrongSide role)

let get_name player = player.name

let get_status player = player.status

let change_status player =
  match get_status player with 
  | Alive -> player.status <- Dead
  | Dead -> player.status <- Alive

let change_charge player = 
  match get_charge player with 
  | Int i when i = 0 -> raise (WrongCharge (get_role player))
  | Int i -> player.charge <- Int (i-1)
  | Unlimited -> player.charge <- Unlimited

let check_role role = 
  match role with 
  | Sheriff Town -> Sheriff Town
  | Citizen Town -> Citizen Town
  | Mafioso Mafia -> Mafioso Mafia 
  | SerialKiller Neutral -> SerialKiller Neutral
  | Vigilante Town -> Vigilante Town
  | Doctor Town -> Doctor Town
  | Retributionist Town -> Retributionist Town
  | Neapolitan Town -> Neapolitan Town
  | _ -> raise (WrongSide role)

let description role = 
  match check_role role with
  | Sheriff _ -> "The Sheriff can investigate a person every night to see if "^
                 "that person is suspicious (Mafia) or not suspicious (Town)."
  | Citizen _ -> "The Citizen has no abilities."
  | Mafioso _ -> "The Mafioso can vote to kill a person every night."
  | SerialKiller _ -> "The Serial Killer can attack a person every night."
  | Vigilante _ -> "The Vigilante can shoot a total of three times. "^
                   "The Vigilante will die from guilt the next night if he "^
                   "shoots a Town."
  | Doctor _ -> "The Doctor can heal a person every night, but not self."
  | Retributionist _ -> "The Retributionist can revive a dead Town once per game."
  | Neapolitan _ -> "The Neapolitan can reveal whether someone is a Townie."

(** [create_charge role] creates the correct [charge] for [role]. *)
let create_charge role = 
  match role with 
  | Sheriff Town -> Unlimited
  | Citizen Town -> Unlimited
  | Mafioso Mafia -> Unlimited
  | SerialKiller Neutral -> Unlimited
  | Vigilante Town -> Int 3
  | Doctor Town -> Unlimited
  | Retributionist Town -> Int 1
  | Neapolitan Town -> Unlimited
  | _ -> raise (WrongCharge role)

let make_player name role = 
  {
    name = name; 
    role = check_role role; 
    status = Alive;
    suspects = [];
    innocents = [];
    attacked = [];
    defended = [];
    doubtlst = [];
    charge = create_charge role;
  }


(** [possible_names] is a list of possible names for bots. *)
let possible_names = [
  "steven";
  "leo";
  "lucy";
  "minh";
  "lonny";  
  "bradford";  
  "joy";  
  "blake";  
  "james"; 
  "zana";  
  "birgit";  
  "functor-man"; 
  "raleigh"; 
  "basil"; 
  "that-freshman";  
  "dorothea";  
  "suzanna";  
  "janine";  
  "zachary";  
  "jane";  
  "higher-order-funk";  
  "dawne";  
  "craig";  
  "micaela";  
  "professor-clarkson";  
  "ashanti";  
  "lucienne";  
  "june";  
  "mellie";  
  "ima";  
  "patty";  
  "angelica";  
  "korey";  
  "edra";
  "david-gries'-twin-brother" 
] 

let get_rnd_name n= 
  List.nth possible_names n

let get_rnd_role n= 
  match n with
  | 0 -> Mafioso Mafia
  | 1 -> Citizen Town
  | 2 -> Sheriff Town
  | 3 -> SerialKiller Neutral
  | 4 -> Vigilante Town
  | 5 -> Doctor Town
  | 6 -> Retributionist Town
  | 7 -> Neapolitan Town
  | _ -> failwith "Error in rnd_role"

let role_to_string rl =
  match rl with
  | Mafioso Mafia -> "Mafioso"
  | Citizen Town -> "Citizen"
  | Sheriff Town -> "Sheriff"
  | SerialKiller Neutral -> "Serial Killer"
  | Vigilante Town -> "Vigilante"
  | Doctor Town -> "Doctor"
  | Retributionist Town -> "Retributionist"
  | Neapolitan Town -> "Neapolitan"
  | _ -> raise Invalid_role

let string_to_role str =
  match String.lowercase_ascii str with
  | s when s="mafioso" -> Mafioso Mafia 
  | s when s="citizen" -> Citizen Town
  | s when s="sheriff" -> Sheriff Town 
  | s when s="serial killer" -> SerialKiller Neutral
  | s when s="vigilante" -> Vigilante Town 
  | s when s="doctor" -> Doctor Town 
  | s when s="retributionist" -> Retributionist Town 
  | s when s="neapolitan" -> Neapolitan Town
  | _ -> raise Invalid_role


let make_random_player n role =
  let nm= get_rnd_name (Random.int n) in
  (make_player nm role) 

let rec name_used nm_1 lst = 
  match lst with
  | [] -> false
  | {name=nm_2; _}::t when nm_1=nm_2 -> true
  | h::t -> name_used nm_1 t

(** [generate_role role num plyrs] creates a list of [num] random players of 
    role [role] and appends them to [plyrs] without repeat names. *)
let rec generate_role role num plyrs=
  match num with 
  | 0-> plyrs
  | i ->
    let new_player = make_random_player (List.length possible_names) (role)
    in if name_used (get_name new_player) plyrs then
      generate_role role num plyrs
    else
      generate_role role (num-1) (List.append plyrs [new_player])

let rec generate_players m_count c_count s_count sk_count v_count d_count r_count 
    n_count =
  let mafia = generate_role (Mafioso Mafia) m_count [] in
  let citizen = generate_role (Citizen Town) c_count mafia in
  let sheriffs = generate_role (Sheriff Town) s_count citizen in
  let retributionist = generate_role (Retributionist Town) sk_count sheriffs in
  let vigilante = generate_role (Vigilante Town) v_count retributionist in
  let serial_killer = generate_role (SerialKiller Neutral) d_count vigilante in
  let neapolitan = generate_role (Neapolitan Town) r_count serial_killer in
  let doctor = generate_role (Doctor Town) n_count neapolitan in
  doctor 


(* let rec generate_random_players count lst role=
   let rl = 
    begin match role with
      |"Mafioso"-> Mafioso Mafia
      |"Citizen"-> Citizen Town
      |"Sheriff"-> Sheriff Town
      |_ -> failwith "Invalid role"
    end in
   match count with
   | 0-> lst
   | i when i>0 -> let nm= get_rnd_name (Random.int ((List.length lst)-1)) in
    let player = (make_player nm rl) in
    if List.mem player lst then generate_random_players count lst role else
      generate_random_players (count-1) (List.append [player] lst) role
   | _ ->failwith "count cannot be negative" *)


let rec get_player_names plyrs str= 
  match plyrs with
  |[]-> str
  | {name=c;_}::t -> get_player_names t str^", "^c


let get_random_player plyrs =
  let num_players = List.length plyrs in 
  List.nth plyrs (Random.int num_players)

let rec get_doubt_helper doubted (lst:doubt list) : int=
  match lst with
  | [] -> 0
  | h::t -> if h.pname = doubted then h.d else get_doubt_helper doubted t

let get_doubt player doubted = get_doubt_helper doubted player.doubtlst

(** [change_doubt_helper lst doubted dval] is a helper to change doubt. *)
let rec change_doubt_helper lst doubted dval = 
  match lst with
  | [] -> ()
  | h::t -> if h.pname = doubted then h.d<-h.d+dval else change_doubt_helper t 
        doubted dval

let change_doubt player doubted dval = change_doubt_helper player.doubtlst 
    doubted dval 

let rec set_doubt_helper lst doubted dval = 
  match lst with
  | [] -> ()
  | h::t -> if h.pname = doubted then h.d<-dval else set_doubt_helper t doubted dval

let set_doubt player doubted dval = set_doubt_helper player.doubtlst doubted dval

let add_doubt player doubted dval = player.doubtlst<-player.doubtlst@
                                                     [{pname=doubted;d=dval}]

(* helper function to add people to the supects list *)
let add_to_suspects player suspect = player.suspects<- player.suspects@[suspect]

(* helper function to tell if a player suspects another *)
let is_a_suspect player suspect= (get_doubt player (get_name suspect))>=4

(* helper function to add people to the innocents list *)
let add_to_innocents player suspect = player.innocents<- player.innocents@[suspect]

(* helper function to tell if a player innocents another *)
let is_innocent player suspect= (get_doubt player (get_name suspect))<4
