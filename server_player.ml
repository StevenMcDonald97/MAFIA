open Yojson
open Yojson.Basic.Util

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

type charge = int

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
  mutable suspects: string list;
  mutable innocents: string list;
  mutable attacked: string list;
  mutable defended: string list;
  mutable doubtlst: doubt list;
  mutable charge: int;
  (* mutable voted: (t * string) list *)
  (* suspects- people who have reaosn to seem guilty
      inncoents - seem innocent/ sheriff knows they're innocent
      attacked - who they've leveled attack agaings
      defended  - who they've defended
      voted - tuple list of player names and how they voted on them*)
}

exception WrongSide of role
exception Invalid_role
exception Invalid_status
exception Invalid_player
exception WrongCharge of role

(*takes in string list and output string representation of the list*)

let string_of_role = function 
  | Sheriff Town  -> "Sheriff Town"
  | Citizen Town -> "Citizen Town"
  | Mafioso Mafia -> "Mafioso Mafia"
  | SerialKiller Neutral -> "SerialKiller Neutral"
  | Vigilante Town -> "Vigilante Town"
  | Doctor Town -> "Doctor Town"
  | Retributionist Town -> "Retributionist Town"
  | Neapolitan Town -> "Neapolitan Town"
  | _ -> raise Invalid_role


let string_of_status = function 
  | Alive -> "Alive"
  | Dead -> "Dead"

(*returns string representation of json representation of doubt*)
let string_of_doubt doubt = 
  "{"^ "\"pname\": \""^doubt.pname^"\", \"d\":"^ (string_of_int doubt.d)^ "}"

(*returns string representation of json representation of a doubt list*)
let string_of_list_of_doubt (l: doubt list): string= 
  let rec helper (l: doubt list) (acc: string) = 
    match l with 
    |h::t when t <> [] ->  helper t (acc^string_of_doubt h ^", " ) 
    |h::t when t = [] -> (acc^ string_of_doubt h) 
    |_ -> acc in
  "["^(helper l "")^"]" 

(*returns a string representation of a string list*)
let string_of_list (l: string list): string= 
  let rec helper (l: string list) (acc: string) = 
    match l with 
    |h::t when t <> [] ->  helper t (acc^"\"" ^ h ^ "\",") 
    |h::t when t = [] -> (acc^ "\"" ^ h ^ "\"") 
    |_ -> acc in
  "["^(helper l "")^"]" 

(*[json_of_player] returns a string representation of json representation of a player*)
let json_of_player (p: t) = "{\"name\":\""^p.name^"\", \"role\":\""^(string_of_role p.role)^"\",\"status\":\""
                            ^ (string_of_status p.status) ^
                            "\", \"suspects\":"^(string_of_list p.suspects)^
                            ", \"innocents\":"^(string_of_list p.innocents)^
                            ", \"attacked\":"^(string_of_list p.attacked)^
                            ", \"defended\":"^(string_of_list p.defended)^
                            ", \"doubtlst\":"^(string_of_list_of_doubt p.doubtlst )^
                            ", \"charge\":" ^(string_of_int (p.charge))^ "}"

let doubt_of_json json = {
  pname = json |> member "pname" |> to_string;
  d = json |> member "d" |> to_int; 
}

let string_to_status s = if s = "Alive" then Alive else Dead 

let string_to_role s = if s =  "Sheriff Town" then Sheriff Town else if s = "Citizen Town"
  then Citizen Town else if s = "Mafioso Mafia" then Mafioso Mafia else if s = "SerialKiller Neutral" 
  then SerialKiller Neutral else if s = "Vigilante Town"
  then Vigilante Town else if s = "Doctor Town" then Doctor Town else if s = "Retributionist Town"
  then Retributionist Town else  Neapolitan Town 


let player_of_json json = {
  name = json |> member "name" |> to_string;
  role = string_to_role (json |> member "role" |> to_string); 
  status = string_to_status (json |> member "status" |> to_string );
  suspects = json |> member "suspects" |> to_list |> List.map to_string;
  innocents  = json |> member "innocents" |>  to_list |> List.map to_string;
  attacked  = json |> member "attacked" |>  to_list |> List.map to_string;
  defended  = json |> member "defended" |>  to_list |> List.map to_string;
  doubtlst = json |> member "doubtlst" |> to_list |> List.map doubt_of_json;
  charge = json|> member "charge" |> to_int
}


let _ = Random.self_init()

let get_role player = player.role

let get_charge player = player.charge

let charge_value charge = charge

(* let rec get_player name players = 
  match players with 
  | [] -> raise Invalid_player
  | h::t -> if h.name = name then h else get_player name t *)

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
  (*match get_charge player with 
    | Int i when !i = 0 -> raise (WrongCharge (get_role player))
    | Int i -> player.charge <- Int (ref (!i-1))
    | Unlimited -> player.charge <- Unlimited*)
  if (get_charge player) = 0 then raise (WrongCharge (get_role player)) else 
  if (get_charge player) > 0 then player.charge <- (((player.charge) - 1)) else if (get_charge player) = -1 then 
    player.charge <- (((player.charge) - 1)) 


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
  | SerialKiller _ -> "The Serial Killer can attack a person every night. "^
                      "The Serial Killer is immune at night."
  | Vigilante _ -> "The Vigilante can shoot a total of three times. "^
                   "The Vigilante will die from guilt the next night if he "^
                   "shoots a Town."
  | Doctor _ -> "The Doctor can heal a person every night, but not self."
  | Retributionist _ -> "The Retributionist can revive a dead Town once per game."
  | Neapolitan _ -> "The Neapolitan can reveal whether someone is a Townie."

(** [create_charge role] creates the correct [charge] for [role]. *)
let create_charge role = 
  match role with 
  | Sheriff Town -> (-1)
  | Citizen Town -> (-1)
  | Mafioso Mafia -> (-1)
  | SerialKiller Neutral -> (-1)
  | Vigilante Town -> 3
  | Doctor Town -> (-1)
  | Retributionist Town -> 1
  | Neapolitan Town -> (-1)
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

let rec all_of_role players role acc =
  match players with
  | [] -> acc
  | h::t when get_role h = role-> all_of_role t role acc@[h]
  | h::t -> all_of_role t role acc


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

let rec generate_players m_count c_count =
(* s_count sk_count v_count d_count r_count n_count = *)
  let mafia = generate_role (Mafioso Mafia) m_count [] in
  let citizen = generate_role (Citizen Town) c_count mafia in 
  citizen
  (* let sheriffs = generate_role (Sheriff Town) s_count citizen in
  let retributionist = generate_role (Retributionist Town) sk_count sheriffs in
  let vigilante = generate_role (Vigilante Town) v_count retributionist in
  let serial_killer = generate_role (SerialKiller Neutral) d_count vigilante in
  let neapolitan = generate_role (Neapolitan Town) r_count serial_killer in
  let doctor = generate_role (Doctor Town) n_count neapolitan in
  doctor  *)


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

let get_doubtlst player = player.doubtlst

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
let add_to_suspects player (suspect:string) = player.suspects<- player.suspects@[suspect]

(* helper function to tell if a player suspects another *)
let is_a_suspect player (suspect:string) = (get_doubt player suspect >= 4)

(* helper function to add people to the innocents list *)
let add_to_innocents player (suspect:string) = player.innocents<- player.innocents@[suspect]

(* helper function to tell if a player innocents another *)
let is_innocent player (suspect:string) = (get_doubt player suspect)<4

