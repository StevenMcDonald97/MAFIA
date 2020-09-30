(** Module to keep track of the state of the game- who is dead, alive, 
    what actions can be taken *)
open Server_player
open Yojson
open Yojson.Basic.Util

(* type representing current game stage *)
(* type stage = Killing | Investigating | Protecting | Supporting 
           | Accusing | Defending | Voting | Neap *)
type stage = Killing | Accusing | Voting 

(* type representing the current game state *)
(*players represents living players*)
(*mafia represents living mafia*)
(*innocents represents living citizens*)
(*dead represents players who have been killed*)
(*time represents current stage*)
(*curr_player represents current player*)
(*night_count represents number of nights elapsed*)
(*killed_tonight represents players killed during the night*)
type t = {
  mutable players : Server_player.t list; 
  mutable mafia : Server_player.t list; 
  mutable town : Server_player.t list; 
  (* mutable neutral : Server_player.t list; *)
  mutable dead : Server_player.t list; 
  mutable time : stage; 
  (* curr_player : Server_player.t; *)
  mutable night_count : int;
  mutable killed_tonight : (role * Server_player.t ) list;
  mutable messages : string list;
  (* mutable vig_guilt: int;
  mutable retributionist_act: bool; *)
  mutable day_count: int;
  mutable accused : Server_player.t
}

exception Non_existent

let string_to_stage = function
  | s when s = "Killing" -> Killing 
  (* | s when s = "Investigating" -> Investigating
  | s when s = "Protecting" -> Protecting
  | s when s = "Supporting" -> Supporting  *)
  | s when s =  "Accusing" -> Accusing
  (* | s when s = "Defending" ->  Defending  *)
  | s when s = "Voting" -> Voting 
  (* | s when s = "Neap" -> Neap  *)
  | _ -> failwith "Invalid stage"

let string_of_stage = function 
  | Killing -> "Killing"
  (* | Investigating -> "Investigating"
  | Protecting -> "Protecting"
  | Supporting -> "Supporting" *)
  | Accusing -> "Accusing"
  (* | Defending -> "Defending" *)
  | Voting -> "Voting"
  (* | Neap -> "Neap" *)

(*let k = Yojson.Basic.from_string "{[\"bro\": \"ski\"]}"
  let p = Yojson.Basic.Util.to_assoc k*)


(*converts json*string tuple to Server_player.t * role tuple*)
let string_to_killed_tonight (a: string * Yojson.Basic.json) = 
  let x, y = a in
  (string_to_role x), (player_of_json y) 

let state_of_json json =
  {
    players = json |> member "players" |> to_list |> List.map player_of_json;
    mafia = json |> member "mafia" |> to_list |> List.map player_of_json;
    town = json |> member "town" |> to_list |> List.map player_of_json;
    (* neutral = json |> member "neutral" |> to_list |> List.map player_of_json; *)
    dead = json |> member "dead" |> to_list |> List.map player_of_json;
    time = json |> member "time" |> to_string |> string_to_stage; 
    (* curr_player = json |> member "curr_player" |> player_of_json; *)
    night_count = json |> member "night_count"|> to_int;
    killed_tonight = if (List.length (to_list (member "killed_tonight" json)) > 0) 
      then (List.map string_to_killed_tonight (to_assoc (member "killed_tonight" json))) else []; 
    messages = json |> member "messages" |> to_list |> List.map to_string;
    (* vig_guilt = json |> member "vig_guilt" |> to_int; *)
    (* retributionist_act = json |> member "retributionist_act" |> to_bool; *)
    day_count = json |> member "day_count" |> to_int;
    accused = json |> member "accused" |> player_of_json
  }

let killed_tonight_to_string (a: role * Server_player.t ) = 
  let x, y = a in
  "{["^(string_of_role x)^", " ^(json_of_player y)^"]}" 

(*takes in list of player jsons and stitches them into a string list of jsons*)
let concat_players (l:string list) = 
  let rec helper (l: string list) (acc: string) = 
    match l with 
    |h::t when t <> [] ->  helper t (acc^ h ^", ") 
    |h::t when t = [] -> (acc^h) 
    |_ -> acc in
  "["^(helper l "")^"]" 

let json_of_state (p: t) = "{ \"players\":" ^ (concat_players (List.map json_of_player p.players ))^
                           ", \"mafia\": " ^ (concat_players (List.map json_of_player p.mafia ))^
                           ", \"town\": " ^ (concat_players (List.map json_of_player p.town ))^
                           (* ", \"neutral\": " ^ (concat_players (List.map json_of_player p.neutral ))^ *)
                           ", \"dead\": " ^ (concat_players (List.map json_of_player p.dead ))^
                           ", \"time\": \"" ^string_of_stage p.time ^ 
                           (* "\", \"curr_player\": " ^json_of_player p.curr_player ^ *)
                           "\", \"night_count\": " ^string_of_int p.night_count ^
                           ", \"killed_tonight\":" ^ concat_players (List.map killed_tonight_to_string p.killed_tonight)  ^
                           ", \"messages\": " ^concat_players p.messages ^
                           (* ", \"vig_guilt\": " ^string_of_int p.vig_guilt ^ *)
                           (* ", \"retributionist_act\": " ^string_of_bool p.retributionist_act ^ *)
                           ", \"day_count\": " ^string_of_int p.day_count ^
                           ", \"accused\": " ^json_of_player p.accused ^ "}"


(* let get_vig_guilt st = st.vig_guilt *)

(* let change_vig_guilt st = st.vig_guilt <- (st.night_count+1) *)

let rec get_mafia players : Server_player.t list= 
  match players with
  | []-> []
  | h::t when is_town h-> get_mafia t
  | h::t when is_neutral h-> get_mafia t
  | h::t -> List.append [h] (get_mafia t)

let rec get_town players = 
  match players with
  | []-> []
  | h::t when is_town h-> List.append [h] (get_town t)
  | h::t -> get_town t

let rec get_neutral players = 
  match players with 
  | [] -> []
  | h::t when is_neutral h -> List.append [h] (get_neutral t)
  | h::t -> get_neutral t

let rec get_group players role lst=
  match players with 
  | []->lst
  | h::t when get_role h = role -> get_group t role (List.append lst [h])
  | h::t-> get_group t role lst


let init_state players bots = {
  players = bots@players;
  mafia = (all_of_role players (Mafioso Mafia) [])@(all_of_role bots (Mafioso Mafia) []);
  town = (all_of_role players (Citizen Town) [])@(all_of_role bots (Citizen Town) []);
  dead = []; 
  time = Killing;
  night_count = 0;
  killed_tonight = [];
  messages = [];
  day_count = 0;
  accused = (List.nth bots 0);
}

let current_players st =
  st.players

let current_mafia st =
  st.mafia

let current_town st =
  st.town

(* let current_neutral st =
  st.neutral *)

let rec role_helper role lst acc =
  match lst with
  | []-> acc
  | h::t when get_role h = role -> role_helper role t acc@[h]
  | h::t -> role_helper role t acc

let current_sheriffs st = 
  role_helper (Sheriff Town) st.town []

let current_vigilantes st = 
  role_helper (Vigilante Town) st.town []

let current_doctors st = 
  role_helper (Doctor Town) st.town []

let current_retributionists st = 
  role_helper (Retributionist Town) st.town []

let current_neapolitans st = 
  role_helper (Neapolitan Town) st.town []

let current_dead st =
  st.dead

let current_stage st =
  st.time

(* let current_player st =
  st.curr_player *)

let current_night st =
  st.night_count

let get_killed_tonight st =
  st.killed_tonight

let add_to_killed st player = st.dead <- st.dead@[player]

let add_to_killed_tonight st player role = 
  if List.mem player (List.map (fun (a,b)->b) st.killed_tonight ) then
    ()
  else
    st.killed_tonight <- st.killed_tonight@[(role, player)]

let remove_from_killed_tonight st player = 
  st.killed_tonight <- List.filter (fun (x,y) -> (y<>player)) st.killed_tonight 
(*st.killed_tonight <- List.remove_assoc player st.killed_tonight*)

let clear_killed_tonight st = 
  st.killed_tonight <- []

let move_player_to_killed st player= 
  st.players <-List.filter  (fun x -> (x<>player) ) st.players;
  st.mafia <-List.filter  (fun x -> (x<>player) ) st.mafia;
  st.town <-List.filter  (fun x -> (x<>player) ) st.town;
  (* st.neutral <-List.filter  (fun x -> (x<>player) ) st.neutral; *)
  st.dead <- st.dead@[player]

let revive_player_from_killed st player = 
  st.players <- st.players@[player];
  st.town <- st.town@[player];
  st.dead <- List.filter (fun x -> x <> player) st.dead

let change_stage st time=
  st.time<-time

let add_night st =st.night_count<-(st.night_count+1)


(** [get_player_help name lst] returns the player in [lst]
    with name [name], or raises an exception if not found  *)
let rec get_player_help name lst =
  match lst with 
  | []-> raise Non_existent
  | h::t when (String.lowercase_ascii(get_name h))= name -> h
  | h::t -> get_player_help name t

let get_player st name = 
  get_player_help (String.lowercase_ascii name) st.players 

let is_player player st = 
  match get_player st player with
  | exception Non_existent-> false
  | n -> true

let is_dead player st =
  match get_player_help player st.dead with 
  | exception Non_existent -> false
  | n -> true

let get_messages st = st.messages

let add_to_messages st messages = 
  st.messages <- st.messages@messages

let clear_messages st = 
  st.messages <- []

let is_active player stage = 
  match stage with
  | Killing -> 
               (* (get_role player) = SerialKiller Neutral ||
               (get_role player) = Vigilante Town || *)
               (get_role player) = Mafioso Mafia
  (* | Investigating ->(get_role player) = Sheriff Town 
  | Neap -> (get_role player) = Neapolitan Town
  | Protecting -> (get_role player) = Doctor Town
  | Supporting -> (get_role player) = Retributionist Town  *)
  | Accusing -> true 
  (* | Defending -> true *)
  | Voting -> true

let add_day st =
  st.day_count <- st.day_count + 1

let get_day st = st.day_count 

let reset_day st =
  st.day_count <- 0

let get_accused st = st.accused

let change_accused st player = 
  st.accused <- player



