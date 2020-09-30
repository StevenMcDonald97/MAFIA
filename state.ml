(** Module to keep track of the state of the game- who is dead, alive, 
    what actions can be taken *)
open Player
open Yojson
open Yojson.Basic.Util

(* type representing current game stage *)
type stage = Killing | Investigating | Protecting | Supporting 
           | Accusing | Defending | Voting | Neap

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
  mutable players : Player.t list; 
  mutable mafia : Player.t list; 
  mutable town : Player.t list; 
  mutable neutral : Player.t list;
  mutable dead : Player.t list; 
  mutable time : stage; 
  curr_player : Player.t;
  mutable night_count : int;
  mutable killed_tonight : (Player.t * role) list;
  mutable messages : string list;
  mutable vig_guilt: int;
  mutable retributionist_act: bool;
  mutable day_count: int;
  mutable accused : Player.t
}

exception Non_existent 

let get_vig_guilt st = st.vig_guilt

let change_vig_guilt st = st.vig_guilt <- (st.night_count+1)



let rec get_mafia players : Player.t list= 
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

(** [shuffle lst] is a random permutation of [lst]. *)
let shuffle (lst : 'a list) : 'a list =
  lst

let init_state player bots = {
  players = shuffle bots;
  mafia = get_group (List.append [player] bots) (Mafioso Mafia) [];
  town = 
    (get_group (List.append [player] bots) (Citizen Town) [])@
    (get_group (List.append [player] bots) (Sheriff Town) [])@
    (get_group (List.append [player] bots) (Vigilante Town) [])@
    (get_group (List.append [player] bots) (Doctor Town) [])@
    (get_group (List.append [player] bots) (Retributionist Town) [])@
    (get_group (List.append [player] bots) (Neapolitan Town) []);
  neutral = get_group (List.append [player] bots) (SerialKiller Neutral) [];
  dead = [];
  time = Killing;
  curr_player = player;
  night_count = 0;
  killed_tonight = [];
  messages = [];
  vig_guilt = (-1);
  retributionist_act = false;
  day_count = 0;
  accused = player;
}

let current_players st =
  st.players

let current_mafia st =
  st.mafia

let current_town st =
  st.town

let current_neutral st =
  st.neutral

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

let current_serialkillers st =
  role_helper (SerialKiller Neutral) st.neutral []

let current_dead st =
  st.dead

let current_stage st =
  st.time

let current_player st =
  st.curr_player

let current_night st =
  st.night_count

let get_killed_tonight st =
  st.killed_tonight

let add_to_killed st player = st.dead <- st.dead@[player]

let add_to_killed_tonight st player role = 
  if List.mem player (List.map (fun (a,b)->a) st.killed_tonight ) then
    ()
  else
    st.killed_tonight <- st.killed_tonight@[(player,role)]

let remove_from_killed_tonight st player = 
  (* st.killed_tonight <- List.filter (fun (x,y) -> (x<>player)) st.killed_tonight *)
  st.killed_tonight <- List.remove_assoc player st.killed_tonight

let clear_killed_tonight st = 
  st.killed_tonight <- []

let move_player_to_killed st player= 
  st.players <-List.filter  (fun x -> (x<>player) ) st.players;
  st.mafia <-List.filter  (fun x -> (x<>player) ) st.mafia;
  st.town <-List.filter  (fun x -> (x<>player) ) st.town;
  st.neutral <-List.filter  (fun x -> (x<>player) ) st.neutral;
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

let get_player name st = 
  get_player_help (String.lowercase_ascii name) st.players 

let is_player player st = 
  match get_player player st with
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

let get_accused st = st.accused

let change_accused st player = 
  st.accused <- player

let is_active player stage st = 
  match stage with
  | Killing -> (get_role player) = SerialKiller Neutral ||
               (get_role player) = Vigilante Town ||
               (get_role player) = Mafioso Mafia
  | Investigating ->(get_role player) = Sheriff Town 
  | Neap -> (get_role player) = Neapolitan Town
  | Protecting -> (get_role player) = Doctor Town
  | Supporting -> (get_role player) = Retributionist Town 
  | Accusing -> true 
  | Defending -> not((get_accused st) = (current_player st))
  | Voting -> not((get_accused st) = (current_player st))

let add_day st =
  st.day_count <- st.day_count + 1

let get_day st = st.day_count 

let reset_day st =
  st.day_count <- 0

