(* Module to keep track of command savailable to the player *)
open Player
type object_phrase = string list

type command = 
  | Describe of object_phrase
  | Rules
  | Quit
  | Living
  | Vote of object_phrase
  | Attack
  | Defend 
  | Pass
  | Accuse of object_phrase
  | Kill of object_phrase
  | Investigate of object_phrase
  | Heal of object_phrase
  | Revive of object_phrase
  | Check of object_phrase


exception Empty
exception Malformed

let rec remove_space (lst:string list) =
  match lst with
  | [] -> []
  | h::t when (h = "") -> remove_space t
  | h::t -> h :: remove_space t

let inner_match lst pattern1 pattern2 = 
  match lst with
  | [] -> pattern1
  | _ -> pattern2
let parse str=
  if str = "" then raise (Empty) else 
    let lst = List.map String.lowercase_ascii 
        (remove_space (String.split_on_char ' ' str)) in
    match lst with
    | h::t when (h = "describe") -> 
      begin match t with
        | h_2::[] -> 
          begin
            match string_to_role h_2 with 
            | exception Invalid_role -> raise (Malformed)
            | _ -> Describe [h_2]
          end
        | _ -> raise (Malformed)
      end
    | h::t when (h = "rules") -> 
      begin match t with
        | [] -> Rules
        | _ -> raise (Malformed) 
      end
    | h::t when (h="quit") -> 
      begin match t with
        | [] -> Quit
        | _ -> raise (Malformed) 
      end
    | h::t when (h = "living") -> 
      begin match t with
        | [] -> Living
        | _ -> raise (Malformed) 
      end  
    | h::t when (h = "vote") ->
      begin match t with
        | "yay"::[] -> Vote t
        | "nay"::[] -> Vote t
        | _  -> raise (Malformed)
      end
    | h::t when h = "yay" || h="nay"->
      begin match t with
        | [] -> Vote [h]
        | _  -> raise (Malformed)
      end
    | h::t when (h = "defend") ->
      begin match t with
        | [] -> Defend 
        | _  -> raise (Malformed)
      end
    | h::t when (h = "attack") ->
      begin match t with
        | [] -> Attack  
        | _  -> raise (Malformed)
      end
    | h::t when (h = "pass") ->
      begin match t with
        | [] -> Pass  
        | _  -> raise (Malformed)
      end
    | h::t when (h = "accuse") ->
      begin match t with
        | h_2::[] -> Accuse t
        | _  -> raise (Malformed)
      end
    | h::t when (h = "kill") -> 
      begin match t with
        | h_2::[] -> Kill t
        | _  -> raise (Malformed)
      end
    | h::t when (h="investigate") ->
      begin match t with 
        | h_2::[] -> Investigate t
        | _  -> raise (Malformed)
      end
    | h::t when (h="heal") ->
      begin match t with 
        | h_2::[] -> Heal t
        | _  -> raise (Malformed)
      end
    | h::t when (h="revive") ->
      begin match t with 
        | h_2::[] -> Revive t
        | _  -> raise (Malformed)
      end
    | h::t when (h="check") ->
      begin match t with 
        | h_2::[] -> Check t
        | _  -> raise (Malformed)
      end
    | _ -> raise (Malformed)