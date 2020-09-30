(* This module controls the visuals of the game *)
open State
open Player

(** Initialize autoreset to false. *)
let _ = ANSITerminal.set_autoreset false; 
  ANSITerminal.(print_string [on_black] "")
let reset () = ANSITerminal.set_autoreset true; ANSITerminal.erase Screen; 
  ANSITerminal.(print_string [default;on_default] "")

let typing_color () = ANSITerminal.(print_string [cyan] "")
let not_typing_color () = ANSITerminal.(print_string [yellow] "")
let graveyard_alive () = ANSITerminal.(print_string [magenta] "")
let mafia_lst () = ANSITerminal.(print_string [red] "")
let town_lst () = ANSITerminal.(print_string [green] "")
let sk_lst () = ANSITerminal.(print_string [blue] "")


(*gotta make sure real player doesn't type in too long of a name, maybe create 
  restriction*)
let print_grave_maf_alive st =
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1;
  graveyard_alive ();
  print_string "GRAVEYARD";
  let current_maf = get_role (current_player st) = Mafioso Mafia in 
  if current_maf then 
    (ANSITerminal.set_cursor 76 1;
     print_string "MAFIA";) else ();
  ANSITerminal.set_cursor 126 1;
  print_string "ALIVE";
  let dead = current_dead st in 
  let mafia = current_mafia st in 
  let alive = (current_player st)::(current_players st) in 
  let rec helper dead ?mafia:(mafia=[]) alive count = 
    let new_count = count+1 in 
    match dead,mafia,alive with 
    | [],[],[] -> print_endline "\n"; not_typing_color ();
    | [],h'::t',h''::t'' -> 
      ANSITerminal.set_cursor 76 count; print_string (get_name h');
      ANSITerminal.set_cursor 126 count; print_string (get_name h'');
      helper [] ~mafia:t' t'' new_count
    | [],[],h''::t'' -> 
      ANSITerminal.set_cursor 126 count; print_string (get_name h'');
      helper [] t'' new_count
    | [],h'::t',[] -> 
      ANSITerminal.set_cursor 76 count; print_string (get_name h');
      helper [] ~mafia:t' [] new_count
    | h::t,[],h''::t'' ->
      ANSITerminal.set_cursor 1 count; print_string 
        ((get_name h)^" ("^role_to_string(get_role h)^")");
      ANSITerminal.set_cursor 126 count; print_string (get_name h'');
      helper t t'' new_count
    | h::t,[],[] -> 
      ANSITerminal.set_cursor 1 count; print_string 
        ((get_name h)^" ("^role_to_string(get_role h)^")");
      helper t [] new_count
    | h::t,h'::t',[] ->
      ANSITerminal.set_cursor 1 count; print_string 
        ((get_name h)^" ("^role_to_string(get_role h)^")");
      ANSITerminal.set_cursor 76 count; print_string (get_name h');
      helper t ~mafia:t' [] new_count
    | h::t,h'::t',h''::t'' -> 
      ANSITerminal.set_cursor 1 count; print_string 
        ((get_name h)^" ("^role_to_string(get_role h)^")");
      ANSITerminal.set_cursor 76 count; print_string (get_name h');
      ANSITerminal.set_cursor 126 count; print_string (get_name h'');
      helper t ~mafia:t' t'' new_count in 
  if current_maf then helper dead ~mafia:mafia alive 2 else helper dead alive 
      2
