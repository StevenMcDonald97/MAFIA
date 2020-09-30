(* Here's where the basic game engine will be implemented. *)
open State
open Player 
open Commands  
open Actions  
open Visuals
open Server
open Client 

exception Invalid_command  
exception Invalid_name
exception Invalid_player

(** Initialzie random to use the default seed *)
let _ = Random.self_init()

(** [retrieve_name n] applies a function n to 
    user input*)
let rec retrieve_name n =
  typing_color ();
  match read_line() with
  | exception End_of_file -> reset (); ()
  | "" -> not_typing_color ();
    print_endline "\nYou can't have an empty name.";
    print_string "> ";
    retrieve_name n
  | name when String.length name > 25 -> not_typing_color ();
    print_endline "\nYou can't have more than 25 characters in your name.";
    print_string "> ";
    retrieve_name n;
  | name -> not_typing_color (); n name

(** [set_doubt1 pl pl_list st] set doubt on each other bot player for [pl]*)
let rec set_doubt1 pl pl_list st =
  match pl_list with
  | [] -> ()
  | h::t -> begin
      add_doubt pl (get_name h) 0;  
      (set_doubt1 pl t st);
    end

(** [set_doubt2 pl_list st] sets doubt *)
let rec set_doubt2 pl_list st =
  let lst = current_players st in
  match pl_list with
  | [] -> ()
  | h::t -> if (h<>current_player st) then begin
      set_doubt1 h lst st;
      set_doubt2 t st;
    end
    else set_doubt2 t st

(** specs, print_killed helper *)
(* let print_killer killer = 
   match killer with 
   | Sheriff Town | Vigilante Town -> town_lst (); print_string (role_to_string 
   killer);
    not_typing_color (); print_string ". "
   | Mafioso Mafia -> mafia_lst (); print_string (role_to_string killer);
    not_typing_color (); print_string ". "
   | SerialKiller Neutral -> sk_lst (); print_string (role_to_string killer);
    not_typing_color (); print_string ". "
   | _ -> raise (Invalid_role) *)

(**[print_killed st] prints the list of 
   people in the current killed_tonight field in [st] as 
   being killed by the corresponding role*)
let rec print_killed = function
  | [] -> not_typing_color (); print_endline ""
  | (victim, killer)::t -> 
    print_string ((get_name victim) ^ " was killed by the "); 
    begin
      match killer with
      | Sheriff Town | Vigilante Town -> town_lst ()
      | Mafioso Mafia -> mafia_lst ()
      | SerialKiller Neutral -> sk_lst ()
      | _ -> raise (Invalid_role)
    end;
    (* let rec print_killers lst = 
       match lst with 
       | [] -> ()
       | h::[] -> print_killer h
       | h::t -> print_killer h; print_string ((get_name victim) ^ " was also 
       killed by the ");
        print_killers t
       in 
       print_killers killer; *)
    print_string (role_to_string killer);
    not_typing_color ();
    print_string (". Their role was ");
    begin
      match get_role victim with 
      | Sheriff Town | Vigilante Town | Citizen Town | Doctor Town | Retributionist Town
      | Neapolitan Town -> town_lst ()
      | Mafioso Mafia -> mafia_lst ()
      | SerialKiller Neutral -> sk_lst ()
      | _ -> raise (Invalid_role)
    end;
    print_string (role_to_string(get_role victim));
    not_typing_color ();
    print_endline "."; 
    print_killed t

(** [print_messages_help messages] recursively prints the
    strings in [messages]  *)
let rec print_messages_help messages : unit=
  match messages with
  | [] -> ()
  | h::t -> not_typing_color (); print_endline h; print_messages_help t

(**[print_messages st] prints the messages in the current_message field
   of st *)
let print_messages st= 
  let messages = (get_messages st) in
  print_messages_help messages; clear_messages st

(** [killable side] creates a list of possible players that can be killed, depending
    on [side] and what night it is. *)
let killable side st = 
  match current_night st with 
  | 0 | 1 | 2 -> 
    if side = Mafia then List.filter 
        (fun x -> x<> (current_player st)) ((current_town st)@(current_neutral st))
    else List.filter (*neutral*)
        (fun x -> x<> (current_player st)) ((current_town st)@(current_mafia st)) 
  | i when i > 2 -> 
    if side = Mafia then (current_town st)@(current_neutral st)
    else (current_town st)@(current_mafia st) (*neutral*)
  | _ -> failwith "not valid night" 

(**[move_dead_to_killed dead st] moved the players in the killed_tonight field of [st]
   to the dead field in [st] *)
let rec move_dead_to_killed dead st=
  match dead with
  | []->()
  | (player, _)::t -> move_player_to_killed st player; move_dead_to_killed t st

(* check end of game conditions in [st] *)
let end_of_game st =
  (* check conditions for end of game, and print end message if they hold *)
  not_typing_color ();
  if get_status (current_player st) = Dead then 
    let () = print_endline "They found you! You've been killed." in 
    Pervasives.exit 0
  else if get_vig_guilt st = current_night st then 
    let () = print_endline "You died from guilt of killing a Town." in 
    Pervasives.exit 0
  else if (* List.length (current_town st) <  *)
    List.length (current_town st) = 0 && List.length (current_neutral st) = 0 
    && (get_role (current_player st)) = Mafioso Mafia then 
    let () =
      print_endline ("Congratulations! You outnumber the townspeople and neutrals."^
                     " The Mafia reign supreme.")
    in Pervasives.exit 0
  else if List.length (current_mafia st) = 0 && List.length (current_neutral st) = 0
          && is_town (current_player st) then 
    let () = print_endline 
        ("Congratulations! All of the mafia and neutrals have been caught. "^
         "It's safe to go out again.")
    in Pervasives.exit 0
  else if ((List.length (current_mafia st)) + (List.length (current_town st)) = 1 
           && is_neutral (current_player st)) then 
    let () =print_endline ("Congratulations! You have taken over the village. "^
                           "The sparse village brings joy to your heart.")
    in Pervasives.exit 0
  else if List.length (current_neutral st) = 0 && 
          (List.length (current_town st) = 0 || 
           List.length (current_mafia st)=0) 
  then
    (reset (); failwith "End of game triggered prematurely")
  else ()

(** [print_stage_prompt stage st accused] prints the message prompt for [stage] *)
let print_stage_prompt stage st accused= 
  not_typing_color ();
  match stage with
  | Killing -> end_of_game st;
    print_endline("Nighttime falls. Now is your chance to strike."); 
    print_endline ("\nType 'Kill' and a character's name whom you with to die,"^
                   " kill nobody, or another command."); ()
  | Neap -> print_endline("Nighttime falls. Whose identity do you wish to check?");
    print_endline ("\nType 'Check' and a character's name whom you wish to "^
                   "expose, check nobody, or another command."); ()
  | Investigating -> print_endline "Nighttime falls. Its time to investigate.";
    print_endline ("\nType 'Investigate' and a character's name whom you wish "^
                   "to investigate, investigate nobody, or another command.");  ()
  | Protecting -> print_endline ("Nighttime falls. You can choose a player to heal.");
    print_endline ("\nType 'Heal' and a character's name whom you wish to heal, "^
                   "heal nobody, or another command."); ()
  | Supporting -> print_endline("Nighttime falls. You can choose a player to revive."); 
    print_endline ("\nType 'Revive' and a character's name whom you wish to "^
                   "revive, revive nobody, or another command.")
  | Accusing -> 
    begin
      if get_day st = 0 then 
        (let () = print_endline "Day breaks. Some have been busy..." in 
         let () = print_killed (get_killed_tonight st) in
         clear_killed_tonight st; end_of_game st) else ();
      print_endline "Do you suspect anyone? Accuse Nobody if you have no suspects." 
    end
  (* else  *)
  (* let () = print_endline "Do you suspect anyone? Accuse Nobody if you 
     have no suspects." in () *)
  | Defending -> print_endline ((get_name accused)^" has been accused of being "^
                                "evil.\n"); 
    print_endline ("Would you like to Defend or Attack them? You may also "^
                   "choose to Pass"); ()
  | Voting ->  print_endline ("\n"^(get_name accused)^" has been accused of "^
                              "being evil. Do you vote yay or nay?"); ()

(** [valid_stage_command st command func] checks if [command] is valid for [stage]. *)
let valid_stage_command st command func = 
  let stage = current_stage st in 
  match parse command with 
  | Vote _ -> stage = Voting
  | Attack -> stage = Defending
  | Defend -> stage = Defending 
  | Pass -> stage = Defending
  | Accuse _ -> stage = Accusing
  | Kill _ -> stage = Killing
  | Investigate _ -> stage = Investigating
  | Heal _ -> stage = Protecting
  | Revive _ -> stage = Supporting
  | Check _ -> stage = Neap
  | _ -> false 

(** [basic_parse st] parses [command] to and acts on it if it is valid, else
    calls [func] on it. *)
let basic_parse st (command:string) func = 
  match parse command with 
  | exception Malformed -> not_typing_color (); 
    print_endline "\nThat's not allowed.";
    print_string "> ";
    typing_color ();
    func st (read_line ())
  | exception Empty -> not_typing_color ();
    print_endline "\nI didn't catch that.";
    print_string "> ";
    typing_color ();
    func st (read_line ())
  | Describe [role] -> not_typing_color (); 
    print_endline ("\n"^description (string_to_role role)); 
    print_string "> ";
    typing_color ();
    func st (read_line ())
  | Rules -> not_typing_color (); 
    print_endline "";
    print_rules ();
    print_string "> ";
    typing_color (); 
    func st (read_line ())
  | Quit -> not_typing_color ();
    print_endline "\nGoodbye."; 
    Pervasives.exit 0
  | _ -> (* () *)
    if valid_stage_command st command func then func st (read_line ())
    else 
      (print_endline "\nThat command is not valid.";
       print_string "> ";
       typing_color ();
       func st (read_line ()))


(** [parse_kill st command] Parse_kill proccesses the kill command during the 
    killing stage for mafia, vigilnates, and serial killers  *)
let rec parse_kill st (command:string) =
  match parse command with 
  | Kill [player] -> not_typing_color ();
    let player_role = (get_role (current_player st)) in 
    let () = 
      match player_role with 
      | Mafioso Mafia -> 
        mafia_kill (current_players st) (current_mafia st) ~target:player st;
        serial_killer_kill (killable Neutral st) (current_neutral st) st
      | Vigilante Town -> vigilante_kill player st; 
        serial_killer_kill (killable Neutral st) (current_neutral st) st; 
      | SerialKiller Neutral -> 
        serial_killer_kill (current_players st) (current_neutral st) ~target:player st;
        mafia_kill (killable Mafia st) (current_mafia st) st;
      | _ -> raise (Invalid_role) in
    if (let messages = get_messages st in 
        List.length messages <> 0 && String.sub (List.hd messages) 0 1 = "\n")
    then (print_messages st; 
          print_string "> ";
          typing_color ();
          parse_kill st (read_line ()))
    else change_stage st Neap
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_kill)
  | _ -> not_typing_color ();
    basic_parse st command (parse_kill)


(** [parse_neapolitan st command] parses [command] for neapolitan. *)
let rec parse_neapolitan st command= 
  match parse command with 
  | Check [player] -> not_typing_color (); 
    neapolitan_reveal (current_players st) ~target:player st; 
    if (let messages = get_messages st in 
        List.length messages <> 0 && String.sub (List.hd messages) 0 1 = "\n")
    then (print_messages st; 
          print_string "> ";
          typing_color ();
          parse_neapolitan st (read_line ()))
    else change_stage st Investigating
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_neapolitan)
  | _ -> not_typing_color ();
    basic_parse st command (parse_neapolitan)

(** [parse_investigate st command] parses [command] for sheriffs. *)
let rec parse_investigate st command=
  match parse command with 
  | Investigate [player] -> not_typing_color ();
    investigate ~suspect:player (current_players st) (current_sheriffs st) st;
    if (let messages = get_messages st in 
        List.length messages <> 0 && String.sub (List.hd messages) 0 1 = "\n")
    then (print_messages st; 
          print_string "> ";
          typing_color ();
          parse_investigate st (read_line ()))
    else change_stage st Protecting
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_investigate)
  | _ -> not_typing_color ();
    basic_parse st command (parse_investigate)

(** [parse_protect st command] parses [command] for doctors. *)
let rec parse_protect st command=
  match parse command with 
  | Heal [player] -> not_typing_color ();
    doctor_heal ~target:player st;
    if (let messages = get_messages st in 
        List.length messages <> 0 && String.sub (List.hd messages) 0 1 = "\n")
    then (print_messages st; 
          print_string "> ";
          typing_color ();
          parse_protect st (read_line ()))
    else change_stage st Supporting
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_protect)
  | _ -> not_typing_color ();
    basic_parse st command (parse_protect)


(** [parse_support st command] parses [command] for retributionists. *)
let rec parse_support st (command:string) =
  let town_dead = (List.filter (fun x -> is_town x) (current_dead st)) in 
  match parse command with 
  | Revive [player] -> not_typing_color(); 
    retributionist_revive town_dead ~target:player st; 
    if (let messages = get_messages st in 
        List.length messages <> 0 && String.sub (List.hd messages) 0 1 = "\n")
    then (print_messages st; 
          print_string "> ";
          typing_color ();
          parse_support st (read_line ()))
    else change_stage st Accusing
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_support)
  | _ -> not_typing_color ();
    basic_parse st command (parse_support)


(** [parse_accuse st command] parses [command] for all. *)
let rec parse_accuse st command=
  match parse command with 
  | Accuse [player] -> not_typing_color ();
    let accused = accuse (current_players st) ~target:player st in 
    if get_messages st = [] then 
      (change_stage st Defending; 
       change_accused st accused)
    else (
      print_messages st; 
      print_string "> ";
      typing_color ();
      parse_accuse st (read_line ()))
  | exception _ -> not_typing_color (); 
    basic_parse st command (parse_accuse)
  | _ -> not_typing_color ();
    basic_parse st command (parse_accuse)


(** [parse_defend accused st command] parses [command] for [accused]. *)
let rec parse_defend accused st command =
  match parse command with 
  | Defend -> defense accused st "defend"; 
    change_stage st Voting; 
    (* if play choose attack, prompt for attack message. 
       bias innocents towards voting yay*)
  | Attack -> defense accused st "attack"; 
    change_stage st Voting;
  | Pass -> defense accused st "pass"; 
    change_stage st Voting;
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_defend accused)
  | _ -> not_typing_color ();
    basic_parse st command (parse_defend accused)

(** [parse_vote accused st command] parses [command] for [accused]. *)
let rec parse_vote accused st command= 
  match parse command with 
  | Vote [v] -> not_typing_color (); 
    let vt = cast_vote v (current_mafia st) (current_town st) accused st in 
    if vt || get_day st >= 2 then
      (* increment night count *)
      let () = add_night st in 
      add_to_messages st [("The day is getting late, Its time for people to go "^
                           "home.")];
      change_stage st Killing;
      reset_day st 
    else 
      (change_stage st Accusing;
       add_day st)
  | exception _ -> not_typing_color ();
    basic_parse st command (parse_vote accused)
  | _ -> not_typing_color ();
    basic_parse st command (parse_vote accused)


(** [play st] plays the game for [st]. *)
let rec play st : unit= 
  let player = current_player st in
  let stage = current_stage st in 
  if (is_active player stage st) then 
    begin 
      (* move players from the killed_tonight list to the dead list *)
      let () = if stage=Accusing 
        then move_dead_to_killed (get_killed_tonight st) st else () in
      (* print the gui *)
      let () = print_grave_maf_alive st in 
      let () = print_messages st in
      print_stage_prompt stage st (get_accused st); 
      let rec read st : unit= 
        print_string "> ";
        typing_color ();
        let command_str = read_line () in 
        match parse command_str with 
        | exception Malformed -> not_typing_color ();
          print_endline "\nThat's not allowed.";
          read st 
        | exception Empty -> not_typing_color ();
          print_endline "\nI didn't catch that.";
          read st 
        | Describe [role] -> not_typing_color (); 
          print_endline (description (string_to_role role)); read st 
        | Rules -> not_typing_color (); 
          print_rules () ; read st 
        | Quit -> not_typing_color ();
          print_endline "\nGoodbye."; Pervasives.exit 0
        | _ ->
          match stage with
          | Killing -> parse_kill st command_str; play st  
          | Investigating -> parse_investigate st command_str; play st 
          | Neap -> parse_neapolitan st command_str; play st  
          | Protecting -> parse_protect st command_str; play st  
          | Supporting -> parse_support st command_str; play st 
          | Accusing -> parse_accuse st command_str; play st  
          | Defending -> parse_defend (get_accused st) st command_str; play st 
          | Voting -> parse_vote (get_accused st) st command_str; play st 
      in
      read st
    end
  else
    match stage with
    | Killing -> 
      mafia_kill (killable Mafia st) (current_mafia st) st; 
      serial_killer_kill (killable Neutral st) (current_neutral st) st; 
      change_stage st Neap;
      play st 
    | Neap -> neapolitan_reveal (current_players st) st; 
      change_stage st Investigating; play st  
    | Investigating ->
      investigate (current_players st) (current_sheriffs st) st;
      change_stage st Protecting; 
      play st
    | Protecting -> 
      doctor_heal st;
      change_stage st Supporting;
      play st 
    | Supporting ->
      let town_dead = (List.filter (fun x -> is_town x) (current_dead st)) in 
      retributionist_revive town_dead st;
      change_stage st Accusing; 
      play st
    | Defending -> 
      print_grave_maf_alive st;
      print_messages st;
      not_typing_color ();
      print_endline ((get_name (current_player st))^" has been accused of being evil.\n"); 
      change_stage st Voting;
      print_endline ("Press enter to continue to the Attack/Defend/Pass stage.");
      print_string "> ";
      typing_color ();
      let () = 
        match read_line () with 
        | _ -> create_bot_messages st (get_accused st); play st in ()
    | Voting -> 
      print_grave_maf_alive st;
      print_messages st;
      not_typing_color ();
      print_endline ("\nPress enter to continue to the Voting stage.");
      print_string "> ";
      typing_color ();
      let () =  
        match read_line () with 
        | _ -> parse_vote (get_accused st) st "nay"; play st in ()
    | _ -> failwith "player should be active but isn't" 


let start_game name = 
  (* for testing specific role *)
  (* let role = Sheriff Town in  *)
  (* let role = Mafioso Mafia in *)
  (* let role = Vigilante Town in *)
  (* let role = SerialKiller Neutral in *)
  (* let role = Neapolitan Town in *)
  (* let role = Citizen Town in *)
  (* let role = Doctor Town in *)
  (* let role = Retributionist Town in *)
  let role = (get_rnd_role (Random.int 8)) in
  let role_description = (description role) in
  let bots = generate_players 3 6 1 1 1 1 1 1 in
  (* let bot_names = get_player_names bots name in *)
  let st = init_state (make_player name role) bots in 
  (* let the_mafia = get_player_names (current_mafia st) "" in *)
  let () = print_grave_maf_alive st in 
  let () = print_endline ("\nHello "^name^", welcome to Functional Town.") in 
  print_endline ("You are a "^(role_to_string role)^". "^
                 role_description);
  let () = print_endline ("\nPress enter to continue.") in
  let () = print_string "> " in 
  typing_color ();
  match read_line () with 
  | _ -> play st 

(* [main ()] prompts for the name of the player, then starts 
    the game. *)
let rec main () =
  ANSITerminal.resize 150 36;
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1;
  ANSITerminal.(print_string [yellow] "OMafia.\n");
  
  ANSITerminal.(print_string [yellow]
                  "OMafia.\n");
  print_endline "Welcome to Mafia.";
  print_endline ("Here are the rules. You may access them again anytime during "^
                 "the game by typing 'rules'.");
  print_rules ();
  print_endline "Please enter your name.";
  print_string  "> ";

  retrieve_name start_game 


(* Execute the game engine. *)
let () = main ()