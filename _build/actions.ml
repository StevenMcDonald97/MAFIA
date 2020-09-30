open State
open Player 
open Visuals

exception NoMajority 
exception Not_a_person
exception Not_a_command

(** Initialzie random to use the default seed *)
let _ = Random.self_init()


let print_rules ()= 
  print_endline
    ("OMafia.\n"^
     "Factions: Mafia, Town, Neutral\n"^
     "Goal: To eliminate everyone in the other Factions.\n"^
     "Two Rotating Phases: Day, Night\n"^
     "   Day: Accusations (max three per day, until someone gets hanged)\n"^
     "        Defend/Attack/Pass (what info do you have to support your vote?)\n"^
     "        Voting (yay or nay, majority decides\n"^
     "   Night: Killing (only killing roles are active)\n"^
     "          Investigation (only investigating roles are active)\n"^
     "          Protecting (only protective roles are active)\n"^
     "          Supporting (only supporting roles are active)\n"^
     "Commands:\n"^
     "   Describe [role]: returns the description of [role]\n"^
     "   Rules: returns the rules of the game\n"^
     "   Quit: ends the game\n"^
     "   Accuse [player]: accuse [player] of being evil\n"^
     "   Vote [player]: votes [player] to be lynched\n"^
     "   Kill [player]: attacks [player]\n"^
     "        Mafioso, Vigilante only\n"^
     "   Investigate [player]: checks if player is mafia or not."^
     "        player is killed if they are mafia\n"^
     "        Sheriff only\n"^
     "   Check [player]: checks if [player] is a Town or not. Cannot kill\n"^
     "        Neapolitan only\n"^
     "   Heal [player]: stops player from being killed that night\n"^
     "        Doctor only\n"^
     "   Revive [player]: revives [player]\n"^
     "        Retributionist only\n"^
     "   Attack: say why the accused is guilty\n"^
     "   Defend: say why the accused is innocent\n"^
     "   Pass: have no say on whether accused is guilty or innocent\n"
    )



(** [random_vote accused player votes st] will print how a player has voted. *)
let random_vote accused player (votes : int) st : int=  
  if get_role player = Mafioso Mafia then
    if get_role accused = Mafioso Mafia then
      let () = add_to_messages st 
          ["   "^(get_name player)^" has voted nay"
          (* with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
             in votes
             else let vote = Random.int 5 in if vote > 1 
             then let () = add_to_messages st ["   "^(get_name player)^" has voted yay"
             (* with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
      in votes+1        
    else
      let () = add_to_messages st ["   "^(get_name player)^" has voted nay" 
      (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] in
        votes else if is_innocent player accused 
        then let () = add_to_messages st ["   "^(get_name player)^" has voted nay" 
        (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
      in votes           
  else if (is_a_suspect player accused) then 
    let () = add_to_messages st ["   "^(get_name player)^" has voted yay" 
    (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
      in votes+1 else let vote = Random.int 6 in if vote > 1 
      then let () = add_to_messages st ["   "^(get_name player)^" has voted yay" 
      (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] in
    (votes+1)            
  else
    let () = add_to_messages st ["   "^(get_name player)^" has voted nay"] in votes  


(** [generate_votes accused plyrs votes st] calcualtes. *)
let rec generate_votes accused plyrs votes st=
  match plyrs with 
  |[]-> votes
  | h::t ->
    generate_votes accused t (votes+(random_vote accused h 0 st)) st         



(* cast_vote vote: Takes argument string [vote], list of mafia [mafia], 
   list of innocents [innocent] and the accused player [accused]. For each bot, 
   randomly calculate whether they vote "yay". Tally total 
   (including player's vote) and return true if tally is more 
   than half of all living people. *)
let cast_vote vote mafia innocent accused st= 
  let player_vote = match vote with
    | str when str="yay" || str= "yes" -> 1
    | _ -> 0
  in let tally =
       generate_votes accused (current_players st) player_vote st in
  if tally > (List.length (current_players st)-tally) then
    let () = (move_player_to_killed st accused) in
    let () = add_to_messages st ["\n"^(get_name accused)^" has been hung\n"] in
    let () = add_to_messages st [("It turns out they were a "^
                                  (role_to_string (get_role accused))^" \n")] 
    in true
  else 
    let () = add_to_messages st ["\n"^(get_name accused)^" has been found "^
                                 "innocent."] in false

(** [valid_target st target role] checks whether [target] is a valid target
    for [role] given [st]. *)
let valid_target st target role =
  try let () = (if role = Retributionist Town 
                then ignore (get_player target (current_dead st))
                else ignore (get_player target (current_players st))) in true
  with Invalid_player -> 
    let () = 
      (if role = Retributionist Town 
       then add_to_messages st [("\nI'm sorry, "^target^" is not a dead player.")]
       else add_to_messages st ["\nI'm sorry, "^target^" is not a living player."]) 
    in false 

(** [valid_target_bool st target role] is mainlyused by bot Retributionists, 
    does not print out any messages and checks whether [target] is a valid target 
    for [role] given [st]. *)
let valid_target_bool st target role =
  try let () = (* (if role = Retributionist Town 
                  then *) ignore (get_player target (current_dead st))
  (* else ignore (get_player target (current_players st))) *) in true
  with Invalid_player -> false

(** [role_continue st ?player ?player_role target] is a helper for continue, to
    check whether we should continue executing night abilities on [target] given
    [player] and [player_role] during [st]. *)
let role_continue st ?player:(player=(make_player "TEST" (Citizen Town))) 
    ?player_role:(player_role=(Citizen Town)) target  = 
  match player_role with 
  | Mafioso Mafia -> 
    if valid_target st target (Mafioso Mafia) then 
      begin
        if (get_role(get_player target (current_players st)) = Mafioso Mafia) 
        then let () = add_to_messages st ["\nYou cannot kill your own teammate."] 
          in false 
        else true 
      end else false
  | Vigilante Town -> 
    if get_vig_guilt st <> (-1)
    then let () = add_to_messages st ["\nYou have put away your gun for shooting a Town."] 
      in false
    else if charge_value (get_charge player) = 0
    then (if get_role(current_player st) = Vigilante Town 
          then let () = add_to_messages st [("\nYou have no more bullets.")] 
            in false else false)
    else valid_target st target (Vigilante Town)
  | SerialKiller Neutral -> valid_target st target (SerialKiller Neutral)
  | Sheriff Town -> valid_target st target (Sheriff Town)
  | Doctor Town -> valid_target st target (Doctor Town)
  | Retributionist Town -> 
    if charge_value (get_charge player) = 0 
    then (if get_role(current_player st) = Retributionist Town 
          then let () = add_to_messages st [("\nYou already revived someone.")] 
            in false else false) (* only add message if you are the ret*)
    else if get_town(current_dead st) = [] 
    then (if get_role(current_player st) = Retributionist Town 
          then let () = add_to_messages st [("\nNo one is dead yet.")] 
            in false else false)
    else if target = "" then true (* for bots, but dead list cannot be empty *)
    else if is_dead target st
    then (if get_role (get_player target (current_dead st)) = Mafioso Mafia 
          || is_neutral (get_player target (current_dead st)) 
          then let () = add_to_messages st ["\nYou can only revive a Town."] 
            in false else true)
    else valid_target st target (Retributionist Town)
  | Neapolitan Town -> valid_target st target (Neapolitan Town)
  | _ -> raise (Invalid_role) 

(** [continue st ?player ?player_role target] checks to see if [player] or
    [player_role] should continue to execute night abilities on [target] given
    [st]. *)
let continue st ?player:(player=(make_player "TEST" (Citizen Town))) 
    ?player_role:(player_role=Citizen Town) target  = 
  let stage = current_stage st in 
  match target with 
  | p when String.lowercase_ascii p = "nobody" && stage = Accusing -> true
  | p when String.lowercase_ascii p = "nobody" -> false
  (* | p when p = "" && player_role = Retributionist Town -> valid_t`arget_bool 
     st target player_role  *)
  (* | p when player_role = Retributionist Town -> valid_target st target 
     player_role *)
  | p when p = "" && player_role <> Retributionist Town -> true
  | p when (current_player st |> get_name = target )-> 
    let () = 
      match stage with 
      | Killing -> add_to_messages st ["\nYou cannot kill yourself."]
      | Investigating -> add_to_messages st ["\nYou cannot investigate yourself."] 
      | Protecting -> add_to_messages st ["\nYou cannot heal yourself."]
      | Supporting -> add_to_messages st ["\nYou cannot revive yourself."]
      | Neap -> add_to_messages st ["\nYou cannot check yourself."]
      | Accusing -> add_to_messages st ["\nYou cannot accuse yourself."]
      | _ -> failwith "no continue here" in 
    false 
  | p when player_role <> Retributionist Town && not (is_player p st) -> 
    let () = add_to_messages st ["\nNo such player exists."] in false
  | p when stage = Accusing && is_player p st ->
    if current_player st |> get_role = Mafioso Mafia &&
       get_role(get_player target (current_players st)) = Mafioso Mafia 
    then let () = add_to_messages st ["\nYou cannot accuse your own teammate."] in 
      false else true
  | p -> if get_role player = Citizen Town then role_continue st 
        ~player_role:(player_role) target
    else role_continue st ~player:player ~player_role:(player_role) target


let mafia_kill innocents mafias ?target:(target="") st =
  if continue st target ~player_role:(Mafioso Mafia) then
    if current_mafia st = [] then () else
      begin
        if target = "" then
          let rnd_player = Random.int (List.length innocents) in 
          let player = (List.nth innocents rnd_player) in
          let rnd_mafia = Random.int (List.length mafias) in 
          let mafia = (List.nth mafias rnd_mafia) in
          if get_status player = Dead then () else change_status player;
          add_to_killed_tonight st player (get_role mafia)
        else let player = (get_player target innocents) in
          change_status player;
          add_to_killed_tonight st player (get_role (current_player st)) 
      end
  else ()

let rec serial_killer_kill players neutrals ?target:(target="") st =
  match current_serialkillers st with
  | []-> ()
  | h::t ->
    if continue st target ~player:h ~player_role:(SerialKiller Neutral) then 
      begin
        if target = "" then 
          let killed = get_random_player players in 
          change_status killed;
          add_to_killed_tonight st killed (get_role h)
        else let player = (get_player target players) in
          if get_status player = Dead then () else change_status player;
          add_to_killed_tonight st player (get_role (current_player st))
      end 


(* bots cant shoot anyone for now *)
let vigilante_kill target st = 
  match current_vigilantes st with 
  | [] -> ()
  | h::t -> 
    if continue st target ~player:h ~player_role:(Vigilante Town) then
      begin
        let player = get_player target (current_players st) in 
        if get_status player = Dead then () 
        else (change_status player;
              change_charge h;
              add_to_killed_tonight st player (Vigilante Town));
        if is_town player then  
          change_vig_guilt st else ()
      end 

let doctor_heal ?target:(target="") st = 
  (* Test whether a valid input has bee passed in by the user *)
  (* if input is valid, execute function and return true, if not skip and return 
     false *)
  match current_doctors st with 
  | [] -> ()
  | h::t ->
    if continue st target ~player:h ~player_role:(Doctor Town) then
      begin
        if target = "" then 
          let others = current_players st in 
          let rnd_player = Random.int (List.length others) in 
          let player = (List.nth others rnd_player) in
          if get_name (current_player st) = target then 
            (if get_status player = Dead
             then (change_status player; 
                   remove_from_killed_tonight st player; 
                   add_to_messages st ["You were attacked, but nursed back to"^
                                       " health."]) else ())
          else if (get_status player = Dead) 
          then let () = change_status player in 
            remove_from_killed_tonight st player else ()
        else let player = get_player target (current_players st) in 
          if get_status player = Dead
          then (change_status player;
                remove_from_killed_tonight st player; 
                if get_role (current_player st) = Doctor Town then
                  add_to_messages st ["The player you healed, "^get_name (player)^
                                      " was attacked, but you nursed them back to"^
                                      " health."] else ()) else ()
      end 


let retributionist_revive town_dead ?target:(target="") st  =
  (* retributionsit revive does nothing if invalid target is given, there are no dead, 
     or the retriubtionsit is out of charges  *)
  match current_retributionists st with 
  | [] -> ()
  | h::t -> 
    if continue st target ~player:h ~player_role:(Retributionist Town) then 
      begin
        if target = "" 
        then (let revived = get_random_player town_dead in 
              if get_status revived = Alive then () else change_status revived;  
              revive_player_from_killed st revived; 
              change_charge h; 
              add_to_messages st [(get_name revived)^" was brought back to life!"])
        else (let revived = get_player target town_dead in 
              if get_status revived = Alive then () else change_status revived;  
              revive_player_from_killed st revived; 
              change_charge h;
              add_to_messages st [(get_name revived)^" was brought back to life!"])
      end



let neapolitan_reveal players ?target:(target="") st =
  (*if (current_neapolitans st = []) && (get_role (current_player st) <> 
    Neapolitan Town) then () 
    else *)
  match current_neapolitans st with 
  | [] -> ()
  | h::t ->
    if continue st target ~player:h ~player_role:(Neapolitan Town) then
      begin
        let tar = if target = "" then get_random_player (current_players st)   
          else get_player target (current_players st)  
        in 
        if (get_role (current_player st)) = Neapolitan Town then
          if List.mem tar (current_town st)  then 
            add_to_messages st [(get_name tar)^" is a member of Town."] 
          else 
            add_to_messages st [(get_name tar)^" does not belong to Town."]
        else ()
      end

let accuse living ?target:(target="") st =
  (* if input is valid, execute function and return true, if not skip and return 
     false *)
  if continue st target then 
    let rec make_accused_lst new_living acc =
      let curr_player = st |> current_player in 
      match new_living with 
      | [] -> acc
      | h::t ->
        begin 
          match get_role h with
          | Mafioso Mafia -> (* if mafia then make sure to not choose from a list
                                of mafias, check if current player is mafia *)
            let possible_accuse = 
              if (get_role curr_player) = Mafioso Mafia 
              then get_town living 
              else curr_player::(get_town living) in
            let player =
              (* there has to be a guard in case the possible_accuse list is 
                 length 0 *)
              if (List.length possible_accuse) <> 0 then
                List.nth possible_accuse (Random.int (List.length possible_accuse)) 
              else (current_player st) 
            in
            if List.mem_assoc player acc 
            then
              let player_count = List.assoc player acc in 
              let lst_wo_player = (List.filter (fun x -> fst x <> player) acc) in 
              make_accused_lst t ((player,player_count+1)::lst_wo_player)
            else make_accused_lst t ((player,1)::acc)
          | Citizen Town | Sheriff Town | SerialKiller Neutral 
          | Vigilante Town | Doctor Town | Retributionist Town 
          | Neapolitan Town-> (* if town then randomly choose from 
                                              living, 
                                              including current player *)
            let possible_accuse = List.filter (fun x -> get_name x <> get_name h) 
                living in 
            let player = get_random_player possible_accuse in 
            if List.mem_assoc player acc then 
              let player_count = List.assoc player acc in 
              let lst_wo_player = (List.filter (fun x -> fst x <> player) acc) in 
              make_accused_lst t ((player,player_count+1)::lst_wo_player)
            else make_accused_lst t ((player,1)::acc)
          | _-> failwith "invalid role/side matching" 
        end in
    let acc = if target = "" || String.lowercase_ascii target = "nobody" 
      then [] else [(get_player target living,4)] in 
    (* if target is given, that is the starting list *)
    let sorted_accused = List.rev (List.sort (fun (_,c1) (_,c2) -> 
        (* sort the tuple list by the values, greatest first *)
        if c1 < c2 then -1 
        else if c1 = c2 then 0
        else 1) (make_accused_lst living acc)) in 
    match List.length sorted_accused with 
    (* if list is empty, error, if 1 element then return it, if more then check 
       if the first 2 have the same values if so then throw nomajority else return 
       the first element *)
    | 0 -> failwith "cannot accuse nobody"
    | i -> (sorted_accused |> List.hd |> fst)

  else current_player st



let investigate ?suspect:(suspect="") (living_players: Player.t list) 
    (sheriffs: Player.t list) (st: State.t)   = 
  (* Test whether a valid input has bee passed in by the user *)
  (* in more advanced gameplay, investigate should not be possible on known-
     innocent characters *)
  (* if input is valid, execute function and return true, if not skip and return 
     false *)
  if continue st suspect ~player_role:(Sheriff Town) then
    if current_sheriffs st = [] then () else
      begin
        let spct = 
          if suspect <> "" then 
            Some (get_player suspect (current_players st))
          else 
            None
        in
        (*acc is death row -- the list of convicted mafia. It does not contain 
          duplicates*)
        let rec helper players sheriffs st acc = 
          match sheriffs with 
          |[] -> acc
          (*candidates is list of living players other than the sheriff currently 
            conducting the investigation*)
          |h::t -> let candidates = List.filter (fun x -> x <> h) players in
            let chosen = get_random_player candidates in 
            (*if chosen is mafioso and has not been sentenced to death row...*)
            if (List.mem chosen (current_mafia st) && (not (List.mem chosen acc)))
            then (helper players t st (chosen::acc))
            else (*if (get_role chosen <> Mafioso Mafia)*) 
              let () = set_doubt h (get_name chosen) 0 in
              helper players t st acc in  
        let cur_death_row =  helper living_players sheriffs st [] in

        let death_row =  
          begin match spct with 
            | None -> cur_death_row
            | Some x -> (* let () = print_grave_maf_alive st in  *)
              let () = if (List.mem x (current_mafia st)) then
                  add_to_messages st 
                    [((get_name x)^" was a Mafioso. He's going to the gallows.")]
                else add_to_messages st 
                    [((get_name x)^" is non-suspicious.")];
                add_to_innocents (current_player st) x
              in
              if List.mem (current_player st) (current_sheriffs st) && 
                 (List.mem x (current_mafia st) && 
                  (not (List.mem x cur_death_row)))
              then  x::cur_death_row 
              else  cur_death_row 
          end in

        let rec mutate_helper acc = 
          match acc with 
          | h::t -> 
            add_to_killed_tonight st h (Sheriff Town);
          | [] -> () in 

        mutate_helper death_row; 
      end
  else ()

(** [defense_messages] are possible defense messages and the accompanying 
    maximal doubt they can incur for each of the other players. *)
let defense_messages = [
  ("they couldn't hurt a fly!",4);
  ("I've known them twenty years and they're innocent.",3);
  ("its not possible they're mafioso.",2);
  ("this is ridiculous!",3);
  ("now why would they accuse them?",2);
  ("I got nothing.",1)
]

(** [attack_messages] are possible attack messages and the accompanying 
    maximal doubt they can incur for each of the other players. *)
let attack_messages = [
  ("kill that mafioso scum!",4);
  ("I mean, it could be a coincidence I saw them going out last night...",3);
  ("I always knew they were no good.",2);
  ("think about how suspiciosly they've been acting.",1);
  ("that critter is a low down no-good varmint!",2)

]
(** [adjust_doubt accused player d lst] adjust doubt of [player] against [accused]
    by at most d *)
let rec adjust_doubt accused player d lst =
  match lst with
  | [] -> ()
  | h::t -> if (h<>accused&&h<>player) then begin
      change_doubt h (get_name accused) (Random.int d); 
      adjust_doubt accused player d t;
    end
    else adjust_doubt accused player d t

(** [get_defense_message accused player st] returns a defense message and adjust
    doubt for [player] against [accused] accordingly. *)
let get_defense_message accused player st = 
  let num = Random.int (List.length defense_messages) in
  let message = List.nth defense_messages num in
  let ()=adjust_doubt accused player (snd message) (current_players st) in
  fst message

(** [get_attack_message accused player st] returns a attack message and adjust
    doubt for [player] against [accused] accordingly. *)
let get_attack_message accused player st = 
  let num = Random.int (List.length attack_messages) in 
  let message = List.nth attack_messages num in
  let ()= adjust_doubt accused player (snd message) (current_players st) in
  fst message

(** [mafia_defend st accused] is for each mafia, choose to defend or not, and 
    only one mafia bot speaks *)
let rec mafia_defend st accused =
  let mafia = current_mafia st in 
  if mafia = [] then () else
    let mafia_def = get_random_player mafia in
    match get_role accused with
    | Mafioso Mafia -> 
      (*  Mafia should defend if the accused is mafia  *)
      if List.length (mafia) > 0 then
        (* add defense to mafia member's defended list*)
        add_to_messages st ["   "^
                            (get_name mafia_def)^" says "^
                            (get_defense_message accused mafia_def st)]; 
    | Citizen Town | Sheriff Town | SerialKiller Neutral | Vigilante Town 
    | Doctor Town | Retributionist Town | Neapolitan Town-> 
      (* mafia randomly attack or defend if the  accused is innocent*)
      if (Random.int 6) > 4 then         
        add_to_messages st ["   "^
                            (get_name mafia_def)^" says "^
                            (get_defense_message accused mafia_def st)]
      else
        add_to_messages st ["   "^
                            (get_name mafia_def)^" says "^
                            (get_attack_message accused mafia_def st)];  
    | _ -> failwith "a role was not matched in mafia_defend"

(** [sheriffs_defend_helper sheriffs accused st messages] geenrates
    sheriffs messages (if any).*)
let rec sheriffs_defend_helper sheriffs accused st messages=
  match sheriffs with 
  |[] -> messages
  | h::t -> 
    let innocent = is_innocent h accused in
    match innocent with
    (* sheriff will defend only if they know the accused is innocent *)
    |true -> sheriffs_defend_helper t accused st messages@["   "^get_defense_message 
                                                             accused h st] 
    (* else they do nothing *)
    |false -> sheriffs_defend_helper t accused st messages

(* [sheriffs_defend st accused] if accused is in sheriffs innocent list, 
   defend. If they're in their suspect list, attack
   otherwise, do nothing. *)
let sheriffs_defend st accused : unit=
  let sheriffs = current_sheriffs st in
  add_to_messages st (sheriffs_defend_helper sheriffs accused st [])

(**  [citizens_defend st accused] is that each civilian might defend or accuse 
     semi-randomly. max 2 messages random; if 0 pass. If 1, attack, if 2, defend. *)
let rec citizens_defend st accused =
  let towns = get_town (current_players st) in 
  if towns = [] then () else
    let citizen = get_random_player towns in 
    let choice = Random.int 3 in 
    if choice = 0 || is_innocent accused citizen then
      add_to_messages st [("   "^
                           (get_name citizen)^" says "^
                           (get_defense_message accused citizen st))]
    else if choice = 1 then 
      add_to_messages st [("   "^(get_name citizen)^" says "^
                           (get_attack_message accused citizen st))]
    else ()

let create_bot_messages st accused : unit =  
  let () = mafia_defend st accused in
  let () = sheriffs_defend st accused in 
  citizens_defend st accused


(* If user adds attack or defense, add to message queue. If they pass, bots may 
   or may not defend or attack *)
let defense accused st def_att= 
  match def_att with
  | "defend" -> let () = not_typing_color () in 
    let () = print_endline ("\nWhat defense would you like to offer for "^
                            (get_name accused)^"?") in
    let () = print_string ("> ") in
    typing_color ();
    let message = ("   "^(get_name (current_player st))^" says "^read_line() ^
                   ".") in add_to_messages st [message];
    create_bot_messages st accused 
  | "attack" -> let () = not_typing_color () in 
    print_endline ("\nWhat attack would you like to offer against "^
                   (get_name accused)^"?");
    let () = print_string ("> ") in 
    typing_color ();
    let message = ("   "^(get_name (current_player st))^" says "^
                   read_line() ^ ".") in add_to_messages st [message];
    create_bot_messages st accused;
  | "pass" -> create_bot_messages st accused
  | _ -> raise Not_a_command

