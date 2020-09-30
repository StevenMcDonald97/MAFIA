open Server_state
open Server_player 

exception Not_a_person
exception Not_a_command
(* exception Invalid_role *)

(* exception InvalidRole of Player.role
   exception InvalidPlayer of Player.t *)

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
     "   Accuse [name]: accuse [name] of being evil\n"^
     "   Vote [name]: votes [name] to be lynched\n"^
     "   Kill [name]: attacks [name]\n"^
     "        Mafioso, Vigilante only\n"^
     "   Investigate [name]: checks if [name] is suspicious or non-suspicious."^
     "        Kills if suspicious\n"^
     "        Sheriff only\n"^
     "   Check [name]: checks if [name] is a Town or not. Cannot kill\n"^
     "        Neapolitan only\n"^
     "   Heal [name]: heals [name]\n"^
     "        Doctor only\n"^
     "   Revive [name]: revives [name]\n"^
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
          [(get_name player)^" has voted nay"
          (* with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
             in votes
             else let vote = Random.int 5 in if vote > 1 
             then let () = add_to_messages st [(get_name player)^" has voted yay"
             (* with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
      in votes+1        
    else
      let () = add_to_messages st [(get_name player)^" has voted nay" 
      (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] in
        votes else if is_innocent player accused 
        then let () = add_to_messages st [(get_name player)^" has voted nay" 
        (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
      in votes           
  else if (is_a_suspect player (get_name accused)) then  
    let () = add_to_messages st [(get_name player)^" has voted yay" 
    (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] 
      in votes+1 else let vote = Random.int 6 in if vote > 1 
      then let () = add_to_messages st [(get_name player)^" has voted yay" 
      (*with doubt "^string_of_int(get_doubt player (get_name accused))*)] in
    (votes+1)            
  else
    let () = add_to_messages st [(get_name player)^" has voted nay"] in votes  

(** [generate_votes accused plyrs votes st] calcualtes. *)
let rec generate_votes accused plyrs votes st=
  match plyrs with 
  |[]-> votes
  | h::t ->
    generate_votes accused t (votes+(random_vote accused h 0 st)) st         

(* cast_vote vote: tally "yay" votes in votes, generate random number of other votes *)
let cast_vote votes st= 
  let accused = get_accused st in 

  let players_tally =  
    List.length (List.filter (fun x-> x="yay") votes) in
  let tally =
        generate_votes accused (current_players st) players_tally st in
  if tally > (List.length (current_players st)-tally) then
    let () = (move_player_to_killed st accused) in
    let () = add_to_messages st ["\n"^(get_name accused)^" has been hung\n"] in
    let () = add_to_messages st [("It turns out they were a "^
                                  (role_to_string (get_role accused))^" \n")] in true
  else 
    let () = add_to_messages st ["\n"^(get_name accused)^" has been found "^
                                 "innocent."] in false


let accuse ?targets:(targets=[]) st=
  match targets with
  | [] -> get_random_player (current_players st)
  | h::t -> h

(* naively just kill first person voted on, don't caluclate kill votes yet *)
let mafia_kill ?targets:(targets=[]) st=
let mafias = current_mafia st in
let innocents = current_town st in
  match targets with
  | [] -> let rnd_player = Random.int (List.length innocents) in 
      let player = (List.nth innocents rnd_player) in
      let rnd_mafia = Random.int (List.length mafias) in 
      let mafia = (List.nth mafias rnd_mafia) in
      change_status player;
      add_to_killed_tonight st player (Mafioso Mafia)
  | h::t -> 
      change_status h;
      add_to_killed_tonight st h (Mafioso Mafia)
