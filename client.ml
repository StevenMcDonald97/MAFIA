open Yojson 
open Yojson.Basic.Util
open Unix
open Server_state
open Server_actions
open Commands
open Server_player
open Server_visuals


(* let s_descr = ref 0 *)
 
(* store current player's data *)
let client_player : Server_player.t option ref = ref None
 
(* get current player *)
let current_player ()= 
  match !client_player with
  | None -> failwith "no player"
  | Some plyr -> plyr

(* ======================= communicate with server =========================== *)

(* read data from server file_descr *)
let read_server len s_descr = 
  try
    let acc = Bytes.make len ' ' in
    let _ = Unix.read s_descr acc 0 len in Bytes.to_string acc
    with exn -> print_endline "read failed"; raise exn

(* write message to a server *)
let write_server message server =
  let _ = (Unix.write_substring server message 0 (String.length message)) in ()

(* read message from server *)
let read_message s_descr = 
  try 
    let message = Bytes.make 1000 ' ' in
    let _ = Unix.read s_descr message 0 1000 in ();
    Bytes.to_string message
  with exn -> ""

(* recieve and process state from server *)
let read_state s_descr = 
    let state_json = read_server 5000 s_descr in
    state_of_json (Yojson.Basic.from_string state_json)




(* ================================== play code =========================== *)

(* check end of game conditions in [st] *)
let end_of_game st current_player=
  (* check conditions for end of game, and print end message if they hold *)
  not_typing_color ();
  if get_status current_player = Dead then 
    let () = print_endline "They found you! You've been killed." in 
    Pervasives.exit 0
  else if (* List.length (current_town st) <  *)
    List.length (current_town st) = 0 
    && (get_role current_player) = Mafioso Mafia then 
    let () =
      print_endline ("Congratulations! You outnumber the townspeople."^
      " The Mafia reign supreme.")
    in Pervasives.exit 0
  else if List.length (current_mafia st) = 0 
          && is_town current_player then 
    let () = print_endline 
        "Congratulations! All of the mafia have been caught. It's safe to go out again." 
    in Pervasives.exit 0
 
  else if (List.length (current_town st) = 0 || 
           List.length (current_mafia st)=0) 
  then
    (reset (); failwith "End of game triggered prematurely")
  else ()


  (**[print_killed st] prints the list of 
  people in the current killed_tonight field in [st] as 
  being killed by the corresponding role*)
let rec print_killed = function
| [] -> not_typing_color (); print_endline ""
| (killer, victim)::t -> 
  print_string ((get_name victim) ^ " was killed by the "); 
  begin
    match killer with 
    | Mafioso Mafia -> mafia_lst ()
    | _ -> raise (Invalid_role)
  end;
  print_string ((role_to_string killer));
  not_typing_color ();
  print_string (". Their role was ");
  begin
    match get_role victim with 
    | Citizen Town 
    | Mafioso Mafia -> mafia_lst ()
    | _ -> raise (Invalid_role)
  end;
  print_string (role_to_string(get_role victim));
  not_typing_color ();
  print_endline "."; 
  print_killed t

(**[move_dead_to_killed dead st] moved the players in the killed_tonight field of [st]
to the dead field in [st] *)
let rec move_dead_to_killed dead st=
match dead with
| []->()
| (_, player)::t -> move_player_to_killed st player; move_dead_to_killed t st

(* print the message prompt for [stage] *)
let print_stage_prompt st current_player= 
  let accused = (get_accused st) in
  let stage = current_stage st in 
  not_typing_color ();
  match stage with
  | Killing -> end_of_game st current_player;
    print_endline("Nighttime falls. Now is your chance to strike."); 
    print_endline 
      ("\nType 'Kill' and a character's name whom you with to die,"^
      " kill nobody, or another command."); ()
  | Accusing -> end_of_game st current_player;
    begin
      if get_day st = 0 then 
        (let () = print_endline "Day breaks. Some have been busy..." in 
         let () = print_killed (get_killed_tonight st) in
         clear_killed_tonight st; end_of_game st current_player)
        else (); 
      print_endline "Do you suspect anyone? Accuse Nobody if you have no suspects." 
    end
  | Voting ->  print_endline ("\n"^(get_name accused)^
    " has been accused of being evil. Do you vote yay or nay?"); ()

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

(* check if a command can be used during a stage *)
let valid_command command stage =
  match command with 
  | Kill _ -> stage = Killing
  | Accuse _ -> stage = Accusing
  | Vote _ -> stage = Voting
  | _ -> failwith "shouldn't have parsed this command"
 

let process str st s_descr= 
  let player = current_player () in
  let stage = current_stage st in
    begin 
      (* move players from the killed_tonight list to the dead list *)
      let () = 
        if stage=Accusing then move_dead_to_killed (get_killed_tonight st) st 
        else () in
      (* print the gui *)
      let () = print_grave_maf_alive st player in  
      let () = print_messages st in
      print_stage_prompt st (get_accused st);
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
        (* if command is valid for this stage and changes state, send to server *)
        | command when valid_command command stage-> 
          begin
            match command with 
            (* catch invalid uses of commands *)
            | Kill [n] when get_role(get_player st n) = Mafioso Mafia -> 
              print_endline "You can't kill a mafioso"; read st
            | Accuse [n] when n = get_name (current_player ()) -> 
              print_endline "You can't accuse yourself"; read st
            | c -> write_server command_str s_descr
            | _ -> print_endline "\nSomething went wrong."
          end
        | _ -> print_endline "\nSomething went wrong.";
         read st

      in
      read st
    end


  (* ================================= server code =================================== *)

(* client code *)
let rec start_client client_fun : unit=
  (* client descriptor) *)
  let c_descr = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in   
  (* get ip, port number *)
  print_endline "What is the game you wish to join? (An IP address)\n";
  print_string "> ";
  let s_addr_plain = read_line() in 
  (* print_endline "Enter server port number\n"; *)
  let s_port = 1400
  in
  let s_addr = 
    try Unix.inet_addr_of_string s_addr_plain
    with exn ->
        Printf.eprintf "%s: server not found\n" s_addr_plain; raise exn
  in try
      let sockaddr = Unix.ADDR_INET(s_addr, s_port) in

      (* connect to the server address  *)
      Unix.connect c_descr sockaddr;

      (* open connection to socket, then get file_descr of server *)
      let in_chan, _ = open_connection sockaddr in
      let s_descr = Unix.descr_of_in_channel in_chan in
 
      client_fun s_descr;
  with exn ->
      Printf.eprintf "could not connect\n";
      start_client client_fun



let client_loop c_sock =
  while true do
    (* buffer for reading from server *)
    print_endline "waiting for state";
    (* check that message is a valid json *)
    try
      let state  = read_state c_sock in
      let () = print_endline "Press enter to continue" in
      let return_message = read_line () in
      let () = write_server return_message c_sock in
      print_grave_maf_alive state (current_player ());

      (* if player is active display prompt, else waiting *)
      if is_active (current_player ()) (current_stage state) then
        let () = print_stage_prompt state (current_player ()) in

        process (read_line ()) state c_sock;

      else 
        print_endline "waiting for other players"; 
    with Yojson.Json_error(_) -> print_endline "not a state";
    print_string "type message >";

    let message = read_line() in 
    let _ = Unix.write_substring c_sock message 0 (String.length message) in ()

  done 


(* establish client connections
    read from clients, send new state to clients *)
    (* s_sock is the socket address of the server *)
let client_service c_sock (*sockaddr*) = 
  (* waits until server sends info *)
      let waiting = ref true in
      Unix.clear_nonblock c_sock;

      try 

        (* while !waiting do
          try  *)
            (* read welcome message from server *)
            (* let welcome_message = read_server 100 c_sock in
            print_endline welcome_message;    *)

            
            print_endline (read_message c_sock);
            (* print_endline (read_message c_sock); *)

            (*send name back to server  *)
            let return_message = read_line() in
            write_server return_message c_sock;

            (* delay to help message order *)
            Unix.sleep 1;

            (* read player data from server *)
            let player_json = read_server 1000 c_sock in
            let player = player_of_json (Yojson.Basic.from_string player_json) in 
            let role = get_role player in 
            let role_description = (description role) in
            print_endline ("You are a "^(role_to_string role)^". "^
            role_description);

            (* set client's player as player *)
            client_player := Some player;

            client_loop c_sock

        with exn -> print_endline "something went wrong"; close c_sock; raise exn
      



 (* pass in server address, port number to start client process *)
 let start_client () = Unix.handle_unix_error start_client client_service 