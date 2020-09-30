open Yojson 
open Yojson.Basic.Util
open Unix
open Server_state 
open Server_actions
open Commands
open Server_player
open Server_visuals

(* =========================== game parsing code ============================*)

(* array to store addresses of clients *)
let clients : Unix.file_descr array ref = ref [||]
(* current players in game *)
let players : Server_player.t list ref = ref []
(* current state of game *)
let game_state : Server_state.t option ref = ref None 

(* change state of game *)
let change_game_state old_state new_state =
   old_state := Some new_state

   (* return current game state  *)
let get_game_state state : Server_state.t= 
  match state with
  | None -> failwith "no state"
  | Some st -> st

(**[move_dead_to_killed dead st] moved the players in the killed_tonight field of [st]
to the dead field in [st] *)
let rec move_dead_to_killed dead st=
  match dead with
  | []->()
  | (_, player)::t -> move_player_to_killed st player; move_dead_to_killed t st
(* 
(* add an element to a hashtable; or if it is in table, increment its count *)
let add_element_to_table table n= 
  if Hashtbl.mem table n then
    let old_val = Hashtbl.find table n in
    let () = Hashtbl.add table n (old_val+1) in
    table
  else
    let () = Hashtbl.add table n 1 in
    table    

(*  *)
let get_most_common lst = 
  let length = List.length lst in
  let table = Hashtbl.create length in
  for i = 0 to (length-1) do
    let _ = (add_element_to_table table (List.nth lst i)) in ()
  done
  let max_count = 0 in
iterate on hashtable, look for largest value


  Hashtbl.iter  *)

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

(* calculate which player is killed, based on user input or bot actions*)
let parse_kill (kills:Server_player.t list) = 
  if List.length kills = 0 then
    mafia_kill (get_game_state !game_state) 
  else
    mafia_kill ~targets:kills (get_game_state !game_state)   
 

(* calculate which player is accused, based on user input or bot actions*)
let parse_accuse accusations = 
  if List.length accusations= 0 then
    accuse (get_game_state !game_state)
  else
    accuse ~targets:accusations (get_game_state !game_state)

(* calculate which player is killed, based on user input *)
let parse_vote votes = 
  cast_vote votes (get_game_state !game_state)



(* given a hashtable containing commands from the current round, 
  and the current stage, process state info based on the commands.
  Update the game state's dead/accused/stage fields*)
let parse_client_commands table stage=
  let st = get_game_state !game_state in
    match stage with 
    | Killing -> 
    (* if players doesn't, exist, is current player, or is mafia, fail *)
      let kills = 
        try Hashtbl.find table "kill" 
        with exn -> [] in 
      parse_kill (List.map (get_player st) kills);
      let () = change_stage (get_game_state !game_state) Accusing in 
      (* move killed players to dead *)
      move_dead_to_killed (get_killed_tonight st) st 
    | Accusing -> 
    (* can't accuse self *)
    (* first move dead to killed, print messages, then proceed with stage *)
      let accusations = 
        try Hashtbl.find table "accuse" 
        with exn -> [] in 
      let accused = parse_accuse (List.map (get_player st) accusations) in
      change_accused (get_game_state !game_state) accused; 
      change_stage (get_game_state !game_state) Voting
    | Voting -> 
    (* should be yay or nay *)
      let votes = 
        try Hashtbl.find table "vote" 
        with exn -> [] in 
      if parse_vote votes then  
        change_stage (get_game_state !game_state) Killing
      else 
        change_stage (get_game_state !game_state) Accusing

   
  (* in 
   

let num_played = _
let acitve players = _
let while loop to limit time players have *)






(* =====================================server code============================== *)






(* run the server, connecting on socket [port] and running [server_func] *)
let main_server port server_func = 
  try
    let my_address = (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0) in
    let sock = (Unix.ADDR_INET(my_address, port)) in
    server_func sock
  with
    Failure("wrong port") ->
      Printf.eprintf "starting server: bad port number\n"

 
(* read data from client file_descr *)
let rec read_client len c_descr = 
    try
      let acc = Bytes.make len ' ' in
      let _ = Unix.read c_descr acc 0 len in Bytes.to_string  acc
    with Unix.Unix_error(Unix.EAGAIN, "read", "") -> ""


(* read input from all clients, limit to 100 length *)
let read_all = 
  Array.map (read_client 100) !clients 


(* convert commands into a tuple. 
  Command_Str should be properly formatted commands
  as described by commands module *)
let parse_command command_str = 
  match parse command_str with
  | exception Empty -> "", [""]
  | exception Malformed-> print_endline "should have caught malformed expression"; "", [""]
  | Kill n -> "kill", n
  | Accuse a when a = ["nobody"] -> "", [""]
  | Accuse a -> "accuse", a
  | Vote v -> "vote", v
  | _ -> failwith "invalid command"


(* read input from all clients, limit to 100 length, return inputs in a hashtable *)
let read_commands s_descr= 
(* block while reading commands *)
  clear_nonblock s_descr;

  let commands_table = Hashtbl.create 15 in 

  let () =
  let current_time = time() in 
    (*give 10 second window for commands *)

  while time() -. current_time < 10. do       

      (* set_nonblock s_descr; *)
      (* establish connections *)
      for i = 0 to (Array.length !clients)-1 do
        try      
          let sock = Array.get !clients i in
          let command_str = read_client 100 sock in
          let () = print_endline command_str in
          let command, opt = parse_command command_str in
          let current_opts = 
            (* get entries already in hash table if they exist *)
            if Hashtbl.mem commands_table command then 
                Hashtbl.find commands_table command 
            else []
          in
          (* add current command to  recieved *)
          Hashtbl.add commands_table command (current_opts@opt)
        with
        | Unix.Unix_error(Unix.EAGAIN, read, "") -> print_endline "can't read command" 
        | exn -> print_endline "send failed"; raise exn
      done 
  done 
  in
  let _ = set_nonblock s_descr in
  commands_table



(* write message to a client *)
let write_client message client =
  let _ = (Unix.write_substring client message 0 (String.length message)) in ()

(* write messsages to all clients *)
let write_all message =
  Array.iter (write_client message) !clients


(* send message to all clients *)
let send_to_all state=
  let message = json_of_state state in
  for i = 0 to (Array.length !clients)-1 do
    (* wait 10 seconds *)
    (* let current_time = time() in           
      while time() -. current_time < 10. do   *)
        try      
          let sock = Array.get !clients i in
          (* send player json to client *)
          write_client message sock; 

          let return_message = read_client 100 sock in
          print_endline return_message;

      with
        | Unix.Unix_error(Unix.EAGAIN, read, "") -> print_endline "can't send"
        | exn -> print_endline "send failed"; raise exn

      (* done *)
      (* open output channel to get name? - for game play*)
  done 

(* generate a player for each connected client *)
let create_players () =
  (* for each client, prompt for name *)
  for i = 0 to (Array.length !clients)-1 do
    (* wait 10 seconds *)
    (* let current_time = time() in           
      while time() -. current_time < 10. do   *)
        try      
          let sock = Array.get !clients i in
          print_endline ("recieving messsage from client #"^string_of_int i);
          (* prompt player for name *)

          (* read name from client *)
          let name = read_client 100 sock in
          if String.length name > 0 then 
            (* let role = (get_rnd_role (Random.int 2)) in *)
            let role = Mafioso Mafia in
            let new_player = make_player name role in
            (* add to list of players *)
            players := !players@[new_player];
            print_endline (string_of_int (List.length !players));
            (* convert player to json  *)
            let json_player = json_of_player new_player in
            (* send player json to client *)
            write_client json_player sock; 
          else ()

      with
        | Unix.Unix_error(Unix.EAGAIN, read, "") -> ()

      (* done *)
      (* open output channel to get name? - for game play*)
  done 




(* establish client connections
    read from clients, send new state to clients *)
let server_service sockaddr = 
  let domain = domain_of_sockaddr sockaddr in
    let s_sock = Unix.socket domain Unix.SOCK_STREAM 0 in
      (* bind socket to address  *)
      try 
        Unix.set_nonblock s_sock;
        let () = Unix.bind s_sock sockaddr in
            Unix.listen s_sock 15;
        let current_time = time() in 
        let count = ref 0 in
        
          (* establish connections *)
          while time() -. current_time < 30. do
            try 
              (* accept clients trying to bind on socket s_sock *)
              let (sock, client) = Unix.accept s_sock in
              (* let _ = write_client ("connection #: "^string_of_int !count) sock in *)
              let _ = write_client "\nWelcome to Functional Town\n What is your name?" sock 
              in
              (* store new client dscriptor in clients *)
              clients := Array.append !clients [|sock|];
              count := Array.length !clients
            with Unix.Unix_error(Unix.EAGAIN, "accept", "") -> ()
          done; 
          print_endline "in second loop";

        (* block while getting user names *)
        Unix.clear_nonblock s_sock;          
        create_players ();
        (* create intial game state *)
        let bots = generate_players 3 9 in
        (* let bot_names = get_player_names bots name in *)
        let initial_st = init_state !players bots in 
        let () = change_game_state game_state initial_st in 
        (* broadcast intial state *)
        send_to_all initial_st; 
        (* non block while waiting for users *)
        Unix.set_nonblock s_sock;  

        while true do

          let current_time = time() in 
            (* listen for ten seconds *)
          let command_table = read_commands s_sock in
 
          parse_client_commands command_table (current_stage (get_game_state !game_state));
          write_all (json_of_state (get_game_state !game_state));

        done 
      with exn -> close s_sock; raise exn

let start_server port = Unix.handle_unix_error main_server port server_service

let start_host () : unit = 
  (*print_endline "\nPlease enter your name.";
  print_string  "> ";
  let player_name = read_line() in*)
  (* print_endline "How many players do you want to play with? (please enter a number)\n";
  print_string "> ";
  let num_players = read_line () in *)
  let port = 1400 in

  start_server port
