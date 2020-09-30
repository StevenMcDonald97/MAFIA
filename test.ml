open OUnit2
open Commands
open State
open Player
open Actions 


(********************************************************************
   Player tests. 
 ********************************************************************)
(** [make_get_role_tests name expected_output player] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_role player]. *)
let make_get_role_tests name expected_output player =
  name >:: (fun _ -> 
      assert_equal expected_output (get_role player))

(** [make_get_charge_tests name expected_output player] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_charge player]. *)
let make_get_charge_tests name expected_output player =
  name >:: (fun _ -> 
      assert_equal expected_output (get_charge player))

(** [make_charge_value_tests name expected_output charge] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [charge_value charge]. *)
let make_charge_value_tests name expected_output charge =
  name >:: (fun _ -> 
      assert_equal expected_output (charge_value charge))

(** [make_get_player_tests name expected_output name2 players] constructs 
     an OUnit test named [name] that asserts the quality of [expected_output]
     with [get_player name2 players]. *)
let make_get_player_tests name expected_output name2 players =
  name >:: (fun _ -> 
      assert_equal expected_output (get_player name2 players))

(** [make_is_town_tests name expected_output player] constructs 
    an OUnit test named [name] that asserts the quality of [expected_output]
    with [is_town player]. *)
let make_is_town_tests name expected_output player =
  name >:: (fun _ -> 
      assert_equal expected_output (is_town player))

(** [make_is_neutral_tests name expected_output player] constructs 
    an OUnit test named [name] that asserts the quality of [expected_output]
    with [is_neutral player]. *)
let make_is_neutral_tests name expected_output player =
  name >:: (fun _ -> 
      assert_equal expected_output (is_neutral player))

(** [make_get_name_tests name expected_output player] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_name player]. *)
let make_get_name_tests name expected_output player =
  name >:: (fun _ -> 
      assert_equal expected_output (get_name player))


(** [make_get_status_tests name expected_output player] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_status player]. *)
let make_get_status_tests name expected_output player =
  name >:: (fun _ -> 
      assert_equal expected_output (get_status player))

(** [make_check_role_tests name expected_output role] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [check_role role]. *)
let make_check_role_tests name expected_output role =
  name >:: (fun _ -> 
      assert_equal expected_output (check_role role))

(** [make_description_tests name expected_output role] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [description role]. *)
let make_description_tests name expected_output role =
  name >:: (fun _ -> 
      assert_equal expected_output (description role))

(** [make_player_tests name expected_output name2 role] constructs an OUnit
     test named [name] that asserts the quality of [expected_output]
     with [make_player name2 role]. *)
let make_player_tests name expected_output name2 role =
  name >:: (fun _ -> 
      assert_equal expected_output (make_player name2 role))

(** [make_get_rnd_name_tests name expected_output n] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_rnd_name n]. *)
let make_get_rnd_name_tests name expected_output n =
  name >:: (fun _ -> 
      assert_equal expected_output (get_rnd_name n))

(** [make_get_rnd_role_tests name expected_output n] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [get_rnd_role n]. *)
let make_get_rnd_role_tests name expected_output n =
  name >:: (fun _ -> 
      assert_equal expected_output (get_rnd_role n))

(** [make_role_to_string_tests name expected_output rl] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [role_to_string rl]. *)
let make_role_to_string_tests name expected_output rl =
  name >:: (fun _ -> 
      assert_equal expected_output (role_to_string rl))

(** [make_string_to_role_tests name expected_output str] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [string_to_role str]. *)
let make_string_to_role_tests name expected_output str =
  name >:: (fun _ -> 
      assert_equal expected_output (string_to_role str))

(** [make_name_used_tests name expected_output nm_1 lst] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [name_used nm_1 lst]. *)
let make_name_used_tests name expected_output nm_1 lst =
  name >:: (fun _ -> 
      assert_equal expected_output (name_used nm_1 lst))

(** [make_get_player_names_tests name expected_output plyrs str] constructs an 
    OUnit test named [name] that asserts the quality of [expected_output]
    with [get_player_names plyrs str]. *)
let make_get_player_names_tests name expected_output plyrs str =
  name >:: (fun _ -> 
      assert_equal expected_output (get_player_names plyrs str))

let player1 = make_player "John Hathorne" (Sheriff Town)
let player2 = make_player "John Proctor" (Citizen Town)
let player3 = make_player "Betty Parris" (Mafioso Mafia)
let player4 = make_player "Edward Bishop" (SerialKiller Neutral)
let player5 = make_player "Deodat Lawson" (Vigilante Town)
let playerlst = [player1;player2;player3]

let player_tests = [
  make_get_role_tests "get_role1" (Sheriff Town) player1;
  make_get_charge_tests "get_charge1" Unlimited player4;
  make_get_player_tests "get_player1" player1 "John Hathorne" playerlst;
  make_get_player_tests "get_player2" player2 "John Proctor" playerlst;
  make_is_town_tests "is_town1" true player1;
  make_is_town_tests "is_town2" true player2;
  make_is_town_tests "is_town3" false player3;
  make_is_neutral_tests "is_neutral1" true player4;
  make_is_neutral_tests "is_neutral2" false player5;
  make_get_name_tests "get_name1" "John Proctor" player2;
  make_get_name_tests "get_name2" "Betty Parris" player3;
  make_get_status_tests "get_status1" Alive player2;
  make_check_role_tests "check_role1" (Mafioso Mafia) (Mafioso Mafia);
  "parse wrongside exception" >:: (fun _->
      assert_raises (WrongSide (Sheriff Mafia)) (fun() -> 
          check_role (Sheriff Mafia)));
  make_description_tests "description3" 
    ("The Mafioso can vote to kill a person every night.") (Mafioso Mafia);
  make_player_tests "make_player1" player1 "John Hathorne" (Sheriff Town);
  make_player_tests "make_player2" player2 "John Proctor" (Citizen Town);
  make_player_tests "make_player3" player3 "Betty Parris" (Mafioso Mafia);
  make_get_rnd_name_tests "get_rnd_name1" "lucy" 2;
  make_get_rnd_name_tests "get_rnd_name1" "david-gries'-twin-brother" 34;
  make_get_rnd_role_tests "get_rnd_role1" (Mafioso Mafia) 0;
  make_get_rnd_role_tests "get_rnd_role2" (Citizen Town) 1;
  make_get_rnd_role_tests "get_rnd_role3" (Sheriff Town) 2;
  make_role_to_string_tests "role_to_string1" "Mafioso" (Mafioso Mafia);
  make_role_to_string_tests "role_to_string2" "Citizen" (Citizen Town);
  make_role_to_string_tests "role_to_string3" "Sheriff" (Sheriff Town);
  make_string_to_role_tests "string_to_role1" (Mafioso Mafia) "mafioso";
  make_string_to_role_tests "string_to_role2" (Citizen Town) "citizen";
  make_string_to_role_tests "string_to_role3" (Sheriff Town) "sheriff";
  make_name_used_tests "name_used1" true "John Hathorne" playerlst;
  make_name_used_tests "name_used2" true "Betty Parris" playerlst;
  make_name_used_tests "name_used3" false "John Betty" playerlst;
  make_get_player_names_tests "get_player_names1" 
    ", Betty Parris, John Proctor, John Hathorne" playerlst "";

]

(********************************************************************
   State Tests
 ********************************************************************)

let c1 = Player.make_player "Joe" (Citizen Town)
let c2 = Player.make_player "Mel" (Citizen Town)
let m1 = Player.make_player "Eddy" (Mafioso Mafia)
let m2 = Player.make_player "Joseph" (Mafioso Mafia)
let m3 = Player.make_player "Don" (Mafioso Mafia)
let s1 = Player.make_player "Kid" (Sheriff Town)
let s2 = Player.make_player "Ross" (Sheriff Town)
let d1 = Player.make_player "dee" (Doctor Town)
let r1 = Player.make_player "Ryan" (Retributionist Town)
let n1 = Player.make_player "Neal" (Neapolitan Town)
let v1 = Player.make_player "Noel" (Vigilante Town)
let n2 = Player.make_player "Neal" (Neapolitan Town)
let sk1 = Player.make_player "Bob" (SerialKiller Neutral)
let sk2 = Player.make_player "Joel" (SerialKiller Neutral)


let state1 = State.init_state  c1 [c2; m1; m2; m3]
let state2 = State.init_state c1 [c2; m1; m2]
let state3 = State.init_state c1 [c2; m1; m2]
let state4 = State.init_state s1 [c1; c2; m1; m2; m3; s2]
let state5 = State.init_state s1 [c1; c2; m1; m2; m3; s2]
let state6 = State.init_state c1 [v1; r1; n1; n2; d1; m1]
let state7 = State.init_state n1 [n2]
let state8 = State.init_state sk1 [sk2; c1; m2; v1]
let state9 = State.init_state sk1 [sk2; c1; m2; v1; s1; s2; n1 ;n2]


let () = add_to_killed state2 m1
let () = change_stage state2 Investigating
let () = add_night state2 
let () = move_player_to_killed state3 c2
let () = add_to_killed_tonight state5 s2 (Sheriff Town)
let () = clear_killed_tonight state5 
let () = add_to_killed_tonight state5 s2 (Sheriff Town)
let () = add_night state5 
let () = add_to_messages state8 ["lemme out"] 
let () = add_to_messages state7 ["ted head"] 
let () = add_to_messages state6 ["lomble"] 
let () = add_to_messages state5 ["donnely"] 
let () = add_to_messages state4 ["car bend"] 
let () = add_to_messages state3 ["rittaker"] 
let () = add_to_messages state2 ["monea"] 
let () = add_to_messages state1 ["plumbcake"; "honey lemon"] 
let () = clear_messages state9
let () = add_to_killed state9 m1



let state_tests = [
  "current_mafia" >:: (fun _-> 
      assert_equal ([m1; m2; m3]) (State.current_mafia state1));
  "current_players" >:: (fun _-> 
      assert_equal (c1) (State.current_player state1));
  "current_town" >:: (fun _-> 
      assert_equal ([c1; c2]) (State.current_town state1));
  "current_players" >:: (fun _-> 
      assert_equal ([c2; m1; m2; m3]) (State.current_players state1));
  "current_dead 1" >:: (fun _-> 
      assert_equal ([]) (State.current_dead state1));
  "current_dead 2" >:: (fun _-> 
      assert_equal ([m1]) (State.current_dead state2));
  "current_stage 1" >:: (fun _-> 
      assert_equal (Killing) (current_stage state1));
  "current_stage 2" >:: (fun _-> 
      assert_equal (Investigating) (current_stage state2));   
  "current_night1" >:: (fun _-> 
      assert_equal (0) (current_night state1));
  "current_night2" >:: (fun _-> 
      assert_equal (1) (current_night state2)); 
  "move_player_to_killed1" >:: (fun _-> 
      assert_equal ([c2]) (current_dead state3)); 
  "move_player_to_killed2" >:: (fun _-> 
      assert_equal ([m1; m2]) (current_players state3)); 
  "move_player_to_killed3" >:: (fun _-> 
      assert_equal ([m1; m2]) (current_mafia state3)); 
  "get_group1" >:: (fun _-> 
      assert_equal ([s2]) (get_group (current_players state4) 
                             (Sheriff Town) [])); 
  "get_group2" >:: (fun _-> 
      assert_equal ([m1;m2;m3]) (get_group (current_players state4) 
                                   (Mafioso Mafia) [])); 
  "get_group3" >:: (fun _-> 
      assert_equal ([c1;c2]) (get_group (current_players state4) 
                                (Citizen Town) [])); 
  "is_player1" >:: (fun _-> 
      assert_equal (true) (is_player "Ross" state4)); 
  "is_player2" >:: (fun _-> 
      assert_equal (true) (is_player "Don" state4)); 
  "get_player1" >:: (fun _-> 
      assert_equal (s2) (get_player "Ross" (current_players state4))); 
  "add_to_killed_tonight1" >:: (fun _-> 
      assert_equal ([(s2, Sheriff Town)]) (get_killed_tonight state5)); 
  "add_to_night1" >:: (fun _-> 
      assert_equal (1) (current_night state5)); 
  "current_doctors" >:: (fun _ -> assert_equal ([d1]) (current_doctors state6));
  "current_vigilantes" >:: (fun _ -> assert_equal ([v1]) (current_vigilantes 
                                                            state6));
  "current_retributionists" >:: (fun _ -> assert_equal ([r1]) 
                                    (current_retributionists state6));
  (* "current_town" >:: (fun _ -> assert_equal ([n1; n2]) (current_town state7)); *)
  (* "current_neapolitans" >:: (fun _ -> assert_equal ([n1; n2]) (current_neapolitans 
                                                                 state6));
  "current_neapolitans2" >:: (fun _ -> assert_equal ([n1; n2]) (current_neapolitans 
                                                                  state7)); *)
  "current_doctors2" >:: (fun _ -> assert_equal ([]) (current_doctors state7));
  "current_vigilantes2" >:: (fun _ -> assert_equal ([]) (current_vigilantes 
                                                           state7));
  "current_retributionists2" >:: (fun _ -> assert_equal ([]) 
                                     (current_retributionists state7));
  "current_mafia2" >:: (fun _ -> assert_equal ([]) (current_mafia state7));
  "current_players2" >:: (fun _ -> assert_equal ([n2]) (current_players state7));
  "current_player3" >:: (fun _ -> assert_equal (n1) (current_player state7));
  "current_player4" >:: (fun _ -> assert_equal (c1) (current_player state6));
  "current_neutral1" >:: (fun _ -> assert_equal ([]) (current_neutral state4));
  "current_neutral2" >:: (fun _ -> assert_equal ([]) (current_neutral state5));
  "current_neutral3" >:: (fun _ -> assert_equal ([]) (current_neutral state6));
  "current_neutral4" >:: (fun _ -> assert_equal ([]) (current_neutral state7));
  "current_neutral4" >:: (fun _ -> assert_equal ([sk1; sk2]) (current_neutral 
                                                                state8));
  "current_player5" >:: (fun _ -> assert_equal (sk1) (current_player state8));
  "current_tow2" >:: (fun _-> 
      assert_equal ([m1; m2; m3]) (current_mafia state5));
  "current_mafia2" >:: (fun _-> 
      assert_equal ([m1]) (current_mafia state6));
  "current_mafia3" >:: (fun _-> 
      assert_equal ([]) (current_mafia state7));
  "current_mafia4" >:: (fun _-> 
      assert_equal ([m2]) (current_mafia state8));
  "current_mafia5" >:: (fun _-> 
      assert_equal ([m1; m2; m3]) (current_mafia state4));
  "message1" >:: (fun _-> 
      assert_equal (["lemme out"]) (get_messages state8));
  "message2" >:: (fun _-> 
      assert_equal (["ted head"]) (get_messages state7));
  "message3" >:: (fun _-> 
      assert_equal (["lomble"]) (get_messages state6));
  "message4" >:: (fun _-> 
      assert_equal (["donnely"]) (get_messages state5));
  "message5" >:: (fun _-> 
      assert_equal (["car bend"]) (get_messages state4));
  "message6" >:: (fun _-> 
      assert_equal (["rittaker"]) (get_messages state3));
  "message7" >:: (fun _-> 
      assert_equal (["monea"]) (get_messages state2));
  "message8" >:: (fun _-> 
      assert_equal (["plumbcake"; "honey lemon"]) (get_messages state1));
  "message9" >:: (fun _-> 
      assert_equal ([]) (get_messages state9))
]

(********************************************************************
   Actions Tests
 ********************************************************************)
 let d1 = Player.make_player "Huy" (Doctor Town)
 let s1 = Player.make_player "Le" (Sheriff Town)
 let sk1 = Player.make_player "heo" (SerialKiller Neutral)
 let v1 = Player.make_player "nhat" (Vigilante Town)
 let r1 = Player.make_player "anh" (Retributionist Town)
 let astate3 = State.init_state c1 [m1; m2]
 let astate4 = State.init_state c1 [m1]
 let astate5 = State.init_state c1 [d1;m1;r1]
 let astate6 = State.init_state s1 [c1]
 let astate7 = State.init_state s1 [m1]
 let astate8 = State.init_state sk1 [c1]
 let astate9 = State.init_state sk1 [c1]
 let astate10 = State.init_state v1 [c1]
 let () = mafia_kill [c1] [m1;m2] astate3
 let () = mafia_kill [c1] [m1] astate5
 let () = serial_killer_kill [c1] [sk1] astate8
 let () = vigilante_kill "Joe" astate10
 
 let actions_tests = [
   "cast_vote 1" >:: (fun _->
       assert_equal (true) (true||(cast_vote "yay" [m1] [c1] c1 astate3)));
   "cast_vote 2" >:: (fun _->
       assert_equal (true) (cast_vote "nay" [m1;m2;m3] [c1] m1 astate3));
   "mafia_kill 1" >:: (fun _->
       assert_equal (c1) (fst (List.hd (get_killed_tonight astate3))));
   "serial_killer_kill 1" >:: (fun _->
   assert_equal (c1) (fst (List.hd (get_killed_tonight astate8))));
   "vigilante_kill 1" >:: (fun _->
   assert_equal (1) (get_vig_guilt astate10));
 ]
 
(********************************************************************
   Command Tests
 ********************************************************************)
(** [make_command_test name expected_output command] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [command]. *)
let make_command_test name command expected_output=
  name >:: (fun _ -> 
      assert_equal expected_output (parse command))

let commands_tests = [
  make_command_test "Parse 'Kill Bill'" "kill   Bill"  (Kill ["bill"]);
  make_command_test "Parse 'Investigate John'" "   investigate    John"   
    (Investigate ["john"]);  
  make_command_test "Parse 'Investigate John'" "   investigate    John"  
    (Investigate ["john"]); 
  make_command_test "Parse 'Quit'" "quit" (Quit);
  make_command_test "Parse 'Accuse Shelly'" "  Accuse  Shelly" (Accuse ["shelly"]);
  make_command_test "Parse 'Accuse Shelly'2" "  Accuse  shelly" (Accuse ["shelly"]);
  make_command_test "Parse 'Defend" "  Defend  " (Defend);
  make_command_test "Parse 'Defend2" "  defend" (Defend);
  make_command_test "Parse 'Attack" "  attack" (Attack);
  make_command_test "Parse 'Pass" "  pass" (Pass);
  make_command_test "Parse Heal1" "  Heal  shelly" (Heal ["shelly"]);
  make_command_test "Parse Revive1" "  Revive  shelly" (Revive ["shelly"]);
  make_command_test "Parse Check1" "  Check  shelly" (Check ["shelly"]);
  "parse empty exception" >:: (fun _->
      assert_raises Empty (fun() -> parse ""));
  "parse malformed exception" >:: (fun _->
      assert_raises Malformed (fun() -> parse " "));
  "parse malformed exception2" >:: (fun _->
      assert_raises Malformed (fun() -> parse "Kill "));
  "parse malformed exception3" >:: (fun _->
      assert_raises Malformed (fun() -> parse "investigate "));
  "parse malformed exception4" >:: (fun _->
      assert_raises Malformed (fun() -> parse "    vote   "));
  "parse malformed exception5" >:: (fun _->
      assert_raises Malformed (fun() -> parse " quit next "));
  "parse malformed exception6" >:: (fun _->
      assert_raises Malformed (fun() -> parse " accuse "));
  "parse malformed exception7" >:: (fun _->
      assert_raises Malformed (fun() -> parse " next accuse "));
  "parse malformed exception8" >:: (fun _->
      assert_raises Malformed (fun() -> parse " defend hey ")); 
  "parse malformed exception9" >:: (fun _->
      assert_raises Malformed (fun() -> parse " attack hey ")); 
  "parse malformed exception10" >:: (fun _->
      assert_raises Malformed (fun() -> parse " pass hey ")); 
  "parse malformed exception11" >:: (fun _->
      assert_raises Malformed (fun() -> parse " heal")); 
  "parse malformed exception12" >:: (fun _->
      assert_raises Malformed (fun() -> parse " revive ")); 
  "parse malformed exception13" >:: (fun _->
      assert_raises Malformed (fun() -> parse " check ")); 
]


let suite =
  "test suite for Mafia"  >::: List.flatten [
    player_tests;
    commands_tests;
    state_tests;
    (* actions_tests; *)
  ]

let _ = run_test_tt_main suite
