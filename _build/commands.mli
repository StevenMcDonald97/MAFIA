(** This is used from A2 starter code
    The type [object_phrase] represents the object phrase that can be part of a 
    player command.  Each element of the list represents a word of the object 
    phrase, where a {i word} is defined as a consecutive sequence of non-space 
    characters.  Thus, no element of the list should contain any leading,
    internal, or trailing spaces.  The list is in the same order as the words 
    in the original player command.  For example:
    - If the player command is ["vote John"], then the object phrase is 
      [["vote"; "John"]].
    - If the player command is ["accuse     Bill"], then the object phrase is
      again [["accuse"; "Bill"]]. 

    An [object_phrase] is not permitted to be the empty list. *)
type object_phrase = string list


(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
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


(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** Documentation taken from A2 starter code 
    [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    kill Bill  "] is [Kill ["Bill"]].
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "go",
    or if the verb is "quit" and there is a non-empty object phrase,
    or if the verb is "go" and there is an empty object phrase.*)
val parse : string -> command

(** [remove_space lst] is the a list of strings that does not contain any empty
    strings, which represent a space given the output condition from 
    [String.split_on_char], which could possibly return an empty list.
    Examples:
    - [remove_space []] is [[]].
    - [remove_space ["go", "clock", "tower"]] is [["go", "clock", "tower"]].
    - [remove_space ["", "go", "", "clock", "", "tower"]] is 
        [["go", "clock", "tower"]].

    - Requires: lst is a string list.
*)

val remove_space : string list -> string list

(** [inner_match lst pattern1 patter2] is the match to the pattern of whether
    or not [lst] is empty.  *)
val inner_match : 'a list -> 'b -> 'b -> 'b


