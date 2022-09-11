open Nfa
open Dfa
open Ast

(* |print_ast| -- prints string representation of regex ast *)
let print_ast re = 
  let rec stringify_ast re = 
    match re with
          Literal a -> "Literal " ^ a
        | Epsilon -> "ε"
        | Union (r1, r2) -> "Union (" ^ stringify_ast r1 ^ " , " ^ stringify_ast r2 ^ ")"
        | Concat (r1, r2) -> "Concat (" ^ stringify_ast r1 ^ " , " ^ stringify_ast r2 ^ ")"
        | Star r1 -> "Star (" ^ stringify_ast r1 ^ ")"
        | Empty -> "∅"
  in
  print_string (stringify_ast re);  print_newline ()

(* |print_nfa| -- prints out nfa representation *)
let print_nfa (n:nfa) = 
  print_string "states: "; List.iter (fun s -> print_int s; print_char ' ') n.states; print_newline ();
  print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
  print_string "start: "; print_int n.start; print_newline ();
  print_string "accepting: "; List.iter (fun s -> print_int s; print_char ' ') n.accepting; print_newline ();
  print_string "transitions: "; print_newline(); List.iter (fun (s,a,t) -> print_string "    "; print_int s; print_string ("\t--"^a^"-->\t"); print_int t; print_newline ()) n.transitions;

  
(* |print_dfa| -- prints out dfa representation, formed by subset construction *)
and print_dfa (n:dfa) = 
  let rec print_state = function
      | State n -> print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') n; print_string "]"
      | ProductState (l,r) -> print_string "( "; print_state l; print_string " , "; print_state r; print_string " )";
  in
  print_string "states: "; List.iter (fun ss -> print_state ss) n.states; print_newline ();
  print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
  print_string "start: "; print_state n.start; print_newline ();
  print_string "accepting: "; List.iter (fun ss -> print_state ss) n.accepting; print_newline ();
  print_string "transitions: "; print_newline (); List.iter (fun (ss,a,tt) -> print_string "    "; print_state ss; print_string ("\t--"^a^"-->\t"); print_state tt; print_newline ()) n.transitions;