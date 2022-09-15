(* |print_ast| -- prints string representation of regex ast *)
val print_ast : Ast.re -> unit

(* |print_nfa| -- prints out nfa representation *)
val print_nfa : Nfa.nfa -> unit

(* |print_dfa| -- prints out dfa representation, formed by subset construction *)
val print_dfa : Dfa.dfa -> unit