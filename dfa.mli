type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; 
    alphabet: string list; 
    transitions: (state * string * state) list; 
    start: state; accepting: state list
}

(* |nfa_to_dfa_subset| -- converts nfa to dfa by the subset construction *)
val nfa_to_dfa_subset : Nfa.nfa -> dfa

(* |nfa_to_dfa| -- converts nfa to dfa by optimised subset construction *)
val nfa_to_dfa : Nfa.nfa -> dfa

(* |reduce_dfa| -- reduces input dfa by removing unreachable states *)
val reduce_dfa : dfa -> dfa

(* |find_unique_word| -- finds a word accepted by the first DFA but not the second *)
val find_unique_word : dfa -> dfa -> string option