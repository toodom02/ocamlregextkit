type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; 
    alphabet: string list; 
    transitions: (state * string * state) list; 
    start: state; accepting: state list
}

(* |nfa_to_dfa| -- converts nfa to dfa by the subset construction *)
val nfa_to_dfa : Nfa.nfa -> dfa

(* |reduce_dfa| -- reduces input dfa by removing unreachable states *)
val reduce_dfa : dfa -> dfa

(* |is_dfa_equal| -- returns Some(x) if x exists in one dfa but not the other *)
val is_dfa_equal : dfa -> dfa -> string option