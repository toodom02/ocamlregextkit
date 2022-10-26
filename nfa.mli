(** Representation of NFAs and implementation of standard operations *)

type state = int
type nfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

(** [re_to_nfa r] returns an NFA constructed from the RE r *)
val re_to_nfa : Re.re -> nfa

(** [eps_reachable_set n ss] returns the set of all epsilon reachable states in the NFA [n] from the set of states [ss] *)
val eps_reachable_set : nfa -> state list -> state list

(** [succ n s w] returns a list of successor states of NFA [n] after reading word [w] from state [s] *)
val succ : nfa -> state -> string -> state list

(** [pred n s] returns a list of states that preceed the state [s] in NFA [n]  *)
val pred : nfa -> state -> state list

(** [merge_alphabets n1 n2] returns a tuple of NFAs [(n1', n2')] such that [n1'] and [n2'] both share a common alphabet *)
val merge_alphabets : nfa -> nfa -> (nfa * nfa)

(** [print n] prints a string representation of the NFA [n] *)
val print : nfa -> unit