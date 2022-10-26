(** Representation of NFAs and implementation of standard operations *)

type state = int
type nfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

(** [re_to_nfa r] returns an NFA constructed from the RE r *)
val re_to_nfa : Re.re -> nfa

(** [merge_alphabets n1 n2] returns a tuple of NFAs [(n1', n2')] such that [n1'] and [n2'] both share a common alphabet *)
val merge_alphabets : nfa -> nfa -> (nfa * nfa)

(** [eps_reachable_set ss n] returns the set of all epsilon reachable states in the NFA [n] from the set of states [ss] *)
val eps_reachable_set : state list -> nfa -> state list

(** [print n] prints a string representation of the NFA [n] *)
val print : nfa -> unit