(** Representation of NFAs and implementation of standard operations *)

type state = int
type nfa = {
    states: state list;
    alphabet: string list;
    transitions: (state * string * state) list;
    start: state;
    accepting: state list
}

(** [create q al t s f] 
    @return the NFA of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f].
    Note that states will be renamed to integers.
    @raise Invalid_argument if [s] is not a valid state in [q]
    @raise Invalid_argument if [f] is not a valid subset of [q]
    @raise Invalid_argument if [t] is not a valid tranition function for states [qs] and alphabet [al]
*)
val create : 'a list -> string list -> ('a * string * 'a) list -> 'a -> 'a list -> nfa

(** [re_to_nfa r] 
    @return an NFA constructed from the RE [r] *)
val re_to_nfa : Tree.re -> nfa

(** [eps_reachable_set n ss] 
    @return the set of all epsilon reachable states in the NFA [n] from the set of states [ss] *)
val eps_reachable_set : nfa -> state list -> state list

(** [reachable_states n]
    @return the set of reachable (connected) states in the NFA [n] *)
val reachable_states : nfa -> state list

(** [succ n s w] 
    @return a list of successor states of NFA [n] after reading word [w] from state [s] *)
val succ : nfa -> state -> string -> state list

(** [pred n s] 
    @return a list of states that preceed the state [s] in NFA [n]  *)
val pred : nfa -> state -> state list

(** [prune n] 
    @return a reduction of NFA [n] by removing unreachable states *)
val prune : nfa -> nfa

(** [is_empty n] 
    @return true iff NFA [n] is empty *)
val is_empty : nfa -> bool

(** [accepts n s] 
    @return true iff NFA [n] accepts string [s] *)
val accepts : nfa -> string -> bool
    
(** [accepted n] 
    @return the shortest string accepted by NFA [n] *)
val accepted : nfa -> string option

(** [merge_alphabets n1 n2] 
    @return a tuple of NFAs [(n1', n2')] such that [n1'] and [n2'] both share a common alphabet *)
val merge_alphabets : nfa -> nfa -> (nfa * nfa)

(** [print n] prints a string representation of the NFA [n] *)
val print : nfa -> unit

(** [export_graphviz n] 
    @return a representation of the NFA [n] in the DOT language for Graphviz *)
val export_graphviz : nfa -> string