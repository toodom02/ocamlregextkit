(** Representation of NFAs and implementation of standard operations *)

type state = int
type nfa = state Adt.automata

(** [get_states n]
    @return a list of all states in NFA [n] *)
val get_states : nfa -> state list

(** [get_alphabet n]
    @return the alphabet of NFA [n] as a list *)
val get_alphabet : nfa -> string list

(** [get_transitions n]
    @return the transition function of NFA [n] as a list of tuples [(s,a,t)] *)
val get_transitions : nfa -> (state * string * state) list

(** [get_start n]
    @return the initial state of NFA [n] *)
val get_start : nfa -> state

(** [get_accepting n]
    @return a list of all accepting states in NFA [n] *)
val get_accepting : nfa -> state list

(** [is_accepting n s]
    @return true iff state [s] is an accepting state of NFA [n] *)
val is_accepting : nfa -> state -> bool

(** [succ n s w]
    @return a list of successor states of NFA [n] after reading word [w] from state [s] *)
val succ : nfa -> state -> string -> state list

(** [pred n s]
    @return a list of states that preceed the state [s] in NFA [n] *)
val pred : nfa -> state -> state list

(** [create q al t s f]
    @return
      the NFA of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f].
      Note that states will be renamed to integers.
    @raise Invalid_argument if [s] is not a valid state in [q]
    @raise Invalid_argument if [f] is not a valid subset of [q]
    @raise Invalid_argument
      if [t] is not a valid tranition function for states [qs] and alphabet [al] *)
val create : 'a list -> string list -> ('a * string * 'a) list -> 'a -> 'a list -> nfa

(** [copy n]
    @return a deep copy of NFA [n] *)
val copy : nfa -> nfa

(** [re_to_nfa r]
    @return an NFA constructed from the RE [r] *)
val re_to_nfa : Tree.re -> nfa

(** [eps_reachable_set n ss]
    @return
      the set of all epsilon reachable states in the NFA [n] from the set of states [ss] *)
val eps_reachable_set : nfa -> state list -> state list

(** [reachable_states n]
    @return the set of reachable (connected) states in the NFA [n] *)
val reachable_states : nfa -> state list

(** [prune n]
    mutates DFA [n] by removing unreachable states *)
val prune : nfa -> unit

(** [is_empty n]
    @return true iff NFA [n] is empty *)
val is_empty : nfa -> bool

(** [is_accepted n s]
    @return true iff NFA [n] accepts string [s] *)
val is_accepted : nfa -> string -> bool

(** [get_accepted n]
    @return the shortest string accepted by NFA [n] *)
val get_accepted : nfa -> string option

(** [merge_alphabets n1 n2]
    mutates NFAs [n1] and [n2] such that they share a common alphabet *)
val merge_alphabets : nfa -> nfa -> unit

(** [print n] prints a string representation of the NFA [n] *)
val print : nfa -> unit

(** [export_graphviz n]
    @return a representation of the NFA [n] in the DOT language for Graphviz *)
val export_graphviz : nfa -> string
