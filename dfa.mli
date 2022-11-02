(** Representation of DFAs and implementation of standard operations *)

type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; 
    alphabet: string list; 
    transitions: (state * string * state) list; 
    start: state; accepting: state list
}

(** [create q al t s f] returns the DFA of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f].
    Note that states will be renamed to integer list. Sink state will be added to make transition function [t] total.
    @raise Invalid_argument if [s] is not a valid state in [q]
    @raise Invalid_argument if [f] is not a valid subset of [q]
    @raise Invalid_argument if [t] contains epsilon-transitions.
    @raise Invalid_argument if [t] is not a valid tranition function for states [qs] and alphabet [al]
*)
val create : 'a list -> string list -> ('a * string * 'a) list -> 'a -> 'a list -> dfa

(** [compliment m] returns the compliment of DFA [m] *)
val compliment : dfa -> dfa

(** [succ m s w] returns the successor state of DFA [m] after reading word [w] from state [s] *)
val succ : dfa -> state -> string -> state

(** [pred m s] returns a list of states that preceed the state [s] in DFA [m] *)
val pred : dfa -> state -> state list

(** [prune m] reduces DFA [m] by removing unreachable states *)
val prune : dfa -> dfa

(** [is_empty m] returns true iff DFA [m] is empty *)
val is_empty : dfa -> bool

(** [accepts m s] returns true iff DFA [m] accepts string [s] *)
val accepts : dfa -> string -> bool

(** [accepted m] returns the shortest string accepted by DFA [m] *)
val accepted : dfa -> string option

(** [product_insterection m1 m2] returns the intersection of DFAs [m1] [m2], by the product construction *)
val product_intersection : dfa -> dfa -> dfa

(** [product_union m1 m2] returns the union of DFAs [m1] [m2], by the product construction *)
val product_union : dfa -> dfa -> dfa

(** [is_equiv m1 m2] returns true if the two DFAs [m1] and [m2] are equivalent, by equivalence closure *)
val is_equiv : dfa -> dfa -> bool

(** [nfa_to_dfa_subset m] returns the NFA equivalent to DFA [m], by the subset construction *)
val nfa_to_dfa_subset : Nfa.nfa -> dfa

(** [nfa_to_dfa m] returns the NFA equivalent to DFA [m], by an optimised subset construction *)
val nfa_to_dfa : Nfa.nfa -> dfa

(** [print n] prints a string representation of the DFA [n] *)
val print : dfa -> unit