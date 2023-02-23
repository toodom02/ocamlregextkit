(** Representation of DFAs and implementation of standard operations *)

type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; 
    alphabet: string list; 
    transitions: (state * string * state) list; 
    start: state; 
    accepting: state list
}

(** [is_accepting n s] 
    @return true iff state [s] is an accepting state of NFA [n] *)
val is_accepting : dfa -> state -> bool

(** [create q al t s f] 
    @return the DFA of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f].
    Note that states will be renamed to integer list. Sink state will be added to make transition function [t] total.
    @raise Invalid_argument if [s] is not a valid state in [q]
    @raise Invalid_argument if [f] is not a valid subset of [q]
    @raise Invalid_argument if [t] contains epsilon-transitions.
    @raise Invalid_argument if [t] is not a valid tranition function for states [qs] and alphabet [al]
*)
val create : 'a list -> string list -> ('a * string * 'a) list -> 'a -> 'a list -> dfa

(** [complement m] 
    @return the complement of DFA [m] *)
val complement : dfa -> dfa

(** [reachable_states m]
    @return the set of reachable (connected) states in the DFA [m] *)
val reachable_states : dfa -> state list

(** [succ m s w] 
    @return the successor state of DFA [m] after reading word [w] from state [s] *)
val succ : dfa -> state -> string -> state

(** [pred m s w] 
    @return the set of predecessor states of DFA [m] before reading word [w] from state [s] *)
val pred : dfa -> state -> string -> state list

(** [prune m] 
    @return a reduction of DFA [m] by removing unreachable states *)
val prune : dfa -> dfa

(** [is_empty m] 
    @return true iff DFA [m] is empty *)
val is_empty : dfa -> bool

(** [accepts m s] 
    @return true iff DFA [m] accepts string [s] *)
val accepts : dfa -> string -> bool

(** [accepted m] 
    @return the shortest string accepted by DFA [m] *)
val accepted : dfa -> string option

(** [product_insterection m1 m2] 
    @return the intersection of DFAs [m1] [m2], by the product construction *)
val product_intersection : dfa -> dfa -> dfa

(** [product_union m1 m2] 
    @return the union of DFAs [m1] [m2], by the product construction *)
val product_union : dfa -> dfa -> dfa

(** [hopcroft_equiv m1 m2] 
    @return true iff the two DFAs [m1] and [m2] are equivalent, by Hopcroft's algorithm *)
val hopcroft_equiv : dfa -> dfa -> bool

(** [symmetric_equiv m1 m2] 
    @return true iff the two DFAs [m1] and [m2] are equivalent, by symmetric difference *)
val symmetric_equiv : dfa -> dfa -> bool

(** [is_equiv m1 m2] synonym for [hopcroft_equiv m1 m2]
    @return true iff the two DFAs [m1] and [m2] are equivalent *)
val is_equiv : dfa -> dfa -> bool

(** [myhill_min m]
    @return minimisation of DFA [m], by Myhill-Nerode theorem *)
val myhill_min : dfa -> dfa

(** [hopcroft_min m]
    @return minimisation of DFA [m], by Hopcroft's algorithm. 
    Note that states will be renamed. *)
val hopcroft_min : dfa -> dfa

(** [brzozowski_min m]
    @return minimisation of DFA [m], by Brzozowski's algorithm. 
    Note that states will be renamed. *)
val brzozowski_min : dfa -> dfa

(** [minimise m] synonym for [hopcroft_min m]
    @return minimisation of DFA [m] *)
val minimise : dfa -> dfa

(** [nfa_to_dfa m] 
    @return the NFA equivalent to DFA [m], by an optimised subset construction *)
val nfa_to_dfa : Nfa.nfa -> dfa

(** [print m] prints a string representation of the DFA [m] *)
val print : dfa -> unit

(** [export_graphviz m] 
    @return a representation of the DFA [m] in the DOT language for Graphviz *)
val export_graphviz : dfa -> string