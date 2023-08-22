(** Representation of DFAs and implementation of standard operations *)

type state = State of int list | ProductState of state * state
type dfa = state Adt.automata

val get_states : dfa -> state list
(** [get_states m] 
    @return a list of all states in DFA [m] *)

val get_alphabet : dfa -> string list
(** [get_alphabet m]
    @return the alphabet of DFA [m] as a list *)

val get_transitions : dfa -> (state * string * state) list
(** [get_transitions m]
    @return the transition function of DFA [m] as a list of tuples [(s,a,t)] *)

val get_start : dfa -> state
(** [get_start m]
    @return the initial state of DFA [m] *)

val get_accepting : dfa -> state list
(** [get_accepting m]
    @return a list of all accepting states in DFA [m] *)

val is_accepting : dfa -> state -> bool
(** [is_accepting m s] 
    @return [true] iff state [s] is an accepting state of DFA [m] *)

val succ : dfa -> state -> string -> state
(** [succ m s w] 
    @return the successor state of DFA [m] after reading word [w] from state [s] *)

val pred : dfa -> state -> string -> state list
(** [pred m s w] 
    @return the set of predecessor states of DFA [m] before reading word [w] from state [s] *)

val create :
  'a list -> string list -> ('a * string * 'a) list -> 'a -> 'a list -> dfa
(** [create q al t s f] 
    @return the DFA of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f].
    Note that states will be renamed to integer list. Sink state will be added to make transition function [t] total.
    @raise Invalid_argument if [s] is not a valid state in [q]
    @raise Invalid_argument if [f] is not a valid subset of [q]
    @raise Invalid_argument if [t] contains epsilon-transitions.
    @raise Invalid_argument if [t] is not a valid tranition function for states [qs] and alphabet [al]
*)

val copy : dfa -> dfa
(** [copy m] 
    @return a deep copy of DFA [m] *)

val complement : dfa -> dfa
(** [complement m] 
    @return the complement of DFA [m] *)

val reachable_states : dfa -> state list
(** [reachable_states m]
    @return the set of reachable (connected) states in the DFA [m] *)

val prune : dfa -> unit
(** [prune m] 
    mutates DFA inplace [m] by removing unreachable states *)

val is_empty : dfa -> bool
(** [is_empty m] 
    @return [true] iff DFA [m] is empty *)

val is_accepted : dfa -> string -> bool
(** [is_accepted m s] 
    @return [true] iff DFA [m] accepts string [s] *)

val get_accepted : dfa -> string option
(** [get_accepted m] 
    @return the shortest string accepted by DFA [m] *)

val product_intersection : dfa -> dfa -> dfa
(** [product_insterection m1 m2] 
    @return the intersection of DFAs [m1] [m2], by the product construction *)

val product_difference : dfa -> dfa -> dfa
(** [product_difference m1 m2] 
    @return the symmetric difference of DFAs [m1] [m2], by the product construction *)

val product_union : dfa -> dfa -> dfa
(** [product_union m1 m2] 
    @return the union of DFAs [m1] [m2], by the product construction *)

val hopcroft_equiv : dfa -> dfa -> bool
(** [hopcroft_equiv m1 m2] 
    @return [true] iff the two DFAs [m1] and [m2] are equivalent, by Hopcroft's algorithm *)

val symmetric_equiv : dfa -> dfa -> bool
(** [symmetric_equiv m1 m2] 
    @return [true] iff the two DFAs [m1] and [m2] are equivalent, by symmetric difference *)

val is_equiv : dfa -> dfa -> bool
(** [is_equiv m1 m2] synonym for [hopcroft_equiv m1 m2]
    @return [true] iff the two DFAs [m1] and [m2] are equivalent *)

val myhill_min : dfa -> unit
(** [myhill_min m]
    minimises DFA [m] inplace, by Myhill-Nerode theorem *)

val hopcroft_min : dfa -> unit
(** [hopcroft_min m]
    minimises DFA [m] inplace, by Hopcroft's algorithm *)

val brzozowski_min : dfa -> dfa
(** [brzozowski_min m]
    @return minimisation of DFA [m], by Brzozowski's algorithm. 
    Note that states will be renamed. *)

val minimise : dfa -> unit
(** [minimise m] synonym for [hopcroft_min m]
    @return minimisation of DFA [m] *)

val nfa_to_dfa : Nfa.nfa -> dfa
(** [nfa_to_dfa m] 
    @return the NFA equivalent to DFA [m], by an optimised subset construction *)

val re_to_dfa : Tree.re -> dfa
(** [re_to_dfa re]
    @return a DFA recognising the language of [re], by Brzozowski's construction  *)

val print : dfa -> unit
(** [print m] prints a string representation of the DFA [m] *)

val export_graphviz : dfa -> string
(** [export_graphviz m] 
    @return a representation of the DFA [m] in the DOT language for Graphviz *)
