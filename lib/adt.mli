type 't automata

(** [get_states m]
    @return a list of all states in [m] *)
val get_states : 't automata -> 't list

(** [iter_states f m]
    applies function [f] to all states [[s1; ...; sn]] of [m] *)
val iter_states : ('t -> unit) -> 't automata -> unit

(** [map_states f m]
    applies function [f] to all states [[s1; ...; sn]] of [m].
    @return the list [[f s1; ...; f sn]]*)
val map_states : ('t -> 'a) -> 't automata -> 'a list

(** [filter_states f m]
    @return list of states of [m] that satisfy the predicate [f] *)
val filter_states : ('t -> bool) -> 't automata -> 't list

(** [exists_states f m]
    checks if at least one state of [m] satisfies the predicate [f]
    @return [true] iff [(f s1) || ... || (f sn)] *)
val exists_states : ('t -> bool) -> 't automata -> bool

(** [for_all_states f m]
    checks if all states [[s1; ...; sn]] of [m] satisfy the predicate [f]
    @return [true] iff [(f s1) && ... && (f sn)] *)
val for_all_states : ('t -> bool) -> 't automata -> bool

(** [get_alphabet m]
    @return the alphabet of [m] as a list *)
val get_alphabet : 't automata -> string list

(** [iter_alphabet f m]
    applies function [f] to all words [[a1; ...; an]] of the alphabet of [m] *)
val iter_alphabet : (string -> unit) -> 't automata -> unit

(** [map_states f m]
    applies function [f] to all words [[a1; ...; an]] of the alphabet of [m].
    @return the list [[f a1; ...; f an]]*)
val map_alphabet : (string -> 'a) -> 't automata -> 'a list

(** [filter_states f m]
    @return list of words of the alphabet of [m] that satisfy the predicate [f] *)
val filter_alphabet : (string -> bool) -> 't automata -> string list

(** [exists_alphabet f m]
    checks if at least one word of the alphabet of [m] satisfies the predicate [f]
    @return [true] iff [(f a1) || ... || (f an)] *)
val exists_alphabet : (string -> bool) -> 't automata -> bool

(** [for_all_alphabet f m]
    checks if all words [[a1; ...; an]] of the alphabet of [m] satisfy the predicate [f]
    @return [true] iff [(f a1) && ... && (f an)] *)
val for_all_alphabet : (string -> bool) -> 't automata -> bool

(** [get_transitions m]
    @return the transition function of [m] as a list of tuples [(s,a,t)] *)
val get_transitions : 't automata -> ('t * string * 't) list

(** [iter_transitions f m]
    applies function [f] to all transitions [(s,a,t)] of [m] *)
val iter_transitions : ('t * string * 't -> unit) -> 't automata -> unit

(** [map_transitions f m]
    applies function [f] to all transitions [(s,a,t)] of [m].
    @return the list [[f (s1,a1,t1); ...; f (sn,an,tn)]] *)
val map_transitions : ('t * string * 't -> 'a) -> 't automata -> 'a list

(** [get_start m]
    @return the initial state of [m] *)
val get_start : 't automata -> 't

(** [get_accepting m]
    @return a list of all accepting states in [m] *)
val get_accepting : 't automata -> 't list

(** [iter_transitions f m]
    applies function [f] to all accepting states [[s1; ...; sn]] of [m] *)
val iter_accepting : ('t -> unit) -> 't automata -> unit

(** [map_states f m]
    applies function [f] to all accepting states [[s1; ...; sn]] of [m].
    @return the list [[f s1; ...; f sn]]*)
val map_accepting : ('t -> 'a) -> 't automata -> 'a list

(** [get_next_states m s a]
    @return the successor states of [m] after reading letter [a] from state [s] *)
val get_next_states : 't automata -> 't -> string -> 't list

(** [get_prev_states m s a]
    @return the list of predecessor states of [m] before reading letter [a] from state [s] *)
val get_prev_states : 't automata -> 't -> string -> 't list

(** [is_accepting m s]
    @return [true] iff state [s] is an accepting state of [m] *)
val is_accepting : 't automata -> 't -> bool

(** [get_reachable_states m]
    @return the set of states reachable for an automaton [m] *)
val get_reachable_states : 't automata -> 't list

(** [filter_states_inplace m f]
    mutates automaton [m] inplace filtering states by function [f] *)
val filter_states_inplace : 't automata -> ('t -> bool) -> unit

(** [merge_states_inplace m p q]
    mutates automaton [m] by merging state [q] into state [p] *)
val merge_states_inplace : 't automata -> 't -> 't -> unit

(** [add_to_alphabet m als]
    mutates automaton [m] inplace by adding [als] to the alphabet *)
val add_to_alphabet : 't automata -> string list -> unit

(** [map_accepting f m]
    mutates automaton [m] inplace by applying function [f] to set of states defining acceptance *)
val map_accepting_inplace : ('t -> bool) -> 't automata -> unit

(** [copy m]
    @return a deep copy of automaton [m] *)
val copy : 't automata -> 't automata

(** [create_automata q al t s f]
    @return
      an automata of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f]. *)
val create_automata
  :  't list
  -> string list
  -> ('t * string * 't) list
  -> 't
  -> 't list
  -> 't automata
