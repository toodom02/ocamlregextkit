type 't automata

val get_states : 't automata -> 't list
(** [get_states m] 
    @return a list of all states in [m] *)

val get_alphabet : 't automata -> string list
(** [get_alphabet m]
    @return the alphabet of [m] as a list *)

val get_transitions : 't automata -> ('t * string * 't) list
(** [get_transitions m]
    @return the transition function of [m] as a list of tuples [(s,a,t)] *)

val get_start : 't automata -> 't
(** [get_start m]
    @return the initial state of [m] *)

val get_accepting : 't automata -> 't list
(** [get_accepting m]
    @return a list of all accepting states in [m] *)

val get_next_states : 't automata -> 't -> string -> 't list
(** [get_next_states m s a] 
    @return the successor states of [m] after reading letter [a] from state [s] *)

val get_prev_states : 't automata -> 't -> string -> 't list
(** [get_prev_states m s a] 
    @return the list of predecessor states of [m] before reading letter [a] from state [s] *)

val is_accepting : 't automata -> 't -> bool
(** [is_accepting m s] 
    @return true iff state [s] is an accepting state of [m] *)

val get_reachable_states : 't automata -> 't list
(** [get_reachable_states m] 
    @return the set of states reachable for an automaton [m] *)

val create_automata :
  't list ->
  string list ->
  ('t * string * 't) list ->
  't ->
  't list ->
  't automata
(** [create_automata q al t s f] 
    @return an automata of States [q], alphabet [al], transition function [t], initial state [s], and accepting states [f].
*)
