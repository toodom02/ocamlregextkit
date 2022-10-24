(** Provides some useful generic functions *)

(** [list_union xs ys] returns the set union of the lists [xs] and [ys] *)
val list_union : 'a list -> 'a list -> 'a list

(** [add_unique x xs] returns the list [x:xs] only if x is not already in xs *)
val add_unique : 'a -> 'a list -> 'a list

(** [reachable_states s t] returns the set of states reachable for an automaton in state [s] with transition function [t] *)
val reachable_states : 'a -> ('a * string * 'a) list -> 'a list 