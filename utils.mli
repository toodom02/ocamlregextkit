(** Provides some useful generic functions *)

(** [list_union xs ys] 
    @return the set union of the lists [xs] and [ys] *)
val list_union : 'a list -> 'a list -> 'a list

(** [add_unique x xs] 
    @return the list [x:xs] only if [x] is not already in [xs] *)
val add_unique : 'a -> 'a list -> 'a list

(** [index x xs] 
    @return Some index of the first occurence of [x] in list [xs], otherwise None *)
val index : 'a -> 'a list -> int option

(** [reachable_states s t] 
    @return the set of states reachable for an automaton in state [s] with transition function [t] *)
val reachable_states : 'a -> ('a * string * 'a) list -> 'a list 