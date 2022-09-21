(* |list_union| -- returns union of the two input lists *)
val list_union : 'a list -> 'a list -> 'a list

(* |add_unique| -- adds elem to list only if it is not already in list *)
val add_unique : 'a -> 'a list -> 'a list

(* |reachable_states| -- returns the set of states reachable the given state *)
val reachable_states : 'a -> ('a * string * 'a) list -> 'a list 