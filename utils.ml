(* |list_union| -- returns union of the two input lists *)
let rec list_union l1 l2 = 
  match l2 with
        [] -> l1
      | x::xs -> if not (List.mem x l1) then list_union (x::l1) xs else list_union l1 xs

(* |add_unique| -- adds e to list l only if it is not already in l *)
let add_unique e l = 
  if List.mem e l then l else e::l

(* |reachable_states| -- returns the set of states reachable from the given state *)
let reachable_states start transitions = 
  let marked = ref [start] and
      changed = ref true in
  while (!changed) do
      changed := false;
      let newMarked = ref !marked in
      List.iter (fun m ->
          List.iter (fun (s,_,t) ->
              if (s = m && not (List.mem t !newMarked)) then (
                  newMarked := t :: !newMarked;
                  changed := true;
              )
          ) transitions;
      ) !marked;
      marked := !newMarked;
  done;
  !marked