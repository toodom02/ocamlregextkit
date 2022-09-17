(* |list_union| -- returns union of the two input lists *)
let rec list_union l1 l2 = 
  match l2 with
        [] -> l1
      | x::xs -> if not (List.mem x l1) then list_union (x::l1) xs else list_union l1 xs

(* |add_unique| -- adds e to list l only if it is not already in l *)
let add_unique e l = 
  if List.mem e l then l else e::l