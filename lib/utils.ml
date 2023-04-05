(* |add_unique| -- adds e to list l only if it is not already in l *)
let add_unique e l = 
    if List.mem e l then l else e::l

(* |list_union| -- returns union of the two input lists *)
let rec list_union l1 = function
    | [] -> l1
    | x::xs -> list_union (add_unique x l1) xs

(* |index| -- finds Some index of x in xs, otherwise returns None *)
let index x xs = 
    let rec aux c = function
        | [] -> None
        | y::ys -> if (x = y) then Some(c) else aux (c+1) ys
    in
    aux 0 xs