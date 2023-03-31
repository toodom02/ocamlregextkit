(* |add_unique| -- adds e to list l only if it is not already in l *)
let add_unique e l = 
    if List.mem e l then l else e::l

(* |list_union| -- returns union of the two input lists *)
let rec list_union l1 l2 = 
    match l2 with
        | [] -> l1
        | x::xs -> list_union (add_unique x l1) xs

(* |list_equal| -- returns true if the two lists contain the same elements (not considering order) *)
let list_equal l1 l2 = 
    let rec rec_equal = function
        | [] -> true
        | x::xs -> if (List.mem x l1) then rec_equal xs else false
    in
    if List.length l1 = List.length l2 then rec_equal l2 else false

(* |index| -- finds Some index of x in xs, otherwise returns None *)
let index x xs = 
    let rec aux ls c =
        match ls with
            | [] -> None
            | y::ys -> if (x = y) then Some(c) else aux ys (c+1)
    in
    aux xs 0