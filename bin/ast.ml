type re = 
      Skip
    | Literal of string     (* a ∈ Σ *)
    | Epsilon               (* ε *)
    | Union of re * re      (* E + R *)
    | Concat of re * re     (* E·R *)
    | Star of re            (* E* *)
    | Empty                 (* ∅ *)

(* |stringify_ast| -- converts regex ast into printable string *)
let rec stringify_ast re = 
    match re with
          Skip -> ""
        | Literal a -> "Literal " ^ a
        | Epsilon -> "ε"
        | Union (r1, r2) -> "Union (" ^ stringify_ast r1 ^ " , " ^ stringify_ast r2 ^ ")"
        | Concat (r1, r2) -> "Concat (" ^ stringify_ast r1 ^ " , " ^ stringify_ast r2 ^ ")"
        | Star r1 -> "Star (" ^ stringify_ast r1 ^ ")"
        | Empty -> "∅"

(* |print_ast| -- prints string representation of regex ast *)
let print_ast re = print_string (stringify_ast re);  print_newline ()