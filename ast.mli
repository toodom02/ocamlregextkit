type re = 
      Literal of string     (* a ∈ Σ *)
    | Epsilon               (* ε *)
    | Empty                 (* ∅ *)
    | Union of re * re      (* E + R *)
    | Concat of re * re     (* E·R *)
    | Star of re            (* E* *)

(* |simplify| -- simplifies input regex. Repeats until no more changes *)
val simplify : re -> re