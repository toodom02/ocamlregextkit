type re = 
      Skip
    | Literal of string     (* a ∈ Σ *)
    | Epsilon               (* ε *)
    | Union of re * re      (* E + R *)
    | Concat of re * re     (* E·R *)
    | Star of re            (* E* *)
    | Empty                 (* ∅ *)
