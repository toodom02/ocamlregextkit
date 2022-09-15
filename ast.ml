type re = 
      Literal of string     (* a ∈ Σ *)
    | Epsilon               (* ε *)
    | Empty                 (* ∅ *)
    | Union of re * re      (* E + R *)
    | Concat of re * re     (* E·R *)
    | Star of re            (* E* *)

(* |simplify_re| -- recursively simplifies the regex, returns regex and flag signalling change *)
let rec simplify_re re flag = 
    match re with

        (* Reduce by Kozen Axioms *)
          Union (r1, Union (r2, r3)) -> let (s1, _) = (simplify_re r1 true) and                             (* a + (b + c) = (a + b) + c*)
                                            (s2, _) = (simplify_re r2 true) and 
                                            (s3, _) = (simplify_re r3 true) in
                                                        (Union(Union(s1, s2), s3), true)  
        | Union (r1, Empty) -> let (s, _) = simplify_re r1 true in (s, true)                                (* a + ∅ = a *)  
        | Union (Empty, r1) -> let (s, _) = simplify_re r1 true in (s, true)                                (* ∅ + a = a *)
        | Union (r1, r2) when r1 = r2 -> let (s,_) = (simplify_re r1 true) in (s, true)                     (* a + a = a *)
        | Concat (r1, Concat (r2, r3)) -> let (s1, _) = (simplify_re r1 true) and                           (* a.(b.c) = (a.b).c *)
                                            (s2, _) = (simplify_re r2 true) and 
                                            (s3, _) = (simplify_re r3 true) in
                                                        (Concat(Concat(s1, s2), s3), true) 
        | Concat (Epsilon, r1) -> let (s, _) = simplify_re r1 true in (s, true)                             (* ε.a = a *)
        | Concat (r1, Epsilon) -> let (s, _) = simplify_re r1 true in (s, true)                             (* a.ε = a *)
        | Union (Concat (r1, r2), Concat (r3, r4)) when r1 = r3 -> let (s1, _) = simplify_re r1 true and    (* ab + ac = a(b+c) *)
                                                                       (s2, _) = simplify_re r2 true and
                                                                       (s4, _) = simplify_re r4 true in
                                                                            (Concat(s1, Union(s2, s4)), true)
        | Union (Concat (r1, r2), Concat (r3, r4)) when r2 = r4 -> let (s1, _) = simplify_re r1 true and    (* ac + bc = (a+b)c *)
                                                                       (s2, _) = simplify_re r2 true and
                                                                       (s3, _) = simplify_re r3 true in
                                                                            (Concat(Union(s1,s3), s2), true)
        | Concat (Empty, _) -> (Empty, true)                                                                (* ∅.a = ∅ *)
        | Concat (_, Empty) -> (Empty, true)                                                                (* a.∅ = ∅ *)          

        (* other reductions *)
        | Concat (Star r1, Star r2) when r1 = r2 -> let (s,_) = (simplify_re r1 true) in                    (* a*a* = a* *)
                                                            (Star s, true)
        | Star (Star r1) -> let (s,_) = (simplify_re r1 true) in (Star s, true)                             (* ( a* )* = a* *)
        | Union (Epsilon, Star r1) -> let (s, _) = (simplify_re r1 true) in (Star s, true)                  (* ε + a* = a* *)
        | Union (Star r1, Epsilon) -> let (s, _) = (simplify_re r1 true) in (Star s, true)                  (* a* + ε = a* *)
        | Star Empty -> (Epsilon, true)                                                                     (* ∅* = ε *)
        | Star Epsilon -> (Epsilon, true)                                                                   (* ε* = ε *)
        | Union (r1, Star r2) when r1 = r2 -> let (s, _) = (simplify_re r1 true) in (Star s, true)          (* a + a* = a* *)
        | Union (Star r1, r2) when r1 = r2 -> let (s, _) = (simplify_re r1 true) in (Star s, true)          (* a* + a = a* *)

        (* otherwise, simplify children *)
        | Literal a -> (Literal a, flag)
        | Epsilon -> (Epsilon, flag)
        | Union (r1, r2) -> let (s1, f1) = simplify_re r1 flag and 
                                (s2, f2) = simplify_re r2 flag in 
                                    (Union (s1, s2), f1 || f2 || flag)
        | Concat (r1, r2) -> let (s1, f1) = simplify_re r1 flag and 
                                 (s2, f2) = simplify_re r2 flag in 
                                    (Concat (s1, s2), f1 || f2 || flag)
        | Star r1 -> let (s, f) = simplify_re r1 flag in (Star s, f || flag)
        | Empty -> (Empty, false)

(* |simplify| -- simplifies input regex. Repeats until no more changes *)
let simplify re =
    let change = ref true and
        reg = ref re in
            while !change do
                let (r, flag) = simplify_re !reg false in
                    change := flag;
                    reg := r;
            done;
        !reg