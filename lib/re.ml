open Tree

exception Syntax_error of string

(* |print| -- prints string representation of regex ast *)
let print re = 
    let rec stringify_ast = function
          Literal a -> "Literal " ^ a
        | Epsilon -> "ε"
        | Union (r1, r2) -> "Union (" ^ stringify_ast r1 ^ " , " ^ stringify_ast r2 ^ ")"
        | Concat (r1, r2) -> "Concat (" ^ stringify_ast r1 ^ " , " ^ stringify_ast r2 ^ ")"
        | Star r1 -> "Star (" ^ stringify_ast r1 ^ ")"
        | Empty -> "∅"
    in
    print_string (stringify_ast re);  print_newline ()

(* |simplify_re| -- recursively simplifies the regex, returns regex and flag signalling change *)
let rec simplify_re re flag = 
    match re with

        (* Reduce by Kozen Axioms *)
          Union (r1, Union (r2, r3)) -> let (s1, _) = (simplify_re r1 true) and                             (* a + (b + c) = (a + b) + c *)
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
        | Union (Concat (r1, r2), r3) when r1 = r3 -> let (s1, _) = simplify_re r1 true and                 (* ab + a = a(b+ε) *)
                                                          (s2, _) = simplify_re r2 true in
                                                                (Concat(s1, Union(s2, Epsilon)), true)
        | Union (Concat (r1, r2), r3) when r2 = r3 -> let (s1, _) = simplify_re r1 true and                 (* ab + b = (a+ε)b *)
                                                          (s2, _) = simplify_re r2 true in
                                                                (Concat(Union(s1, Epsilon), s2), true)
        | Union (r1, Concat (r2, r3)) when r1 = r2 -> let (s1, _) = simplify_re r1 true and                 (* a + ab = a(b+ε) *)
                                                          (s3, _) = simplify_re r3 true in
                                                                (Concat(s1, Union(s3, Epsilon)), true)
        | Union (r1, Concat (r2, r3)) when r1 = r3 -> let (s1, _) = simplify_re r1 true and                 (* b + ab = (a+ε)b *)
                                                          (s2, _) = simplify_re r2 true in
                                                                (Concat(Union(s2, Epsilon), s1), true)
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
let rec simplify re =
    let (r, flag) = simplify_re re false in
    if flag then simplify r
    else r

let parse s =
    let lexbuf = Lexing.from_string s in
    try
        Parser.regex Lexer.token lexbuf
    with 
        Parsing.Parse_error -> 
            let tok = Lexing.lexeme lexbuf in
            raise (Syntax_error ("Syntax Error at token "^tok))