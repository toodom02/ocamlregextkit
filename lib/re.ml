open Tree

exception Syntax_error of string

(* |print| -- prints string representation of re *)
let print re = 
    let rec stringify_ast = function
          Literal a -> a
        | Epsilon -> "ε"
        | Union (r1, r2) -> "(" ^ stringify_ast r1 ^ " + " ^ stringify_ast r2 ^ ")"
        | Concat (r1, r2) -> "(" ^ stringify_ast r1 ^ " . " ^ stringify_ast r2 ^ ")"
        | Star r1 -> stringify_ast r1 ^ "*"
        | Empty -> "∅"
    in
    print_string (stringify_ast re);  print_newline ()

(* |export_graphviz| -- exports the AST in the DOT language for Graphviz *)
let export_graphviz re =
    let count = ref 0 in
    let rec graphvizify parent = function
          Literal a -> incr count; (string_of_int !count) ^ " [label=\""^a^"\", shape=ellipse, ];\n"^ (string_of_int parent) ^ " -> " ^ (string_of_int !count) ^ "[label=\"\", ];\n"
        | Epsilon -> incr count; (string_of_int !count) ^ " [label=\"ε\", shape=ellipse, ];\n"^ (string_of_int parent) ^ " -> " ^ (string_of_int !count) ^ "[label=\"\", ];\n"
        | Union (r1, r2) -> incr count;
            let c = !count in (graphvizify c r1) ^ (graphvizify c r2) ^
            (string_of_int c) ^ " [label=\"Union\", shape=ellipse, ];\n"^ (string_of_int parent) ^ " -> " ^ (string_of_int c) ^ "[label=\"\", ];\n"
        | Concat (r1, r2) -> incr count;
            let c = !count in (graphvizify c r1) ^ (graphvizify c r2) ^
            (string_of_int c) ^ " [label=\"Concat\", shape=ellipse, ];\n"^ (string_of_int parent) ^ " -> " ^ (string_of_int c) ^ "[label=\"\", ];\n"
        | Star r1 -> incr count;
            let c = !count in (graphvizify c r1) ^
            (string_of_int c) ^ " [label=\"Star\", shape=ellipse, ];\n"^ (string_of_int parent) ^ " -> " ^ (string_of_int c) ^ "[label=\"\", ];\n"
        | Empty -> incr count; (string_of_int !count) ^ " [label=\"∅\", shape=ellipse, ];\n"^ (string_of_int parent) ^ " -> " ^ (string_of_int !count) ^ "[label=\"\", ];\n"
    in
    "digraph G {\n0 [label=\"\", shape=none, height=0, width=0, ]\n" ^ graphvizify 0 re ^ "}"

(* |simplify_re| -- recursively simplifies the regex *)
let rec simplify_re = function
    (* Reduce by Kozen Axioms *)
      Union (r1, Union (r2, r3)) -> simplify_re (Union(Union(r1, r2), r3))                              (* a + (b + c) = (a + b) + c *)                              
    | Union (r1, Empty) -> simplify_re r1                                                               (* a + ∅ = a *)  
    | Union (Empty, r1) -> simplify_re r1                                                               (* ∅ + a = a *)
    | Union (r1, r2) when r1 = r2 -> simplify_re r1                                                     (* a + a = a *)
    | Concat (r1, Concat (r2, r3)) -> simplify_re (Concat(Concat(r1, r2), r3))                          (* a.(b.c) = (a.b).c *)
    | Concat (Epsilon, r1) -> simplify_re r1                                                            (* ε.a = a *)
    | Concat (r1, Epsilon) -> simplify_re r1                                                            (* a.ε = a *)
    | Union (Concat (r1, r2), Concat (r3, r4)) when r1 = r3 -> simplify_re (Concat(r1, Union(r2, r4)))  (* ab + ac = a(b+c) *)
    | Union (Concat (r1, r2), Concat (r3, r4)) when r2 = r4 -> simplify_re (Concat(Union(r1, r3), r2))  (* ac + bc = (a+b)c *)
    | Union (Concat (r1, r2), r3) when r1 = r3 -> simplify_re (Concat(r1, Union(r2, Epsilon)))          (* ab + a = a(b+ε) *)
    | Union (Concat (r1, r2), r3) when r2 = r3 -> simplify_re (Concat(Union(r1, Epsilon), r2))          (* ab + b = (a+ε)b *)
    | Union (r1, Concat (r2, r3)) when r1 = r2 -> simplify_re (Concat(r1, Union(r3, Epsilon)))          (* a + ab = a(b+ε) *)
    | Union (r1, Concat (r2, r3)) when r1 = r3 -> simplify_re (Concat(Union(r2, Epsilon), r1))          (* b + ab = (a+ε)b *)
    | Concat (Empty, _) -> Empty                                                                        (* ∅.a = ∅ *)
    | Concat (_, Empty) -> Empty                                                                        (* a.∅ = ∅ *)          

    (* other reductions *)
    | Concat (Star r1, Star r2) when r1 = r2 -> simplify_re (Star r1)   (* a*a* = a* *)
    | Star (Star r1) -> simplify_re (Star r1)                           (* ( a* )* = a* *)
    | Union (Epsilon, Star r1) -> simplify_re (Star r1)                 (* ε + a* = a* *)
    | Union (Star r1, Epsilon) -> simplify_re (Star r1)                 (* a* + ε = a* *)
    | Union (r1, Star r2) when r1 = r2 -> simplify_re (Star r1)         (* a + a* = a* *)
    | Union (Star r1, r2) when r1 = r2 -> simplify_re (Star r1)         (* a* + a = a* *)
    | Star Empty -> Epsilon                                             (* ∅* = ε *)
    | Star Epsilon -> Epsilon                                           (* ε* = ε *)

    (* otherwise, simplify children *)
    | Literal a -> Literal a
    | Epsilon -> Epsilon
    | Union (r1, r2) -> let s1 = simplify_re r1 and 
                            s2 = simplify_re r2 in 
                                Union (s1, s2)
    | Concat (r1, r2) -> let s1 = simplify_re r1 and 
                             s2 = simplify_re r2 in 
                                Concat (s1, s2)
    | Star r1 -> let s = simplify_re r1 in Star s
    | Empty -> Empty

(* |simplify| -- simplifies input regex. Repeats until no more changes *)
let simplify re =
    let r = ref re and newr = ref (simplify_re re) in
    while (!r <> !newr) do
        r := !newr; newr := simplify_re !r
    done;
    !r

(* |parse| -- converts string into AST representation *)
let parse s =
    let lexbuf = Lexing.from_string s in
    try
        Parser.regex Lexer.token lexbuf
    with 
        Parsing.Parse_error -> 
            let tok = Lexing.lexeme lexbuf in
            raise (Syntax_error ("Syntax Error at token "^tok))