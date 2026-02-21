open Alcotest
open Regextkit

(* ---------- Helpers ---------- *)

let check_re name expected actual =
  check bool name true (expected = actual)

let lit a = Tree.Literal a
let eps = Tree.Epsilon
let empty = Tree.Empty
let union a b = Tree.Union (a,b)
let concat a b = Tree.Concat (a,b)
let star a = Tree.Star a

(* ---------- simplify basic axioms ---------- *)

let test_simplify_union_empty () =
  let r = union (lit "a") empty in
  check_re "a + ∅ = a"
    (lit "a")
    (Re.simplify r)

let test_simplify_concat_epsilon () =
  let r = concat eps (lit "a") in
  check_re "ε.a = a"
    (lit "a")
    (Re.simplify r)

let test_simplify_star_star () =
  let r = star (star (lit "a")) in
  check_re "(a*)* = a*"
    (star (lit "a"))
    (Re.simplify r)

let test_simplify_empty_star () =
  check_re "∅* = ε"
    eps
    (Re.simplify (star empty))

(* ---------- factoring ---------- *)

let test_simplify_factoring_left () =
  let r =
    union
      (concat (lit "a") (lit "b"))
      (concat (lit "a") (lit "c"))
  in
  let expected =
    concat (lit "a")
      (union (lit "b") (lit "c"))
  in
  check_re "ab + ac = a(b+c)"
    expected
    (Re.simplify r)

let test_simplify_duplicate_union () =
  let r = union (lit "a") (lit "a") in
  check_re "a + a = a"
    (lit "a")
    (Re.simplify r)

(* ---------- ordering ---------- *)

let test_union_order_literals () =
  let r = union (lit "b") (lit "a") in
  check_re "ordering literals"
    (union (lit "a") (lit "b"))
    (Re.simplify r)

let test_get_alphabet_duplicates () =
  let r = union (lit "a") (lit "a") in
  let alph = Re.get_alphabet r |> List.sort compare in
  check (list string)
    "alphabet duplicate removal"
    ["a"]
    alph

(* ---------- alphabet ---------- *)

let test_get_alphabet () =
  let r =
    union
      (concat (lit "a") (lit "b"))
      (star (lit "c"))
  in
  let alph = Re.get_alphabet r |> List.sort compare in
  check (list string)
    "alphabet extraction"
    ["a";"b";"c"]
    alph

let test_parse_concat_implicit_and_dot () =
  match Re.parse "a b" with
  | Concat (_,_) -> ()
  | _ -> Alcotest.check bool "parse implicit concat failed" true false;
  match Re.parse "a.b" with
  | Concat (_,_) -> ()
  | _ -> Alcotest.check bool "parse explicit concat failed" true false

let test_parse_precedence () =
  match Re.parse "a+bc" with
  | Union (_, Concat (_, _)) -> ()
  | _ -> Alcotest.check bool "parse precedence failed" true false

(* ---------- nullable ---------- *)

let test_nullable () =
  check bool "ε nullable"
    true (Re.is_nullable eps);
  check bool "a not nullable"
    false (Re.is_nullable (lit "a"));
  check bool "a* nullable"
    true (Re.is_nullable (star (lit "a")));
  check bool "(ε.a) nullable"
    false (Re.is_nullable (concat eps (lit "a")))

(* ---------- derivative ---------- *)

let test_derivative_literal () =
  check_re "d_a(a)=ε"
    eps
    (Re.derivative (lit "a") "a")

let test_derivative_union () =
  let r = union (lit "a") (lit "b") in
  let expected = union eps empty in
  check_re "d_a(a+b)"
    expected
    (Re.derivative r "a")

let test_derivative_nonmatching_literal () =
  check_re "d_b(a)=∅"
    empty
    (Re.derivative (lit "a") "b")

let test_derivative_concat_nonnullable () =
  let r = concat (lit "a") (lit "b") in
  (* d_a(ab) = (ε).b -> simplifies to b *)
  check_re "d_a(ab)=b"
    (lit "b")
    (Re.simplify (Re.derivative r "a"))

let test_derivative_concat_nullable () =
  let r = concat eps (lit "a") in
  check_re "d_a(εa)=ε"
    eps
    (Re.simplify (Re.derivative r "a"))

(* ---------- simplify fixed point ---------- *)

let test_simplify_fixpoint () =
  let r =
    union
      (union (lit "a") empty)
      empty
  in
  check_re "fixpoint simplify"
    (lit "a")
    (Re.simplify r)

let test_simplify_union_contains () =
  let r = union (lit "a") (union (lit "a") (lit "b")) in
  check_re "a + (a + b) = a + b"
    (union (lit "a") (lit "b"))
    (Re.simplify r)

let test_simplify_union_with_star_repeated () =
  let r = union (concat (lit "a") (lit "a")) (star (lit "a")) in
  check_re "aa + a* = a*"
    (star (lit "a"))
    (Re.simplify r)

let test_simplify_concat_with_empty () =
  check_re "∅.a = ∅"
    empty
    (Re.simplify (concat empty (lit "a")));
  check_re "a.∅ = ∅"
    empty
    (Re.simplify (concat (lit "a") empty))

let test_simplify_concat_star_same () =
  let r = concat (star (lit "a")) (lit "a") in
  check_re "a* . a = a a*"
    (concat (lit "a") (star (lit "a")))
    (Re.simplify r)

let test_simplify_star_epsilon () =
  check_re "ε* = ε"
    eps
    (Re.simplify (star eps))

(* ---------- parse ---------- *)

let test_parse_simple () =
  match Re.parse "a" with
  | Literal "a" -> ()
  | _ -> Alcotest.check bool "parse failed" true false

let test_parse_union () =
  match Re.parse "a+b" with
  | Union (_,_) -> ()
  | _ -> Alcotest.check bool "parse union failed" true false

let test_parse_error () =
  check_raises
    "syntax error"
    (Re.Syntax_error "Syntax Error at token $")
    (fun () -> ignore (Re.parse "$"))

let test_parse_bad_token () =
  check_raises
    "bad token"
    (Re.Syntax_error "Syntax Error at token @")
    (fun () -> ignore (Re.parse "@"))

let test_parse_tokens_variants () =
  match Re.parse "EPSILON" with
  | Epsilon -> ()
  | _ -> Alcotest.check bool "parse EPSILON" true false;
  match Re.parse "ε" with
  | Epsilon -> ()
  | _ -> Alcotest.check bool "parse ε" true false;
  match Re.parse "EMPTY" with
  | Empty -> ()
  | _ -> Alcotest.check bool "parse EMPTY" true false;
  match Re.parse "∅" with
  | Empty -> ()
  | _ -> Alcotest.check bool "parse ∅" true false

(* ---------- graphviz ---------- *)

let test_export_graphviz_structure () =
  let r = union (lit "a") (lit "b") in
  let dot = Re.export_graphviz r in
  check bool "starts with digraph"
    true (String.sub dot 0 10 = "digraph G ");
  check bool "contains Union label"
    true (String.contains dot 'U')

(* ---------- Test Runner ---------- *)

let () =
  run "Re Tests"
    [
      ("simplify",
        [ test_case "union empty" `Quick test_simplify_union_empty
        ; test_case "concat epsilon" `Quick test_simplify_concat_epsilon
        ; test_case "star star" `Quick test_simplify_star_star
        ; test_case "empty star" `Quick test_simplify_empty_star
        ; test_case "factoring" `Quick test_simplify_factoring_left
        ; test_case "duplicate union" `Quick test_simplify_duplicate_union
        ; test_case "ordering" `Quick test_union_order_literals
        ; test_case "fixpoint" `Quick test_simplify_fixpoint
        ; test_case "union contains" `Quick test_simplify_union_contains
        ; test_case "union with star repeated" `Quick test_simplify_union_with_star_repeated
        ; test_case "concat with empty" `Quick test_simplify_concat_with_empty
        ; test_case "concat star same" `Quick test_simplify_concat_star_same
        ; test_case "star epsilon" `Quick test_simplify_star_epsilon
        ]);
      ("alphabet",
        [ test_case "get_alphabet" `Quick test_get_alphabet
        ; test_case "alphabet duplicates" `Quick test_get_alphabet_duplicates ]);
      ("nullable",
        [ test_case "nullable cases" `Quick test_nullable ]);
      ("derivative",
        [ test_case "literal" `Quick test_derivative_literal
        ; test_case "union" `Quick test_derivative_union
        ; test_case "nonmatching literal" `Quick test_derivative_nonmatching_literal
        ; test_case "concat nonnullable" `Quick test_derivative_concat_nonnullable
        ; test_case "concat nullable" `Quick test_derivative_concat_nullable
        ]);
      ("parse",
        [ test_case "parse literal" `Quick test_parse_simple
        ; test_case "parse union" `Quick test_parse_union
        ; test_case "parse error" `Quick test_parse_error
        ; test_case "parse bad token" `Quick test_parse_bad_token
        ; test_case "parse concat and precedence" `Quick test_parse_concat_implicit_and_dot
        ; test_case "parse precedence" `Quick test_parse_precedence
        ; test_case "parse token variants" `Quick test_parse_tokens_variants
        ]);
      ("graphviz",
        [ test_case "structure" `Quick test_export_graphviz_structure ]);
    ]