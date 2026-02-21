open Alcotest
open Regextkit

(* ---------- Helpers ---------- *)

let check_bool = check bool
let check_opt_string = check (option string)

let accepts m s =
  check_bool ("accepts \"" ^ s ^ "\"") true (Dfa.is_accepted m s)

let rejects m s =
  check_bool ("rejects \"" ^ s ^ "\"") false (Dfa.is_accepted m s)

(* Simple DFA: accepts a* *)
let dfa_a_star () =
  Dfa.create
    [0]
    ["a"]
    [ (0, "a", 0) ]
    0
    [0]

(* DFA: accepts exactly "a" *)
let dfa_single_a () =
  Dfa.create
    [0;1]
    ["a"]
    [ (0,"a",1) ]
    0
    [1]

(* DFA: accepts strings with even number of a *)
let dfa_even_a () =
  Dfa.create
    [0;1]
    ["a"]
    [ (0,"a",1); (1,"a",0) ]
    0
    [0]

(* ---------- create ---------- *)

let test_create_invalid_init () =
  check_raises
    "invalid init"
    (Invalid_argument "DFA Initial State not in States")
    (fun () ->
       ignore (Dfa.create [0] ["a"] [] 1 []))
 
let test_create_epsilon_transition () =
  check_raises
    "epsilon forbidden"
    (Invalid_argument "DFA cannot contain ε-transitions")
    (fun () ->
       ignore (Dfa.create [0;1] ["a"] [ (0,"ε",1) ] 0 []))
 
(* ---------- acceptance ---------- *)

let test_acceptance_a_star () =
  let m = dfa_a_star () in
  accepts m "";
  accepts m "a";
  accepts m "aaa"

let test_acceptance_single_a () =
  let m = dfa_single_a () in
  rejects m "";
  accepts m "a";
  rejects m "aa"

(* ---------- complement ---------- *)

let test_complement () =
  let m = dfa_single_a () in
  let c = Dfa.complement m in
  accepts c "";
  rejects c "a"

(* ---------- get_accepted ---------- *)

let test_get_accepted () =
  let m = dfa_single_a () in
  check_opt_string "shortest word"
    (Some "a")
    (Dfa.get_accepted m)

let test_empty_dfa () =
  let m =
    Dfa.create [0] ["a"] [ (0,"a",0) ] 0 []
  in
  check_bool "is empty"
    true (Dfa.is_empty m)

let test_get_accepted_none () =
  let m = Dfa.create [0] ["a"] [ (0,"a",0) ] 0 [] in
  check_opt_string "no accepted" None (Dfa.get_accepted m)

let test_get_accepted_empty () =
  let m = dfa_a_star () in
  check_opt_string "shortest empty" (Some "") (Dfa.get_accepted m)

let test_create_invalid_accept () =
  check_raises
    "invalid accepting"
    (Invalid_argument "DFA Accepting State not in States")
    (fun () -> ignore (Dfa.create [0] ["a"] [] 0 [1]))

let test_create_invalid_transition_symbol () =
  check_raises
    "invalid transition symbol"
    (Invalid_argument "DFA Transition function not valid")
    (fun () -> ignore (Dfa.create [0] ["a"] [ (0,"b",0) ] 0 [0]))

let test_create_duplicate_transition () =
  check_raises
    "duplicate transition"
    (Invalid_argument "DFA Transition function not valid")
    (fun () -> ignore (Dfa.create [0;1] ["a"] [ (0,"a",1); (0,"a",1) ] 0 [1]))

let test_create_adds_sink_on_missing_transitions () =
  let m = Dfa.create [0;1] ["a";"b"] [ (0,"a",1) ] 0 [1] in
  let states = Dfa.get_states m in
  check int "sink added" 3 (List.length states)

let test_succ_on_missing_transition_returns_sink () =
  let m = Dfa.create [0] ["a";"b"] [ (0,"a",0) ] 0 [] in
  let s = Dfa.succ m (Dfa.get_start m) "b" in
  let states = Dfa.get_states m in
  check bool "succ returns sink" true (List.mem s states)

(* ---------- product construction ---------- *)

let test_product_union () =
  let m1 = dfa_single_a () in
  let m2 = dfa_even_a () in
  let u = Dfa.product_union m1 m2 in
  accepts u "";
  accepts u "a";
  accepts u "aa"

let test_product_intersection () =
  let m1 = dfa_single_a () in
  let m2 = dfa_even_a () in
  let i = Dfa.product_intersection m1 m2 in
  rejects i "";
  rejects i "a";
  rejects i "aa"

let test_product_difference () =
  let m1 = dfa_even_a () in
  let m2 = dfa_single_a () in
  let d = Dfa.product_difference m1 m2 in
  accepts d "";
  accepts d "aa";
  rejects d "aaa"

let test_product_alpha_mismatch () =
  let m1 = Dfa.create [0] ["a"] [ (0,"a",0) ] 0 [0]
  and m2 = Dfa.create [0] ["b"] [ (0,"b",0) ] 0 [0] in
  check_raises
    "product alphabet mismatch"
    (Invalid_argument "Cannot perform product operation over different alphabets")
    (fun () -> ignore (Dfa.product_union m1 m2))

let test_product_with_sinks () =
  let m1 = Dfa.create [0] ["a";"b"] [ (0,"a",0) ] 0 [0]
  and m2 = Dfa.create [0] ["a";"b"] [ (0,"b",0) ] 0 [0] in
  let u = Dfa.product_union m1 m2 in
  (* union of a* and b* *)
  accepts u "";
  accepts u "a";
  accepts u "b";
  rejects u "ab";
  rejects u "ba"


(* ---------- equivalence ---------- *)

let test_equivalence () =
  let m1 = dfa_even_a () in
  let m2 = dfa_even_a () in
  check_bool "equivalent"
    true (Dfa.hopcroft_equiv m1 m2)

let test_non_equivalence () =
  let m1 = dfa_even_a () in
  let m2 = dfa_single_a () in
  check_bool "not equivalent"
    false (Dfa.hopcroft_equiv m1 m2)

let test_symmetric_equiv_consistency () =
  let m1 = dfa_even_a () in
  let m2 = dfa_single_a () in
  check_bool "symmetric vs hopcroft"
    (Dfa.hopcroft_equiv m1 m2)
    (Dfa.symmetric_equiv m1 m2)

(* ---------- minimisation ---------- *)

let test_minimise_equivalence () =
  let m = dfa_even_a () in
  let m_copy = Dfa.copy m in
  Dfa.minimise m_copy;
  check_bool "minimisation preserves language"
    true (Dfa.hopcroft_equiv m m_copy)

let test_brzozowski_min () =
  let m = dfa_even_a () in
  let m2 = Dfa.brzozowski_min m in
  check_bool "brzozowski preserves language"
    true (Dfa.hopcroft_equiv m m2)

(* ---------- re_to_dfa ---------- *)

let test_re_to_dfa () =
  let r = Re.parse "a* + b" in
  let d = Dfa.re_to_dfa r in
  accepts d "";
  accepts d "aaa";
  accepts d "b";
  rejects d "bb"

(* ---------- Runner ---------- *)

let () =
  run "DFA Tests"
    [
      ("create",
        [ test_case "invalid init" `Quick test_create_invalid_init
        ; test_case "epsilon forbidden" `Quick test_create_epsilon_transition
        ; test_case "invalid accepting" `Quick test_create_invalid_accept
        ; test_case "invalid transition symbol" `Quick test_create_invalid_transition_symbol
        ; test_case "duplicate transition" `Quick test_create_duplicate_transition
        ; test_case "sink added" `Quick test_create_adds_sink_on_missing_transitions
        ; test_case "succ on missing transition" `Quick test_succ_on_missing_transition_returns_sink
        ]);
      ("acceptance",
        [ test_case "a*" `Quick test_acceptance_a_star
        ; test_case "single a" `Quick test_acceptance_single_a
        ]);
      ("complement",
        [ test_case "basic complement" `Quick test_complement ]);
      ("emptiness",
        [ test_case "is empty" `Quick test_empty_dfa ]);
      ("get_accepted",
        [ test_case "shortest word" `Quick test_get_accepted
        ; test_case "none when empty" `Quick test_get_accepted_none
        ; test_case "shortest empty" `Quick test_get_accepted_empty ]);
      ("product",
        [ test_case "union" `Quick test_product_union
        ; test_case "intersection" `Quick test_product_intersection
        ; test_case "difference" `Quick test_product_difference
        ; test_case "alphabet mismatch" `Quick test_product_alpha_mismatch
        ; test_case "sinks" `Quick test_product_with_sinks ]);
      ("equivalence",
        [ test_case "equivalent" `Quick test_equivalence
        ; test_case "not equivalent" `Quick test_non_equivalence
        ; test_case "symmetric consistency" `Quick test_symmetric_equiv_consistency ]);
      ("minimisation",
        [ test_case "hopcroft" `Quick test_minimise_equivalence
        ; test_case "brzozowski" `Quick test_brzozowski_min ]);
      ("regex integration",
        [ test_case "re_to_dfa" `Quick test_re_to_dfa ])
    ]