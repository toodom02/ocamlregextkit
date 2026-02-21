open Alcotest
open Regextkit

(* ---------- Helpers ---------- *)

let check_bool = check bool
let check_opt_string = check (option string)
let check_list_int = check (list int)

let accepts n s =
  check_bool ("accepts \"" ^ s ^ "\"") true (Nfa.is_accepted n s)

let rejects n s =
  check_bool ("rejects \"" ^ s ^ "\"") false (Nfa.is_accepted n s)

(* ---------- Sample NFAs ---------- *)

(* NFA: accepts a* *)
let nfa_a_star () =
  Nfa.create
    [0]
    ["a"]
    [ (0, "a", 0) ]
    0
    [0]

(* NFA: accepts exactly "a" *)
let nfa_single_a () =
  Nfa.create
    [0;1]
    ["a"]
    [ (0,"a",1) ]
    0
    [1]

(* NFA: contains epsilon transition from 0 -> 1 *)
let nfa_eps () =
  Nfa.create
    [0;1]
    ["a"]
    [ (0,"ε",1) ]
    0
    [1]

(* ---------- create ---------- *)

let test_create_invalid_init () =
  check_raises
    "invalid init"
    (Invalid_argument "NFA Initial State not in States")
    (fun () ->
       ignore (Nfa.create [0] ["a"] [] 1 []))

let test_create_invalid_accept () =
  check_raises
    "invalid accepting"
    (Invalid_argument "NFA Accepting State not in States")
    (fun () ->
       ignore (Nfa.create [0] ["a"] [] 0 [1]))

let test_create_invalid_trans () =
  check_raises
    "invalid transition"
    (Invalid_argument "NFA Transition not valid")
    (fun () ->
       ignore (Nfa.create [0] ["a"] [ (0,"b",0) ] 0 [0]))

(* ---------- epsilon reachability ---------- *)

let test_eps_reachable () =
  let n = nfa_eps () in
  check_list_int "epsilon reachable from 0" [0;1] (Nfa.eps_reachable_set n [0]);
  check_list_int "epsilon reachable from 1" [1] (Nfa.eps_reachable_set n [1])

(* ---------- acceptance ---------- *)

let test_acceptance_a_star () =
  let n = nfa_a_star () in
  accepts n "";
  accepts n "a";
  accepts n "aaa"

let test_acceptance_single_a () =
  let n = nfa_single_a () in
  rejects n "";
  accepts n "a";
  rejects n "aa"

let test_acceptance_eps () =
  let n = nfa_eps () in
  accepts n "";
  rejects n "a"

(* ---------- succ ---------- *)

let test_succ () =
  let n = nfa_single_a () in
  check_list_int "succ 0 'a'" [1] (Nfa.succ n 0 "a");
  check_list_int "succ 1 'a'" [] (Nfa.succ n 1 "a")

(* ---------- pred ---------- *)

let test_pred () =
  let n = nfa_single_a () in
  check_list_int "pred 1" [0] (Nfa.pred n 1);
  check_list_int "pred 0" [] (Nfa.pred n 0)

(* ---------- reachable_states & prune ---------- *)

let test_reachable_and_prune () =
  let n =
    Nfa.create [0;1;2] ["a"] [ (0,"a",1) ] 0 [1]
  in
  check_list_int "reachable states" [0;1] (List.sort compare (Nfa.reachable_states n));
  Nfa.prune n;
  check_list_int "pruned states" [0;1] (List.sort compare (Nfa.get_states n))

(* ---------- is_empty ---------- *)

let test_empty_nfa () =
  let n =
    Nfa.create [0] ["a"] [ (0,"a",0) ] 0 []
  in
  check_bool "is empty" true (Nfa.is_empty n)

(* ---------- get_accepted ---------- *)

let test_get_accepted () =
  let n = nfa_single_a () in
  check_opt_string "shortest word" (Some "a") (Nfa.get_accepted n)

(* ---------- copy ---------- *)

let test_copy () =
  let n = nfa_single_a () in
  let n2 = Nfa.copy n in
  check_bool "copy preserves acceptance" true (Nfa.is_accepted n2 "a")

(* ---------- re_to_nfa ---------- *)

let test_re_to_nfa () =
  let r = Re.parse "a* + b" in
  let n = Nfa.re_to_nfa r in
  accepts n "";
  accepts n "aaa";
  accepts n "b";
  rejects n "bb"

let test_eps_reachable_chain () =
  let n =
    Nfa.create [0;1;2] ["a"] [ (0,"ε",1); (1,"ε",2) ] 0 [2]
  in
  check_list_int "epsilon chain reachable from 0" [0;1;2] (Nfa.eps_reachable_set n [0])

let test_succ_with_epsilon_then_symbol () =
  let n =
    Nfa.create [0;1;2] ["a"] [ (0,"ε",1); (1,"a",2) ] 0 [2]
  in
  check_list_int "succ through epsilon then a" [2] (Nfa.succ n 0 "a")

let test_succ_with_symbol_then_epsilon () =
  let n =
    Nfa.create [0;1;2] ["a"] [ (0,"a",1); (1,"ε",2) ] 0 [2]
  in
  check_list_int "succ through a then epsilon" [1;2] (Nfa.succ n 0 "a")

let test_pred_epsilon_chain () =
  let n =
    Nfa.create [0;1;2] ["a"] [ (0,"ε",1); (1,"ε",2); (0,"a",2) ] 0 [2]
  in
  Nfa.print n;
  check_list_int "pred of 2 includes 0 and 1" [0;1] (Nfa.pred n 2)

let test_merge_alphabets_expands () =
  let n1 = Nfa.create [0] ["a"] [ (0,"a",0) ] 0 [0]
  and n2 = Nfa.create [0] ["b"] [ (0,"b",0) ] 0 [0] in
  Nfa.merge_alphabets n1 n2;
  check (list string) "merged alphabet" ["a";"b"] (List.sort compare (Nfa.get_alphabet n1))

let test_get_accepted_empty_and_none () =
  let n_accept_empty = Nfa.create [0] ["a"] [] 0 [0] in
  check_opt_string "accepts empty" (Some "") (Nfa.get_accepted n_accept_empty);
  let n_none = Nfa.create [0] ["a"] [ (0,"a",0) ] 0 [] in
  check_opt_string "no accepted" None (Nfa.get_accepted n_none)

let test_prune_removes_unreachable () =
  let n = Nfa.create [0;1;2] ["a"] [ (0,"a",1) ] 0 [1] in
  (* state 2 is unreachable *)
  Nfa.prune n;
  check_list_int "pruned states" [0;1] (Nfa.get_states n)

let test_export_graphviz_contains_nodes () =
  let n = Nfa.re_to_nfa (Re.parse "a+b") in
  let dot = Nfa.export_graphviz n in
  check bool "contains digraph" true (String.sub dot 0 9 = "digraph G")

(* ---------- Runner ---------- *)

let () =
  run "NFA Tests"
    [
      ("create",
        [ test_case "invalid init" `Quick test_create_invalid_init
        ; test_case "invalid accepting" `Quick test_create_invalid_accept
        ; test_case "invalid transition" `Quick test_create_invalid_trans
        ]);
      ("epsilon",
        [ test_case "reachable" `Quick test_eps_reachable
        ; test_case "epsilon chain" `Quick test_eps_reachable_chain ]);
      ("acceptance",
        [ test_case "a*" `Quick test_acceptance_a_star
        ; test_case "single a" `Quick test_acceptance_single_a
        ; test_case "epsilon" `Quick test_acceptance_eps
        ]);
      ("succ",
        [ test_case "succ" `Quick test_succ
        ; test_case "succ via epsilon" `Quick test_succ_with_epsilon_then_symbol 
        ; test_case "succ via symbol then epsilon" `Quick test_succ_with_symbol_then_epsilon ]);
      ("pred",
        [ test_case "pred" `Quick test_pred
        ; test_case "pred epsilon chain" `Quick test_pred_epsilon_chain ]);
      ("reachable/prune",
        [ test_case "reachable & prune" `Quick test_reachable_and_prune
        ; test_case "prune removes unreachable" `Quick test_prune_removes_unreachable ]);
      ("emptiness",
        [ test_case "is empty" `Quick test_empty_nfa ]);
      ("get_accepted",
        [ test_case "shortest word" `Quick test_get_accepted
        ; test_case "empty and none" `Quick test_get_accepted_empty_and_none ]);
      ("copy",
        [ test_case "copy" `Quick test_copy ]);
      ("alphabets",
        [ test_case "merge alphabets" `Quick test_merge_alphabets_expands ]);
      ("graphviz",
        [ test_case "export graphviz" `Quick test_export_graphviz_contains_nodes ]);
      ("regex integration",
        [ test_case "re_to_nfa" `Quick test_re_to_nfa ])
    ]