(* Demo program that compares two input regex *)

open Regextkit

(* |test_dfa_pred_succ| -- Tests DFAs pred and succ methods by checking that for each state, the predecessors of a successor contains itself *)
let test_dfa_pred_succ (m: Dfa.dfa) =
    if List.exists (fun s ->
        List.exists (fun a ->
            let succ = Dfa.succ m s a in
            not (List.mem s (Dfa.pred m succ a))
        ) m.alphabet
    ) m.states then exit 1

(* |test_dfa_total| -- Tests that DFA is total, i.e. each state has exactly one transition for each letter *)
let test_dfa_total (m: Dfa.dfa) =
    if not (List.for_all (fun s ->
        List.for_all (fun a ->
            let ts = List.find_all (fun (s',a',_) -> s = s' && a = a') m.transitions in
            List.length ts = 1
        ) m.alphabet
    ) m.states) then exit 1

(* |test_nfa_pred_succ| -- Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself *)
let test_nfa_pred_succ (n: Nfa.nfa) =
    if List.exists (fun s ->
        let pred = Nfa.pred n s in
        List.exists (fun ss ->
            not (List.exists (fun a -> List.mem s (Nfa.succ n ss a)) n.alphabet)
        ) pred
    ) n.states then exit 1

let main () = 
    (* Get CLI args *)
    let fns = ref [] in
    Arg.parse [] (fun s -> fns := !fns @ [s]) "";
    if List.length !fns <> 2 then (
      print_string "Usage: demo \"<regex>\" \"<regex>\"\n";
      exit 2
    );

    (* Parse strings as REs *)
    let re1 = Re.parse (List.hd !fns) and
        re2 = Re.parse (List.nth !fns 1) in

    (* Reduce REs *)
    let re1' = Re.simplify re1 and
        re2' = Re.simplify re2 in

    (* Convert REs to NFAs *)
    let nfa1 = Nfa.re_to_nfa re1' and
        nfa2 = Nfa.re_to_nfa re2' in

    (* Merge the DFA alphabets *)
    let (nfa1', nfa2') = Nfa.merge_alphabets nfa1 nfa2 in

    (* Convert NFAs to DFAs *)
    let dfa1 = Dfa.nfa_to_dfa nfa1' and
        dfa2 = Dfa.nfa_to_dfa nfa2' in

    (* Complement DFAs *)
    let comp1 = Dfa.complement dfa1 and
        comp2 = Dfa.complement dfa2 in

    (* Intersect DFAs *)
    let fst_and_not_snd = Dfa.product_intersection dfa1 comp2 and
        snd_and_not_fst = Dfa.product_intersection dfa2 comp1 in

    (* Find shortest unique strings *)
    let accepted1 = Dfa.accepted fst_and_not_snd and
        accepted2 = Dfa.accepted snd_and_not_fst in

    (* Testing DFA invariants *)
    test_dfa_pred_succ dfa1; test_dfa_pred_succ comp1;
    test_dfa_pred_succ dfa2; test_dfa_pred_succ comp2;
    test_dfa_pred_succ fst_and_not_snd; test_dfa_pred_succ snd_and_not_fst;
    test_dfa_total dfa1; test_dfa_total comp1;
    test_dfa_total dfa2; test_dfa_total comp2;
    test_dfa_total fst_and_not_snd; test_dfa_total snd_and_not_fst;

    (* Testing NFA invariants *)
    test_nfa_pred_succ nfa1; test_nfa_pred_succ nfa2;
    test_nfa_pred_succ nfa1'; test_nfa_pred_succ nfa2';

    (* Test that our equivalence functions all give the same result *)
    if (Dfa.closure_equiv dfa1 dfa2 <> Dfa.symmetric_equiv dfa1 dfa2) then exit 1;
    if (Dfa.closure_equiv dfa1 dfa2 <> Dfa.hopcroft_equiv dfa1 dfa2) then exit 1;

    (* Test that minimisation works as expected *)
    let myhillmin1 = Dfa.myhill_min dfa1 and
        myhillmin2 = Dfa.myhill_min dfa2 and
        hopcroftmin1 = Dfa.hopcroft_min dfa1 and
        hopcroftmin2 = Dfa.hopcroft_min dfa2 and
        brzozowskimin1 = Dfa.brzozowski_min dfa1 and
        brzozowskimin2 = Dfa.brzozowski_min dfa2 in
    if not (Dfa.is_equiv dfa1 myhillmin1) then exit 1;
    if not (Dfa.is_equiv dfa2 myhillmin2) then exit 1;
    if not (Dfa.is_equiv dfa1 hopcroftmin1) then exit 1;
    if not (Dfa.is_equiv dfa2 hopcroftmin2) then exit 1;
    if not (Dfa.is_equiv dfa1 brzozowskimin1) then exit 1;
    if not (Dfa.is_equiv dfa2 brzozowskimin2) then exit 1;
    if not (List.length myhillmin1.states = List.length brzozowskimin1.states) then exit 1;
    if not (List.length myhillmin2.states = List.length brzozowskimin2.states) then exit 1;
    if not (List.length myhillmin1.states = List.length hopcroftmin1.states) then exit 1;
    if not (List.length myhillmin2.states = List.length hopcroftmin2.states) then exit 1;

    if (Option.is_none accepted1 && Option.is_none accepted2) then (
        print_string "Input regex are equal\n";
        exit 0
    );
    if (Option.is_some accepted1) then Printf.printf "'%s' exists in the first but not the second\n" (Option.get accepted1);
    if (Option.is_some accepted2) then Printf.printf "'%s' exists in the second but not the first\n" (Option.get accepted2);

    exit 0

let () = main ()