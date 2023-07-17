(* Demo program that compares two input regex *)

open Regextkit

(* |test_dfa_pred_succ| -- Tests DFAs pred and succ methods by checking that for each state, the predecessors of a successor contains itself *)
let test_dfa_pred_succ m =
    if List.exists (fun s ->
        List.exists (fun a ->
            let succ = DfaHash.succ m s a in
            not (List.mem s (DfaHash.pred m succ a))
        ) (DfaHash.get_alphabet m)
    ) (DfaHash.get_states m) then exit 1

(* |test_dfa_total| -- Tests that DFA is total, i.e. each state has exactly one transition for each letter *)
let test_dfa_total m =
    if not (List.for_all (fun s ->
        List.for_all (fun a ->
            let ts = List.find_all (fun (s',a',_) -> s = s' && a = a') (DfaHash.get_transitions m) in
            List.length ts = 1
        ) (DfaHash.get_alphabet m)
    ) (DfaHash.get_states m)) then exit 1

(* |test_nfa_pred_succ| -- Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself *)
let test_nfa_pred_succ n =
    if List.exists (fun s ->
        let pred = NfaHash.pred n s in
        List.exists (fun ss ->
            not (List.exists (fun a -> List.mem s (NfaHash.succ n ss a)) (NfaHash.get_alphabet n))
        ) pred
    ) (NfaHash.get_states n) then exit 1

let main () = 
    (* Get CLI args *)
    let args = Sys.argv in
    if Array.length args <> 3 then (
        print_string "Usage: demo \"<regex>\" \"<regex>\"\n";
        exit 2
    );

    (* Parse strings as REs *)
    let re1 = Re.parse args.(1) and
        re2 = Re.parse args.(2) in

    (* Reduce REs *)
    let re1' = Re.simplify re1 and
        re2' = Re.simplify re2 in

    (* Convert REs to NFAs *)
    let nfa1 = NfaHash.re_to_nfa re1' and
        nfa2 = NfaHash.re_to_nfa re2' in

    (* Merge the NFA alphabets *)
    let (nfa1', nfa2') = NfaHash.merge_alphabets nfa1 nfa2 in

    (* Convert NFAs to DFAs *)
    let dfa1 = DfaHash.nfa_to_dfa nfa1' and
        dfa2 = DfaHash.nfa_to_dfa nfa2' in

    (* Complement DFAs *)
    let comp1 = DfaHash.complement dfa1 and
        comp2 = DfaHash.complement dfa2 in

    (* Intersect DFAs *)
    let fst_and_not_snd = DfaHash.product_intersection dfa1 comp2 and
        snd_and_not_fst = DfaHash.product_intersection dfa2 comp1 in

    (* Find shortest unique strings *)
    let accepted1 = DfaHash.get_accepted fst_and_not_snd and
        accepted2 = DfaHash.get_accepted snd_and_not_fst in

    (* Test brzozowski construction *)
    let brzozo1 = DfaHash.re_to_dfa re1' and
        brzozo2 = DfaHash.re_to_dfa re2' in

    if not (DfaHash.is_equiv dfa1 brzozo1) then exit 1;
    if not (DfaHash.is_equiv dfa2 brzozo2) then exit 1;

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
    if (DfaHash.symmetric_equiv dfa1 dfa2 <> DfaHash.hopcroft_equiv dfa1 dfa2) then exit 1;

    (* Test that minimisation works as expected *)
    let myhillmin1 = DfaHash.myhill_min dfa1 and
        myhillmin2 = DfaHash.myhill_min dfa2 and
        hopcroftmin1 = DfaHash.hopcroft_min dfa1 and
        hopcroftmin2 = DfaHash.hopcroft_min dfa2 and
        brzozowskimin1 = DfaHash.brzozowski_min dfa1 and
        brzozowskimin2 = DfaHash.brzozowski_min dfa2 in
    if not (DfaHash.is_equiv dfa1 myhillmin1) then exit 1;
    if not (DfaHash.is_equiv dfa2 myhillmin2) then exit 1;
    if not (DfaHash.is_equiv dfa1 hopcroftmin1) then exit 1;
    if not (DfaHash.is_equiv dfa2 hopcroftmin2) then exit 1;
    if not (DfaHash.is_equiv dfa1 brzozowskimin1) then exit 1;
    if not (DfaHash.is_equiv dfa2 brzozowskimin2) then exit 1;
    if not (List.length (DfaHash.get_states myhillmin1) = List.length (DfaHash.get_states brzozowskimin1)) then exit 1;
    if not (List.length (DfaHash.get_states myhillmin2) = List.length (DfaHash.get_states brzozowskimin2)) then exit 1;
    if not (List.length (DfaHash.get_states myhillmin1) = List.length (DfaHash.get_states hopcroftmin1)) then exit 1;
    if not (List.length (DfaHash.get_states myhillmin2) = List.length (DfaHash.get_states hopcroftmin2)) then exit 1;

    if (Option.is_none accepted1 && Option.is_none accepted2) then (
        print_string "Input regex are equal\n";
        exit 0
    );
    if (Option.is_some accepted1) then Printf.printf "'%s' exists in the first but not the second\n" (Option.get accepted1);
    if (Option.is_some accepted2) then Printf.printf "'%s' exists in the second but not the first\n" (Option.get accepted2);

    exit 0

let () = main ()