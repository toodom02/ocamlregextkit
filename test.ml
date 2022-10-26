
(* Tests DFAs pred and succ methods by checking that for each state, the successor of a predecessor is itself*)
let test_dfa_pred_succ m =
    List.iter (fun s -> 
        let pred = Dfa.pred m s in
        List.iter (fun ss ->
            if not (List.exists (fun a ->
                Dfa.succ m ss a = s
            ) m.alphabet) then exit 1;
        ) pred;
    ) m.states

(* Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself*)
let test_nfa_pred_succ n =
    List.iter (fun s -> 
        let pred = Nfa.pred n s in
        List.iter (fun ss ->
            if not (List.exists (fun a ->
                List.mem s (Nfa.succ n ss a)
            ) n.alphabet) then exit 1;
        ) pred;
    ) n.states