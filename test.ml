
(* |test_dfa_pred_succ| -- Tests DFAs pred and succ methods by checking that for each state, the predecessors of a successor contains itself *)
let test_dfa_pred_succ m =
    if List.exists (fun s ->
        List.exists (fun a ->
            let succ = Dfa.succ m s a in
            not (List.mem s (Dfa.pred m succ a))
        ) m.alphabet
    ) m.states then exit 1

(* |test_nfa_pred_succ| -- Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself *)
let test_nfa_pred_succ n =
    if List.exists (fun s ->
        let pred = Nfa.pred n s in
        List.exists (fun ss ->
            not (List.exists (fun a -> List.mem s (Nfa.succ n ss a)) n.alphabet)
        ) pred
    ) n.states then exit 1

(* |generate_random_dfa| -- Generates a randomised DFA of n states, with each state having transitions to random states and 10% chance of final state *)
let generate_random_dfa n =
    Random.self_init ();
    let states = List.init n succ and
        alphabet = ["a";"b"] in
    let initial = List.hd states and
        final = List.filter (fun _ -> (Random.float 1. < 0.1)) states and
        transition = 
            let tran = ref [] in
            List.iter (fun s ->
                List.iter (fun a ->
                    tran := (s,a,List.nth states (Random.int (List.length states)))::!tran
                ) alphabet;
            ) states;
            !tran
        in
    Dfa.create states alphabet transition initial final

(* Output intended to be saved to CSV *)
let equiv_tester () = 
    Printf.printf "# States,Total Equiv,Total Hopcroft,Total Symmetric,Equivalence,Hopcroft,Symmetric,Total Equiv Same,Total Hopcroft Same,Total Symmetric Same,Equivalence,Hopcroft,Symmetric\n";
    let cumul_time_closure = ref 0. and
        cumul_time_hopcroft = ref 0. and
        cumul_time_symmetric = ref 0. and
        cumul_time_closure_same = ref 0. and
        cumul_time_hopcroft_same = ref 0. and
        cumul_time_symmetric_same = ref 0. in
    let s = ref 1 in
    while !s <= 20 do
        let i = ref 0 in
        while !i < 100 do
            let d1 = generate_random_dfa !s and
                d2 = generate_random_dfa !s in

            (* case 1: Closure equiv *)
            let start_1 = Sys.time () in
            let res_1 = Dfa.closure_equiv d1 d2 in
            cumul_time_closure := (Sys.time () -. start_1) +. !cumul_time_closure;

            (* case 2: Hopcroft equiv *)
            let start_2 = Sys.time () in
            let res_2 = Dfa.hopcroft_equiv d1 d2 in
            cumul_time_hopcroft  := (Sys.time () -. start_2) +. !cumul_time_hopcroft;

            (* case 3: Symmetric equiv *)
            let start_3 = Sys.time () in
            let res_3 = Dfa.symmetric_equiv d1 d2 in
            cumul_time_symmetric := (Sys.time () -. start_3) +. !cumul_time_symmetric;

            (* Sanity check that our results are the same *)
            if res_1 <> res_2 || res_2 <> res_3 then (print_string "Failed\n"; exit 1);

            (* Now testing identical DFAs *)

            (* case 1: Closure equiv *)
            let start_11 = Sys.time () in
            let res_11 = Dfa.closure_equiv d1 d1 in
            cumul_time_closure_same := (Sys.time () -. start_11) +. !cumul_time_closure_same;

            (* case 2: Hopcroft equiv *)
            let start_12 = Sys.time () in
            let res_12 = Dfa.hopcroft_equiv d1 d1 in
            cumul_time_hopcroft_same  := (Sys.time () -. start_12) +. !cumul_time_hopcroft_same;

            (* case 3: Symmetric equiv *)
            let start_13 = Sys.time () in
            let res_13 = Dfa.symmetric_equiv d1 d1 in
            cumul_time_symmetric_same := (Sys.time () -. start_13) +. !cumul_time_symmetric_same;

            (* Sanity check that our results are the same *)
            if res_11 <> res_12 || res_12 <> res_13 then (print_string "Failed\n"; exit 1);

            i := !i + 1
        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f" !s !cumul_time_closure !cumul_time_hopcroft !cumul_time_symmetric (!cumul_time_closure /. 100.) (!cumul_time_hopcroft /. 100.) (!cumul_time_symmetric /. 100.);
        Printf.printf "%i,%f,%f,%f,%f,%f,%f\n" !s !cumul_time_closure_same !cumul_time_hopcroft_same !cumul_time_symmetric_same (!cumul_time_closure_same /. 100.) (!cumul_time_hopcroft_same /. 100.) (!cumul_time_symmetric_same /. 100.);
        s := !s + 1
    done;  

    exit 0

(* Output intended to be saved to CSV *)
let min_tester () = 
    Printf.printf "# States,Total Myhill,Total Hopcroft,Total Brzozowski,Myhill,Hopcroft,Brzozowski\n";
    let cumul_time_myhill = ref 0. and
        cumul_time_hopcroft = ref 0. and
        cumul_time_brzozowski = ref 0. in
    let s = ref 1 in
    while !s <= 20 do
        let i = ref 0 in
        while !i < 100 do
            let d = generate_random_dfa !s in

            test_dfa_pred_succ d;

            (* case 1: Myhill min *)
            let start_1 = Sys.time () in
            let res_1 = Dfa.myhill_min d in
            cumul_time_myhill := (Sys.time () -. start_1) +. !cumul_time_myhill;

            (* case 2: Hopcroft equiv *)
            let start_2 = Sys.time () in
            let res_2 = Dfa.hopcroft_min d in
            cumul_time_hopcroft  := (Sys.time () -. start_2) +. !cumul_time_hopcroft;

            (* case 3: Brzozowski equiv *)
            let start_3 = Sys.time () in
            let res_3 = Dfa.brzozowski_min d in
            cumul_time_brzozowski := (Sys.time () -. start_3) +. !cumul_time_brzozowski;

            (* Sanity check that our results are the same *)
            if not (Dfa.is_equiv d res_1) || not (Dfa.is_equiv res_1 res_2) || not (Dfa.is_equiv res_2 res_3) then (print_string "Failed\n"; exit 1);
            if not (List.length d.states >= List.length res_1.states) || not (List.length res_1.states = List.length res_2.states) || not (List.length res_2.states = List.length res_3.states) then (print_string "Failed\n"; exit 1);
            if not (List.length res_1.transitions = List.length res_2.transitions) || not (List.length res_2.transitions = List.length res_3.transitions) then (print_string "Failed\n"; exit 1);

            i := !i + 1
        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f\n" !s !cumul_time_myhill !cumul_time_hopcroft !cumul_time_brzozowski (!cumul_time_myhill /. 100.) (!cumul_time_hopcroft /. 100.) (!cumul_time_brzozowski /. 100.);
        s := !s + 1
    done;  

    exit 0

let test = equiv_tester ()