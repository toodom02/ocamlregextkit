open Regextkit

(* |test_dfa_pred_succ| -- Tests DFAs pred and succ methods by checking that for each state, the predecessors of a successor contains itself *)
let _test_dfa_pred_succ (m: Dfa.dfa) =
    if List.exists (fun s ->
        List.exists (fun a ->
            let succ = Dfa.succ m s a in
            not (List.mem s (Dfa.pred m succ a))
        ) m.alphabet
    ) m.states then exit 1

(* |test_nfa_pred_succ| -- Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself *)
let _test_nfa_pred_succ (n: Nfa.nfa) =
    if List.exists (fun s ->
        let pred = Nfa.pred n s in
        List.exists (fun ss ->
            not (List.exists (fun a -> List.mem s (Nfa.succ n ss a)) n.alphabet)
        ) pred
    ) n.states then exit 1

(* |test_dfa_total| -- Tests that DFA is total, i.e. each state has exactly one transition for each letter *)
let _test_dfa_total (m: Dfa.dfa) =
    if not (List.for_all (fun s ->
        List.for_all (fun a ->
            let ts = List.find_all (fun (s',a',_) -> s = s' && a = a') m.transitions in
            List.length ts = 1
        ) m.alphabet
    ) m.states) then exit 1

(* |generate_random_dfa| -- Generates a randomised DFA of n connected states, with each state having transitions to random states and 10% chance of final state *)
let generate_random_dfa n =
    Random.self_init ();
    let states = List.init n Fun.id and
        alphabet = ["a";"b"] in
    let initial = List.hd states and
        final = List.filter (fun _ -> (Random.float 1. < 0.1)) states in
    let transition = 
        let tran = ref [] and
            connected = ref [initial] and
            unconnected = ref (List.tl states) in
        (* create connected graph *)
        while (List.length !unconnected > 0) do
            let dest = List.hd !unconnected in
            unconnected := List.tl !unconnected;
            let eligable_states = List.filter (fun s -> not (List.for_all (fun a -> List.exists (fun (s',a',_) -> s'=s && a'=a) !tran) alphabet)) !connected in
            let randconnected = List.nth eligable_states (Random.int (List.length eligable_states)) in
            let eligable_letters = List.filter (fun a -> not (List.exists (fun (s,a',_) -> s = randconnected && a = a') !tran)) alphabet in
            let randletter = List.nth eligable_letters (Random.int (List.length eligable_letters)) in
            tran := (randconnected, randletter, dest)::!tran;
            connected := dest::!connected;
        done;
        (* fill in remaining transitions *)
        List.iter (fun s ->
            List.iter (fun a ->
                if not (List.exists (fun (s',a',_) -> s = s' && a = a') !tran) then (
                    let randomstate = List.nth states (Random.int (List.length states)) in
                    tran := (s,a,randomstate)::!tran
                )
            ) alphabet;
        ) states;
        !tran
    in
    (* add trivial state for non-minimality *)
    let states = -1::states in
    let transition = (-1,"a",-1)::(-1,"b",-1)::transition in
    Dfa.create states alphabet transition initial final

(* Output intended to be saved to CSV *)
let _equiv_tester () = 
    Printf.printf "# States,Total Hopcroft,Total Symmetric,Hopcroft,Symmetric,Total Hopcroft Same,Total Symmetric Same,Hopcroft,Symmetric\n";
    let iters = 1000 in
    for s = 1 to 20 do
        let cumul_time_hopcroft = ref 0. and
            cumul_time_symmetric = ref 0. and
            cumul_time_hopcroft_same = ref 0. and
            cumul_time_symmetric_same = ref 0. in
        for _ = 1 to iters do
            let d1 = generate_random_dfa s and
                d2 = generate_random_dfa s in

            (* case 1: Hopcroft equiv *)
            let start_2 = Sys.time () in
            let res_2 = Dfa.hopcroft_equiv d1 d2 in
            cumul_time_hopcroft  := (Sys.time () -. start_2) +. !cumul_time_hopcroft;

            (* case 2: Symmetric equiv *)
            let start_3 = Sys.time () in
            let res_3 = Dfa.symmetric_equiv d1 d2 in
            cumul_time_symmetric := (Sys.time () -. start_3) +. !cumul_time_symmetric;

            (* Sanity check that our results are the same *)
            if res_2 <> res_3 then (print_string "Failed\n"; exit 1);

            (* Now testing identical DFAs *)

            (* case 1: Hopcroft equiv *)
            let start_12 = Sys.time () in
            let res_12 = Dfa.hopcroft_equiv d1 d1 in
            cumul_time_hopcroft_same  := (Sys.time () -. start_12) +. !cumul_time_hopcroft_same;

            (* case 2: Symmetric equiv *)
            let start_13 = Sys.time () in
            let res_13 = Dfa.symmetric_equiv d1 d1 in
            cumul_time_symmetric_same := (Sys.time () -. start_13) +. !cumul_time_symmetric_same;

            (* Sanity check that our results are the same *)
            if res_12 <> res_13 then (print_string "Failed\n"; exit 1);
        done;
        Printf.printf "%i,%f,%f,%f,%f," s !cumul_time_hopcroft !cumul_time_symmetric (!cumul_time_hopcroft /. (float_of_int iters)) (!cumul_time_symmetric /. (float_of_int iters));
        Printf.printf "%f,%f,%f,%f\n" !cumul_time_hopcroft_same !cumul_time_symmetric_same (!cumul_time_hopcroft_same /. (float_of_int iters)) (!cumul_time_symmetric_same /. (float_of_int iters));
    done

(* Output intended to be saved to CSV *)
let _min_tester () = 
    Printf.printf "# States,Total Myhill,Total Hopcroft,Total Brzozowski,Myhill,Hopcroft,Brzozowski\n";
    let iters = 1000 in
    for s = 1 to 20 do
        let cumul_time_myhill = ref 0. and
            cumul_time_hopcroft = ref 0. and
            cumul_time_brzozowski = ref 0. in
        for _ = 1 to iters do
            let d = generate_random_dfa s in

            _test_dfa_pred_succ d;

            (* case 1: Myhill min *)
            let start_1 = Sys.time () in
            let res_1 = Dfa.myhill_min d in
            cumul_time_myhill := (Sys.time () -. start_1) +. !cumul_time_myhill;

            (* case 2: Hopcroft min *)
            let start_2 = Sys.time () in
            let res_2 = Dfa.hopcroft_min d in
            cumul_time_hopcroft  := (Sys.time () -. start_2) +. !cumul_time_hopcroft;

            (* case 3: Brzozowski min *)
            let start_3 = Sys.time () in
            let res_3 = Dfa.brzozowski_min d in
            cumul_time_brzozowski := (Sys.time () -. start_3) +. !cumul_time_brzozowski;

            (* Sanity check that our results are the same *)
            if not (Dfa.is_equiv d res_1) || not (Dfa.is_equiv res_1 res_2) || not (Dfa.is_equiv res_2 res_3) then (print_string "Failed\n"; exit 1);
            if not (List.length d.states >= List.length res_1.states) || not (List.length res_1.states = List.length res_2.states) || not (List.length res_2.states = List.length res_3.states) then (print_string "Failed\n"; exit 1);
            if not (List.length res_1.transitions = List.length res_2.transitions) || not (List.length res_2.transitions = List.length res_3.transitions) then (print_string "Failed\n"; exit 1);
        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f\n" s !cumul_time_myhill !cumul_time_hopcroft !cumul_time_brzozowski (!cumul_time_myhill /. (float_of_int iters)) (!cumul_time_hopcroft /. (float_of_int iters)) (!cumul_time_brzozowski /. (float_of_int iters));
    done

let () = _equiv_tester (); _min_tester ()