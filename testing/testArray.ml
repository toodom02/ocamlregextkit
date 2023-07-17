open Regextkit

(* |test_dfa_pred_succ| -- Tests DFAs pred and succ methods by checking that for each state, the predecessors of a successor contains itself *)
let _test_dfa_pred_succ m =
    if List.exists (fun s ->
        List.exists (fun a ->
            let succ = DfaArray.succ m s a in
            not (List.mem s (DfaArray.pred m succ a))
        ) (DfaArray.get_alphabet m)
    ) (DfaArray.get_states m) then exit 1

(* |test_dfa_total| -- Tests that DFA is total, i.e. each state has exactly one transition for each letter *)
let _test_dfa_total m =
    if not (List.for_all (fun s ->
        List.for_all (fun a ->
            let ts = List.find_all (fun (s',a',_) -> s = s' && a = a') (DfaArray.get_transitions m) in
            List.length ts = 1
        ) (DfaArray.get_alphabet m)
    ) (DfaArray.get_states m)) then exit 1

(* |test_nfa_pred_succ| -- Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself *)
let _test_nfa_pred_succ n =
    if List.exists (fun s ->
        let pred = NfaArray.pred n s in
        List.exists (fun ss ->
            not (List.exists (fun a -> List.mem s (NfaArray.succ n ss a)) (NfaArray.get_alphabet n))
        ) pred
    ) (NfaArray.get_states n) then exit 1

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
    (
        Dfa.create states alphabet transition initial final,
        DfaArray.create states alphabet transition initial final,
        DfaHash.create states alphabet transition initial final
    )


(* circular DFA, with unraveled tail (for minimisation) *)
(* let generate_circular_dfa n =
    let states = List.init (n*2) Fun.id and
        trans = List.init (n*2) (fun i -> if i < n then (i,"a",i+1) else (i,"a",(i+1) mod n + n)) and
        start = 0 and
        accepting = [0;n] in
    (
        Dfa.create states ["a"] trans start accepting,
        DfaArray.create states ["a"] trans start accepting,
        DfaHash.create states ["a"] trans start accepting
    ) *)

(* Generates pairs of DFAs s.t. L(0) = x mod n = r and L(1) = x mod 2n = r or n+r *)
(* let generate_equivalent_modular_dfas n =
    Random.self_init ();
    let diff = if n <= 1 then 0 else Random.int (n-1) in
    let states = List.init (n) Fun.id and
        trans = List.init (n) (fun i -> (i,"a",(i+1) mod n)) and
        start = 0 and
        accepting = [diff] in
    let states2 = List.init (n*2) Fun.id and
        trans2 = List.init (n*2) (fun i -> (i,"a",(i+1) mod (n*2))) and
        start2 = 0 and
        accepting2 = [diff;n+diff] in
    (
        ( 
            Dfa.create states ["a"] trans start accepting,
            Dfa.create states2 ["a"] trans2 start2 accepting2
        ),
        ( 
            DfaArray.create states ["a"] trans start accepting,
            DfaArray.create states2 ["a"] trans2 start2 accepting2
        ),
        ( 
            DfaHash.create states ["a"] trans start accepting,
            DfaHash.create states2 ["a"] trans2 start2 accepting2
        )
    ) *)
(* 
(* Generates 'random' RE *)
let generate_random_regex n =
    Random.self_init ();
    let alphabet = ["a";"b";"c"] in
    let bracks = ref 0 in
    let str = List.init (5*n-1) (fun i -> 
        if (i mod 5 = 1) then (if (Random.float 1. < 0.1) then "?" else List.nth alphabet (Random.int (List.length alphabet)))
        else if (i mod 5 = 2) then (if (Random.float 1. < 0.2 && !bracks > 0) then (decr bracks; ")") else "")
        else if (i mod 5 = 3) then (if (Random.float 1. < 0.1) then "*" else "")
        else if (i mod 5 = 4) then (List.nth ["+";"."] (Random.int 2))
        else if (Random.float 1. < 0.2) then (incr bracks; "(")
        else ""
    ) in
    let str = if !bracks > 0 then str @ List.init !bracks (fun _ ->")") else str in
    let string = String.concat "" str in
    Re.parse string

let _construction_tester () = 
    print_string "CONSTRUCTING DFA\n";
    Printf.printf "Size, Total Nfa->Dfa, Total Brzozowski, Nfa->Dfa, Brzozowski, Minimal, Minimal States, Mean Absolute Error\n";
    let iters = 100 in
    for s = 1 to 50 do
        let cumul_time_subset = ref 0. and
            cumul_time_brzozowski = ref 0. and
            num_minimal = ref 0 and
            num_states = ref 0 and
            error = ref 0 in
        for _ = 1 to iters do
            let re = generate_random_regex s in

            (* Case 1: Nfa->Dfa *)
            let start_1 = Sys.time () in
            let nfa = NfaArray.re_to_nfa re in
            let dfa = DfaArray.nfa_to_dfa nfa in
            let minimal = DfaArray.hopcroft_min dfa in
            cumul_time_subset := (Sys.time () -. start_1) +. !cumul_time_subset;

            (* Case 2: Brzozowski *)
            let start_2 = Sys.time () in
            let dfa2 = DfaArray.re_to_dfa re in
            cumul_time_brzozowski := (Sys.time () -. start_2) +. !cumul_time_brzozowski;

            if not (DfaArray.is_equiv minimal dfa2) then (print_string "Failed\n"; exit 1);
            if (List.length (DfaArray.get_states minimal) = List.length (DfaArray.get_states dfa2)) then incr num_minimal;
            error := (List.length (DfaArray.get_states dfa2) - List.length (DfaArray.get_states minimal)) + !error;
            num_states := List.length (DfaArray.get_states minimal) + !num_states;

        done;
        Printf.printf "%i,%f,%f,%f,%f,%i,%f,%f\n" s !cumul_time_subset !cumul_time_brzozowski (!cumul_time_subset /. (float_of_int iters)) (!cumul_time_brzozowski /. (float_of_int iters)) !num_minimal (float_of_int !num_states /. float_of_int iters) ((float_of_int !error) /. (float_of_int iters));
    done *)

(* Output intended to be saved to CSV *)
let _equiv_tester () = 
    print_string "RANDOM EQUIV\n";
    Printf.printf "# States,Total Hopcroft D,Total Symmetric D,Total Hopcroft A, Total Symmetric A, Total Hopcroft H, Total Symmetric H\n";
    let iters = 10 in
    for s = 1 to 20 do
        let cumul_time_hopcroftD = ref 0. and
            cumul_time_hopcroftA = ref 0. and
            cumul_time_hopcroftH = ref 0. and
            cumul_time_symmetricD = ref 0. and
            cumul_time_symmetricA = ref 0. and
            cumul_time_symmetricH = ref 0. in
        for _ = 1 to iters do
            let (d1,a1,h1) = generate_random_dfa s and
                (d2,a2,h2) = generate_random_dfa s in

            (* case 1: Hopcroft equiv *)
            let start_2D = Sys.time () in
            let res_2D = Dfa.hopcroft_equiv d1 d2 in
            cumul_time_hopcroftD  := (Sys.time () -. start_2D) +. !cumul_time_hopcroftD;
            let start_2A = Sys.time () in
            let res_2A = DfaArray.hopcroft_equiv a1 a2 in
            cumul_time_hopcroftA  := (Sys.time () -. start_2A) +. !cumul_time_hopcroftA;
            let start_2H = Sys.time () in
            let res_2H = DfaHash.hopcroft_equiv h1 h2 in
            cumul_time_hopcroftH  := (Sys.time () -. start_2H) +. !cumul_time_hopcroftH;

            (* case 2: Symmetric equiv *)
            let start_3D = Sys.time () in
            let res_3D = Dfa.symmetric_equiv d1 d2 in
            cumul_time_symmetricD := (Sys.time () -. start_3D) +. !cumul_time_symmetricD;
            let start_3A = Sys.time () in
            let res_3A = DfaArray.symmetric_equiv a1 a2 in
            cumul_time_symmetricA := (Sys.time () -. start_3A) +. !cumul_time_symmetricA;
            let start_3H = Sys.time () in
            let res_3H = DfaHash.symmetric_equiv h1 h2 in
            cumul_time_symmetricH := (Sys.time () -. start_3H) +. !cumul_time_symmetricH;

            (* Sanity check that our results are the same *)
            if res_2D <> res_3D then (print_string "Failed\n"; exit 1);
            if res_2A <> res_3A then (print_string "Failed\n"; exit 1);
            if res_2H <> res_3H then (print_string "Failed\n"; exit 1);
            if res_2H <> res_3D then (print_string "Failed\n"; exit 1);
            if res_2H <> res_3A then (print_string "Failed\n"; exit 1);

        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f\n" s !cumul_time_hopcroftD !cumul_time_symmetricD !cumul_time_hopcroftA !cumul_time_symmetricA !cumul_time_hopcroftH !cumul_time_symmetricH ;
    done

(* let _circular_equiv_tester () = 
    print_string "CIRCULAR EQUIV\n";
    Printf.printf "#,Total Hopcroft,Total Symmetric,Hopcroft,Symmetric\n";
    let iters = 100 in
    for s = 1 to 20 do
        let cumul_time_hopcroft = ref 0. and
            cumul_time_symmetric = ref 0. in
        for _ = 1 to iters do
            let (d1,d2) = generate_equivalent_modular_dfas s in

            (* case 1: Hopcroft equiv *)
            let start_2 = Sys.time () in
            let res_2 = DfaArray.hopcroft_equiv d1 d2 in
            cumul_time_hopcroft  := (Sys.time () -. start_2) +. !cumul_time_hopcroft;

            (* case 2: Symmetric equiv *)
            let start_3 = Sys.time () in
            let res_3 = DfaArray.symmetric_equiv d1 d2 in
            cumul_time_symmetric := (Sys.time () -. start_3) +. !cumul_time_symmetric;

            (* Sanity check that our results are the same *)
            if res_2 <> res_3 then (print_string "Failed\n"; exit 1);
        done;
        Printf.printf "%i,%f,%f,%f,%f\n" s !cumul_time_hopcroft !cumul_time_symmetric (!cumul_time_hopcroft /. (float_of_int iters)) (!cumul_time_symmetric /. (float_of_int iters));
    done *)

(* Output intended to be saved to CSV *)
let _min_tester () = 
    print_string "RANDOM MINIMISATION\n";
    Printf.printf "# States,Total Myhill D,Total Hopcroft D,Total Brzozowski D,Total Myhill A,Total Hopcroft A,Total Brzozowski A,Total Myhill H,Total Hopcroft H,Total Brzozowski H\n";
    let iters = 1 in
    for s = 1 to 20 do
        let cumul_time_myhillD = ref 0. and
            cumul_time_myhillA = ref 0. and
            cumul_time_myhillH = ref 0. and
            cumul_time_hopcroftD = ref 0. and
            cumul_time_hopcroftA = ref 0. and
            cumul_time_hopcroftH = ref 0. and
            cumul_time_brzozowskiD = ref 0. and
            cumul_time_brzozowskiA = ref 0. and
            cumul_time_brzozowskiH = ref 0. in
        for _ = 1 to iters do
            let (d,a,h) = generate_random_dfa s in

            (* case 1: Myhill min *)
            let start_1D = Sys.time () in
            let res_1D = Dfa.myhill_min d in
            cumul_time_myhillD := (Sys.time () -. start_1D) +. !cumul_time_myhillD;
            let start_1A = Sys.time () in
            let res_1A = DfaArray.myhill_min a in
            cumul_time_myhillA := (Sys.time () -. start_1A) +. !cumul_time_myhillA;
            let start_1H = Sys.time () in
            let res_1H = DfaHash.myhill_min h in
            cumul_time_myhillH := (Sys.time () -. start_1H) +. !cumul_time_myhillH;

            (* case 2: Hopcroft min *)
            let start_2D = Sys.time () in
            let res_2D = Dfa.hopcroft_min d in
            cumul_time_hopcroftD  := (Sys.time () -. start_2D) +. !cumul_time_hopcroftD;
            let start_2A = Sys.time () in
            let res_2A = DfaArray.hopcroft_min a in
            cumul_time_hopcroftA  := (Sys.time () -. start_2A) +. !cumul_time_hopcroftA;
            let start_2H = Sys.time () in
            let res_2H = DfaHash.hopcroft_min h in
            cumul_time_hopcroftH  := (Sys.time () -. start_2H) +. !cumul_time_hopcroftH;

            (* case 3: Brzozowski min *)
            let start_3D = Sys.time () in
            let res_3D = Dfa.brzozowski_min d in
            cumul_time_brzozowskiD := (Sys.time () -. start_3D) +. !cumul_time_brzozowskiD;
            let start_3A = Sys.time () in
            let res_3A = DfaArray.brzozowski_min a in
            cumul_time_brzozowskiA := (Sys.time () -. start_3A) +. !cumul_time_brzozowskiA;
            let start_3H = Sys.time () in
            let res_3H = DfaHash.brzozowski_min h in
            cumul_time_brzozowskiH := (Sys.time () -. start_3H) +. !cumul_time_brzozowskiH;

            (* Sanity check that our results are the same *)
            if not (Dfa.is_equiv d res_1D) || not (Dfa.is_equiv res_1D res_2D) || not (Dfa.is_equiv res_2D res_3D) then (print_string "Failed\n"; exit 1);
            if not (DfaArray.is_equiv a res_1A) || not (DfaArray.is_equiv res_1A res_2A) || not (DfaArray.is_equiv res_2A res_3A) then (print_string "Failed\n"; exit 1);
            if not (DfaHash.is_equiv h res_1H) || not (DfaHash.is_equiv res_1H res_2H) || not (DfaHash.is_equiv res_2H res_3H) then (print_string "Failed\n"; exit 1);
        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f,%f,%f,%f\n" s !cumul_time_myhillD !cumul_time_hopcroftD !cumul_time_brzozowskiD !cumul_time_myhillA !cumul_time_hopcroftA !cumul_time_brzozowskiA !cumul_time_myhillH !cumul_time_hopcroftH !cumul_time_brzozowskiH;
    done

(* let _min_circular () = 
    print_string "CIRCULAR MINIMISATION\n";
    Printf.printf "#,Total Myhill,Total Hopcroft,Total Brzozowski,Myhill,Hopcroft,Brzozowski\n";
    let iters = 100 in
    for s = 1 to 20 do
        let cumul_time_myhill = ref 0. and
            cumul_time_hopcroft = ref 0. and
            cumul_time_brzozowski = ref 0. in
        for _ = 1 to iters do
            let d = generate_circular_dfa s in

            _test_dfa_pred_succ d;

            (* case 1: Myhill min *)
            let start_1 = Sys.time () in
            let res_1 = DfaArray.myhill_min d in
            cumul_time_myhill := (Sys.time () -. start_1) +. !cumul_time_myhill;

            (* case 2: Hopcroft min *)
            let start_2 = Sys.time () in
            let res_2 = DfaArray.hopcroft_min d in
            cumul_time_hopcroft  := (Sys.time () -. start_2) +. !cumul_time_hopcroft;

            (* case 3: Brzozowski min *)
            let start_3 = Sys.time () in
            let res_3 = DfaArray.brzozowski_min d in
            cumul_time_brzozowski := (Sys.time () -. start_3) +. !cumul_time_brzozowski;

            (* Sanity check that our results are the same *)
            if not (DfaArray.is_equiv d res_1) || not (DfaArray.is_equiv res_1 res_2) || not (DfaArray.is_equiv res_2 res_3) then (print_string "Failed\n"; exit 1);
            if not (List.length (DfaArray.get_states d) > List.length (DfaArray.get_states res_1)) || not (List.length (DfaArray.get_states res_1) = List.length (DfaArray.get_states res_2)) || not (List.length (DfaArray.get_states res_2) = List.length (DfaArray.get_states res_3)) then (print_string "Failed\n"; exit 1);
            if not (List.length (DfaArray.get_transitions res_1) = List.length (DfaArray.get_transitions res_2)) || not (List.length (DfaArray.get_transitions res_2) = List.length (DfaArray.get_transitions res_3)) then (print_string "Failed\n"; exit 1);
        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f\n" s !cumul_time_myhill !cumul_time_hopcroft !cumul_time_brzozowski (!cumul_time_myhill /. (float_of_int iters)) (!cumul_time_hopcroft /. (float_of_int iters)) (!cumul_time_brzozowski /. (float_of_int iters));
    done *)

let () = _min_tester ();