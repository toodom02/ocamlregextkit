
(* |test_dfa_pred_succ| -- Tests DFAs pred and succ methods by checking that for each state, the successor of a predecessor is itself*)
let test_dfa_pred_succ m =
    List.iter (fun s -> 
        let pred = Dfa.pred m s in
        List.iter (fun ss ->
            if not (List.exists (fun a ->
                Dfa.succ m ss a = s
            ) m.alphabet) then exit 1;
        ) pred;
    ) m.states

(* |test_nfa_pred_succ| -- Tests NFAs pred and succ methods by checking that for each state, the successor of a predecessor contains itself*)
let test_nfa_pred_succ n =
    List.iter (fun s -> 
        let pred = Nfa.pred n s in
        List.iter (fun ss ->
            if not (List.exists (fun a ->
                List.mem s (Nfa.succ n ss a)
            ) n.alphabet) then exit 1;
        ) pred;
    ) n.states

(* |generate_random_dfa| -- Generates a randomised DFA of n states, with 50% change of transition to a random state and 10% chance of final state *)
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
                    if (Random.float 1. < 0.5) then  
                        tran := (s,a,List.nth states (Random.int (List.length states)))::!tran
                ) alphabet;
            ) states;
            !tran
        in
    Dfa.create states alphabet transition initial final

(* Output intended to be saved to CSV *)
let equiv_tester () = 
    Printf.printf "# States,Total Equiv,Total Hopcroft,Total Symmetric,Equivalence,Hopcroft,Symmetric\n";
    let cumul_time_1 = ref 0. and
        cumul_time_2 = ref 0. and
        cumul_time_3 = ref 0. in
    let s = ref 1 in
    while !s <= 20 do
        let i = ref 0 in
        while !i < 100 do
            let d1 = generate_random_dfa !s and
                d2 = generate_random_dfa !s in

            (* case 1: Spivey equiv *)
            let start_1 = Sys.time () in
            let res_1 = Dfa.spivey_equiv d1 d2 in
            cumul_time_1 := (Sys.time () -. start_1) +. !cumul_time_1;

            (* case 2: Hopcroft equiv *)
            let start_2 = Sys.time () in
            let res_2 = Dfa.hopcroft_equiv d1 d2 in
            cumul_time_2  := (Sys.time () -. start_2) +. !cumul_time_2;

            (* case 3: Symmetric equiv *)
            let start_3 = Sys.time () in
            let res_3 = Dfa.symmetric_equiv d1 d2 in
            cumul_time_3 := (Sys.time () -. start_3) +. !cumul_time_3;

            (* Sanity check that our results are the same *)
            if res_1 <> res_2 || res_2 <> res_3 then (print_string "Failed\n"; exit 1);
            i := !i + 1
        done;
        Printf.printf "%i,%f,%f,%f,%f,%f,%f\n" !s !cumul_time_1 !cumul_time_2 !cumul_time_3 (!cumul_time_1 /. 100.) (!cumul_time_2 /. 100.) (!cumul_time_3 /. 100.);
        s := !s + 1
    done;  

    exit 0

(* DFA EQUIVALENCE PROFILING: *)

(* 
# States,Total Equiv,Total Hopcroft,Total Symmetric,Equivalence,Hopcroft,Symmetric
1,0.019179,0.002027,0.008271,0.000192,0.000020,0.000083
2,0.096559,0.004719,0.055363,0.000966,0.000047,0.000554
3,0.323253,0.008787,0.253573,0.003233,0.000088,0.002536
4,0.938099,0.015983,1.231936,0.009381,0.000160,0.012319
5,2.154567,0.023768,3.789507,0.021546,0.000238,0.037895
6,4.257602,0.034019,9.702856,0.042576,0.000340,0.097029
7,7.052971,0.043628,19.043564,0.070530,0.000436,0.190436
8,11.920810,0.053763,37.516150,0.119208,0.000538,0.375162
9,18.391339,0.065982,70.708952,0.183913,0.000660,0.707090
10,28.092763,0.077379,126.857153,0.280928,0.000774,1.268572
11,37.288480,0.087334,193.734282,0.372885,0.000873,1.937343
12,50.163428,0.097156,286.227691,0.501634,0.000972,2.862277
13,66.400085,0.108666,439.814623,0.664001,0.001087,4.398146
14,82.617314,0.120168,655.593900,0.826173,0.001202,6.555939
15,109.955797,0.131787,961.134972,1.099558,0.001318,9.611350
16,147.865135,0.146480,1405.575614,1.478651,0.001465,14.055756
17,180.683764,0.161531,2039.359874,1.806838,0.001615,20.393599
18,237.273642,0.178340,2915.697415,2.372736,0.001783,29.156974
19,295.069809,0.195083,4096.657180,2.950698,0.001951,40.966572
20,378.420464,0.215296,5667.295597,3.784205,0.002153,56.672956
*)


let test = equiv_tester ()