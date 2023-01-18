(* Code to profile equivalence and minimisation functions for DFAs *)
(* includes lots of duplicated code in order to profile segments *)

open Landmark
open Regextkit.Dfa

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
    create states alphabet transition initial final

let add_unique e l = 
    if List.mem e l then l else e::l

let rec list_union l1 l2 = 
    match l2 with
          [] -> l1
        | x::xs -> list_union (add_unique x l1) xs

let disjoin_dfas m1 m2 =
    let rec negate_state = function
            State xs -> State (List.rev_map (fun x -> -x-1) xs)
          | ProductState (s1,s2) -> ProductState (negate_state s1, negate_state s2)
    in

    let merged_alphabet = list_union m1.alphabet m2.alphabet in

    (* need to merge alphabets and disjoin our DFAs by renaming states in m2, by negative numbers *)
    ({
        states = m1.states;
        alphabet = merged_alphabet;
        transitions = m1.transitions;
        start = m1.start;
        accepting = m1.accepting;
    },
    {
        states = List.rev_map (fun s -> negate_state s) m2.states;
        alphabet = merged_alphabet;
        transitions = List.rev_map (fun (s,a,t) -> (negate_state s,a,negate_state t)) m2.transitions;
        start = negate_state m2.start;
        accepting = List.rev_map (fun s -> negate_state s) m2.accepting;
    })

let main = register "main"
let disjoin = register "disjoin DFAs"
let loop = register "main loop"
let complementing = register "Complementing DFAs"
let product_intersecting = register "Intersecting DFAs"
let product_unioning = register "Unioning DFAs"
let emptiness = register "Checking emptiness"

let _profile_symmetric_equiv m1 m2 =
    start_profiling ();
    enter main;
    enter complementing;
    let comp1 = complement m1 and
        comp2 = complement m2 in
    exit complementing;
    enter product_intersecting;
    let m1notm2 = product_intersection m1 comp2 and
        m2notm1 = product_intersection comp1 m2 in
    exit product_intersecting;
    enter product_unioning;
    let emp = product_union m1notm2 m2notm1 in
    exit product_unioning;
    enter emptiness;
    let _ = is_empty emp in
    exit emptiness;
    exit main

let successors = register "Find successors"
let mergestates = register "Merge states"
let checkequivalent = register "Check equivalence"
let finding = register "Find merged state"

let _profile_hopcroft_equiv m1 m2 =
    start_profiling ();
    enter main;
    enter disjoin;
    let (m1', m2') = disjoin_dfas m1 m2 in
    exit disjoin;
    let merged_states = ref (List.rev_map (fun s -> [s]) (m1'.states @ m2'.states)) and
        stack = ref [] in

    merged_states := List.filter_map (fun s -> if List.mem m2'.start s then Some(m1'.start::s) else if List.mem m1'.start s then None else Some(s)) !merged_states;
    stack := [(m1'.start, m2'.start)];
    enter loop;
    while (List.length !stack > 0) do
        let (q1,q2) = List.hd !stack in
        stack := List.tl !stack;
        List.iter (fun a ->
            enter successors;
            let succ1 = succ m1' q1 a and succ2 = succ m2' q2 a in
            exit successors;
            enter finding;
            let r1 = List.find (fun s -> List.mem succ1 s) !merged_states and
                r2 = List.find (fun s -> List.mem succ2 s) !merged_states in
                exit finding;
            if (r1 <> r2) then (
                stack := (succ1, succ2)::!stack;
                enter mergestates;
                merged_states := List.filter_map (fun s -> 
                    if (s = r1) then None
                    else if (s = r2) then Some(r1@s)
                    else Some(s)
                ) !merged_states;
                exit mergestates
            )
        ) m1'.alphabet
    done;
    exit loop;

    enter checkequivalent;
    let _ = List.for_all (fun ss ->
        List.for_all (fun s -> List.mem s m1'.accepting || List.mem s m2'.accepting) ss || 
        List.for_all (fun s -> not (List.mem s m1'.accepting || List.mem s m2'.accepting)) ss
    ) !merged_states in
    exit checkequivalent;
    exit main

let prunedfa = register "Prune DFA"
let findpairs = register "Find pairs of states"
let newtransitions = register "Find new transitions"
let newaccept = register "Find new accepting states"
let newstart = register "Find new start states"
let checkmarked = register "Check if state is marked"
let initialmarked = register "Initialise marked states"

let _profile_myhill_min m =
    start_profiling ();
    enter main;
    enter prunedfa;
    let m' = prune m in
    exit prunedfa;
    enter findpairs;
    let allpairs = 
        let rec find_pairs xss yss =
            match xss, yss with
                  ([],_) -> []
                | (_,[]) -> []
                | (x::xs,ys) -> List.rev_append (List.rev_map (fun y -> (x, y)) ys) (find_pairs xs (List.tl ys))
        in
        find_pairs m'.states m'.states
    in
    exit findpairs;
    enter initialmarked;
    let marked = ref (List.filter (fun (p,q) ->
            (List.mem p m'.accepting && not (List.mem q m'.accepting)) || (not (List.mem p m'.accepting) && List.mem q m'.accepting)
        ) allpairs) in
    let unmarked = ref (List.filter (fun ss -> not (List.mem ss !marked)) allpairs) and
        stop = ref false in
    exit initialmarked;
    enter loop;
    while (not !stop) do
        stop := true;
        let newunmarked = ref [] in
        List.iter (fun (p,q) ->
            if (List.exists (fun a -> 
                enter successors;
                let succp = succ m p a and succq = succ m q a in
                exit successors;
                enter checkmarked;
                let t = List.mem (succp, succq) !marked || List.mem (succq, succp) !marked in
                exit checkmarked;
                t
            ) m'.alphabet)
            then (marked := (p,q)::!marked; stop := false) else newunmarked := (p,q)::!newunmarked;
        ) !unmarked;
        unmarked := !newunmarked
    done;
    exit loop;

    (* unmarked gives us all pairs of indistinguishable states *)
    (* merge these states! *)

    let rec contains ps p = 
        if ps = p then true
        else 
            match ps with
                  State _ -> false
                | ProductState (s,s') -> contains s p || contains s' p
    in

    enter mergestates;
    let merged_states = 
        let merged = ref [] and
            seen = ref [] in
        List.iter (fun (p,q) ->
            if (List.mem p !seen && List.mem q !seen) then (
                let s = List.find (fun ps -> contains ps p) !merged and
                    s' = List.find (fun ps -> contains ps q) !merged in
                if (s <> s') then
                    merged := ProductState(s,s')::(List.filter (fun ps -> ps <> s && ps <> s') !merged)
            ) else if (List.mem p !seen) then (
                let s = List.find (fun ps -> contains ps p) !merged in
                merged := ProductState(s,q)::(List.filter(fun ps -> ps <> s) !merged);
                seen := q::!seen
            ) else if (List.mem q !seen) then (
                let s' = List.find (fun ps -> contains ps q) !merged in
                merged := ProductState(p,s')::(List.filter(fun ps -> ps <> s') !merged);
                seen := p::!seen
            ) else if (p = q) then (
                merged := p::!merged;
                seen := p::!seen
            ) else (
                merged := ProductState(p,q)::!merged;
                seen := p::q::!seen
            )
        ) !unmarked;
        !merged
    in
    exit mergestates;

    enter newtransitions;
    let _ = List.fold_left (fun acc (s,a,t) -> add_unique (List.find (fun s' -> contains s' s) merged_states, a, List.find (fun t' -> contains t' t) merged_states) acc) [] m'.transitions in
    exit newtransitions; enter newaccept;
    let _ = List.fold_left (fun acc s -> add_unique (List.find (fun s' -> contains s' s) merged_states) acc) [] m'.accepting in
    exit newaccept; enter newstart;
    let _ = List.find (fun s -> contains s m'.start) merged_states in
    exit newstart;
    exit main

let initialisepartition = register "Initialise partition"
let partitionR = register "Partition R"
let refinePartition = register "Refine Partition"
let calculateX = register "Calculate X"

let _profile_hopcroft_min m =
    start_profiling ();
    enter main;
    enter prunedfa;
    let m' = prune m in
    exit prunedfa;

    enter initialisepartition;
    let p = ref [] in
    let qnotf = List.filter (fun s -> not (List.mem s m'.accepting)) m'.states in
    if List.length qnotf > 0 && List.length m'.accepting > 0 then p := [m'.accepting; qnotf] 
    else if List.length m'.accepting > 0 then p := [m'.accepting]
    else if List.length qnotf > 0 then p := [qnotf];
    let w = ref !p in
    exit initialisepartition;

    enter loop;
    while (List.length !w > 0) do
        let a = List.hd !w in
        w := List.tl !w;
        List.iter (fun c ->
            enter calculateX;
            let x = List.fold_left (fun acc (s,c',t) -> if c = c' && List.mem t a then add_unique s acc else acc) [] m'.transitions in
            exit calculateX;
            let newp = ref [] in
            List.iter (fun y ->
                enter partitionR;
                let xinty = List.filter (fun s -> List.mem s x) y and
                    ynotx = List.filter (fun s -> not (List.mem s x)) y in
                exit partitionR;
                enter refinePartition;
                if (List.length xinty > 0 && List.length ynotx > 0) then (
                    newp := xinty::ynotx::!newp;
                    if (List.mem y !w) then (
                        w := xinty::ynotx::(List.filter (fun s -> s <> y) !w)
                    ) else (
                        if List.length xinty <= List.length ynotx then (
                            w := xinty::!w
                        ) else w := ynotx::!w
                    )
                ) else newp := y::!newp;
                exit refinePartition;
            ) !p;
            p := !newp
        ) m'.alphabet
    done;
    exit loop;

    let newstates = List.init (List.length !p) (fun s -> State [s]) in
    enter newstart;
    let _ = List.find (function
              State [ss] ->  List.exists (fun s -> s = m'.start) (List.nth !p ss)
            | _ -> false
        ) newstates in
    exit newstart;
    enter newaccept;
    let _ = List.filter (function
              State [ss] -> List.exists (fun s -> List.mem s m'.accepting) (List.nth !p ss)
            | _ -> false
        ) newstates in
    exit newaccept;
    enter newtransitions;
    let _ = List.fold_left (fun acc (s,a,t) ->
            add_unique (List.find (function
                  State [ss] ->  List.exists (fun s' -> s' = s) (List.nth !p ss)
                | _ -> false
            ) newstates, a, List.find (function
                  State [ss] ->  List.exists (fun s' -> s' = t) (List.nth !p ss)
                | _ -> false
            ) newstates) acc    
        ) [] m'.transitions in
    exit newtransitions;
    exit main

let firstrevanddet = register "1st reverse and determinise"
let revanddet = register "2nd reverse and determinise"
let newstats = register "Find new states"

let index x xs = 
    let rec aux ls c =
        match ls with
              [] -> None
            | y::ys -> if (x = y) then Some(c) else aux ys (c+1)
    in
    aux xs 0

let _profile_brzozowski_min m =
    start_profiling ();
    enter main;
    let reverse_and_determinise d =
        let get_state s = Option.get (index s d.states) in
        let newstart = (List.map (fun s -> get_state s) d.accepting) in
        let newstates = ref [State newstart] and
            newtrans = ref [] and
            stack = ref [newstart] and
            donestates = ref [newstart] in
        
        while (List.length !stack > 0) do
            let currentstate = List.hd !stack in
            stack := List.tl !stack;
            List.iter (fun a ->
                let nextstate = ref [] in
                enter newtransitions;
                List.iter (fun (s',a',t) ->
                    if a = a' && List.mem (get_state t) currentstate then (
                        nextstate := add_unique (get_state s') !nextstate
                    )
                ) d.transitions;
                exit newtransitions;

                nextstate := List.sort compare !nextstate;

                enter newstats;
                if not (List.mem !nextstate !donestates) then (
                    stack := !nextstate::!stack;
                    donestates := !nextstate::!donestates;
                    newstates := add_unique (State !nextstate) !newstates;
                );
                exit newstats;

                newtrans := (State currentstate, a, State !nextstate)::!newtrans
            ) d.alphabet
        done;

        enter newaccept;
        let newaccepting = List.filter_map (function
                  State s -> if List.mem (get_state d.start) s then Some(State s) else None
                | _ -> None
            ) !newstates
        in
        exit newaccept;

        {
            states = !newstates;
            alphabet = d.alphabet;
            transitions = !newtrans;
            start = State newstart;
            accepting = newaccepting;
        }
    in
    enter firstrevanddet;
    (* reverse DFA *)
    let drd = reverse_and_determinise m in
    (* reverse Drd *)
    exit firstrevanddet;
    enter revanddet;
    let _ = reverse_and_determinise drd in
    exit revanddet;
    exit main

let main () = 
    let size = ref 0 in
    Arg.parse [
        ("-a",Arg.Int (function i -> size:=i),"");
        ] (fun _ -> ()) "ERROR";

    let dfa1 = generate_random_dfa !size in
    _profile_hopcroft_equiv dfa1 dfa1;
    Printf.printf "\n====================================\n\n%i States\n" (!size+1)

let () = main ()