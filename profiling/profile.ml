(* Code to profile equivalence and minimisation functions *)

open Landmark
open Dfa


let generate_random_dfa n =
    Random.self_init ();
    let states = List.init n Fun.id and
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
    create states alphabet transition initial final


let disjoin_dfas m1 m2 =
    let rec negate_state = function
            State xs -> State (List.rev_map (fun x -> -x-1) xs)
          | ProductState (s1,s2) -> ProductState (negate_state s1, negate_state s2)
    in

    let merged_alphabet = Utils.list_union m1.alphabet m2.alphabet in

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
let calce = register "calculate E"
let equivclosure = register "calculate equiv closure"
let symclose = register "calculate symmetric closure"
let refclose = register "calculate reflexive closure"
let tranclose = register "calculate transitive closure"
let calcdelta = register "calculate delta"


let profile_closure_equiv (m1:dfa) (m2:dfa) =
    start_profiling ();

    enter disjoin;
    let (m1', m2') = disjoin_dfas m1 m2 in
    exit disjoin;
    let merged_states = m1'.states @ m2'.states in

    (* Find Reflex, Symmetric, & Transitive closure (Warshall's algo) *)
    let equiv_closure qs =
        if List.length qs = 0 then [] else (
        enter symclose;
        let symclosure = List.fold_left (fun a (q1,q2) -> (q2,q1)::a) qs qs in
        exit symclose; enter refclose;
        let reflexclosure = List.fold_left (fun a s -> (s,s)::a) symclosure merged_states in
        exit refclose; enter tranclose;
        let tranclosure = 
            (* adjacency matrix *)
            let res = ref (List.map (fun q -> List.map (fun q' -> if (List.exists (fun (q1,q2) -> q1 = q && q2 = q') reflexclosure) then 1 else 0) merged_states) merged_states) in

            List.iteri (fun k _ ->
                List.iteri (fun i _ ->
                    List.iteri (fun j _ ->
                        if (List.nth (List.nth !res i) j) <> 1 then
                            (* if res[i][k] = 1 and res[k][j] = 1 *)
                            if (List.nth (List.nth !res i) k) = 1 && (List.nth (List.nth !res k) j) = 1 then
                                (* set res[i][j] to 1 *)
                                res := List.mapi (fun ind xs -> List.mapi (fun ind' x -> if (ind = i && ind' = j) then 1 else x) xs) !res;
                    ) merged_states;
                ) merged_states;
            ) merged_states;

            (* convert back into a list of relations *)
            let rel = ref [] in
            List.iteri (fun i xs ->
                List.iteri (fun j x -> 
                    if x = 1 then rel := (List.nth merged_states i, List.nth merged_states j)::!rel
                ) xs;
            ) !res;
            !rel
        in
        exit tranclose;
        tranclosure
        )
    in

    let delta (u,v) = 
        let res = ref [] and
            union_trans = m1'.transitions @ m2'.transitions in
        List.iter (fun a -> 
            let deltu = ref u and
                deltv = ref v in
            List.iter (fun (s,a',t) ->
                if (a = a') then (
                    if (s = u) then deltu := t
                    else if (s = v) then deltv := t
                )
            ) union_trans;
            res := (!deltu, !deltv)::!res;
        ) m1'.alphabet;
        !res
    in

    enter calce;
    let e = List.concat (List.rev_map (fun e1 -> List.filter_map (fun e2 -> 
        if (List.mem e1 m1'.accepting && List.mem e2 m2'.accepting) then Some(e1,e2)
        else if (List.mem e1 m1'.accepting || List.mem e2 m2'.accepting) then None
        else Some(e1,e2)
        ) m2'.states) m1'.states) in
    exit calce;
    
    let flag = ref false and
        q = ref [] and
        w = ref [(m1'.start, m2'.start)] in
    enter loop;
    while (List.length !w > 0 && not !flag) do
        let uv = List.hd !w in
        w := List.tl !w;

        if not (List.mem uv e) then (
            flag := true;
        ) else (
            enter equivclosure;
            let clos = equiv_closure !q in
            exit equivclosure;
            if not (List.mem uv clos) then (
            q := uv::!q;
            enter calcdelta;
            let newdelt = delta uv in
            exit calcdelta;
            w := (newdelt) @ !w
        ))
    done;
    exit loop

let complimenting = register "Complimenting DFAs"
let product_intersecting = register "Intersecting DFAs"
let product_unioning = register "Unioning DFAs"
let emptiness = register "Checking emptiness"

let profile_symmetric_equiv m1 m2 =
    start_profiling ();
    enter main;
    enter complimenting;
    let comp1 = compliment m1 and
        comp2 = compliment m2 in
    exit complimenting;
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

let profile_hopcroft_equiv m1 m2 =
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

let profile_myhill_min m =
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
    let _ = List.fold_left (fun acc (s,a,t) -> Utils.add_unique (List.find (fun s' -> contains s' s) merged_states, a, List.find (fun t' -> contains t' t) merged_states) acc) [] m'.transitions in
    exit newtransitions; enter newaccept;
    let _ = List.fold_left (fun acc s -> Utils.add_unique (List.find (fun s' -> contains s' s) merged_states) acc) [] m'.accepting in
    exit newaccept; enter newstart;
    let _ = List.find (fun s -> contains s m'.start) merged_states in
    exit newstart;
    exit main

let initialisepartition = register "Initialise partition"
let partitionR = register "Partition R"
let refinePartition = register "Refine Partition"
let calculateX = register "Calculate X"

let profile_hopcroft_min m =
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
            let x = List.fold_left (fun acc (s,c',t) -> if c = c' && List.mem t a then Utils.add_unique s acc else acc) [] m'.transitions in
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
    let _ = List.find (fun state ->
            match state with
                    State [ss] ->  List.exists (fun s -> s = m'.start) (List.nth !p ss)
                | _ -> false
        ) newstates in
    exit newstart;
    enter newaccept;
    let _ = List.filter (fun state -> 
            match state with
                    State [ss] -> List.exists (fun s -> List.mem s m'.accepting) (List.nth !p ss)
                | _ -> false
        ) newstates in
    exit newaccept;
    enter newtransitions;
    let _ = List.fold_left (fun acc (s,a,t) ->
            Utils.add_unique (List.find (fun state ->
                match state with
                        State [ss] ->  List.exists (fun s' -> s' = s) (List.nth !p ss)
                    | _ -> false
            ) newstates, a, List.find (fun state ->
                match state with
                        State [ss] ->  List.exists (fun s' -> s' = t) (List.nth !p ss)
                    | _ -> false
            ) newstates) acc    
        ) [] m'.transitions in
    exit newtransitions;
    exit main

let firstrevanddet = register "1st reverse and determinise"
let revanddet = register "2nd reverse and determinise"
let newstats = register "Find new states"

let profile_brzozowski_min m =
    start_profiling ();
    enter main;
    let reverse_and_determinise d =
        let get_state s = Option.get (Utils.index s d.states) in
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
                        nextstate := Utils.add_unique (get_state s') !nextstate
                    )
                ) d.transitions;
                exit newtransitions;

                nextstate := List.sort compare !nextstate;

                enter newstats;
                if not (List.mem !nextstate !donestates) then (
                    stack := !nextstate::!stack;
                    donestates := !nextstate::!donestates;
                    newstates := Utils.add_unique (State !nextstate) !newstates;
                );
                exit newstats;

                newtrans := (State currentstate, a, State !nextstate)::!newtrans
            ) d.alphabet
        done;

        enter newaccept;
        let newaccepting = List.filter_map (fun state -> 
            match state with
                    State s -> if List.mem (get_state d.start) s then Some(State s) else None
                | ProductState (_,_) -> None
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
        ] (function s -> ()) "ERROR";

    let dfa1 = generate_random_dfa !size in
    profile_hopcroft_min dfa1;
    Printf.printf "\n====================================\n\n%i States\n" (!size+1)

let profile = main ()