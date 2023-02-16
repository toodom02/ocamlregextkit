type state = State of int list | ProductState of state * state
type dfa = {
    states: state array; alphabet: string array; transitions: int array array; start: int; accepting: bool array
}

let rec stringify_state = function
      State n -> "[ " ^ (List.fold_left (fun acc s -> acc ^ string_of_int s ^ " ") "" n) ^ "]"
    | ProductState (l,r) -> "(" ^ stringify_state l ^ " , " ^ stringify_state r ^ ")"

(* |print| -- prints out dfa representation *)
let print m = 
    print_string "states: "; Array.iter (fun s -> print_string (stringify_state s)) m.states; print_newline ();
    print_string "alphabet: "; Array.iter (fun a -> print_string a; print_char ' ') m.alphabet; print_newline ();
    print_string "start: "; print_string (stringify_state m.states.(m.start)); print_newline ();
    print_string "accepting: "; Array.iteri (fun i b -> if b then print_string (stringify_state m.states.(i))) m.accepting; print_newline ();
    print_string "transitions: "; print_newline(); 
        Array.iteri (fun s arr -> Array.iteri (fun a t -> (print_string "    "; print_string (stringify_state m.states.(s)); print_string ("\t--"^m.alphabet.(a)^"-->\t"); print_string (stringify_state m.states.(t)); print_newline())) arr) m.transitions

(* |complement| -- returns the complement of input dfa *)
let complement m = 
    {
        states = m.states;
        alphabet = m.alphabet;
        transitions = m.transitions;
        start = m.start;
        accepting = Array.map (fun s -> not s) m.accepting  
    }

(* |reachable_states| -- returns the set of reachable states in dfa m *)
let reachable_states m = 
    let rec find_reachable_states marked =
        let newmarked = List.fold_right Utils.add_unique (List.concat_map (fun s -> List.map (fun i -> m.transitions.(s).(i)) (List.init (Array.length m.alphabet) Fun.id)) marked) marked in
        if marked <> newmarked then find_reachable_states newmarked
        else List.sort compare newmarked
    in
    find_reachable_states [m.start]

(* |succ| -- the resulting state of dfa m after reading symbol *)
let succ m state symbol = 
    match Utils.array_index symbol m.alphabet with
          None -> raise (Invalid_argument "symbol not in alphabet")
        | Some i -> m.transitions.(state).(i)

(* |prune| -- reduces input dfa by pruning unreachable states *)
let prune m = 
    let marked = reachable_states m in
    let unreachable_states = List.filter (fun i -> not (List.mem i marked)) (List.init (Array.length m.states) Fun.id) in
    let removeIs is = List.fold_left (fun (arr,c) i -> ((Array.append (Array.sub arr 0 (i-c)) (Array.sub arr (i - c + 1) (Array.length arr - (i-c) - 1)), c+1))) (is,0) unreachable_states in
    let (newStates,_) = removeIs m.states and
        (newTrans,_) = removeIs m.transitions and
        (newAcc,_) = removeIs m.accepting in
    Array.iteri (fun i _ ->
        Array.iteri (fun j _ ->
            let t = m.states.(m.transitions.(i).(j)) in
            let newti = Option.get (Utils.array_index t newStates) in
            newTrans.(i).(j) <- newti
        ) m.alphabet    
    ) newStates;
    {
        states = newStates;
        alphabet = m.alphabet;
        start = m.start;
        transitions = newTrans;
        accepting = newAcc
    }

(* |is_empty| -- returns true iff dfa has no reachable accepting states *)
let is_empty m =
    let marked = reachable_states m in
    not (List.exists (fun s -> m.accepting.(s)) marked)

(* |accepted| -- returns the shortest word accepted by dfa m *)
let accepted m =
    let queue = ref [(m.start, "")] and
        seen = ref [] and
        shortest = ref None in
    while Option.is_none !shortest && List.length !queue > 0 do
        let (currentState, currentWord) = List.hd !queue in
        if m.accepting.(currentState) then (shortest := Some(currentWord))
        else (
            seen := currentState::!seen;
            queue := (List.tl !queue) @ 
                List.filter_map (fun a -> 
                    let t = m.transitions.(currentState).(a) in
                    if not (List.mem t !seen) then Some((t, currentWord^m.alphabet.(a))) else None
                ) (List.init (Array.length m.alphabet) Fun.id)
        )
    done;
    !shortest

(* |find_product_trans| -- returns { ((l,r),a,(l',r')) : (l,a,l') ∧ (r,a,r') } *)
let find_product_trans m1 m2 cartStates alphabet = 
    let arr = Array.make_matrix (Array.length cartStates) (Array.length alphabet) (-1) in
    Array.iteri (fun i s ->
        match s with
              ProductState (l,r) ->
                Array.iteri (fun j a ->
                    let lRes = succ m1 (Option.get (Utils.array_index l m1.states)) a and
                        rRes = succ m2 (Option.get (Utils.array_index r m2.states)) a in
                    let k = Option.get (Utils.array_index (ProductState(m1.states.(lRes),m2.states.(rRes))) cartStates) in
                    arr.(i).(j) <- k
                ) alphabet
            | _ -> ()
    ) cartStates;
    arr

let cross_product a b =
    Array.fold_left Array.append [||] (Array.map (fun e1 -> Array.map (fun e2 -> ProductState (e1,e2)) b) a)

(* |product_intersection| -- returns the intersection of two input dfas, using the product construction *)
let product_intersection m1 m2 =
    let cartesianStates = cross_product m1.states m2.states in
    let unionAlphabet = Utils.array_union m1.alphabet m2.alphabet in

    let cartTrans = find_product_trans m1 m2 cartesianStates unionAlphabet and
        cartAccepting = Array.map (fun s ->
            match s with
                  ProductState(l,r) ->
                    let li = Option.get (Utils.array_index l m1.states) and
                        ri = Option.get (Utils.array_index r m2.states) in
                        m1.accepting.(li) && m2.accepting.(ri)
                | _ -> false
        ) cartesianStates in
    {
        states = cartesianStates;
        alphabet = unionAlphabet;
        transitions = cartTrans;
        start = Option.get (Utils.array_index (ProductState (m1.states.(m1.start), m2.states.(m2.start))) cartesianStates);
        accepting = cartAccepting;
    }

(* |product_union| -- returns the union of two input dfas, using the product construction *)
let product_union m1 m2 =
    let cartesianStates = cross_product m1.states m2.states in
    let unionAlphabet = Utils.array_union m1.alphabet m2.alphabet in

    let cartTrans = find_product_trans m1 m2 cartesianStates unionAlphabet and
        cartAccepting = Array.map (fun s ->
            match s with
                  ProductState(l,r) ->
                    let li = Option.get (Utils.array_index l m1.states) and
                        ri = Option.get (Utils.array_index r m2.states) in
                        m1.accepting.(li) || m2.accepting.(ri)
                | _ -> false
        ) cartesianStates in
    {
        states = cartesianStates;
        alphabet = unionAlphabet;
        transitions = cartTrans;
        start = Option.get (Utils.array_index (ProductState (m1.states.(m1.start), m2.states.(m2.start))) cartesianStates);
        accepting = cartAccepting;
    }

(* |disjoin_dfas| -- returns a tuple of disjoint DFAs, over the same alphabet *)
(* NB: Transition functions may no longer be Total *)
(* let disjoin_dfas m1 m2 =
    (* need to merge alphabets and disjoin our DFAs by renaming states in m2, by negative numbers *)
    let rec negate_state = function
          State xs -> State (List.rev_map (fun x -> -x-1) xs)
        | ProductState (s1,s2) -> ProductState (negate_state s1, negate_state s2)
    in

    let merged_alphabet = Utils.list_union m1.alphabet m2.alphabet in

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
    }) *)

(* |hopcroft_equiv| -- returns true iff DFAs are equivalent, by Hopcroft's algorithm *)
(* let hopcroft_equiv m1 m2 =
    let (m1', m2') = disjoin_dfas m1 m2 in
    let merged_states = ref (List.rev_map (fun s -> [s]) (m1'.states @ m2'.states)) and
        stack = ref [] in

    merged_states := List.filter_map (fun s -> if List.mem m2'.start s then Some(m1'.start::s) else if List.mem m1'.start s then None else Some(s)) !merged_states;
    stack := [(m1'.start, m2'.start)];
    while (List.length !stack > 0) do
        let (q1,q2) = List.hd !stack in
        stack := List.tl !stack;
        List.iter (fun a ->
            let succ1 = succ m1' q1 a and succ2 = succ m2' q2 a in
            let r1 = List.find (List.mem succ1) !merged_states and
                r2 = List.find (List.mem succ2) !merged_states in
            if (r1 <> r2) then (
                stack := (succ1, succ2)::!stack;
                merged_states := List.filter_map (fun s -> 
                    if (s = r1) then None
                    else if (s = r2) then Some(r1@s)
                    else Some(s)
                ) !merged_states
            )
        ) m1'.alphabet
    done;

    List.for_all (fun ss ->
        List.for_all (fun s -> List.mem s m1'.accepting || List.mem s m2'.accepting) ss || 
        List.for_all (fun s -> not (List.mem s m1'.accepting || List.mem s m2'.accepting)) ss
    ) !merged_states *)

let symmetric_equiv m1 m2 =
    let comp1 = complement m1 and
        comp2 = complement m2 in
    let m1notm2 = product_intersection m1 comp2 and
        m2notm1 = product_intersection comp1 m2 in
        (is_empty m1notm2) && (is_empty m2notm1)

(* |is_equiv| -- synonym for hopcroft_equiv *)
(* let is_equiv = hopcroft_equiv *)

(* helper func returns true iff state p is within some product state *)
(* let rec contains p ps = 
    if ps = p then true
    else 
        match ps with
              State _ -> false
            | ProductState (s,s') -> contains p s || contains p s' *)

(* |myhill_min| -- returns minimised DFA by myhill nerode *)
(* let myhill_min m =
    let m' = prune m in

    let allpairs = 
        let rec find_pairs xss yss =
            match xss, yss with
                  ([],_) -> []
                | (_,[]) -> []
                | (x::xs,ys) -> List.rev_append (List.rev_map (fun y -> (x, y)) ys) (find_pairs xs (List.tl ys))
        in
        find_pairs m'.states m'.states
    in
    let marked = ref (List.filter (fun (p,q) ->
            (List.mem p m'.accepting && not (List.mem q m'.accepting)) || (not (List.mem p m'.accepting) && List.mem q m'.accepting)
        ) allpairs) in
    let unmarked = ref (List.filter (fun ss -> not (List.mem ss !marked)) allpairs) and
        stop = ref false in

    while (not !stop) do
        stop := true;
        let newunmarked = ref [] in
        List.iter (fun (p,q) ->
            if (List.exists (fun a -> 
                let succp = succ m p a and succq = succ m q a in
                List.mem (succp, succq) !marked || List.mem (succq, succp) !marked
            ) m'.alphabet)
            then (marked := (p,q)::!marked; stop := false) else newunmarked := (p,q)::!newunmarked;
        ) !unmarked;
        unmarked := !newunmarked
    done;

    (* unmarked gives us all pairs of indistinguishable states *)
    (* merge these states! *)
    let merged_states = 
        let merged = ref [] and
            seen = ref [] in
        List.iter (fun (p,q) ->
            if (List.mem p !seen && List.mem q !seen) then (
                let s = List.find (contains p) !merged and
                    s' = List.find (contains q) !merged in
                if (s <> s') then
                    merged := ProductState(s,s')::(List.filter (fun ps -> ps <> s && ps <> s') !merged)
            ) else if (List.mem p !seen) then (
                let s = List.find (contains p) !merged in
                merged := ProductState(s,q)::(List.filter((<>) s) !merged);
                seen := q::!seen
            ) else if (List.mem q !seen) then (
                let s' = List.find (contains q) !merged in
                merged := ProductState(p,s')::(List.filter((<>) s') !merged);
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

    let newtrans = List.fold_left (fun acc (s,a,t) -> Utils.add_unique (List.find (contains s) merged_states, a, List.find (contains t) merged_states) acc) [] m'.transitions and
        newaccepting = List.fold_left (fun acc s -> Utils.add_unique (List.find (contains s) merged_states) acc) [] m'.accepting and
        newstart = List.find (contains m'.start) merged_states in

    {
        states = merged_states;
        alphabet = m'.alphabet;
        transitions = newtrans;
        start = newstart;
        accepting = newaccepting;
    } *)

(* |brzozowski_min| -- minimise input DFA by Brzozowski's algorithm *)
(* let brzozowski_min m =
    let reverse_and_determinise d =
        let get_state s = Option.get (Utils.index s d.states) in
        let newstart = (List.map get_state d.accepting) in
        let newstates = ref [State newstart] and
            newtrans = ref [] and
            stack = ref [newstart] and
            donestates = ref [newstart] in
        
        while (List.length !stack > 0) do
            let currentstate = List.hd !stack in
            stack := List.tl !stack;
            List.iter (fun a ->
                let nextstate = ref [] in
                List.iter (fun (s',a',t) ->
                    if a = a' && List.mem (get_state t) currentstate then (
                        nextstate := Utils.add_unique (get_state s') !nextstate
                    )
                ) d.transitions;

                nextstate := List.sort compare !nextstate;

                if not (List.mem !nextstate !donestates) then (
                    stack := !nextstate::!stack;
                    donestates := !nextstate::!donestates;
                    newstates := Utils.add_unique (State !nextstate) !newstates;
                );

                newtrans := (State currentstate, a, State !nextstate)::!newtrans
            ) d.alphabet
        done;

        let newaccepting = List.filter_map (function
                  State s -> if List.mem (get_state d.start) s then Some(State s) else None
                | _ -> None
            ) !newstates
        in

        {
            states = !newstates;
            alphabet = d.alphabet;
            transitions = !newtrans;
            start = State newstart;
            accepting = newaccepting;
        }
    in
    (* reverse DFA *)
    let drd = reverse_and_determinise m in
    (* reverse Drd *)
    reverse_and_determinise drd *)

(* |hopcroft_min| -- minimise input DFA by Hopcroft's algorithm *)
(* let hopcroft_min m =
    let m' = prune m in

    let p = ref [] in
    let qnotf = List.filter (fun s -> not (List.mem s m'.accepting)) m'.states in
    if List.length qnotf > 0 && List.length m'.accepting > 0 then p := [m'.accepting; qnotf] 
    else if List.length m'.accepting > 0 then p := [m'.accepting]
    else if List.length qnotf > 0 then p := [qnotf];
    let w = ref !p in

    while (List.length !w > 0) do
        let a = List.hd !w in
        w := List.tl !w;
        List.iter (fun c ->
            let x = List.fold_left (fun acc (s,c',t) -> if c = c' && List.mem t a then Utils.add_unique s acc else acc) [] m'.transitions in
            let newp = ref [] in
            List.iter (fun y ->
                let xinty = List.filter (fun s -> List.mem s x) y and
                    ynotx = List.filter (fun s -> not (List.mem s x)) y in
                if (List.length xinty > 0 && List.length ynotx > 0) then (
                    newp := xinty::ynotx::!newp;
                    if (List.mem y !w) then (
                        w := xinty::ynotx::(List.filter ((<>) y) !w)
                    ) else (
                        if List.length xinty <= List.length ynotx then (
                            w := xinty::!w
                        ) else w := ynotx::!w
                    )
                ) else newp := y::!newp
            ) !p;
            p := !newp
        ) m'.alphabet
    done;

    let merged_states = List.fold_left (fun acc ss ->
        if List.length ss > 1 then 
        (List.fold_left (fun acc' s -> ProductState(acc',s)) (List.hd ss) (List.tl ss))::acc
        else (List.hd ss)::acc 
    ) [] !p in

    let newtrans = List.fold_left (fun acc (s,a,t) -> Utils.add_unique (List.find (contains s) merged_states, a, List.find (contains t) merged_states) acc) [] m'.transitions and
        newaccepting = List.fold_left (fun acc s -> Utils.add_unique (List.find (contains s) merged_states) acc) [] m'.accepting and
        newstart = List.find (contains m'.start) merged_states in

    {
        states = merged_states;
        alphabet = m'.alphabet;
        transitions = newtrans;
        start = newstart;
        accepting = newaccepting;
    } *)

(* |minimise| -- synonym for hopcroft_min *)
(* let minimise = hopcroft_min *)

(* |nfa_to_dfa| -- converts nfa to dfa by the subset construction *)
let nfa_to_dfa (n: Nfa.nfa) =
    let newstarti = Nfa.eps_reachable_set n [n.start] in
    let newstart = List.map (fun s -> n.states.(s)) newstarti in
    let epsindex = Option.get (Utils.array_index "ε" n.alphabet) in
    let newalph = Array.append (Array.sub n.alphabet 0 epsindex) (Array.sub n.alphabet (epsindex + 1) (Array.length n.alphabet - epsindex - 1)) in
    let newtrans = ref [||] and
        newstates = ref [|State newstart|] and
        stack = ref [newstarti] and
        donestates = ref [newstarti] in
    while (List.length !stack > 0) do
        let currentstate = List.hd !stack in
        stack := List.tl !stack;
        let currentTrans = Array.make (Array.length newalph) (-1) in
        Array.iteri (fun i a ->
            let oldi = Option.get (Utils.array_index a n.alphabet) in
            let nextstate = List.concat_map (fun s -> n.transitions.(s).(oldi)) currentstate in
            let epsnexti = Nfa.eps_reachable_set n nextstate in
            let epsnext = List.map (fun s -> n.states.(s)) epsnexti in
            if (not (List.mem epsnexti !donestates)) then (
                stack := !stack @ [epsnexti];
                donestates := epsnexti::!donestates;
                newstates := Array.append !newstates [|State epsnext|];
            );
            currentTrans.(i) <- Option.get (Utils.array_index (State epsnext) !newstates);
        ) newalph;
        newtrans := Array.append !newtrans [|currentTrans|];
    done;

    let newaccepting = Array.map (function 
            State ss -> 
            List.exists (fun s ->
                let ind = Option.get (Utils.array_index s n.states) in
                n.accepting.(ind)
            ) ss
        | _ -> false
    ) !newstates in
        
    {
        states = !newstates;
        alphabet = newalph;
        transitions = !newtrans;
        start = 0;
        accepting = newaccepting;
    }

(* |create| -- Creates DFA, Renames states as their index in qs *)
let create qs alph tran init fin =
    let alph = List.fold_right (Utils.add_unique) (List.sort compare alph) [] in
    (* Check parameters for correctness *)
    if not (List.mem init qs) then raise (Invalid_argument "DFA Initial State not in States");
    List.iter (fun f -> 
        if not (List.mem f qs) then raise (Invalid_argument "DFA Accepting State not in States")
    ) fin;
    let checkseentran = ref [] in
    List.iter (fun (s,a,t) ->
        if (a = "ε") then raise (Invalid_argument "DFA cannot contain ε-transitions");
        if (List.mem (s,a) !checkseentran) then raise (Invalid_argument "DFA Transition function not valid") else checkseentran := (s,a)::!checkseentran;
        if not (List.mem a alph && List.mem s qs && List.mem t qs) then raise (Invalid_argument "DFA Transition function not valid")
    ) tran;

    (* last state a sink state *)
    let sinkstatei = List.length qs in
    let newstates = Array.init (List.length qs + 1) (fun i -> State [i]) in
    let newalphabet = Array.init (List.length alph) (List.nth alph) in
    let newinit = Option.get (Utils.index init qs) in
    let newtran = Array.make_matrix (List.length qs + 1) (List.length alph) (sinkstatei) in
    List.iter (fun (s,a,t) ->
        newtran.(Option.get (Utils.index s qs)).(Option.get (Utils.index a alph)) <- (Option.get (Utils.index t qs))
    ) tran;
    let finList = List.rev_map (fun s -> Option.get (Utils.index s qs)) fin in
    let newfin = Array.init (List.length qs + 1) (fun s -> if (List.mem s finList) then true else false) in

    {
        states = newstates;
        alphabet = newalphabet;
        start = newinit;
        accepting = newfin;
        transitions = newtran;
    }