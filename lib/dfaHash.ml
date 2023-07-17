type state = State of int list | ProductState of state * state
type dfa = state AdtHash.automata

type product_op = Union | Intersection | SymmetricDifference

let get_states = AdtHash.get_states
let get_alphabet = AdtHash.get_alphabet
let get_transitions = AdtHash.get_transitions
let get_start = AdtHash.get_start
let get_accepting = AdtHash.get_accepting

(* |is_accepting| -- returns true if state s is accepting *)
let is_accepting = AdtHash.is_accepting

(* |succ| -- the resulting state of dfa m after reading symbol *)
let succ m s a = List.hd (AdtHash.get_next_states m s a)

(* |pred| -- returns the state preceeding state in dfa m before reading symbol *)
let pred = AdtHash.get_prev_states

let rec stringify_state = function
  | State n -> "[ " ^ (List.fold_left (fun acc s -> acc ^ string_of_int s ^ " ") "" n) ^ "]"
  | ProductState (l,r) -> "(" ^ stringify_state l ^ " , " ^ stringify_state r ^ ")"

(* |print| -- prints out dfa representation *)
let print m = 
    print_string "states: "; List.iter (fun s -> print_string (stringify_state s)) (get_states m); print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') (get_alphabet m); print_newline ();
    print_string "start: "; print_string (stringify_state (get_start m)); print_newline ();
    print_string "accepting: "; List.iter (fun s -> print_string (stringify_state s)) (get_accepting m); print_newline ();
    print_string "transitions: "; print_newline (); List.iter (fun (s,a,t) -> print_string "    "; print_string (stringify_state s); print_string ("\t--"^a^"-->\t"); print_string (stringify_state t); print_newline ()) (get_transitions m)

(* |export_graphviz| -- exports the dfa in the DOT language for Graphviz *)
let export_graphviz d =
    Printf.sprintf "digraph G {\n n0 [label=\"\", shape=none, height=0, width=0, ]\n%s\nn0 -> \"%s\";\n%s\n}"

    (List.fold_left (fun a s -> 
        let shape = "ellipse, " ^ if AdtHash.is_accepting d s then "peripheries=2, " else "" in
        Printf.sprintf "%s\"%s\" [shape=%s];\n" a (stringify_state s) shape
      ) "" (get_states d))
        
    (stringify_state (get_start d))

    (List.fold_left (fun acc (s,a,t) ->
            Printf.sprintf "%s\"%s\" -> \"%s\" [label=\"%s\", ];\n" acc (stringify_state s) (stringify_state t) a
    ) "" (get_transitions d))

(* |complement| -- returns the complement of input dfa *)
let complement m = 
    AdtHash.create_automata (get_states m) (get_alphabet m) (get_transitions m) (get_start m) (List.filter (fun s -> not (is_accepting m s)) (get_states m))

(* |reachable_states| -- returns the set of reachable states in dfa m *)
let reachable_states = AdtHash.get_reachable_states

(* |prune| -- reduces input dfa by pruning unreachable states *)
let prune m = 
    let marked = reachable_states m in
    AdtHash.create_automata (List.filter (fun s -> List.mem s marked) (get_states m)) (get_alphabet m) (List.filter (fun (s,_,_) -> List.mem s marked) (get_transitions m)) (get_start m) (List.filter (fun s -> List.mem s marked) (get_accepting m))

(* |is_empty| -- returns true iff dfa has no reachable accepting states *)
let is_empty m =
    let marked = reachable_states m in
    not (List.exists (is_accepting m) marked)

(* |is_accepted| -- returns true iff string s is accepted by the dfa m *)
let is_accepted m s =
    let rec does_accept state = function
        | "" -> is_accepting m state
        | str -> does_accept (succ m state (String.make 1 str.[0])) (String.sub str 1 ((String.length str) - 1))
    in
    does_accept (get_start m) s

(* |get_accepted| -- returns the shortest word accepted by dfa m *)
let get_accepted m =
    let queue = ref [(get_start m, "")] and
        seen = ref [] and
        shortest = ref None in
    while Option.is_none !shortest && List.length !queue > 0 do
        let (currentState, currentWord) = List.hd !queue in
        if is_accepting m currentState then (shortest := Some(currentWord))
        else (
            seen := currentState::!seen;
            let newt = List.filter_map (fun a -> 
                let t = succ m currentState a in
                if not (List.mem t !seen) then Some((t, currentWord^a)) else None
            ) (get_alphabet m) in
            queue := (List.tl !queue) @ newt
        )
    done;
    !shortest

let product_construction op m1 m2 =
    let cross_product a b =
        List.concat (List.rev_map (fun e1 -> List.rev_map (fun e2 -> ProductState(e1,e2)) b) a)
    in
    (* |find_product_trans| -- returns { ((l,r),a,(l',r')) : (l,a,l') ∧ (r,a,r') } *)
    let find_product_trans m1 m2 cartStates alphabet = 
        List.fold_left (fun acc s ->
            match s with
                | ProductState (l,r) -> 
                    List.fold_left (fun acc' a ->
                        let lRes = succ m1 l a and rRes = succ m2 r a in
                        (ProductState(l,r),a,ProductState(lRes,rRes))::acc'
                    ) acc alphabet
                | _ -> acc
        ) [] cartStates
    in
    let cartesianStates = cross_product (get_states m1) (get_states m2) in
    let unionAlphabet = Utils.list_union (get_alphabet m1) (get_alphabet m2) in
    let cartTrans = find_product_trans m1 m2 cartesianStates unionAlphabet and
        cartAccepting = List.filter (function 
            | ProductState (l,r) -> (
                match op with 
                    | Union -> is_accepting m1 l || is_accepting m2 r
                    | Intersection -> is_accepting m1 l && is_accepting m2 r
                    | SymmetricDifference -> is_accepting m1 l && not (is_accepting m2 r) || not (is_accepting m1 l) && is_accepting m2 r
                )
            | _ -> false
        ) cartesianStates in
    AdtHash.create_automata cartesianStates unionAlphabet cartTrans (ProductState (get_start m1, get_start m2)) cartAccepting

(* |product_intersection| -- returns the intersection of two input dfas, using the product construction *)
let product_intersection = product_construction Intersection

(* |product_union| -- returns the union of two input dfas, using the product construction *)
let product_union = product_construction Union

(* |product_difference| -- returns the symmetric difference of two input dfas, using the product construction *)
let product_difference = product_construction SymmetricDifference

(* |disjoin_dfas| -- returns a tuple of disjoint DFAs, over the same alphabet *)
let disjoin_dfas m1 m2 =
    (* need to merge alphabets and disjoin our DFAs by renaming states in m2, by negative numbers *)
    let rec negate_state = function
        | State xs -> State (List.rev_map (fun x -> -x-1) xs)
        | ProductState (s1,s2) -> ProductState (negate_state s1, negate_state s2)
    in

    let merged_alphabet = Utils.list_union (get_alphabet m1) (get_alphabet m2) in

    let missingalph1 = List.filter (fun a -> not (List.mem a (get_alphabet m1))) merged_alphabet and
        missingalph2 = List.filter (fun a -> not (List.mem a (get_alphabet m2))) merged_alphabet in

    (* find a sink states, and add missing transitions *)
    let missingtran1 = ref [] and hassink1 = ref false in
    if List.length merged_alphabet > List.length (get_alphabet m1) then (
        let sink = 
            match List.find_opt (fun s -> not (is_accepting m1 s) && List.for_all (fun a -> succ m1 s a = s) (get_alphabet m1)) (get_states m1) with
                | Some t -> (hassink1 := true; t)
                | None -> State []
        in
        missingtran1 := List.concat_map (fun a -> List.map (fun s -> (s,a,sink)) (get_states m1)) missingalph1
    );
    let missingtran2 = ref [] and hassink2 = ref false in
    if List.length merged_alphabet > List.length (get_alphabet m2) then (
        let sink = 
            match List.find_opt (fun s -> not (is_accepting m2 s) && List.for_all (fun a -> succ m2 s a = s) (get_alphabet m2)) (get_states m2) with
                | Some t -> (hassink2 := true; t)
                | None -> State []
        in
        missingtran2 := List.concat_map (fun a -> List.map (fun s -> (s,a,sink)) (Utils.add_unique sink (get_states m2))) missingalph2
    );

    let newstate1 = if !hassink1 then (get_states m1) else State []::(get_states m1) and
        newtrans1 = get_transitions m1 @ !missingtran1 in
    let newstate2 = if !hassink2 then (get_states m2) else State []::(get_states m2) and
        newtrans2 = get_transitions m2 @ !missingtran2 in

    (
        AdtHash.create_automata newstate1 merged_alphabet (newtrans1) (get_start m1) (get_accepting m1),
        AdtHash.create_automata (List.rev_map (fun s -> negate_state s) newstate2) merged_alphabet (List.rev_map (fun (s,a,t) -> (negate_state s,a,negate_state t)) newtrans2) (negate_state (get_start m2)) (List.rev_map (fun s -> negate_state s) (get_accepting m2))
    )

(* |hopcroft_equiv| -- returns true iff DFAs are equivalent, by Hopcroft's algorithm *)
let hopcroft_equiv m1 m2 =
    let (m1', m2') = disjoin_dfas m1 m2 in
    let merged_states = ref (List.rev_map (fun s -> [s]) (get_states m1' @ get_states m2')) and
        stack = ref [] in

    merged_states := List.filter_map (fun s -> if List.mem (get_start m2') s then Some(get_start m1'::s) else if List.mem (get_start m1') s then None else Some(s)) !merged_states;
    stack := [(get_start m1', get_start m2')];
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
        ) (get_alphabet m1')
    done;

    List.for_all (fun ss ->
        List.for_all (fun s -> is_accepting m1' s || is_accepting m2' s) ss || 
        List.for_all (fun s -> not (is_accepting m1' s || is_accepting m2' s)) ss
    ) !merged_states

(* |symmetric_equiv| -- returns true iff DFAs are equivalent, by Symmetric Difference *)
let symmetric_equiv m1 m2 = is_empty (product_difference m1 m2)

(* |is_equiv| -- synonym for hopcroft_equiv *)
let is_equiv = hopcroft_equiv

(* helper func returns true iff state p is within some product state *)
let rec contains p ps = 
    if ps = p then true
    else 
        match ps with
            | State _ -> false
            | ProductState (s,s') -> contains p s || contains p s'

(* |myhill_min| -- returns minimised DFA by myhill nerode *)
let myhill_min m =
    let m' = prune m in

    let allpairs = 
        let rec find_pairs xss yss =
            match xss, yss with
                | ([],_) -> []
                | (_,[]) -> []
                | (x::xs,ys) -> List.rev_append (List.rev_map (fun y -> (x, y)) ys) (find_pairs xs (List.tl ys))
        in
        find_pairs (get_states m') (get_states m')
    in
    let marked = ref (List.filter (fun (p,q) ->
            let pa = is_accepting m' p and qa = is_accepting m' q in
            (pa && not qa) || (not pa && qa)
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
            ) (get_alphabet m'))
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

    let newtrans = List.fold_left (fun acc (s,a,t) -> Utils.add_unique (List.find (contains s) merged_states, a, List.find (contains t) merged_states) acc) [] (get_transitions m') and
        newaccepting = List.fold_left (fun acc s -> Utils.add_unique (List.find (contains s) merged_states) acc) [] (get_accepting m') and
        newstart = List.find (contains (get_start m')) merged_states in

    AdtHash.create_automata merged_states (get_alphabet m') newtrans newstart newaccepting

(* |brzozowski_min| -- minimise input DFA by Brzozowski's algorithm *)
let brzozowski_min m =
    let reverse_and_determinise d =
        let get_state s = Option.get (Utils.index s (get_states d)) in
        let newstart = List.sort compare (List.map get_state (get_accepting d)) in
        let newstates = ref [State newstart] and
            newtrans = ref [] and
            stack = ref [newstart] and
            donestates = ref [newstart] in
        
        while (List.length !stack > 0) do
            let currentstate = List.hd !stack in
            stack := List.tl !stack;
            List.iter (fun a ->
                let nextstate = ref [] in
                List.iter (fun t ->
                    let preds = pred d (List.nth (get_states d) t) a in
                    nextstate := Utils.list_union !nextstate (List.map get_state preds)
                ) currentstate;
                nextstate := List.sort compare !nextstate;

                if not (List.mem !nextstate !donestates) then (
                    stack := !nextstate::!stack;
                    donestates := !nextstate::!donestates;
                    newstates := Utils.add_unique (State !nextstate) !newstates;
                );

                newtrans := (State currentstate, a, State !nextstate)::!newtrans
            ) (get_alphabet d)
        done;

        let newaccepting = List.filter_map (function
                | State s -> if List.mem (get_state (get_start d)) s then Some(State s) else None
                | _ -> None
            ) !newstates
        in

        AdtHash.create_automata !newstates (get_alphabet d) !newtrans (State newstart) newaccepting
    in
    (* reverse DFA *)
    let drd = reverse_and_determinise m in
    (* reverse Drd *)
    reverse_and_determinise drd

(* |hopcroft_min| -- minimise input DFA by Hopcroft's algorithm *)
let hopcroft_min m =
    let m' = prune m in

    let p = ref [] in
    let qnotf = List.filter (fun s -> not (is_accepting m' s)) (get_states m') in
    if List.length qnotf > 0 then p := [qnotf];
    if List.length (get_accepting m') > 0 then p := (get_accepting m')::!p;

    let w = ref !p in

    while (List.length !w > 0) do
        let s = List.hd !w in
        w := List.tl !w;
        List.iter (fun a ->
            let l_a = List.fold_left (fun acc t -> Utils.list_union acc (pred m' t a)) [] s in
            let newp = ref [] in
            List.iter (fun r ->
                let r_1 = List.filter (fun s -> List.mem s l_a) r and
                    r_2 = List.filter (fun s -> not (List.mem s l_a)) r in
                if (List.length r_1 > 0 && List.length r_2 > 0) then (
                    newp := r_1::r_2::!newp;
                    if (List.mem r !w) then (
                        w := r_1::r_2::(List.filter ((<>) r) !w)
                    ) else (
                        if List.length r_1 <= List.length r_2 then (
                            w := r_1::!w
                        ) else w := r_2::!w
                    )
                ) else newp := r::!newp
            ) !p;
            p := !newp
        ) (get_alphabet m')
    done;

    let merged_states = List.fold_left (fun acc ss ->
        if List.length ss > 1 then 
        (List.fold_left (fun acc' s -> ProductState(acc',s)) (List.hd ss) (List.tl ss))::acc
        else (List.hd ss)::acc 
    ) [] !p in

    let newtrans = List.fold_left (fun acc (s,a,t) -> Utils.add_unique (List.find (contains s) merged_states, a, List.find (contains t) merged_states) acc) [] (get_transitions m') and
        newaccepting = List.fold_left (fun acc s -> Utils.add_unique (List.find (contains s) merged_states) acc) [] (get_accepting m') and
        newstart = List.find (contains (get_start m')) merged_states in

    AdtHash.create_automata merged_states (get_alphabet m') newtrans newstart newaccepting

(* |minimise| -- synonym for hopcroft_min *)
let minimise = hopcroft_min

(* |nfa_to_dfa| -- converts nfa to dfa by the subset construction *)
let nfa_to_dfa (n: NfaHash.nfa) =
    let newstart = NfaHash.eps_reachable_set n [get_start n] in
    let newtrans = ref [] and
        newstates = ref [State newstart] and
        stack = ref [newstart] and
        donestates = ref [newstart] in
    
    while (List.length !stack > 0) do
        let currentstate = List.hd !stack in
        stack := List.tl !stack;
        List.iter (fun a ->
            let nextstate = List.concat_map (fun s -> AdtHash.get_next_states n s a) currentstate in
            let epsnext = NfaHash.eps_reachable_set n nextstate in
            if (not (List.mem epsnext !donestates)) then (
                stack := epsnext::!stack;
                donestates := epsnext::!donestates;
                newstates := (State epsnext)::!newstates;
            );
            newtrans := (State currentstate, a, State epsnext)::!newtrans;
        ) (get_alphabet n)
    done;

    let newaccepting = List.filter (function
        | State s -> List.exists (NfaHash.is_accepting n) s
        | _ -> false
    ) !newstates in

    AdtHash.create_automata !newstates (get_alphabet n) !newtrans (State newstart) newaccepting

(* |re_to_dfa| -- Converts re to (almost) minimal dfa by Brzozowski derivatives *)
let re_to_dfa (r: Tree.re) =
    let r' = Re.simplify r in
    let alphabet = Re.get_alphabet r' in
    let w = ref [(r',State [0])] in
    let states = ref [(r', State [0])] in
    let trans = ref [] in
    let count = ref 0 in
    while (List.length !w > 0) do
        let (re,s) = List.hd !w in
        w := List.tl !w;
        List.iter (fun c ->
            let deriv = Re.simplify (Re.derivative re c) in
            let t = match List.assoc_opt deriv !states with
                | Some tt -> tt
                | None -> (incr count; w := (deriv, State [!count])::!w; states := (deriv, State [!count])::!states; State [!count])
            in
            trans := (s,c,t)::!trans;
        ) alphabet;
    done;

    let (_,newstates) = List.split !states in
    let accepting = List.filter_map (fun (re,s) -> if Re.is_nullable re then Some(s) else None) !states in
    AdtHash.create_automata newstates alphabet !trans (State [0]) accepting

(* |create| -- Creates DFA, Renames states as their index in qs *)
let create qs alph tran init fin =

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

    let newstates = List.init (List.length qs) (fun i -> State [i]) in
    let newinit = State [Option.get (Utils.index init qs)]
    and newtran = 
        List.rev_map (fun (s,a,t) ->
            (State [Option.get (Utils.index s qs)], a, State [Option.get (Utils.index t qs)])
        ) tran
    and newfin = 
        List.rev_map (fun s ->
            State [Option.get (Utils.index s qs)]
        ) fin
    in

    (* missing transitions for total transition function *)
    (* find a sink states, and add missing transitions *)
    let missingtran = ref [] and hassink1 = ref false and sink = ref (State []) in
    if List.length newtran < (List.length newstates * List.length alph) then (
        sink := (
            match List.find_opt (fun s -> not (List.mem s newfin) && List.for_all (fun (s',_,t') -> s' <> s || t' = s) newtran) newstates with
                | Some t -> (hassink1 := true; t)
                | None -> State [List.length qs]
        );
        missingtran := List.concat_map (fun a ->
            List.filter_map (fun s ->
                if not (List.exists (fun (s',a',_) -> s = s' && a = a') newtran)
                    then Some(s,a,!sink)
                else None
            ) (Utils.add_unique !sink newstates)
        ) alph;
    );
    let newstates = if List.length !missingtran > 0 then newstates @ [!sink] else newstates in
    let newtrans = if List.length !missingtran > 0 then !missingtran @ newtran else !missingtran @ newtran in

    AdtHash.create_automata newstates alph newtrans newinit newfin