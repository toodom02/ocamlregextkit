type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

(* |print| -- prints out dfa representation *)
let print m = 
    let rec print_state = function
          State n -> print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') n; print_string "]"
        | ProductState (l,r) -> print_string "( "; print_state l; print_string " , "; print_state r; print_string " )"
    in
    print_string "states: "; List.iter (fun ss -> print_state ss) m.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') m.alphabet; print_newline ();
    print_string "start: "; print_state m.start; print_newline ();
    print_string "accepting: "; List.iter (fun ss -> print_state ss) m.accepting; print_newline ();
    print_string "transitions: "; print_newline (); List.iter (fun (ss,a,tt) -> print_string "    "; print_state ss; print_string ("\t--"^a^"-->\t"); print_state tt; print_newline ()) m.transitions

(* |compliment| -- returns the compliment of input dfa *)
let compliment m = 
    {
        states = m.states;
        alphabet = m.alphabet;
        transitions = m.transitions;
        start = m.start;
        accepting = List.filter (fun ss -> not (List.mem ss m.accepting)) m.states;  
    }

(* |succ| -- the resulting state of dfa m after reading symbol *)
let succ m state symbol = 
    let rec sinkState = function
        | State _ -> State []
        | ProductState (l,r) -> ProductState (sinkState l,sinkState r)
    in

    let postState = 
        List.find_map (fun (s,a,t) -> 
            if s = state && a = symbol then Some(t) else None
        ) m.transitions in
    if Option.is_none postState then sinkState m.start else Option.get postState

(* |pred| -- returns the state preceeding state in dfa m before reading symbol *)
let pred m state symbol = 
    List.filter_map (fun (s,a,t) ->
        if (t = state && a = symbol) then Some(s) else None
    ) m.transitions

(* |prune| -- reduces input dfa by pruning unreachable states *)
let prune m = 
    let marked = Utils.reachable_states m.start m.transitions in
    {
        states = List.filter (fun s -> List.mem s marked) m.states;
        alphabet = m.alphabet;
        start = m.start;
        transitions = List.filter (fun (s,_,_) -> List.mem s marked) m.transitions;
        accepting = List.filter (fun s -> List.mem s marked) m.accepting
    }

(* |is_empty| -- returns true iff dfa has no reachable accepting states *)
let is_empty m =
    let marked = Utils.reachable_states m.start m.transitions in
    not (List.exists (fun s -> List.mem s m.accepting) marked)

(* |accepts| -- returns true iff string s is accepted by the dfa m *)
let accepts m s =
    let rec does_accept state str =
        match str with
            | "" -> List.mem state m.accepting
            | _ -> does_accept (succ m state (String.make 1 str.[0])) (String.sub str 1 ((String.length str) - 1))
    in
    does_accept m.start s

(* |accepted| -- returns the shortest word accepted by dfa m *)
let accepted m =
    let queue = ref [(m.start, "")] and
        seen = ref [] and
        shortest = ref None in
    while Option.is_none !shortest && List.length !queue > 0 do
        let (currentState, currentWord) = List.hd !queue in
        if List.mem currentState m.accepting then (shortest := Some(currentWord))
        else (
            seen := currentState::!seen;
            queue := (List.tl !queue) @ 
                List.filter_map (fun (s,a,t) -> 
                    if s = currentState && not (List.mem t !seen) then Some((t,currentWord^a)) else None
                ) m.transitions;
        )
    done;
    !shortest

(* |find_product_trans| -- returns { ((l,r),a,(l',r')) : (l,a,l') ∧ (r,a,r') }*)
let find_product_trans m1 m2 cartStates alphabet = 
    let newTrans = ref [] in
    List.iter (fun state ->
        match state with
            | ProductState (l,r) -> 
                List.iter (fun a -> 
                    let lRes = succ m1 l a and
                        rRes = succ m2 r a in
                            newTrans := (ProductState (l,r), a, ProductState (lRes,rRes))::!newTrans;
                ) alphabet
            | State _ -> ()
    ) cartStates;
    !newTrans

let cross_product a b =
    List.concat (List.rev_map (fun e1 -> List.rev_map (fun e2 -> ProductState (e1,e2)) b) a)

(* |product_intersection| -- returns the intersection of two input dfas, using the product construction *)
(* exponential blowup here! *)
let product_intersection m1 m2 =
    let cartesianStates = cross_product m1.states m2.states in
    let unionAlphabet = Utils.list_union m1.alphabet m2.alphabet in
    let cartTrans = find_product_trans m1 m2 cartesianStates unionAlphabet and
        cartAccepting = List.filter (fun state -> 
            match state with
                | ProductState (l,r) -> List.mem l m1.accepting && List.mem r m2.accepting
                | State _ -> false
        ) cartesianStates in
    {
        states = cartesianStates;
        alphabet = unionAlphabet;
        transitions = cartTrans;
        start = ProductState (m1.start, m2.start);
        accepting = cartAccepting;
    }

(* |product_union| -- returns the union of two input dfas, using the product construction *)
(* exponential blowup here! *)
let product_union m1 m2 =
    let cartesianStates = cross_product m1.states m2.states in
    let unionAlphabet = Utils.list_union m1.alphabet m2.alphabet in
    let cartTrans = find_product_trans m1 m2 cartesianStates unionAlphabet and
        cartAccepting = List.filter (fun state -> 
            match state with
                | ProductState (l,r) -> List.mem l m1.accepting || List.mem r m2.accepting
                | State _ -> false
        ) cartesianStates in
    {
        states = cartesianStates;
        alphabet = unionAlphabet;
        transitions = cartTrans;
        start = ProductState (m1.start, m2.start);
        accepting = cartAccepting;
    }

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

(* |closure_equiv| -- returns true iff input DFAs are equivalent, by equivalence closure *)
let closure_equiv m1 m2 =
    let (m1', m2') = disjoin_dfas m1 m2 in
    let merged_states = m1'.states @ m2'.states in

    (* Find Reflex, Symmetric, & Transitive closure (Warshall's algo) *)
    let equiv_closure qs =
        if List.length qs = 0 then [] else
        let symclosure = List.fold_left (fun a (q1,q2) -> (q2,q1)::a) qs qs in
        let reflexclosure = List.fold_left (fun a s -> (s,s)::a) symclosure merged_states in

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
        tranclosure
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

    let e = List.concat (List.rev_map (fun e1 -> List.filter_map (fun e2 -> 
        if (List.mem e1 m1'.accepting && List.mem e2 m2'.accepting) then Some(e1,e2)
        else if (List.mem e1 m1'.accepting || List.mem e2 m2'.accepting) then None
        else Some(e1,e2)
        ) m2'.states) m1'.states) in
    
    let flag = ref false and
        q = ref [] and
        w = ref [(m1'.start, m2'.start)] in
    while (List.length !w > 0 && not !flag) do
        let uv = List.hd !w in
        w := List.tl !w;

        if not (List.mem uv e) then (
            flag := true;
        ) else if not (List.mem uv (equiv_closure !q)) then (
            q := uv::!q;
            w := (delta uv) @ !w
        )
    done;
    not !flag

(* |hopcroft_equiv| -- returns true iff DFAs are equivalent, by Hopcroft's algorithm *)
let hopcroft_equiv m1 m2 =
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
            let r1 = List.find (fun s -> List.mem succ1 s) !merged_states and
                r2 = List.find (fun s -> List.mem succ2 s) !merged_states in
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
    ) !merged_states

let symmetric_equiv m1 m2 =
    let comp1 = compliment m1 and
        comp2 = compliment m2 in
    let m1notm2 = product_intersection m1 comp2 and
        m2notm1 = product_intersection comp1 m2 in
    let emp = product_union m1notm2 m2notm1 in
        is_empty emp

(* |is_equiv| -- synonym for hopcroft_equiv *)
let is_equiv = hopcroft_equiv

(* |myhill_min| -- returns minimised DFA by myhill nerode *)
let myhill_min m =
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
            if (List.exists (fun a -> List.mem (succ m p a, succ m q a) !marked || List.mem (succ m q a, succ m p a) !marked) m'.alphabet)
            then (marked := (p,q)::!marked; stop := false) else newunmarked := (p,q)::!newunmarked;
        ) !unmarked;
        unmarked := !newunmarked
    done;

    (* unmarked gives us all pairs of indistinguishable states *)
    (* merge these states! *)

    let rec contains ps p = 
        if ps = p then true
        else 
            match ps with
                  State _ -> false
                | ProductState (s,s') -> contains s p || contains s' p
    in

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

    let newtrans = List.fold_left (fun acc (s,a,t) -> Utils.add_unique (List.find (fun s' -> contains s' s) merged_states, a, List.find (fun t' -> contains t' t) merged_states) acc) [] m'.transitions and
        newaccepting = List.fold_left (fun acc s -> Utils.add_unique (List.find (fun s' -> contains s' s) merged_states) acc) [] m'.accepting and
        newstart = List.find (fun s -> contains s m'.start) merged_states in

    {
        states = merged_states;
        alphabet = m'.alphabet;
        transitions = newtrans;
        start = newstart;
        accepting = newaccepting;
    }

(* |brzozowski_min| -- minimise input DFA by Brzozowski's algorithm *)
let brzozowski_min m =
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

        let newaccepting = List.filter_map (fun state -> 
            match state with
                  State s -> if List.mem (get_state d.start) s then Some(State s) else None
                | ProductState (_,_) -> None
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
    reverse_and_determinise drd

let hopcroft_min m =
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
                        w := xinty::ynotx::(List.filter (fun s -> s <> y) !w)
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

    let newstates = List.init (List.length !p) (fun s -> State [s]) in
    let newstart = List.find (fun state ->
            match state with
                  State [ss] ->  List.exists (fun s -> s = m'.start) (List.nth !p ss)
                | _ -> false
        ) newstates and
        newaccepting = List.filter (fun state -> 
            match state with
                  State [ss] -> List.exists (fun s -> List.mem s m'.accepting) (List.nth !p ss)
                | _ -> false
        ) newstates and
        newtrans = List.fold_left (fun acc (s,a,t) ->
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

    {
        states = newstates;
        alphabet = m'.alphabet;
        transitions = newtrans;
        start = newstart;
        accepting = newaccepting;
    }

(* |powerset| -- returns the powerset of input list *)
(* produces list of size 2^|s| *)
let powerset s =
    let prepend l x = List.fold_left (fun a b -> (x::b)::a) [] l in 
    let fold_func a b = List.rev_append (prepend a b) a in 
        List.fold_left fold_func [[]] (List.rev s)

(* |find_dfa_trans| -- returns a list of transitions for the dfa *)
(* this is where all the time is spent... *)
let find_dfa_trans newstates (n: Nfa.nfa) = 
    let newtrans = ref [] in
    List.iter (fun state ->
        match state with
            | State ss -> 
                    List.iter (fun a ->
                        let temptrans = ref [] in
                        List.iter (fun t ->
                            if List.exists (fun s -> List.mem (s,a,t) n.transitions) ss then (
                                temptrans := Utils.add_unique t !temptrans;
                            );
                        ) n.states;
                        newtrans := (State ss, a, State !temptrans) :: !newtrans;
                    ) n.alphabet
            | ProductState _ -> ()
    ) newstates;
    
    (* add states epsilon reachable from these *)
    List.rev_map (fun state -> 
        match state with 
            | (State ss, a, State tt) -> 
                let reachable = Nfa.eps_reachable_set n tt in (State ss, a, State reachable) 
            | (ss,a,tt) -> (ss,a,tt) (* This function should never be called for anything other than (State ss, a, State tt) *)
    ) !newtrans

(* |nfa_to_dfa| -- converts nfa to dfa by the subset construction *)
(* exponential blowup here! *)
let nfa_to_dfa_subset (n: Nfa.nfa) = 
    let newstates = List.rev_map (fun s -> State s) (powerset n.states) in
    let newtrans = find_dfa_trans newstates n and
        newaccepting = List.filter (fun state -> 
            match state with
                | State ss -> (List.exists (fun s -> List.mem s n.accepting) ss)
                | _ -> false
        ) newstates in
    {
        states = newstates;
        alphabet = n.alphabet;
        transitions = newtrans;
        start = State (Nfa.eps_reachable_set n [n.start]);
        accepting = newaccepting;
    }

(* 

An improved algorithm for nfa-dfa conversion:

start = eps_closure(n.start)
transitions = []
stack = [start]
seenstates = [start]

while stack not empty:
    currentstate = stack.pop
    for a in n.alphabet:
        nextstate = []
        for s in currentstate:
            for (s',a',t) in n.transitions:
                if s' = s && a' = a then 
                    nextstate = t::nextstate
        
        if eps_closure(nextstate) not in seenstates:
            stack.add(eps_closure(nextstate))
            seenstates = eps_closure(nextstate)::seenstates
        transitions = (currentstate, a, eps_closure(nextstate))::transitions

*)

let nfa_to_dfa (n: Nfa.nfa) =
    let newstart = Nfa.eps_reachable_set n [n.start] in
    let newtrans = ref [] and
        newstates = ref [State newstart] and
        stack = ref [newstart] and
        donestates = ref [newstart] in
    
    while (List.length !stack > 0) do
        let currentstate = List.hd !stack in
        stack := List.tl !stack;
        List.iter (fun a ->
            let nextstate = ref [] in
            List.iter (fun s ->
                List.iter (fun (s',a',t) ->
                    if (s' = s && a' = a) then nextstate := t::!nextstate
                ) n.transitions
            ) currentstate;
            let epsnext = Nfa.eps_reachable_set n !nextstate in
            if (not (List.mem epsnext !donestates)) then (
                stack := epsnext::!stack;
                donestates := epsnext::!donestates;
                newstates := (State epsnext)::!newstates;
            );
            newtrans := (State currentstate, a, State epsnext)::!newtrans;
        ) n.alphabet
    done;

    let newaccepting = List.filter (fun state ->
        match state with
              State s -> List.exists (fun s' -> List.mem s' n.accepting) s
            | _ -> false
    ) !newstates in
        
    {
        states = !newstates;
        alphabet = n.alphabet;
        transitions = !newtrans;
        start = State newstart;
        accepting = newaccepting;
    }

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

    let newstates = List.init (List.length qs + 1) (fun i -> State [i]) in
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
    let missingtran = 
        let missing = ref [] and
            sink = State [List.length qs] in
        List.iter (fun a ->
            List.iter (fun q ->
                if not (List.exists (fun (s,a',t) -> s = q && a = a') newtran) then missing := (q,a,sink)::!missing
            ) newstates;
        ) alph;
        !missing
    in

    {
        states = newstates;
        alphabet = alph;
        start = newinit;
        accepting = newfin;
        transitions = Utils.list_union missingtran newtran;
    }