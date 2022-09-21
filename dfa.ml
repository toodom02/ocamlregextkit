type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

(* |dfa_compliment| -- returns the compliment of input dfa *)
let dfa_compliment m = 
    {
        states = m.states;
        alphabet = m.alphabet;
        transitions = m.transitions;
        start = m.start;
        accepting = List.filter (fun ss -> not (List.mem ss m.accepting)) m.states;  
    }

(* |find_resulting_state| -- finds the resulting state of dfa after reading a symbol *)
let rec find_resulting_state initialState symbol transitions = 
    let rec sinkState = function
        | State _ -> State []
        | ProductState (l,r) -> ProductState (sinkState l,sinkState r)
    in

    match transitions with
        | [] -> sinkState initialState
        | (s,a,t)::trans -> 
            if (s = initialState && a = symbol) then t
            else find_resulting_state initialState symbol trans

(* |find_product_trans| -- returns { ((l,r),a,(l',r')) : (l,a,l') ∧ (r,a,r') }*)
let find_product_trans cartStates fstTrans sndTrans alphabet = 
    let newTrans = ref [] in
    List.iter (fun state ->
        match state with
            | ProductState (l,r) -> 
                List.iter (fun a -> 
                    let lRes = find_resulting_state l a fstTrans and
                        rRes = find_resulting_state r a sndTrans in
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
    let cartTrans = find_product_trans cartesianStates m1.transitions m2.transitions unionAlphabet and
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
    let cartTrans = find_product_trans cartesianStates m1.transitions m2.transitions unionAlphabet and
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

(* |reduce_dfa| -- reduces input dfa by removing unreachable states *)
let reduce_dfa n = 
    let marked = Utils.reachable_states n.start n.transitions in
    {
        states = List.filter (fun s -> List.mem s marked) n.states;
        alphabet = n.alphabet;
        start = n.start;
        transitions = List.filter (fun (s,_,_) -> List.mem s marked) n.transitions;
        accepting = List.filter (fun s -> List.mem s marked) n.accepting
    }

(* |is_dfa_empty| -- returns None iff input dfa is empty, otherwise Some(reachable accepting states) *)
let is_dfa_empty n =
    let marked = Utils.reachable_states n.start n.transitions in
    match List.filter (fun m -> List.mem m n.accepting) marked with
        | [] -> None
        | xs -> Some xs

(* |reverse_search_word| -- finds a path in DFA n from the start state to acceptingState *)
let reverse_search_word n acceptingState = 
    let rec find_word_dfs currentState visited path = 
        if currentState = n.start then 
            Some(path)
        else 
            List.find_map (fun (s,a,t) ->
                if (t = currentState && not (List.mem s visited)) then (
                    find_word_dfs s (t::visited) (a^path)
                ) else None
            ) n.transitions
    in
    find_word_dfs acceptingState [acceptingState] ""

(* |find_unique_word| -- finds a word accepted by DFA m but not m' *)
let find_unique_word m m' =
    let comp' = dfa_compliment m' in
    let fst_and_not_snd = product_intersection m comp' in
    let reachable_accepting_states = is_dfa_empty fst_and_not_snd in
    if (Option.is_some (reachable_accepting_states)) then
        reverse_search_word fst_and_not_snd (List.hd (Option.get reachable_accepting_states))
    else None

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
                let reachable = Nfa.eps_reachable_set tt n in (State ss, a, State reachable) 
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
        start = State (Nfa.eps_reachable_set [n.start] n);
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
    let newstart = Nfa.eps_reachable_set [n.start] n in
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
            let epsnext = Nfa.eps_reachable_set !nextstate n in
            newstates := Utils.add_unique (State epsnext) !newstates;
            if (not (List.mem epsnext !donestates)) then (
                stack := epsnext::!stack;
                donestates := epsnext::!donestates;
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
