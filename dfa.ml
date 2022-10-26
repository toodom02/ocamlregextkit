type state = State of int list | ProductState of state * state
type dfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

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

(* |pred| -- returns the set of states preceeding state in dfa m *)
let pred m state = 
    let pred = ref [] in
    List.iter (fun (s,a,t) ->
        if t = state then pred := Utils.add_unique s !pred
    ) m.transitions;
    !pred

(* |prune| -- reduces input dfa by pruning unreachable states *)
let prune n = 
    let marked = Utils.reachable_states n.start n.transitions in
    {
        states = List.filter (fun s -> List.mem s marked) n.states;
        alphabet = n.alphabet;
        start = n.start;
        transitions = List.filter (fun (s,_,_) -> List.mem s marked) n.transitions;
        accepting = List.filter (fun s -> List.mem s marked) n.accepting
    }

(* |is_empty| -- returns None iff input dfa is empty, otherwise Some(reachable accepting states) *)
let is_empty n =
    let marked = Utils.reachable_states n.start n.transitions in
    List.exists (fun m -> List.mem m n.accepting) marked

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

(* |print| -- prints out dfa representation *)
let print n = 
let rec print_state = function
    | State n -> print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') n; print_string "]"
    | ProductState (l,r) -> print_string "( "; print_state l; print_string " , "; print_state r; print_string " )";
in
print_string "states: "; List.iter (fun ss -> print_state ss) n.states; print_newline ();
print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
print_string "start: "; print_state n.start; print_newline ();
print_string "accepting: "; List.iter (fun ss -> print_state ss) n.accepting; print_newline ();
print_string "transitions: "; print_newline (); List.iter (fun (ss,a,tt) -> print_string "    "; print_state ss; print_string ("\t--"^a^"-->\t"); print_state tt; print_newline ()) n.transitions;