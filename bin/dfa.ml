open Nfa

type state = int list
type dfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

type product_state = state * state
type product_dfa = {
    states: product_state list; alphabet: string list; transitions: (product_state * string * product_state) list; start: product_state; accepting: product_state list
}

type product_product_state = product_state * product_state
type product_product_dfa = {
    states: product_product_state list; alphabet: string list; transitions: (product_product_state * string * product_product_state) list; start: product_product_state; accepting: product_product_state list
}

(* |dfa_compliment| -- returns the compliment of input dfa *)
let dfa_compliment (m:dfa):dfa = 
    {
        states = m.states;
        alphabet = m.alphabet;
        transitions = m.transitions;
        start = m.start;
        accepting = List.filter (fun ss -> not (List.mem ss m.accepting)) m.states;  
        (*possible issues here with ordering of lists. Should be fine for now as we don't change the order of any states*)
        (* TODO: make sure states are always ordered? To ensure this is never a problem. Perhaps not worth the computation, though*)
    }

(* |list_union| -- returns union of the two input lists *)
let rec list_union l1 l2 = 
    match l2 with
          [] -> l1
        | x::xs -> if not (List.mem x l1) then list_union (x::l1) xs else list_union l1 xs

let find_resulting_state initialState symbol transitions = 
    let resultState = ref [] in
    List.iter (fun (s,a,t) ->
        if (s = initialState && a = symbol) then resultState := t
    ) transitions;
    !resultState

(* |find_product_trans| -- returns { ((l,r),a,(l',r')) : (l,a,r) ∨ (l',a,r') } ish*)
let find_product_trans cartStates fstTrans sndTrans alphabet = 
    let newTrans = ref [] in
    List.iter (fun (l,r) -> 
        List.iter (fun a -> 
            let lRes = find_resulting_state l a fstTrans and
                rRes = find_resulting_state r a sndTrans in
                    newTrans := ((l,r),a,(lRes,rRes))::!newTrans;
        ) alphabet
    ) cartStates;
    !newTrans

(* |product_intersection| -- returns the union of two input dfas, using the product construction (of a subset constructed dfa) *)
let product_intersection (m1:dfa) (m2:dfa) : product_dfa =
    let cartesianStates = List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1,e2)) m2.states) m1.states) in
    let unionAlphabet = list_union m1.alphabet m2.alphabet in
    let cartTrans = find_product_trans cartesianStates m1.transitions m2.transitions unionAlphabet and
        cartAccepting = List.filter (fun (l,r) -> List.mem l m1.accepting && List.mem r m2.accepting) cartesianStates in
    {
        states = cartesianStates;
        alphabet = unionAlphabet;
        transitions = cartTrans;
        start = (m1.start, m2.start);
        accepting = cartAccepting;
    }

let find_resulting_product_state initialState symbol transitions = 
    let resultState = ref ([],[]) in
    List.iter (fun (s,a,t) ->
        if (s = initialState && a = symbol) then resultState := t
    ) transitions;
    !resultState

(* |find_product_product_trans| -- returns { ((l,r),a,(l',r')) : (l,a,r) ∨ (l',a,r') } ish*)
let find_product_product_trans cartStates fstTrans sndTrans alphabet = 
    let newTrans = ref [] in
    List.iter (fun (l,r) -> 
        List.iter (fun a -> 
            let lRes = find_resulting_product_state l a fstTrans and
                rRes = find_resulting_product_state r a sndTrans in
                    newTrans := ((l,r),a,(lRes,rRes))::!newTrans;
        ) alphabet
    ) cartStates;
    !newTrans

(* |product_union| -- returns the union of two input dfas, using the product construction (of a product constructed, subset constructed dfa) *)
let product_union (m1:product_dfa) (m2:product_dfa) : product_product_dfa =
    let cartesianStates = List.concat (List.map (fun e1 -> List.map (fun e2 -> (e1,e2)) m2.states) m1.states) in
    let unionAlphabet = list_union m1.alphabet m2.alphabet in
    let cartTrans = find_product_product_trans cartesianStates m1.transitions m2.transitions unionAlphabet and
        cartAccepting = List.filter (fun (l,r) -> List.mem l m1.accepting || List.mem r m2.accepting) cartesianStates in
    {
        states = cartesianStates;
        alphabet = unionAlphabet;
        transitions = cartTrans;
        start = (m1.start, m2.start);
        accepting = cartAccepting;
    }

(* |powerset| -- returns the powerset of input list *)
let rec powerset xs =
    match xs with
        | [] -> [[]]
        | x :: xs -> let ps = powerset xs in
            ps @ List.map (fun ss -> x :: ss) ps

(* |add_unique| -- adds e to list l only if it is not already in l *)
let add_unique e l = 
    if List.mem e l then l else l @ [e]

(* |eps_reachable_set| -- returns set of all epsilon reachable states from input set of states *)
let eps_reachable_set ss trans =

    let get_reachable_set states =    
        List.fold_right add_unique (List.filter_map (fun (s,a,t) -> if List.mem s states && a = "ε" then Some(t) else None) trans) states
    in

    (* iterate reachable set until no changes *)
    let sts = ref (get_reachable_set ss) in
        let newSts = ref (get_reachable_set !sts) in
        while (!sts <> !newSts) do
            sts := !newSts;
            newSts := get_reachable_set !sts
        done;
        !sts

(* |find_dfa_trans| -- returns a list of transitions for the dfa *)
let find_dfa_trans newstates trans alphabet allstates = 
    let newtrans = ref [] in
    List.iter (fun ss -> 
        List.iter (fun a ->
            let temptrans = ref [] in
            List.iter (fun t ->
                if List.exists (fun s -> List.mem (s,a,t) trans) ss then (
                    temptrans := add_unique t !temptrans;
                );
            ) allstates;
            (* ignore transitions to sink state [] for simplicity *)
            if not (List.length !temptrans = 0) then
                newtrans := (ss,a,!temptrans) :: !newtrans;
        ) alphabet
    ) newstates;
    
    (* add states epsilon reachable from these *)
    List.map (fun (ss,a,tt) -> 
        let reachable = eps_reachable_set tt trans in (ss, a, reachable) 
    ) !newtrans

(* |nfa_to_dfa| -- converts nfa to dfa by the subset construction *)
let nfa_to_dfa (n: nfa): dfa = 
    let newstates = powerset n.states in
    let newtrans = find_dfa_trans newstates n.transitions n.alphabet n.states and
        newaccepting = List.filter (fun ss -> (List.exists (fun s -> List.mem s n.accepting) ss)) newstates in
        {
            states = newstates;
            alphabet = n.alphabet;
            transitions = newtrans;
            start = eps_reachable_set [n.start] n.transitions;
            accepting = newaccepting;
        }

(* the following functions are used to print out representations of states/dfas *)

let rec print_subset_state (n:state) =
    print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') n; print_string "]"

and print_product_state (n:product_state) =
    let (l,r) = n in
    print_string "( "; print_subset_state l; print_string " , "; print_subset_state r; print_string " )";

and print_product_product_state (n:product_product_state) = 
    let (l,r) = n in
    print_string "( "; print_product_state l; print_string " , "; print_product_state r; print_string " )";

(* |print_dfa| -- prints out dfa representation, formed by subset construction *)
and print_dfa (n:dfa) = 
    print_string "states: "; List.iter (fun ss -> print_subset_state ss) n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_subset_state n.start; print_newline ();
    print_string "accepting: "; List.iter (fun ss -> print_subset_state ss) n.accepting; print_newline ();
    print_string "transitions: "; print_newline (); List.iter (fun (ss,a,tt) -> print_string "    "; print_subset_state ss; print_string ("\t--"^a^"-->\t"); print_subset_state tt; print_newline ()) n.transitions;

(* |print_product_dfa| -- prints out product dfa representation *)
and print_product_dfa (n:product_dfa) = 
    print_string "states: "; List.iter (fun s -> print_product_state s) n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_product_state n.start; print_newline ();
    print_string "accepting: "; List.iter (fun s -> print_product_state s) n.accepting; print_newline ();
    print_string "transitions: "; print_newline (); List.iter (fun (l,a,r) -> print_string "    "; print_product_state l; print_string ("\t--"^a^"-->\t"); print_product_state r; print_newline ()) n.transitions;

(* |print_product_product_dfa| -- prints out dfa formed by product construction of two product constructed dfas *)
and print_product_product_dfa (n:product_product_dfa) =
    print_string "states: "; List.iter (fun s -> print_product_product_state s) n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_product_product_state n.start; print_newline ();
    print_string "accepting: "; List.iter (fun s -> print_product_product_state s) n.accepting; print_newline ();
    print_string "transitions: "; print_newline (); List.iter (fun (l,a,r) -> print_string "    "; print_product_product_state l; print_string ("\t--"^a^"-->\t"); print_product_product_state r; print_newline ()) n.transitions;