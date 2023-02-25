open Tree

type state = int
type nfa = {
    states: state array; alphabet: string array; transitions: int list array array; start: int; accepting: bool array
}

(* |is_accepting| -- returns true if state s is accepting *)
let is_accepting n s = n.accepting.(s)

(* |print| -- prints out nfa representation *)
let print n = 
    print_string "states: "; Array.iter (fun s -> print_int s; print_char ' ') n.states; print_newline ();
    print_string "alphabet: "; Array.iter (fun a -> if a <> "ε" then (print_string a; print_char ' ')) n.alphabet; print_newline ();
    print_string "start: "; print_int n.states.(n.start); print_newline ();
    print_string "accepting: "; Array.iteri (fun i b -> if (b) then (print_int n.states.(i); print_char ' ')) n.accepting; print_newline ();
    print_string "transitions: "; print_newline(); 
        Array.iteri (fun s arr -> Array.iteri (fun a ts -> List.iter (fun t -> (print_string "    "; print_int (n.states.(s)); print_string ("\t--"^n.alphabet.(a)^"-->\t"); print_int (n.states.(t)); print_newline())) ts) arr) n.transitions

(* |eps_reachable_set| -- returns list of all epsilon-reachable states from input list of states *)
let eps_reachable_set n ss =
    match Utils.array_index "ε" n.alphabet with
        | None -> List.sort compare ss
        | Some epsi ->
            let rec get_reachable_set states =
                let news = List.fold_right Utils.add_unique (List.concat_map (fun s -> n.transitions.(s).(epsi)) states) states in
                if states <> news then get_reachable_set news
                else news
            in
            List.sort compare (get_reachable_set ss)

(* |reachable_states| -- returns the set of reachable states in nfa n *)
let reachable_states n = 
    let rec find_reachable_states marked =
        let newmarked = List.fold_right Utils.add_unique (List.concat_map (fun s -> List.concat_map (fun i -> n.transitions.(s).(i)) (List.init (Array.length n.alphabet) Fun.id)) marked) marked in
        if marked <> newmarked then find_reachable_states newmarked
        else List.sort compare newmarked
    in
    find_reachable_states [n.start]

(* |succ| -- the resulting states of nfa n after reading symbol *)
let succ n statei symbol =
    match Utils.array_index symbol n.alphabet with
        | None -> []
        | Some symboli ->
            let initial_reachable = eps_reachable_set n [statei] in    
            let succs = List.fold_right Utils.add_unique (List.concat_map (fun s -> n.transitions.(s).(symboli)) initial_reachable) [] in
            eps_reachable_set n succs

(* |is_empty| -- returns true iff nfa has no reachable accepting states *)
let is_empty n =
    let marked = reachable_states n in
    not (List.exists (is_accepting n) marked)

(* |merge_alphabets| -- returns pair of nfas with a common alphabet *)
let merge_alphabets n1 n2 =
    let newalphabet = Utils.array_union n1.alphabet n2.alphabet in
    let transitions1 = Array.init (Array.length n1.states) (fun s -> Array.init (Array.length newalphabet) (fun a -> if a >= (Array.length n1.alphabet) then [] else n1.transitions.(s).(a))) in
    let transitions2 = Array.init (Array.length n2.states) (fun s -> Array.init (Array.length newalphabet) (fun a -> if Array.mem newalphabet.(a) n2.alphabet then n2.transitions.(s).(Option.get (Utils.array_index newalphabet.(a) n2.alphabet)) else [])) in
    ({
        states = n1.states;
        alphabet = newalphabet;
        start = n1.start;
        accepting = n1.accepting;
        transitions = transitions1;
    },
    {
        states = n2.states;
        alphabet = newalphabet;
        start = n2.start;
        accepting = n2.accepting;
        transitions = transitions2;
    })

(* |create| -- Creates NFA, Renames states as their index in qs *)
let create qs alph tran init fin =
    let alph = List.fold_right (Utils.add_unique) (List.sort compare ("ε"::alph)) [] in
    (* Check parameters for correctness *)
    if not (List.mem init qs) then raise (Invalid_argument "NFA Initial State not in States");
    List.iter (fun f -> 
        if not (List.mem f qs) then raise (Invalid_argument "NFA Accepting State not in States")
    ) fin;
    List.iter (fun (s,a,t) ->
        if not ((a = "ε" || List.mem a alph) && List.mem s qs && List.mem t qs) then raise (Invalid_argument "NFA Transition not valid")
    ) tran;

    let newstates = Array.init (List.length qs) Fun.id in
    let newinit = Option.get (Utils.index init qs) in
    let newalphabet = Array.init (List.length alph) (List.nth alph) in
    let newtran = Array.make_matrix (List.length qs) (List.length alph) ([]) in
    List.iter (fun (s,a,t) ->
        newtran.(Option.get (Utils.index s qs)).(Option.get (Utils.index a alph)) <- ((Option.get (Utils.index t qs))::newtran.(Option.get (Utils.index s qs)).(Option.get (Utils.index a alph)))
    ) tran;
    let finList = List.rev_map (fun s -> Option.get (Utils.index s qs)) fin in
    let newfin = Array.init (List.length qs) (fun s -> if (List.mem s finList) then true else false) in
    {
        states = newstates;
        alphabet = newalphabet;
        transitions = newtran;
        start = newinit;
        accepting = newfin;
    }
  
let counter = ref 0

(* |construct_rec_nfa| -- recursively builds NFA for given re *)
let rec construct_rec_nfa = function
    | Literal a -> counter := !counter + 2;
            ([!counter - 2; !counter - 1], [a], !counter - 2, [!counter - 1], [(!counter - 2, a, !counter - 1)])
    | Epsilon -> counter := !counter + 1;
            ([!counter - 1], [], !counter - 1, [!counter - 1], [])
    | Empty -> counter := !counter + 1;
            ([!counter - 1], [], !counter - 1, [], [])
    | Union (r1, r2) -> let (states1, alph1, start1, acc1, trans1) = construct_rec_nfa r1 and (states2, alph2, start2, acc2, trans2) = construct_rec_nfa r2 in
            counter := !counter + 1;
            ((!counter - 1) :: (states1 @ states2), Utils.list_union alph1 alph2, !counter - 1, acc1 @ acc2,
            ((!counter - 1, "ε", start1) :: trans1) @ ((!counter - 1, "ε", start2) :: trans2))        
    | Concat (r1, r2) -> let (states1, alph1, start1, acc1, trans1) = construct_rec_nfa r1 and (states2, alph2, start2, acc2, trans2) = construct_rec_nfa r2 in
            let newtrans = List.rev_map (fun s -> (s,"ε",start2)) acc1 in
            (states1 @ states2, Utils.list_union alph1 alph2, start1, acc2, trans1 @ newtrans @ trans2)
    | Star r -> let (states1, alph1, start1, acc1, trans1) = construct_rec_nfa r in
            let newtrans = List.rev_map (fun s -> (s, "ε", start1)) acc1 in
            counter := !counter + 1;
            ((!counter - 1) :: states1, alph1, !counter - 1, (!counter - 1) :: acc1,
            (!counter - 1, "ε", start1) :: newtrans @ trans1)

(* |re_to_nfa| -- converts input regex AST into nfa *)
let re_to_nfa re = 
    counter := 0;
    let (states, alphabet, start, accepting, transitions) = construct_rec_nfa re in
    create states alphabet transitions start accepting