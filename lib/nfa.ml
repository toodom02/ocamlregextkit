open Tree

type state = int
type nfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

(* |print| -- prints out nfa representation *)
let print n = 
    print_string "states: "; List.iter (fun s -> print_int s; print_char ' ') n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_int n.start; print_newline ();
    print_string "accepting: "; List.iter (fun s -> print_int s; print_char ' ') n.accepting; print_newline ();
    print_string "transitions: "; print_newline(); List.iter (fun (s,a,t) -> print_string "    "; print_int s; print_string ("\t--"^a^"-->\t"); print_int t; print_newline ()) n.transitions

(* |export_graphviz| -- exports the nfa in the DOT language for Graphviz *)
let export_graphviz d =
    Printf.sprintf "digraph G {\n n0 [label=\"\", shape=none, height=0, width=0, ]\n%s\nn0 -> %s;\n%s\n}"

    (List.fold_left (fun a s -> 
            let shape = if List.mem s d.accepting then "doublecircle" else "circle" in
            Printf.sprintf "%s\"%s\" [shape=%s, ];\n" a (string_of_int s) shape
        ) "" d.states)
        
    (string_of_int d.start)

    (List.fold_left (fun acc (s,a,t) ->
            Printf.sprintf "%s\"%s\" -> \"%s\" [label=\"%s\", ];\n" acc (string_of_int s) (string_of_int t) a
    ) "" d.transitions)

let counter = ref 0;;

(* |construct_rec_nfa| -- recursively builds NFA for given re *)
let rec construct_rec_nfa = function
      Literal a -> counter := !counter + 2;
                {states = [!counter - 2; !counter - 1]; alphabet = [a]; start = !counter - 2; accepting = [!counter - 1]; transitions = [(!counter - 2, a, !counter - 1)]}
    | Epsilon -> counter := !counter + 1;
                {states = [!counter - 1]; alphabet = []; start = !counter - 1; accepting = [!counter - 1]; transitions = []}
    | Empty -> counter := !counter + 1;
                {states = [!counter - 1]; alphabet = []; start = !counter - 1; accepting = []; transitions = []}
    | Union (r1, r2) -> let nfa1 = construct_rec_nfa r1 and nfa2 = construct_rec_nfa r2 in
                counter := !counter + 1;
                {states = (!counter - 1) :: (nfa1.states @ nfa2.states); alphabet = Utils.list_union nfa1.alphabet nfa2.alphabet; start = !counter - 1; accepting = nfa1.accepting @ nfa2.accepting; 
                transitions = ((!counter - 1, "ε", nfa1.start) :: nfa1.transitions) @ ((!counter - 1, "ε", nfa2.start) :: nfa2.transitions)}
    | Concat (r1, r2) -> let nfa1 = construct_rec_nfa r1 and nfa2 = construct_rec_nfa r2 in
                let newtrans = List.rev_map (fun s -> (s,"ε",nfa2.start)) nfa1.accepting in
                {states = nfa1.states @ nfa2.states; alphabet = Utils.list_union nfa1.alphabet nfa2.alphabet; start = nfa1.start; accepting = nfa2.accepting;
                transitions = nfa1.transitions @ newtrans @ nfa2.transitions}
    | Star r -> let nfa1 = construct_rec_nfa r in
                let newtrans = List.rev_map (fun s -> (s, "ε", nfa1.start)) nfa1.accepting in
                counter := !counter + 1;
                {states = (!counter - 1) :: nfa1.states; alphabet = nfa1.alphabet; start = !counter - 1; accepting = (!counter - 1) :: nfa1.accepting;
                transitions = (!counter - 1, "ε", nfa1.start) :: newtrans @ nfa1.transitions}

(* |re_to_nfa| -- converts input regex AST into nfa *)
let re_to_nfa re = 
    let n = construct_rec_nfa re in
    counter := 0;
    {
        states = List.sort compare n.states;
        alphabet = List.rev n.alphabet;
        start = n.start;
        accepting = n.accepting;
        transitions = n.transitions;
    }

(* |eps_reachable_set| -- returns set of all epsilon-reachable states from input set of states *)
let eps_reachable_set n ss =

    let get_reachable_set states =    
        List.fold_right Utils.add_unique (List.filter_map (fun (s,a,t) -> if List.mem s states && a = "ε" then Some(t) else None) n.transitions) states
    in

    (* iterate reachable set until no changes *)
    let sts = ref (get_reachable_set ss) in
    let newSts = ref (get_reachable_set !sts) in
    while (!sts <> !newSts) do
        sts := !newSts;
        newSts := get_reachable_set !sts
    done;
    List.sort compare !sts

(* |reachable_states| -- returns the set of reachable states in nfa n *)
let reachable_states n = Utils.reachable_states n.start n.transitions

(* |succ| -- the resulting states of nfa n after reading symbol *)
let succ n state symbol =
    let initial_reachable = eps_reachable_set n [state] in    
    let succs = List.fold_left (fun acc (s,a,t) -> 
        if a = symbol && List.mem s initial_reachable then Utils.add_unique t acc else acc
    ) [] n.transitions in
    eps_reachable_set n succs

(* |pred| -- returns the set of states preceeding state in nfa n *)
let pred n state =
    (* all states from which state is eps_reachable *)
    let epspreds = List.filter (fun s -> List.mem state (eps_reachable_set n [s])) n.states and
        preds = ref [] in
    List.iter (fun tt ->
        List.iter (fun (s,a,t) ->
            if (tt = t && a <> "ε") then preds := Utils.add_unique s !preds;
        ) n.transitions;
    ) epspreds;
    List.filter (fun s -> List.exists (fun ss -> List.mem ss !preds) (eps_reachable_set n [s])) n.states

(* |prune| -- reduces input nfa by pruning unreachable states *)
let prune n = 
    let marked = reachable_states n in
    {
        states = List.filter (fun s -> List.mem s marked) n.states;
        alphabet = n.alphabet;
        start = n.start;
        transitions = List.filter (fun (s,_,_) -> List.mem s marked) n.transitions;
        accepting = List.filter (fun s -> List.mem s marked) n.accepting
    }

(* |is_empty| -- returns true iff nfa has no reachable accepting states *)
let is_empty n =
    let marked = reachable_states n in
    not (List.exists (fun m -> List.mem m n.accepting) marked)

(* |accepts| -- returns true iff string s is accepted by the nfa n. Can take a long time *)
let accepts n s =
    let sts = ref (eps_reachable_set n [n.start]) in
    for i = 0 to String.length s - 1 do
        let c = (String.make 1 s.[i]) in
        sts := eps_reachable_set n (List.fold_left (fun a ss -> Utils.list_union (succ n ss c) a) [] !sts);
    done;
    List.exists (fun st -> List.mem st n.accepting) !sts

(* |accepted| -- returns the shortest word accepted by dfa m *)
let accepted n =
    let queue = ref (List.rev_map (fun s -> (s,"")) (eps_reachable_set n [n.start])) and
        seen = ref [] and
        shortest = ref None in
    while Option.is_none !shortest && List.length !queue > 0 do
        let (currentState, currentWord) = List.hd !queue in
        if List.mem currentState n.accepting then (shortest := Some(currentWord))
        else (
            seen := currentState::!seen;
            queue := (List.tl !queue) @ 
                List.filter_map (fun (s,a,t) -> 
                    if s = currentState && not (List.mem t !seen) then 
                        if a = "ε" then Some((t,currentWord)) 
                        else Some((t,currentWord^a)) 
                    else None
                ) n.transitions;
        )
    done;
    !shortest

(* |merge_alphabets| -- returns pair of nfas with a common alphabet *)
let merge_alphabets n1 n2 =
    let newalphabet = Utils.list_union n1.alphabet n2.alphabet in
    ({
        states = n1.states;
        alphabet = newalphabet;
        start = n1.start;
        accepting = n1.accepting;
        transitions = n1.transitions;
    },
    {
        states = n2.states;
        alphabet = newalphabet;
        start = n2.start;
        accepting = n2.accepting;
        transitions = n2.transitions;
    })

(* |create| -- Creates NFA, Renames states as their index in qs *)
let create qs alph tran init fin =

    (* Check parameters for correctness *)
    if not (List.mem init qs) then raise (Invalid_argument "NFA Initial State not in States");
    List.iter (fun f -> 
        if not (List.mem f qs) then raise (Invalid_argument "NFA Accepting State not in States")
    ) fin;
    List.iter (fun (s,a,t) ->
        if not ((a = "ε" || List.mem a alph) && List.mem s qs && List.mem t qs) then raise (Invalid_argument "NFA Transition not valid")
    ) tran;

    let newstates = List.init (List.length qs) Fun.id in
    let newinit = Option.get (Utils.index init qs)
    and newtran = 
        List.rev_map (fun (s,a,t) ->
            (Option.get (Utils.index s qs), a, Option.get (Utils.index t qs))
        ) tran
    and newfin = 
        List.rev_map (fun s ->
            Option.get (Utils.index s qs)
        ) fin
    in

    {
        states = newstates;
        alphabet = alph;
        start = newinit;
        accepting = newfin;
        transitions = newtran;
    }
  