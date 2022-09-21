open Ast

type state = int
type nfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
}

let counter = ref 0;;
let alphabet: string list ref = ref [];;

(* |construct_rec_nfa| -- recursively builds NFA for given re *)
let rec construct_rec_nfa re = 
    match re with
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

(* |construct_nfa| -- initiates call to construct_rec_nfa *)
let construct_nfa re = 
    let n = construct_rec_nfa re in
    counter := 0;
    {
        states = List.sort compare n.states;
        alphabet = List.rev n.alphabet;
        start = n.start;
        accepting = n.accepting;
        transitions = n.transitions;
    }

(* |merge_alphabets| -- returns an nfa with the alphabet unioned with another nfa *)
let merge_alphabets n n' =
    {
        states = n.states;
        alphabet = Utils.list_union n.alphabet n'.alphabet;
        start = n.start;
        accepting = n.accepting;
        transitions = n.transitions;
    }

(* |eps_reachable_set| -- returns set of all epsilon-reachable states from input set of states *)
let eps_reachable_set ss n =

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