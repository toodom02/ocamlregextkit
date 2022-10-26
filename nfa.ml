open Re

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

(* TODO [succ m s w] returns the state of NFA [m] after reading word [w] from state [s] *)
(* val succ : dfa -> state -> string -> state *)

(* TODO [pred m s] returns a list of states that preceed the state [s] in NFA [m] *)
(* val pred : dfa -> state -> state list *)

(* |merge_alphabets| -- returns nfas with the alphabet unioned with the other nfa *)
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
    }
    )

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

(* |print| -- prints out nfa representation *)
let print n = 
    print_string "states: "; List.iter (fun s -> print_int s; print_char ' ') n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_int n.start; print_newline ();
    print_string "accepting: "; List.iter (fun s -> print_int s; print_char ' ') n.accepting; print_newline ();
    print_string "transitions: "; print_newline(); List.iter (fun (s,a,t) -> print_string "    "; print_int s; print_string ("\t--"^a^"-->\t"); print_int t; print_newline ()) n.transitions;
  