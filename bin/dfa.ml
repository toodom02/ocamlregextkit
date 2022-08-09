open Nfa

type state = (int list)
type dfa = {
    states: state list; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: state list
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

(* |print_dfa| -- prints out nfa representation *)
let print_dfa n = 
    print_string "states: "; List.iter (fun ss -> print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') ss; print_char ']') n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') n.start; print_char ']'; print_newline ();
    print_string "accepting: "; List.iter (fun ss -> print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') ss; print_char ']') n.accepting; print_newline ();
    print_string "transitions: "; print_newline(); List.iter (fun (ss,a,tt) -> print_string "    "; print_string "[ "; List.iter (fun s -> print_int s; print_char ' ') ss; print_char ']'; print_string ("\t--"^a^"-->\t"); print_string "[ "; List.iter (fun t -> print_int t; print_char ' ') tt; print_char ']'; print_newline ()) n.transitions;
