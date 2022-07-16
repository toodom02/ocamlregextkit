open Nfa

module S = Set.Make(Int)
module SS = Set.Make(S)

type state = SS.elt
type dfa = {
  states: SS.t; alphabet: string list; transitions: (state * string * state) list; start: state; accepting: SS.t
}

(* |powerset| -- returns the powerset of input set *)
let powerset xs = 
  S.fold (fun x ps -> 
      SS.fold (fun ss -> SS.add (S.add x ss)) ps ps) xs 
         (SS.singleton S.empty)

(* |reachable_set| -- returns set of all epsilon reachable states from input set of states *)
let reachable_set ss trans =

    let get_reachable_set states =    
        List.fold_right S.add (List.filter_map (fun (s,a,t) -> if S.mem s states && a = "ε" then Some(t) else None) trans) states
    in

    (* iterate reachable set until no changes *)
    let sts = ref (get_reachable_set ss) in
        let newSts = ref (get_reachable_set !sts) in
        while (!sts <> !newSts) do
            sts := !newSts;
            newSts := get_reachable_set !sts
        done;
        !sts


(* |find_newtrans| -- returns a list of transitions for the dfa *)
let find_newtrans sss trans alphabet allstates = 
    let newtrans = ref [] in
    SS.iter (fun ss -> 
        List.iter (fun a ->
            let temptrans = ref S.empty in
            S.iter (fun t ->
                if not (S.is_empty ss) && S.exists (fun s -> List.mem (s,a,t) trans) ss then 
                    temptrans := S.add t !temptrans
            ) allstates;
            newtrans := (ss,a,!temptrans) :: !newtrans;
        ) alphabet
    ) sss;
    (* add states epsilon reachable from these *)
    List.map (fun (ss,a,tt) -> 
        let reachable = reachable_set tt trans in (ss, a, reachable) 
    ) !newtrans

(* |nfa_to_dfa| -- converts nfa to dfa by the subset construction *)
let nfa_to_dfa (n: nfa): dfa = 
    let allstates = List.fold_right S.add n.states S.empty in
    let newstates = powerset allstates in
    let newtrans = find_newtrans newstates n.transitions n.alphabet allstates and
        newaccepting = SS.filter (fun ss -> (S.exists (fun s -> List.mem s n.accepting) ss)) newstates in
        {
            states = newstates;
            alphabet = n.alphabet;
            transitions = newtrans;
            start = reachable_set (S.singleton n.start) n.transitions;
            accepting = newaccepting;
        }

(* |print_dfa| -- prints out nfa representation *)
let print_dfa n = 
    print_string "states: "; SS.iter (fun ss -> print_string "[ "; S.iter (fun s -> print_int s; print_char ' ') ss; print_char ']') n.states; print_newline ();
    print_string "alphabet: "; List.iter (fun a -> print_string a; print_char ' ') n.alphabet; print_newline ();
    print_string "start: "; print_string "[ "; S.iter (fun s -> print_int s; print_char ' ') n.start; print_char ']'; print_newline ();
    print_string "accepting: "; SS.iter (fun ss -> print_string "[ "; S.iter (fun s -> print_int s; print_char ' ') ss; print_char ']') n.accepting; print_newline ();
    print_string "transitions: "; print_newline(); List.iter (fun (ss,a,tt) -> print_string "    "; print_string "[ "; S.iter (fun s -> print_int s; print_char ' ') ss; print_char ']'; print_string ("\t--"^a^"-->\t"); print_string "[ "; S.iter (fun t -> print_int t; print_char ' ') tt; print_char ']'; print_newline ()) n.transitions;