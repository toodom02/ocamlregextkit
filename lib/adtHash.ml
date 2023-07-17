type 't automata = {
    states: 't list; 
    alphabet: string list; 
    transitions: ('t, (string, 't) Hashtbl.t) Hashtbl.t;
    transitions_list: ('t * string * 't) list;
    inverse_transitions: ('t, (string, 't) Hashtbl.t) Hashtbl.t;
    start: 't; 
    accepting: ('t, bool) Hashtbl.t;
    accepting_list: 't list;
}

let get_states m = m.states

let get_alphabet m = m.alphabet

let get_transitions m = m.transitions_list

let get_start m = m.start

let get_accepting m = m.accepting_list

let get_next_states m s a = Hashtbl.find_all (Hashtbl.find m.transitions s) a

let get_prev_states m t a = Hashtbl.find_all (Hashtbl.find m.inverse_transitions t) a

let is_accepting m s = Option.is_some (Hashtbl.find_opt m.accepting s)

let get_reachable_states m = 
    let rec find_reachable_states marked =
        let newmarked = List.fold_left (fun acc s -> List.fold_left (fun acc2 a -> Utils.list_union acc2 (get_next_states m s a)) acc m.alphabet) marked marked in
        if marked <> newmarked then find_reachable_states newmarked
        else newmarked
    in
  find_reachable_states [m.start]

let create_automata qs alph tran init fin =
    let length = List.length qs in
    let transitions = Hashtbl.create length in
    let inverse_transitions = Hashtbl.create length in
    List.iter (fun s -> 
        let tbl = Hashtbl.create (List.length alph) in
        let tbl2 = Hashtbl.create (List.length alph) in
        Hashtbl.add transitions s tbl;
        Hashtbl.add inverse_transitions s tbl2
    ) qs;
    List.iter (fun (s,a,t) ->
        Hashtbl.add (Hashtbl.find transitions s) a t;
        Hashtbl.add (Hashtbl.find inverse_transitions t) a s;
    ) tran;
    let accepting = Hashtbl.create length in
    List.iter (fun s ->
        Hashtbl.add accepting s true;    
    ) fin;
    {
        states = qs;
        alphabet = List.sort compare alph;
        transitions = transitions;
        transitions_list = tran;
        inverse_transitions = inverse_transitions;
        start = init;
        accepting = accepting;
        accepting_list = fin;
    }