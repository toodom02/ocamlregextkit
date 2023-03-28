type 't automata = {
    states: 't list; alphabet: string list; transitions: ('t * string * 't) list; start: 't; accepting: 't list
}

let get_states m = m.states

let get_alphabet m = m.alphabet

let get_transitions m = m.transitions

let get_start m = m.start

let get_accepting m = m.accepting

let get_next_states m s a = 
  List.filter_map (fun (s',a',t) -> if s=s' && a=a' then Some(t) else None) m.transitions

let get_prev_states m t a =
  List.filter_map (fun (s,a',t') ->
    if (t = t' && a = a') then Some(s) else None
  ) m.transitions

let is_accepting m s = List.mem s m.accepting

let create_automata qs alph tran init fin =
  {
    states = qs;
    alphabet = alph;
    transitions = tran;
    start = init;
    accepting = fin;
  }