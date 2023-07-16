type 't automata = {
    states: 't array; alphabet: string array; transitions: int list array array; inverse_transitions: int list array array; start: int; accepting: bool array;
    states_list: 't list; alphabet_list: string list; transitions_list: ('t * string * 't) list; accepting_list: 't list;
}

let find_index a n =
  let rec index i = 
    if i = Array.length a then None
    else if a.(i) = n then Some(i)
    else index (i+1)
  in
  index 0

let get_states m = m.states_list

let get_alphabet m = m.alphabet_list

let get_transitions m = m.transitions_list

let get_start m = m.states.(m.start)

let get_accepting m = m.accepting_list

let get_next_states m s a = 
  let ai_opt = find_index m.alphabet a and
      si_opt = find_index m.states s in
  if Option.is_none ai_opt || Option.is_none si_opt then [] else
  List.map (fun i -> m.states.(i)) m.transitions.(Option.get si_opt).(Option.get ai_opt)

  let get_prev_states m t a = 
    let ai_opt = find_index m.alphabet a and
        ti_opt = find_index m.states t in
    if Option.is_none ai_opt || Option.is_none ti_opt then [] else
    List.map (fun i -> m.states.(i)) m.inverse_transitions.(Option.get ti_opt).(Option.get ai_opt)

let is_accepting m s = 
  match find_index m.states s with
    | Some si -> m.accepting.(si)
    | None -> false

let get_reachable_states m = 
  let rec find_reachable_states marked =
    let newmarked = List.fold_left (fun acc s -> Utils.list_union acc (List.concat (List.mapi (fun a _ -> m.transitions.(s).(a)) m.alphabet_list))) marked marked in
    if marked <> newmarked then find_reachable_states newmarked
    else List.map (fun s -> m.states.(s)) newmarked
  in
  find_reachable_states [m.start]

let create_automata qs alph tran init fin =
  let alph = List.sort compare alph in
  let containseps = List.exists (fun (_,a,_) -> a = "ε") tran in
  let alpheps = if containseps then "ε"::alph else alph in
  let states = Array.of_list qs in
  let alphabet = Array.of_list alpheps in
  let transitions = Array.make_matrix (Array.length states) (Array.length alphabet) [] and
      inverse_transitions = Array.make_matrix (Array.length states) (Array.length alphabet) [] in

  List.iter (fun (s,a,t) ->
    let si = Option.get (find_index states s) and
        ti = Option.get (find_index states t) and
        ai = Option.get (find_index alphabet a) in
    transitions.(si).(ai) <- ti::transitions.(si).(ai);
    inverse_transitions.(ti).(ai) <- si::inverse_transitions.(ti).(ai);
  ) tran;

  let start = Option.get (find_index states init) in
  let accepting = Array.init (Array.length states) (fun s -> List.mem states.(s) fin) in
  {
    states = states;
    alphabet = alphabet;
    transitions = transitions;
    inverse_transitions = inverse_transitions;
    start = start;
    accepting = accepting;
    states_list = qs;
    alphabet_list = alph;
    transitions_list = tran;
    accepting_list = fin;
  }