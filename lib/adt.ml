type 't automata = {
  mutable states : 't list;
  mutable alphabet : string list;
  transitions : ('t, (string, 't) Hashtbl.t) Hashtbl.t;
  start : 't;
  accepting : ('t, bool) Hashtbl.t;
}

let get_states m = m.states
let set_states m qs = m.states <- qs
let get_alphabet m = m.alphabet
let set_alphabet m alph = m.alphabet <- alph

let get_transitions m =
  Hashtbl.fold
    (fun s ats acc -> Hashtbl.fold (fun a t acc' -> (s, a, t) :: acc') ats acc)
    m.transitions []

let get_start m = m.start

let get_accepting m =
  Hashtbl.fold (fun k v acc -> if v then k :: acc else acc) m.accepting []

let get_next_states m s a = Hashtbl.find_all (Hashtbl.find m.transitions s) a

let get_prev_states m t a =
  Hashtbl.fold
    (fun s v acc ->
      Hashtbl.fold
        (fun a' t' acc' -> if a = a' && t = t' then s :: acc' else acc')
        v acc)
    m.transitions []

let is_accepting m s =
  match Hashtbl.find_opt m.accepting s with None -> false | Some res -> res

let get_reachable_states m =
  let rec find_reachable_states marked =
    let newmarked =
      List.fold_left
        (fun acc s ->
          List.fold_left
            (fun acc2 a -> Utils.list_union acc2 (get_next_states m s a))
            acc m.alphabet)
        marked marked
    in
    if marked <> newmarked then find_reachable_states newmarked else newmarked
  in
  find_reachable_states [ m.start ]

let filter_states m f =
  set_states m (List.filter f m.states);
  Hashtbl.filter_map_inplace
    (fun s ts -> if f s then Some ts else None)
    m.transitions;
  Hashtbl.filter_map_inplace
    (fun s b -> if f s then Some b else None)
    m.accepting

let add_to_alphabet m alph =
  set_alphabet m (List.sort compare (Utils.list_union m.alphabet alph))

let map_accepting f m =
  Hashtbl.filter_map_inplace (fun k _ -> Some (f k)) m.accepting

let copy m =
  {
    states = m.states;
    alphabet = m.alphabet;
    transitions = Hashtbl.copy m.transitions;
    start = m.start;
    accepting = Hashtbl.copy m.accepting;
  }

let create_automata qs alph tran init fin =
  let length = List.length qs in
  let transitions = Hashtbl.create length in
  List.iter
    (fun s ->
      let tbl = Hashtbl.create (List.length alph) in
      Hashtbl.add transitions s tbl)
    qs;
  List.iter (fun (s, a, t) -> Hashtbl.add (Hashtbl.find transitions s) a t) tran;
  let accepting = Hashtbl.create length in
  List.iter (fun s -> Hashtbl.add accepting s (List.mem s fin)) qs;
  {
    states = qs;
    alphabet = List.sort compare alph;
    transitions;
    start = init;
    accepting;
  }
