type 't automata =
  { mutable states : 't list
  ; mutable alphabet : string list
  ; transitions : ('t, (string, 't) Hashtbl.t) Hashtbl.t
  ; mutable start : 't
  ; accepting : ('t, bool) Hashtbl.t
  }

let get_states m = m.states
let set_states m qs = m.states <- qs
let iter_states f m = List.iter f m.states
let filter_states f m = List.filter f m.states
let exists_states f m = List.exists f m.states
let for_all_states f m = List.for_all f m.states
let map_states f m = List.map f m.states
let get_alphabet m = m.alphabet
let set_alphabet m alph = m.alphabet <- alph
let iter_alphabet f m = List.iter f m.alphabet
let filter_alphabet f m = List.filter f m.alphabet
let map_alphabet f m = List.map f m.alphabet
let exists_alphabet f m = List.exists f m.alphabet
let for_all_alphabet f m = List.for_all f m.alphabet

let get_transitions m =
  Hashtbl.fold
    (fun s ats acc -> Hashtbl.fold (fun a t acc' -> (s, a, t) :: acc') ats acc)
    m.transitions
    []
;;

let iter_transitions f m =
  Hashtbl.iter (fun s v -> Hashtbl.iter (fun a t -> f (s, a, t)) v) m.transitions
;;

let map_transitions f m =
  Hashtbl.fold
    (fun s v acc -> Hashtbl.fold (fun a t acc' -> f (s, a, t) :: acc') v acc)
    m.transitions
    []
;;

let get_start m = m.start

let get_accepting m =
  Hashtbl.fold (fun k v acc -> if v then k :: acc else acc) m.accepting []
;;

let iter_accepting f m = Hashtbl.iter (fun s v -> if v then f s) m.accepting

let map_accepting f m =
  Hashtbl.fold (fun s v acc -> if v then f s :: acc else acc) m.accepting []
;;

let get_next_states m s a = Hashtbl.find_all (Hashtbl.find m.transitions s) a

let get_prev_states m t a =
  Hashtbl.fold
    (fun s v acc ->
      Hashtbl.fold (fun a' t' acc' -> if a = a' && t = t' then s :: acc' else acc') v acc)
    m.transitions
    []
;;

let is_accepting m s =
  match Hashtbl.find_opt m.accepting s with
  | None -> false
  | Some res -> res
;;

let get_reachable_states m =
  let rec find_reachable_states marked =
    let newmarked =
      List.fold_left
        (fun acc s ->
          List.fold_left
            (fun acc2 a -> Utils.list_union acc2 (get_next_states m s a))
            acc
            m.alphabet)
        marked
        marked
    in
    if marked <> newmarked then find_reachable_states newmarked else newmarked
  in
  find_reachable_states [ m.start ]
;;

let filter_states_inplace m f =
  set_states m (List.filter f m.states);
  Hashtbl.filter_map_inplace (fun s ts -> if f s then Some ts else None) m.transitions;
  Hashtbl.filter_map_inplace (fun s b -> if f s then Some b else None) m.accepting
;;

let merge_states_inplace m p q =
  filter_states_inplace m (fun s -> s <> q);
  Hashtbl.iter
    (fun _ v -> Hashtbl.filter_map_inplace (fun _ t -> Some (if t = q then p else t)) v)
    m.transitions;
  if m.start = q then m.start <- p
;;

let add_to_alphabet m alph =
  set_alphabet m (List.sort compare (Utils.list_union m.alphabet alph))
;;

let map_accepting_inplace f m =
  Hashtbl.filter_map_inplace (fun k _ -> Some (f k)) m.accepting
;;

let copy m =
  let copytrans = Hashtbl.copy m.transitions in
  Hashtbl.filter_map_inplace (fun _ v -> Some (Hashtbl.copy v)) copytrans;
  { states = m.states
  ; alphabet = m.alphabet
  ; transitions = copytrans
  ; start = m.start
  ; accepting = Hashtbl.copy m.accepting
  }
;;

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
  { states = qs; alphabet = List.sort compare alph; transitions; start = init; accepting }
;;
