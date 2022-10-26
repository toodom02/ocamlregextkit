(* Demo program that compares two input regex *)

let main () = 
    (* Get CLI args *)
    let fns = ref [] in
    Arg.parse [] (fun s -> fns := !fns @ [s]) "";
    if List.length !fns <> 2 then (
      print_string "Usage: demo \"<regex>\" \"<regex>\"\n";
      exit 2;
    );

    (* Parse strings as REs *)
    let re1 = Ast.parse (List.hd !fns) and
        re2 = Ast.parse (List.nth !fns 1) in

    (* Reduce REs *)
    let re1' = Re.simplify re1 and
        re2' = Re.simplify re2 in

    (* Convert REs to NFAs *)
    let nfa1 = Nfa.re_to_nfa re1' and
        nfa2 = Nfa.re_to_nfa re2' in

    (* Merge the DFA alphabets *)
    let (nfa1', nfa2') = Nfa.merge_alphabets nfa1 nfa2 in

    (* Convert NFAs to DFAs *)
    let dfa1 = Dfa.nfa_to_dfa nfa1' and
        dfa2 = Dfa.nfa_to_dfa nfa2' in

    (* Compliment DFAs *)
    let comp1 = Dfa.compliment dfa1 and
        comp2 = Dfa.compliment dfa2 in

    (* Intersect DFAs *)
    let fst_and_not_snd = Dfa.product_intersection dfa1 comp2 and
        snd_and_not_fst = Dfa.product_intersection dfa2 comp1 in

    (* Find shortest unique strings *)
    let accepted1 = Dfa.accepted fst_and_not_snd and
        accepted2 = Dfa.accepted snd_and_not_fst in

    if (Option.is_none accepted1 && Option.is_none accepted2) then (
        print_string "Input regex are equal\n";
        exit 0;
    );
    if (Option.is_some accepted1) then Printf.printf "'%s' exists in the first but not the second\n" (Option.get accepted1);
    if (Option.is_some accepted2) then Printf.printf "'%s' exists in the second but not the first\n" (Option.get accepted2);

    exit 0

let demo = main ()