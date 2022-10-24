(* |parse| -- parse a string as a regular expression *)
let parse s : Ast.re =
    let lexbuf = Lexing.from_string s in
    try
        Parser.regex Lexer.token lexbuf
    with 
        Parsing.Parse_error -> 
            let tok = Lexing.lexeme lexbuf in
            Printf.printf "[ERROR] Syntax Error at token '%s'\n" tok;
            exit 1

(* |main| -- main read-print loop *)
let main () =
    let dflag = ref false and
        vflag = ref false and
        oflag = ref false and
        fns = ref [] and
        usage = "Usage: compregex [-option] \"<regex>\" \"<regex>\"" in
    Arg.parse [
        ("-v", Arg.Set vflag, " Output stages and timings of the program");
        ("-d", Arg.Set dflag, " Output constructed ASTs, NFAs, and DFAs for debugging");
        ("-O", Arg.Set oflag, " Use optimised DFA construction");
    ] (fun s -> fns := !fns @ [s]) usage;
    if List.length !fns <> 2 then (
        Printf.eprintf "%s\n" usage;
        exit 2;
    );
    let reg1 = List.hd !fns and
        reg2 = List.nth !fns 1 in
    try
        let starttime = Sys.time() in
        if !vflag then print_string "Constructing ASTs...\t";
        let re = parse reg1 in
        let simp_re = Ast.simplify re in
        let re2 = parse reg2 in
        let simp_re2 = Ast.simplify re2 in
        let asttime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (asttime -. starttime);
        if !dflag then (
            Ast.print simp_re;
            print_newline ();
            Ast.print simp_re2;
            print_newline ();
        );

        if !vflag then print_string "Constructing NFAs...\t";
        let nfa = Nfa.re_to_nfa simp_re and
            nfa2 = Nfa.re_to_nfa simp_re2 in

        (* need to correct each nfa's alphabets to be the union, otherwise we 
            have an issue where the compliment of one language is the same as the compliment of another
            (e.g. with languages EPSILON,  a* ) *)
        let (new_nfa, new_nfa2) = Nfa.merge_alphabets nfa nfa2 in
        let nfatime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (nfatime -. asttime);
        if !dflag then (
            Nfa.print new_nfa;
            print_newline ();
            Nfa.print new_nfa2;
            print_newline ();
        );

        if !vflag then print_string "Constructing DFAs...\t";
        let dfa = if !oflag then Dfa.nfa_to_dfa new_nfa else Dfa.nfa_to_dfa_subset new_nfa and
            dfa2 = if !oflag then Dfa.nfa_to_dfa new_nfa2 else Dfa.nfa_to_dfa_subset new_nfa2 in
        let dfatime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (dfatime -. nfatime);
        if !dflag then (
            Dfa.print dfa;
            print_newline ();
            Dfa.print dfa2;
            print_newline ();
        );

        (* No need to reduce DFAs if we use our optimised *)
        if !vflag && not !oflag then print_string "Reducing DFAs...\t";
        let reduced_dfa = if !oflag then dfa else Dfa.prune dfa and
            reduced_dfa2 = if !oflag then dfa2 else Dfa.prune dfa2 in
        let reddfatime = Sys.time() in
        if !vflag && not !oflag then Printf.printf "[DONE] %fs\n" (reddfatime -. dfatime);
        if !dflag && not !oflag then (
            Dfa.print reduced_dfa;
            print_newline ();
            Dfa.print reduced_dfa2;
            print_newline ();
        );

        if !vflag then print_string "Comparing DFAs...\t";
        let string1 = Dfa.find_unique_word reduced_dfa reduced_dfa2 and
            string2 = Dfa.find_unique_word reduced_dfa2 reduced_dfa in
        let finaltime = Sys.time() in
        if !vflag then Printf.printf "[DONE] %fs\n" (finaltime -. reddfatime);

        if (Option.is_none string1 && Option.is_none string2) then (
            print_string "Input regex are equal\n";
            exit 0;
        );
        begin 
            match string1 with
                | Some word -> Printf.printf "'%s' exists in the first but not the second\n" word;
                | None -> ();
        end;
        begin
            match string2 with
                | Some word -> Printf.printf "'%s' exists in the second but not the first\n" word;
                | None -> ();
        end;

        exit 0;
        
    with
        | e ->
            let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            Printf.eprintf "[ERROR] %s%s\n" msg stack;
            raise e
  
let compregex = main ()