(* |parse| -- parse a string as a regular expression *)
let parse s =
    Parser.regex Lexer.token (Lexing.from_string s)

(* |main| -- main read-print loop *)
let main () =
    print_string "Welcome to the regex toolkit\n";
    try
        while true do
            try
                print_string "? "; flush stdout;
                let line = input_line stdin in
                print_string "? "; flush stdout;
                let line2 = input_line stdin in

                print_string "Constructing ASTs...\t";
                let re = parse line in
                let simp_re = Ast.simplify re in
                let re2 = parse line2 in
                let simp_re2 = Ast.simplify re2 in
                print_string "[DONE]\n";
                (* Print.print_ast simp_re; *)
                (* print_newline (); *)
                (* Print.print_ast simp_re2; *)
                (* print_newline (); *)

                print_string "Constructing NFAs...\t";
                let nfa = Nfa.construct_nfa simp_re and
                    nfa2 = Nfa.construct_nfa simp_re2 in
                (* Print.print_nfa nfa; *)
                (* print_newline (); *)
                (* Print.print_nfa nfa2; *)
                (* print_newline (); *)
                print_string "[DONE]\n";

                (* need to correct each nfa's alphabets to be the union, otherwise we 
                   have an issue where the compliment of one language is the same as the compliment of another
                   (e.g. with languages EPSILON,  a* ) *)
                let new_nfa = Nfa.merge_alphabets nfa nfa2 and
                    new_nfa2 = Nfa.merge_alphabets nfa2 nfa in

                print_string "Constructing DFAs...\t";
                let dfa = Dfa.nfa_to_dfa new_nfa and
                    dfa2 = Dfa.nfa_to_dfa new_nfa2 in
                (* Print.print_dfa dfa; *)
                (* print_newline (); *)
                (* Print.print_dfa dfa22; *)
                (* print_newline (); *)
                print_string "[DONE]\n";

                print_string "Reducing DFAs...\t";
                let reduced_dfa = Dfa.reduce_dfa dfa and
                    reduced_dfa2 = Dfa.reduce_dfa dfa2 in
                (* Print.print_dfa reduced_dfa; *)
                (* print_newline (); *)
                (* Print.print_dfa reduced_dfa2; *)
                (* print_newline (); *)
                print_string "[DONE]\n";

                begin 
                    match (Dfa.is_dfa_equal reduced_dfa reduced_dfa2) with
                        | Some word -> Printf.printf "'%s' exists in one regex but not the other" word;
                        | None -> print_string "Input regex are equal";
                end;
                print_newline ();
                print_newline ();
                
            with
                | Failure msg -> Printf.printf "[ERROR] Syntax Error - %s\n\n" msg;
                | e ->
                    let msg = Printexc.to_string e
                    and stack = Printexc.get_backtrace () in
                    Printf.eprintf "[ERROR] %s%s\n" msg stack;
                    raise e
        done
    with End_of_file -> 
        print_string "\nBye\n"
  
let regex = main ()


(* TODO: Some issue with the following strings:
        ? (1+0.1)*.(0+?)
        ? (1*.0.1.1* ).(0+?)+1*.(0+?)

        caused by stack overflow from powerset (and from typing each set as State)
*)