open Ast
open Nfa
open Dfa

(* |parse| -- parse a string as a regular expression *)
let parse s =
    Parser.regex Lexer.token (Lexing.from_string s)

(* |main| -- main read-print loop *)
let main () =
    print_string "Welcome to the regex toolkit\n";
    try
        while true do
            print_string "? "; flush stdout;
            let line = input_line stdin in
            print_string "? "; flush stdout;
            let line2 = input_line stdin in
            try
                let re = parse line in
                let simp_re = simplify re in
                print_ast simp_re;
                print_newline ();
                let nfa = construct_nfa simp_re in
                (* print_nfa nfa2; *)
                (* print_newline (); *)
                let dfa = nfa_to_dfa nfa in
                (* print_dfa dfa2; *)
                (* print_newline (); *)
                let comp = dfa_compliment dfa in
                (* print_dfa comp; *)
                (* print_newline (); *)

                let re2 = parse line2 in
                let simp_re2 = simplify re2 in
                print_ast simp_re2;
                print_newline ();
                let nfa2 = construct_nfa simp_re2 in
                (* print_nfa nfa2; *)
                (* print_newline (); *)
                let dfa2 = nfa_to_dfa nfa2 in
                (* print_dfa dfa22; *)
                (* print_newline (); *)
                let comp2 = dfa_compliment dfa2 in
                (* print_dfa comp; *)
                (* print_newline (); *)

                let fst_and_not_snd = product_intersection dfa comp2 and
                    not_fst_and_snd = product_intersection comp dfa2 in

                let product_dfa = product_union fst_and_not_snd not_fst_and_snd in
                
                (* print_dfa fst_and_not_snd;
                print_newline ();
                print_dfa not_fst_and_snd;
                print_newline (); *)

                print_dfa product_dfa;
                print_newline ();
                
            with 
                 _ -> print_string "syntax error"; print_newline ()
        done
    with End_of_file -> 
        print_string "\nBye\n"
  
let regex = main ()