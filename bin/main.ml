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
                (* print_newline (); *)
                let nfa = construct_nfa simp_re in
                (* print_nfa nfa; *)
                (* print_newline (); *)
                let dfa = nfa_to_dfa nfa in
                print_dfa dfa;
                (* print_newline (); *)
                let reduced_dfa = reduce_dfa dfa in
                print_dfa reduced_dfa;
                (* print_newline (); *)
                let comp = dfa_compliment reduced_dfa in
                (* print_dfa comp; *)
                (* print_newline (); *)
               

                let re2 = parse line2 in
                let simp_re2 = simplify re2 in
                print_ast simp_re2;
                (* print_newline (); *)
                let nfa2 = construct_nfa simp_re2 in
                (* print_nfa nfa2; *)
                (* print_newline (); *)
                let dfa2 = nfa_to_dfa nfa2 in
                (* print_dfa dfa22; *)
                (* print_newline (); *)
                let reduced_dfa2 = reduce_dfa dfa2 in
                let comp2 = dfa_compliment reduced_dfa2 in
                (* print_dfa comp; *)
                (* print_newline (); *)

                let fst_and_not_snd = product_intersection reduced_dfa comp2 and
                    not_fst_and_snd = product_intersection comp reduced_dfa2 in

                let product_dfa = product_union (reduce_dfa fst_and_not_snd) (reduce_dfa not_fst_and_snd) in
                
                (* print_dfa fst_and_not_snd; *)
                (* print_newline (); *)
                (* print_dfa not_fst_and_snd; *)
                (* print_newline (); *)

                (* print_dfa product_dfa; *)
                (* print_newline (); *)

                if (is_dfa_empty (reduce_dfa product_dfa)) then
                    print_string "Input regex are equal" 
                else print_string "Input regex are not equal";
                print_newline ();
                print_newline ();
                
            with 
                 _ -> print_string "syntax error"; print_newline ()
        done
    with End_of_file -> 
        print_string "\nBye\n"
  
let regex = main ()