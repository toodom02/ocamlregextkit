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
            try
                let re = parse line in
                let simp_re = simplify re in
                print_ast simp_re;
                print_newline ();
                let nfa2 = construct_nfa simp_re in
                print_nfa nfa2;
                print_newline ();
                let dfa2 = nfa_to_dfa nfa2 in
                print_dfa dfa2;
                print_newline ();
            with 
                 _ -> print_string "syntax error"; print_newline ()
        done
    with End_of_file -> 
        print_string "\nBye\n"
  
let regex = main ()