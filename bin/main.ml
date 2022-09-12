open Ast
open Nfa
open Dfa
(* open Print *)

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
                let simp_re = simplify re in
                (* print_ast simp_re; *)
                (* print_newline (); *)
                let re2 = parse line2 in
                let simp_re2 = simplify re2 in
                (* print_ast simp_re2; *)
                (* print_newline (); *)
                print_string "[DONE]\n";

                print_string "Constructing NFAs...\t";
                let nfa = construct_nfa simp_re in
                (* print_nfa nfa; *)
                (* print_newline (); *)
                let nfa2 = construct_nfa simp_re2 in
                (* print_nfa nfa2; *)
                (* print_newline (); *)
                print_string "[DONE]\n";

                (* need to correct each nfa's alphabets to be the union, otherwise we 
                   have an issue where the compliment of one language is the same as the compliment of another
                   (e.g. with languages EPSILON,  a* ) *)
                let new_nfa = merge_alphabets nfa nfa2 and
                    new_nfa2 = merge_alphabets nfa2 nfa in

                print_string "Constructing DFAs...\t";
                let dfa = nfa_to_dfa new_nfa in
                (* print_dfa dfa; *)
                (* print_newline (); *)
                let reduced_dfa = reduce_dfa dfa in
                (* print_dfa reduced_dfa; *)
                (* print_newline (); *)
                let dfa2 = nfa_to_dfa new_nfa2 in
                (* print_dfa dfa22; *)
                (* print_newline (); *)
                let reduced_dfa2 = reduce_dfa dfa2 in
                (* print_dfa reduced_dfa2; *)
                (* print_newline (); *)
                print_string "[DONE]\n";

                begin 
                    match (is_dfa_equal reduced_dfa reduced_dfa2) with
                        | Some word -> Printf.printf "'%s' exists in one regex but not the other" word;
                        | None -> print_string "Input regex are equal";
                end;
                print_newline ();
                print_newline ();
                
            with
                | Parser.Error -> print_string "[ERROR] Syntax Error\n\n"
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