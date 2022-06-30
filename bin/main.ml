open Ast

(* |parse| -- parse a string as a regular expression *)
let parse s =
    Parser.regex Lexer.token (Lexing.from_string s)

let () = print_ast (simplify (parse "(0.1.1.0 + 0.1).(1.0)*"))
let () = print_ast (simplify (parse "0.1.(1.0)*"))