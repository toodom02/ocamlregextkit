open Ast

(* |parse| -- parse a string as a regular expression *)
let parse s =
    Parser.regex Lexer.token (Lexing.from_string s)

let () = print_ast (parse "(a + b)* + a.c + EPSILON")