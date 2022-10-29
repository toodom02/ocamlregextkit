exception Syntax_error of string

let parse s =
    let lexbuf = Lexing.from_string s in
    try
        Parser.regex Lexer.token lexbuf
    with 
        Parsing.Parse_error -> 
            let tok = Lexing.lexeme lexbuf in
            raise (Syntax_error ("Syntax Error at token "^tok))