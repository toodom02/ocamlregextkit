let parse s =
    let lexbuf = Lexing.from_string s in
    try
        Parser.regex Lexer.token lexbuf
    with 
        Parsing.Parse_error -> 
            let tok = Lexing.lexeme lexbuf in
            Printf.printf "[ERROR] Syntax Error at token '%s'\n" tok;
            exit 1