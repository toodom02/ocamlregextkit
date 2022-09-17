%{
open Ast
%}

%token <string>   IDENT
%token            UNION
%token            CONCAT
%token            STAR
%token            EMPTY
%token            EPSILON
%token            LPAR RPAR
%token            EOF BADTOK

/* associativity and precedence */
%left             UNION
%left             CONCAT
%left             STAR

%type <Ast.re>    regex
%start            regex

%%

regex :
      re EOF                        { $1 } ;

re :
      IDENT                         { Literal $1 }
    | EPSILON                       { Epsilon }
    | EMPTY                         { Empty }
    | LPAR re RPAR                  { $2 }
    | re UNION re                   { Union ($1, $3) }
    | re CONCAT re                  { Concat ($1, $3) }
    | re STAR                       { Star $1 }
    | re re                         { Concat ($1, $2) } ;