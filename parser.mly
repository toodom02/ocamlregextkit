%{
open Ast
%}

%token <string>     IDENT
%token              UNION CONCAT STAR
%token              EMPTY EPSILON
%token              LPAR RPAR
%token              EOF BADTOK

/* associativity and precedence, lowest to highest */
%left               UNION
%left               CONCAT
%left               STAR
%nonassoc           LPAR RPAR
%nonassoc           IDENT EPSILON EMPTY

%type <Ast.re>      regex
%start              regex

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
    | re re %prec CONCAT            { Concat ($1, $2) }
    | re STAR                       { Star $1 } ;