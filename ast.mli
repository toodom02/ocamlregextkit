(** Interface to generate Abstract Syntax Trees for Regular Expressions *)

(** [Syntax_error] is raised when the parser encounters a parsing error. It should include the problem character *)
exception Syntax_error of string

(** [parse s] Invokes the parser on string [s] to create a Regular Expression Abstract Syntax Tree 
    @raise Syntax_error if [s] contains an invalid character
*)
val parse : string -> Re.re