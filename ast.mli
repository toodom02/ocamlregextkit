(** Interface to generate Abstract Syntax Tree for Regular Expressions *)

(** [parse s] Invokes the parser on string [s] to create a Regular Expression Abstract Syntax Tree *)
val parse : string -> Re.re