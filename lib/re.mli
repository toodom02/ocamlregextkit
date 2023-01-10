(** Representation of Regular Expressions and implementation of standard operations *)

open Tree

(** [Syntax_error] is raised when the parser encounters a parsing error. It should include the problem character *)
exception Syntax_error of string

(** [simplify r] attempts to simplify RE [r] by Kozen Axioms 
    @return a simplfication of RE [r] *)
val simplify : re -> re

(** [print r] prints a string representation of [r] *)
val print : re -> unit 

(** [export_graphviz r] 
    @return a representation of the Abstract Syntax Tree representing [r] in the DOT language for Graphviz *)
val export_graphviz : re -> string

(** [parse s] Invokes the parser on string [s] to create a Regular Expression Abstract Syntax Tree 
    @return Regular Expression Abstract Syntax Tree for string [s]
    @raise Syntax_error if [s] contains an invalid character *)
val parse : string -> re