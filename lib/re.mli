(** Representation of Regular Expressions and implementation of standard operations *)

open Tree

exception Syntax_error of string
(** [Syntax_error] is raised when the parser encounters a parsing error. It should include the problem character *)

val simplify : re -> re
(** [simplify r] attempts to simplify RE [r] by Kozen Axioms 
    @return a simplfication of RE [r] *)

val print : re -> unit
(** [print r] prints a string representation of [r] *)

val export_graphviz : re -> string
(** [export_graphviz r] 
    @return a representation of the Abstract Syntax Tree representing [r] in the DOT language for Graphviz *)

val get_alphabet : re -> string list
(** [get_alphabet r]
    @return the alphabet of RE [r] as a list *)

val is_nullable : re -> bool
(** [is_nullable r]
    @return true iff Epsilon is accepted by the RE [r] *)

val derivative : re -> string -> re
(** [derivative r a]
    @return the Brzozowski derivative of RE [r] with respect to symbol [a] *)

val parse : string -> re
(** [parse s] Invokes the parser on string [s] to create a Regular Expression Abstract Syntax Tree 
    @return Regular Expression Abstract Syntax Tree for string [s]
    @raise Syntax_error if [s] contains an invalid character *)
