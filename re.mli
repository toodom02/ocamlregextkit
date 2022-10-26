(** Representation of Regular Expressions and implementation of standard operations *)

type re = 
      Literal of string     (* a ∈ Σ *)
    | Epsilon               (* ε *)
    | Empty                 (* ∅ *)
    | Union of re * re      (* E + R *)
    | Concat of re * re     (* E·R *)
    | Star of re            (* E* *)

(** [simplify r] attempts to simplify RE [r] by Kozen Axioms *)
val simplify : re -> re

(** [print r] prints a string representation of the Abstract Syntax Tree representing [r] *)
val print : re -> unit 