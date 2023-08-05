(** Type definition of Regular Expression Abstract Syntax Tree *)

type re =
  | Literal of string (* a ∈ Σ *)
  | Epsilon           (* ε *)
  | Empty             (* ∅ *)
  | Union of re * re  (* E + R *)
  | Concat of re * re (* E·R *)
  | Star of re        (* E* *)
