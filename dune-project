(lang dune 3.4)

(name ocamlregextkit)

(generate_opam_files true)

(source
 (github toodom02/ocamlregextkit))

(authors "Dominic Too")

(maintainers "Dominic Too")

(license GPL-3.0-or-later)

(documentation https://toodom02.github.io/ocamlregextkit/)

(package
 (name ocamlregextkit)
 (synopsis "A regular expression toolkit for OCaml")
 (description "Provides data structures and algorithms for Regular Expressions, Deterministic Finite Automata, and Non-Deterministic Finite Automata")
 (depends 
    (ocaml (>= 4.14))
    dune
  )
 (tags
  (automata "regular expressions" "regular languages" regex library DFA NFA RE)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
