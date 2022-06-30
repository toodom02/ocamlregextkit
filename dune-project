(lang dune 2.1)

(name regextkit)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github toodom02/regex-toolkit))

(authors "Dominic Too")

(maintainers "Dominic Too")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name regextkit)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
