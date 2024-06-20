(lang dune 3.16)

(name jsoml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "rbnrayan")

(maintainers "rbnrayan")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name jsoml)
 (synopsis "A Json parser")
 (description "A Json parse implemented in Ocaml for learning purposes")
 (depends ocaml dune)
 (tags
  (topics ocaml json parser lexer)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
