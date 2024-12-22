(lang dune 3.15)

(name dust)

(generate_opam_files true)

(source
 (github bricct/dust))

(authors "Trey Briccetti")

(maintainers "Trey Briccetti")

(license "ISC")

(documentation https://url/to/documentation)

(package
 (name dust)
 (synopsis "Declarative TUI Apps")
 (description "A small TUI framework powered by lwt and notty")
 (depends ocaml dune notty lwt)
 (tags
  ("TUI" "Lwt" "Notty")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
