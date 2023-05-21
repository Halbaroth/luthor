(lang dune 3.7)

(name luthor)

(generate_opam_files true)

(source
 (github Halbaroth/luthor))

(authors "Pierre Villemot")

(maintainers "Pierre Villemot")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name luthor)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   ppx_sexp_conv
   ppx_compare
   ppx_deriving
   (ppx_inline_test :with-test)
   (ppx_assert :with-test))
 (tags
  (topics "to describe" your project)))
