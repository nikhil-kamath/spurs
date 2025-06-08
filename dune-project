(lang dune 3.17)

(name spurs)
(version v0.1.1)

(generate_opam_files true)

(source
 (github nikhil-kamath/spurs))

(authors "Nikhil Kamath <nikhil.k123234@gmail.com>")

(maintainers "Nikhil Kamath <nikhil.k123234@gmail.com")

(license MIT)

(documentation https://nikhil-kamath.github.io/spurs/)

(package
 (name spurs)
 (version 0.1.1)
 (synopsis "A lightweight native sparse matrix library")
 (depends
  (ocaml (>= 5.3.0))
  ppx_deriving
  ppx_fields_conv
  fmt
  (alcotest :with-test)
  (qcheck :with-test)
  (qcheck-alcotest :with-test))
 (tags
  ("sparse" "matrices")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
