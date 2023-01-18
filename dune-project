(lang dune 3.4)

(name tsc)

(generate_opam_files true)

(license MIT)

(maintainers "ajob410@gmail.com")

(authors "Alistair O'Brien")

(package
 (name tsc)
 (synopsis "TypeScript type checker in OCaml")
 (depends dune core ppx_jane fmt))
