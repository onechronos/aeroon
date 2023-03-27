(lang dune 2.9)

(generate_opam_files true)

(name aeroon)

(source
 (github onechronos/aeroon))

(license MIT)

(authors "Mika Illouz" "Simon Cruanes")

(maintainers "mika@illouz.net" "simon@imandra.ai")

(package
 (name aeroon)
 (synopsis "OCaml bindings to the aeron communication system")
 (description "a wrapper to Aeron's C client")
 (depends
  dune
  logs
  (odoc :with-doc)
  (containers-thread
   (and
    (>= 3.0)
    (< 4.0)))
  (ocaml
   (>= "4.03"))))
