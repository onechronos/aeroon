(lang dune 3.0)
(using ctypes 0.1)

(generate_opam_files true)

(name aeroon)
(source (github onechronos/aeroon))
(license MIT)
(authors "Mika Illouz" "Simon Cruanes")
(maintainers "mika@illouz.net" "simon@imandra.ai")

(package
 (name aeroon)
 (synopsis "OCaml bindings to the aeron communication system")
 (description "A Ctypes/Cstubs-based binding to Aeron's C client")
 (depends dune ctypes ctypes-foreign (ocaml (>= "4.03"))))

