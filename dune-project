(lang dune 2.0)
(using menhir 2.0)

(generate_opam_files true)

(name evm_decompiler)
(version 0.0.1)
(authors "Georgiy Komarov")
(license  MIT)
(source (github jubnzv/evm-decompiler))
(maintainers "Georgiy Komarov <jubnzv@gmail.com>")

(package
  (name evm_decompiler)
  (synopsis "")
  (depends
        (ocaml (>= 4.13))
        (core :build)
        (clap :build)
        (sedlex :build)))
