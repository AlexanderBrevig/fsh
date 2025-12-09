(lang dune 3.17)

(name fsh)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name fsh)
 (synopsis "Forth-inspired shell")
 (description "A concatenative, stack-based shell with Forth-like syntax")
 (depends
  (ocaml (>= 5.3.0))
  core
  core_unix
  ppx_jane
  lambda-term
  lwt
  lwt_react
  zed)
 (tags
  ("shell" "forth" "concatenative")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
