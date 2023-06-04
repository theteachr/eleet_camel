(lang dune 3.6)

(name eleet_camel)

(generate_opam_files true)

(source
 (github theteachr/eleet_camel))

(authors "theteachr")

(maintainers "theteachr")

(license LICENSE)

(package
 (name eleet_camel)
 (synopsis "Collection of solutions to LeetCode problems")
 (depends ocaml dune)
 (tags
  (ocaml leetcode-solutions)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
