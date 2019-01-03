Some Notes
---

* The name of the language: it's spelled "café";  one may also use "cafe" if in
  a non-unicode context.   The name should be capitalized as if it were an
  improper noun  (that means, in titles and at the beginnings of sentences).
  The name of the compiler is `cafec`.

Code of Conduct
---

* I feel that it's necessary to have a code of conduct. It can be found in
  [code-of-conduct.md][coc].
* In order to contribute to this project, or to be a part of the café community,
  you must follow this code of conduct whenever you're in a public forum
  dedicated to café (i.e., this github).
* We understand that sometimes people are frustrated, or having a bad day, and
  sometimes we screw up. This lays out our ideal, but it's okay to screw up in
  good faith! We all fall down, we just have to get back up again.
* Most of all, be friendly to people.

Building
---

Prerequisites
===

* Jane Street's [base] and [ppx_let]
* Daniel Bünzli's [uunf], [uutf], and [uucp]
* For testing: Sylvain Le Gall's [OUnit]

In order to install dependencies, you should be able to simply run
`dune external-lib-deps --missing @@default`.

Actually Building
===

You should be able to (after installing dependencies), run `dune build`.

Testing
===

The tests in `test/` are not yet even close to useful.
I hope to work on tests once I finish the lvalue overhaul.

The only real examples of café code are in `language/` - `language/scratch.cf`
is a place for testing new features, and will likely be removed from the
repository once I finish the lvalue overhaul. In `language/misc`, I have
some somewhat "real" examples of café.
Currently, there're advent of code solutions,
but there might be more at some point.

[coc]: https://github.com/ubsan/cafe/blob/master/code-of-conduct.md
[base]: https://github.com/janestreet/base
[ppx_let]: https://github.com/janestreet/ppx_let
[uunf]: http://erratique.ch/software/uunf
[uutf]: http://erratique.ch/software/uutf
[uucp]: http://erratique.ch/software/uucp
[OUnit]: http://ounit.forge.ocamlcore.org/
