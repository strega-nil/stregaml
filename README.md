Compile Instructions
---

You'll also need [pred] in order to compile the compiler - this is a
standard library extension.

You'll want to follow [these][opam-pin] instructions; basically,

```bash
git clone git@github.com:ubsan/pred
cd pred
opam pin add pred .
```

Some Notes
---

* The name of the language: it's spelled "café";
  one may also use "cafe" if in a non-unicode context.
  The name should be capitalized as if it were an improper noun
  (that means, in titles and at the beginnings of sentences).
* any SCREAMING_SNAKES identifiers are intended as a stop-gap,
  to be replaced later, and only exist because the compiler isn't
  really very good yet. Examples include the operator functions,
  which will be replaced by the appropriate operators eventually.
  
[pred]: https://www.github.com/ubsan/pred
[opam-pin]: https://opam.ocaml.org/doc/Packaging.html
