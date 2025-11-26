# Lox

An interpreter for the [Lox language](https://craftinginterpreters.com/the-lox-language.html), written in OCaml.

## Build

To build the interpreter, ensure that you have [dune](https://dune.build/) >= 3.20 and the following dependencies:

- [Core](https://ocaml.org/p/core/v0.17.1) 0.17.1,
- [Core_unix](https://ocaml.org/p/core_unix/v0.17.1) 0.17.1, and
- [Menhir](https://ocaml.org/p/menhir/20250912) 20250912.

Then, run:

```shell
$ dune build
```

The interpreter executable can then be found at `./_build/default/bin/lox.exe`.
In `./test_programs/`, there are a bunch of test programs from the original
[Crafting Interpreters](https://github.com/munificent/craftinginterpreters/tree/master/test) GitHub.

## Notes

My implementation differs in some ways from Rob Nystrom's:

- I use [Menhir](https://gallium.inria.fr/~fpottier/menhir/) to generate the parser from the grammar, rather than hand-rolling it myself.
  I had to modify the grammar a bit to resolve shift-reduce conflicts: in particular, I adjusted the assignment rule to take just a `<call>`
  on the left-hand side, instead of `(<call> DOT)? <identifier>`.
  Then, a second pass through the tree verifies that assignment targets are lvalues (i.e. they end with an `<identifier>`).
- Instead of a resolver, I create a new environment whenever a new variable is declared.
  Because it's rather annoying to manually plum the environments through the code, it's implemented as essentially the
  [State monad](https://wiki.haskell.org/State_Monad) from Haskell.
- I deviate slightly from Lox's original semantics for variable declaration:
  ```
  var a = 0;
  {
    var a = a;
    print a;
  }
  ```
  The original semantics reject this fragment of code, but I'm of the opinion that it's perfectly reasonable.
  Hence, my implementation allows it.
- Since I don't use a resolver, I don't (currently) check for duplicate declarations statically; they are instead handled dynamically.
  Similarly, invalid uses of `this` are also checked dynamically.
