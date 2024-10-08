# midoriKanren Improvements and Experiments to try

## midoriKanren Improvements

First TODO: go through Barath's Master's dissertation and see which improvements to the 2021 metaKanren make sense to port over to midoriKanren:

https://web2py.iiit.ac.in/research_centres/publications/download/mastersthesis.pdf.b81e61d00632f930.746865736973202832292e706466.pdf

Currently there is a single-binding `let` form supported by `eval-gexpro`, and no `let` expression supported by `eval-schemeo`.  Also, there are currently separate closures for relations (`closr`) and closures for Scheme procedures (`scheme-closr`).  Need to rethink how all the various `evalo`s and closure types should be combined, how `lambda` should be involved, how `define` and `defrel` should be added, etc.

Once `let` is implemented properly, these sorts of tests should work:

```
(test "trs1-1b"
  ;; from frame 60 on page 15 of TRS1
  (run* (x)
    (eval-programo
     `(run* (q)
        (let ((a (== #t q))
              (b (== #f q)))
          b))
     x))
  '((#f)))
```

Use CLP(FD) or CLP(Z) for `run n` counts, rather than Peano numerals in the form of nested parens.

Try adding `copy_term` as an operator---can we get a reasonable relational behavior?

Implement Hoare logic, relational style, with CLP(Set)

Split-environment representation (or use sets).

Write down and keep updated the new grammar.

Write a parser, outside of midoriKanren, in regular Scheme, that parses a program and gives useful errors.

Write a linter as well.

Add `=/=`, `symbolo`, `numbero`, `absento` constraints to the interpreter.

Use the updated CLP(Set) implementation to make run and run* return sets of answers instead of lists.  (Actually, should use the multisets from the CLP(Set) paper.)

Simplify the existing implementation as much as possible.  Instead of enumerating atomic terms, consider adding an `atomo` lazy constraint to faster-miniKanren.

See if the `delay`s are in the right places, and conj*/disj* behavior has the correct associativity, to match faster-miniKanren's search.

Play with occurs-check for the miniKanren subset.

Add `lambda`, `equal?`, `list`, `cond`, `match`, `and`, and other useful parts of Scheme.  (Check out the "simple" (unoptimized version) of the Barliman interpreter.)
Add `quasiquote` and `unquote`.

Add `defrel`, `define`, `begin`.

Use a non-empty initial environment, and make built-in functions primitives, like in the Barliman interpreter.  Similarly with miniKanren, if that makes sense (`==` should be a built in).

Modify the interpreter to allow Scheme and miniKanren to be mixed more freely.  For example, the outer expression shouldn't have to be a `run`.  In fact, Scheme code should be able to call multiple `run` expressions.

Add a real `letrec` along with `lambda`.  Make sure the syntax lines up so we can copy-and-paste code into miniKanren.

Add `conda`, `condu`, `onceo`, etc., to the relational mk interpreter.  Play with logical side effects in the context of the relational interpreter.

Also add Prolog-style collecting operations: `setof`, `listof`, `bagof`, whatever.  In fact, should revisit Prolog to see which other problem operators seem fine in the relational interpreter version of miniKanren.

Add a midoriScheme waiter to Chez, so we can use midoriScheme as the REPL.

Make sure we can run TRS1 and TRS2 examples, including TRS1 examples that mix Scheme and miniKanren code.


## midoriKanren Experiments

Come up with more interesting and creative queries!!

Write queries that fully take advantage of the `run` "collecting" semantics.  (Suggested by Nada Amin)

Add explicit errors to the Scheme portion.

Experiment with Prolog-style meta-programming and meta-interpreters.  Do `asset` and `retract` make sense now?  Could we write a simple Prolog interpreter this way?

Add normalization-by-evaluation evaluator.

Add abstract interpreter (ideally for both Scheme and miniKanren).

Add parsers, type inferencers, etc.

Play with Scheme-in-Scheme, like in the 2017 ICFP Pearl.

Add a miniKanren-in-miniKanren (in midoriKanren).

Add tabling.

Add macro expander.

See if the Barliman optimizations can be applied to midoriKanren.

Add delimited continuations.

Add probabilistic interpreter.

Add input/output, errors, mutation, and other effects.

Staging, supercompilation, optimizing compilers, etc.  "Throw all the computer science at it," as Edward Kmett would say.

# Questions

? Do we need to go deeper than one level of interpretation?  Do we gain useful expressive power, or is one level sufficient?