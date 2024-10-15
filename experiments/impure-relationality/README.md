# Experiment: Impure Relationality Experiments

## The Idea

In the relational Scheme interpreter (`evalo`), Scheme functions like `append` can be used as relations through the use of logic variables in argument positions.  Use of `append` with quotatation can recapitulate the behavior of `appendo`, as we show in the ICFP 2017 Pearl.

To what extent does a purely relational miniKanren-in-miniKanren evaluator, such as metaKanren, allow for interesting and useful emergent behavior of operations with "logical side-effects," such as `conda`, `condu`, `onceo`, cut (`!`), `copy_term`, etc.?  Could you embed the miniKanren from TRS1, complete with `conda` and `condu`, in a relational interpreter, and do cool synthesis things?  Would you need to enforce the "G-rule" in the implementation of `conda`/`condu`?

Bharath has implemented non-relational control operations in the extended metaKanren for his Master's dissertation.  Are there interesting examples of such behavior in the dissertation or example programs?

## Approach

Look at Bharath's work on non-relational control operators: `ifte`, `once`, and `var?`/`varo`.  Are these in the `condau` branch of the private repo?

As needed, carefully encode one or more interesting operations that don't seem to have a purely relational interpretation.  `conda`/`condu` are obvious candidates.  Beware the "G-rule"!

As Bharath points out, we need to heed Oleg's warning about the combination of constraints and `conda`/`condu` resulting in unsound behavior.

## Future Work

Encode a Prolog interpreter with `assert`, `retract`, and all the other stuff.

Prolog-style meta-interpreter in metaKanren and/or midoriKanren.