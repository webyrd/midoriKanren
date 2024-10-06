# midoriKanren Improvements and Experiments to try

## midoriKanren Improvements

Play with occurs-check for the miniKanren subset.

Add `lambda`, `list`, `cond`, and other useful parts of Scheme.

Use a non-empty initial environment, and make built-in functions primitives, like in the Barliman interpreter.  Similarly with miniKanren, if that makes sense (`==` should be a built in).

Modify the interpreter to allow Scheme and miniKanren to be mixed more freely.  For example, the outer expression shouldn't have to be a `run`.  In fact, Scheme code should be able to call multiple `run` expressions.


## midoriKanren Experiments

Add normalization-by-evaluation evaluator.

Add abstract interpreter (ideally for both Scheme and miniKanren).