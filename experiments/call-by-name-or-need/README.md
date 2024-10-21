# Experiment: Implement call-by-name or call-by-need for functions in midoriKanren

## The Idea

As Michael Ballantyne points out, there is no reason we need to implement exactly the same semantics as miniKanren-and-Scheme for midoriKanren.  Michael gives this nice example:

```
(== (list 1 2) (list (f x) 2))
```

Currently we would evaluate the call `(f x)` before performing the unification.  However, it could help the call to `f` if we knew that `1` is the expected value of `(f x)`.  (Curry might use needed narrowing to do the equivalent.)

Could we implement call-by-name and/or call-by-need semantics to make Michael's example work?

Or use delyed goals?

Or do we need to implement needed narrowing, etc.?

## Approach

Implement call-by-name/call-by-need sementics for the relational interpreter, and see if that gives us the desired behavior, and helps with avoiding generate-and-test or branching prematurely in synthesis.

## Future Work

Implement Curry-like evaluation strategies.