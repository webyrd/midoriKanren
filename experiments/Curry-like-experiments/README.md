# Experiment: Curry-like Experiments

## The Idea

Michael Ballantyne proposed using midoriKanren to emulate Curry-style functional-logic programming.

Function bodies should be able to contain constraints, and goals should be able to express things like:

```
(== x (func y z))
```

Michael wasn't sure whether shallow or deep embeddings would work best (or maybe a combination?).

Michael pointed out that the in the code from my Lambda World 2024 talk, non-determinism from holes (logic variables) in the Scheme portions show up "at the outer level," while non-determinism from `disj` shows up "at the inner level."  Michael thinks in the mixture of Scheme and mk he envisions, the non-determinism would show up at the *same level*, like in a functional-logic programming language. (WEB: I'm not sure I entirely understand what Michael means--a few examples could go a long way.)

Michael also thinks this would be particulatly interested for staged evaluation.

Michael found this example from the [Curry Wikipedia page](https://en.wikipedia.org/wiki/Curry_%28programming_language%29).

Given a functional definition of `append`, define a function that gets the last element of it:

```
last xs | ys++[e] =:= xs = e where ys,e free
```

Michael made up some syntax as a starting point for what we might try to implement in midoriKanren:

```
(letrec-fn ([last (lambda (xs)
                    (fresh (ys e)
		      (== xs (append ys (list e)))
		      (return e)))]))
```

## Approach

Refine the syntax, implement anything critical that is missing, and try it out!

Also, both Michael and Nada Amin think that the aggregation behavior is interesting and important.  Nada is interested in what we can express from getting back aggregated answers from `run^n`/`run*` for example.  What games can you play with `absento` over these aggregated results?  What changes/improves if you use CLP(Set) to represent the results from a `run`?  Etc.  Lots to explore here.

## Future Work

Try more Curry examples, think about needed narrowing and residuation.