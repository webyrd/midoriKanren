# Experiment: Curry-like Experiments

## The Idea

Michael Ballantyne proposed using midoriKanren to emulate Curry-style functional-logic programming.

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

## Future Work

Try more Curry examples, think about needed narrowing and residuation.