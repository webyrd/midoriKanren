# Experiment: `full-interp` meets microKanren

## The Idea

The original version of midoriKanren was a modified, extended variant of Bharathi Ramana Joshi's 2021 version of `metaKanren`.  (Bharath improved metaKanren as part of his Master's dissertation work, but these improvements were not in the original midoriKanren.)

Basically, the original midoriKanren added more Scheme to metaKanren.

Another approach to a midoriKanren-like system would be to add miniKanren or microKanren to a relational Scheme interpeter.

Which approach is cleaner in practice, I'm not sure.  It may be that it's useful to both add Scheme to miniKanren, and to add miniKanren to Scheme, and then write a new midoriKanren from scratch using what we've learned.  Or perhaps adding miniKanren to an existing relational Scheme interpreter works best.

The goal of this experiment is to better understand which approach seems most promising, and to try to push the expressiveness and implementation of a midoriKanren-like system, especially when it comes to combining Scheme and miniKanren expressions.  For example, in the original midoriKanren there are separate `letrec`-like forms to define recursive Scheme procedures and to define recursive miniKanren relations---I suspect it would be much cleaner to instead have the "real" Scheme `letrec`, and layer syntactic sugar on top if desired.

## The Approach

Starting from [`full-interp.scm`](https://github.com/michaelballantyne/faster-minikanren/blob/master/full-interp.scm) from Michael Ballantyne's [`faster-miniKanren`](https://github.com/michaelballantyne/faster-miniKanren) implementation, add [microKanren](https://github.com/jasonhemann/microKanren) as described in Hemann and Friedman's [2013 Scheme Workshop paper](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf).

Make sure `==` is implemented as a primitive procedure in the initial environment, just like `cons`.  You should be able to write and evaluate expressions like:

```
(let ((l (list ==)))
  (let ((ans (run* (q) ((car l) q 4))))
    (if (null? ans)
        'doh
	(car ans))))
```

Multiple `run*` should be allowed within an expression.  No need for `run*` to appear at the top-level.

`letrec` and `lambda` are allowed, of course.  Scheme procedures can be defined outside or inside of `run*`, just like in "regular" TRS1-style miniKanren.

miniKanren relations are just Scheme proceedures.

Should be able to mix-and-match Scheme and miniKanren code, just like we do in the first edition of TRS.

Should be able to copy expressions that mix-and-match Scheme and miniKanren from this version of midoriKanren, and paste and run those expressions in Chez REPL with miniKanren loaded.  Similarly, should be able to take Scheme+miniKanren expressions from Chez with miniKanren loaded, and paste and run those expressions in this variant of midoriKanren.  (Just like we can do both of these things with Scheme code and `evalo`.)

## Future Work

Once we've combined `full-inter.scm` with microKanren, we will want to extend microKanren to handle full miniKanren.  Similarly, we may want to go beyond `full-interp.scm` towards the Barliman interpreter in [`interp-simple.scm`](https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/interp-simple.scm) or even the [fully optimized Barliman interpeter](https://github.com/webyrd/Barliman/blob/master/cocoa/Barliman/mk-and-rel-interp/interp.scm).  These interpreters add a lot of complexity, though.

`full-interp.scm` + microKanren/miniKanren should be an excellent target for staged evaluation.