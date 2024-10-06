# midoriKanren

midoriKanren is a relational interpreter combining miniKanren-in-miniKanren and Scheme-in-miniKanren.

midoriKanren is a modified and extended version of Bharathi Ramana Joshi's `metaKanren`, mashed up with ideas and implementation techniques from the various relational Scheme interpreters that we and others have created over the years (for example, as seen in the 2017 ICFP Pearl).

midoriKanren has been tested in Chez Scheme 10.0.0.


metaKanren:

https://github.com/iambrj/metaKanren

https://icfp21.sigplan.org/details/minikanren-2021-papers/4/metaKanren-Towards-a-Metacircular-Relational-Interpreter

metaKanren: Towards A Metacircular Relational Interpreter.
Bharathi Ramana Joshi and William E. Byrd, 2021 miniKanren Workshop (online).

Full paper in the miniKanren 2021 Workshop proceedings:

https://tspace.library.utoronto.ca/handle/1807/110263

https://www.youtube.com/watch?v=1bUIRi6pZow

Also, Bharath's Master's Dissertation gives a more detailed and updated description of metaKanren:

https://web2py.iiit.ac.in/research_centres/publications/download/mastersthesis.pdf.b81e61d00632f930.746865736973202832292e706466.pdf


ICFP 2017 Pearl:

A Unified Approach to Solving Seven Programming Problems (Functional Pearl).
William E. Byrd, Michael Ballantyne, Gregory Rosenblatt, Matthew Might,
ICFP 2017, Oxford, UK.

https://dl.acm.org/doi/10.1145/3110252

https://dl.acm.org/doi/pdf/10.1145/3110252

https://github.com/gregr/icfp2017-artifact-auas7pp

https://podcasts.ox.ac.uk/unified-approach-solving-seven-programming-problems-functional-pearl


midoriKanren relies on the `faster-miniKanren` git submodule, from Michael Ballantyne.


The goals of midoriKanren is to create a relational interpreter than can handle both miniKanren and an interesting subset of Scheme, allowing Scheme and miniKanren code to be mixed freely, while still allowing logic variables to be used anywhere in the resulting code.

The resulting relational interpreter will allow us to push further the idea of using interpreters written as pure relations to encapsulate relational behavior, including synthesis, even while supporting paradigms and language features that aren't relational.

In other words, everything becomes relational through this One Weird Trick!

Just as Simon Peyton-Jones says that Haskell is the finest language he knows of for imperative programming, we want to be able to say that midoriKanren is the finest finest language we know of for programming with logical side-effects.

Please see `TODO.md` for a list of pending improvements and experiments.

The initial modifications to metaKanren were made by Will Byrd October 3--4, 20204, in Cádiz, Spain, in preparation for his talk at Lambda World 2024:

```
An Imperishable Wonderland of Infinite Fun

In which we will discover the Wonders of Relational Programming, and
learn of its deep connections with Functional Programming. We will
explore bizarre programs, and muse about what comes next, now that
Functional Programming has won.
```
