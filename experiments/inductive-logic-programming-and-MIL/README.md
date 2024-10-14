# Experiment: Inductive Logic Programming (ILP) and Meta-Interpretive Learning (MIL)

## The Idea

The paper [Turning 30: New Ideas in Inductive Logic Programming, by Cropper, Dumančić, and Muggleton. Proceedings of the Twenty-Ninth International Joint Conference on Artificial Intelligence (IJCAI-20), Survey Track](https://www.ijcai.org/Proceedings/2020/0673.pdf) decribes the basic ideas of Inductive Logic Programming (ILP), along with more recent advances in Meta-Interpretive Learning (MIL) that make predicate invention and synthesis of recursive logic programs feasible.

With an mk-in-mk, it seems that we should be able to encode ILP problems.  It also seems like we should be able to encode MIL problems, where we use "outer" query variables to represent the predicate symbols.

How many of these examples can we express in a straightforward manner?  Does staging help?

What about the meta-rules, like the meta chain rule given in the paper?  Does that help reduce the search space?

Can we combine the ILP/MIL techniques with our standard approaches to synthesis?

## Approach

Try encoding simple ILP examples described in the paper, and work up to MIL examples.

## Future Work

There are tons of papers on ILP, and modern ILP systems like [Popper](https://github.com/logic-and-learning-lab/Popper/) also support Answer Set Programming (ASP) and the ability to call out to SAT/SMT solvers.