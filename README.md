# Curios

Curios is an interpreter (and hopefully, in the future, a compiler) for a work-in-progress dependently typed functional programming language. The project is still very young, there is only an abstract syntax tree and a parser for it, but progress, albeit slow, is steady.

In the [test file](https://github.com/valmirjunior0088/curios/blob/master/test/Spec.hs) there is an example of what the source code looks like along with the abstract syntax tree that represents it. My first objective is to be able to represent the `Either` data type that is commonly available in other functional programming languages with only its primitives. To be able to achieve this, I still need to research what are the fundamental notions that need to be available as language constructs and implement them, meaning I still can't fully represent it (it is possible to represent the types of the constructors, but the eliminator for the inductive type has `unknown` all over as a placeholder).

The roadmap forward consists of three tasks, which will help me solve this problem and others:
1. What is the shape of the low level `Term` structure that represents the primitive calculus?
2. How do I translate the high level `Expression` language onto this low level `Term` language?
3. How do I evaluate/run `Term`s? (Better yet, how can computational power be added to this abstract calculus?)

Feel free to throw suggestions my way! Anything helps, really. There's a lot that still needs to be learned in order for this project to grow, and I love any and all discussions about type theory.