# Curios

Curios is a functional language with dependent types and algebraic effects that compiles to WebAssembly. Most languages with dependent types evolved from proof assistants, where non-determinism is a property to be excluded rather than embraced - Curios inverts this, aiming to bring dependent function types (Π-types, λ-abstractions), dependent tuple types (Σ-types, dependent pairs), and dependent enumeration types (disjoint sets of atoms with dependent elimination semantics) to a programming context where non-determinism is simply part of daily life.

Dependent types pay off most in a handful of recurring patterns. Length-indexed collections rule out bounds errors by construction, replacing runtime panics with type-level guarantees. Typed format strings derive their argument list directly from the format value, eliminating a whole class of variadic bugs. Dependent records encode protocol state in the type itself, turning invalid transitions into compile-time errors rather than runtime failures.

Algebraic effects bring composability to side effects: I/O, exceptions, and async/await all layer naturally without monad transformer stacks, and handlers can be swapped out - mocking I/O in tests, for instance, without touching call sites. Dependent effects extend this further, allowing an effect's return type to depend on the request value. This enables typed interaction protocols where each response's shape is statically determined by what was asked, catching mismatches that untyped effect systems leave to runtime.

The project's progress is being tracked [here](https://github.com/users/valmirjunior0088/projects/3/views/1).
