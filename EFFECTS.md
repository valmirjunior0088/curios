# Algebraic Effects

Algebraic effects allow computations to _perform_ operations — such as reading
state, throwing exceptions, or yielding values — without committing to a
concrete implementation. A _handler_ intercepts these operations and defines
their meaning. This gives the composability of monads without the nesting
problem.

At runtime, an effectful computation runs until it either returns a final value
or yields an effect request. An evaluator loop inspects the result, dispatches
the request to the appropriate handler, and re-enters the computation with the
handler's answer.

The computation is compiled as a resumable state machine that jumps to the
correct resumption point on re-entry using saved state. This trampoline strategy
avoids first-class continuations and fits naturally into the existing
second-class CPS intermediate representation.

## Syntax

Three syntactic forms are introduced: effect declarations, `yield` expressions,
and `handle` blocks.

### Effect Declarations

An effect declaration defines a named effect with one or more operations, each
with a typed signature:

```
effect State
  get : () -> S;
  put : S -> ();
```

Parameterized effects are expressed via type-level lambdas:

```
let State : (S : Type) -> Type = S => effect
  get : () -> S;
  put : S -> ();;
```

### Yield

A `yield` expression invokes an effect operation, suspending the current
computation until a handler provides a response:

```
yield get ()
```

```
yield put 42
```

### Handle

A `handle` block installs a handler around a computation. Each arm matches an
effect operation and receives two bindings: the operation's argument and `k`,
the continuation representing the rest of the computation after the yield site.

To resume the computation, apply `k` to the value the operation should return:

```
handle computation
  get () k => k current_state;
  put s k => k ();
```

To abort instead of resuming — for exception-like behavior — discard `k` and
evaluate to a bare value, which becomes the result of the entire `handle`
expression:

```
handle computation
  fail msg k => Error msg;
```

An optional final arm transforms the computation's return value when it
completes without performing any further effects. When omitted, it defaults to
the identity function:

```
handle computation
  get () k => k current_state;
  put s k => k ();
  x => x + 1
```

## Semantics

### Effect Types

An effectful computation that produces a value of type `B` while performing
effects in row `R` has type `[R] B`. A function that accepts an `A` and returns
such a computation has type `A -> [R] B`.

Effect rows are sets of named effects with an optional tail variable representing
unhandled effects:

```
[]          -- empty row, no effects
[State]     -- single effect
[State | R] -- State plus whatever effects R contains
```

### Reduction Rules

An effect type with an empty row reduces to its output type:

```
[] B ~> B
```

Nested effect types flatten by row union:

```
[R] ([S] B) ~> [R | S] B
```

### Handler Obligations

For every operation declared as `op : A -> B` in an effect `E`, a handler arm
must satisfy:

```
op : A -> (B -> [R] C) -> [R] C
```

Where `A` is the operation argument type, `B -> [R] C` is the continuation `k`,
and `[R] C` is the type of the entire handle expression. The handler arm either
applies `k` to a value of type `B` to resume, or discards `k` and evaluates
directly to a value of type `C` to abort.

### Handle Typing

A `handle` expression eliminates one effect from the row:

```
handle : [E | R] B -> Handler E R C -> [R] C
```

Where `Handler E R C` is the record of obligations derived mechanically from
the declaration of `E`. Each `handle` peels exactly one effect from the row.
Handling all effects in a row produces a pure value of type `C`.
