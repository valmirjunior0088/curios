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
effect State {
  get : () -> S;
  put : S -> ();
}
```

Parameterized effects are expressed via type-level lambdas:

```
let State : (S : Type) -> Type = S => effect {
  get : () -> S;
  put : S -> ();
};
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
effect operation and binds the operation's argument. The keyword `resume`
represents the continuation — the rest of the computation after the yield site.

`resume` is second-class: it is an expression that is only complete when applied
to a value, and cannot be stored, returned, or passed as an argument on its own.
This, in effect, enforces that `resume` is single-shot — it can only be invoked
once, at the point where it appears.

To resume the computation, apply `resume` to the value the operation should
return:

```
let run_state = state => computation => handle computation {
  get () => resume state;
  put next_state => run_state next_state (resume ());
  x => (state, x)
};
```

To abort instead of resuming — for exception-like behavior — do not call
`resume` and evaluate to a bare value, which becomes the result of the entire
`handle` expression:

```
handle computation {
  fail msg => Error msg;
}
```

An optional final arm transforms the computation's return value when it
completes without performing any further effects. When omitted, it defaults to
the identity function:

```
let run_state = state => computation =>
  handle computation {
    get () => resume state;
    put next_state => run_state next_state (resume ());
    x => (state, x + 1)
  };
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
[] A ~> A
```

Nested effect types flatten by row union:

```
[T] ([U] A) ~> [T | U] A
```

### Handler Obligations

For every operation declared as `op : A -> B` in an effect `E`, a handler arm
must satisfy:

```
op : A -> [R] C
```

Where `A` is the operation argument type and `[R] C` is the type of the entire
handle expression. Within the arm, the keyword `resume` is bound with type
`B -> [R] C`. The handler arm either applies `resume` to a value of type `B` to
continue the computation, or ignores `resume` and evaluates directly to a value
of type `C` to abort.

### Handle Typing

A `handle` expression eliminates one effect from the row:

```
handle : [E | R] B -> Handler E R C -> [R] C
```

Where `Handler E R C` is the record of obligations derived mechanically from
the declaration of `E`. Each `handle` peels exactly one effect from the row.
Handling all effects in a row produces a pure value of type `C`.
