<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Dependent types](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [x] General and mutual recursion
- [x] [Very dependent types](http://www.nuprl.org/documents/Hickey/FormalObjectsinTypeTheory.pdf)
- [x] Errors with source positions
- [x] Interpreter
- [ ] WebAssembly generation
- [ ] Errors with human-readable types and terms
- [ ] Module system
- [ ] Data types
- [ ] Library for state and side-effects
- [ ] System input and output
- [ ] Irrelevant arguments
- [ ] Implicit arguments
- [ ] Dependency manager

### How do I run this thing?

`stack run ~/example.crs example` will typecheck the `~/example.crs` file. The `example` argument is optional, and if supplied, will print the declaration and definition of said name;

### Primitives
- `Boolean`: (lambda-encoded) `true` and `false`;
- `Text`: `~~` (length), `++` (concatenate);
- `Integer`: `+`, `-`, `*`, `/`, `=`, `<`, `<=`, `>`, `>=`;
- `Real`: `+.`, `-.`, `*.`, `/.`, `=.`, `<.`, `<=.`, `>.`, `>=.`;

### Example source

```
let the(A: Type, a: A): A =
  a
end

let Unit: Type =
  -> self {
    P: -> { Unit, Type },
    P(unit),
    P(self)
  }
end

let unit: Unit =
  fn { P, p_unit, p_unit }
end

let Natural: Type =
  -> self {
    P: -> { Natural, Type },
    P(zero),
    -> { n: Natural, P(succ(n)) },
    P(self)
  }
end

let zero: Natural =
  fn { P, p_zero, p_succ, p_zero }
end

let succ(n: Natural): Natural =
  fn { P, p_zero, p_succ, p_succ(n) }
end

let Pair(A: Type, B: Type): Type =
  -> self {
    P: -> { Pair(A, B), Type },
    -> { a: A, b: B, P(pair(A, B, a, b)) },
    P(self)
  }
end

let pair(A: Type, B: Type, a: A, b: B): Pair(A, B) =
  fn { P, p_pair, p_pair(a, b) }
end

let List(A: Type): Type =
  -> self {
    P: -> { List(A), Type },
    P(empty(A)),
    -> { a: A, rest: List(A), P(push(A, a, rest)) },
    P(self)
  }
end

let empty(A: Type): List(A) =
  fn { P, p_empty, p_push, p_empty }
end

let push(A: Type, a: A, rest: List(A)): List(A) =
  fn { P, p_empty, p_push, p_push(a, rest) }
end

let Stream(A: Type): Type =
  -> self {
    P: -> { Stream(A), Type },
    -> { a: A, stream: Stream(A), P(next(A, a, stream)) },
    P(self)
  }
end

let next(A: Type, a: A, stream: Stream(A)): Stream(A) =
  fn { P, p_next, p_next(a, stream) }
end

let take(A: Type, quantity: Natural, stream: Stream(A)): List(A) =
  quantity(
    fn { _, List(A) },
    empty(A),
    fn { n,
      stream(
        fn { _, List(A) },
        fn { a, rest, push(A, a, take(A, n, rest)) }
      )
    }
  )
end

let twelve: Integer =
  +(4, -(16, 8))
end

let pi: Real =
  /.(22.0, 7.0)
end

let hello_world_length: Integer =
  ~~("Hello world!")
end

let stream_of_ones: Stream(Integer) =
  next(Integer, 1, stream_of_ones)
end

let list_of_ones: List(Integer) =
  take(Integer, succ(succ(zero)), stream_of_ones)
end

let Vector(size: Natural, A: Type): Type =
  -> self {
    P: -> { size: Natural, Vector(size, A), Type },
    P(zero, vempty(A)),
    -> { size: Natural, a: A, rest: Vector(size, A), P(succ(size), vpush(size, A, a, rest)) },
    P(size, self)
  }
end

let vempty(A: Type): Vector(zero, A) =
  fn { P, p_empty, p_push, p_empty }
end

let vpush(size: Natural, A: Type, a: A, rest: Vector(size, A)): Vector(succ(size), A) =
  fn { P, p_empty, p_push, p_push(size, a, rest) }
end
```
