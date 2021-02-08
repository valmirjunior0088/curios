<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Dependent types](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [x] General and mutual recursion
- [x] [Very dependent types](http://www.nuprl.org/documents/Hickey/FormalObjectsinTypeTheory.pdf)
- [x] Better error messages
- [x] Interpreter
- [ ] WebAssembly generation
- [ ] Irrelevant arguments
- [ ] System input and output
- [ ] Module system
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
    -> { natural: Natural, P(successor(natural)) },
    P(self)
  }
end

let zero: Natural =
  fn { P, p_zero, p_successor, p_zero }
end

let successor(natural: Natural): Natural =
  fn { P, p_zero, p_sucessor, p_sucessor(natural) }
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
    -> { a: A, list: List(A), P(push(A, a, list)) },
    P(self)
  }
end

let empty(A: Type): List(A) =
  fn { P, p_empty, p_push, p_empty }
end

let push(A: Type, a: A, list: List(A)): List(A) =
  fn { P, p_empty, p_push, p_push(a, list) }
end

let Stream(A: Type): Type =
  -> self {
    P: -> { Stream(A), Type },
    -> { a: A, stream: Stream(A), P(next(A, a, stream)) },
    P(self)
  }
end

let next(A: Type, a: A, stream: Stream(A)): Stream(A) =
  fn { P, p_build, p_build(a, stream) }
end

let one: Natural =
  successor(zero)
end

let ones: Stream(Natural) =
  next(Natural, one, ones)
end

let twelve: Integer =
  +(4, -(16, 8))
end

let pi: Real =
  /.(22.0, 7.0)
end
```
