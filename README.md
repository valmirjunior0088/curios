<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Dependent types](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [x] General and mutual recursion
- [x] [Very dependent types](http://www.nuprl.org/documents/Hickey/FormalObjectsinTypeTheory.pdf)
- [ ] Better error messages (1/2)
  - [x] Report on source positions
  - [ ] Pretty-print terms
- [ ] Interpreter
- [ ] WebAssembly generation
- [ ] Irrelevant arguments
- [ ] System input and output
- [ ] Module system
- [ ] Dependency manager

### How do I run this thing?

- `stack run print "(-> A: Type, a: A, A)"` will print the internal representation of the type of the identity function;
- `stack run check ~/example.crs example` will typecheck the `~/example.crs` file. The `example` argument is optional, and if supplied, will print the type and body of said definition;

### Example source

```
let identity: (-> A: Type, a: A, A) =
  (fn A, a, a)
end

let Boolean: Type =
  (-> self |
    P: (-> Boolean, Type),
    (P true),
    (P false),
    (P self)
  )
end

let true: Boolean =
  (fn P, p_true, p_false, p_true)
end

let false: Boolean =
  (fn P, p_true, p_false, p_false)
end

let Natural: Type =
  (-> self |
    P: (-> Natural, Type),
    (P zero),
    (-> natural: Natural, (P (successor natural))),
    (P self)
  )
end

let zero: Natural =
  (fn P, p_zero, p_successor, p_zero)
end

let successor: (-> Natural, Natural) =
  (fn natural,
    (fn P, p_zero, p_sucessor, (p_sucessor natural))
  )
end

let List: (-> Type, Type) =
  (fn A,
    (-> self |
      P: (-> (List A), Type),
      (P (empty A)),
      (-> a: A, list: (List A), (P (push A a list))),
      (P self)
    )
  )
end

let empty: (-> A: Type, (List A)) =
  (fn A,
    (fn P, p_empty, p_push, p_empty)
  )
end

let push: (-> A: Type, a: A, (List A), (List A)) =
  (fn A, a, list,
    (fn P, p_empty, p_push, (p_push a list))
  )
end

let Stream: (-> Type, Type) =
  (fn A,
    (-> self |
      P: (-> (Stream A), Type),
      (-> a: A, stream: (Stream A), (P (build A a stream))),
      (P self)
    )
  )
end

let build: (-> A: Type, A, (Stream A), (Stream A)) =
  (fn A, a, stream,
    (fn P, p_build, (p_build a stream))
  )
end

let ones: (Stream Natural) =
  (build Natural (successor zero) ones)
end

let Unit: Type =
  (-> self |
    P: (-> Unit, Type),
    (P unit),
    (P self)
  )
end

let unit: Unit =
  (fn P, p_unit, p_unit)
end

let Pair: (-> Type, Type, Type) =
  (fn A, B,
    (-> self |
      P: (-> (Pair A B), Type),
      (-> a: A, b: B, (P (pair A B a b))),
      (P self)
    )
  )
end

let pair: (-> A: Type, B: Type, A, B, (Pair A B)) =
  (fn A, B, a, b,
    (fn P, p_pair, (p_pair a b))
  )
end
```
