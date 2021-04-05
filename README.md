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
defn the (A : Type) (a : A) : A {
  a
}

defn Unit : Type {
  (self | P : Unit -> Type)
    -> P unit
    -> P self
}

defn unit : Unit {
  P => p_unit => p_unit
}

defn Natural : Type {
  (self | P : Natural -> Type)
    -> P zero
    -> ((n : Natural) -> P (succ n))
    -> P self
}

defn zero : Natural {
  P => p_zero => p_succ => p_zero
}

defn succ (n : Natural) : Natural {
  P => p_zero => p_succ => p_succ n
}

defn Pair (A : Type) (B : Type) : Type {
  (self | P : Pair A B -> Type)
    -> ((a : A) -> (b : B) -> P (pair A B a b))
    -> P self
}

defn pair (A : Type) (B : Type) (a : A) (b : B) : Pair A B {
  P => p_pair => p_pair a b
}

defn List (A : Type) : Type {
  (self | P : List A -> Type)
    -> P (empty A)
    -> ((a : A) -> (rest : List A) -> P (push A a rest))
    -> P self
}

defn empty (A : Type) : List A {
  P => p_empty => p_push => p_empty
}

defn push (A : Type) (a : A) (rest : List A) : List A {
  P => p_empty => p_push => p_push a rest
}

defn map (A : Type) (B : Type) (transform : A -> B) (list : List A) : List B {
  list
    (_ => List B)
    (empty B)
    (a => rest => push B (transform a) (map A B transform rest))
}

defn Stream (A : Type) : Type {
  (self | P : Stream A -> Type)
    -> ((a : A) -> (rest : Stream A) -> P (next A a rest))
    -> P self
}

defn next (A : Type) (a : A) (rest : Stream A) : Stream A {
  P => p_next => p_next a rest
}

defn take (A : Type) (quantity : Natural) (stream : Stream A) : List A {
  quantity
    (_ => List A)
    (empty A)
    (n => stream (_ => List A) (a => rest => push A a (take A n rest)))
}

defn Vector (size : Natural) (A : Type) : Type {
  (self | P : (size : Natural) -> Vector size A -> Type)
    -> P zero (vempty A)
    -> (
      (size : Natural)
        -> (a : A)
        -> (rest : Vector size A)
        -> P (succ size) (vpush size A a rest)
    )
    -> P size self
}

defn vempty (A : Type) : Vector zero A {
  P => p_vempty => p_vpush => p_vempty
}

defn vpush (size : Natural) (A : Type) (a : A) (rest : Vector size A) : Vector (succ size) A {
  P => p_vempty => p_vpush => p_vpush size a rest
}

defn vhead (size : Natural) (A : Type) (vector : Vector (succ size) A) : A {
  vector
    (size => vector => size (_ => Type) Unit (_ => A))
    unit
    (size => a => rest => a)
}

defn vtail (size : Natural) (A : Type) (vector : Vector (succ size) A) : Vector size A {
  vector
    (size => vector => size (_ => Type) Unit (n => Vector n A))
    unit
    (size => a => rest => rest)
}

defn twelve : Integer {
  + 4 (- 16 8)
}

defn pi : Real {
  /' 22.0 7.0
}

defn hello_world_length : Integer {
  ~~ "Hello world!"
}

defn stream_of_ones : Stream Integer {
  next Integer 1 stream_of_ones
}

defn list_of_ones : List Integer {
  take Integer (succ (succ zero)) stream_of_ones
}

defn list_of_naturals : List Natural {
  push Natural (succ (succ zero)) (push Natural (succ zero) (empty Natural))
}

defn natural_to_text (natural : Natural) : Text {
  natural
    (_ => Text)
    "Z"
    (n => ++ "S" (natural_to_text n))
}

defn list_of_texts : List Text {
  map Natural Text natural_to_text list_of_naturals
}

defn list_to_text_aux (list : List Text) : Text {
  list
    (_ => Text)
    " ]"
    (text => rest => ++ (++ " " text) (list_to_text_aux rest))
}

defn list_to_text (list : List Text) : Text {
  ++ "[" (list_to_text_aux list)
}

defn example : Text {
  list_to_text list_of_texts
}
```
