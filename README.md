<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Dependent types](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [x] General and mutual recursion
- [x] [Self types](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf)
- [x] Errors with source positions
- [x] WebAssembly generation
- [ ] Type erasure

### System dependencies

1) For the interpreter:

    - [`stack`](https://www.haskellstack.org/).

2) For the compiler:

    - [`stack`](https://www.haskellstack.org/);
    - [`clang`](https://clang.llvm.org/);
    - [`lld`](https://lld.llvm.org/).

### How do I run this thing?

Make sure the program has a `main` definition. Then, inside the `runtime` folder:

1) Running `make interpret` will:

    1) Typecheck and interpret the `program.crs` file;

    2) Print a representation of the resulting term.

2) Running `make compile` will:

    1) Typecheck and compile the the `program.crs` file;

    2) Open a server in `localhost:8080` to serve the resulting WebAssembly binary.

### Example source

```
defn int32_sum (left : Int32) (right : Int32) : Int32 {
  #[int32_sum left right]
}

defn Unit : Type {
  self @> (P : Unit -> Type)
    -> P unit
    -> P self
}

defn unit : Unit {
  data P => case_unit => case_unit
}

defn Nat : Type {
  self @> (P : Nat -> Type)
    -> P zero
    -> ((n : Nat) -> P (succ n))
    -> P self
}

defn zero : Nat {
  data P => case_zero => case_succ => case_zero
}

defn succ (n : Nat) : Nat {
  data P => case_zero => case_succ => case_succ n
}

defn List (A : Type) : Type {
  self @> (P : List A -> Type)
    -> P (nil A)
    -> ((value : A) -> (rest : List A) -> P (cons A value rest))
    -> P self
}

defn nil (A : Type) : List A {
  data P => case_nil => case_cons => case_nil
}

defn cons (A : Type) (value : A) (rest : List A) : List A {
  data P => case_nil => case_cons => case_cons value rest
}

defn Stream (A : Type) : Type {
  self @> (P : Stream A -> Type)
    -> ((value : A) -> (rest : Unit -> Stream A) -> P (next A value rest))
    -> P self
}

defn next (A : Type) (value : A) (rest : Unit -> Stream A) : Stream A {
  data P => case_next => case_next value rest
}

defn take (A : Type) (quantity : Nat) (stream : Stream A) : List A {
  (case quantity) (_ => List A)
    (nil A)
    (n => (case stream) (_ => List A)
      (value => rest =>
        cons A value (take A n (rest unit))
      )
    )
}

defn int32_stream (initial : Int32) : Stream Int32 {
  next Int32 initial (unit => int32_stream #[int32_sum initial 1])
}

defn foldr
  (A : Type)
  (B : Type)
  (go : A -> B -> B)
  (initial : B)
  (list : List A)
:
  B
{
  (case list) (_ => B)
    (initial)
    (value => rest =>
      go value (foldr A B go initial rest)
    )
}

defn int32_list : List Int32 {
  take Int32 (succ (succ (succ (succ zero)))) (int32_stream 127)
}

defn main : Int32 {
  foldr Int32 Int32 int32_sum 0 int32_list
}
```
