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

### How do I run this thing?

Inside the `runtime` folder, running `make` will:

1) Typecheck and compile the the `program.crs` file;

2) Open a server in `localhost:8080` to serve the resulting WebAssembly binary.

### Example source

```
defn int32_sum (left : Int32) (right : Int32) : Int32 {
  #[int32_sum left right]
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
  cons Int32 1 (cons Int32 2 (cons Int32 3 (nil Int32)))
}

defn main : Int32 {
  foldr Int32 Int32 int32_sum 0 int32_list
}
```
