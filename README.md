<p align="center">
  <img width="350" height="183" src="https://github.com/valmirjunior0088/curios/raw/master/logo.png">
</p>

### Checklist

- [x] [Dependent types](https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/henk.pdf)
- [x] General and mutual recursion
- [x] [Self types](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf)
- [x] Errors with source positions
- [x] WebAssembly generation

### How do I run this thing?

Inside the `runtime` folder, running `make` will:

1) Typecheck and compile the the `program.crs` file;

2) Open a server in `localhost:8080` to serve the resulting WebAssembly binary.

### Example source

```
defn the (A : Type) (a : A) : A {
  a
}

defn Unit : Type {
  self @> (P : Unit -> Type)
    -> P unit
    -> P self
}

defn unit : Unit {
  data P => p_unit => p_unit
}

defn Nat : Type {
  self @> (P : Nat -> Type)
    -> (P zero)
    -> ((n : Nat) -> P (succ n))
    -> P self
}

defn zero : Nat {
  data P => p_zero => p_succ => p_zero
}

defn succ (n : Nat) : Nat {
  data P => p_zero => p_succ => p_succ n
}

defn Bool : Type {
  self @> (P : Bool -> Type)
    -> P true
    -> P false
    -> P self
}

defn true : Bool {
  data P => p_true => p_false => p_true
}

defn false : Bool {
  data P => p_true => p_false => p_false
}

defn is_zero (n : Nat) : Bool {
  (case n) (_ => Bool) (true) (_ => false)
}

defn int32_sum (one : Int32) (other : Int32) : Int32 {
  #[int32_sum one other]
}
```
