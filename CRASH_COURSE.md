# Crash Course

This document assumes a Rust background. It skips ceremony and goes straight to where Curios and Rust differ.

## Bindings and functions

In Rust, values and functions are declared differently. In Curios, everything is a value bound with `let`. A function is just a value whose type is a function type.

```rust
// Rust
let x: u32 = 42;
fn double(n: u32) -> u32 { n * 2 }
```

```
-- Curios
let x : Nat = 42;
let double(n : Nat) -> Nat =
    Nat.mul(n, 2);
```

`double(n : Nat) -> Nat = …` is function-definition shorthand: it names the parameter and the result type. The underlying value is a lambda, written `n => body` (the same as `|n| body` in Rust), so the shorthand desugars to `let double : Nat -> Nat = n => Nat.mul(n, 2);`.

A function can take several parameters at once. A call passes them in parentheses, comma-separated — `add(2, 3)`:

```
let add(a : Nat, b : Nat) -> Nat =
    Nat.add(a, b);
```

When a function's result is itself a function, calls chain: `f(a)(b)`.

Recursive functions use `rec` instead of `let`; the same `(params) -> R` shorthand works:

```
rec fact(n : Nat) -> Nat =
    match n : Nat;
    | 0 => 1;
    | pred ih => Nat.mul(Nat.add(pred, 1), ih);
```

`match` over a `Nat` is one of the two ways to recurse in Curios. With the `| 0` / `| pred ih` branch shape it is structural recursion: `| 0 =>` handles the base case; `| pred ih =>` receives the predecessor and the result already computed for it (`ih`, short for induction hypothesis). The other way is `rec`, which introduces a named binding that can call itself freely — used when the recursion is not over a natural number, or when the function is mutually recursive.

The primitive types map to Rust as follows:

| Curios | Rust equivalent |
| ------ | --------------- |
| `Bln`  | `bool`          |
| `Nat`  | `u32`           |
| `Int`  | `i32`           |
| `Flt`  | `f32`           |
| `Bin`  | `Vec<u8>`       |

Integer and float literals require an explicit sign: `+42`, `-7`, `+1.5`. Natural number literals are unsigned and have no sign: `42`. String literals (`"hello"`) have type `Bin`.

## Tuples and atoms

Rust has structs and tuples as separate concepts. Curios has one aggregate type — tuple — which optionally names its fields:

```
-- Unnamed (like a Rust tuple)
let point : { Nat, Nat } = (3, 4);

-- Named (like a Rust struct, but names are documentation only)
let point : { x : Nat, y : Nat } = (3, 4);
```

There are no struct declarations. The type expression is the definition. Named and unnamed field types are identical to the type checker; labels exist only for readers.

To extract a field, use dot notation:

```
let x_coord(p : { Nat, Nat }) -> Nat =
    p.0;
```

Atoms are Curios's replacement for fieldless `enum` variants. An atom value is written `'foo`; its type is the set of atoms it can belong to, written `'[foo, bar]`.

```rust
// Rust
enum Direction { North, South }
let d: Direction = Direction::North;
```

```
-- Curios
let d : '[north, south] = 'north;
```

No declaration needed. Matching uses `match`:

```
let opposite(d : '[north, south]) -> '[north, south] =
    match d : '[north, south];
    | 'north => 'south;
    | 'south => 'north;;
```

Each branch ends with `;`. The motive (here just `'[north, south]`) gives the return type; it can optionally name the scrutinee with `label =>` when the return type depends on the scrutinee's value.

## Sum types

Rust encodes sum types with `enum`. Curios has no `enum` keyword. The idiom is a dependent tuple: the type of the second field is a `match` on the value of the first.

```rust
// Rust
enum Shape {
    Circle(f32),
    Rectangle(f32, f32),
}
```

```
-- Curios
let Shape : Type = {
    tag : '[circle, rectangle],
    match tag : _ => Type;
    | 'circle    => Flt;
    | 'rectangle => { Flt, Flt }; };
```

The first field selects the variant; the second field's type is determined by it. Construction is a plain tuple:

```
let c : Shape = ('circle, +3.0);
let r : Shape = ('rectangle, (+2.0, +4.0));
```

Elimination matches on `s.0` to dispatch on the tag, then accesses `s.1` for the payload. Inside each branch the type checker knows the concrete type of `s.1`:

```
let area(s : Shape) -> Flt =
    match s.0 : Flt;
    | 'circle    => Flt.mul(s.1, s.1);
    | 'rectangle => Flt.mul(s.1.0, s.1.1);
```

In the `'circle` branch `s.1 : Flt`; in the `'rectangle` branch `s.1 : { Flt, Flt }`. No downcasting, no `unwrap`.

## Dependent function types

A function type `(label : A) -> B` binds `label` so it can appear in `B`. This means the return type can be a function of the argument's value — not just its type. Multiple parameters are written `(a : A, b : B) -> C`, and later parameters and the result may mention earlier ones.

The simplest example is the identity function, which works for any type:

```
let id(T : Type, x : T) -> T = x;
```

In Rust this would be `fn id<T>(x: T) -> T { x }`. In Curios, `T` is an ordinary argument; you call `id(Nat, 42)` or `id(Bin, "hello")`. There are no angle brackets.

The motive in `match` is the same mechanism: `match head : label => T;` computes the return type from the scrutinee's value. When the return type actually varies per branch, that computation is non-trivial:

```
let default_for : (tag : '[nat, bin]) ->
    match tag : _ => Type;
    | 'nat => Nat;
    | 'bin => Bin; = tag =>
    match tag : _ => match tag : _ => Type; | 'nat => Nat; | 'bin => Bin;;
    | 'nat => 0;
    | 'bin => "";;
```

The caller that passes `'nat` gets back a `Nat`; the caller that passes `'bin` gets back a `Bin`. The type checker tracks this without any enum wrapping on the return side.

## Payoff: length-indexed vectors

Rust's `Vec<T>` does not track length in the type. `[T; N]` does, but `N` must be a compile-time constant — you cannot abstract over it at runtime. In Curios, length indexing falls out of a `match` over the length:

```
rec Vec(T : Type, n : Nat) -> Type =
    match n : Type;
    | 0 => '[nil];
    | pred ih => { T, ih };
```

The `match` here builds a type: for length 0 the type is the single-atom sentinel `'[nil]`; for length `n+1` the type is a pair of an element and the type for `n` (bound to `ih`). The concrete expansions:

```
-- Vec(Nat, 0)  =  '[nil]
-- Vec(Nat, 1)  =  { Nat, '[nil] }
-- Vec(Nat, 2)  =  { Nat, { Nat, '[nil] } }
```

Values follow the same structure:

```
let nil  : Vec(Nat, 0) = 'nil;
let one  : Vec(Nat, 1) = (42, 'nil);
let two  : Vec(Nat, 2) = (1, (2, 'nil));
```

`head` is only defined for non-empty vectors. A non-empty vector is exactly a head element paired with a shorter tail — the tuple `{ T, Vec(T, n) }`, which is `Vec(T, n+1)` unfolded one step — so `head` takes that tuple directly:

```
let head(T : Type, n : Nat, v : { T, Vec(T, n) }) -> T =
    v.0;
```

Since `Vec(T, 2)` reduces to `{ T, Vec(T, 1) }`, a length-2 vector is accepted where `{ T, Vec(T, n) }` is expected (with `n = 1`); inside, `v.0 : T` and `v.1 : Vec(T, n)`. An empty vector has type `'[nil]`, not a pair, so passing one is a type error. There is no runtime check and no `Option` in the return type — the length guarantee is structural.

## Payoff: typed format strings

Length-indexed vectors rule out bounds errors. The same mechanism rules out variadic argument mismatches. The standard library module `fmt` exports `printf`, a function whose argument list is determined by the format string at compile time — not by a macro, but by an ordinary dependent function type.

```
pub mod fmt;
fmt/printf("%s is %d")("Alice")(30)
-- output: Alice is 30
```

`"%s is %d"` calls for a `Bin` argument (for `%s`) followed by a `Nat` argument (for `%d`). The type of `fmt/printf("%s is %d")` is `Bin -> Nat -> Bin` — computed by reducing the format string during type checking, so the format string and each argument are applied in turn. Swapping the types is a compile-time error:

```
pub mod fmt;
fmt/printf("%d")("Alice")
-- TypeMismatch: %d expects Nat, but "Alice" has type Bin
```

The `examples/crs_printf.rs` program runs both cases and asserts the output and the error. `examples/crs_json_codec.rs` shows a larger program combining file-backed modules, dependent sum types, and arrays to encode and decode a `json/Value` tree.
