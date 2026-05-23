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
let double : Nat -> Nat = n =>
    Nat.mul n 2;
```

`n => body` is a lambda — the same as `|n| body` in Rust. Multi-argument functions are curried: `A -> B -> C` means `A -> (B -> C)`, and `f a b` applies `f` to `a` first and then the result to `b`.

```
let add : Nat -> Nat -> Nat = a => b =>
    Nat.add a b;
```

Recursive functions use `rec` instead of `let`:

```
rec fact : Nat -> Nat = n =>
    Nat.fold n : _ => Nat;
    | 0 => 1;
    | pred ih => Nat.mul (Nat.add pred 1) ih;
```

`Nat.fold` is one of the two ways to recurse in Curios. It is structural recursion over a natural number: `| 0 =>` handles the base case; `| pred ih =>` receives the predecessor and the result already computed for it (`ih`, short for induction hypothesis). The other way is `rec`, which introduces a named binding that can call itself freely — used when the recursion is not over a natural number, or when the function is mutually recursive.

The primitive types map to Rust as follows:

| Curios | Rust equivalent |
|--------|-----------------|
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
let x_coord : (_ : { Nat, Nat }) -> Nat = p =>
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
let opposite : (_ : '[north, south]) -> '[north, south] = d =>
    match d : _ => '[north, south];
    | 'north => 'south;
    | 'south => 'north;;
```

Each branch ends with `;`. The motive (`_ => '[north, south]`) names the scrutinee and gives the return type — when the return type is constant, the name is `_`.

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
let area : (_ : Shape) -> Flt = s =>
    match s.0 : _ => Flt;
    | 'circle    => Flt.mul s.1 s.1;
    | 'rectangle => Flt.mul s.1.0 s.1.1;
```

In the `'circle` branch `s.1 : Flt`; in the `'rectangle` branch `s.1 : { Flt, Flt }`. No downcasting, no `unwrap`.

## Dependent function types

A function type `(label : A) -> B` binds `label` so it can appear in `B`. This means the return type can be a function of the argument's value — not just its type.

The simplest example is the identity function, which works for any type:

```
let id : (T : Type) -> T -> T = T => x => x;
```

In Rust this would be `fn id<T>(x: T) -> T { x }`. In Curios, `T` is an ordinary argument; you call `id Nat 42` or `id Bin "hello"`. There are no angle brackets.

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

Rust's `Vec<T>` does not track length in the type. `[T; N]` does, but `N` must be a compile-time constant — you cannot abstract over it at runtime. In Curios, length indexing falls out of `Nat.fold`:

```
rec Vec : (_ : Type) -> (_ : Nat) -> Type = T => n =>
    Nat.fold n : _ => Type;
    | 0 => '[nil];
    | pred ih => { T, ih };
```

`Nat.fold` here builds a type: for length 0 the type is the single-atom sentinel `'[nil]`; for length `n+1` the type is a pair of an element and the type for `n` (bound to `ih`). The concrete expansions:

```
-- Vec Nat 0  =  '[nil]
-- Vec Nat 1  =  { Nat, '[nil] }
-- Vec Nat 2  =  { Nat, { Nat, '[nil] } }
```

Values follow the same structure:

```
let nil  : Vec Nat 0 = 'nil;
let one  : Vec Nat 1 = (42, 'nil);
let two  : Vec Nat 2 = (1, (2, 'nil));
```

`head` is only defined for non-empty vectors. Passing an empty vector is a type error:

```
let head : (T : Type) -> (n : Nat) -> (_ : Vec T (Nat.add n 1)) -> T = T => n => v =>
    v.0;
```

`Vec T (Nat.add n 1)` expands to `{ T, Vec T n }`, so `v.0 : T` and `v.1 : Vec T n`. There is no runtime check and no `Option` in the return type — the length guarantee is structural.
