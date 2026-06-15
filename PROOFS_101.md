# Proofs 101

This document assumes you have read `CRASH_COURSE.md`. It teaches one new skill — proving — using zero new language features. Every mechanism here (unions, indices, `match`, implicit binders) is one you already know; what changes is what you point them at. Snippets assume `use /std/{Nat, Str, Bln, Eq, Lst, Vec, Io};`.

## A proof is a test that checks every input

In Rust, you gain confidence that `add(n, 0) == n` by testing it:

```rust
#[test]
fn add_zero() {
    assert_eq!(add(7, 0), 7); // checks one n per run
}
```

In Curios, you can state it as a type and prove it:

```
let add_zero(n : Nat) -> Eq(Nat/add(n, 0), n) = …;
```

If this definition type-checks, the equation holds for **every** `n` — the check happens at compile time, once. And because proofs are erased along with all other type-level data, `add_zero` costs nothing at runtime. A test samples; a proof quantifies.

The rest of this document builds up to that definition and past it.

## Propositions are types

The trick underneath everything: a proposition is encoded as a _type_, and proving it means _writing a value of that type_. The type checker is the proof checker.

The trivially true proposition is the empty tuple — `()` proves it:

```
let trivially_true : {} = ();
```

The false proposition is a type with no values — a `union` with zero cases:

```
union Void
end
```

Nothing constructs a `Void`, so merely holding one is a contradiction. Eliminating it is a `match` with zero arms: there are no constructors, so every arm is vacuously covered and the match checks at _any_ motive — from the absurd, anything follows:

```
let absurd(@A : Type, contradiction : Void) -> A =
    match contradiction : A
    end;
```

Negation is a function into `Void` — "a proof of `P` would be absurd":

```
let Not(P : Type) -> Type = P -> Void;
```

All three ship in the standard library as `/std/Void` (`Void`, `Void/absurd`, `Void/Not`); this document declares them inline so every snippet stands on its own.

## Equality is an indexed union

`/std/Eq` is the workhorse proposition. It is an ordinary indexed union, two pages after `CRASH_COURSE.md` taught you `Vec`:

```
union Eq(@A : Type) : (x : A, y : A)
| refl(@z : A) : (z, z)
end
```

Read it as: `Eq(x, y)` is the proposition "`x` equals `y`", and its only constructor lives at indices `(z, z)` — both sides the same. So a proof exists exactly when the checker can see the sides are equal. The parameter is marked `@`, making it implicit at the type too — recoverable from the indices, written `Eq(@Nat, x, y)` only when you want it pinned. The payload is implicit as well (`@z`), pinned by the type you check against:

```
let two_is_two : Eq(2, 2) = Eq/refl();
```

`Eq(2, 3)` rejects `Eq/refl()` with a `TypeMismatch`: the constructor demands both indices be the same `z`, and `2` and `3` are not.

Matching is where proofs pay rent. As with any indexed union, **matching refines the indices inside the arm**: scrutinizing a `p : Eq(x, y)` makes `x` and `y` the same thing in the `refl` arm, so obligations that were stuck now reduce. Symmetry is the whole technique in four lines:

```
let sym(@A : Type, @x : A, @y : A, p : Eq(x, y)) -> Eq(y, x) =
    match p : (q : Eq(A, s, t)) => Eq(t, s)
    | refl(z) => Eq/refl()
    end;
```

(One discipline to note: the motive's _type-pattern_ `Eq(A, s, t)` spells every slot, parameters included, even though use sites elide the implicit `A` — the pattern is the eliminator's positional contract, not an application.) Inside the arm both `s` and `t` are `z`, so the demanded `Eq(t, s)` is `Eq(z, z)` — which `Eq/refl()` proves. `/std/Eq` ships the basic kit, all built the same way:

| Name    | Statement                                          |
| ------- | -------------------------------------------------- |
| `sym`   | `Eq(x, y) -> Eq(y, x)`                             |
| `trans` | `Eq(x, y) -> Eq(y, z) -> Eq(x, z)`                 |
| `cong`  | `Eq(x, y) -> Eq(f(x), f(y))` for any `f`           |
| `subst` | `Eq(x, y) -> P(x) -> P(y)` for any `P : A -> Type` |

```
let flipped : Eq(2, 2) = Eq/sym(two_is_two);
let chained : Eq(2, 2) = Eq/trans(two_is_two, flipped);
```

## Induction is `match` with `ih`

`CRASH_COURSE.md` introduced `| pred + 1, ih` as "the result already computed for the predecessor". When the result is a _proof_, `ih` is literally the **induction hypothesis**. Here is the promised theorem:

```
let succ_f(n : Nat) -> Nat = Nat/succ(n);

let add_zero(n : Nat) -> Eq(Nat/add(n, 0), n) =
    match n : (m) => Eq(Nat/add(m, 0), m)
    | 0 => Eq/refl()
    | pred + 1, ih => Eq/cong(succ_f, ih)
    end;
```

Walking through it:

- The **motive** `(m) => Eq(Nat/add(m, 0), m)` states what is being proven at each value — it is the induction _statement_.
- The **base case** must prove `Eq(Nat/add(0, 0), 0)`. `Nat/add(0, 0)` reduces to `0` during checking, so `Eq/refl()` closes it.
- The **inductive step** holds `ih : Eq(Nat/add(pred, 0), pred)` and must prove the statement at `pred + 1`. The checker knows `Nat/add(Nat/succ(pred), 0)` is `Nat/succ(Nat/add(pred, 0))` definitionally, so applying `cong` with `succ_f` to `ih` lands exactly there.

No induction keyword, no tactics — the same `match` you compute with, aimed at a proposition.

## Proving things false

To prove a negation, accept the impossible proof and derive `Void`. The first instinct — eliminate it like `Void`, with a zero-arm match, since no constructor fits the indices — does not get past the checker:

```
let zero_is_not_one(p : Eq(0, 1)) -> Void =
    match p : Void
    end;
-- rejected: missing arm 'refl': its index target is not provably
-- impossible at the scrutinee's indices — write the arm
```

Arm omission is verified by the index inverter, and the inverter is deliberately small: each constructor binder may constrain _one_ index position. `refl`'s `z` pins both positions at once, which is beyond it, so the arm stays mandatory. (Contrast `Vec`, where omitting `nil` at length `Nat/succ(n)` works — there `0` clashes with a constructor form in a single position.)

The standard route is to _discriminate and transport_: write a predicate that distinguishes the two sides, then carry a trivial proof across the equality with `subst`:

```
let IsZero(n : Nat) -> Type =
    match n : Type
    | 0 => {}
    | pred + 1, _ => Void
    end;

let zero_is_not_one : Not(Eq(0, 1)) =
    (p) => Eq/subst(IsZero, p, ());
```

`IsZero(0)` is `{}`, so `()` proves it; `subst` rewrites along `p : Eq(0, 1)` to produce `IsZero(1)` — which is `Void`. The supposed proof of `0 = 1` has been converted into the absurd, which is exactly what `Not` asked for. Note the proof of a negation is just a lambda.

## Payoff: proofs that move data

So far the proofs proved things _about_ programs. They also work _inside_ programs: `subst` can re-type real data along an equality, replacing what Rust would handle with a runtime check or an `unsafe` transmute.

```
let StrVec(k : Nat) -> Type = Vec(Str, k);

let cast(@n : Nat, @m : Nat, p : Eq(n, m), v : Vec(Str, n)) -> Vec(Str, m) =
    Eq/subst(StrVec, p, v);
```

`cast` changes a vector's _type_ — its length index — without touching the value, and only when handed evidence the lengths agree. Combined with `add_zero`, it dissolves the kind of index bookkeeping that piles up around length-indexed structures:

```
let single : Vec(Str, 1) = Vec/cons("hi", Vec/nil());
let recast : Vec(Str, Nat/add(1, 0)) = cast(Eq/sym(add_zero(1)), single);
```

`Vec(Str, Nat/add(1, 0))` and `Vec(Str, 1)` are the same length, but the checker wants the _types_ to convert — `sym(add_zero(1))` is the evidence, and it erases: at runtime `recast` **is** `single`.

## Payoff: sortedness as a precondition

Rust's `binary_search` documents its precondition — _"if the slice is not sorted, the returned result is unspecified and meaningless"_ — in a doc comment, because the type system cannot see it. In Curios the invariant is a type, assembled from two moves already on the table: a comparison turned into a proposition (the `IsZero` trick aimed at `Nat/lte`) and a recursive type-level function:

```
let Lte(a : Nat, b : Nat) -> Type =
    match Nat/lte(a, b) : Type
    | true => {}
    | false => Void
    end;

rec IsSorted(l : Lst(Nat)) -> Type =
    match l : Type
    | nil() => {}
    | cons(x, rest) =>
        match rest : Type
        | nil() => {}
        | cons(y, _) => { Lte(x, y), IsSorted(rest) }
        end
    end;
```

Read `IsSorted(l)` as "every adjacent pair is in order": the empty and one-element lists are trivially sorted (`{}`), and a longer list demands a tuple — head ≤ next, and the rest sorted. Because both functions _compute_, the proposition at a concrete list reduces all the way down to nested units, and the proof writes itself:

```
let one_two_three : Lst(Nat) = Lst/cons(1, Lst/cons(2, Lst/cons(3, Lst/nil())));
let sorted_proof : IsSorted(one_two_three) = ((), ((), ()));
```

Claim the same about an unsorted list and the first tuple slot demands `Lte(3, 1)` — which reduces to `Void`, and nothing fills it:

```
let three_one : Lst(Nat) = Lst/cons(3, Lst/cons(1, Lst/nil()));
let unsorted_proof : IsSorted(three_one) = ((), ());
-- rejected: the first field's expected type is Lte(3, 1), which is Void
```

Now the precondition moves into a signature. `search` walks a sorted list and gives up as soon as it passes where the target would be — a shortcut that is only correct _because_ the list is sorted, so the function demands the evidence. Its recursive call needs the tail's invariant, which a two-line lemma extracts (matching on `rest` lets `p`'s type reduce in each arm, where its shape is known):

```
let tail_sorted(@x : Nat, @rest : Lst(Nat), p : IsSorted(Lst/cons(x, rest))) -> IsSorted(rest) =
    match rest : (r) => IsSorted(r)
    | nil() => ()
    | cons(y, tail) => p.1
    end;

rec search(target : Nat, l : Lst(Nat), sorted : IsSorted(l)) -> Bln =
    match l : Bln
    | nil() => false
    | cons(x, rest) =>
        match Nat/eql(x, target) : Bln
        | true => true
        | false =>
            match Nat/gt(x, target) : Bln
            | true => false
            | false => search(target, rest, tail_sorted(sorted))
            end
        end
    end;

let found : Bln = search(2, one_two_three, sorted_proof);
```

Calling `search` on `three_one` hits the same wall as `unsorted_proof`: there is no third argument to give. The "unspecified and meaningless" footnote became a compile-time rejection — and, as always, `sorted` erases, so the compiled `search` is just the loop.

Every snippet in this document is compiled, run, and its three rejections asserted by `examples/crs_proofs.rs`.
