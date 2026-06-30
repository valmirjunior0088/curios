# SYNTAX.md

A pragmatic reference for the curios surface language (`.crs`). It covers every construct with examples, organized for lookup rather than cover-to-cover reading.

`curios/src/text/parse.rs` is the **source of truth**. Where this document and the parser disagree, the parser wins — and this file should be fixed. The standard library under `curios/std/` and `curios/syn/` is the best corpus of idiomatic usage.

## Lexical structure

Comments run from `--` to end of line; there are no block comments. Whitespace is insignificant except that it separates tokens and disambiguates a few operators (see [Operators](#operators)).

Identifiers are runs of alphanumeric characters and `_`. The reserved keywords are `let`, `match`, `rec`, `mod`, `use`, `pub`, `end`, `false`, `true`, `induct`, `struct`, `record` (plus the contextual `and`, and the sort words `Type` and `Prop`). A keyword may not appear as a path segment.

**Paths.** Names are segments joined by `/`. A leading `/` makes the path absolute (rooted at the module tree); otherwise it is relative to the current scope. Examples: `Nat` (relative), `Option/none` (member of `Option`), `/std/Lst` (absolute), `/sys/Io` (the primitives module). `_` is a valid identifier character, so `to_str` and `is_none` are single segments.

```
-- this is a comment
use /std/{Nat};        -- absolute path
Option/some(x)         -- qualified member
```

## Literals

| Kind               | Examples                                | Notes                                                                                                                                                                                                                  |
| ------------------ | --------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Integer            | `0`, `42`, `0xFF`, `0b1010`, `-7`, `+3` | Decimal, hex (`0x`), or binary (`0b`). The concrete type (`Nat`/`Int`/`Flt`) is chosen by elaboration; a written sign rules out `Nat`. A sign must be glued to the digits — `-42` is a literal, `- 42` is subtraction. |
| Float              | `5.0`, `-0.5`, `1.0e9`                  | Must contain a dot **and** a following decimal digit. `5.0` is a `Flt`, not `5` projected. Supports sign and `e`/`E` exponent.                                                                                         |
| Char               | `'c'`, `'\n'`, `'\''`                   | A fixed `Nat` codepoint (monomorphic). Escapes: `\n \t \r \\ \'`.                                                                                                                                                      |
| String             | `"hi"`, `"a\tb\n"`                      | A `Str`. Escapes: `\n \t \r \\ \"`.                                                                                                                                                                                    |
| Bytestring (`Bin`) | `\48\69`, `\\`                          | Each byte is `\` followed by exactly two hex digits. The empty bytestring is `\\`.                                                                                                                                     |
| List               | `[]`, `[1, 2, 3]`                       | Sugar producing a `Lst` (a linked cons-spine).                                                                                                                                                                         |
| Array (`Arr`)      | `[\|\|]`, `[\|1, 2, 3\|]`               | The native contiguous sequence. Element-type-checked: it borrows `Arr(T)`'s `T` from the expected type, and a non-empty literal can also synthesize `T` from its elements — but a bare empty `[\|\|]` with no expected type cannot, so annotate it (`[\|\|] : Arr(Nat)`). |
| Boolean            | `true`, `false`                         | Keywords.                                                                                                                                                                                                              |

## Types and sorts

`Type` and `Prop` are the two sorts (`Prop` for proof-irrelevant propositions). A hole `?` stands for a term to be inferred (a fresh metavariable); `_` is the wildcard binder and the nat-switch default.

**Function types (Π).** `(p : A, q : B) -> R`. Parameters may be unlabeled (`(A) -> B`) and may be marked implicit with `@` (`(@A : Type, x : A) -> A`). Dependent: later parameters and the result may mention earlier labels.

**Tuple types (Σ).** `{ x : A, y : B }`, with later fields able to depend on earlier ones. Fields may be unlabeled (`{ A, B }`). The empty tuple type `{}` is the unit type.

```
(@A : Type, x : A, y : A) -> Eq(x, y)   -- dependent Π with an implicit parameter
{ secs : Nat, nanos : Nat }             -- Σ / anonymous record type
{}                                       -- unit type
```

## Expressions

**Names and application.** `f(a, b)`; pass an implicit argument explicitly with `@`: `f(@A, x)`.

**Projection.** `.0` (positional) or `.label` (named): `p.fst`, `t.0`.

**Lambdas.** `(x, y) => body`. Each parameter is a single binder name (`_` to ignore) with an optional `: T` annotation; `(x)` is sugar for `(x : _)`.

```
(x) => x
(f, m) => Option/map(f, m)
(x : Nat) => x + 1
```

**`let`.** Binds a value for the rest of the term (everything after the `;`):

```
let x = compute(a);          -- type inferred
let y : Nat = 0;             -- type annotated
let f(n : Nat) -> Nat =      -- function-definition sugar (local; type optional)
    n + 1;
let p = pair;                -- single binder; destructure with projections (p.0, p.1)
body_using_the_bindings
```

**`rec`.** Locally-scoped recursive definitions; types are required. Mutually-recursive groups are joined with `and`:

```
rec go(rest : Lst(A), acc : Nat) -> Nat =
    match rest
    | nil()        => acc
    | cons(_, tail) => go(tail, acc + 1)
    end;
go(l, 0)
```

**`let !` (do-notation).** `let ! = <bind>;` introduces monadic binding for the rest of the block, where `<bind>` is an atomic term denoting a binary bind (`(M A, (A) -> M B) -> M B`). Each subsequent `e!` is desugared through it. There is no `end` — the block ends with the enclosing term.

```
let ! = Parse/bind;
let c = take_char()!;        -- `!` sequences through the chosen bind
pure(c)
```

**Postfix `!`.** Outside the `let ! =` header, a trailing `!` on an atomic term is the bang operator (`x!`). It is not parsed inside `!=`.

## Operators

Binary operators, overloaded across numeric types. Listed loosest to tightest binding; **all are left-associative** and **require surrounding whitespace** (`a + b`, not `a+b` — the space is also what separates the operator `-` from a glued literal sign).

| Precedence   | Operators                   |
| ------------ | --------------------------- |
| 1 (loosest)  | `\|\|`                      |
| 2            | `&&`                        |
| 3            | `==` `!=` `<` `>` `<=` `>=` |
| 4            | `+` `-`                     |
| 5 (tightest) | `*` `/` `%`                 |

## Binders

Every binder — `let`, lambda parameter, function parameter, and constructor-arm payload — is a single name (`_` to ignore). There are no compound binding patterns: destructure a tuple or struct with projections (`p.0`, `p.label`).

Match arms dispatch on one constructor each: `| tag(x, …) =>` binds the payload by name (see [Match](#match)). There are no nested, literal, or catch-all patterns in arms.

## Match

`match <head> [: <motive>] <arms> end`. The optional motive states the result type / return-type family (needed for dependent elimination). There are specialized arm shapes per scrutinee kind.

**Motives** take three forms:

```
match x : Nat ...                          -- constant motive
match v : (v : Vec(A, k)) => Vec(B, k) ... -- scrutinee-bound motive
match p : (q : Eq(A, s, t)) => Eq(t, s) ...-- annotated (names the type + its slots)
```

**Inductive** (the general form). Each arm is `| tag(x, …) => body` — one constructor, binding its payload by name (`_` to ignore). There are no catch-all (`| x =>` / `| _ =>`), nested, or literal patterns, and each constructor is handled by at most one arm. Zero arms is legal (for scrutinees no constructor can inhabit):

```
match m
| some(a) => f(a)
| none()  => default
end
```

**Boolean.** Exactly the two arms, in either order:

```
match cond | true => a | false => b end
```

**Nat — induction** (zero plus successor with an induction hypothesis):

```
match n : (m) => Lte(m, m)
| 0           => Lte/z()
| pred + 1; ih => Lte/s(ih)
end
```

The `;` separates the scrutinee's shape (`pred + 1`) from the induction hypothesis `ih` (the recursive result on `pred`).

**Nat — switch/dispatch** (literal cases plus a mandatory `_` default):

```
match d
| 0 => a
| 1 => b
| _ => fallback
end
```

**`Arr` fold** (empty arm uses the empty-array literal `[||]`; the cons arm `head, ..tail; ih` peels the leading element `head` off the rest `tail`, with `ih` the fold of `tail`):

```
match a
| [||]              => base
| head, ..tail; ih  => step(head, ih)
end
```

**`Bin` fold** (identical, but the empty arm is the empty bytestring `\\` and `head` is the leading byte, a `Nat`):

```
match b
| \\                => base
| head, ..tail; ih  => step(head, ih)
end
```

## Top-level items

A module is a sequence of items; an entrypoint file is items followed by a final term. Every item except `use`/`mod` headers may be prefixed with `pub` to export it.

**`let` / `rec`.** Top-level definitions. Unlike local `let`, the type is **required** (function sugar or `: T =`). `rec ... and ...` defines mutually-recursive groups.

```
pub let map(@A : Type, @B : Type, f : (A) -> B, m : Option(A)) -> Option(B) =
    match m
    | some(a) => Option/some(f(a))
    | none()  => Option/none()
    end;

pub rec len(@A : Type, l : Lst(A)) -> Nat = ...;
```

**`mod`.** A submodule, either inline (`pub mod Name ... end`) or file-backed (`pub mod Name;`, loaded from `Name.crs`). The `std.crs` / `syn.crs` index files are just lists of file-backed `mod` declarations plus re-exports.

```
pub mod Nat;                 -- load Nat.crs
pub mod Inner                -- inline module
  pub let x : Nat = 0;
end
```

**`use`.** Bring names into scope. A named group `{...}` lists items; `*` is a glob. Within a group, a bare name imports both the module and the value of that name, `mod X` imports only the module, `let Y` imports only the value.

```
use /std/{Bln, Nat};             -- import several names
pub use Option/*;                -- re-export everything (e.g. constructors)
pub use Arr/{let Arr};           -- re-export only the value `Arr`
use /syn/Lst/{nil, cons};        -- import specific members
```

**`induct`.** An inductive type. After the name come optional parameters `(p : T)` (mark with `@` to make them implicit at the type constructor; they are always implicit at value constructors). A **required** `: Sort` declares the result sort — `Type` or `Prop` — written `: Sort` for a plain type or `: (indices) -> Sort` to also declare an index telescope. Each case is `| name(payload)` with an optional `: (targets)` stating its index instantiation — required exactly when the type is indexed. Mutually-recursive families join with `and`; the block ends with `end`.

```
pub induct Option(A : Type) : Type   -- parameterized, lands in Type
| some(A)
| none()
end

pub induct Vec(T : Type) : (n : Nat) -> Type -- indexed by a Nat
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
end

pub induct Eq(@A : Type) : (x : A, y : A) -> Prop  -- a proposition
| refl(@z : A) : (z, z)
end
```

Payload binders may be named (`x : T`), named-implicit (`@m : T`, in scope for later binders and the target), or bare positional types (`A`).

**`struct` / `record`.** A nominal record type. The keyword sets _representation visibility_: `record` makes the representation **public** (callers construct and project directly); `struct` makes it **private** to the declaring module (construct/project only via exported helpers, else a `PrivateRepresentation` error). Optional parameters and a **required** `: Sort` work as for `induct` (a struct has no indices, so its sort is the bare `: Type` or `: Prop`; a `Prop` struct's fields must all be non-informative). Fields are labeled (`x : A`) or a single unlabeled field forms a newtype.

```
pub record Pair(A : Type, B : Type) : Type {   -- transparent
    fst : A,
    snd : B
}

pub record Meters : Type { Nat }   -- newtype; project with `.0`; erases to the bare field

pub struct Token : Type { Bin }    -- opaque: representation private to this module
```

**Construction and projection.** Build with `Name { field = value, ... }` (or `Name(params) { ... }` when the type takes parameters); read fields with `.label` or `.0`.

```
let p = Pair { fst = 1, snd = 2 };
let sum = p.fst + p.snd;
let fst = p.fst;                 -- bind a field via projection
```

## Proofs (Eq idioms)

Propositional equality `Eq` is an ordinary indexed inductive in `curios/std/Eq.crs`; proofs are built and eliminated with `match` like any other inductive. The common combinators — `refl`, `sym`, `trans`, `cong`, `subst` — live there and are the idiomatic building blocks:

```
pub let sym(@A : Type, @x : A, @y : A, p : Eq(x, y)) -> Eq(y, x) =
    match p : (q : Eq(A, s, t)) => Eq(t, s)
    | refl(z) => Eq/refl()
    end;
```
