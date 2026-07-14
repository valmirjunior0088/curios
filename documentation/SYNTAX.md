# SYNTAX.md

A pragmatic reference for the Curios surface language (`.crs`). It covers every construct with examples, organized for lookup rather than cover-to-cover reading.

`curios-text/src/parse.rs` is the **source of truth**. Where this document and the parser disagree, the parser wins — and this file should be fixed. The standard library under `curios-text/std/` and `curios-text/syn/` is the best corpus of idiomatic usage.

## Lexical structure

Comments run from `--` to end of line; there are no block comments. Whitespace is insignificant except that it separates tokens and disambiguates a few operators (see [Operators](#operators)).

Identifiers are runs of alphanumeric characters and `_`. The reserved keywords are `let`, `match`, `rec`, `mod`, `use`, `pub`, `end`, `false`, `true`, `induct`, `struct`, `record` (plus the contextual `and`, and the sort words `Type` and `Prop`). A keyword may not appear as a path segment.

**Paths.** Names are segments joined by `/`. A leading `/` makes the path absolute (rooted at the module tree); otherwise it is relative to the current scope. Examples: `Nat` (relative), `Option/none` (member of `Option`), `/std/Lst` (absolute), `/sys/Io` (the primitives module). `_` is a valid identifier character, so `to_str` and `is_none` are single segments.

```
-- this is a comment
use /std/{Nat}; -- absolute path
Option/some(x) -- qualified member
```

## Literals

| Kind               | Examples                                | Notes                                                                                                                                                                                                                  |
| ------------------ | --------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Integer            | `0`, `42`, `0xFF`, `0b1010`, `-7`, `+3` | Decimal, hex (`0x`), or binary (`0b`). The concrete type (`Nat`/`Int`/`Flt`) is chosen by elaboration; a written sign rules out `Nat`. A sign must be glued to the digits — `-42` is a literal, `- 42` is subtraction. |
| Float              | `5.0`, `-0.5`, `1.0e9`                  | Must contain a dot **and** a following decimal digit. `5.0` is a `Flt`, not `5` projected. Supports sign and `e`/`E` exponent.                                                                                         |
| Char               | `'c'`, `'\n'`, `'\''`                   | A fixed `Nat` codepoint (monomorphic). Escapes: `\n \t \r \\ \'`.                                                                                                                                                      |
| String             | `"hi"`, `"a\tb\n"`                      | A `Str`. Escapes: `\n \t \r \\ \"`.                                                                                                                                                                                    |
| Bytestring (`Bin`) | `\48\69`, `\\`, `\01\..rest`            | Each byte is `\` followed by exactly two hex digits. The empty bytestring is `\\`. A `\..` segment splices another `Bin` in place — any position, any count. The literal is one whitespace-free token: a spread operand is an atomic term in glued form (a name path with glued projections/calls/`!` — `\..hdr.bytes`, `\..f(x)`) or a parenthesized term `\..(term)`, and the literal continues only when the very next character is `\`, so `\..xs \01` ends at `xs`. |
| List (`Lst`)       | `[]`, `[1, 2, 3]`, `[1, ..xs, 4]`       | The native contiguous sequence. Element-type-checked: it borrows `Lst(T)`'s `T` from the expected type, and a non-empty literal can also synthesize `T` from its elements — but a bare empty `[]` with no expected type cannot, so annotate it (`[] : Lst(Nat)`). A `..xs` entry splices another list in place — any position, any count, full terms (brackets delimit, so `[.. xs]` may be spaced); the borrow covers spread operands and elements alike. |
| Boolean            | `true`, `false`                         | Keywords.                                                                                                                                                                                                              |

## Types and sorts

`Type` and `Prop` are the two sorts (`Prop` for proof-irrelevant propositions). A goal `?` stands for a term the compiler should determine and *report*: the module still elaborates fully, then compilation fails with the goal's local scope (`name : type` per binder), its type (`? : T`), and the solution unification committed (`? = t`, absent when nothing determined it) — a development probe, never a shippable construct. `_` is the wildcard binder and the nat-switch default.

**Function types (Π).** `(p : A, q : B) -> R`. Parameters may be unlabeled (`(A) -> B`) and may be marked implicit with `@` (`(@A : Type, x : A) -> A`). Dependent: later parameters and the result may mention earlier labels.

**Tuple types (Σ).** `{x : A, y : B}`, with later fields able to depend on earlier ones. Fields may be unlabeled (`{A, B}`). The empty tuple type `{}` is the unit type. A function-typed field may use the signature sugar `name(params) -> T` (shorthand for `name : (params) -> T`), and the last field may carry a trailing comma — both also apply to `struct`/`record` declarations, which share this field grammar.

```
(@A : Type, x : A, y : A) -> Eq(x, y) -- dependent Π with an implicit parameter
{secs : Nat, nanos : Nat} -- Σ / anonymous record type
{len(s : Str) -> Nat} -- signature sugar: len : (s : Str) -> Nat
{} -- unit type
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
let x = compute(a); -- type inferred
let y : Nat = 0; -- type annotated
let f(n : Nat) -> Nat = -- function-definition sugar (local; type optional)
    n + 1;
let p = pair; -- single binder; destructure with projections (p.0, p.1)
let (x, y) = pair; -- tuple pattern; sugar for the two `let`s above
let Point { x, y } = p; -- struct pattern (field-punned)
body_using_the_bindings
```

**`rec`.** Locally-scoped recursive definitions; types are required. Mutually-recursive groups are joined with `and`:

```
rec go(rest : Lst(A), acc : Nat) -> Nat =
    match rest
    | [] => acc
    | [_, ..tail] => go(tail, acc + 1)
    end;
go(l, 0)
```

**Postfix `!` (do-notation).** A trailing `!` on an atomic term extracts the result of a monadic action: `e!` hoists `e` to the top of its enclosing *region* and sequences it through the `Monad` concept (each site desugars to `/syn/Monad/bind(e, continuation)`, and the action's type resolves the witness — see [Concepts](#concepts-witnesses-and-instance-arguments)). Every value body is a region; a lambda body, match arm, or `rec` item starts a fresh one, and a `let` tail continues the region after the binder. No header or `end` is needed; a `!` in a type is an error, and `!` is not parsed inside `!=`.

```
let parser : Parse(Nat) =
    let a = Parse/any_byte!; -- Monad(Parse) resolved from the action's type
    let b = Parse/any_byte!;
    Parse/pure(a + b);
```

The standard library ships `Monad` witnesses for `Option`, `Lst`, `Task`, and `Parse`; declaring `satisfy Monad(M) { … }` makes `!` work with your own type. Inside monad-generic code a local `use Monad(M)` binder resolves the sites, so generic do-notation works too.

## Operators

Binary operators. Listed loosest to tightest binding; **all are left-associative** and **require surrounding whitespace** (`a + b`, not `a+b` — the space is also what separates the operator `-` from a glued literal sign).

| Precedence   | Operators                   |
| ------------ | --------------------------- |
| 1 (loosest)  | `\|\|`                      |
| 2            | `&&`                        |
| 3            | `==` `!=` `<` `>` `<=` `>=` |
| 4            | `+` `-`                     |
| 5 (tightest) | `*` `/` `%`                 |

Every operator, `&&`/`||` included, dispatches through a standard-library concept (see [Concepts](#concepts-witnesses-and-instance-arguments)): `+ - * / %` through `Add`/`Sub`/`Mul`/`Div`/`Rem`, `&&`/`||` through `And`/`Or`, `==`/`!=` through `Eql` (`!=` negates the result), and the comparisons through `Cmp`. Both operands must share one type; a bare integer literal defaults it to `Nat` (`Int` if signed). Primitive witnesses cover `Nat`/`Int`/`Flt` (plus `Eql`/`And`/`Or` on `Bln`, `Eql` on `Bin` and `Str`), and compile to the bare primitive instruction — declaring a witness (e.g. `satisfy Add(Point) { … }`) makes the operator work on your own type at no cost to the primitive cases.

## Binders

A `let` binder, lambda parameter, or function-definition-sugar parameter is either a single name (`_` to ignore) or a tuple/struct destructuring pattern — `(x, y)`, `Point { x, y }`, `Point { loc = (x, y) }` — nested arbitrarily, and mixed freely with plain names in the same parameter list. A pattern desugars to exactly the hand-written projection chain it stands for (`let (x, y) = pair;` is sugar for `let x = pair.0; let y = pair.1;`); a struct pattern's written head name is descriptive only, never resolved or validated — writing the wrong same-shape struct name is not an error. Field-punning (`Point { x, y }`) is the ordinary positional case, not separate syntax.

A match-arm pattern may nest arbitrarily — see [Match](#match). A constructor's payload position accepts another full pattern (`some(some(x))`), the scrutinee itself may be a tuple or struct matched positionally (including a tuple of several independent scrutinees dispatched by row), and the `Bln`/`Nat`/`Lst`/`Bin` literal shapes can appear as sub-patterns too (`some([head, ..tail])`). A leaf position still binds a single name only (`_` to ignore) — there is no catch-all (`| x =>` / `| _ =>`) at any depth, so every arm names a concrete shape.

## Match

`match <head> [: <motive>] <arms> end`. The optional motive states the result type / return-type family (needed for dependent elimination). There are specialized arm shapes per scrutinee kind.

**Motives** take three forms:

```
match x : Nat ... -- constant motive
match v : (v : Vec(A, k)) => Vec(B, k) ... -- scrutinee-bound motive
match p : (q : Eq(A, s, t)) => Eq(t, s) ... -- annotated (names the type + its slots)
```

**Inductive** (the general form). Each arm is `| pattern => body`, where `pattern` is a constructor applied to (possibly nested) sub-patterns (`tag(x, …)`, `some(some(x))`), a tuple (`(p, q, …)`) or struct (`Name { f, … }`) pattern matched positionally against a tuple/struct-valued scrutinee, or one of the `Bln`/`Nat`/`Lst`/`Bin` literal leaves, nested at any depth. A leaf binds a single name only (`_` to ignore). The concrete arms carry no row priority — each shape is handled by at most one arm — so every combination an arm needs to reach must be spelled out. Zero arms is legal (for scrutinees no constructor can inhabit):

```
match m
| some(a) => f(a)
| none() => default
end
```

**The `_` default.** A single **final, top-level, bare** `| _ =>` arm may follow the concrete constructor arms of an inductive match; it covers every constructor no earlier arm names. Only a bare `_` is a default — a *named* final binder (`| rest =>`) among concrete arms is a mistake, not a catch-all, and is rejected. The default is still forbidden nested inside a payload (that mixes a binder with a concrete shape in one column, which stays an error) and absent from the `Bln` and fold (`Nat`-induction/`Lst`/`Bin`) forms, whose shapes are already exhaustive. A lone `_` with no concrete arms is not a default at all — it is the plain binder form (equivalent to a `let`). The default binds nothing and is checked at the unrefined scrutinee against the motive.

```
match m
| some(a) => f(a)
| _ => default          -- covers none() and any other constructor
end
```

A scrutinee can be a tuple of several independent values, matched row by row — since there is no catch-all row, every reachable combination needs an arm, so this pays off when the code would otherwise nest one match inside another to cover the same combinations. Columns are consumed left to right, rows grouping by each column's shape before the next column is examined — so a row's later column may be a plain binder when its earlier columns already set it apart from every concrete-shaped row (`(none(), _)` below); only a binder meeting a concrete shape in the same column of the same group is an error, which is why the discriminating column has to come first (`(_, none())` alongside a `(some(x), …)` row is rejected):

```
match (a, b)
| (some(x), some(y)) => x + y
| (some(x), none()) => x
| (none(), _) => 0
end
```

A tuple or struct pattern matched against a scrutinee with no constructor tag of its own (a plain `{A, B}`/`Name` value, not an inductive) desugars to plain projection rather than a real dispatch — equivalent to the destructuring `let` forms in [Binders](#binders).

**Boolean.** Exactly the two arms, in either order:

```
match cond | true => a | false => b end
```

**Nat — induction** (zero plus successor with an induction hypothesis):

```
match n : (m) => Lte(m, m)
| 0 => Lte/z()
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

The literal cases may also appear nested inside a constructor payload (`some(5)`); there the enclosing match's top-level `| _ =>` supplies the switch's mandatory default. Whether a `Nat` match is induction or dispatch is decided purely by its arms — a `pred + 1; ih` successor arm makes it induction, literal cases make it dispatch, and the two cannot be mixed in one match.

**`Lst` fold** (empty arm uses the empty-list literal `[]`; the cons arm `[head, ..tail]; ih` mirrors the list literal's own bracket-and-comma shape, peeling the leading element `head` off the rest `tail`, with `ih` the fold of `tail` — a plain case-split may omit `; ih`):

```
match a
| [] => base
| [head, ..tail]; ih => step(head, ih)
end
```

**`Bin` fold** (identical in spirit, but the empty arm is the empty bytestring `\\` and the cons arm `\head\..tail; ih` mirrors the `Bin` literal's own backslash-delimited shape; `head` is the leading byte, a `Nat`):

```
match b
| \\ => base
| \head\..tail; ih => step(head, ih)
end
```

**Headless (condition ladder).** `match` with **no head term** — arms are tried top-to-bottom and the first that fires selects its body, closed by a mandatory `| _ =>` default. Two arm shapes:

- `| cond => body` — a `Bln` **condition**; the arm fires when `cond` is `true`.
- `| pattern = value => body` — a refutable **bind** (Rust `if let`); the arm fires when `value` matches `pattern`, binding its sub-patterns in `body`. The pattern must be refutable — a bare binder (`| x = v =>`) is always-fires, so it is rejected in favor of a `let`.

```
match
| Nat/in_range(c, 0x00, 0x7F) => Class/ascii()
| some(x) = lookup(k)         => use(x)
| _                           => Class/bad()
end
```

The ladder desugars to nested `Bln` matches (for conditions) and single-row inductive matches with the rest-of-ladder as their default (for binds), so an arm's body inherits the definitional refinement of its condition exactly as the hand-nested `match cond | true => … | false => … end` would — the rewrite keys on the canonicalized condition term, so `Eq/refl()` mints propositional evidence inside an arm. The `_` is mandatory because a ladder enumerates no shapes it could exhaust (a *dispatch* form, like the `Nat` switch, not an *elimination*).

## Top-level items

A module is a sequence of items; an entrypoint file is items followed by a final term. Every item except `use`/`mod` headers may be prefixed with `pub` to export it.

**Private items stay out of public interfaces.** A `pub` item's declared signature must only mention items that are themselves publicly reachable: the annotation of a `pub let`/`rec`, a `pub concept`'s parameter and field types (superclasses included), a `pub induct`'s parameter, index, and constructor payload types, and a `pub struct`/`record`'s parameter types (plus field types when the representation is public — a `struct`'s hidden fields are not interface). Violations are a compile error naming both items. Bodies are exempt — a public function may call private helpers freely — which also means a transparent type alias (`pub let X : Type = <private>`) can still leak definitionally; the check covers declared signatures.

**`let` / `rec`.** Top-level definitions. Unlike local `let`, the type is **required** (function sugar or `: T =`). `rec ... and ...` defines mutually-recursive groups.

```
pub let map(@A : Type, @B : Type, f : (A) -> B, m : Option(A)) -> Option(B) =
    match m
    | some(a) => Option/some(f(a))
    | none() => Option/none()
    end;

pub rec len(@A : Type, l : Lst(A)) -> Nat = ...;
```

**`foreign`.** A binding implemented by the embedder rather than by Curios code — the user-facing FFI boundary. The type is a **wire signature**: `(T, ...) -> T` or a bare `T` for a zero-argument foreign (mirroring `sys_io`'s own `io_clock_wall`-style ops), where each `T` is one of the six wire types `Nat`, `Int`, `Bln`, `Bin`, `Io`, `Lst(T)` — not an arbitrary Curios `Type`, since these are exactly the shapes that can cross the host boundary. The name is the guest binding; the wasm import name is the declaration's fully qualified name (e.g. `/foo/frobnicate`), imported under the `ffi` namespace, so declarations in different modules never collide on the wire. An embedder supplies an implementation under that qualified name via `curios-rt::ForeignBindings` (native) or `hooks.foreign` (the JS harness).

```
foreign frobnicate : (Nat, Bin) -> Nat;
pub foreign log : (Bin) -> Nat;
```

**`mod`.** A submodule, either inline (`pub mod Name ... end`) or file-backed (`pub mod Name;`, loaded from `Name.crs`). The `std.crs` / `syn.crs` index files are just lists of file-backed `mod` declarations plus re-exports.

```
pub mod Nat; -- load Nat.crs
pub mod Inner -- inline module
  pub let x : Nat = 0;
end
```

**`use`.** Bring names into scope. A named group `{...}` lists items; `*` is a glob. Within a group, a bare name imports both the module and the value of that name, `mod X` imports only the module, `let Y` imports only the value.

```
use /std/{Bln, Nat}; -- import several names
pub use Option/*; -- re-export everything (e.g. constructors)
pub use Lst/{let Lst}; -- re-export only the value `Lst`
use /syn/Str/{classify, step}; -- import specific members
```

**`induct`.** An inductive type. After the name come optional parameters `(p : T)` (mark with `@` to make them implicit at the type constructor; they are always implicit at value constructors). A **required** `: Sort` declares the result sort — `Type` or `Prop` — written `: Sort` for a plain type or `: (indices) -> Sort` to also declare an index telescope. Each case is `| name(payload)` with an optional `: (targets)` stating its index instantiation — required exactly when the type is indexed. Mutually-recursive families join with `and`; the block ends with `end`.

```
pub induct Option(A : Type) : Type -- parameterized, lands in Type
| some(A)
| none()
end

pub induct Vec(T : Type) : (n : Nat) -> Type -- indexed by a Nat
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
end

pub induct Eq(@A : Type) : (x : A, y : A) -> Prop -- a proposition
| refl(@z : A) : (z, z)
end
```

Payload binders may be named (`x : T`), named-implicit (`@m : T`, in scope for later binders and the target), or bare positional types (`A`).

**`struct` / `record`.** A nominal record type. The keyword sets _representation visibility_: `record` makes the representation **public** (callers construct and project directly); `struct` makes it **private** to the declaring module (construct/project only via exported helpers, else a `PrivateRepresentation` error). Optional parameters and a **required** `: Sort` work as for `induct` (a struct has no indices, so its sort is the bare `: Type` or `: Prop`; a `Prop` struct's fields must all be non-informative). Fields are labeled (`x : A`) or a single unlabeled field forms a newtype.

```
pub record Pair(A : Type, B : Type) : Type { -- transparent
    fst : A,
    snd : B
}

pub record Meters : Type { Nat } -- newtype; project with `.0`; erases to the bare field

pub struct Token : Type { Bin } -- opaque: representation private to this module
```

**Construction and projection.** Build with `Name { field = value, ... }` (or `Name(params) { ... }` when the type takes parameters); read fields with `.label` or `.0`. A function-valued field may use the definition sugar `name(args) = body` (shorthand for `name = (args) => body`), and the last field may carry a trailing comma — both also apply to tuple literals `(a = x, b = y)`.

**Spread/update.** `Name { ..base, field = value, … }` copies `base` — which must itself be a `Name` — replacing the named fields. The spread must be the first entry and at most one is allowed; every override must be labeled, and overrides follow the declared field order (an order-preserving subsequence — written order stays check order, as everywhere). `Name { ..base }` is the identity copy, and the head may re-pin parameters so an update can change them — every copied field is checked against the new instantiation, so overriding a field that a copied field's type depends on is a type error unless the dependent field is overridden consistently too. The sequence literals spread too — `[a, ..xs, b]` and `\00\..bytes\01` (see the literal table) — but there spread means *concatenation*, so it is positional and repeatable rather than single, leading, and labeled. There is no tuple spread, and no string spread — a string literal's UTF-8 validity derivation needs concrete bytes.

```
let p = Pair { fst = 1, snd = 2 };
let sum = p.fst + p.snd;
let fst = p.fst; -- bind a field via projection
let api = Api { base = 3, bump(x) = x + 1 }; -- definition sugar
let q = Pair { ..p, snd = 9 }; -- update: fst copied from p
let r : Pair(Str, Nat) = Pair { ..p, fst = "x" }; -- parameter-changing update
```

## Concepts, witnesses, and instance arguments

Ad-hoc polymorphism is expressed with three constructs. A **concept** is a record-shaped interface; a **witness** is a registered inhabitant of a concept for a given type; a **`use` binder** is a third parameter plicity the elaborator fills by resolution rather than unification. `concept` and `satisfy` are contextual keywords — legal identifiers and path segments everywhere else, recognized only at item start (`concept` optionally after `pub`; a witness is anonymous, so `pub` does not apply).

**`concept`.** A concept lowers to an ordinary `record` (its representation is always public) plus a method wrapper synthesized into the concept's namespace for each non-`use` field. Fields are signatures — `name : T`, or the function sugar `name(params) -> T` (shorthand for `name : (params) -> T`, the same sugar any record field admits). A field prefixed with `use` is a **superclass** edge — anonymous, and given no wrapper: its type must be a concept application, and an instance of the outer concept in scope yields the inner one by resolution. The result sort (`Type` or `Prop`) is required; a `Prop` concept's witnesses erase entirely. Concept and witness field lists admit a trailing comma, like every field list.

A parameter marked with the contextual keyword `out` is an **output position** (a functional dependency): it is excluded from the witness key and pinned by whichever witness the input positions select. At least one parameter must be an input. `out` stays a valid identifier — the marker form needs a binder after it, so a parameter *named* `out` still parses.

Parameters may be higher-kinded: `Monad(M : (Type) -> Type)` keys witnesses on the type *constructor* (`Lst`, `Option`), and the elaborator can infer an unapplied constructor argument from an applied occurrence (`Lst(A)` teaches it `M := Lst`).

```
pub concept Show(A : Type) : Type {
    show(A) -> Str -- signature sugar
}

pub concept Ord(A : Type) : Type {
    use Eql(A), -- superclass: an Ord(A) grants an Eql(A)
    cmp(A, A) -> Order
}

pub concept Convert(A : Type, out B : Type) : Type {
    convert(A) -> B -- B is determined by the witness for A
}

pub concept Monad(M : (Type) -> Type) : Type {
    pure(@A : Type, a : A) -> M(A),
    bind(@A : Type, @B : Type, m : M(A), f : (A) -> M(B)) -> M(B)
}
```

**`satisfy`.** A witness is anonymous — `satisfy (tele)? C(args) { … }` — because nothing ever needs to name one: dispatch reaches it through resolution, an explicit override at a call site takes any concept-typed value, and diagnostics identify it by concept, key, and declaring module. It desugars to a compiler-named top-level definition `let witness#N(tele) -> C(args) = C(args) { … }`; the body is a struct literal, so field coverage and labels are checked for free. Fields are implementations — `name = body`, or the definition sugar `name(args) = body`. The telescope admits only `@` and `use` parameters — an explicit binder is illegal, since nothing supplies it at resolution time.

```
satisfy Show(Nat) {
    show(n) = Nat/to_str(n)
}

-- A premised witness: the `use Show(A)` premise is resolved recursively.
satisfy(@A : Type, use Show(A)) Show(Lst(A)) {
    show(l) = Lst/fold(l, "", (x, acc) => Str/concat(acc, Show/show(x)))
}
```

Every concept–key pair has **at most one** witness, program-wide (global coherence); a duplicate registration is a compile error wherever it is declared — module visibility never scopes the table, only names, and a witness has none. A witness keys on the concept and the tuple of *rigid heads* of the concept's input parameters (each an inductive, a struct/record, or a primitive type constructor); `out` parameters are excluded from the key and pinned by the resolved witness, and everything else is checked by unification at resolution time. For a *second* instance of the same key — a descending order, a case-insensitive equality — declare an ordinary value of the concept type (`let desc : Ord(Nat) = Ord { cmp(a, b) = … };`) and pass it where wanted with `use desc`.

**The orphan rule.** A witness may be declared only where the concept it witnesses, or at least one rigid type in its key, is already declared — never by an unrelated third party. Without this, two independently-developed packages could each legally `satisfy` the same concept+type, a collision that is unfixable once both are linked into one program. The standard library (`/sys`/`/syn`/`/std`) is exempt from this check against itself — the three are one coordinated implementation, not independent packages, so e.g. a `/std`-declared `Eql(Str)` witness bridging `/sys`'s `Eql` concept and `/syn`'s `Str` type is the sanctioned pattern, not an orphan instance. A violation reports as an orphan-witness error naming the concept, the key, and the offending declaration.

**`use` fields in concept literals.** A concept's `use`-marked (superclass) fields leave the positional field sequence in every literal of that concept — witness bodies included — exactly as witness slots leave the argument list at call sites. An omitted `use` field becomes a resolution goal (local binders first, then the table); an explicit fill is the entry form `use <term>`, pairing with the `use`-marked positions in declaration order. A `use` field has no label to assign by — it is either omitted or filled positionally with `use <term>` — and a `use` entry in a non-concept literal is an error. So `satisfy Ord(Nat) { cmp(a, b) = … }` resolves its `Eql(Nat)` superclass from the table, a premised `satisfy(@A : Type, use Ord(A)) Ord(Lst(A)) { … }` resolves its superclass from the premise, and `Ord { use my_eql, cmp(a, b) = … }` overrides explicitly. In a spread literal, `..base` *copies* the superclass fields from the base rather than re-resolving them; an explicit `use <term>` entry after the spread still overrides one.

**`use` binders and arguments.** A `use` parameter is legal anywhere Π binders appear (function/`let`/`rec`/`satisfy` telescopes): `use T`, always anonymous — an instance is reached by resolution, never by name. It is filled by resolution at call sites and joins the instance scope for the body. At a call site, `use <term>` supplies one explicitly, overriding table resolution; it sits alongside `@`-arguments and plain arguments.

```
pub let join(@A : Type, use Show(A), l : Lst(A)) -> Str =
    Lst/fold(l, "", (x, acc) => Str/concat(acc, Show/show(x)));

join([1, 2, 3]) -- resolves the Lst witness over the Nat witness
join(use my_dict, [1, 2, 3]) -- explicit override
```

**Resolution** proceeds deterministically: local `use` binders innermost-first; then superclass projections of local binders, breadth-first by depth (two matches at the same minimal depth are ambiguous); then the global table, keyed by the concept and the input parameters' rigid heads. A goal with an unsolved metavariable in any input position waits until it is solved. The standard library provides `Show`, `Eql` (value-level equality — distinct from propositional `Eq`), `Ord`, `Monad`, and the operator concepts `Add`/`Sub`/`Mul`/`Div`/`Rem`/`Cmp` (see [Operators](#operators)), with witnesses for the primitive types. `Monad` itself is homed in `/syn` — it is what the postfix `!` desugars to — and `/std/Monad` is the user-facing facade; each monad's witness lives beside its type (`/std/Option`, `/std/Lst`, `/std/Task`, `/std/Parse`).

## Proofs (Eq idioms)

Propositional equality `Eq` is an ordinary indexed inductive in `curios-text/std/Eq.crs`; proofs are built and eliminated with `match` like any other inductive. The common combinators — `refl`, `sym`, `trans`, `cong`, `subst` — live there and are the idiomatic building blocks:

```
pub let sym(@A : Type, @x : A, @y : A, p : Eq(x, y)) -> Eq(y, x) =
    match p : (q : Eq(A, s, t)) => Eq(t, s)
    | refl(z) => Eq/refl()
    end;
```
