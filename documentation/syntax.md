# Syntax

This document defines the surface language accepted in `.crs` files. It is a reference for writing and reading Curios programs, not a description of compiler internals. An implementation disagreement is a language conformance bug: either the implementation or this document must be corrected.

A `.crs` file is a sequence of top-level items. An entrypoint closes with one final term, the description the program performs; a module file has no final term and is items alone. Everything below is an item, a term, or the spelling of one of their parts.

Examples use declarations from `/std`, the standard library every program may name. The authored library under `curios-text/std/` is the main corpus of complete programs.

- [Lexical structure](#lexical-structure)
- [Literals](#literals)
- [Sorts and types](#sorts-and-types)
- [Expressions](#expressions)
- [Operators](#operators)
- [Sequencing and effects](#sequencing-and-effects)
- [Pattern matching](#pattern-matching)
- [Guarded ladders (`choose`)](#guarded-ladders-choose)
- [Declarations and modules](#declarations-and-modules)
- [Recursive groups](#recursive-groups)
- [Inductive declarations](#inductive-declarations)
- [Structure declarations](#structure-declarations)
- [Concepts and witnesses](#concepts-and-witnesses)
- [Foreign declarations](#foreign-declarations)
- [Equality and proofs](#equality-and-proofs)
- [Quick reference](#quick-reference)

## Lexical structure

### Whitespace and comments

Spaces, tabs, and newlines separate tokens but otherwise have no meaning. Some operators require surrounding whitespace, as specified in [Operators](#operators).

A line comment begins with `-- ` — the two dashes and a space — and continues to the end of the line; a bare `--` ending its line is an empty comment. There are no block comments, and `--` glued to what follows it is refused rather than read as one, except as the opening of the documentation comment below.

```crs
-- A complete line comment.
let n = 1; -- A trailing comment.
n
```

A documentation comment begins with `--- ` — or is a bare `---`, an empty line of prose — and is syntax rather than a comment: the parser attaches it to what it documents. Consecutive `---` lines form one block, and a block immediately precedes a `let` or an `and` member, an `induct`, a `struct`, a `concept`, a `satisfy`, a `foreign` or a `mod`, or a constructor, a field or a concept method inside one of those; blank lines and plain comments between the block and its declaration are insignificant. The block above a `mod` documents the module it declares, which is where a module's prose lives. A block takes lines of its own, so `---` may not follow code; a block before anything else, a second block before the same declaration, a block before `use` and a block before `test` are each refused, and so is `---` glued to what follows it, `----` included.

```crs
--- Twice the input.
---
--- Never overflows, since `Nat` is unbounded.
pub let double(n: Nat) -> Nat =
    n + n;
```

Every comma-separated list — parameter and argument lists, tuple and struct fields, list literals, import groups — admits one optional trailing comma before its closing delimiter. A comma alone does not form an empty list.

### Identifiers

An identifier is a nonempty sequence of Unicode alphanumeric characters and `_`.

A name beginning with `_` is one the author keeps unused: [`curios lint`](usage.md#lint) never reports an `_`-prefixed binder or declaration, nor anything inside an `_`-prefixed module. `_` alone is a binder that names nothing, and `@_` is its implicit form.

The following words are reserved and cannot be used as path segments:

| Declaration and expression words | Literal words |
| --- | --- |
| `let`, `match`, `choose`, `mod`, `use`, `pub`, `end`, `induct`, `struct`, `foreign` | `true`, `false` |

`concept`, `satisfy`, `and`, and `test` are contextual words. They are recognized only in the grammatical positions that use them and remain valid identifiers and path segments elsewhere. `Type` and `Prop` denote sorts when parsed as terms, but they are not globally forbidden path segments.

### Paths

A path is one or more identifier segments separated by `/`. A leading `/` anchors the path at the compilation root.

A path without that leading `/` is relative to the module it is written in: its head must be a declaration of that module or a name that module imported. Enclosing modules are not searched, so an ancestor's declaration is reached by writing it absolute or by importing it — which is what the report says when one is not found.

```crs
Nat                 -- a declaration or import of this module
Option/some         -- member of Option
/std/List           -- absolute name
/std/Nat/lt         -- absolute name through a nested module
```

The root `/sys` is the compiler's own and a program may not name it: it holds the intrinsic types, the host's operations, and the propositions a decided bound is stated in. Naming it is refused, pointing at the `/std` module that stands in front of it — `Nat` is reached as `/std/Nat`. It is named in this document only where the mechanism behind a form is the point.

That refusal is one case of a general rule: **a unit reaches the prefixes it declared a dependency on, plus `/std`, and no others.** A package declares them in its manifest; a transitive dependency is in the compilation but is not one of them. Every unit is mounted, so an undeclared prefix is still there, and writing one is refused at the reference, naming the prefix and saying it was not declared — a name is never reported unbound because a dependency was missing. The concepts the surface forms desugar into are ordinary `/std` declarations: `+` dispatches through `/std/ops/Add`, a `!` through `/std/Monad`, and a `test` through `/std/Test`.

A path is whitespace-free: every separator touches both of its neighbors. Infix operators are the opposite — they require whitespace on both sides (see [Operators](#operators)) — so `a/b` is only ever the path and `a / b` only ever the division, and the asymmetric spellings `a/ b` and `a /b` satisfy neither grammar.

## Literals

### Numeric literals

Integer literals may be decimal, hexadecimal, or binary. An optional sign must touch the digits.

```crs
0
42
0xFF
0b1010
-7
+3
```

Elaboration chooses `Nat`, `Bool`, `Byte`, `Int`, or `Flt` from context. A *negative* sign excludes `Nat`, `Bool`, and `Byte`; any written sign, `+` as well as `-`, makes the unconstrained default `Int` rather than `Nat`. `Byte` is selected only by an expected `Byte` type and accepts values from `0` through `255`; `Bool` is selected only by an expected `Bool` type and accepts `0` and `1`. An unconstrained unsigned integer defaults to `Nat`.

`-42` is one literal. `- 42` is parsed as an operator occurrence and is not a signed literal. The space is load-bearing.

A floating-point literal has a decimal point followed by at least one decimal digit. It may have a sign and an `e` or `E` exponent.

```crs
5.0
-0.5
1.0e9
```

Floating-point literals have type `Flt`. `5.` is not one, and is refused rather than read as the numeral `5` with a stray dot after it.

The values no decimal spells have literals of their own, each with a required sign: `+inf.0` and `-inf.0` are the infinities, `+nan.0` is the default quiet NaN, and `-nan.0` is the same NaN with its sign set. Without its sign, `inf.0` is field `0` of a binder named `inf`. A decimal too large for `Flt` is refused rather than rounded to an infinity. Every other NaN has no literal, and is built from its bytes with `Flt/of_le_bytes`, which is how a report spells one ([A literal is realized by its expected type](design/surface/a-literal-is-realized-by-its-expected-type.md)).

```crs
+inf.0
-inf.0
+nan.0
```

### Character and string literals

A character literal contains one Unicode scalar value or one supported escape, and is polymorphic exactly as a numeral is: it realizes as the proof-certified `Char` wherever nothing pins it, and as the code point at an expected `Nat`, `Byte`, or `Int` (`Byte` refuses a code point past `255`; `Bool` and `Flt` never realize from a character). `Char` excludes the surrogate range and values above `U+10FFFF`; `Char/to_nat` converts a *value*, whose type is already fixed. In a match, a character literal is a `Nat` dispatch case — see [Natural-number dispatch](#natural-number-dispatch).

```crs
'c'
'\n'
'\''
```

Character escapes are `\n`, `\t`, `\r`, `\\`, `\'`, and `\u{…}` with one to six hexadecimal digits naming a Unicode scalar value — `'\u{301}'` is the combining acute accent, which no keyboard types on its own. A surrogate, a value past `U+10FFFF`, or a malformed brace is a parse error, as is any other unrecognized escape in a character literal.

A string literal has type `Str`.

```crs
"hello"
"first\nsecond"
```

String escapes are `\n`, `\t`, `\r`, `\\`, `\"`, and `\u{…}` as in a character literal. An unrecognized escape in a string literal is not an error: the backslash and the following character both stand for themselves, so `"\%"` is the two-character string `\%`, and so is `"\u"` — only the brace reserves the Unicode form, and a malformed `\u{…}` is a parse error. A string literal is not a format string.

A block string literal spans lines. It opens with `"""` and a newline — blanks between the two are allowed — and closes with a newline, optional whitespace and `"""`; both delimiters take their newline, so the value is exactly the lines between, joined by newlines, with no newline before the first or after the last.

```crs
let page: Str =
    """
    <ul>
        <li>one</li>
    </ul>
    """;
```

The leading whitespace the non-blank lines and the closer's line share is removed from each, so a block reads at the indentation of the code around it and content indented past the closer keeps the difference; a whitespace-only line becomes an empty line and takes no part in that prefix. Trailing whitespace is stripped from each line's final run of text, which is why an escape at the end of a line survives it: `\u{20}` spells a space the stripping would otherwise take. Escapes are the one-line form's, translated after the stripping, and a backslash before a newline joins the two lines. A `"` inside is itself, and three quotes are spelled `\"""`. The two spellings differ in nothing else: the value above is `"<ul>\n    <li>one</li>\n</ul>"`.

A one-line string literal does not span lines: a raw newline inside `"…"` is refused, naming the block form.

`Str` stores certified UTF-8 bytes and is addressed by position: a `/std/Str/At(s)` is a byte offset into `s` at which a character begins, so a piece cut between two positions is whole characters, and stepping from one position to the next reads one Unicode scalar value (`Char`). Its length, folding, and search operations count and visit scalar values, not bytes or grapheme clusters.

### Boolean literals

`true` and `false` have type `Bool`.

### List literals

A list literal constructs `List(T)`. Entries are elements or spreads; a spread inserts every element of another list at its position.

```crs
[]
[1, 2, 3]
[head, ..middle, tail]
```

A nonempty literal may infer `T` from its elements. An empty literal needs an expected list type from its position, such as a binder annotation:

```crs
let empty: List(Nat) = [];
```

Spreads may appear in any position and may be repeated. Every element and spread operand must agree on the same element type.

### Packed literals

Packed literals are bracketed like [list literals](#list-literals) and selected by a grain letter glued to the bracket: `b[…]` builds `Bits`, `x[…]` builds `Bytes`. A bare `[…]` remains `List`.

An entry is a term contributing one atom — a `Bool` in a `Bits` literal, a `Byte` in a `Bytes` literal — or a `..` spread contributing a whole packed value of the same kind. A constant atom is a [numeric literal](#numeric-literals) realized at the grain's element type: `0` or `1` for `Bits`, `0` through `255` in any radix for `Bytes`. A [character literal](#character-and-string-literals) is a constant `Bytes` atom when its code point fits the byte — `x['H', 'i']` — and no character is a bit.

```crs
b[]                -- empty Bits
b[0, 1, 1]
x[]                -- empty Bytes
x[0x48, 0x69]
```

Packed atoms are written least-significant first. The first bit written occupies the least-significant available packed bit.

```crs
b[head, ..tail]
x[0x48, ..suffix, 0x00]
x[..header.bytes]
x[..make_bytes(n)]
x[..prefix, b]
x[pick(flag, a, b)]
```

`b[h, ..t]` is the cons of `h` onto `t`, and `x[..acc, b]` appends `b` to `acc`; neither operation has a separate named form.

Only the grain letter's junction with `[` is tight, which is what keeps `b` and `x` usable as ordinary binders: in `b [1]` the term ends at `b`, and an identifier merely ending in the grain letter never begins a packed literal. Past the `[` the literal lexes like any other bracketed list — whitespace is free, one trailing comma is admitted, and operands are arbitrary terms needing no parentheses. `Bits` and `Bytes` cannot be mixed.

Adjacent constant atoms lower to a single packed constant rather than a chain of appends, so a literal written entirely from numerals is compile-time constant data with no marker needed to say so.

## Sorts and types

### Sorts

`Type` is the sort of computational types. `Prop` is the sort of proof-irrelevant propositions.

Although the surface spelling is always the nullary term `Type`, each occurrence has an implicit level in a cumulative hierarchy. The compiler infers those levels and generalizes reusable declarations over them; there is no syntax for universe variables, levels, or explicit universe arguments. A type accepted at one level is also accepted where a higher level is required, and a value of an `induct` or a `struct` whose level only sizes its parameters is accepted at every such level: a function answering `Result(E, Nat)` is sequenced with `!` in a region that answers `Result(E, Type)`.

All inhabitants of the same proposition are definitionally irrelevant, so a proof does its thinking at compile time and then weighs nothing at runtime. Eliminating a proposition into a computational result is restricted: the proposition must be empty, or have one constructor whose payloads are each non-informative or fixed by the family's indices — which is what lets an `Eq` proof be matched to produce data. Proofs may always be eliminated to prove another proposition. Why the sorts are shaped this way is [`Prop` is strict, proof-irrelevant, and definitionally K](design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md) and [A universe level is implicit, cumulative, and settles by where it came from](design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-came-from.md).

### Function types

A function type is a parenthesized dependent parameter list followed by `->` and its result. The list may be empty: `() -> T` is a nullary function, whose call site writes `f()`.

A parameter is plain, implicit (`@`) or a witness (`use`), and after the mark comes its type. A plain or implicit parameter is written `name: type` or as the type alone. A witness parameter is its type alone, `use Show(A)`: it has no name here or anywhere, since a witness is reached by resolution and never by name.

```crs
(Nat) -> Nat
(x: Nat, y: Nat) -> Nat
(@A: Type, x: A) -> A
(n: Nat, @Holds(0 < n)) -> Nat
(@A: Type, use Show(A), value: A) -> Str
```

Every site that declares a telescope writes its members this way — a function type, a `let` or `satisfy` telescope, a constructor's payload and a declaration's type parameters — and `use _`, which states no type, is refused in each. A definition and a declaration's parameters name their plain members, since what follows is what uses them.

Later parameter types and the result may refer to earlier named parameters.

### Tuple types

A tuple type is a dependent field telescope enclosed in braces.

```crs
{Nat, Bool}
{fst: Nat, snd: Bool}
{value: A, proof: Valid(value)}
{}
```

Later fields may refer to earlier named fields. The empty tuple type `{}` is the unit type.

Labels are part of a tuple type's identity: `{Nat, Bool}`, `{a: Nat, b: Bool}` and `{x: Nat, y: Bool}` are three distinct types, and a value of one is not a value of another. Function-type parameter names carry no such weight; only tuple labels do.

A labeled function field may use signature sugar:

```crs
{run(input: Bytes) -> Async(Nat)}
```

This is equivalent to:

```crs
{run: (input: Bytes) -> Async(Nat)}
```

## Expressions

### Unit and tuples

`()` is the unit value. It is distinct from `{}`, the unit type.

Tuple values use parentheses and comma-separated fields:

```crs
(1, true)
(left = 1, right = true)
()
```

A one-field tuple is written `(x,)`; the trailing comma is what separates it from the parenthesized term `(x)`. A labeled single field needs no comma, since `=` already disambiguates it: `(only = 1)`.

A literal is measured against its expected type's labels position by position. An unlabeled literal takes its labels from a labeled expected type, so `(1, true)` is a `{a: Nat, b: Bool}` where one is expected; a labeled literal is refused where the expected label at that position differs or is absent, and fields are never reordered to match. A literal with no expected type — an unannotated `let`, a projection head — synthesizes the non-dependent product with the labels it wrote: `(a = 1, b = true)` is a `{a: Nat, b: Bool}` and `(1, true)` a `{Nat, Bool}`, which no later annotation can relabel. A labeled tuple is projected by position or by label: `z.0` and `z.a` name the same field.

Labeled fields may use function-definition sugar:

```crs
(base = 3, bump(x) = x + 1)
```

### Names, calls, and projections

A path refers to a value. A call supplies a parenthesized argument list:

```crs
f(x)
Map/lookup(map, key)
```

An argument is written under the mark of the parameter it supplies:

- `value` supplies an explicit parameter;
- `@value` supplies an implicit parameter;
- `use value` supplies a witness parameter explicitly.

The plain arguments are the explicit parameters, in order, and each is written. Between two of them the hidden arguments are written in the order of their parameters, from the first, and the rest may be left out. `@_` and `use _` hold a parameter's place and leave it to the elaborator, which is how a later hidden parameter is written alone.

```crs
f(@Nat, x)
join(@_, use custom_show, values)
```

Against `join(@A: Type, use Show(A), values: List(A))`, each of `join(values)`, `join(@Nat, values)` and `join(@_, use custom_show, values)` is accepted. `join(use custom_show, values)` is refused, since the hidden parameters before `values` open with `@A`, and so is `join(values, use custom_show)`, which writes the witness after the argument it precedes. The same rule matches written members to slots wherever a telescope is filled: a call, a [lambda](#lambdas)'s binders and a [concept literal](#superclass-fields-in-literals)'s entries.

Omitted implicit arguments are inferred, and never picked: where two implicits stand under an operation that commutes — `x * y` against `a * b`, which `x := a` and `x := b` both satisfy — the call is refused as a conversion it cannot decide, naming the implicits never solved, until one is written or the expected type fixes it. One implicit standing as a factor has one solution and is solved to it, however the sums are written: `w * (y + z)` against `x * z + x * y` gives `w := x`. An omitted implicit whose type is a proposition — a bound — is filled where the proposition reduces to `True`, and otherwise proved from the facts in scope where it follows from them by linear arithmetic ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)). Omitted witness arguments are resolved as described in [Witness resolution](#witness-resolution).

A call fills exactly one parameter list — the one its head's type opens with. A function whose result is itself a function is called once per list: `let f(T: Type) -> (Nat) -> Type` is written `f(T)(n)`, and so is an indexed family, `Sized(T)(n)`. A list whose parameters are all hidden is no exception: its call carries its `@` and `use` arguments, or none, ahead of the next list's call — `Eq()(x, y)`, `Eq(@Nat)(x, y)`. Why is [A call fills one parameter group](design/theory/a-call-fills-one-parameter-group.md).

A projection is positional or labeled:

```crs
pair.0
pair.fst
configuration.network.port
```

A position counts the plain fields. A hidden one — a concept's superclass field — takes none, so over a concept value `.0` is its first method.

Calls, projections, and postfix [`!`](#postfix-) may be chained.

### Lambdas

A lambda is a comma-separated parameter list followed by `=>` and a body.

```crs
(x) => x
(x: Nat) => x + 1
(f, value) => f(value)
```

Lambda parameters may be plain binders or irrefutable tuple and struct patterns. An annotation may be written when the parameter type is not supplied by context.

A lambda's parameter list is a dependent telescope, exactly as a function type's is: a later parameter's annotation may name the parameters written before it, including the leaf names bound by an earlier tuple or struct pattern. An earlier parameter shadows a like-named module binding inside a later annotation, just as it does inside the body.

```crs
(s: A, t: A, q: Eq()(s, t)) => proof(q)
((lo, hi), q: Eq()(lo, hi)) => lo
```

A lambda parameter carries the mark of the slot it binds, and after the mark comes a binder: `@name` binds an implicit slot, an unmarked binder an explicit one, and `use _` holds a witness slot's place. A witness has no binder — the body reaches it by resolution — and a lambda never states a witness slot's type: the expected function type states it. A lambda with no expected function type therefore has no `use` member, and a function that must declare one is a `let` with a telescope. After `@` a lambda reads a binder and never a type, so an implicit slot the body does not name is `@_`, or `@_: T` to state its type. The mark applies to the slot the parameter occupies whatever the pattern shape, and each written binder is checked against the plicity of the slot it claims when the lambda is checked against an expected function type.

```crs
(@A, value) => value
(@A, use _, value) => Show/show(value)
```

An omitted implicit or witness binder is inserted from the expected function type, so hidden binders may be left out when the body does not name them. The binders meet their slots as a call's arguments do: a plain binder binds the next explicit slot, the hidden slots before it inserted where they are not written, and the hidden binders written before it bind the hidden slots in order from the first. A plain binder never binds a hidden slot. Against `(@A: Type, use Show(A), value: A) -> Str`, each of `(value) => …`, `(@A, value) => …` and `(@A, use _, value) => …` is accepted; `(use _, value) => …` is not, because the hidden slots open with `@A`, and `(A, show, value) => …` is not, because `A` binds the sole explicit slot and the rest are surplus.

### Local `let`

A local `let` binds a value throughout the term after its terminating `;`.

```crs
let x = compute();
let y: Nat = 0;
x + y
```

Function-definition sugar introduces parameters and an optional result type:

```crs
let increment(n: Nat) -> Nat = n + 1;
increment(4)
```

A `let` or `satisfy` telescope is a signature, and its parameters are written as a [function type](#function-types)'s are, with one difference: a plain parameter is always named, `name: type`, since the body is what uses it. An `@` parameter may be its type alone — `let positive(n: Nat, @Holds(0 < n)) -> Nat = n;` takes the bound as an implicit argument, and the body has it as a fact in scope — and a witness parameter is `use Concept(args)`. The `label(params) = value` sugar inside tuple, struct, and witness bodies binds as a [lambda](#lambdas) does instead: a binder under `@` or no mark, its annotation optional since the field's declared type supplies it, and `use _`.

The binder may be an irrefutable tuple or struct pattern:

```crs
let (x, y) = pair;
let Point { x, y } = point;
x + y
```

A binding is in scope of its own value, so a local function may call itself. A binding that mentions itself states its type, since a body that mentions the binding cannot be the source of it, and is a plain name rather than a pattern; a binding whose value performs `!` cannot mention itself, since the action runs before the binding exists. Bindings that mention one another are declared as one group with `and` — see [Recursive groups](#recursive-groups).

Because a binding is in scope of its own value, `let n: Nat = n + 1;` names the binding it declares rather than an outer `n`, and is refused as the recursive value it is: a value may mention itself only under a lambda, where it is a recursive value computed the first time it is read.

### Irrefutable binder patterns

The binders of `let`, lambdas, function-definition sugar, and the `;` fold-hypothesis position of `Nat`/`List`/`Bits`/`Bytes` match arms accept nested tuple and struct patterns.

```crs
let (x, (_, y)) = value;
let Point { loc = (x, y), color } = point;
body
```

These patterns are projection sugar, not runtime matches. The struct head is documentary and is not resolved or checked. An unlabeled field is matched positionally: its place among the pattern's fields is its place among the plain fields, whose order is part of the type. A `label = pattern` field projects that label and holds the place it is written at, as a labelled entry of a literal does. A hidden field — a structure's `@` field, a concept's superclass field — takes no position and a pattern's field takes no mark, so it is read by its label, written after the fields read by position: over `struct Sized: Type { @len: Nat, items: Vec(Nat)(len) }`, `Sized { items, len = n }` binds both. A pattern over a concept value writes its methods alone. Field punning such as `Point { x, y }` is the positional form, whose sub-patterns happen to be binders named after the fields. Parentheses group a pattern without changing it, so `((x, y))` is `(x, y)`.

Refutable patterns belong only to `match`.

### Written goals

`?` is a development goal. It asks the elaborator to infer as much as possible, then reports the local scope, the expected type and the candidate fits it found, and fails compilation.

```crs
let compose(@A: Type, @B: Type, @C: Type, f: (B) -> C, g: (A) -> B) -> (A) -> C =
    ?;
compose
```

A goal is never accepted in a successfully compiled program.

A definition that holds a goal is read by every other declaration as a name of its type, bound to nothing: the others are still elaborated, so each reports its own goals and refusals in the same run, and a term that would unfold the definition stays stuck on its name. A goal in the definition's own type, or in a type, a concept or a witness, leaves no type to read it by, so what reads that declaration is withheld until the goal is filled.

### Whole-term forms and operand positions

`let`, lambdas, and function types are whole-term forms: a body or tail extends to the end of the enclosing term. There is no expression-level `term: type` ascription; a `:` annotation appears only in binder, signature, and motive positions.

An infix operand is an applied atom: a literal, name, sort (`Type`/`Prop`), tuple, tuple type, structure literal, goal, `match`, `choose`, or parenthesized term, followed by any chain of calls, projections, and postfix `!`. A `match` and a `choose` are atoms because `end` closes them, so nothing after one can be read as part of it. A whole-term form is not an operand; parenthesize it to use it as one.

```crs
1 + match flag | true => 1 | false => 0 end
match chosen | left() => f | right() => g end(x)
1 + (let n = 2; n * n)
```

Positions that accept a full term need no parentheses: call arguments, list elements, field values, match scrutinees, and arm bodies.

## Operators

All infix operators require whitespace on both sides and associate to the left.

| Precedence | Operators | Concept dispatch |
| --- | --- | --- |
| 1, loosest | `\|\|` | `Or` |
| 2 | `&&` | `And` |
| 3 | `==`, `!=`, `<`, `>`, `<=`, `>=` | `Eql`, `Cmp` |
| 4 | `+`, `-` | `Add`, `Sub` |
| 5, tightest | `*`, `/`, `%` | `Mul`, `Div`, `Rem` |

Both operands of an operator have the same type. `==` and `!=` are two separate methods of `Eql`, `eql` and `neq`, so a witness supplies both; `!=` is not a negation applied to `eql`.

An operator's result type is whatever its concept's method declares: `+`, `-`, `*`, `/`, `%`, `&&` and `||` return the operand type, while `==`, `!=`, `<`, `>`, `<=` and `>=` return `Bool`.

`/` and `%` additionally carry the precondition their concept declares. `Div` and `Rem` each have an `Ok(A) -> Prop` field, and the operator inserts an implicit proof of `Ok(divisor)` — so `a / b` on `Nat` must discharge `Holds(0 < b)`: by reduction where `b` is a literal, and from the facts in scope — a hypothesis or a guard that `0 < b` — otherwise ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)). A carrier whose division is total states `True` and pays nothing, which is what keeps `/` a single operator over carriers that disagree about whether it can fail ([A bound is stated in a decided proposition and discharged by reduction](design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)).

Operator notation always uses witness resolution, including intrinsic operands. Standard witnesses cover the intrinsic types, while a `satisfy` declaration enables the same notation for a user-defined type.

## Sequencing and effects

### Postfix `!`

`action!` is monadic sequencing. Each occurrence is equivalent to a call to `/std/Monad/bind(action, continuation)` in the monad of its region.

```crs
use /std/{Nat, Byte, Bytes, Parse};
pub let parser: Parse(Bytes, Nat) =
    let a = Parse/bytes/byte!;
    let b = Parse/bytes/byte!;
    Parse/pure(Byte/to_nat(a) + Byte/to_nat(b));
```

Every value body is a sequencing region. Lambda bodies, match arms, and recursive member bodies begin fresh regions; the tail after a local `let` remains in the same region. There is no `let !` header or matching `end`.

A region's monad is read from the region's type and never inferred from a sequenced action. A region whose type is not yet known waits for it, and one whose type can never name a monad — the body of a lambda in inference position, say — is rejected with a request to annotate the enclosing result type.

An action whose own monad differs from the region's is lifted through the declared `Monad/Lift` witness for that ordered pair, and a pair with no witness is rejected. See [Lifting between monads](#lifting-between-monads).

Postfix `!` is not allowed in types. The token `!=` is an infix operator and is not parsed as postfix `!` followed by `=`.

### Host effects and `Io`

Every operation that touches the host — writing a handle, reading a clock, calling a `foreign` function, exiting — has result type `Io(T)`: a *description* of a computation yielding a `T`, not the `T`. Guest cell and channel operations also return `Io(T)`, since they allocate or observe state within the guest instance. Calling one performs nothing, so `let greeting: Io({}) = print("hello");` has printed nothing.

**There is no operation taking an `Io(T)` to a `T`** ([Effects are descriptions, and the carrier has no eliminator](design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)). A description is performed only by being the program's tail, which the emitted entrypoint forces once. So a function whose result type is not an `Io` cannot perform an effect, and a `!` may only appear in a region whose type is a monad — a `(Str, Bool) -> Bool` has nowhere to sequence one.

`Io/pure` wraps a value as a description performing nothing and `Io/bind` sequences one into another, but postfix `!` reaches `Io` through its `Monad` witness like any other monad. Binding a description does not perform it, and forcing one twice performs it twice:

```crs
use /std/{Io, print};
let once: Io({}) = print("x");
let _ = once!;
once                            -- prints "x" twice in total
```

`Io` is not matchable: it has no constructors to enumerate, so a `match` over one is rejected, whether it writes constructor arms or none at all. A lone `| _ =>` arm is an irrefutable binder match rather than an elimination, and is accepted as the binding it is.

A host operation that can fail is declared `Try(M, E, A)` over its base monad — `Try(Io, Io/Error, File)` for `File/open`, `Try(Async, Io/Error, Socket)` for `tcp/Socket/connect`. Through the edges `/std/Try` declares, a `Try` region sequences a `Try` over its own base or over one that embeds in it, a bare `Result` as early return, and an `Io` or `Async` action wherever its base admits one — which is how an `Io` or `Async` base's own actions are sequenced; an action of any other base is refused. `Try/raise` stops the region, `Try/rescue` handles the stop, and `Try/run` hands the outcome back as an `M(Result(E, A))`.

```crs
use /std/{File, Path, Bytes, Try, Io};
let contents: Try(Io, Io/Error, Bytes) =
    let f = File/open(Path/of_str("notes.txt"), File/Mode/read())!;
    let text = File/read_all(Path/of_str("notes.txt"))!;
    let _ = File/close(f)!;
    Try/pure(text);
```

### Lifting between monads

`/std/Monad/Lift(M, N)` declares the canonical embedding of monad `M` into monad `N`: one method, `Monad/lift`, taking an `M(A)` to an `N(A)`, with `Monad` witnesses for both sides as superclasses — so an embedding between non-monads cannot be declared. Like every witness, one `Monad/Lift` witness may occupy each ordered pair of monads program-wide, so which embedding runs is a fact about the program, never about a call site.

```crs
satisfy Monad/Lift(Io, Async) {
    lift = Async/lift,
}
```

With that witness declared — `/std/Async` declares it — an `Io` action sequences directly inside an `Async` region, and the `!` inserts the lift:

```crs
use /std/{Async, print};
pub let fiber: Async({}) =
    let _ = print("hello\n")!;
    Async/pure(());
```

The explicit spelling `Monad/lift(action)` names the same embedding, with the target monad inferred from the region. A region's tail — the last expression of a value body, a lambda body, or a match arm — is lifted by the same read when its head's declared monad and the region's are both monads and differ; a tail that is no monadic action keeps the ordinary type mismatch. The read is of the action's *head's declaration*, so one whose head is not a declared name — a projection, a call of a lambda — is not embedded on its own: it reports as an action of one monad where another is expected, and `Monad/lift(action)` is the spelling that embeds it.

Embeddings never chain. Declaring `Monad/Lift(Io, Job)` and `Monad/Lift(Job, Sched)` does not let an `Io` action sequence in a `Sched` region: the missing `Monad/Lift(Io, Sched)` is reported, together with any chain of declared embeddings that would have reached it. The composite is declared like any other — a decision about `Sched`, written by its author, not derived by the compiler ([A fallible operation returns `Try`, and `!` lifts along declared edges](design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md)).

## Pattern matching

### Match shell and motives

A headed match has a scrutinee, an optional motive, one `| pattern => body` arm per case, and a closing `end`. An arm may be left out where that constructor's index target is *provably* impossible at the scrutinee's indices — a match over a `Sized(T)(n + 1)` needs no `empty()` arm — and where it is not provable the missing arm is demanded by name. A scrutinee whose type reduces to an inductive with no constructors takes no arms at all: `match contradiction end` is how a proof of an empty type is discharged, with a motive where the result has to be spelled. Where the facts in scope refute each other and no written term says so, `False/refuted()` is the contradiction to match on ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)).

The motive states the result type as a family. It is an ordinary term, checked against the eliminator's motive type — a function of the scrutinee's indices, in declaration order, and then the scrutinee:

```text
(indices) -> Scrutinee(indices) -> Sort
```

There is no motive grammar: what follows `:` is parsed as a term and terminates at the first arm, since `|` is not an infix operator ([A motive is a term, not a grammar](design/theory/a-motive-is-a-term-not-a-grammar.md)).

```crs
match b: (_) => Nat                                -- result ignores the scrutinee
match n: (m) => P(m)                               -- result depends on it
match p: (s, t, q) => Eq()(t, s)                   -- an indexed family
match p: (s: A, t: A, q: Eq()(s, t)) => Eq()(t, s) -- with written annotations
match p: discriminates_eq                          -- a named family
match v                                            -- omitted; inferred
```

The number of binders is fixed by the eliminated type: one per index, then one for the scrutinee. A non-indexed scrutinee — every intrinsic carrier, and any inductive declared without an index telescope — takes exactly one, so a result that ignores it is written `(_) => T`; the `Sized` declared under [Inductive declarations](#inductive-declarations) has one index and takes two binders, and `Eq` has two and takes three.

Parameters are never binders. They are uniform across constructors and fixed by the scrutinee's type, so the motive body reaches them through the ambient scope — exactly as a constructor's case target states only index expressions.

Each arm is checked against the motive at that constructor's target indices, and the match as a whole at the scrutinee's actual indices. A `| _ =>` default binds nothing and refines no index, so it is checked at the actual indices too.

A binder may be written bare, as `_`, or annotated. An annotation is an ordinary type in an ordinary position: checked by conversion against the binder's expected type, obeying the usual plicity rules, and free to name the binders before it. Annotating the scrutinee binder is how a reader recovers the eliminated family on the motive line.

```crs
match p: (s, t, q: Eq()(s, t)) => Eq()(t, s)
```

Omitting the motive asks the elaborator to infer it. In a position with an expected type, the result is that expected type as written, and each arm is checked against it with the scrutinee standing for the arm's case: a variable scrutinee and its variable indices are substituted for, an expression scrutinee's written occurrences are replaced, and inside the arm the scrutinee reduces to the case wherever else it is met — so a hypothesis whose type mentions the scrutinee needs no convoy to ride along. A fold over `Nat`, `List`, `Bits` or `Bytes` whose arm reads its induction hypothesis — names it, or holds a goal `?` that could — is the exception: the hypothesis is typed at the result at the tail, so its motive is abstracted over the scrutinee instead. A case split — the same match with no `; ih` read — is not. Prefer omission wherever inference succeeds. A motive has to be written where there is nothing to infer from — a type-level match whose result appears in a signature, or an elimination in inference position — and where the result must depend on the scrutinee beyond its written occurrences.

A fold's motive (`Nat`, `List`, `Bits`, `Bytes`, with an arm reading its hypothesis) reaches its scrutinee only through the binder it declares: the `; ih` hypothesis is typed at the motive opened at the tail, and a motive that named the scrutinee instead would have that name refined to the arm's own value. `match n: (m) => P(m)` is accepted; `match n: (_) => P(n)` is refused.

A motive may only be written where the head dispatches directly: every arm's top-level pattern must be the same dispatchable shape. A tuple-scrutinee matrix, a struct-headed match, or a plain-binder match builds no core eliminator for the motive to attach to, and rejects one.

### Inductive patterns

An inductive pattern names a constructor and supplies one pattern per plain payload.

```crs
match option
| some(value) => use(value)
| none() => fallback
end
```

A constructor is named bare: the scrutinee's type supplies the namespace, so `Option/some(n)` is refused as a pattern. A pattern is written as the call that builds the value is: the plain payloads each take a pattern, in order, and a payload the constructor declared implicit (`@`) is left out, or written under `@` ahead of the plain payload it precedes, where the arm names it. A payload left out is bound all the same, so a bound it states is a fact in the arm. Rows of one constructor write the same hidden payloads. Witness payloads are not a surface feature, so `use` is not accepted in a constructor pattern.

```crs
match vector
| nil() => fallback
| cons(head, tail) => head
end

match vector
| nil() => 0
| cons(@length, head, tail) => length + 1
end
```

A pattern may nest *inside* a constructor, tuple, or struct field, and parentheses group one as they group an irrefutable pattern. The operands of the `Nat`, `List`, `Bits` and `Bytes` leaves are plain binder names rather than patterns — `[some(x), ..tail]` is not a pattern — while the `; ih` binding takes a full irrefutable pattern.

```crs
match value
| some([head, ..tail]) => consume(head, tail)
| none() => empty
end
```

Concrete constructor rows have no priority. Each reachable combination needed by the program must be represented by a compatible row.

### Dispatch default arm

A final top-level bare `_` may follow any run of concrete *dispatching* arms — inductive constructors, `true`/`false`, `Nat` shapes, `[]`/`[head, ..tail]`, `b[]`/`x[]` — and covers every shape not named earlier. It is not available after tuple or struct arms, which project exhaustively rather than dispatching.

```crs
match option
| some(value) => use(value)
| _ => fallback
end
```

Only a bare `_` in this exact position is a default, and nested wildcard defaults are not accepted. `_` is the only wildcard: a final arm binding a name called `rest` is a binder, and is refused as a catch-all. A lone `_` with no concrete arm is an irrefutable binder match rather than an inductive default.

### Multiple scrutinees

A tuple scrutinee supports matrix matching over several values. Columns are considered left to right, grouping rows by the shape in each column.

```crs
match (left, right)
| (some(x), some(y)) => x + y
| (some(x), none()) => x
| (none(), _) => 0
end
```

A binder may occupy a later column when an earlier column has already distinguished its row. A binder and a concrete shape cannot compete in the same column of the same group.

Tuple and struct patterns over non-inductive values compile to projections rather than constructor dispatch.

### Boolean match

A `Bool` match covers both shapes: one `true` arm and one `false` arm in either order, or one of them followed by the bare `_` default described under [Dispatch default arm](#dispatch-default-arm).

```crs
match condition
| true => yes
| false => no
end
```

### Natural-number induction

Natural-number induction has a zero arm and a successor arm. The successor arm binds the predecessor before `+ 1`; a binding after `;` receives the induction hypothesis for the predecessor, and omitting it makes the arm an ordinary case split. `+ 1` takes whitespace on both sides, like an infix operator: `pred+1` is not a successor pattern.

```crs
match n: (m) => P(m)
| 0 => base
| predecessor + 1; hypothesis => step(predecessor, hypothesis)
end
```

The `;` binding names the fold result rather than part of the scrutinee, so it accepts the same irrefutable tuple and struct patterns a `let` binder does:

```crs
match n
| 0 => (0, true)
| predecessor + 1; (count, live) => (count + 1, live)
end
```

### Natural-number dispatch

A natural-number dispatch has literal arms and a mandatory `_` default.

```crs
match tag
| 0 => first
| 1 => second
| _ => otherwise
end
```

Induction arms and literal-dispatch arms cannot be mixed in one match.

A dispatch literal is a numeric literal or a character literal — the latter matching its scalar value, so `match Char/to_nat(c) | '\n' => … | _ => … end` is how a `Char` is dispatched, with the conversion visible at the head. `Nat` is unbounded; a dispatch key is not. A dispatch literal is written whole, but a key is compiled into 32 bits, and a key past them is refused at its arm rather than changing what the program means.

### List fold and case split

A list match uses `[]` for the empty case and `[head, ..tail]` for the nonempty case. A binding after `;` receives the fold result for `tail` — a plain name or an irrefutable tuple/struct pattern, exactly as in [natural-number induction](#natural-number-induction); omit it for an ordinary case split.

Both cases are required: unlike an inductive family, this carrier has no vacuity inversion to prune one. A trailing `| _ =>` may stand in for the missing one.

```crs
match values
| [] => base
| [head, ..tail]; hypothesis => step(head, hypothesis)
end
```

### Packed folds

`Bits` and `Bytes` arms are a [list fold](#list-fold-and-case-split) under the grain letter that selects the carrier, with every rule of one: both cases required, the `;` binding optional, a trailing `| _ =>` standing in for a missing case. A `Bits` head has type `Bool`; a `Bytes` head has type `Byte`.

```crs
match bits
| b[] => base
| b[head, ..tail]; hypothesis => step(head, hypothesis)
end

match bytes
| x[] => base
| x[head, ..tail] => inspect(head, tail)
end
```

## Guarded ladders (`choose`)

`choose` is an ordered guarded ladder, not a match — it consumes no scrutinee. Arms are tried from top to bottom, and a final `_` arm is mandatory.

```crs
choose
| condition => when_true
| some(value) = lookup(key) => when_found(value)
| _ => fallback
end
```

A condition arm fires when its expression evaluates to `true`. A bind arm evaluates the expression on the right of `=` and fires when it matches the refutable pattern on the left. A bare binder is not allowed as a bind-arm pattern, because it cannot fail: an arm that always fires is a `let`.

Each selected arm receives the same definitional refinement that an equivalent nested headed match would provide.

## Declarations and modules

An entrypoint consists of zero or more top-level items followed by exactly one final term — the description the program performs. That term has type `Io({})`: a program describes doing something and yielding nothing, so a tail that computes a result must discard it explicitly rather than have the result dropped for it. A module file consists of top-level items only and has no final term.

### Top-level definitions

Top-level `let` declarations require a type annotation. Function-definition sugar supplies the annotation as a parameter telescope and result type.

That requirement is also what separates items from the final term: an *unannotated* top-level binding in an entrypoint is not an item at all, but a local `let` opening the final term. The difference is not only scope — an item's value body is its own sequencing region, so a `!` in it sequences within that definition, while a local `let`'s value shares the final term's region and sequences with the rest of the program.

```crs
pub let zero: Nat = 0;

pub let map(@A: Type, @B: Type, value: Option(A), f: (A) -> B) -> Option(B) =
    match value
    | some(x) => Option/some(f(x))
    | none() => Option/none()
    end;
```

A top-level definition is in scope of its own body, and definitions that reference one another are declared as one group with `and` — see [Recursive groups](#recursive-groups).

### Test declarations

A `test` declaration names a check: a description of type `/std/Test`, built from that module's combinators, collected per unit in declaration order and run by `curios test`, which owns [what it reports](usage.md#test).

```crs
use /std/{Nat, Eq, Test};

test the_answer_holds =
    Test/assert(21 * 2 == 42);

let _right_identity(n: Nat) -> Eq()(n + 0, n) =
    Eq/refl();
```

A test takes no parameters. A claim about *every* instantiation is a proposition rather than a description, so it is a `let` whose type states the claim and whose body proves it, checked by the kernel on every build — the second declaration above, its leading `_` marking a declaration that exists for its type rather than its callers ([A test is a declared description, and a proof is a `let`](design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md)).

To check a claim at instances you choose, make it an ordinary definition returning `Test` and schedule a table: `test table = Test/all(List/map(cases, ((a, b)) => claim(a, b)));` is one test over the author's own cases, whose failure names the case's position. A test registers like a private definition of type `() -> Test`: referable within its subtree, and colliding with a sibling of the same name.

`test` is contextual: a keyword only where an item may start, an ordinary name everywhere else. A test is never `pub`, its name being a report line rather than an export, and no documentation comment may precede one. Its body is its own sequencing region typed at `Test`, which is no monad, so a bare `!` is refused where it is written; an effectful test enters `Io` through `Test/perform`'s thunk.

### Modules

A file-backed module ends its declaration with `;` and loads `Name.crs`. An inline module ends with `end`.

```crs
pub mod Nat;

pub mod Internal
    pub let value: Nat = 1;
end
```

A header's file-backed modules live in its **stem directory**. `mod Nat;` written in `foo.crs` loads `foo/Nat.crs`, and `Nat`'s own file-backed modules load from `foo/Nat/`. One rule governs every file in the language, so the file handed to `curios run` is a header like any other: `mod Nat;` in `main.crs` loads `main/Nat.crs`. `main.crs` is not special; it is only the file you pointed at.

A package's library header is the single exception, and it is a fact about package layout rather than about the language — see [What a package is made of](usage.md#what-a-package-is-made-of). A stem is never part of a name: neither `exe` nor `lib` can be written in a path.

### Imports and re-exports

`use` imports through a group: a braced list `path/{…}` or a glob `path/*`. There is no bare `use path;` form — a single import is written `use /std/{Nat};`. The path may be dropped entirely, leaving the root-anchored `use /{Name};`, which is how a nested module reaches a declaration of the compilation root. Prefixing a `use` with `pub` re-exports what it imports.

```crs
use /std/{Nat, Bool};
pub use Option/*;
use /std/Nat/{Lt};
use /{Owner};
```

A `use` takes effect where it is written: the items after it in its module see what it imports, and the items before it do not. The module's own declarations have no such order, and are in scope throughout it.

Inside a group, a bare name imports both a child module and a value with that name when both exist. `mod Name` imports only the module namespace; `let Name` imports only the value namespace.

```crs
pub use List/{let List};
use Package/{mod Syntax, let parse};
```

### Visibility

One rule governs every declaration, in both namespaces:

> A declaration written **without** `pub` in module `M` is visible exactly within `M`'s subtree — `M` itself and its descendants at any depth. A declaration written **with** `pub` is additionally visible wherever `M` itself is visible.

Reachability along a path is the conjunction of that rule at each hop, and the root's subtree is the whole program. So a descendant may name its ancestors' private declarations, while ancestors and siblings may not: `Owner/Worker` can reach a private binding of `Owner` — written absolute or imported, since a relative path never climbs — but neither `Owner` nor a sibling `Owner/Other` can reach a private binding of `Owner/Worker`. Privacy points down the tree, and only down.

`pub` inside a private module means "wherever this module is visible", which is that module's own audience rather than the whole program — the facade pattern, where a public module re-exports selected names out of a private child.

`struct`, `induct`, and `concept` have a second, declaration-local `pub` before their result sort; this independently exposes their representation, under the same subtree rule. A private representation is transparent throughout its declaring module's subtree, so an abstraction can be implemented across several files without exporting how it is built.

Globs are the exception: `use M/*` and `pub use M/*` import the exported surface only, never a subtree-private declaration. Reaching one always requires naming it.

A public interface cannot mention an item its own consumers cannot reach. The interface includes:

- parameters, indices, and result sorts of publicly reachable nominal declarations;
- struct and concept fields when the representation is public;
- inductive constructor signatures when the inductive representation is public;
- declared types of definitions.

The check compares audiences rather than declaration paths, so a name re-exported out of a private child counts as visible wherever the re-export puts it, and an item reaching only a subtree may freely mention other declarations of that subtree. It follows re-exports, identity aliases, and direct-headed type-family aliases whose declared result structurally ends in literal `Type` or `Prop`. Definition bodies are not interface, and neither are members synthesized into a nested namespace — an inductive's constructors, a concept's method wrappers — so a constructor facade may hand out values of a type the consumer cannot name.

## Recursive groups

A declaration is in scope of its own body, so it may recurse with nothing said. Declarations that reference one another are declared as one group with `and`, whose members all register before any body is elaborated; two that reference each other without being grouped are refused, naming both. One rule, five spellings, differing only in what a later member carries and what closes the group:

| Form | A later member is written | The group closes with |
| --- | --- | --- |
| Local `let` | `and name: T = …` | one `;` |
| Top-level `let` | `and`, its own `pub`, then `name: T = …` | one `;` |
| `induct` | `and`, its own markers, then a head and its cases | one `end` |
| `struct`, `concept` | `and`, its own markers, then a whole declaration | the last member's `}` |
| `satisfy` | `and`, then a whole witness | the last member's `}` |

A local group's first member may be a pattern; every later one is a plain name stating its type, since a body mentioning the binding cannot be the source of it.

```crs
let even(n: Nat) -> Bool =
    match n
    | 0 => true
    | p + 1; _ => odd(p)
    end
and odd(n: Nat) -> Bool =
    match n
    | 0 => false
    | p + 1; _ => even(p)
    end;
even(input)
```

Visibility is per member, never per group: a `pub` before `let`, `induct`, `struct` or `concept` covers the first member, and one before `and` covers that member alone. A witness is never `pub`, so neither is a member of a witness group.

## Inductive declarations

An inductive declaration introduces a nominal family and its constructors.

```crs
pub induct Option(A: Type): pub Type
| some(A)
| none()
end
```

Parameters follow the name, each named, and a list holds at least one: a family with none is written with no list, `induct Empty: Type`. A parameter marked `@` is implicit at the type constructor, and a plain or `@` parameter is implicit at every value constructor. A parameter written `use Concept(args)` is a premise the family is declared under, a witness slot of the type constructor and of every value constructor alike — see [A type declared under a premise](#a-type-declared-under-a-premise).

The required result annotation is either a sort or an index telescope followed by a sort:

```crs
pub induct Sized(T: Type): (length: Nat) -> pub Type
| empty(): (0)
| push(@n: Nat, head: T, tail: Sized(T)(n)): (n + 1)
end
```

A family with both is a function of its parameters returning a function of its indices, and is applied the way it is declared: `Sized` has type `(T: Type) -> (length: Nat) -> Type` and is written `Sized(T)(n)`, so `Sized(T)` is itself the family `(length: Nat) -> Type` that a match over it eliminates. A family with only parameters or only indices takes them in one call — `Option(A)`, `Tagged(3, s)` below. Why is [A call fills one parameter group](design/theory/a-call-fills-one-parameter-group.md).

Each index binder may be named or left bare — `(length: Nat)` and `(Nat)` are both well-formed — and an index never takes `@`. The name is never in scope in the constructor cases; it appears in the family's printed signature, and a later entry of the same telescope may depend on it. That dependency is what makes the annotation a telescope rather than a list of types:

```crs
pub induct Tagged: (size: Nat, contents: Sized(Nat)(size)) -> pub Type
| tag(@size: Nat, @contents: Sized(Nat)(size)): (size, contents)
end
```

Each constructor of an indexed family must state the indices it produces after `:`. A non-indexed constructor does not accept a target.

A constructor's payload is a signature: each member is `name: type` or its type alone, plain or `@`, so `at(n: Nat, @Holds(0 < n))` takes its bound as an implicit argument. A payload takes no `use` member, and an index takes no mark.

An inductive proposition is declared with `Prop`:

```crs
pub induct Eq(@A: Type): (left: A, right: A) -> pub Prop
| refl(@value: A): (value, value)
end
```

The outer `pub` exports the family name; the inner exports construction and every form of elimination, and without it constructor access and pattern matching are restricted to the declaring module's subtree.

Mutually recursive inductives are separated by `and` within one block, which a single `end` closes — see [Recursive groups](#recursive-groups).

## Structure declarations

A structure is a nominal dependent record.

```crs
pub struct Pair(A: Type, B: Type): pub Type {
    fst: A,
    snd: B,
}
```

A single unlabeled field defines a newtype-like structure and is projected with `.0`.

```crs
pub struct Meters: pub Type { Nat }
```

The outer `pub` exports the type name; the inner exports construction and projection, and without it those are restricted to the declaring module's subtree. A `Prop` structure may contain only non-informative fields.

Parameters are written as an inductive's are: plain, `@`, or a `use Concept(args)` premise ([A type declared under a premise](#a-type-declared-under-a-premise)).

A field is plain or hidden. A hidden field is written under `@`, as `@label: type` or its type alone, and takes no position among the fields an author writes: a literal leaves it out, a projection `.N` and a pattern's positional field count the plain fields alone, and it is read by its label. A bound declared this way is stated once, on the data it constrains: it is discharged where the value is built, and is a fact wherever the value is in scope.

```crs
pub struct Positive: pub Type { n: Nat, @Holds(0 < n) }
pub struct Sized: pub Type { @len: Nat, items: Vec(Nat)(len) }
```

A field takes no `use` member, a structure's premise being a `use` parameter, and a tuple type's field takes no mark.

Structures whose fields name one another are declared as one group with `and`; a lone structure may name itself in its fields with nothing said. See [Recursive groups](#recursive-groups).

```crs
pub struct Node: pub Type { value: Nat, next: Option(Edge) }
and Edge: pub Type { weight: Nat, to: Node }
```

### Structure literals

A structure value names its type and supplies its fields. A head may be applied before the field block, and an applied head is the type written as it is anywhere else — a call of the type former, taking the marks, the `?` holes and the omitted hidden arguments a call takes. A list with nothing written is that call too, never the bare head: `Box() { … }` leaves `@A` to inference as `Box()` does in a signature, and `Pair() { … }` is refused as `Pair()` is.

```crs
Pair { fst = 1, snd = true }
Pair(Nat, Bool) { fst = 1, snd = true }
Box(@Nat) { value = 1 }
Api { base = 3, bump(x) = x + 1 }
```

Fields are checked in declaration order. Function-definition sugar is equivalent to assigning a lambda.

A hidden field is left out, and filled as a call's omitted `@` argument is: a value by the fields whose types mention it, a bound by reduction or from the facts in scope. Nothing filling it is reported at the literal, by the proposition it states. It may be written under its mark, ahead of the plain field it precedes, as a call's hidden arguments are written from the first of their run: `@label = value`, the value alone, or `@_` to hold its place.

```crs
Positive { n = 4 }
Sized { @len = 2, items = two }
Sized { @_, items = two }
```

### Structure update

A leading `..base` copies a value of the same nominal structure. Labeled entries following it replace fields.

```crs
Pair { ..pair, snd = false }
Pair(Str, Nat) { ..pair, fst = "new" }
```

A spread copies the plain fields. A hidden field is never copied, since what it states is stated of the fields the new value has: left out, it is filled anew as in any literal, so a bound an override breaks is refused, and the base's own is written where it is meant — `Positive { ..p, @ok = p.ok }`.

The spread must be first and may occur only once. Every override must be labeled and overrides must follow declaration order. The head may choose different parameters, but every copied and replaced field is checked at that new instantiation; dependent fields must therefore remain consistent.

Tuple and string literals do not have this update form. List and packed spreads are concatenation forms governed by their literal sections.

## Concepts and witnesses

Concepts provide ad-hoc polymorphism. A concept is a record-shaped interface, a witness is a registered inhabitant of a concept application, and a `use` parameter asks the elaborator to supply such an inhabitant.

`concept` and `satisfy` are contextual words: they remain ordinary identifiers outside their declaration positions.

### Concept declarations

A concept has zero or more parameters, a required representation sort, and a field list. The representation sort follows the struct rules: `: pub Type` declares a transparent concept, `: Type` a *sealed* one whose representation is private to its declaring module's subtree, so witness declarations, dictionary literals, structure updates and raw field projections are permitted only there. Resolution, `use` parameters and the generated method wrappers work the same either way, and the concept name's own visibility stays independent of its representation.

```crs
pub concept Show(A: Type): pub Type {
    show(A) -> Str,
}

pub concept Monad(M: (Type) -> Type): pub Type {
    pure(@A: Type, A) -> M(A),
    bind(@A: Type, @B: Type, M(A), (A) -> M(B)) -> M(B),
}
```

Every ordinary field receives a wrapper in the concept's namespace, so `Show/show(value)` asks for an implicit witness of `Show(A)` and projects its `show` implementation. A method's wrapper takes the concept's parameters and the witness in the same parameter list as the method's own, so a method is called once, as it is declared: `Show/show(value)`, and `Fun/name(@Pair)` for a `name() -> Str` at `Pair`. A field that is not a function is reached by the call that supplies the witness, `Sized/Carrier(@Nat)`.

Concepts whose method types name one another's dictionaries are declared as one group with `and`, as structures are — see [Recursive groups](#recursive-groups). A superclass cycle — `use B(A)` in `A` and `use A(B)` in `B` — is refused whether or not the two are declared together, since resolution could never discharge it.

The field list is a dependent telescope: later fields may refer to earlier named fields. In a generated wrapper such a reference becomes the corresponding projection of the resolved witness, so the wrapper's type constrains that witness's own implementations.

```crs
pub concept Idem(A: Type): pub Type {
    op(A) -> A,
    law(x: A) -> Eq()(op(op(x)), op(x)),
}
```

A field whose type is a proposition about earlier fields is a law. `satisfy` cannot register a witness for such a concept without supplying a proof that discharges the law at the implementations that witness supplies, so a witness violating it is rejected where it is declared.

A field's result may itself be a sort, which makes the field an associated type each witness chooses. `Div`'s `Ok(A) -> Prop` is what lets every carrier state its own division precondition, and a witness supplies it with the same field sugar as any other:

```crs
satisfy Rem(Nat) {
    Ok(b) = Holds(0 < b),
    rem = rem,
}
```

A field beginning with `use` is an anonymous superclass edge. Its type must reduce to a concept application, as a `use` parameter's must, so an alias of one is an edge.

```crs
pub concept Ord(A: Type): pub Type {
    use Eql(A),
    ord(A, A) -> Ordering,
}
```

A local `Ord(A)` witness can therefore satisfy an `Eql(A)` goal by superclass projection.

A sealed concept's fields are not part of its public interface: a `pub` sealed concept may reference private names in its field types, so a private superclass is a hidden obligation resolution discharges without the consumer naming it ([Privacy is scoped to a subtree](design/surface/privacy-is-scoped-to-a-subtree.md)). A transparent `pub` concept's field types are interface and must be `pub` themselves.

A concept returning `Prop` (or `pub Prop`) has proof-irrelevant witnesses that erase completely.

### Witness declarations

`satisfy` registers an anonymous witness. Its terminal type is a concept application, written as any call is — `satisfy Named(@Nat) { … }` over `concept Named(@A: Type)` — and its body supplies the concept fields — or holds `..` alone, asking the compiler to write them; see [Derived witnesses](#derived-witnesses).

```crs
satisfy Show(Nat) {
    show(n) = Nat/to_str(n),
}
```

A witness may quantify over implicit parameters and require other witnesses. A nonempty telescope is separated from the concept application by `=>`. It cannot declare explicit parameters, because resolution has no explicit arguments to supply.

```crs
satisfy (@A: Type, use Show(A)) => Show(List(A)) {
    show(values) = List/fold(values, "", (value, result) => Str/concat(result, Show/show(value))),
}
```

Every witness is keyed by the concept name and the tuple of rigid heads of every concept parameter, and its signature spells the key: each parameter is written as the type it is. A head is an inductive, a structure, an intrinsic type or a tuple type, applied or not; a name bound to one, which is read through; or a *partially applied* family written as a lambda, `(A: Type) => State(S, A)`, which keys on the applied head. A head that only computation reaches, `Show(Chosen(true))` over a `Chosen` that picks its result, is refused where the witness is declared, with the key it computes: a witness is known by its key before anything is elaborated. Remaining arguments below those heads are checked by unification after lookup.

A tuple type is keyed by its *shape*: the label at each field position, arity implied, field types excluded. Labels are part of a tuple type's identity, so `Show({Nat, Bool})`, `Show({a: Nat, b: Bool})` and `Show({x: Nat, y: Bool})` are three keys for three types. `{}` keys as the empty shape, and a constructor whose body is a tuple type — `let Pair(A: Type) -> Type = {Nat, A};` — keys on that body's shape in the higher-kinded position. `/std/Tuple` writes the tuple-keyed witnesses the standard library has: `Show`, `Spell`, `Eql` and `Ord` at the positional shapes of nought through eight fields. A labeled product wanting the same is written as a `struct`.

```crs
satisfy (@A: Type, @B: Type, use Show(A), use Show(B)) => Show({A, B}) {
    show(t) = Str/concat("(", Str/concat(Show/show(t.0), Str/concat(", ", Str/concat(Show/show(t.1), ")")))),
}
```

A function type is **not** keyed: a `satisfy` whose concept parameter reduces to one is refused as unkeyable, with the same report a variable head gets. A function becomes a monad by being wrapped in a nominal type, which is `/std/State`'s idiom. Why is [Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md).

Witnesses that resolve through each other are declared as one group with `and` — see [Recursive groups](#recursive-groups). A lone witness may resolve through its own entry with nothing said.

```crs
satisfy Show(Tree) {
    show(t) = match t | leaf(n) => Nat/to_str(n) | node(f) => Show/show(f) end,
}
and Show(Forest) {
    show(f) = match f | nil() => "" | cons(t, rest) => Str/concat(Show/show(t), Show/show(rest)) end,
}
```

A globally registered witness therefore requires a concept with at least one parameter; a parameterless one is still usable through an ordinary value supplied in a local `use` scope. Parameters key independently — `Into(Nat, Str)` and `Into(Nat, Bool)` are distinct keys — so a call must determine every parameter from its explicit arguments, its expected result, or an explicitly supplied witness before lookup can proceed.

Only one witness may occupy a key across the whole program. Module visibility does not scope witness registration, but a *sealed* concept's representation does gate declaration: its witnesses may only be declared within the concept's declaring module's subtree.

To use a second dictionary for the same key on a *transparent* concept, construct an ordinary concept value and supply it explicitly (a sealed concept forbids the literal outside its module):

```crs
let reverse: Ord(Nat) = Ord { ord(a, b) = reversed(a, b) };
sort(@_, use reverse, values)
```

### Derived witnesses

A witness whose block holds `..` alone is derived: `satisfy Spell(Point) { .. }`, or `satisfy (@A: Type, use Spell(A)) => Spell(Tree(A)) { .. }` under a telescope, and either form may join an `and` group beside written members. The dots are not an entry — nothing is written beside them and they take no comma — so a body is written whole or derived whole. The signature is the programmer's — it registers, keys, and meets the orphan and sealing rules exactly as a written witness does — and the compiler writes the body from the declaration of the type in the key ([A witness body may be written by the compiler](design/surface/a-witness-body-may-be-written-by-the-compiler.md)). Derivability is a property of the concept: `Spell`, `Eql`, `Ord` and `Hash` derive, every other concept refuses the form by name, and the hand-written witness remains the norm.

```crs
struct Point: pub Type { x: Nat, y: Nat }
induct Tree(A: Type): pub Type | leaf(A) | node(Tree(A), Tree(A)) end

satisfy Spell(Point) {
    ..
}

satisfy (@A: Type, use Spell(A)) => Spell(Tree(A)) {
    ..
}
and (@A: Type, use Eql(A)) => Eql(Tree(A)) {
    ..
}
and (@A: Type, use Eql(A), use Ord(A)) => Ord(Tree(A)) {
    ..
}
```

The key must be a declared `induct` or `struct` — not an intrinsic carrier, a tuple or function shape, or a concept's own record — fully applied, representation-transparent where the witness is declared, and not a proposition. An implicit payload or a hidden field is inferred by the re-parsed text and takes no part, and a payload that is itself a type is refused; every other goes through its own witness, resolved in the witness's scope — a telescope premise, the witness's own entry, or a member of the same `and` group. A missing one is reported against the constructor and payload, naming the `use` premise to add when the payload's type is a telescope variable.

| Concept | The body the compiler writes | A proof payload |
| --- | --- | --- |
| `Spell` | the constructor, qualified by its type's own name, applied to the explicit payloads — `Tree/node(Tree/leaf(1), Tree/leaf(2))`, `Option/some(3)` — and a struct as its literal, `Point { x = 1, y = 2 }`, positional where a field has no label | spells as the written goal `?` |
| `Eql` | structural: the same constructor with pairwise equal payloads, `!=` its negation | compares as nothing |
| `Ord` | constructors ranked by declaration position, payloads compared only where the constructors agree, taking the first that differs | contributes nothing |
| `Hash` | the constructor's ordinal, then the explicit payloads' encodings, each part behind its own four-byte length so no two distinct values share a byte string; a struct takes ordinal zero | takes no part |

A derived `Spell`'s text re-parses wherever the type's name is visible unqualified, which is wherever a value of it is written, so it reads in a report as the author would have written it. `Ord` has `Eql` as a superclass, so a derived one asks for the key's equality witness and reports at the declaration when there is none. A proof payload never contributes anywhere, since two values differing only in a proof are the same value.

### Witness premises

A witness premise must be a concept application strictly smaller than the witness's own: every variable in it is bound by the witness's telescope, no variable occurs more often in it than in the witness's concept application, and it has fewer nodes in all. A premise may therefore name a constant beside a binder — `use Monad/Lift(Io, M)` under `Monad/Lift(Io, (A: Type) => Try(M, E, A))` — while `use Show(A)` under `Show(A)` is refused. Resolution terminates because the premises shrink ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

### Orphan rule

A witness may be declared only by the compilation root that owns its concept or at least one rigid type head in its key, which is what stops two independent parties from defining the same globally coherent instance ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

A tuple shape is owned by no root, as an intrinsic type former is, so a tuple-keyed witness is declared where its concept is: a program writes tuple witnesses for its own concepts and cannot add one for a `/std` concept at a shape `/std` did not write. No root is exempt, the standard library included — it declares every concept it witnesses, so the first clause admits it on the same terms as anyone.

### Superclass fields in literals

A concept's superclass fields are hidden members of a concept value. They take no position where it is read — a projection `.N` and a pattern's positional field count the methods alone — and a literal's entries follow the declaration as a call's arguments follow a function's parameters: the fields are written in order, and before each the `use value` entries fill the superclass slots that precede it, from the first. A slot left out is filled by witness resolution, and `use _` holds a slot's place to the same effect, so `Both { use _, use second, both = … }` supplies the second of two edges alone.

```crs
Ord { use custom_eql, ord(a, b) = reversed(a, b) }
```

A witness body never writes one: a `use` entry in a `satisfy` is refused by name, so resolution fills every superclass slot of a registered witness, and the `Eql(A)` reached through a local `Ord(A)` is the one the table holds ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

In a structure update, a spread copies superclass fields from the base. A `use value` after the spread replaces the next superclass slot after the entries before it, and `use _` leaves one as the spread leaves it, copied.

### Witness parameters and arguments

A witness parameter is written `use Concept(args)` in a function type or definition telescope. It has no name and joins the witness scope of the function body, which is exactly the `use` members in scope. Its type must reduce to a concept application, since resolution answers nothing else: any other type is refused where the parameter is declared — in a signature or a witness telescope — and a proof meant to be discharged is an implicit `@` parameter instead. A dictionary a program wants to name is an ordinary value — a `let`, or a plain or `@` parameter of the concept's type — and reaches a witness slot through a written `use value`.

```crs
pub let join(@A: Type, use Show(A), values: List(A)) -> Str =
    List/fold(values, "", (value, result) => Str/concat(result, Show/show(value)));
```

At a call site, `use value` supplies a witness argument explicitly and overrides resolution.

```crs
join([1, 2, 3])
join(@_, use custom_show, [1, 2, 3])
```

### A type declared under a premise

A `struct` or an `induct` takes `use Concept(args)` among its parameters, and the dictionary it is applied at is then an argument of the type.

```crs
pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
    key: K,
    value: V,
}

pub let holds(@K: Type, use Key(K), @V: Type, slot: Slot(K, V), key: K) -> Bool =
    Key/same(slot.key, key);
```

The parameter has no name and is in the witness scope of the declaration, so a field's type, an index's type and a constructor's payload resolve through it. Where the type is written the slot is a call's: `Slot(Nat, Str)` leaves it to resolution and `Slot(Nat, use reversed, Str)` fills it. Under a `use Key(K)` premise, as in `holds`, the nearest witness is the premise itself, so the signature's `Slot(K, V)` names the dictionary the function was given. Two types that differ in the dictionary are different types, since conversion compares it as it compares any argument, and a value holds no dictionary: the parameter belongs to the type, as `K` does.

A structure literal with a bare head takes the dictionary from its expected type, or from resolution where there is none. At an inductive's value constructors the parameter stays a witness slot — resolved, or supplied with `use value` — where a plain or `@` parameter is implicit.

A value is read under the dictionary its type names. `holds(slot, key)` over a `slot: Slot(Nat, use reversed, Str)` runs under `reversed`: the call's own slot is still open when the argument is checked, the argument's type fills it, and resolution leaves a filled slot alone ([Witness resolution](#witness-resolution)). Two values under two dictionaries cannot both be arguments of an operation whose signature names the dictionary once, and the call is a type mismatch.

A concept takes no `use` parameter: its premise is a superclass field, and a witness is keyed by the type heads of its concept's parameters, which a dictionary does not have.

### Witness resolution

An omitted witness argument is resolved in this order:

1. Search local `use` parameters from innermost to outermost; the first direct match wins.
2. Search superclass projections of local witnesses breadth-first; more than one match at the same minimum depth is ambiguous.
3. Look up the concept and the rigid heads of every parameter in the global witness table.

A slot is resolved only while it is open: one that unification has already solved — an expected type or an argument's type named the dictionary — is left as it stands. A slot is also resolved where it stands, so a premise whose concept arguments are known when the call reaches it takes the registered witness before a later argument could name another, and that argument is then a type mismatch; `use value` at the slot says which is meant.

If any concept parameter is still headed by an unsolved metavariable, resolution waits until that metavariable is solved. A selected global witness is instantiated with fresh implicit arguments, its witness premises are resolved recursively, and its full result type is unified with the goal. A unit's witnesses are known by key before any of them is elaborated, so a witness may be declared after the code that resolves through it, and what that code means does not depend on which side of it the witness is written: a goal finds the witness declared at its key, and a key no witness is declared at is reported where the goal stands. A declaration and a witness that each need the other have no order to be elaborated in, and are refused together, by name.

Higher-kinded parameters are keyable. When conversion establishes a shape such as `M(A) = Option(Nat)`, it may infer `M` as the `Option` type constructor, allowing `Monad(Option)` lookup. An under-applied shape such as `M(A) = State(S, Nat)` infers `M` right-biasedly, as `(A) => State(S, A)`: the final argument is the abstracted one, which is why a family intended as a monad orders its parameters context first and result last.

## Foreign declarations

A `foreign` declaration introduces a value implemented by the embedder. Its declared type uses the wire grammar rather than arbitrary Curios types.

```crs
foreign random: Nat;
foreign frobnicate: (Nat, Bytes) -> Nat;
pub foreign log: (Bytes) -> Nat;
foreign close: (Handle) -> {};
foreign flip: (Byte) -> Byte;
foreign read: (Handle, Nat) -> {status: Nat, bytes: Bytes};
```

The wire types are `Nat`, `Int`, `Bool`, `Byte`, `Flt`, `Bytes`, `Bits`, `Handle`, and `List(T)`, spelled bare: the wire grammar is a closed vocabulary that resolves no names, so `/std/Nat` is refused where `Nat` is meant. A `Byte` crosses as the integer it is, in both directions; a host answering one outside `0..=255` stops the program rather than handing it a different byte. A `Nat` or `Int` crosses as a 64-bit integer although the program's are unbounded: a `Nat` argument crosses below `2⁶⁴` and an `Int` between `-2⁶³` and `2⁶³ - 1`, and a larger one stops the program rather than crossing changed. A result comes back as a 64-bit integer too — a `Nat` read unsigned and an `Int` signed — and the program boxes it, so every result the integer holds arrives whole. A wire signature is a wire result for a zero-argument foreign, or a parenthesized wire parameter list followed by `->` and a wire result.

A wire result is a wire type, or a braced list of labelled wire types — the [tuple type](#tuple-types) the call yields. `{}` is no result at all, which is the unit type; `()` is the unit value and never stands here. Two or more fields are the tuple the guest projects by name, as `/sys` reads `.status` and `.bytes` off a host read.

A tuple type's labels are part of its identity, so nothing may be invented, dropped or moved: the fields cross in the order written, a reference result — `Bytes`, `Bits`, `Handle` or `List(T)` — in whatever slot it is written, and as many of them as the row has. The one spelling refused is a single result in braces: it is written bare rather than as `{value: T}`, because one result crosses as itself and a one-field brace would name a tuple the row cannot carry.

`Bytes` and `Bits` are distinct wire types over one payload. A row states which grain it means, and a declaration is where the embedder and the program agree on it: `(Bits) -> Bits` says the datum is a bit run where both sides read it, and the guest builds `Bits` directly rather than a `Bytes` the call site converts. What crosses is the same flat byte array either way — outbound the run's `ceil(len / 8)` packed bytes, inbound that payload sealed at eight times its byte count. **The wire carries no bit length**, so a `Bits` run whose length is not a multiple of eight crosses as its packed bytes, trailing padding zeroed, and returns at `8n`; a host with a 20-bit datum sends its own length in band, as it would anyway.

`List` does not nest, and its element vocabulary is narrower still: an element must be `Nat`, `Int`, `Bool`, `Bytes`, `Bits`, or `Handle`, so `List(List(T))`, `List(Flt)` and `List(Byte)` are all rejected — a list of bytes is the `Bytes` a row already spells. A list of `Nat`, `Int` or `Bool` crosses flat, one element per value, each narrowed going out and boxed coming back by the program exactly as a lone one is, so an element past the wire stops the program as an argument would; a list of `Bytes`, `Bits` or `Handle` crosses as its elements' payloads. A `Flt` crosses on its own as a raw binary64, and no operation has asked for a list of them. How the declaration reaches the embedder is the host ABI's concern rather than the surface language's.

## Equality and proofs

Propositional equality `Eq` is an ordinary indexed inductive proposition from `/std/Eq`. Its proofs use the same constructors, functions, and match forms as other inductives; `Eq` gets no syntax of its own.

```crs
pub let sym(@A: Type, @x: A, @y: A, proof: Eq()(x, y)) -> Eq()(y, x) =
    match proof: (left, right, p) => Eq()(right, left)
    | refl(@value) => Eq/refl()
    end;
```

The standard equality operations include reflexivity, symmetry, transitivity, congruence, and substitution. `Eq` is propositional equality; `Eql` is the value-level concept used by `==` and `!=`.

### Bounds from the facts in scope

A bound is stated and proved in `/std/Bool`'s vocabulary, which a program imports by name: `use /std/Bool/{True, False, Holds};`. `Holds(b)` is the proposition that the decision `b` holds: it reduces to `True`, proved by `True/qed()`, where `b` is `true`, and to the empty `False` where it is not.

A bound reduction does not decide is proved by the elaborator where it follows by linear arithmetic from the facts in scope: the hypotheses and their proof fields one level down, and the guards of the arms around it, each read through the local definitions and refinements in scope. The fragment is `Nat` and `Int` comparisons with literal coefficients; at `Nat`, also a truncated subtraction, through its two cases, and a quotient or remainder at any nonzero divisor, through the quotient's bounds; and, where linear arithmetic alone finds none, the products of pairs of facts and the negated goal. A bound one of those facts states outright is proved by the fact, whatever the proposition is — an opaque `Bool` function's result, an equation, a family of its own. The proof is an ordinary term both checkers recheck, so nothing it adds is trusted ([A bound that follows from the facts in scope is proved by the elaborator](design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md)).

```crs
let get(xs: List(Nat), i: Nat, m: Nat, p: Holds(i < m), q: Holds(m <= List/len(xs))) -> Nat =
    List/get(xs, i);
```

Two `/std` functions reach the same proof where no bound asks for it. `True/proved()` states a fact a term needs, its proposition written or pinned by the expectation. `False/refuted()` produces the contradiction a zero-arm match eliminates, in an arm the facts rule out:

```crs
use /std/Bool/{False};

let clamp(n: Nat, q: Holds(n <= 10)) -> Nat =
    match n > 20 | true => match False/refuted() end | false => n end;
```

A bound the facts do not imply is refused. The report names the facts considered and the ones it could not read, and gives an assignment of the atoms under which the facts hold and the bound fails. A written goal `?` over a bound reports the same.

## Quick reference

| Form | Meaning |
| --- | --- |
| `-- ` | Line comment |
| `--- ` | Documentation comment, attached to the declaration below it |
| `{}` / `()` | Unit type / unit value |
| `@A: Type` / `@T` / `@value` | Implicit parameter or hidden structure field, named or as its type alone / explicitly supplied implicit argument or hidden field |
| `use C(A)` / `use _` / `use value` | Witness parameter of a signature / its place in a lambda / explicitly supplied witness argument, or superclass field of a concept literal |
| `?` | Written goal — reports scope, type and fits, then fails compilation |
| `term!` | Monadic bind through `Monad`, lifting a cross-monad action through `Monad/Lift` |
| `"""` … `"""` | Block string literal — the lines between the delimiters, their shared indentation removed |
| `b[...]` / `x[...]` | `Bits` / `Bytes` literal — grain letter glued to the bracket |
| `Name { ... }` | Structure or concept literal |
| `Name { ..base, ... }` | Structure update |
| `struct Slot(K: Type, use Key(K)): …` | A type declared under a premise — the dictionary is an argument of the type |
| `match term ... end` | Typed elimination or dispatch |
| `choose ... end` | Ordered guarded ladder |
| `test name = body;` | Declared test — a `/std/Test` description, collected per unit and run by `curios test` |
| `satisfy C(args) { ... }` | Globally registered anonymous witness |
| `satisfy C(args) { .. }` | Derived witness — the compiler writes the body |
| `satisfy (@A: Type, use C(A)) => D(args) { ... }` | Witness under a telescope |
| `use /std/{Nat};` / `use /std/*` / `use /{Name};` | Import a group, the exported surface, or a name from the compilation root |
| `… and …` | [Recursive group](#recursive-groups) — `let`, `induct`, `struct`, `concept` or `satisfy` |
