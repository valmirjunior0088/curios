# Syntax

This document defines the surface language accepted in `.crs` files. It is a reference for writing and reading Curios programs, not a description of compiler internals. An implementation disagreement is a language conformance bug: either the implementation or this document must be corrected.

A `.crs` file is a sequence of top-level items. An entrypoint closes with one final term, the description the program performs; a module file has no final term and is items alone. Everything below is an item, a term, or the spelling of one of their parts.

Examples use declarations from `/std`, the standard library every program may name. The authored library under `curios-prelude-archive/std/` is the main corpus of complete programs.

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

A name beginning with `_` is one the author keeps unused: [`curios lint`](usage.md#lint) never reports an `_`-prefixed binder or declaration, nor anything inside an `_`-prefixed module. `_` alone is a binder that names nothing.

The following words are reserved and cannot be used as path segments:

| Declaration and expression words | Literal words |
| --- | --- |
| `let`, `match`, `choose`, `mod`, `use`, `pub`, `end`, `induct`, `struct`, `foreign` | `true`, `false` |

Twelve words. The rest are yours.

`concept`, `satisfy`, `and`, and `test` are contextual words. They are recognized only in the grammatical positions that use them and remain valid identifiers and path segments elsewhere. `Type` and `Prop` denote sorts when parsed as terms, but they are not globally forbidden path segments.

### Paths

A path is one or more identifier segments separated by `/`. A leading `/` anchors the path at the compilation root.

A path without that leading `/` is relative to the module it is written in: its head must be a declaration of that module or a name that module imported. Enclosing modules are not searched, so an ancestor's declaration is reached by writing it absolute or by importing it — which is what the report says when one is not found.

```crs
Nat                 -- a declaration or import of this module
Option/some         -- member of Option
/std/List           -- absolute name
/std/Nat/Lt         -- absolute name through a nested module
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

String escapes are `\n`, `\t`, `\r`, `\\`, `\"`, and `\u{…}` as in a character literal. An unrecognized escape in a string literal is not an error: the backslash and the following character both stand for themselves, so `"\%"` is the two-character string `\%`, and so is `"\u"` — only the brace reserves the Unicode form, and a malformed `\u{…}` is a parse error. A string literal is not a format string and does not try to guess which of the two you meant.

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

`Str` stores certified UTF-8 bytes. Its logical length, indexing, slicing, folding, and search operations count Unicode scalar values (`Char`), not bytes or grapheme clusters.

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

Although the surface spelling is always the nullary term `Type`, each occurrence has an implicit level in a cumulative hierarchy. The compiler infers those levels and generalizes reusable declarations over them; there is no syntax for universe variables, levels, or explicit universe arguments. A type accepted at one level is also accepted where a higher level is required. The hierarchy is there; you just never write it down.

All inhabitants of the same proposition are definitionally irrelevant, so a proof does its thinking at compile time and then weighs nothing at runtime. Eliminating a proposition into a computational result is restricted: the proposition must be empty, or have one constructor whose payloads are each non-informative or fixed by the family's indices — which is what lets an `Eq` proof be matched to produce data. Proofs may always be eliminated to prove another proposition. Why the sorts are shaped this way is [`Prop` is strict, proof-irrelevant and definitionally K](design/language/prop-is-strict-proof-irrelevant-and-definitionally-k.md) and [Implicit cumulative universes, general recursion](design/language/implicit-cumulative-universes-general-recursion.md).

### Function types

A function type is a parenthesized dependent parameter list followed by `->` and its result. The list may be empty: `() -> T` is a nullary function, whose call site writes `f()`.

An explicit parameter is written `name: type` or as an unlabeled type. An implicit parameter begins with `@`. A witness parameter begins with `use` and is anonymous.

```crs
(Nat) -> Nat
(x: Nat, y: Nat) -> Nat
(@A: Type, x: A) -> A
(@A: Type, use Show(A), value: A) -> Str
```

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

Labels are part of a tuple type's identity: `{Nat, Bool}`, `{a: Nat, b: Bool}` and `{x: Nat, y: Bool}` are three distinct types, and a value of one is not a value of another. Function-type parameter names carry no such weight; only tuple labels do. Labels are not decoration.

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

Arguments are divided into three independent queues:

- `value` supplies an explicit parameter;
- `@value` supplies an implicit parameter;
- `use value` supplies a witness parameter explicitly.

```crs
f(@Nat, x)
join(use custom_show, values)
```

Omitted implicit arguments are inferred. Omitted witness arguments are resolved as described in [Witness resolution](#witness-resolution).

A projection is positional or labeled:

```crs
pair.0
pair.fst
configuration.network.port
```

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
(s: A, t: A, q: Eq(s, t)) => proof(q)
((lo, hi), q: Eq(lo, hi)) => lo
```

A lambda parameter carries the same plicity mark as a function-type parameter: `@name` binds an implicit slot, `use name` binds a witness slot, and an unmarked binder binds an explicit slot. The mark applies to the slot the parameter occupies whatever the pattern shape. Each written binder is checked against the plicity of the slot it claims when the lambda is checked against an expected function type.

```crs
(@A, value) => value
(@A, use show, value) => Show/show(value)
```

An omitted implicit or witness binder is inserted from the expected function type, so hidden binders may be left out when the body does not name them. Alignment is positional by plicity: each written binder claims the next slot of its own plicity, and every skipped implicit or witness slot before it is inserted. A plain binder never silently binds a hidden slot. Against `(@A: Type, use Show(A), value: A) -> Str`, each of `(value) => …`, `(@A, value) => …`, `(use show, value) => …` and `(@A, use show, value) => …` is accepted; `(A, show, value) => …` is not, because `A` binds the sole explicit slot and the rest are surplus.

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

Every parameter of a `let` or `satisfy` telescope must be annotated; only a `use` parameter is written without one. The `label(params) = value` sugar inside tuple, struct, and witness bodies takes the annotation as optional, since the field's declared type supplies it.

The binder may be an irrefutable tuple or struct pattern:

```crs
let (x, y) = pair;
let Point { x, y } = point;
x + y
```

A binding is in scope of its own value, so a local function may call itself. A binding that mentions itself states its type, since a body that mentions the binding cannot be the source of it, and is a plain name rather than a pattern; a binding whose value performs `!` cannot mention itself, since the action runs before the binding exists. Bindings that mention one another are declared as one group with `and` — see [Recursive groups](#recursive-groups).

Because a binding is in scope of its own value, `let n = n + 1;` names the binding it declares rather than an outer `n`, and is refused as the recursive value it is: a value may mention itself only under a lambda, where it is a recursive value computed the first time it is read.

### Irrefutable binder patterns

The binders of `let`, lambdas, function-definition sugar, and the `;` fold-hypothesis position of `Nat`/`List`/`Bits`/`Bytes` match arms accept nested tuple and struct patterns.

```crs
let (x, (_, y)) = value;
let Point { loc = (x, y), color } = point;
body
```

These patterns are projection sugar, not runtime matches. The struct head is documentary and is not resolved or checked. An unlabeled field is matched positionally; a `label = pattern` field projects that label. Field punning such as `Point { x, y }` is the positional form, whose sub-patterns happen to be binders named after the fields. Parentheses group a pattern without changing it, so `((x, y))` is `(x, y)`.

Refutable patterns belong only to `match`.

### Written goals

`?` is a development goal. It asks the elaborator to infer as much as possible, then reports the local scope, the expected type and the candidate fits it found, and fails compilation.

```crs
let compose(@A: Type, @B: Type, @C: Type, f: (B) -> C, g: (A) -> B) -> (A) -> C =
    ?;
compose
```

The compiler tells you everything it knows about the hole, and then refuses to build. A goal is never accepted in a successfully compiled program.

### Whole-term forms and operand positions

`let`, `match`, `choose`, lambdas, and function types are whole-term forms: a body or tail extends to the end of the enclosing term. There is no expression-level `term: type` ascription; a `:` annotation appears only in binder, signature, and motive positions.

An infix operand is an applied atom: a literal, name, sort (`Type`/`Prop`), tuple, tuple type, structure literal, goal, or parenthesized term, followed by any chain of calls, projections, and postfix `!`. A whole-term form is not an operand; parenthesize it to use it as one.

```crs
1 + (match flag | true => 1 | false => 0 end)
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

`/` and `%` additionally carry the precondition their concept declares. `Div` and `Rem` each have an `Ok(A) -> Prop` field, and the operator inserts an implicit proof of `Ok(divisor)` — so `a / b` on `Nat` must discharge `Nat/Lt(0, b)`. A carrier whose division is total states `Bool/True` and pays nothing, which is what keeps `/` a single operator over carriers that disagree about whether it can fail ([A bound is stated in a decided proposition and discharged by reduction](design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)). Dividing by zero is not a runtime surprise here; it is something you prove will not happen.

Operator notation always uses witness resolution, including intrinsic operands. Standard witnesses cover the intrinsic types, while a `satisfy` declaration enables the same notation for a user-defined type.

## Sequencing and effects

### Postfix `!`

`action!` is monadic sequencing. Each occurrence is equivalent to a call to `/std/Monad/bind(action, continuation)` in the monad of its region.

```crs
use /std/{Nat, Byte, Parse};
pub let parser: Parse(Nat) =
    let a = Parse/any_byte!;
    let b = Parse/any_byte!;
    Parse/pure(Byte/to_nat(a) + Byte/to_nat(b));
```

Every value body is a sequencing region. Lambda bodies, match arms, and recursive member bodies begin fresh regions; the tail after a local `let` remains in the same region. There is no `let !` header or matching `end`.

A region's monad is read from the region's type and never inferred from a sequenced action. A region whose type is not yet known waits for it, and one whose type can never name a monad — the body of a lambda in inference position, say — is rejected with a request to annotate the enclosing result type.

An action whose own monad differs from the region's is lifted through the declared `Lift` witness for that ordered pair, and a pair with no witness is rejected. See [Lifting between monads](#lifting-between-monads).

Postfix `!` is not allowed in types. The token `!=` is an infix operator and is not parsed as postfix `!` followed by `=`.

### Host effects and `Io`

Every operation that touches the host — writing a handle, reading a clock, allocating or reading a cell, calling a `foreign` function, exiting — has result type `Io(T)`: a *description* of a computation yielding a `T`, not the `T`. Calling one performs nothing, so `let greeting: Io({}) = print("hello");` has printed nothing.

**There is no operation taking an `Io(T)` to a `T`** ([Effects are descriptions and the carrier has no eliminator](design/language/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)). A description is performed only by being the program's tail, which the emitted entrypoint forces once. So a function whose result type is not an `Io` cannot perform an effect, and a `!` may only appear in a region whose type is a monad — a `(Str, Bool) -> Bool` has nowhere to sequence one.

`Io/pure` wraps a value as a description performing nothing and `Io/bind` sequences one into another, but postfix `!` reaches `Io` through its `Monad` witness like any other monad. Binding a description does not perform it, and forcing one twice performs it twice:

```crs
use /std/{Io, print};
let once: Io({}) = print("x");
let _ = once!;
once                            -- prints "x" twice in total
```

An `Io` is a noun, not a verb.

`Io` is not matchable: it has no constructors to enumerate, so a `match` over one is rejected, whether it writes constructor arms or none at all. A lone `| _ =>` arm is an irrefutable binder match rather than an elimination, and is accepted as the binding it is.

A host operation that can fail is declared `Try(M, E, A)` over its base monad — `Try(Io, Io/Error, File)` for `File/open`, `Try(Async, Io/Error, Socket)` for `tcp/Socket/connect`. Through the edges `/std/Try` declares, a `Try` region sequences a `Try` over the same base, a bare `Result` as early return, an action of the base, and an `Io` action wherever the base admits one; `Try/raise` stops the region, `Try/rescue` handles the stop, and `Try/run` hands the outcome back as an `M(Result(E, A))`.

```crs
use /std/{File, Path, Bytes, Try, Io};
let contents: Try(Io, Io/Error, Bytes) =
    let f = File/open(Path/of_str("notes.txt"), File/Mode/read())!;
    let text = File/read_all(Path/of_str("notes.txt"))!;
    let _ = File/close(f)!;
    Try/pure(text);
```

### Lifting between monads

`/std/Lift(M, N)` declares the canonical embedding of monad `M` into monad `N`: one method, `lift`, taking an `M(A)` to an `N(A)`, with `Monad` witnesses for both sides as superclasses — so an embedding between non-monads cannot be declared. Like every witness, one `Lift` witness may occupy each ordered pair of monads program-wide, so which embedding runs is a fact about the program, never about a call site.

```crs
satisfy Lift(Io, Async) {
    lift = lift,
}
```

With that witness declared — `/std/Async` declares it — an `Io` action sequences directly inside an `Async` region, and the `!` inserts the lift:

```crs
use /std/{Async, print};
pub let fiber: Async({}) =
    let _ = print("hello\n")!;
    Async/pure(());
```

The explicit spelling `lift(action)` names the same embedding, with the target monad inferred from the region. A region's tail — the last expression of a value body, a lambda body, or a match arm — is lifted by the same read when its head's declared monad and the region's are both monads and differ; a tail that is no monadic action keeps the ordinary type mismatch. The read is of the action's *head's declaration*, so one whose head is not a declared name — a projection, a call of a lambda — is not embedded on its own: it reports as an action of one monad where another is expected, and `lift(action)` is the spelling that embeds it.

Embeddings never chain. Declaring `Lift(Io, Job)` and `Lift(Job, Sched)` does not let an `Io` action sequence in a `Sched` region: the missing `Lift(Io, Sched)` is reported, together with any chain of declared embeddings that would have reached it. The composite is declared like any other — a decision about `Sched`, written by its author, not derived by the compiler ([Monads embed along declared edges, and `!` lifts across them](design/language/monads-embed-along-declared-edges-and-bang-lifts-across-them.md)).

## Pattern matching

### Match shell and motives

A headed match has a scrutinee, an optional motive, one `| pattern => body` arm per case, and a closing `end`. An arm may be left out where that constructor's index target is *provably* impossible at the scrutinee's indices — a match over a `Sized(T, n + 1)` needs no `empty()` arm — and where it is not provable the missing arm is demanded by name. A scrutinee whose type reduces to an inductive with no constructors takes no arms at all: `match contradiction end` is how a proof of an empty type is discharged, with a motive where the result has to be spelled.

The motive states the result type as a family. It is an ordinary term, checked against the eliminator's motive type — a function of the scrutinee's indices, in declaration order, and then the scrutinee:

```text
(indices) -> Scrutinee(indices) -> Sort
```

There is no motive grammar: what follows `:` is parsed as a term and terminates at the first arm, since `|` is not an infix operator ([A motive is a term, not a grammar](design/language/a-motive-is-a-term-not-a-grammar.md)).

```crs
match b: (_) => Nat                            -- result ignores the scrutinee
match n: (m) => P(m)                           -- result depends on it
match p: (s, t, q) => Eq(t, s)                 -- an indexed family
match p: (s: A, t: A, q: Eq(s, t)) => Eq(t, s) -- with written annotations
match p: discriminates_eq                      -- a named family
match v                                        -- omitted; inferred
```

The number of binders is fixed by the eliminated type: one per index, then one for the scrutinee. A non-indexed scrutinee — every intrinsic carrier, and any inductive declared without an index telescope — takes exactly one, so a result that ignores it is written `(_) => T`; the `Sized` declared under [Inductive declarations](#inductive-declarations) has one index and takes two binders, and `Eq` has two and takes three.

Parameters are never binders. They are uniform across constructors and fixed by the scrutinee's type, so the motive body reaches them through the ambient scope — exactly as a constructor's case target states only index expressions.

Each arm is checked against the motive at that constructor's target indices, and the match as a whole at the scrutinee's actual indices. A `| _ =>` default binds nothing and refines no index, so it is checked at the actual indices too.

A binder may be written bare, as `_`, or annotated. An annotation is an ordinary type in an ordinary position: checked by conversion against the binder's expected type, obeying the usual plicity rules, and free to name the binders before it. Annotating the scrutinee binder is how a reader recovers the eliminated family on the motive line.

```crs
match p: (s, t, q: Eq(s, t)) => Eq(t, s)
```

Omitting the motive asks the elaborator to infer it. Over a variable scrutinee in a position with an expected type, the result is that expected type as written, and each arm is checked against it with the scrutinee and its variable indices standing for the arm's case — so a hypothesis whose type mentions the scrutinee needs no convoy to ride along. Over an expression scrutinee the expected type is abstracted over the expression's occurrences instead. Prefer omission wherever inference succeeds. A motive has to be written where there is nothing to infer from — a type-level match whose result appears in a signature, or an elimination in inference position — and where an occurrence of an expression scrutinee is not there to abstract.

A fold's motive (`Nat`, `List`, `Bits`, `Bytes`) reaches its scrutinee only through the binder it declares: the `; ih` hypothesis is typed at the motive opened at the tail, and a motive that named the scrutinee instead would have that name refined to the arm's own value. `match n: (m) => P(m)` is accepted; `match n: (_) => P(n)` is refused.

A motive may only be written where the head dispatches directly: every arm's top-level pattern must be the same dispatchable shape. A tuple-scrutinee matrix, a struct-headed match, or a plain-binder match builds no core eliminator for the motive to attach to, and rejects one.

### Inductive patterns

An inductive pattern names a constructor and supplies one pattern per payload position.

```crs
match option
| some(value) => use(value)
| none() => fallback
end
```

A constructor is named bare: the scrutinee's type supplies the namespace, so `Option/some(n)` is refused as a pattern. A payload position the constructor declared implicit (`@`) must be matched with `@`; a plain payload is matched without a mark. A constructor pattern supplies one pattern per payload position, hidden ones included — omitted hidden payload patterns are not inserted, unlike lambda binders. Witness payloads are not a surface feature, so `use` is not accepted in a constructor pattern.

```crs
match vector
| nil() => fallback
| cons(@length, head, tail) => head
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

let _right_identity(n: Nat) -> Eq(n + 0, n) =
    Eq/refl();
```

A test takes no parameters. A claim about *every* instantiation is a proposition rather than a description, so it is a `let` whose type states the claim and whose body proves it, checked by the kernel on every build — the second declaration above, its leading `_` marking a declaration that exists for its type rather than its callers ([A test is a check that runs, and a proof is a `let`](design/language/a-test-is-a-check-that-runs-and-a-proof-is-a-let.md)). A test runs; a proof does not have to.

To check a claim that is true but not a theorem at instances you choose, make it an ordinary definition returning `Test` and schedule a table: `Test/all(List/map(cases, ((a, b)) => claim(a, b)))` is one test over the author's own cases, whose failure names the case's position. That works because a test registers like a private definition — referable within its subtree, and colliding with a sibling of the same name.

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

A package's library header is the single exception, and it is a fact about package layout rather than about the language — see [What a package is made of](usage.md#what-a-package-is-made-of). A stem is never part of a name: neither `main` nor `lib` can be written in a path.

### Imports and re-exports

`use` imports through a group: a braced list `path/{…}` or a glob `path/*`. There is no bare `use path;` form — a single import is written `use /std/{Nat};`. The path may be dropped entirely, leaving the root-anchored `use /{Name};`, which is how a nested module reaches a declaration of the compilation root. Prefixing a `use` with `pub` re-exports what it imports.

```crs
use /std/{Nat, Bool};
pub use Option/*;
use /std/Nat/{Lt};
use /{Owner};
```

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
| `satisfy` | `and`, then a whole witness | the last member's `}` or `;` |

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

Parameters follow the name. A parameter marked `@` is implicit at the type constructor; all inductive parameters are implicit at value constructors.

The required result annotation is either a sort or an index telescope followed by a sort:

```crs
pub induct Sized(T: Type): (length: Nat) -> pub Type
| empty(): (0)
| push(@n: Nat, head: T, tail: Sized(T, n)): (n + 1)
end
```

Each index binder may be named or left bare — `(length: Nat)` and `(Nat)` are both well-formed — and an index never takes `@`. The name is never in scope in the constructor cases; it appears in the family's printed signature, and a later entry of the same telescope may depend on it. That dependency is what makes the annotation a telescope rather than a list of types:

```crs
pub induct Tagged: (size: Nat, contents: Sized(Nat, size)) -> pub Type
| tag(@size: Nat, @contents: Sized(Nat, size)): (size, contents)
end
```

Each constructor of an indexed family must state the indices it produces after `:`. A non-indexed constructor does not accept a target.

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

Structures whose fields name one another are declared as one group with `and`; a lone structure may name itself in its fields with nothing said. See [Recursive groups](#recursive-groups).

```crs
pub struct Node: pub Type { value: Nat, next: Option(Edge) }
and Edge: pub Type { weight: Nat, to: Node }
```

### Structure literals

A structure value names its type and supplies its fields. Parameterized heads may supply type parameters before the field block.

```crs
Pair { fst = 1, snd = true }
Pair(Nat, Bool) { fst = 1, snd = true }
Api { base = 3, bump(x) = x + 1 }
```

Fields are checked in declaration order. Function-definition sugar is equivalent to assigning a lambda.

### Structure update

A leading `..base` copies a value of the same nominal structure. Labeled entries following it replace fields.

```crs
Pair { ..pair, snd = false }
Pair(Str, Nat) { ..pair, fst = "new" }
```

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

Every ordinary field receives a wrapper in the concept's namespace, so `Show/show(value)` asks for an implicit witness of `Show(A)` and projects its `show` implementation.

Concepts whose method types name one another's dictionaries are declared as one group with `and`, as structures are — see [Recursive groups](#recursive-groups). A superclass cycle — `use B(A)` in `A` and `use A(B)` in `B` — is refused whether or not the two are declared together, since resolution could never discharge it.

The field list is a dependent telescope: later fields may refer to earlier named fields. In a generated wrapper such a reference becomes the corresponding projection of the resolved witness, so the wrapper's type constrains that witness's own implementations.

```crs
pub concept Idem(A: Type): pub Type {
    op(A) -> A,
    law(x: A) -> Eq(op(op(x)), op(x)),
}
```

A field whose type is a proposition about earlier fields is a law. `satisfy` cannot register a witness for such a concept without supplying a proof that discharges the law at the implementations that witness supplies, so a witness violating it is rejected where it is declared — at the declaration, not in the tests you meant to write.

A field's result may itself be a sort, which makes the field an associated type each witness chooses. `Div`'s `Ok(A) -> Prop` is what lets every carrier state its own division precondition, and a witness supplies it with the same field sugar as any other:

```crs
satisfy Rem(Nat) {
    Ok(b) = Nat/Lt(0, b),
    rem = rem,
}
```

A field beginning with `use` is an anonymous superclass edge. Its type must be a concept application.

```crs
pub concept Ord(A: Type): pub Type {
    use Eql(A),
    ord(A, A) -> Ordering,
}
```

A local `Ord(A)` witness can therefore satisfy an `Eql(A)` goal by superclass projection.

A sealed concept's fields are not part of its public interface: a `pub` sealed concept may reference private names in its field types, so a private superclass is a hidden obligation resolution discharges without the consumer naming it ([Concept representations may be sealed](design/language/concept-representations-may-be-sealed.md)). A transparent `pub` concept's field types are interface and must be `pub` themselves.

A concept returning `Prop` (or `pub Prop`) has proof-irrelevant witnesses that erase completely.

### Witness declarations

`satisfy` registers an anonymous witness. Its terminal type is a concept application and its body supplies the concept fields — or is omitted, asking the compiler to write it; see [Derived witnesses](#derived-witnesses).

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

Every registered witness is keyed by the concept name and the tuple of rigid heads of every concept parameter. Each head must reduce to an inductive, structure, intrinsic type, tuple type, or supported higher-kinded type constructor — including a *partially applied* family written as a lambda, `(A: Type) => State(S, A)`, which keys on the applied head. Remaining arguments below those heads are checked by unification after lookup.

A tuple type is keyed by its *shape*: the label at each field position, arity implied, field types excluded. Labels are part of a tuple type's identity, so `Show({Nat, Bool})`, `Show({a: Nat, b: Bool})` and `Show({x: Nat, y: Bool})` are three keys for three types. `{}` keys as the empty shape, and a constructor whose body is a tuple type — `let Pair(A: Type) -> Type = {Nat, A};` — keys on that body's shape in the higher-kinded position. `/std/Tuple` writes the tuple-keyed witnesses the standard library has: `Show`, `Spell`, `Eql` and `Ord` at the positional shapes of nought through eight fields. A labeled product wanting the same is written as a `struct`.

```crs
satisfy (@A: Type, @B: Type, use Show(A), use Show(B)) => Show({A, B}) {
    show(t) = Str/concat("(", Str/concat(Show/show(t.0), Str/concat(", ", Str/concat(Show/show(t.1), ")")))),
}
```

A function type is **not** keyed: a `satisfy` whose concept parameter reduces to one is refused as unkeyable, with the same report a variable head gets. A function becomes a monad by being wrapped in a nominal type, which is `/std/State`'s idiom. Why the shape was tried and retired is [A tuple type is keyed by the part of its identity conversion keeps](design/language/a-tuple-type-is-keyed-by-the-part-of-its-identity-conversion-keeps.md).

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

Only one witness may occupy a key across the whole program; there is no "the `Show` I meant here". Module visibility does not scope witness registration, but a *sealed* concept's representation does gate declaration: its witnesses may only be declared within the concept's declaring module's subtree.

To use a second dictionary for the same key on a *transparent* concept, construct an ordinary concept value and supply it explicitly (a sealed concept forbids the literal outside its module):

```crs
let reverse: Ord(Nat) = Ord { ord(a, b) = reversed(a, b) };
sort(use reverse, values)
```

### Derived witnesses

A witness may omit its body: `satisfy Spell(Point);`, or `satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));` under a telescope, and either form may join an `and` group beside written members. The signature is the programmer's — it registers, keys, and meets the orphan and sealing rules exactly as a written witness does — and the compiler writes the body from the declaration of the type in the key ([A witness body may be written by the compiler](design/language/a-witness-body-may-be-written-by-the-compiler.md)). Derivability is a property of the concept: `Spell`, `Eql`, `Ord` and `Hash` derive, every other concept refuses the form by name, and the hand-written witness remains the norm.

```crs
struct Point: pub Type { x: Nat, y: Nat }
induct Tree(A: Type): pub Type | leaf(A) | node(Tree(A), Tree(A)) end

satisfy Spell(Point);
satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));
and (@A: Type, use Eql(A)) => Eql(Tree(A));
and (@A: Type, use Eql(A), use Ord(A)) => Ord(Tree(A));
```

The key must be a declared `induct` or `struct` — not an intrinsic carrier, a tuple or function shape, or a concept's own record — fully applied, representation-transparent where the witness is declared, and not a proposition. An implicit payload is inferred by the re-parsed text and takes no part, and a payload that is itself a type is refused; every other goes through its own witness, resolved in the witness's scope — a telescope premise, the witness's own entry, or a member of the same `and` group. A missing one is reported against the constructor and payload, naming the `use` premise to add when the payload's type is a telescope variable.

| Concept | The body the compiler writes | A proof payload |
| --- | --- | --- |
| `Spell` | the constructor, qualified by its type's own name, applied to the explicit payloads — `Tree/node(Tree/leaf(1), Tree/leaf(2))`, `Option/some(3)` — and a struct as its literal, `Point { x = 1, y = 2 }`, positional where a field has no label | spells as the written goal `?` |
| `Eql` | structural: the same constructor with pairwise equal payloads, `!=` its negation | compares as nothing |
| `Ord` | constructors ranked by declaration position, payloads compared only where the constructors agree, taking the first that differs | contributes nothing |
| `Hash` | the constructor's ordinal, then the explicit payloads' encodings, each part behind its own four-byte length so no two distinct values share a byte string; a struct takes ordinal zero | takes no part |

A derived `Spell`'s text re-parses wherever the type's name is visible unqualified, which is wherever a value of it is written, so it reads in a report as the author would have written it. `Ord` has `Eql` as a superclass, so a derived one asks for the key's equality witness and reports at the declaration when there is none. A proof payload never contributes anywhere, since two values differing only in a proof are the same value.

### Witness premises

A witness premise must be a concept application strictly smaller than the witness's own: every variable in it is bound by the witness's telescope, no variable occurs more often in it than in the witness's concept application, and it has fewer nodes in all. A premise may therefore name a constant beside a binder — `use Lift(Io, M)` under `Lift(Io, (A: Type) => Try(M, E, A))` — while `use Show(A)` under `Show(A)` is refused. Recursive resolution terminates because the premises shrink, not because anybody counted ([A witness premise is smaller than its head](design/language/a-witness-premise-is-smaller-than-its-head.md)).

### Orphan rule

A witness may be declared only by the compilation root that owns its concept or at least one rigid type head in its key, which is what stops two independent parties from defining the same globally coherent instance ([Concepts resolve with global coherence](design/language/concepts-resolve-with-global-coherence.md)).

A tuple shape is owned by no root, as an intrinsic type former is, so a tuple-keyed witness is declared where its concept is: a program writes tuple witnesses for its own concepts and cannot add one for a `/std` concept at a shape `/std` did not write. No root is exempt, the standard library included — it declares every concept it witnesses, so the first clause admits it on the same terms as anyone.

### Superclass fields in literals

A concept's superclass fields remain positional slots in a concept value. Omitting one asks witness resolution to fill it, and in a concept literal `use value` fills the next superclass slot explicitly.

```crs
Ord { use custom_eql, ord(a, b) = reversed(a, b) }
```

A witness body never writes one: a `use` entry in a `satisfy` is refused by name, so resolution fills every superclass slot of a registered witness, and the `Eql(A)` reached through a local `Ord(A)` is the one the table holds ([Concepts resolve with global coherence](design/language/concepts-resolve-with-global-coherence.md)).

In a structure update, a spread copies superclass fields from the base. An explicit `use value` after the spread replaces the corresponding slot.

### Witness parameters and arguments

A witness parameter is written `use Concept(args)` in a function type or definition telescope. It is anonymous but joins the witness scope of the function body. Its type must reduce to a concept application, since resolution answers nothing else: any other type is refused where the parameter is declared — in a signature, a witness telescope, or a lambda's annotation — and a proof meant to be discharged is an implicit `@` parameter instead.

```crs
pub let join(@A: Type, use Show(A), values: List(A)) -> Str =
    List/fold(values, "", (value, result) => Str/concat(result, Show/show(value)));
```

At a call site, `use value` supplies a witness argument explicitly and overrides resolution.

```crs
join([1, 2, 3])
join(use custom_show, [1, 2, 3])
```

### Witness resolution

An omitted witness argument is resolved in this order:

1. Search local `use` parameters from innermost to outermost; the first direct match wins.
2. Search superclass projections of local witnesses breadth-first; more than one match at the same minimum depth is ambiguous.
3. Look up the concept and the rigid heads of every parameter in the global witness table.

If any concept parameter is still headed by an unsolved metavariable, resolution waits until that metavariable is solved. A selected global witness is instantiated with fresh implicit arguments, its witness premises are resolved recursively, and its full result type is unified with the goal. A rigid key with no entry yet defers rather than failing, so a witness may be declared after the code that resolves through it; what is still deferred once the unit has elaborated is reported then.

Higher-kinded parameters are keyable. When conversion establishes a shape such as `M(A) = Option(Nat)`, it may infer `M` as the `Option` type constructor, allowing `Monad(Option)` lookup. An under-applied shape such as `M(A) = State(S, Nat)` infers `M` right-biasedly, as `(A) => State(S, A)`: the final argument is the abstracted one, which is why a family intended as a monad orders its parameters context first and result last.

## Foreign declarations

A `foreign` declaration introduces a value implemented by the embedder. Its declared type uses the wire grammar rather than arbitrary Curios types.

```crs
foreign random: Nat;
foreign frobnicate: (Nat, Bytes) -> Nat;
pub foreign log: (Bytes) -> Nat;
foreign close: (Handle) -> {};
foreign read: (Handle, Nat) -> {status: Nat, bytes: Bytes};
```

The wire types are `Nat`, `Int`, `Bool`, `Flt`, `Bytes`, `Bits`, `Handle`, and `List(T)`, spelled bare: the wire grammar is a closed vocabulary that resolves no names, so `/std/Nat` is refused where `Nat` is meant. Eight words that look like types and are not. A `Nat` or `Int` crosses as a 32-bit word although the program's are unbounded: a `Nat` argument crosses below `2³¹` and an `Int` between `-2³¹` and `2³¹ - 1`, and a larger one stops the program rather than crossing changed. A wire signature is a wire result for a zero-argument foreign, or a parenthesized wire parameter list followed by `->` and a wire result.

A wire result is a wire type, or a braced list of labelled wire types — the [tuple type](#tuple-types) the call yields. `{}` is no result at all, which is the unit type; `()` is the unit value and never stands here. Two or more fields are the tuple the guest projects by name, as `/std` reads `.status` and `.bytes` off a host read.

Two spellings are refused, and both for the same reason: a tuple type's labels are part of its identity, so nothing may be invented, dropped or moved. A single result is written bare rather than as `{value: T}`, because one result crosses as itself and a one-field brace would name a tuple the row cannot carry. And a reference result — `Bytes`, `Bits`, `Handle` or `List(T)` — is written last, because that is the one slot it may take; a signature that puts one earlier is refused rather than reordered, since the reordered tuple would be a different type from the one declared.

`Bytes` and `Bits` are distinct wire types over one payload. A row states which grain it means, and a declaration is where the embedder and the program agree on it: `(Bits) -> Bits` says the datum is a bit run where both sides read it, and the guest builds `Bits` directly rather than a `Bytes` the call site converts. What crosses is the same flat byte array either way — outbound the run's `ceil(len / 8)` packed bytes, inbound that payload sealed at eight times its byte count. **The wire carries no bit length**, so a `Bits` run whose length is not a multiple of eight crosses as its packed bytes, trailing padding zeroed, and returns at `8n`; a host with a 20-bit datum sends its own length in band, as it would anyway. `Byte` is not a wire type.

`List` does not nest, and its element vocabulary is narrower still: an element must be `Nat`, `Int`, `Bool`, `Bytes`, `Bits`, or `Handle`, so `List(List(T))` and `List(Flt)` are both rejected. A `Flt` crosses on its own as a raw binary64; carrying one inside a list would mean boxing each element, which no operation has asked for. `List` is in practice reachable only from builtin `/sys` operations — an embedder implementing a `foreign` declaration binds it through typed host closures, and the shapes those provide are the ones the builtins use. How the declaration reaches the embedder is the host ABI's concern rather than the surface language's.

## Equality and proofs

Propositional equality `Eq` is an ordinary indexed inductive proposition from `/std/Eq`. Its proofs use the same constructors, functions, and match forms as other inductives; `Eq` gets no syntax of its own, which is the point.

```crs
pub let sym(@A: Type, @x: A, @y: A, proof: Eq(x, y)) -> Eq(y, x) =
    match proof: (left, right, p) => Eq(right, left)
    | refl(@value) => Eq/refl()
    end;
```

The standard equality operations include reflexivity, symmetry, transitivity, congruence, and substitution. `Eq` is propositional equality; `Eql` is the value-level concept used by `==` and `!=`.

## Quick reference

| Form | Meaning |
| --- | --- |
| `-- ` | Line comment |
| `--- ` | Documentation comment, attached to the declaration below it |
| `{}` / `()` | Unit type / unit value |
| `@A: Type` / `@value` | Implicit binder / explicitly supplied implicit argument |
| `use C(A)` / `use value` | Witness binder / explicitly supplied witness argument, or superclass field of a concept literal |
| `?` | Written goal — reports scope, type and fits, then fails compilation |
| `term!` | Monadic bind through `Monad`, lifting a cross-monad action through `Lift` |
| `"""` … `"""` | Block string literal — the lines between the delimiters, their shared indentation removed |
| `b[...]` / `x[...]` | `Bits` / `Bytes` literal — grain letter glued to the bracket |
| `Name { ... }` | Structure or concept literal |
| `Name { ..base, ... }` | Structure update |
| `match term ... end` | Typed elimination or dispatch |
| `choose ... end` | Ordered guarded ladder |
| `test name = body;` | Declared test — a `/std/Test` description, collected per unit and run by `curios test` |
| `satisfy C(args) { ... }` | Globally registered anonymous witness |
| `satisfy C(args);` | Derived witness — the compiler writes the body |
| `satisfy (@A: Type, use C(A)) => D(args) { ... }` | Witness under a telescope |
| `use /std/{Nat};` / `use /std/*` / `use /{Name};` | Import a group, the exported surface, or a name from the compilation root |
| `… and …` | [Recursive group](#recursive-groups) — `let`, `induct`, `struct`, `concept` or `satisfy` |
