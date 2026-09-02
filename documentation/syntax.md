# Syntax

This document defines the surface language accepted in `.crs` files. It is a reference for writing and reading Curios programs, not a description of compiler internals. An implementation disagreement is a language conformance bug: either the implementation or this document must be corrected.

Examples use declarations from `/std` and `/syn`. The authored libraries under `curios-prelude-archive/std/` and `curios-prelude-archive/syn/` are the main corpus of complete programs.

- [Lexical structure](#lexical-structure)
- [Literals](#literals)
- [Sorts and types](#sorts-and-types)
- [Expressions](#expressions)
- [Operators](#operators)
- [Pattern matching](#pattern-matching)
- [Guarded ladders (`choose`)](#guarded-ladders-choose)
- [Declarations and modules](#declarations-and-modules)
- [Inductive declarations](#inductive-declarations)
- [Structure declarations](#structure-declarations)
- [Concepts and witnesses](#concepts-and-witnesses)
- [Foreign declarations](#foreign-declarations)
- [Equality and proofs](#equality-and-proofs)
- [Quick reference](#quick-reference)

## Lexical structure

### Whitespace and comments

Spaces, tabs, and newlines separate tokens but otherwise have no meaning. Some operators require surrounding whitespace, as specified in [Operators](#operators).

A line comment begins with `--` and continues to the end of the line. There are no block comments.

```crs
-- A complete line comment.
let n = 1; -- A trailing comment.
n
```

Every comma-separated list — parameter and argument lists, tuple and struct fields, list literals, import groups — admits one optional trailing comma before its closing delimiter. A comma alone does not form an empty list.

### Identifiers

An identifier is a nonempty sequence of Unicode alphanumeric characters and `_`.

The following words are reserved and cannot be used as path segments:

| Declaration and expression words | Literal words |
| --- | --- |
| `let`, `match`, `choose`, `mod`, `use`, `pub`, `end`, `induct`, `struct`, `foreign` | `true`, `false` |

`concept`, `satisfy`, and `and` are contextual words. They are recognized only in the grammatical positions that use them and remain valid identifiers and path segments elsewhere. `Type` and `Prop` denote sorts when parsed as terms, but they are not globally forbidden path segments.

### Paths

A path is one or more identifier segments separated by `/`. A leading `/` makes the path absolute; otherwise name resolution begins in the current lexical and module scope.

```crs
Nat                 -- relative name
Option/some         -- member of Option
/std/List           -- absolute name
/sys/Handle         -- absolute intrinsic declaration
```

A path is whitespace-free: every separator touches both of its neighbors. Infix operators are the opposite — they require whitespace on both sides (see [Operators](#operators)) — so `a/b` is only ever the path and `a / b` only ever the division, and the asymmetric spellings `a/ b` and `a /b` satisfy neither grammar. A packed `Bits` or `Bytes` literal glues its grain letter to the opening bracket and admits whitespace freely thereafter; see [Packed literals](#packed-literals).

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

Elaboration chooses `Nat`, `Bool`, `Byte`, `Int`, or `Flt` from context. A written sign excludes `Nat`, `Bool`, and `Byte`. `Byte` is selected only by an expected `Byte` type and accepts values from `0` through `255`; `Bool` is selected only by an expected `Bool` type and accepts `0` and `1`. An unconstrained unsigned integer defaults to `Nat`; an unconstrained signed integer defaults to `Int`.

`-42` is one literal. `- 42` is parsed as an operator occurrence and is not a signed literal.

A floating-point literal has a decimal point followed by at least one decimal digit. It may have a sign and an `e` or `E` exponent.

```crs
5.0
-0.5
1.0e9
```

Floating-point literals have type `Flt`. `5.` is not a floating-point literal.

### Character and string literals

A character literal contains one Unicode scalar value or one supported escape. It is a polymorphic literal spelled by its scalar value: it realizes as the proof-certified `Char` wherever nothing pins it, and as `Nat`, `Byte`, or `Int` — the code point — where one of those is expected, under the same rules as a numeral (`Byte` refuses a code point past `255`; `Bool` and `Flt` never realize from a character). `Char` excludes the surrogate range and values above `U+10FFFF`; use `Char/to_nat` for an explicit code-point conversion of a *value*, whose type is already fixed. In a match, a character literal is a `Nat` dispatch case — see [Natural-number dispatch](#natural-number-dispatch).

```crs
'c'
'\n'
'\''
```

Character escapes are `\n`, `\t`, `\r`, `\\`, and `\'`. An unrecognized escape in a character literal is a parse error.

A string literal has type `Str`.

```crs
"hello"
"first\nsecond"
```

String escapes are `\n`, `\t`, `\r`, `\\`, and `\"`. An unrecognized escape in a string literal is not an error: the backslash and the following character both stand for themselves, so `"\%"` is the two-character string `\%`. This is unlike a character literal, where an unrecognized escape is a parse error.

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

An entry is a term contributing one atom — a `Bool` in a `Bits` literal, a `Byte` in a `Bytes` literal — or a `..` spread contributing a whole packed value of the same kind. A constant atom is a [numeric literal](#numeric-literals) realized at the grain's element type: `0` or `1` in a `Bits` literal, `0` through `255` (any radix) in a `Bytes` literal. A [character literal](#character-and-string-literals) is a constant atom of a `Bytes` literal when its code point fits the byte — `x['H', 'i']` — and no character is a bit.

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

Only the grain letter's junction with `[` is tight: `b [1]` is the binder `b` followed by a list literal, and an identifier merely ending in the grain letter never begins a packed literal. Past the `[`, the literal lexes like any other bracketed list — whitespace is free, one trailing comma is admitted, and entry and spread operands are arbitrary terms needing no parentheses. `Bits` and `Bytes` cannot be mixed.

Adjacent constant atoms lower to a single packed constant rather than a chain of appends, so a literal written entirely from numerals is compile-time constant data with no marker needed to say so.

## Sorts and types

### Sorts

`Type` is the sort of computational types. `Prop` is the sort of proof-irrelevant propositions.

Although the surface spelling is always the nullary term `Type`, each occurrence has an implicit level in a cumulative hierarchy. The compiler infers those levels and generalizes reusable declarations over them; there is no syntax for universe variables, levels, or explicit universe arguments. A type accepted at one level is also accepted where a higher level is required.

All inhabitants of the same proposition are definitionally irrelevant. Eliminating a proposition into informative data is restricted; proofs may always be eliminated to prove another proposition.

### Function types

A function type is a parenthesized dependent parameter list followed by `->` and its result.

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

A literal is measured against the labels of its expected type position by position. An unlabeled literal checks against a labeled type and takes its labels from it, so `(1, true)` is a `{a: Nat, b: Bool}` where one is expected. A labeled literal is refused where the expected type's label at that position differs or is absent; fields are never reordered to match. A literal with no expected type — an unannotated `let`, a projection head — synthesizes the non-dependent product with the labels it wrote: `(a = 1, b = true)` is a `{a: Nat, b: Bool}`, and `(1, true)` a `{Nat, Bool}`, which no later annotation can relabel. A labeled tuple is projected by position or by label: `z.0` and `z.a` name the same field.

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

Calls, projections, and postfix `!` may be chained.

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

An omitted implicit or witness binder is inserted automatically from the expected function type, so hidden binders may be left out when the body does not name them. Alignment is positional by plicity: among the parameters of the expected type, each written binder claims the next slot of its own plicity, and every skipped implicit or witness slot before it is inserted. A plain binder never silently binds a hidden slot. For the expected type `(@A: Type, use Show(A), value: A) -> Str`, every one of `(value) => …`, `(@A, value) => …`, `(use show, value) => …`, and `(@A, use show, value) => …` is accepted; `(A, show, value) => …` is not, because `A` binds the sole explicit slot and the remaining binders are surplus.

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

A binding is in scope of its own value, so a local function may call itself. A binding that mentions itself states its type, since a body that mentions the binding cannot be the source of it, and is a plain name rather than a pattern; a binding whose value performs `!` cannot mention itself, since the action runs before the binding exists. Bindings that mention one another are declared as one group with `and`, every member after the first a plain name with a type.

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

Because a binding is in scope of its own value, `let n = n + 1;` names the binding it declares rather than an outer `n`, and is refused as the recursive value it is: a value may mention itself only under a lambda, where it is a recursive value computed the first time it is read.

### Irrefutable binder patterns

The binders of `let`, lambdas, function-definition sugar, and the `;` fold-hypothesis position of `Nat`/`List`/`Bits`/`Bytes` match arms accept nested tuple and struct patterns.

```crs
let (x, (_, y)) = value;
let Point { loc = (x, y), color } = point;
body
```

These patterns are projection sugar, not runtime matches. The struct head is documentary and is not resolved or checked. An unlabeled field is matched positionally; a `label = pattern` field projects that label. Field punning such as `Point { x, y }` is the positional form, whose sub-patterns happen to be binders named after the fields.

Refutable patterns belong only to `match`.

### Written goals

`?` is a development goal. It asks the elaborator to infer as much as possible, records the local scope and expected type, and then causes compilation to fail with a report.

```crs
let compose(@A: Type, @B: Type, @C: Type, f: (B) -> C, g: (A) -> B) -> (A) -> C =
    ?;
compose
```

A goal is never accepted in a successfully compiled program.

### Postfix `!`

`action!` is monadic sequencing. Each occurrence is equivalent to a call to `/syn/Monad/bind(action, continuation)` in the monad of its region.

```crs
let parser: Parse(Nat) =
    let a = Parse/any_byte!;
    let b = Parse/any_byte!;
    Parse/pure(a + b);
```

Every value body is a sequencing region. Lambda bodies, match arms, and recursive member bodies begin fresh regions; the tail after a local `let` remains in the same region. There is no `let !` header or matching `end`.

A region's monad is read from the region's type and never inferred from a sequenced action. A region whose type is not yet known waits for it, and one whose type can never name a monad — the body of a lambda in inference position, say — is rejected with a request to annotate the enclosing result type.

An action whose own monad differs from the region's is lifted: the `!` wraps the action in `/syn/Lift`'s `lift`, and the declared `Lift` witness for that ordered pair of monads carries it into the region. A pair with no declared witness is rejected. See [Lifting between monads](#lifting-between-monads).

Postfix `!` is not allowed in types. The token `!=` is an infix operator and is not parsed as postfix `!` followed by `=`.

### Host effects and `Io`

Every operation that touches the host — writing a handle, reading a clock, allocating or reading a cell, calling a `foreign` function, exiting — has result type `Io(T)`. An `Io(T)` is a *description* of a computation yielding a `T`, not the `T`. Calling such an operation performs nothing; it builds a description.

```crs
use /std/{Io, print};
let greeting: Io({}) = print("hello");   -- nothing has been printed
```

`Io/pure` wraps a value as a description that performs nothing, and `Io/bind` sequences one description into another. Postfix `!` is the ordinary sequencing form and reaches `Io` through its `Monad` witness like any other monad:

```crs
use /std/{Io, print};
let shout(s: Str) -> Io({}) =
    let _ = print(s)!;
    print("!\n");
```

**There is no operation taking an `Io(T)` to a `T`.** A description is performed only by being the program's tail, which the emitted entrypoint forces once. So a function whose result type is not an `Io` cannot perform an effect, and a `!` may only appear in a region whose type is a monad — a `(Str, Bool) -> Bool` has nowhere to sequence one.

```crs
use /std/{print};
let probe(tag: Str, r: Bool) -> Bool =
    let _ = print(tag)!;   -- rejected: this region's type is `Bool`, not a monad
    r;
```

Binding a description does not perform it, and forcing one twice performs it twice:

```crs
use /std/{Io, print};
let once: Io({}) = print("x");
let _ = once!;
once                            -- prints "x" twice in total
```

`Io` is not matchable: it has no constructors to enumerate, so a `match` over one is rejected.

### Lifting between monads

`/syn/Lift(M, N)` declares the canonical embedding of monad `M` into monad `N`: one method, `lift`, taking an `M(A)` to an `N(A)`, with `Monad` witnesses for both sides as superclasses — so an embedding between non-monads cannot be declared. Like every witness, one `Lift` witness may occupy each ordered pair of monads program-wide, so which embedding runs is a fact about the program, never about a call site.

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

The explicit spelling `lift(action)` names the same embedding, with the target monad inferred from the region.

Embeddings never chain. Declaring `Lift(Io, Job)` and `Lift(Job, Sched)` does not let an `Io` action sequence in a `Sched` region: the missing `Lift(Io, Sched)` is reported, together with any chain of declared embeddings that would have reached it, and the composite embedding is declared like any other — a decision about `Sched`, written by its author, not derived by the compiler.

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
| 3 | `==`, `!=`, `<`, `>`, `<=`, `>=` | `Equal`, `Compare` |
| 4 | `+`, `-` | `Add`, `Subtract` |
| 5, tightest | `*`, `/`, `%` | `Multiply`, `Divide`, `Remainder` |

Both operands of an operator have the same type. `==` and `!=` are two separate methods of `Equal`, `eql` and `neq`, so a witness supplies both; `!=` is not a negation applied to `eql`.

An operator's result type is whatever its `/syn` method declares: `+`, `-`, `*`, `/`, `%`, `&&` and `||` return the operand type, while `==`, `!=`, `<`, `>`, `<=` and `>=` return `Bool`.

`/` and `%` additionally carry the precondition their concept declares. `Divide` and `Remainder` each have an `Ok(A) -> Prop` field, and the operator inserts an implicit proof of `Ok(divisor)` — so `a / b` on `Nat` must discharge `Nat/Lt(0, b)`. A carrier whose division is total states `True` and pays nothing, which is what keeps `/` a single operator over carriers that disagree about whether it can fail.

Operator notation always uses witness resolution, including intrinsic operands. Standard witnesses cover the intrinsic types, while a `satisfy` declaration enables the same notation for a user-defined type.

## Pattern matching

### Match shell and motives

A headed match has a scrutinee, an optional motive, one or more `| pattern => body` arms unless the eliminated type is empty, and a closing `end`.

The motive states the result type as a family. It is an ordinary term, checked against the eliminator's motive type — a function of the scrutinee's indices, in declaration order, and then the scrutinee:

```text
(indices) -> Scrutinee(indices) -> Sort
```

There is no motive grammar. What follows `:` is parsed as a term and terminates at the first arm, since `|` is not an infix operator.

```crs
match b: (_) => Nat                            -- result ignores the scrutinee
match n: (m) => P(m)                           -- result depends on it
match p: (s, t, q) => Eq(t, s)                 -- an indexed family
match p: (s: A, t: A, q: Eq(s, t)) => Eq(t, s) -- with written annotations
match p: discriminates_eq                      -- a named family
match v                                        -- omitted; inferred
```

The number of binders is fixed by the eliminated type: one per index, then one for the scrutinee. A non-indexed scrutinee — every intrinsic carrier, and any inductive declared without an index telescope — therefore takes exactly one binder, so a result that ignores the scrutinee is written `(_) => T`. `Vec(T)` has one index and takes two binders; `Eq` has two and takes three.

Parameters are never binders. They are uniform across constructors and fixed by the scrutinee's type, so the motive body refers to them through the ambient scope, exactly as the declaration side states only index expressions in a constructor's case target.

Each arm is checked against the motive at that constructor's target indices, and the match as a whole at the scrutinee's actual indices. A `| _ =>` default binds nothing and refines no index, so it is checked at the actual indices too.

A binder may be written bare, as `_`, or annotated. An annotation is an ordinary type in an ordinary position: it is checked by conversion against the binder's expected type, obeys the usual plicity rules, and may name the binders written before it. Annotating the scrutinee binder is how a reader recovers the eliminated family on the motive line.

```crs
match p: (s, t, q: Eq(s, t)) => Eq(t, s)
```

Omitting the motive asks the elaborator to infer it. Prefer omission wherever inference succeeds; a written motive is needed where there is nothing to infer from — a type-level match whose result appears in a signature, or an elimination in inference position.

A motive may only be written where the head dispatches directly: every arm's top-level pattern must be the same dispatchable shape. A tuple-scrutinee matrix, a struct-headed match, or a plain-binder match builds no core eliminator for the motive to attach to, and rejects one.

### Inductive patterns

An inductive pattern names a constructor and supplies one pattern per payload position.

```crs
match option
| some(value) => use(value)
| none() => fallback
end
```

A payload position the constructor declared implicit (`@`) must be matched with `@`; a plain payload is matched without a mark. A constructor pattern supplies one pattern per payload position, hidden ones included — omitted hidden payload patterns are not inserted (unlike lambda binders). Witness payloads are not a surface feature, so `use` is not accepted in a constructor pattern.

```crs
match vector
| nil() => fallback
| cons(@length, head, tail) => head
end
```

A pattern may nest *inside* a constructor, tuple, or struct field. The operands of the `Nat`, `List`, `Bits`, and `Bytes` leaves are plain binder names rather than patterns — `[some(x), ..tail]` is not a pattern — while the `; ih` binding takes a full irrefutable pattern.

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

Only a bare `_` in this exact position is a default. A named binder is not a catch-all. Nested wildcard defaults are not accepted. A lone `_` with no concrete arm is an irrefutable binder match rather than an inductive default.

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

A dispatch literal is a numeric literal or a character literal — the latter matching its scalar value, so `match Char/to_nat(c) | '\n' => … | _ => … end` is how a `Char` is dispatched, with the conversion visible at the head. A dispatch literal must fit in 32 bits; a larger numeral is a parse error, even though `Nat` itself is unbounded.

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

`Bits` and `Bytes` use their literal grain letters to select the carrier, and their arms take the same shape as a [list fold](#list-fold-and-case-split). The nonempty arm binds the leading element and tail; an optional binding after `;` receives the fold result for the tail, as a plain name or an irrefutable tuple/struct pattern. A `Bits` head has type `Bool`; a `Bytes` head has type `Byte`. Both cases are required here too, and a trailing `| _ =>` may stand in for the missing one.

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

A condition arm fires when its expression evaluates to `true`. A bind arm evaluates the expression on the right of `=` and fires when it matches the refutable pattern on the left. A bare binder is not allowed as a bind-arm pattern because it cannot fail; use `let` instead.

Each selected arm receives the same definitional refinement that an equivalent nested headed match would provide.

## Declarations and modules

An entrypoint consists of zero or more top-level items followed by exactly one final term — the description the program performs. That term has type `Io({})`: a program describes doing something and yielding nothing, so a tail that computes a result must discard it explicitly rather than have the result dropped for it. A module file consists of top-level items only and has no final term.

### Top-level definitions

Top-level `let` declarations require a type annotation. Function-definition sugar supplies the annotation as a parameter telescope and result type.

That requirement is also what separates items from the final term: an *unannotated* top-level binding in an entrypoint is not an item at all, but a local `let` opening the final term. The difference is not only scope. An item's value body is its own sequencing region, so a `!` written in it sequences within that definition; a local `let`'s value shares the final term's region, so a `!` written there sequences with the rest of the program.

```crs
pub let zero: Nat = 0;

pub let map(@A: Type, @B: Type, value: Option(A), f: (A) -> B) -> Option(B) =
    match value
    | some(x) => Option/some(f(x))
    | none() => Option/none()
    end;
```

A top-level definition is in scope of its own body, so it may recurse with nothing said. Definitions that reference one another are declared as one group with `and`; each member takes its own `pub` marker — before `let` for the first member and before `and` for each later member — and one `;` terminates the whole group. Two definitions that reference each other without being declared as a group are refused, naming both.

### Test declarations

A `test` declaration declares a named test: a description of type `/syn/Test`, built with the combinators `/std/Test` exports. The parentheses are required and hold the telescope a `let` signature holds: the declaration is the function-definition sugar it lowers to — a definition of declared type `(params) -> /syn/Test`, its body checked under the lambda binding every parameter.

```crs
use /std/{Nat, Test};

test the_answer_holds() =
    Test/check(21 * 2 == 42);

test add_commutes(n: Nat, m: Nat) =
    Test/check(n + m == m + n);
```

A test with empty parentheses runs once. A test with parameters is a claim about every instantiation of them, and the runner takes the strongest discharge it can: a body the kernel settles under the whole telescope — `Test/refl(n + 0, n, Eq/refl())` — is closed through `Test/settled` and reports `proved`, exactly as its nullary twin would; any other body is a *property* — exhausted over every value when its parameters' types are finite and small enough (`Bool`, `Ordering`, `Byte`, `Option` and tuples of those) for the whole domain to fit the case budget, probed over drawn arguments otherwise. What `curios test` reports for one, and how a counterexample is spelled, is [Testing](usage.md#testing). Which types can be drawn is the standard library's roster of `/std/Test/Draw` witnesses, each declared in the module owning its carrier, and a program writes `Draw` for its own types: `draw` for a sample, and `all` for the whole domain when there is a small one. The property is resolved as a witness of `/std/Test/Property` at the function type over the test's *explicit* parameters: a `use` premise in the telescope is resolved rather than drawn, so `test reflexive(use Equal(Nat), n: Nat)` probes `n` alone, and an implicit is solved from the explicit parameters that mention it. A parameter of a type nothing draws, an implicit nothing fixes, or a dependent telescope, such as a proof about an earlier parameter, is reported at the declaration as that missing witness.

`test` is contextual: it is a keyword only where an item may start, and `test` stays an ordinary name everywhere else. A test is never `pub` — its name is its report line, not an export — but it is otherwise registered like a private definition: referable within its subtree, and colliding with a sibling declaration of the same name. Being referable is what makes a parameterized test a family: `Test/all(List/map(cases, ((a, b)) => add_commutes(a, b)))` is the same claim over a table the author wrote, one test whose failure names the case's position. The body is its own sequencing region typed at `Test`, which is no monad, so a bare `!` is refused where it is written; an effectful test enters `Io` through `Test/perform`'s thunk. Each unit's tests are collected in declaration order.

### Modules

A file-backed module ends its declaration with `;` and loads `Name.crs`. An inline module ends with `end`.

```crs
pub mod Nat;

pub mod Internal
    pub let value: Nat = 1;
end
```

A header's file-backed modules live in its **stem directory**. `mod Nat;` written in `foo.crs` loads `foo/Nat.crs`, and `Nat`'s own file-backed modules load from `foo/Nat/`. One rule governs every file in the language, so the file handed to `curios run` is a header like any other: `mod Nat;` in `main.crs` loads `main/Nat.crs`.

A package's library header is the single exception, and it is a fact about package layout rather than about the language — see [What a package is made of](usage.md#what-a-package-is-made-of). A stem is never part of a name: neither `main` nor `lib` can be written in a path.

### Imports and re-exports

`use` imports through a group: a braced list `path/{…}` or a glob `path/*`. There is no bare `use path;` form — a single import is written `use /std/{Nat};`. Prefixing it with `pub` re-exports what it imports.

```crs
use /std/{Nat, Bool};
pub use Option/*;
use /std/Nat/{Lt};
```

Inside a group, a bare name imports both a child module and a value with that name when both exist. `mod Name` imports only the module namespace; `let Name` imports only the value namespace.

```crs
pub use List/{let List};
use Package/{mod Syntax, let parse};
```

### Visibility

One rule governs every declaration, in both namespaces:

> A declaration written **without** `pub` in module `M` is visible exactly within `M`'s subtree — `M` itself and its descendants at any depth. A declaration written **with** `pub` is additionally visible wherever `M` itself is visible.

Reachability along a path is the conjunction of that rule at each hop, and the root's subtree is the whole program. So a descendant may name its ancestors' private declarations, while ancestors and siblings may not: `Owner/Worker` can reach a private binding of `Owner`, but neither `Owner` nor a sibling `Owner/Other` can reach a private binding of `Owner/Worker`. `pub` inside a private module means "wherever this module is visible", which is that module's own audience rather than the whole program — the facade pattern, where a public module re-exports selected names out of a private child.

`struct`, `induct`, and `concept` have a second, declaration-local `pub` before their result sort; this independently exposes their representation, under the same subtree rule. A private representation is transparent throughout its declaring module's subtree, so an abstraction can be implemented across several files without exporting how it is built.

Globs are the exception: `use M/*` and `pub use M/*` import the exported surface only, never a subtree-private declaration. Reaching one always requires naming it.

A public interface cannot mention an item its own consumers cannot reach. The interface includes:

- parameters, indices, and result sorts of publicly reachable nominal declarations;
- struct and concept fields when the representation is public;
- inductive constructor signatures when the inductive representation is public;
- declared types of definitions.

The check compares audiences rather than declaration paths, so a name re-exported out of a private child counts as visible wherever the re-export puts it, and an item that reaches only a subtree may freely mention other declarations of that subtree. It follows re-exports, identity aliases, and direct-headed type-family aliases whose declared result structurally ends in literal `Type` or `Prop`. Ordinary definition bodies are not part of the public interface, and neither are the signatures of members synthesized into a nested namespace — an inductive's constructors and a concept's method wrappers — so a constructor facade may hand out values of a type the consumer cannot name.

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
pub induct Vec(T: Type): (length: Nat) -> pub Type
| nil(): (0)
| cons(@n: Nat, head: T, tail: Vec(T, n)): (n + 1)
end
```

Each index binder may be named or left bare — `(length: Nat)` and `(Nat)` are both well-formed — and an index never takes `@`. The name is never in scope in the constructor cases; it appears in the family's printed signature, and a later entry of the same telescope may depend on it. That dependency is what makes the annotation a telescope rather than a list of types:

```crs
pub induct Tagged: (size: Nat, contents: Vec(Nat, size)) -> pub Type
| tag(@size: Nat, @contents: Vec(Nat, size)): (size, contents)
end
```

Each constructor of an indexed family must state the indices it produces after `:`. A non-indexed constructor does not accept a target.

An inductive proposition is declared with `Prop`:

```crs
pub induct Eq(@A: Type): (left: A, right: A) -> pub Prop
| refl(@value: A): (value, value)
end
```

The outer `pub` exports the family name. The inner `pub` exports construction and every form of elimination. Without the inner marker, constructor access and pattern matching are restricted to the declaring module's subtree.

Mutually recursive inductives are separated by `and` within one block. Each member has its own outer and representation visibility markers.

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

The outer `pub` exports the type name. The inner `pub` exports construction and projection. Without the inner marker, those operations are restricted to the declaring module's subtree.

A `Prop` structure may contain only non-informative fields.

Structures whose fields name one another are declared as one group with `and`; each member takes its own `pub` markers, before `struct` for the first and before `and` for each later one. A lone structure may name itself in its fields with nothing said.

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

A concept has zero or more parameters, a required representation sort, and a field list. The representation sort follows the struct rules: `: pub Type` declares a transparent concept, and `: Type` a *sealed* one, whose representation is private to its declaring module's subtree — witness declarations, dictionary literals, structure updates, and raw field projections are then permitted only there. Resolution, `use` parameters, and the generated method wrappers work the same either way, and visibility of the concept's name remains independent of its representation.

```crs
pub concept Show(A: Type): pub Type {
    show(A) -> Str,
}

pub concept Monad(M: (Type) -> Type): pub Type {
    pure(@A: Type, value: A) -> M(A),
    bind(@A: Type, @B: Type, action: M(A), next: (A) -> M(B)) -> M(B),
}
```

Every ordinary field receives a wrapper in the concept's namespace, so `Show/show(value)` asks for an implicit witness of `Show(A)` and projects its `show` implementation.

Concepts whose method types name one another's dictionaries are declared as one group with `and`, as structures are. A superclass cycle — `use B(A)` in `A` and `use A(B)` in `B` — is refused whether or not the two are declared together, since resolution could never discharge it.

The field list is a dependent telescope: later fields may refer to earlier named fields. In a generated wrapper such a reference becomes the corresponding projection of the resolved witness, so the wrapper's type constrains that witness's own implementations.

```crs
pub concept Idem(A: Type): pub Type {
    op(A) -> A,
    law(x: A) -> Eq(op(op(x)), op(x)),
}
```

A field whose type is a proposition about earlier fields is a law. `satisfy` cannot register a witness for such a concept without supplying a proof that discharges the law at the implementations that witness supplies, so a witness violating it is rejected where it is declared.

A field's result may itself be a sort, which makes the field an associated type each witness chooses. `Divide`'s `Ok(A) -> Prop` is what lets every carrier state its own division precondition, and a witness supplies it with the same field sugar as any other:

```crs
satisfy Remainder(Nat) {
    Ok(b) = Nat/Lt(0, b),
    rem = rem,
}
```

A field beginning with `use` is an anonymous superclass edge. Its type must be a concept application.

```crs
pub concept Ordered(A: Type): pub Type {
    use Equal(A),
    cmp(A, A) -> Ordering,
}
```

A local `Ordered(A)` witness can therefore satisfy an `Equal(A)` goal by superclass projection.

A sealed concept's fields are not part of its public interface: a `pub` sealed concept may reference private names in its field types — a private superclass is a hidden obligation that resolution discharges without the consumer naming it. A transparent `pub` concept's field types are interface and must be `pub` themselves.

A concept returning `Prop` (or `pub Prop`) has proof-irrelevant witnesses that erase completely.

### Witness declarations

`satisfy` registers an anonymous witness. Its terminal type is a concept application and its body supplies the concept fields — or is omitted, asking the compiler to write it; see [Derived witnesses](#derived-witnesses).

```crs
satisfy Show(Nat) {
    show(n) = Nat/to_str(n),
}
```

A witness may quantify over implicit parameters and require other witnesses. A nonempty telescope is separated from the concept application by `=>`. It cannot declare explicit parameters because resolution has no explicit arguments to supply.

```crs
satisfy (@A: Type, use Show(A)) => Show(List(A)) {
    show(values) = List/fold(values, "", (value, result) => Str/concat(result, Show/show(value))),
}
```

Every registered witness is keyed by the concept name and the tuple of rigid heads of every concept parameter. Each head must reduce to an inductive, structure, intrinsic type, tuple type, function type, or supported higher-kinded type constructor — including a *partially applied* family written as a lambda, `(A: Type) => State(S, A)`, which keys on the applied head. Remaining arguments below those heads are checked by unification after lookup.

A tuple type is keyed by its *shape*: the label at each field position, arity implied, field types excluded. Labels are part of a tuple type's identity, so `Show({Nat, Bool})`, `Show({a: Nat, b: Bool})` and `Show({x: Nat, y: Bool})` are three keys for three types, and a witness for one does not serve another. `{}` keys as the empty shape, and a constructor whose body is a tuple type — `let Pair(A: Type) -> Type = {Nat, A};` — keys on that body's shape in the higher-kinded position. The standard library writes tuple-keyed witnesses for the positional shapes in `/std/Tuple`, whose header states which concept reaches which arity and why the ceiling sits where it does; a labeled product wanting the same is written as a `struct`.

```crs
satisfy (@A: Type, @B: Type, use Show(A), use Show(B)) => Show({A, B}) {
    show(t) = Str/concat("(", Str/concat(Show/show(t.0), Str/concat(", ", Str/concat(Show/show(t.1), ")")))),
}
```

A function type is keyed by its *plicity vector*: the mark at each parameter position, arity implied, domains and result excluded. Plicity and arity are part of a function type's identity, so `Tag((Nat) -> Nat)`, `Tag((@n: Nat) -> Nat)` and `Tag((Nat) -> (Nat) -> Nat)` are three keys for three types, and a witness for one does not serve another — while binder names are not part of it: `(a: Nat) -> Nat` and `(b: Nat) -> Nat` are one key. `() -> A` keys as the empty vector, a distinct type from `A`, and a constructor whose body is a function type — `let Reader(A: Type) -> Type = (Nat) -> A;` — keys on that body's vector in the higher-kinded position. The result type is not in the key, so a concept commits, per shape, to one result discipline. The standard library writes function-keyed witnesses for one concept alone, `/std/Test/Property`, whose function instance has one canonical meaning — a property probed at its arity, `-> Test` the discipline at every shape. No meaningful `Show` exists at a function type, `Equal` at one is undecidable, and `Monad` at one is declined deliberately — the nominal wrapper, `/std/State`'s idiom, is how a function becomes a monad.

Two witnesses that resolve through each other are declared as one group with `and`; each member is a whole witness, with its own telescope where it has one, and the group's members register before any body elaborates. A lone witness may resolve through its own entry with nothing said; two that resolve through each other without being declared as a group are refused, naming both.

```crs
satisfy Show(Tree) {
    show(t) = match t | leaf(n) => Nat/to_str(n) | node(f) => Show/show(f) end,
}
and Show(Forest) {
    show(f) = match f | nil() => "" | cons(t, rest) => Str/concat(Show/show(t), Show/show(rest)) end,
}
```

A globally registered witness therefore requires a concept with at least one parameter. A parameterless concept can still be used through an ordinary value supplied in a local `use` scope.

For example, witnesses for `Into(Nat, Str)` and `Into(Nat, Bool)` have distinct keys. A call must determine both parameters from its explicit arguments, expected result, or an explicitly supplied witness before automatic lookup can proceed.

Only one witness may occupy a key across the whole program. Module visibility does not scope witness registration, but a *sealed* concept's representation does gate declaration: its witnesses may only be declared within the concept's declaring module's subtree.

To use a second dictionary for the same key on a *transparent* concept, construct an ordinary concept value and supply it explicitly (a sealed concept forbids the literal outside its module):

```crs
let reverse: Ordered(Nat) = Ordered { cmp(a, b) = compare_reverse(a, b) };
sort(use reverse, values)
```

### Derived witnesses

A witness may omit its body: `satisfy Spell(Point);`, or `satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));` under a telescope, and either form may join an `and` group beside written members. The signature is the programmer's — it registers, keys, and meets the orphan and sealing rules exactly as a written witness does — and the compiler writes the body from the declaration of the type in the key. Derivability is a property of the concept: `Spell` and `Equal` derive, every other concept refuses the form by name, and the hand-written witness remains the norm.

```crs
struct Point: pub Type { x: Nat, y: Nat }
induct Tree(A: Type): pub Type | leaf(A) | node(Tree(A), Tree(A)) end

satisfy Spell(Point);
satisfy (@A: Type, use Spell(A)) => Spell(Tree(A));
and (@A: Type, use Equal(A)) => Equal(Tree(A));
```

The key must be a declared `induct` or `struct` — not an intrinsic carrier, a tuple or function shape, or a concept's own record — fully applied, representation-transparent where the witness is declared, and not a proposition. An implicit payload is inferred by the re-parsed text and takes no part; a proof payload spells as the written goal `?` and compares as nothing; a payload that is itself a type is refused; every other payload goes through its own witness, resolved in the witness's scope — a telescope premise, the witness's own entry, or a member of the same `and` group — and a missing one is reported against the constructor and payload, naming the `use` premise to add when the payload's type is a telescope variable.

A derived `Spell` spells a value as its constructor, qualified by its type's own name, applied to its explicit payloads — `Tree/node(Tree/leaf(1), Tree/leaf(2))`, `Option/some(3)` — and a struct as its literal, `Point { x = 1, y = 2 }`, positionally where a field has no label; the text re-parses wherever the type's name is visible unqualified, which is wherever a value of it is written, and reads in a report as the author would have written it. A derived `Equal` is structural — the same constructor with pairwise equal payloads — and `!=` is its negation. The standard library derives both for `Option`, `Result` and `Ordering`.

### Witness premises

A witness premise must be a concept application strictly smaller than the witness's own: every variable in it is bound by the witness's telescope, no variable occurs more often in it than in the witness's concept application, and it has fewer nodes in all. A premise may therefore name a constant beside a binder — `use Lift(Io, M)` under `Lift(Io, (A: Type) => Try(M, E, A))` — while `use Show(A)` under `Show(A)` is refused. This is what makes recursive resolution structurally decreasing without fuel or tabling.

### Orphan rule

A witness may be declared only by the compilation root that owns its concept or at least one rigid type head in its key. This prevents independent third parties from defining the same globally coherent instance.

A tuple shape is owned by no root, as an intrinsic type former is. A tuple-keyed witness is therefore declared where its concept is declared, or by a privileged root: a program writes tuple witnesses for its own concepts, and cannot add one for a `/std` concept at a shape `/std` did not write.

A function type's plicity vector is owned by no root either, and there the consequence bites harder: the useful key space is nearly one point — `(_) -> _` above all — so a concept's owner claiming a shape claims it program-wide. A program writes function witnesses for its own concepts, and cannot add one for a `/std` concept at any shape.

The coordinated `/sys`, `/syn`, and `/std` roots are exempt from the restriction against one another.

### Superclass fields in literals

A concept's superclass fields remain positional slots in concept values and witness bodies. Omitting one asks witness resolution to fill it. `use value` fills the next superclass slot explicitly.

```crs
Ordered { use custom_eql, cmp(a, b) = compare(a, b) }
```

In a structure update, a spread copies superclass fields from the base. An explicit `use value` after the spread replaces the corresponding slot.

### Witness parameters and arguments

A witness parameter is written `use Concept(args)` in a function type or definition telescope. It is anonymous but joins the witness scope of the function body.

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

If any concept parameter is still headed by an unsolved metavariable, resolution waits until that metavariable is solved. A selected global witness is instantiated with fresh implicit arguments, its witness premises are resolved recursively, and its full result type is unified with the goal.

Higher-kinded parameters are keyable. When conversion establishes a shape such as `M(A) = Option(Nat)`, it may infer `M` as the `Option` type constructor, allowing `Monad(Option)` lookup. An under-applied shape such as `M(A) = State(S, Nat)` infers `M` right-biasedly, as `(A) => State(S, A)`: the final argument is the abstracted one, which is why a family intended as a monad orders its parameters context first and result last.

## Foreign declarations

A `foreign` declaration introduces a value implemented by the embedder. Its declared type uses the wire grammar rather than arbitrary Curios types.

```crs
foreign random: Nat;
foreign frobnicate: (Nat, Bytes) -> Nat;
pub foreign log: (Bytes) -> Nat;
```

The wire types are `Nat`, `Int`, `Bool`, `Bytes`, `Handle`, and `List(T)`. A wire signature is a bare wire result type for a zero-argument foreign or a parenthesized wire parameter list followed by `->` and a wire result type.

`Byte` and `Bits` are not distinct wire types. `List` does not nest: its element must be `Nat`, `Int`, `Bool`, `Bytes`, or `Handle`, so `List(List(T))` is rejected. `List` is in practice reachable only from builtin `/sys` operations — an embedder implementing a `foreign` declaration binds it through typed host closures, and the shapes those provide are the ones the builtins use. How the declaration reaches the embedder is the host ABI's concern rather than the surface language's.

## Equality and proofs

Propositional equality `Eq` is an ordinary indexed inductive proposition from `/std/Eq`. Its proofs use the same constructors, functions, and match forms as other inductives.

```crs
pub let sym(@A: Type, @x: A, @y: A, proof: Eq(x, y)) -> Eq(y, x) =
    match proof: (left, right, p) => Eq(right, left)
    | refl(@value) => Eq/refl()
    end;
```

The standard equality operations include reflexivity, symmetry, transitivity, congruence, and substitution. `Eq` is propositional equality; `Equal` is the value-level concept used by `==` and `!=`.

## Quick reference

| Form | Meaning |
| --- | --- |
| `{}` | Unit type |
| `()` | Unit value |
| `@A: Type` | Implicit binder |
| `@value` | Explicitly supplied implicit argument |
| `use C(A)` | Automatically resolved witness binder |
| `use value` | Explicitly supplied witness argument or superclass field |
| `?` | Written elaboration goal that always reports and fails compilation |
| `term!` | Monadic bind through `Monad`, lifting a cross-monad action through `Lift` |
| `b[...]` | `Bits` literal — grain letter glued to the bracket |
| `x[...]` | `Bytes` literal |
| `Name { ... }` | Structure or concept literal |
| `Name { ..base, ... }` | Structure update |
| `match term ... end` | Typed elimination or dispatch |
| `choose ... end` | Ordered guarded ladder |
| `test name(params) = body;` | Declared test — a `/syn/Test` description, collected per unit; with parameters, a claim over every instantiation: proved when the kernel settles the body, exhausted over a small finite domain, probed over drawn arguments otherwise |
| `satisfy C(args) { ... }` | Globally registered anonymous witness |
| `satisfy C(args);` | Derived witness — the compiler writes the body |
| `satisfy (@A: Type, use C(A)) => D(args) { ... }` | Parameterized globally registered anonymous witness |
| `satisfy C(A) { ... } and D(B) { ... }` | Witnesses that resolve through each other, declared as one group |
| `struct A: pub Type { ... } and B: pub Type { ... }` | Structures whose fields name one another, declared as one group |
| `concept A(T): pub Type { ... } and B(T): pub Type { ... }` | Concepts whose method types name one another, declared as one group |
| `let f(…) -> T = … and g(…) -> U = …;` | Mutually recursive group, at the top level or locally |
