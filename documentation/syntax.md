# Syntax

This is the surface language a `.crs` file is written in: each form, what it means, and what is refused. Where the compiler and this document disagree, one of them is wrong, and the disagreement is a bug.

A `.crs` file is a sequence of top-level items. A program closes with one final term, the description it performs; a module is items alone. Everything below is an item, a term, or a part of one.

Every `crs` example is a whole program or module that `curios wonder` reports nothing for, which `curios/src/tests/reference.rs` holds. A `text` example shows a form on its own. The examples name declarations from `/std`, the standard library every program may name, whose source under `curios-prelude/std/` is the largest body of Curios there is.

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

Spaces, tabs, newlines and carriage returns separate tokens and mean nothing else. Some operators need whitespace around them ([Operators](#operators)).

A byte-order mark at the head of a file is passed over. No other character that prints as nothing or as a blank may stand outside a literal or a comment: a control character; white space other than the space, a no-break space among it; a character Unicode calls default-ignorable, such as a zero-width space or joiner or a direction override; and a byte-order mark past the head. A report spells such a character by its code point, `U+00A0`. Inside a literal it stands as written.

A line comment begins with `-- `, two dashes and a space, and runs to the end of the line; a bare `--` that ends its line is an empty comment. There are no block comments. `--` glued to what follows it is refused, except as the `---` of a documentation comment.

```crs
-- A complete line comment.
let greeting = "hello\n"; -- A trailing comment.
/std/print(greeting)
```

A documentation comment begins with `--- `, or is a bare `---`, an empty line of prose. It is syntax rather than a comment: it documents the declaration after it. Consecutive `---` lines are one block, which stands before a `let` or an `and` member, an `induct`, a `struct`, a `concept`, a `satisfy`, a `foreign` or a `mod`, or before a constructor, a field or a method inside one. Blank lines and plain comments between a block and its declaration do not matter. The block above a `mod` documents the module it declares.

A block takes lines of its own, so `---` cannot follow code. A block before anything not listed above is refused, `use` and `test` included, and so are a second block before one declaration and `---` glued to what follows it, `----` included.

```crs
use /std/{Nat};

--- Twice the input.
---
--- Never overflows, since `Nat` is unbounded.
pub let double(n: Nat) -> Nat =
    n + n;
```

Every comma-separated list — parameters, arguments, tuple and structure fields, list entries, import groups — takes one trailing comma before its closing bracket. A comma alone is no empty list.

### Identifiers

An identifier is a non-empty run of Unicode alphanumeric characters and `_`.

A declaration whose name begins with `_` is kept unused on purpose: [`curios lint`](usage.md#lint) never reports it, and what it reads counts as used. `_` alone is a binder that names nothing, and `@_` is its implicit form, for a slot nothing reads. A binder has no kept form: a binder with any other name is reported where nothing reads it, whatever it begins with.

These words are reserved, and no path segment may be one:

| Declaration and expression words | Literal words |
| --- | --- |
| `let`, `match`, `choose`, `mod`, `use`, `pub`, `end`, `induct`, `struct`, `foreign` | `true`, `false` |

`concept`, `satisfy`, `and` and `test` are contextual: each is a word only where the grammar uses it, and an ordinary identifier elsewhere. `Type`, `Prop` and `Sort` are sorts where they stand as terms, and may still be path segments.

### Paths

A path is one or more identifiers separated by `/`. A leading `/` anchors it at the compilation root.

A path without the leading `/` is relative to the module it is written in: its head is a declaration of that module or a name the module imports. Enclosing modules are not searched, so an ancestor's declaration is written absolute or imported, as the report of a missing name says.

```text
Nat                 -- a declaration or import of this module
Option/some         -- member of Option
/std/List           -- absolute name
/std/Nat/lt         -- absolute name through a nested module
```

A path is whitespace-free: each `/` touches both of its neighbours. An infix operator needs whitespace on both sides, so `a/b` is always the path, `a / b` always the division, and `a/ b` and `a /b` are neither.

A unit reaches the prefixes it declares a dependency on, and `/std`, and no others. A package declares its dependencies in [its manifest](usage.md#the-manifest), and a dependency's own dependencies are compiled but not reachable. A name under an undeclared prefix is refused where it is written, saying the prefix was not declared.

`/sys` is the compiler's own root, which only the standard library may name. It holds the intrinsic types, the host's operations, the propositions a bound is stated in, and the Unicode tables `/std/Char` reads. A program naming it is refused as for any name that does not resolve, with the path that carries what it reached for: `Nat` is `/std/Nat`.

The forms the language spells with symbols dispatch through ordinary `/std` declarations: `+` through `/std/ops/Add`, `!` through `/std/Monad`, and `test` through `/std/Test`.

## Literals

### Numeric literals

An integer literal is decimal, hexadecimal after `0x`, or binary after `0b`. A sign, `-` or `+`, touches the digits.

```text
0
42
0xFF
0b1010
-7
+3
```

A numeral takes its type from where it stands: `Nat`, `Int`, `Byte`, `Bool` or `Flt`. A `-` rules out `Nat`, `Byte` and `Bool`. `Byte` is taken only where a `Byte` is expected, from `0` through `255`, and `Bool` only where a `Bool` is expected, `0` or `1`. Where nothing gives it a type, a numeral is `Nat`, or `Int` when it is written with either sign.

A numeral has no type given once the term around it has read every part and none gave it one: a call reads its arguments and its expected type, a list or a structure literal its entries, a `match` its arms, and an operator both operands. So `g(5, b)` and `g(b, 5)` read alike, `b` typing the numeral in either order. Numerals that share one type default together: to `Int` where one of them is signed, and to `Nat` otherwise.

`-42` is one literal. `- 42` is none: between two operands it is a subtraction, and with no operand before it it is refused. The space is load-bearing.

A floating-point literal has a decimal point with at least one digit on each side, and may have a sign and an `e` or `E` exponent. Its type is `Flt`.

```text
5.0
-0.5
1.0e9
```

Neither `5.` nor `.5` is a literal: each is refused, naming the spelling that is. A decimal too large for `Flt` is refused rather than rounded to an infinity.

The values no decimal spells have literals of their own, each with a required sign: `+inf.0` and `-inf.0` are the infinities, `+nan.0` is the default quiet NaN, and `-nan.0` is that NaN with its sign set. Without a sign, `inf.0` is field `0` of a binder named `inf`. Every other NaN has no literal: `Flt/of_le_bytes` builds it from its bytes, which is how a report spells one. Why: [A literal is realized by its expected type](design/surface/a-literal-is-realized-by-its-expected-type.md).

```text
+inf.0
-inf.0
+nan.0
```

### Character and string literals

A character literal holds one Unicode scalar value or one escape. Like a numeral, it takes its type from where it stands: a `Char`, certified by proof, where nothing pins it, and its code point where a `Nat`, a `Byte` or an `Int` is expected. A `Byte` refuses a code point past `255`, and neither `Bool` nor `Flt` takes a character. `Char` excludes the surrogates and everything past `U+10FFFF`. `Char/to_nat` converts a value whose type is already fixed. In a `match`, a character literal is a `Nat` dispatch case ([Natural-number dispatch](#natural-number-dispatch)).

```text
'c'
'\n'
'\''
```

Character escapes are `\n`, `\t`, `\r`, `\\`, `\'`, and `\u{…}`, which names a Unicode scalar value in one to six hexadecimal digits: `'\u{301}'` is the combining acute accent, which no keyboard types on its own. A surrogate, a value past `U+10FFFF`, a malformed brace and any other escape are refused.

A string literal has type `Str`.

```text
"hello"
"first\nsecond"
```

String escapes are `\n`, `\t`, `\r`, `\\`, `\"`, and `\u{…}` as in a character literal. Any other backslash stands for itself, with the character after it: `"\%"` is the two characters `\%`, and so is `"\u"`. A malformed `\u{…}` is refused. A string literal is not a format string.

A block string spans lines. It opens with `"""` and a newline, blanks allowed between the two, and closes with a newline, optional blanks and `"""`. Its value is the lines between, joined by newlines, with no newline before the first or after the last.

```crs
use /std/{Str};

pub let page: Str =
    """
    <ul>
        <li>one</li>
    </ul>
    """;
```

The leading whitespace that the closing line and every non-blank line share is removed from each, so a block reads at the indentation of the code around it, and a line indented further keeps the difference. A line of whitespace alone becomes empty and counts toward no prefix. Trailing whitespace is stripped from each line, so `\u{20}` keeps a space at a line's end. The escapes are a one-line string's, read after the stripping, and a backslash before a newline joins the two lines. A `"` inside stands for itself, and three in a row are written `\"""`. The value above is `"<ul>\n    <li>one</li>\n</ul>"`.

A one-line string literal holds no raw newline; one that does is refused, naming the block form.

`Str` holds certified UTF-8 and is addressed by position. A `/std/Str/At(s)` is a byte offset into `s` where a character begins, so a piece cut between two positions is whole characters, and a step from one position to the next reads one Unicode scalar value, a `Char`. Length, folding and search count and visit scalar values, not bytes or grapheme clusters.

### Boolean literals

`true` and `false` have type `Bool`.

### List literals

A list literal builds a `List(T)`. Its entries are elements and spreads: a spread `..xs` inserts every element of `xs` where it stands. Spreads may stand anywhere and repeat, and every element and spread agrees on `T`.

```text
[]
[1, 2, 3]
[head, ..middle, tail]
```

A non-empty literal may infer `T` from its elements. An empty one takes its type from where it stands, such as an annotation:

```crs
use /std/{List, Nat};

pub let empty: List(Nat) = [];
```

### Packed literals

A packed literal builds `Bytes`. It is a [list literal](#list-literals) with the letter `x` glued to its bracket, `x[…]`; a bare `[…]` is a `List`.

An entry is a term giving one `Byte`, or a spread `..b` giving a whole `Bytes`. A constant entry is a [numeral](#numeric-literals) from `0` through `255` in any radix, or a [character literal](#character-and-string-literals) whose code point fits a byte: `x['H', 'i']`. The first entry written is the first byte.

```text
x[]                -- empty Bytes
x[0x48, 0x69]
```

```text
x[head, ..tail]
x[0x48, ..suffix, 0x00]
x[..header.bytes]
x[..make_bytes(n)]
x[..prefix, b]
x[pick(flag, a, b)]
```

`x[h, ..t]` puts `h` before `t`, and `x[..acc, b]` appends `b` to `acc`; neither has another spelling.

Only the `x` and the `[` are glued, so `x` is still an ordinary name: in `x [1]` the term ends at `x`, and a name that merely ends in `x` never opens a packed literal. Inside the brackets a packed literal reads as a list literal does: whitespace is free, one trailing comma is taken, and an entry is any term.

## Sorts and types

### Sorts

`Type` is the sort of computational types, and `Prop` the sort of proof-irrelevant propositions.

Each written `Type`, `Prop` or `Sort` stands at a level of a cumulative hierarchy. The compiler infers the levels and generalizes a declaration over them; no syntax writes a level. A type accepted at one level is accepted where a higher one is required, and a proposition likewise. A proposition sits at the level of the types it quantifies over or holds a value of, so no proposition quantifies over the propositions of its own level.

A proposition is never accepted where a type is required, nor a type where a proposition is: a predicate is no family of types, and a function over `(A: Type)` takes no proposition for `A`. A value of an `induct` or a `struct` whose level only sizes its parameters is accepted at every such level: a function answering `Result(E, Nat)` is sequenced with `!` in a region that answers `Result(E, Type)`.

`Sort` is a sort the declaration leaves to each caller: `Prop` at a use that hands it a proposition, and `Type` at one that hands it a type. It is written in the signature of a top-level `let`, `induct` or `struct`, and a use states it by inference, as it states a level.

```crs
use /std/{Eq};

pub let keep(@A: Sort, a: A) -> A = a;

pub induct Maybe(A: Sort): pub Type
| some(A)
| none()
end

pub let carry(@A: Type, @x: A, @y: A, P: (A) -> Sort, p: Eq()(x, y), v: P(x)) -> P(y) =
    match p | refl() => v end;
```

Each written `Sort` is a sort of its own, and two the declaration requires to be equal are one. The declaration is checked with each of them unknown, so a value at one is neither known to be a proof nor known to be data:

- two such values are not equal for being proofs;
- a proposition is matched into one only where it could be matched into data;
- what computes one terminates, as a proof must.

These are refused:

- a tuple type over two `Sort`s, which is a proposition where both are `Prop` and a type otherwise;
- a `Sort` the declaration's own terms decide, which is written as the `Type` or `Prop` it is;
- a `Sort` two of its terms decide apart, refused at the second, naming the first;
- a `Sort` in a body, a local definition, a `concept` or a `satisfy`, and as the sort of an `induct` or a `struct` after its `:`, which is `Type` or `Prop`;
- more than eight `Sort`s in one declaration.

Every two proofs of one proposition are equal by definition, so a proof does its work at compile time and weighs nothing at run time.

A proposition is taken apart into data only where it is empty, or has one constructor whose every payload its indices determine; that is what lets an `Eq` proof be matched to produce data. A payload no index pins, a proof among them, is taken apart only into another proposition, and a proof may always be taken apart to prove one.

Why: [`Prop` is strict, proof-irrelevant, and definitionally K](design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md) and [A universe level is implicit, cumulative, and settles by where it stands](design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md).

### Function types

A function type is a parenthesized parameter list, `->` and its result. The list may be empty: `() -> T` is a function of no parameters, called `f()`.

A parameter is plain, implicit (`@`) or a witness (`use`), and its type follows the mark. A plain or implicit parameter is written `name: type`, or as its type alone. A witness parameter is its type alone, `use Show(A)`: a witness is never named, since resolution reaches it.

```text
(Nat) -> Nat
(x: Nat, y: Nat) -> Nat
(@A: Type, x: A) -> A
(n: Nat, @Holds(0 < n)) -> Nat
(@A: Type, use Show(A), value: A) -> Str
```

A parameter's type and the result may name the parameters before it.

Every telescope is written this way: a function type, the parameters of a `let` or a `satisfy`, a constructor's payload and a declaration's type parameters. `use _`, which states no type, is refused in each. A definition and a declaration name their plain and `@` parameters, since their bodies use them.

### Tuple types

A tuple type is a field telescope in braces. A field's type may name the fields before it, and `{}` is the unit type.

```text
{Nat, Bool}
{fst: Nat, snd: Bool}
{value: A, proof: Valid(value)}
{}
```

A tuple type's labels are part of it: `{Nat, Bool}`, `{a: Nat, b: Bool}` and `{x: Nat, y: Bool}` are three types, and no value of one is a value of another. A function type's parameter names carry no such weight.

A labeled field whose type is a function may be written as a signature, so

```text
{run(input: Bytes) -> Io(Nat)}
```

is

```text
{run: (input: Bytes) -> Io(Nat)}
```

## Expressions

### Unit and tuples

`()` is the unit value, and `{}` its type.

A tuple value is its fields in parentheses:

```text
(1, true)
(left = 1, right = true)
()
```

A tuple of one field is written `(x,)`, since `(x)` is `x` in parentheses. A labeled one needs no comma: `(only = 1)`.

A tuple literal is read against its expected type's labels, position by position, and its fields are never reordered. An unlabeled literal takes the expected labels, so `(1, true)` is a `{a: Nat, b: Bool}` where one is expected. A labeled literal is refused where the expected label at a position differs or is missing. With no expected type — an unannotated `let`, a projection's head — a literal has the labels it wrote: `(a = 1, b = true)` is a `{a: Nat, b: Bool}`, `(1, true)` is a `{Nat, Bool}`, and no later annotation relabels either.

A labeled tuple is projected by position or by label: `z.0` and `z.a` name one field. A labeled field may be written as a function definition:

```text
(base = 3, bump(x) = x + 1)
```

### Names, calls, and projections

A path names a value. A call follows it with its arguments in parentheses:

```text
f(x)
Map/get(map, key)
```

An argument is written with the mark of the parameter it fills:

- `value` fills an explicit parameter;
- `@value` fills an implicit one;
- `use value` fills a witness parameter.

Every explicit parameter is written, in order. Between two of them, the hidden arguments are written in the order of their parameters, from the first, and the rest may be left out. `@_` and `use _` hold a hidden parameter's place and leave it to the compiler, which is how a later one is written alone.

```text
f(@Nat, x)
join(@_, use custom_show, values)
```

Against `join(@A: Type, use Show(A), values: List(A))`, `join(values)`, `join(@Nat, values)` and `join(@_, use custom_show, values)` are each accepted. `join(use custom_show, values)` is refused, since the hidden parameters before `values` open with `@A`, and so is `join(values, use custom_show)`, which writes the witness after the argument it comes before. The same rule fills every telescope: a call's arguments, a [lambda](#lambdas)'s binders, and a [concept literal](#superclass-fields-in-literals)'s entries.

An omitted implicit argument is inferred, never picked. Where two implicits stand under an operation that commutes — `x * y` against `a * b`, which both `x := a` and `x := b` satisfy — the call is refused, naming the implicits it could not solve, until one is written or the expected type settles it. One implicit standing as a factor has one solution, however the sums are written: `w * (y + z)` against `x * z + x * y` gives `w := x`.

An omitted implicit whose type is a proposition, a bound, is filled where the proposition reduces to `True`, and otherwise proved from the facts in scope ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)). An omitted witness is resolved ([Witness resolution](#witness-resolution)).

An implicit is inferred from every part of its call, whichever is written first. An argument whose type states it gives it. Where only a guess is left — two calls of one function held to equal arguments, or `M(A)` against `Two(Nat, Bool)`, read as `M := (x) => Two(Nat, x)` — the guess is taken once the call has read its arguments and its expected type, so a later argument that states `M` outright gives it. Two arguments that would guess one implicit differently refuse the call, naming the implicit and both values.

Inference is local to a statement. A `let` with no annotation has its type before the next line is read, and nothing a later line says reaches back into it. A projection, a `match` or a call over a value whose type is still unknown is refused where it is written.

A call fills one parameter list: the one its head's type opens with. A function whose result is a function is called once per list: `let f(T: Type) -> (Nat) -> Type` is called `f(T)(n)`, and an indexed family `Sized(T)(n)`. A list of hidden parameters alone is no exception: its call carries its `@` and `use` arguments, or none, before the next list's call, as in `Eq()(x, y)` and `Eq(@Nat)(x, y)`. Why: [A call fills one parameter group](design/theory/a-call-fills-one-parameter-group.md).

A projection names a field by position or by label:

```text
pair.0
pair.fst
configuration.network.port
```

A position counts the plain fields. A hidden field, a concept's superclass field, takes no position, so on a concept value `.0` is its first method.

Calls, projections and postfix [`!`](#postfix-) chain.

### Lambdas

A lambda is a parameter list, `=>` and a body.

```text
(x) => x
(x: Nat) => x + 1
(f, value) => f(value)
```

A parameter is a binder, or an irrefutable tuple or structure pattern. It may be annotated where its type is not given by context.

A lambda's parameters are a telescope, as a function type's are: an annotation may name the parameters before it, including the names an earlier pattern binds. An earlier parameter shadows a module's binding of the same name in a later annotation, as it does in the body.

```text
(s: A, t: A, q: Eq()(s, t)) => proof(q)
((lo, hi), q: Eq()(lo, hi)) => lo
```

A parameter carries the mark of the slot it binds: an unmarked binder binds an explicit slot, `@name` an implicit one, and `use _` holds a witness slot's place. A witness has no binder, since the body reaches it by resolution, and a lambda never states a witness's type, which its expected function type states. A lambda with no expected function type has no `use` parameter: a function that declares one is a `let` with a telescope. After `@` comes a binder and never a type, so an implicit slot the body does not name is `@_`, or `@_: T` to state its type.

```text
(@A, value) => value
(@A, use _, value) => Show/show(value)
```

A hidden binder the body does not name may be left out: it is inserted from the expected function type. The binders meet their slots as a call's arguments meet parameters. A plain binder binds the next explicit slot and never a hidden one, and the hidden binders written before it bind the hidden slots before it, from the first. Against `(@A: Type, use Show(A), value: A) -> Str`, `(value) => …`, `(@A, value) => …` and `(@A, use _, value) => …` are each accepted. `(use _, value) => …` is not, since the hidden slots open with `@A`, and neither is `(A, show, value) => …`, where `A` binds the one explicit slot and the rest are surplus.

### Local `let`

A local `let` binds a value in the term after its `;`.

```crs
use /std/{Nat};

pub let sum: Nat =
    let x = 2;
    let y: Nat = 3;
    x + y;
```

A local function is written with parameters and an optional result type:

```crs
use /std/{Nat};

pub let five: Nat =
    let increment(n: Nat) -> Nat = n + 1;
    increment(4);
```

The parameters of a `let` or a `satisfy` are a signature, written as a [function type](#function-types)'s, except that a plain or `@` parameter is always named, since the body uses it. One the body never names is `_: type`: `let positive(n: Nat, @_: Holds(0 < n)) -> Nat = n;` takes its bound as an implicit argument, which the body has as a fact. A witness parameter is `use Concept(args)`. A field written `label(params) = value` inside a tuple, a structure or a witness binds as a [lambda](#lambdas) does: a binder, marked `@` or not, its annotation optional, or `use _`.

A binder may be an irrefutable tuple or structure pattern:

```crs
use /std/{Nat};

pub struct Point: pub Type { x: Nat, y: Nat }

pub let sum(pair: {Nat, Nat}, point: Point) -> Nat =
    let (a, b) = pair;
    let Point { x, y } = point;
    a + b + x + y;
```

A binding is in scope of its own value, so a local function may call itself. A binding that names itself states its type, since the body naming it cannot give it one, and is a plain name rather than a pattern. A binding whose value performs `!` cannot name itself, since the action runs before the binding exists. Bindings that name one another are one group, joined by `and` ([Recursive groups](#recursive-groups)).

So `let n: Nat = n + 1;` names the binding it declares, not an outer `n`, and is refused: a value names itself only under a lambda, where it is a recursive value computed the first time it is read.

### Irrefutable binder patterns

The binders of a `let`, a lambda and a function's parameters, and the induction hypothesis after `;` in a `Nat`, `List` or `Bytes` arm, take nested tuple and structure patterns.

```crs
use /std/{Nat};

pub struct Point: pub Type { loc: {Nat, Nat}, color: Nat }

pub let total(value: {Nat, {Nat, Nat}}, point: Point) -> Nat =
    let (a, (_, b)) = value;
    let Point { loc = (x, y), color } = point;
    a + b + x + y + color;
```

Such a pattern projects, and matches nothing at run time. A structure pattern names the structure its binder is; a head naming another structure, or nothing, is refused.

- An unlabeled field is matched by position among the plain fields, whose order is part of the type.
- A field written `label = pattern` projects that label, holding the position it is written at, as a labeled entry of a literal does.
- A hidden field — a structure's `@` field, a concept's superclass field — takes no position, and a pattern's field takes no mark, so it is read by its label, after the fields read by position: over `struct Sized: Type { @len: Nat, items: Vec(Nat, len) }`, `Sized { items, len = n }` binds both.
- A pattern over a concept value writes its methods alone.

`Point { x, y }` is the positional form, its fields binders named after the fields. Parentheses group a pattern without changing it: `((x, y))` is `(x, y)`.

A pattern that can fail belongs to `match`.

### Written goals

`?` is a goal: the compiler infers what it can, reports the scope, the expected type and the candidate fits it found, and fails the compilation. A goal written as a type, `let y: ? = e`, reports the type of `e`.

```text
let compose(@A: Type, @B: Type, @C: Type, f: (B) -> C, g: (A) -> B) -> (A) -> C =
    ?;
compose
```

No program with a goal compiles. The other declarations are still checked in the same run, each reporting its own goals and refusals: they read a definition that holds a goal as a name of its type, bound to nothing, so a term that would unfold it stays stuck. A goal in a definition's own type, or in a type, a concept or a witness, leaves no type to read the declaration by, so what reads it waits until the goal is filled.

### Whole-term forms and operand positions

A `let`, a lambda and a function type are whole-term forms: a body or a tail extends to the end of the term around it. There is no ascription `term: type`: a `:` annotation stands only at a binder, a signature and a motive.

An infix operand is an atom — a literal, a name, a sort (`Type`, `Prop` or `Sort`), a tuple, a tuple type, a structure literal, a goal, a `match`, a `choose`, or a term in parentheses — followed by any chain of calls, projections and postfix `!`. A `match` and a `choose` are atoms because `end` closes them. A whole-term form is no operand until it is put in parentheses.

```text
1 + match flag | true => 1 | false => 0 end
match chosen | left() => f | right() => g end(x)
1 + (let n = 2; n * n)
```

A position that takes a whole term needs no parentheses: a call's argument, a list element, a field's value, a scrutinee and an arm's body.

## Operators

Every infix operator needs whitespace on both sides, and associates to the left.

| Precedence | Operators | Concept dispatch |
| --- | --- | --- |
| 1, loosest | `\|\|` | `Or` |
| 2 | `&&` | `And` |
| 3 | `==`, `!=`, `<`, `>`, `<=`, `>=` | `Eql`, `Cmp` |
| 4 | `+`, `-` | `Add`, `Sub` |
| 5, tightest | `*`, `/`, `%` | `Mul`, `Div`, `Rem` |

An operator is a call of its concept's method, resolved as any witness is, intrinsic operands included. `/std` declares the witnesses for the intrinsic types and for some of its own, and a `satisfy` declares one for any other type. Both operands have one type. `+`, `-`, `*`, `/`, `%`, `&&` and `||` answer that type, and `==`, `!=`, `<`, `>`, `<=` and `>=` answer `Bool`.

`==` and `!=` are two methods of `Eql`, `eql` and `neq`, so a witness supplies both, and `!=` is no negation of `eql`.

`&&` and `||` are calls of `And/and` and `Or/or`, as `+` is of `Add/add`, and not conditionals: neither operand is a fact in the other. So `x != 0 && 10 / x > 1` is refused, and `choose | x != 0 => 10 / x > 1 | _ => false end` is how the guard reaches the division.

`/` and `%` carry the precondition their concept declares. `Div` and `Rem` each have a field `Ok(A) -> Prop`, and the operator takes an implicit proof of `Ok(divisor)`. On `Nat`, `a / b` needs `Nat/NonZero(b)`, which is `Holds(b != 0)`: reduction proves it where `b` is a literal, and the facts in scope prove it otherwise — a hypothesis or a guard that `b != 0`, or that `0 < b`, the same proposition at `Nat` ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)).

A carrier whose division is total states `True`, and pays nothing. Why: [A bound is stated in a decided proposition and discharged by reduction](design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md).

`+` also joins two texts, as `Str/concat` does: `"Hello, " + name`. Both operands are texts; a text built from values of other types is written with `Fmt/render`.

## Sequencing and effects

### Postfix `!`

`action!` sequences: each one is a call of `/std/Monad/bind(action, continuation)` in the monad of its region.

```crs
use /std/{Nat, Byte, Bytes, Parse};
pub let parser() -> Parse(Bytes, Nat) =
    let a = Parse/bytes/byte!;
    let b = Parse/bytes/byte!;
    Parse/pure(Byte/to_nat(a) + Byte/to_nat(b));
```

A `!` may stand at any depth where its region's type sequences: in a top-level value, a program's final term, a test's body or a witness's field. A function needs no permission to hold one. A function of no parameters is written `parser()` and called `parser()`.

A local `let`'s value, annotated or not, shares the region around it, so an unused first binding moves no action to another region. An independent computation is a function, called where its description is needed. A `match` arm and a `choose` arm each keep a region of their own, whose monad comes from the arm's own expected type. No `let !` header or `end` marks a region. Why: [Bang sequencing is governed by the region's type](design/surface/bang-sequencing-is-governed-by-the-regions-type.md).

A region's monad is read from its type, never from an action in it. A region whose type is not known yet waits for it, and one whose type can never name a monad — a lambda's body in inference position, say — is refused, asking for the result type around it to be annotated.

An action of another monad than its region's is lifted through the `Monad/Lift` witness declared for that pair, and refused where none is ([Lifting between monads](#lifting-between-monads)).

A type may be computed under the same rules: a pure monad may compute one, whose result its ordinary eliminator takes out. An annotation keeps its expectation, so an `Io(Type)` description is no `Type`, and a computation of a type is checked to terminate. `!=` is an operator, never a `!` followed by `=`.

### Host effects and `Io`

Every operation that touches the host — writing to a handle, reading a clock, calling a `foreign` function, exiting — answers an `Io(T)`: a description of a computation that yields a `T`, not the `T`. An operation on a cell or a channel answers an `Io(T)` too, since it makes or observes state of the program. Calling one performs nothing: `let greeting: Io({}) = print("hello");` has printed nothing.

**No operation takes an `Io(T)` to a `T`** ([Effects are descriptions, and the carrier has no eliminator](design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)). A description is performed only as a program's final term, which `/std/Program` performs once ([Declarations and modules](#declarations-and-modules)). So a function whose result is no `Io` performs no effect, and a `!` stands only in a region whose type is a monad: a `(Str, Bool) -> Bool` has nowhere to sequence one.

`Io/pure` wraps a value as a description that does nothing, and `Io/bind` sequences two, but `!` reaches `Io` through its `Monad` witness, as it reaches any monad. Binding a description does not perform it, and one performed twice runs twice:

```crs
use /std/{Io, print};
let once: Io({}) = print("x");
let main() -> Io({}) =
    let _ = once!;
    once;
main()                          -- prints "x" twice in total
```

`Io` cannot be matched: it has no constructors, so a `match` over one is refused, with constructor arms or with none. A lone `| _ =>` arm binds rather than takes apart, and is accepted as the binding it is.

A host operation that can fail answers a `Try(M, E, A)` over its base monad: `Try(Io, Io/Error, File)` for `File/open`, and `Try(Io, Io/Error, Socket)` for `tcp/Socket/connect`. Through the edges `/std/Try` declares, a `Try` region sequences a `Try` over its own base or over one that embeds in it, a bare `Result` as an early return, and an `Io` action wherever its base admits one, which is how an `Io` base's own actions are sequenced. An action of any other base is refused. `Try/raise` stops the region, `Try/rescue` handles the stop, and `Try/run` hands the outcome back as an `M(Result(E, A))`.

```crs
use /std/{File, Path, Bytes, Try, Io};
pub let contents() -> Try(Io, Io/Error, Bytes) =
    Try/scope((s) =>
        let f = File/open(s, Path/of_str("notes.txt"), File/Mode/read())!;
        let text = Io/read_all(f)!;
        let _ = Io/close(f)!;
        Try/pure(text));
```

### Lifting between monads

`/std/Monad/Lift(M, N)` is the embedding of monad `M` in monad `N`: one method, `Monad/lift`, which takes an `M(A)` to an `N(A)`, with the `Monad` witnesses of both as superclasses, so no embedding between non-monads can be declared. As for any witness, one `Monad/Lift` witness at most stands for each ordered pair of monads in a program, so which embedding runs is a fact of the program, not of a call.

For a monad `Job` that wraps a description, with its `Monad` witness, the embedding of `Io` is one line, and with it declared an `Io` action sequences in a `Job` region, the `!` inserting the lift:

```crs
use /std/{Io, Monad, print};

pub struct Job(A: Type): pub Type { Io(A) }

satisfy Monad(Job) {
    pure(a) = Job { Io/pure(a) },
    bind(job, next) = Job { Io/bind(job.0, (a) => next(a).0) },
}

satisfy Monad/Lift(Io, Job) {
    lift(action) = Job { action },
}

pub let job() -> Job({}) =
    let _ = print("hello\n")!;
    Monad/pure(());
```

`Monad/lift(action)` names the same embedding, its target monad read from the region. A region's tail — the last term of a value's body, of a lambda's body or of an arm — is lifted the same way, where its declared monad and the region's are both monads and differ; a tail that is no monadic action keeps its type mismatch.

The lift reads the declaration of the action's head. An action whose head is no declared name — a projection, a call of a lambda — is not lifted: it is reported as an action of one monad where another is expected, and `Monad/lift(action)` lifts it. A declared result computed from an argument is read at that argument where it is settled before elaboration — a string literal, `true`, a closed call written whole — so `Fmt/print("%\n")(n)` lifts as the `Io({})` its format string makes it. One computed from a local, or from a numeral no type has realized yet, is not read, and `Monad/lift` lifts it.

Embeddings never chain. With `Monad/Lift(Io, Job)` and `Monad/Lift(Job, Sched)` declared, an `Io` action does not sequence in a `Sched` region: the missing `Monad/Lift(Io, Sched)` is reported, with any chain of declared embeddings that would have reached it. The composite is declared as any witness is, by whoever writes `Sched`. Why: [A fallible operation returns `Try`, and `!` lifts along declared edges](design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md).

## Pattern matching

### Match shell and motives

A `match` has a scrutinee, an optional motive, an arm `| pattern => body` for each case — or for several, written as [alternatives](#alternatives) — and a closing `end`.

An arm may be left out where its constructor's indices provably cannot meet the scrutinee's: a match over a `Sized(T)(n + 1)` needs no `empty()` arm. Where that is not provable, the match is refused, naming the value no arm matches as a pattern. A scrutinee whose type reduces to an inductive with no constructors takes no arms: `match contradiction end` discharges a proof of an empty type, with a motive where the result has to be spelled. Where the facts in scope contradict each other and no written term says so, `proved(@False)` is the contradiction to match on ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)).

The motive states the result type as a family. It is an ordinary term, checked against a function of the scrutinee's indices, in their declared order, and of the scrutinee:

```text
(indices) -> Scrutinee(indices) -> Sort
```

What follows the `:` is read as a term, up to the first arm, since `|` is no infix operator. Why: [A motive is a term, not a grammar](design/theory/a-motive-is-a-term-not-a-grammar.md).

```text
match b: (_) => Nat                                -- result ignores the scrutinee
match n: (m) => P(m)                               -- result depends on it
match p: (s, t, q) => Eq()(t, s)                   -- an indexed family
match p: (s: A, t: A, q: Eq()(s, t)) => Eq()(t, s) -- with written annotations
match p: discriminates_eq                          -- a named family
match v                                            -- omitted; inferred
```

The motive binds one name per index, then one for the scrutinee. A scrutinee with no indices — every intrinsic type, and every inductive declared without an index telescope — takes one, so a result that ignores it is `(_) => T`. The `Sized` of [Inductive declarations](#inductive-declarations) has one index and takes two binders, and `Eq` has two and takes three.

A type's parameters are never binders: they are the same in every constructor and fixed by the scrutinee's type, so the motive reaches them from the scope around it, as a constructor's target states only its indices.

A binder is a name, `_`, or annotated. An annotation is an ordinary type, checked against the binder's expected type, and may name the binders before it. Annotating the scrutinee's binder spells the family on the motive line:

```text
match p: (s, t, q: Eq()(s, t)) => Eq()(t, s)
```

Each arm is checked against the motive at its constructor's indices, and the match as a whole at the scrutinee's. A `| _ =>` arm binds nothing and refines no index, so it is checked at the scrutinee's indices too.

An arm's body is typed under the cases its own pattern states and nothing else, once for each of its [alternatives](#alternatives). What a body may rest on — the case of its scrutinee, a bound a case decides, the indices a constructor fixes — is what its own arm writes, whatever other arms the match holds: an arm that writes a binder where another names a constructor learns nothing of that constructor.

Without a motive, the compiler infers one, and that is the form to prefer. Where the match has an expected type, the result is that type as written, and each arm is checked against it with the scrutinee standing for the arm's case: a variable scrutinee and its variable indices are substituted, an expression scrutinee's written occurrences are replaced, and inside the arm the scrutinee reduces to the case wherever else it is met, so a hypothesis about the scrutinee needs no help to follow it.

A fold over `Nat`, `List` or `Bytes` whose arm reads its induction hypothesis — names it, or holds a goal that could — differs twice. Its hypothesis is typed at the result at the tail, so its motive is abstracted over the scrutinee instead. And its arms learn nothing of the scrutinee, since the fold takes each arm again at every tail, where the scrutinee is not that arm's case: a hypothesis about the scrutinee rides in the motive — `match n: (m) => (h: P(m)) -> Q(m) | … end(h)` — and is stated again at each tail. A case split, the same match with no hypothesis read, is not a fold.

A motive is written where the result depends on the scrutinee and nothing states it — a match in inference position whose arms answer different types — and where the result depends on the scrutinee beyond its written occurrences. A match at the type level needs none, in a signature or in another match's motive, since its arms decide the sort it lands in.

A fold's motive states its result at each tail through the binder it declares, and the hypothesis is typed at the motive opened at the tail: `match n: (m) => P(m)` asks its arms for `P(0)` and `P(k + 1)`, the hypothesis standing at `P(k)`. A motive that names the scrutinee instead, `match n: (_) => P(n)`, states `P(n)` at every tail, and the arms answer it as written.

A motive is written only where every arm's top-level pattern, in each of its alternatives, is the same constructor shape of the scrutinee. A match over a tuple of scrutinees, over a structure, or with a binder alone for its pattern takes no motive.

### Inductive patterns

An inductive pattern names a constructor and gives a pattern for each plain payload.

```crs
use /std/{Nat, Option};

pub let or_zero(option: Option(Nat)) -> Nat =
    match option
    | some(value) => value
    | none() => 0
    end;
```

A constructor is named bare, since the scrutinee's type gives its namespace: `Option/some(n)` is refused as a pattern. A pattern is written as the call that builds the value: each plain payload takes a pattern, in order, and an implicit (`@`) payload is left out, or written under `@` before the plain payload it precedes, where the arm names it. A payload left out is bound all the same, so a bound it states is a fact in the arm. Two arms of one constructor may write different hidden payloads. A pattern takes no witness payload, so `use` is refused in one.

```crs
use /std/{Nat};

pub induct Vector(T: Type): (length: Nat) -> pub Type
| nil(): (0)
| cons(@length: Nat, head: T, tail: Vector(T)(length)): (length + 1)
end

pub let head_or(@n: Nat, vector: Vector(Nat)(n), fallback: Nat) -> Nat =
    match vector
    | nil() => fallback
    | cons(head, _) => head
    end;

pub let length(@T: Type, @n: Nat, vector: Vector(T)(n)) -> Nat =
    match vector
    | nil() => 0
    | cons(@length, _, _) => length + 1
    end;
```

A pattern nests inside a constructor's, a tuple's or a structure's field, and parentheses group one. The operands of the `Nat`, `List` and `Bytes` patterns are binders rather than patterns, so `[some(x), ..tail]` is refused, while the hypothesis after `;` takes any irrefutable pattern.

```crs
use /std/{List, Nat, Option};

pub let first_or_zero(value: Option(List(Nat))) -> Nat =
    match value
    | some([head, ..tail]) => head + List/len(tail)
    | some([]) => 0
    | none() => 0
    end;
```

### Binders beside patterns

A binder — a name or `_` — may stand wherever a pattern may, beside the patterns other arms write in its place. Its arm is then taken under every case of that place: each case another arm names, and every case none does.

```crs
use /std/{Nat, Option};

pub let inner_or(value: Option(Option(Nat))) -> Nat =
    match value
    | some(some(inner)) => inner
    | some(_) => 0
    | none() => 1
    end;
```

Arms have no priority: the order they are written in never changes what a match means. Two arms may match one value only where the first written is a case of the other — the other matching every value it does — so a general arm stands after its exceptions, as `some(_)` stands after `some(some(inner))`. Three refusals hold the rule:

- Two arms that match one value, neither a case of the other, are refused, naming the value they share as a pattern, which is the arm that parts them: `(true, _)` and `(_, true)` meet at `(true, true)`.
- An arm no value reaches is refused: one written after an arm it is a case of, and a final `_` after arms that cover every case already.
- A value no arm matches is refused, named as the pattern that matches it — `(some(_), none())` — with `_` wherever no case was read.

### Alternatives

An arm may write several patterns before its `=>`, each after a bar of its own, and is taken by a value any of them matches.

```crs
use /std/{Bool, Char};

pub let is_space(c: Char) -> Bool =
    match Char/to_nat(c)
    | ' ' | '\t' | '\n' | '\r' => true
    | _ => false
    end;
```

Every alternative binds the same names, and the body reads each as the alternative that matched binds it. A name may stand at a different place in each alternative, and its type may differ. A name one alternative binds and another does not is refused.

```crs
use /std/{Bool, Html, Str};

pub let named(h: Html, name: Str) -> Bool =
    match h
    | element(tag, _, _) | void(tag, _) => Str/eql_ignoring_ascii_case(tag, name)
    | _ => false
    end;
```

The body is typed once for each alternative, under that alternative's cases and nothing else, as an arm of its own would be: an arm with alternatives means the arms it abbreviates. A proof that holds case by case is written once for all of them.

```crs
use /std/{Bool, Int};

pub let at_least(a: Int) -> Bool/Holds(-1 <= Int/sign(a)) =
    match Int/view(a)
    | nonneg(0) | nonneg(_ + 1) | neg(_) => Bool/True/qed()
    end;
```

The alternatives of one arm match no value in common, so no order among them decides which a value takes. An alternative that is a case of another is refused, and two that meet are refused, naming the value they share: `(true, _) | (_, true)` meet at `(true, true)`, and are written `(true, _) | (false, true)`. Against the other arms, each alternative is held to the rules of [Binders beside patterns](#binders-beside-patterns).

Alternatives stand at the top of an arm, and no pattern holds them: `some('a' | 'b')` is written `some('a') | some('b')`.

### The arm that cannot fail

A final arm whose pattern takes nothing apart that could fail — most often a bare `_` — takes every value the arms before it leave.

```crs
use /std/{Nat, Option};

pub let or_zero(option: Option(Nat)) -> Nat =
    match option
    | some(value) => value
    | _ => 0
    end;
```

It states no case, so its body is typed where the match stands, at the match's own type, however deep the patterns before it go. After arms that each take the scrutinee itself apart — constructors, `true` and `false`, `Nat` shapes, `[]` and `[head, ..tail]`, `x[]` — it is written `_`: a final arm binding a name there is refused, since the scrutinee is in scope under the name it already has. A lone `_` with no other arm binds rather than matches.

### Multiple scrutinees

A tuple of scrutinees is matched column by column, from the left, grouping the arms by the shape each writes in a column. Each value of the tuple is a scrutinee of its own: an arm that states a case of one refines that value as a match on it alone would.

```crs
use /std/{Nat, Option};

pub let sum(left: Option(Nat), right: Option(Nat)) -> Nat =
    match (left, right)
    | (some(x), some(y)) => x + y
    | (some(x), none()) => x
    | (none(), _) => 0
    end;
```

A binder may stand in any column, under the rules of [Binders beside patterns](#binders-beside-patterns):

```crs
use /std/{Nat, Option};

pub let sum(left: Option(Nat), right: Option(Nat)) -> Nat =
    match (left, right)
    | (some(x), some(y)) => x + y
    | (_, none()) => 0
    | (none(), some(y)) => y
    end;
```

A structure pattern names the structure its scrutinee is.

### Boolean match

A `Bool` match has a `true` arm and a `false` arm in either order, or one of them followed by the bare `_` of [The arm that cannot fail](#the-arm-that-cannot-fail).

```crs
use /std/{Bool, Str};

pub let answer(condition: Bool) -> Str =
    match condition
    | true => "yes"
    | false => "no"
    end;
```

### Natural-number induction

Induction on a `Nat` has a zero arm and a successor arm. The successor arm binds the predecessor before `+ 1`, and a binding after `;` takes the induction hypothesis for the predecessor; without it, the arm is an ordinary case split. The `+` takes whitespace on both sides, as an operator does: `pred+1` is no successor pattern.

```crs
use /std/{Nat};

pub let induction(P: (Nat) -> Type, base: P(0), step: (k: Nat, P(k)) -> P(k + 1), n: Nat) -> P(n) =
    match n: (m) => P(m)
    | 0 => base
    | predecessor + 1; hypothesis => step(predecessor, hypothesis)
    end;
```

The binding after `;` names the fold's result, not a part of the scrutinee, so it takes the irrefutable patterns a `let` does:

```crs
use /std/{Bool, Nat};

pub let count(n: Nat) -> {Nat, Bool} =
    match n
    | 0 => (0, true)
    | _ + 1; (count, live) => (count + 1, live)
    end;
```

### Natural-number dispatch

A dispatch on a `Nat` has literal arms and an arm that writes a binder, which takes every other value, since no set of literals covers every natural.

```crs
use /std/{Nat, Str};

pub let name(tag: Nat) -> Str =
    match tag
    | 0 => "first"
    | 1 => "second"
    | _ => "otherwise"
    end;
```

One match does not mix induction arms and literal arms.

A dispatch literal is any natural, written whole, as a numeral or as a character literal, which matches its scalar value: `match Char/to_nat(c) | '\n' => … | _ => … end` is how a `Char` is dispatched, the conversion visible at the head.

### List fold and case split

A list match writes `[]` for the empty case and `[head, ..tail]` for the rest. A binding after `;` takes the fold's result for `tail` — a name or an irrefutable tuple or structure pattern, as in [natural-number induction](#natural-number-induction); without it, the arm is an ordinary case split.

Both cases are required, since a list has no index to rule one out, and a final `| _ =>` may stand for the one left out.

```crs
use /std/{List, Nat};

pub let sum(values: List(Nat)) -> Nat =
    match values
    | [] => 0
    | [head, .._]; hypothesis => head + hypothesis
    end;
```

### Packed folds

A `Bytes` match is a [list fold](#list-fold-and-case-split) under the bracket `x[`, with every rule of one: both cases required, the binding after `;` optional, and a final `| _ =>` standing for a case left out. Its head is a `Byte`.

```crs
use /std/{Byte, Bytes, Nat};

pub let total(bytes: Bytes) -> Nat =
    match bytes
    | x[] => 0
    | x[head, .._]; hypothesis => Byte/to_nat(head) + hypothesis
    end;

pub let first_or_zero(bytes: Bytes) -> Byte =
    match bytes
    | x[] => 0
    | x[head, .._] => head
    end;
```

## Guarded ladders (`choose`)

`choose` is a ladder of guarded arms, tried from the top, with no scrutinee; its final arm is `| _ =>`, which is required.

```crs
use /std/{Bool, Map, Nat, Str};

pub let found(condition: Bool, map: Map(Str, Nat), key: Str) -> Nat =
    choose
    | condition => 0
    | some(value) = Map/try_get(map, key) => value
    | _ => 1
    end;
```

A condition arm is taken where its term is `true`. A bind arm, `| pattern = term =>`, is taken where the term matches its pattern, or one of the [alternatives](#alternatives) written there: `| left(x) | right(x) = either => x`. A bind arm's pattern can fail: a bare binder, and a tuple or structure pattern that takes nothing apart, are refused, since such an arm is always taken and is written as a `let`.

A taken arm learns what an equivalent nested `match` would: its condition, or its pattern's case.

## Declarations and modules

A program is top-level items followed by one final term, the description it performs, which may sequence with `!` directly. A module is top-level items alone.

The final term is performed through `/std/Program`, the concept whose `execute` says what a value of a type does as a whole program, so its type is any with a witness there:

- an `Io({})` is the program itself; no other result has a witness, so a final term that computes a value discards it explicitly;
- a `Try(Io, E, {})` whose `E` has a `Show` witness is run, and a failure is written to standard error as it shows and ends the program with status `1`;
- a `/std/Test({})` is reported as `curios test` reports one, and ends the program with status `1` where it did not hold;
- a `()` is an empty program.

Each tail of the final term closes the program: a tail is reached through a local `let`, and through a `match` that writes no motive to each of its arms, so two arms may close one program at two types. A `match` with a written motive is one term of the type it states, and a written goal is asked at `Io({})`. A program's function keeps its own result type and is called as the final term, which `/std/Program` performs with no `Program/execute` written:

```crs
use /std/{Io, Try, File, Path, print};
let main() -> Try(Io, Io/Error, {}) =
    let text = File/read_all_text(Path/of_str("notes.txt"))!;
    print(text);

main()
```

### Top-level definitions

A top-level `let` states its type, by an annotation or as a function's parameters and result type.

That is also what tells an item from the final term: in a program, a top-level `let` with no annotation is no item but a local `let` that opens the final term. A top-level definition's value is checked at the type its annotation states. A local binding shares the region around it, and an annotation gives its value no region of its own.

```crs
use /std/{Nat, Option};

pub let zero: Nat = 0;

pub let map(@A: Type, @B: Type, value: Option(A), f: (A) -> B) -> Option(B) =
    match value
    | some(x) => Option/some(f(x))
    | none() => Option/none()
    end;
```

A top-level definition is in scope of its own body, and definitions that name one another are one group, joined by `and` ([Recursive groups](#recursive-groups)).

### Test declarations

A `test` declaration names a check: a description of type `/std/Test({})`, built with that module's combinators. `curios test` runs a unit's tests, module by module in the order they are declared, and is the only build that compiles them ([`curios test`](usage.md#test)).

```crs
use /std/{Nat, Eq, Test};

test the_answer_holds =
    Test/assert(21 * 2 == 42);

let _right_identity(n: Nat) -> Eq()(n + 0, n) =
    Eq/refl();
```

A test takes no parameters. A claim about every instance is a proposition, not a description: it is a `let` whose type states the claim and whose body proves it, checked on every build, as the second declaration above is, its `_` marking a declaration that exists for its type. Why: [A test is a declared description, and a proof is a `let`](design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md).

A claim checked at chosen instances is a definition answering `Test({})`, run over a table: `test table = Test/all(List/map(cases, ((a, b)) => claim(a, b)));` is one test over the author's cases, whose failure names the case's position.

A test's path is its report line and what [`curios test --filter`](usage.md#test) selects by prefix, and nothing else names it: its name binds nothing, and is unique among its module's declarations. `test` is a word only where an item may start, and an ordinary name elsewhere. A test is never `pub`, since its name is no export, and no documentation comment precedes one.

`Test(A)` is a monad: a test's body sequences assertions, `Io` actions and fallible actions with `!`, at its fixed type `Test({})` and through the declared embeddings, and ends in a `Test({})`. A failed assertion or a lifted error stops that test.

### Modules

A module declared with `;` loads its file, `Name.crs`. An inline module ends with `end`.

```text
pub mod Nat;

pub mod Internal
    pub let value: Nat = 1;
end
```

A unit's root — the file handed to `curios run`, a package's `lib.crs` or `exe.crs`, a file checked as a module of its own — loads its modules from the directory it sits in, and every module below it from its stem's directory: `mod Nat;` in `main.crs` loads `Nat.crs` beside it, and `Nat`'s own modules load from `Nat/`. Below the root a module's path and its file's path are one string, so `/Nat/Lt` is `Nat/Lt.crs`. A stem is never part of a name, so no path names `main`, `exe` or `lib`. Roots in one directory share it, and a file two units both declare is refused ([What a package is made of](usage.md#what-a-package-is-made-of)).

### Imports and re-exports

`use` imports a group: a braced list, `path/{…}`, or a glob, `path/*`. There is no `use path;`: a single import is `use /std/{Nat};`. A group with no path, `use /{Name};`, imports from the compilation root, which is how a nested module reaches a root declaration. `pub use` re-exports what it imports.

```text
use /std/{Nat, Bool};
pub use Option/*;
use /std/Nat/{Lt};
use /{Owner};
```

A `use` takes effect where it is written: the items after it see what it imports, and the items before it do not. A module's own declarations have no such order and are in scope throughout it.

In a group, a bare name imports both a module and a value of that name, where both exist. `mod Name` imports only the module, and `let Name` only the value.

```text
pub use List/{let List};
use Package/{mod Syntax, let parse};
```

### Visibility

One rule governs every declaration, in both namespaces:

> A declaration written **without** `pub` in module `M` is visible exactly within `M`'s subtree — `M` itself and its descendants at any depth. A declaration written **with** `pub` is visible wherever `M` itself is visible, too.

A path is reachable where the rule holds at each of its hops, and the root's subtree is the whole program. So a module may name its ancestors' private declarations, and its ancestors and siblings may not name its own: `Owner/Worker` reaches a private binding of `Owner`, written absolute or imported since a relative path never climbs, while neither `Owner` nor `Owner/Other` reaches a private binding of `Owner/Worker`. Privacy points down the tree.

`pub` in a private module means wherever that module is visible, not the whole program: a public module re-exports chosen names out of a private child.

`struct`, `induct` and `concept` take a second `pub`, before their sort, which exposes their representation under the same rule. A private representation is open throughout its module's subtree, so an abstraction may be built across several files without exporting how.

A glob, `use M/*` or `pub use M/*`, imports what `M` exports and never a private declaration of its subtree, which is reached only by naming it.

A public interface never mentions what its consumers cannot reach. The interface is:

- the parameters, indices and sorts of a public type;
- the fields of a structure or a concept whose representation is public;
- the constructors of an inductive whose representation is public;
- the declared types of definitions.

The check compares audiences, not paths: a name re-exported out of a private child is visible wherever the re-export puts it, and an item seen only within a subtree may mention that subtree's declarations.

The check follows re-exports, aliases that are a name alone, and aliases of a type family that apply one head directly and whose declared result ends, as written, in `Type` or `Prop`. A definition's body is no interface, and neither is what a nested namespace binds — a type's constructors, a concept's methods — so a module of constructors may hand out values of a type its consumer cannot name. A name under `/sys`, which only the standard library may name, is seen within the standard library and wherever one of its exports shows it.

## Recursive groups

A declaration is in scope of its own body, so it may call itself with nothing said. Declarations that name one another are one group, joined by `and`, all of whose members are declared before any body is checked; two that name each other without being grouped are refused, naming both.

A top-level group holds nothing else: a top-level declaration may name any other wherever it is written, so a top-level group whose members do not all reach one another, through the names they write or the witnesses they resolve to, is refused, naming the parts it falls into, each to be declared alone. A local group may hold a member the others do not reach, since it also orders the values its recursive bindings compute on demand.

One rule has five spellings, which differ in what a later member carries and what closes the group:

| Form | A later member is written | The group closes with |
| --- | --- | --- |
| Local `let` | `and name: T = …` | one `;` |
| Top-level `let` | its own `pub`, `and`, then `name: T = …` | one `;` |
| `induct` | its own `pub`, `and`, then a head and its cases | one `end` |
| `struct`, `concept` | its own `pub`, `and`, then a whole declaration | the last member's `}` |
| `satisfy` | `and`, then a whole witness | the last member's `}` |

A local group's first member may be a pattern; every later one is a name that states its type, since a body naming the binding cannot give it one.

```crs
use /std/{Bool, Nat};

pub let parity(input: Nat) -> Bool =
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
    even(input);
```

Visibility is per member: a `pub` before `let`, `induct`, `struct` or `concept` covers the first member, and one before `and` covers that member alone. A witness is never `pub`, and neither is a member of a witness group.

A group's types hold one another in their cases and fields, never in what they are indexed by: a family's parameters and indices, and a structure's or a concept's parameters, name nothing of their group. So `induct W | base() | wrap(x: W, s: S(x)) and S: (W) -> Prop | mk(v: W): (v) end` is refused at `S`.

## Inductive declarations

An inductive declaration introduces a family and its constructors.

```crs
pub induct Option(A: Type): pub Type
| some(A)
| none()
end
```

Parameters follow the name, each named, and a list holds at least one: a family with none has no list, `induct Empty: Type`. A parameter marked `@` is implicit at the type, and every plain or `@` parameter is implicit at the constructors. A parameter `use Concept(args)` is a premise the family is declared under, a witness slot of the type and of every constructor ([A type declared under a premise](#a-type-declared-under-a-premise)).

The result is a sort, or an index telescope and a sort:

```crs
use /std/{Nat};

pub induct Sized(T: Type): (length: Nat) -> pub Type
| empty(): (0)
| push(@n: Nat, head: T, tail: Sized(T)(n)): (n + 1)
end
```

A family with both is a function of its parameters answering a function of its indices, and is applied as it is declared: `Sized` has type `(T: Type) -> (length: Nat) -> Type` and is written `Sized(T)(n)`, so `Sized(T)` is itself the family `(length: Nat) -> Type` a match over it takes apart. A family with only parameters, or only indices, takes them in one call: `Option(A)`, `Tagged(3, s)` below. Why: [A call fills one parameter group](design/theory/a-call-fills-one-parameter-group.md).

An index may be named or bare — `(length: Nat)` and `(Nat)` are both well-formed — and never takes `@`. An index's name is not in scope in the constructors: it shows in the family's printed signature, and a later index may depend on it, which is what makes the result a telescope rather than a list of types.

```crs
use /std/{Nat, Vec};

pub induct Tagged: (size: Nat, contents: Vec(Nat, size)) -> pub Type
| tag(@size: Nat, @contents: Vec(Nat, size)): (size, contents)
end
```

Each constructor of an indexed family states the indices it produces after `:`, and a constructor of a family with no indices states none.

A constructor's payload is a signature: each member is `name: type` or its type alone, plain or `@`, so `at(n: Nat, @Holds(0 < n))` takes its bound as an implicit argument. A payload takes no `use` member, and an index takes no mark.

An inductive proposition is declared at `Prop`:

```crs
pub induct Eq(@A: Type): (left: A, right: A) -> pub Prop
| refl(@value: A): (value, value)
end
```

The outer `pub` exports the family's name. The inner one exports its constructors and every way of taking it apart; without it, building and matching it are kept to the declaring module's subtree.

Inductives that name one another are one group, joined by `and` and closed by one `end` ([Recursive groups](#recursive-groups)).

## Structure declarations

A structure is a nominal record whose fields may depend on one another.

```crs
pub struct Pair(A: Type, B: Type): pub Type {
    fst: A,
    snd: B,
}
```

A structure of one unlabeled field wraps it, and is projected with `.0`.

```crs
use /std/{Nat};

pub struct Meters: pub Type { Nat }
```

The outer `pub` exports the type's name. The inner one exports building it and projecting it; without it, both are kept to the declaring module's subtree. A structure at `Prop` holds only propositions.

Parameters are written as an inductive's are: plain, `@`, or a premise `use Concept(args)` ([A type declared under a premise](#a-type-declared-under-a-premise)).

A field is plain or hidden. A hidden field is written under `@`, as `@label: type` or its type alone, and takes no position among the fields an author writes: a literal leaves it out, a projection `.N` and a pattern's positional fields count only the plain ones, and it is read by its label. A bound declared this way is stated once, on the data it constrains: it is proved where the value is built, and is a fact wherever the value is in scope.

```crs
use /std/{Nat, Vec};
use /std/Bool/{Holds};

pub struct Positive: pub Type { n: Nat, @Holds(0 < n) }
pub struct Sized: pub Type { @len: Nat, items: Vec(Nat, len) }
```

A field takes no `use` member, since a structure's premise is a `use` parameter, and a tuple type's field takes no mark.

Structures whose fields name one another are one group, joined by `and`; a lone structure may name itself in its fields with nothing said ([Recursive groups](#recursive-groups)).

```crs
use /std/{Nat, Option};

pub struct Node: pub Type { value: Nat, next: Option(Edge) }
pub and Edge: pub Type { weight: Nat, to: Node }
```

### Structure literals

A structure value names its type and gives its fields. Its head may be applied, and an applied head is the type written as anywhere else: a call of the type, taking the marks, the `?` holes and the omitted hidden arguments a call takes. An empty argument list is a call too, never the bare head: `Box() { … }` leaves `@A` to inference as `Box()` does in a signature, and `Pair() { … }` is refused as `Pair()` is.

```text
Pair { fst = 1, snd = true }
Pair(Nat, Bool) { fst = 1, snd = true }
Box(@Nat) { value = 1 }
Api { base = 3, bump(x) = x + 1 }
```

Fields are checked in their declared order. A field written as a function definition is a lambda assigned to it.

A hidden field is left out and filled as a call's omitted `@` argument is: a value by the fields whose types mention it, a bound by reduction or from the facts in scope. One nothing fills is reported at the literal, by the proposition it states. It may be written under its mark before the plain field it precedes, as a call's hidden arguments are, from the first of their run: `@label = value`, the value alone, or `@_` to hold its place.

```text
Positive { n = 4 }
Sized { @len = 2, items = two }
Sized { @_, items = two }
```

### Structure update

A leading `..base` copies a value of the same structure, and the labeled entries after it replace fields.

```text
Pair { ..pair, snd = false }
Pair(Str, Nat) { ..pair, fst = "new" }
```

A spread copies the plain fields. A hidden field is never copied, since it states something of the fields the new value has: it is filled anew as in any literal, so a bound an override breaks is refused, and the base's own is written where it is meant, `Positive { ..p, @ok = p.ok }`.

The spread stands first and once. Every override is labeled, and the overrides follow the declared order. The head may choose other parameters, and every copied and replaced field is checked at them, so dependent fields stay consistent.

Tuple and string literals have no update form. A spread in a list or a packed literal concatenates ([List literals](#list-literals)).

## Concepts and witnesses

A concept is an interface shaped as a record, a witness is a registered value of a concept at some types, and a `use` parameter asks the compiler to supply one. Together they give ad-hoc polymorphism.

`concept` and `satisfy` are words only where a declaration starts, and ordinary identifiers elsewhere.

### Concept declarations

A concept has parameters, possibly none, a representation sort, and a list of fields. The sort follows a structure's rule: `: pub Type` declares a transparent concept, and `: Type` a *sealed* one, whose representation is private to its module's subtree, so only there may a witness be declared, a concept value be written, updated or taken apart. Resolution, `use` parameters and a method called by its path work alike for both, and the concept's name has its own visibility.

```crs
use /std/{Str};

pub concept Show(A: Type): pub Type {
    show(A) -> Str,
}

pub concept Monad(M: (Type) -> Type): pub Type {
    pure(@A: Type, A) -> M(A),
    bind(@A: Type, @B: Type, M(A), (A) -> M(B)) -> M(B),
}
```

Every field is a method, named by its path in the concept's namespace: `Show/show(value)` asks for a witness of `Show(A)` and projects its `show`, as an operator does.

A method's call takes the concept's parameters and the witness in one parameter list with the method's own, so a method is called once, as it is declared: `Show/show(value)`, `Fun/name(@Pair)` for a `name() -> Str` at `Pair`, and `Show/show(@Nat, use loud, n)` with its witness written.

A field that is no function is reached by the call that gives the witness, `Sized/Carrier(@Nat)`, and a method named where a function is expected is its field of the witness its type resolves: `List/map(xs, Show/show)`. A method is a field of its concept, and no definition of its own.

Concepts whose methods' types name one another's dictionaries are one group, joined by `and` ([Recursive groups](#recursive-groups)). A superclass cycle — `use B(A)` in `A` and `use A(B)` in `B` — is refused, grouped or not, since resolution could never end.

The fields are a telescope: a field's type may name the fields before it. In a method's call such a name is the matching field of the resolved witness, so the call's type constrains that witness's own implementations.

```crs
use /std/{Eq};

pub concept Idem(A: Type): pub Type {
    op(A) -> A,
    law(x: A) -> Eq()(op(op(x)), op(x)),
}
```

A field whose type is a proposition about the fields before it is a law. A witness of such a concept supplies the proof of the law at its own implementations, so a witness that breaks it is refused where it is declared.

A field whose result is a sort is a type each witness chooses. `Div`'s `Ok(A) -> Prop` is how each carrier states its own division precondition, and a witness supplies it as any other field:

```crs
use /std/{Nat};
use /std/ops/{Rem};

pub struct Hours: pub Type { Nat }

satisfy Rem(Hours) {
    Ok(b) = Nat/NonZero(b.0),
    rem(a, b) = Hours { a.0 % b.0 },
}
```

A field written `use` is a superclass, unnamed. Its type reduces to a concept application, as a `use` parameter's does, so an alias of one is a superclass too. A field takes no `@`: a concept's hidden members are its superclasses.

```crs
use /std/ops/{Eql};
use /std/Ord/{Ordering};

pub concept Ord(A: Type): pub Type {
    use Eql(A),
    ord(A, A) -> Ordering,
}
```

So a local `Ord(A)` witness answers an `Eql(A)` goal through its superclass.

A sealed concept's fields are not part of its public interface: a `pub` sealed concept may name private declarations in its fields' types, so a private superclass is an obligation resolution discharges without the consumer naming it ([Privacy is scoped to a subtree](design/surface/privacy-is-scoped-to-a-subtree.md)). A transparent `pub` concept's fields' types are interface, and are `pub` themselves.

A concept at `Prop` has proof-irrelevant witnesses, which leave nothing at run time.

### Witness declarations

`satisfy` registers a witness, which has no name. Its type is a concept application, written as any call is — `satisfy Named(@Nat) { … }` over `concept Named(@A: Type)` — and its body supplies the concept's fields, or holds `..` alone, for the compiler to write them ([Derived witnesses](#derived-witnesses)).

```crs
use /std/{Nat, Show};

pub struct Meters: pub Type { Nat }

satisfy Show(Meters) {
    show(m) = Nat/to_str(m.0),
}
```

A witness may take implicit parameters and require other witnesses, in a telescope separated from the concept application by `=>`. It takes no explicit parameter, since resolution has no explicit argument to give.

```crs
use /std/{List, Show};

pub struct Bag(A: Type): pub Type { List(A) }

satisfy (@A: Type, use Show(A)) => Show(Bag(A)) {
    show(bag) = List/fold(bag.0, "", (value, result) => result + Show/show(value)),
}
```

A witness is keyed by its concept and by the head of each of the concept's arguments, and its signature spells the key, each argument written as the type it is. A head is an inductive, a structure, an intrinsic type or a tuple type, applied or not; a name bound to one, read through; or a family applied in part, written as a lambda, `(A: Type) => State(S, A)`, which keys on the head it applies. A head only computation reaches, `Show(Chosen(true))` over a `Chosen` that picks its result, is refused where the witness is declared, naming the key it computes: a witness is known by its key before anything is checked. The arguments below the heads are matched after the lookup.

A tuple type is keyed by its shape — the label at each position, which gives its length — and not by its fields' types. Since labels are part of a tuple type, `Show({Nat, Bool})`, `Show({a: Nat, b: Bool})` and `Show({x: Nat, y: Bool})` are three keys. `{}` keys as the empty shape, and a family whose body is a tuple type, `let Pair(A: Type) -> Type = {Nat, A};`, keys on that body's shape. `/std/Tuple` writes the standard library's tuple witnesses, `Show`, `Spell`, `Eql` and `Ord` at the unlabeled shapes of nought through eight fields; a labeled product that wants them is a `struct`. A program writes tuple witnesses for its own concepts ([Orphan rule](#orphan-rule)):

```crs
use /std/{Show, Str};

pub concept Describe(A: Type): pub Type {
    describe(A) -> Str,
}

satisfy (@A: Type, @B: Type, use Show(A), use Show(B)) => Describe({A, B}) {
    describe(t) = "(" + Show/show(t.0) + ", " + Show/show(t.1) + ")",
}
```

A function type has no key: a `satisfy` whose concept argument reduces to one is refused, as a variable head is. A function becomes a monad by being wrapped in a nominal type, as `/std/State` is. Why: [Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md).

Witnesses that resolve through one another are one group, joined by `and` ([Recursive groups](#recursive-groups)). A lone witness may resolve through its own key with nothing said. A method written by another method of its witness — `neq(a, b) = Bool/not(a == b)` — reads that method alone, so only a method that reaches itself makes its witness recursive.

```crs
use /std/{Nat, Show};

pub induct Tree: pub Type
| leaf(Nat)
| node(Forest)
pub and Forest: pub Type
| nil()
| cons(Tree, Forest)
end

satisfy Show(Tree) {
    show(t) = match t | leaf(n) => Show/show(n) | node(f) => Show/show(f) end,
}
and Show(Forest) {
    show(f) = match f | nil() => "" | cons(t, rest) => Show/show(t) + Show/show(rest) end,
}
```

A registered witness needs a concept with at least one parameter; a concept with none is still used through a value supplied to a `use` parameter. Each argument keys on its own — `Into(Nat, Str)` and `Into(Nat, Bool)` are two keys — so a call settles every argument, from its explicit arguments, its expected result or a written witness, before the lookup.

One witness at most stands at a key in a whole program. Module visibility does not scope a witness, but a sealed concept's representation does: its witnesses are declared only within its module's subtree.

A second dictionary for a transparent concept at the same key is an ordinary concept value, supplied explicitly; a sealed concept refuses the literal outside its module.

```crs
use /std/{List, Nat, Ord};

pub let descending(values: List(Nat)) -> List(Nat) =
    let reverse: Ord(Nat) = Ord { ord(a, b) = Ord/ord(b, a) };
    List/sort(@_, use reverse, values);
```

### Derived witnesses

A witness whose body is `..` alone is derived: the compiler writes its body from the declaration of the type in its key. `satisfy Spell(Point) { .. }` derives one, and so does `satisfy (@A: Type, use Spell(A)) => Spell(Tree(A)) { .. }` under a telescope; either may stand in an `and` group beside written witnesses. The dots are no entry — nothing stands beside them and they take no comma — so a body is written whole or derived whole. The signature is the author's, and registers, keys and meets the orphan and sealing rules as a written witness does. Why: [A witness body may be written by the compiler](design/surface/a-witness-body-may-be-written-by-the-compiler.md).

`Spell`, `Eql`, `Ord` and `Hash` derive; every other concept refuses the form by name, and a written witness stays the norm.

```crs
use /std/{Nat, Ord, Spell};
use /std/ops/{Eql};

pub struct Point: pub Type { x: Nat, y: Nat }
pub induct Tree(A: Type): pub Type | leaf(A) | node(Tree(A), Tree(A)) end

satisfy Spell(Point) {
    ..
}

satisfy (@A: Type, use Spell(A)) => Spell(Tree(A)) {
    ..
}

satisfy (@A: Type, use Eql(A)) => Eql(Tree(A)) {
    ..
}

satisfy (@A: Type, use Eql(A), use Ord(A)) => Ord(Tree(A)) {
    ..
}
```

The key is a declared `induct` or `struct`, fully applied, whose representation is open where the witness is declared; an intrinsic type, a tuple or function type and a concept's record are refused.

A hidden payload or field takes part as a plain one does, `Spell` writing it under its mark — `Tagged/tag(@9, 1)`, `Sized { @len = 9, items = 1 }` — unless it is a type or a proof, which the text infers when it is read back. A plain payload that is itself a type is refused.

Every other payload goes through its own witness, resolved in the witness's scope: a premise of the telescope, the witness's own key, or a member of its `and` group. A missing one is reported at the constructor and payload, naming the `use` premise to add when the payload's type is a variable of the telescope.

| Concept | The body the compiler writes | A proof payload |
| --- | --- | --- |
| `Spell` | the constructor, qualified by its type's own name, applied to the explicit payloads — `Tree/node(Tree/leaf(1), Tree/leaf(2))`, `Option/some(3)` — and a structure as its literal, `Point { x = 1, y = 2 }`, positional where a field has no label | spells as the written goal `?` |
| `Eql` | structural: the same constructor with equal payloads, pair by pair, and `!=` its negation | compares as nothing |
| `Ord` | constructors ranked by their declared position, and payloads compared only where the constructors agree, the first that differs deciding | contributes nothing |
| `Hash` | the constructor's ordinal, then the explicit payloads' encodings, each part behind its own four-byte length so no two values share a byte string; a structure takes ordinal zero | takes no part |

A derived `Spell`'s text reads back wherever its type's name is visible unqualified, which is wherever a value of it is written, so a report shows a value as its author would write it. `Ord` has `Eql` as a superclass, so a derived `Ord` asks for its key's equality witness, and is refused where there is none. A proof payload never contributes, since two values that differ only in a proof are one value.

### Witness premises

A witness's premise is a concept application smaller than the witness's own: each of its variables is bound by the witness's telescope, none occurs in it more often than in the witness's application, and it has fewer nodes in all. So a premise may name a constant beside a binder — `use Monad/Lift(Io, M)` under `Monad/Lift(Io, (A: Type) => Try(M, E, A))` — while `use Show(A)` under `Show(A)` is refused. Resolution ends because the premises shrink ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

### Orphan rule

A witness is declared only by the compilation root that owns its concept or at least one head in its key, so no two independent parties can declare the same witness ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

No root owns a tuple shape, as none owns an intrinsic type, so a tuple-keyed witness is declared where its concept is: a program writes tuple witnesses for its own concepts, and adds none for a `/std` concept at a shape `/std` did not write. No root is exempt, the standard library included, which declares every concept it witnesses.

### Superclass fields in literals

A concept's superclasses are hidden members of a concept value. They take no position when it is read — a projection `.N` and a pattern's positional fields count the methods alone — and a literal's entries follow the declaration as a call's arguments follow a function's parameters: the fields in order, and before each, `use value` entries filling the superclass slots before it, from the first. A slot left out is resolved, and `use _` holds a slot's place to the same effect, so `Both { use _, use second, both = … }` supplies the second of two superclasses alone.

```text
Ord { use custom_eql, ord(a, b) = reversed(a, b) }
```

A witness's body writes none: a `use` entry in a `satisfy` is refused by name, so resolution fills every superclass of a registered witness, and the `Eql(A)` reached through a local `Ord(A)` is the registered one ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

In an update, a spread copies the superclasses from the base. A `use value` after the spread replaces the next superclass slot after the entries before it, and `use _` leaves one as the spread copied it.

### Witness parameters and arguments

A witness parameter is written `use Concept(args)` in a function type or a definition's parameters. It has no name, and joins the witnesses the body resolves from, which are the `use` members in scope. Its type reduces to a concept application, since resolution answers nothing else; any other type is refused where the parameter is declared, and a proof to be discharged is an `@` parameter instead. A dictionary a program names is an ordinary value — a `let`, or a plain or `@` parameter of the concept's type — and fills a witness slot through `use value`.

```crs
use /std/{List, Show, Str};

pub let join(@A: Type, use Show(A), values: List(A)) -> Str =
    List/fold(values, "", (value, result) => Str/concat(result, Show/show(value)));
```

At a call, `use value` supplies a witness explicitly, and resolution does not run for that slot.

```text
join([1, 2, 3])
join(@_, use custom_show, [1, 2, 3])
```

### A type declared under a premise

A `struct` or an `induct` may take `use Concept(args)` among its parameters, and the dictionary it is applied at is then an argument of the type.

```crs
use /std/{Bool};

pub concept Key(K: Type): pub Type {
    same(K, K) -> Bool,
}

pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
    key: K,
    value: V,
}

pub let holds(@K: Type, use Key(K), @V: Type, slot: Slot(K, V), key: K) -> Bool =
    Key/same(slot.key, key);
```

The parameter has no name, and joins the witnesses the declaration resolves from, so a field's type, an index's type and a constructor's payload resolve through it. Where the type is written, the slot is a call's: `Slot(Nat, Str)` leaves it to resolution, and `Slot(Nat, use reversed, Str)` fills it. Under a `use Key(K)` premise, as in `holds`, the nearest witness is the premise, so the signature's `Slot(K, V)` names the dictionary the function was given. Two types that differ in their dictionary are two types, and a value holds no dictionary: the parameter belongs to the type, as `K` does.

A structure literal with a bare head takes the dictionary from its expected type, or from resolution where there is none. At an inductive's constructors the parameter stays a witness slot — resolved, or supplied with `use value` — where a plain or `@` parameter is implicit.

A value is read under the dictionary its type names. `holds(slot, key)` over a `slot: Slot(Nat, use reversed, Str)` runs under `reversed`: the call's own slot is still open when the argument is checked, the argument's type fills it, and resolution leaves a filled slot alone ([Witness resolution](#witness-resolution)). Two values under two dictionaries are not both arguments of an operation whose signature names the dictionary once: the call is a type mismatch.

A concept takes no `use` parameter: its premise is a superclass, and a witness is keyed by the heads of its concept's arguments, which a dictionary does not have.

### Witness resolution

An omitted witness is resolved in this order:

1. The local `use` parameters, from the innermost out; the first that matches directly wins.
2. The superclasses of the local witnesses, breadth first; two matches at the least depth are ambiguous.
3. The global table, by the concept and the head of each of its arguments.

A slot is resolved only while it is open: one that an expected type or an argument's type has already filled is left as it stands. A call's slot is resolved once the call has read its arguments and met its expected type, so a dictionary one of them names is the call's, wherever the premise is declared and whenever its arguments became known; a call none of them names one for takes the registered witness, and `use value` at the slot says which is meant. A superclass of a witness is the registered witness, and nothing names another for it.

Where an argument of the concept is still an unknown, resolution waits until it is known. A global witness is instantiated with fresh implicit arguments, its premises are resolved in turn, and its whole type is unified with the goal.

A unit's witnesses are known by key before any of them is checked, so a witness may be declared after the code that resolves through it, and which side of it the witness stands on changes nothing: a goal finds the witness declared at its key, and a key with no witness is reported where the goal stands. A declaration and a witness that each need the other have no order to be checked in, and are refused together, by name.

A higher-kinded argument is keyable. Where `M(A)` meets `Option(Nat)`, `M` is inferred as `Option`, so `Monad(Option)` is looked up. Where `M(A)` meets an application of more arguments, `State(S, Nat)`, `M` takes all but the last, `(A) => State(S, A)`, so a family meant as a monad orders its parameters context first and result last.

## Foreign declarations

A `foreign` declaration introduces a function implemented outside the program: by a WebAssembly module or a JavaScript gateway its package names, by the embedder, or by the host. It is written as a function with no body, its operands and its result in the wire grammar rather than as arbitrary Curios types.

```crs
pub foreign random() -> Nat;
pub foreign frobnicate(Nat, Bytes) -> Nat;
pub foreign log(Bytes) -> Nat;
pub foreign close(Handle) -> {};
pub foreign flip(Byte) -> Byte;
pub foreign read(Handle, Nat) -> {status: Nat, bytes: Bytes};
pub foreign abort(Byte) -> Never;
```

The wire types are `Nat`, `Int`, `Bool`, `Byte`, `Flt`, `Bytes`, `Handle`, `List(T)` and `Option(T)`, written bare: the wire grammar is a closed vocabulary that resolves no names, so `/std/Nat` is refused where `Nat` is meant.

- A `Byte` crosses as the integer it is, both ways, and a host that answers one outside `0..=255` stops the program rather than handing it another byte.
- A `Nat` or an `Int` crosses as a 64-bit integer, although the program's are unbounded: a `Nat` argument crosses below `2⁶⁴` and an `Int` between `-2⁶³` and `2⁶³ - 1`, and a larger one stops the program rather than crossing changed. A result comes back as a 64-bit integer too, a `Nat` read unsigned and an `Int` signed, so every result arrives whole.
- A `Flt` crosses as a binary64.

The operands follow the name, as a function's parameters do, each its wire type alone, with no name. No colon stands between the name and the operands, since a declaration's signature is not its type: a call yields an `Io` of the result, so `frobnicate` above is a `(Nat, Bytes) -> Io(Nat)`. Every `foreign` is a function, of no operand where its list is empty: `random` above is a `() -> Io(Nat)`, called `random()`.

A result is a wire type, or a braced list of labeled wire types, the [tuple type](#tuple-types) the call yields. Every field is labeled, each by a name written once; `_`, which a tuple type reads as no label, is refused. `{}` is no result at all, the unit type; `()` is the unit value and never stands here. Two or more fields are a tuple the program projects by label.

A tuple type's labels are part of it, so nothing is invented, dropped or moved: the fields cross in the order written, and a reference result — `Bytes`, `Handle`, `List(T)` or `Option(T)` — crosses in whatever position it is written, as many as the row has. A single result in braces is refused: it is written bare, not `{value: T}`, since one result crosses as itself, and a brace of one field would name a tuple the row cannot carry.

`Never` in place of a result says the call does not return, which differs from returning nothing. It is no type and stands nowhere else. A declaration that states it takes the type its call yields as an implicit argument before its own, so `abort` above is `(@A: Type, Byte) -> Io(A)`, a description that ends the program and so fits a region of any type; `/std/Proc/exit` is one. That argument joins the operands, so `foreign gone() -> Never;`, of no operand, is `(@A: Type) -> Io(A)` and is called `gone()`.

`List` and `Option` do not nest, and their elements are `Nat`, `Int`, `Bool`, `Bytes` or `Handle`: `List(List(T))`, `List(Flt)`, `List(Byte)` and an `Option` inside a `List` are refused, and a list of bytes is the `Bytes` a row already spells.

A list of `Nat`, `Int` or `Bool` crosses flat, one element per value, each narrowed going out and boxed coming back as a lone one is, so an element too large for the wire stops the program as an argument would. A list of `Bytes` or `Handle` crosses as its elements' payloads. An `Option(T)` crosses as `none`, which costs nothing, or as the one value `some` holds, so an embedder reads and answers an option of its element: a Rust binding an `Option`, a browser hook the value or `null`. How a declaration reaches its embedder is the host's interface, [`curios-abi`](../curios-abi/README.md)'s.

## Equality and proofs

Propositional equality, `Eq`, is an ordinary indexed inductive proposition of `/std/Eq`. Its proofs use the constructors, functions and `match` forms of any inductive; `Eq` has no syntax of its own.

```crs
use /std/{Eq};

pub let sym(@A: Type, @x: A, @y: A, proof: Eq()(x, y)) -> Eq()(y, x) =
    match proof: (left, right, _) => Eq()(right, left)
    | refl() => Eq/refl()
    end;
```

The standard equality operations include reflexivity, symmetry, transitivity and congruence, and `Eq/subst`, which carries a `P(x)` to a `P(y)` along an equation: a proof where `P` is a family of propositions, and a value where it is one of types, `P` being declared into a `Sort`. `Eq` is propositional equality; `Eql` is the concept `==` and `!=` call.

### Bounds from the facts in scope

A bound is stated and proved in `/std/Bool`'s vocabulary, which a program imports by name: `use /std/Bool/{True, False, Holds};`. `Holds(b)` is the proposition that the decision `b` holds: it reduces to `True`, proved by `True/qed()`, where `b` is `true`, and to the empty `False` where it is not.

A bound that reduction does not decide is proved by the compiler where it follows by linear arithmetic from the facts in scope: the hypotheses, their proof fields one level down, and the guards of the arms around it, each read through the local definitions and refinements in scope. The fragment is:

- comparisons of `Nat` and `Int` terms with literal coefficients, the bound being any of `<`, `<=`, `==` and `!=`;
- a disequality among the facts — an `==` that came back `false`, a `!=` that came back `true`, or a hypothesis stating either — through the two cases the order of its sides makes;
- `min` and `max`, through the two cases the order of their operands makes, and an `Int`'s `abs` and `sign`, through the cases its operand's sign makes;
- at `Nat`, a truncated subtraction, through what both of its cases state and, inside a sum, through its two cases; a quotient or a remainder by any nonzero divisor, through the quotient's bounds; and a power of a base reduction shows to be at least one, as the power of two a shift by an unknown count multiplies by is;
- where linear arithmetic alone finds no proof, the products of pairs of facts and of the negated goal.

A bound one of those facts states outright is proved by the fact, whatever the proposition is: an opaque `Bool` function's result, an equation, a family of its own. Why: [A bound that follows from the facts in scope is proved by the elaborator](design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md).

```crs
use /std/{List, Nat};
use /std/Bool/{Holds};

pub let get(xs: List(Nat), i: Nat, m: Nat, p: Holds(i < m), q: Holds(m <= List/len(xs))) -> Nat =
    List/get(xs, i);
```

`proved`, imported loose from `/std` as `print` is, reaches the same proof where no bound asks for it. It states a fact a term needs, its proposition written or set by the expected type, and at `False` it is the contradiction a match with no arms takes apart, in an arm the facts rule out:

```crs
use /std/{Nat, proved};
use /std/Bool/{False, Holds};

pub let clamp(n: Nat, q: Holds(n <= 10)) -> Nat =
    match n > 20 | true => match proved(@False) end | false => n end;
```

A bound the facts do not imply is refused. The report names the facts it considered and the ones it could not read, and gives values for the atoms under which the facts hold and the bound fails. A goal `?` written for a bound reports the same.

## Quick reference

| Form | Meaning |
| --- | --- |
| `-- ` | Line comment |
| `--- ` | Documentation comment, attached to the declaration below it |
| `{}` / `()` | Unit type / unit value |
| `@A: Type` / `@T` / `@value` | Implicit parameter or hidden structure field, named or as its type alone / implicit argument or hidden field written explicitly |
| `use C(A)` / `use _` / `use value` | Witness parameter of a signature / its place in a lambda / witness argument written explicitly, or a concept literal's superclass |
| `?` | Goal — reports the scope, the type and the fits, and fails the compilation |
| `term!` | Monadic bind through `Monad`, lifting an action of another monad through `Monad/Lift` |
| `"""` … `"""` | Block string — the lines between the delimiters, their shared indentation removed |
| `x[...]` | `Bytes` literal — the letter glued to the bracket |
| `Name { ... }` | Structure or concept literal |
| `Name { ..base, ... }` | Structure update |
| `struct Slot(K: Type, use Key(K)): …` | A type declared under a premise — the dictionary is an argument of the type |
| `match term ... end` | Typed elimination or dispatch |
| `choose ... end` | Guarded ladder |
| `test name = body;` | Test — a `/std/Test` description, run by `curios test` |
| `satisfy C(args) { ... }` | Registered witness, which has no name |
| `satisfy C(args) { .. }` | Derived witness — the compiler writes the body |
| `satisfy (@A: Type, use C(A)) => D(args) { ... }` | Witness under a telescope |
| `use /std/{Nat};` / `use /std/*` / `use /{Name};` | Import a group, what a module exports, or a name of the compilation root |
| `… and …` | [Recursive group](#recursive-groups) — `let`, `induct`, `struct`, `concept` or `satisfy` |
