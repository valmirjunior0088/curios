# Language

This is the surface language a `.crs` file is written in: each form, what it means, and what is refused. Where the compiler and this document disagree, one of them is wrong, and the disagreement is a bug.

A `.crs` file is a sequence of top-level items. A program closes with one final term, the description it performs; a module is items alone. Everything below is an item, a term, or a part of one.

Every `crs` example is a whole program or module that `curios wonder` reports nothing for and `curios format` leaves as it is, which `curios/src/tests/reference.rs` holds. A `text` example shows a form on its own. The examples name declarations from `/std`, the standard library every program may name, whose source under `curios-prelude-build/std/` is the largest body of Curios there is.

- [Lexical structure](#lexical-structure)
- [Literals](#literals)
- [Sorts and types](#sorts-and-types)
- [Expressions](#expressions)
- [Operators](#operators)
- [Sequencing and effects](#sequencing-and-effects)
- [Pattern matching](#pattern-matching)
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

An identifier is a non-empty run of alphanumeric characters and `_` whose first is alphabetic or `_`: a character is alphabetic where the Unicode Character Database the toolchain pins calls it `Alphabetic`, and alphanumeric where it is alphabetic or a number, of the general category `Nd`, `Nl` or `No` ([`curios-unicode`](../curios-unicode/README.md)). A word that begins with a digit is a number wherever a term stands, so it is no name: `let 5 = 3;` is refused.

A declaration whose name begins with `_` is kept unused on purpose: [`curios lint`](usage.md#lint) never reports it, and what it reads counts as used. `_` alone is a binder that names nothing, and `@_` is its implicit form, for a slot nothing reads. A binder has no kept form: a binder with any other name is reported where nothing reads it, whatever it begins with.

These words are reserved, and no path segment may be one:

| Declaration and expression words | Literal words |
| --- | --- |
| `let`, `match`, `mod`, `use`, `pub`, `end`, `induct`, `struct`, `foreign` | `true`, `false` |

`concept`, `satisfy`, `and` and `test` are contextual: each is a word only where the grammar uses it, and an ordinary identifier elsewhere. `Type`, `Prop` and `Sort` are sorts where they stand as terms, so no binder may be one, which nothing could read; a path segment or a field's label may still be one.

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

`-42` is one literal. `- 42` is none: between two operands it is a subtraction, and with no operand before it it is refused. The space is load-bearing.

A `-` leads a numeral past zero: a signed numeral is the integer its sign and digits spell, and an integer has no negative zero, so `-0` is no numeral in any radix. The zero of an `Int` is `+0`, and a float's negative zero is the decimal `-0.0`.

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

A character literal holds one Unicode scalar value or one escape. Like a numeral, it takes its type from where it stands: a `Char`, certified by proof, where nothing pins it, and its code point where a `Nat`, a `Byte` or an `Int` is expected. A `Byte` refuses a code point past `255`, and neither `Bool` nor `Flt` takes a character. `Char` excludes the surrogates and everything past `U+10FFFF`. `Char/to_nat` converts a value whose type is already fixed. In a `match`, a character literal is a dispatch case of a `Char`, a `Nat` or a `Byte` ([Character and string dispatch](#character-and-string-dispatch), [Number patterns](#number-patterns)).

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

pub let empty: List(Nat) =
    [];
```

Written as a pattern, a list literal is a [run](#list-case-split).

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

Only the `x` and the `[` are glued, so `x` is still an ordinary name: in `x [1]` the term ends at `x`, and a name that merely ends in `x` never opens a packed literal. Inside the brackets a packed literal reads as a list literal does: whitespace is free, one trailing comma is taken, and an entry is any term. Written as a pattern, it is a run under `x[` ([Packed case split](#packed-case-split)).

## Sorts and types

### Sorts

`Type` is the sort of computational types, and `Prop` the sort of proof-irrelevant propositions. Each written `Type`, `Prop` or `Sort` stands at a level the compiler infers, and no syntax writes a level.

`Sort` is a sort the declaration leaves to each caller: `Prop` at a use that hands it a proposition, and `Type` at one that hands it a type. It is written in the signature of a top-level `let`, `induct` or `struct`, and a use states it by inference, as it states a level. It is refused in a body, a local definition, a `concept` or a `satisfy`, and as the sort of an `induct` or a `struct` after its `:`, which is `Type` or `Prop`.

```crs
use /std/{Eq};

pub let keep(@A: Sort, a: A) -> A =
    a;

pub induct Maybe(A: Sort): pub Type
| some(A)
| none()
end

pub let carry(@A: Type, @x: A, @y: A, P: (A) -> Sort, p: Eq()(x, y), v: P(x)) -> P(y) =
    match p | refl() => v end;
```

Why: [`Prop` is strict, proof-irrelevant, and definitionally K](design/theory/prop-is-strict-proof-irrelevant-and-definitionally-k.md), [A universe level is implicit, cumulative, and settles by where it stands](design/theory/a-universe-level-is-implicit-cumulative-and-settles-by-where-it-stands.md) and [A declaration leaves a sort to its caller](design/theory/a-declaration-leaves-a-sort-to-its-caller.md).

### Function types

A function type is a parenthesized parameter list, `->` and its result. The list may be empty: `() -> T` is a function of no parameters, called `f()`.

A parameter is plain, implicit (`@`) or a witness (`use`), and its type follows the mark. A plain or implicit parameter is written `name: type`, or as its type alone. A witness parameter is its type alone, `use Show(A)`: a witness is never named.

```text
(Nat) -> Nat
(x: Nat, y: Nat) -> Nat
(@A: Type, x: A) -> A
(n: Nat, @Holds(0 < n)) -> Nat
(@A: Type, use Show(A), value: A) -> Str
```

A parameter's type and the result may name the parameters before it.

Every telescope is written this way: a function type, the parameters of a `let` or a `satisfy`, a constructor's payload and a declaration's type parameters. `use _`, which states no type, is refused in each. A definition and a declaration name their plain and `@` parameters.

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

An omitted implicit argument is inferred, from every part of its call and within the statement it stands in, and never picked between two solutions. Why: [A term is one where conversion says so](design/arithmetic/a-term-is-one-where-conversion-says-so.md) and [`curios-elab`](../curios-elab/README.md#postponement-is-a-parked-obligation-never-a-raw-substitution).

An omitted implicit whose type is a proposition, a bound, is filled where the proposition reduces to `True`, and otherwise proved from the facts in scope ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)). An omitted witness is resolved ([Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md)).

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

A parameter is a binder, or an [irrefutable pattern](#irrefutable-binder-patterns): a tuple, a structure or a constructor. It may be annotated where its type is not given by context.

A lambda's parameters are a telescope, as a function type's are: an annotation may name the parameters before it, including the names an earlier pattern binds. An earlier parameter shadows a module's binding of the same name in a later annotation, as it does in the body.

```text
(s: A, t: A, q: Eq()(s, t)) => proof(q)
((lo, hi), q: Eq()(lo, hi)) => lo
```

A parameter carries the mark of the slot it binds: an unmarked binder binds an explicit slot, `@name` an implicit one, and `use _` holds a witness slot's place. A witness has no binder, and a lambda never states a witness's type, which its expected function type states. A lambda with no expected function type has no `use` parameter: a function that declares one is a `let` with a telescope. After `@` comes a binder and never a type, so an implicit slot the body does not name is `@_`, or `@_: T` to state its type.

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

The parameters of a `let` or a `satisfy` are a signature, written as a [function type](#function-types)'s, except that a plain or `@` parameter is always named. One the body never names is `_: type`: `let positive(n: Nat, @_: Holds(0 < n)) -> Nat = n;` takes its bound as an implicit argument, which the body has as a fact. A witness parameter is `use Concept(args)`. A field written `label(params) = value` inside a tuple, a structure or a witness binds as a [lambda](#lambdas) does: a name or a [pattern](#irrefutable-binder-patterns), marked `@` or not, its annotation optional, or `use _`.

A binder may be an irrefutable tuple, structure or constructor pattern:

```crs
use /std/{Nat};

pub struct Point: pub Type {
    x: Nat,
    y: Nat,
}

pub let sum(pair: {Nat, Nat}, point: Point) -> Nat =
    let (a, b) = pair;
    let Point { x, y } = point;
    a + b + x + y;
```

A binding is in scope of its own value, so a local function may call itself. A binding that names itself states its type, since the body naming it cannot give it one, and is a plain name rather than a pattern. A binding whose value performs `!` cannot name itself, since the action runs before the binding exists. Bindings that name one another are one group, joined by `and` ([Recursive groups](#recursive-groups)).

So `let n: Nat = n + 1;` names the binding it declares, not an outer `n`, and is refused: a value names itself only under a lambda, where it is a recursive value computed the first time it is read.

### Irrefutable binder patterns

The binders of a `let`, a lambda and a function's parameters take nested tuple, structure and constructor patterns.

```crs
use /std/{Nat};

pub struct Point: pub Type {
    loc: {Nat, Nat},
    color: Nat,
}

pub let total(value: {Nat, {Nat, Nat}}, point: Point) -> Nat =
    let (a, (_, b)) = value;
    let Point { loc = (x, y), color } = point;
    a + b + x + y + color;
```

Such a pattern projects, and matches nothing at run time. A structure pattern names the structure its binder is; a head naming another structure, or nothing, is refused. A tuple pattern writes every field of its tuple, `_` for one it does not read: `(a, b)` takes apart a pair, and a triple is refused. `()`, which writes none, is the pattern of `{}`, as `()` is its value: `let () = print("hi")!;`.

A structure pattern writes its fields where its literal would write them, and may leave any of them out: it reads a value its literal builds whole.

- An unlabeled field reads the next plain field, whose order is part of the type.
- A field written `label = pattern` reads the field its label names, as a labeled entry of a literal does.
- A hidden field is written under its mark, as a literal's entry is: `@label = pattern`, or `@pattern` alone for the next hidden field of its run, ahead of the plain field the run precedes. A hidden field's label written with no mark is refused. Over `struct Sized: Type { @len: Nat, items: Vec(Nat, len) }`, `Sized { @len = n, items }` and `Sized { @n, items }` bind both.
- Each field reads past the one before it, so the fields are written in their declared order, and one written out of it is refused.
- A pattern over a concept value writes its methods alone; a superclass field has no binder.

`Point { x, y }` is the positional form, its fields binders named after the fields. Parentheses group a pattern without changing it: `((x, y))` is `(x, y)`.

A constructor pattern `tag(p, …)` holds its value to that one case, as a match's lone arm does: its payloads bind, an index the case solves refines what follows, and a value whose type has another case is refused as a match missing that arm is, naming the case. It is written where a match of one arm would be:

```crs
use /std/{Eq};

pub let symmetric(@A: Type, @x: A, @y: A, p: Eq()(x, y)) -> Eq()(y, x) =
    let refl() = p;
    Eq/refl();
```

A `let` written `let name(…) =` holds its value to the case `name`, and one written `let name(…) -> T =` defines a function: the `->` that states a function's result is what tells the two apart.

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

A `let`, a lambda and a function type are whole-term forms: a body or a tail extends to the end of the term around it. There is no ascription `term: type`: a `:` annotation stands only at a binder and a signature.

A `let` chain stands only as a body: a function's, a lambda's, a `match` arm's, a test's or a top-level value's, a program's final term, and the tail of another chain. Anywhere else — a `let`'s value, an argument, a list element, a field's value, a scrutinee, an operand, between parentheses — it is refused, so a value is never a chain, and a value that sequences is a function, `let name() -> T = …;`, called where its value is needed. Why: [Bang sequencing is governed by the region's type](design/surface/bang-sequencing-is-governed-by-the-regions-type.md).

An infix operand is an atom — a literal, a name, a sort (`Type`, `Prop` or `Sort`), a tuple, a tuple type, a structure literal, a goal, a `match`, or a term in parentheses — followed by any chain of calls, projections and postfix `!`. A `match`, headed or not, is an atom because `end` closes it. A whole-term form is no operand until it is put in parentheses.

```text
1 + match flag | true => 1 | false => 0 end
match chosen | left() => f | right() => g end(x)
1 + ((x) => x * x)(2)
```

A position that takes a whole term needs no parentheses: a call's argument, a list element, a field's value, a scrutinee and an arm's body, a `let` chain standing only in the last.

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

`&&` and `||` are calls of `And/and` and `Or/or`, as `+` is of `Add/add`, and not conditionals: neither operand is a fact in the other. So `x != 0 && 10 / x > 1` is refused, and `match | x != 0, 10 / x > 1 => true | _ => false end` is how a guard reaches the division.

`/` and `%` take an implicit proof of the precondition their concept declares, `Ok(divisor)`: on `Nat`, `Nat/NonZero(b)`, which is `Holds(b != 0)`, filled as any omitted bound is ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)).

Why: [A bound is stated in a decided proposition and discharged by reduction](design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md).

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

A `!` stands in a function's body, at any depth there: a declared function's, a local function's, a lambda's or a witness method's, and a test's body and a program's final term, each a function of none the runner calls. A top-level value and a witness's value hold none, and a `!` there is refused: a value that sequences is a function of none, called where its description is needed.

A `match` arm and a guard each keep a region of their own, and a local `let`'s value, which is a term and never a chain ([Whole-term forms and operand positions](#whole-term-forms-and-operand-positions)), shares the region around it; no `let !` header or `end` marks a region. Why: [Bang sequencing is governed by the region's type](design/surface/bang-sequencing-is-governed-by-the-regions-type.md).

An action of another monad than its region's is lifted through the `Monad/Lift` witness declared for that pair, and refused where none is ([Lifting between monads](#lifting-between-monads)).

`!=` is an operator, never a `!` followed by `=`.

### Host effects and `Io`

Every operation that touches the host — writing to a handle, reading a clock, calling a `foreign` function, exiting — answers an `Io(T)`: a description of a computation that yields a `T`, not the `T`, which only a program's final term performs ([Declarations and modules](#declarations-and-modules)). A host operation that can fail answers a `Try(M, E, A)` over its base monad, `Try(Io, Io/Error, File)` for `File/open`, and `!` sequences it in a `Try` region. Why: [Effects are descriptions, and the carrier has no eliminator](design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md).

```crs
use /std/{File, Path, Bytes, Try, Io};
use /std/Io/{Reader};

pub let contents() -> Try(Io, Io/Error, Bytes) =
    Try/scope((s) =>
        let f = File/open(s, Path/of_str("notes.txt"), File/Mode/read())!;
        let r = Reader/new(f)!;
        let text = Reader/rest(r)!;
        let () = Io/close(f)!;
        Try/pure(text));
```

### Lifting between monads

`/std/Monad/Lift(M, N)` is the embedding of monad `M` in monad `N`: one method, `Monad/lift`, which takes an `M(A)` to an `N(A)`, with the `Monad` witnesses of both as superclasses, so no embedding between non-monads can be declared. As for any witness, one `Monad/Lift` witness at most stands for each ordered pair of monads in a program, so which embedding runs is a fact of the program, not of a call.

For a monad `Job` that wraps a description, with its `Monad` witness, the embedding of `Io` is one line, and with it declared an `Io` action sequences in a `Job` region, the `!` inserting the lift:

```crs
use /std/{Io, Monad, print};

pub struct Job(A: Type): pub Type {
    Io(A),
}

satisfy Monad(Job) {
    pure(a) = Job { Io/pure(a) },
    bind(job, next) = Job { Io/bind(job.0, (a) => next(a).0) },
}

satisfy Monad/Lift(Io, Job) {
    lift(action) = Job { action },
}

pub let job() -> Job({}) =
    let () = print("hello\n")!;
    Monad/pure(());
```

`Monad/lift(action)` names the same embedding, its target monad read from the region, and a region's tail is lifted as an action in it is.

Embeddings never chain: an action of `Io` sequences in a `Sched` region only where `Monad/Lift(Io, Sched)` is declared. Why: [A fallible operation returns `Try`, and `!` lifts along declared edges](design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md).

## Pattern matching

### Match shell

A `match` has a scrutinee, an arm `| pattern => body` for each case — or for several, written as [alternatives](#alternatives) — and a closing `end`. Its result is the type expected where it stands, and a match writes no result type of its own. Why: [An arm is checked in a context specialized by index inversion](design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md).

An arm may be left out where its constructor's indices cannot meet the scrutinee's: a match over a `Sized(T)(n + 1)` needs no `empty()` arm, and one over a type with no constructors, `match contradiction end`, writes no arm at all. Where the facts in scope contradict each other, `proved(@False)` is the contradiction to match on ([Bounds from the facts in scope](#bounds-from-the-facts-in-scope)).

### Inductive patterns

An inductive pattern names a constructor and gives a pattern for each plain payload.

```crs
use /std/{Nat, Option};

pub let or_zero(option: Option(Nat)) -> Nat =
    match option | some(value) => value | none() => 0 end;
```

A constructor is named bare, in the namespace of the scrutinee's type: `Option/some(n)` is refused as a pattern. A pattern is written as the call that builds the value: each plain payload takes a pattern, in order, and an implicit (`@`) payload is left out, or written under `@` before the plain payload it precedes, where the arm names it. Two arms of one constructor may write different hidden payloads. A pattern takes no witness payload, so `use` is refused in one.

```crs
use /std/{Nat};

pub induct Vector(T: Type): (length: Nat) -> pub Type
| nil(): (0)
| cons(@length: Nat, head: T, tail: Vector(T)(length)): (length + 1)
end

pub let head_or(@n: Nat, vector: Vector(Nat)(n), fallback: Nat) -> Nat =
    match vector | nil() => fallback | cons(head, _) => head end;

pub let length(@T: Type, @n: Nat, vector: Vector(T)(n)) -> Nat =
    match vector | nil() => 0 | cons(@length, _, _) => length + 1 end;
```

A pattern nests inside a constructor's, a tuple's or a structure's field and in a run's element ([List case split](#list-case-split)), and parentheses group one. A successor's predecessor and an open run's rest are binders, so `[a, ..[b, ..t]]` is written `[a, b, ..t]`. Why: [Patterns are compiled where their types are known](design/surface/patterns-are-compiled-where-their-types-are-known.md).

```crs
use /std/{List, Nat, Option};

pub let first_or_zero(value: Option(List(Nat))) -> Nat =
    match value
    | some([head, ..tail]) => head + List/len(tail)
    | some([]) => 0
    | none() => 0
    end;
```

### Tuple and structure patterns

A tuple pattern `(p, q)` and a structure pattern `Point { x, y = q }` take a value apart field by field, each field a pattern of its own, written as the [irrefutable binder patterns](#irrefutable-binder-patterns) of a `let` are. A structure pattern names the structure its scrutinee is.

A tuple written as the scrutinee, `match (left, right)`, is matched column by column, from the left, each of its values a scrutinee of its own.

```crs
use /std/{Nat, Option};

pub let sum(left: Option(Nat), right: Option(Nat)) -> Nat =
    match (left, right)
    | (some(x), some(y)) => x + y
    | (some(x), none()) => x
    | (none(), _) => 0
    end;
```

### Boolean match

A `Bool` match has a `true` arm and a `false` arm in either order, or one of them followed by the bare `_` of [The arm that cannot fail](#the-arm-that-cannot-fail).

```crs
use /std/{Bool, Str};

pub let answer(condition: Bool) -> Str =
    match condition | true => "yes" | false => "no" end;
```

### Number patterns

A `Nat` is taken apart by its literals, its successors and a binder. A literal is the successors it builds over zero, and a successor arm binds a predecessor before a `+` and a step of one or more: `p + 2` takes the value apart two successors deep, so `p` is the value less two. The `+` takes whitespace on both sides, as an operator does: `pred+1` is no successor pattern, and `p + 0` is the binder `p`.

```crs
use /std/{Nat};

pub let induction(P: (Nat) -> Type, base: P(0), step: (k: Nat, P(k)) -> P(k + 1), n: Nat) -> P(n) =
    match n
    | 0 => base
    | predecessor + 1 => step(predecessor, induction(P, base, step, predecessor))
    end;
```

Literals and successor arms meet as every pattern does ([Binders beside patterns](#binders-beside-patterns)): a literal a successor arm takes is an exception written before it, as `7` is in `| 0 => … | 1 => … | 7 => … | p + 2 => …`, and a value a step leaves below it is another arm's, so `| 0 => … | p + 2 => …` leaves `1` to no arm and is refused. An arm that writes a binder takes every other value, and knows its value is none of the literals written before it. A match whose only literal is `0` is a case split: its arm that writes a binder is the successor's, so `| 0 => … | _ => …` is `| 0 => … | _ + 1 => …`.

```crs
use /std/{Nat, Str};

pub let name(tag: Nat) -> Str =
    match tag | 0 => "first" | 1 => "second" | _ => "otherwise" end;
```

A literal is any natural, written whole, as a numeral or as a character literal, which matches its scalar value.

A numeral pattern takes its type from its column as a [numeral](#numeric-literals) does from where it stands, so a `Bool` is taken apart by `0` and `1` as well as by `false` and `true`. A numeral pattern written with a sign is an `Int`'s, which no `Nat`, `Byte` or `Bool` takes: `| +1 =>` is refused where `| 1 =>` is taken.

An `Int` is dispatched on its keys, each written with its sign, beside an arm that writes a binder, which takes every other value and knows its value is none of the keys. A numeral without a sign and a character are no `Int`'s key, and are refused naming the key a reader means: `| 1 =>` as `+1`, and `| 'a' =>` as `+97`. With no arm past the keys, the value named as matched by none is the key of least magnitude no arm writes, the nonnegative first.

```crs
use /std/{Int, Str};

pub let classify(i: Int) -> Str =
    match i | -1 => "minus one" | +0 => "zero" | _ => "otherwise" end;
```

A `Byte` is dispatched on its values, each a numeral from `0` through `255` or a character whose code point fits a byte, as a [packed literal](#packed-literals)'s constant entries are, beside an arm that writes a binder. Each arm knows the byte it took and the arm past the keys knows the byte is none of them, as an `Int`'s do. Its `0` is a literal like any other, and a byte takes no successor pattern.

```crs
use /std/{Byte, Nat};

pub let class(b: Byte) -> Nat =
    match b | 0 => 0 | '\n' => 1 | _ => 2 end;
```

### Character and string dispatch

A `Char` is dispatched on character literals and a `Str` on string literals, beside an arm that writes a binder, which takes every other value.

```crs
use /std/{Char, Nat, Str};

pub let spacing(c: Char) -> Nat =
    match c | ' ' | '\t' => 0 | '\n' => 1 | _ => 2 end;

pub let command(name: Str) -> Nat =
    match name | "build" => 0 | "test" => 1 | _ => 2 end;
```

A character matches by its code point and a string by its bytes, a one-line and a block string alike, so a string pattern stands wherever a pattern does: `| some("--help") =>`, `| ["run", ..rest] =>`. A `Char` takes no numeral, and a `Str` no packed run.

A floating-point literal is no pattern: a pattern tests a value's identity, which tells `+0.0` from `-0.0` and one NaN from another, where `==` does neither, so a [guard](#guards) tests a float, `| x, x == 1.5 =>`.

### List case split

A list pattern is a run, written as the [list literal](#list-literals) that builds the value: `[]`, `[a, b]` for a list of exactly those elements, and `[a, b, ..rest]` for a list they begin. Each element is a pattern at the element's type. The rest stands last, after one element at least, and is a name or `_`.

```crs
use /std/{List, Nat, Option};

pub let sum(values: List(Nat)) -> Nat =
    match values | [] => 0 | [head, ..tail] => head + sum(tail) end;

pub let last(values: List(Nat)) -> Option(Nat) =
    match values
    | [] => Option/none()
    | [only] => Option/some(only)
    | [_, ..rest] => last(rest)
    end;

pub let present(values: List(Option(Nat))) -> List(Nat) =
    match values
    | [] => []
    | [none(), ..rest] => present(rest)
    | [some(value), ..rest] => [value, ..present(rest)]
    end;
```

A run takes its elements from the front, so a rest written before an element is refused.

### Packed case split

A `Bytes` pattern is a [run](#list-case-split) under the bracket `x[`, as the [packed literal](#packed-literals) is, with every rule of a list's, each element a pattern at `Byte`, a byte's literals among them ([Number patterns](#number-patterns)).

```crs
use /std/{Byte, Bytes, Nat};

pub let total(bytes: Bytes) -> Nat =
    match bytes
    | x[] => 0
    | x[head, ..tail] => Byte/to_nat(head) + total(tail)
    end;

pub let first_or_zero(bytes: Bytes) -> Byte =
    match bytes | x[] => 0 | x[head, .._] => head end;

pub let second_or_zero(bytes: Bytes) -> Nat =
    match bytes | x[_, second, .._] => Byte/to_nat(second) | _ => 0 end;

pub let escapes(bytes: Bytes) -> Nat =
    match bytes
    | x[] => 0
    | x['%', _, _, ..rest] => 1 + escapes(rest)
    | x[_, ..rest] => escapes(rest)
    end;
```

### Binders beside patterns

A binder — a name or `_` — may stand wherever a pattern may, beside the patterns other arms write in its place.

```crs
use /std/{Nat, Option};

pub let inner_or(value: Option(Option(Nat))) -> Nat =
    match value
    | some(some(inner)) => inner
    | some(_) => 0
    | none() => 1
    end;
```

Arms have no priority: the order they are written in never changes what a match means. Two arms may match one value only where the first written is a case of the other, so a general arm stands after its exceptions, as `some(_)` stands after `some(some(inner))`. A binder the arms before it leave one case to be reached at is that case's arm and rests on what the case states, as a pattern written there would: beside `| true =>`, `| c =>` knows `c` is `false`, and beside `| 0 =>` a binder is the successor's. One still reached at several cases rests on none of them. Three refusals hold the rule, each naming a value as the pattern that matches it, with `_` wherever no case was read:

- two arms that match one value, neither a case of the other: `(true, _)` and `(_, true)` meet at `(true, true)`;
- an arm no value reaches: one written after an arm it is a case of, and a final `_` after arms that cover every case already;
- a value no arm matches: `(some(_), none())`.

Why: [Patterns are compiled where their types are known](design/surface/patterns-are-compiled-where-their-types-are-known.md).

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

The alternatives of one arm match no value in common: an alternative that is a case of another is refused, and two that meet are refused, naming the value they share, so `(true, _) | (_, true)` is written `(true, _) | (false, true)`. Against the other arms, each alternative is held to the rules of [Binders beside patterns](#binders-beside-patterns).

Alternatives stand at the top of an arm, and no pattern holds them: `some('a' | 'b')` is written `some('a') | some('b')`.

### Guards

An arm may write guards after its patterns, each after a comma: a `Bool` term, which passes where it is `true`, or `pattern = term`, which passes where the term's value matches the pattern. An arm is taken by a value its pattern matches where every guard passes. Its guards run from the left, each only where the ones before it passed, and the names a bind guard binds are in scope in the guards after it and in the body.

```crs
use /std/{Nat, Option};

pub let size(value: Option(Nat)) -> Nat =
    match value
    | some(n), n > 9 => 2
    | some(n), n != 0, 100 / n > 20 => 1
    | _ => 0
    end;
```

Where a guard fails, the value goes on to the arms after it, in the order they are written, and a value only guarded arms reach is a value no arm matches. Guards never order patterns: two arms whose patterns meet are held to the rules of [Binders beside patterns](#binders-beside-patterns) as they would be with no guard.

A bind guard whose pattern cannot fail is refused: a binding with no case to test is a `let`. A guard takes no [alternatives](#alternatives); the value they would test is written as the head. A guard's term is an operand, so a `let`, a lambda or a function type stands in one in parentheses.

`&&` is no chain of guards: `a && b` evaluates both, and `| a, b =>` evaluates `b` only where `a` holds. Why: [An arm is taken by its patterns, then its guards](design/surface/an-arm-is-taken-by-its-patterns-then-its-guards.md).

### The arm that cannot fail

A final arm whose pattern takes nothing apart that could fail — most often a bare `_` — takes every value the arms before it leave.

```crs
use /std/{Nat, Option};

pub let or_zero(option: Option(Nat)) -> Nat =
    match option | some(value) => value | _ => 0 end;
```

After arms that each take the scrutinee itself apart — constructors, `true` and `false`, `Nat` shapes, runs under `[` and `x[` — a final arm may bind a name, which names the value those arms leave: under a head that is an expression, `match n + 1 | 0 => … | m => … end`, it is that value's one name. A name that is a constructor of the family the arms take apart is refused there, since it binds every value they leave where the case was likely meant: `| none =>` after `| some(x) =>` is written `| none() =>`. A literal's dispatch names no constructor, so over one any name binds. Under a head that is itself a name, the final arm's name only renames it, which `renaming-binder` reports ([`curios lint`](usage.md#lint)). A lone `_` with no other arm binds rather than matches.

### A match with no head

A `match` that writes no scrutinee is guards alone: each arm is a list of guards, the arms are tried from the top, and the final arm is `| _ =>`, which is required.

```crs
use /std/{Bool, Map, Nat, Str};

pub let found(condition: Bool, map: Map(Str, Nat), key: Str) -> Nat =
    match
    | condition => 0
    | some(value) = Map/try_get(map, key) => value
    | _ => 1
    end;
```

## Declarations and modules

A program is top-level items followed by one final term, the description it performs, which may sequence with `!` directly, as a function's body does. A module is top-level items alone.

The final term is performed through `/std/Program`, the concept whose `execute` says what a value of a type does as a whole program, so its type is any with a witness there:

- an `Io({})` is the program itself; no other result has a witness, so a final term that computes a value discards it explicitly;
- a `Try(Io, E, {})` whose `E` has a `Show` witness is run, and a failure is written to standard error as it shows and ends the program with status `1`;
- a `()` is an empty program.

A `/std/Test({})` closes no program: a test is [declared](#test-declarations) and run by `curios test`, which presents what it found.

Each tail of the final term closes the program: a tail is reached through a local `let`, and through a `match` to each of its arms, so two arms may close one program at two types. A written goal is asked at `Io({})`. A program's function keeps its own result type and is called as the final term, which `/std/Program` performs with no `Program/execute` written:

```crs
use /std/{Io, Try, File, Path, print};

let main() -> Try(Io, Io/Error, {}) =
    let text = File/read_all_text(Path/of_str("notes.txt"))!;
    print(text);

main()
```

### Top-level definitions

A top-level `let` states its type, by an annotation or as a function's parameters and result type.

That is also what tells an item from the final term: in a program, a top-level `let` with no annotation is no item but a local `let` that opens the final term. A top-level definition's value is checked at the type its annotation states, and holds no `!`: a definition that sequences is a function, of no parameters where it takes none.

A top-level value is computed the first time the program reads it, once, and not at all where nothing reads it, as a recursive value is: a program that never reads a value whose computation does not end runs to its end, and one that reads it stops where it reads it. Why: [A top-level value is computed where its unit is compiled](design/compilation/a-top-level-value-is-computed-where-its-unit-is-compiled.md).

```crs
use /std/{Nat, Option};

pub let zero: Nat =
    0;

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
    Test/assert("twenty-one doubled is forty-two", 21 * 2 == 42);

let _right_identity(n: Nat) -> Eq()(n + 0, n) =
    Eq/refl();
```

A test takes no parameters. A claim about every instance is a proposition, not a description: it is a `let` whose type states the claim and whose body proves it, checked on every build, as the second declaration above is, its `_` marking a declaration that exists for its type. Why: [A test is a declared description, and a proof is a `let`](design/tools/a-test-is-a-declared-description-and-a-proof-is-a-let.md).

A claim checked at chosen instances is a definition answering `Test({})`, run over a table: `test table = Test/all(List/map(cases, ((a, b)) => claim(a, b)));` is one test over the author's cases, reporting each case that fails by its position: the cases are independent, so each runs whatever the others found. `Test/step(label, test)` names a part of a test the same way, its label leading the path of each failure the part reports.

A test's path is its report line and what [`curios test --filter`](usage.md#test) selects by prefix, and nothing else names it: its name binds nothing, and is unique among its module's declarations. A test is never `pub`, and no documentation comment precedes one.

`Test(A)` is a monad: a test's body sequences assertions, `Io` actions and fallible actions with `!`, at its fixed type `Test({})` and through the declared embeddings, and ends in a `Test({})`. A failed assertion or a lifted error stops that test, past what `Test/all` gathers of its independent cases.

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

A public interface mentions only what its consumers reach. Why: [Privacy is scoped to a subtree](design/surface/privacy-is-scoped-to-a-subtree.md).

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
    let even(n: Nat) -> Bool = match n | 0 => true | p + 1 => odd(p) end
    and odd(n: Nat) -> Bool = match n | 0 => false | p + 1 => even(p) end;
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

pub struct Meters: pub Type {
    Nat,
}
```

The outer `pub` exports the type's name. The inner one exports building it and projecting it; without it, both are kept to the declaring module's subtree. A structure at `Prop` holds only propositions.

Parameters are written as an inductive's are: plain, `@`, or a premise `use Concept(args)` ([A type declared under a premise](#a-type-declared-under-a-premise)).

A field is plain or hidden. A hidden field is written under `@`, as `@label: type` or its type alone, and takes no position among the plain fields an author writes: a literal and a pattern leave it out or write it under its mark, `@label = …` or alone from the first of its run, a projection `.N` and a pattern's positional fields count only the plain ones, and a projection reads it by its label. Why: [A hidden member takes no position](design/theory/a-hidden-member-takes-no-position.md).

```crs
use /std/{Nat, Vec};
use /std/Bool/{Holds};

pub struct Positive: pub Type {
    n: Nat,
    @Holds(0 < n),
}

pub struct Sized: pub Type {
    @len: Nat,
    items: Vec(Nat, len),
}
```

A field takes no `use` member, since a structure's premise is a `use` parameter, and a tuple type's field takes no mark.

Structures whose fields name one another are one group, joined by `and`; a lone structure may name itself in its fields with nothing said ([Recursive groups](#recursive-groups)).

```crs
use /std/{Nat, Option};

pub struct Node: pub Type {
    value: Nat,
    next: Option(Edge),
}
pub and Edge: pub Type {
    weight: Nat,
    to: Node,
}
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

A hidden field is left out, filled as a call's omitted `@` argument is, or written under its mark before the plain field it precedes, as a call's hidden arguments are, from the first of their run: `@label = value`, the value alone, or `@_` to hold its place.

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

A spread copies the plain fields, and a hidden field is filled anew, as in any literal: the base's own is written where it is meant, `Sized { ..s, @len = s.len }`.

The spread stands first and once. Every override is labeled, and the overrides follow the declared order. The head may choose other parameters, and every copied and replaced field is checked at them, so dependent fields stay consistent.

Tuple and string literals have no update form. A spread in a list or a packed literal concatenates ([List literals](#list-literals)).

## Concepts and witnesses

A concept is an interface shaped as a record, a witness is a registered value of a concept at some types, and a `use` parameter asks the compiler to supply one. Together they give ad-hoc polymorphism.

### Concept declarations

A concept has parameters, possibly none, a representation sort, and a list of fields. The sort follows a structure's rule: `: pub Type` declares an *unsealed* concept, and `: Type` a *sealed* one, whose representation is private to its module's subtree, so only there may a witness be declared, a concept value be written, updated or taken apart. Resolution, `use` parameters and a method called by its path work alike for both, and the concept's name has its own visibility.

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

Concepts whose methods' types name one another's dictionaries are one group, joined by `and` ([Recursive groups](#recursive-groups)). A superclass cycle — `use B(A)` in `A` and `use A(B)` in `B` — is refused, grouped or not.

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

pub struct Hours: pub Type {
    Nat,
}

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

A concept at `Prop` has proof-irrelevant witnesses, which leave nothing at run time.

### Witness declarations

`satisfy` registers a witness, which has no name. Its type is a concept application, written as any call is — `satisfy Named(@Nat) { … }` over `concept Named(@A: Type)` — and its body supplies the concept's fields, or holds `..` alone, for the compiler to write them ([Derived witnesses](#derived-witnesses)).

```crs
use /std/{Nat, Show};

pub struct Meters: pub Type {
    Nat,
}

satisfy Show(Meters) {
    show(m) = Nat/to_str(m.0),
}
```

A witness may take implicit parameters and require other witnesses, in a telescope separated from the concept application by `=>`. It takes no explicit parameter.

```crs
use /std/{List, Show, Str};

pub struct Bag(A: Type): pub Type {
    List(A),
}

satisfy (@A: Type, use Show(A)) => Show(Bag(A)) {
    show(bag) = List/fold(bag.0, "", (value, result) => Str/concat(result, Show/show(value))),
}
```

A witness is keyed by its concept and by the head of each of the concept's arguments, which its signature spells: an inductive, a structure, an intrinsic type or a tuple type's shape, applied or not, or a family applied in part, written as a lambda, `(A: Type) => State(S, A)`. A function type has no key. One witness at most stands at a key in a whole program, declared where its concept or a head of its key is, and its `use` premises are smaller than its own application. Why: [Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md).

Witnesses that resolve through one another are one group, joined by `and` ([Recursive groups](#recursive-groups)). A lone witness may resolve through its own key with nothing said. A method written by another method of its witness — `neq(a, b) = Bool/not(a == b)` — reads that method alone, so only a method that reaches itself makes its witness recursive.

```crs
use /std/{Nat, Show, Str};

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
    show(f) =
        match f
        | nil() => ""
        | cons(t, rest) => Str/concat(Show/show(t), Show/show(rest))
        end,
}
```

A concept with no parameter has no registered witness: a value of it is supplied to a `use` parameter.

A second dictionary for an unsealed concept at the same key is an ordinary concept value, supplied explicitly; a sealed concept refuses the literal outside its module.

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

pub struct Point: pub Type {
    x: Nat,
    y: Nat,
}

pub induct Tree(A: Type): pub Type
| leaf(A)
| node(Tree(A), Tree(A))
end

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

The key is a declared `induct` or `struct` whose representation is open where the witness is declared, and every payload goes through its own witness, resolved in the witness's scope. What each concept's body is, is [A witness body may be written by the compiler](design/surface/a-witness-body-may-be-written-by-the-compiler.md)'s.

### Superclass fields in literals

A concept's superclasses are hidden members of a concept value. They take no position when it is read — a projection `.N` and a pattern's positional fields count the methods alone — and a literal's entries follow the declaration as a call's arguments follow a function's parameters: the fields in order, and before each, `use value` entries filling the superclass slots before it, from the first. A slot left out is resolved, and `use _` holds a slot's place to the same effect, so `Both { use _, use second, both = … }` supplies the second of two superclasses alone.

```text
Ord { use custom_eql, ord(a, b) = reversed(a, b) }
```

A witness's body writes none: a `use` entry in a `satisfy` is refused by name, and resolution fills every superclass of a registered witness.

In an update, a spread copies the superclasses from the base. A `use value` after the spread replaces the next superclass slot after the entries before it, and `use _` leaves one as the spread copied it.

### Witness parameters and arguments

A witness parameter is written `use Concept(args)` in a function type or a definition's parameters. It has no name, and joins the witnesses the body resolves from, which are the `use` members in scope. Its type reduces to a concept application; any other type is refused where the parameter is declared, and a proof to be discharged is an `@` parameter instead. A dictionary a program names is an ordinary value — a `let`, or a plain or `@` parameter of the concept's type — and fills a witness slot through `use value`.

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

The parameter has no name. Where the type is written, the slot is a call's: `Slot(Nat, Str)` leaves it to resolution, and `Slot(Nat, use reversed, Str)` fills it. A structure literal with a bare head takes the dictionary from its expected type, and an inductive's constructors take the parameter as a witness slot. A concept takes no `use` parameter. Why: [Concepts resolve with global coherence](design/surface/concepts-resolve-with-global-coherence.md).

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

How each wire type crosses is [`curios-abi`](../curios-abi/README.md)'s.

The operands follow the name, as a function's parameters do, each its wire type alone, with no name. No colon stands between the name and the operands, since a declaration's signature is not its type: a call yields an `Io` of the result, so `frobnicate` above is a `(Nat, Bytes) -> Io(Nat)`. Every `foreign` is a function, of no operand where its list is empty: `random` above is a `() -> Io(Nat)`, called `random()`.

A result is a wire type, or a braced list of labeled wire types, the [tuple type](#tuple-types) the call yields. Every field is labeled, each by a name written once; `_`, which a tuple type reads as no label, is refused. `{}` is no result at all, the unit type; `()` is the unit value and never stands here. Two or more fields are a tuple the program projects by label.

A tuple type's labels are part of it, so nothing is invented, dropped or moved: the fields cross in the order written, and a reference result — `Bytes`, `Handle`, `List(T)` or `Option(T)` — crosses in whatever position it is written, as many as the row has. A single result in braces is refused: it is written bare, not `{value: T}`, since one result crosses as itself, and a brace of one field would name a tuple the row cannot carry.

`Never` in place of a result says the call does not return, which differs from returning nothing. It is no type and stands nowhere else. A declaration that states it takes the type its call yields as an implicit argument before its own, so `abort` above is `(@A: Type, Byte) -> Io(A)`, a description that ends the program and so fits a region of any type; `/std/Proc/exit` is one. That argument joins the operands, so `foreign gone() -> Never;`, of no operand, is `(@A: Type) -> Io(A)` and is called `gone()`.

`List` and `Option` do not nest, and their elements are `Nat`, `Int`, `Bool`, `Bytes` or `Handle`: `List(List(T))`, `List(Flt)`, `List(Byte)` and an `Option` inside a `List` are refused, and a list of bytes is the `Bytes` a row already spells.

## Equality and proofs

Propositional equality, `Eq`, is an ordinary indexed inductive proposition of `/std/Eq`. Its proofs use the constructors, functions and `match` forms of any inductive; `Eq` has no syntax of its own.

```crs
use /std/{Eq};

pub let sym(@A: Type, @x: A, @y: A, proof: Eq()(x, y)) -> Eq()(y, x) =
    match proof | refl() => Eq/refl() end;
```


### Bounds from the facts in scope

A bound is stated and proved in `/std/Bool`'s vocabulary, which a program imports by name: `use /std/Bool/{True, False, Holds};`. `Holds(b)` is the proposition that the decision `b` holds: it reduces to `True`, proved by `True/qed()`, where `b` is `true`, and to the empty `False` where it is not.

A bound that reduction does not decide is proved by the compiler where it follows from the facts in scope — the hypotheses, their proof fields and the guards of the arms around it — by linear arithmetic. Why: [A bound that follows from the facts in scope is proved by the elaborator](design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md).

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
    match n > 20
    | true => match proved(@False) end
    | false => n
    end;
```

A bound the facts do not imply is refused, the report naming the facts it read and values under which they hold and the bound fails.

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
| `[a, b, ..rest]` / `x[a, ..rest]` | A run — a list or `Bytes` literal written as a pattern, its rest a binder |
| `\| pattern, guard, … =>` | An arm taken where its pattern matches and each guard passes: a `Bool`, or `pattern = term` |
| `match \| guard, … => … \| _ => … end` | A match with no head: guards alone, closed by a default |
| `test name = body;` | Test — a `/std/Test` description, run by `curios test` |
| `satisfy C(args) { ... }` | Registered witness, which has no name |
| `satisfy C(args) { .. }` | Derived witness — the compiler writes the body |
| `satisfy (@A: Type, use C(A)) => D(args) { ... }` | Witness under a telescope |
| `use /std/{Nat};` / `use /std/*;` / `use /{Name};` | Import a group, what a module exports, or a name of the compilation root |
| `… and …` | [Recursive group](#recursive-groups) — `let`, `induct`, `struct`, `concept` or `satisfy` |
