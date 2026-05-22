# Curios Syntax Reference

## Lexical basics

**Identifiers** are sequences of alphanumeric characters and underscores. Keywords are reserved and may not be used as identifiers.

**Keywords**: `let` `rec` `and` `pub` `match` `split` `mod` `use` `end` `def`

**Paths** are slash-separated identifiers: `Foo/bar`, `Std/List/length`. They refer to values in nested modules.

**Whitespace** (spaces, tabs, newlines) is insignificant except as a separator between tokens.

**Comments** start with `--` and extend to the end of the line.

## Source files

A source file is either a **module** (a sequence of top-level declarations) or an **entrypoint** (a sequence of top-level declarations followed by a term). The CLI and tests use entrypoints; imported files are modules.

## Top-level declarations

All top-level declarations accept an optional `pub` prefix to export the name to importing modules.

### Value binding

```
pub let name : Type = body;
```

Binds `name` to `body` of type `Type`. The semicolon is required.

### Recursive bindings

```
pub rec f : A = body_f
pub and g : B = body_g;
```

Declares a group of mutually recursive bindings. Each binding in the group independently accepts `pub`. The entire group is terminated by a single semicolon after the last binding.

A single-binding group is valid:

```
pub rec fact : Nat -> Nat = n =>
    Nat.fold n : _ => Nat;
    | 0 => 1;
    | pred ih => Nat.mul (Nat.add pred 1) ih;
```

### Type definition

```
pub def Name (witness) items... end
```

Defines a named type backed by `witness`. The body is a sequence of top-level declarations available within the definition. Used to introduce newtypes and associated operations.

### Submodule

External reference (the module is loaded from a file):

```
pub mod Name;
```

Inline definition:

```
pub mod Name
  items...
end
```

### Import

```
pub use path;
```

Brings the names exported by `path` into scope. An absolute path (from the root module) is written with a leading `/`:

```
use /Std/Prelude;
```

## Terms

### Application

```
f a b c
```

Function application is left-associative juxtaposition. Each argument must be an **atomic term** — a literal, a name, a parenthesized term, or another atomic form listed below. To pass a compound expression as an argument, wrap it in parentheses:

```
Nat.add (Nat.mul 2 3) 1
```

### Lambda

```
label => body
```

Introduces a function with parameter `label` in scope in `body`.

### Local let

```
let name : Type = body;
tail
```

Binds `name` to `body` for the rest of `tail`. The type annotation and semicolon are required.

### Local rec

```
rec f : A = body_f
and g : B = body_g;
tail
```

Mutually recursive local bindings. Unlike top-level `rec`, the `and` clauses do not accept `pub`. Terminated by a semicolon; the remaining expression `tail` follows.

### Match

Eliminates an atom type:

```
match head : label => Motive;
| 'foo => body_foo;
| 'bar => body_bar;
```

`label` names the scrutinee in the motive. Every atom in the type must have a branch. No default branch.

### Split

Eliminates a tuple:

```
split head : label => Motive;
| (f1, f2, ...) => body
```

Binds each field of `head` to the listed names. There is exactly one branch.

### Coercion

```
Name.into value
Name.from value
```

Converts between a defined type and its witness type.

## Types

### Universe

```
Type
```

The type of all types.

### Function type

Non-dependent (output does not mention the input):

```
A -> B
```

Dependent (output may mention the input by `label`):

```
(label : A) -> B
```

### Atom type

```
'[foo, bar, baz]
```

A finite set of atoms. The order of labels does not matter.

### Tuple type

```
{ A, B }
{ label1 : A, label2 : B }
```

Fields may optionally be named. Labels are used for documentation only; they do not affect the type's identity.

### Array type

```
Arr T
```

A homogeneous array of elements of type `T`. `T` must be atomic; write `Arr (Arr Nat)` for nested arrays.

### Primitive types

| Type  | Description         |
|-------|---------------------|
| `Nat` | Natural number (u32) |
| `Int` | Signed integer (i32) |
| `Flt` | Single-precision float (f32) |
| `Bin` | Byte sequence |

## Literals

### Natural numbers

```
42
'a'     -- character code point as Nat
```

Char literals support escapes: `'\n'` `'\t'` `'\r'` `'\\'` `'\''`.

### Integers

A sign is required:

```
+42
-7
```

### Floats

A sign, a decimal point, and at least one digit after the point are all required:

```
+1.0
-3.14
+6.022e23
```

### Strings

```
"hello, world"
```

Supports escapes: `\n` `\t` `\r` `\\` `\"`. A string literal has type `Bin`.

### Byte sequences

Raw bytes written as consecutive hex pairs, each prefixed with `\`:

```
\ef\bb\bf
```

Has type `Bin`.

### Arrays

```
[1, 2, 3]
```

### Atoms

```
'foo
```

A single atom value. Its type is any atom type that includes `foo`.

### Tuples

```
(a, b)
(a, b, c)
```

At least two elements are required.

## Primitive operations

All primitive operations are prefix and take **atomic** arguments. Parenthesise compound arguments.

### Nat

| Operation        | Arity | Description           |
|------------------|-------|-----------------------|
| `Nat.add a b`    | 2     | Addition              |
| `Nat.sub a b`    | 2     | Subtraction           |
| `Nat.mul a b`    | 2     | Multiplication        |
| `Nat.div a b`    | 2     | Division              |
| `Nat.rem a b`    | 2     | Remainder             |
| `Nat.eql a b`    | 2     | Equality              |
| `Nat.neq a b`    | 2     | Inequality            |
| `Nat.lt a b`     | 2     | Less than             |
| `Nat.gt a b`     | 2     | Greater than          |
| `Nat.lte a b`    | 2     | Less than or equal    |
| `Nat.gte a b`    | 2     | Greater than or equal |
| `Nat.to_int a`   | 1     | Convert to Int        |
| `Nat.to_flt a`   | 1     | Convert to Flt        |

### Nat.fold

```
Nat.fold n : label => Motive;
| 0 => zero_case;
| pred ih => succ_case;
```

Structural recursion over a natural number. `pred` is the predecessor; `ih` is the induction hypothesis (result for `pred`).

### Nat.match

```
Nat.match n : label => Motive;
| 0 => body;
| 3 => body;
| _ => default;
```

Pattern match on specific natural number values. The default branch (`| _ =>`) is required and must appear last.

### Int

| Operation        | Arity | Description           |
|------------------|-------|-----------------------|
| `Int.add a b`    | 2     | Addition              |
| `Int.sub a b`    | 2     | Subtraction           |
| `Int.mul a b`    | 2     | Multiplication        |
| `Int.div a b`    | 2     | Division              |
| `Int.rem a b`    | 2     | Remainder             |
| `Int.eql a b`    | 2     | Equality              |
| `Int.neq a b`    | 2     | Inequality            |
| `Int.lt a b`     | 2     | Less than             |
| `Int.gt a b`     | 2     | Greater than          |
| `Int.lte a b`    | 2     | Less than or equal    |
| `Int.gte a b`    | 2     | Greater than or equal |
| `Int.to_nat a`   | 1     | Convert to Nat        |
| `Int.to_flt a`   | 1     | Convert to Flt        |

### Flt

| Operation          | Arity | Description           |
|--------------------|-------|-----------------------|
| `Flt.add a b`      | 2     | Addition              |
| `Flt.sub a b`      | 2     | Subtraction           |
| `Flt.mul a b`      | 2     | Multiplication        |
| `Flt.div a b`      | 2     | Division              |
| `Flt.eql a b`      | 2     | Equality              |
| `Flt.neq a b`      | 2     | Inequality            |
| `Flt.lt a b`       | 2     | Less than             |
| `Flt.gt a b`       | 2     | Greater than          |
| `Flt.lte a b`      | 2     | Less than or equal    |
| `Flt.gte a b`      | 2     | Greater than or equal |
| `Flt.min a b`      | 2     | Minimum               |
| `Flt.max a b`      | 2     | Maximum               |
| `Flt.neg a`        | 1     | Negation              |
| `Flt.abs a`        | 1     | Absolute value        |
| `Flt.sqrt a`       | 1     | Square root           |
| `Flt.floor a`      | 1     | Floor                 |
| `Flt.ceil a`       | 1     | Ceiling               |
| `Flt.trunc a`      | 1     | Truncate toward zero  |
| `Flt.nearest a`    | 1     | Round to nearest      |
| `Flt.to_nat a`     | 1     | Convert to Nat        |
| `Flt.to_int a`     | 1     | Convert to Int        |

### Bin

| Operation              | Arity    | Description                         |
|------------------------|----------|-------------------------------------|
| `Bin.len a`            | 1        | Byte length                         |
| `Bin.eql a b`          | 2        | Equality                            |
| `Bin.get a i`          | 2        | Byte at index `i`                   |
| `Bin.slice a start end`| 3        | Subsequence from `start` to `end`   |
| `Bin.append a byte`    | 2        | Append a single byte                |
| `Bin.concat a, b, ...` | variadic | Concatenate any number of sequences |

### Arr

| Operation               | Arity    | Description                         |
|-------------------------|----------|-------------------------------------|
| `Arr.len a`             | 1        | Element count                       |
| `Arr.get a i`           | 2        | Element at index `i`                |
| `Arr.slice a start end` | 3        | Subarray from `start` to `end`      |
| `Arr.append a elem`     | 2        | Append a single element             |
| `Arr.concat a, b, ...`  | variadic | Concatenate any number of arrays    |

`Bin.concat` and `Arr.concat` take comma-separated atomic arguments (not juxtaposed):

```
Bin.concat "hello" ", " "world"
Arr.concat [1, 2] [3, 4] [5]
```

## Idioms

### Sum types

Curios has no built-in sum type. The idiom is a dependent tuple whose second field's type is determined by the first field, an atom drawn from a finite set.

**Definition**

```
let Result : (_ : Type) -> (_ : Type) -> Type = A => B => {
    tag : '[ok, err],
    match tag : _ => Type;
    | 'ok  => A;
    | 'err => B; };
```

The first field `tag` is an atom type listing all variants. The second field is a `match` on `tag` that selects the payload type for each variant.

**Construction**

```
let good : Result Nat Bin = ('ok,  42);
let bad  : Result Nat Bin = ('err, "something went wrong");
```

A value is a two-element tuple of the variant atom and its payload.

**Elimination**

Use `split` to unpack the tuple, then `match` on the tag with the payload in scope:

```
let unwrap_or : (A : Type) -> (_ : Result A Bin) -> (_ : A) -> A = A => r => default =>
    split r : _ => A; | (tag, payload) =>
    match tag : _ => A;
    | 'ok  => payload;
    | 'err => default;;
```

The `;;` at the end closes the last `match` branch and then the surrounding `let` (or whatever expression contains the `split`).

### Recursive types

A recursive type uses a top-level `rec` binding that refers to itself in its own body. Combined with the sum type idiom, this gives linked lists, trees, and similar structures.

**Definition**

```
rec List : (_ : Type) -> Type = A => {
    tag : '[nil, cons],
    match tag : _ => Type;
    | 'nil  => '[unit];
    | 'cons => { A, List A }; };
```

`'[unit]` serves as a single-atom placeholder for the empty payload — there is no built-in unit type. The `cons` branch holds the head element and a recursive `List A` tail.

**Construction**

```
let empty : List Nat        = ('nil,  'unit);
let one   : List Nat        = ('cons, (1, ('nil, 'unit)));
let three : List Nat        = ('cons, (1, ('cons, (2, ('cons, (3, ('nil, 'unit)))))));
```

**Elimination**

A recursive function over the list is itself written with `rec`:

```
rec length : (A : Type) -> (_ : List A) -> Nat = A => list =>
    split list : _ => Nat; | (tag, payload) =>
    match tag : _ => Nat;
    | 'nil  => 0;
    | 'cons =>
        split payload : _ => Nat; | (head, tail) =>
        Nat.add 1 (length A tail);;
```

The `;;` at the end closes the last `match` branch and then the top-level `rec` binding.
