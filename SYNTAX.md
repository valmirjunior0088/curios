# Curios Syntax Reference

## Lexical basics

**Identifiers** are sequences of alphanumeric characters and underscores. Keywords are reserved and may not be used as identifiers.

**Keywords**: `let` `rec` `and` `pub` `match` `mod` `use` `end` `false` `true`

**Paths** are slash-separated identifiers: `Foo/bar`, `Std/List/length`. They refer to values in nested modules.

The primitive type names (`Nat` `Int` `Flt` `Bin` `Arr` `Bln`) and the universe `Type` are **not** reserved as path segments, so a module may share the name of the type it operates on — e.g. a module `Nat` whose members are reached as `Nat/double`. This is unambiguous because member access uses a slash (`Nat/double`) while primitive operations use a dot (`Nat.add`). A **bare** occurrence always denotes the primitive (the bare form wins), so such a module is reached only through its members (`Nat/…`) or `use`, never by a bare reference.

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

A function may be defined with shorthand that names its parameters and result type:

```
pub let add(a : Nat, b : Nat) -> Nat = Nat.add(a, b);
```

This is sugar for binding `add`, of type `(a : Nat, b : Nat) -> Nat`, to the lambda `(a, b) => Nat.add(a, b)`.

### Recursive bindings

```
pub rec f : A = body_f
pub and g : B = body_g;
```

Declares a group of mutually recursive bindings. Each binding in the group independently accepts `pub`. The entire group is terminated by a single semicolon after the last binding. Each binding accepts the same forms as `let` — either `name : type = value` or the function-definition shorthand `name(params) -> R = body`:

```
pub rec fact(n : Nat) -> Nat =
    match n : Nat
    | 0 => 1
    | pred ih => Nat.mul(Nat.add(pred, 1), ih)
    end;
```

Every `match` is closed by `end`; the trailing `;` then closes the recursive-binding group.

A binding's value may be any term, not only a lambda. In particular it can be a call that references other members of the group, so combinator-style definitions can be written point-free rather than eta-expanded:

```
pub rec decode : Parse(Value) = (input, pos) => -- … uses parse_arr, parse_obj …
pub and parse_arr : Parse(Value) =
    Parse/bind(Nat, Value, Parse/take_byte('['), _ => -- … uses decode … );
```

Members may refer to one another freely through such calls. The sole exception: two bindings whose values are *calls that each require the other's result* form a cycle with no way to tie the knot, and the group is rejected.

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

When using an absolute path, the first segment must refer to a `pub mod` at the root. A private root module cannot be accessed via an absolute path.

## Terms

### Application

```
f(a, b, c)
```

Function application is written with parentheses and comma-separated arguments. A call may supply several arguments at once; for a function whose result is itself a function, calls chain:

```
f(a, b)
f(a)(b)
```

Arguments are arbitrary terms — there is no need to parenthesise compound arguments beyond the call's own parentheses:

```
Nat.add(Nat.mul(2, 3), 1)
```

### Lambda

```
label => body
(a, b) => body
```

Introduces a function. A single parameter may be written without parentheses; multiple parameters are parenthesised and comma-separated. All parameters are in scope in `body`.

### Local let

```
let name : Type = body;
tail
```

Binds `name` to `body` for the rest of `tail`. The type annotation and semicolon are required. The function-definition shorthand is also available locally:

```
let add(a : Nat, b : Nat) -> Nat = Nat.add(a, b);
tail
```

### Local rec

```
rec f : A = body_f
and g : B = body_g;
tail
```

Mutually recursive local bindings. Unlike top-level `rec`, the `and` clauses do not accept `pub`. Terminated by a semicolon; the remaining expression `tail` follows.

### Match

`match` is the single elimination form. The branch shapes determine which kind of value is eliminated, and the head's type must agree.

```
match head : motive
| ...
end
```

The `motive` gives the result type. It may name the scrutinee — `label => Type` — or omit the name when the result type does not depend on the scrutinee — just `Type`. Every `match` is closed by `end`; branches are introduced by `|` and are bounded by the next `|` or by `end`.

**Atoms** — one branch per atom in the head's atom type; no default branch:

```
match tag : Type
| 'foo => body_foo
| 'bar => body_bar
end
```

**Booleans** — both branches required, either order:

```
match cond : Bin
| true  => true_body
| false => false_body
end
```

**Structural induction over `Nat`** — `| 0` is the base case; `| pred ih` binds the predecessor and the result already computed for it (`ih`, the induction hypothesis):

```
match n : Nat
| 0 => zero_case
| pred ih => succ_case
end
```

**Sparse dispatch on `Nat`** — specific values plus a mandatory `| _` default that must appear last:

```
match n : Nat
| 0 => body
| 3 => body
| _ => default
end
```

### Field access

Reads a field from a tuple by numeric index:

```
e.0
e.1
```

Indices are zero-based. Chains are supported: `e.0.1` reads field 1 of field 0 of `e`.

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

Dependent, one or more named parameters (each parameter may be mentioned by later parameters and by the output):

```
(a : A) -> B
(a : A, b : B) -> C
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

Fields may optionally be named. Labels are used for documentation only; they do not affect the type's identity. The empty tuple type `{}` (whose only value is `()`) serves as a unit.

### Array type

```
Arr(T)
```

A homogeneous array of elements of type `T`. Write `Arr(Arr(Nat))` for nested arrays.

### Primitive types

| Type  | Description                  |
| ----- | ---------------------------- |
| `Bln` | Boolean                      |
| `Nat` | Natural number (u32)         |
| `Int` | Signed integer (i32)         |
| `Flt` | Single-precision float (f32) |
| `Bin` | Byte sequence                |

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

### Booleans

```
false
true
```

Boolean literals. Their type is `Bln`.

### Tuples

```
()
(a, b)
(a, b, c)
```

Zero or more elements.

## Primitive operations

All primitive operations use call syntax: the operation name followed by parenthesised, comma-separated arguments. Arguments are arbitrary terms.

```
Nat.add(a, b)
Bin.slice(s, start, end)
```

### Nat

| Operation        | Arity | Description           | Returns |
| ---------------- | ----- | --------------------- | ------- |
| `Nat.add(a, b)`  | 2     | Addition              | `Nat`   |
| `Nat.sub(a, b)`  | 2     | Subtraction           | `Nat`   |
| `Nat.mul(a, b)`  | 2     | Multiplication        | `Nat`   |
| `Nat.div(a, b)`  | 2     | Division              | `Nat`   |
| `Nat.rem(a, b)`  | 2     | Remainder             | `Nat`   |
| `Nat.eql(a, b)`  | 2     | Equality              | `Bln`   |
| `Nat.neq(a, b)`  | 2     | Inequality            | `Bln`   |
| `Nat.lt(a, b)`   | 2     | Less than             | `Bln`   |
| `Nat.gt(a, b)`   | 2     | Greater than          | `Bln`   |
| `Nat.lte(a, b)`  | 2     | Less than or equal    | `Bln`   |
| `Nat.gte(a, b)`  | 2     | Greater than or equal | `Bln`   |
| `Nat.to_int(a)`  | 1     | Convert to Int        | `Int`   |
| `Nat.to_flt(a)`  | 1     | Convert to Flt        | `Flt`   |
| `Nat.to_str(a)`  | 1     | Convert to Bin        | `Bin`   |
| `Nat.succ(a)`    | 1     | Successor (add 1)     | `Nat`   |
| `Nat.succ(n, a)` | 2     | Add `n` successors    | `Nat`   |

Structural induction and sparse dispatch over a `Nat` are written with [`match`](#match) (the `| 0` / `| pred ih` and `| n` / `| _` branch shapes, respectively).

### Int

| Operation       | Arity | Description           | Returns |
| --------------- | ----- | --------------------- | ------- |
| `Int.add(a, b)` | 2     | Addition              | `Int`   |
| `Int.sub(a, b)` | 2     | Subtraction           | `Int`   |
| `Int.mul(a, b)` | 2     | Multiplication        | `Int`   |
| `Int.div(a, b)` | 2     | Division              | `Int`   |
| `Int.rem(a, b)` | 2     | Remainder             | `Int`   |
| `Int.eql(a, b)` | 2     | Equality              | `Bln`   |
| `Int.neq(a, b)` | 2     | Inequality            | `Bln`   |
| `Int.lt(a, b)`  | 2     | Less than             | `Bln`   |
| `Int.gt(a, b)`  | 2     | Greater than          | `Bln`   |
| `Int.lte(a, b)` | 2     | Less than or equal    | `Bln`   |
| `Int.gte(a, b)` | 2     | Greater than or equal | `Bln`   |
| `Int.to_nat(a)` | 1     | Convert to Nat        | `Nat`   |
| `Int.to_flt(a)` | 1     | Convert to Flt        | `Flt`   |
| `Int.to_str(a)` | 1     | Convert to Bin        | `Bin`   |

### Flt

| Operation        | Arity | Description           | Returns |
| ---------------- | ----- | --------------------- | ------- |
| `Flt.add(a, b)`  | 2     | Addition              | `Flt`   |
| `Flt.sub(a, b)`  | 2     | Subtraction           | `Flt`   |
| `Flt.mul(a, b)`  | 2     | Multiplication        | `Flt`   |
| `Flt.div(a, b)`  | 2     | Division              | `Flt`   |
| `Flt.eql(a, b)`  | 2     | Equality              | `Bln`   |
| `Flt.neq(a, b)`  | 2     | Inequality            | `Bln`   |
| `Flt.lt(a, b)`   | 2     | Less than             | `Bln`   |
| `Flt.gt(a, b)`   | 2     | Greater than          | `Bln`   |
| `Flt.lte(a, b)`  | 2     | Less than or equal    | `Bln`   |
| `Flt.gte(a, b)`  | 2     | Greater than or equal | `Bln`   |
| `Flt.min(a, b)`  | 2     | Minimum               | `Flt`   |
| `Flt.max(a, b)`  | 2     | Maximum               | `Flt`   |
| `Flt.neg(a)`     | 1     | Negation              | `Flt`   |
| `Flt.abs(a)`     | 1     | Absolute value        | `Flt`   |
| `Flt.sqrt(a)`    | 1     | Square root           | `Flt`   |
| `Flt.floor(a)`   | 1     | Floor                 | `Flt`   |
| `Flt.ceil(a)`    | 1     | Ceiling               | `Flt`   |
| `Flt.trunc(a)`   | 1     | Truncate toward zero  | `Flt`   |
| `Flt.nearest(a)` | 1     | Round to nearest      | `Flt`   |
| `Flt.to_nat(a)`  | 1     | Convert to Nat        | `Nat`   |
| `Flt.to_int(a)`  | 1     | Convert to Int        | `Int`   |
| `Flt.to_str(a)`  | 1     | Convert to Bin        | `Bin`   |

### Bin

| Operation                  | Arity    | Description                         | Returns |
| -------------------------- | -------- | ----------------------------------- | ------- |
| `Bin.len(a)`               | 1        | Byte length                         | `Nat`   |
| `Bin.eql(a, b)`            | 2        | Equality                            | `Bln`   |
| `Bin.get(a, i)`            | 2        | Byte at index `i`                   | `Nat`   |
| `Bin.slice(a, start, end)` | 3        | Subsequence from `start` to `end`   | `Bin`   |
| `Bin.append(a, byte)`      | 2        | Append a single byte                | `Bin`   |
| `Bin.concat(a, b, ...)`    | variadic | Concatenate any number of sequences | `Bin`   |

### Arr

| Operation                  | Arity    | Description                      | Returns  |
| -------------------------- | -------- | -------------------------------- | -------- |
| `Arr.len(a)`               | 1        | Element count                    | `Nat`    |
| `Arr.get(a, i)`            | 2        | Element at index `i`             | `T`      |
| `Arr.slice(a, start, end)` | 3        | Subarray from `start` to `end`   | `Arr(T)` |
| `Arr.append(a, elem)`      | 2        | Append a single element          | `Arr(T)` |
| `Arr.concat(a, b, ...)`    | variadic | Concatenate any number of arrays | `Arr(T)` |

`Bin.concat` and `Arr.concat` take any number of comma-separated arguments:

```
Bin.concat("hello", ", ", "world")
Arr.concat([1, 2], [3, 4], [5])
```

### Sys

| Operation      | Arity | Description                                                   | Returns |
| -------------- | ----- | ------------------------------------------------------------- | ------- |
| `Sys.print(a)` | 1     | Print `a : Bin` to stdout                                     | `{}`    |
| `Sys.read`     | 0     | Read a line from stdin (`\n` included); empty `Bin` means EOF | `Bin`   |

## Idioms

### Sum types

Curios has no built-in sum type. The idiom is a dependent tuple whose second field's type is determined by the first field, an atom drawn from a finite set.

**Definition**

```
let Result(A : Type, B : Type) -> Type = {
    tag : '[ok, err],
    match tag : Type
    | 'ok  => A
    | 'err => B
    end };
```

The first field `tag` is an atom type listing all variants. The second field is a `match` on `tag` that selects the payload type for each variant.

**Construction**

```
let good : Result(Nat, Bin) = ('ok,  42);
let bad  : Result(Nat, Bin) = ('err, "something went wrong");
```

A value is a two-element tuple of the variant atom and its payload.

**Elimination**

Use `match` on the first field to dispatch on the tag, then access the second field for the payload:

```
let unwrap_or(A : Type, r : Result(A, Bin), default : A) -> A =
    match r.0 : A
    | 'ok  => r.1
    | 'err => default
    end;
```

`end` closes the `match`; the trailing `;` closes the enclosing `let`.

### Recursive types

A recursive type uses a top-level `rec` binding that refers to itself in its own body. Combined with the sum type idiom, this gives linked lists, trees, and similar structures.

**Definition**

```
rec List : (A : Type) -> Type = A => {
    tag : '[nil, cons],
    match tag : Type
    | 'nil  => {}
    | 'cons => { A, List(A) }
    end };
```

The empty tuple type `{}` serves as the placeholder for the empty payload. The `cons` branch holds the head element and a recursive `List(A)` tail.

**Construction**

```
let empty : List(Nat) = ('nil,  ());
let one   : List(Nat) = ('cons, (1, ('nil, ())));
let three : List(Nat) = ('cons, (1, ('cons, (2, ('cons, (3, ('nil, ())))))));
```

**Elimination**

A recursive function over the list is itself written with `rec`:

```
rec length(A : Type, list : List(A)) -> Nat =
    match list.0 : Nat
    | 'nil  => 0
    | 'cons => Nat.add(1, length(A, list.1.1))
    end;
```

`end` closes the `match`; the trailing `;` closes the top-level `rec` binding.
