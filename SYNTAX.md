# Curios Syntax Reference

## Lexical basics

**Identifiers** are sequences of alphanumeric characters and underscores. Keywords are reserved and may not be used as identifiers.

**Keywords**: `let` `rec` `and` `pub` `match` `mod` `use` `end` `false` `true` `union` `with`

**Paths** are slash-separated identifiers: `Foo/bar`, `Std/List/length`. They refer to values in nested modules. Absolute paths start at the root with `/`, for example `/sys/Nat/add`.

The universe `Type` is built in. Primitive types and operations are exposed through the automatically prepended `/sys` module, so `/sys/Nat`, `/sys/Bin`, and `/sys/Io/print` parse as ordinary paths. A source file can import those names with `use /sys/{Nat, Bin, Io};`. The standard library is prepended the same way under `/std` (its sources live in `std/` alongside the compiler) and re-exports the same API plus higher-level helpers.

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
pub let add(a : /sys/Nat, b : /sys/Nat) -> /sys/Nat = /sys/Nat/add(a, b);
```

This is sugar for binding `add`, of type `(a : /sys/Nat, b : /sys/Nat) -> /sys/Nat`, to the lambda `(a, b) => /sys/Nat/add(a, b)`.

### Recursive bindings

```
pub rec f : A = body_f
pub and g : B = body_g;
```

Declares a group of mutually recursive bindings. Each binding in the group independently accepts `pub`. The entire group is terminated by a single semicolon after the last binding. Each binding accepts the same forms as `let` — either `name : type = value` or the function-definition shorthand `name(params) -> R = body`:

```
pub rec fact(n : /sys/Nat) -> /sys/Nat =
    match n : /sys/Nat
    | 0 => 1
    | pred + 1, ih => /sys/Nat/mul(/sys/Nat/succ(pred), ih)
    end;
```

Every `match` is closed by `end`; the trailing `;` then closes the recursive-binding group.

A binding's value may be any term, not only a lambda. In particular it can be a call that references other members of the group, so combinator-style definitions can be written point-free rather than eta-expanded:

```
pub rec decode : Parse(Value) = (input, pos) => -- … uses parse_arr, parse_obj …
pub and parse_arr : Parse(Value) =
    Parse/bind(Parse/take_byte('['), (_) => -- … uses decode … );
```

Members may refer to one another freely through such calls. The sole exception: two bindings whose values are _calls that each require the other's result_ form a cycle with no way to tie the knot, and the group is rejected.

### Union

```
pub union Result(A : Type, B : Type)
| ok(A)
| err(B)
end
```

Declares a sum type and a constructor module with the same name. The type name is bound as `Result` — a type-constructor function applied explicitly, `Result(Nat, Bin)` — and each constructor is bound under the constructor module. The union's parameters are _implicit_ at the constructors: `Result/ok(value)` infers them, and a call-site `@` supplies one positionally (`Result/ok(@Nat, @Bin, value)`). A constructor is exactly as visible as its union: a bare `union` is usable throughout the declaring module, and `pub union` additionally exports the type and its constructors. A payload-less case uses empty parentheses:

```
pub union Option(A : Type)
| none()
| some(A)
end
```

A union may declare zero cases, making the type uninhabited. Its eliminator is a `match` with zero arms — with no constructors every omission is vacuously justified, so the match checks at any motive:

```
union Void
end

let absurd(@A : Type, v : Void) -> A =
    match v : A
    end;
```

Mutually recursive unions are declared with `and` and are closed by a single `end`:

```
pub union Tree(A : Type)
| node(A, Forest(A))
pub and Forest(A : Type)
| nil()
| cons(Tree(A), Forest(A))
end
```

#### Indices

A union may declare an **index telescope** after a `:` in its head. A parameter is uniform — every constructor targets the same instantiation — but an index is _constrained per case_: each case states, after its payload, the parenthesized index expressions it inhabits.

```
pub union Vec(T : Type) : (n : Nat)
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
end
```

- The head's index names (`n`) are documentary — needed only when a later index's type depends on an earlier one; they are not in scope in the cases. `: (Nat)` is equally legal.
- A case target states _only_ the index expressions — never the union name or the parameters, which are forced uniform by construction. Targets are required on every case exactly when the head declares indices, with arity matching the index telescope's.
- Payload binders may be named (`m : Nat`) — required when a later payload type or the target mentions them — and a named binder may carry `@`, making it implicit at the value constructor (`m` above is recoverable from `xs`, so values are written `Vec/cons(x, xs)`).

The type constructor stays flat and explicit — `Vec : (T : Type, n : Nat) -> Type`, applied `Vec(Bin, 3)` — so use sites never distinguish parameters from indices.

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
use path/{item, item, ...};
use path/*;
```

A `use` declaration must end in either a brace group `/{...}` or a glob `/*`; bare `use path;` is not allowed.

An absolute path (from the root module) is written with a leading `/`. To import directly from the root, leave the path empty:

```
use /std/{Bin, Arr};
use /{Foo};
```

When using an absolute path, the first segment must refer to a `pub mod` at the root. A private root module cannot be accessed via an absolute path.

Each item inside the group may be:

- `Name` — import both the module _and_ the binding named `Name`, if either exists. Errors if neither exists publicly. This is the default.
- `mod Name` — import only the module named `Name`. Errors if there is no public module by that name.
- `let Name` — import only the binding named `Name`. Errors if there is no public binding by that name.

```
use /std/{Bin, Arr};               -- both kinds of each (where present)
use /std/{mod Bin, let Nat};       -- module `Bin`, binding `Nat`
use /std/{};                       -- empty group; no-op
```

A glob `use path/*;` imports every public child module and binding of `path`.

`pub use ...;` re-exports the imported names from the enclosing module.

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
/sys/Nat/add(/sys/Nat/mul(2, 3), 1)
```

### Lambda

```
(a) => body
(a, b) => body
(a : A, b : B) => body
```

Introduces a function. Parameters are parenthesised and comma-separated, and each may carry an optional type annotation. `(x) => body` is shorthand for `(x : _) => body`: the omitted domain is solved from the expected function type when the lambda is checked, or synthesized from the annotation when the lambda is in inference position (for instance, the body of a typeless `let`). A lambda whose domain cannot be determined — a bare `(x) => body` with no expected type — is rejected. All parameters are in scope in `body`.

### Holes

```
?
```

A hole is a placeholder elaborated to a fresh metavariable. The type checker solves it from surrounding constraints when possible:

```
let id(T : Type, x : T) -> T = x;
id(?, 5)        -- the hole is solved as /sys/Nat
```

An unsolved hole is rejected during type checking.

### Implicit parameters

```
let id(@T : Type, x : T) -> T = x;
id(5)           -- T is inferred
id(@Nat, 5)     -- T is given
```

A binder marked `@` is an implicit parameter: **an automatic `?`**. At every call site the elaborator fills it with a fresh hole unless an `@`-argument supplies it. Nothing else changes — conversion ignores the marks entirely, so a value of an implicit type flows anywhere its arity fits.

Implicit parameters may appear anywhere in the telescope, not just as a prefix; `@` marks work the same in standalone Π-types (`rec` signatures, annotations):

```
rec bind : (@A : Type, @B : Type) -> (Parse(A), A -> Parse(B)) -> Parse(B) = …;
```

Call sites follow a two-queue rule: plain arguments fill the explicit binders in telescope order, `@`-arguments fill the implicit binders in telescope order, matched independently — `f(@Nat, x)` and `f(x, @Nat)` are the same call. An implicit the elaborator cannot infer is rejected with the binder's name; supply it explicitly with `@`.

Union parameters need no marks: they are implicit at the value constructors and explicit at the type constructor by definition (see Union above).

### Local let

```
let name : Type = body;
tail
```

Binds `name` to `body` for the rest of `tail`. The semicolon is required. The type annotation is **optional for a local `let`** — `let name = body;` infers it from `body` (equivalently, write `: _`):

```
let n = 5;                 -- n : Nat, inferred
let f = (x : Nat) => x;    -- f : (Nat) -> Nat, inferred from the annotation
tail
```

The body must be inferable: a bare `let f = (x) => x;` (nothing constrains the domain), or a tuple with no annotation, is rejected. Top-level `let` and every `rec` binding (local or top-level) still require an explicit type — a `rec` group's mutually recursive types cannot be inferred from their bodies. The function-definition shorthand is also available locally:

```
let add(a : /sys/Nat, b : /sys/Nat) -> /sys/Nat = /sys/Nat/add(a, b);
tail
```

### Local rec

```
rec f : A = body_f
and g : B = body_g;
tail
```

Mutually recursive local bindings. Unlike top-level `rec`, the `and` clauses do not accept `pub`. Terminated by a semicolon; the remaining expression `tail` follows.

### With and bang

```
with bind body
action!
```

`with` introduces monadic sequencing sugar. The `bind` term is an atomic term denoting a binary bind operation of shape `(M A, A -> M B) -> M B` — typically a reference like `Parse/bind`, whose type parameters are implicit and inferred per use. Inside the `body`, postfix `!` marks an action whose result should be bound inline:

```
with Parse/bind
    let a = parse_a!;
    let b = parse_b!;
    combine(a, b)
```

The desugarer rewrites each `!` into an application of the active bind to the action and a generated continuation. The bind term is re-elaborated at each `!` site, so its implicit parameters (and any holes it contains) are fresh for each action and can solve to different types.

Bang is only valid inside a `with` body. A `!` in a call or tuple is collected left-to-right; a `!` in a `match` scrutinee runs before branching, while bangs inside branches stay branch-local. Lambda bodies and nested `with` blocks start their own sequencing regions.

### Match

`match` is the single elimination form. The branch shapes determine which kind of value is eliminated, and the head's type must agree.

```
match head : motive
| ...
end
```

The `motive` gives the result type — one grammar growing, with the binder parenthesized in every form (motives look exactly like the lambdas they morally are):

```
match v : P                          -- constant: the result does not depend on v
match v : (x) => P                   -- depends on the scrutinee
match v : (x : Vec(T, k)) => P       -- union scrutinees: depends on the indices too
```

In the annotated form the binder's type is the scrutinee's type with its index slots opened — index binders appear where they naturally live. Parameter slots take the actual parameter written verbatim (checked), `_`, or a name binding it; index slots take a fresh name or `_`. The motive may also be omitted entirely when the arms determine it. Every `match` is closed by `end`; branches are introduced by `|` and are bounded by the next `|` or by `end`.

**Booleans** — both branches required, either order:

```
match cond : /sys/Bln
| true  => true_body
| false => false_body
end
```

**Structural induction over `Nat`** — `| 0` is the base case; `| pred + 1, ih` binds the predecessor and the result already computed for it (`ih`, the induction hypothesis):

```
match n : /sys/Nat
| 0 => zero_case
| pred + 1, ih => succ_case
end
```

**Sparse dispatch on `Nat`** — specific values plus a mandatory `| _` default that must appear last:

```
match n : /sys/Nat
| 0 => body
| 3 => body
| _ => default
end
```

**Unions** — one branch per constructor; each branch lists binders for that constructor's payload:

```
match r : A
| ok(value) => value
| err(_) => default
end
```

Matching an _indexed_ union gets three further behaviours, by the shape of each scrutinee index:

- a **variable** index is refined to the case's target inside each arm, so hypotheses mentioning it reduce there (`match p : Vec(Bin, m)` through an `Eq(Nat, n, m)` learns `n := z`, `m := z` in the `refl` arm);
- a **constructor-form** index is inverted against each case's target: arm binders are pinned to forced values (`Nat/succ(n)` against `cons`'s `Nat/succ(j)` pins `j := n`), and a case whose target _definitely clashes_ (`nil`'s `0` against `Nat/succ(n)`) may simply be omitted — the checker verifies the omission, and there is no `impossible` keyword:

```
let first(@T : Type, @n : Nat, v : Vec(T, Nat/succ(n))) -> T =
    match v : T
    | cons(j, x, xs) => x
    end;
```

- an **opaque** index (an application like `f(x)`) carries information only through the motive.

The inverter is deliberately small — first-order, constructor forms, each binder constraining one position — and anything beyond that simply keeps the arm mandatory.

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

### Tuple type

```
{ A, B }
{ label1 : A, label2 : B }
```

Fields may optionally be named. Labels are used for documentation only; they do not affect the type's identity. The empty tuple type `{}` (whose only value is `()`) serves as a unit.

### Array type

```
/sys/Arr(T)
```

A homogeneous array of elements of type `T`. Write `/sys/Arr(/sys/Arr(/sys/Nat))` for nested arrays.

### Primitive types

| Type       | Description                  |
| ---------- | ---------------------------- |
| `/sys/Bln` | Boolean                      |
| `/sys/Nat` | Natural number               |
| `/sys/Int` | Signed integer (i32)         |
| `/sys/Flt` | Single-precision float (f32) |
| `/sys/Bin` | Byte sequence                |

`Nat` literals are arbitrary precision while parsing and type-level reduction are in progress. Erasure narrows runtime `Nat` values to `u32`; WebAssembly code generation then represents them as packed `i31ref`, so emitted literals must fit in the signed 31-bit range.

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

Supports escapes: `\n` `\t` `\r` `\\` `\"`. A string literal has type `/sys/Bin`.

### Byte sequences

Raw bytes written as consecutive hex pairs, each prefixed with `\`:

```
\ef\bb\bf
```

Has type `/sys/Bin`.

### Arrays

```
[1, 2, 3]
```

### Booleans

```
false
true
```

Boolean literals. Their type is `/sys/Bln`.

### Tuples

```
()
(a,)
(a, b)
(a, b, c)
```

Zero or more elements. A trailing comma is required for the one-element case to distinguish it from a parenthesized expression.

## Primitive operations

All primitive operations use call syntax: the operation name followed by parenthesised, comma-separated arguments. Arguments are arbitrary terms.

```
/sys/Nat/add(a, b)
/sys/Bin/slice(s, start, end)
```

These are normal path references. After `use /sys/{Nat, Bin};`, the same calls can be written `Nat/add(a, b)` and `Bin/slice(s, start, end)`.

### Nat

| Operation            | Arity | Description           | Returns    |
| -------------------- | ----- | --------------------- | ---------- |
| `/sys/Nat/succ(a)`   | 1     | Successor             | `/sys/Nat` |
| `/sys/Nat/add(a, b)` | 2     | Addition              | `/sys/Nat` |
| `/sys/Nat/sub(a, b)` | 2     | Subtraction           | `/sys/Nat` |
| `/sys/Nat/mul(a, b)` | 2     | Multiplication        | `/sys/Nat` |
| `/sys/Nat/div(a, b)` | 2     | Division              | `/sys/Nat` |
| `/sys/Nat/rem(a, b)` | 2     | Remainder             | `/sys/Nat` |
| `/sys/Nat/eql(a, b)` | 2     | Equality              | `/sys/Bln` |
| `/sys/Nat/neq(a, b)` | 2     | Inequality            | `/sys/Bln` |
| `/sys/Nat/lt(a, b)`  | 2     | Less than             | `/sys/Bln` |
| `/sys/Nat/gt(a, b)`  | 2     | Greater than          | `/sys/Bln` |
| `/sys/Nat/lte(a, b)` | 2     | Less than or equal    | `/sys/Bln` |
| `/sys/Nat/gte(a, b)` | 2     | Greater than or equal | `/sys/Bln` |
| `/sys/Nat/to_int(a)` | 1     | Convert to Int        | `/sys/Int` |
| `/sys/Nat/to_flt(a)` | 1     | Convert to Flt        | `/sys/Flt` |
| `/sys/Nat/to_str(a)` | 1     | Convert to Bin        | `/sys/Bin` |

Structural induction and sparse dispatch over a `Nat` are written with [`match`](#match) (the `| 0` / `| pred + 1, ih` and `| n` / `| _` branch shapes, respectively). Use `/sys/Nat/succ(a)` for the successor of a natural number.

### Int

| Operation            | Arity | Description           | Returns    |
| -------------------- | ----- | --------------------- | ---------- |
| `/sys/Int/add(a, b)` | 2     | Addition              | `/sys/Int` |
| `/sys/Int/sub(a, b)` | 2     | Subtraction           | `/sys/Int` |
| `/sys/Int/mul(a, b)` | 2     | Multiplication        | `/sys/Int` |
| `/sys/Int/div(a, b)` | 2     | Division              | `/sys/Int` |
| `/sys/Int/rem(a, b)` | 2     | Remainder             | `/sys/Int` |
| `/sys/Int/eql(a, b)` | 2     | Equality              | `/sys/Bln` |
| `/sys/Int/neq(a, b)` | 2     | Inequality            | `/sys/Bln` |
| `/sys/Int/lt(a, b)`  | 2     | Less than             | `/sys/Bln` |
| `/sys/Int/gt(a, b)`  | 2     | Greater than          | `/sys/Bln` |
| `/sys/Int/lte(a, b)` | 2     | Less than or equal    | `/sys/Bln` |
| `/sys/Int/gte(a, b)` | 2     | Greater than or equal | `/sys/Bln` |
| `/sys/Int/to_nat(a)` | 1     | Convert to Nat        | `/sys/Nat` |
| `/sys/Int/to_flt(a)` | 1     | Convert to Flt        | `/sys/Flt` |
| `/sys/Int/to_str(a)` | 1     | Convert to Bin        | `/sys/Bin` |

### Flt

| Operation             | Arity | Description           | Returns    |
| --------------------- | ----- | --------------------- | ---------- |
| `/sys/Flt/add(a, b)`  | 2     | Addition              | `/sys/Flt` |
| `/sys/Flt/sub(a, b)`  | 2     | Subtraction           | `/sys/Flt` |
| `/sys/Flt/mul(a, b)`  | 2     | Multiplication        | `/sys/Flt` |
| `/sys/Flt/div(a, b)`  | 2     | Division              | `/sys/Flt` |
| `/sys/Flt/eql(a, b)`  | 2     | Equality              | `/sys/Bln` |
| `/sys/Flt/neq(a, b)`  | 2     | Inequality            | `/sys/Bln` |
| `/sys/Flt/lt(a, b)`   | 2     | Less than             | `/sys/Bln` |
| `/sys/Flt/gt(a, b)`   | 2     | Greater than          | `/sys/Bln` |
| `/sys/Flt/lte(a, b)`  | 2     | Less than or equal    | `/sys/Bln` |
| `/sys/Flt/gte(a, b)`  | 2     | Greater than or equal | `/sys/Bln` |
| `/sys/Flt/min(a, b)`  | 2     | Minimum               | `/sys/Flt` |
| `/sys/Flt/max(a, b)`  | 2     | Maximum               | `/sys/Flt` |
| `/sys/Flt/neg(a)`     | 1     | Negation              | `/sys/Flt` |
| `/sys/Flt/abs(a)`     | 1     | Absolute value        | `/sys/Flt` |
| `/sys/Flt/sqrt(a)`    | 1     | Square root           | `/sys/Flt` |
| `/sys/Flt/floor(a)`   | 1     | Floor                 | `/sys/Flt` |
| `/sys/Flt/ceil(a)`    | 1     | Ceiling               | `/sys/Flt` |
| `/sys/Flt/trunc(a)`   | 1     | Truncate toward zero  | `/sys/Flt` |
| `/sys/Flt/nearest(a)` | 1     | Round to nearest      | `/sys/Flt` |
| `/sys/Flt/to_nat(a)`  | 1     | Convert to Nat        | `/sys/Nat` |
| `/sys/Flt/to_int(a)`  | 1     | Convert to Int        | `/sys/Int` |
| `/sys/Flt/to_le_bin(a)` | 1   | Convert to Bin bytes   | `/sys/Bin` |
| `/sys/Flt/to_str(a)`  | 1     | Convert to Bin        | `/sys/Bin` |

### Bin

| Operation                       | Arity | Description                       | Returns    |
| ------------------------------- | ----- | --------------------------------- | ---------- |
| `/sys/Bin/len(a)`               | 1     | Byte length                       | `/sys/Nat` |
| `/sys/Bin/eql(a, b)`            | 2     | Equality                          | `/sys/Bln` |
| `/sys/Bin/get(a, i)`            | 2     | Byte at index `i`                 | `/sys/Nat` |
| `/sys/Bin/slice(a, start, end)` | 3     | Subsequence from `start` to `end` | `/sys/Bin` |
| `/sys/Bin/append(a, byte)`      | 2     | Append a single byte              | `/sys/Bin` |
| `/sys/Bin/concat(a, b)`         | 2     | Concatenate two sequences         | `/sys/Bin` |

### Arr

| Operation                       | Arity | Description                    | Returns       |
| ------------------------------- | ----- | ------------------------------ | ------------- |
| `/sys/Arr/len(a)`               | 1     | Element count                  | `/sys/Nat`    |
| `/sys/Arr/get(a, i)`            | 2     | Element at index `i`           | `T`           |
| `/sys/Arr/slice(a, start, end)` | 3     | Subarray from `start` to `end` | `/sys/Arr(T)` |
| `/sys/Arr/append(a, elem)`      | 2     | Append a single element        | `/sys/Arr(T)` |
| `/sys/Arr/concat(a, b)`         | 2     | Concatenate two arrays         | `/sys/Arr(T)` |

`Bin/concat` and `Arr/concat` concatenate two operands; chain calls to join more:

```
/sys/Bin/concat(/sys/Bin/concat("hello", ", "), "world")
/sys/Arr/concat(/sys/Arr/concat([1, 2], [3, 4]), [5])
```

### Io

| Operation          | Arity | Description                                                        | Returns    |
| ------------------ | ----- | ------------------------------------------------------------------ | ---------- |
| `/sys/Io/print(a)` | 1     | Print `a : /sys/Bin` to stdout                                     | `{}`       |
| `/sys/Io/read()`   | 0     | Read a line from stdin (`\n` included); empty `/sys/Bin` means EOF | `/sys/Bin` |

## Idioms

### Sum types

Use `union` to declare a sum type. A union is a primitive _nominal_ (inductive) type: two unions are the same type only if they are the same declaration, and its values are built exclusively through its constructors. At runtime a constructor value is one flat record `(tag, payload...)`.

**Definition**

```
union Result(A : Type, B : Type)
| ok(A)
| err(B)
end
```

The union binds the type name `Result` and a constructor module `Result` containing `ok` and `err`.

**Construction**

```
let good : Result(Nat, Bin) = Result/ok(42);
let bad  : Result(Nat, Bin) = Result/err("something went wrong");
```

The union's type parameters are implicit at the constructors — inference fills them from the payload or the expected type. Supply one positionally with a call-site `@` when you want it pinned: `Result/ok(@Nat, @Bin, 42)`.

**Elimination**

Use `match` on the union value. Constructor branches bind the payload fields directly:

```
let unwrap_or(A : Type, r : Result(A, /sys/Bin), default : A) -> A =
    match r : A
    | ok(value) => value
    | err(_) => default
    end;
```

`end` closes the `match`; the trailing `;` closes the enclosing `let`.

### Recursive types

Recursive types are written as recursive unions. A union case may refer back to the union being declared.

**Definition**

```
union List(A : Type)
| nil()
| cons(A, List(A))
end
```

The `nil` branch has no payload. The `cons` branch holds the head element and a recursive `List(A)` tail.

**Construction**

```
let empty : List(Nat) = List/nil();
let one   : List(Nat) = List/cons(1, List/nil());
let three : List(Nat) =
    List/cons(1,
    List/cons(2,
    List/cons(3, List/nil())));
```

**Elimination**

A recursive function over the list is itself written with `rec`:

```
rec length(A : Type, list : List(A)) -> /sys/Nat =
    match list : /sys/Nat
    | nil() => 0
    | cons(_, tail) => /sys/Nat/add(1, length(A, tail))
    end;
```

`end` closes the `match`; the trailing `;` closes the top-level `rec` binding.
