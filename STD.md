# Curios Standard Library

The standard library lives under `/std`, prepended to every program automatically (like `/sys`). Its sources are ordinary Curios in `std/*.crs`, declared by the `std.crs` manifest and embedded into the compiler binary at build time.

Each scalar and collection module re-exports its `/sys` counterpart (`pub use /sys/Nat/*;` and the type itself), so `use /std/{Nat};` brings in both the primitives and the helpers below — programs rarely need `/sys` paths directly. The manifest also re-exports each module's principal type at the `/std` root.

- [Scalars](#scalars) — `Nat`, `Int`, `Flt`, `Bln`
- [Bytes and arrays](#bytes-and-arrays) — `Bin`, `Arr`, `Char`, `Str`
- [IO](#io) — `Io`, `File`
- [Data types](#data-types) — `Option`, `Result`, `Lst`, `Vec`
- [Proofs](#proofs) — `Eq`, `Void`
- [Parsing and formatting](#parsing-and-formatting) — `Parse`, `Json`, `Fmt`

## Scalars

### `/std/Nat`

| Binding         | Type                | Description             |
| --------------- | ------------------- | ----------------------- |
| `of_str(str)` | `Bin -> Nat`        | Parse a decimal numeral |
| `min(a, b)`     | `(Nat, Nat) -> Nat` | Minimum                 |

### `/std/Int`

| Binding         | Type         | Description                                       |
| --------------- | ------------ | ------------------------------------------------- |
| `of_str(str)` | `Bin -> Int` | Parse a decimal numeral with optional leading `-` |
| `abs(n)`        | `Int -> Nat` | Absolute value                                    |

### `/std/Flt`

| Binding         | Type         | Description                                                       |
| --------------- | ------------ | ----------------------------------------------------------------- |
| `of_str(str)` | `Bin -> Flt` | Parse a decimal `digits.digits` numeral with optional leading `-` |

### `/std/Bln`

| Binding     | Type         | Description           |
| ----------- | ------------ | --------------------- |
| `to_str(b)` | `Bln -> Str` | `"true"` or `"false"` |

## Bytes and arrays

### `/std/Bin`

| Binding              | Type                                      | Description                                |
| -------------------- | ----------------------------------------- | ------------------------------------------ |
| `fold(str, init, f)` | `(@A : Type, Bin, A, (Nat, A) -> A) -> A` | Left fold over the bytes                   |
| `concat_all(parts)`  | `Arr(Bin) -> Bin`                         | Concatenate every part                     |
| `join(sep, parts)`   | `(Bin, Arr(Bin)) -> Bin`                  | Concatenate with a separator between parts |

### `/std/Arr`

| Binding              | Type                                                  | Description                 |
| -------------------- | ----------------------------------------------------- | --------------------------- |
| `fold(arr, init, f)` | `(@T : Type, @A : Type, Arr(T), A, (T, A) -> A) -> A` | Left fold over the elements |
| `map(f, arr)`        | `(@A : Type, @B : Type, A -> B, Arr(A)) -> Arr(B)`    | Elementwise map             |

### `/std/Char`

Byte classifiers over ASCII code points (`Nat -> Bln`): `is_whitespace`, `is_digit`, `is_lower`, `is_upper`, `is_alpha`, `is_alphanumeric`.

### `/std/Str`

`Str` is the UTF-8 string type (re-exported from `/sys`; `"..."` literals have
this type). It has its own first-class operations — they share `Bin`'s *runtime
representation* (the conversions are no-ops) but never appear as `Bin` ops at the
surface. `of_bin` is the single, checked bridge from arbitrary bytes into text;
within `Str`-to-`Str` code it is never needed. (There is no unchecked
constructor in this API — the only trust is the inherent primitive trust behind
`to_str`/`concat`, exactly as for `Bin/len`.)

| Binding               | Type                    | Description                                                     |
| --------------------- | ----------------------- | -------------------------------------------------------------- |
| `to_bin(s)`           | `Str -> Bin`            | The underlying UTF-8 bytes (total)                             |
| `of_bin(b)`           | `Bin -> Option(Str)`    | Checked construction: `some` iff `b` is well-formed UTF-8       |
| `is_utf8(b)`          | `Bin -> Bln`            | Whether `b` is well-formed UTF-8                               |
| `empty`               | `Str`                   | The empty string                                               |
| `concat(a, b)`        | `(Str, Str) -> Str`     | Concatenate two strings                                        |
| `concat_all(parts)`   | `Arr(Str) -> Str`       | Concatenate every part                                         |
| `join(sep, parts)`    | `(Str, Arr(Str)) -> Str`| Concatenate with a separator between parts                     |
| `eql(a, b)`           | `(Str, Str) -> Bln`     | String equality (byte equality; UTF-8 is canonical)            |
| `len(s)`              | `Str -> Nat`            | Codepoint count (Unicode scalar values, *not* bytes/graphemes) |
| `trim_start(str)`     | `Bin -> Nat`            | Index of the first non-whitespace byte                         |
| `trim_stop(str)`      | `Bin -> Nat`            | Index one past the last non-whitespace byte                    |
| `trim(str)`           | `Bin -> Bin`            | The slice between the two                                      |

## IO

### `/std/Io`

Layers safe conveniences over the `/sys/Io` handle primitives (which it re-exports).

| Binding     | Type                                          | Description                                                                                                                               |
| ----------- | --------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| `print(s)`  | `Str -> {}`                                   | Write a string to stdout, best-effort: the write status is dropped, like printing to a closed pipe                                        |
| `Reader`    | `Type` (`= { Io, Bin }`)                      | A buffered reader: the handle plus bytes already read but not yet consumed                                                                |
| `reader(h)` | `Io -> Reader`                                | A fresh reader with an empty buffer                                                                                                       |
| `Buf(A)`    | `Type -> Type` (`= Reader -> { Reader, A }`)  | The buffered-reader state monad — the reader threads explicitly through actions, exactly like `Parse` threads its (input, position) state |
| `pure(a)`   | `(@A : Type, A) -> Buf(A)`                    | Lift a value                                                                                                                              |
| `bind`      | `(@A, @B) -> (Buf(A), A -> Buf(B)) -> Buf(B)` | Sequence two actions (use with `let ! = Io/bind;` blocks)                                                                                     |
| `run(m, h)` | `(@A : Type, Buf(A), Io) -> A`                | Run an action against a fresh reader on `h`                                                                                               |
| `read_line` | `Buf(Option(Bin))`                            | The next line, including its trailing `\n`; `none` means end of input                                                                     |

`read_line` delivers a final unterminated line before EOF as `some`; any non-ok refill status (EOF or an IO error) ends the stream — an error-propagating reader is future work.

### `/std/File`

`File` is an **abstract handle** — its own opaque type, distinct from a bare `Io` handle (stdin/stdout, a socket) and reachable only through the operations below. It is a zero-cost newtype over `Io`, so the abstraction is free at runtime. There is no public `open` or `close`: `using`/`read_all` bracket them automatically, so a handle can never leak from the safe layer or be closed twice (`/sys/Io/open` remains the explicit escape hatch).

```
union Mode  | read() | write() | append() end   -- File/Mode/read() etc.
union Error | not_found() | permission_denied() | exists() | other(Nat) end

File/using(path, mode, body)   -- (@A : Type, Bin, Mode, File -> A) -> Result(A, Error)
File/read_all(path)            -- Bin -> Result(Bin, Error)
File/read(f, n)                -- (File, Nat) -> { status : Nat, bytes : Bin }
File/write(f, b)               -- (File, Bin) -> Nat
```

`using` is the one doorway to a file handle: open, run `body` on the `File` it yields, close. Inside the body, `read`/`write` are the operations on that handle. The handle never outlives the bracket — an effect delayed past it, such as a `body` result that is itself a closure performing IO, would touch a closed handle. `Error` mirrors the `/sys/Io` status contract (0 ok, 1 eof, 2 not found, 3 permission denied, 4 exists, 5+ other). Programs run with the invoking user's filesystem access — there is no sandbox.

## Data types

### `/std/Option`

```
pub union Option(A : Type)
| some(A)
| none()
end
```

### `/std/Result`

```
pub union Result(A : Type, E : Type)
| success(A)
| failure(E)
end
```

### `/std/Lst`

The linked list, `nil()` / `cons(A, Lst(A))`, with:

| Binding     | Type                            | Description                |
| ----------- | ------------------------------- | -------------------------- |
| `len(l)`    | `(@A : Type, Lst(A)) -> Nat`    | Length                     |
| `rev(l)`    | `(@A : Type, Lst(A)) -> Lst(A)` | Reversal                   |
| `to_arr(l)` | `(@A : Type, Lst(A)) -> Arr(A)` | Conversion to a flat array |

### `/std/Vec`

The length-indexed vector — the canonical indexed union (see SYNTAX.md's Indices section):

```
pub union Vec(T : Type) : (n : Nat)
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
end
```

`cons`'s length binder is implicit at the constructor (`@m` is recoverable from `xs`), so values are written `Vec/cons(x, xs)`.

| Binding        | Type                                                          | Description                                                             |
| -------------- | ------------------------------------------------------------- | ----------------------------------------------------------------------- |
| `len(v)`       | `(@T, @n : Nat, Vec(T, n)) -> Nat`                            | The length is the index: `n` rides in implicitly and is simply returned |
| `append(v, w)` | `(@T, @n, @m, Vec(T, n), Vec(T, m)) -> Vec(T, Nat/add(n, m))` | Concatenation, length-summing by type                                   |
| `map(f, v)`    | `(@A, @B, @n, A -> B, Vec(A, n)) -> Vec(B, n)`                | Length-preserving map                                                   |
| `first(v)`     | `(@T, @n, Vec(T, Nat/succ(n))) -> T`                          | Head — only accepts non-empty vectors                                   |
| `rest(v)`      | `(@T, @n, Vec(T, Nat/succ(n))) -> Vec(T, n)`                  | Tail — only accepts non-empty vectors                                   |

In `first`/`rest` the `nil` arm is provably impossible at length `Nat/succ(n)` and simply omitted: the checker verifies the omission, and `cons`'s length binder is pinned to `n`.

## Proofs

### `/std/Eq`

Propositional equality: `Eq(x, y)` is inhabited exactly when `x` and `y` are equal, and its only constructor pins both indices to the same value. The parameter is `@`-marked — implicit at the type, recoverable from the indices, pinned with `Eq(@Nat, x, y)` when wanted. Matching on a proof refines the indices inside the arm (`x := z`, `y := z`), which is what makes the eliminators typecheck. See PROOFS_101.md for the full story.

```
pub union Eq(@A : Type) : (x : A, y : A)
| refl(@z : A) : (z, z)
end
```

| Binding          | Type                                       | Description                                                |
| ---------------- | ------------------------------------------ | ---------------------------------------------------------- |
| `sym(p)`         | `Eq(x, y) -> Eq(y, x)`                     | Symmetry                                                   |
| `trans(p, q)`    | `(Eq(x, y), Eq(y, z)) -> Eq(x, z)`         | Transitivity                                               |
| `cong(f, p)`     | `(f : A -> B, Eq(x, y)) -> Eq(f(x), f(y))` | Congruence                                                 |
| `subst(P, p, v)` | `(P : A -> Type, Eq(x, y), P(x)) -> P(y)`  | Transport: rewrite `x` to `y` under an arbitrary predicate |

(All `@A`/`@x`/`@y`/`@z` parameters elided above; they infer.)

### `/std/Void`

The uninhabited type: a union with zero cases. No value of `Void` can be constructed, so holding one is itself a contradiction — and eliminating it is a match with zero arms, which checks at any motive.

| Binding                 | Type                           | Description                              |
| ----------------------- | ------------------------------ | ---------------------------------------- |
| `absurd(contradiction)` | `(@A : Type, Void) -> A`       | From the absurd, anything follows        |
| `Not(P)`                | `Type -> Type` (`= P -> Void`) | Negation: a proof of `P` would be absurd |

## Parsing and formatting

### `/std/Parse`

Byte-level parser combinators over a `(input : Bin, position : Nat)` state:

```
Parse(A) = (Bin, Nat) -> Result({ Nat, A }, Str)
```

— success carries the new position and the value; failure carries a message
(`Str`). The input being parsed stays a `Bin` (raw bytes).

| Binding                             | Description                                       |
| ----------------------------------- | ------------------------------------------------- |
| `run(p, input)`                     | Run from position 0; `Result(A, Str)`             |
| `pure(a)` / `fail(msg)`             | Constant success / failure                        |
| `map(f, p)`                         | Map the result                                    |
| `bind`                              | Sequencing, shaped for `let ! = Parse/bind;` blocks |
| `or(p, q)`                          | Try `p`, fall back to `q`                         |
| `and(p, q)`                         | Both in sequence; pairs the results as `{ A, B }` |
| `and_drop(p, q)` / `and_keep(p, q)` | Both in sequence; keep the first / second result  |
| `any_byte` / `peek_byte`            | Next byte, consuming / not consuming              |
| `take_byte(expected)`               | Exactly the given byte                            |
| `take_while(pred)`                  | Longest run of bytes satisfying `pred`            |
| `many0(p)`                          | Zero or more `p`, collected in an `Arr`           |
| `sep_by0(elem, sep)`                | Zero or more `elem` separated by `sep`            |

### `/std/Json`

A JSON tree and a byte-level codec:

```
pub union Json
| null() | bln(Bln) | num(Flt) | str(Str)
| arr(Arr(Json)) | obj(Arr({ Str, Json }))
end
```

| Binding     | Type          | Description                                      |
| ----------- | ------------- | ------------------------------------------------ |
| `encode(v)` | `Json -> Str` | Serialize                                        |
| `decode`    | `Parse(Json)` | Parser for a JSON value, built from `/std/Parse` |

(`decode` parses a `Bin`; `encode` returns a `Str` — feed it back in via
`Str/to_bin` to round-trip.)

`examples/crs_json_codec.rs` round-trips a tree through both.

### `/std/Fmt`

Typed format strings: the argument list of `printf` is computed from the format string at compile time by an ordinary dependent function — no macro.

| Binding                  | Description                                                                           |
| ------------------------ | ------------------------------------------------------------------------------------- |
| `Fmt`                    | The parsed format AST: `nil()`, `lit(Str, Fmt)`, `str(Fmt)` (`%s`), `nat(Fmt)` (`%d`) |
| `parse(input)`           | `Bin -> Fmt` — parse a format string                                                  |
| `format_type_with(T, f)` | The dependent-type computation: the curried function type a format demands            |
| `format(s)`              | Build a `Str` by applying the demanded arguments (`s : Str`)                          |
| `printf(s)`              | Like `format`, but also prints the result                                             |

`/std/Fmt/printf("%s is %d")` has type `Str -> Nat -> Str`; `%s` takes a `Str`, and passing a `Bin` where `%d` expects a `Nat` is a compile-time `TypeMismatch`. See `examples/crs_printf.rs`.
