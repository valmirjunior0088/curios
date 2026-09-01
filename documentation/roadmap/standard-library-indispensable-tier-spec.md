# The standard library's indispensable tier

## Status

Researched, not designed. This file records a survey of what nine standard libraries agree on, the distance between that and `/std` — read from source and probed through `wonder` on 2026-09-01 — the items that close it in the order they land, and the decisions taken while planning them. The modules keep their own contracts in `curios-prelude-archive/std/` and nothing here restates one. Nothing is started.

## Why it exists

`/std` grew by consumer: every module exists because a codec, a benchmark or a proof needed it. That made it deep where a consumer pushed — a certified `BigNat` algebra, a crit-bit `Map` with an injectivity law, a poll-driven `Async` — and thin where none did. Nothing in it sorts. A string cannot be compared. `List` has ten functions. `Vec` cannot be indexed. There is no `Set`. A proof cannot recurse along a measure.

A survey of Lean 4 (`Init`, `Std`, Batteries), Agda (`agda-stdlib` 2.x), Idris 2 (`base`), Rocq (`Stdlib`), Haskell (`base`), OCaml (`Stdlib` 5.x), Rust (`core`, `alloc`, `std`), Gleam and Zig was taken to separate what every one of them ships from what only some do. The tier every library agrees on is the one specified here: a capability is in it when each of the nine has it and a program of ordinary shape meets its absence early. Two further tiers — what most libraries ship, and what a consumer must first ask for — came out of the same survey and are deliberately not in this file.

## What is certain

Read from source and probed through `wonder`, in the order the items below land.

- **Ordering stops at the numerics.** `Cmp` and `Ord` have witnesses on `Nat`, `Int`, `Flt`, `Char`, `Byte`, `BigNat`, `BigInt`, `{}` and tuples up to eight, and none on `Str`, `Bytes`, `Bool`, `List`, `Option` or `Result`. `"a" < "b"` is refused with `no witness of Cmp(Str) found`. `Key(Str)` lets a string be a map key and not be compared. `Vec` and `Map` have no `Eql` or `Show`.
- **`/std/List` holds `get`, `len`, `slice` and `map` from `/sys` — appending is the spread literal — and `try_get`, `drop`, `flatten`, `fold`, `find` and `balanced`.** Nothing sorts, filters, reverses, zips, takes, partitions, searches by index or traverses under a monad. `find` is its own index loop under the proof-carrying `Nat/Lt` invariant `fold` also carries, and every further predicate function written the same way would repeat both.
- **Every fold walks left to right with `(elem, acc)`.** `List`, `Bits`, `Bytes`, `Str` and `Map` agree, eight `/std` callers rely on the order, and `List.crs` states none of it. The right fold is the language's: the `; ih` binding on a `[head, ..tail]` arm is the structural recursion, on every carrier, and the optimizer works on it.
- **`/std/Str` holds `concat`, `slice`, `flatten`, `eql`, `eql_ascii_ci`, `repeat`, `fold`, `len`, `is_empty`, `find_index`, `find`, `try_get`, `get`, `join` and `trim`.** It composes and walks; it does not decompose. Its bytes are UTF-8 under a validity proof, and UTF-8 bytewise order is scalar-value order, so a code-point comparison needs no decoding.
- **`/std/Option` and `/std/Result` have `bind`, `map`, `unwrap_or` and their predicates; `/std/Vec` has `len`, `append`, `map`, `first` and `rest`.** `Vec` cannot be indexed, folded, zipped or built from a list. Indexing a `Str` or `Bytes` takes `(i: Nat, @ok: Nat/Lt(i, len))` with the bound discharged by reduction, which is the shape a `Vec` index takes.
- **Symbolic `Nat` arithmetic is largely definitional.** `n + 0 = n`, `0 + n = n`, `(a + b) + c = a + (b + c)`, `a + b = b + a`, `a * b = b * a`, `a * (b + c) = a * b + a * c`, `n * 0 = 0`, `n - n = 0`, `(n + m) - m = n`, `n * 2 = n + n`, `n / 1 = n`, `n < n = false`, `n < n + 1 = true` and `Nat/Le(n, n + m)` all close by `Eq/refl()` or `True/qed()`. `n < m = not(m <= n)` and `Nat/Le(x / 256, x)` do not. The lemma corpus Rocq's `Arith` and Agda's `Data.Nat.Properties` carry is therefore mostly unnecessary; what is missing is what reduction cannot do — an induction principle stronger than the successor's — and `pow`, `gcd`, `lcm`, `log2`, `sqrt` and `Int/sign`, which every peer has and `/std` does not.
- **There is no `Set`, and `Map`'s keys are `Bytes` through `Key`.** `Map.crs` records that `Key(Nat)` was attempted and abandoned because its base-256 encoding recursed on `x / 256` and "intrinsic `Nat` eliminates only by unary successor, so no induction principle reaches that recursion". `Map` has `get`, `has`, `insert`, `remove`, `fold`, `entries`, `keys`, `values` and `of`, and no update, map, filter or union.
- **Well-founded recursion can be declared, and not used where it is needed.** An accessibility predicate declared as `induct Accessible(@A: Type, R: (A, A) -> Prop): (A) -> Prop | intro(@x: A, below: (y: A, r: R(y, x)) -> Accessible(R, y)): (x) end` is accepted, passes positivity, and a `Type`-valued fixpoint over it elaborates. The same fixpoint at a `Prop`-valued result is refused: `a proof in '/strong' is a proof position but reaches '/strong', which is not known to terminate`. Totality is decided per recursive group by size-change termination, and the recursive call goes through `below(m, lt)`, a function-typed constructor payload, which the analysis does not count as a decrease. Programs never needed the predicate — general recursion is unrestricted where erasure keeps the result — and proofs, which do, cannot use it.

## The items, in landing order

Cheapest and most consumed first. Each item depends on the ones before it and on nothing after.

### 1. Ordering witnesses

`Cmp` and `Ord` on `Str`, `Bytes`, `Bool`, `List`, `Option` and `Result`; `Eql` and `Show` on `Vec` and `Map`. `Cmp(Str)` is `Bytes` comparison, justified by the one line above: UTF-8 bytewise order is scalar-value order, so `sort` on strings means code-point order and decodes nothing. `List` compares lexicographically, `Option` with `none` below `some`, `Result` with `failure` below `success`, `Bool` with `false` below `true`, each stated once in its module. Everything below sorts on this.

### 2. `List`

The functions every peer has: `reverse`, `filter`, `filter_map`, `any`, `all`, `zip`, `unzip`, `take`, `take_while`, `drop_while`, `partition`, `contains`, `index_of`, `find_index`, `last`, `range`, `replicate`, `concat_map`, `intersperse`. Three that structure the rest:

- **`fold_until`**, a left fold whose step returns `Step/continue(acc)` or `Step/stop(acc)` and ends at the first stop. `any`, `all`, `contains`, `index_of` and `find_index` are each one line over it, and the `Nat/Lt` invariant `find` carries today is proved once. `Step` is its own inductive rather than `Result`, because a stop is not a failure.
- **`traverse` and `each`** over any `Monad`: `traverse(@M, use Monad(M), l, f: (T) -> M(B)) -> M(List(B))`, and `each` discarding the results. Plain functions, one per container, not a concept.
- **`sort` over `Ord` and `sort_by` over a comparator**, a stable merge sort, uncertified.

`fold` keeps its name and its `(elem, acc)` order, and `List.crs` gains the line stating its direction. No `fold_right` is added: the `; ih` arm is the right fold, and a library twin would spend a stack frame per element on what the syntax already optimizes.

### 3. `Str` decomposition

`split`, `split_once`, `starts_with`, `ends_with`, `contains`, `replace`, `lines`, `to_list`, `of_char`, `trim_start`, `trim_end`, `pad_start`, `pad_end`. Every result is a `Str` carried under the validity proof `slice` already threads; nothing re-validates.

### 4. `Option`, `Result` and `Vec`

`Option`: `or`, `or_else`, `filter`, `flatten`, `to_result`, `unwrap_or_else`, and `get(o, @ok: IsSome(o))` with `IsSome` reducing to `True` or `False` by a match — the shape `Nat/Lt` has. `Result`: `to_option`, `unwrap_or_else`. `Vec`: `get(v, i, @ok: Nat/Lt(i, n))`, `to_list`, `of_list`, `replicate`, `zip`, `fold`. `of_list` returns the dependent pair `{n: Nat, Vec(T, n)}`, since the length is not known statically; it is not an `Option`.

### 5. Numerics and strong induction

`Nat`: `pow`, `gcd`, `lcm`, `log2`, `sqrt`, `is_even`. `Int`: `of_nat`, `min`, `max`, `sign`. And `Nat/Lt/strong`, course-of-values induction — `strong(P, step: (n, ih: (m, Nat/Lt(m, n)) -> P(m)) -> P(n), n) -> P(n)` — proved by ordinary induction on a bound `k` under `Nat/Le(n, k)`, which needs no new judgment. It is named under `Nat/Lt` because the hypothesis it hands the step is `Lt(m, n)`; the bound it descends on is the mechanism, not the principle. It is what item six's `Key(Nat)` proof uses, and what any proof along `x / 256` uses until item seven.

### 6. `Set` and `Map`

`Set` is a struct over `Map({})` and mirrors `Map`'s surface with the value elided: `empty`, `len`, `has`, `insert`, `remove`, `fold`, `to_list`, `of`. `Map` gains `update`, `map`, `filter`, `union` and `get_or`, and `Set` gains `union` with it; set algebra exists exactly where `Map` has the counterpart, so there is no `intersect` or `difference` until a `Map` consumer wants one. `Key(Nat)` lands here, its injectivity proved by item five's `Nat/Lt/strong`, so numeric sets and maps work.

### 7. `/std/WellFounded`

The module's namesake is `WellFounded(R)`: every element is accessible under `R`, the constructive definition Rocq, Agda, Idris and Lean all use, and the one that is the induction principle. Beneath it, `WellFounded/Accessible(R, x)` as the inductive predicate above, `WellFounded/recurse` as the fixpoint, and `WellFounded/lt` proving `<` on `Nat` well-founded. The names are chosen over `Acc` because `/std` names its propositions as adjectives about the thing they qualify — `Finite`, `Valid`, `Canonical` — and `WellFounded` is the claim a user makes about their own relation.

The item has a second half in the compiler, landed after this specification: totality counts a call as decreasing when its argument is a constructor payload bound by the pattern on the scrutinee, applied to anything. That is the rule under which `recurse` is usable in a proof position, and it touches `curios-analysis`, both drivers and the kernel's re-check. Until it lands, the library half elaborates and serves programs, which never needed it; item five's `Nat/Lt/strong` serves the proofs.

## Decisions taken

- Sort lands uncertified and stable. A `Sorted` proposition and a permutation statement are a later item with a consumer.
- `Set` is `Map({})` and mirrors `Map`. An `Ord`-keyed tree waits for a consumer with non-`Bytes` keys.
- `traverse` is a function per container. No `Functor`, `Applicative`, `Traversable` or `Monoid` concept: `map` is per type, `/sys/List/map` is an intrinsic, and a concept costs resolution on every call for no consumer.
- `fold` keeps its name and order; `fold_until` uses `Step`; no `fold_right`.
- `Vec/get` and `Option/get` take a decided-proposition bound, as `Str/get` does; `Vec/of_list` returns a dependent pair.
- `Nat/Lt/strong` lands before `WellFounded`, and the totality rule is this campaign's, not a blocker deferred elsewhere.

## Deliberately not specified

Certified sort. `Dec` keeping the refutation beside `Option(P)`. Derivation slots for `Ord`, `Show` or `Draw`. `Functor` and its family. Filesystem and subprocess rows in the host ABI, which are four implementations each. The ASCII predicates on `Char` under Unicode names. The JSON and TOML number carriers, which are decided together beside the TOML specification. The two tiers below this one. Spellings beyond the ones the items fix. Performance: a `sort` written over `fold` is correct first.
