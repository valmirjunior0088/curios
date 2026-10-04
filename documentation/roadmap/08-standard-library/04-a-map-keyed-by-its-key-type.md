# A map keyed by its key type

Working specification for `/std/Map` as `Map(K, V)`: one map whose key type is part of its type, whose leaves store the key rather than its encoding, and whose type names the dictionary it was built under. It replaces today's `Map(V)` and absorbs `Set(K)` rather than sitting beside either.

Downstream of the elaborator leaving alone a witness slot that unification has already solved: a `struct` takes the `use` type parameter this is declared with, and a type naming a dictionary other than the registered one is not yet read back under it. Nothing here can land before that, for the reason under *Why the type names the dictionary*.

## Today's contract, and why it is honest

`Map(V)` is a crit-bit trie over `Bytes`: `leaf(key: Bytes, value: V)` and `fork(crit: Nat, zero, one)`, with every keyed operation taking `use Key(K)` and reducing its key through `Hash/hash`. The type mentions no `K` because the structure indexes encodings, which is what its own documentation says — "a map from encoded keys to values". A fork tests one critical bit, so the full byte string at the leaf is what turns a walk into a hit; storing it is not optional.

Two consequences follow that are not written down.

- **Keys of two types with one encoding are one key.** `Key`'s law is injective within a type and says nothing across types, and `Hash(Str)` agrees with `Hash(Bytes)` on `"a"` and `x[97]`, so a value filed under one reads back under the other. Recorded in [findings](00-findings.md).
- **Enumeration hands back encodings.** `fold`, `entries`, `keys` and `filter` are written over `Bytes`, and `Key` offers `hash` and no inverse, so a caller who put in `Str` keys cannot get them out. `Set(K)` exists to work around exactly this, by storing the key as the value so its elements come back typed.

## What is wanted

One map. `Map(K, V)`, keys in, keys out, and a `Bytes` key for whoever wants one by saying `Map(Bytes, V)`.

## The three designs, and why the third

**A phantom key type.** `Map(K, V)` over an unchanged representation, `K` named in no field. The cross-type read becomes a type error. It does **not** give typed keys back: `keys` still answers `Bytes`, because the trie holds encodings and `Key` has no inverse. Safety without recovery, and a parameter no field uses.

**The key beside its encoding.** `leaf(key: Bytes, original: K, value: V)`. Both consequences go, at the price of holding the key twice under an invariant — `Hash/hash(original) = key` — that nothing checks. A second copy of one fact whose agreement nothing enforces is the shape `.claude/rules/documentation.md` rejects.

**The key, its encoding derived.** `leaf(key: K, value: V)`, encoded when a comparison needs it. Both consequences go, the key is held once, and no invariant is invented. It is also **free in order**: `Map/get` already reduces its query once through `Hash/hash` and then compares the whole byte string at the leaf with `Bytes/eql`, which is linear in the key, so re-encoding the leaf's key is the same order as the comparison it feeds. One encode per leaf compared, and the walk's cost is unchanged.

The third is the one to take. It is the only design that is simultaneously one type, typed keys, one copy of the key and no new invariant — and it **removes** a type rather than adding one: `Set(K)` becomes `Map(K, {})`, so storing the key as its own value stops being a workaround and becomes the degenerate case.

## Why the type names the dictionary

Today's stored encoding is a self-check. A fork holds a bit index into the encoding that built it, and the leaf holds that encoding. An author may supply a dictionary explicitly — `use value` overrides resolution — and a walk under a different one may go astray, but `Bytes/eql` then fails and the answer is `none()`. That is why the module can record the consequence as two members and not a wrong one.

Deriving the encoding removes the check, and two failures become reachable that are not today:

- a walk goes the wrong way, re-encodes the leaf's key under the same wrong dictionary, and **matches** — a wrong answer rather than a missing one;
- an insertion computes a critical bit under a dictionary other than the one the forks above it were built with, breaking the invariant that each fork's bit discriminates its subtrees, for every later lookup.

Global coherence does not prevent it: one witness per key governs *resolution*, and `use value` bypasses resolution. So the dictionary is a parameter of the type, and conversion holds every operation to it:

```crs
pub struct Map(K: Type, use Key(K), V: Type): Type {
    size: Nat,
    root: Option(Node(K, V)),
}

pub let get(@K: Type, use Key(K), @V: Type, m: Map(K, V), key: K) -> Option(V) = …;
```

`Map(Str, Nat)` resolves `Key(Str)` where the type is written, and inside `get` the same spelling resolves to `get`'s own premise. At a call the key type is an implicit, so the premise's slot is still open when the map is checked, and unifying the map's type solves the key type and the dictionary together: the dictionary an operation walks under is the one the map's type names, by typing. A map built under `use other` is a `Map(Nat, use other, V)`, a different type from `Map(Nat, V)`, and a walk under the wrong dictionary is a type error rather than a caveat. The value itself holds no dictionary: a type erases, and each operation receives its premise as an argument, as it does today.

[Lean's `Std.HashMap`](https://github.com/leanprover/lean4/blob/master/src/Std/Data/HashMap/Basic.lean) is this design: `structure HashMap (α) (β) [BEq α] [Hashable α]`, with its operations taking the instances from the map's type and its constructors resolving them. [`/std/Parse`](../../../curios-text/std/Parse.crs) is it already, as a family: `Parse(I: Type, use Input(I), A: Type)`.

## What it decides

- **Every function over a map carries `use Key(K)`**, since it takes the premise to spell `Map(K, V)` at all: the keyed operations keep theirs, and `len`, `map`, `values`, `fold` and the rest gain one. The parameter sits beside `K` and never last, so `(V: Type) => Map(K, V)` stays the family a higher-kinded concept reads.
- **Enumeration is typed.** `fold`'s step takes `(K, V, A)`, `entries` answers `List({K, V})`, `keys` answers `List(K)`, and `filter`'s predicate takes `(K, V)`. None of them encodes anything, since the key is what is stored.
- **`union` takes two maps of one type**, so of one dictionary, and merges their tries as it does today. A map moves to another dictionary by `rekey`, which re-inserts: `rekey(@K: Type, @from: Key(K), use Key(K), @V: Type, m: Map(K, use from, V)) -> Map(K, V)`, the dictionary it leaves named through an `@` parameter the map's type determines.
- **A witness over a map takes the premise**: `satisfy (@K: Type, use Key(K), @V: Type, use Show(K), use Show(V)) => Show(Map(K, V))`. Its head is unified with the goal before its premises are tried, so the dictionary is the goal's.
- **`Set(K)` is `Map(K, {})`**, declared `Set(K: Type, use Key(K))`.

## Still to refine

- **`Toml`'s two maps** become `Map(Bytes, Toml)` and `Map(Bytes, Origin)`, which is what they already mean, since `seg_key` builds byte-string keys itself. Its call sites are the blast radius to measure.
- **Whether a heterogeneous map is lost that anyone wanted.** Keying by encoding across types is what makes the cross-type read possible, and no consumer uses it; the spec assumes nobody wants it back, and says so here rather than discovering it later.
- **What the unused premise costs.** `len` and its like receive a dictionary they never read. It is one argument per call, and nothing has measured it.
- **The acceptance criteria**: the findings probe answers `none()` and then refuses to elaborate; a map built under a second dictionary is read back under it with no dictionary written at the reads, and is refused where a map of the registered one is expected; `/std` builds with `Set` as a specialization; `Toml` round-trips; and the corpus's `/data/map` program, which exercises the lookups, the canonical shape and the rewriting functions, passes unchanged but for its key types.

## Rejected

- **The dictionary carried in the value**, as a `use` field of the structure. A field with no label cannot be read, a structure's value is not in the witness scope, and reading it through the fields of a local is a second resolution rule; and two maps of one key type could then carry two dictionaries behind one type, which leaves `union` to re-insert one side because no equality over dictionaries can be written.
- **The dictionary taken at each operation and named nowhere**, today's contract: sound only while the stored encoding is there to refuse a walk gone astray, which storing the key removes.
- **Refusing an explicit dictionary for a key**, so that coherence alone decided: `use value` is how a program chooses a second witness, and a type that names its dictionary makes the choice safe rather than forbidden.
