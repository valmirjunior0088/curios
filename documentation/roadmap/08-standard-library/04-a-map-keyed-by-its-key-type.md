# A map keyed by its key type

Working specification for `/std/Map` as `Map(K, V)`: one map whose key type is part of its type, whose leaves store the key rather than its encoding, and which carries the dictionary it was built under. It replaces today's `Map(V)` and absorbs `Set(K)` rather than sitting beside either.

Downstream of [plicity on a telescope's members](../03-surface/02-plicity-on-a-telescopes-members.md)'s stage 1, which admits the carried dictionary. Nothing here can land before it, for the reason under *Why the dictionary must be carried*.

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

## Why the dictionary must be carried

Today's stored encoding is a self-check. A fork holds a bit index into the encoding that built it, and the leaf holds that encoding. An author may supply a dictionary explicitly — `use value` overrides resolution — and a walk under a different one may go astray, but `Bytes/eql` then fails and the answer is `none()`. That is why the module can record the consequence as two members and not a wrong one.

Deriving the encoding removes the check, and two failures become reachable that are not today:

- a walk goes the wrong way, re-encodes the leaf's key under the same wrong dictionary, and **matches** — a wrong answer rather than a missing one;
- an insertion computes a critical bit under a dictionary other than the one the forks above it were built with, breaking the invariant that each fork's bit discriminates its subtrees, for every later lookup.

Global coherence does not prevent it: one witness per key governs *resolution*, and `use value` bypasses resolution. A premise does not either — `satisfy (@V: Type, use Eql(V)) => Eql(Map(V))` is resolved where the witness is used, not where the value was built. So the dictionary belongs in the value:

```crs
pub struct Map(K: Type, V: Type): Type {
    use Hash(K),
    size: Nat,
    root: Option(Node(K, V)),
}
```

and `Map`'s documented two-dictionary hazard stops being a caveat, because there is only the dictionary the map carries.

## What it decides

- **Every keyed operation drops its `use Key(K)` parameter** and reads the carried dictionary. `get`, `has`, `insert`, `remove`, `of`, `get_or` and `update` lose a parameter; `len`, `map` and `values` are unchanged.
- **Enumeration is typed.** `fold`'s step takes `(K, V, A)`, `entries` answers `List({K, V})`, `keys` answers `List(K)`, and `filter`'s predicate takes `(K, V)`. None of them encodes anything, since the key is what is stored.
- **`union` re-inserts.** Two maps of one `K` may carry different dictionaries, and witness equality is not available — a dictionary is a record of functions and there is no `Eql` over them. So `union(a, b)` re-inserts `b`'s entries under `a`'s dictionary: linear in `b` and total, where requiring agreement would need an equality that cannot be written.
- **`Set(K)` is `Map(K, {})`**, and its four key-taking operations lose their `use Key(K)` with the rest.

## Still to refine

- **Where the dictionary is read from.** A carried `use` field is the premise of this spec; whether an operation may still *override* it — and what that would mean for a structure whose forks were built under another — needs stating before anything is implemented. The straightforward rule is that it may not, which is the whole point of carrying it.
- **`Toml`'s two maps** become `Map(Bytes, Toml)` and `Map(Bytes, Origin)`, which is what they already mean, since `seg_key` builds byte-string keys itself. Its call sites are the blast radius to measure.
- **Whether a heterogeneous map is lost that anyone wanted.** Keying by encoding across types is what makes the cross-type read possible, and no consumer uses it; the spec assumes nobody wants it back, and says so here rather than discovering it later.
- **The acceptance criteria**: the findings probe answers `none()` and then refuses to elaborate; `/std` builds with `Set` as a specialization; `Toml` round-trips; and the corpus's `/data/map` program, which exercises the lookups, the canonical shape and the rewriting functions, passes unchanged but for its key types.
