# Plicity on a telescope's members

Working specification for making a member's plicity — explicit, implicit (`@`) or witness (`use`) — a uniform property of every telescope the surface writes, rather than a property of function parameters with carve-outs elsewhere. Three stages: the first states and represents the rule the language already half-implements, the second admits `use` fields on a structure, the third admits `@` fields.

## What this builds on

[Plicity is part of function identity](../../design/theory/plicity-is-part-of-function-identity.md) settles plicity for function types: a binder's mark is part of a function type's identity and calling convention, convertibility is plicity-aware, and a plain binder may not claim a marked slot. Its **Rejected** list already decides the pattern question — "inserting omitted hidden constructor-pattern arguments by analogy with lambdas" — which is why `wrap(m)` is refused where `wrap(@m)` is required.

That decision's frame is a *function type's* identity. Two things sit outside it today:

- an **inductive constructor's payloads already carry plicity** — `| wrap(@n: Nat): (n)` is accepted, and `Eq/refl(@z: A)` is how the standard library uses it;
- a **structure's and a concept's fields do not** — `@` in field position is refused by the parser, and no document says why.

A structure's fields are a telescope: `Str/At`'s `within` may mention `offset` because the former's parameters are its fields in order. So the restriction to explicit fields is a property of the surface, not of records.

## Stage 0 — the rule, stated once and represented once

No new capability. What is accepted stays accepted; what is refused is refused for a stated reason.

### The warts this closes

- **`use x` in a declaration position is silently misread.** It is accepted and means "an anonymous slot whose *type* is `x`". An author reaching for the lambda idiom (`use show`) is told `unbound variable: show`, which never says a binder is not allowed there. Where a type-valued name happens to be in scope it compiles as something else entirely.
- **The `use x: T` refusals leak parser backtracking.** `Expected '=>'` in a function type, `Expected ')', obtained ':'` in a definition telescope, `Expected '}', obtained ':'` in a concept body: three positions, three messages, each naming the alternative that failed rather than the rule.
- **`use T` in a lambda** is refused by the same leak (`Expected '->'`), and `use _` is the undocumented spelling of what the author wanted.
- **Anonymity is represented three ways.** `FuncTypeParam` carries `label: Option<Label>`; `FuncSugarParam` carries a `Pattern` it keeps and then ignores, its printer branching on `plicity == Plicity::Witness`; `ConceptField` carries `is_super: bool` *beside* `label: Label::from("")`. One concept, three mechanisms, so the rule is re-derived per node instead of stated.
- **The rule is nowhere stated.** `syntax.md`'s witness-parameter paragraph asserts that a `use` parameter is anonymous and that "a proof meant to be discharged is an implicit `@` parameter instead", without saying why `@` is excluded from anonymity.

### What it establishes

One sentence, in `documentation/syntax.md`:

> A telescope parameter is `name: type`, and either half may be omitted where the other determines it — the type where the expected type gives it, the binder where the slot is never named.

Anonymity becomes one represented property across `FuncTypeParam`, `FuncSugarParam` and `ConceptField`, which retires the `Witness` branch in the sugar printer and the `is_super`/empty-label pair in the concept field. **`@T` in a definition telescope falls out of that**: the `use`-only carve-out is what refuses it, so removing the carve-out admits `@Holds(0 < b)` without a separate feature. The function-type parser already accepts the anonymous form at all three plicities, so the behaviour is not new — it is made uniform.

The diagnostics follow the two the language already states in domain terms: "a `use` parameter's type must be a concept application — found: …", and "names '/e' as a superclass, but '/e' is not a registered concept". A bare name in a declaration position must say that a binder is not accepted there, rather than reporting a missing type.

`use C` for a nullary concept stays legal, so the bare-name fix is a message rather than a grammar restriction.

### Acceptance criteria

- `(@T) -> U`, `(use C(T)) -> U` and `(T) -> U` behave identically before and after.
- `pub let f(n: Nat, @Holds(0 < n)) -> Nat` elaborates, and behaves exactly as `@_: Holds(0 < n)` does today, the bound prover's reading of the hypothesis included.
- Each refusal above names the rule, and none names a parser alternative.
- `curios format` round-trips every form, and the grammar, Zed and VS Code steps pass.

## Stage 1 — `use` fields on a structure

A structure's field may carry the witness mark. The slot is resolved at construction and carried in the value, so every operation over that value uses the dictionary the value was built with.

```crs
pub struct Map(K: Type, V: Type): Type {
    use Hash(K),
    size: Nat,
    root: Option(Node(K, V)),
}
```

**Only resolution is needed**, which already works at every other `use` position, so this stage owes no inference contract.

- **Construction** fills an omitted `use` field by resolution, and `use value` supplies one explicitly, as at a call site.
- **Projection** is unmarked — `v.field`. A mark constrains how a member is *given*, never how it is read; nothing in the language is writable and unreadable.
- **Patterns** require the mark, per the decision's existing rejection.
- **A spread copies a `use` field from its base.** A witness is a *choice* fixed at construction, and carrying it is the point.

### Why this is soundness and not ergonomics

`/std/Map` is a crit-bit trie whose forks hold bit indices into an encoding and whose leaves hold that encoding. A dictionary supplied explicitly on a later operation may send a walk astray, but the stored bytes are then a self-check: `Bytes/eql` fails and the answer is `none()`. That is why the module can honestly record the consequence as two members rather than a wrong one.

A trie that stores the key and derives its bytes loses that self-check, and two failures become reachable that are not today: a walk that goes the wrong way, re-encodes the leaf under the same wrong dictionary and *matches*; and an insertion that computes a critical bit under a different dictionary than the forks above it, breaking the invariant that each fork's bit discriminates its subtrees. Global coherence does not prevent it, because one witness per key governs resolution while `use value` overrides resolution. Nor does a premise — `satisfy (@V: Type, use Eql(V)) => Eql(Map(V))` is resolved where the witness is used, not where the value was built.

So a structure that keys itself by an encoding is sound only with the dictionary carried, and `Map(K, V)` is downstream of this stage.

### Acceptance criteria

- A `use` field is resolved when omitted and accepted when supplied, and a value carries it.
- `/std/Set`'s four key-taking operations stop threading `use Key(K)` separately.
- `Map`'s documented two-dictionary hazard becomes unstateable rather than sharper.

## Stage 2 — `@` fields on a structure

A structure's field may carry the implicit mark, filled at construction by the routes an omitted `@` argument already uses: the proposition reduces to `True` and is filled with its constructor; it follows from the facts in scope by linear arithmetic; or it is determined by unification from the other fields or the expected type. Written explicitly as `S { x = k, @ok = p }`, and refused with the proposition it could not discharge.

```crs
pub struct At(s: Str): pub Type {
    offset: Nat,
    @within: Holds(offset <= Bytes/len(s.bytes)),
    @boundary: Holds(starts(Bytes/drop(s.bytes, offset, @within))),
}
```

`Str/At` is the case: two proof fields written at every construction site, where the same proposition as a function argument fills itself. `finish` writes `within = Nat/le/refl(Bytes/len(s.bytes))` by hand, and under the first route it reduces to `True` and disappears.

**A spread re-infers every `@` field and never copies one.** An `@` field is a *consequence* of the other fields, so copying preserves a stale proof about changed values. Today that survives only by luck of the carrier: `Holds(0 < 1)` and `Holds(0 < 2)` are both `True`, so a stale copy typechecks, while `n = 0` correctly fails — sound by reduction rather than by design. A spread that cannot re-infer a field is refused, naming the proposition.

**Rejected:** re-infer, else reuse the base's, else fail. It makes one source text mean two things depending on whether inference happened to succeed, which is the spelling-dependence [What conversion still decides by spelling or by cap](../04-arithmetic/01-decided-by-spelling-or-cap.md) already owns.

That the two marks want opposite spread rules is why this is a stage of its own rather than part of stage 1: a witness is a choice to preserve, a proof is a consequence to recompute.

### Still to refine

- **Eta.** Records have eta, so a value must convert with its field-by-field expansion. A `Prop` field is free under irrelevance; an `@T: Type` field needs the field determined, and that case wants a worked example before anything is implemented.
- **Cost.** Filling a field runs the evaluation a bound does, so a record built in a loop pays per construction, priced by [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md).
- **Derived witnesses.** `Spell`, `Eql`, `Hash` and `Ord` are written over a structure's fields; `Spell` must skip an implicit field, since a spelled value reads back and an implicit field is not written.
- **The smallest honest slice**, if this is ever staged further: `@` fields filled by reduction alone, which covers `Str/At` entirely, needs no prover work, and defers the cost and eta questions with the other two routes.
