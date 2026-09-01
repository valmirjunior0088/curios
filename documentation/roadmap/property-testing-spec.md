# Property-based testing: a parameterized test is probed over drawn arguments

## Status

Nothing is built. This specification depends on two grounds: the landed test harness (see [the decision](../design/toolchain/a-test-is-a-declared-description-run-by-a-synthesized-tail.md)) — the `test name() = body;` form whose empty parentheses it opens as its seam, the synthesized tail, `Test`/`Verdict` and the combinators — and the [function-key specification](func-witness-keys-spec.md), because every goal below keys on a function type. The machinery it composes was probed on 2026-08-31 against the tuple landing: premise regularity, superclass projection, per-shape witness rosters, and deferral all behave as cited.

## Mission

```crs
test add_commutes(n: Nat, m: Nat) =
    Test/check(n + m == m + n);
```

`curios test` runs the body over drawn arguments, small first; a failing case reports the drawn arguments, spelled; the run is deterministic, a function of the sources alone. A test with empty parentheses is exactly the harness's test, untouched.

## Design

### Permanent decisions

1. **The declaration is the property, and arity is the dispatch — never the body's type.** A parameterized test's body still elaborates against `Test`, under the authored telescope, declared type `(params…) -> /syn/Test`; the invented nullary lambda of the harness's lowering generalizes to the telescope the parentheses spell. There is no `Bool`-bodied form: a bare comparison writes `Test/check(…)` around itself, so one body type serves every test and the elaborator never chooses a contract from what the author happened to write.
2. **The tail closes a parameterized test through one emitted name.** The synthesized tail pairs a nullary test with its own `Global`, as the harness states; a parameterized one becomes `("path", () => Test/property(t))` — `property` a new `SyntaxRegistry` slot beside `main`'s, declared in `/syn/Test.crs` and re-exported by the `/std/Test` facade. The application carries one witness goal, `Property((params…) -> Test)`, resolved during the tail's check as any authored tail's goals are; a parameter type the roster cannot draw fails that goal at the declaration, with the ordinary missing-witness report naming what is missing.
3. **`Property` judges purely, in `Verdict`, and only its base witness sees a `Test`.** `probe(A, Seed, Nat) -> Verdict` is the per-case judgment. `Test`'s representation privacy is load-bearing: no `/std` module can match a description, so the per-arity witnesses are result-generic — `Property((A) -> R)` with a `use Property(R)` premise, which regularity demands anyway, since `use Property(Test)` applies a concept to a constant — and the one witness that eliminates a description, `Property(Test)`, is declared in `/syn/Test.crs`, the module allowed to: `theorem()` is `passed` (the kernel settled the proposition over the open binders, once, at elaboration), `verdict(v)` is `v`, and `action(_)` is `failed("an action cannot be probed")` — the pure runner performs nothing, and effectful properties wait behind the same seam.
4. **Deterministic by default.** `Test/property` runs a fixed case count from a fixed default seed, the size growing over the run so early counterexamples come small; drawing is pure and the seed splittable, so no case's draws depend on another's appetite.
5. **A failure names its arguments.** The per-arity witness draws every argument, applies the property, probes the result, and on failure reports the drawn arguments, spelled, before the inner report — which is why `Spell` premises ride beside the `Draw` premises.

### The concepts

```crs
-- /std/Test/Seed.crs: the splittable pure generator state — make, split, next.
-- /std/Test/Draw.crs:
pub concept Draw(A: Type): pub Type {
    draw(Seed, Nat) -> {A, Seed},
}
-- /std/Test/Property.crs:
pub concept Property(A: Type): pub Type {
    probe(A, Seed, Nat) -> Verdict,
}
```

`Property/run(@A: Type, use Property(A), prop: A, seed: Seed, cases: Nat) -> Verdict` is the loop, in `/std/Test/Property.crs`; `Test/property(@A: Type, use Property(A), prop: A) -> Test` wraps its verdict and lives beside the combinators it joins, in `/syn/Test.crs`, because only that module may build a description from one.

The concept is a noun and its method a verb, as `Ord`'s is `cmp`: the concept spells what the missing-witness report at a parameterized declaration names — `Property((Foo) -> Test)`, the feature its author was using — and what `Test/property` resolves, while `probe` names the act, one case put to the claim.

### The witnesses

- `Property(Test)` in `/syn/Test.crs`, as decision 3 states it.
- `satisfy (@A: Type, @R: Type, use Draw(A), use Spell(A), use Property(R)) => Property((A) -> R)` and its siblings through arity 8, in `/std/Test/Property.crs`: each splits the seed, draws its arguments, applies, and delegates the result to the `Property(R)` premise. The premises are regular on the witness's own binders and the recursion is structurally decreasing on the type, so the existing termination discipline accepts them as written. The generic result is what lets a curried spelling compose through recursion while a declaration's uncurried type finds its own arity directly — and, since the key ignores the result, the one-witness-per-key rule makes `-> Test` at each arity the concept's single, canonical discipline.
- `Draw` for the carriers — `Nat`, `Int`, `Byte`, `Bool`, `Char`, `Str`, `Bits`, `Bytes`, `Order` — for `Option`, `Result` and `List` over premises, and for the positional tuple shapes `{}` through arity 8, beside the shapes' `Show`, `Eql` and `Ord`. `Flt` is deferred until its distribution — non-finite values included — is decided deliberately rather than inherited from bit noise.

### Limits, stated

- **Non-dependent parameters only.** A dependent telescope does not unify with the witnesses' shapes, so `test bounded(n: Nat, p: Lt(n, 100)) = …` reports its missing `Property` at the declaration. A conditional-property combinator is Adjacent work; a generator for proofs is nobody's.
- **The ceiling and the roster are `/std`'s promise.** A function shape and a tuple shape are owned by no root, so only the concept's home extends either — the same standing the tuple witnesses have. A program writes `Draw` for its own types and `Property` for its own concepts, freely.

### Reporting

A property failure flows through the harness's ordinary `failed` rendering; the message carries the spelled arguments and the inner report, and no new reporting machinery exists. The exit codes, filters and report lines are the harness's, unchanged.

## Non-goals

- Shrinking: small-first sizing keeps early counterexamples small, and a `shrink` field is a compatible later addition to `Draw`.
- Effectful properties, and any change to `Property(Test)`'s `action` refusal.
- Derived `Draw` — the auto-derive seam, later.
- `Bool`-bodied tests, and any type-directed dispatch at the `test` form.
- `Draw` at labeled tuple shapes.
- Any change to `Verdict`, `Test`, or the harness's pinned combinator signatures.

## What is left to build

Each item is one authorization and one commit, with its tests; the full gate runs once after the last. Items 1 and 2 need the harness's step 2 and the function-key specification; item 3 needs the harness's step 4.

1. `/std/Test/Seed.crs` and `/std/Test/Draw.crs` with the roster, declared as submodules of the `/std/Test` facade — which stays otherwise a facade: `pub mod` lines beside its re-export, no definition of its own. Tests: a drawn sequence pinned under a fixed seed, tuple-shape composition through premises, and the prelude build exercising every witness on each workspace check.
2. `/std/Test/Property.crs` with the per-arity witnesses and `Property/run`; `Property(Test)` and `Test/property` in `/syn/Test.crs`, the facade re-export, and the registry slot. Tests: `Test/property` applied explicitly in `run`-style programs — a passing property, a failing one with its counterexample line pinned, a curried property resolving through recursion, a `theorem()`-bodied case passing.
3. The seam opened: a parameter telescope admitted between the test parentheses, lowering under the authored telescope, and the tail's closing from decision 2. Tests: the Mission program's report line; a failing property's counterexample through `curios test`; two runs byte-identical; a dependent or proof-typed parameter reporting its missing witness at the declaration; a nullary test byte-identical to before; `pub test` still refused.
4. Documentation: `syntax.md`'s test section; a decision file, `documentation/design/language/a-parameterized-test-is-a-property.md`; this file deleted and its roadmap line checked.

## Verification

- The Mission program passes; negating it fails with a spelled counterexample naming both arguments.
- Determinism: two consecutive `curios test` runs of a property package produce identical output.
- `test t(n: Nat) = Test/refl(n + 0, n, …);` — a parameterized theorem — passes with the kernel having settled it once.
- A parameter with no `Draw` reports the missing witness at the declaration's span.
- The harness's own verification suite still passes unchanged for nullary tests.
- The gate in `CLAUDE.md`.

## Completion criteria

A parameterized test is a property probed over drawn arguments with one body type, one declaration form, and no type-directed dispatch; a failure names its counterexample, spelled; the run is deterministic; and this specification is gone.

### Decisions taken here, still reversible

- **100 cases**, size equal to the case index, from a fixed default seed. Each is one number in one place.
- **The counterexample message shape.**
- **The module layout**: `Seed`, `Draw`, `Property` as three submodules under the `/std/Test` facade, so everything testing-shaped is found under the one name and the paths read as the feature — `Test/Draw`, `Test/Property` — at the cost of `Seed` sitting under `Test` should something un-testing-shaped ever want a pure splittable generator.
- **The ceiling is 8**, matching the tuple shapes'.
- **`Test/property` is public** rather than emission-only, so a property can also be built and combined by hand.

## Adjacent work

- Shrinking, as a `Draw` extension.
- A conditional-property combinator (draw-retry with a give-up bound), for the preconditions the non-dependent limit excludes.
- Derived `Draw` through the auto-derive seam.
- Effectful properties through `Property(Test)`'s `action` arm.
- `Draw(Flt)` with a deliberate distribution.
- `Dec` by enumeration, the proof-side sibling ([function-key specification](func-witness-keys-spec.md), Adjacent work).
