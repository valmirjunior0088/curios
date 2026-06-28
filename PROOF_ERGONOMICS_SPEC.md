# Design specification: definitional branch refinement for proof-code ergonomics

## Summary

The proof code in `std` — `std/Str.crs` above all — is painful out of
proportion to what it proves. This spec diagnoses *why*, and proposes a single
kernel change that removes the largest share of the pain:

> **Generalize `refine_head` from `Var`/`Proj` scrutinees to any *stuck-neutral*
> scrutinee.** In a match arm, register the branch-local definitional reduct
> `scrutinee ⇝ constructor`, so the arm body sees the equation the match
> discriminated on — for `match classify(c) | ascii() => …`, the body learns
> `classify(c) ≡ Class/ascii()` and reduces through it.

This dissolves the **propositional-equality plumbing** that is the worst of the
pain: `Eq/subst`/`Eq/cong`, the `step_lead_*` bridging lemmas, and the
`Eq/refl()`-remember convoys exist *only* because a `match` on a compound term
teaches its arms nothing. With the refinement they become definitional and
vanish.

It is a **purely additive** kernel change: refinements only ever *add* reducts,
inside an arm's frame, suppressed during metavariable re-validation. Nothing that
type-checks today stops type-checking; some things that needed a manual proof
stop needing one.

Two further levers are recorded for completeness — **Γ_dep** (motive abstraction
of a dependent hypothesis; the smaller, complementary lever for the residue) and
a **zero-cost precheck** (some convoys are likely already dead). An arithmetic
`Lte`/`Lt` decision procedure and emptiness-lemma automation are named and
deliberately deferred.

## Background — where the pain actually is

Walking `std/Str.crs`, the pain falls into four kinds, worst first:

1. **Propositional equality plumbing.** `Eq/subst`, `Eq/cong`, and the lemmas
   that exist only to feed them — `step_lead_lead`, `step_lead_ascii`,
   `step_lead_bad` (`std/Str.crs:234–245`) — plus the `Eq/refl()`-remember
   convoy in `decode_head` (`std/Str.crs:247–262`). This is the most cognitively
   expensive code: you hand-carry an equation the machine should already know.
2. **Emptiness lemmas.** `bad_uninhabited`, `cont0_uninhabited`
   (`std/Str.crs:146`, `:178`): hand-written recursive proofs that a proposition
   is uninhabited, to feed `False/absurd`.
3. **Refinement convoys.** The `let go = match s : (s) => (p : T(s)) -> Goal | …
   ; go(d)` wrappers (`peel_byte`'s `go`, the `on_range` blocks).
4. **Order/bounds threading.** `Nat/Lte`/`Lt` witnesses dragged through
   recursion (`take_conts`, `at`) and the `Lt` obligation on every `Bin/at`.

### The unifying observation

Almost every painful proof in `Str.crs` is the same shape:

> *the system will not reduce `X` to `Y` inside this branch, so I prove `X = Y`
> propositionally and `subst`.*

`decode_head` matches `classify(c)`, cannot get the system to see
`classify(c) = Class/ascii()` in the ascii arm, and so threads an `eq` and
substitutes three times through the `step_lead_*` lemmas. `on_range`
(`std/Str.crs:161`, `:181`) matches `Nat/in_range(c, lo, hi)` and convoys `rest`
for the identical reason. The propositional apparatus is a workaround for a
*missing definitional reduction*.

### The cure already exists — and is under-powered

The kernel already turns "the branch discriminated on this" into a definitional
fact: `refine_head` (`src/core/typing.rs:292`). In a match arm it registers the
counterfactual reduct `scrutinee ⇝ arm-value`, frame-scoped to the arm. But it
fires only for two scrutinee shapes:

```rust
pub fn refine_head(context: &mut Context, head: &Term, value: &Term) {
    match &**head {
        Subterm::Var(var) => context.refine(var.unwrap(), value),
        Subterm::Proj(Proj { head, field: Field::Index(index) }) =>
            context.refine_projection(head.clone(), *index, value.clone()),
        _ => {}                       // <-- compound scrutinee: nothing learned
    }
}
```

A compound scrutinee such as `classify(c)` or `Nat/in_range(c, lo, hi)` hits the
`_ => {}` arm and records nothing, so its body never learns the equation — hence
the propositional workaround. The `Var`/`Proj` restriction was never a soundness
requirement; it is a **key-stability** requirement (below), and a stuck neutral
satisfies it just as well.

### Why `step(c, lead())` reduces once `classify(c)` is refined

`step` (`syn/Str.crs:39–57`) is *defined* by matching on its scan, and in the
`lead()` case by matching on `classify(c)`:

```
| lead() =>
    match classify(c)
    | ascii()            => Scan/lead()
    | lead(cont, lo, hi, _) => Scan/cont(cont, lo, hi)
    | bad()              => Scan/bad()
    end
```

So `step(c, Scan/lead())` is stuck *only* on `classify(c)`. If an arm refines
`classify(c) ⇝ Class/lead(k, lo, hi, payload)`, then `step(c, Scan/lead())`
reduces to `Scan/cont(k, lo, hi)` **definitionally** — which is exactly what
`step_lead_lead` proves propositionally. The bridging lemma becomes redundant.
The same holds for the `ascii`/`bad` arms. This is the concrete mechanism by
which the lever pays off; it is not hypothetical.

## The mechanism today (what we build on)

The refinement machinery already has every part needed except the general key:

| Concern | Today | File |
|---|---|---|
| Var reduct store | `refinements: Vec<HashMap<String, Term>>` | `context.rs` (field) |
| Proj reduct store | `refinement_projections: Vec<HashMap<(Term, usize), Term>>` | `context.rs` (field) |
| Register | `refine` / `refine_projection` | `context.rs:321`, `:335` |
| Consult (var) | `var_reduct` | `context.rs:348`, used `reduce.rs:297` |
| Consult (proj) | `proj_reduct` | `context.rs:360`, used `reduce.rs:92`, `:124` |
| Suppress for §7.4 re-validation | `with_suppressed_refinements`, `suppress_refinements` | `context.rs:387` |
| Fast-path gate | `has_refinements` | `context.rs:375`, gates `convert.rs:1212` |
| Additivity invariant | "only ever *add* reductions" | `convert.rs:1201` |

The `refinement_projections` store already keys a reduct on a **`Term`** (the
projection base). Generalizing to a whole stuck-neutral term is the same move,
one notch wider.

## Proposed change — stuck-neutral refinement

### Soundness and stability

- **Soundness.** A `match` on `e` with an arm for constructor `K` runs that arm
  only when `e` reduces to `K …`. So inside the `K` arm, `e ≡ K …` is a true
  equation; registering it as a frame-scoped reduct asserts nothing false. This
  is the same justification that licenses `b ⇝ cons(h, t)` when matching a
  variable `b`, and it is identical to the Rung-B index refinement the arm loop
  already performs (`elaborate_match.rs:751–760`).
- **Stability (the real constraint).** A reduct keyed on a term is only useful if
  the key cannot itself reduce and so go stale mid-conversion. A **stuck
  neutral** — a variable-headed `Apply`/`Proj` spine with no redex and no
  metavariable at the head — is already in weak-head normal form at its head, so
  it is a stable key, exactly as `Var` and `Proj` are. This is why the gate is
  *stuck-neutral*, not *any compound term*: a redex scrutinee (e.g. an applied
  lambda) must reduce first and key on its normal form.
- **Re-validation.** Stuck-neutral reducts join the existing suppressible store,
  so `with_suppressed_refinements` (§7.4 metavariable re-validation) ignores them
  with no new code path.
- **Additivity / backward compatibility.** Refinements only *add* reducts within
  an arm frame; they can make more terms convertible, never fewer. Every program
  that checks today still checks.

### Kernel changes

#### 1. Context — a term-keyed reduct store (`src/core/context.rs`)

Mirror `refinement_projections` exactly, keyed on the whole stuck-neutral term:

```rust
// field, beside refinement_projections:
refinement_terms: Vec<HashMap<Term, Term>>,

/// Register a counterfactual refinement of a stuck-neutral scrutinee
/// (`refine_head` on an application/projection spine). The key must be the
/// weak-head-normal form of the scrutinee, so lookup in `reduce_apply` —
/// which sees the reduced spine — hits it.
pub fn refine_term(&mut self, scrutinee: Term, value: Term) {
    self.refinement_terms.last_mut().unwrap().insert(scrutinee, value);
    self.reductions.clear();
}

/// The reduct of a stuck-neutral term, unless refinements are suppressed.
pub fn term_reduct(&self, scrutinee: &Term) -> Option<&Term> {
    if self.suppress_refinements { return None; }
    self.refinement_terms.iter().rev().find_map(|m| m.get(scrutinee))
}
```

Extend the three existing maintenance points the same way the projection store
is wired: push/pop with each frame (`with_frame`/`assume` bookkeeping), include
in `has_refinements`, and the `vec![HashMap::new()]` seed in `Context::new`.

#### 2. Reducer — consult the store for a stuck application (`src/core/reduce.rs`)

`reduce_apply` (`:65`) forms a stuck `Apply` when the head is not a `Func`. Add
the lookup there, mirroring `reduce_proj`'s post-reduction consultation
(`:122–128`):

```rust
head => {
    let stuck = Term::from(Subterm::Apply(Apply { head: head.into(), params, plicities }));
    match context.term_reduct(&stuck) {
        Some(v) => Ok(Reduce::Continue(v.clone())),
        None    => Ok(Reduce::Break(stuck)),
    }
}
```

Gate behind `context.has_refinements()` so the overwhelmingly common
refinement-free path pays nothing (the `Term` key carries a cached hash, per the
`mutable_key_type` note in `context.rs`).

#### 3. `refine_head` — register for stuck neutrals (`src/core/typing.rs:292`)

Replace the `_ => {}` arm:

```rust
other => {
    if is_stuck_neutral(other) {
        // Key on the weak-head-normal scrutinee so the reducer's lookup matches.
        let key = reduce_with(context, head)?;     // already WHNF for a stuck neutral
        context.refine_term(key, value.clone());
    }
}
```

`is_stuck_neutral` is the gate: a `Var`-headed `Apply` or `Proj` chain, no
metavariable at the head, head not further reducible. (`Var`/`Proj` keep their
existing dedicated paths; this arm is only the wider case.) Start conservative —
a `Subterm::Apply`/`Subterm::Proj` whose ultimate head is a free `Var` and whose
WHNF is unchanged — and widen only if a real site needs it.

#### 4. No changes to coverage, erasure, or codegen

Refinements are a convertibility aid consulted by `reduce`; they never justify a
typing on their own (the motive does), they are erased, and they do not alter
exhaustiveness or inversion. Sections of `elaborate_match.rs` that compute
coverage and inversion are untouched.

## Effect on `std/Str.crs`

| Site | Scrutinee | Today | After |
|---|---|---|---|
| `decode_head` `go` (`:247`) | `classify(c)` | `(eq : Eq(classify(c), cl)) -> Nat` convoy + two `Eq/subst` + `go(Eq/refl())` | bare `match classify(c) | ascii … | lead … | bad …`; motive is plain `Nat` |
| `step_lead_lead/ascii/bad` (`:234–245`) | — | three lemmas feeding the substs | **deleted** |
| `cont_len` `on_range` (`:161`) | `Nat/in_range(c, lo, hi)` | convoy abstracting `rest` | bare `match Nat/in_range(c, lo, hi)`; `rest`'s type reduces per arm |
| `cont0_uninhabited` `on_range` (`:181`) | `Nat/in_range(c, lo, hi)` | convoy abstracting `rest` | bare; lemma body shrinks (the lemma itself stays — see below) |
| `step`'s own nested matches (`syn/Str.crs:43,46`) | `Nat/in_range`, `rem == 1` | (internal) | callers like `fold`'s `match rem == 1` (`std/Str.crs:281`) reduce through them when refined |

In `decode_head` specifically, the `lead` arm's
`Eq/subst((sc) => Utf8(sc, t), step_lead_lead(c, eq), peel_byte(…))` collapses to
just `peel_byte(…)`: with `classify(c)` refined, `peel_byte`'s result type
`Utf8(step(c, Scan/lead()), t)` already reduces to `Utf8(Scan/cont(k, lo, hi), t)`,
which is what `cont_len` demands. Both substs and all three bridging lemmas go.

**Not removed by this lever** (recorded honestly): `bad_uninhabited` and
`cont0_uninhabited` remain as lemmas — they are genuine recursive emptiness
proofs; refinement only simplifies their *bodies*. Eliminating the lemmas
themselves is the emptiness-automation lever (deferred).

## Complementary lever — Γ_dep (the residue)

Refinement makes the *scrutinee* reduce. It cannot abstract an *independent
hypothesis* whose type tracks an index — e.g. `peel_byte`'s
`nz : Nat/Lt(0, Bin/len(b))` (`std/Str.crs:205–214`), which is a separate
binder, not a goal reduction. That residue wants **Γ_dep**: synthesize the
dependent motive `(x, q) ↦ Π(nz : Lt(0, len(x))). Goal` and rebuild the match as
`Apply(Match, [nz])` — the eliminator the convoy writes by hand.

Γ_dep is the smaller, second lever, and it is **best implemented on top of
telescopes**: the dependent context is a `Telescope<Term>`, built with
`Term::func_type`/`Telescope::build`, walked per-arm with `Telescope::walk`
(re-assuming under the original labels so a bare arm body resolves by
shadowing), opened for the result type, and applied with `Term::apply` — reusing
the same combinators `check_inductive_motive` and the arm loop already thread
(`elaborate_match.rs`). The only non-telescope part is the occurrence-abstraction
that turns scrutinee/index/Γ-member occurrences into binders (`Scope::close`
name-based; `abstract_occurrences`, `convert.rs:922`, term-based — to be lifted
out of the solver and shared).

Crucially, **the refinement lever shrinks what Γ_dep must cover**: with
stuck-neutral refinement handling `decode_head` and both `on_range` blocks,
Γ_dep is left with only `peel_byte` (and whatever the precheck below does not
already retire). See the `convoy-elimination-plan` memo for the full prior
analysis; this spec supersedes its framing of Γ_dep as the primary lever.

## Zero-cost precheck (do this first)

The session that shipped `e4e7489` added `Nat` `refine_head`. Several bare-var
convoys may already be dead and merely uncashed — in particular `at`'s `on_b`,
`on_sc`, `on_i` (`std/Str.crs:337–368`), which wrap matches whose result is a
plain `Nat` over scrutinees (`b`, `sc`, `i`) that `refine_head` now refines.
Strip them, rebuild, run the Str examples. Anything that passes is free pain
removed before any kernel work.

## Ordering and scope

1. **Precheck** — strip already-dead convoys (`at`). Zero new code.
2. **Stuck-neutral refinement** — the kernel change above. Retires the
   eq/subst apparatus (`decode_head`), the `step_lead_*` lemmas, and both
   `on_range` blocks. The headline win.
3. **Γ_dep (telescope-native)** — mop up `peel_byte` and any bare-var residue.
4. **Deferred:** an `Lte`/`Lt` decision procedure for the order/bounds threading
   (pain category 4), and emptiness-lemma automation (category 2). Both are
   larger, separate efforts.

## Risks and open questions

- **Exact `is_stuck_neutral`.** The conservative definition (free-`Var`-headed
  `Apply`/`Proj`, WHNF unchanged, no head metavariable) is the safe start. Open:
  whether to admit projection-of-application spines and primitive-headed neutrals
  (`Nat/in_range` is a `/sys` primitive application — confirm it presents as a
  stuck `Apply` to `reduce_apply`, not a special primitive node, before relying
  on it for `on_range`).
- **Key canonicalization.** Registration (`refine_head`) and lookup
  (`reduce_apply`) must key on the *same* normal form, or the lookup silently
  misses. The spec keys on WHNF at both ends; verify `reduce_apply`'s
  reconstructed `stuck` term is structurally equal to `refine_head`'s
  `reduce_with(head)` (same plicities, same param forms).
- **Performance.** One `HashMap<Term, Term>` lookup per stuck application *when
  refinements are live*; gated to nothing otherwise by `has_refinements`. Expect
  negligible cost; confirm against the std-elaboration timing (the
  `std-elaboration-memo`).
- **Interaction with `solve_refinement_free`** (`convert.rs:1205`). Metavariable
  solving deliberately retries refinement-free first; stuck-neutral reducts join
  the same `has_refinements` gate, so the existing two-phase logic carries over
  unchanged — but it is the one place to re-read when wiring `has_refinements`.

## Verification

- `cargo build` — kernel compiles.
- `cargo test` — full library suite (738 today); pay attention to
  `src/tests/inference.rs` (motive/refinement behavior) and any Str-exercising
  suite.
- Examples end-to-end: `crs_proofs`, `crs_eq` (equality apparatus),
  `crs_printf` and `crs_json_codec` (drive `std/Str`). Each must still produce
  its asserted output.
- **Before/after on `std/Str.crs`:** record the line and lemma count. Target
  deletions: `step_lead_lead`, `step_lead_ascii`, `step_lead_bad`; the `eq`
  convoy and two substs in `decode_head`; the two `on_range` convoys. Net
  removed proof code is the headline metric for "less painful."
- **Negative check:** a `match` on a compound scrutinee whose arms are
  *inconsistent* with the refinement must still fail to type — refinement adds
  reducts, it does not weaken `expect`. Add a test asserting that a body relying
  on the *wrong* constructor's equation is rejected.

## Out of scope

- **`fold`'s outer `match s.bytes`** (`std/Str.crs:265`). Its motive abstracts
  `acc : {A, Nat}`, whose type is independent of the scrutinee; it is a genuine
  dependent fold whose motive *is* the loop invariant, not boilerplate. Neither
  refinement nor Γ_dep should touch it — synthesizing it would mean guessing the
  invariant. This is where convoy-elimination correctly stops.
- **Arithmetic / bounds threading** (pain category 4) — wants a decision
  procedure, a much larger effort.
- **Emptiness-lemma automation** (`bad_uninhabited`, `cont0_uninhabited` as
  lemmas) — a separate inversion-strengthening effort.
- No change to refinement *semantics* for `Var`/`Proj`; only the *key* widens.
