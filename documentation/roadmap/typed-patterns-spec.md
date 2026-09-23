# Typed patterns

Working specification for compiling nested pattern matches where the scrutinee's type is known — in elaboration rather than in text lowering — so that a wildcard may stand beside a concrete pattern in any column, coverage is decided against each type's real constructors, and an arm no value can reach is reported. The algorithm is Maranget's decision-tree compilation ("Compiling Pattern Matching to Good Decision Trees", ML Workshop 2008) with the usefulness check of "Warnings for Pattern Matching" (JFP 2007); what changes is where it runs.

## What this builds on

`curios-text/src/into_core/match_compile.rs` already compiles a pattern matrix into Core's single-level match forms, and most of Maranget's scheme is there:

- **specialization** — rows grouped by the constructor in a column, the constructor's arguments becoming new columns (`compile_ctor`, and the carrier paths `compile_bool`, `compile_nat`, `compile_list`, `compile_bin`);
- **the variable rule** — a column in which every row is a binder is retired, each name bound to the column;
- **tuple and struct columns** — flattened into their fields by projection (`compile_fields`), never dispatched;
- **a default** — the top-level `| _ =>` catch-all (`split_catch_all`), and a `choose` bind arm's rest-of-ladder, compiled once and shared behind a by-name thunk as the fallthrough of every sub-match that leaves a shape uncovered.

Core elaboration (`curios-elab/src/elaborate/match_.rs`, `elaborate_induct_match`) knows each scrutinee's inductive and its constructors, checks coverage, and inverts indices to discharge constructors an indexed family cannot produce (Rung-C vacuity). The kernel, erasure and every stage below read only the single-level forms.

## The gap

A binder and a concrete pattern cannot share a column of one group. [syntax.md](../syntax.md)'s multiple-scrutinee section states it; the compiler refuses with `match arm patterns disagree on shape for the same column`, and `curios-text`'s `nested_underscore_mixed_with_concrete_stays_inconsistent_shape` pins it. A wildcard is accepted only where earlier columns have already split its row into a group of its own, which is why `(some(n), some(v)) | (some(_), none()) | (none(), _)` compiles and each of these, all exhaustive, does not:

```crs
match (a, b) | (some(_), _) => 1 | (_, _) => 2 end
match (a, b) | (some(x), some(y)) => x + y | (_, none()) => 0 | (none(), some(y)) => y end
match (a, flag) | (_, true) => 1 | (some(x), false) => x | (none(), false) => 0 end
match m | some(some(x)) => x | some(_) => 0 | none() => 1 end
```

Code works around it by nesting matches by hand, which is what `/std/Flt/reduction` does for `product` and `sum`.

## Why lowering cannot close it

Maranget's step for a column mixing a binder with constructors is to put each binder row into every constructor's group, with a wildcard for each argument, and to make the binder rows alone — the *default matrix* — the case for every constructor no row names. The second half needs the column's full constructor set, and text lowering does not have it: it is type-blind, and a constructor pattern is a bare tag because the scrutinee's type supplies its namespace.

`Bool`, `Nat`, `List`, `Bits` and `Bytes` are the exception — their shapes are fixed, so lowering can place a binder row in every group and needs no default matrix. For an `induct` it must guess, and both guesses are wrong somewhere, which a lowering-side attempt made on 2026-09-23 found:

- **Always emit the default matrix as the Core match's default.** When every constructor is named, the default is dead, and elaboration still checks it: the second example above lowers the column's default to the binder rows, `match b | none() => 0 end`, which is not exhaustive on its own, and the whole match is refused though it is exhaustive.
- **Emit no default matrix.** A binder row then covers no constructor that lacks a written row, and `match (a, b) | (some(x), _) => 1 | (_, _) => 2 end` is refused for `none` though the second row covers it.

Neither patch holds. Detecting dead code after synthesizing it — skipping a default no constructor reaches — would also stop checking a hand-written catch-all after exhaustive arms. Choosing columns by Maranget's necessity heuristic (split first on a column with no wildcard) avoids the default matrix in most matches but not in all: `(some(_), none()) | (none(), _) | (_, some(_))` is exhaustive and has no wildcard-free column.

## Design

**Lowering hands Core the matrix, not the tree.** A new Core input form holds a match as written: the scrutinees, the optional motive, and the rows, each a list of Core patterns and a body lowered once. Core patterns mirror the surface's — constructor with plicity-marked arguments, binder, tuple, struct, and the carrier leaves (`Bool` literal, `Nat` zero/successor-with-hypothesis/literal, character as a `Nat` literal, list and packed-sequence empty/cons-with-hypothesis). A `| _ =>` catch-all is an ordinary final row of wildcards; a `choose` bind arm is a one-row matrix whose fallthrough is the rest of the ladder, as today. Only the elaborator reads the form; it never reaches the kernel.

**The elaborator compiles it, knowing every column's type.** The recursion is today's, moved: specialize, retire variable columns, flatten tuples and structs, dispatch carriers. What the types add:

- a binder row joins every constructor's group of the column's inductive, named or not, so a default matrix arises only where a user's catch-all or a bind arm's fallthrough supplies one, and nothing dead is ever built;
- a constructor Rung-C discharges is not a group at all, so a binder row is not specialized into an impossible case;
- the motive attaches to the head's split exactly as a written motive attaches today; a match whose head is not a single dispatch still refuses one;
- a `Nat` column is induction when a row peels a successor and literal dispatch otherwise, and dispatch still needs a default — a binder row now supplies it as well as a catch-all does.

**Bodies are shared, not copied.** A binder row joining several groups would copy its body into each leaf. Each body a row reaches from more than one leaf is bound once as a join point — a local function of the row's pattern variables — and every leaf applies it to what that path bound, so code size stays linear in the arms however the tree branches. A body reached once stays inline, which is every body in a match without mixed columns and keeps today's output there.

**`!` in a body.** Lowering hoists a `!` in an arm body today with the tree already built around it — the matrix compiler takes the region lowering as its leaf. With bodies lowered once, before the tree exists, each body has to be lowered as the region it is now at its leaf, so that where a `!` sequences does not move; the first thing to establish is that the leaf's region is the body's own and depends on nothing the tree binds above it, which the region fixtures in `curios/src/tests` then hold.

**Column order.** Leftmost first, as today; it is correct and it keeps the output of every current match unchanged. A necessity-based choice shrinks trees, and is worth adding only for a match whose tree is measured to be large.

## Diagnostics

The typed compiler computes Maranget's *usefulness* as it goes, which gives two reports the untyped one cannot make:

- **Non-exhaustive, with a witness.** A value no row matches is reported as a pattern — `(none(), some(_))` is not matched — rather than as a missing constructor inside a synthesized sub-match.
- **Redundant arms.** A row useful for no value is an error naming it: a second arm shadowed by an earlier wildcard (`| (_, true) => … | (some(x), true) => …`), and a `| _ =>` after arms that already cover every constructor, which is checked today and silently unreachable.

Whether a redundant arm is an error or a warning is decided when this lands. There is no warning channel in elaboration today, which argues for an error, as a duplicate row is one now.

## What changes and what stays

Changes: the Core input form and its patterns; the matrix compiler moves from `curios-text` to `curios-elab`, keeping its structure; [syntax.md](../syntax.md) drops the column restriction and states first-match order for rows that overlap; `nested_underscore_mixed_with_concrete_stays_inconsistent_shape` becomes a test that the match compiles and answers each case. `/std`'s hand-nested matches, `/std/Flt/reduction`'s among them, may be flattened after, as their own commit.

Stays: every Core match form the elaborator emits, and so the kernel, erasure, the continuation stage and the emitter; the rule that `_` is the only catch-all and a named final arm is refused; tuple and struct patterns projecting rather than dispatching; `choose`.

## Verification

- The four matches under "The gap" compile, and a table of inputs checks each answers by first-match order, folded and executed.
- Every existing match lowers to the same Core as before, checked by printing the `core-elab` rung of `/std` before and after and comparing: leftmost column order and inline single-leaf bodies make this an identity for any match without a mixed column.
- A body reached from several leaves appears once in the output, and the output grows linearly in a family of matches whose tree does not.
- Each diagnostic has a fixture: a non-exhaustive match reporting its witness, a shadowed arm, a dead catch-all, and an indexed family where Rung-C removes a constructor and a binder row must not be specialized into it.
- `cargo x clippy`'s elaboration of all of `/std` and the full test suite pass unchanged.

## Completion

Done when a wildcard may stand in any column beside concrete patterns, coverage and redundancy are decided against the scrutinee's type with the diagnostics above, no match compiles to a default no constructor can reach, and every match that compiled before compiles to the same Core.
