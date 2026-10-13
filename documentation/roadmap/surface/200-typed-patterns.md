# Typed patterns

Working specification for compiling a match's patterns where the type of what they match is known — in elaboration, not in text lowering — so that a binder may stand beside a concrete pattern in any column, coverage is decided against each column's real constructors and reported as the value no arm matches, and an arm no value reaches is refused. Arm order still never changes what a match means, and the kernel, erasure and every stage below still read the single-level match forms they read today.

## What the code does today

**Lowering compiles the patterns, before any type is known.** `curios-text/src/into_core/match_compile.rs` takes a match's arms as a matrix of patterns over its scrutinee's columns and compiles it into Core's single-level match forms: rows are grouped by the constructor in the leftmost column and the constructor's arguments become new columns (`compile_ctor`, and `compile_bool`, `compile_nat`, `compile_list` and `compile_bin` for the carriers), a column in which every row is a binder is retired with each name bound to it, a tuple or struct column is flattened into projections (`compile_fields`), and a final top-level `| _ =>` is split off as the default of every dispatch it reaches (`split_catch_all`). A `choose` bind arm is a one-row matrix whose default is the rest of the ladder (`lower_bind_arm`).

**A binder beside a concrete pattern is refused, in every column and at every carrier.** A constructor pattern is a bare tag whose namespace the scrutinee's type supplies, so lowering cannot say which constructors a binder beside one stands for, and `MatchCompiler::compile` refuses the column: `match arm patterns disagree on shape for the same column`, pinned by `curios-text`'s `nested_underscore_mixed_with_concrete_stays_inconsistent_shape`. Each of these is refused so, and in each no two arms match one value but an arm and one written before it that is a case of it:

```crs
match (a, b) | (some(x), some(y)) => x + y | (_, none()) => 0 | (none(), some(y)) => y end
match (a, flag) | (_, true) => 1 | (some(x), false) => x | (none(), false) => 0 end
match (p, q) | (true, true) => 1 | (_, false) => 2 | (false, true) => 3 end
match (a, b) | (some(x), some(y)) => x + y | _ => 0 end
match (a, b) | (some(_), _) => 1 | (_, _) => 2 end
match m | some(some(x)) => x | some(_) => 0 | none() => 1 end
```

Code works around it by nesting matches by hand and writing a body once for each place it is reached from, as `/std/Flt/reduction`'s `product` and `sum` do.

**Elaboration types one level at a time.** `curios-elab/src/elaborate/match_.rs` reads the scrutinee's type and types each single-level form: `elaborate_induct_match` holds every written arm to a constructor the inductive declares, excuses an absent arm only where `solve_indices` finds a definite clash, and checks each arm's body in a frame that assumes the payload, refines the scrutinee to the case, applies the index solution and re-types the locals it reaches (`assume_payload`, `refine_head`, `retype_locals`), against the result at that case (`MatchResult::at`). What a body may rest on — the case of its scrutinee, a bound a guard decides, its region's monad, the type a numeral is realized at — is what that frame and that result hold.

**A default reached from more than one dispatch is typed away from its match.** `lower_defaulted_matrix` binds it once, as a function of no argument under a `let` whose type is a hole, and calls it from each dispatch. The function is elaborated where the `let` stands, before the arms and in inference, so its body is typed with no result to be checked at. Three programs are refused for it, each accepted where its default is not nested:

```crs
let f(m: Option(Option(Int))) -> Int =
    match m | some(some(x)) => x | _ => 0 end;
-- type mismatch: inferred /std/Nat, expected Int, placed at the match

let g(m: Option(Option(Int))) -> Int =
    choose | some(some(x)) = m => x | _ => 0 end;
-- the same mismatch

let h(m: Option(Option(Nat))) -> Io({}) =
    match m
    | some(some(_)) => print("deep\n")
    | _ =>
        let _ = print("a\n")!;
        print("b\n")
    end;
-- the monad of this region was never determined
```

**A struct pattern's head is never held to its scrutinee's type.** `compile_fields` projects a struct pattern's fields by label and compares its head with the other rows' alone, so `match a | B { x } => x end` is accepted at `a: A` where `A` and `B` each declare an `x`.

**Arm order decides nothing.** [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md) rejects first-match priority, "so arm order never changes meaning and coverage is checked", and [syntax.md](../../syntax.md) states it: concrete constructor rows have no priority, and a final `_` covers the shapes no arm names. It holds today because no two arms can match one value, the final `_` apart.

## Goal

- A binder — a name or `_` — stands beside a concrete pattern in any column, at an inductive and at every carrier, and beside a tuple or struct pattern.
- A value no arm matches is reported as a pattern, `(none(), some(_))`, where today a missing constructor is reported inside a match the compiler synthesized.
- An arm no value reaches is refused: one written after an arm it is a case of, and a final `_` after arms that already cover every case, which today is accepted and never taken.
- The three programs above compile.
- A struct pattern names the structure its scrutinee is.

## Decisions

### Patterns are compiled where their types are known

**Decision.** Lowering hands Core the match as written, in a sixth member of `curios_core::Transient`: the scrutinee, the optional motive, and the rows, each a pattern and a body. A pattern mirrors the surface's — a constructor tag with plicity-marked arguments, a binder, a tuple, a struct, and the carrier leaves — with a tag kept as the atom it is written as. A row's body is an ordinary function over the binders its pattern writes, in the order it writes them, placed where the arm's pattern is written, as a `Bang`'s continuation is an ordinary function: the member's children are terms, and no walk of Core learns a binder from it. The body is lowered once, under its row's names, as a term or as a region as the match's position has it. A final `| _ =>` is the row it is written as, and a `choose` bind arm is the two rows `pattern => body | _ => rest`. Only the elaborator reads the form: it never survives elaboration, and the kernel refuses it as it refuses every `Transient` (`curios_cert::Error::NotCore`).

**Rationale.** Maranget's step for a column that mixes a binder with constructors puts each binder row into every constructor's group and makes the binder rows alone the case for every constructor no row names ("Compiling Pattern Matching to Good Decision Trees", §2.2), and the second half needs the column's signature. Lowering does not have it, and both of its guesses are wrong somewhere: a default it always emits is dead where every constructor is named, and elaboration still checks it, so `(some(x), some(y)) | (_, none()) | (none(), some(y))` lowers a default `match b | none() => 0 end` that is not exhaustive and the match is refused; with none emitted a binder row covers no constructor that lacks a row. A body's region is its own: `Lowerer::region` reads the body and the names bound around it and nothing of the tree, so a body lowered before the tree exists sequences what it sequences today.

**Rejected.** Closing the gap in lowering, by either guess or by skipping a synthesized default no constructor reaches, which stops checking a default a reader wrote after exhaustive arms. Resolving a tag where the pattern is lowered: the scrutinee's type is the elaborator's to find.

### Arm order never decides

**Decision.** Two arms may match one value only where the arm written first is a case of the other: its pattern is an instance of the other's. A value is taken by the first arm that matches it, which under that rule is the most specific one, so reading the arms first to last and reading each arm as its pattern less the cases written before it give one answer. Two arms that match one value, neither a case of the other, are refused, naming both and the value. An arm written after one it is a case of is reached by no value, and is refused as every unreachable arm is. The final `_` is the rule's oldest instance.

**Rationale.** It is the rule the tree states, kept as binders arrive: an accepted match means the same in every order of its arms that is accepted, and a reader who assumes first-match order is never misled, because the general arm stands after its exceptions. What it refuses is exactly a match whose meaning would turn on order: `(true, _) => 1 | (_, true) => 2 | (false, false) => 3` answers `1` at `(true, true)` under first-match order and `2` with its first two arms swapped. Peers that take first-match order document what it costs a proof: Agda's "the first pattern that matches the actual parameters is the one that is used" leaves clauses that "do not hold definitionally" and an `--exact-split` flag to refuse them ([Function Definitions](https://agda.readthedocs.io/en/latest/language/function-definitions.html)), and Cockx, Piessens and Devriese observe that under it "the order of the patterns can matter" and propose clauses that hold in any order ([Overlapping and Order-Independent Patterns](https://link.springer.com/chapter/10.1007%2F978-3-642-54833-8_6), ESOP 2014).

**Rejected.**

- **First-match order for any two arms**, Maranget's textual priority rule. It reverses a rejection the tree records without a reason the goal supplies: every program under *What the code does today* compiles without it. What it buys is one arm for `(_, nan(_))` written after `(nan(_), _)`, where this rule asks for an arm per remaining case of the first column.
- **The most specific arm in any order.** A general arm written first reads as shadowing the arms after it.
- **Arms that agree where they overlap**, which Cockx, Piessens and Devriese check by confluence: it asks conversion to compare two bodies under a unifier, to admit arms a reader can as well write apart.

### A row is typed once, where its own pattern states it

**Decision.** A row's body is elaborated once, in the context of the match with the row's own pattern assumed and nothing else: each constructor or carrier case the row writes is entered as an arm of that case is entered today — the payload assumed and aligned to the binders the row writes (`align`), the scrutinee refined, the indices solved, the locals re-typed — and nothing is assumed of a column where the row writes a binder. The body is checked against the match's result at the row's pattern. So what a row's body may rest on is what the row says, whichever other rows the match holds and in whichever order its columns are split. Rows of one constructor may write different hidden payloads, each aligned by itself. The scrutinee is elaborated once, where the match is entered, and a column is a term with its type, so a row's walk and the tree read one spelling of it. The frames at the match's entry are held (`Context::hold_frames`), and where the tree first reaches a row its body is elaborated standing on them (`Context::standing_on`) with the row's cases entered above, so bodies are elaborated in the order they are today, and a metavariable a body leaves waits on those frames as any other does.

**Rationale.**

- **A body typed where its leaves meet is typed without what its row states.** The default bound once is that body today, and the three programs above are refused for it. Two more shapes are refused the same way when written by hand: `List/drop(xs, k)` bound above the match on `k <= List/len(xs)` it is called under reports that nothing discharged the bound, and an `Eq/refl()` that checks in each arm of a match is a mismatch outside them.
- **A body typed again at every leaf is accepted or refused by the rows beside it.** A column is split because some row names a constructor in it, so a body reached under that split is typed with a case another row caused, and loses it when that row is edited; a later choice of column order would move which programs type. It also elaborates a body once for each leaf, reports one written goal once for each, and needs every hole of the lowered body minted again, since a hole is born once for an id with its context frozen (`elaborate_metavar`).
- **Peers type an alternative at its own patterns.** Lean: "In both subsequent patterns in the same match alternative and the right-hand side's type, occurrences of the discriminant are replaced by the pattern that it was matched against" ([Pattern Matching](https://lean-lang.org/doc/reference/latest/Terms/Pattern-Matching/)). Agda accepts `max m zero = m` with `m` a variable and compiles it to a case tree that splits `m`, which is why "the clause `max m zero = m` does not hold definitionally" ([Function Definitions](https://agda.readthedocs.io/en/latest/language/function-definitions.html)). Rocq expands instead, "into a sequence of match on simple patterns", and its manual says of the result that it "may succeed to build an expression whose sub-expressions are well typed when the whole expression is not", with an error that "makes reference to the expanded expression" ([Extended pattern matching](https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/match.html)).

**Rejected.** A body bound once as a local function and applied from each leaf: Core has no equation outside an arm, so the kernel types such a function without the row's cases, and a function written without its parameters' types is refused, "the type of parameter 'x' was never determined". Elaborating the lowered body at each leaf, for the reasons above.

### A leaf holds the body with the leaf's values in its variables' places

**Decision.** Where the tree reaches a row, what it writes is the row's elaborated body with each binder of the row's own cases replaced by the leaf's (`Term::substitute`): an instance. The elaborator does not type it again. The kernel types it where it stands, as it types every instance of a term it types ([An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)).

**Rationale.** A leaf's frame holds every case the row states and, for a column the row leaves to a binder, a case more; typing is closed under substitution and under a case added, which is the rule that decision holds the kernel to, with the arms no reduction takes excused. An elaborator that typed each instance again would refuse, with the kernel, a program that is well typed, the alternative that decision rejects for a type the elaborator derives.

**Rejected.** A second kind of Core node that shares a body among leaves: the kernel would need a rule for what the body may assume, which is the equation store [A guard is a hypothesis its arm binds](../soundness/201-a-guard-is-a-hypothesis-its-arm-binds.md) proposes to take out of it. What the copies cost in a compiled program is [a body an arm writes once is emitted once](../compilation/209-a-body-an-arm-writes-once-is-emitted-once.md)'s.

### The row that cannot fail is where the match falls through

**Decision.** A final row whose pattern holds no refutable pattern — a top-level `_`, and the `| _ => rest` of a bind arm — states no case, so the rule above types it in the match's own context, against the match's result at the scrutinee as it stands (`MatchResult::of`): the rule a default has today. It is elaborated once, after the rows before it: written in place where one leaf reaches it, and where more do, bound once where the match stands, at the function type that result gives, and called from each — what lowering's two rules for sharing a default come to today (`arms_all_flat`, `refutation_count`). The three programs under *What the code does today* compile, a numeral realized at `Int` and a `!` sequenced in its region's monad.

**Rationale.** The row is typed with no case of its own to assume, so the kernel types one binding of it where the match stands, and a ladder of bind arms stays as long as it is written: each arm's rest is one function, where a copy for every refutable pattern of the arm before it would double at each arm. Elaborated after the arms and checked, it is typed as an unnested default is, in checking and in inference alike.

**Rejected.** Keeping the hole for its type, which is what types it in inference. A copy at each leaf, for the ladder.

### The tree is Maranget's, with the signature read off the type

**Decision.** Columns are taken left to right, as today. A column in which every row is a binder is retired. A tuple or struct column is flattened into projections, a binder row taking a binder for each field, and a struct pattern's head is held to the structure its column's type is. Any other column is split by its type, which the elaborator reads as it reads an arm's scrutinee: the rows that name a constructor are grouped under it; a binder row joins every group a row names, with a binder for each payload; and the constructors no row names, where inversion does not rule out every one of them, are one default arm over the binder rows. `Bool` is split into its two cases, `Nat` by induction or by dispatch as its rows write it, a binder row supplying the default a dispatch requires, and a list or a packed sequence into its two cases. A constructor inversion rules out is no group unless a row names it, and an arm written for one is typed as it is today. A column is a fold where a row of its cons group reads its hypothesis, as today, and a written motive attaches to the head's split, as today.

**Rationale.** "The default case is present, if and only if the set { c1, …, cz } is not a signature" (Maranget, §3): the type is what says so, and nothing dead is built. One default for the constructors no row names is today's `| _ =>` at any depth, and it bounds a row's instances by the constructors rows name, where a group for every constructor of a wide inductive would write one for each. Left to right is what [syntax.md](../../syntax.md) states, and a fold's hypothesis means what it means by where its column is split; no row's typing follows the order, so a later choice of column by necessity (Maranget, §7) changes which tree is built and not which programs type.

**Rejected.** A group for every constructor, named or not: a binder row's instances then number the constructors of its column. Maranget's necessity heuristic now: it changes what every match compiles to, for trees nothing has measured as large.

### Every eliminator types its arms through one door

**Decision.** Each eliminator of `curios-elab/src/elaborate/match_.rs` — the nominal one, `Bool`'s, the `Nat` dispatch, and the three folds — is three steps: it resolves the scrutinee and the result, it enters a case, which is the frame an arm is typed in and the expectation it is typed at, and it rebuilds the match from its arms. What an arm's body is comes from whoever asked: a single-level match hands its lowered arm to be checked, and the matrix compiler hands the tree beneath the case. A row's walk enters the row's own cases by the second step alone and builds nothing.

**Rationale.** What an arm assumes is stated once and read three ways, so the tree, a row's context and a match written one level deep cannot come apart. Nothing the compiler keeps while it works — the frames held, the rows elaborated, which rows a leaf took, the cases on the path — passes through Core.

**Rejected.** An arm's body being the matrix left, a transient elaborated again inside the arm: the compiler's state then travels through a table keyed by the matrix. A second statement of an arm's frame for the row's walk.

### Coverage and reachability are read off the tree

**Decision.** A leaf is where no column is left. Its candidates are the rows that reached it, in the order they are written: the first is taken, and it must be an instance of each of the others, or the match is refused naming the two arms and the leaf's value written as a pattern, which is the arm that parts them. A leaf with no candidate is a value no arm matches, and the match is refused with that value written as a pattern, the constructors on its path with `_` wherever no case was read. A row no leaf takes is refused, `duplicate or overlapping match arm`, as a repeated row is today, with the arm before it that covers it named. A redundant arm is an error and no lint.

**Rationale.** The tree visits every case some value has and no other, so both answers are exact at the type, inversion included, where usefulness computed over the matrix alone (Maranget, "Warnings for Pattern Matching", JFP 2007) knows no index. They are the two questions rustc asks of usefulness, "detecting unreachable code (which is useful to the user), and ensuring that matches are exhaustive" ([Pattern and exhaustiveness checking](https://rustc-dev-guide.rust-lang.org/pat-exhaustive-checking.html)). A repeated row is an error today, and [A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md) admits four lints, each decided where names resolve; an arm is found unreachable in elaboration, which has an error for it and no lint channel.

**Rejected.** A second pass computing usefulness beside the tree: a second statement of which cases there are. A lint, which would extend that decision to a finding lowering cannot make.

## What stays

Every Core match form the elaborator writes, and so the kernel, erasure, the continuation stage and the emitter. The rule that a top-level `_` is the only spelling of the row that cannot fail, and that a named final arm is refused. `choose`, and the refinement each of its arms receives. The operands of the `Nat`, `List`, `Bits` and `Bytes` leaves are plain binders: `[a, b]` and `[some(x), ..rest]` are refused by the parser, and neither is this specification's.

## Stages

Each stage is one commit under its own check, and the suite and `cargo xtask workspace lint` hold at every one.

0. **`wonder stage` reprints a library's file at the Core rungs.** The fix [the tools findings](../tools/000-findings.md) state for *No query reprints a library's declaration at a Core rung*, taken first because the checks of stages 1 and 3 read `/std` at `core-elab`. Check: that entry's own. The commit deletes the entry.
1. **The matrix reaches Core, and the elaborator compiles it as lowering did.** The `Transient` member with its printer (`curios-core/src/print.rs`), its surface form for a report (`curios-text/src/from_core.rs`), its archive derive and its place in `Transient::subterms` and `map_subterms`; lowering writes it and the compiler leaves `match_compile.rs`; the matrix compiler, lowering's algorithm over Core's patterns, builds in `curios-elab` the tree lowering built, which is then elaborated as any term is. Three refusals are read off the syntax and stay lowering's, `BindArmIrrefutable`, `MatchNamedCatchAll` and `MatrixMotiveRequiresCtorHead`; the six that are the tree's, `MatrixInconsistentShape`, `MatrixDuplicateRow`, `MatrixIncompleteCarrierMatch`, `MatrixNatDispatchNeedsDefault`, `MatrixMixedNatDispatch` and `MatrixMixedBinGrain`, are raised from `curios-elab` in the words they have; the default bound once is built as it is today. Check: the comparison below holds, and the tests of `curios-text`'s `lower_tests` that pin a matrix refusal pass from `curios/src/tests/matching/matrix_tests.rs`, where a program reaches elaboration, with the tests that file and `curios/src/tests/binders.rs` held unchanged.
2. **The row that cannot fail is typed where the match stands.** Check: the three programs compile and run, a test each in `curios/src/tests/matching/matrix_tests.rs` and `choose_tests.rs`; the comparison admits the type a default bound once is annotated at, and a default one leaf reaches written in place.
3. **A row is typed where its pattern states it, and a leaf holds its instance.** Each eliminator of `match_.rs` becomes its three steps, and the compiler types its arms through them, where stage 1 built a tree for them to type. Every row of a match that compiles today reaches one leaf, whose frame is the row's own, so nothing a program means moves. Check: the comparison holds against stage 2; `curios/src/tests/matching/instance_tests.rs` gains a row whose body holds a match of its own, reached at two leaves.
4. **A binder joins the cases beside it.** The split by type, the default over the constructors no row names, the leaf's rule, the struct head held to its column's type, and the refusals with their reports. Check: a struct pattern naming another structure than its scrutinee's is refused; the six matches under *What the code does today* compile, and a table of inputs holds each to the answer its arms state, folded and executed; `nested_underscore_mixed_with_concrete_stays_inconsistent_shape` becomes a test that the match compiles; a fixture each for a value no arm matches reported as its pattern, an arm after one it is a case of, a final `_` after exhaustive arms, two arms that meet with neither a case of the other, an indexed family where inversion removes a constructor and a binder row is not specialized into it, and a body that rests on a bound its own row's guard decides, reached at two leaves. Every dead `| _ =>` the refusal finds in `/std` is deleted in this commit.
5. **The contract moves.** [syntax.md](../../syntax.md)'s pattern-matching sections state the rules above in place of the column restriction; a design decision under `documentation/design/surface/` records that patterns are compiled where their types are known and that a row is typed where its pattern states it, with the alternatives rejected here; [An arm is checked in a context specialized by index inversion](../../design/theory/an-arm-is-checked-in-a-context-specialized-by-index-inversion.md) keeps its rejection of first-match priority, restated for arms that may now meet.
6. **`/std`'s hand-nested matches are flattened**, `/std/Flt/reduction`'s among them. Check: `curios-std`'s corpus tests, and the lint's certification of the prelude.

## Verification

**The comparison.** Counted. A stage that moves no meaning is held to the stage before it over all of `/std`, three ways. Its elaborated module: `/std` is one unit, so one question reprints all of it, `cargo run --release --package curios -- wonder stage core-elab curios-std/src/lib.crs`, by the compiler built before the stage's commit and by the one built after it, each output split into declarations at the lines that open at the margin, and the declarations compared by name. Its erased forms: the part of the library's stored unit that holds them, the second of the eight `curios_unit::apart_parts` hands out, split into one form for each item by `curios_archive::parted`, each form digested, and the two lists compared as multisets. And its certification, which `curios-prelude`'s build makes of the whole library.

Byte for byte is more than a faithful move keeps. A product's factors are ordered, and a bound's proof is chosen among ties, by a rank that follows how many names a declaration minted before ([the compilation findings](../compilation/000-findings.md), *Which proof the elaborator writes, and which spelling it solves an implicit to*), and a compiler that moves from lowering to elaboration moves that count. So a stage holds where every declaration is the same bytes but those that differ inside a proof or a hidden argument alone — in the order of a commutative operation's operands, or in which lemma proves a bound — where every erased form is the same bytes but one such a hidden argument reaches, and where the library certifies. A stage's check states what else it admits.

**The kernel types what the elaborator did not.** Stage 3 hands the kernel instances the elaborator never typed, so a disagreement between the checkers there is a refusal with no arm of a reader's to place it at. It is held three ways: the whole prelude certifies, the instance tests run, and a fixture of stage 4 puts a row whose body rests on its own guard at two leaves, one under a case the row does not state.

## Completion and retirement

Done when a binder stands beside a concrete pattern in any column, a value no arm matches and an arm no value reaches are each refused with the reports above, a row's body is typed once at its own pattern, the three programs compile, a struct pattern names its scrutinee's structure, and every match that compiled before compiles to the same Core but for the type at a default bound once.

The contract moves to [syntax.md](../../syntax.md) and `curios-elab`'s match documentation, and the rationale and the rejected alternatives to the decisions stage 5 names. Replace the roadmap entry with a checked summary, repoint [A hidden member takes no position](../../design/theory/a-hidden-member-takes-no-position.md)'s link to this file at the decision that replaces it, verify that nothing else references this filename, and delete it.
