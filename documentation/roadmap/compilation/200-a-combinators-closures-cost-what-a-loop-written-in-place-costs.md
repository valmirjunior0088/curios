# A combinator's closures cost what a loop written in place costs

Working specification for compiling a call that passes a function a known function as the code a reader would have written with that function in place: a copy of the callee for that call, in which the call through the closure is a direct call the inliner can take. Its consumers are `/std/Str/fold` under `programs/walk`, `/std/Map/insert`'s key dictionary under `programs/spines`, and `Handle/retried`, a combinator `/std` holds back until its closures cost nothing.

## What this builds on

- **A known function is an atom.** A function named as a value is an `Atom::Fun`, and a call through a parameter becomes a call of a known function once a pass substitutes the atom for the parameter (`rewrite_atoms`, `curios-cps/src/optimize.rs`'s round).
- **A recursive component's invariant argument is substituted in place.** `scc_invariant_knowns` (`curios-cps/src/optimize/specialize.rs`) solves, over every call into a recursive component, which parameters every entry hands one literal or one function, and substitutes them; a function counts only where the member's body may name it (`CallAnalysis::lexical_scope`), since a recursive callee is exactly what the inliner declines to bring into scope.
- **A recursive component is copied per calling context.** `specialize_scc_calls` (`specialize/scc.rs`) groups a component's outside calls by their arguments and copies the component for a context whose known arguments differ from what every caller agrees on, at most `SCC_CLONE_NODE_LIMIT` (256) nodes a copy and `SCC_COPY_NODE_LIMIT` a module; the copy joins the original's `LetFun`, so it shares its lexical scope, and its one context is then substituted. Both read `eligible_sccs`, which admits a component only where it is recursive, no member escapes and none is the entry. The restriction to recursion has no recorded reason.
- **A function that is not recursive is the inliner's.** `inline_known_calls` (`optimize/inline.rs`) copies a callee with one call site of up to 256 nodes into it, and one with several of up to `MULTI_SITE_INLINE_LIMIT`, sixteen, into each, a value whose doc states why it is not tuned to clear one measured callee (`curios-cps/src/optimize.rs`).
- **Specialization is `curios-cps`'s** ([The ANF optimizer is thin](../../../curios-anf/README.md#the-anf-optimizer-is-thin)), and a constant `print`'s description is copied before it, by ANF's closed evaluator under `DESCRIPTION_COPY_NODE_LIMIT` (`curios-anf/src/optimize/evaluate/budget.rs`), as `curios`'s `tests::fmt::a_constant_prints_descriptions_are_copied_within_the_evaluators_cap` holds.

## The gap

**A function that is not recursive, called from several places with a known function, keeps it a closure.** Neither substitution nor copying admits it, since `eligible_sccs` asks for recursion, and the inliner copies it into its callers only below sixteen nodes.

- **`programs/walk`.** `/std/Str/fold` is called by the walk's loop with its step, `/loop/1`, and by `/std/Nat/of_str` with its own, so its loop calls the step through the closure for every character, boxing the character and the accumulator on the way; the step's body alone takes 9 of 65 samples (gdb over the `profile` feature's perf map, at `2c7bbb7d5`). The step is defined inside the loop, just before the call, so a copy beside `Str/fold`'s top-level `LetFun`, where `specialize_scc_calls` puts one, could not name it.
- **`programs/spines`.** `/std/Map/insert` is called at both its sites with one function, `/std/Hash/nat/code`, so every caller agrees, and still `/std/Map/internal/Node/insert`, recursive, receives it through `Map/insert`'s parameter, and its leaf calls the encoding through a closure.
- **`Handle/retried`.** `Handle/wait` states that readiness is advisory, so what waits asks again where the host would still block, and six readings of `/std` write that step as their own `blocked()` arm (`curios-prelude/std/internal/Handle.crs`'s write, flush and finish loops, `tcp/Listener.crs`, `tcp/Socket.crs`, `Proc/Child.crs`); a reading that forgot it raised `would_block` as `other(0)`, as `Child/wait` did. One combinator, `Handle/retried(h, interest, attempt, status)`, states it once and costs two things the print path relies on: the description a constant `print` copies weighs past the closed evaluator's cap (213 nodes against 160), so no constant `print` folds; and the projection of each reply's status stays an indirect call (`tests::codegen::structural::closure_tests::a_returned_closure_every_caller_applies_is_absorbed`, `loop_tests::trees_hot_arithmetic_has_no_indirect_calls`). Written as a local loop that captures the attempt and the projection, the copy stays under the cap and a constant `print` still does not fold (`tests::fmt::print_constant_args_collapses_at_anf`, `tests::fmt::constant_args_leave_no_formatter_in_cps`), for a reason the closed evaluator holds. The first cost is the closed evaluator's and runs before the lowering into CPS, so no copy this specification makes reaches it.

**Measured**, timed at `2c7bbb7d5`, release, x86-64 Linux, five interleaved rounds each, median and span of the time at N less the time at N = 1, which cancels compiling and starting:

| Program | Variant | Time |
| --- | --- | --- |
| `programs/walk`, N = 1 500 000 | as written | 2.79 s [2.77–2.85] |
| | the fold written in the program as a recursion over the text's bytes, the step in place, no validity witness: a bound | 1.78 s [1.76–1.79] |
| | `MULTI_SITE_INLINE_LIMIT` at 48 | 2.24 s [2.22–2.26] |
| `programs/spines`, N = 500 000, seven rounds | as written | 0.278 s [0.275–0.280] |
| | `MULTI_SITE_INLINE_LIMIT` at 48 | 0.256 s [0.255–0.263] |

**Admitting every non-recursive component to both passes is measured worse.** With `eligible_sccs` admitting a component that is not recursive, `Node/insert` loses the dictionary's parameter and its leaf calls `/std/Hash/nat/from` directly, as the substitution should; but the copier also copies functions for contexts of literals, `programs/walk`'s module grows by about two fifths and `programs/spines`' by a third, walk's step stays a closure (the scope above), and window virtualization no longer reaches walk's fold, which slices a rope a character: 6.01 s [5.84–6.25] against 2.75 s, three rounds, same protocol.

## Prior art

- **Lean 4** specializes a function at a call for an instance argument by default, and, where the declaration is tagged `@[specialize]`, for a function argument its recursive calls hold fixed; a parameter the attribute names is specialized even where recursion does not fix it, and a specialization that does not terminate is stopped by stack depth ([`SpecParamInfo`](https://lean-lang.org/doc/api/Lean/Compiler/LCNF/SpecInfo.html)).
- **OCaml's Flambda** specializes a recursive function at a call where an invariant argument is known, and attempts nothing for a function that is not obviously recursive, which is its inliner's ([Optimisation with Flambda](https://ocaml.org/manual/flambda.html)) — the split this compiler has today.
- **GHC's call-pattern specialisation** copies a recursive function for the constructors a call passes; its source's note on specialising for lambda parameters calls a lambda a sort of constructor for functions, and the code beside it disables lambda specialisation as fragile, its specialisation loop able to run without end ([`SpecConstr`](https://downloads.haskell.org/~ghc/7.4.1/docs/html/libraries/ghc/src/SpecConstr.html)).
- **Lambda set specialization** types each function value by the set of lambdas it may be and monomorphizes over those sets, up to 6.85× under MLton ([Brandon, Driscoll, Dai, Berkow and Milano, PLDI 2023](https://cs.princeton.edu/~mpmilano/publication/lss/)); Rust reaches the same by giving every closure its own type.

## Decisions

- **A call that passes a known function is specialized**, the first of the two designs this specification reserved. Settled on 2026-11-08.
- **The specialization is `curios-cps`'s**, as every specialization is ([The ANF optimizer is thin](../../../curios-anf/README.md#the-anf-optimizer-is-thin)).

## Open questions

- **Which parameter earns a copy.** A function argument the callee's body applies, or hands on to a callee that applies it, as `Map/insert` hands its dictionary to `Node/insert`; an argument the body only stores buys nothing. Recommended: the transitive form, read off the call analysis's sites.
- **Substitution before copying.** Where every call agrees on a function, as `Map/insert`'s two do, substituting it in place copies nothing; the measured experiment shows the copier, not the substitution, is what costs. Recommended: extend `scc_invariant_knowns` to a function that is not recursive, for function atoms alone, as the first stage.
- **Where a copy stands.** A copy whose body names the passed function has to be in that function's scope, so walk's copy of `Str/fold` stands at or below `/loop/1`'s `LetFun`, inside the loop, where a recursive component's copy joins its original's group. Recommended: the copy joins the passed function's `LetFun`, which the call is inside, and the call then calls the copy, its one caller, which the single-site inliner takes.
- **What a copy may spend.** `SCC_CLONE_NODE_LIMIT` and the module's SCC allowance, or an allowance of its own. Recommended: an allowance of its own beside the three the round holds, so the recursive specializer's spending is unchanged, sized by counting what `/std`'s programs spend.
- **A copy that calls its original with another function.** A combinator that recurses through a new lambda at each level would copy without end where nothing bounds it; the allowance does, which is not refunded. Recommended: a context is a callee and its function atoms, and a copy's own calls of its original are not candidates in the sweep that made it.
- **What a constant `print` over `Handle/retried` needs.** Whether the description still weighs past the closed evaluator's cap once the closures are specialized below it, and what keeps the local loop's `print` from folding. Settled by writing `Handle/retried` into the six readings of a scratch copy of the library and reading `heaviest_description_copy` and the two folding tests; where the cap still binds, the description weight the reservation named second is this question's answer, and a stage of its own.

## Stages

1. **A function every call agrees on is substituted into a function that is not recursive.** Check: `programs/spines`' optimized CPS calls `Hash/nat/code` directly from `Node/insert`, held as a codegen test beside `loop_tests`; a specializer unit test of a two-site function both of whose calls pass one function, and one whose calls disagree, which stays as it is.
2. **A call that passes a known function calls a copy.** Check: `programs/walk`'s fold calls its step directly and keeps its window parameters (`walk_mirror_family_isolates_each_obligation`'s per-loop slice claim, restated for the fold itself); a capturing step's copy stands in its scope; a copy past the allowance is declined and nothing is minted; the timings above retaken.
3. **`Handle/retried` replaces the six readings' `blocked()` arms**, once the last open question is settled and its answer has landed. Check: the five tests named above, with `curios-prelude`'s tcp, fs, Tty and Tui tests.

## Verification

`cargo xtask workspace lint` and each stage's tests by name, with the corpus tests that reach `curios-cps`, `curios-prelude`'s, and the timings retaken under the protocol above with the variants interleaved. The full gate runs once, after the last stage.

## Rejected

- **Every non-recursive component admitted to both passes**, measured above: it copies for literal contexts, grows the module and loses window virtualization, and still leaves walk's step a closure.
- **A larger `MULTI_SITE_INLINE_LIMIT`**: at 48, about 6% more code and 0.1 s more compiling for walk; at 160, three fifths more code and two and a half times the compile; and it copies every small function into every caller whether or not a known argument is what the copy buys, which is the reason the limit's doc gives for not tuning it to one callee.
- **An attribute naming what to specialize**, as Lean's and Flambda's: a spelling promising what the analysis should earn, which [A value costs when it is kept, not when it is named](../../design/compilation/a-value-costs-when-it-is-kept-not-when-it-is-named.md) declines for unboxed types.
- **A description's weight as the closures' design**: it reaches the closed evaluator's cap and leaves the call through the closure; it stays the answer the last open question may need for `Handle/retried`'s `print`.
- **Lambda sets typed over the whole program**: the module already sees every call a copy is made for.

## Completion and retirement

Which call earns a copy, where the copy stands and what it may spend go to `curios-cps`'s README as a decision beside [A pass applies the independent plans its snapshot admits](../../../curios-cps/README.md#a-pass-applies-the-independent-plans-its-snapshot-admits), and to `specialize.rs`'s rustdoc; the stages' tests stay. Once those are recorded and no reference to this specification remains, this file is deleted.
