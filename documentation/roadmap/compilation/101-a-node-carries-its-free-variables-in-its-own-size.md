# A node carries its free variables in its own size

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a compilation end in time proportional to what was written plus what the budget counted, and a walk pass over a subterm a fact its node carries proves it has nothing to find in. The fact a node carries of its free variables is a set copied from its children, so a term whose every level names one more local costs the square of its depth to know them.

**Not refined yet.** It waits on the census and the choice of representation under *Refinement*.

## What is wrong

`Term::free_vars_shared` and `mentions_free` (`curios-core/src/term.rs`) read a set memoized on each node (`FreeCache`, `curios-core/src/term/frees.rs`), filled bottom-up from the children's (`Subterm::free_vars_from_children`, `curios-core/src/term/subterm.rs`). A node that adds no identity and whose free variables come through one child shares that child's set; a node with two children that carry free variables copies the first child's set and extends it. So `((n + x0) + x1) + … + x{k}`, each level naming one more local, holds sets of one, two and up to k members, each built by copying the one below: k² time and memory.

Probed at `25125304c` with the release compiler, timed once: `n` bindings `let x{i} = a + i;`, then `let g = (n: Nat) => n + x0 + … + x{n-1};` and `g(1)`, takes 0.51 and 1.63 s at 1 000 and 2 000 bindings, where the same bindings summed with no lambda take 0.29, 0.56 and 1.15 s at 1 000, 2 000 and 4 000. Four of twelve samples under gdb at 2 000 stand in `warm_frees` and `free_vars_shared`: the lowering's `collect_let`, the elaborator's `substituted_by_value` twice, and the kernel's `Scope::bind`. The same reading stands under the totality checks (`reach::seeds`, `proof_position`) in the samples of a long sum of distinct calls (`curios/src/tests/laws/nested.rs`'s `called` at 512 summands).

## What is known

- **The readers.** `free_vars_shared` and `free_vars` are read at 73 places in 35 files, across both checkers, the lowering and the shared analyses, most of them asking membership or iterating; `mentions_free` asks membership alone. A set is filled for a minority of nodes on a compilation, the rest reading the packed scalars (`ScalarCache`), whose `newest_local` already lets a capture pass over a subterm.
- **The empty set is shared**, one canonical `Rc`, and a chain that adds nothing shares its child's: the copy is paid only where two children carry free variables.
- **Prior art.** Lean 4 keeps no set on an expression: its `Expr.Data` packs a hash and flags, `hasFVar` among them, and a walk collects free variables where asked, remembering what it visited. GHC's free-variable annotation keeps a set per node over persistent integer maps, so a set one larger shares the one below.

## Refinement

- **The census it waits on.** How large the sets `/std`'s elaboration and certification fill are, and how many are filled, by a `sample!` at the fill over `cargo xtask profile-prelude elaboration` and `certification`: what a representation costs the common case is the argument for it.
- **The representation**, one of three: a persistent ordered set (`Sorted`, `curios-utilities/src/persistent.rs`) merged smaller into larger, whose node per member is paid by every small set; a set that is a sorted vector while small and persistent past a size; or no set at all, the scalars' flags passing over what holds no local, and a set collected by a remembered walk where a reader needs one, as Lean does.
- **Check**: the probe above within a tenth of 2.0 per doubling, held as a growth row of `curios/src/tests/growth.rs`; `/std`'s image unchanged at one path; `profile-prelude`'s allocations and peak for both halves no worse than before, counted.
