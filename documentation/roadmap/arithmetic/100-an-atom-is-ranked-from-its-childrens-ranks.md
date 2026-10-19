# An atom is ranked from its children's ranks

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a rebuild rebuild a node once per depth. Ranking an atom rebuilds the whole atom, so an atom that holds an atom is rebuilt once for itself and once for every atom around it.

**Not refined yet.** It waits on the census under *Refinement*, which says what the rank costs the library, and on the design of a rank read off the children's.

## What is wrong

`Term::rank` (`curios-core/src/term.rs`) remembers a rank per node, and computes one by `rank::ranked` (`curios-core/src/term/rank.rs`): the term respelled whole, each binder, metavariable and universe or quality metavariable spelled by where a walk first meets it, then hashed, then its identities' ages listed. The spelling is the whole term's, so no rank is read off its children's, and an atom whose operand holds an atom ranks the inner one again inside its own respelling.

Probed at `25125304c`: `g(x_d + g(x_{d-1} + … g(x1 + x0)))` against the same spine over `u0`, claimed by `Eq/refl()` and refused, timed once at each depth with the release compiler, takes 0.10, 0.18, 0.45, 1.50 and 5.37 s at depths 64, 128, 256, 512 and 1 024, a gibibyte at the last; six of nine samples under gdb at 1 024 stand in `ranked` and `respelled`, under `Atoms::keyed` under `Nat::summand`, reached from the conversion chain's `read` and from `Nat::sum` in reduction. Both checkers rank through the one function.

## What is known

- **The order is decided.** [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) orders an atom by its shape, then by the age of the identities it holds, each identity read by comparison and never by its value, so that no order follows a display built and discarded, a memo's answer or a probe that opened a binder. A rank read off children's must order atoms by the same rule, or restate it there.
- **A rank is compared, never read** (`curios_algebra::Rank`): any keys that order serve.
- **Hashing every subterm modulo renaming is a solved problem.** Maziarz, Ellis, Lawrence, Fitzgibbon and Peyton Jones hash every subterm of a term modulo α-equivalence in O(n log² n), each node's summary its structure and a map from each free variable to the positions it holds, merged smaller into larger ([Hashing Modulo Alpha-Equivalence](https://arxiv.org/pdf/2105.02856), PLDI 2021). Dropping the variables' names from that map leaves this rank's shape: `f(x, y)` and `f(y, z)` alike, `f(x, x)` apart.

## Refinement

- **The census it waits on.** What ranking costs `/std`: a `sample!` at `ranked`, its terms' sizes and the share of `cargo xtask profile-prelude elaboration` and `certification` it takes. Under a hundredth of either half, this specification goes to the probe above as a growth row and states its cost there; past it, the design below.
- **The design.** A shape composed of the children's summaries, as the paper's; the ages compared only where two shapes tie, by a walk of the two atoms in the order their identities are first met; and the same function under both checkers. The order moves where the hash does, so `/std`'s image moves with it, and the change is read against the law suites (`curios/src/tests/laws/`) and the read-back sweep.
- **Check**: the probe above within a tenth of 2.0 per doubling from depth 256, and both checkers' rank tests.
