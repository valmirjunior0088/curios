# An atom is ranked from its children's ranks

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a rebuild rebuild a node once per depth. Ranking an atom rebuilds the whole atom, so an atom that holds an atom is rebuilt once for itself and once for every atom around it.

**Not refined yet.** It waits on the design of a rank read off the children's, under *Refinement*.

## What is wrong

`Term::rank` (`curios-core/src/term.rs`) remembers a rank per node, and computes one by `rank::ranked` (`curios-core/src/term/rank.rs`): the term respelled whole, each binder, metavariable and universe or quality metavariable spelled by where a walk first meets it, then hashed, then its identities' ages listed. The spelling is the whole term's, so no rank is read off its children's, and an atom whose operand holds an atom ranks the inner one again inside its own respelling.

Probed at `25125304c`: `g(x_d + g(x_{d-1} + … g(x1 + x0)))` against the same spine over `u0`, claimed by `Eq/refl()` and refused, timed once at each depth with the release compiler, takes 0.10, 0.18, 0.45, 1.50 and 5.37 s at depths 64, 128, 256, 512 and 1 024, a gibibyte at the last; six of nine samples under gdb at 1 024 stand in `ranked` and `respelled`, under `Atoms::keyed` under `Nat::summand`, reached from the conversion chain's `read` and from `Nat::sum` in reduction. Both checkers rank through the one function.

## What is known

- **The order is decided.** [A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) orders an atom by its shape, then by the age of the identities it holds, each identity read by comparison and never by its value, so that no order follows a display built and discarded, a memo's answer or a probe that opened a binder. A rank read off children's must order atoms by the same rule, or restate it there.
- **A rank is compared, never read** (`curios_algebra::Rank`): any keys that order serve.
- **Hashing every subterm modulo renaming is a solved problem.** Maziarz, Ellis, Lawrence, Fitzgibbon and Peyton Jones hash every subterm of a term modulo α-equivalence in O(n log² n), each node's summary its structure and a map from each free variable to the positions it holds, merged smaller into larger ([Hashing Modulo Alpha-Equivalence](https://arxiv.org/pdf/2105.02856), PLDI 2021). Dropping the variables' names from that map leaves this rank's shape: `f(x, y)` and `f(y, z)` alike, `f(x, x)` apart.
- **What the rank costs the library**, which the design is weighed against and which retires nothing, since a program pays the square above whatever `/std` pays. Mixed, at `f0712f8e4`, over `cargo xtask profile-prelude elaboration` and `certification` in a profiling build that carried a scratch copy's counters, each time a median of three readings with its span. Elaboration ranks 77,059 atoms, walking 1,407,217 nodes, counted, in 845.0 ms [842.1–846.0] of `elaborate_and_zonk_with_prelude`'s 60,013 ms [59,935–60,041], about 1.4%, and allocates 794.7 MB of its 25,806.7 MB doing so, counted. Certification ranks 26,165, walking 225,112 nodes, in 222.1 ms [220.5–222.4] of `recheck_module`'s 22,551 ms [22,512–22,709], about 1.0%, and allocates 161.6 MB of its 8,235.0 MB.

## Refinement

- **The design.** A shape composed of the children's summaries, as the paper's; the ages compared only where two shapes tie, by a walk of the two atoms in the order their identities are first met; and the same function under both checkers. The order moves where the hash does, so `/std`'s image moves with it, and the change is read against the law suites (`curios/src/tests/laws/`) and the read-back sweep.
- **The bound and its measurement.** State the bound of the chosen summary and of the age comparisons separately. The cited O(n log² n) algorithm supplies a candidate, not a linear bound: a design that retains its logarithmic factors states them in the owning decision rather than promising a doubling ratio near two. Count summary construction, members merged and nodes visited by age comparisons at depths 256, 512, 1 024, 2 048 and 4 096; the same-shaped comparisons exercise the tie walk as well as different shapes. The normalized counts must meet the chosen bound and expose no repeated whole-subterm respelling. Both checkers' rank tests, the law suites and the read-back sweep hold the resulting order; `profile-prelude` reads the common-case time and allocations against an interleaved baseline.
