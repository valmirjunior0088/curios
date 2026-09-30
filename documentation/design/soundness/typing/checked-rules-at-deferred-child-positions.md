# Checked rules at deferred child positions

**Assumes.** An argument, a payload component and a record field reach the checked rules, not only inference and subsumption.

**Status.** **argued**, and held by two fixtures and the mutation that separates them from decoration. `check` dispatches three rules before falling through to infer-then-subsume — let-descent, Π-introduction and Σ-introduction — each because inference at that position manufactures the wrong type: inferring a tuple's components independently yields a non-dependent tuple type whose telescope binds nothing, which no conversion relates to a telescope whose later entries mention its binders. A walk that reaches argument, payload and field positions through inference alone skips all three, in the refusing direction, and invisibly, since the shapes that show it — a dependent tuple or a lambda needing its expectation in argument position — are absent from the prelude.

`a_dependent_tuple_in_argument_position_reaches_the_sigma_rule` applies a function of `((t : Type, x : t)) -> {}` to `(Nat, 7)`, and `a_lambda_in_argument_position_reaches_the_pi_rule` passes `(n) => (Nat, n)` where that pair is the codomain; both refuse with the independent-inference mismatch when the application arm is mutated back to infer-then-subsume. Children are checked by descending into them, so the rules apply because there is no other path.

A child records at both its expected and its inferred type for the erasure obligations — a superset of the positions, which is the safe direction for an obligation that exists to be discharged; no fixture probes the widening.
