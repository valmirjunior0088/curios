# A test is a check that runs, and a proof is a `let`

**Decision.** A `test` declaration is a name and a description: `test foo = Test/assert(2 + 2 == 4);`. It takes no parameters and no parentheses, its body is a `/std/Test`, and its verdict is computed when it runs — `passed`, `failed`, `trapped`, or `exited N`. A claim about *every* instantiation is not a description and nothing that runs decides one: it is a proposition, so it is an ordinary `let` whose type states the claim and whose body proves it, checked by the kernel wherever its unit is checked. `let _right_identity(n: Nat) -> Eq(n + 0, n) = Eq/refl();` — the leading `_` marking a declaration that exists for its type rather than its callers, which is what the unused-declaration lint asks for and what makes the intent legible.

Because `test` is for what cannot be proved, the proof rung leaves `/std/Test` with the parameters that reached it. `Test` is `verdict(Verdict) | action(Io(Test))`; `theorem()`, `Settled`, `prove`, `refl` and `settled` are gone, and `proved` leaves the report vocabulary. `Test/check` is renamed `Test/assert`, which is what a bare boolean reads as.

**Rationale.** Two designs preceded this one, and both were built far enough to judge.

The first removed property-based testing and made a test's parameters mean *theorem*: the elaborator asked its conversion oracle whether the body was `theorem()` under the whole telescope and refused the declaration when it was not. The argument was sound — a `verdict` is a fact about one instance, a telescope quantifies over all of them, and the pair has nothing to report — and it is still the reason parameters cannot mean anything else. What it never asked is whether the surviving form earned its notation. It does not:

```curios
test right_identity(n: Nat) = Test/refl(n + 0, n, Eq/refl());   -- the form it left
let  right_identity(n: Nat) -> Eq(n + 0, n) = Eq/refl();        -- what the language already had
```

The `let` states the proposition in the type, states the proof as the body, and is checked on every build. The `test` buries the proposition in a combinator's argument list, passes the proof positionally, and yields a payload-free tag that is couriered to a runtime printer so a line can be emitted about a fact established at elaboration. Every difference favours the `let` except one: the report line.

The second dispatched on a declared type instead of arity — `test f(n: Nat) -> Eq(n + 0, n) = Eq/refl();` — which is better but is still dispatch. This one has none: `test` means one thing, decided by nothing.

**What `theorem()` was, and why it went.** A nullary constructor with no payload. `prove`, `refl` and `settled` each took a proof, discarded it — the parameter was erased — and returned the same constant, so the proof did its work by *typechecking* and the tag recorded only that this had happened. Being payload-free was correct, since a proof of a decided proposition erases; existing at run time was not. Keeping it would also have kept `Test/refl`, and with it two ways to state a proof — the overlap that made the subsystem feel convoluted in the first place.

**What is lost, stated plainly.** A reported inventory of proofs. `curios test` no longer shows which propositions hold; the build passing is the only signal, and `wonder tests` lists descriptions alone. That is arguably the stronger arrangement — a broken proof fails the build rather than reddening a line — but it is less visible, and it is the one thing a reader will miss.

**The thunk stays in the lowering.** `test foo = e;` still lowers to a definition of type `() -> Test` with body `() => e`. `Test/main` holds every test in one list and forces only the one its index selected, so a bare value would run every test's body on every instantiation. The parentheses spelled that thunk before they left the surface; the lowering keeps it, and `test` is no longer function-definition sugar but its own form that synthesizes one.

**Rejected.** *A second keyword,* `theorem f(x)` beside `test f()`. The theorem/example split does not run along the arity axis — a nullary `Test/refl` was a theorem too — so a second keyword would cut across the grid instead of along it. With proofs living in `let`, there is no split left to name. *Keeping the parentheses empty,* `test foo() = …`. They would spell a telescope the form does not admit, and an empty parameter list that can never be non-empty is a slot inviting the question this decision answers. *A migration refusal* pointing a parameterized `test` at the `let` form: transition scaffolding for sources this change updates in the same commit, where the ordinary `Expected '='` says enough.

**What this does not buy.** A way to check a general claim at instances the author did not write. A `let` returning `Test`, scheduled by a nullary test through `Test/all` over a table, checks the author's own cases and no others.
