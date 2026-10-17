# A classing reads an atom as its reader meets it

[A term is one where conversion says so](../../design/arithmetic/a-term-is-one-where-conversion-says-so.md) decides that no verdict follows how a term is spelled, and holds a classing asked for inside a classing to atoms that stand inside the two being compared. An atom is handed out in weak-head form, so a call the walk unfolds stands inside nothing that was written, and two spellings of one value under such a call are two atoms wherever a classing is asking. The nesting rule predates [Conversion recurrence](../../design/soundness/conversion-recurrence.md)'s pair rule, which now ends the regress the nesting rule was written against: this specification removes the nesting rule and leaves a classing bounded as every comparison is.

## What is wrong

`Classes::join` (`curios-analysis/src/classify.rs`) classes an atom met while another classing is asking only where it stands inside one of the two atoms asked about (`stands_inside`). The walk forces every atom before handing it out, and a stuck `match` keeps its arms as written, so a call written in an arm is met as its unfolding, which no term asked about holds. Under `let g(n: Nat) -> Nat = match n == 3 | true => 7 | false => n end;` and `let step(m: Nat, mode: Mode) -> Int = let up = match mode | near() => m * m > 5 | far() => false end; Nat/to_int(match up | true => 1 | false => 0 end);`, both checkers refuse `(a, c, mode) => let m = g(a + c); step(m, mode) + step(m + 1, mode)` as the same function with `g(c + a)`, the elaborator saying no operand of one side of a commutative operation is an operand of the other: the two `step` calls are classed, and inside their comparison the two unfoldings of `g` stand inside neither and are passed over. With `g` a parameter, which does not unfold, both hold the two one. A family's name written and the type an implicit is solved to are another pair of spellings that meets it, `step(weigh(List/map(@_, @T, ps, f)), mode)` beside `step(weigh(List/map(ps, f)), mode)`, `weigh` folding the list; and so is `k(k(a + b, s) * k(c + d, s), s) * k(k(c + d, s) * k(a + b, s), s)` against its commutation over `let k(n: Nat, s: Bool) -> Nat = match s | true => n + 1 | false => 0 end;`, refused at every depth from two.

**The rule makes a verdict follow its path, and the kernel files it as the pair's.** A comparison made while a classing asks is restricted by what that classing asked, and the kernel's verdicts are remembered by the type and the two sides alone (`Kernel::convert_store`), so a refusal reached inside a classing answers the same pair outside it. `step(g(a + c), mode)` against `step(g(c + a), mode)` is held one by the elaborator, which files no answer held inside another classing's pair (`Context::asked_of`), and refused by the kernel: the congruence under `Nat/to_int` compares the two stuck `match`es again outside the classing and meets the refusal filed inside it. A memo changes only resource verdicts ([`curios-cert`'s README](../../../curios-cert/README.md#the-kernel-memoizes-its-own-evaluation-and-a-memo-changes-only-resource-verdicts)), and this one changes a semantic verdict. A reduct a judgment's reduction takes inside a classing is filed the same way in the kernel's tables for terms that name a local (`refolded`, `curios-cert/src/kernel/whnf.rs`), read and not probed. And a question reduction asks inside a classing's comparison takes that classing's restriction with it (each checker's `asked_classes`), where a question is a comparison of its own that takes nothing from the goal reduction was serving.

**The four `let`s of `UNCERTIFIED` are this** (`curios-pipeline/src/standard/tests.rs`). `signals/scaled_prod_sum` written again with `List/map`'s result type `@reduction/Term` given is refused by the kernel beside its source, a classing's comparison ending on an operation left unpaired after dozens of atoms passed over.

Each program probed on top of `34f5e7600`.

## What the nesting rule was for

It landed at `994a93235` against a regress: two calls of a recursive function that differ in an argument unfold to two stuck `match`es whose arms hold the same two calls one step on, and comparing one pair classed the next until the stack ran out. `13ce06afa` then gave conversion its own end for that regress, keyed by the heads and not by a spelling: a pair of calls is unfolded against itself once, and met again inside that unfolding is apart. A classing compares inside the pairs of the goal it belongs to in both checkers (`classes`, each checker's `convert/intrinsic.rs`), and both checkers' forcing keeps a recursive call folded where its unfolding is stuck and still names its group (`force`, `force_rec`), so the calls a classing meets one step on reach `unfolded_retry` and the pair rule as any two folded calls do. The decision's reasons were written before the pair rule: that nothing ends the nesting but the stack, and, against leaving apart two calls of a function whose calls are being compared, that an atom reaches a classing forced and names no function, which is false of a recursive call that stays stuck.

## Change

One stage, settled with the user on 2026-10-17.

`Classes::of` and `Classes::join` take no pair asked, and `stands_inside` goes, with the state that carries the pair in each checker (`Kernel::asked` and `Kernel::ask`, `Asking::asked` and `Asking::ask`), the bracket each checker puts around a classing's question, and the elaborator's answer cache's mark of whether a classing asked the pair (`Context::asked_of`, the `bool` of `Caches::same`'s key). A question's verdict is then a function of its pair in both checkers, which is what each checker's tables already assume, so neither needs an exception, and a reduction's question takes nothing from the classing it is asked inside. `UNCERTIFIED` empties.

The decision's bullet holding a classing inside the two atoms being compared, and its rationale that classings nest no deeper than a term does, give way to a classing being bounded as every comparison is, by the pair rule; its rejected entries gain holding a classing inside the two atoms being compared, for the reasons above, and lose leaving apart two calls of a function whose calls are being compared, which the pair rule now does. [Conversion recurrence](../../design/soundness/conversion-recurrence.md) states that the goal's pairs are all that bound a classing nested inside another's question, and its rejected entry for no answer to a pair met again stops leaning on a nested classing leaving two atoms apart.

Shown in a copy of `34f5e7600` with `Classes::join` reading no pair asked: every program above held one by both checkers, the four `UNCERTIFIED` `let`s certified, and `/std` elaborated to the same items, every item's `Debug` digest the same and `certification.rkyv` byte for byte; `cargo xtask workspace test` passed but for the classification test that pins the rule and the two parts of the read-back the four `let`s are read back in.

## Settled against

- **Holding the nesting rule over each atom as written**, the walk handing out beside each atom the term it met before forcing it. It reaches the same programs, and keeps a second end for the regress the pair rule ends, verdicts that follow their path, so both checkers' tables keep an exception, and a second spelling of every atom carried through the shared walk and both checkers.
- **Keying the kernel's verdicts by the pair a classing asks**, as the elaborator keys its answers. It ends the checkers' disagreement and leaves both refusing the programs above.
- **Unfolding one definition at a time in the kernel and trying the spines at each**, as Lean's kernel does (`lazy_delta_reduction`, the arguments of one head compared first and the definition of greater height unfolded otherwise) and Rocq's (`Conv_oracle`). It reaches pairs whose calls meet before either is forced, and none the elaborator refuses too.
- **One spelling of a family where its name is written and where a type is solved for**, as Rocq's `Ind` and Lean's constants name an inductive type. It removes one pair of spellings, and the commuted sums above refuse with no family in them.

## Check

- The programs above as rows of `curios`'s `tests::laws::written`, held by both checkers at every order of their binders swept; `two_calls_of_a_recursive_function_that_differ_are_refused` as it is.
- `UNCERTIFIED` empty, and the read-back passing.
- `certification.rkyv` unchanged.
- `std_identity_census`, which elaborates `/std` from two floors of its binder counters, since the classings this asks for open binders.
- `runaway_measurements`, taken again, a rule by which conversion ends a comparison having changed.
- Figures the change may move, not taken again: the budget `/std` consumes, and [A stuck fold keeps what its walk found](200-a-stuck-fold-keeps-what-its-walk-found.md)'s timed figures. The questions classings put over `/std`, counted in that copy with a temporary counter, are about 13,600 in elaboration, 107 of them asked inside a classing, and 1,600 in certification with the rule, and about 19,200, 5,694 inside a classing, and 1,600 without it.

## Retirement

The change cites nothing of this specification once it lands, `UNCERTIFIED`'s comment going with the list, so it is deleted then.
