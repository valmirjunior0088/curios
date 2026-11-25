//! What a program that grows costs to answer.
//!
//! A runaway is a short program whose one claim makes a checker do more the longer it is let run: a recursion that takes a step a unit, a pair of calls whose unfolding meets the pair again, a sum that gains a summand or a product a factor at every step, a type that nests a constructor at every step. None is refused for what it is written as. Each is answered by a verdict where a rule decides it, inside a budget its fixture states, and ended by its budget where none does; what a unit of that budget buys is the half of `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`'s premise a count of units cannot show.
//!
//! A runaway that answers inside a stated budget is a fixture here, and so is a proof a rule keeps inside one, stated whole; `runaway_measurements`, among the instrumented tests, counts what every runaway costs at a budget and at twice it.
//!
//! What a budget may never do is decide: the two fixtures at the end are a program refused at every budget, where a clash concluded from an unfinished computation would have excused an arm at an equation a lemma inhabits, and a pair of programs with one edge, where a memo crossing a declaration boundary would have let a neighbour pay for a fold. Both are stated at budgets rather than at sizes, so they read what exhaustion is allowed to conclude rather than what a unit buys.

use crate::test_support::*;

/// What `shape` is refused for under `budget`.
fn refused(budget: u64, shape: Runaway) -> String {
    typecheck_within(budget, &runaway(shape)).expect_err("the runaway's claim is not accepted")
}

/// A recursion at a literal no budget reaches is ended by its budget, and says so.
#[test]
fn a_recursion_no_budget_reaches_is_ended_by_its_budget() {
    let report = refused(25_000, Runaway::Spin);

    assert!(report.contains("budget spent"), "{report}");
}

/// A pair of calls whose unfolding meets the pair again is told apart and not left to its budget, whatever the calls build on the way: a sum one summand longer under a binder, a sum built apart from what they differ in, a sum of calls under no binder, and nothing at all.
#[test]
fn a_pair_of_calls_met_again_is_told_apart_inside_a_small_budget() {
    for shape in [
        Runaway::Sums,
        Runaway::SumsApart,
        Runaway::SumsOfCalls,
        Runaway::Halvings,
    ] {
        let report = refused(25_000, shape);

        assert!(report.contains("type mismatch"), "{report}");
    }
}

/// Two recursions of one shape, each of whose unfoldings is the other's one step on, are one, and both checkers hold them so inside a small budget.
#[test]
fn two_recursions_of_one_shape_are_one_inside_a_small_budget() {
    assert_eq!(typecheck_within(25_000, &runaway(Runaway::Rounds)), Ok(()));
}

/// A sum that gains a call at every step, whose atoms each fold classes again, is told apart from what it is claimed equal to inside half a million units.
#[test]
fn a_sum_classed_at_every_step_is_told_apart_inside_its_budget() {
    let report = refused(500_000, Runaway::Classes(62));

    assert!(report.contains("type mismatch"), "{report}");
}

/// A product that gains a factor at every step is told apart from what it is claimed equal to inside two million units: a square of its steps, where a fold charged by its monomials alone spent a multiple of them and built the square for nothing.
#[test]
fn a_product_that_gains_a_factor_a_step_is_told_apart_inside_its_budget() {
    let report = refused(2_000_000, Runaway::Powers(250));

    assert!(report.contains("type mismatch"), "{report}");
}

/// A power widened and compared at every step is reduced to its value inside two and a half million units, each step paying for the factors of one image and not for their square.
#[test]
fn a_power_widened_at_every_step_is_reduced_inside_its_budget() {
    let report = refused(2_500_000, Runaway::Images(250, 50));

    assert!(report.contains("type mismatch"), "{report}");
}

/// One term built by two recursions, the second opened alone a step at a time, is held one by both checkers inside four hundred thousand units. The certifier nests a comparison at every step it opens, so what holds its depth is what each level costs it to reach, and no charge of its own.
#[test]
fn a_side_opened_alone_a_step_at_a_time_is_held_inside_its_budget() {
    assert_eq!(
        typecheck_within(400_000, &runaway(Runaway::Stacks(500))),
        Ok(())
    );
}

/// A recursion whose last step applies a variable is held two thousand steps deep inside three hundred thousand units, as the same recursion ending at its argument is: forcing it keeps the application it ends at, where keeping the call made conversion run what was left of the recursion again at every step it opened.
#[test]
fn a_recursion_that_ends_at_an_applied_variable_is_held_inside_its_budget() {
    assert_eq!(
        typecheck_within(300_000, &runaway(Runaway::Walks(2000))),
        Ok(())
    );
}

/// Absorption over two hundred and fifty-six atoms is decided inside a million units, in both checkers: a decision diagram reads a chain of conjunctions in the square of its atoms, where a table over them would double with every one.
#[test]
fn a_comparison_over_many_atoms_is_decided_inside_its_budget() {
    assert_eq!(
        typecheck_within(1_000_000, &runaway(Runaway::Absorption(256))),
        Ok(())
    );
}

/// Sixteen pairs read from the side that meets every `xi` before any `yi` are one value at every budget that can decide them and at none that cannot: refused for a million units, the budget spent deciding them, and held inside four million, a larger budget deciding what a smaller one could not. The same pair posed from the other side is held inside four hundred thousand: the order a diagram reads its atoms in moves what a comparison costs and never what it answers.
#[test]
fn an_order_that_costs_more_is_decided_by_a_larger_budget() {
    let report = refused(1_000_000, Runaway::Pairs(16));
    assert!(
        report.contains("budget spent") && report.contains("deciding two Bool terms"),
        "{report}"
    );
    assert_eq!(
        typecheck_within(4_000_000, &runaway(Runaway::Pairs(16))),
        Ok(())
    );
    assert_eq!(
        typecheck_within(400_000, &runaway(Runaway::PairsTurned(16))),
        Ok(())
    );
}

/// Two types nested alike five hundred deep and apart at their core are told apart inside a million units.
#[test]
fn two_types_nested_alike_are_told_apart_inside_their_budget() {
    let report = refused(1_000_000, Runaway::Nests(500));

    assert!(report.contains("type mismatch"), "{report}");
}

/// A proof whose guards meet an arm on an expression at every step: `/std`'s former `Int/DivMod/unique`, which splits an integer by its sign and bounds both splits through the elaborator's procedure, every bound a match on a comparison inside the arms of the ones before it.
const A_PROOF_WHOSE_GUARDS_EVERY_ARM_SETTLES_AGAIN: &str = r#"
    use /std/{Nat, Int, Eq, proved, Bool, Io};
    use /std/ops/{Eql};

    struct DivMod(n: Int, d: Nat): Type {
        quotient: Int,
        remainder: Nat,
        joined: Eq()(n, quotient * Nat/to_int(d) + Nat/to_int(remainder)),
        bounded: Bool/Holds(remainder < d),
    }

    let of(n: Int, d: Nat, @ok: Bool/Holds(0 < d)) -> DivMod(n, d) =
        match Int/view(n)
        | nonneg(m) =>
            DivMod {
                quotient = Nat/to_int(m / d),
                remainder = m % d,
                joined =
                    Eql/eq(
                        Nat/to_int(m),
                        Nat/to_int(m / d) * Nat/to_int(d) + Nat/to_int(m % d),
                        proved()),
                bounded = Bool/True/qed() }
        | neg(m) =>
            DivMod {
                quotient = -1 - Nat/to_int(m / d),
                remainder = d - 1 - m % d,
                joined =
                    Eql/eq(
                        -1 - Nat/to_int(m),
                        (-1 - Nat/to_int(m / d)) * Nat/to_int(d) + Nat/to_int(d - 1 - m % d),
                        proved()),
                bounded = proved() }
        end;

    let unique(
        n: Int,
        d: Nat,
        q: Int,
        r: Nat,
        @ok: Bool/Holds(0 < d),
        joined: Eq()(n, q * Nat/to_int(d) + Nat/to_int(r)),
        bounded: Bool/Holds(r < d),
    ) -> {quotient: Eq()(q, of(n, d).quotient), remainder: Eq()(r, of(n, d).remainder)} =
        let split = of(n, d);
        let same: Bool/Holds(
            q * Nat/to_int(d) + Nat/to_int(r)
                == split.quotient * Nat/to_int(d) + Nat/to_int(split.remainder)) = proved();
        let quotient: Eq()(q, split.quotient) = Eql/eq(q, split.quotient, proved());
        let rest: Bool/Holds(
            split.quotient * Nat/to_int(d) + Nat/to_int(r)
                == split.quotient * Nat/to_int(d) + Nat/to_int(split.remainder)) =
            Eq/subst(
                (x: Int) =>
                    Bool/Holds(
                        x * Nat/to_int(d) + Nat/to_int(r)
                            == split.quotient * Nat/to_int(d) + Nat/to_int(split.remainder)),
                quotient,
                same);
        (quotient = quotient, remainder = Eql/eq(r, split.remainder, proved()));

    Io/pure(())
    "#;

/// A proof whose guards meet an arm on an expression at every step answers inside a million units, in both checkers: under each arm every guard in force is settled again from its own spelling, a walk of the term its key already reduced to, where settled again from its key each guard would unfold again at every arm inside it.
#[test]
fn a_proof_whose_guards_every_arm_settles_again_answers_inside_its_budget() {
    assert_eq!(
        typecheck_within(1_000_000, A_PROOF_WHOSE_GUARDS_EVERY_ARM_SETTLES_AGAIN),
        Ok(())
    );
}

/// A clash the budget cannot finish is not concluded, at any budget. `countdown(2000)` is `0`, so `Eq()(countdown(2000), 0)` is inhabited — and `zero` proves it by induction without running the recursion, a cheap proof of an expensive equation. The arm of `match h end` may be left out only where the two forcings of `refl`'s binder definitely clash; they never do, because the two sides are one number. A budget that cannot reduce the recursion leaves them undecided and one that can holds them one, so the program is refused at every budget, below for exhaustion and above for the arm.
///
/// What an acceptance between the two would be: the arm excused at an equation `zero(2000)` inhabits, and `_absurd(zero(2000))` a closed inhabitant of `False`. The three budgets are the three refusals the sweep finds — reduction out of steps, conversion out of steps, and the arm demanded — so an acceptance window opening anywhere between them fails here.
const A_CLASH_OVER_AN_EQUATION_A_LEMMA_INHABITS: &str = r#"
    use /std/{Nat, Eq, Io};
    use /std/Bool/{False};

    let countdown(n : Nat) -> Nat =
        match n
        | 0 => 0
        | k + 1 => countdown(k)
        end;

    let zero(n : Nat) -> Eq()(countdown(n), 0) =
        match n
        | 0 => Eq/refl()
        | k + 1 => zero(k)
        end;

    let _absurd(h : Eq()(countdown(2000), 0)) -> False =
        match h end;

    Io/pure(())
    "#;

#[test]
fn a_clash_the_budget_cannot_finish_is_not_concluded() {
    for budget in [1_000, 10_000, 100_000] {
        let report = typecheck_within(budget, A_CLASH_OVER_AN_EQUATION_A_LEMMA_INHABITS)
            .expect_err("the budget concluded a clash over two terms it holds equal");

        assert!(
            report.contains("budget spent") || report.contains("no arm matches"),
            "at {budget}: {report}",
        );
    }
}

/// Two independent recursions, reduced in one declaration.
const TWO_RECURSIONS_COLD: &str = r#"
    use /std/{Nat, Eq, Io};

    let f(n : Nat) -> Nat =
        match n
        | 0 => 0
        | k + 1 => f(k)
        end;

    let g(n : Nat) -> Nat =
        match n
        | 0 => 1
        | k + 1 => g(k)
        end;

    let _both() -> Eq()((f(2000), g(2000)), (0, 1)) = Eq/refl();

    Io/pure(())
    "#;

/// The same, under a declaration that has already reduced one of the two.
const TWO_RECURSIONS_WARMED: &str = r#"
    use /std/{Nat, Eq, Io};

    let f(n : Nat) -> Nat =
        match n
        | 0 => 0
        | k + 1 => f(k)
        end;

    let g(n : Nat) -> Nat =
        match n
        | 0 => 1
        | k + 1 => g(k)
        end;

    let _warm() -> Eq()(f(2000), 0) = Eq/refl();

    let _both() -> Eq()((f(2000), g(2000)), (0, 1)) = Eq/refl();

    Io/pure(())
    "#;

/// A declaration's verdict does not follow what was compiled before it. `_both` reduces two recursions no unfolding makes one — `f` answers `0` where `g` answers `1`, so neither side's reduct is the other's — and a declaration that has already reduced `f(2000)` stands before it. Every memo is cleared where the budget is restored, so the second declaration pays for `f(2000)` again and both programs have one edge.
///
/// The margin is what gives it teeth: one recursion alone is answered in well under the lower budget here and the two only above it, so a memo crossing the boundary would buy `_both` the whole of `f(2000)` — most of what the pair costs — and carry the warmed program under the budget it is asserted to be refused inside. Reading `cold` at both budgets is what says the two bracket the edge rather than sitting on one side of it.
#[test]
fn a_declarations_verdict_does_not_follow_a_warm_neighbour() {
    for source in [TWO_RECURSIONS_COLD, TWO_RECURSIONS_WARMED] {
        typecheck_within(170_000, source).expect_err("answered inside a budget below its edge");
        assert_eq!(typecheck_within(240_000, source), Ok(()));
    }
}
