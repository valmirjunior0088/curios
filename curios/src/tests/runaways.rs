//! What a program that grows costs to answer.
//!
//! A runaway is a short program whose one claim makes a checker do more the longer it is let run: a recursion that takes a step a unit, a pair of calls whose unfolding meets the pair again, a sum that gains a summand or a product a factor at every step, a type that nests a constructor at every step. None is refused for what it is written as. Each is answered by a verdict where a rule decides it, inside a budget its fixture states, and ended by its budget where none does; what a unit of that budget buys is the half of `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`'s premise a count of units cannot show.
//!
//! A runaway that answers inside a stated budget is a fixture here; `runaway_measurements`, among the instrumented tests, counts what every one costs at a budget and at twice it.
//!
//! What a budget may never do is decide: the two fixtures at the end are a program refused at every budget, where a clash concluded from an unfinished computation would have excused an arm at an equation a lemma inhabits, and a pair of programs with one edge, where a memo crossing a declaration boundary would have let a neighbour pay for a fold. Both are stated at budgets rather than at sizes, so they read what exhaustion is allowed to conclude rather than what a unit buys.

use {super::typecheck_within, curios_utilities::fill};

/// What a runaway's claim makes a checker do.
#[derive(Clone, Copy)]
pub(super) enum Runaway {
    /// `spin(k)` reduced at a literal no budget reaches: a step a unit, and nothing built.
    Spin,
    /// `f(c + k, k)` against itself at another first argument: unfolded, the two calls meet again under a binder, over a sum one summand longer.
    Sums,
    /// `f(c, a + k, k)` against itself at another first argument: the sum that grows is built apart from the argument the two calls differ in.
    SumsApart,
    /// `g(h, s, c + h(d), s(d), n / 2)` against itself at another third argument: unfolded, the two calls meet again under no binder, over a sum that has gained a call.
    SumsOfCalls,
    /// `g(c, n / 2)` against itself at another first argument: unfolded, the two calls meet again with nothing built.
    Halvings,
    /// `f(k) + 2` against `2 + h(k)`: two recursions of one shape, each of whose unfoldings is the other's one step on.
    Rounds,
    /// `acc(h, k, c + h(k))` reduced at so many steps: a sum that gains a call a step, whose atoms each fold classes again.
    Classes(u32),
    /// `pw(k, c * y, y)` reduced at so many steps: a product that gains a factor a step.
    Powers(u32),
    /// A power of so many factors widened and compared at each of so many steps: an image written, and read back, a step.
    Images(u32, u32),
    /// `nest(k, T)` at so many steps, over two types: two types nested alike and apart at their core.
    Nests(u32),
    /// `g(tower(g, k))` against `stack(g, k, g(top))` at so many steps: one term built by two recursions, the second of which a checker opens alone a step at a time, a comparison nested in the last at each.
    Stacks(u32),
    /// `walk(g, k, top)` at so many steps, against `g(top)`: a recursion whose last step applies a variable.
    Walks(u32),
}

const POWER: &str =
    "let pw(n: Nat, c: Nat, y: Nat) -> Nat = match n | 0 => c | k + 1 => pw(k, c * y, y) end;";

/// A runaway as a whole program: its definitions, its one claim closed by `Eq/refl()`, and an entrypoint that reads none of them.
pub(super) fn runaway(shape: Runaway) -> String {
    // What reads the claim after it, where a row measures what the kernel made of it: the kernel files what a judgment cost it where it begins the next.
    let mut reader = String::new();
    let (imports, definitions, claim) = match shape {
        Runaway::Spin => (
            "Nat, Eq, Io",
            "let spin(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => spin(k) end;".to_string(),
            "let claim: Eq()(spin(100000000), 0)".to_string(),
        ),
        Runaway::Sums => (
            "Nat, Eq, Io",
            "let f(c: Nat, n: Nat) -> Nat = match n | 0 => 0 | k + 1 => f(c + k, k) end;"
                .to_string(),
            "let claim(x: Nat, y: Nat) -> Eq()(f(x, y), f(2, y))".to_string(),
        ),
        Runaway::SumsApart => (
            "Nat, Eq, Io",
            "let f(c: Nat, a: Nat, n: Nat) -> Nat = match n | 0 => 0 | k + 1 => f(c, a + k, k) end;"
                .to_string(),
            "let claim(x: Nat, y: Nat) -> Eq()(f(x, 0, y), f(2, 0, y))".to_string(),
        ),
        Runaway::SumsOfCalls => (
            "Nat, Eq, Io",
            "let g(h: (Nat) -> Nat, s: (Nat) -> Nat, c: Nat, d: Nat, n: Nat) -> Nat =\n    match n == 0 | true => 0 | false => g(h, s, c + h(d), s(d), n / 2) end;"
                .to_string(),
            "let claim(h: (Nat) -> Nat, s: (Nat) -> Nat, x: Nat, d: Nat, y: Nat)\n    -> Eq()(g(h, s, x, d, y), g(h, s, 2, d, y))"
                .to_string(),
        ),
        Runaway::Halvings => (
            "Nat, Eq, Io",
            "let g(c: Nat, n: Nat) -> Nat = match n == 0 | true => 0 | false => g(c, n / 2) end;"
                .to_string(),
            "let claim(x: Nat, y: Nat) -> Eq()(g(x, y), g(2, y))".to_string(),
        ),
        Runaway::Rounds => (
            "Nat, Eq, Io",
            "let f(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => f(k) + 2 end;\n\nlet h(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => 2 + h(k) end;"
                .to_string(),
            "let claim(n: Nat) -> Eq()(f(n), h(n))".to_string(),
        ),
        Runaway::Classes(steps) => (
            "Nat, Eq, Io",
            "let acc(h: (Nat) -> Nat, n: Nat, c: Nat) -> Nat = match n | 0 => c | k + 1 => acc(h, k, c + h(k)) end;"
                .to_string(),
            format!("let claim(h: (Nat) -> Nat, x: Nat) -> Eq()(acc(h, {steps}, x), x)"),
        ),
        Runaway::Powers(steps) => (
            "Nat, Eq, Io",
            POWER.to_string(),
            format!("let claim(a: Nat, b: Nat, y: Nat) -> Eq()(pw({steps}, a + b, y), a)"),
        ),
        Runaway::Images(factors, steps) => (
            "Nat, Eq, Io",
            format!(
                "{POWER}\n\nlet widen(n: Nat, c: Nat) -> Nat =\n    match n | 0 => 0 | k + 1 => match Nat/to_int(c + k) < +0 | true => 1 | false => widen(k, c) end end;"
            ),
            format!("let claim(y: Nat) -> Eq()(widen({steps}, pw({factors}, y, y)), 1)"),
        ),
        Runaway::Nests(steps) => (
            "Nat, Int, Option, Eq, Io",
            "let nest(n: Nat, T: Type) -> Type = match n | 0 => T | k + 1 => Option(nest(k, T)) end;"
                .to_string(),
            format!("let claim: Eq()(nest({steps}, Nat), nest({steps}, Int))"),
        ),
        Runaway::Stacks(steps) => {
            reader.push_str(
                "let read(g: (Nat) -> Nat) -> Nat = match claim(g) | refl => 0 end;\n\n",
            );
            (
                "Nat, Eq, Io",
                "let tower(g: (Nat) -> Nat, n: Nat) -> Nat = match n | 0 => 0 | k + 1 => g(tower(g, k)) end;\n\nlet stack(g: (Nat) -> Nat, n: Nat, top: Nat) -> Nat = match n | 0 => top | k + 1 => stack(g, k, g(top)) end;"
                    .to_string(),
                format!("let claim(g: (Nat) -> Nat) -> Eq()(tower(g, {steps}), stack(g, {steps}, 0))"),
            )
        }
        Runaway::Walks(steps) => (
            "Nat, Eq, Io",
            "let walk(g: (Nat) -> Nat, n: Nat, top: Nat) -> Nat = match n | 0 => g(top) | k + 1 => walk(g, k, top) end;"
                .to_string(),
            format!("let claim(g: (Nat) -> Nat, x: Nat) -> Eq()(walk(g, {steps}, x), g(x))"),
        ),
    };

    fill!(
        "use /std/{${imports}};\n\n${definitions}\n\n${claim} = Eq/refl();\n\n${reader}Io/pure(())\n",
        imports,
        definitions,
        claim,
        reader
    )
}

/// What `shape` is refused for under `budget`.
fn refused(budget: u64, shape: Runaway) -> String {
    typecheck_within(budget, &runaway(shape)).expect_err("the runaway's claim is not accepted")
}

/// A recursion at a literal no budget reaches is ended by its budget, and says so.
#[test]
fn a_recursion_no_budget_reaches_is_ended_by_its_budget() {
    let report = refused(25_000, Runaway::Spin);

    assert!(report.contains("ran out of steps"), "{report}");
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

/// One term built by two recursions, the second opened alone a step at a time, is held one by both checkers inside four hundred thousand units. The kernel nests a comparison at every step it opens, so what holds its depth is what each level costs it to reach, and no charge of its own.
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

/// Two types nested alike five hundred deep and apart at their core are told apart inside a million units.
#[test]
fn two_types_nested_alike_are_told_apart_inside_their_budget() {
    let report = refused(1_000_000, Runaway::Nests(500));

    assert!(report.contains("type mismatch"), "{report}");
}

/// A clash the budget cannot finish is not concluded, at any budget. `countdown(2000)` is `0`, so `Eq()(countdown(2000), 0)` is inhabited — and `zero` proves it by induction without running the fold, a cheap proof of an expensive equation. The arm of `match h end` may be left out only where the two forcings of `refl`'s binder definitely clash; they never do, because the two sides are one number. A budget that cannot reduce the fold leaves them undecided and one that can holds them one, so the program is refused at every budget, below for exhaustion and above for the arm.
///
/// What an acceptance between the two would be: the arm excused at an equation `zero(2000)` inhabits, and `_absurd(zero(2000))` a closed inhabitant of `False`. The three budgets are the three refusals the sweep finds — reduction out of steps, conversion out of steps, and the arm demanded — so an acceptance window opening anywhere between them fails here.
const A_CLASH_OVER_AN_EQUATION_A_LEMMA_INHABITS: &str = r#"
    use /std/{Nat, Eq, Io};
    use /std/Bool/{False};

    let countdown(n : Nat) -> Nat =
        match n : (_) => Nat
        | 0 => 0
        | k + 1; ih => ih
        end;

    let zero(n : Nat) -> Eq()(countdown(n), 0) =
        match n : (m) => Eq()(countdown(m), 0)
        | 0 => Eq/refl()
        | k + 1; ih => ih
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
            report.contains("ran out of steps") || report.contains("no arm matches"),
            "at {budget}: {report}",
        );
    }
}

/// Two independent folds, reduced in one declaration.
const TWO_FOLDS_COLD: &str = r#"
    use /std/{Nat, Eq, Io};

    let f(n : Nat) -> Nat =
        match n : (_) => Nat
        | 0 => 0
        | k + 1; ih => ih
        end;

    let g(n : Nat) -> Nat =
        match n : (_) => Nat
        | 0 => 1
        | k + 1; ih => ih
        end;

    let _both() -> Eq()((f(2000), g(2000)), (0, 1)) = Eq/refl();

    Io/pure(())
    "#;

/// The same, under a declaration that has already reduced one of the two.
const TWO_FOLDS_WARMED: &str = r#"
    use /std/{Nat, Eq, Io};

    let f(n : Nat) -> Nat =
        match n : (_) => Nat
        | 0 => 0
        | k + 1; ih => ih
        end;

    let g(n : Nat) -> Nat =
        match n : (_) => Nat
        | 0 => 1
        | k + 1; ih => ih
        end;

    let _warm() -> Eq()(f(2000), 0) = Eq/refl();

    let _both() -> Eq()((f(2000), g(2000)), (0, 1)) = Eq/refl();

    Io/pure(())
    "#;

/// A declaration's verdict does not follow what was compiled before it. `_both` reduces two folds no unfolding makes one — `f` answers `0` where `g` answers `1`, so neither side's reduct is the other's — and a declaration that has already reduced `f(2000)` stands before it. Every memo is cleared where the budget is restored, so the second declaration pays for `f(2000)` again and both programs have one edge.
///
/// The margin is what gives it teeth: one fold alone is answered in well under the lower budget here and the two only above it, so a memo crossing the boundary would buy `_both` the whole of `f(2000)` — most of what the pair costs — and carry the warmed program under the budget it is asserted to be refused inside. Reading `cold` at both budgets is what says the two bracket the edge rather than sitting on one side of it.
#[test]
fn a_declarations_verdict_does_not_follow_a_warm_neighbour() {
    for source in [TWO_FOLDS_COLD, TWO_FOLDS_WARMED] {
        typecheck_within(70_000, source).expect_err("answered inside a budget below its edge");
        assert_eq!(typecheck_within(90_000, source), Ok(()));
    }
}
