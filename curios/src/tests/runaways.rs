//! What a program that grows costs to answer.
//!
//! A runaway is a short program whose one claim makes a checker do more the longer it is let run: a recursion that takes a step a unit, a pair of calls whose unfolding meets the pair again, a sum that gains a summand or a product a factor at every step, a type that nests a constructor at every step. None is refused for what it is written as. Each is answered by a verdict where a rule decides it, inside a budget its fixture states, and ended by its budget where none does; what a unit of that budget buys is the half of `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`'s premise a count of units cannot show.
//!
//! A runaway that answers inside a stated budget is a fixture here; `runaway_measurements`, among the instrumented tests, counts what every one costs at a budget and at twice it.

use super::typecheck_within;

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
}

const POWER: &str =
    "let pw(n: Nat, c: Nat, y: Nat) -> Nat = match n | 0 => c | k + 1 => pw(k, c * y, y) end;";

/// A runaway as a whole program: its definitions, its one claim closed by `Eq/refl()`, and an entrypoint that reads none of them.
pub(super) fn runaway(shape: Runaway) -> String {
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
    };

    format!("use /std/{{{imports}}};\n\n{definitions}\n\n{claim} = Eq/refl();\n\nIo/pure(())\n")
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

/// Two types nested alike five hundred deep and apart at their core are told apart inside a million units.
#[test]
fn two_types_nested_alike_are_told_apart_inside_their_budget() {
    let report = refused(1_000_000, Runaway::Nests(500));

    assert!(report.contains("type mismatch"), "{report}");
}
