//! What a combinator web costs to compile, held by the suite at the sizes each test states; what it can drive a compilation to spend is measured by the bench (`xbench/src/measurements/unfolding.rs`).

use {
    crate::test_support::*, curios_elab::DEFAULT_BUDGET, curios_module::RootSource,
    curios_pipeline::test_support::*, curios_surface::Entrypoint, std::fmt::Write,
};

/// A web of `rules` conjunctions, each definition naming the one before it twice, claimed `true` at a binder by `Eq/refl()`: a claim that does not hold, since the web is `n % 2 == 0`.
fn conjunctions(rules: usize) -> String {
    let mut source = String::from(
        "use /std/{Nat, Bool, Eq};\n\n\
         let Pred: Type = (x: Nat) -> Bool;\n\
         let both(p: Pred, q: Pred) -> Pred = (x) => p(x) && q(x);\n\
         let base: Pred = (x) => x % 2 == 0;\n\n",
    );
    for rule in 0..rules {
        let previous = match rule {
            0 => "base".to_string(),
            _ => format!("r{}", rule - 1),
        };
        let _ = writeln!(source, "let r{rule}: Pred = both({previous}, {previous});");
    }
    let _ = writeln!(
        source,
        "\nlet probe(n: Nat) -> Eq()(r{}(n), true) = Eq/refl();\n",
        rules - 1
    );
    source.push_str(TAIL);
    source
}

/// [`numerics`]' web against a literal through the comparison fold rather than through `Eq()`: `top(n) < 5` reduces on both sides of a conversion, as a guard's proposition does, and a comparison reads a side against a literal as it stands, so the web is never distributed.
fn numerics_compared(rules: usize) -> String {
    let mut source = numeric_web(rules, "use /std/{Nat};\nuse /std/Bool/{Holds};\n\n");
    source.push_str("let probe(n: Nat, h: Holds(top(n) < 5)) -> Holds(top(n) + 0 < 5) = h;\n\n");
    source.push_str(TAIL);
    source
}

/// **The decision's own probe.** Deciding `Eq()(top(n), 0)` for a symbolic `n` needs the sum's head, not its normal form: a stuck sum whose summands are not literal zero is not zero. Eager folding would build the linear combination first — ~φ²ⁿ monomials, since the web's degree is Fibonacci in its size — and the units would grow with it. The folds answer the weak-head form and the peel clashes from the head, so the units grow with the weak-head DAG, which memoization keeps linear in `n`. The control is the increment: each further definition costs about what the previous one did.
#[test]
fn a_symbolic_web_compares_against_zero_in_linear_units() {
    let units = |rules: usize| {
        let source = numerics(rules);
        let entrypoint = source.parse::<Entrypoint>().expect("the web parses");
        typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
            .expect("the web elaborates within the default budget")
            .consumption
            .units()
    };

    let curve = [6usize, 7, 8, 9].map(units);
    let increments = [
        curve[1] - curve[0],
        curve[2] - curve[1],
        curve[3] - curve[2],
    ];

    // Linear: the last increment is within a small factor of the first. Doubling per definition would put it at four times or more, which is exactly what this refuses.
    assert!(
        increments[2] <= increments[0].saturating_mul(2).max(2_000),
        "units grew superlinearly across the web: {curve:?}, increments {increments:?}"
    );
}

/// The numeric door at ten definitions, where distribution in full doubles the sum's summands per definition. It compiles because a product of two symbolic sums is its own weak-head form, distributed by `Nat::normalize` only where a value is asked for; because a monomial and each of its factors are one node per distinct structure inside a product, so a merge is a pointer test and a fresh spine is never cache-warmed; and because a traversal re-enters `recurse` per level, so capturing the normal form for a conversion goal chains stack segments instead of running one to its guard page. Without them this size runs for minutes, fills gigabytes and faults. Not a measurement — a refusal to regress to not compiling.
#[test]
fn a_ten_definition_numeric_web_compiles() {
    let outcome = compile_only(&numerics(10));

    assert_eq!(outcome, Ok(()), "the ten-definition numeric web compiles");
}

/// The numeric web against a literal through the comparison fold, where [`a_symbolic_web_compares_against_zero_in_linear_units`] takes it through `Eq()`: a comparison reads a side against a literal as it stands, so `top(n) < 5` is left open on the web's weak-head form and its budget is spent on no distribution. Not a measurement — a refusal to regress to running out of steps.
#[test]
fn a_symbolic_web_compared_against_a_literal_elaborates_within_the_budget() {
    let outcome = compile_only(&numerics_compared(10));

    assert_eq!(
        outcome,
        Ok(()),
        "the ten-definition web compared against a literal elaborates"
    );
}

/// The proved door at twenty definitions, each naming the one before it twice: the `&&` and `||` web its comparison against `true` reads is a graph of a few dozen nodes and a tree of millions. It compiles because the decision diagram reads a term once; read once per path, the comparison runs the declaration out of its budget well short of this size. Not a measurement — a refusal to regress to not compiling.
#[test]
fn a_proved_web_of_twenty_definitions_compiles() {
    let outcome = compile_only(&predicates(20, Consumed::Proved, true));

    assert_eq!(
        outcome,
        Ok(()),
        "the proved web of twenty definitions compiles"
    );
}

/// The compared door at twenty definitions: under a guard on the web at `n`, an arm comparing forms of the web at `m` and `k`. Each of its stuck forms names a binder the guard does not where conversion reads it, so the filter in front of the equation puts it to none, and the arm costs what it would under no guard. Not a measurement — a refusal to regress: put to the equation, each form would settle the guard's web and be classed against it, which runs the declaration out of its budget at this size.
#[test]
fn a_guard_over_a_web_puts_forms_over_other_binders_to_no_equation() {
    let outcome = compile_only(&predicates(20, Consumed::Compared, true));

    assert_eq!(
        outcome,
        Ok(()),
        "the compared web of twenty definitions compiles"
    );
}

/// A `&&` web of twenty definitions, each naming the one before it twice, claimed `true`: a graph of a node a definition and a tree of a million paths, which the converters flatten before they peel it against the literal. It is refused for the mismatch it is, inside the default budget, because a connective is flattened once per subterm; flattened once per path, the comparison ran the declaration out of its budget from sixteen definitions on. Not a measurement — a refusal to regress to exhaustion.
#[test]
fn a_conjunction_web_of_twenty_definitions_is_refused_as_a_mismatch() {
    let outcome = compile_only(&conjunctions(20));
    let refusal = outcome.expect_err("the web is no tautology");

    assert!(
        refusal.contains("type mismatch") && !refusal.contains("ran out of steps"),
        "the web is refused for its budget rather than its mismatch:\n{refusal}"
    );
}

/// **The guard [`combinator_sharing_measurements`] cannot be**, because a probe is ignored and nothing runs it.
///
/// What it holds is the growth *law* rather than a number: a combinator application written inside a `!` continuation must cost what the identical application written as a top-level item costs — and what not writing it at all costs, since a shared residual group is bound once and reused. The spellings denote the same grammar, and a group bound inside the candidate's block instead would make the first differ from the others by an order of magnitude at this size — `n²` copies against `n`.
///
/// **The baseline is measured, never written down.** A count here is the grammar's `n` plus whatever `/std/Parse` and its own users spell, and that second term is no part of this claim. A literal baseline fails a test about sharing as the standard library grows, for a reason that has nothing to do with sharing. [`Inner::None`] *is* that term, taken at the same size and in the same run, so the library may grow — it moves all three counts together — while a spelling that stopped sharing still stands out at once: quadratic is 66 against 23 at eight rules.
///
/// Eight rules rather than sixteen because this one is not ignored.
#[test]
fn an_application_inside_a_continuation_is_shared_like_one_at_item_level() {
    let absent = copies(&anf_optm(&grammar(8, Inner::None)), SEQUENCED);
    let inside = copies(&anf_optm(&grammar(8, Inner::InBlock)), SEQUENCED);
    let hoisted = copies(&anf_optm(&grammar(8, Inner::Hoisted)), SEQUENCED);

    assert!(
        absent > 0,
        "the instrument counts nothing: no emitted function is named for `{SEQUENCED}`, so the two laws below would hold between zeroes"
    );
    assert_eq!(
        inside, hoisted,
        "where a combinator application is written must not decide what it costs"
    );
    assert_eq!(
        inside, absent,
        "a shared application must add no copy over not writing it at all"
    );
}
