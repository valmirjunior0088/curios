//! What a tower costs to compile.
//!
//! A tower is a chain of `let`s in which each line names the line before it twice: `n` lines are a graph of `n` nodes whose tree has `2ⁿ`. A stage that walks the graph pays a line for a line; one that walks it once per path doubles with every line, and at sixty lines does not answer. Every tower here is an ordinary program that reduces nothing, so what it costs is what the stages other than reduction cost — the half of `documentation/design/soundness/a-reduction-step-costs-what-it-builds.md`'s premise a budget cannot hold, and the subject of `documentation/design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md`.
//!
//! A shape that compiles at sixty lines is a fixture here; `tower_measurements`, among the instrumented tests, counts what every shape costs each checker at two heights. What a tower's type costs to *show* is held here too: its print is its tree, and both a rung and a report end.

use {
    crate::test_support::*, curios_elab::DEFAULT_BUDGET, curios_module::RootSource,
    curios_pipeline::compile_with_prelude, curios_surface::Entrypoint,
};

/// Whether `source` compiles, with what it is refused for where it does not.
fn compiles(source: &str) -> Result<(), String> {
    let entrypoint = source.parse::<Entrypoint>().expect("a tower parses");

    compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
        .map(|_| ())
        .map_err(|error| error.to_string())
}

/// The chain every tower is read against: sixty lines each naming the one before it once, in a function's body and in a recursive member's.
#[test]
fn a_sixty_line_chain_compiles() {
    for stands in [Stands::Function, Stands::Member] {
        assert_eq!(compiles(&tower(Shape::Chain(stands), 60)), Ok(()));
    }
}

/// The towers both checkers walk in their size, at sixty lines: calls, pairs, lists, arms on a variable and arms on an expression in a function's body and in a recursive member's, lines that each call their member, pairs handed to their member, both aliases, a structure's field type, the two towers claimed equal, and the two chains of calls built apart.
#[test]
fn a_sixty_line_tower_compiles() {
    for shape in [
        Shape::Calls(Stands::Function),
        Shape::Calls(Stands::Member),
        Shape::Pairs(Stands::Function),
        Shape::Pairs(Stands::Member),
        Shape::Lists(Stands::Function),
        Shape::Lists(Stands::Member),
        Shape::Arms(Stands::Function),
        Shape::Arms(Stands::Member),
        Shape::Guards(Stands::Function),
        Shape::Guards(Stands::Member),
        Shape::Recursions,
        Shape::Handed,
        Shape::Aliases,
        Shape::Arrows,
        Shape::Fields,
        Shape::TwoTowers,
        Shape::Apart,
    ] {
        assert_eq!(compiles(&tower(shape, 60)), Ok(()));
    }
}

/// A tower of bounded lines compiles at sixty lines: each line names the one before three times, and the proof bounding the last is checked by folding comparisons over the whole tower, each side read for what it is at least and what bounds it, while the certifier remembers each line's reduct.
///
/// Mutation-checked: with a remembered reduct's hit adding its computation's identities to the counter, the certifier passes its identity space before seventeen lines; with the two `Nat` bound folds reading the tower once per path, every line doubles the time.
#[test]
fn a_sixty_line_bounded_tower_compiles() {
    assert_eq!(compiles(&tower(Shape::Bounded, 60)), Ok(()));
}

/// A tower's `core-elab` rung prints. The type each `let` was elaborated at is a graph the rung prints as its tree — sixty lines of pairs are a type of 2⁶⁰ leaves — and a print bounded by its term's graph shows its beginning and elides the rest.
#[test]
fn a_towers_elaborated_rung_prints() {
    let printed = core_elab(&tower(Shape::Pairs(Stands::Function), 60));

    assert!(printed.contains('…'));
}

/// A mismatch over a tower reports: the type a sixty-line tower of pairs has is shown elided, where its print whole is one no report survives writing.
#[test]
fn a_mismatch_over_a_tower_reports() {
    let mut source = String::from("use /std/{Nat};\n\nlet tower(n: Nat) -> Nat =\n");
    chain(&mut source, "    ", "x", 60, "(n, n)", |before| {
        format!("({before}, {before})")
    });
    source.push_str("    x60;\n\n/std/print(\"ok\\n\")\n");

    let report = compiles(&source).expect_err("a pair is no `Nat`");

    assert!(report.contains('…'), "{report}");
}
