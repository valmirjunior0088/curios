//! The grid of bounds proved from the facts in scope: omitted bounds the elaborator must fill with a proof both checkers recheck, and controls it must refuse.
//!
//! **A row is a bound and the facts it follows from.** A claim row states a proposition over its binders and asks for it twice: through `proved(@claim)`, whose bound is decided where the call is inserted, and through `proved()` pinned by the item's declared result, whose bound waits for the expectation and is decided when it is retried. A body row states a call whose own bound is omitted — `List/get(xs, i)` — or a contradiction arm, `match Bool/False/refuted() end`, and is asked once.
//!
//! **Each row names the stage of `documentation/roadmap/algebra/02-bounds-from-facts-spec.md` that fills it.** A row whose stage has landed must compile; a row whose stage has not must be refused for its bound, and nothing else — a row refused for a misspelling would pass as refused, so the refusal must name what nothing discharged. A control is refused for its bound at every stage: it states what the procedure's fragment leaves out.

use super::{core_elab, typecheck};

/// The last stage of algebra part 2 that has landed.
const LANDED: u8 = 5;

/// The imports every program opens with.
const HEADER: &str = "use /std/{Nat, Int, Bool, Char, List, Eq, Io, proved};";

/// What a row asks the procedure for.
enum Ask {
    /// A proposition, asked at insertion and at retry.
    Claim(&'static str),
    /// A body checked against a result type, whose omitted bounds are the ask.
    Body {
        result: &'static str,
        body: &'static str,
    },
}

/// One row: the stage that fills it, the binders its facts are stated under, and what it asks.
struct Row {
    stage: u8,
    binders: &'static str,
    ask: Ask,
}

const fn claim(stage: u8, binders: &'static str, claim: &'static str) -> Row {
    Row {
        stage,
        binders,
        ask: Ask::Claim(claim),
    }
}

const fn body(stage: u8, binders: &'static str, result: &'static str, body: &'static str) -> Row {
    Row {
        stage,
        binders,
        ask: Ask::Body { result, body },
    }
}

const ROWS: &[Row] = &[
    // A tautology conversion decides and reduction does not.
    claim(2, "b: Bool", "Bool/Holds(b || Bool/not(b))"),
    claim(2, "b: Bool", "Bool/Holds(Bool/not(b && Bool/not(b)))"),
    // The spec's own rows: a strict bound composed with a loose one, from hypotheses and from a guard.
    body(
        3,
        "xs: List(Nat), i: Nat, m: Nat, p: Nat/Lt(i, m), q: Nat/Le(m, List/len(xs))",
        "Nat",
        "List/get(xs, i)",
    ),
    body(
        3,
        "xs: List(Nat), i: Nat, m: Nat, q: Nat/Le(m, List/len(xs))",
        "Nat",
        "match i < m | true => List/get(xs, i) | false => 0 end",
    ),
    claim(
        3,
        "i: Nat, m: Nat, n: Nat, p: Nat/Lt(i, m), q: Nat/Le(m, n)",
        "Nat/Lt(i, n)",
    ),
    claim(
        3,
        "i: Int, m: Int, n: Int, p: Int/Lt(i, m), q: Int/Le(m, n)",
        "Int/Lt(i, n)",
    ),
    // A slack the direct form closes with a literal fact.
    claim(3, "i: Nat, n: Nat, p: Nat/Lt(i, n)", "Nat/Le(i, n)"),
    claim(3, "i: Int, n: Int, p: Int/Lt(i, n)", "Int/Le(i, n)"),
    claim(
        3,
        "a: Nat, b: Nat, c: Nat, p: Nat/Le(a, b), q: Nat/Le(b, c)",
        "Nat/Le(a, c + 5)",
    ),
    // A fact scaled by a literal: two hex digits make a byte.
    claim(
        3,
        "a: Nat, b: Nat, p: Nat/Lt(a, 16), q: Nat/Lt(b, 16)",
        "Nat/Lt(a * 16 + b, 256)",
    ),
    claim(
        3,
        "a: Int, b: Int, p: Int/Lt(a, +16), q: Int/Lt(b, +16)",
        "Int/Lt(a * +16 + b, +256)",
    ),
    // A proof field one level down.
    claim(3, "v: {x: Nat, ok: Nat/Lt(x, 10)}", "Nat/Lt(v.x, 20)"),
    // An equation a hypothesis holds.
    body(
        3,
        "xs: List(Nat), n: Nat, e: Eq()(List/len(xs), n), i: Nat, p: Nat/Lt(i, n)",
        "Nat",
        "List/get(xs, i)",
    ),
    // A local definition, unfolded.
    body(
        3,
        "i: Nat, n: Nat, p: Nat/Lt(i, n)",
        "{}",
        "let k = n + 2; let _: Nat/Lt(i + 1, k) = proved(); ()",
    ),
    // A guard's false arm, and a variable refined to a successor.
    body(
        3,
        "n: Int",
        "{}",
        "match n >= +0 | true => () | false => let _: Int/Le(+0, +0 - n) = proved(); () end",
    ),
    body(
        3,
        "k: Nat, n: Nat, p: Nat/Le(k, n)",
        "{}",
        "match k | 0 => () | j + 1 => let _: Nat/Lt(j, n) = proved(); () end",
    ),
    // A natural's bound read at `Int`.
    claim(
        3,
        "m: Nat, n: Nat, i: Int, p: Nat/Lt(m, n), q: Int/Le(i, Nat/to_int(m))",
        "Int/Lt(i, Nat/to_int(n))",
    ),
    // A conjunction in a hypothesis, through a function unfolding to one and through `&&`.
    claim(
        3,
        "c: Char, h: Bool/Holds(Char/is_upper(c))",
        "Nat/Lt(c.code, 0x5B)",
    ),
    claim(3, "x: Nat, h: Bool/Holds(x >= 3 && x <= 9)", "Nat/Le(x, 9)"),
    // A range check's guard, and a guard on a function unfolding to one: `Char/to_ascii_lower`'s bound.
    body(
        3,
        "n: Nat",
        "{}",
        "match Nat/in_range(n, 0, 0x7F) | true => let _: Nat/Lt(n, 0x80) = proved(); () | false => () end",
    ),
    body(
        3,
        "c: Char",
        "{}",
        "match Char/is_upper(c) | true => let _: Nat/Lt(c.code + 0x20, 0xD800) = proved(); () | false => () end",
    ),
    // An `==` guard's true arm, as the equation it gives.
    body(
        3,
        "x: Nat, y: Nat, p: Nat/Lt(y, 5)",
        "{}",
        "match x == y | true => let _: Nat/Lt(x, 5) = proved(); () | false => () end",
    ),
    // A goal that needs the negated goal scaled: the refuting form.
    claim(4, "a: Nat, b: Nat, p: Nat/Le(a * 2, b * 2)", "Nat/Le(a, b)"),
    claim(4, "x: Nat, p: Nat/Le(x * 3, 10)", "Nat/Le(x, 3)"),
    claim(4, "x: Int, p: Int/Le(x * +3, +10)", "Int/Le(x, +3)"),
    // `proved` pinned by a later argument: its bound waits for the proposition, and is proved from the scope's facts when it is known.
    body(
        4,
        "i: Nat, m: Nat, n: Nat, p: Nat/Lt(i, m), q: Nat/Le(m, n)",
        "Nat",
        "let f(r: Nat/Lt(i, n)) -> Nat = 0; let x = proved(); f(x)",
    ),
    // Contradiction arms: facts that refute each other, from hypotheses and from a guard.
    body(
        4,
        "n: Nat, p: Nat/Le(0x80, n), q: Nat/Le(n, 0x7F)",
        "Nat",
        "match Bool/False/refuted() end",
    ),
    body(
        4,
        "n: Nat, q: Nat/Le(n, 10)",
        "Nat",
        "match n > 20 | true => match Bool/False/refuted() end | false => 0 end",
    ),
    body(
        4,
        "n: Int, q: Int/Le(n, +10)",
        "Int",
        "match n > +20 | true => match Bool/False/refuted() end | false => +0 end",
    ),
    // Truncated subtraction, read by the case split its definition makes.
    claim(
        5,
        "k: Nat, n: Nat, ok: Nat/Le(k, n)",
        "Nat/Le(k + (n - k), n)",
    ),
    claim(5, "a: Nat, n: Nat, p: Nat/Lt(a, n)", "Nat/Lt(0, n - a)"),
    claim(
        5,
        "c: Nat, lo: Nat, p: Nat/Le(lo, c)",
        "Nat/Le(lo - 0x80, c - 0x80)",
    ),
    // A case the arm's guard already decides is read without a split.
    body(
        5,
        "k: Nat, n: Nat",
        "Nat",
        "match k <= n | true => let _: Nat/Le(k + (n - k), n) = proved(); 0 | false => 0 end",
    ),
    // Division and remainder by a literal, through the quotient and remainder they denote.
    claim(
        5,
        "code: Nat, p: Nat/Lt(code, 0x800)",
        "Nat/Lt(code / 0x40, 0x20)",
    ),
    claim(5, "x: Nat, y: Nat, p: Nat/Lt(y, 3)", "Nat/Lt(x % 5 + y, 7)"),
    claim(5, "x: Nat", "Nat/Le(x / 64 + x / 64, x)"),
    // A truncated subtraction of a remainder, under a guard on it: `Char/hex_digit`'s shape.
    body(
        5,
        "n: Nat",
        "Nat",
        "match n % 16 < 10 | true => 0 | false => let _: Nat/Lt(n % 16 - 10, 6) = proved(); 0 end",
    ),
    // A remainder divided again, under `==` guards: `Str/Valid`'s shape, whose inner remainder is lifted away before the outer one.
    body(
        5,
        "code: Nat",
        "Nat",
        "match code >= 0x800 | false => 0 | true => match code / 4096 == 0 | false => 0 | true => match (code % 4096) / 64 >= 32 | true => 0 | false => match Bool/False/refuted() end end end end",
    ),
    // Products of two facts for a variable multiplier: `Nat/div_mod`'s two proofs.
    claim(
        6,
        "m: Nat, n: Nat, d: Nat, ok: Nat/Lt(0, d), p: Nat/Le(m * d, n)",
        "Nat/Le(m, Nat/div(n, d, @ok))",
    ),
    claim(
        6,
        "m: Nat, n: Nat, d: Nat, ok: Nat/Lt(0, d), p: Nat/Lt(n, m * d)",
        "Nat/Lt(Nat/div(n, d, @ok), m)",
    ),
    claim(
        6,
        "a: Int, b: Int, p: Int/Le(+0, a), q: Int/Le(+0, b)",
        "Int/Le(+0, a * b)",
    ),
];

/// What the procedure must refuse, at every stage.
const CONTROLS: &[Row] = &[
    // A decision no tautology settles, with nothing in scope that could.
    claim(0, "b: Bool", "Bool/Holds(b)"),
    // No fact in scope.
    claim(0, "i: Nat, n: Nat", "Nat/Lt(i, n)"),
    // A fact too weak for the goal.
    claim(0, "i: Nat, n: Nat, p: Nat/Le(i, n)", "Nat/Lt(i, n)"),
    // A contradiction only over the integers: `2 * x = 3` has a rational solution, and finding the integer refutation needs a cut.
    body(
        0,
        "x: Nat, p: Nat/Le(x * 2, 3), q: Nat/Le(3, x * 2)",
        "Nat",
        "match Bool/False/refuted() end",
    ),
    // A product of three facts.
    claim(0, "x: Nat, p: Nat/Le(x * x * x, 7)", "Nat/Le(x, 1)"),
    // A remainder by a literal at `Int`, whose bounds conversion does not decide there.
    claim(0, "x: Int", "Int/Lt(x % +3, +3)"),
    // Rewriting under a function by an equation, which is congruence rather than arithmetic.
    claim(
        0,
        "f: (Nat) -> Nat, r: Nat, s: Nat, e: Eq()(r, s), p: Nat/Lt(f(r), 10)",
        "Nat/Lt(f(s), 10)",
    ),
    // A range check's false arm, a disjunction.
    body(
        0,
        "n: Nat, q: Nat/Le(n, 10)",
        "Nat",
        "match Nat/in_range(n, 3, 20) | true => 0 | false => let _: Nat/Lt(n, 3) = proved(); 0 end",
    ),
    // A guard's fact at a retry the hole was born outside of: the bound is decided under the refinements its slot was born under, and the arm's are not among them.
    body(
        0,
        "i: Nat, m: Nat, n: Nat, q: Nat/Le(m, n)",
        "Nat",
        "let h = proved(); match i < m | true => let _: Nat/Lt(i, n) = h; 0 | false => 0 end",
    ),
];

/// The programs stating what `ask` asks under `binders`: a claim at insertion and at retry, a body once.
fn programs(binders: &str, ask: &Ask) -> Vec<String> {
    let item = |result: &str, body: &str| {
        format!("{HEADER}\nlet row({binders}) -> {result} = {body};\nIo/pure(())")
    };
    match *ask {
        Ask::Claim(claim) => vec![
            item(claim, &format!("proved(@{claim})")),
            item(claim, "proved()"),
        ],
        Ask::Body { result, body } => vec![item(result, body)],
    }
}

/// Every program of `rows` the compiler puts on the other side than `filled` says, with the compiler's answer.
fn misplaced(rows: &[&Row], filled: bool) -> Vec<String> {
    let all = rows.iter().flat_map(|row| programs(row.binders, &row.ask));
    verdicts(all, filled)
}

/// Every one of `programs` the compiler puts on the other side than `filled` says: refused where it should compile, or compiled — or refused for anything but its bound — where it should be refused for its bound.
fn verdicts(programs: impl IntoIterator<Item = String>, filled: bool) -> Vec<String> {
    programs
        .into_iter()
        .filter_map(|program| match (typecheck(&program), filled) {
            (Ok(()), true) => None,
            (Err(error), false) if error.contains("nothing discharged") => None,
            (Ok(()), false) => Some(format!("filled, but stated as refused:\n{program}")),
            (Err(error), _) => Some(format!("{error}\n{program}")),
        })
        .collect()
}

#[test]
fn every_row_a_landed_stage_covers_is_filled_at_insertion_and_at_retry() {
    let rows = ROWS
        .iter()
        .filter(|row| row.stage <= LANDED)
        .collect::<Vec<_>>();
    let found = misplaced(&rows, true);
    assert!(found.is_empty(), "{}", found.join("\n\n"));
}

#[test]
fn every_row_awaiting_its_stage_is_refused_for_its_bound() {
    let rows = ROWS
        .iter()
        .filter(|row| row.stage > LANDED)
        .collect::<Vec<_>>();
    let found = misplaced(&rows, false);
    assert!(found.is_empty(), "{}", found.join("\n\n"));
}

#[test]
fn every_control_is_refused_for_its_bound() {
    let rows = CONTROLS.iter().collect::<Vec<_>>();
    let found = misplaced(&rows, false);
    assert!(found.is_empty(), "{}", found.join("\n\n"));
}

/// The binders of a row, split where a comma is not inside a type's brackets.
fn binders(row: &Row) -> Vec<&'static str> {
    let (mut depth, mut start, mut found) = (0usize, 0usize, Vec::new());
    for (index, character) in row.binders.char_indices() {
        match character {
            '(' | '{' => depth += 1,
            ')' | '}' => depth -= 1,
            ',' if depth == 0 => {
                found.push(row.binders[start..index].trim());
                start = index + 1;
            }
            _ => {}
        }
    }
    found.push(row.binders[start..].trim());
    found
}

/// Whether a binder states a fact: its type is a comparison, an equation or a decision, which nothing but the bound refers to.
fn is_fact(binder: &str) -> bool {
    let type_ = binder.split_once(':').map_or("", |(_, type_)| type_.trim());
    [
        "Nat/Lt(",
        "Nat/Le(",
        "Int/Lt(",
        "Int/Le(",
        "Eq()(",
        "Bool/Holds(",
    ]
    .iter()
    .any(|head| type_.starts_with(head))
}

#[test]
fn every_fact_of_a_filled_row_is_needed() {
    // The spec's mutation check: a filled row with one of its facts dropped is refused for its bound, so no row is filled by a fact it does not state. A guard is no binder, and is left in place.
    let mut found = Vec::new();
    for row in ROWS.iter().filter(|row| row.stage <= LANDED) {
        let all = binders(row);
        for dropped in all.iter().filter(|binder| is_fact(binder)) {
            let kept = all
                .iter()
                .filter(|binder| binder != &dropped)
                .copied()
                .collect::<Vec<_>>()
                .join(", ");
            found.extend(
                verdicts(programs(&kept, &row.ask), false)
                    .into_iter()
                    .map(|report| format!("without `{dropped}`:\n{report}")),
            );
        }
    }
    assert!(found.is_empty(), "{}", found.join("\n\n"));
}

/// The byte row: two hex digits make a byte, a fact scaled by a literal summed with another.
const BYTE: &str = "use /std/{Nat, Io};
let byte(a: Nat, b: Nat, p: Nat/Lt(a, 16), q: Nat/Lt(b, 16)) -> Nat/Lt(a * 16 + b, 256) = PROOF;
Io/pure(())";

#[test]
fn a_filled_row_files_the_same_proof_every_time() {
    // One program, one certificate, one proof: the search is deterministic, so the elaborated item is the same on every run. CI runs this on each platform it builds for.
    let omitted = "use /std/{Nat, Io, proved};
let byte(a: Nat, b: Nat, p: Nat/Lt(a, 16), q: Nat/Lt(b, 16)) -> Nat/Lt(a * 16 + b, 256) = proved(@Nat/Lt(a * 16 + b, 256));
Io/pure(())";
    let first = core_elab(omitted);
    assert_eq!(core_elab(omitted), first);
    assert!(
        first.contains("mul_mono_r"),
        "the byte row is proved by a sum over a scaled fact:\n{first}"
    );
}

#[test]
fn a_certificate_corrupted_by_one_multiplier_is_refused_by_both_checkers() {
    // The byte row's proof in the shape the procedure writes it, `16 · (a + 1) <= 16 · 16` summed with `b + 1 <= 16`, then with the multiplier moved off 16 and the two facts swapped: a wrong certificate is a term that does not check.
    let proof = |k: u32, first: &str, second: &str| {
        format!(
            "Nat/Le/add(@(a + 1) * {k}, @16 * {k}, @b + 1, @16, Nat/Le/mul_mono_r(@a + 1, @16, {k}, {first}), {second})"
        )
    };
    let program = |proof: String| BYTE.replace("PROOF", &proof);
    assert_eq!(typecheck(&program(proof(16, "p", "q"))), Ok(()));
    for (k, first, second) in [(15, "p", "q"), (17, "p", "q"), (16, "q", "p")] {
        assert!(
            typecheck(&program(proof(k, first, second))).is_err(),
            "a certificate scaled by {k} over {first} and {second} checked"
        );
    }
}

#[test]
fn a_refused_bound_names_the_facts_it_considered_and_a_counterexample() {
    let refusal = |binders: &str, claim: &str| {
        let program =
            format!("{HEADER}\nlet row({binders}) -> {claim} = proved(@{claim});\nIo/pure(())");
        typecheck(&program).expect_err("the control is refused")
    };

    // A fact too weak for the goal: named, and a counterexample at the one value it leaves.
    let weak = refusal("i: Nat, n: Nat, p: Nat/Le(i, n)", "Nat/Lt(i, n)");
    assert!(
        weak.contains("it does not follow from the facts in scope:\n    p: i <= n"),
        "{weak}"
    );
    assert!(weak.contains("it fails at"), "{weak}");

    // A contradiction only the integers see: the counterexample is a fraction.
    let rational = refusal(
        "x: Nat, p: Nat/Le(x * 2, 3), q: Nat/Le(3, x * 2)",
        "Nat/Le(x, 0)",
    );
    assert!(rational.contains("3/2"), "{rational}");

    // A contradiction goal the facts do not refute: they all hold at the assignment, which is a fraction here.
    let absurd = {
        let program = format!(
            "{HEADER}\nlet row(x: Nat, p: Nat/Le(x * 2, 3), q: Nat/Le(3, x * 2)) -> Nat = match Bool/False/refuted() end;\nIo/pure(())"
        );
        typecheck(&program).expect_err("the control is refused")
    };
    assert!(
        absurd.contains("the facts in scope do not refute each other")
            && absurd.contains("they all hold at x = 3/2"),
        "{absurd}"
    );

    // A decision the procedure reads nothing in says nothing new.
    let silent = refusal("b: Bool", "Bool/Holds(b)");
    assert!(!silent.contains("facts in scope"), "{silent}");
}

#[test]
fn a_written_goal_over_a_bound_shows_what_the_procedure_found() {
    let goal = |binders: &str| {
        let program = format!(
            "{HEADER}\nlet row({binders}) -> Nat = List/get(@Nat, xs, i, @?);\nIo/pure(())"
        );
        typecheck(&program).expect_err("a written goal never compiles")
    };

    // Where the facts prove the bound, the proof is a candidate the author can paste.
    let proved = goal("xs: List(Nat), i: Nat, m: Nat, p: Nat/Lt(i, m), q: Nat/Le(m, List/len(xs))");
    assert!(
        proved.contains("? \u{2248}") && proved.contains("add("),
        "{proved}"
    );

    // Where they do not, the goal says what a refused bound says: the facts, and where the bound fails.
    let refused = goal("xs: List(Nat), i: Nat, m: Nat, q: Nat/Le(m, List/len(xs))");
    assert!(
        refused.contains("it does not follow from the facts in scope")
            && refused.contains("it fails at"),
        "{refused}"
    );
}
