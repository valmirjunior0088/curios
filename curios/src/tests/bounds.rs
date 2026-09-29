//! The grid of bounds proved from the facts in scope: omitted bounds the elaborator must fill with a proof both checkers recheck, and controls it must refuse.
//!
//! **A row is a bound and the facts it follows from.** A claim row states a proposition over its binders and asks for it twice: through `hold(@claim)`, whose bound is decided where the call is inserted, and through `hold()` pinned by the item's declared result, whose bound waits for the expectation and is decided when it is retried. A body row states a call whose own bound is omitted — `List/get(xs, i)` — or a contradiction arm, and is asked once. `hold` and `refute` are declared by each program, as `/std/proved` and `/std/Bool/False/refuted` state them, so a row does not change when those land.
//!
//! **Each row names the stage of `documentation/roadmap/algebra/02-bounds-from-facts-spec.md` that fills it.** A row whose stage has landed must compile; a row whose stage has not must be refused for its bound, and nothing else — a row refused for a misspelling would pass as refused, so the refusal must name what nothing discharged. A control is refused for its bound at every stage: it states what the procedure's fragment leaves out.

use super::typecheck;

/// The last stage of algebra part 2 that has landed.
const LANDED: u8 = 2;

/// The imports every program opens with, and the two entry points each declares for itself.
const HEADER: &str = "use /std/{Nat, Int, Bool, List, Eq, Io};
let hold(@P: Prop, @p: P) -> P = p;
let refute(@p: Bool/False) -> Bool/False = p;";

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
        "let k = n + 2; let _: Nat/Lt(i + 1, k) = hold(); ()",
    ),
    // A guard's false arm, and a variable refined to a successor.
    body(
        3,
        "n: Int",
        "{}",
        "match n >= +0 | true => () | false => let _: Int/Le(+0, +0 - n) = hold(); () end",
    ),
    body(
        3,
        "k: Nat, n: Nat, p: Nat/Le(k, n)",
        "{}",
        "match k | 0 => () | j + 1 => let _: Nat/Lt(j, n) = hold(); () end",
    ),
    // A natural's bound read at `Int`.
    claim(
        3,
        "m: Nat, n: Nat, i: Int, p: Nat/Lt(m, n), q: Int/Le(i, Nat/to_int(m))",
        "Int/Lt(i, Nat/to_int(n))",
    ),
    // A goal that needs the negated goal scaled: the refuting form.
    claim(4, "a: Nat, b: Nat, p: Nat/Le(a * 2, b * 2)", "Nat/Le(a, b)"),
    claim(4, "x: Nat, p: Nat/Le(x * 3, 10)", "Nat/Le(x, 3)"),
    claim(4, "x: Int, p: Int/Le(x * +3, +10)", "Int/Le(x, +3)"),
    // Contradiction arms: facts that refute each other, from hypotheses and from a guard.
    body(
        4,
        "n: Nat, p: Nat/Le(0x80, n), q: Nat/Le(n, 0x7F)",
        "Nat",
        "match refute() end",
    ),
    body(
        4,
        "n: Nat, q: Nat/Le(n, 10)",
        "Nat",
        "match n > 20 | true => match refute() end | false => 0 end",
    ),
    body(
        4,
        "n: Int, q: Int/Le(n, +10)",
        "Int",
        "match n > +20 | true => match refute() end | false => +0 end",
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
    // Division and remainder by a literal, through the quotient and remainder they denote.
    claim(
        5,
        "code: Nat, p: Nat/Lt(code, 0x800)",
        "Nat/Lt(code / 0x40, 0x20)",
    ),
    claim(5, "x: Nat, y: Nat, p: Nat/Lt(y, 3)", "Nat/Lt(x % 5 + y, 7)"),
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
        "match refute() end",
    ),
    // A product of three facts.
    claim(0, "x: Nat, p: Nat/Le(x * x * x, 7)", "Nat/Le(x, 1)"),
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
        "match Nat/in_range(n, 3, 20) | true => 0 | false => let _: Nat/Lt(n, 3) = hold(); 0 end",
    ),
    // A guard's fact at a retry the hole was born outside of: the bound is decided under the refinements its slot was born under, and the arm's are not among them.
    body(
        0,
        "i: Nat, m: Nat, n: Nat, q: Nat/Le(m, n)",
        "Nat",
        "let h = hold(); match i < m | true => let _: Nat/Lt(i, n) = h; 0 | false => 0 end",
    ),
];

impl Row {
    /// The programs stating the row: a claim at insertion and at retry, a body once.
    fn programs(&self) -> Vec<String> {
        let item = |result: &str, body: &str| {
            format!(
                "{HEADER}\nlet row({}) -> {result} = {body};\nIo/pure(())",
                self.binders
            )
        };
        match self.ask {
            Ask::Claim(claim) => vec![
                item(claim, &format!("hold(@{claim})")),
                item(claim, "hold()"),
            ],
            Ask::Body { result, body } => vec![item(result, body)],
        }
    }
}

/// Every program of `rows` the compiler puts on the other side than `filled` says, with the compiler's answer.
fn misplaced(rows: &[&Row], filled: bool) -> Vec<String> {
    rows.iter()
        .flat_map(|row| row.programs())
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
