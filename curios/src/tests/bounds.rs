//! The grid of bounds proved from the facts in scope: omitted bounds the elaborator must fill with a proof both checkers recheck, and controls it must refuse.
//!
//! **A row is a bound and the facts it follows from.** A claim row states a proposition over its binders and asks for it twice: through `proved(@claim)`, whose bound is decided where the call is inserted, and through `proved()` pinned by the item's declared result, whose bound waits for the expectation and is decided when it is retried. A body row states a call whose own bound is omitted — `List/get(xs, i)` — or a contradiction arm, `match Bool/False/refuted() end`, and is asked once.
//!
//! **Every row compiles, and every control is refused for its bound and nothing else** — a control refused for a misspelling would pass as refused, so the refusal must name what nothing discharged. A control states what the procedure's fragment leaves out. Why the procedure is what it is is `documentation/design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md`, and its contract is `curios-elab`'s `entailment` documentation.

use super::{core_elab, typecheck};

/// The imports every program opens with.
const HEADER: &str = "use /std/{Nat, Int, Bool, Byte, Bytes, Char, List, Option, Str, Eq, Io}; use /std/Bool/{True, False, Holds};";

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

/// One row: the binders its facts are stated under, and what it asks.
struct Row {
    binders: &'static str,
    ask: Ask,
}

const fn claim(binders: &'static str, claim: &'static str) -> Row {
    Row {
        binders,
        ask: Ask::Claim(claim),
    }
}

const fn body(binders: &'static str, result: &'static str, body: &'static str) -> Row {
    Row {
        binders,
        ask: Ask::Body { result, body },
    }
}

const ROWS: &[Row] = &[
    // A tautology conversion decides and reduction does not.
    claim("b: Bool", "Holds(b || Bool/not(b))"),
    claim("b: Bool", "Holds(Bool/not(b && Bool/not(b)))"),
    // A strict bound composed with a loose one, from hypotheses and from a guard.
    body(
        "xs: List(Nat), i: Nat, m: Nat, p: Holds(i < m), q: Holds(m <= List/len(xs))",
        "Nat",
        "List/get(xs, i)",
    ),
    body(
        "xs: List(Nat), i: Nat, m: Nat, q: Holds(m <= List/len(xs))",
        "Nat",
        "match i < m | true => List/get(xs, i) | false => 0 end",
    ),
    claim(
        "i: Nat, m: Nat, n: Nat, p: Holds(i < m), q: Holds(m <= n)",
        "Holds(i < n)",
    ),
    claim(
        "i: Int, m: Int, n: Int, p: Holds(i < m), q: Holds(m <= n)",
        "Holds(i < n)",
    ),
    // A slack the direct form closes with a literal fact.
    claim("i: Nat, n: Nat, p: Holds(i < n)", "Holds(i <= n)"),
    claim("i: Int, n: Int, p: Holds(i < n)", "Holds(i <= n)"),
    claim(
        "a: Nat, b: Nat, c: Nat, p: Holds(a <= b), q: Holds(b <= c)",
        "Holds(a <= c + 5)",
    ),
    // A fact scaled by a literal: two hex digits make a byte.
    claim(
        "a: Nat, b: Nat, p: Holds(a < 16), q: Holds(b < 16)",
        "Holds(a * 16 + b < 256)",
    ),
    claim(
        "a: Int, b: Int, p: Holds(a < +16), q: Holds(b < +16)",
        "Holds(a * +16 + b < +256)",
    ),
    // A proof field one level down.
    claim("v: {x: Nat, ok: Holds(x < 10)}", "Holds(v.x < 20)"),
    // An equation a hypothesis holds.
    body(
        "xs: List(Nat), n: Nat, e: Eq()(List/len(xs), n), i: Nat, p: Holds(i < n)",
        "Nat",
        "List/get(xs, i)",
    ),
    // A local definition, unfolded.
    body(
        "i: Nat, n: Nat, p: Holds(i < n)",
        "{}",
        "let k = n + 2; let _: Holds(i + 1 < k) = True/proved(); ()",
    ),
    // A local definition bound to a call, `Str`'s `occurrence`: the facts over it are written over its name, where the call's reduct would carry the body it unfolds to, whose arms elaborate only where they were written.
    body(
        "s: Str, at: Str/At(s), @here: Holds(at.offset < Bytes/len(s.bytes)), again: (p: Str/At(s)) -> Option({start: Str/At(s), ordered: Holds(p.offset <= start.offset)})",
        "Option({start: Str/At(s), ordered: Holds(at.offset <= start.offset)})",
        "let onward = Str/At/skip(s, at); match again(onward.past) | some(found) => Option/some((start = found.start, ordered = True/proved())) | none() => Option/none() end",
    ),
    // A local definition bound to the item's own recursive call, which reduction reads as the slot the member is known by while it is checked: the bound waits on that slot alone, and is proved over the definition's name.
    body(
        "k: Nat",
        "{at: Nat, below: Holds(at <= k)}",
        "match k | 0 => (at = 0, below = True/qed()) | j + 1 => let back = row(j); (at = back.at, below = True/proved()) end",
    ),
    // A guard's false arm, and a variable refined to a successor.
    body(
        "n: Int",
        "{}",
        "match n >= +0 | true => () | false => let _: Holds(+0 <= +0 - n) = True/proved(); () end",
    ),
    body(
        "k: Nat, n: Nat, p: Holds(k <= n)",
        "{}",
        "match k | 0 => () | j + 1 => let _: Holds(j < n) = True/proved(); () end",
    ),
    // A natural's bound read at `Int`.
    claim(
        "m: Nat, n: Nat, i: Int, p: Holds(m < n), q: Holds(i <= Nat/to_int(m))",
        "Holds(i < Nat/to_int(n))",
    ),
    // A conjunction in a hypothesis, through a function unfolding to one and through `&&`.
    claim(
        "c: Char, h: Holds(Char/is_upper(c))",
        "Holds(c.code < 0x5B)",
    ),
    claim("x: Nat, h: Holds(x >= 3 && x <= 9)", "Holds(x <= 9)"),
    // A range check's guard, and a guard on a function unfolding to one: `Char/to_ascii_lower`'s bound.
    body(
        "n: Nat",
        "{}",
        "match Nat/in_range(n, 0, 0x7F) | true => let _: Holds(n < 0x80) = True/proved(); () | false => () end",
    ),
    body(
        "c: Char",
        "{}",
        "match Char/is_upper(c) | true => let _: Holds(c.code + 0x20 < 0xD800) = True/proved(); () | false => () end",
    ),
    // A range check's guard over a call and over a local definition: its bounds are proved over the operands the arm's key spells, which both checkers hold, where their reducts — the intrinsic a call unfolds to, the value a definition stands for — are a spelling one of them misses.
    body(
        "c: Byte",
        "{}",
        "match Nat/in_range(Byte/to_nat(c), 0xF0, 0xF4) | true => let _: Holds(0xF0 <= Byte/to_nat(c)) = True/proved(); () | false => () end",
    ),
    body(
        "m: Nat",
        "{}",
        "let n = m + 0; match Nat/in_range(n, 0xF0, 0xF4) | true => let _: Holds(0xF0 <= m) = True/proved(); () | false => () end",
    ),
    // A guard respelled: the goal commutes the guard's sum, and the guard's own equation answers it.
    body(
        "a: Nat, b: Nat",
        "{}",
        "match a + b < 10 | true => let _: Holds(b + a < 10) = True/proved(); () | false => () end",
    ),
    // An `==` guard's true arm, as the equation it gives.
    body(
        "x: Nat, y: Nat, p: Holds(y < 5)",
        "{}",
        "match x == y | true => let _: Holds(x < 5) = True/proved(); () | false => () end",
    ),
    // A goal that needs the negated goal scaled: the refuting form.
    claim("a: Nat, b: Nat, p: Holds(a * 2 <= b * 2)", "Holds(a <= b)"),
    claim("x: Nat, p: Holds(x * 3 <= 10)", "Holds(x <= 3)"),
    claim("x: Int, p: Holds(x * +3 <= +10)", "Holds(x <= +3)"),
    // `proved` pinned by a later argument: its bound waits for the proposition, and is proved from the scope's facts when it is known.
    body(
        "i: Nat, m: Nat, n: Nat, p: Holds(i < m), q: Holds(m <= n)",
        "Nat",
        "let f(r: Holds(i < n)) -> Nat = 0; let x = True/proved(); f(x)",
    ),
    // Contradiction arms: facts that refute each other, from hypotheses and from a guard.
    body(
        "n: Nat, p: Holds(0x80 <= n), q: Holds(n <= 0x7F)",
        "Nat",
        "match False/refuted() end",
    ),
    body(
        "n: Nat, q: Holds(n <= 10)",
        "Nat",
        "match n > 20 | true => match False/refuted() end | false => 0 end",
    ),
    body(
        "n: Int, q: Holds(n <= +10)",
        "Int",
        "match n > +20 | true => match False/refuted() end | false => +0 end",
    ),
    // A range check from zero over a local definition, `Str`'s shape: the check folds to its upper bound, which is still proved over the check the arm recorded.
    body(
        "c: Byte, q: Holds(Nat/in_range(Byte/to_nat(c), 0x80, 0xBF))",
        "Nat",
        "let n = Byte/to_nat(c); match Nat/in_range(n, 0, 0x7F) | true => match False/refuted() end | false => 0 end",
    ),
    // A hypothesis the arm's guard reduces to an empty proposition refutes the scope by itself, for a contradiction and for a bound alike.
    body(
        "n: Nat, q: Holds(Nat/in_range(n, 0x80, 0xBF))",
        "Nat",
        "match n <= 0x7F | true => match False/refuted() end | false => 0 end",
    ),
    body(
        "n: Nat, p: Holds(0x80 <= n)",
        "{}",
        "match n <= 0x7F | true => let _: Holds(n < 3) = True/proved(); () | false => () end",
    ),
    body(
        "n: Int, p: Holds(+0x80 <= n)",
        "Int",
        "match n <= +0x7F | true => match False/refuted() end | false => +0 end",
    ),
    // Truncated subtraction, read by the case split its definition makes.
    claim(
        "k: Nat, n: Nat, ok: Holds(k <= n)",
        "Holds(k + (n - k) <= n)",
    ),
    claim("a: Nat, n: Nat, p: Holds(a < n)", "Holds(0 < n - a)"),
    claim(
        "c: Nat, lo: Nat, p: Holds(lo <= c)",
        "Holds(lo - 0x80 <= c - 0x80)",
    ),
    // A case the arm's guard already decides is read without a split.
    body(
        "k: Nat, n: Nat",
        "Nat",
        "match k <= n | true => let _: Holds(k + (n - k) <= n) = True/proved(); 0 | false => 0 end",
    ),
    // Division and remainder by a literal, through the quotient and remainder they denote.
    claim(
        "code: Nat, p: Holds(code < 0x800)",
        "Holds(code / 0x40 < 0x20)",
    ),
    claim("x: Nat, y: Nat, p: Holds(y < 3)", "Holds(x % 5 + y < 7)"),
    claim("x: Nat", "Holds(x / 64 + x / 64 <= x)"),
    // A truncated subtraction of a remainder, under a guard on it: `Char/hex_digit`'s shape.
    body(
        "n: Nat",
        "Nat",
        "match n % 16 < 10 | true => 0 | false => let _: Holds(n % 16 - 10 < 6) = True/proved(); 0 end",
    ),
    // A remainder divided again, under `==` guards: `Str/Valid`'s shape, whose inner remainder is lifted away before the outer one.
    body(
        "code: Nat",
        "Nat",
        "match code >= 0x800 | false => 0 | true => match code / 4096 == 0 | false => 0 | true => match (code % 4096) / 64 >= 32 | true => 0 | false => match False/refuted() end end end end",
    ),
    // Products of two facts for a variable multiplier: `Nat/div_mod`'s two proofs.
    claim(
        "m: Nat, n: Nat, d: Nat, ok: Holds(0 < d), p: Holds(m * d <= n)",
        "Holds(m <= Nat/div(n, d, @ok))",
    ),
    claim(
        "m: Nat, n: Nat, d: Nat, ok: Holds(0 < d), p: Holds(n < m * d)",
        "Holds(Nat/div(n, d, @ok) < m)",
    ),
    claim(
        "a: Int, b: Int, p: Holds(+0 <= a), q: Holds(+0 <= b)",
        "Holds(+0 <= a * b)",
    ),
    // A bound two guards imply through a product, `(n - i - 1) * k >= 0`, which the refinement seam leaves stuck.
    body(
        "n: Nat, k: Nat, l: List(Nat), i: Nat",
        "List(Nat)",
        "match i < n | false => [] | true => match List/len(l) == n * k | false => [] | true => List/slice(l, i * k, k) end end",
    ),
];

/// What the procedure must refuse.
const CONTROLS: &[Row] = &[
    // A decision no tautology settles, with nothing in scope that could.
    claim("b: Bool", "Holds(b)"),
    // No fact in scope.
    claim("i: Nat, n: Nat", "Holds(i < n)"),
    // A fact too weak for the goal.
    claim("i: Nat, n: Nat, p: Holds(i <= n)", "Holds(i < n)"),
    // A contradiction only over the integers: `2 * x = 3` has a rational solution, and finding the integer refutation needs a cut.
    body(
        "x: Nat, p: Holds(x * 2 <= 3), q: Holds(3 <= x * 2)",
        "Nat",
        "match False/refuted() end",
    ),
    // A product of three facts.
    claim("x: Nat, p: Holds(x * x * x <= 7)", "Holds(x <= 1)"),
    // A remainder by a literal at `Int`, whose bounds conversion does not decide there.
    claim("x: Int", "Holds(x % +3 < +3)"),
    // Rewriting under a function by an equation, which is congruence rather than arithmetic.
    claim(
        "f: (Nat) -> Nat, r: Nat, s: Nat, e: Eq()(r, s), p: Holds(f(r) < 10)",
        "Holds(f(s) < 10)",
    ),
    // A range check's false arm, a disjunction.
    body(
        "n: Nat, q: Holds(n <= 10)",
        "Nat",
        "match Nat/in_range(n, 3, 20) | true => 0 | false => let _: Holds(n < 3) = True/proved(); 0 end",
    ),
    // A guard's fact at a retry the hole was born outside of: the bound is decided under the refinements its slot was born under, and the arm's are not among them.
    body(
        "i: Nat, m: Nat, n: Nat, q: Holds(m <= n)",
        "Nat",
        "let h = True/proved(); match i < m | true => let _: Holds(i < n) = h; 0 | false => 0 end",
    ),
];

/// The programs stating what `ask` asks under `binders`: a claim at insertion and at retry, a body once.
fn programs(binders: &str, ask: &Ask) -> Vec<String> {
    let item = |result: &str, body: &str| {
        format!("{HEADER}\nlet row({binders}) -> {result} = {body};\nIo/pure(())")
    };
    match *ask {
        Ask::Claim(claim) => vec![
            item(claim, &format!("True/proved(@{claim})")),
            item(claim, "True/proved()"),
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
fn every_row_is_filled_at_insertion_and_at_retry() {
    let rows = ROWS.iter().collect::<Vec<_>>();
    let found = misplaced(&rows, true);
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

/// Whether a binder states a fact nothing but the bound refers to: its type is a comparison, an equation or a decision, and what the row asks never names it — `ok` in `Nat/div(n, d, @ok)` is the division's own proof, which dropping would leave unbound.
fn is_fact(binder: &str, row: &Row) -> bool {
    let Some((name, type_)) = binder.split_once(':') else {
        return false;
    };
    let asked = match row.ask {
        Ask::Claim(claim) => [claim, ""],
        Ask::Body { result, body } => [result, body],
    };
    let named = format!("@{}", name.trim());
    let mentioned = asked.iter().any(|text| {
        text.match_indices(&named).any(|(at, _)| {
            !text[at + named.len()..]
                .starts_with(|next: char| next.is_alphanumeric() || next == '_')
        })
    });
    let stated = ["Eq()(", "Holds("]
        .iter()
        .any(|head| type_.trim().starts_with(head));
    stated && !mentioned
}

#[test]
fn every_fact_of_a_filled_row_is_needed() {
    // The mutation check: a filled row with one of its facts dropped is refused for its bound, so no row is filled by a fact it does not state. A guard is no binder, and is left in place.
    let mut found = Vec::new();
    for row in ROWS {
        let all = binders(row);
        for dropped in all.iter().filter(|binder| is_fact(binder, row)) {
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
use /std/Bool/{Holds};
let byte(a: Nat, b: Nat, p: Holds(a < 16), q: Holds(b < 16)) -> Holds(a * 16 + b < 256) = PROOF;
Io/pure(())";

#[test]
fn a_filled_row_files_the_same_proof_every_time() {
    // One program, one certificate, one proof: the search is deterministic, so the elaborated item is the same on every run. CI runs this on each platform it builds for.
    let omitted = "use /std/{Nat, Io};
use /std/Bool/{True, Holds};
let byte(a: Nat, b: Nat, p: Holds(a < 16), q: Holds(b < 16)) -> Holds(a * 16 + b < 256) = True/proved(@Holds(a * 16 + b < 256));
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
            "Nat/le/add(@(a + 1) * {k}, @16 * {k}, @b + 1, @16, Nat/le/mul_mono_r(@a + 1, @16, {k}, {first}), {second})"
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
        let program = format!(
            "{HEADER}\nlet row({binders}) -> {claim} = True/proved(@{claim});\nIo/pure(())"
        );
        typecheck(&program).expect_err("the control is refused")
    };

    // A fact too weak for the goal: named, and a counterexample at the one value it leaves.
    let weak = refusal("i: Nat, n: Nat, p: Holds(i <= n)", "Holds(i < n)");
    assert!(
        weak.contains("it does not follow from the facts in scope:\n    p: i <= n"),
        "{weak}"
    );
    assert!(weak.contains("it fails at"), "{weak}");

    // A contradiction only the integers see: the counterexample is a fraction.
    let rational = refusal(
        "x: Nat, p: Holds(x * 2 <= 3), q: Holds(3 <= x * 2)",
        "Holds(x <= 0)",
    );
    assert!(rational.contains("3/2"), "{rational}");

    // A contradiction goal the facts do not refute: they all hold at the assignment, which is a fraction here.
    let absurd = {
        let program = format!(
            "{HEADER}\nlet row(x: Nat, p: Holds(x * 2 <= 3), q: Holds(3 <= x * 2)) -> Nat = match False/refuted() end;\nIo/pure(())"
        );
        typecheck(&program).expect_err("the control is refused")
    };
    assert!(
        absurd.contains("the facts in scope do not refute each other")
            && absurd.contains("they all hold at x = 3/2"),
        "{absurd}"
    );

    // A guard written through `Cmp` is recorded as written and as the dispatch resolves, and is one fact.
    let guarded = {
        let program = format!(
            "{HEADER}\nlet row(n: Nat) -> Nat = match n <= 0x7F | true => match False/refuted() end | false => 0 end;\nIo/pure(())"
        );
        typecheck(&program).expect_err("the control is refused")
    };
    assert_eq!(guarded.matches("a guard,").count(), 1, "{guarded}");

    // A decision the procedure reads nothing in says nothing new.
    let silent = refusal("b: Bool", "Holds(b)");
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
    let proved =
        goal("xs: List(Nat), i: Nat, m: Nat, p: Holds(i < m), q: Holds(m <= List/len(xs))");
    assert!(
        proved.contains("? \u{2248}") && proved.contains("add("),
        "{proved}"
    );

    // Where they do not, the goal says what a refused bound says: the facts, and where the bound fails.
    let refused = goal("xs: List(Nat), i: Nat, m: Nat, q: Holds(m <= List/len(xs))");
    assert!(
        refused.contains("it does not follow from the facts in scope")
            && refused.contains("it fails at"),
        "{refused}"
    );
}
