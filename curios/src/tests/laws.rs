//! The definitional-law grid: which equations over the intrinsic carriers the normalizer closes by computation, and which it refuses, stated so that both halves are checked.
//!
//! A held row is a law both checkers decide for every value, so it is stated as an `Eq/refl()` proof and compiled. A refused row is a law the normalizer does *not* take — kept here rather than in prose so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing. The refused half cannot be stated as a proof, so both halves are also stated as written goals and read back through the `? ≈ Eq/refl()` candidate line, which is the compiler's own answer to "does refl fit here": a held row must get the line and a refused row must not. The two directions check each other — if the candidate search stopped reporting, every held row would fail the goal test. That alone does not keep a refused row from passing vacuously, because the search can stop *partway*: every goal of one program draws its candidate attempts from a single budget, so a program with enough goals runs it dry and every goal after that point reports no candidate at all — which is exactly what a refused row is expected to show, and the refused rows come last. So each program ends on a sentinel, a row `refl` trivially closes, stated after the refused ones: a sentinel without its line means the search ran dry before the refused rows were reached, and the test says so rather than passing them.
//!
//! **State a row at every carrier, and state it first.** A grid stating a law at one carrier and not another passes exactly as a complete one does, and what it hides is invisible from reading the two implementations side by side: the bit grain went without `eql` while its byte twin had it, and `List` went without both seam-index rows while `Bytes` and `Bits` held them. Each was a real incompleteness, and each was found by stating the row rather than by inspection. A row stated before an implementation moves turns the change that follows into a refactor with an oracle.
//!
//! **Two sources of rows.** `generated` states every family `curios-algebra`'s law table declares, at every carrier declaring it, and holds each instance against the carrier's semantics at closed values as well (`semantics`). `written` states the rest by hand: the laws no family states, the controls beside the laws a rule must stop short of, and the refused candidates. A law a family states is not also written.
//!
//! Every refused row is a candidate law or a control, never a bug. A candidate needs a rule in `curios-core`'s `reduce::intrinsic`, which both checkers share, so taking one is an addition to the trusted base and is recorded in `documentation/soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md` beside the grid that probes it over values. A control is a claim that is *not* a law, stated beside the held rows whose rule must stop short of it and marked as one where it stands: a rule widened past its soundness moves a control to the held side, and the goal test names it.

use super::typecheck;

mod audit;
mod generated;
mod semantics;
mod written;

/// The last goal of every program the goal test states: trivially closed by `refl`, so its candidate line is evidence that the search still had budget when the refused rows before it were answered.
const SENTINEL: &str = "Eq(0, 0)";

const IMPORTS: &str = "use /std/{Nat, Int, Bool, Byte, Bytes, Bits, List, Str, Char, Flt, Eq, Io};";

/// One program stating each row — its binders, and its claim — as an item with `body`, in order, over a unit tail.
fn program(rows: &[(String, String)], body: &str) -> String {
    let items = rows
        .iter()
        .enumerate()
        .map(|(index, (binders, claim))| format!("let law{index}({binders}) -> {claim} = {body};"))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{IMPORTS}\n{items}\nIo/pure(())")
}

/// Whether every row closes by `Eq/refl()`, through both checkers: one program for all of them, and on its failure one per row, so the report names the rows that failed.
fn closes(rows: &[(String, String)]) -> Result<(), Vec<String>> {
    let body = "Eq/refl()";
    if typecheck(&program(rows, body)).is_ok() {
        return Ok(());
    }
    Err(rows
        .iter()
        .filter_map(|row| {
            typecheck(&program(std::slice::from_ref(row), body))
                .err()
                .map(|error| format!("`{}`: {error}", row.1))
        })
        .collect())
}

/// The rows of one program the compiler puts on the other side than stated: every row as a written goal, then the sentinel, read back through the candidate line. The first `held` rows are stated as held and the rest as refused.
fn misplaced(name: &str, rows: &[(String, String)], held: usize) -> Vec<String> {
    let mut rows = rows.to_vec();
    rows.push((String::new(), SENTINEL.to_owned()));
    let error =
        typecheck(&program(&rows, "?")).expect_err("a program of written goals never compiles");
    let reports = error.split("goal `?`").skip(1).collect::<Vec<_>>();
    assert_eq!(
        reports.len(),
        rows.len(),
        "{name}: one report per row, got:\n{error}"
    );

    let mut misplaced = Vec::new();
    for (index, ((_, row), report)) in rows.iter().zip(reports).enumerate() {
        let fits = report.contains("? \u{2248} Eq/refl()");
        if index + 1 == rows.len() {
            if !fits {
                misplaced.push(format!(
                    "{name}: the candidate search ran dry before the sentinel, so the refused rows above it were answered by an empty budget and not by the normalizer — split the carrier"
                ));
            }
            continue;
        }
        let is_held = index < held;
        if fits != is_held {
            misplaced.push(format!(
                "{name}: `{row}` is {} but the compiler {} close it by refl",
                if is_held { "held" } else { "refused" },
                if fits { "does" } else { "does not" }
            ));
        }
    }
    misplaced
}
