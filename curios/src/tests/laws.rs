//! The definitional-law grid: which equations over the intrinsic carriers the normalizer closes by computation, and which it refuses, stated so that both halves are checked.
//!
//! A held row is a law both checkers decide for every value, so it is stated as an `Eq/refl()` proof and compiled. A refused row is a law the normalizer does *not* take — kept here rather than in prose so the refused set is a record rather than a rumor, and so that taking one later is a row moving, not a test appearing. The refused half cannot be stated as a proof, so both halves are also stated as written goals and read back through the `? ≈ Eq/refl()` candidate line, which is the compiler's own answer to "does refl fit here": a held row must get the line and a refused row must not. The two directions check each other — if the candidate search stopped reporting, every held row would fail the goal test. That alone does not keep a refused row from passing vacuously, because the search can stop *partway*: every goal of one program draws its candidate attempts from a single budget, so a program with enough goals runs it dry and every goal after that point reports no candidate at all — which is exactly what a refused row is expected to show, and the refused rows come last. So each program ends on a sentinel, a row `refl` trivially closes, stated after the refused ones: a sentinel without its line means the search ran dry before the refused rows were reached, and the test says so rather than passing them.
//!
//! **State a row at every carrier, and state it first.** A grid stating a law at one carrier and not another passes exactly as a complete one does, and what it hides is invisible from reading the two implementations side by side: such an incompleteness is found by stating the row rather than by inspection. A row stated before an implementation moves turns the change that follows into a refactor with an oracle.
//!
//! **Two sources of rows.** `generated` states every family `curios-algebra`'s law table declares, at every carrier declaring it, and holds each instance against the carrier's semantics at closed values as well (`semantics`). `written` states the rest by hand: the laws no family states, the controls beside the laws a rule must stop short of, and the refused candidates. A law a family states is not also written.
//!
//! Every refused row is a candidate law or a control, never a bug. A candidate needs a rule in `curios-core`'s `reduce::intrinsic`, which both checkers share, so taking one is an addition to the trusted base and is recorded in `documentation/design/soundness/conversion/intrinsic-fold-laws-and-the-free-monoid-peel.md` beside the grid that probes it over values. A control is a claim that is *not* a law, stated beside the held rows whose rule must stop short of it and marked as one where it stands: a rule widened past its soundness moves a control to the held side, and the goal test names it.
//!
//! **Each checker is asked by itself.** The candidate line is the elaborator's answer, and a compilation asks the kernel only once the elaborator accepts, so neither says what the kernel makes of a row the elaborator refuses: a kernel that held a control would pass both. [`kernel_alone`] puts every row to the kernel with the elaborator's verdict left out, and a refused row must be refused there too, as a mismatch — a budget that ran out compared nothing.

use {
    super::typecheck,
    curios_cert::Error,
    curios_core::{Definition, FuncType, Global, Item, Subterm, Term, Zonked},
    curios_pipeline::{DEFAULT_STEP_BUDGET, recheck_with_prelude, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::Qualifier,
};

mod audit;

mod generated;
use generated::*;

mod semantics;
use semantics::*;

mod written;

/// The last goal of every program the goal test states: trivially closed by `refl`, so its candidate line is evidence that the search still had budget when the refused rows before it were answered.
const SENTINEL: &str = "Eq()(0, 0)";

const IMPORTS: &str = "use /std/{Nat, Int, Bool, Byte, Bytes, Bits, List, Str, Char, Flt, Eq, Io}; use /std/Bool/{Holds};";

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

/// What the kernel says of each row when it alone is asked, in the rows' order: `None` where it holds the row, and its refusal where it does not.
///
/// One program states each row twice: `stated`, a function into `Prop` whose body is the claim, and `proved`, `Eq/refl()` at the claim's left side against itself. Both elaborate whatever the row's side, and [`typecheck_with_prelude`] stops short of the kernel. The claim the kernel then judges takes `stated`'s lambda as its type, a function type over the same telescope, and `proved`'s as its body: every term in it is one the elaborator built, and the one comparison it leaves the kernel is the row's left side against its right, at their type.
fn kernel_alone(rows: &[(String, String)]) -> Vec<Option<Error>> {
    let items = rows
        .iter()
        .enumerate()
        .map(|(index, (binders, claim))| {
            let (left, _) = sides(claim);
            format!(
                "let stated{index}({binders}) -> Prop = {claim};\nlet proved{index}({binders}) -> Eq()({left}, {left}) = Eq/refl();"
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let entrypoint = format!("{IMPORTS}\n{items}\nIo/pure(())")
        .parse::<Entrypoint>()
        .expect("the rows parse");
    let mut program = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
        .unwrap_or_else(|refused| panic!("a row's statement does not elaborate:\n{refused}"))
        .program;

    let named = |name: String| Global::Authored(Qualifier::from([name]));
    let claims = (0..rows.len())
        .map(|index| {
            let definition = |name: Global| {
                program
                    .module
                    .items
                    .iter()
                    .find_map(|item| match item {
                        Item::Let(definition) if definition.name == name => Some(definition),
                        _ => None,
                    })
                    .unwrap_or_else(|| panic!("the program defines {name}"))
            };
            let stated = definition(named(format!("stated{index}")));
            let proved = definition(named(format!("proved{index}")));
            // Both are stated under one binder list, so the elaborator generalizes them alike; a claim typed under one context and proved under another would be refused for its levels and say nothing of the row.
            assert_eq!(stated.universe_context, proved.universe_context);
            let Subterm::Func(function) = &*stated.body else {
                panic!("a row's statement is a function of its binders");
            };

            Definition {
                name: named(format!("claim{index}")),
                type_: Term::from(Subterm::FuncType(FuncType::new(
                    function.telescope.clone(),
                    function.plicities().to_vec(),
                ))),
                ..proved.clone()
            }
        })
        .collect::<Vec<_>>();
    let names = claims.iter().map(|claim| claim.name).collect::<Vec<_>>();
    program
        .module
        .items
        .extend(claims.into_iter().map(Item::Let));

    let program = Zonked::project(&program).expect("an elaborated program is zonked");
    let verdicts = recheck_with_prelude(&program, DEFAULT_STEP_BUDGET);
    // The statements and the reflexivity proofs are the elaborator's own, so a refusal of anything but a claim is the reader's fault and not a row's verdict.
    for verdict in &verdicts {
        assert!(
            verdict.name.is_some_and(|name| names.contains(&name)),
            "the kernel refuses more than a row's claim: {verdict:?}"
        );
    }

    names
        .iter()
        .map(|name| {
            verdicts
                .iter()
                .find(|verdict| verdict.name == Some(*name))
                .map(|verdict| verdict.error.clone())
        })
        .collect()
}

/// The rows the kernel, asked alone, puts on the other side than stated: the first `held` are stated as held and the rest as refused. A refused row counts as refused only where the kernel compares its sides and finds them apart, so one it refuses any other way — a spent budget among them — is reported too.
fn misplaced_by_the_kernel(name: &str, rows: &[(String, String)], held: usize) -> Vec<String> {
    kernel_alone(rows)
        .into_iter()
        .zip(rows)
        .enumerate()
        .filter_map(|(index, (refusal, (_, row)))| match (index < held, refusal) {
            (true, None) | (false, Some(Error::Mismatch { .. })) => None,
            (true, Some(error)) => Some(format!(
                "{name}: `{row}` is held but the kernel, asked alone, refuses it: {error}"
            )),
            (false, None) => Some(format!(
                "{name}: `{row}` is refused but the kernel, asked alone, holds it"
            )),
            (false, Some(error)) => Some(format!(
                "{name}: `{row}` is refused, but the kernel, asked alone, does not refuse it as a mismatch: {error}"
            )),
        })
        .collect()
}

/// The two sides of a claim `Eq()(left, right)`.
fn sides(claim: &str) -> (&str, &str) {
    let inner = claim
        .strip_prefix("Eq()(")
        .and_then(|rest| rest.strip_suffix(')'))
        .unwrap_or_else(|| panic!("`{claim}` is no equation"));
    match top_level(inner)[..] {
        [left, right] => (left, right),
        _ => panic!("`{claim}` does not have two sides"),
    }
}

/// A comma-separated list's items: split at its top-level commas, those outside every bracket and every quoted literal, so a binder's proposition or a claim's side stays whole.
fn top_level(list: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let (mut depth, mut start) = (0usize, 0usize);
    let mut quote = None;
    let mut characters = list.char_indices();
    while let Some((at, character)) = characters.next() {
        match (quote, character) {
            (Some(_), '\\') => {
                characters.next();
            }
            (Some(open), _) if character == open => quote = None,
            (Some(_), _) => {}
            (None, '"' | '\'') => quote = Some(character),
            (None, '(' | '[' | '{') => depth += 1,
            (None, ')' | ']' | '}') => depth -= 1,
            (None, ',') if depth == 0 => {
                parts.push(list[start..at].trim());
                start = at + 1;
            }
            (None, _) => {}
        }
    }
    parts.push(list[start..].trim());
    parts.retain(|part| !part.is_empty());
    parts
}

/// One binder of a telescope the sweep reorders: its name, its type, and the binders its type names.
struct Binder<'a> {
    name: &'a str,
    type_: &'a str,
    needs: &'a [&'a str],
}

/// `count` orders of `binders`, each a telescope as source: the order they are declared in, that order reversed, and shuffles drawn from a fixed sequence, so every run states the same orders. A binder is kept after the binders its type names, by taking from each candidate order the first binder whose needs are already placed.
///
/// **An order is a verdict's adversary.** A binder's identity is minted where it is declared, so a structural hash — the order a product holds its factors in, the side a linear view puts an atom on — moves with the declaration order, and a verdict that rests on a hash holds at one order and fails at another. Two fixed orders cannot tell such a verdict from a law; a sweep can.
fn orders(binders: &[Binder<'_>], count: usize) -> Vec<String> {
    let declared = (0..binders.len()).collect::<Vec<_>>();
    let mut state: u64 = 0x9E37_79B9_7F4A_7C15;
    let mut draw = move |below: usize| {
        state = state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        (state >> 33) as usize % below
    };
    (0..count)
        .map(|order| {
            let mut candidate = declared.clone();
            match order {
                0 => {}
                1 => candidate.reverse(),
                _ => {
                    for at in (1..candidate.len()).rev() {
                        candidate.swap(at, draw(at + 1));
                    }
                }
            }
            let mut placed: Vec<usize> = Vec::new();
            while !candidate.is_empty() {
                let next = candidate
                    .iter()
                    .position(|index| {
                        binders[*index]
                            .needs
                            .iter()
                            .all(|need| placed.iter().any(|done| binders[*done].name == *need))
                    })
                    .expect("a binder's needs are binders of the telescope, with no cycle");
                placed.push(candidate.remove(next));
            }
            placed
                .iter()
                .map(|index| format!("{}: {}", binders[*index].name, binders[*index].type_))
                .collect::<Vec<_>>()
                .join(", ")
        })
        .collect()
}
