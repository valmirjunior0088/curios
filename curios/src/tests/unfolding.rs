//! What a combinator web costs to compile, and where.
//!
//! Two measurements of what a combinator web can drive a compilation to spend: how a case refinement is keyed and what an index inversion reduces, and how a reified closure is shared — the Cont fixpoint's cost is `tests::fixpoint`'s. They are kept in code rather than prose because a figure taken by a throwaway script does not survive being re-measured.
//!
//! None asserts. A measurement that fails is a measurement with an opinion, and what these report is a cost, not a contract — see `curios-prelude-archive`'s `stored_prelude_measurements`, whose shape this follows.

use {
    super::ersd_optm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude, typecheck_with_prelude_measured},
    curios_text::{Entrypoint, RootSource},
    std::{fmt::Write, time::Instant},
};

/// Where a rule's inner combinator applications sit. The grammar is identical in all four; only where the applications are written moves.
#[derive(Clone, Copy)]
enum Inner {
    /// No combinator is applied inside a continuation at all.
    None,
    /// `Parse/many0(prev)` and `Parse/sep_by0(prev, eq)` written where they are used — inside a `!` continuation, which is a block.
    InBlock,
    /// The same two applications, named as top-level items and referenced from the continuation.
    Hoisted,
}

/// A grammar of `rules` rules in the `/std/Json/decode` idiom: each rule is a top-level `Parse` definition built from a `!` chain over the two rules before it.
fn grammar(rules: usize, inner: Inner) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Byte, Bytes, Bool, List, Option, Result, Parse, Io};\n\n\
         let ws: Parse(Bytes, Bytes) = Parse/bytes/take_while((b) => b == 0x20);\n\
         let dig: Parse(Bytes, Bytes) = Parse/bytes/take_while((b) => 0x30 <= Byte/to_nat(b));\n\
         let eq: Parse(Bytes, Bytes) = Parse/map(Parse/bytes/literal(\"=\"), (_) => Str/to_bytes(\"=\"));\n\n",
    );

    for rule in 0..rules {
        let previous = if rule >= 1 {
            format!("r{}", rule - 1)
        } else {
            "dig".to_string()
        };
        let older = if rule >= 2 {
            format!("r{}", rule - 2)
        } else {
            "ws".to_string()
        };

        let (many, separated) = match inner {
            Inner::None => (None, None),
            Inner::InBlock => (
                Some(format!("Parse/many0({previous})")),
                Some(format!("Parse/sep_by0({previous}, eq)")),
            ),
            Inner::Hoisted => {
                let _ = writeln!(
                    source,
                    "let m{rule}: Parse(Bytes, List(Bytes)) = Parse/many0({previous});"
                );
                let _ = writeln!(
                    source,
                    "let s{rule}: Parse(Bytes, List(Bytes)) = Parse/sep_by0({previous}, eq);"
                );
                (Some(format!("m{rule}")), Some(format!("s{rule}")))
            }
        };

        let _ = writeln!(source, "let r{rule}: Parse(Bytes, Bytes) =");
        let _ = writeln!(source, "    let x0 = {previous}!;");
        let _ = writeln!(source, "    let x1 = {older}!;");
        let _ = writeln!(source, "    let x2 = {previous}!;");
        if let (Some(many), Some(separated)) = (many, separated) {
            let _ = writeln!(source, "    let m = {many}!;");
            let _ = writeln!(source, "    let s = {separated}!;");
        }
        let _ = writeln!(source, "    let e = eq!;");
        let _ = writeln!(source, "    Parse/pure(x0);");
    }

    let _ = writeln!(source, "\nlet top: Parse(Bytes, Bytes) = r{};\n", rules - 1);
    source.push_str(ENTRY);
    source
}

/// The entry every grammar shares: a runtime-tainted input, so nothing folds the parse away.
const ENTRY: &str = r#"let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Parse/run(top, bytes): (_) => Io({})
    | success(out) =>
        match Str/of_bytes(out): (_) => Io({})
        | some(s) => /std/print(s)
        | none() => /std/print("invalid utf-8\n")
        end
    | failure(e) => /std/print(e.message)
    end
| none() => /std/print("no input\n")
end
"#;

/// The tail every measured program here shares: one that *infers*.
///
/// No `!`, unlike [`ENTRY`]: these programs are put to the two checkers through `typecheck_with_prelude_measured`, which elaborates an unannotated entrypoint in `Mode::Infer` where the compile path checks it against `Io({})` — so a top-level `!` has no region type to read its monad from. Nothing measured here needs the runtime taint either, since both checkers walk every declaration whether the entry reaches it or not.
const TAIL: &str = "/std/print(\"ok\")\n";

/// How many emitted functions carry `needle` in their debug name — one copy of a source function per hit.
fn copies(module: &curios_ersd::Module, needle: &str) -> usize {
    module
        .functions()
        .iter()
        .flatten()
        .filter(|function| {
            function
                .debug_name
                .as_deref()
                .is_some_and(|name| name.contains(needle))
        })
        .count()
}

/// Every emitted function, live slots only.
fn emitted(module: &curios_ersd::Module) -> usize {
    module.functions().iter().flatten().count()
}

/// How a web's value is consumed — the axis that decides whether the kernel ever demands it.
#[derive(Clone, Copy)]
pub(super) enum Consumed {
    /// Applied to the declaration's own binder and returned. Nothing demands the value, so nothing reduces the web.
    Applied,
    /// Scrutinized by a `match` at that binder, so the case equation's subject mentions a local.
    Scrutinized,
    /// Scrutinized by a `match` at a literal. The equation's subject is local-free, which is the control separating *a scrutinee* from *a scrutinee mentioning a binder* — the local-free path has the evaluation memos and the closed machine, and the local-bearing one has neither.
    ScrutinizedClosed,
    /// Named in the index of an `Eq` the declaration takes a proof of, and eliminated. No case equation is registered for the web at all — the scrutinee is the proof, a bare variable — and the web is reduced anyway, by `invert_indices` unifying `(top(n), true)` against `refl`'s `(z, z)` through `Judge::convert_at`.
    ///
    /// The second door onto the same reduction, and the one no refinement key reaches, in both checkers; what keeps it flat is weak-head reduction's and the closed machine's, not the inverter's, as [`scrutinee_refinement_measurements`] states. [`numerics`] is the same door over `Nat`, whose fold laws read both operands.
    Proved,
}

/// The declaration that consumes a web named `top{suffix}`, which is the only thing that differs between the arms of both measurements below.
fn consumer(suffix: &str, consumed: Consumed) -> String {
    match consumed {
        Consumed::Applied => format!("let probe{suffix}(n: Nat) -> Bool = top{suffix}(n);\n\n"),
        Consumed::Scrutinized => format!(
            "let probe{suffix}(n: Nat) -> Str =\n    match top{suffix}(n): (_) => Str | true => \"y\" | false => \"n\" end;\n\n"
        ),
        Consumed::ScrutinizedClosed => format!(
            "let probe{suffix}(n: Nat) -> Str =\n    match top{suffix}(7): (_) => Str | true => \"y\" | false => \"n\" end;\n\n"
        ),
        Consumed::Proved => format!(
            "let probe{suffix}(n: Nat, e: Eq()(top{suffix}(n), true)) -> Str =\n    match e: (_, _, _) => Str | refl(@z) => \"y\" end;\n\n"
        ),
    }
}

/// A web of `rules` predicate definitions consumed as `consumed` says, with each definition naming the one before it once or twice.
pub(super) fn predicates(rules: usize, consumed: Consumed, twice: bool) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Bool, Eq};\n\n\
         let Pred: Type = (x: Nat) -> Bool;\n\
         let both(p: Pred, q: Pred) -> Pred = (x) => p(x) && q(x);\n\
         let anyof(p: Pred, q: Pred) -> Pred = (x) => p(x) || q(x);\n\
         let base: Pred = (x) => x % 2 == 0;\n\
         let other: Pred = (x) => x % 3 == 0;\n\n",
    );

    for rule in 0..rules {
        let previous = if rule >= 1 {
            format!("r{}", rule - 1)
        } else {
            "base".to_string()
        };
        let older = if rule >= 2 {
            format!("r{}", rule - 2)
        } else {
            "other".to_string()
        };
        let second = match twice {
            true => previous.clone(),
            false => "base".to_string(),
        };
        let _ = writeln!(
            source,
            "let r{rule}: Pred = both({previous}, anyof({older}, {second}));"
        );
    }

    let _ = writeln!(source, "\nlet top: Pred = r{};\n", rules - 1);

    source.push_str(&consumer("", consumed));

    source.push_str(TAIL);
    source
}

/// Compile for its verdict and its wall clock alone.
fn compile_only(source: &str) -> (Result<(), String>, f64) {
    let entrypoint = source.parse::<Entrypoint>().expect("probe parses");
    let start = Instant::now();
    let outcome = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .map(|_| ())
    .map_err(|error| error.to_string());

    (outcome, start.elapsed().as_secs_f64())
}

/// What a combinator application costs when it is written inside a `!` continuation rather than at item level.
///
/// # How to take it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture combinator_sharing_measurements
/// ```
///
/// Release only. The wall clocks are dominated by `curios_cont::optimize`, which a debug build prices differently; the *counts* are deterministic and hold in either profile.
///
/// # What it last printed
///
/// **Release**, `aarch64-apple-darwin`.
///
/// | spelling | `Parse/bind` copies at 16 rules | emitted functions | compile | growth of copies |
/// | --- | --- | --- | --- | --- |
/// | no application inside a continuation | 18 | 166 | 0.69 s | `n + 2` |
/// | **as written** — applications inside the continuation | **18** | **262** | **1.52 s** | **`n + 2`** |
/// | applications hoisted to items | 18 | 262 | 1.54 s | `n + 2` |
///
/// **Where the application is written decides nothing.** The as-written row and the hoisted row agree to the copy, to the function, and to within a hundredth of a second, at every size — 4/108, 6/130, 10/174, 14/218, 18/262. The hoisted spelling is the sharing emulated in source and therefore a ceiling, and the as-written spelling reaches it because a replacement's residual group is bound at item level. Bound inside the candidate's block, the as-written row grows as `n² + 2`.
///
/// # Where the time goes
///
/// Write `grammar(16, Inner::InBlock)` and `grammar(16, Inner::Hoisted)` to files and take each under the profiler:
///
/// ```sh
/// cargo xtask profile --profile release <file>
/// ```
///
/// Same host:
///
/// | | as written | hoisted |
/// | --- | --- | --- |
/// | `compile_entrypoint` | 1 043 ms | 1 027 ms |
/// | `cont_optimize` | 891 ms | 874 ms |
/// | `cont_optimize` allocations | 7 889 915 | 7 889 915 |
/// | `evaluate_closed_terms` | 12 ms | 12 ms |
///
/// **The allocation counts are identical**, which says more than the wall clocks: the two spellings hand the fixpoint the same module, so what it does with them cannot differ. `evaluate_closed_terms`, the pass this measurement is about, is never the cost; what it *produces* can be, because the fixpoint below it prices the module it is handed — `tests::fixpoint` carries that measurement.
#[test]
#[ignore = "measurement: reports what a spelling costs rather than asserting"]
fn combinator_sharing_measurements() {
    println!("rules  inner            bind copies  functions  compile");
    for inner in [Inner::None, Inner::InBlock, Inner::Hoisted] {
        let label = match inner {
            Inner::None => "none",
            Inner::InBlock => "in a continuation",
            Inner::Hoisted => "hoisted to items",
        };
        for rules in [2usize, 4, 8, 12, 16] {
            let source = grammar(rules, inner);
            let start = Instant::now();
            let module = ersd_optm(&source);
            let elapsed = start.elapsed().as_secs_f64();
            println!(
                "{rules:<6} {label:<16} {:<12} {:<10} {elapsed:.2} s",
                copies(&module, "/std/Parse/bind"),
                emitted(&module),
            );
        }
    }
}

/// What a web of combinator definitions costs to compile, and what consuming its value adds to that.
///
/// # How to take it
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture scrutinee_refinement_measurements
/// ```
///
/// # What it last printed
///
/// **Debug**, `aarch64-apple-darwin`. Debug rather than release deliberately: debug is the canary for exhaustion that release hides, and the 3.3 s every row shares is the debug prelude restore plus a trivial compile — read the columns against each other.
///
/// | definitions | applied | scrutinized | proved | numeric, proved |
/// | --- | --- | --- | --- | --- |
/// | 8 | 3.57 s | 3.33 s | 3.36 s | 3.35 s |
/// | 10 | 3.27 s | 3.33 s | 3.35 s | 3.35 s |
/// | 12 | 3.30 s | 3.32 s | 3.35 s | 3.37 s |
/// | 13 | 3.30 s | 3.33 s | 3.37 s | 3.37 s |
///
/// **Every door is flat.** The `once` and `closed` scrutinized columns read the same and are omitted. `a_ten_definition_numeric_web_compiles` and `a_symbolic_web_compares_against_zero_in_linear_units` are the fixtures that refuse a regression.
///
/// Each door stays flat for a reason held where it stands. A case refinement is keyed at the written spelling, so keying never demands the scrutinee's reduction — a web nothing reduces is flat however it fans out, and what makes a reduction unaffordable is a subject that mentions a binder, the term `Memos::storable` may not remember and the closed machine may not take. `&&` and `||` reduce their right operand only behind a literal left, and the closed machine keeps a global argument as a name rather than substituting its value, both in `curios-core`, which the two checkers share. A `Nat` fold must read its right operand for its identity laws, so its web's weak-head form is its normalization: it stays linear because the kernel's `Memos` keeps a local-bearing reduct while the equations in force stand, the elaborator's cache admits a universe metavariable, the kernel's `capture` is memoized on node and depth, `Term::eq` remembers the pairs it has entered, and a product of two symbolic sums is its own weak-head form, distributed only where a value is asked for.
///
/// The programs end in a plain `/std/print("ok")` rather than a runtime-tainted parse, so these wall clocks are the two checkers and nothing downstream of them.
#[test]
#[ignore = "measurement: reports what a scrutinee costs rather than asserting"]
fn scrutinee_refinement_measurements() {
    println!(
        "{:<12} {:<7} {:<18} {:<9} outcome",
        "definitions", "named", "consumed", "compile"
    );

    for (twice, consumed, label) in [
        (true, Consumed::Applied, "applied"),
        (false, Consumed::Scrutinized, "scrutinized"),
        (true, Consumed::Scrutinized, "scrutinized"),
        (true, Consumed::ScrutinizedClosed, "scrutinized, closed"),
        (true, Consumed::Proved, "proved"),
    ] {
        let sizes: &[usize] = match consumed {
            Consumed::Proved => &[8, 10, 12, 13],
            _ => &[8, 10, 12, 13, 14, 20],
        };

        for &rules in sizes {
            let (outcome, elapsed) = compile_only(&predicates(rules, consumed, twice));
            let verdict = match &outcome {
                Ok(()) => "compiled".to_string(),
                Err(error) => error.lines().next().unwrap_or("refused").to_string(),
            };
            let named = match twice {
                true => "twice",
                false => "once",
            };

            println!("{rules:<12} {named:<7} {label:<18} {elapsed:>7.2} s  {verdict}");
        }
    }

    // The same web over `Nat`, proved — the shape a `Bool` fold's short-circuit does not reach, because a `Nat` fold must read its right operand for its identity laws where a `Bool` fold behind a stuck left need not.
    for &rules in &[8usize, 10, 12, 13] {
        let (outcome, elapsed) = compile_only(&numerics(rules));
        let verdict = match &outcome {
            Ok(()) => "compiled".to_string(),
            Err(error) => error.lines().next().unwrap_or("refused").to_string(),
        };
        println!(
            "{rules:<12} {:<7} {:<18} {elapsed:>7.2} s  {verdict}",
            "twice", "numeric, proved"
        );
    }
}

/// The [`predicates`] web carried over `Nat` — `+` for `both`, `*` for `anyof`, remainders for the leaves — and proved at `Eq()(top(n), 0)`. Each rule names the one before it twice, as the `twice` arm of [`predicates`] does.
fn numerics(rules: usize) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Eq};\n\n\
         let Fn: Type = (x: Nat) -> Nat;\n\
         let both(p: Fn, q: Fn) -> Fn = (x) => p(x) + q(x);\n\
         let anyof(p: Fn, q: Fn) -> Fn = (x) => p(x) * q(x);\n\
         let base: Fn = (x) => x % 2;\n\
         let other: Fn = (x) => x % 3;\n\n",
    );

    for rule in 0..rules {
        let previous = if rule >= 1 {
            format!("r{}", rule - 1)
        } else {
            "base".to_string()
        };
        let older = if rule >= 2 {
            format!("r{}", rule - 2)
        } else {
            "other".to_string()
        };
        let _ = writeln!(
            source,
            "let r{rule}: Fn = both({previous}, anyof({older}, {previous}));"
        );
    }

    let _ = writeln!(source, "\nlet top: Fn = r{};\n", rules - 1);
    source.push_str(
        "let probe(n: Nat, e: Eq()(top(n), 0)) -> Str =\n    match e: (_, _, _) => Str | refl(@z) => \"y\" end;\n\n",
    );
    source.push_str(TAIL);
    source
}

/// **The decision's own probe.** Deciding `Eq()(top(n), 0)` for a symbolic `n` needs the sum's head, not its normal form: a stuck sum whose summands are not literal zero is not zero. Eager folding would build the linear combination first — ~φ²ⁿ monomials, since the web's degree is Fibonacci in its size — and the units would grow with it. The folds answer the weak-head form and the peel clashes from the head, so the units grow with the weak-head DAG, which memoization keeps linear in `n`. The control is the increment: each further definition costs about what the previous one did.
#[test]
fn a_symbolic_web_compares_against_zero_in_linear_units() {
    let units = |rules: usize| {
        let source = numerics(rules);
        let entrypoint = source.parse::<Entrypoint>().expect("the web parses");
        let (_, _, consumption) =
            typecheck_with_prelude_measured(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
                .expect("the web elaborates within the default budget");
        consumption.units()
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
    let (outcome, _) = compile_only(&numerics(10));

    assert_eq!(outcome, Ok(()), "the ten-definition numeric web compiles");
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
    let absent = copies(&ersd_optm(&grammar(8, Inner::None)), "/std/Parse/bind");
    let inside = copies(&ersd_optm(&grammar(8, Inner::InBlock)), "/std/Parse/bind");
    let hoisted = copies(&ersd_optm(&grammar(8, Inner::Hoisted)), "/std/Parse/bind");

    assert_eq!(
        inside, hoisted,
        "where a combinator application is written must not decide what it costs"
    );
    assert_eq!(
        inside, absent,
        "a shared application must add no copy over not writing it at all"
    );
}
