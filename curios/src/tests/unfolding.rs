//! What a combinator web costs to compile, and where.
//!
//! Two measurements, for the two specifications that lean on them: `documentation/roadmap/technical_debts/02-point-free-unfolding-spec.md` for how a reified closure is shared, and `documentation/roadmap/technical_debts/01-kernel-scrutinee-key-spec.md` for how a case refinement is keyed. Both are here rather than in prose because both specifications were preceded by a document whose figures were taken by a throwaway script, and none of that document's three load-bearing claims survived being re-measured.
//!
//! Neither asserts. A measurement that fails is a measurement with an opinion, and what these report is a cost, not a contract — see `curios-prelude-archive`'s `stored_prelude_measurements`, whose shape this follows.

use {
    super::ersd_optm,
    curios_pipeline::{DEFAULT_STEP_BUDGET, compile_with_prelude},
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
    /// Written inside the continuation as [`Inner::InBlock`], with every rule additionally eta-expanded through a `Parse { run = … }` so no fold reaches it — what a `delay` combinator would give a user.
    EtaInBlock,
}

/// A grammar of `rules` rules in the `/std/Json/decode` idiom: each rule is a top-level `Parse` definition built from a `!` chain over the two rules before it.
fn grammar(rules: usize, inner: Inner) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Byte, Bytes, Bool, List, Option, Result, Parse, Io};\n\n\
         let ws: Parse(Bytes) = Parse/take_while((b) => b == 0x20);\n\
         let dig: Parse(Bytes) = Parse/take_while((b) => 0x30 <= Byte/to_nat(b));\n\
         let eq: Parse(Bytes) = Parse/map(Parse/take_literal(\"=\"), (_) => Str/to_bytes(\"=\"));\n\n",
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

        let eta = matches!(inner, Inner::EtaInBlock);
        let (many, separated) = match inner {
            Inner::None => (None, None),
            Inner::InBlock | Inner::EtaInBlock => (
                Some(format!("Parse/many0({previous})")),
                Some(format!("Parse/sep_by0({previous}, eq)")),
            ),
            Inner::Hoisted => {
                let _ = writeln!(
                    source,
                    "let m{rule}: Parse(List(Bytes)) = Parse/many0({previous});"
                );
                let _ = writeln!(
                    source,
                    "let s{rule}: Parse(List(Bytes)) = Parse/sep_by0({previous}, eq);"
                );
                (Some(format!("m{rule}")), Some(format!("s{rule}")))
            }
        };

        let head = match eta {
            true => format!("r{rule}_body"),
            false => format!("r{rule}"),
        };
        let _ = writeln!(source, "let {head}: Parse(Bytes) =");
        let _ = writeln!(source, "    let x0 = {previous}!;");
        let _ = writeln!(source, "    let x1 = {older}!;");
        let _ = writeln!(source, "    let x2 = {previous}!;");
        if let (Some(many), Some(separated)) = (many, separated) {
            let _ = writeln!(source, "    let m = {many}!;");
            let _ = writeln!(source, "    let s = {separated}!;");
        }
        let _ = writeln!(source, "    let e = eq!;");
        let _ = writeln!(source, "    Parse/pure(x0);");
        if eta {
            let _ = writeln!(
                source,
                "let r{rule}: Parse(Bytes) = Parse {{ run(input, pos) = r{rule}_body.run(input, pos) }};"
            );
        }
    }

    let _ = writeln!(source, "\nlet top: Parse(Bytes) = r{};\n", rules - 1);
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
    | failure(m) => /std/print(m)
    end
| none() => /std/print("no input\n")
end
"#;

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

/// A web of `rules` predicate definitions consumed by a match or by a bare application, with each definition naming the one before it once or twice.
fn predicates(rules: usize, scrutinize: bool, twice: bool) -> String {
    let mut source = String::from(
        "use /std/{Str, Nat, Bool, Option, Io};\n\n\
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

    // The only difference between the two probes: whether the web's value reaches a match scrutinee.
    source.push_str(match scrutinize {
        true => {
            "let probe(n: Nat) -> Str =\n    match top(n): (_) => Str | true => \"y\" | false => \"n\" end;\n\n"
        }
        false => "let probe(n: Nat) -> Bool = top(n);\n\n",
    });

    source.push_str(
        "let input = /std/read()!;\n\
         match input: (_) => Io({})\n\
         | some(_) => /std/print(\"ok\\n\")\n\
         | none() => /std/print(\"none\\n\")\n\
         end\n",
    );
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
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`, at the commit that landed the reification memo.
///
/// | spelling | `Parse/bind` copies at 16 rules | emitted functions | compile | growth of copies |
/// | --- | --- | --- | --- | --- |
/// | no application inside a continuation | 18 | 166 | 0.52 s | `n + 2` |
/// | applications hoisted to items | 18 | 262 | 1.19 s | `n + 2` |
/// | every rule eta-expanded, applications left in place | 18 | 312 | 4.39 s | `n + 2` |
/// | **as written** — applications inside the continuation | **258** | 566 | **23.21 s** | **`n² + 2`** |
///
/// The quadratic is exact over `n` ∈ {2, 4, 8, 12, 16}: 6, 18, 66, 146, 258. The other three are exactly `n + 2` — hoisting the applications costs precisely what never writing them inside a continuation costs.
///
/// **Hoisting beats eta-expanding**, which is what says the cure is sharing rather than refusal: 1.19 s against 4.39 s and 262 functions against 312, for the same grammar. Eta-expansion declines the folds; hoisting performs them *and* shares the result.
///
/// # What the landed memo bought
///
/// Taken against a worktree at the commit before the reification memo, with this file grafted in — the two trees differ by exactly the two files that memo touched:
///
/// ```sh
/// git worktree add <dir> <the commit before the memo>
/// cp curios/src/tests/unfolding.rs <dir>/curios/src/tests/unfolding.rs   # and register the module
/// cd <dir> && CARGO_TARGET_DIR=<scratch> cargo test --release --package curios --lib \
///     -- --ignored --nocapture combinator_sharing_measurements
/// ```
///
/// `Parse/bind` copies at each size, before the memo against after:
///
/// | spelling | 2 | 4 | 8 | 12 | 16 | |
/// | --- | --- | --- | --- | --- | --- | --- |
/// | no application in a continuation | 5 | 12 | 38 | 80 | **138** | before |
/// | | 4 | 6 | 10 | 14 | **18** | after |
/// | hoisted to items | 7 | 24 | 94 | 212 | **378** | before |
/// | | 4 | 6 | 10 | 14 | **18** | after |
/// | in a continuation | 7 | 24 | 94 | 212 | **378** | before |
/// | | 6 | 18 | 66 | 146 | **258** | after |
/// | every rule eta-expanded | 4 | 6 | 10 | 14 | 18 | before |
/// | | 4 | 6 | 10 | 14 | 18 | after |
///
/// Three things this says that a before-and-after on one spelling would not.
///
/// **The memo carries the ordinary case, not the pathological one.** On the spelling with no application inside a continuation — the well-behaved one — the series was quadratic and is now exactly `n + 2`; at sixteen rules the module went from 302 emitted functions to 166 and the compile from 3.23 s to 0.52 s. On the pathological spelling it buys 378 → 258, which is real and modest.
///
/// **Before the memo, where the application was written made no difference at all.** `hoisted to items` and `in a continuation` measured identically at every size. The asymmetry between them is something the memo *created*, by reaching item-level candidates and not block-level ones — which is why widening its reach is the cure rather than replacing it.
///
/// **The eta spelling is untouched by the memo**, to the copy, which is the check that it declines the folds rather than sharing them.
///
/// # Where the time goes
///
/// Write `grammar(16, Inner::InBlock)` and `grammar(16, Inner::Hoisted)` to files and take each under the profiler:
///
/// ```sh
/// make curios/profile CURIOS_PROFILE_SOURCE=<file>
/// ```
///
/// Same date and host. `curios_cont::optimize` is **21 998 ms of a 22 193 ms** compile as written — 99.1%, allocating 15.2 GB across 223.8 M allocations — against **602 ms of 737 ms** hoisted, at 458 MB and 6.5 M. `evaluate_closed_terms`, which is the pass this specification is about, is **19 ms** and runs four rounds rather than the eight the driver allows. For calibration, a plain `Toml/decode` program with no point-free code in it spends **4 719 ms of 4 836 ms** in the same pass.
///
#[test]
#[ignore = "measurement: reports what a spelling costs rather than asserting"]
fn combinator_sharing_measurements() {
    println!("rules  inner            bind copies  functions  compile");
    for inner in [
        Inner::None,
        Inner::InBlock,
        Inner::Hoisted,
        Inner::EtaInBlock,
    ] {
        let label = match inner {
            Inner::None => "none",
            Inner::InBlock => "in a continuation",
            Inner::Hoisted => "hoisted to items",
            Inner::EtaInBlock => "in a continuation, eta",
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

/// What the kernel spends re-deriving a case refinement's key, against what the same web costs when its value never reaches a scrutinee.
///
/// # How to take it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_refinement_measurements
/// ```
///
/// # What it last printed
///
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`, at the same commit.
///
/// | definitions | named twice, not scrutinized | named once, scrutinized | named twice, scrutinized |
/// | --- | --- | --- | --- |
/// | 8 | 0.28 s | 0.26 s | 0.29 s |
/// | 10 | 0.26 s | 0.28 s | 0.42 s |
/// | 12 | 0.26 s | 0.28 s | 1.21 s |
/// | 13 | 0.26 s | 0.30 s | 2.60 s |
/// | 14 | 0.27 s | 0.31 s | **refused** — the kernel's reduction budget |
/// | 20 | 0.27 s | 0.99 s | **refused** |
///
/// Both conditions are necessary and neither is sufficient. A web nothing scrutinizes is flat however it fans out; a web that *is* scrutinized costs what its fan-out is — the middle column still names each definition twice across the chain, once as the previous rule and once as the older one, and grows accordingly, just far more slowly than naming it twice within one rule.
///
/// Under `--features profile` at 13 definitions, `recheck` is 2 881 ms of a 3 061 ms compile — 94.1%, allocating 6 955 MB across 74.8 M allocations — against `elaborate_and_zonk`'s 13 ms and 134 k allocations. The two checkers are deciding the same terms; only one of them reduces.
#[test]
#[ignore = "measurement: reports what a scrutinee costs rather than asserting"]
fn scrutinee_refinement_measurements() {
    println!("definitions  named   scrutinized  outcome");
    for (twice, scrutinize) in [(true, false), (false, true), (true, true)] {
        for rules in [8usize, 10, 12, 13, 14, 20] {
            let (outcome, elapsed) = compile_only(&predicates(rules, scrutinize, twice));
            let verdict = match &outcome {
                Ok(()) => "compiled".to_string(),
                Err(error) => error.lines().next().unwrap_or("refused").to_string(),
            };
            let named = match twice {
                true => "twice",
                false => "once",
            };
            println!("{rules:<12} {named:<7} {scrutinize:<12} {elapsed:.2} s  {verdict}");
        }
    }
}
