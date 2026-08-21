//! What a combinator web costs to compile, and where.
//!
//! Three measurements, for the specifications that lean on them: `documentation/roadmap/technical_debts/02-point-free-unfolding-spec.md` for how a reified closure is shared, and `documentation/roadmap/technical_debts/01-kernel-scrutinee-key-spec.md` for how a case refinement is keyed and what filling the retention allowance costs. All three are here rather than in prose because both specifications were preceded by a document whose figures were taken by a throwaway script, and none of that document's three load-bearing claims survived being re-measured.
//!
//! None asserts. A measurement that fails is a measurement with an opinion, and what these report is a cost, not a contract — see `curios-prelude-archive`'s `stored_prelude_measurements`, whose shape this follows.

use {
    super::ersd_optm,
    curios_core::Consumption,
    curios_pipeline::{
        DEFAULT_STEP_BUDGET, compile_with_prelude, recheck_with_prelude_measured,
        typecheck_with_prelude_measured,
    },
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

/// A web of `rules` predicate definitions whose combinators dispatch through a stuck `match` rather than through `&&`, so the web's weak-head normal form is a tower of stuck matches rather than a tree of folded intrinsics.
///
/// Same fan-out as [`predicates`] — each rule names the one before it twice — and the same scrutinized consumer. What differs is the shape reduction has to build, and that is what the step in [`scrutinee_retention_measurements`] is a function of.
fn dispatched(rules: usize, scrutinize: bool, suffix: &str, moduli: (u64, u64)) -> String {
    let (even, odd) = moduli;
    let mut source = format!(
        "let Pred{suffix}: Type = (x: Nat) -> Bool;\n\
         let both{suffix}(p: Pred{suffix}, q: Pred{suffix}) -> Pred{suffix} =\n\
         \x20   (x) => match p(x): (_) => Bool | true => q(x) | false => false end;\n\
         let anyof{suffix}(p: Pred{suffix}, q: Pred{suffix}) -> Pred{suffix} =\n\
         \x20   (x) => match p(x): (_) => Bool | true => true | false => q(x) end;\n\
         let base{suffix}: Pred{suffix} = (x) => x % {even} == 0;\n\
         let other{suffix}: Pred{suffix} = (x) => x % {odd} == 0;\n\n"
    );

    for rule in 0..rules {
        let previous = match rule >= 1 {
            true => format!("r{}{suffix}", rule - 1),
            false => format!("base{suffix}"),
        };
        let older = match rule >= 2 {
            true => format!("r{}{suffix}", rule - 2),
            false => format!("other{suffix}"),
        };
        let _ = writeln!(
            source,
            "let r{rule}{suffix}: Pred{suffix} = both{suffix}({previous}, anyof{suffix}({older}, {previous}));"
        );
    }

    let _ = writeln!(
        source,
        "\nlet top{suffix}: Pred{suffix} = r{}{suffix};\n",
        rules - 1
    );

    let _ = match scrutinize {
        true => writeln!(
            source,
            "let probe{suffix}(n: Nat) -> Str =\n\
             \x20   match top{suffix}(n): (_) => Str | true => \"y\" | false => \"n\" end;\n"
        ),
        false => writeln!(
            source,
            "let probe{suffix}(n: Nat) -> Bool = top{suffix}(n);\n"
        ),
    };

    source
}

/// One or more [`dispatched`] webs under the shared header, with a tail that infers.
///
/// No `!` in the entry, unlike [`ENTRY`]: this probe puts the program to the two checkers through `typecheck_with_prelude_measured`, which elaborates an unannotated entrypoint in `Mode::Infer` where the compile path checks it against `Io({})` — so a top-level `!` has no region type to read its monad from. Nothing here needs the runtime taint either, since both checkers walk every declaration whether the entry reaches it or not.
fn dispatched_program(webs: &[(usize, bool, &str, (u64, u64))]) -> String {
    let mut source = String::from("use /std/{Str, Nat, Bool};\n\n");

    for (rules, scrutinize, suffix, moduli) in webs {
        source.push_str(&dispatched(*rules, *scrutinize, suffix, *moduli));
    }

    source.push_str("/std/print(\"ok\")\n");
    source
}

/// What each checker's heaviest declaration spent on `source` at `budget`, what the kernel's walk retained, and how long the two took together.
fn checker_cost(budget: u64, source: &str) -> (Consumption, Consumption, u64, f64) {
    let entrypoint = source.parse::<Entrypoint>().expect("the probe parses");
    let start = Instant::now();

    let (module, _obligations, elaborator, _retained) =
        typecheck_with_prelude_measured(budget, &entrypoint, &RootSource::none())
            .expect("the probe elaborates");
    let (verdicts, kernel) = recheck_with_prelude_measured(&module, budget);
    let elapsed = start.elapsed().as_secs_f64();

    assert!(verdicts.is_empty(), "the kernel accepts it: {verdicts:?}");

    (
        elaborator,
        kernel.heaviest_declaration(),
        kernel.retained(),
        elapsed,
    )
}

/// Where the step at eighteen definitions comes from, read off the compilation-scoped retention allowance rather than guessed at from a wall clock.
///
/// # How to take it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_retention_measurements
/// ```
///
/// # What it is for
///
/// [`scrutinee_refinement_measurements`] reports a cost that grows with the web. This one reports a cost that does *not*: with the combinators dispatching through a stuck `match` instead of `&&`, the compile is flat through seventeen definitions, steps by a factor of fifty at eighteen, and stays there to forty. A wall clock cannot tell a fan-out from a ceiling, so the column that decides it is here instead — and the three controls are what say which ceiling.
///
/// The kernel's only compilation-scoped, never-refunded, budget-independent bound is [`curios_core::DEFAULT_RETENTION_QUOTA`]. Every other counter it holds is per declaration and restored at each item boundary.
///
/// # What it last printed
///
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`.
///
/// ```text
///   rules   kernel units   depth  kernel retained  elab units   compile
///   15             23415      18        892370244        9302     0.08 s
///   16             24649      19        964531570        9302     0.05 s
///   17             25976      20        999919608        9302     0.05 s
///   18             53433       6        999999999        9302     2.66 s
///   19             53433       6       1000000000        9302     2.68 s
///   20             53433       6        999999991        9302     2.68 s
///   40             61821      43        999999986        9302     2.70 s
/// ```
///
/// **The step is the retention quota filling, and the ladder walks right up to it.** One fifteen-definition web has already consumed 89% of the compilation's whole allowance; seventeen reaches 99.99%; eighteen saturates. Past that every insertion is refused, the reduction runs cold, and the compile costs fifty times what it did one definition earlier.
///
/// **It is not the step budget.** The same web at twenty definitions spends the identical 53 433 units and takes the identical time across a sixty-four-fold budget range — a fixed `n` across budgets cannot see a threshold, which is what an earlier reading of this got wrong; sweeping the threshold is what settles it.
///
/// **It is not the program's size either.** One, two and three *different* eighteen-definition webs in one file all cost 2.7 s and all saturate: the first web spends the allowance and the rest run cold for free. A per-declaration cost would have tripled.
///
/// **What consumes it is the scrutinee.** The same webs with `top(n)` never reaching a match retain 191 177 units at eighteen definitions and 201 209 at forty — four orders of magnitude less, flat, and fast. Only `assume_case_value`'s reduction fills the quota.
///
/// The charged units barely move across the step (25 976 → 53 433) while the wall clock moves fifty-fold, because a term-keyed hit is free by design: what the memo buys is not charged, so what losing it costs is not charged either.
#[test]
#[ignore = "measurement: reports where a ceiling is rather than asserting one"]
fn scrutinee_retention_measurements() {
    println!("  the ladder — one scrutinized web, at the default budget");
    println!(
        "  {:<6}  {:>12}  {:>6}  {:>14}  {:>10}  {:>8}",
        "rules", "kernel units", "depth", "kernel retained", "elab units", "compile"
    );
    for rules in [15usize, 16, 17, 18, 19, 20, 40] {
        let source = dispatched_program(&[(rules, true, "", (2, 3))]);
        let (elaborator, kernel, retained, elapsed) = checker_cost(DEFAULT_STEP_BUDGET, &source);

        println!(
            "  {rules:<6}  {:>12}  {:>6}  {:>14}  {:>10}  {elapsed:>7.2} s",
            kernel.units(),
            kernel.peak_depth(),
            retained,
            elaborator.units(),
        );
    }

    println!(
        "\n  control: the same web at twenty definitions, across a sixty-four-fold budget range"
    );
    let source = dispatched_program(&[(20, true, "", (2, 3))]);
    for budget in [
        DEFAULT_STEP_BUDGET / 8,
        DEFAULT_STEP_BUDGET,
        DEFAULT_STEP_BUDGET * 8,
    ] {
        let (_, kernel, retained, elapsed) = checker_cost(budget, &source);

        println!(
            "  budget {budget:<12}  kernel units {:>12}  retained {retained:>14}  {elapsed:>7.2} s",
            kernel.units(),
        );
    }

    println!("\n  control: how many eighteen-definition webs one program contains");
    for webs in [
        &[(18usize, true, "", (2u64, 3u64))][..],
        &[(18, true, "", (2, 3)), (18, true, "B", (5, 7))][..],
        &[
            (18, true, "", (2, 3)),
            (18, true, "B", (5, 7)),
            (18, true, "C", (11, 13)),
        ][..],
    ] {
        let source = dispatched_program(webs);
        let (_, kernel, retained, elapsed) = checker_cost(DEFAULT_STEP_BUDGET, &source);

        println!(
            "  {} web(s)  kernel units {:>12}  retained {retained:>14}  {elapsed:>7.2} s",
            webs.len(),
            kernel.units(),
        );
    }

    println!("\n  control: the same web at eighteen and forty definitions, never scrutinized");
    for rules in [18usize, 40] {
        let source = dispatched_program(&[(rules, false, "", (2, 3))]);
        let (_, kernel, retained, elapsed) = checker_cost(DEFAULT_STEP_BUDGET, &source);

        println!(
            "  {rules:<6}  kernel units {:>12}  retained {retained:>14}  {elapsed:>7.2} s",
            kernel.units(),
        );
    }
}
