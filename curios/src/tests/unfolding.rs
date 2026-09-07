//! What a combinator web costs to compile, and where.
//!
//! Three measurements, for five compiler cliffs `documentation/roadmap.md` records closed: how a case refinement is keyed, how a reified closure is shared, what the Cont fixpoint costs (measured in `tests::fixpoint`), what an index inversion reduces, and what filling the retention allowance cost. What is left of them here is the before-and-after each probe carries. All three measurements are here rather than in prose because those specifications were preceded by a document whose figures were taken by a throwaway script, and none of that document's three load-bearing claims survived being re-measured.
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

        let (many, separated) = match inner {
            Inner::None => (None, None),
            Inner::InBlock => (
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

        let _ = writeln!(source, "let r{rule}: Parse(Bytes) =");
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
    /// The second door onto the same reduction, and the one no refinement key reaches. Both checkers paid it, which is what made it a different defect rather than the same one — the third of the compiler cliffs `documentation/roadmap.md` records, closed: the cost was never the inverter's but weak-head reduction's, which normalized a `&&`/`||` tree whole to decide a fold a stuck left had already settled, and the closed machine's, which substituted a global's value for its name. [`numerics`] is the same door over `Nat`, where the first of those is the fold laws' to keep.
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
            "let probe{suffix}(n: Nat, e: Eq(top{suffix}(n), true)) -> Str =\n    match e: (_, _, _) => Str | refl(@z) => \"y\" end;\n\n"
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
/// The tables below carry a fourth row, every rule eta-expanded through `Parse { run = … }`, that this test no longer takes: `Parse`'s representation has since been sealed, so the spelling is not one a user can write — which is the sealing's point rather than a loss to the measurement, since the row only ever reported what a `delay` combinator would cost, and `Parse` exports none.
///
/// # What it last printed
///
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`, with a replacement's residual group bound at item level.
///
/// | spelling | `Parse/bind` copies at 16 rules | emitted functions | compile | growth of copies |
/// | --- | --- | --- | --- | --- |
/// | no application inside a continuation | 18 | 166 | 0.69 s | `n + 2` |
/// | **as written** — applications inside the continuation | **18** | **262** | **1.52 s** | **`n + 2`** |
/// | applications hoisted to items | 18 | 262 | 1.54 s | `n + 2` |
/// | every rule eta-expanded, applications left in place | 18 | 278 | 1.64 s | `n + 2` |
///
/// **Where the application is written no longer decides anything.** The as-written row and the hoisted row agree to the copy, to the function, and to within a hundredth of a second, at every size — 4/108, 6/130, 10/174, 14/218, 18/262. The hoisted spelling was the cure emulated in source and therefore a ceiling; this is that ceiling reached.
///
/// # What it printed before let-insertion
///
/// Same day, same host, with a block-owned candidate's group spliced into its own block.
///
/// | spelling | copies at 16 rules | emitted functions | compile | growth |
/// | --- | --- | --- | --- | --- |
/// | no application inside a continuation | 18 | 166 | 0.63 s | `n + 2` |
/// | applications hoisted to items | 18 | 262 | 1.29 s | `n + 2` |
/// | every rule eta-expanded | 18 | 312 | 4.58 s | `n + 2` |
/// | **as written** | **258** | **566** | **24.23 s** | **`n² + 2`** |
///
/// The quadratic was exact over `n` ∈ {2, 4, 8, 12, 16}: 6, 18, 66, 146, 258. Sixteen rules cost 14× the copies and 16× the wall clock of the identical grammar with the same applications named as items first.
///
/// **Eta-expansion improved too, and by less**, which is the same reading the earlier table gave: 312 functions to 278, 4.58 s to 1.64 s. Eta declines the folds rather than sharing them, so it gains only what its remaining block candidates share — hoisting performs the folds *and* shares the result, and is still the cheaper of the two.
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
/// cargo x profile <file>
/// ```
///
/// Same date and host, with the group bound at item level:
///
/// | | as written | hoisted |
/// | --- | --- | --- |
/// | `compile_entrypoint` | 1 043 ms | 1 027 ms |
/// | `cont_optimize` | 891 ms | 874 ms |
/// | `cont_optimize` allocations | 7 889 915 | 7 889 915 |
/// | `evaluate_closed_terms` | 12 ms | 12 ms |
///
/// **The allocation counts are identical**, which says more than the wall clocks: the two spellings now hand the fixpoint the same module, so what it does with them cannot differ. As written, `curios_cont::optimize` was **21 998 ms of a 22 193 ms** compile — 99.1%, at 15.2 GB and 223.8 M allocations — against **602 ms of 737 ms** hoisted. It is 891 ms and 7.9 M allocations now: 25× less time and 28× fewer allocations, all of it from handing that pass a module a quarter the size.
///
/// `evaluate_closed_terms`, the pass this measurement is about, was 19 ms and is 12 ms; it never was the cost. What it *produces* was, because the fixpoint below it was super-quadratic in module size — still 97.6% of an ordinary `Toml/decode` compile, with no point-free code in it anywhere. That cliff is closed too, and `tests::fixpoint` carries what it was and what is left of it.
///
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
/// cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_refinement_measurements
/// ```
///
/// # What it last printed
///
/// Taken **2026-08-24**, **debug**, `aarch64-apple-darwin`, with a product of two symbolic sums left as its own weak-head form and distributed only where a value is asked for by name (`262e5387`, `82f424bc`), one node per distinct monomial and factor inside a product, a term comparison answering before it allocates, and a traversal re-entering its stack guard per level. Debug rather than release deliberately: debug has been the canary for exhaustion that release hid, and the 3.3 s every row shares is the debug prelude restore plus a trivial compile — read the columns against each other, not against the release tables below.
///
/// | definitions | applied | scrutinized | proved | numeric, proved |
/// | --- | --- | --- | --- | --- |
/// | 8 | 3.57 s | 3.33 s | 3.36 s | 3.35 s |
/// | 10 | 3.27 s | 3.33 s | 3.35 s | 3.35 s |
/// | 12 | 3.30 s | 3.32 s | 3.35 s | 3.37 s |
/// | 13 | 3.30 s | 3.33 s | 3.37 s | 3.37 s |
///
/// **Every door is flat, and the numeric one is now indistinguishable from the rest.** The `once` and `closed` scrutinized columns read the same and are omitted. This is the section that replaces one that had decayed: the table below it was taken twenty hours *before* `3a624381` distributed multiplication in full, and after that commit the numeric door at ten definitions did not finish — seven minutes and ten gigabytes on this host, then a stack fault — while the recorded figure still read 0.15 s. A figure whose method no longer reproduces it is what `documentation/roadmap.md`'s measurement rule forbids, and it cost a day's investigation a wrong premise before it was dated. `a_ten_definition_numeric_web_compiles` and `a_symbolic_web_compares_against_zero_in_linear_units` are the fixtures that refuse that regression now, so this table has a control that the last did not.
///
/// # What it printed before distribution in full
///
/// Taken **2026-08-22**, **release**, `x86_64-unknown-linux-gnu`, twenty hours before `3a624381` made `NatMul` distribute in full — which is why the numeric column below is flat on a compiler that did not yet build the polynomial — with the kernel remembering a local-bearing reduct for as long as the equations in force stand, the elaborator's cache admitting a universe metavariable, `capture` and `Term::eq` each walking a graph in its own size, and — from the day before — `&&`/`||` reducing their right operand only behind a literal left and the closed machine keeping a global argument as a name.
///
/// | definitions | proved | numeric, proved |
/// | --- | --- | --- |
/// | 8 | 0.14 s | 0.15 s |
/// | 10 | 0.15 s | 0.15 s |
/// | 12 | 0.15 s | 0.15 s |
/// | 13 | 0.15 s | 0.15 s |
///
/// The first four columns of the earlier table did not move and are omitted. **Both doors are flat.** Under the profiler at thirteen definitions the numeric web is a 106 ms compile, `recheck` 12 ms and `elaborate_and_zonk` 16 ms, where the day before it was 4 452 ms with `recheck` 4 370 and elaboration 165.
///
/// # What it printed with the boolean cure alone
///
/// Taken **2026-08-21**, the day before, on the same host.
///
/// | definitions | proved | numeric, proved |
/// | --- | --- | --- |
/// | 8 | 0.15 s | 0.19 s |
/// | 10 | 0.15 s | 0.42 s |
/// | 12 | 0.15 s | 1.78 s |
/// | 13 | 0.16 s | 4.01 s |
///
/// **The proved door was flat**, and on the same host it read 0.23, 0.57, 2.55 and 5.53 s the same morning. Under `--features profile` at 13 definitions `recheck` is **13.6 ms of a 108 ms compile**, 120 k allocations, where it was 5 842 ms of 6 138 ms, 72 M allocations, 6.3 GB; `elaborate_and_zonk` is 13.8 ms where it was 220 ms.
///
/// The cost was never the inverter's. Conversion is weak-head-and-compare, and `true` against a stuck term stops at the heads; what the inverter's `force` paid for was weak-head reduction itself, twice over. `reduce_bool_binary` reduced both operands of a `&&`/`||` before it could know the fold was settled by a stuck left, so the weak-head form of the web's top was its full normalization, `2^n` with nothing remembering a local-bearing term — the whole of the elaborator's share and most of the kernel's. And the closed machine, handed the *closed* `both(r11, anyof(r11, r11))`, substituted `r11`'s value where the strategy keeps the name, so every definition's value held the previous one's twice, a graph whose tree was `2^n`, stored by the unfold memo at the tree's footprint and opened as a tree by the strategy's own beta — the 1.3 s that survived the first cure alone, and the whole of [`scrutinee_retention_measurements`]' retention ladder. Each is closed where it stood: `reduce_bool_binary` and the machine's `args`, in `curios-core`, which both checkers share.
///
/// **The numeric column was the same door over `Nat`, and it was the cliff that was left.** `reduce_nat_binary` reads its right operand for its identity laws — `x + 0` is `x` whatever the left — so a stuck left does not settle its fold the way a `Bool`'s does, and the web's weak-head form is its normalization: a graph whose tree is `2^n`, and ×2.2 per definition for every walk that saw the tree. Four of them did, and each is closed in its own place. The kernel re-derived the graph at every demand, remembering nothing local-bearing; `Memos` now keeps such a reduct for as long as the equations in force stand. The elaborator's cache refused every term naming a universe metavariable, which inside the declaration instantiating a polymorphic web is all of them; reduction is parametric in levels, and the cache admits them. The kernel's conversion history captured every goal's terms as trees; `capture` is memoized on node and depth. And the elaborator's cache lookup compared a forced graph against an equal, distinct key pair by pair along the tree; `Term::eq` remembers the pairs it has entered.
///
/// # What it printed with the key at the written spelling and both operands eager
///
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`, with a case refinement keyed at the written spelling.
///
/// | definitions | applied | named once, scrutinized | named twice, scrutinized | scrutinized at a literal | proved |
/// | --- | --- | --- | --- | --- | --- |
/// | 8 | 0.10 s | 0.07 s | 0.07 s | 0.07 s | 0.11 s |
/// | 10 | 0.07 s | 0.08 s | 0.08 s | 0.08 s | 0.28 s |
/// | 12 | 0.07 s | 0.08 s | 0.08 s | 0.08 s | 1.25 s |
/// | 13 | 0.08 s | 0.08 s | 0.08 s | 0.08 s | 2.89 s |
/// | 14 | 0.08 s | 0.08 s | 0.08 s | 0.08 s | — |
/// | 20 | 0.08 s | 0.08 s | 0.08 s | 0.08 s | — |
///
/// Flat in the first four columns, and they are each other's controls: what a `match` is written over no longer decides anything, at any size, and fourteen definitions compile where they refused. The first row is the first compile of the run and carries its warm-up.
///
/// **The last column was the door this key does not reach**, and it is here so that a reader can see the two apart. `Eq(top(n), true)` eliminated at `refl` registers no case equation for the web — the scrutinee is a proof variable — and reduces it anyway, through the index inversion the elimination rule runs. It was unchanged by that commit, to the wall clock, and it was exponential in *both* checkers rather than one; the section above is where it went.
///
/// Under `--features profile` at 13 definitions — `cargo x profile <the same program>` — `recheck` is **7.9 ms of a 64 ms compile**, 112 k allocations, tenth in the table and below `elaborate_and_zonk`'s 10.4 ms. Peak memory is 24.9 MiB. The figures it replaced are two paragraphs down.
///
/// # What it printed with the key at the reduced spelling
///
/// Taken the same day on the same host, before `Scope::refine` stopped reducing. The `applied` column was the same then and is omitted; the last column did not exist.
///
/// | definitions | named once, scrutinized | named twice, scrutinized |
/// | --- | --- | --- |
/// | 8 | 0.26 s | 0.29 s |
/// | 10 | 0.28 s | 0.42 s |
/// | 12 | 0.28 s | 1.21 s |
/// | 13 | 0.30 s | 2.60 s |
/// | 14 | 0.31 s | **refused** — the kernel's reduction budget |
/// | 20 | 0.99 s | **refused** |
///
/// Both conditions were necessary and neither was sufficient. A web nothing scrutinized was flat however it fanned out; a web that *was* scrutinized cost what its fan-out was — the middle column still names each definition twice across the chain, once as the previous rule and once as the older one, and grew accordingly, just far more slowly than naming it twice within one rule.
///
/// Under `--features profile` at 13 definitions, `recheck` was 2 881 ms of a 3 061 ms compile — 94.1%, allocating 6 955 MB across 74.8 M allocations — against `elaborate_and_zonk`'s 13 ms and 134 k allocations. The two checkers were deciding the same terms; only one of them reduced. It is now 7.9 ms and 112 k allocations: a 364× fall in time and 668× in allocation, on the judgment rather than on the program.
///
/// **The `scrutinized at a literal` column is the control that identified the trigger, and it was not in the earlier table.** The same web under the same `match`, with the scrutinee applied to `7` rather than to the declaration's binder, was already flat at every size — same call site, same full reduction, nothing folded away by elaboration. What made the reduction unaffordable was never the `match`; it was that its subject mentioned a binder, which is exactly the term `Memos::storable` may not remember and the closed machine may not take.
///
/// The programs end in a plain `/std/print("ok")` rather than a runtime-tainted parse, so these wall clocks are the two checkers and nothing downstream of them. That is also why they are lower across the board than the earlier table's.
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
        // The proved door is exponential in both checkers and is not this measurement's subject; four rungs are enough to show it did not move. `scrutinee_retention_measurements` is where it is measured properly.
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

    // The same web over `Nat`, proved — the shape the boolean cure does not reach, because a `Nat` fold must read its right operand for its identity laws where a `Bool` fold behind a stuck left need not.
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

/// The [`predicates`] web carried over `Nat` — `+` for `both`, `*` for `anyof`, remainders for the leaves — and proved at `Eq(top(n), 0)`. Each rule names the one before it twice, as the `twice` arm of [`predicates`] does.
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
        "let probe(n: Nat, e: Eq(top(n), 0)) -> Str =\n    match e: (_, _, _) => Str | refl(@z) => \"y\" end;\n\n",
    );
    source.push_str(TAIL);
    source
}

/// **The decision's own probe.** Deciding `Eq(top(n), 0)` for a symbolic `n` needs the sum's head, not its normal form: a stuck sum whose summands are not literal zero is not zero. Under eager folding the fold built the linear combination first — ~φ²ⁿ monomials, since the web's degree is Fibonacci in its size — and the units grew with it. The folds now answer the weak-head form and the peel clashes from the head, so the units grow with the weak-head DAG, which memoization keeps linear in `n`. The control is the increment: each further definition costs about what the previous one did.
#[test]
fn a_symbolic_web_compares_against_zero_in_linear_units() {
    let units = |rules: usize| {
        let source = numerics(rules);
        let entrypoint = source.parse::<Entrypoint>().expect("the web parses");
        let (_, _, consumption, _) =
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

/// The numeric door at a size that used to be unreachable: ten definitions took between three and seven minutes, ten gigabytes, and then a `SIGBUS`, where eight took thirteen seconds — the recorded flat table in [`scrutinee_refinement_measurements`] predates distribution in full, which doubles the sum's summands per definition. Four things closed it, and this is their positive control — the last being the decision that a product of two symbolic sums is its own weak-head form, distributed by `Nat::normalize` only where a value is asked for, which is what took this from 87 s and six gigabytes to four seconds and a tenth of one: a monomial and each of its factors are one node per distinct structure inside a product, so a merge is a pointer test and a fresh spine is never cache-warmed; and a traversal re-enters `recurse` per level, so capturing the normal form for a conversion goal chains stack segments instead of running one to its guard page. Not a measurement — a refusal to regress to not compiling.
#[test]
fn a_ten_definition_numeric_web_compiles() {
    let (outcome, _) = compile_only(&numerics(10));

    assert_eq!(outcome, Ok(()), "the ten-definition numeric web compiles");
}

/// A web of `rules` predicate definitions whose combinators dispatch through a stuck `match` rather than through `&&`, so the web's weak-head normal form is a tower of stuck matches rather than a tree of folded intrinsics.
///
/// Same fan-out as [`predicates`] — each rule names the one before it twice — and the same scrutinized consumer. What differs is the shape reduction has to build, and that is what the step in [`scrutinee_retention_measurements`] is a function of.
fn dispatched(rules: usize, consumed: Consumed, suffix: &str, moduli: (u64, u64)) -> String {
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

    source.push_str(&consumer(suffix, consumed));
    source
}

/// One or more [`dispatched`] webs under the shared header and [`TAIL`].
fn dispatched_program(webs: &[(usize, Consumed, &str, (u64, u64))]) -> String {
    let mut source = String::from("use /std/{Str, Nat, Bool, Eq};\n\n");

    for (rules, consumed, suffix, moduli) in webs {
        source.push_str(&dispatched(*rules, *consumed, suffix, *moduli));
    }

    source.push_str(TAIL);
    source
}

/// What each checker's heaviest declaration spent on `source` at `budget`, what the kernel's walk retained, and how long the two took together.
fn checker_cost(budget: u64, source: &str) -> (Consumption, Consumption, u64, f64) {
    let entrypoint = source.parse::<Entrypoint>().expect("the probe parses");
    let start = Instant::now();

    let (module, _obligations, elaborator, _retained) =
        typecheck_with_prelude_measured(budget, &entrypoint, &RootSource::none())
            .expect("the probe elaborates");
    let module = curios_core::Zonked::project(&module).expect("the checked module is zonked");
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

/// What a combinator web costs the compilation's retention allowance, by the door its value is demanded through.
///
/// # How to take it
///
/// ```sh
/// cargo test --release --package curios --lib -- --ignored --nocapture scrutinee_retention_measurements
/// ```
///
/// # What it is for
///
/// [`scrutinee_refinement_measurements`] reports wall clocks. This one reports the one resource the kernel holds that is *compilation*-scoped, never refunded, and unaffected by `--budget`: [`curios_core::DEFAULT_RETENTION_QUOTA`]. Crossing it stops every memo insertion, and a program large enough to cross it stops being linear in what it spends — which is invisible in a wall clock until the moment it is the whole of one.
///
/// # What it last printed
///
/// Taken **2026-08-22**, **release**, `x86_64-unknown-linux-gnu`, with the closed machine keeping a global argument as a name and the declaration-scoped memo tables no longer charged against the allowance — the proved ladder alone, the rest of the run being unchanged.
///
/// ```text
///   the ladder — one proved web, at the default budget
///   rules   kernel units   depth  kernel retained  elab units   compile
///   6              22363       6                0       14073     0.04 s
///   8              22363       6                0       16593     0.04 s
///   10             22363       6                0       19113     0.04 s
///   11             22363       6                0       20373     0.05 s
///   12             22363       6                0       21633     0.04 s
///   13             22482      16                0       22893     0.05 s
/// ```
///
/// **`kernel retained` reads the unfold table alone now**, and a web of universe-polymorphic definitions stores nothing there — the scrutinized ladder reads 2 501 at every size for the same reason. The day before, with the term-keyed tables still charged but the machine keeping names, the same column read 215 949 to 262 121, linear at about twelve thousand a definition; both tables are kept below.
///
/// **The proved door's retention was the machine's.** Thirteen definitions retain 262 121 units where they retained 309 948 607 — a thousandth — and the column grows by about twelve thousand a definition, linearly. What the unfold memo was retaining was the closed machine's reduct of each definition: it substituted the previous definition's *value* for its name at every beta, so each value held the one before it twice, and `footprint` priced the graph as the tree it unfolds to — exactly the overcount the retention specification hypothesized, with the thing that built the graph now named. A bare name stays a name, as it does under the strategy, and the ladder below is what this one printed before.
///
/// # What it printed with the machine substituting values
///
/// Taken **2026-08-21**, **release**, `aarch64-apple-darwin`, with a case refinement keyed at the written spelling.
///
/// ```text
///   the ladder — one scrutinized web, at the default budget
///   rules   kernel units   depth  kernel retained  elab units   compile
///   15             22400       6           211155        9302     0.07 s
///   18             22400       6           212523        9302     0.05 s
///   20             22400       6           213435        9302     0.05 s
///   40             22400       6           222555        9302     0.05 s
///
///   the ladder — one proved web, at the default budget
///   rules   kernel units   depth  kernel retained  elab units   compile
///   6              22400       6           836355       13869     0.04 s
///   8              22400       6          3962757       16279     0.05 s
///   10             22400       6         22197227       18689     0.08 s
///   11             22400       6         53329085       19894     0.13 s
///   12             22400       6        128490445       21099     0.23 s
///   13             22400       6        309948607       22304     0.48 s
///
///   control: the same proved web at 12 definitions, across a sixty-four-fold budget range
///   budget 3750000       retained      128490445     0.24 s
///   budget 30000000      retained      128490445     0.23 s
///   budget 240000000     retained      128490445     0.24 s
///
///   control: how many proved webs one program contains, at 12 definitions each
///   1 web(s)  retained      128490445     0.23 s
///   2 web(s)  retained      256803853     0.43 s
///   3 web(s)  retained      385117261     0.62 s
///
///   control: the same web at eighteen and forty definitions, applied and nothing more
///   18      retained         190873     0.05 s
///   40      retained         200905     0.06 s
/// ```
///
/// **The scrutinized door is closed.** Forty definitions retain 222 555 units — a fifth of a percent of the allowance — where fifteen used to retain 892 370 244 and eighteen saturated. That door was `assume_case_value` reducing to key a case refinement, and it no longer reduces.
///
/// **The proved door was open, and it was the same reduction reached another way.** `Eq(top(n), true)` eliminated at `refl` registers no case equation for the web at all — the scrutinee is a proof variable — and reduces it anyway, through `invert_indices` unifying the actual indices against `(z, z)`. Retention grew by a factor of about 2.4 per definition: thirteen definitions consumed 31% of the whole compilation's allowance, and fifteen would have exhausted it. The wall clock grew with it, and *both checkers* paid — `elab units` climbs on this ladder where it is flat on the other, which is what made it a different defect. [`scrutinee_refinement_measurements`] carries where both went.
///
/// **It is not the step budget.** The same proved web across a sixty-four-fold budget range retains the identical figure and takes the identical time. Every other counter the kernel holds is per declaration and restored at each item boundary; this one is not, which is why a wall clock alone cannot tell a fan-out from a ceiling.
///
/// **The allowance was the compilation's, not the declaration's, and that was the fifth cliff.** Three independent twelve-definition proofs in one file retained three times what one did, and a quota measured for headroom against the fixed prelude — which used eleven percent of it — was a quota three ordinary proofs could exhaust. It is still the compilation's, by the decision `curios-core/src/retention.rs` states; what changed is what it prices — storage that outlives the budget that built it, and nothing that dies with a declaration — so the proofs retain nothing of it now, and a crossing shows in the profiler under `retention::refused` instead of in a wall clock.
///
/// # What it printed with the key at the reduced spelling
///
/// The scrutinized ladder, before `Scope::refine` stopped reducing, on the same day and host. The proved ladder is unchanged by that commit and is not repeated.
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
/// One fifteen-definition web had already consumed 89% of the allowance; seventeen reached 99.99%; eighteen saturated, and every later insertion was refused. That is where the step at eighteen definitions came from, and the same three controls identified it: budget-invariant across sixty-four-fold, invariant in the web's size past the threshold — eighteen and forty allocated 112.73 M and 112.76 M — and invariant in how many webs the program contained, since the first exhausted the allowance and the rest ran cold.
///
#[test]
#[ignore = "measurement: reports where a ceiling is rather than asserting one"]
fn scrutinee_retention_measurements() {
    /// The size the two controls below are taken at: far enough up the proved ladder for the allowance to be the story, and small enough to finish in a fraction of a second.
    const PROVED: usize = 12;

    for (consumed, label, sizes) in [
        (
            Consumed::Scrutinized,
            "scrutinized",
            &[15usize, 16, 17, 18, 19, 20, 40][..],
        ),
        (Consumed::Proved, "proved", &[6, 8, 10, 11, 12, 13][..]),
    ] {
        println!("  the ladder — one {label} web, at the default budget");
        println!(
            "  {:<6}  {:>12}  {:>6}  {:>14}  {:>10}  {:>8}",
            "rules", "kernel units", "depth", "kernel retained", "elab units", "compile"
        );

        for &rules in sizes {
            let source = dispatched_program(&[(rules, consumed, "", (2, 3))]);
            let (elaborator, kernel, retained, elapsed) =
                checker_cost(DEFAULT_STEP_BUDGET, &source);

            println!(
                "  {rules:<6}  {:>12}  {:>6}  {:>14}  {:>10}  {elapsed:>7.2} s",
                kernel.units(),
                kernel.peak_depth(),
                retained,
                elaborator.units(),
            );
        }

        println!();
    }

    println!(
        "  control: the same proved web at {PROVED} definitions, across a sixty-four-fold budget range"
    );
    let source = dispatched_program(&[(PROVED, Consumed::Proved, "", (2, 3))]);
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

    println!(
        "\n  control: how many proved webs one program contains, at {PROVED} definitions each"
    );
    for webs in [
        &[(PROVED, Consumed::Proved, "", (2u64, 3u64))][..],
        &[
            (PROVED, Consumed::Proved, "", (2, 3)),
            (PROVED, Consumed::Proved, "B", (5, 7)),
        ][..],
        &[
            (PROVED, Consumed::Proved, "", (2, 3)),
            (PROVED, Consumed::Proved, "B", (5, 7)),
            (PROVED, Consumed::Proved, "C", (11, 13)),
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

    println!(
        "\n  control: the same web at eighteen and forty definitions, applied and nothing more"
    );
    for rules in [18usize, 40] {
        let source = dispatched_program(&[(rules, Consumed::Applied, "", (2, 3))]);
        let (_, kernel, retained, elapsed) = checker_cost(DEFAULT_STEP_BUDGET, &source);

        println!(
            "  {rules:<6}  kernel units {:>12}  retained {retained:>14}  {elapsed:>7.2} s",
            kernel.units(),
        );
    }
}

/// **The guard [`combinator_sharing_measurements`] cannot be**, because a probe is ignored and nothing runs it.
///
/// What it holds is the growth *law* rather than a number: a combinator application written inside a `!` continuation must cost what the identical application written as a top-level item costs — and what not writing it at all costs, since a shared residual group is bound once and reused. The spellings denote the same grammar, and before that group was bound at item level the first differed from the others by an order of magnitude at this size and by fourteen times at sixteen rules — `n²` copies against `n`.
///
/// **The baseline is measured, never written down.** A count here is the grammar's `n` plus whatever `/std/Parse` and its own users spell, and that second term is no part of this claim. Written as a literal it said `10`, ordinary standard-library growth carried it to `23`, and a test about sharing then failed for a reason that has nothing to do with sharing. [`Inner::None`] *is* that term, taken at the same size and in the same run, so the library may grow — it moves all three counts together — while a spelling that stopped sharing still stands out at once: quadratic is 66 against 23 at eight rules.
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
