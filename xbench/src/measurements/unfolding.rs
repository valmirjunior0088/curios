//! What a combinator web costs to compile, and where.
//!
//! Two measurements of what a combinator web can drive a compilation to spend: how a case refinement is keyed and what an index inversion reduces, and how a reified closure is shared — the CPS fixpoint's cost is `fixpoint`'s. They are kept in code rather than prose because a figure taken by a throwaway script does not survive being re-measured.
//!
//! None asserts. A measurement that fails is a measurement with an opinion, and what these read is a cost, not a contract.

use {
    curios::test_support::*,
    std::time::Instant,
    xbench::{Measure, read, read_elapsed, turn},
};

/// What a combinator application costs when it is written inside a `!` continuation rather than at item level.
///
/// # How to take it
///
/// `cargo xbench measure combinator_sharing_measurements` takes it: `<spelling> at <rules>`, a compile's time through the ANF optimizer, every spelling at every size turned round-robin three times after an untimed round, with `/chain copies`, the copies of `Parse/chain`, and `/functions`, the functions it emits. The wall clocks are dominated by `curios_cps::optimize`; the counts are deterministic.
///
/// # What it last printed
///
/// Taken on top of `9c543476a`, **release**, `x86_64-unknown-linux-gnu`, at a load average under two. A copy is counted by the function a `Parse` region's `!` comes to, `Parse/chain`.
///
/// | spelling | `Parse/chain` copies at 16 rules | emitted functions | compile | growth of copies |
/// | --- | --- | --- | --- | --- |
/// | no application inside a continuation | 18 | 380 | 0.53 s | `n + 2` |
/// | **as written** — applications inside the continuation | **18** | **476** | **0.95 s** | **`n + 2`** |
/// | applications hoisted to items | 18 | 476 | 1.03 s | `n + 2` |
///
/// **Where the application is written decides nothing.** The as-written row and the hoisted row agree to the copy and to the function at every size — 4/322, 6/344, 10/388, 14/432, 18/476 — and to within eight hundredths of a second. The hoisted spelling is the sharing emulated in source and therefore a ceiling, and the as-written spelling reaches it because a replacement's residual group is bound at item level. Bound inside the candidate's block, the as-written row grows as `n² + 2`.
///
/// # Where the time goes
///
/// Write `grammar(16, Inner::InBlock)` and `grammar(16, Inner::Hoisted)` to files and take each under the profiler:
///
/// ```sh
/// cargo xtask profile release <file>
/// ```
///
/// Same host, at `9c543476a`, one profiled run of each:
///
/// | | as written | hoisted |
/// | --- | --- | --- |
/// | `compile_entrypoint` | 1 479 ms | 1 658 ms |
/// | `cps_optimize` | 604 ms | 604 ms |
/// | `cps_optimize` allocations | 5 814 986 | 5 815 088 |
/// | `evaluate_closed_terms` | 8 ms | 7 ms |
///
/// **The allocation counts agree to 102 in 5.8 million**, which says more than the wall clocks: the fixpoint does the same work on either spelling's module. `evaluate_closed_terms`, the pass this measurement is about, is never the cost; what it *produces* can be, because the fixpoint below it prices the module it is handed — `tests::instrumented::fixpoint` carries that measurement.
pub fn combinator_sharing_measurements() {
    let mut spellings = Vec::new();
    for (inner, label) in [
        (Inner::None, "none"),
        (Inner::InBlock, "in a continuation"),
        (Inner::Hoisted, "hoisted to items"),
    ] {
        for rules in [2usize, 4, 8, 12, 16] {
            spellings.push((
                format!("{label} at {rules}"),
                rules as u64,
                grammar(rules, inner),
            ));
        }
    }

    let mut compiles = spellings
        .iter()
        .map(|(name, rules, source)| {
            move || {
                let start = Instant::now();
                let module = anf_optm(source);
                read_elapsed(name, Some(*rules), start.elapsed());
                read(
                    &format!("{name}/chain copies"),
                    Measure::Count,
                    Some(*rules),
                    copies(&module, SEQUENCED) as u64,
                );
                read(
                    &format!("{name}/functions"),
                    Measure::Count,
                    Some(*rules),
                    emitted(&module) as u64,
                );
            }
        })
        .collect::<Vec<_>>();
    let mut turned = compiles
        .iter_mut()
        .map(|compile| compile as &mut dyn FnMut())
        .collect::<Vec<_>>();
    turn(3, &mut turned);
}

/// One door [`scrutinee_refinement_measurements`] times: whether each definition of its web names the one before it once or twice, how the web's value is consumed, and the program at a size.
struct Door {
    named: &'static str,
    consumed: &'static str,
    program: fn(usize) -> String,
}

/// What a web of combinator definitions costs to compile, and what consuming its value adds to that.
///
/// # How to take it
///
/// `cargo xbench measure scrutinee_refinement_measurements` takes it: `<named>, <consumed> at <definitions>`, a compile's time, every door at every size turned round-robin five times after an untimed round, and `/outcome/<outcome>`, a figure of one, `compiled` or the first line of the refusal.
///
/// # What it last printed
///
/// Timed, taken on top of `9c543476a` on `x86_64-unknown-linux-gnu`, in the test profile, at a load average under two: five rounds, each over every door and size so no row is read on a warmer machine than another. Median of the five, with their span. The 0.06 s every row shares is the prelude's restore and a trivial compile, so the columns are read against each other.
///
/// | definitions | applied | scrutinized | proved | numeric, proved |
/// | --- | --- | --- | --- | --- |
/// | 8 | 0.06 s [0.06–0.11] | 0.06 s [0.06–0.06] | 0.07 s [0.07–0.07] | 0.07 s [0.07–0.08] |
/// | 10 | 0.06 s [0.06–0.06] | 0.06 s [0.06–0.06] | 0.08 s [0.08–0.08] | 0.08 s [0.08–0.08] |
/// | 12 | 0.06 s [0.06–0.06] | 0.06 s [0.06–0.07] | 0.08 s [0.08–0.08] | 0.09 s [0.09–0.09] |
/// | 13 | 0.06 s [0.06–0.06] | 0.06 s [0.06–0.07] | 0.08 s [0.08–0.08] | 0.10 s [0.10–0.10] |
/// | 14 | 0.06 s [0.06–0.06] | 0.07 s [0.07–0.07] | 0.08 s [0.08–0.08] | 0.11 s [0.11–0.11] |
/// | 20 | 0.07 s [0.07–0.07] | 0.07 s [0.07–0.07] | 0.10 s [0.10–0.10] | 0.18 s [0.18–0.18] |
///
/// **Every door but the numeric one is flat.** The applied and scrutinized doors grow by a hundredth of a second from eight definitions to twenty and the proved door by three hundredths; the numeric door grows from 0.07 s to 0.18 s, 0.11 s over the applied door at twenty. The `once` and `closed` scrutinized columns read within a hundredth of the one shown and are omitted. `a_ten_definition_numeric_web_compiles`, `a_proved_web_of_twenty_definitions_compiles` and `a_symbolic_web_compares_against_zero_in_linear_units` are the fixtures that refuse a regression.
///
/// Each door is held where it stands for a reason of its own. A case refinement is keyed at the written spelling, so keying never demands the scrutinee's reduction — a web nothing reduces is flat however it fans out, and what makes a reduction unaffordable is a subject that mentions a binder, the term `Memos::storable` may not remember and the closed machine may not take. `&&` and `||` reduce their right operand only behind a literal left, and the closed machine keeps a global argument as a name rather than substituting its value, both in `curios-core`, which the two checkers share. A comparison of two `Bool` webs is put to the decision diagram, which reads a term once, so the proved door costs its web's graph. A `Nat` fold must read its right operand for its identity laws, so its web's weak-head form is its normalization: it stays within a fifth of a second at twenty definitions because the certifier's `Memos` keeps a local-bearing reduct while the equations in force stand, the elaborator's cache admits a universe metavariable, the certifier's `capture` is memoized on node and depth, `Term::eq` remembers the pairs it has entered, and a product of two symbolic sums is its own weak-head form, distributed only where a value is asked for.
///
/// The programs end in a plain `/std/print("ok")` rather than a runtime-tainted parse, so these wall clocks are the two checkers and nothing downstream of them.
pub fn scrutinee_refinement_measurements() {
    const SIZES: [usize; 6] = [8, 10, 12, 13, 14, 20];

    let doors = [
        Door {
            named: "twice",
            consumed: "applied",
            program: |rules| predicates(rules, Consumed::Applied, true),
        },
        Door {
            named: "once",
            consumed: "scrutinized",
            program: |rules| predicates(rules, Consumed::Scrutinized, false),
        },
        Door {
            named: "twice",
            consumed: "scrutinized",
            program: |rules| predicates(rules, Consumed::Scrutinized, true),
        },
        Door {
            named: "twice",
            consumed: "scrutinized, closed",
            program: |rules| predicates(rules, Consumed::ScrutinizedClosed, true),
        },
        Door {
            named: "twice",
            consumed: "proved",
            program: |rules| predicates(rules, Consumed::Proved, true),
        },
        // The same web over `Nat`, proved — the shape a `Bool` fold's short-circuit does not reach, because a `Nat` fold must read its right operand for its identity laws where a `Bool` fold behind a stuck left need not.
        Door {
            named: "twice",
            consumed: "numeric, proved",
            program: numerics,
        },
    ];

    let mut compiles = doors
        .iter()
        .flat_map(|door| SIZES.map(|rules| (door, rules)))
        .map(
            |(
                Door {
                    named,
                    consumed,
                    program,
                },
                rules,
            )| {
                move || {
                    let name = format!("{named}, {consumed} at {rules}");
                    let (outcome, elapsed) = compile_only(&program(rules));
                    read_elapsed(&name, Some(rules as u64), elapsed);

                    let ended = match &outcome {
                        Ok(()) => "compiled",
                        Err(error) => error.lines().next().unwrap_or("refused"),
                    };
                    read(
                        &format!("{name}/outcome/{ended}"),
                        Measure::Count,
                        Some(rules as u64),
                        1,
                    );
                }
            },
        )
        .collect::<Vec<_>>();
    let mut turned = compiles
        .iter_mut()
        .map(|compile| compile as &mut dyn FnMut())
        .collect::<Vec<_>>();
    turn(5, &mut turned);
}
