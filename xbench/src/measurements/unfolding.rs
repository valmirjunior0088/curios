//! What a combinator web costs to compile, and where.
//!
//! Two measurements of what a combinator web can drive a compilation to spend: how a case refinement is keyed and what an index inversion reduces, and how a reified closure is shared — the CPS fixpoint's cost is `fixpoint`'s. They are kept in code rather than prose because a figure taken by a throwaway script does not survive being re-measured.
//!
//! None asserts. A measurement that fails is a measurement with an opinion, and what these read is a cost, not a contract.

use {
    crate::{Measure, phase, read, turn},
    curios::{TALLIES, test_support::*},
};

/// What a combinator application costs when it is written inside a `!` continuation rather than at item level.
///
/// # How to take it
///
/// `cargo xbench measure combinator_sharing_measurements` takes it: `<spelling> at <rules>`, a compile's time through the ANF optimizer, every spelling at every size turned round-robin three times after an untimed round, with `/chain copies`, the copies of `Parse/chain`, and `/functions`, the functions it emits. The wall clocks are dominated by `curios_cps::optimize`; the counts are deterministic.
///
/// # Its records
///
/// `cargo xbench report combinator_sharing_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
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
                let compiled = phase(TALLIES, || anf_optm(source));
                compiled.read(name, Some(*rules));
                let module = compiled.answer;
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
/// # Its records
///
/// `cargo xbench report scrutinee_refinement_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
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
                    let compiled = phase(TALLIES, || compile_only(&program(rules)));
                    compiled.read(&name, Some(rules as u64));
                    let outcome = compiled.answer;

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
