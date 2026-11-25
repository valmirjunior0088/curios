//! What the closed machine spends per element on three recursion shapes, read rather than asserted: the assertions are the budget bounds of the core's machine suite.

use {
    crate::{Measure, read},
    curios_core::{Demand, Intrinsic, Term, reduce_closed, test_support::*},
};

/// What the three recursion shapes spend per element, read rather than asserted — the assertions live in the budget bounds of the ordinary tests above. `cargo xbench measure spend_probe` takes it: `non-tail at <n>`, `tail at <n>` and `neutral at <n>`, the units each shape spends over `n` elements, and `neutral at <n>/<category>`, what the neutral fold spent in each category of the price list. Set `CURIOS_MACHINE_TRACE` to shrink the ladder and print every dispatch.
pub fn spend_probe() {
    let sizes: Vec<usize> = match std::env::var_os("CURIOS_MACHINE_TRACE") {
        Some(_) => vec![3],
        None => vec![250, 500, 1000, 2000],
    };
    for n in sizes {
        let budget = 1_000_000_000u64;
        let mut host = MachineHost::new(budget);
        let count = non_tail(vec![7; n], nat(0), |call| {
            Term::intrinsic(Intrinsic::nat_add(call, nat(1)))
        });
        reduce_closed(&mut host, count, Demand::Forced).unwrap();
        let non_tail_spent = budget - host.budget;

        let mut host = MachineHost::new(budget);
        let term = tail_fold(vec![3; n], |acc, h| {
            Term::intrinsic(Intrinsic::nat_add(
                acc,
                Term::intrinsic(Intrinsic::ByteToNat(h)),
            ))
        });
        reduce_closed(&mut host, term, Demand::Forced).unwrap();
        let tail_spent = budget - host.budget;

        let mut host = MachineHost::new(budget);
        let term = tail_fold(vec![3; n], |acc, h| {
            Term::intrinsic(Intrinsic::nat_add(acc, h))
        });
        reduce_closed(&mut host, term, Demand::Forced).unwrap();
        let size = Some(n as u64);
        read(
            &format!("non-tail at {n}"),
            Measure::Count,
            size,
            non_tail_spent,
        );
        read(&format!("tail at {n}"), Measure::Count, size, tail_spent);
        read(
            &format!("neutral at {n}"),
            Measure::Count,
            size,
            budget - host.budget,
        );
        for (category, spent) in &host.by_category {
            read(
                &format!("neutral at {n}/{category:?}"),
                Measure::Count,
                size,
                *spent,
            );
        }
    }
}
