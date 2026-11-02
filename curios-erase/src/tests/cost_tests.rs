//! What erasure costs: a projection or call chain in work proportional to its depth, a doubling term in its own size, and a refusal past the budget in erasure's words.

use {
    crate::test_support::{definition, fresh, module, nat_lit, try_erase, zonked},
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_cert::{Globals, certify_module},
    curios_elab::DEFAULT_BUDGET,
    curios_utilities::{Plicity, grown},
};

/// What erasing a function over a chain of `depth` eliminations spends, in budget units, read off the reader its erasure ran through. `step` wraps the type one level deeper and `eliminate` the term.
fn chain_cost(
    depth: usize,
    step: impl Fn(curios_core::Term) -> curios_core::Term,
    eliminate: impl Fn(curios_core::Term) -> curios_core::Term,
) -> u64 {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let mut domain = nat.clone();
    for _ in 0..depth {
        domain = step(domain);
    }
    let x = fresh(Some("x"));
    let mut body = curios_core::Term::free_var(&x);
    for _ in 0..depth {
        body = eliminate(body);
    }
    let function = curios_core::Term::func([(x, domain.clone())], body);
    let function_type =
        curios_core::Term::func_type_marked([(Plicity::Explicit, x, domain)], nat.clone());

    let unit = zonked(
        &module(
            vec![definition("reads", function_type, function)],
            nat_lit(0),
        )
        .module,
    );
    // On a grown stack, as every entry point erases: a chain this deep is walked a frame a level.
    grown(|| {
        let globals = Globals::default();
        let (_, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
        let places = curios_core::Places::of(unit.as_module());
        kernel.read(|reader| {
            crate::erase_items(
                reader,
                Erasures::nothing(),
                (unit.as_module(), &places),
                None,
            )
            .expect("the unit erases");

            reader.consumed().units()
        })
    })
}

/// A head's type is read once however many eliminations stand on it, so twice the chain is at most twice the work. Reading each head afresh at every level it sits under makes it four times.
#[test]
fn a_projection_chain_erases_in_work_proportional_to_its_depth() {
    let cost = |depth| {
        chain_cost(
            depth,
            |inner| {
                let kept = fresh(Some("kept"));
                let pad = fresh(Some("pad"));
                curios_core::Term::tuple_type([
                    (kept, inner),
                    (
                        pad,
                        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                    ),
                ])
            },
            |head| curios_core::Term::proj(head, 0),
        )
    };
    let (short, long) = (cost(256), cost(512));

    assert!(
        long < 2 * short,
        "a 512-deep chain spent {long} where a 256-deep one spent {short}",
    );
}

/// The same for a function applied once per parameter list.
#[test]
fn a_curried_call_chain_erases_in_work_proportional_to_its_depth() {
    let cost = |depth| {
        chain_cost(
            depth,
            |inner| {
                let n = fresh(Some("n"));
                curios_core::Term::func_type_marked(
                    [(
                        Plicity::Explicit,
                        n,
                        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                    )],
                    inner,
                )
            },
            |head| curios_core::Term::apply(head, [nat_lit(1)]),
        )
    };
    let (short, long) = (cost(256), cost(512));

    assert!(
        long < 2 * short,
        "a 512-deep chain spent {long} where a 256-deep one spent {short}",
    );
}

/// A record of two fields at one type, `levels` deep: a graph of `levels` nodes over a tree of two to the `levels` leaves. Its binders are minted from `mint`, so two such types name no binder in common.
fn doubled_record(levels: u32, mint: u32) -> curios_core::Term {
    (0..levels).fold(
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        |type_, level| {
            let first = curios_core::Free::local(u64::from(mint + 2 * level), None);
            let second = curios_core::Free::local(u64::from(mint + 2 * level + 1), None);

            curios_core::Term::tuple_type([(first, type_.clone()), (second, type_)])
        },
    )
}

/// Erasure answers a term that shares in the term's own size: sixty levels that each hold the one below twice, a tree no walk per path finishes, as `walk_tests` puts the crate's other walks to one. A function at the doubling type, as a program's entry and as a definition; a chain of sixty projections out of a value at it; and sixty definitions that each pair the one before with itself, projected, whose type is its tail's with every binding substituted (`Lowering::type_of`).
///
/// Mutation-checked: with a type read again where `Lowering::type_of` kept one, the row of definitions does not return, and with the type a lambda is checked at walked once per occurrence, none of the other three does.
#[test]
fn erasure_answers_a_doubling_term_in_its_own_size() {
    let nat_type = || curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let doubled = doubled_record(60, 5_000);

    let identity = |mint: u64| {
        let a = curios_core::Free::local(mint, Some("a"));
        (
            curios_core::Term::func([(a, doubled.clone())], curios_core::Term::free_var(&a)),
            curios_core::Term::func_type([(a, doubled.clone())], doubled.clone()),
        )
    };
    let (entry, entry_type) = identity(4_000);
    let (held, held_type) = identity(4_001);

    let b = curios_core::Free::local(4_002, Some("b"));
    let chain = (0..60).fold(curios_core::Term::free_var(&b), |head, _| {
        curios_core::Term::proj(head, 0usize)
    });

    // `(let x0 = (n, n); let x1 = (x0, x0); …; x59).0`, each definition stated at its own doubling type: the projection's head is the whole chain, whose type is read by substituting every binding.
    let n = curios_core::Free::local(4_003, Some("n"));
    let names = (0..60u64)
        .map(|level| curios_core::Free::local(4_100 + level, Some("x")))
        .collect::<Vec<_>>();
    let mut paired = curios_core::Term::free_var(&names[59]);
    for level in (0..60u32).rev() {
        let below = match level {
            0 => curios_core::Term::free_var(&n),
            _ => curios_core::Term::free_var(&names[level as usize - 1]),
        };
        paired = curios_core::Term::let_(
            &names[level as usize],
            doubled_record(level + 1, 10_000 + 200 * level),
            curios_core::Term::tuple([below.clone(), below]),
            paired,
        );
    }
    let paired = curios_core::Term::proj(paired, 0usize);

    let rows = [
        (
            "a function at a doubling type",
            try_erase(&module(Vec::new(), entry), entry_type).is_ok(),
        ),
        (
            "a definition at a doubling type",
            try_erase(
                &module(vec![definition("held", held_type, held)], nat_lit(7)),
                nat_type(),
            )
            .is_ok(),
        ),
        (
            "a projection chain out of a doubling type",
            try_erase(
                &module(
                    Vec::new(),
                    curios_core::Term::func([(b, doubled.clone())], chain),
                ),
                curios_core::Term::func_type([(b, doubled.clone())], nat_type()),
            )
            .is_ok(),
        ),
        (
            "sixty definitions that each pair the one before",
            try_erase(
                &module(
                    Vec::new(),
                    curios_core::Term::func([(n, nat_type())], paired),
                ),
                curios_core::Term::func_type([(n, nat_type())], doubled_record(59, 30_000)),
            )
            .is_ok(),
        ),
    ];

    for (row, erased) in rows {
        assert!(erased, "{row} did not erase");
    }
}

/// An erasure that runs out of its budget is refused in erasure's words, at the term it was reading: the program was certified, and only erasing it stopped.
#[test]
fn an_erasure_out_of_budget_is_refused_in_erasures_words() {
    let unit = zonked(&module(Vec::new(), nat_lit(0)).module);
    let globals = Globals::default();
    let (_, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
    let sum = curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(nat_lit(2), nat_lit(3)));
    let error = kernel.read(|reader| {
        // The whole budget spent, as a reading that ran out of it leaves it.
        reader.restore_budget();
        reader
            .spend(curios_core::Cost::units(DEFAULT_BUDGET))
            .expect("the budget affords itself");

        crate::reduced(reader, &sum).expect_err("a spent budget refuses the reading")
    });
    // Spelled as a report spells a unit's terms, the unit's own names by its lexicon.
    let spelling = curios_core::Spelling::default()
        .with_lexicons(&unit.as_module().lexicon(), &[])
        .with_erased_universes();

    assert_eq!(
        error.format(spelling, &[]),
        "erasure ran out of steps reading: 2 + 3\n  recursion depth needed 1024 units with 0 left"
    );
}
