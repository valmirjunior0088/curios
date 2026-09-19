//! The per-declaration reduction budget, at both edges.
//!
//! A budget is a refusal about cost rather than about meaning, so each row is a pair in spirit: a computation the default budget affords, and one no budget should. A regression here shows up as a program that used to compile and now cannot, or as one that spends unboundedly before saying so.

use {
    crate::tests::{error, run, typecheck_within},
    curios_pipeline::DEFAULT_STEP_BUDGET,
};

// A `rec` that never reduces (`go : Bool = go`) forces forever when demanded in type position — same infinite-spin behavior as a top-level `rec` — so a step budget stops it with an error rather than hanging.
#[test]
fn nonproductive_inner_rec_in_type_position_exhausts_its_budget() {
    let source = r#"
        use /std/{Bool};
        let spin : Bool =
            let go : Bool = go;
            go;
        let bad : Type =
            match spin : (_) => Type
            | true => {}
            | false => {}
            end;
        let x : bad = ();
        0
        "#;

    error(source);
}

/// The map-wall coda's elaboration-runaway record, resolved. The pathology — a `rec` over a packed accumulator called at a *literal* depth, scrutinised by any comparison under a `match`, spinning elaboration past twenty minutes at ×4 per +2 of depth with flat RSS — was named by stack sampling on 2026-08-20: every sample sat in `Term::any_metavar` under `Context::reduce`'s cache-write gate. Each unfolding substitutes the accumulator into two positions, so reduction results are linear DAGs with exponential tree expansions, and the walk's only prune — the cached `has_metavar` bit — was defeated by the metavariables the results name, so every cache write re-paid the full expansion, uncharged by the budget. The cure is the visited set in `Term::any_metavar`, pinned structurally by curios-core's `any_metavar_visits_a_shared_subterm_once` and end to end by the sibling test below. What this test pins is the other repair the hunt made: `Context::within_allowance` swallowed a *declaration's* exhaustion as an ordinary allowance bail whenever the remainder was below the cap, letting elaboration continue at zero budget — re-raised now, so a budget too small to finish the depth-30 chain refuses loudly instead of spinning.
#[test]
fn a_literal_depth_packed_recursion_refuses_within_a_small_budget() {
    let source = r#"
        use /std/{Nat, List, Bits, Bool, Str, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let grown = b[t, 1];
        let widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30, grown);
        match Bits/len(wide) == 32
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert!(crate::tests::typecheck_within(1_000, source).is_err());
}

/// The runaway pathology end to end, at the depth that used to spin past twenty minutes: with the deduped metavariable walk the chain elaborates, compiles, and runs within the default budget. Kept beside the small-budget probe above so the pair states both directions — a budget too small refuses loudly, the default one finishes.
#[test]
fn a_literal_depth_packed_recursion_compiles_within_the_default_budget() {
    let source = r#"
        use /std/{Nat, List, Bits, Bool, Str, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let grown = b[t, 1];
        let widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30, grown);
        match Bits/len(wide) == 32
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert_eq!(run(source), b"ok\n");
}

/// Acceptance is upward closed in the budget: a program one budget accepts, every larger budget accepts, and what a smaller one refuses it refuses for the budget.
///
/// Nothing in `spend`'s type says so, and the perimeter used to record that nothing held it. What holds it is that running out never *selects* anything (`documentation/design/language/the-soundness-perimeter.md`): both checkers zero the remainder on the refusal, a real exhaustion propagates, and `Context::within_allowance` absorbs one only where the cap and not the declaration's remainder was the binding limit — so a capped attempt answers the same under every budget that affords the cap, and a run under a larger budget replays a smaller one step for step up to the smaller one's first real exhaustion. That is an argument from reading the sites that catch a reduction error, and this is what would see it fail: a fallback taken *because* the budget ran dry, going on to accept, shows up as a rung that accepts below one that refuses, or as a refusal that names no budget.
///
/// Each program is marginal for a different reason, so that the ladder crosses each mechanism's own threshold rather than one: a literal-depth recursion the type level evaluates, guards whose refinement keys are canonicalized under that cap, a type-level `rec` whose descent the size-change engine reads only through an unfolding — the read that answers *unread* when the budget dies under it — and a proof closed by evaluating its subject.
///
/// Every rung is compiled, the ones past the first acceptance included: the floor measurements in `reduction` walk the same ladder and stop at the first budget that accepts, which is reading a floor off the very property this asserts.
#[test]
#[ignore = "a sweep: compiles every program at every rung of a doubling ladder"]
fn acceptance_is_upward_closed_in_the_budget() {
    let programs = [
        (
            "a literal-depth packed recursion",
            r#"
            use /std/{Nat, List, Bits, Bool, Str, proc};
            let taint = List/len(proc/args!);
            let t: Bool = taint == 0;
            let grown = b[t, 1];
            let widen(n: Nat, acc: Bits) -> Bits =
                match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
            let wide = widen(30, grown);
            match Bits/len(wide) == 32
            | true => /std/print("ok\n")
            | false => /std/print("bad\n")
            end
            "#,
        ),
        (
            "guards discharging window bounds",
            r#"
            use /std/{Str, Bytes, Nat};
            let head(b : Bytes) -> Bytes =
                match 10 <= Bytes/len(b) | true => Bytes/slice(b, 0, 10) | false => x[] end;
            let interior(b : Bytes, k : Nat) -> Bytes =
                match 1 + k <= Bytes/len(b) | true => Bytes/slice(b, 1, k) | false => x[] end;
            let named = x[0x61, 0x62, 0x63];
            let over_a_definition(k : Nat) -> Bytes =
                match 1 + k <= Bytes/len(named)
                | true => Bytes/slice(named, 1, k)
                | false => x[]
                end;
            /std/print("ok")
            "#,
        ),
        (
            "a type-level recursion descending by division",
            r#"
            use /std/{Nat};
            let Good(n : Nat) -> Type =
                match n
                | 0 => {}
                | _ => Good(n / 2)
                end;
            let held : Good(4) = ();
            /std/print("ok")
            "#,
        ),
        (
            "a proof closed by evaluating its subject",
            r#"
            use /std/{Nat, Eq};
            let sum(n : Nat) -> Nat =
                match n
                | 0 => 0
                | p + 1; below => below + p
                end;
            let _held : Eq(sum(200), 19900) = Eq/refl();
            /std/print("ok")
            "#,
        ),
    ];

    let ladder = std::iter::successors(Some(1024u64), |budget| budget.checked_mul(2))
        .take_while(|budget| *budget < DEFAULT_STEP_BUDGET)
        .chain([DEFAULT_STEP_BUDGET])
        .collect::<Vec<_>>();

    for (label, source) in programs {
        let mut accepted_at = None;

        for budget in &ladder {
            match (typecheck_within(*budget, source), accepted_at) {
                (Ok(()), None) => accepted_at = Some(*budget),
                (Ok(()), Some(_)) => {}
                (Err(refusal), None) => assert!(
                    refusal.contains("ran out"),
                    "{label}, at {budget}: refused below its first acceptance for something other than the budget:\n{refusal}",
                ),
                (Err(refusal), Some(floor)) => panic!(
                    "{label}: accepted at {floor} and refused at the larger {budget}:\n{refusal}"
                ),
            }
        }

        // A program the lowest rung accepts, or the highest refuses, puts nothing to the property: the ladder has to cross its threshold.
        assert!(
            accepted_at.is_some_and(|floor| floor != ladder[0]),
            "{label}: first accepted at {accepted_at:?}, so the ladder crossed no threshold",
        );
    }
}
