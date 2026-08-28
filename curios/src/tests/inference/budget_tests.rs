//! The per-declaration reduction budget, at both edges.
//!
//! A budget is a refusal about cost rather than about meaning, so each row is a pair in spirit: a computation the default budget affords, and one no budget should. A regression here shows up as a program that used to compile and now cannot, or as one that spends unboundedly before saying so.

use crate::tests::{error, run};

// A `rec` that never reduces (`go : Bool = go`) forces forever when demanded in type position — same infinite-spin behavior as a top-level `rec` — so a step budget stops it with an error rather than hanging.
#[test]
fn nonproductive_inner_rec_in_type_position_exhausts_its_budget() {
    let source = r#"
        use /std/{Bool};
        let spin : Bool =
            rec go : Bool = go;
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
        use /std/{Nat, List, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let grown = b[t, 1];
        rec widen(n: Nat, acc: Bits) -> Bits =
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
        use /std/{Nat, List, Bits, Bool, Str, Handle, proc};
        let taint = List/len(proc/args!);
        let t: Bool = taint == 0;
        let grown = b[t, 1];
        rec widen(n: Nat, acc: Bits) -> Bits =
            match n | 0 => acc | _ => widen(n - 1, b[..acc, t]) end;
        let wide = widen(30, grown);
        match Bits/len(wide) == 32
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert_eq!(run(source), b"ok\n");
}
