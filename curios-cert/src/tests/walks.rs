//! Every walk the certifier runs over whole terms, over one doubling term: sixty levels that each hold the one below twice, a tree no walk per path finishes and a graph of sixty-one nodes — a sum of the level below with itself around a local, and a record of two fields at the level below. A walk the certifier adds joins the table here, so one a change makes per-path again stalls its row rather than waiting for a profile; the walks it shares with the elaborator are `curios-core`'s, held by that crate's table. The first conversion row compares two spellings of one graph, which equality answers; the second compares a tower of applications over a local with one over a redex that reduces to it, which only a remembered verdict answers. Inference and checking are rows over a pair of the level below with itself, sixty deep over the same local: the first infers each field and the second checks it against a record of two fields at the level below, so neither is answered by the other's table.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{Level, Reducer, Term},
};

/// A sum of the level below with itself, sixty levels deep over `base`, under the share keys from `keys` on.
fn sum(base: &str, keys: usize) -> String {
    doubled("#NatAdd(${below}, ${again})", base, 60, keys)
}

/// A record of two fields at the level below, sixty levels deep over `base`, under the share keys from `keys` on.
fn record(base: &str, keys: usize) -> String {
    doubled("{${below}, ${again}}", base, 60, keys)
}

/// A pair of the level below with itself, sixty levels deep over `base`, under the share keys from `keys` on.
fn pair(base: &str, keys: usize) -> String {
    doubled("(${below}, ${again})", base, 60, keys)
}

/// A certifier with `n: Nat`, `a: Type` and the hypotheses `more` assumed, and a fresh budget — each row spends its own, so one row's cost cannot exhaust the next — and `terms` written over them.
fn over<const N: usize>(more: &str, terms: &str) -> (Certifier<'static>, [Term; N]) {
    let mut certifier = Certifier::new(1_000_000, SYNTAX);
    let terms = under(
        &mut certifier,
        &fill!("n: #NatType, a: Type${more}", more),
        terms,
    );

    (certifier, terms)
}

/// Sixty `let`s, each a match on `g(b)` with the line before in both arms. Every arm holds its own equation, so nothing typed under one answers under the other; the tail is typed over the binders, and each line once.
fn let_chain() -> String {
    let lines = (1..=60)
        .map(|line| {
            fill!(
                "let x${line}: #NatType = #{goal = #NatType} match g(b) | false => x${before} | true => x${before} end; ",
                line,
                before = line - 1
            )
        })
        .collect::<String>();

    fill!("(let x0: #NatType = n; ${lines}x60)", lines)
}

#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let nat = term("#NatType");
    let ground = Ok(Sort::Type(Level::zero()));

    let rows: Vec<(&str, bool)> = vec![
        ("weak-head reduction", {
            let (mut certifier, [sum]) = over("", &fill!("${sum},", sum = sum("n", 0)));
            certifier.reduce_forced(sum).is_ok()
        }),
        ("conversion", {
            let (mut certifier, [sum, again]) = over(
                "",
                &fill!("${sum}, ${again}", sum = sum("n", 0), again = sum("n", 60)),
            );
            convert(&mut certifier, &nat, &sum, &again).is_ok_and(|equal| equal)
        }),
        ("conversion of two spellings", {
            let applied = |base: &str, keys| doubled("f(${below}, ${again})", base, 60, keys);
            let (mut certifier, [over_n, over_redex]) = over(
                ", f: (x: #NatType, y: #NatType) -> #NatType",
                &fill!(
                    "${over_n}, ${over_redex}",
                    over_n = applied("n", 0),
                    over_redex = applied("((y: #NatType) => y)(n)", 60)
                ),
            );
            convert(&mut certifier, &nat, &over_n, &over_redex).is_ok_and(|equal| equal)
        }),
        ("a `let` chain through arms on an expression", {
            let (mut certifier, [chain]) = over(
                ", g: (x: #BoolType) -> #BoolType, b: #BoolType",
                &fill!("${chain},", chain = let_chain()),
            );
            infer(&mut certifier, &chain) == Ok(nat.clone())
        }),
        ("the sort of a closed type", {
            let (mut certifier, [record]) =
                over("", &fill!("${record},", record = record("#NatType", 0)));
            Sort::of(&mut certifier, &record) == ground
        }),
        ("the sort of a type over a local", {
            let (mut certifier, [record]) = over("", &fill!("${record},", record = record("a", 0)));
            Sort::of(&mut certifier, &record) == ground
        }),
        ("inference", {
            let (mut certifier, [pair]) = over("", &fill!("${pair},", pair = pair("n", 0)));
            infer(&mut certifier, &pair).is_ok()
        }),
        ("checking", {
            let (mut certifier, [pair, record]) = over(
                "",
                &fill!(
                    "${pair}, ${record}",
                    pair = pair("n", 0),
                    record = record("#NatType", 60)
                ),
            );
            check(&mut certifier, &pair, &record).is_ok()
        }),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term did not answer");
    }
}
