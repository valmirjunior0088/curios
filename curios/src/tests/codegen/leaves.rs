//! A body a match writes at more than one leaf is emitted once.

use crate::test_support::*;

/// `pick`'s second arm, `(_, none())`, is written at two leaves of the match, `(some(_), none())` and `(none(), none())`, and reads only `n`: a sum of forty products of `n` modulo primes, each product reduced modulo `65537`. Its input is the count of the program's arguments plus seven, so each leaf is reached by a run.
const SHARED: &str = r#"
    use /std/{Nat, List, Option, Proc};
    let half(n: Nat) -> Option(Nat) = match n % 2 | 0 => Option/some(n) | _ => Option/none() end;
    let third(n: Nat) -> Option(Nat) = match n % 3 | 0 => Option/some(n) | _ => Option/none() end;
    let pick(a: Option(Nat), b: Option(Nat), n: Nat) -> Nat =
        match (a, b)
        | (some(x), some(y)) => x + y
        | (_, none()) => (((n * 3 + 424243) % 1000003 * (n + 13) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 26) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 39) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 52) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 65) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 78) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 91) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 104) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 117) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 130) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 143) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 156) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 169) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 182) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 195) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 208) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 221) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 234) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 247) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 260) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 273) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 286) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 299) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 312) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 325) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 338) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 351) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 364) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 377) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 390) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 403) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 416) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 429) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 442) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 455) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 468) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 481) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 494) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 507) % 65537) + ((n * 3 + 424243) % 1000003 * (n + 520) % 65537)) % 100003
        | (none(), some(y)) => y
        end;
    let entry() -> /std/Io({}) =
        let n = List/length(Proc/args()!) + 7;
        /std/print(Nat/to_str(pick(half(n), third(n), n)));
    entry()
    "#;

/// `pick`'s second arm binds `x` at two positions, `(some(x), none())` and `(none(), some(x))`, so its two leaves read two values: the sum of `SHARED`'s forty products, over `x`.
const ALTERNATIVES: &str = r#"
    use /std/{Nat, List, Option, Proc};
    let half(n: Nat) -> Option(Nat) = match n % 2 | 0 => Option/some(n) | _ => Option/none() end;
    let third(n: Nat) -> Option(Nat) = match n % 3 | 0 => Option/some(n) | _ => Option/none() end;
    let pick(a: Option(Nat), b: Option(Nat), n: Nat) -> Nat =
        match (a, b)
        | (some(x), some(y)) => x + y
        | (some(x), none()) | (none(), some(x)) => (((x * 3 + 424243) % 1000003 * (x + 13) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 26) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 39) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 52) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 65) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 78) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 91) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 104) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 117) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 130) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 143) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 156) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 169) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 182) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 195) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 208) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 221) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 234) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 247) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 260) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 273) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 286) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 299) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 312) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 325) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 338) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 351) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 364) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 377) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 390) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 403) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 416) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 429) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 442) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 455) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 468) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 481) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 494) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 507) % 65537) + ((x * 3 + 424243) % 1000003 * (x + 520) % 65537)) % 100003
        | (none(), none()) => n
        end;
    let entry() -> /std/Io({}) =
        let n = List/length(Proc/args()!) + 7;
        /std/print(Nat/to_str(pick(half(n), third(n), n)));
    entry()
    "#;

/// The reductions modulo `65537` `text` holds, forty to a copy of the shared body.
fn reductions(text: &str) -> usize {
    text.matches("65537").count()
}

/// A body written at two leaves that reads only what both leaves share is emitted once: the optimized module and the WebAssembly each hold one copy of its forty reductions, and each leaf answers what it computes.
///
/// Mutation-checked: with no class merged, each holds two copies.
#[test]
fn a_body_written_at_two_leaves_is_emitted_once() {
    assert_eq!(reductions(&cps_optm(SHARED)), 40);
    assert_eq!(reductions(&wat(SHARED)), 40);
    for (args, answer) in [
        (&[][..], "62984"),
        (&["a"][..], "61330"),
        (&["a", "b"][..], "9"),
        (&["a", "b", "c", "d", "e"][..], "24"),
    ] {
        assert_eq!(
            String::from_utf8(run_raw(SHARED, args)).unwrap().trim(),
            answer,
            "{args:?}"
        );
    }
}

/// A body written at two leaves that read it apart, past the line the inliner copies a callee at, is emitted once too, what each leaf binds handed to it: the optimized module and the WebAssembly each hold one copy of its forty reductions, and each leaf answers what it computes.
///
/// Mutation-checked: with a class kept only where its members read alike, each holds two copies.
#[test]
fn a_body_two_leaves_bind_apart_is_emitted_once() {
    assert_eq!(reductions(&cps_optm(ALTERNATIVES)), 40);
    assert_eq!(reductions(&wat(ALTERNATIVES)), 40);
    for (args, answer) in [
        (&[][..], "7"),
        (&["a"][..], "61330"),
        (&["a", "b"][..], "90987"),
        (&["a", "b", "c", "d", "e"][..], "24"),
    ] {
        assert_eq!(
            String::from_utf8(run_raw(ALTERNATIVES, args))
                .unwrap()
                .trim(),
            answer,
            "{args:?}"
        );
    }
}
