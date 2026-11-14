//! The arm rule of a fold: an arm reduction takes at every tail learns nothing of the scrutinee.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::Term,
};

/// What a fold at `P` is built from: an opaque `P : (Nat) -> Type` with `z : P(0)` and `s : (k : Nat, P(k)) -> P(k + 1)` to inhabit its arms honestly, and a scrutinee `n : Nat` with `h : P(n)`, a hypothesis about it.
const SCENE: &str =
    "z: /P(0), s: (k: #NatType, x: /P(k)) -> /P(#Nat(Succ(1, k))), n: #NatType, h: /P(n)";

/// `terms`, written over [`SCENE`]'s hypotheses and `more`, opened in `certifier` with `P` declared.
fn scene<const N: usize>(certifier: &mut Certifier, more: &str, terms: &str) -> [Term; N] {
    opaque(certifier, "P", "(n: #NatType) -> Type");

    under(
        certifier,
        &fill!("${scene}${more}", scene = SCENE, more),
        terms,
    )
}

/// `match scrutinee : (m) => P(m) | 0 => identity | pred + 1; ih => cons end`.
fn fold(scrutinee: &str, identity: &str, cons: &str) -> String {
    fill!(
        "match ${scrutinee}: (m) => /P(m) | 0 => ${identity} | pred + 1; ih => ${cons} end",
        scrutinee,
        identity,
        cons
    )
}

/// The step every honest cons arm takes.
const STEP: &str = "s(pred, ih)";

/// The control: arms that inhabit the motive at their case values, reading nothing of the scrutinee, are a fold typed at the motive of the scrutinee.
#[test]
fn a_fold_whose_arms_inhabit_the_motive_at_their_cases_is_typed_at_its_scrutinee() {
    let mut certifier = certifier();
    let [folded, at_n] = scene(
        &mut certifier,
        "",
        &fill!("${fold}, /P(n)", fold = fold("n", "z", STEP)),
    );

    assert_eq!(infer(&mut certifier, &folded), Ok(at_n));
}

/// The identity arm of a fold is taken at the end of every value, where the scrutinee is whatever it was: `h : P(n)` does not inhabit `P(0)` there. Typed with `n` at the arm's case it would, and the fold at `2` would hand back as `P(0)` a proof of `P(2)`.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the fold is accepted.
#[test]
fn the_identity_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    let mut certifier = certifier();
    let [folded] = scene(
        &mut certifier,
        "",
        &fill!("${fold},", fold = fold("n", "h", STEP)),
    );

    assert!(matches!(
        infer(&mut certifier, &folded),
        Err(Error::Mismatch { .. })
    ));
}

/// The cons arm is taken at every tail, and `n` is `pred + 1` at the outermost alone: `h : P(n)` does not inhabit `P(pred + 1)`, here behind a function that takes the hypothesis and drops it, so the arm reads one.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the fold is accepted.
#[test]
fn the_cons_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    let mut certifier = certifier();
    let [folded] = scene(
        &mut certifier,
        "",
        &fill!(
            "${fold},",
            fold = fold("n", "z", "((x: /P(pred)) => h)(ih)")
        ),
    );

    assert!(matches!(
        infer(&mut certifier, &folded),
        Err(Error::Mismatch { .. })
    ));
}

/// A scrutinee that is no variable is substituted nowhere, and an arm taken once reads it through a recorded equation. An arm of a fold records none: under `e : P(g(n))`, the identity arm of a fold over `g(n)` does not read `e` at `P(0)`.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the fold is accepted.
#[test]
fn an_arm_of_a_fold_over_a_call_records_no_equation() {
    let mut certifier = certifier();
    let [folded] = scene(
        &mut certifier,
        ", g: (y: #NatType) -> #NatType, e: /P(g(n))",
        &fill!("${fold},", fold = fold("g(n)", "e", STEP)),
    );

    assert!(matches!(
        infer(&mut certifier, &folded),
        Err(Error::Mismatch { .. })
    ));
}

/// A case split is taken once, at the scrutinee itself, and keeps the specialization: the same match with a cons arm that reads no hypothesis has `h` at `P(0)` in one arm and at `P(pred + 1)` in the other.
#[test]
fn an_arm_of_a_case_split_reads_its_scrutinee_at_its_case() {
    let mut certifier = certifier();
    let [split, at_n] = scene(
        &mut certifier,
        "",
        &fill!("${fold}, /P(n)", fold = fold("n", "h", "h")),
    );

    assert_eq!(infer(&mut certifier, &split), Ok(at_n));
}
