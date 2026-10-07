//! The arm rule of a fold: an arm reduction takes at every tail learns nothing of the scrutinee.

use {
    super::test_support::*,
    crate::{Error, Kernel, infer},
    curios_core::Term,
};

/// What a fold at `P` is built from: an opaque `P : (Nat) -> Type` with `z : P(0)` and `s : (k : Nat, P(k)) -> P(k + 1)` to inhabit its arms honestly, and a scrutinee `n : Nat` with `h : P(n)`, a hypothesis about it.
struct Scene {
    p: Term,
    z: Term,
    s: Term,
    n: Term,
    h: Term,
}

fn scene(kernel: &mut Kernel) -> Scene {
    let p = opaque_family(kernel, binder(50, "P"));
    let at = |index: Term| Term::apply(p.clone(), [index]);

    let z = binder(51, "z");
    let s = binder(52, "s");
    let n = binder(53, "n");
    let h = binder(54, "h");
    let k = binder(55, "k");

    kernel.assume(&z, &at(nat(0)));
    kernel.assume(
        &s,
        &Term::func_type(
            [(k, nat_type()), (binder(56, "x"), at(Term::free_var(&k)))],
            at(succ(Term::free_var(&k))),
        ),
    );
    kernel.assume(&n, &nat_type());
    kernel.assume(&h, &at(Term::free_var(&n)));

    Scene {
        p,
        z: Term::free_var(&z),
        s: Term::free_var(&s),
        n: Term::free_var(&n),
        h: Term::free_var(&h),
    }
}

/// `match scrutinee : (m) => P(m) | 0 => identity | pred + 1; ih => cons(pred, ih) end`.
fn fold(
    scene: &Scene,
    scrutinee: Term,
    identity: Term,
    cons: impl FnOnce(Term, Term) -> Term,
) -> Term {
    let m = binder(60, "m");
    let pred = binder(61, "pred");
    let ih = binder(62, "ih");

    Term::nat_match(
        scrutinee,
        Some(&m),
        Term::apply(scene.p.clone(), [Term::free_var(&m)]),
        identity,
        &pred,
        &ih,
        cons(Term::free_var(&pred), Term::free_var(&ih)),
    )
}

/// The step every honest cons arm takes: `s(pred, ih)`.
fn step(scene: &Scene) -> impl FnOnce(Term, Term) -> Term + '_ {
    |pred, ih| Term::apply(scene.s.clone(), [pred, ih])
}

/// The control: arms that inhabit the motive at their case values, reading nothing of the scrutinee, are a fold typed at the motive of the scrutinee.
#[test]
fn a_fold_whose_arms_inhabit_the_motive_at_their_cases_is_typed_at_its_scrutinee() {
    let mut kernel = kernel();
    let scene = scene(&mut kernel);
    let term = fold(&scene, scene.n.clone(), scene.z.clone(), step(&scene));

    assert_eq!(
        infer(&mut kernel, &term),
        Ok(Term::apply(scene.p.clone(), [scene.n.clone()]))
    );
}

/// The identity arm of a fold is taken at the end of every value, where the scrutinee is whatever it was: `h : P(n)` does not inhabit `P(0)` there. Typed with `n` at the arm's case it would, and the fold at `2` would hand back as `P(0)` a proof of `P(2)`.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the fold is accepted.
#[test]
fn the_identity_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    let mut kernel = kernel();
    let scene = scene(&mut kernel);
    let term = fold(&scene, scene.n.clone(), scene.h.clone(), step(&scene));

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. })
    ));
}

/// The cons arm is taken at every tail, and `n` is `pred + 1` at the outermost alone: `h : P(n)` does not inhabit `P(pred + 1)`, here behind a function that takes the hypothesis and drops it, so the arm reads one.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the fold is accepted.
#[test]
fn the_cons_arm_of_a_fold_reads_its_scrutinee_at_no_case() {
    let mut kernel = kernel();
    let scene = scene(&mut kernel);
    let term = fold(&scene, scene.n.clone(), scene.z.clone(), |pred, ih| {
        let dropped = Term::func(
            [(binder(63, "x"), Term::apply(scene.p.clone(), [pred]))],
            scene.h.clone(),
        );
        Term::apply(dropped, [ih])
    });

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. })
    ));
}

/// A scrutinee that is no variable is substituted nowhere, and an arm taken once reads it through a recorded equation. An arm of a fold records none: under `e : P(g(n))`, the identity arm of a fold over `g(n)` does not read `e` at `P(0)`.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the fold is accepted.
#[test]
fn an_arm_of_a_fold_over_a_call_records_no_equation() {
    let mut kernel = kernel();
    let scene = scene(&mut kernel);
    let g = binder(57, "g");
    let e = binder(58, "e");
    kernel.assume(
        &g,
        &Term::func_type([(binder(59, "y"), nat_type())], nat_type()),
    );
    let call = Term::apply(Term::free_var(&g), [scene.n.clone()]);
    kernel.assume(&e, &Term::apply(scene.p.clone(), [call.clone()]));

    let term = fold(&scene, call, Term::free_var(&e), step(&scene));

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(Error::Mismatch { .. })
    ));
}

/// A case split is taken once, at the scrutinee itself, and keeps the specialization: the same match with a cons arm that reads no hypothesis has `h` at `P(0)` in one arm and at `P(pred + 1)` in the other.
#[test]
fn an_arm_of_a_case_split_reads_its_scrutinee_at_its_case() {
    let mut kernel = kernel();
    let scene = scene(&mut kernel);
    let term = fold(&scene, scene.n.clone(), scene.h.clone(), |_, _| {
        scene.h.clone()
    });

    assert_eq!(
        infer(&mut kernel, &term),
        Ok(Term::apply(scene.p.clone(), [scene.n.clone()]))
    );
}
