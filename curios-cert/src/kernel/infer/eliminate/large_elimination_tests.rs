//! The large-elimination guard: a singleton whose index pins its payload eliminates into a type, and every other proposition with a constructor eliminates into a proposition alone.

use {
    super::test_support::*,
    crate::{Error, infer},
    curios_core::{Term, UniverseContext},
};

/// A proposition whose single constructor's payload is *pinned* by its index target — the `Eq`/`refl` shape. Matching `(z)` against a value recovers `z`, so eliminating tells a program nothing it did not already know, and the large elimination is admitted, Without this, `Eq/subst` is unstatable.
#[test]
fn a_singleton_whose_index_pins_its_payload_eliminates_into_a_type() {
    let mut kernel = kernel();
    let z = binder(0, "z");
    let arm_binder = binder(10, "z");

    let family = declare(
        &mut kernel,
        "Pinned",
        Term::prop(),
        vec![carrying("refl", z, nat_type(), Term::free_var(&z))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("refl", vec![arm_binder], Term::free_var(&arm_binder))],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// The same shape with a *non-injective* index target. `blur(a)` mentions `a`, but knowing `blur(a)` recovers nothing — `blur` need not be injective, and in the program this rule exists for it is the constant zero. So the payload is not pinned, the proposition carries something a program could read back, and eliminating it into a relevant type is refused.
///
/// Reading occurrence as determination is exactly the defect this rule exists to avoid: it admits this elimination, and a closed inhabitant of `False` follows from it.
#[test]
fn a_singleton_whose_index_merely_mentions_its_payload_does_not() {
    let mut kernel = kernel();
    let a = binder(0, "a");
    let blur = binder(1, "blur");
    let arm_binder = binder(10, "a");

    kernel
        .declare(
            &blur,
            &Term::func_type([(binder(2, "n"), nat_type())], nat_type()),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");

    let family = declare(
        &mut kernel,
        "Loose",
        Term::prop(),
        vec![carrying(
            "mk",
            a,
            nat_type(),
            Term::apply(Term::free_var(&blur), [Term::free_var(&a)]),
        )],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("mk", vec![arm_binder], Term::free_var(&arm_binder))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(Error::LargeElimination(family)),
    );
}

/// The same shape with a payload that is *itself a type*. `mk(a : Type 0)` pins nothing — the index target is the constant `0` — so eliminating into `Type` hands a program the type the proposition was built with, while irrelevance says every inhabitant is the same one. `mk({})` and `mk(False)` are convertible at a `Prop`-sorted family, `refl` therefore proves `Eq()(mk({}), mk(False))`, congruence through this eliminator carries that to `Eq()({}, False)`, and `subst` turns `()` into a closed inhabitant of `False`.
///
/// A component whose *type is a universe* carries information however completely erasure deletes it: erasure governs what the runtime observes, while irrelevance is a claim about definitional equality, and conversion reads this component back in full. `curios-elab`'s `singleton_eliminable` requires each binder to be `Prop`-sorted or index-pinned and refuses every surface spelling, so only a hand-built derivation reaches this rule; `crate::recheck::proposition_tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds the whole-module walk to it.
///
/// `a_singleton_whose_index_pins_its_payload_eliminates_into_a_type` is the control: a payload the targets do recover must stay eliminable, or `Eq/subst` becomes unstatable.
#[test]
fn a_singleton_carrying_a_type_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();
    let a = binder(0, "a");
    let arm_binder = binder(10, "a");

    let family = declare(
        &mut kernel,
        "TypeBox",
        Term::prop(),
        vec![carrying("mk", a, Term::type_ground(), nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        Term::type_ground(),
        vec![("mk", vec![arm_binder], Term::free_var(&arm_binder))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(Error::LargeElimination(family)),
    );
}

/// The other half of the same clause: a payload whose type is `Prop` holds a proposition *as data* rather than proving one. `Sort::of(Prop)` is `Type 0`, so the component is informative for exactly the reason above — `mk(True)` and `mk(False)` are convertible at a `Prop`-sorted family while this eliminator tells them apart, and transporting a proof of `True` along the resulting equality inhabits `False`.
///
/// Worth its own fixture: a clause excusing universe-typed components would name a sort of either quality, and one that named `Type` alone would leave this open.
#[test]
fn a_singleton_carrying_a_proposition_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();
    let a = binder(0, "a");
    let arm_binder = binder(10, "a");

    let family = declare(
        &mut kernel,
        "PropBox",
        Term::prop(),
        vec![carrying("mk", a, Term::prop(), nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        Term::type_ground(),
        vec![("mk", vec![arm_binder], Term::free_var(&arm_binder))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(Error::LargeElimination(family)),
    );
}

/// A payload that is itself a proof is not excused. Irrelevance makes any two of them interchangeable, so nothing a program computes could tell which it was handed; but the arm is handed the proof the match was given, and a recursion along such a payload has a type reduce the lemma that built it. A proposition holding a proof no index pins is taken apart only into a proposition.
///
/// The distinction from the fixture above is what the payload is. There its type is `Prop` itself, so the payload *is* a proposition carried as data; here its type is a family declared at `Prop`, so the payload inhabits one. Neither is a payload an index pins, which is the one thing the guard asks.
#[test]
fn a_singleton_carrying_a_proof_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();
    let p = binder(0, "p");
    let arm_binder = binder(10, "p");

    let held = declare(
        &mut kernel,
        "Held",
        Term::prop(),
        vec![nullary("qed", nat(0))],
    );
    let proof = Term::induct_type(held, Vec::<Term>::new(), [nat(0)]);

    let family = declare(
        &mut kernel,
        "ProofBox",
        Term::prop(),
        vec![carrying("mk", p, proof, nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("mk", vec![arm_binder], nat(0))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(Error::LargeElimination(family)),
    );
}

/// An empty proposition eliminates into anything: there is nothing to have received, so nothing to extract.
#[test]
fn an_empty_proposition_eliminates_into_a_type() {
    let mut kernel = kernel();

    let family = declare(&mut kernel, "Absurd", Term::prop(), Vec::new());
    let term = eliminate(&mut kernel, &family, nat(0), nat_type(), Vec::new());

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// Two constructors mean the value says *which*, which is information a relevant result could branch on.
#[test]
fn a_proposition_with_two_constructors_does_not_eliminate_into_a_type() {
    let mut kernel = kernel();

    let family = declare(
        &mut kernel,
        "Two",
        Term::prop(),
        vec![nullary("here", nat(0)), nullary("there", nat(0))],
    );

    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        nat_type(),
        vec![("here", Vec::new(), nat(1)), ("there", Vec::new(), nat(2))],
    );

    assert_eq!(
        infer(&mut kernel, &term),
        Err(Error::LargeElimination(family)),
    );
}

/// The guard is about *relevance*, not about propositions as such: eliminating a proposition into another proposition is always fine, since irrelevance makes the result indistinguishable either way.
#[test]
fn a_proposition_eliminates_into_a_proposition_however_many_constructors() {
    let mut kernel = kernel();

    let target = declare(
        &mut kernel,
        "Target",
        Term::prop(),
        vec![nullary("only", nat(0))],
    );
    let target_type = Term::induct_type(target, Vec::<Term>::new(), [nat(0)]);

    let family = declare(
        &mut kernel,
        "Two",
        Term::prop(),
        vec![nullary("here", nat(0)), nullary("there", nat(0))],
    );

    let proof = Term::variant(target, Vec::<Term>::new(), "only", Vec::<Term>::new());
    let term = eliminate(
        &mut kernel,
        &family,
        nat(0),
        target_type.clone(),
        vec![
            ("here", Vec::new(), proof.clone()),
            ("there", Vec::new(), proof),
        ],
    );

    assert_eq!(infer(&mut kernel, &term), Ok(target_type));
}
