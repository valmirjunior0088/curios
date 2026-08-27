//! Structure occurrences at their declared parameter count, and the syntax elaboration may leave behind.

use {
    crate::{KernelError, check, infer},
    curios_core::{
        Global, InductDecl, Intrinsic, MetaId, StructType, Subterm, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::Qualifier,
};

use super::test_support::*;

/// A list or a cell *of* proofs is not a proposition, and the typing rule has to say so — `Sort::of` already does.
///
/// The two answers came from two implementations of one rule. `Sort::of` routes a parameterized former through `sort_of_intrinsic`, which lands a `Prop`-sorted element at `Type 0` on the reasoning `sort/tests.rs` states: a list has a length and a cell has an identity, so their inhabitants are distinguishable however indistinguishable the elements are. The typing rule computed the *element's* sort instead and reported that as the former's, so `List(P)` inferred at `Prop` while `Sort::of(List(P))` said `Type 0`.
///
/// Only one of those can be right, and the disagreement is a closed inhabitant of `False`. `Prop` is the type of propositions, so a former admitted there stands wherever one is wanted: at `(X : Prop, x : X, y : X) -> Eq(@X, x, y)` — reflexivity discharges it, since irrelevance identifies any two inhabitants of `X` — instantiating `X` at `List(P)` yields `Eq(List(P), [p], [])` for a one-element list against the empty one. Congruence through `List/len` carries that to `Eq(1, 0)`, and transport turns `()` into a proof of `False`.
///
/// Verified while the hole was open: `check(List(P), Prop)` returned `Ok(())`, `check_definition` accepted that lemma, and `infer` on the instantiated application returned `Eq(List P, [p], [])`. The surface route was live too — `curios/src/tests/perimeter::a_list_of_proofs_is_not_a_proposition` is the program, which elaborated and which the compile-path recheck certified. It stopped short of a runtime only in erasure, which refuses any call whose every argument erases; that is a separate defect of the erase boundary, and the same lemma at a genuine proposition trips it identically.
///
/// The controls are the other half. A list at a relevant element still reports that element's level, `List(Type 0)` included, so the fix cannot have pinned every former at zero; and a genuine proposition still stands where a `Prop` is wanted, so it cannot have closed the hole by refusing the position outright.
#[test]
fn a_list_or_cell_of_proofs_is_not_a_proposition() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["P"]));

    // `induct P : Prop end` — an empty proposition, so `Sort::of` reads `Prop` off its declaration.
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort: Term::prop(),
            module: Qualifier::from(["P"]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );
    let proposition = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    let list = Term::intrinsic(Intrinsic::ListType(proposition.clone()));
    let cell = Term::intrinsic(Intrinsic::CellType(proposition.clone()));

    assert_eq!(infer(&mut kernel, &list), Ok(Term::type_ground()));
    assert_eq!(infer(&mut kernel, &cell), Ok(Term::type_ground()));

    assert!(matches!(
        check(&mut kernel, &list, &Term::prop()),
        Err(KernelError::Mismatch { .. }),
    ));
    assert!(matches!(
        check(&mut kernel, &cell, &Term::prop()),
        Err(KernelError::Mismatch { .. }),
    ));

    // A former still carries its element's level rather than being pinned at zero, and a proposition still stands where one is wanted.
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::ListType(nat_type()))
        ),
        Ok(Term::type_ground()),
    );
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::ListType(Term::type_ground())),
        ),
        Ok(Term::type_at(one())),
    );
    assert_eq!(check(&mut kernel, &proposition, &Term::prop()), Ok(()));
}

/// Elaboration-only syntax reaching the kernel means a term was handed over before elaboration finished with it.
#[test]
fn elaboration_only_syntax_is_refused() {
    let mut kernel = kernel();

    let metavar = Term::hole(MetaId::from(0usize));
    assert!(matches!(
        infer(&mut kernel, &metavar),
        Err(KernelError::NotCore(_)),
    ));
}

/// A nominal occurrence states as many parameters as its declaration declares, and the two rules that open a `struct` arity at an occurrence's parameters must check that before opening it.
///
/// `Telescope::open` asserts on a count mismatch, so an unguarded rule does not refuse the item — it **aborts the walk**, losing every other verdict, which is what makes `recheck_module_verdicts`' count a count. `Sort::of` and `synth_neutral` both guarded this; `infer`'s projection rule and `check`'s record rule reached `open` behind `check_instance` alone, which decides the *universe* width and says nothing about the parameters.
///
/// Neither is reachable from a surface program — the elaborator does not emit an occurrence at the wrong count — so this is the shape that needs a term built by hand, and it is a fail-open-to-abort rather than an unsoundness. Both rules are reached here at a one-parameter family occurring with none: the projection through a variable assumed at that type, and the record through a literal checked against it.
#[test]
fn a_structure_occurrence_at_the_wrong_parameter_count_is_refused() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);

    let short: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: Vec::new(),
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &short);

    assert!(
        matches!(
            infer(&mut kernel, &Term::proj(Term::free_var(&value), 0)),
            Err(KernelError::Arity { .. }),
        ),
        "a projection opened the arity at a parameter count the declaration does not have",
    );

    assert!(
        matches!(
            check(&mut kernel, &Term::tuple([nat(0)]), &short),
            Err(KernelError::Arity { .. }),
        ),
        "a record literal opened the arity at a parameter count the declaration does not have",
    );
}

/// The control: at the parameter count it declares, the same family still projects and still accepts a record.
///
/// Without it the guard above would pass just as well if the rules refused every `struct` occurrence.
#[test]
fn a_structure_occurrence_at_its_declared_parameter_count_still_works() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);

    let exact: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: vec![nat_type()],
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &exact);

    assert_eq!(
        infer(&mut kernel, &Term::proj(Term::free_var(&value), 0)),
        Ok(nat_type()),
    );
    assert_eq!(check(&mut kernel, &Term::tuple([nat(7)]), &exact), Ok(()));
}
