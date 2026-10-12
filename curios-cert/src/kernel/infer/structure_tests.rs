//! Structure occurrences at their declared parameter count, and the syntax elaboration may leave behind.

use {
    super::test_support::*,
    crate::{Error, check, infer},
    curios_core::{
        Global, InductDecl, Intrinsic, MetavarId, Struct, StructType, Subterm, Telescope, Term,
        Transient, UniverseContext, Universes,
    },
    curios_utilities::Qualifier,
};

/// A list or a cell *of* proofs is not a proposition, and the typing rule has to say so — `Sort::of` already does.
///
/// `Sort::of` routes a parameterized former through `sort_of_intrinsic`, which lands a `Prop`-sorted element at `Type 0` on the reasoning `sort/tests.rs` states: a list has a length and a cell has an identity, so their inhabitants are distinguishable however indistinguishable the elements are. The typing rule must agree: reporting the *element's* sort as the former's would infer `List(P)` at `Prop` while `Sort::of(List(P))` says `Type 0`.
///
/// Only one of those can be right, and the disagreement is a closed inhabitant of `False`. `Prop` is the type of propositions, so a former admitted there stands wherever one is wanted: at `(X : Prop, x : X, y : X) -> Same(@X)(x, y)`, an equation between two proofs of `X` — its one constructor discharges it, since irrelevance identifies any two inhabitants of `X` — instantiating `X` at `List(P)` yields `Same(@List(P))([p], [])` for a one-element list against the empty one. Congruence through `List/len` carries that to `1` and `0`, and transport turns `()` into a proof of `False`.
///
/// `curios/src/tests/board`'s `a_list_of_proofs_is_not_a_proposition` is the surface program.
///
/// The controls are the other half. A list at a relevant element still reports that element's level, `List(Type 0)` included, so the rule does not pin every former at zero; and a genuine proposition still stands where a `Prop` is wanted, so it does not refuse the position outright.
#[test]
fn a_list_or_cell_of_proofs_is_not_a_proposition() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["P"]));

    // `induct P : Prop end` — an empty proposition, so `Sort::of` reads `Prop` off its declaration.
    kernel
        .declare_induct(
            &name,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new(),
                result_sort: Term::prop(),
                module: Qualifier::from(["P"]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");
    let proposition = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    let list = Term::intrinsic(Intrinsic::ListType(proposition.clone()));
    let cell = Term::intrinsic(Intrinsic::CellType(proposition.clone()));

    assert_eq!(infer(&mut kernel, &list), Ok(Term::type_ground()));
    assert_eq!(infer(&mut kernel, &cell), Ok(Term::type_ground()));

    assert!(matches!(
        check(&mut kernel, &list, &Term::prop()),
        Err(Error::Mismatch { .. }),
    ));
    assert!(matches!(
        check(&mut kernel, &cell, &Term::prop()),
        Err(Error::Mismatch { .. }),
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

    // A derived witness body that elaboration never wrote is the same class of residue as a metavariable: the transient is refused wholesale, so no derivation can smuggle an unchecked body past the kernel.
    let derive = Term::from(Subterm::Transient(Transient::Derive));
    assert!(matches!(
        infer(&mut kernel, &derive),
        Err(Error::NotCore(_)),
    ));

    let metavar = Term::hole(MetavarId::from(0usize));
    assert!(matches!(
        infer(&mut kernel, &metavar),
        Err(Error::NotCore(_)),
    ));
}

/// A nominal occurrence states as many parameters as its declaration declares, and the two rules that open a `struct` arity at an occurrence's parameters must check that before opening it.
///
/// `Telescope::open` asserts on a count mismatch, so an unguarded rule does not refuse the item — it **aborts the walk**, losing every other verdict, which is what makes `recheck_module_verdicts`' count a count. `check_instance` decides the *universe* width and says nothing about the parameters, so `infer`'s projection rule and `check`'s record rule reach `open` only through the checked handle, as `Sort::of` and `synth_neutral` do.
///
/// Neither is reachable from a surface program — the elaborator does not emit an occurrence at the wrong count — so this is the shape that needs a term built by hand, and it is a fail-open-to-abort rather than an unsoundness. Both rules are reached here at a one-parameter family occurring with none: the projection through a variable assumed at that type, and the record through its literal, which states the parameters it is built at.
#[test]
fn a_structure_occurrence_at_the_wrong_parameter_count_is_refused() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);

    let short: Term = Subterm::StructType(StructType {
        name,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &short);

    assert!(
        matches!(
            infer(
                &mut kernel,
                &Term::proj_struct(Term::free_var(&value), name, 0)
            ),
            Err(Error::Arity { .. }),
        ),
        "a projection opened the arity at a parameter count the declaration does not have",
    );

    assert!(
        matches!(
            infer(&mut kernel, &boxed(name, Vec::new(), nat(0))),
            Err(Error::Arity { .. }),
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
        universes: Universes::default(),
        params: vec![nat_type()],
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &exact);

    assert_eq!(
        infer(
            &mut kernel,
            &Term::proj_struct(Term::free_var(&value), name, 0)
        ),
        Ok(nat_type()),
    );
    assert_eq!(
        check(&mut kernel, &boxed(name, vec![nat_type()], nat(7)), &exact),
        Ok(())
    );
}

/// A structure's type has one literal, the structure's own: a tuple literal of the same fields is no value of it.
///
/// Elaboration refuses the literal there, so only a term built by hand holds one, and a rule that took it field by field as a Σ's is taken admitted a value nothing reads: a projection out of a structure names it and reads a literal of that structure alone, so `.0` of a binder bound to `(7)` at `Boxed(Nat)` was typed at `Nat` and reduced to no literal. The control above is the literal that is one.
#[test]
fn a_tuple_literal_is_no_value_of_a_structure() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);

    assert!(matches!(
        check(
            &mut kernel,
            &Term::tuple([nat(7)]),
            &Term::struct_type(name, [nat_type()])
        ),
        Err(Error::Mismatch { .. }),
    ));
}

/// The literal of the one-field structure [`parameterized_struct`] declares, at `params`, holding `value`: entry-free, as elaboration leaves one.
fn boxed(name: Global, params: Vec<Term>, value: Term) -> Term {
    Subterm::Struct(Struct {
        name,
        universes: Universes::default(),
        params,
        fields: vec![value],
        entries: Vec::new(),
    })
    .into()
}

/// A projection says what its slot is a slot of, and the kernel holds that to its head's type.
///
/// The name is what a reader of the term reads the field by with no type in hand: a report writes a structure's field by its label or its place among the plain ones, which it looks up under the name the projection carries. A name nothing held to the head's type would be read as a field of a structure the value is none of, so each way the two can part is refused: a tuple's slot off a structure's value, a structure's off a tuple, and a slot of one structure off a value of another. The control above is the pair that meets.
#[test]
fn a_slot_of_one_thing_read_off_a_value_of_another_is_refused() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);
    let other = Global::Authored(Qualifier::from(["Other"]));

    let exact: Term = Subterm::StructType(StructType {
        name,
        universes: Universes::default(),
        params: vec![nat_type()],
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &exact);
    let pair = binder(3, "p");
    kernel.assume(&pair, &Term::tuple_type([(binder(4, "a"), nat_type())]));

    assert!(matches!(
        infer(&mut kernel, &Term::proj(Term::free_var(&value), 0)),
        Err(Error::NotASlotOf { named: None, .. }),
    ));
    assert!(matches!(
        infer(&mut kernel, &Term::proj_struct(Term::free_var(&pair), name, 0)),
        Err(Error::NotASlotOf { named: Some(named), .. }) if named == name,
    ));
    assert!(matches!(
        infer(&mut kernel, &Term::proj_struct(Term::free_var(&value), other, 0)),
        Err(Error::NotASlotOf { named: Some(named), .. }) if named == other,
    ));

    // The tuple's own slot still reads.
    assert_eq!(
        infer(&mut kernel, &Term::proj(Term::free_var(&pair), 0)),
        Ok(nat_type()),
    );
}
