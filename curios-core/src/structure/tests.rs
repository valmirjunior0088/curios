//! A structure read a facet at a time answers what it answers read whole.

use {crate::*, curios_utilities::Qualifier};

/// `struct Dep(A: Type, B: Type) { x: A, y: B, z: A(x, y), w: B(z, A) }`: each field names parameters and fields before it, so reading one opens the parameters and the projections of the fields before it.
fn dependent() -> (StructDecl, Global) {
    let name = Global::Authored(Qualifier::from(["Dep"]));
    let a = Free::local(900, Some("A"));
    let b = Free::local(901, Some("B"));
    let x = Free::local(902, Some("x"));
    let y = Free::local(903, Some("y"));
    let z = Free::local(904, Some("z"));
    let w = Free::local(905, Some("w"));
    let fields = Telescope::build(
        [
            (x, Term::free_var(&a)),
            (y, Term::free_var(&b)),
            (
                z,
                Term::apply(Term::free_var(&a), [Term::free_var(&x), Term::free_var(&y)]),
            ),
            (
                w,
                Term::apply(Term::free_var(&b), [Term::free_var(&z), Term::free_var(&a)]),
            ),
        ],
        (),
    );
    let declaration = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build([(a, Term::type_ground()), (b, Term::type_ground())], fields),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    (declaration, name)
}

/// One field's type read off the declaration alone is the type the whole field telescope opened at the parameters gives it, at every slot, and a slot past the fields reads none.
#[test]
fn a_field_read_alone_is_the_field_read_whole() {
    let (declaration, name) = dependent();
    let params = [
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::BoolType),
    ];
    let value = Term::free_var(&Free::local(950, Some("v")));
    let universes = Universes::default();
    let whole = declaration.fields_at(&params);

    for slot in 0..5 {
        let field = Field::Struct(name, slot);
        assert_eq!(
            declaration
                .at(&universes)
                .field_type(&params, &value, &field)
                .expect("no universe to instantiate"),
            whole.field_type_from(&value, &field),
            "slot {slot}"
        );
    }
}

/// The parameters read alone are the arity's entries, ending in nothing in place of the fields.
#[test]
fn the_parameters_read_alone_are_the_aritys_entries() {
    let (declaration, _) = dependent();
    let parameters = declaration
        .at(&Universes::default())
        .parameters()
        .expect("no universe to instantiate");

    assert_eq!(parameters.len(), 2);
    assert_eq!(parameters.labels(), declaration.arity.labels());
    assert_eq!(parameters.terminal(), &());
}
