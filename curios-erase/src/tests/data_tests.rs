//! Data and its eliminations: variants and tuples by their registered schemas, payloads through newtypes, and each kind of match by what it erases to.

use {
    crate::{
        test_support::{
            bound, definition, erase, erase_alone, fresh, global, module, nat_lit, newtype_struct,
            nominal, struct_type_of, zonked,
        },
        *,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

/// The recorded payload row — field hint and carrier shape — of the one constructor of the single-constructor family whose debug name ends in `name`. The arena is what erasure wrote; the header the printer renders from it is a second spelling, and a test that scraped that spelling would pass by matching nothing if it moved.
fn payload(module: &curios_anf::Module, name: &str) -> Vec<(&'static str, curios_anf::FieldShape)> {
    let family = module
        .families()
        .iter()
        .find(|family| {
            family
                .debug_name
                .as_deref()
                .is_some_and(|debug_name| debug_name.ends_with(name))
        })
        .unwrap_or_else(|| panic!("the fixture registers {name}"));
    let [constructor] = family.constructors[..] else {
        panic!("{name} has exactly one constructor");
    };

    module
        .constructor(constructor)
        .expect("the constructor is registered")
        .fields
        .iter()
        .map(|field| {
            let hint = match field.debug_name.as_deref() {
                Some("x") => "x",
                other => panic!("unexpected field hint {other:?}"),
            };
            (hint, field.shape)
        })
        .collect()
}

fn opt_type() -> curios_core::Term {
    curios_core::Term::induct_type(
        nominal("Opt"),
        Vec::<curios_core::Term>::new(),
        Vec::<curios_core::Term>::new(),
    )
}

// induct Opt : Type | none() | some(x : Nat) end — `none` is tag 0, `some` tag 1 (registry-sorted). Registered on the module so erasure seeds it.
fn opt_induct() -> curios_core::InductDecl {
    let x = fresh(Some("x"));
    curios_core::InductDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::done(())),
        constructors: Vec::from([
            (
                curios_core::Atom::from("none"),
                curios_core::InductParam::new(curios_core::Telescope::done(Vec::new()), vec![]),
            ),
            (
                curios_core::Atom::from("some"),
                curios_core::InductParam::new(
                    curios_core::Telescope::build(
                        [(
                            x,
                            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                        )],
                        Vec::new(),
                    ),
                    vec![Plicity::Explicit],
                ),
            ),
        ])
        .into(),
        result_sort: curios_core::Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

#[test]
fn a_variant_constructs_with_its_registered_schema() {
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let body = curios_core::Term::variant(
        nominal("Opt"),
        Vec::<curios_core::Term>::new(),
        curios_core::Atom::from("some"),
        [nat_lit(6)],
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: curios_core::Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, opt_type());
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Rec
    value ~v0$/Opt
      Return Unit
entry
  Let ~v1 = Construct ~t1 [Nat(6)]
  Return ~v1
"
    );
    // The family the construction registered against, asked of the arena the header renders from: a nullary `none` and a `some` carrying one `Nat`, with `~t1` — the constructed one — the second.
    let [family] = erased.families() else {
        panic!("the fixture registers one family");
    };
    let [none, some] = family.constructors[..] else {
        panic!("Opt has two constructors");
    };
    assert!(
        erased
            .constructor(none)
            .expect("none is registered")
            .fields
            .is_empty()
    );
    assert_eq!(
        erased
            .constructor(some)
            .expect("some is registered")
            .fields
            .iter()
            .map(|field| field.shape)
            .collect::<Vec<_>>(),
        vec![curios_anf::FieldShape::Number]
    );
}

/// A row is its declaration's. The item declaring `Opt` registers the family, each field's shape with it; an item that constructs an `Opt` imports what it reads of the family, its count and the one constructor it builds, at its position and width and saying nothing of a field; and a program holds one family, the declaration's.
#[test]
fn a_row_is_registered_by_the_item_that_declares_it() {
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let former = curios_core::Item::Let(Box::new(curios_core::Definition {
        name: nominal("Opt"),
        kind: curios_core::DefinitionKind::Authored,
        universe_context: curios_core::UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: curios_core::Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        value_sort: None,
        type_: curios_core::Term::type_ground(),
        body: opt_type(),
    }));
    let six = definition(
        "six",
        opt_type(),
        curios_core::Term::variant(
            nominal("Opt"),
            Vec::<curios_core::Term>::new(),
            curios_core::Atom::from("some"),
            [nat_lit(6)],
        ),
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: vec![former, six].into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: curios_core::Entrypoint {
            body: curios_core::Term::free_var(&global("six")),
            type_: None,
        },
    };
    // The shapes of `some`'s payload in the one family `module` holds, the constructor at its second position.
    let payload = |module: &curios_anf::Module| {
        let [family] = module.families() else {
            panic!("the module holds one family");
        };
        let some = family
            .constructors
            .iter()
            .copied()
            .find(|&constructor| module.constructor_position(constructor) == Some(1))
            .expect("the module holds some");

        module
            .constructor(some)
            .expect("some is registered")
            .fields
            .iter()
            .map(|field| field.shape)
            .collect::<Vec<_>>()
    };

    let unit = erase_alone(&zonked(&fixture.module)).expect("the unit erases");
    let fragment = |name: &str| {
        unit.find(&nominal(name), Sorts::NONE)
            .expect("the unit declares it")
            .module()
    };
    assert_eq!(
        payload(fragment("Opt")),
        vec![curios_anf::FieldShape::Number]
    );
    assert_eq!(
        payload(fragment("six")),
        vec![curios_anf::FieldShape::Opaque]
    );
    let imported = fragment("six");
    assert_eq!(imported.families()[0].constructors.len(), 1);
    assert_eq!(
        imported.imports().families().values().collect::<Vec<_>>(),
        [&2]
    );

    assert_eq!(
        payload(&erase(&fixture, opt_type())),
        vec![curios_anf::FieldShape::Number]
    );
}

#[test]
fn a_multi_field_tuple_shares_the_width_schema() {
    let a = fresh(Some("a"));
    let b = fresh(Some("b"));
    let pair = fresh(Some("pair"));
    let tuple_type = curios_core::Term::tuple_type([
        (
            a,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
        (
            b,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
    ]);
    let body = curios_core::Term::let_(
        &pair,
        tuple_type.clone(),
        curios_core::Term::tuple([nat_lit(1), nat_lit(2)]),
        curios_core::Term::proj(curios_core::Term::free_var(&pair), 1),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0$pair = Product ~p0 [Nat(1), Nat(2)]
  Let ~v1 = Project ~p0.1 ~v0$pair
  Return ~v1
"
    );
    // The width row itself — that it is the interned shared one, which is the test's subject.
    let [schema] = erased.products() else {
        panic!("the fixture registers one schema");
    };
    assert!(schema.is_shared());
    assert_eq!(schema.width(), 2);
}

#[test]
fn an_anonymous_tuple_keeps_a_field_for_its_proof() {
    let x = fresh(Some("x"));
    let w = fresh(Some("w"));
    let sub = fresh(Some("sub"));
    let p = fresh(Some("P"));
    // { x : Nat, w : Prop-valued } is two fields, the proof's holding the unit constant: an anonymous tuple has no declaration to say which it keeps, and a reader that knows `w`'s type as a variable reads two.
    let subset_type = curios_core::Term::tuple_type([
        (
            x,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
        // Quantifying over `Prop` lands a level up.
        (
            w,
            curios_core::Term::prop_at(curios_core::Level::constant(1)),
        ),
    ]);
    let body = curios_core::Term::let_(
        &sub,
        subset_type.clone(),
        curios_core::Term::tuple([
            nat_lit(9),
            curios_core::Term::func_type(
                [(p, curios_core::Term::prop())],
                curios_core::Term::free_var(&p),
            ),
        ]),
        curios_core::Term::proj(curios_core::Term::free_var(&sub), 0),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0$sub = Product ~p0 [Nat(9), Unit]
  Let ~v1 = Project ~p0.0 ~v0$sub
  Return ~v1
"
    );
}

#[test]
fn a_bool_match_erases_to_a_switch_bool() {
    let body = curios_core::Term::bool_match(
        curios_core::Term::intrinsic(curios_core::Intrinsic::Bool(true)),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(10),
        nat_lit(20),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0 = SwitchBool Bool(true)
    false
      Return Nat(10)
    true
      Return Nat(20)
  Return ~v0
"
    );
}

#[test]
fn a_dead_hypothesis_nat_match_peels_to_a_dispatch() {
    let pred = fresh(Some("pred"));
    let ih = fresh(Some("ih"));
    // match n | 0 => 0 | succ pred (ih dead) => pred — a case split, not a fold.
    let items = vec![definition(
        "n",
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = curios_core::Term::nat_match(
        curios_core::Term::free_var(&global("n")),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        curios_core::Term::free_var(&pred),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v1 = SwitchNat Nat(5)
    case 0
      Return Nat(0)
    default
      Let ~v0$pred = Operation NatSub [Nat(5), Nat(1)]
      Return ~v0$pred
  Return ~v1
"
    );
}

#[test]
fn a_live_hypothesis_nat_match_erases_to_a_fold() {
    let pred = fresh(Some("pred"));
    let ih = fresh(Some("ih"));
    // match n | 0 => 0 | succ pred ih => ih + 2 — genuine induction.
    let items = vec![definition(
        "n",
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = curios_core::Term::nat_match(
        curios_core::Term::free_var(&global("n")),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
            curios_core::Term::free_var(&ih),
            nat_lit(2),
        )),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v3 = FoldNat Nat(5)
    zero
      Return Nat(0)
    step(~v0$pred, ~v1$ih)
      Let ~v2 = Operation NatAdd [~v1$ih, Nat(2)]
      Return ~v2
  Return ~v3
"
    );
}

#[test]
fn a_live_hypothesis_list_match_erases_to_a_sequence_fold() {
    let h = fresh(Some("h"));
    let t = fresh(Some("t"));
    let ih = fresh(Some("ih"));
    // match xs | [] => 0 | h :: t (ih) => ih + 1 — a length fold over a list.
    let list_ty = curios_core::Term::intrinsic(curios_core::Intrinsic::ListType(
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    ));
    let items = vec![definition(
        "xs",
        list_ty.clone(),
        curios_core::Term::intrinsic(curios_core::Intrinsic::List {
            element: curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            items: vec![nat_lit(1)],
        }),
    )];
    let body = curios_core::Term::list_match(
        curios_core::Term::free_var(&global("xs")),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &[&h, &t, &ih],
        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
            curios_core::Term::free_var(&ih),
            nat_lit(1),
        )),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Let ~v0$/xs = Sequence ListBuild [Nat(1)]
entry
  Let ~v5 = FoldSequence List ~v0$/xs
    empty
      Return Nat(0)
    step(~v1$h, ~v2$t, ~v3$ih)
      Let ~v4 = Operation NatAdd [~v3$ih, Nat(1)]
      Return ~v4
  Return ~v5
"
    );
}

#[test]
fn a_variant_match_binds_payload_without_projections() {
    let x = fresh(Some("x"));
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let scrutinee = curios_core::Term::variant(
        nominal("Opt"),
        Vec::<curios_core::Term>::new(),
        curios_core::Atom::from("some"),
        [nat_lit(6)],
    );
    let body = curios_core::Term::induct_match(
        scrutinee,
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        [
            ("none", Vec::<curios_core::Free>::new(), nat_lit(0)),
            ("some", vec![x], curios_core::Term::free_var(&x)),
        ],
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: curios_core::Entrypoint { body, type_: None },
    };
    let erased = erase(
        &fixture,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Rec
    value ~v0$/Opt
      Return Unit
entry
  Let ~v1$scrutinee = Construct ~t1 [Nat(6)]
  Let ~v3 = MatchVariant ~d0 ~v1$scrutinee
    arm ~t0()
      Return Nat(0)
    arm ~t1(~v2$x)
      Return ~v2$x
  Return ~v3
"
    );
}

#[test]
fn an_effectful_scrutinee_is_erased_once() {
    let x = fresh(Some("x"));
    let pred = fresh(Some("pred"));
    let ih = fresh(Some("ih"));
    // The peel path re-derives Core terms from the head (`n - 1`); an effectful compound head must still evaluate exactly once, through the alias.
    let io_read =
        curios_core::Term::apply(curios_core::Term::free_var(&global("read")), [nat_lit(0)]);
    let items = vec![definition(
        "read",
        curios_core::Term::func_type(
            [(
                x,
                curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            )],
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
        curios_core::Term::func(
            [(
                x,
                curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            )],
            curios_core::Term::free_var(&x),
        ),
    )];
    let body = curios_core::Term::nat_match(
        io_read,
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        curios_core::Term::free_var(&pred),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    let read = erased.function_ids().next().expect("the read function");
    let applications = bound(&erased)
        .filter(|rhs| {
            matches!(
                rhs,
                curios_anf::Rhs::Apply {
                    callee: curios_anf::Atom::Function(callee),
                    ..
                } if *callee == read
            )
        })
        .count();
    assert_eq!(
        applications, 1,
        "the compound scrutinee is applied exactly once"
    );
}

/// A no-parameter family with the single unary constructor `mk(x: payload)`.
fn unary_induct(payload: curios_core::Term) -> curios_core::InductDecl {
    let x = fresh(Some("x"));
    curios_core::InductDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::done(())),
        constructors: Vec::from([(
            curios_core::Atom::from("mk"),
            curios_core::InductParam::new(
                curios_core::Telescope::build([(x, payload)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )])
        .into(),
        result_sort: curios_core::Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// The payload-shape classifier chases newtype chains and terminates on cycles: a payload wrapped in two single-field structs lands on `Nat` and records `Immediate`; a payload whose struct names itself is `Opaque` because the visited guard cuts the cycle (a self-referential struct elaborates — it is merely uninhabited); a boxed `Flt` payload records its carrier, `Flt`.
#[test]
fn payload_shapes_chase_newtype_chains_and_terminate_on_cycles() {
    let mut struct_decls = BTreeMap::new();
    struct_decls.insert(
        nominal("Meters"),
        newtype_struct(
            "m",
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
    );
    struct_decls.insert(
        nominal("Outer"),
        newtype_struct("o", struct_type_of("Meters")),
    );
    struct_decls.insert(nominal("Loop"), newtype_struct("s", struct_type_of("Loop")));
    let induct_type = |family: &str| {
        curios_core::Term::induct_type(
            nominal(family),
            Vec::<curios_core::Term>::new(),
            Vec::<curios_core::Term>::new(),
        )
    };
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Wrapped"), unary_induct(struct_type_of("Outer")));
    induct_decls.insert(nominal("Knotted"), unary_induct(struct_type_of("Loop")));
    induct_decls.insert(
        nominal("Boxed"),
        unary_induct(curios_core::Term::intrinsic(
            curios_core::Intrinsic::FltType,
        )),
    );
    // A payload whose type is itself a single-constructor family chases through it — the collapsed encoding makes such a family *be* its payload — while a recursive single-constructor family terminates at the visited guard.
    induct_decls.insert(
        nominal("Inner"),
        unary_induct(curios_core::Term::intrinsic(
            curios_core::Intrinsic::NatType,
        )),
    );
    induct_decls.insert(nominal("Chained"), unary_induct(induct_type("Inner")));
    induct_decls.insert(nominal("Selfy"), unary_induct(induct_type("Selfy")));

    // The families register lazily on first construction, and the classifier reads the declared field types: each family is built by a function of its payload, so a family no value inhabits is built as readily as the others.
    let building = |family: &str, payload: curios_core::Term| {
        let value = fresh(Some("v"));
        (
            curios_core::Term::func(
                [(value, payload.clone())],
                curios_core::Term::variant(
                    nominal(family),
                    Vec::<curios_core::Term>::new(),
                    curios_core::Atom::from("mk"),
                    [curios_core::Term::free_var(&value)],
                ),
            ),
            curios_core::Term::func_type([(value, payload)], induct_type(family)),
        )
    };
    let (built, types): (Vec<_>, Vec<_>) = [
        ("Wrapped", struct_type_of("Outer")),
        ("Knotted", struct_type_of("Loop")),
        (
            "Boxed",
            curios_core::Term::intrinsic(curios_core::Intrinsic::FltType),
        ),
        ("Chained", induct_type("Inner")),
        ("Selfy", induct_type("Selfy")),
    ]
    .into_iter()
    .map(|(family, payload)| building(family, payload))
    .unzip();
    let body = curios_core::Term::tuple(built);
    let expected = curios_core::Term::tuple_type(
        ["a", "b", "c", "d", "e"]
            .into_iter()
            .map(|label| fresh(Some(label)))
            .zip(types),
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            struct_decls,
            [],
        ),
        entry: curios_core::Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, expected);

    assert_eq!(
        payload(&erased, "/Wrapped"),
        vec![("x", curios_anf::FieldShape::Number)]
    );
    assert_eq!(
        payload(&erased, "/Knotted"),
        vec![("x", curios_anf::FieldShape::Opaque)]
    );
    // `Boxed` holds a `Flt`, and the recorder names that carrier rather than merely withholding `immediate`.
    assert_eq!(
        payload(&erased, "/Boxed"),
        vec![("x", curios_anf::FieldShape::Flt)]
    );
    assert_eq!(
        payload(&erased, "/Chained"),
        vec![("x", curios_anf::FieldShape::Number)]
    );
    assert_eq!(
        payload(&erased, "/Selfy"),
        vec![("x", curios_anf::FieldShape::Opaque)]
    );
}
