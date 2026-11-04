//! Core text proper: each node in its form, under the decorator stating every field its form does not build.

use {
    crate::{Spellable, Spelling},
    curios_common::Qualifier,
    curios_core::{
        Atom, CalleeId, Cases, Definition, Definitions, Field, Free, Global, HeadKey, InductDecl,
        InductParam, Intrinsic, Item, Level, Match, MatchResult, MetavarId, MetavarOrigin, Nat,
        Nominal, Proj, Quality, QualityParam, RecItem, Sort, StructDecl, Subterm, Telescope, Term,
        UniverseContext, UniverseParam, Universes, Var, WitnessId, WitnessKey, WitnessOrigin,
    },
    curios_num::Floating,
    std::rc::Rc,
};

/// `term` as Core text proper writes it.
fn written(term: &Term) -> String {
    term.spelled(&Rc::new(Spelling::core_text())).to_string()
}

fn global(path: &[&str]) -> Global {
    Global::Authored(Qualifier::from(path.to_vec()))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// A sort's word builds it at level 0 and at the quality the word names, and a decorator states any other.
#[test]
fn a_sort_states_the_level_and_the_quality_its_word_does_not() {
    let one = Level::zero().succ().expect("level zero has a successor");

    assert_eq!(written(&Term::type_ground()), "Type");
    assert_eq!(written(&Term::prop()), "Prop");
    assert_eq!(written(&Term::type_at(one)), "#{level = 1} Type");
    assert_eq!(
        written(&Term::from(Subterm::Sort(Sort {
            quality: Quality::Param(QualityParam(1)),
            level: Level::zero(),
        }))),
        "#{quality = Param(1)} Sort"
    );
}

/// A path builds an instance at no universe, and a decorator states the one a name stands at; a call's head that takes one is parenthesized, so it states the head's and not the call's.
#[test]
fn a_name_states_its_instance_and_a_call_parenthesizes_a_decorated_head() {
    let at = Universes {
        levels: vec![Level::param(UniverseParam(0))],
        qualities: vec![Quality::Type],
    };
    let instance = Term::instance(global(&["f"]), at);

    assert_eq!(
        written(&instance),
        "#{universes = {levels = [u], qualities = [Type]}} /f"
    );
    assert_eq!(
        written(&Term::apply(instance, [nat_type()])),
        "(#{universes = {levels = [u], qualities = [Type]}} /f)(%NatType)"
    );
}

/// An intrinsic is `%` and its variant's Core name over its operands, and a literal form builds its own node: a numeral its `Nat`, `true` its `Bool`, `x[…]` its `Bytes`.
#[test]
fn an_intrinsic_is_its_variant_and_a_literal_its_own_form() {
    let n = Term::free_var(&Free::local(0, Some("n")));
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    assert_eq!(
        written(&Term::intrinsic(Intrinsic::nat_add(n, three.clone()))),
        "%NatAdd(n, 3)"
    );
    assert_eq!(written(&three), "3");
    assert_eq!(written(&Term::intrinsic(Intrinsic::Bool(true))), "true");
    assert_eq!(
        written(&Term::intrinsic(Intrinsic::Flt(Floating::infinite(true)))),
        "%Flt(-inf.0)"
    );
}

/// A family applied as a node of its own is its former's call under the node it is, as a constructor's value is its constructor's call, the family's parameters under `@` ahead of its payload.
#[test]
fn a_family_and_a_constructors_value_state_the_node_they_are() {
    let eq = global(&["Eq"]);
    let one = || Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));

    assert_eq!(
        written(&Term::induct_type(eq, [nat_type()], [one(), one()])),
        "#{node = InductType} /Eq(%NatType)(1, 1)"
    );
    assert_eq!(
        written(&Term::variant(
            global(&["Result"]),
            [nat_type()],
            "success",
            [one()]
        )),
        "#{node = Variant} /Result/success(@%NatType, 1)"
    );
}

/// A bound index no scope in the text binds states its binding, and a metavariable its id and its origin.
#[test]
fn a_loose_index_and_a_metavariable_state_what_their_forms_do_not() {
    assert_eq!(
        written(&Term::var(Var::bound(2))),
        "#{binding = Bound(2)} _"
    );
    assert_eq!(written(&Term::hole(7)), "#{id = 7, origin = Hole} ?");
}

/// A projection out of a declared record states the record and the field's place.
#[test]
fn a_projection_out_of_a_record_states_the_record() {
    let projection = Term::from(Subterm::Proj(Proj {
        head: Term::free_var(&Free::local(0, Some("p"))),
        field: Field::Struct(global(&["Pair"]), 0),
    }));

    assert_eq!(written(&projection), "#{field = Struct(/Pair, 0)} (p).0");
}

/// A match is written in the surface's spelling of its cases, and one with no motive states the goal its arms are checked against.
#[test]
fn a_match_states_the_goal_it_is_checked_against() {
    let b = Free::local(0, Some("b"));
    let number = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let matched = Term::from(Subterm::Match(Match {
        head: Term::free_var(&b),
        result: MatchResult::Ambient(nat_type()),
        cases: Cases::Bool {
            false_case: number(0),
            true_case: number(1),
        },
    }));

    assert_eq!(
        written(&matched),
        "#{result = Ambient(%NatType)} match b\n| false =>\n    0\n| true =>\n    1\nend"
    );
}

/// A type former's η-form is written as the lambda it is, which a reader is shown as the former.
#[test]
fn a_type_formers_eta_form_is_written_as_its_lambda() {
    let x = Free::local(0, Some("x"));
    let lambda = Term::func(
        [(x, nat_type())],
        Term::induct_type(global(&["Sign"]), Vec::<Term>::new(), [Term::free_var(&x)]),
    );

    assert_eq!(written(&lambda), "(x) => #{node = InductType} /Sign()(x)");
    assert_eq!(lambda.faithful().to_string(), "/Sign");
}

/// A witness is named by what it witnesses, its concept and key after `~`.
#[test]
fn a_witness_is_named_by_its_concept_and_key() {
    let witness = Global::Witness(WitnessId::Spelled {
        concept: Qualifier::from(["std", "Show"]),
        key: WitnessKey::from([HeadKey::Nat]),
        module: Qualifier::from(["std", "Nat"]),
        place: 0,
    });

    assert_eq!(
        written(&Term::instance(witness, Universes::default())),
        "/std/Nat/witness~/std/Show(Nat)"
    );
}

/// A method named by its path is written as that path under the node it is, and a goal a call of it opened states the method as its concept and field.
#[test]
fn a_method_named_by_its_path_is_its_path_under_its_node() {
    let show = global(&["std", "Show", "Show"]);
    let goal = Term::metavar_birthed(
        MetavarId(3),
        MetavarOrigin::Witness(WitnessOrigin {
            func: CalleeId::Method {
                concept: show,
                field: "show".into(),
            },
            role: None,
        }),
        Vec::new(),
    );

    assert_eq!(
        written(&Term::method(show, "show")),
        "#{node = Method} /std/Show/Show/show"
    );
    assert_eq!(
        written(&goal),
        "#{id = 3, origin = Witness {func = Method {concept = /std/Show/Show, field = \"show\"}, role = None}} ?"
    );
}

/// `induct /Bit: Type | zero() | one() end`'s entry, whose name stands in the root.
fn bit() -> InductDecl {
    let case = |tag: &str| {
        (
            Atom::from(tag),
            InductParam::new(
                Telescope::build(Vec::<(Free, Term)>::new(), Vec::new()),
                Vec::new(),
            ),
        )
    };

    InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            Vec::<(Free, Term)>::new(),
            Telescope::build(Vec::<(Free, Term)>::new(), ()),
        ),
        constructors: [case("zero"), case("one")].into_iter().collect(),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: false,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// A module of `/Bit`'s former, a group of its own as a family's is, beside `constructors`, in the order a module is stored in.
fn module(declaration: InductDecl, constructors: Vec<Definition>) -> Definitions {
    let former = Definition::former(
        global(&["Bit"]),
        Qualifier::empty(),
        Nominal::Inductive(declaration),
    );
    let items = [Item::Rec(RecItem::new(vec![(
        Free::local(0, Some("Bit")),
        former,
    )]))]
    .into_iter()
    .chain(
        constructors
            .into_iter()
            .map(|constructor| Item::Let(Box::new(constructor))),
    )
    .collect::<Vec<_>>();

    Definitions {
        items: items.into(),
    }
    .ordered()
}

/// A family is written in its form, which builds its former and its constructors, so no item of either is written.
#[test]
fn a_family_is_written_in_its_form_and_its_constructors_are_left_to_it() {
    let constructors =
        Definition::constructors(Qualifier::from(["Bit"]), Qualifier::empty(), &bit());

    assert_eq!(
        module(bit(), constructors).faithful().to_string(),
        "induct /Bit: Type\n| zero()\n| one()\nend\n"
    );
}

/// A constructor its family's form would not make is written out, under the kind that says whose it is.
#[test]
fn a_constructor_its_family_does_not_make_is_written_out() {
    let mut constructors =
        Definition::constructors(Qualifier::from(["Bit"]), Qualifier::empty(), &bit());
    constructors[1].body = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));

    let written = module(bit(), constructors).faithful().to_string();
    assert!(
        written.contains("#{kind = InductiveConstructor {owner = /Bit, tag = one}} let /Bit/one"),
        "{written}"
    );
    assert!(!written.contains("let /Bit/zero"), "{written}");
}

/// A constructor its family's form implies and the module lacks is stated absent.
#[test]
fn a_constructor_the_module_lacks_is_stated_absent() {
    let mut constructors =
        Definition::constructors(Qualifier::from(["Bit"]), Qualifier::empty(), &bit());
    constructors.truncate(1);

    assert_eq!(
        module(bit(), constructors).faithful().to_string(),
        "#{absent = [/Bit/one]} induct /Bit: Type\n| zero()\n| one()\nend\n"
    );
}

/// A record is written in its form, each field by its label under the parameters it reads, a lone record's item a `let`.
#[test]
fn a_record_is_written_in_its_form() {
    let (a, b) = (Free::local(0, Some("A")), Free::local(1, Some("B")));
    let (fst, snd) = (Free::local(2, Some("fst")), Free::local(3, Some("snd")));
    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(a, Term::type_ground()), (b, Term::type_ground())],
            Telescope::build([(fst, Term::free_var(&a)), (snd, Term::free_var(&b))], ()),
        ),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let former = Definition::former(
        global(&["Pair"]),
        Qualifier::empty(),
        Nominal::Struct(record),
    );
    let definitions = Definitions {
        items: vec![Item::Let(Box::new(former))].into(),
    };

    assert_eq!(
        definitions.faithful().to_string(),
        "struct /Pair(A: Type, B: Type): pub Type { fst: A, snd: B }\n"
    );
}
