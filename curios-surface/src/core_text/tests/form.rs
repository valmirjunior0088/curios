//! Core text proper: each node in its form, under the decorator stating every field its form does not build.

use {
    super::test_support::*,
    crate::{CoreText, Refusal, Spellable, Spelling, parse_whole_term},
    curios_common::{Qualifier, Source},
    curios_core::{
        Arm, CalleeId, Cases, Definitions, Field, Free, Func, FuncType, Global, HeadKey, Intrinsic,
        Item, Let, Level, Match, Matrix, MetavarId, MetavarOrigin, Nat, Nominal, Nominals, Pattern,
        Proj, Quality, QualityParam, Row, Sort, StructDecl, Subterm, Telescope, Term,
        UniverseContext, UniverseParam, Universes, Var, WitnessId, WitnessKey, WitnessOrigin,
    },
    curios_num::{Floating, Integer, Rounding},
    std::rc::Rc,
};

/// `term` written whole, checked against `expected`, then read back: the term it reads back as, and that term written whole again, which is the same text where everything the text states was read.
fn whole_round_trip(term: &Term, expected: &str) -> Term {
    let whole = Rc::new(Spelling::core_text().written_whole());
    let text = term.spelled(&whole).to_string();
    assert_eq!(text, expected);
    let parsed = parse_whole_term::<CoreText>(&Source::inline(&text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let read = term_to_core(&parsed).unwrap_or_else(|error| panic!("{text}\n{error}"));
    assert_eq!(
        read.spelled(&whole).to_string(),
        text,
        "the label a text states is read"
    );

    read
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
        "(#{universes = {levels = [u], qualities = [Type]}} /f)(#NatType)"
    );
}

/// An intrinsic is `#` and its variant's Core name over its operands, and a literal form builds its own node: a numeral its `Nat`, `true` its `Bool`, `0x[…]` its `Bytes`.
#[test]
fn an_intrinsic_is_its_variant_and_a_literal_its_own_form() {
    let n = Term::free_var(&Free::local(0, Some("n")));
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    assert_eq!(
        written(&Term::intrinsic(Intrinsic::nat_add(n, three.clone()))),
        "#NatAdd(n, 3)"
    );
    assert_eq!(written(&three), "3");
    assert_eq!(written(&Term::intrinsic(Intrinsic::Bool(true))), "true");
    // A signed numeral is the `Int` it spells and a decimal or a non-finite literal the `Flt`, each read back as the value it writes.
    for (value, text) in [
        (Intrinsic::Int(Integer::from(-3i32)), "-3"),
        (Intrinsic::Int(Integer::from(0i32)), "+0"),
        (Intrinsic::Flt(Floating::infinite(true)), "-inf.0"),
        (
            Intrinsic::Flt(Floating::of_natural(&3u32.into(), Rounding::TiesToEven)),
            "+3.0",
        ),
    ] {
        let term = Term::intrinsic(value);
        assert_eq!(written(&term), text);
        assert_eq!(read_back(&term), term, "{text}");
    }
}

/// A family applied as a node of its own is its former's call under the node it is, as a constructor's value is its constructor's call, the family's parameters under `@` ahead of its payload.
#[test]
fn a_family_and_a_constructors_value_state_the_node_they_are() {
    let eq = global(&["Eq"]);
    let one = || Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));

    assert_eq!(
        written(&Term::induct_type(eq, [nat_type()], [one(), one()])),
        "#{node = InductType} /Eq(#NatType)(1, 1)"
    );
    assert_eq!(
        written(&Term::variant(
            global(&["Result"]),
            [nat_type()],
            "success",
            [one()]
        )),
        "#{node = Variant} /Result/success(@#NatType, 1)"
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

    assert_eq!(written(&projection), "#{field = Struct(/Pair, 0)} p.0");
}

/// A match is written in the surface's spelling of its cases, and states the goal its arms are checked against.
#[test]
fn a_match_states_the_goal_it_is_checked_against() {
    let b = Free::local(0, Some("b"));
    let number = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let matched = Term::from(Subterm::Match(Match {
        head: Term::free_var(&b),
        goal: nat_type(),
        cases: Cases::Bool {
            false_case: number(0),
            true_case: number(1),
        },
    }));

    assert_eq!(
        written(&matched),
        "#{goal = #NatType} match b | false => 0 | true => 1 end"
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

    assert_eq!(
        written(&lambda),
        "(x: #NatType) => #{node = InductType} /Sign()(x)"
    );
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
        "#{id = 3, origin = Witness { func = Method { concept = /std/Show/Show, field = \"show\" }, role = None }} ?"
    );
}

/// A family is written in its form, whose entry its former and its constructors are made of, so neither is written.
#[test]
fn a_family_is_written_in_its_form() {
    assert_eq!(
        module(bit(), Vec::new()).faithful().to_string(),
        "induct /Bit: Type\n| zero()\n| one()\nend\n"
    );
}

/// A version's items stand at places spaced apart, which no count of them names, and are written as the module they hold: a family's form.
#[test]
fn a_version_is_written_as_the_module_it_holds() {
    let made = module(bit(), Vec::new());
    let version = Definitions {
        items: made.items.version(),
    };

    assert!(version.items.is_version());
    assert_eq!(version.faithful().to_string(), made.faithful().to_string());
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
    let definitions = Definitions {
        items: vec![Item::Nominal(Nominals::new(vec![(
            global(&["Pair"]),
            Nominal::Struct(record),
        )]))]
        .into(),
    };

    assert_eq!(
        definitions.faithful().to_string(),
        "struct /Pair(A: Type, B: Type): pub Type {\n    fst: A,\n    snd: B,\n}\n"
    );
}

/// What Core keeps of a binder beside its hint is written whole before the binder's word, each field by the name the label gives it, and read back onto the label: where the binder sits among its declaration's written binders, and that it binds a proof. A lambda's `h` and a `let`'s `k` state both; a report names each by its word alone.
#[test]
fn a_binders_label_is_written_whole_before_its_word_and_read_back() {
    let (p, h, k) = (
        Free::local_written(0, Some("P"), 0),
        Free::local_written(1, Some("h"), 1),
        Free::local_written(2, Some("k"), 2),
    );
    let Subterm::Let(block) = Term::unwrap_or_clone(Term::let_(
        &k,
        Term::free_var(&p),
        Term::free_var(&h),
        Term::free_var(&k),
    )) else {
        unreachable!("a `let` was built");
    };
    let (bindings, tail) = block.into_parts();
    let block = Term::from(Subterm::Let(Let::new(bindings, tail.with_proofs(&[true]))));
    let Subterm::Func(Func { telescope }) = Term::unwrap_or_clone(Term::func(
        [(p, Term::prop()), (h, Term::free_var(&p))],
        block,
    )) else {
        unreachable!("a function was built");
    };
    let term = Term::from(Subterm::Func(Func {
        telescope: telescope.with_proofs(&[false, true]),
    }));

    let read = whole_round_trip(
        &term,
        "(#{written = 0} P: Prop, #{written = 1, proof = true} h: P) =>\n    let #{written = 2, proof = true} k: P = h;\n    k",
    );
    assert_eq!(read, term);
    assert_eq!(written(&term), "(P: Prop, h: P) => let k: P = h;\n    k");
}

/// A binder written as its type alone that states something of its label is written whole as `_`, which reads back as no hint: a function type's unnamed parameter that binds a proof.
#[test]
fn an_unnamed_binder_that_states_its_label_is_written_as_a_wildcard() {
    let (x, unnamed) = (Free::local(0, Some("x")), Free::local(1, None));
    let Subterm::FuncType(FuncType { telescope }) = Term::unwrap_or_clone(Term::func_type(
        [(x, nat_type()), (unnamed, Term::prop())],
        nat_type(),
    )) else {
        unreachable!("a function type was built");
    };
    let term = Term::from(Subterm::FuncType(FuncType {
        telescope: telescope.with_proofs(&[false, true]),
    }));

    let read = whole_round_trip(&term, "(x: #NatType, #{proof = true} _: Prop) -> #NatType");
    let Subterm::FuncType(FuncType { telescope }) = Term::unwrap_or_clone(read) else {
        panic!("a function type reads back as one");
    };
    assert_eq!(telescope.proofs(), [false, true]);
    assert_eq!(telescope.labels(), ["x", ""]);
}

/// A row writes each place by the word its function gives the binder, and a bare `_` is a wildcard, which binds nothing: a place whose word is not its binder's is refused.
#[test]
fn a_rows_places_are_its_functions_words_and_a_bare_underscore_is_a_wildcard() {
    let m = Free::local(1, Some("m"));
    let numeral = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let term = Term::matrix(Matrix {
        head: numeral(1),
        arms: vec![
            Arm {
                alternatives: vec![Row {
                    pattern: Pattern::Binder,
                    guards: Vec::new(),
                    body: Term::func([(m, nat_type())], Term::free_var(&m)),
                }],
            },
            Arm {
                alternatives: vec![Row {
                    pattern: Pattern::WILDCARD,
                    guards: Vec::new(),
                    body: Term::func(Vec::<(Free, Term)>::new(), numeral(0)),
                }],
            },
        ],
    });

    assert_eq!(
        written(&term),
        "#{node = Matrix} match 1 | m => (m: #NatType) => m | _ => () => 0 end"
    );
    assert_eq!(read_back(&term), term);
    assert!(matches!(
        refused("#{node = Matrix} match 1 | m => (n: #NatType) => n | _ => () => 0 end").0,
        Refusal::Malformed(what) if what.contains("places")
    ));
}

/// A lambda's parameter states its domain, which a program's leaves to the elaborator.
#[test]
fn a_lambda_states_each_domain() {
    assert!(matches!(refused("(x) => x").0, Refusal::Malformed(_)));
}

/// A decorator states only fields its node holds, each once, and a node only a form of its shape fills.
#[test]
fn a_decorator_states_what_its_node_holds_once() {
    assert_eq!(
        refused("#{level = 1} /f").0,
        Refusal::UnknownField("level".to_string())
    );
    assert_eq!(
        refused("#{universes = {}, universes = {}} /f").0,
        Refusal::RepeatedField("universes".to_string())
    );
    assert_eq!(
        refused("#{node = Banana} /f(x)").0,
        Refusal::UnknownNode("Banana".to_string())
    );
}

/// A name is absolute or a binder's word, and an intrinsic one of Core's variants.
#[test]
fn a_name_is_absolute_and_an_intrinsic_one_of_cores() {
    assert_eq!(refused("a/b").0, Refusal::RelativeName("/a/b".to_string()));
    assert_eq!(
        refused("#Banana").0,
        Refusal::UnknownIntrinsic("Banana".to_string())
    );
}

/// An integer's dispatch writes each key as the signed numeral it is, and reads back as the switch it was: its carrier read off its keys with no type at hand.
#[test]
fn an_integers_dispatch_writes_its_keys_signed() {
    let text = "#{goal = #NatType} match +7 | -1 => 10 | +0 => 20 | +2 => 30 | _ => 99 end";
    let parsed = parse_whole_term::<CoreText>(&Source::inline(text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let read = term_to_core(&parsed).unwrap_or_else(|error| panic!("{text}\n{error}"));

    assert_eq!(
        written(&read),
        "#{goal = #NatType} match +7\n| -1 => 10\n| +0 => 20\n| +2 => 30\n| _ => 99\nend"
    );
    assert_eq!(read_back(&read), read);
}

/// A byte's dispatch writes each key as the value it is, `#Byte(65)`, and reads back as the switch it was; a natural's key among a byte's is refused where it is written.
#[test]
fn a_bytes_dispatch_writes_its_keys_as_bytes() {
    let text =
        "#{goal = #NatType} match #Byte(7) | #Byte(65) => 10 | #Byte(66) => 20 | _ => 99 end";
    let parsed = parse_whole_term::<CoreText>(&Source::inline(text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let read = term_to_core(&parsed).unwrap_or_else(|error| panic!("{text}\n{error}"));

    assert_eq!(written(&read), text);
    assert_eq!(read_back(&read), read);
    assert!(matches!(
        refused("#{goal = #NatType} match #Byte(7) | #Byte(65) => 10 | 66 => 20 | _ => 99 end"),
        (Refusal::Malformed(_), Some(_))
    ));
}
