//! Core text proper: each node in its form, under the decorator stating every field its form does not build.

use {
    super::*,
    crate::{
        CoreFile, CoreText, ReadError, Refusal, Spellable, Spelling, parse_whole_term, to_core,
    },
    curios_common::{Qualifier, Source},
    curios_core::{
        Arm, Atom, CalleeId, Cases, Constructors, Definition, DefinitionKind, Definitions,
        Elements, Field, Free, Func, FuncType, Global, Guard, HeadKey, InductDecl, InductParam,
        Intrinsic, Item, Let, Level, Match, Matrix, MetavarId, MetavarOrigin, Nat, Nominal,
        Nominals, NumLit, Numeral, Pattern, Proj, Quality, QualityParam, RecItem, Row, Slot, Sort,
        StructDecl, Subterm, Telescope, Term, UniverseContext, UniverseParam, Universes, Var,
        WitnessId, WitnessKey, WitnessOrigin, WrittenAt,
    },
    curios_num::{Floating, Integer, Natural, Rounding},
    std::rc::Rc,
};

/// The pattern of the unsigned numeral `value`.
fn unsigned(value: usize) -> Pattern {
    Pattern::Literal(NumLit::Number(Numeral::Natural(value.into())))
}

/// The term `term` writes, read on its own.
fn term_to_core(term: &crate::Term<CoreText>) -> Result<Term, ReadError> {
    core_read(term, &mut Reader::new(None))
}

/// `term` as Core text proper writes it.
fn written(term: &Term) -> String {
    term.spelled(&Rc::new(Spelling::core_text())).to_string()
}

/// The term `term`'s Core text reads back as.
fn read_back(term: &Term) -> Term {
    let text = written(term);
    let parsed = parse_whole_term::<CoreText>(&Source::inline(&text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    term_to_core(&parsed).unwrap_or_else(|error| panic!("{text}\n{error}"))
}

/// The module `definitions`' Core text reads back as.
fn module_read_back(definitions: &Definitions) -> Definitions {
    let text = definitions.faithful().to_string();
    let file = text
        .parse::<CoreFile>()
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let (read, entry) = to_core(&file, None).unwrap_or_else(|error| panic!("{text}\n{error}"));
    assert!(entry.is_none(), "{text}");

    read
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
        "(#{universes = {levels = [u], qualities = [Type]}} /f)(#NatType)"
    );
}

/// An intrinsic is `#` and its variant's Core name over its operands, and a literal form builds its own node: a numeral its `Nat`, `true` its `Bool`, `x[…]` its `Bytes`.
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

/// `induct /Bit: Type | zero() | one() end`'s entry, whose name stands in the root.
fn bit() -> InductDecl {
    let case = |tag: &str| {
        (
            Atom::from(tag),
            InductParam::over(
                &[],
                Telescope::build(Vec::<(Free, Term)>::new(), Vec::new()),
            ),
        )
    };

    InductDecl::new(
        UniverseContext::empty(),
        Telescope::build(
            Vec::<(Free, Term)>::new(),
            Telescope::build(Vec::<(Free, Term)>::new(), ()),
        ),
        Constructors::try_from(vec![case("zero"), case("one")]).unwrap(),
        Term::type_ground(),
        Qualifier::empty(),
        false,
    )
}

/// A module declaring `/Bit`, beside the `let`s of `beside`, in the order a module is stored in.
fn module(declaration: InductDecl, beside: Vec<Definition>) -> Definitions {
    let items = [Item::Nominal(Nominals::new(vec![(
        global(&["Bit"]),
        Nominal::Inductive(declaration),
    )]))]
    .into_iter()
    .chain(
        beside
            .into_iter()
            .map(|definition| Item::Let(Box::new(definition))),
    )
    .collect::<Vec<_>>();

    Definitions {
        items: items.into(),
    }
    .ordered()
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

/// A constructor is its family's, made of the family's entry, and no kind a definition states: a definition stating itself one is refused where Core text reads it, by the kinds a definition does state.
#[test]
fn a_definition_stating_itself_a_constructor_is_refused_where_it_is_read() {
    let text = "induct /Bit: pub Type\n| one()\nend\n#{kind = InductiveConstructor { owner = /Bit, tag = two }} let /Bit/two: /Bit = 1;\n";
    let file = text.parse::<CoreFile>().unwrap();
    let error = to_core(&file, None).map(|_| ()).expect_err(text);

    assert!(
        matches!(&error.refusal, Refusal::Malformed(what) if what.contains("`Witness` or `Test`")),
        "{:?}",
        error.refusal
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

/// Every node a test above writes reads back as itself: closed under a function where it names a local, since a reader mints a local of its own for a word no binder binds.
#[test]
fn every_node_written_reads_back_as_itself() {
    let n = Free::local(0, Some("n"));
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let one = || Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
    let over = |binder: Free, term: Term| Term::func([(binder, nat_type())], term);
    let at = Universes {
        levels: vec![Level::param(UniverseParam(0))],
        qualities: vec![Quality::Type],
    };
    let instance = Term::instance(global(&["f"]), at);
    let witness = Global::Witness(WitnessId::Spelled {
        concept: Qualifier::from(["std", "Show"]),
        key: WitnessKey::from([HeadKey::Nat]),
        module: Qualifier::from(["std", "Nat"]),
        place: 0,
    });
    let show = global(&["std", "Show", "Show"]);
    let terms = [
        Term::type_ground(),
        Term::prop(),
        Term::type_at(Level::zero().succ().expect("level zero has a successor")),
        Term::from(Subterm::Sort(Sort {
            quality: Quality::Param(QualityParam(1)),
            level: Level::zero(),
        })),
        instance.clone(),
        Term::apply(instance, [nat_type()]),
        over(
            n,
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), three.clone())),
        ),
        three,
        Term::intrinsic(Intrinsic::Bool(true)),
        Term::intrinsic(Intrinsic::Flt(Floating::infinite(true))),
        Term::induct_type(global(&["Eq"]), [nat_type()], [one(), one()]),
        Term::variant(global(&["Result"]), [nat_type()], "success", [one()]),
        Term::var(Var::bound(2)),
        Term::hole(7),
        over(
            n,
            Term::from(Subterm::Proj(Proj {
                head: Term::free_var(&n),
                field: Field::Struct(global(&["Pair"]), 0),
            })),
        ),
        over(
            n,
            Term::from(Subterm::Match(Match {
                head: Term::free_var(&n),
                goal: nat_type(),
                cases: Cases::Bool {
                    false_case: one(),
                    true_case: one(),
                },
            })),
        ),
        over(
            n,
            Term::induct_type(global(&["Sign"]), Vec::<Term>::new(), [Term::free_var(&n)]),
        ),
        Term::instance(witness, Universes::default()),
        Term::method(show, "show"),
        // A match as it is written, an arm of two alternatives beside one of one, which states the rows each arm holds; and one of arms of one row each, which states none.
        Term::matrix(Matrix {
            head: one(),
            arms: vec![
                Arm {
                    alternatives: vec![
                        Row {
                            pattern: unsigned(3),
                            guards: Vec::new(),
                            body: Term::func(Vec::<(Free, Term)>::new(), one()),
                        },
                        Row {
                            pattern: unsigned(0),
                            guards: Vec::new(),
                            body: Term::func(Vec::<(Free, Term)>::new(), one()),
                        },
                    ],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Binder,
                        guards: Vec::new(),
                        body: over(Free::local(1, Some("m")), one()),
                    }],
                },
            ],
        }),
        Term::matrix(Matrix {
            head: one(),
            arms: vec![
                Arm {
                    alternatives: vec![Row {
                        pattern: unsigned(0),
                        guards: Vec::new(),
                        body: Term::func(Vec::<(Free, Term)>::new(), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Binder,
                        guards: Vec::new(),
                        body: over(Free::local(1, Some("m")), one()),
                    }],
                },
            ],
        }),
        // A row with guards, each the function over the binders written before it: a condition over the pattern's, and a bind whose pattern binds nothing more, the body over them all.
        Term::matrix(Matrix {
            head: one(),
            arms: vec![
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Binder,
                        guards: vec![
                            Guard::Holds(over(Free::local(1, Some("m")), one())),
                            Guard::Binds {
                                pattern: unsigned(0),
                                value: over(Free::local(1, Some("m")), one()),
                            },
                        ],
                        body: over(Free::local(1, Some("m")), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Binder,
                        guards: Vec::new(),
                        body: over(Free::local(1, Some("m")), one()),
                    }],
                },
            ],
        }),
        // A wildcard binds nothing, so its row's function binds nothing for it; a binder with no name is a place all the same, and a successor's predecessor is one whatever its word.
        Term::matrix(Matrix {
            head: one(),
            arms: vec![
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Succ {
                            predecessor: Slot::Binder,
                            step: Natural::from(2u32),
                        },
                        guards: Vec::new(),
                        body: over(Free::local(1, None), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Binder,
                        guards: Vec::new(),
                        body: over(Free::local(1, None), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::List(Elements::Open {
                            first: Box::new(Pattern::WILDCARD),
                            then: Vec::new(),
                            rest: Slot::Binder,
                        }),
                        guards: Vec::new(),
                        body: over(Free::local(1, Some("rest")), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::WILDCARD,
                        guards: Vec::new(),
                        body: Term::func(Vec::<(Free, Term)>::new(), one()),
                    }],
                },
            ],
        }),
        // A run's elements are patterns of their own: a closed run ends where they do, an open one's rest follows them, and a packed run is a run under `x[`.
        Term::matrix(Matrix {
            head: one(),
            arms: vec![
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::List(Elements::Closed(Vec::new())),
                        guards: Vec::new(),
                        body: Term::func(Vec::<(Free, Term)>::new(), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::List(Elements::Closed(vec![
                            Pattern::Binder,
                            Pattern::Variant {
                                tag: Atom::from("none"),
                                at: WrittenAt::default(),
                                args: Vec::new(),
                            },
                        ])),
                        guards: Vec::new(),
                        body: over(Free::local(1, Some("first")), one()),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::List(Elements::Open {
                            first: Box::new(Pattern::WILDCARD),
                            then: vec![Pattern::Binder],
                            rest: Slot::Binder,
                        }),
                        guards: Vec::new(),
                        body: Term::func(
                            [
                                (Free::local(1, Some("second")), nat_type()),
                                (Free::local(2, Some("rest")), nat_type()),
                            ],
                            one(),
                        ),
                    }],
                },
                Arm {
                    alternatives: vec![Row {
                        pattern: Pattern::Bytes(Elements::Open {
                            first: Box::new(Pattern::Binder),
                            then: vec![Pattern::WILDCARD],
                            rest: Slot::Wildcard,
                        }),
                        guards: Vec::new(),
                        body: over(Free::local(1, Some("byte")), one()),
                    }],
                },
            ],
        }),
        Term::metavar_birthed(
            MetavarId(3),
            MetavarOrigin::Witness(WitnessOrigin {
                func: CalleeId::Method {
                    concept: show,
                    field: "show".into(),
                },
                role: None,
            }),
            Vec::new(),
        ),
    ];

    for term in terms {
        assert_eq!(read_back(&term), term, "{}", written(&term));
    }
}

/// Every module a test above writes reads back as itself: a family from its form, and a record from its form.
#[test]
fn every_module_written_reads_back_as_itself() {
    let (a, b) = (Free::local(0, Some("A")), Free::local(1, Some("B")));
    let (fst, snd) = (Free::local(2, Some("fst")), Free::local(3, Some("snd")));
    let record = Nominals::new(vec![(
        global(&["Pair"]),
        Nominal::Struct(StructDecl {
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
        }),
    )]);
    let modules = [
        module(bit(), Vec::new()),
        Definitions {
            items: vec![Item::Nominal(record)].into(),
        },
    ];

    for definitions in modules {
        assert_eq!(
            module_read_back(&definitions),
            definitions,
            "{}",
            definitions.faithful()
        );
    }
}

/// A group's member is named two ways inside its group, and each reads back as itself: by its path, which is what the group binds, and as an instance of it, a global the group binds nothing of, which is written under `#{node = Instance}`.
#[test]
fn a_member_named_at_its_instance_inside_its_group_reads_back_apart_from_its_binder() {
    let name = global(&["loop"]);
    let local = Free::local(905, Some("loop"));
    let grouped = |body: Term| {
        let definition = Definition {
            name,
            kind: DefinitionKind::Authored,
            universe_context: UniverseContext::empty(),
            island: Qualifier::empty(),
            proof: false,
            value_sort: None,
            type_: nat_type(),
            body,
        };
        Definitions {
            items: vec![Item::Rec(RecItem::new(vec![(local, definition)]))].into(),
        }
    };
    let by_its_binder = grouped(Term::free_var(&local));
    let at_its_instance = grouped(Term::instance(name, Universes::default()));

    assert_ne!(by_its_binder, at_its_instance);
    for definitions in [by_its_binder, at_its_instance] {
        assert_eq!(
            module_read_back(&definitions),
            definitions,
            "{}",
            definitions.faithful()
        );
    }
}

/// A local no binder in the text binds is written whole by its identity and read back as that local, which a reader's own binders are minted apart from; a report names it by its word alone.
#[test]
fn a_local_no_binder_binds_reads_back_as_itself() {
    let stray = Free::local(0, Some("stray"));
    let term = Term::func(
        [(Free::local(1, Some("x")), nat_type())],
        Term::free_var(&stray),
    );
    let whole = Rc::new(Spelling::core_text().written_whole());
    let text = term.spelled(&whole).to_string();
    let parsed = parse_whole_term::<CoreText>(&Source::inline(&text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));

    assert_eq!(text, "(x: #NatType) => #{binding = Free(0)} stray");
    let read = term_to_core(&parsed).unwrap_or_else(|error| panic!("{text}\n{error}"));
    assert_eq!(read, term, "{text}");
    assert_eq!(written(&term), "(x: #NatType) => stray");
}

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

/// A node met again is written once under sharing and read back as that one node: a tower of sixty pairs, each over one pair below twice, is a graph of sixty-one nodes whose tree has 2⁶⁰ leaves, and it is written and read in its graph's size. A term with no node met twice is written as a whole write without sharing writes it.
#[test]
fn a_node_met_again_is_written_once_and_read_back_as_that_node() {
    let tower = (0..60u64).fold(nat_type(), |below, level| {
        let first = Free::local(2 * level + 100, Some("first"));
        let second = Free::local(2 * level + 101, Some("second"));

        Term::tuple_type([(first, below.clone()), (second, below)])
    });
    let definitions = Definitions {
        items: vec![Item::Let(Box::new(Definition {
            name: global(&["tower"]),
            kind: DefinitionKind::Authored,
            universe_context: UniverseContext::empty(),
            island: Qualifier::empty(),
            proof: false,
            value_sort: None,
            type_: Term::type_ground(),
            body: tower,
        }))]
        .into(),
    };
    let sharing = Rc::new(Spelling::core_text().written_whole().with_sharing());
    let text = definitions.spelled(&sharing).to_string();
    let file = text
        .parse::<CoreFile>()
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let (read, _) = to_core(&file, None).unwrap_or_else(|error| panic!("{text}\n{error}"));

    assert_eq!(read, definitions, "{text}");
    let Item::Let(definition) = &read.items[0] else {
        panic!("a definition reads back as one: {text}");
    };
    // Sixty pairs, and the `Nat` beneath them at each of the two places it stands: a node with no child is written where it stands rather than named.
    assert_eq!(definition.body.distinct_nodes(), 62, "{text}");
    assert_eq!(text.matches("share =").count(), 59, "{text}");

    let small = Term::tuple_type([(Free::local(0, Some("x")), nat_type())]);
    let whole = Rc::new(Spelling::core_text().written_whole());
    assert_eq!(
        small.spelled(&sharing).to_string(),
        small.spelled(&whole).to_string()
    );
}

/// What reading `text` as a term refuses, and where the refusal begins in it.
fn refused(text: &str) -> (Refusal, Option<usize>) {
    let parsed = parse_whole_term::<CoreText>(&Source::inline(text))
        .unwrap_or_else(|error| panic!("{text}\n{}", error.format()));
    let error = term_to_core(&parsed).expect_err(text);

    (error.refusal, error.span.map(|span| span.start))
}

/// A form whose meaning needs typing or resolution is a program's, refused by name where it stands, and so is a place only a program holds.
#[test]
fn a_form_only_a_program_writes_is_refused_by_name_where_it_stands() {
    for (text, form, at) in [
        ("f(a + b)", "an operator", 2),
        ("f(match | _ => a end)", "a match with no head", 2),
        ("f(\"text\")", "a string", 2),
        ("f(?)", "a goal `?`", 2),
    ] {
        assert_eq!(
            refused(text),
            (Refusal::ProgramForm(form), Some(at)),
            "{text}"
        );
    }
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

/// A test's form and a foreign row's are made against the scope a reader is handed, and none is refused.
#[test]
fn a_form_that_implies_a_definition_needs_a_scope() {
    for text in ["test /t = 1;\n", "foreign /f(Bytes) -> Bytes;\n"] {
        let file = text.parse::<CoreFile>().unwrap();
        assert!(matches!(
            to_core(&file, None).map(|_| ()),
            Err(ReadError {
                refusal: Refusal::NoScope(_),
                ..
            })
        ));
    }
}

/// A family's cases are under distinct tags ([`curios_core::Constructors`]), so a case whose tag a case before it has is refused by name where it stands rather than read hidden behind the earlier one, which every reader of the family would resolve the tag to.
#[test]
fn a_family_naming_one_case_twice_is_refused_where_the_repeat_stands() {
    let text = "induct /Two: pub Type\n| a()\n| a()\nend\n";
    let file = text.parse::<CoreFile>().unwrap();
    let error = to_core(&file, None).map(|_| ()).expect_err(text);

    assert_eq!(
        (error.refusal, error.span.map(|span| span.start)),
        (Refusal::RepeatedCase("a".to_string()), Some(30)),
    );
}

/// A level's `max` holds one part a head and one constant, as the printer writes it, so a `max` naming one twice is refused by name where the repeat stands rather than read as one of the two: `max(u + 1, u)` would otherwise be `u`, a level below the one written, and `max(5, 0)` would be `0`. Mutation-checked: with the parts folded by assignment, both read.
#[test]
fn a_max_naming_one_part_twice_is_refused_where_the_repeat_stands() {
    for (text, part, at) in [
        ("#{level = max(u + 1, u)} Type", "`u`", 21),
        ("#{level = max(5, 0)} Type", "a constant", 17),
    ] {
        assert_eq!(
            refused(text),
            (Refusal::RepeatedLevelPart(part.to_string()), Some(at)),
            "{text}"
        );
    }
}

/// A dispatch's keys are written ascending, each once, as the switch they read as holds them ([`curios_core::Keyed`]), so a key at or below one before it is refused where its arm stands rather than read as a switch other than the one written.
#[test]
fn a_dispatch_key_out_of_order_is_refused_where_its_arm_stands() {
    for text in [
        "#{goal = #NatType} match 5 | 3 => 0 | 1 => 1 | _ => 2 end",
        "#{goal = #NatType} match 5 | 1 => 0 | 1 => 1 | _ => 2 end",
    ] {
        assert_eq!(
            refused(text),
            (Refusal::DisorderedKey("1".to_string()), Some(43)),
            "{text}"
        );
    }
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
