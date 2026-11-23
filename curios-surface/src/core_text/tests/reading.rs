//! What Core text reads back: every node and module as itself, a member, a local and a node met again, and the forms refused where they stand.

use {
    super::test_support::*,
    crate::{
        CoreFile, CoreText, ReadError, Refusal, Spellable, Spelling, parse_whole_term, to_core,
    },
    curios_common::{Qualifier, Source},
    curios_core::{
        Arm, Atom, CalleeId, Cases, Definition, DefinitionKind, Definitions, Elements, Field, Free,
        Global, Guard, HeadKey, Intrinsic, Item, Level, Match, Matrix, MetavarId, MetavarOrigin,
        Nat, Nominal, Nominals, NumLit, Numeral, Pattern, Proj, Quality, QualityParam, RecItem,
        Row, Slot, Sort, StructDecl, Subterm, Telescope, Term, UniverseContext, UniverseParam,
        Universes, Var, WitnessId, WitnessKey, WitnessOrigin, WrittenAt,
    },
    curios_num::{Floating, Natural},
    std::rc::Rc,
};

/// The pattern of the unsigned numeral `value`.
fn unsigned(value: usize) -> Pattern {
    Pattern::Literal(NumLit::Number(Numeral::Natural(value.into())))
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
        // A run's elements are patterns of their own: a closed run ends where they do, an open one's rest follows them, and a packed run is a run under `0x[`.
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
