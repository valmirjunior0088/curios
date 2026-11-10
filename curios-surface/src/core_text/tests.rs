//! Core text proper: each node in its form, under the decorator stating every field its form does not build.

use {
    super::*,
    crate::{
        CoreFile, CoreText, ReadError, Refusal, Spellable, Spelling, parse_whole_term, to_core,
    },
    curios_common::{Qualifier, Source},
    curios_core::{
        Atom, CalleeId, Cases, Definition, Definitions, Field, Free, Global, HeadKey, InductDecl,
        InductParam, Intrinsic, Item, Level, Match, MatchResult, MetavarId, MetavarOrigin, Nat,
        Nominal, Proj, Quality, QualityParam, RecItem, Sort, StructDecl, Subterm, Telescope, Term,
        UniverseContext, UniverseParam, Universes, Var, WitnessId, WitnessKey, WitnessOrigin,
    },
    curios_num::Floating,
    std::rc::Rc,
};

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
    assert_eq!(
        written(&Term::intrinsic(Intrinsic::Flt(Floating::infinite(true)))),
        "#Flt(-inf.0)"
    );
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
        "#{result = Ambient(#NatType)} match b | false => 0 | true => 1 end"
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

/// A version's items stand at places spaced apart, which no count of them names, and are written as the module they hold: a family's form, its constructors left to it.
#[test]
fn a_version_is_written_as_the_module_it_holds() {
    let constructors =
        Definition::constructors(Qualifier::from(["Bit"]), Qualifier::empty(), &bit());
    let made = module(bit(), constructors);
    let version = Definitions {
        items: made.items.version(),
    };

    assert!(version.items.is_version());
    assert_eq!(version.faithful().to_string(), made.faithful().to_string());
}

/// A constructor its family's form would not make is written out, under the kind that says whose it is.
#[test]
fn a_constructor_its_family_does_not_make_is_written_out() {
    let mut constructors =
        Definition::constructors(Qualifier::from(["Bit"]), Qualifier::empty(), &bit());
    constructors[1].body = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));

    let written = module(bit(), constructors).faithful().to_string();
    assert!(
        written.contains("#{kind = InductiveConstructor { owner = /Bit, tag = one }} let /Bit/one"),
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
                result: MatchResult::Ambient(nat_type()),
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

/// Every module a test above writes reads back as itself: a family's constructors made where its form implies them, one it does not make standing in its place, one it lacks made nowhere, and a record from its form.
#[test]
fn every_module_written_reads_back_as_itself() {
    let made = Definition::constructors(Qualifier::from(["Bit"]), Qualifier::empty(), &bit());
    let mut rewritten = made.clone();
    rewritten[1].body = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
    let mut lacking = made.clone();
    lacking.truncate(1);
    let (a, b) = (Free::local(0, Some("A")), Free::local(1, Some("B")));
    let (fst, snd) = (Free::local(2, Some("fst")), Free::local(3, Some("snd")));
    let record = Definition::former(
        global(&["Pair"]),
        Qualifier::empty(),
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
    );
    let modules = [
        module(bit(), made),
        module(bit(), rewritten),
        module(bit(), lacking),
        Definitions {
            items: vec![Item::Let(Box::new(record))].into(),
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
        ("f(choose | _ => a end)", "a `choose`", 2),
        ("f(\"text\")", "a string", 2),
        ("f(?)", "a goal `?`", 2),
        ("f(-1)", "a signed numeral", 2),
    ] {
        assert_eq!(
            refused(text),
            (Refusal::ProgramForm(form), Some(at)),
            "{text}"
        );
    }
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
