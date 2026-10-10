//! A declaration that leaves a sort to its caller: judged at every assignment of the sorts it leaves, and stated whole at each use.

use {
    super::test_support::*,
    crate::{Counted, Error, Globals, Verdict},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Definition, Free, Global, InductDecl, InductParam, Intrinsic, Item, Level, Module,
        Nat, Quality, QualityParam, Rec, RecItem, Subterm, Telescope, Term, UniverseContext,
        Universes, test_support::declaring,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::BTreeSet,
};

/// A context that leaves `sorts` sorts to its caller and binds no level.
fn leaving(sorts: usize) -> UniverseContext {
    UniverseContext {
        parameter_count: 0,
        quality_count: sorts,
        constraints: Vec::new(),
    }
}

/// The sort a declaration's quality parameter `index` stands for, at level zero.
fn left(index: usize) -> Term {
    Term::sort_at(Quality::Param(QualityParam(index)), Level::zero())
}

fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// `let name: type_ = body`, leaving `sorts` sorts to its caller.
fn generic(name: &Global, sorts: usize, type_: Term, body: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Definition {
            universe_context: leaving(sorts),
            ..definition
        }),
        item => item,
    }
}

/// An occurrence of `name` stating `sorts`.
fn at(name: &Global, sorts: impl IntoIterator<Item = Quality>) -> Term {
    Term::instance_of(
        &Free::from(name),
        Universes {
            levels: Vec::new(),
            qualities: sorts.into_iter().collect(),
        },
    )
}

/// A module of `items` declaring `families`.
fn module(items: Vec<Item>, families: impl IntoIterator<Item = (Global, InductDecl)>) -> Module {
    declaring(
        Module {
            mounts: Vec::new(),
            items: items.into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        families,
        [],
        [],
    )
}

fn verdicts(module: &Module) -> Vec<Verdict> {
    fixture_verdicts(module, 1_000_000, &Globals::default(), SYNTAX)
}

/// The refusal a verdict reached at one assignment, with the assignment: `None` for a refusal that names none.
fn at_sorts(verdict: &Verdict) -> Option<(&[Quality], &Error)> {
    match &verdict.error {
        Error::AtSorts { sorts, refusal } => Some((sorts, refusal)),
        _ => None,
    }
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn zero() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))
}

/// `induct Held: Prop | qed() end`, with the family and its one proof.
fn held() -> ((Global, InductDecl), Term, Term) {
    let name = named("Held");
    let qed = (
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    );

    (
        (name, proposition(vec![qed])),
        Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new()),
        Term::variant(name, Vec::<Term>::new(), "qed", Vec::<Term>::new()),
    )
}

/// `let id(A: Sort, a: A) -> A = a`, the sort its caller's.
fn identity() -> Item {
    let (carrier, held) = (Free::local(10, Some("A")), Free::local(11, Some("a")));
    let binders = || [(carrier, left(0)), (held, Term::free_var(&carrier))];

    generic(
        &named("id"),
        1,
        Term::func_type(binders(), Term::free_var(&carrier)),
        Term::func(binders(), Term::free_var(&held)),
    )
}

/// `let name(A: Sort) -> codomain = A`: well typed where the sort `A` is declared at is `codomain`, and at no other.
fn handing_back(name: &str, codomain: Term) -> Item {
    let carrier = Free::local(12, Some("A"));

    generic(
        &named(name),
        1,
        Term::func_type([(carrier, left(0))], codomain),
        Term::func([(carrier, left(0))], Term::free_var(&carrier)),
    )
}

/// `let name(X: sort) -> sort = X`: what takes a type of the one sort `sort` is.
fn taking_only(name: &Global, sort: Term) -> Item {
    let held = Free::local(22, Some("X"));

    authored(
        name,
        Term::func_type([(held, sort.clone())], sort.clone()),
        Term::func([(held, sort)], Term::free_var(&held)),
    )
}

/// `induct Through(A: Sort): Type | through(x: only(A)) end`: a family over a sort its caller states whose one payload is well typed where `only` takes that sort, and at no other.
fn holding_through(only: &Global) -> (Global, InductDecl) {
    let (carrier, held) = (Free::local(23, Some("A")), Free::local(24, Some("x")));
    let payload = Term::apply(
        Term::free_var(&Free::from(only)),
        [Term::free_var(&carrier)],
    );

    (
        named("Through"),
        InductDecl {
            universe_context: leaving(1),
            arity: Telescope::build([(carrier, left(0))], Telescope::done(())),
            constructors: vec![(
                Atom::from("through"),
                InductParam::new(
                    Telescope::build([(carrier, left(0)), (held, payload)], Vec::new()),
                    vec![Plicity::Implicit, Plicity::Explicit],
                ),
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    )
}

/// `induct Box(A: Sort): Type | box(a: A) end`, the sort of what it holds its caller's.
fn box_over_a_sort(result_sort: Term) -> (Global, InductDecl) {
    let (carrier, held) = (Free::local(20, Some("A")), Free::local(21, Some("a")));

    (
        named("Box"),
        InductDecl {
            universe_context: leaving(1),
            arity: Telescope::build([(carrier, left(0))], Telescope::done(())),
            constructors: vec![(
                Atom::from("box"),
                InductParam::new(
                    Telescope::build(
                        [(carrier, left(0)), (held, Term::free_var(&carrier))],
                        Vec::new(),
                    ),
                    vec![Plicity::Implicit, Plicity::Explicit],
                ),
            )],
            result_sort,
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    )
}

/// `induct Tagged: Type | tag() end`, leaving a sort to its caller that nothing of it mentions.
fn tagged() -> (Global, InductDecl) {
    (
        named("Tagged"),
        InductDecl {
            universe_context: leaving(1),
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("tag"),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    )
}

/// A family at the sorts an occurrence states, applied to `params`.
fn family_at(name: Global, sorts: impl IntoIterator<Item = Quality>, params: Vec<Term>) -> Term {
    Term::induct_type_at(
        name,
        Universes {
            levels: Vec::new(),
            qualities: sorts.into_iter().collect(),
        },
        params,
        Vec::<Term>::new(),
    )
}

/// One definition serves data and proofs: each use states the sort it stands at, and reads the definition's type and body at it.
#[test]
fn one_definition_is_used_at_a_type_and_at_a_proposition() {
    let (held, held_type, qed) = held();
    let id = named("id");

    let module = module(
        vec![
            identity(),
            authored(
                &named("at_data"),
                nat(),
                Term::apply(at(&id, [Quality::Type]), [nat(), zero()]),
            ),
            authored(
                &named("at_proof"),
                held_type.clone(),
                Term::apply(at(&id, [Quality::Prop]), [held_type, qed]),
            ),
        ],
        [held],
    );

    let verdicts = verdicts(&module);
    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A use may state either sort, so a declaration is held to both: one well typed where its sort is `Prop` and at no other is refused at `Type`, and its mirror at `Prop`. A walk that judged the first assignment alone would certify the first, and one that judged the last alone the second. Put to each rule that judges a declaration: a definition's, a recursive group's and a registry entry's.
#[test]
fn a_declaration_ill_typed_at_one_sort_alone_is_refused_at_it() {
    for (sort, well_typed_at, refused_at) in [
        (Term::prop(), "`Prop`", Quality::Type),
        (Term::type_ground(), "`Type`", Quality::Prop),
    ] {
        let grouped = match handing_back("back", sort.clone()) {
            Item::Let(definition) => Item::Rec(RecItem::new(vec![definition])),
            item => item,
        };
        let only = named("only");

        for (rule, module) in [
            (
                "a definition",
                module(vec![handing_back("back", sort.clone())], []),
            ),
            ("a recursive group", module(vec![grouped], [])),
            (
                "a registry entry",
                module(
                    vec![taking_only(&only, sort.clone())],
                    [holding_through(&only)],
                ),
            ),
        ] {
            let verdicts = verdicts(&module);

            assert!(
                verdicts.iter().any(|verdict| matches!(
                    at_sorts(verdict),
                    Some((sorts, Error::Mismatch { .. })) if sorts == [refused_at]
                )),
                "{rule}, well typed at {well_typed_at} alone: {verdicts:?}",
            );
        }
    }
}

/// The control for the fixture above: the same definition whose codomain is the sort it is handed is well typed at both.
#[test]
fn a_definition_well_typed_at_every_sort_is_accepted() {
    let module = module(vec![handing_back("back", left(0))], []);
    let verdicts = verdicts(&module);

    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// An occurrence states as many sorts as its declaration leaves, each `Prop` or `Type`: one stating none denotes no instance, one stating too few is short of what the type is read at, and one stating a parameter would be judged at a sort that is neither.
#[test]
fn an_occurrence_states_every_sort_its_declaration_leaves() {
    let id = named("id");
    let both = named("both");
    let (carrier, other, held) = (
        Free::local(13, Some("A")),
        Free::local(14, Some("B")),
        Free::local(15, Some("a")),
    );
    let binders = || {
        [
            (carrier, left(0)),
            (other, left(1)),
            (held, Term::free_var(&carrier)),
        ]
    };
    let two_sorts = generic(
        &both,
        2,
        Term::func_type(binders(), Term::free_var(&carrier)),
        Term::func(binders(), Term::free_var(&held)),
    );

    type Expected = fn(&Error) -> bool;
    let cases: [(&str, Vec<Item>, Expected); 3] = [
        (
            "no sort stated",
            vec![
                identity(),
                authored(
                    &named("user"),
                    nat(),
                    Term::apply(Term::free_var(&Free::from(&id)), [nat(), zero()]),
                ),
            ],
            |error| matches!(error, Error::MissingUniverseInstance { .. }),
        ),
        (
            "one sort stated of two",
            vec![
                two_sorts,
                authored(
                    &named("user"),
                    nat(),
                    Term::apply(at(&both, [Quality::Type]), [nat(), nat(), zero()]),
                ),
            ],
            |error| {
                matches!(
                    error,
                    Error::Arity {
                        counted: Counted::Sorts,
                        expected: 2,
                        actual: 1,
                    }
                )
            },
        ),
        (
            "a parameter stated by a declaration that leaves none",
            vec![
                identity(),
                authored(
                    &named("user"),
                    nat(),
                    Term::apply(at(&id, [Quality::Param(QualityParam(0))]), [nat(), zero()]),
                ),
            ],
            |error| matches!(error, Error::OpenSort(_)),
        ),
    ];

    for (label, items, expected) in cases {
        let verdicts = verdicts(&module(items, []));

        assert!(
            verdicts.iter().any(|verdict| expected(&verdict.error)),
            "{label}: {verdicts:?}",
        );
    }
}

/// A quality parameter past the ones its declaration leaves is bound by nothing: no assignment the declaration is judged at reaches it.
#[test]
fn a_sort_past_the_ones_a_declaration_leaves_is_refused() {
    let carrier = Free::local(16, Some("A"));
    let module = module(
        vec![generic(
            &named("past"),
            1,
            Term::func_type([(carrier, left(1))], left(1)),
            Term::func([(carrier, left(1))], Term::free_var(&carrier)),
        )],
        [],
    );
    let verdicts = verdicts(&module);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::OpenSort(_))),
        "{verdicts:?}",
    );
}

/// A family over a sort its caller states is one declaration, held at a type and at a proposition: its telescopes are typed at each assignment, and what is decided of the declaration as data — its polarities, its variances — reads no sort.
#[test]
fn a_family_over_a_sort_holds_data_and_proofs() {
    let (held, held_type, qed) = held();
    let (name, declaration) = box_over_a_sort(Term::type_ground());
    let boxed = |sort: Quality, carrier: Term, value: Term| {
        authored(
            &named(match sort.is_prop() {
                true => "a_proof",
                false => "a_number",
            }),
            family_at(name, [sort], vec![carrier.clone()]),
            Term::variant_at(
                name,
                Universes {
                    levels: Vec::new(),
                    qualities: vec![sort],
                },
                [carrier],
                "box",
                [value],
            ),
        )
    };

    let module = module(
        vec![
            boxed(Quality::Type, nat(), zero()),
            boxed(Quality::Prop, held_type, qed),
        ],
        [held, (name, declaration)],
    );

    let verdicts = verdicts(&module);
    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A family's own sort is one sort whatever a use states: the rules that tell a proof from data by a declaration read the sort it declares, as written.
#[test]
fn a_family_whose_own_sort_is_left_to_its_caller_is_refused() {
    let module = module(Vec::new(), [box_over_a_sort(left(0))]);
    let verdicts = verdicts(&module);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::OpenSort(_))),
        "{verdicts:?}",
    );
}

/// Two instances of one family apart in a sort are two types, whatever else they share: nothing of `Tagged` mentions the sort it leaves, and its instance at `Prop` is still no instance at `Type`.
#[test]
fn two_instances_of_a_family_apart_in_a_sort_are_two_types() {
    let (name, _) = tagged();
    let held = Free::local(17, Some("x"));
    let coercion = |to: Quality| {
        let from = family_at(name, [Quality::Prop], Vec::new());

        module(
            vec![authored(
                &named("coerce"),
                Term::func_type([(held, from.clone())], family_at(name, [to], Vec::new())),
                Term::func([(held, from)], Term::free_var(&held)),
            )],
            [tagged()],
        )
    };

    let apart = verdicts(&coercion(Quality::Type));
    assert!(
        apart
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Mismatch { .. })),
        "{apart:?}",
    );

    let same = verdicts(&coercion(Quality::Prop));
    assert!(same.is_empty(), "{same:?}");
}

/// A top-level group leaves a sort to its caller as a definition does: judged at every assignment, defined as stated, and total under its own spelling where it is at each.
#[test]
fn a_recursive_group_is_used_at_a_type_and_at_a_proposition() {
    let (held, held_type, qed) = held();
    let id = named("id");
    let group = match identity() {
        Item::Let(definition) => Item::Rec(RecItem::new(vec![definition])),
        item => item,
    };

    let module = module(
        vec![
            group,
            authored(
                &named("at_data"),
                nat(),
                Term::apply(at(&id, [Quality::Type]), [nat(), zero()]),
            ),
            authored(
                &named("at_proof"),
                held_type.clone(),
                Term::apply(at(&id, [Quality::Prop]), [held_type, qed]),
            ),
        ],
        [held],
    );

    let verdicts = verdicts(&module);
    assert!(verdicts.is_empty(), "{verdicts:?}");
}

/// A group inside a term binds no sort: a quality parameter there is the enclosing declaration's, so a group whose own context leaves one is refused where it is judged.
#[test]
fn a_group_inside_a_term_leaves_no_sort_to_a_caller() {
    let member = Free::local(18, Some("m"));
    let built = Term::rec([(member, nat(), zero())], Term::free_var(&member));
    let Subterm::Rec(Rec { group, tail }) = &*built else {
        panic!("a `rec` is built as one");
    };
    let local = Term::from(Subterm::Rec(Rec {
        group: group.clone().with_universe_context(leaving(1)),
        tail: tail.clone(),
    }));

    let verdicts = verdicts(&module(vec![authored(&named("user"), nat(), local)], []));

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::GroupLeavesSort)),
        "{verdicts:?}",
    );
}

/// A declaration is judged at two to the count of the sorts it leaves, so the count is bounded.
#[test]
fn a_declaration_leaves_no_more_sorts_than_the_limit() {
    let stated = UniverseContext::QUALITY_LIMIT + 1;
    let module = module(vec![generic(&named("many"), stated, nat(), zero())], []);
    let verdicts = verdicts(&module);

    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::TooManySorts { stated: found } if found == stated
        )),
        "{verdicts:?}",
    );
}
