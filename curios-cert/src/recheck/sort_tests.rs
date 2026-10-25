//! A declaration that leaves a sort to its caller: judged at every assignment of the sorts it leaves, and stated whole at each use.

use {
    super::test_support::*,
    crate::{Counted, Error, Globals, Verdict},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Definition, Free, Global, InductDecl, InductParam, Intrinsic, Item, Level, Many,
        Module, Nat, Quality, QualityParam, Rec, RecItem, Scope, StructDecl, Subterm, Telescope,
        Term, UniverseContext, UniverseParam, Universes, test_support::declaring,
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
        Item::Let(definition) => Item::Let(Box::new(Definition {
            universe_context: leaving(sorts),
            ..*definition
        })),
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
            )]
            .into(),
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
            )]
            .into(),
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
            )]
            .into(),
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

/// `induct Eq(@A: Sort): (x: A, y: A) -> Prop | refl(@z: A): (z, z) end`, the equality over a sort its caller states.
fn equality_over_a_sort() -> (Global, InductDecl) {
    let (carrier, this, that, value) = (
        Free::local(60, Some("A")),
        Free::local(61, Some("x")),
        Free::local(62, Some("y")),
        Free::local(63, Some("z")),
    );

    (
        named("Eq"),
        InductDecl {
            universe_context: leaving(1),
            arity: Telescope::build(
                [(carrier, left(0))],
                Telescope::build(
                    [
                        (this, Term::free_var(&carrier)),
                        (that, Term::free_var(&carrier)),
                    ],
                    (),
                ),
            ),
            constructors: vec![(
                Atom::from("refl"),
                InductParam::new(
                    Telescope::build(
                        [(carrier, left(0)), (value, Term::free_var(&carrier))],
                        vec![Term::free_var(&value), Term::free_var(&value)],
                    ),
                    vec![Plicity::Implicit, Plicity::Implicit],
                ),
            )]
            .into(),
            result_sort: Term::prop(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    )
}

/// The one sort a declaration leaves, as an occurrence inside it states it.
fn left_open() -> Universes {
    Universes {
        levels: Vec::new(),
        qualities: vec![Quality::Param(QualityParam(0))],
    }
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
            Item::Let(definition) => Item::Rec(RecItem::new(vec![*definition])),
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

/// A refusal at one assignment spells the declaration's terms at it, where the declaration's text says `Sort`. Its report restates the type the declaration states, read at the assignment, which is where a reader sees which written sort stood at which.
#[test]
fn a_refusal_at_one_sort_restates_the_declaration_at_it() {
    let module = module(vec![handing_back("back", Term::prop())], []);
    let reports = verdicts(&module)
        .iter()
        .map(|verdict| verdict.format_with(&module, &[], &SYNTAX))
        .collect::<Vec<_>>();

    assert!(
        reports.iter().any(|report| report.starts_with(
            "with the sorts it leaves to its caller at `Type`, where it reads `(A: Type) -> Prop`: "
        )),
        "{reports:?}",
    );
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

/// Two instances of one family apart in a sort are two types, whatever else they share: nothing of `Tagged` mentions the sort it leaves, and its instance at `Prop` is still no instance at `Type`. `curios`'s `tests::board::subsumption_tests` puts two such instances to the elaborator under this name. Rename both or neither.
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
        Item::Let(definition) => Item::Rec(RecItem::new(vec![*definition])),
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

/// A definition generic in a sort stands at `Type` at one use, where matching a proposition of two constructors into it would tell its proofs apart: judged at `Prop` the match is a proof's, and at `Type` it is the large elimination the guard refuses, so the definition is refused at that assignment. `curios`'s `tests::board::subsumption_tests` puts the same proposition to the elaborator under this name. Rename both or neither.
#[test]
fn a_proposition_is_not_matched_into_a_sort_left_to_a_caller() {
    let nullary = |tag: &str| {
        (
            Atom::from(tag),
            InductParam::new(Telescope::done(Vec::new()), Vec::new()),
        )
    };
    let two = named("Two");
    let two_type = Term::induct_type(two, Vec::<Term>::new(), Vec::<Term>::new());
    let (carrier, chosen, this, that, scrutinee) = (
        Free::local(30, Some("A")),
        Free::local(31, Some("t")),
        Free::local(32, Some("x")),
        Free::local(33, Some("y")),
        Free::local(34, Some("s")),
    );
    let binders = || {
        [
            (carrier, left(0)),
            (chosen, two_type.clone()),
            (this, Term::free_var(&carrier)),
            (that, Term::free_var(&carrier)),
        ]
    };
    let pick = generic(
        &named("pick"),
        1,
        Term::func_type(binders(), Term::free_var(&carrier)),
        Term::func(
            binders(),
            Term::induct_match_scoped_marked(
                Term::free_var(&chosen),
                Scope::close(Many(1), &[&scrutinee], Term::free_var(&carrier)),
                [
                    ("a", Vec::new(), Term::free_var(&this)),
                    ("b", Vec::new(), Term::free_var(&that)),
                ],
                None,
            ),
        ),
    );

    let verdicts = verdicts(&module(
        vec![pick],
        [(two, proposition(vec![nullary("a"), nullary("b")]))],
    ));

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::LargeElimination(_))) if sorts == [Quality::Type]
        )),
        "{verdicts:?}",
    );
}

/// A proposition carries only proofs at every assignment of the sorts it leaves: `struct W(A: Sort): Prop { a: A }` holds a proof at `Prop` and a value at `Type`, where any two of its values would be one with their fields apart, so it is refused at that assignment. `curios`'s `tests::board::subsumption_tests` puts the same structure to the elaborator under this name. Rename both or neither.
#[test]
fn a_proposition_holds_no_field_at_a_sort_left_to_its_caller() {
    let (carrier, held) = (Free::local(40, Some("A")), Free::local(41, Some("a")));
    let declaration = StructDecl {
        universe_context: leaving(1),
        arity: Telescope::build(
            [(carrier, left(0))],
            Telescope::build([(held, Term::free_var(&carrier))], ()),
        ),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let module = declaring(
        Module {
            mounts: Vec::new(),
            items: Vec::new().into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        [],
        [(named("W"), declaration)],
        [],
    );

    let verdicts = verdicts(&module);

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::Informative { .. })) if sorts == [Quality::Type]
        )),
        "{verdicts:?}",
    );
}

/// A value at a sort left to a caller is a proof at the use that states `Prop`, so a recursion answering one owes a proof's descent: `rec spin(A: Sort, n: Nat) -> A = spin(A, n)` is a proof's recursion at `Prop`, where it does not descend, and is refused at that assignment alone.
#[test]
fn a_recursion_into_a_sort_left_to_its_caller_descends_as_a_proof_must() {
    let spin = named("spin");
    let (carrier, count) = (Free::local(50, Some("A")), Free::local(51, Some("n")));
    let binders = || [(carrier, left(0)), (count, nat())];
    let group = match generic(
        &spin,
        1,
        Term::func_type(binders(), Term::free_var(&carrier)),
        Term::func(
            binders(),
            Term::apply(
                Term::free_var(&Free::from(&spin)),
                [Term::free_var(&carrier), Term::free_var(&count)],
            ),
        ),
    ) {
        Item::Let(definition) => Item::Rec(RecItem::new(vec![*definition])),
        item => item,
    };

    let verdicts = verdicts(&module(vec![group], []));

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::NotDescending { .. })) if sorts == [Quality::Prop]
        )),
        "{verdicts:?}",
    );
}

/// A recursive group's members state no sort its context does not leave: `rec id(A: Sort, a: A) -> A` under a context leaving none is refused, its members being read as definitions under the group's own context.
#[test]
fn a_group_stating_a_sort_its_context_does_not_leave_is_refused() {
    let group = match identity() {
        Item::Let(definition) => Item::Rec(RecItem::new(vec![Definition {
            universe_context: leaving(0),
            ..*definition
        }])),
        item => item,
    };

    let verdicts = verdicts(&module(vec![group], []));

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::OpenSort(_))),
        "{verdicts:?}",
    );
}

/// The sort a definition states its value inhabits is read at each assignment of the sorts its declaration leaves, though no scan before the judgment reads it: one past those sorts is a sort no assignment states, and is refused where the value's own sort is put beside it.
#[test]
fn a_value_sort_past_the_sorts_a_declaration_leaves_is_refused() {
    let item = match generic(&named("T"), 1, Term::type_ground(), nat()) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            value_sort: Some(left(1)),
            ..*definition
        })),
        item => item,
    };

    let verdicts = verdicts(&module(vec![item], []));

    assert!(
        verdicts.iter().any(|verdict| matches!(
            at_sorts(verdict),
            Some((sorts, Error::ValueSort { .. })) if sorts == [Quality::Prop]
        )),
        "{verdicts:?}",
    );
}

/// A recursive occurrence that only a sort left to a caller would uncover: at `Prop` the match on `h` is its arm, an equation between two proofs being one, and the arm holds `Bad` left of an arrow. The entry is decided as it is stated, where the match stands, and a match the walk cannot see through is read at `Mixed`, so the occurrence is refused whatever a use states. `curios`'s `tests::positivity::refusal_tests` puts the same declaration to the elaborator under this name. Rename both or neither.
#[test]
fn an_occurrence_a_sort_left_to_a_caller_would_uncover_is_refused() {
    let (eq, eq_decl) = equality_over_a_sort();
    let bottom = named("False");
    let bad = named("Bad");
    let (carrier, this, that, proof, eater, value, eaten) = (
        Free::local(70, Some("A")),
        Free::local(71, Some("x")),
        Free::local(72, Some("y")),
        Free::local(73, Some("h")),
        Free::local(74, Some("f")),
        Free::local(75, Some("z")),
        Free::local(76, Some("b")),
    );
    let (left_index, right_index, scrutinee) = (
        Free::local(77, None),
        Free::local(78, None),
        Free::local(79, None),
    );
    let equation = Term::induct_type_at(
        eq,
        left_open(),
        vec![Term::free_var(&carrier)],
        vec![Term::free_var(&this), Term::free_var(&that)],
    );
    let itself = Term::induct_type_at(
        bad,
        left_open(),
        vec![
            Term::free_var(&carrier),
            Term::free_var(&this),
            Term::free_var(&that),
            Term::free_var(&proof),
        ],
        Vec::<Term>::new(),
    );
    // match h : Eq(A)(x, y) | refl(@z) => (b: Bad(A, x, y, h)) -> False end
    let payload = Term::induct_match_scoped_marked(
        Term::free_var(&proof),
        Scope::close(
            Many(3),
            &[&left_index, &right_index, &scrutinee],
            Term::prop(),
        ),
        [(
            "refl",
            vec![(Plicity::Implicit, value)],
            Term::func_type(
                [(eaten, itself)],
                Term::induct_type(bottom, Vec::<Term>::new(), Vec::<Term>::new()),
            ),
        )],
        None,
    )
    .stating(equation.clone());
    let declaration = InductDecl {
        universe_context: leaving(1),
        arity: Telescope::build(
            [
                (carrier, left(0)),
                (this, Term::free_var(&carrier)),
                (that, Term::free_var(&carrier)),
                (proof, equation.clone()),
            ],
            Telescope::done(()),
        ),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build(
                    [
                        (carrier, left(0)),
                        (this, Term::free_var(&carrier)),
                        (that, Term::free_var(&carrier)),
                        (proof, equation),
                        (eater, payload),
                    ],
                    Vec::new(),
                ),
                vec![
                    Plicity::Implicit,
                    Plicity::Explicit,
                    Plicity::Explicit,
                    Plicity::Explicit,
                    Plicity::Explicit,
                ],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let verdicts = verdicts(&module(
        Vec::new(),
        [
            (eq, eq_decl),
            (bottom, proposition(Vec::new())),
            (bad, declaration),
        ],
    ));

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotPositive { .. })),
        "{verdicts:?}",
    );
}

/// A level carried only by a match that stands is held: `induct Lv(@A: Sort, x: A, y: A, h: Eq(x, y)): Type u | mk(f: match h : (_, _, _) => Type u | refl(@z) => Nat end) end` holds `u` in its motive alone, where conversion compares two such matches, since at `Type` a match on `h` stands whatever `x` and `y` are. So two instances of `Lv` at `Type` apart in `u` are two types, and a value of one is no value of the other.
#[test]
fn a_level_a_standing_match_carries_is_held_by_its_family() {
    let (eq, eq_decl) = equality_over_a_sort();
    let lv = named("Lv");
    let level = Level::param(UniverseParam(0));
    let (carrier, this, that, proof, field, value) = (
        Free::local(80, Some("A")),
        Free::local(81, Some("x")),
        Free::local(82, Some("y")),
        Free::local(83, Some("h")),
        Free::local(84, Some("f")),
        Free::local(85, Some("z")),
    );
    let (left_index, right_index, scrutinee) = (
        Free::local(86, None),
        Free::local(87, None),
        Free::local(88, None),
    );
    let equation = |universes: Universes, carrier: &Free, this: &Free, that: &Free| {
        Term::induct_type_at(
            eq,
            universes,
            vec![Term::free_var(carrier)],
            vec![Term::free_var(this), Term::free_var(that)],
        )
    };
    let payload = Term::induct_match_scoped_marked(
        Term::free_var(&proof),
        Scope::close(
            Many(3),
            &[&left_index, &right_index, &scrutinee],
            Term::type_at(level.clone()),
        ),
        [("refl", vec![(Plicity::Implicit, value)], nat())],
        None,
    )
    .stating(equation(left_open(), &carrier, &this, &that));
    let parameters = || {
        [
            (carrier, left(0)),
            (this, Term::free_var(&carrier)),
            (that, Term::free_var(&carrier)),
            (proof, equation(left_open(), &carrier, &this, &that)),
        ]
    };
    let [a, x, y, h] = parameters();
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 1,
            constraints: Vec::new(),
        },
        arity: Telescope::build(parameters(), Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build([a, x, y, h, (field, payload)], Vec::new()),
                vec![
                    Plicity::Implicit,
                    Plicity::Explicit,
                    Plicity::Explicit,
                    Plicity::Explicit,
                    Plicity::Explicit,
                ],
            ),
        )]
        .into(),
        result_sort: Term::type_at(level),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    // coerce(A: Type, x: A, y: A, h: Eq(x, y), v: Lv.{0; Type}(A, x, y, h)) -> Lv.{1; Type}(A, x, y, h) = v
    let (held_carrier, held_this, held_that, held_proof, held) = (
        Free::local(90, Some("A")),
        Free::local(91, Some("x")),
        Free::local(92, Some("y")),
        Free::local(93, Some("h")),
        Free::local(94, Some("v")),
    );
    let at_type = Universes {
        levels: Vec::new(),
        qualities: vec![Quality::Type],
    };
    let lv_at = |level: Level| {
        Term::induct_type_at(
            lv,
            Universes {
                levels: vec![level],
                qualities: vec![Quality::Type],
            },
            vec![
                Term::free_var(&held_carrier),
                Term::free_var(&held_this),
                Term::free_var(&held_that),
                Term::free_var(&held_proof),
            ],
            Vec::<Term>::new(),
        )
    };
    let binders = || {
        [
            (held_carrier, Term::type_ground()),
            (held_this, Term::free_var(&held_carrier)),
            (held_that, Term::free_var(&held_carrier)),
            (
                held_proof,
                equation(at_type.clone(), &held_carrier, &held_this, &held_that),
            ),
            (held, lv_at(Level::zero())),
        ]
    };
    let coerce = named("coerce");
    let module = module(
        vec![authored(
            &coerce,
            Term::func_type(binders(), lv_at(Level::constant(1))),
            Term::func(binders(), Term::free_var(&held)),
        )],
        [(eq, eq_decl), (lv, declaration)],
    );

    let verdicts = verdicts(&module);

    assert!(
        !verdicts.iter().any(|verdict| verdict.name == Some(lv)),
        "the family is accepted: {verdicts:?}",
    );
    assert!(
        verdicts.iter().any(|verdict| verdict.name == Some(coerce)
            && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}",
    );
}
