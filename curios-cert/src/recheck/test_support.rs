//! The hand-built adversarial modules the recheck suites forge, and the declarations they forge them from.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `recheck`, and nothing outside it.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::{Globals, Kernel, Verdict},
    curios_abi::{ForeignFunction, Namespace, WireSignature, WireType},
    curios_core::{
        Atom, Definition, DefinitionKind, Entrypoint, Free, Func, FuncType, Global, InductDecl,
        InductParam, Intrinsic, Item, Level, Many, Module, Nat, RecGroup, RecMemberScopes, Scope,
        StructDecl, StructType, Subterm, Telescope, Term, Totality, UniverseConstraint,
        UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext, UniverseParam,
        derived_binder_floor,
    },
    curios_utilities::{Plicity, Qualifier, SyntaxRegistry},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A top-level definition, as `recheck_module_verdicts` binds one.
pub(super) fn authored(name: &Global, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: name.clone(),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        // Non-recursive and `ProcExit`-free, so the honest flag; `partial_definitions` recomputes it.
        totality: Totality::Total,
        type_,
        body,
    })
}

/// [`authored`] for a body that recurses without descending. `partial_definitions` recomputes the flag and reports a recorded `Total` it disagrees with, so a partial body must say so.
pub(super) fn authored_partial(name: &Global, type_: Term, body: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Definition {
            totality: Totality::Partial,
            ..definition
        }),
        item => item,
    }
}

/// A nullary `Prop`-sorted family: `False` itself, and the shape `Box` takes but for its payload.
pub(super) fn proposition(constructors: Vec<(Atom, InductParam)>) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors,
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// `induct Eq(@A : Type 1) : (x : A, y : A) -> Prop | refl(z : A) : (z, z) end` — the equality the forgeries transport along.
pub(super) fn equality_declaration() -> InductDecl {
    let type_1 = Term::type_at(Level::zero().succ().expect("level zero has a successor"));
    let carrier = Free::local(20, Some("A"));
    let left = Free::local(21, Some("x"));
    let right = Free::local(22, Some("y"));
    let value = Free::local(23, Some("z"));

    let mut declaration = proposition(vec![(
        Atom::from("refl"),
        InductParam {
            telescope: Telescope::build(
                [
                    (carrier.clone(), type_1.clone()),
                    (value.clone(), Term::free_var(&carrier)),
                ],
                vec![Term::free_var(&value), Term::free_var(&value)],
            ),
            plicities: vec![Plicity::Implicit, Plicity::Explicit],
        },
    )]);
    declaration.arity = Telescope::build(
        [(carrier.clone(), type_1.clone())],
        Telescope::build(
            [
                (left, Term::free_var(&carrier)),
                (right, Term::free_var(&carrier)),
            ],
            (),
        ),
    );

    declaration
}

/// The module the doc comment above describes: three declarations, and the five definitions that close on `False`.
pub(super) fn forgery() -> Module {
    let type_0 = Term::type_ground();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let box_name = Global::Authored(Qualifier::from(["Box"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));

    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let box_type = Term::induct_type(box_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let boxed =
        |carried: Term| Term::variant(box_name.clone(), Vec::<Term>::new(), "mk", [carried]);
    let equality = |carrier: Term, left: Term, right: Term| {
        Term::induct_type(equality_name.clone(), [carrier], [left, right])
    };
    let reflexivity = |carrier: Term, value: Term| {
        Term::variant(equality_name.clone(), [carrier], "refl", [value])
    };

    // induct Box : Prop | mk(a : Type 0) end
    let payload = Free::local(10, Some("a"));
    let box_decl = proposition(vec![(
        Atom::from("mk"),
        InductParam {
            telescope: Telescope::build([(payload, type_0.clone())], Vec::new()),
            plicities: vec![Plicity::Explicit],
        },
    )]);

    let equality_decl = equality_declaration();

    // unbox : (Box) -> Type 0 = (b) => match b : (_) => Type 0 | mk(a) => a end
    let unbox_name = Global::Authored(Qualifier::from(["unbox"]));
    let subject = Free::local(30, Some("b"));
    let scrutinee = Free::local(31, Some("s"));
    let opened = Free::local(32, Some("a"));
    let unbox = authored(
        &unbox_name,
        Term::func_type(
            [(Free::local(33, Some("b")), box_type.clone())],
            type_0.clone(),
        ),
        Term::func(
            [(subject.clone(), box_type.clone())],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(Many(1), &[&scrutinee], type_0.clone()),
                [(
                    "mk",
                    vec![(Plicity::Explicit, opened.clone())],
                    Term::free_var(&opened),
                )],
                None,
            ),
        ),
    );
    let unboxed = |carried: Term| Term::apply(Term::free_var(&Free::from(&unbox_name)), [carried]);

    // boxes_equal : (A : Type 0, B : Type 0) -> Eq(Box, mk(A), mk(B)) = refl(mk(A))
    let boxes_equal_name = Global::Authored(Qualifier::from(["boxes_equal"]));
    let this = Free::local(40, Some("A"));
    let that = Free::local(41, Some("B"));
    let boxes_equal = authored(
        &boxes_equal_name,
        Term::func_type(
            [
                (this.clone(), type_0.clone()),
                (that.clone(), type_0.clone()),
            ],
            equality(
                box_type.clone(),
                boxed(Term::free_var(&this)),
                boxed(Term::free_var(&that)),
            ),
        ),
        Term::func(
            [
                (this.clone(), type_0.clone()),
                (that.clone(), type_0.clone()),
            ],
            reflexivity(box_type.clone(), boxed(Term::free_var(&this))),
        ),
    );

    // types_equal : (A : Type 0, B : Type 0) -> Eq(Type 0, A, B)
    //   = match boxes_equal(A, B) : (x, y, _) => Eq(Type 0, unbox(x), unbox(y))
    //     | refl(z) => refl(unbox(z)) end
    let types_equal_name = Global::Authored(Qualifier::from(["types_equal"]));
    let source = Free::local(50, Some("A"));
    let target = Free::local(51, Some("B"));
    let motive_left = Free::local(52, Some("x"));
    let motive_right = Free::local(53, Some("y"));
    let motive_proof = Free::local(54, Some("q"));
    let arm_value = Free::local(55, Some("z"));
    let types_equal = authored(
        &types_equal_name,
        Term::func_type(
            [
                (source.clone(), type_0.clone()),
                (target.clone(), type_0.clone()),
            ],
            equality(
                type_0.clone(),
                Term::free_var(&source),
                Term::free_var(&target),
            ),
        ),
        Term::func(
            [
                (source.clone(), type_0.clone()),
                (target.clone(), type_0.clone()),
            ],
            Term::induct_match_scoped_marked(
                Term::apply(
                    Term::free_var(&Free::from(&boxes_equal_name)),
                    [Term::free_var(&source), Term::free_var(&target)],
                ),
                Scope::close(
                    Many(3),
                    &[&motive_left, &motive_right, &motive_proof],
                    equality(
                        type_0.clone(),
                        unboxed(Term::free_var(&motive_left)),
                        unboxed(Term::free_var(&motive_right)),
                    ),
                ),
                [(
                    "refl",
                    vec![(Plicity::Explicit, arm_value.clone())],
                    reflexivity(type_0.clone(), unboxed(Term::free_var(&arm_value))),
                )],
                None,
            ),
        ),
    );

    // cast : (A : Type 0, B : Type 0, v : A) -> B
    //   = (match types_equal(A, B) : (x, y, _) => (x) -> y | refl(z) => (w) => w end)(v)
    let cast_name = Global::Authored(Qualifier::from(["cast"]));
    let from = Free::local(60, Some("A"));
    let into = Free::local(61, Some("B"));
    let carried = Free::local(62, Some("v"));
    let cast_left = Free::local(63, Some("x"));
    let cast_right = Free::local(64, Some("y"));
    let cast_proof = Free::local(65, Some("q"));
    let cast_value = Free::local(66, Some("z"));
    let coerced = Free::local(67, Some("w"));
    let identity = Free::local(68, Some("w"));
    let cast_params = [
        (from.clone(), type_0.clone()),
        (into.clone(), type_0.clone()),
        (carried.clone(), Term::free_var(&from)),
    ];
    let cast = authored(
        &cast_name,
        Term::func_type(cast_params.clone(), Term::free_var(&into)),
        Term::func(
            cast_params,
            Term::apply(
                Term::induct_match_scoped_marked(
                    Term::apply(
                        Term::free_var(&Free::from(&types_equal_name)),
                        [Term::free_var(&from), Term::free_var(&into)],
                    ),
                    Scope::close(
                        Many(3),
                        &[&cast_left, &cast_right, &cast_proof],
                        Term::func_type(
                            [(coerced, Term::free_var(&cast_left))],
                            Term::free_var(&cast_right),
                        ),
                    ),
                    [(
                        "refl",
                        vec![(Plicity::Explicit, cast_value.clone())],
                        Term::func(
                            [(identity.clone(), Term::free_var(&cast_value))],
                            Term::free_var(&identity),
                        ),
                    )],
                    None,
                ),
                [Term::free_var(&carried)],
            ),
        ),
    );

    // forged : False = cast({}, False, ())
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        false_type.clone(),
        Term::apply(
            Term::free_var(&Free::from(&cast_name)),
            [
                Term::tuple_type_unit(),
                false_type.clone(),
                Term::tuple(Vec::<Term>::new()),
            ],
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![unbox, boxes_equal, types_equal, cast, forged],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([
            (false_name, proposition(Vec::new())),
            (box_name, box_decl),
            (equality_name, equality_decl),
        ]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        // The module as a whole is a closed program of type `False`.
        entry: Some(Entrypoint {
            body: Term::free_var(&Free::from(&forged_name)),
            type_: Some(false_type),
        }),
    }
}

/// The binder a one-member group's scopes close over.
pub(super) fn member() -> Free {
    Free::local(900, Some("f"))
}

/// `let bad : Absurd = <the member selection of `rec f : Absurd = body`>`, with `Absurd` an empty proposition.
pub(super) fn selection_module(body: Term) -> Module {
    let name = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let f = member();

    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(Many(1), &[&f], absurd.clone()),
        body: Scope::close(Many(1), &[&f], body),
    }]);

    Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["bad"])),
            absurd.clone(),
            Term::rec_proj(group, 0),
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(name, proposition(Vec::new()))]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::free_var(&Free::from(&Global::Authored(Qualifier::from(["bad"])))),
            type_: Some(absurd),
        }),
    }
}

/// A one-constructor indexed family whose constructor aims at `target`, carried as a registry entry with no items lowering it.
pub(super) fn indexed_module(target: Term) -> Module {
    let family = Global::Authored(Qualifier::from(["Indexed"]));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        // The family states one index, because its constructor aims at one. Declaring none while a constructor targets one is a malformed declaration in its own right, which the terminal clause now reports as an arity.
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(902, Some("i")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::done(vec![target]),
                plicities: Vec::new(),
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `let held : Type(level) = Nat`, as a whole module.
pub(super) fn level_definition(level: &Level) -> Module {
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::type_at(level.clone()),
        body: Term::intrinsic(Intrinsic::NatType),
    };

    Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// A constructor-free family declared at `Type(level)`, carried as a registry entry.
pub(super) fn level_registry(level: &Level) -> Module {
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        result_sort: Term::type_at(level.clone()),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// A family indexed by the proposition `Held`, whose one constructor states its index either as a diverging `rec` or as `Held/qed()`.
pub(super) fn indexed_by_proof(diverging: bool) -> Module {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let witness = Free::local(900, Some("p"));

    let target = match diverging {
        true => Term::rec(
            [(witness.clone(), held.clone(), Term::free_var(&witness))],
            Term::free_var(&witness),
        ),
        false => Term::variant(
            held_name.clone(),
            Vec::<Term>::new(),
            "qed",
            Vec::<Term>::new(),
        ),
    };

    let family = Global::Authored(Qualifier::from(["Indexed"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(901, Some("i")), held.clone())],
            (),
        )),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::done(vec![target]),
                plicities: Vec::new(),
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let qed = (
        Atom::from("qed"),
        InductParam {
            telescope: Telescope::done(Vec::new()),
            plicities: Vec::new(),
        },
    );

    Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(held_name, proposition(vec![qed])), (family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `Two`, at whatever sort its caller declares it, and `Held(t : Two)` with one constructor targeting `Two/a()`.
pub(super) fn clashing_index_decls(
    carrier_sort: Term,
) -> (Global, Global, BTreeMap<Global, InductDecl>) {
    let two_name = Global::Authored(Qualifier::from(["Two"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let two = Term::induct_type(two_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let nullary = |tag: &str| {
        (
            Atom::from(tag),
            InductParam {
                telescope: Telescope::done(Vec::new()),
                plicities: Vec::new(),
            },
        )
    };

    let two_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![nullary("a"), nullary("b")],
        result_sort: carrier_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let held_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(Free::local(700, Some("t")), two)], ())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::done(vec![Term::variant(
                    two_name.clone(),
                    Vec::<Term>::new(),
                    "a",
                    Vec::<Term>::new(),
                )]),
                plicities: Vec::new(),
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    (
        two_name.clone(),
        held_name.clone(),
        BTreeMap::from([(two_name, two_decl), (held_name, held_decl)]),
    )
}

/// The module the doc comment above describes, with `Two`'s sort written as a redex that reduces to `Prop`.
pub(super) fn aliased_sort_forgery() -> Module {
    let sort = Free::local(701, Some("s"));
    let aliased = Term::apply(
        Term::func([(sort.clone(), Term::type_ground())], Term::free_var(&sort)),
        [Term::prop()],
    );

    let (two_name, held_name, mut decls) = clashing_index_decls(aliased);
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    decls.insert(false_name, proposition(Vec::new()));

    let at_b = Term::induct_type(
        held_name.clone(),
        Vec::<Term>::new(),
        [Term::variant(
            two_name,
            Vec::<Term>::new(),
            "b",
            Vec::<Term>::new(),
        )],
    );

    // held : Held(Two/b()) = Held/mk() — accepted because irrelevance identifies the two index values.
    let held_value = Global::Authored(Qualifier::from(["held"]));
    let held = authored(
        &held_value,
        at_b,
        Term::variant(held_name, Vec::<Term>::new(), "mk", Vec::<Term>::new()),
    );

    // forged : False = match held : (t, s) => False end — no arms, `mk` excused as impossible.
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        false_type.clone(),
        Term::induct_match_scoped_marked(
            Term::free_var(&Free::from(&held_value)),
            Scope::close(
                Many(2),
                &[&Free::local(702, Some("t")), &Free::local(703, Some("h"))],
                false_type.clone(),
            ),
            Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![held, forged],
        universe_seeds: Vec::new(),
        induct_decls: decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::free_var(&Free::from(&forged_name)),
            type_: Some(false_type),
        }),
    }
}

/// The control: the same two families with `Two` at `Type 0`, where `Two/b()` really does clash with `Two/a()`. Nothing inhabits `Held(Two/b())`, so the module proves nothing — what it pins is that the empty elimination stays legal.
pub(super) fn relevant_index_control() -> Module {
    let (two_name, held_name, mut decls) = clashing_index_decls(Term::type_ground());
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    decls.insert(false_name, proposition(Vec::new()));

    let at_b = Term::induct_type(
        held_name,
        Vec::<Term>::new(),
        [Term::variant(
            two_name,
            Vec::<Term>::new(),
            "b",
            Vec::<Term>::new(),
        )],
    );

    // vacuous : (h : Held(Two/b())) -> False = (h) => match h : (t, s) => False end
    let vacuous_name = Global::Authored(Qualifier::from(["vacuous"]));
    let subject = Free::local(710, Some("h"));
    let vacuous = authored(
        &vacuous_name,
        Term::func_type([(subject.clone(), at_b.clone())], false_type.clone()),
        Term::func(
            [(subject.clone(), at_b)],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(
                    Many(2),
                    &[&Free::local(711, Some("t")), &Free::local(712, Some("h"))],
                    false_type,
                ),
                Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
                None,
            ),
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![vacuous],
        universe_seeds: Vec::new(),
        induct_decls: decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `Two : Type 0` with constructors `a` and `b`, `Held(t : Two)` whose two constructors both target `Two/a()` under the given tags, and `vacuous : (x : Held(Two/b())) -> False` eliminating with no arms at all.
///
/// Both constructors target `Two/a()`, so at `Two/b()` every one of them is genuinely impossible and the elimination is legal — *when the tags are distinct*. Giving them the same tag changes nothing about the targets and everything about which entry the coverage rule reads.
pub(super) fn shadowed_constructor(tags: [&str; 2]) -> Module {
    let two_name = Global::Authored(Qualifier::from(["Two"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let two = Term::induct_type(two_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let at = |tag: &str| {
        Term::variant(
            two_name.clone(),
            Vec::<Term>::new(),
            tag,
            Vec::<Term>::new(),
        )
    };
    let nullary = |tag: &str, targets: Vec<Term>| {
        (
            Atom::from(tag),
            InductParam {
                telescope: Telescope::done(targets),
                plicities: Vec::new(),
            },
        )
    };

    let two_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![nullary("a", Vec::new()), nullary("b", Vec::new())],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let held_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(Free::local(800, Some("t")), two)], ())),
        constructors: vec![
            nullary(tags[0], vec![at("a")]),
            nullary(tags[1], vec![at("a")]),
        ],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let at_b = Term::induct_type(held_name, Vec::<Term>::new(), [at("b")]);
    let vacuous_name = Global::Authored(Qualifier::from(["vacuous"]));
    let subject = Free::local(801, Some("x"));
    let vacuous = authored(
        &vacuous_name,
        Term::func_type([(subject.clone(), at_b.clone())], false_type.clone()),
        Term::func(
            [(subject.clone(), at_b)],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(
                    Many(2),
                    &[&Free::local(802, Some("t")), &Free::local(803, Some("s"))],
                    false_type,
                ),
                Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
                None,
            ),
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![vacuous],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([
            (two_name, two_decl),
            (Global::Authored(Qualifier::from(["Held"])), held_decl),
            (false_name, proposition(Vec::new())),
        ]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// [`level_definition`] with a universe scheme of `parameter_count` parameters.
pub(super) fn scheme_definition(level: &Level, parameter_count: usize) -> Module {
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::type_at(level.clone()),
        body: Term::intrinsic(Intrinsic::NatType),
    };

    Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// [`level_registry`] with a universe scheme of `parameter_count` parameters.
pub(super) fn scheme_registry(level: &Level, parameter_count: usize) -> Module {
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        result_sort: Term::type_at(level.clone()),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `let held : Type.{u} = Levelled.{…}`, where `Levelled` declares two universe parameters and the occurrence supplies `width` of them.
pub(super) fn instance_of_width(width: usize) -> Module {
    let family = Global::Authored(Qualifier::from(["Levelled"]));

    // Two parameters and no constraints — the shape `check_instance`'s loop never inspects.
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        result_sort: Term::type_at(Level::param(UniverseParam(1))),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let levels = vec![Level::param(UniverseParam(0)); width];
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::type_at(Level::param(UniverseParam(0))),
        body: Term::induct_type_at(
            family.clone(),
            levels,
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };

    Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `let held : claimed = <a forged host row returning one `Nat`>`, with `False` declared alongside.
pub(super) fn forged_foreign(claimed: &Term, false_name: &Global) -> Module {
    let row = Arc::new(ForeignFunction {
        namespace: Namespace::Ffi,
        name: "/forged".to_string(),
        subject: None,
        label: "forged".to_string(),
        signature: WireSignature {
            params: Vec::new(),
            results: vec![("value".to_string(), WireType::Nat)],
        },
    });

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: claimed.clone(),
        body: Term::foreign(row, Vec::new()),
    };

    Module {
        mounts: Vec::new(),
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(false_name.clone(), proposition(Vec::new()))]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `Foo`'s registry entry and its type-former definition, declaring `registry` and `definition` universe parameters respectively.
pub(super) fn disagreeing_schemes(registry: usize, definition: usize) -> Module {
    let family = Global::Authored(Qualifier::from(["Foo"]));

    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: registry,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        result_sort: Term::type_at(Level::param(UniverseParam(0))),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    // The type-former binding, whose body is the family's own normal form at the registry's width.
    let former = Definition {
        name: family.clone(),
        kind: DefinitionKind::InductiveType,
        universe_context: UniverseContext {
            parameter_count: definition,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::type_at(Level::param(UniverseParam(0))),
        body: Term::induct_type_at(
            family.clone(),
            vec![Level::param(UniverseParam(0)); registry],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };

    Module {
        mounts: Vec::new(),
        items: vec![Item::Let(former)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `extract : (p : P(0)) -> Nat`, eliminating the two-constructor proposition `P` under a motive whose inner switch states `sort` while every arm of it is `Nat`.
pub(super) fn lying_motive(sort: Term) -> Module {
    let family = Global::Authored(Qualifier::from(["P"]));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));

    let nullary = |tag: &str| {
        (
            Atom::from(tag),
            InductParam {
                telescope: Telescope::done(vec![zero.clone()]),
                plicities: Vec::new(),
            },
        )
    };
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(600, Some("i")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        constructors: vec![nullary("mk"), nullary("mk2")],
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let at_zero = Term::induct_type(family.clone(), Vec::<Term>::new(), [zero.clone()]);

    let index = Free::local(601, Some("i"));
    let scrutinee = Free::local(602, Some("s"));
    let motive_body = Term::switch_scoped(
        Term::free_var(&index),
        Scope::close(Many(1), &[&Free::local(603, Some("k"))], sort),
        [(0u32, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );

    let subject = Free::local(604, Some("p"));
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let extract = authored(
        &Global::Authored(Qualifier::from(["extract"])),
        Term::func_type(
            [(subject.clone(), at_zero.clone())],
            Term::intrinsic(Intrinsic::NatType),
        ),
        Term::func(
            [(subject.clone(), at_zero)],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(Many(2), &[&index, &scrutinee], motive_body),
                [
                    ("mk", Vec::new(), literal(7)),
                    ("mk2", Vec::new(), literal(9)),
                ],
                None,
            ),
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![extract],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// The three ways an occurrence of a one-parameter, one-index family can disagree with it.
pub(super) fn arity_cases() -> Vec<(&'static str, Vec<Term>, Vec<Term>)> {
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    vec![
        ("no parameters", Vec::new(), vec![zero.clone()]),
        (
            "no indices",
            vec![Term::intrinsic(Intrinsic::NatType)],
            Vec::new(),
        ),
        (
            "two indices",
            vec![Term::intrinsic(Intrinsic::NatType)],
            vec![zero.clone(), zero],
        ),
    ]
}

/// `let held : Type = F(params)(indices)`, where `F(A : Type) : (i : Nat) -> Type` declares one of each.
pub(super) fn occurrence_module(params: Vec<Term>, indices: Vec<Term>) -> Module {
    let family = Global::Authored(Qualifier::from(["F"]));
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build(
            [(Free::local(950, Some("A")), Term::type_ground())],
            Telescope::build(
                [(
                    Free::local(951, Some("i")),
                    Term::intrinsic(Intrinsic::NatType),
                )],
                (),
            ),
        ),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [(Free::local(952, Some("A")), Term::type_ground())],
                    vec![zero],
                ),
                plicities: vec![Plicity::Implicit],
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::type_ground(),
        Term::induct_type(family.clone(), params, indices),
    );

    Module {
        mounts: Vec::new(),
        items: vec![held],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// The four ways a one-parameter nominal value can disagree with its declaration.
pub(super) fn nominal_value_cases() -> Vec<(&'static str, Module)> {
    let nat = Term::intrinsic(Intrinsic::NatType);
    vec![
        ("struct at no parameters", struct_value_module(Vec::new())),
        (
            "struct at two parameters",
            struct_value_module(vec![nat.clone(), nat.clone()]),
        ),
        ("variant at no parameters", variant_value_module(Vec::new())),
        (
            "variant at two parameters",
            variant_value_module(vec![nat.clone(), nat]),
        ),
    ]
}

/// `struct S(A : Type) : Type { f : A }`, with a literal at `params`.
pub(super) fn struct_value_module(params: Vec<Term>) -> Module {
    let name = Global::Authored(Qualifier::from(["S"]));
    let a = Free::local(980, Some("A"));
    let declaration = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(a.clone(), Term::type_ground())],
            Telescope::build([(Free::local(981, Some("f")), Term::free_var(&a))], ()),
        ),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let declared: Term = Subterm::StructType(StructType {
        name: name.clone(),
        universes: Vec::new(),
        params: vec![Term::intrinsic(Intrinsic::NatType)],
    })
    .into();

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        declared,
        Term::struct_(
            name.clone(),
            params,
            [Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)))],
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![held],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::from([(name, declaration)]),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `induct F(A : Type) : Type | mk(x : A) end`, with a constructor application at `params`.
pub(super) fn variant_value_module(params: Vec<Term>) -> Module {
    let family = Global::Authored(Qualifier::from(["F"]));
    let a = Free::local(970, Some("A"));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(a.clone(), Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [
                        (a.clone(), Term::type_ground()),
                        (Free::local(971, Some("x")), Term::free_var(&a)),
                    ],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Implicit, Plicity::Explicit],
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::induct_type(
            family.clone(),
            [Term::intrinsic(Intrinsic::NatType)],
            Vec::<Term>::new(),
        ),
        Term::variant(
            family.clone(),
            params,
            "mk",
            [Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)))],
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![held],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// The two shapes: an application that under-saturates its lambda, and a neutral spine whose `plicities` are shorter than the arguments applied to it.
pub(super) fn unsaturated_cases() -> Vec<(&'static str, Module)> {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let g = Free::local(992, Some("g"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    let two_binder = |result: Term| {
        Telescope::build([(a.clone(), nat.clone()), (b.clone(), nat.clone())], result)
    };
    let module_of = |items: Vec<Item>| Module {
        mounts: Vec::new(),
        items,
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    };

    // `f : (a : Nat, b : Nat) -> Type`, applied to one argument in a type position.
    let former = Global::Authored(Qualifier::from(["f"]));
    let plicities = vec![Plicity::Explicit, Plicity::Explicit];
    let former_def = authored(
        &former,
        Subterm::FuncType(FuncType::new(
            two_binder(Term::type_ground()),
            plicities.clone(),
        ))
        .into(),
        Subterm::Func(Func::new(two_binder(nat.clone()), plicities)).into(),
    );
    let under_applied = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::apply(Term::free_var(&Free::from(&former)), [three.clone()]),
        three.clone(),
    );

    // `held : (g : (a : Nat, b : Nat) -> Type) -> g(3)`, where `g`'s type carries no plicities at all — a drift the sealed constructor refuses to build, spelled through the test-only verbatim door because archive restoration can still deliver it and the kernel's guard is what this fixture pins.
    let short_plicities: Term = Subterm::FuncType(curios_core::test_support::func_type_verbatim(
        two_binder(Term::type_ground()),
        Vec::new(),
    ))
    .into();
    let neutral = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Subterm::FuncType(FuncType::new(
            Telescope::build(
                [(g.clone(), short_plicities)],
                Term::apply(Term::free_var(&g), [three.clone()]),
            ),
            vec![Plicity::Explicit],
        ))
        .into(),
        three,
    );

    vec![
        (
            "a lambda applied to fewer arguments than it binds",
            module_of(vec![former_def, under_applied]),
        ),
        (
            "a neutral spine whose plicities are shorter than its arguments",
            module_of(vec![neutral]),
        ),
    ]
}

/// The three shapes: an arm over- and under-binding its payload, and a recursive application short of its member's binders.
pub(super) fn unguarded_opener_cases() -> Vec<(&'static str, Module)> {
    vec![
        (
            "an arm binding two components of a one-component payload",
            arm_module(vec![
                (Plicity::Explicit, Free::local(996, Some("a"))),
                (Plicity::Explicit, Free::local(997, Some("b"))),
            ]),
        ),
        (
            "an arm binding none of a one-component payload",
            arm_module(Vec::new()),
        ),
        (
            "a recursive application short of its binders",
            rec_apply_module(),
        ),
    ]
}

/// `held : match F/mk(3) : (s) => Type | mk(<binders>) => Nat end`, for `induct F : Type | mk(x : Nat) end`.
pub(super) fn arm_module(binders: Vec<(Plicity, Free)>) -> Module {
    let family = Global::Authored(Qualifier::from(["F"]));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [(Free::local(995, Some("x")), nat.clone())],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let declared = Term::induct_match_scoped_marked(
        Term::variant(family.clone(), Vec::<Term>::new(), "mk", [three.clone()]),
        Scope::close(
            Many(1),
            &[&Free::local(998, Some("s"))],
            Term::type_ground(),
        ),
        [("mk", binders, nat)],
        None,
    );

    Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["held"])),
            declared,
            three,
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `rec f : (a : Nat, b : Nat) -> Type = (a, b) => Nat; f(3)` as a declared type.
pub(super) fn rec_apply_module() -> Module {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let f = Free::local(992, Some("f"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let plicities = vec![Plicity::Explicit, Plicity::Explicit];

    let member_type: Term = Subterm::FuncType(FuncType::new(
        Telescope::build(
            [(a.clone(), nat.clone()), (b.clone(), nat.clone())],
            Term::type_ground(),
        ),
        plicities.clone(),
    ))
    .into();
    let member_body: Term = Subterm::Func(Func::new(
        Telescope::build([(a.clone(), nat.clone()), (b.clone(), nat.clone())], nat),
        plicities,
    ))
    .into();

    let selection = Term::rec([(f.clone(), member_type, member_body)], Term::free_var(&f));

    Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["held"])),
            Term::apply(selection, [three.clone()]),
            three,
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `sink : (Nat) -> Nat`, honestly stamped `Partial`: its body projects `rec f : (Nat) -> Nat = (n) => f(n)`, which does not descend.
pub(super) fn diverging_sink() -> Item {
    let f = Free::local(920, Some("f"));
    let n = Free::local(921, Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);

    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(
            Many(1),
            &[&f],
            Term::func_type([(n.clone(), nat.clone())], nat.clone()),
        ),
        body: Scope::close(
            Many(1),
            &[&f],
            Term::func(
                [(n.clone(), nat.clone())],
                Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
            ),
        ),
    }]);

    authored_partial(
        &Global::Authored(Qualifier::from(["sink"])),
        Term::func_type([(Free::local(922, Some("n")), nat.clone())], nat),
        Term::rec_proj(group, 0),
    )
}

/// `reaches : (Nat) -> Nat = (m) => sink(m)` at the caller's stamp: partial by closure and nothing else, so a `Total` stamp here is the lie only the transitive comparison sees.
pub(super) fn reaching_definition(totality: Totality) -> Item {
    let m = Free::local(923, Some("m"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let sink = Free::from(&Global::Authored(Qualifier::from(["sink"])));

    let item = authored(
        &Global::Authored(Qualifier::from(["reaches"])),
        Term::func_type([(Free::local(924, Some("m")), nat.clone())], nat.clone()),
        Term::func(
            [(m.clone(), nat)],
            Term::apply(Term::free_var(&sink), [Term::free_var(&m)]),
        ),
    );

    match item {
        Item::Let(definition) => Item::Let(Definition {
            totality,
            ..definition
        }),
        item => item,
    }
}

/// `induct Vouched : Prop | qed(u : {})`, the proposition the proof below inhabits.
pub(super) fn vouched_declaration() -> (Global, InductDecl) {
    let name = Global::Authored(Qualifier::from(["Vouched"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("qed"),
            InductParam {
                telescope: Telescope::build(
                    [(Free::local(925, Some("u")), Term::tuple_type_unit())],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    (name, declaration)
}

/// `held : Vouched = ((g : (Nat) -> Nat) => Vouched/qed(()))(reaches)` — a proof whose free variables name `reaches` and nothing else, so its verdict is exactly the stamp's.
pub(super) fn held_proof() -> Item {
    let (vouched_name, _) = vouched_declaration();
    let vouched = Term::induct_type(vouched_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let g = Free::local(926, Some("g"));
    let nat = Term::intrinsic(Intrinsic::NatType);

    authored(
        &Global::Authored(Qualifier::from(["held"])),
        vouched,
        Term::apply(
            Term::func(
                [(
                    g,
                    Term::func_type([(Free::local(927, Some("n")), nat.clone())], nat),
                )],
                Term::variant(
                    vouched_name,
                    Vec::<Term>::new(),
                    "qed",
                    [Term::tuple(Vec::<Term>::new())],
                ),
            ),
            [Term::free_var(&Free::from(&Global::Authored(
                Qualifier::from(["reaches"]),
            )))],
        ),
    )
}

/// The library whose stamp is on trial — `sink` and `reaches` — with `held` and its proposition beside them when the fixture needs a proof reaching the lie.
pub(super) fn stamp_trial_module(reaches: Totality, with_proof: bool) -> Module {
    let mut items = vec![diverging_sink(), reaching_definition(reaches)];
    let mut induct_decls = BTreeMap::new();

    if with_proof {
        let (vouched_name, declaration) = vouched_declaration();
        induct_decls.insert(vouched_name, declaration);
        items.push(held_proof());
    }

    Module {
        mounts: Vec::new(),
        items,
        universe_seeds: Vec::new(),
        induct_decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// The proof alone, as the compile path shapes it: the library lives in the environment, and only `held` is judged.
pub(super) fn carried_proof_module() -> Module {
    let (vouched_name, declaration) = vouched_declaration();

    Module {
        mounts: Vec::new(),
        items: vec![held_proof()],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(vouched_name, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// `induct Held : Prop | qed(u : {})`, with `bad : Held` built either from `exit(0)` or from `()`.
pub(super) fn proof_carrying_unit(exiting: bool) -> Module {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("qed"),
            InductParam {
                telescope: Telescope::build(
                    [(Free::local(910, Some("u")), Term::tuple_type_unit())],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let payload = match exiting {
        true => Term::intrinsic(Intrinsic::ProcExit(Term::intrinsic(Intrinsic::Nat(
            Nat::new(0usize),
        )))),
        false => Term::tuple(Vec::<Term>::new()),
    };

    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(held_name.clone(), declaration);

    Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["bad"])),
            held,
            Term::variant(held_name, Vec::<Term>::new(), "qed", [payload]),
        )],
        universe_seeds: Vec::new(),
        induct_decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    }
}

/// `induct Held : Type | mk(n : Nat)` with one item building `Held/mk` at `payload_count` arguments, the constructor's plicity vector either honest or empty.
pub(super) fn plicity_module(honest: bool, payload_count: usize) -> Module {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [(
                        Free::local(920, Some("n")),
                        Term::intrinsic(Intrinsic::NatType),
                    )],
                    Vec::new(),
                ),
                plicities: match honest {
                    true => vec![Plicity::Explicit],
                    false => Vec::new(),
                },
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let payload = (0..payload_count)
        .map(|_| Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))))
        .collect::<Vec<_>>();

    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(held_name.clone(), declaration);

    Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["value"])),
            held,
            Term::variant(held_name, Vec::<Term>::new(), "mk", payload),
        )],
        universe_seeds: Vec::new(),
        induct_decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    }
}

/// `Held : (n : Nat) -> Type 0 | yes() : (0)`, the family whose two instances the forged equation coerces between.
pub(super) fn indexed_family(
    index: Free,
    nat_type: Term,
    zero: Term,
    result_sort: Term,
) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(index, nat_type)], ())),
        constructors: vec![(
            Atom::from("yes"),
            InductParam {
                telescope: Telescope::done(vec![zero]),
                plicities: Vec::new(),
            },
        )],
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// The module the doc comment below describes: four declarations, and the four definitions that close on `False`.
pub(super) fn index_forgery() -> Module {
    let type_0 = Term::type_ground();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let true_name = Global::Authored(Qualifier::from(["True"]));
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));

    let true_type = Term::induct_type(true_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(
        true_name.clone(),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    );
    let held_at = |index: Term| Term::induct_type(held_name.clone(), Vec::<Term>::new(), [index]);
    let yes = Term::variant(
        held_name.clone(),
        Vec::<Term>::new(),
        "yes",
        Vec::<Term>::new(),
    );
    let equality = |carrier: Term, left: Term, right: Term| {
        Term::induct_type(equality_name.clone(), [carrier], [left, right])
    };
    let reflexivity = |carrier: Term, value: Term| {
        Term::variant(equality_name.clone(), [carrier], "refl", [value])
    };

    // induct True : Prop | qed() end, and the empty induct False : Prop end
    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam {
            telescope: Telescope::done(Vec::new()),
            plicities: Vec::new(),
        },
    )]);
    let false_decl = proposition(Vec::new());

    let equality_decl = equality_declaration();

    let held_decl = indexed_family(
        Free::local(70, Some("n")),
        nat_type.clone(),
        nat(0),
        type_0.clone(),
    );

    // forged : Eq(True, 0, 1) = refl(True, qed())
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        equality(true_type.clone(), nat(0), nat(1)),
        reflexivity(true_type.clone(), qed),
    );

    // cast : (Held(0)) -> Held(1)
    //   = match forged : (s, t, q) => (Held(s)) -> Held(t) | refl(z) => (w) => w end
    let cast_name = Global::Authored(Qualifier::from(["cast"]));
    let motive_left = Free::local(80, Some("s"));
    let motive_right = Free::local(81, Some("t"));
    let motive_proof = Free::local(82, Some("q"));
    let arm_value = Free::local(83, Some("z"));
    let carried = Free::local(84, Some("w"));
    let identity = Free::local(85, Some("w"));
    let cast = authored(
        &cast_name,
        Term::func_type([(carried.clone(), held_at(nat(0)))], held_at(nat(1))),
        Term::induct_match_scoped_marked(
            Term::free_var(&Free::from(&forged_name)),
            Scope::close(
                Many(3),
                &[&motive_left, &motive_right, &motive_proof],
                Term::func_type(
                    [(carried, held_at(Term::free_var(&motive_left)))],
                    held_at(Term::free_var(&motive_right)),
                ),
            ),
            [(
                "refl",
                vec![(Plicity::Explicit, arm_value.clone())],
                Term::func(
                    [(identity.clone(), held_at(Term::free_var(&arm_value)))],
                    Term::free_var(&identity),
                ),
            )],
            None,
        ),
    );

    // held : Held(1) = cast(yes())
    let held_value_name = Global::Authored(Qualifier::from(["held"]));
    let held_value = authored(
        &held_value_name,
        held_at(nat(1)),
        Term::apply(Term::free_var(&Free::from(&cast_name)), [yes]),
    );

    // boom : False = match held : (n, w) => False end
    let boom_name = Global::Authored(Qualifier::from(["boom"]));
    let boom_index = Free::local(90, Some("n"));
    let boom_scrutinee = Free::local(91, Some("w"));
    let boom = authored(
        &boom_name,
        false_type.clone(),
        Term::induct_match_scoped_marked(
            Term::free_var(&Free::from(&held_value_name)),
            Scope::close(Many(2), &[&boom_index, &boom_scrutinee], false_type.clone()),
            Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![forged, cast, held_value, boom],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([
            (true_name, true_decl),
            (false_name, false_decl),
            (equality_name, equality_decl),
            (held_name, held_decl),
        ]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    }
}

/// A stuck `match` on `scrutinee` whose motive claims `Prop` while both arms inhabit `Nat` — the one shape in Core that states its own sort instead of having one derived.
pub(super) fn lying_type(scrutinee: &Free) -> Term {
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    Term::bool_match(
        Term::free_var(scrutinee),
        None,
        Term::prop(),
        nat_type.clone(),
        nat_type,
    )
}

/// One module per position at which a term stands as a *type*, each carrying [`lying_motive`] at that position and nothing else wrong.
pub(super) fn lying_type_positions() -> Vec<(&'static str, Module)> {
    let boolean = Term::intrinsic(Intrinsic::BoolType);
    let unit_type = Term::tuple_type_unit();
    let unit = Term::tuple(Vec::<Term>::new());
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    let b = Free::local(300, Some("b"));
    let x = Free::local(301, Some("x"));
    let y = Free::local(302, Some("y"));
    let lie = lying_type(&b);

    let probe_module = |body: Term, type_: Term| Module {
        mounts: Vec::new(),
        items: vec![authored(
            &Global::Authored(Qualifier::from(["probe"])),
            type_,
            body,
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    };

    vec![
        // probe : (b : Bool, x : <lie>) -> {} = (b, x) => ()
        (
            "a function type's domain",
            probe_module(
                Term::func(
                    [(b.clone(), boolean.clone()), (x.clone(), lie.clone())],
                    unit.clone(),
                ),
                Term::func_type(
                    [(b.clone(), boolean.clone()), (x.clone(), lie.clone())],
                    unit_type.clone(),
                ),
            ),
        ),
        // probe : (b : Bool) -> <lie> = (b) => 0
        (
            "a function type's codomain",
            probe_module(
                Term::func([(b.clone(), boolean.clone())], zero.clone()),
                Term::func_type([(b.clone(), boolean.clone())], lie.clone()),
            ),
        ),
        // probe : (b : Bool) -> {<lie>} = (b) => (0)
        (
            "a tuple type's component",
            probe_module(
                Term::func([(b.clone(), boolean.clone())], Term::tuple([zero.clone()])),
                Term::func_type(
                    [(b.clone(), boolean.clone())],
                    Term::tuple_type(vec![(y.clone(), lie.clone())]),
                ),
            ),
        ),
        // probe : {} = ((b : Bool, x : <lie>) => ())(true, 0)
        (
            "a lambda's domain annotation",
            probe_module(
                Term::apply(
                    Term::func(
                        [(b.clone(), boolean.clone()), (x.clone(), lie.clone())],
                        unit.clone(),
                    ),
                    [truth.clone(), zero.clone()],
                ),
                unit_type.clone(),
            ),
        ),
        // probe : (b : Bool) -> {} = (b) => let y : <lie> = 0; ()
        (
            "a let binding's declared type",
            probe_module(
                Term::func(
                    [(b.clone(), boolean.clone())],
                    Term::let_(&y, lie.clone(), zero.clone(), unit.clone()),
                ),
                Term::func_type([(b.clone(), boolean.clone())], unit_type.clone()),
            ),
        ),
        // probe : (b : Bool) -> {} = (b) => (rec y : <lie> = 0; ())
        (
            "a rec member's declared type",
            probe_module(
                Term::func(
                    [(b.clone(), boolean.clone())],
                    Term::rec([(y.clone(), lie.clone(), zero.clone())], unit.clone()),
                ),
                Term::func_type([(b.clone(), boolean)], unit_type),
            ),
        ),
    ]
}

/// `struct Wrap(b : Bool) : Prop { held : match b : Prop | false => .. | true => .. end }` — a structure whose field type is a stuck `match` claiming, through its motive, to be a proposition.
pub(super) fn computed_field_wrapper(false_case: Term, true_case: Term) -> StructDecl {
    let scrutinee = Free::local(60, Some("b"));
    let field = Term::bool_match(
        Term::free_var(&scrutinee),
        None,
        Term::prop(),
        false_case,
        true_case,
    );

    StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(scrutinee, Term::intrinsic(Intrinsic::BoolType))],
            Telescope::build([(Free::local(61, Some("held")), field)], ()),
        ),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// `Wrap(true)`, the instance at which the field type above reduces to its true arm.
pub(super) fn wrap_at_true(wrap_name: &Global) -> Term {
    Subterm::StructType(StructType {
        name: wrap_name.clone(),
        universes: Vec::new(),
        params: vec![Term::intrinsic(Intrinsic::Bool(true))],
    })
    .into()
}

/// `wrapped : Wrap(true) = Wrap(true){held}`.
pub(super) fn wrapped_at_true(wrap_name: &Global, held: Term) -> Item {
    authored(
        &Global::Authored(Qualifier::from(["wrapped"])),
        wrap_at_true(wrap_name),
        Term::struct_(
            wrap_name.clone(),
            [Term::intrinsic(Intrinsic::Bool(true))],
            [held],
        ),
    )
}

/// [`index_forgery`]'s transport, reached through a `Prop`-sorted structure that really carries a `Nat`.
pub(super) fn computed_field_forgery() -> Module {
    let type_0 = Term::type_ground();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));

    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let held_at = |index: Term| Term::induct_type(held_name.clone(), Vec::<Term>::new(), [index]);
    let yes = Term::variant(
        held_name.clone(),
        Vec::<Term>::new(),
        "yes",
        Vec::<Term>::new(),
    );
    let wrap = wrap_at_true(&wrap_name);
    let wrapping = |held: Term| {
        Term::struct_(
            wrap_name.clone(),
            [Term::intrinsic(Intrinsic::Bool(true))],
            [held],
        )
    };

    let false_decl = proposition(Vec::new());
    let held_decl = indexed_family(
        Free::local(70, Some("n")),
        nat_type.clone(),
        nat(0),
        type_0.clone(),
    );

    // forged : Eq(Wrap(true), Wrap(true){0}, Wrap(true){1}) = refl(Wrap(true), Wrap(true){0})
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        Term::induct_type(
            equality_name.clone(),
            [wrap.clone()],
            [wrapping(nat(0)), wrapping(nat(1))],
        ),
        Term::variant(equality_name.clone(), [wrap], "refl", [wrapping(nat(0))]),
    );

    // cast : (Held(0)) -> Held(1)
    //   = match forged : (s, t, q) => (Held(s.0)) -> Held(t.0) | refl(z) => (w) => w end
    let cast_name = Global::Authored(Qualifier::from(["cast"]));
    let motive_left = Free::local(80, Some("s"));
    let motive_right = Free::local(81, Some("t"));
    let motive_proof = Free::local(82, Some("q"));
    let arm_value = Free::local(83, Some("z"));
    let carried = Free::local(84, Some("w"));
    let identity = Free::local(85, Some("w"));
    let cast = authored(
        &cast_name,
        Term::func_type([(carried.clone(), held_at(nat(0)))], held_at(nat(1))),
        Term::induct_match_scoped_marked(
            Term::free_var(&Free::from(&forged_name)),
            Scope::close(
                Many(3),
                &[&motive_left, &motive_right, &motive_proof],
                Term::func_type(
                    [(
                        carried,
                        held_at(Term::proj(Term::free_var(&motive_left), 0)),
                    )],
                    held_at(Term::proj(Term::free_var(&motive_right), 0)),
                ),
            ),
            [(
                "refl",
                vec![(Plicity::Explicit, arm_value.clone())],
                Term::func(
                    [(
                        identity.clone(),
                        held_at(Term::proj(Term::free_var(&arm_value), 0)),
                    )],
                    Term::free_var(&identity),
                ),
            )],
            None,
        ),
    );

    // held : Held(1) = cast(yes())
    let held_value_name = Global::Authored(Qualifier::from(["held"]));
    let held_value = authored(
        &held_value_name,
        held_at(nat(1)),
        Term::apply(Term::free_var(&Free::from(&cast_name)), [yes]),
    );

    // boom : False = match held : (n, w) => False end
    let boom_index = Free::local(90, Some("n"));
    let boom_scrutinee = Free::local(91, Some("w"));
    let boom = authored(
        &Global::Authored(Qualifier::from(["boom"])),
        false_type.clone(),
        Term::induct_match_scoped_marked(
            Term::free_var(&Free::from(&held_value_name)),
            Scope::close(Many(2), &[&boom_index, &boom_scrutinee], false_type.clone()),
            Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    Module {
        mounts: Vec::new(),
        items: vec![forged, cast, held_value, boom],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([
            (false_name, false_decl),
            (equality_name, equality_declaration()),
            (held_name, held_decl),
        ]),
        struct_decls: BTreeMap::from([(
            wrap_name,
            computed_field_wrapper(nat_type.clone(), nat_type),
        )]),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    }
}

/// The scheme both universe-occurrence fixtures below are built on: `def A<u, v | u + 1 <= v> : Type v = Type u`.
///
/// Its constraint is load-bearing rather than decorative, which is the whole point of the shape: checking the body needs `Type (u + 1) <= Type v`, and that holds under this hypothesis and under nothing else. So an occurrence of `A` is legitimate exactly where it discharges the constraint at its own levels, and [`Kernel::check_instance`](crate::Kernel) is what does that.
pub(super) fn scheme_context() -> UniverseContext {
    let ordered = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .checked_add(1)
            .expect("level admits the offset"),
        upper: Level::param(UniverseParam(1)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };

    UniverseContext {
        parameter_count: 2,
        constraints: vec![ordered],
    }
}

/// The same two parameters with the scheme's constraint *not* declared — the context a user of `A` stands in when it has assumed nothing about its own levels.
pub(super) fn open_context() -> UniverseContext {
    UniverseContext {
        parameter_count: 2,
        constraints: Vec::new(),
    }
}

/// The scheme above, followed by one `user` that spells an occurrence of it.
///
/// `user` is `(its own universe context, the term it is defined as)`, and its declared type is `Type v` throughout — so the two spellings of the occurrence differ in nothing but whether the instance is stated.
pub(super) fn universe_scheme_module(user: Option<(UniverseContext, Term)>) -> Module {
    let scheme = Definition {
        name: Global::Authored(Qualifier::from(["A"])),
        kind: DefinitionKind::Authored,
        universe_context: scheme_context(),
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::type_at(Level::param(UniverseParam(0))),
    };

    let mut items = vec![Item::Let(scheme)];
    if let Some((universe_context, body)) = user {
        items.push(Item::Let(Definition {
            name: Global::Authored(Qualifier::from(["user"])),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            totality: Totality::Total,
            type_: Term::type_at(Level::param(UniverseParam(1))),
            body,
        }));
    }

    Module {
        mounts: Vec::new(),
        items,
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    }
}

/// `A` alone, and `A` at the using item's own two parameters — the two spellings of one occurrence.
pub(super) fn scheme_occurrences() -> (Term, Term) {
    let name = Free::from(&Global::Authored(Qualifier::from(["A"])));
    let bare = Term::free_var(&name);
    let instance = Term::instance_of(
        &name,
        vec![
            Level::param(UniverseParam(0)),
            Level::param(UniverseParam(1)),
        ],
    );

    (bare, instance)
}

/// How `coerce`'s body below reaches its declared result type.
pub(super) enum Route {
    /// Directly, with no arm open — so no case equation is in scope and conversion answers about the two indices honestly.
    Direct,
    /// Through a `match` on `f<0>(x)` under a motive constant at the declared result, so the arm's case equation is the only thing the body's conversion can be consulting.
    ConstantMotive,
    /// Through a `match` on `f<0>(x)` under the dependent motive `(s) => Q(s)`, where the equation is *load-bearing*: the arm body has type `Q(f<0>(x))` and must check at `Q(wrap(T))`, which nothing but the equation can bridge.
    DependentMotive,
}

/// A stuck scrutinee whose two spellings differ in nothing but a universe level, and a coercion between the two types they index.
///
/// `induct E : Type 3 | wrap(T : Type 2) end` is an ordinary `Type`-sorted family whose payload is a universe, `induct Q : (e : E) -> Type 0` has no constructors and exists only to be indexed by one, and `f<u | u + 1 <= 2> : (x : Nat) -> E = (x) => match x | 0 => wrap(Type u) | _ => wrap(Type 0) end` mentions its universe parameter in a *payload* position. Both `f<0>` and `f<1>` discharge the constraint, and `f<u>(x)` sticks on the local `x` — so `f<0>(x)` and `f<1>(x)` are two stuck terms that differ only in a level and whose reducts genuinely differ, which is the pair the key must keep apart.
///
/// The item under test is `coerce : (x : Nat, q : Q(f<0>(x))) -> Q(f<target>(x))`.
pub(super) fn universe_refinement_module(target: Level, route: Route) -> Module {
    let one = Level::zero().succ().expect("level zero has a successor");
    let two = one.clone().succ().expect("level one has a successor");
    let three = two.clone().succ().expect("level two has a successor");

    let e_name = Global::Authored(Qualifier::from(["E"]));
    let q_name = Global::Authored(Qualifier::from(["Q"]));
    let f_name = Global::Authored(Qualifier::from(["f"]));

    let e_type = Term::induct_type(e_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let e_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("wrap"),
            InductParam {
                telescope: Telescope::build(
                    [(Free::local(610, Some("T")), Term::type_at(two.clone()))],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort: Term::type_at(three),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let q_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(611, Some("e")), e_type.clone())],
            (),
        )),
        constructors: Vec::new(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };

    let bounded = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .succ()
            .expect("level has a successor"),
        upper: two,
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };

    let wrap_at = |level: Level| {
        Term::variant(
            e_name.clone(),
            Vec::<Term>::new(),
            "wrap",
            [Term::type_at(level)],
        )
    };

    let x = Free::local(620, Some("x"));
    let s = Free::local(621, Some("s"));
    let f_item = Item::Let(Definition {
        name: f_name.clone(),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            constraints: vec![bounded],
        },
        island: Qualifier::default(),
        totality: Totality::Total,
        type_: Term::func_type(
            [(
                Free::local(622, Some("x")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            e_type.clone(),
        ),
        body: Term::func(
            [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
            Term::switch_scoped(
                Term::free_var(&x),
                Scope::close(Many(1), &[&s], e_type.clone()),
                [(0u32, wrap_at(Level::param(UniverseParam(0))))],
                wrap_at(Level::zero()),
            ),
        ),
    });

    let f_at = |level: Level, arg: &Free| {
        Term::apply(
            Term::instance_of(&Free::from(&f_name), vec![level]),
            [Term::free_var(arg)],
        )
    };
    let q_at = |index: Term| Term::induct_type(q_name.clone(), Vec::<Term>::new(), [index]);

    let cx = Free::local(630, Some("x"));
    let cq = Free::local(631, Some("q"));
    let cs = Free::local(632, Some("s"));
    let ct = Free::local(633, Some("T"));

    let declared = q_at(f_at(target, &cx));
    let arm = |motive: Term| {
        Term::induct_match_scoped_marked(
            f_at(Level::zero(), &cx),
            Scope::close(Many(1), &[&cs], motive),
            [(
                "wrap",
                vec![(Plicity::Explicit, ct.clone())],
                Term::free_var(&cq),
            )],
            None,
        )
    };
    let body = match route {
        Route::Direct => Term::free_var(&cq),
        Route::ConstantMotive => arm(declared.clone()),
        Route::DependentMotive => arm(q_at(Term::free_var(&cs))),
    };

    let parameters = [
        (cx.clone(), Term::intrinsic(Intrinsic::NatType)),
        (cq.clone(), q_at(f_at(Level::zero(), &cx))),
    ];
    let coerce = authored(
        &Global::Authored(Qualifier::from(["coerce"])),
        Term::func_type(parameters.clone(), declared),
        Term::func(parameters, body),
    );

    Module {
        mounts: Vec::new(),
        items: vec![f_item, coerce],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(e_name, e_decl), (q_name, q_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::intrinsic(Intrinsic::NatType),
            type_: None,
        }),
    }
}

/// The environment a walk is handed: everything `module` puts in scope, at the floor its own terms derive.
pub(super) fn already_judged(module: &Module) -> Globals {
    Globals::of(module, derived_binder_floor(module))
}

/// A module carrying `items` and `induct_decls` and nothing else.
pub(super) fn collision_module(
    items: Vec<Item>,
    induct_decls: BTreeMap<Global, InductDecl>,
) -> Module {
    Module {
        mounts: Vec::new(),
        items,
        universe_seeds: Vec::new(),
        induct_decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        entry: Some(Entrypoint {
            body: Term::tuple(Vec::<Term>::new()),
            type_: None,
        }),
    }
}

/// The definition name the environment and the module both declare.
pub(super) fn shadowed_name() -> Global {
    Global::Authored(Qualifier::from(["shadowed"]))
}

/// The name judged in both configurations, whose body is the only thing that reveals which `shadowed` the walk used.
pub(super) fn reader_name() -> Global {
    Global::Authored(Qualifier::from(["reader"]))
}

/// The family name the environment and the module both declare.
pub(super) fn shadowed_family() -> Global {
    Global::Authored(Qualifier::from(["Shadowed"]))
}

/// `let shadowed : Nat = 0` beside `induct Shadowed : Type 0 end` — the two entries an earlier walk is taken to have judged.
pub(super) fn judged_environment() -> Module {
    collision_module(
        vec![authored(
            &shadowed_name(),
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        )],
        BTreeMap::from([(
            shadowed_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )]),
    )
}

/// `let shadowed : Bool = true` beside `let reader : Nat = shadowed`.
pub(super) fn shadowing_items() -> Module {
    collision_module(
        vec![
            authored(
                &shadowed_name(),
                Term::intrinsic(Intrinsic::BoolType),
                Term::intrinsic(Intrinsic::Bool(true)),
            ),
            authored(
                &reader_name(),
                Term::intrinsic(Intrinsic::NatType),
                Term::free_var(&Free::from(&shadowed_name())),
            ),
        ],
        BTreeMap::new(),
    )
}

/// `induct Shadowed : Type 0 | mk(a : payload) end`, carried as a registry entry with no items lowering it.
pub(super) fn shadowing_registry(payload: Term) -> Module {
    collision_module(
        Vec::new(),
        BTreeMap::from([(
            shadowed_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: vec![(
                    Atom::from("mk"),
                    InductParam {
                        telescope: Telescope::build(
                            [(Free::local(910, Some("a")), payload)],
                            Vec::new(),
                        ),
                        plicities: vec![Plicity::Explicit],
                    },
                )],
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
            },
        )]),
    )
}

/// The whole-module walk over a bare fixture, the evidence wrapper bypassed: the modules built here are adversarial by design — some deliberately carry what `Zonked` refuses — and the kernel's own refusals are the thing under test, so nothing may stand between a forged module and the walk.
pub(super) fn fixture_verdicts(
    module: &Module,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    super::verdicts_from(Kernel::new(budget, syntax), module, globals)
}
