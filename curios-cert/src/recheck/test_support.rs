//! The hand-built adversarial modules the recheck suites forge, and the declarations they forge them from.
//!
//! Built by hand because a refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program. Reaching it means constructing the finished module here and asking `recheck_definitions_verdicts` directly.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `recheck`, and nothing outside it.

use {
    crate::*,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_analysis::test_support::SYNTAX,
    curios_common::{Candidates, Plicity, Qualifier, SyntaxRegistry},
    curios_core::{
        Atom, Certification, Certified, Definition, DefinitionKind, Definitions, Entrypoint, Free,
        Func, FuncType, Global, InductDecl, InductParam, Intrinsic, Item, Level, Many, Nat,
        Nominal, Places, Polarity, Program, Reads, RecGroup, RecItem, Scope, StructDecl,
        StructType, Subterm, Telescope, Term, UniverseConstraint, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseParam, Universes, Variance,
        test_support::*,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A top-level definition, as `recheck_definitions_verdicts` binds one.
pub(super) fn authored(name: &Global, type_: Term, body: Term) -> Item {
    Item::Let(Box::new(Definition {
        name: *name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_,
        body,
    }))
}

/// [`authored`] marked a proof, as the module a walk hands back marks one its declaration wrote at a proposition: what a unit in scope holds.
pub(super) fn authored_proof(name: &Global, type_: Term, body: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            proof: true,
            ..*definition
        })),
        item => item,
    }
}

/// A nullary `Prop`-sorted family: `False` itself, and the shape `Box` takes but for its payload.
pub(super) fn proposition(constructors: Vec<(Atom, InductParam)>) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: constructors.into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// `induct Eq(@A : Type 1) : (x : A, y : A) -> Prop 1 | refl(z : A) : (z, z) end` — the equality the forgeries transport along, a proposition at its carrier's level, where a value of the carrier fits.
pub(super) fn equality_declaration() -> InductDecl {
    let type_1 = Term::type_at(Level::zero().succ().expect("level zero has a successor"));
    let carrier = Free::local(20, Some("A"));
    let left = Free::local(21, Some("x"));
    let right = Free::local(22, Some("y"));
    let value = Free::local(23, Some("z"));

    let mut declaration = proposition(vec![(
        Atom::from("refl"),
        InductParam::new(
            Telescope::build(
                [(carrier, type_1.clone()), (value, Term::free_var(&carrier))],
                vec![Term::free_var(&value), Term::free_var(&value)],
            ),
            vec![Plicity::Implicit, Plicity::Explicit],
        ),
    )]);
    declaration.result_sort =
        Term::prop_at(Level::zero().succ().expect("level zero has a successor"));
    declaration.arity = Telescope::build(
        [(carrier, type_1.clone())],
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
pub(super) fn forgery() -> Program {
    let type_0 = Term::type_ground();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let box_name = Global::Authored(Qualifier::from(["Box"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));

    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let box_type = Term::induct_type(box_name, Vec::<Term>::new(), Vec::<Term>::new());

    let boxed = |carried: Term| Term::variant(box_name, Vec::<Term>::new(), "mk", [carried]);
    let equality = |carrier: Term, left: Term, right: Term| {
        Term::induct_type(equality_name, [carrier], [left, right])
    };
    let reflexivity =
        |carrier: Term, value: Term| Term::variant(equality_name, [carrier], "refl", [value]);

    // induct Box : Prop 1 | mk(a : Type 0) end — at the level its payload fits under, so the declaration stands and the guard is what refuses `unbox`.
    let payload = Free::local(10, Some("a"));
    let mut box_decl = proposition(vec![(
        Atom::from("mk"),
        InductParam::new(
            Telescope::build([(payload, type_0.clone())], Vec::new()),
            vec![Plicity::Explicit],
        ),
    )]);
    box_decl.result_sort = Term::prop_at(Level::zero().succ().expect("level zero has a successor"));

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
            [(subject, box_type.clone())],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(Many(1), &[&scrutinee], type_0.clone()),
                [(
                    "mk",
                    vec![(Plicity::Explicit, opened)],
                    Term::free_var(&opened),
                )],
                None,
            ),
        ),
    );
    let unboxed =
        |carried: Term| Term::apply(Term::instance(unbox_name, Universes::default()), [carried]);

    // boxes_equal : (A : Type 0, B : Type 0) -> Eq(Box, mk(A), mk(B)) = refl(mk(A))
    let boxes_equal_name = Global::Authored(Qualifier::from(["boxes_equal"]));
    let this = Free::local(40, Some("A"));
    let that = Free::local(41, Some("B"));
    let boxes_equal = authored(
        &boxes_equal_name,
        Term::func_type(
            [(this, type_0.clone()), (that, type_0.clone())],
            equality(
                box_type.clone(),
                boxed(Term::free_var(&this)),
                boxed(Term::free_var(&that)),
            ),
        ),
        Term::func(
            [(this, type_0.clone()), (that, type_0.clone())],
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
            [(source, type_0.clone()), (target, type_0.clone())],
            equality(
                type_0.clone(),
                Term::free_var(&source),
                Term::free_var(&target),
            ),
        ),
        Term::func(
            [(source, type_0.clone()), (target, type_0.clone())],
            Term::induct_match_scoped_marked(
                Term::apply(
                    Term::instance(boxes_equal_name, Universes::default()),
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
                    vec![(Plicity::Explicit, arm_value)],
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
        (from, type_0.clone()),
        (into, type_0.clone()),
        (carried, Term::free_var(&from)),
    ];
    let cast = authored(
        &cast_name,
        Term::func_type(cast_params.clone(), Term::free_var(&into)),
        Term::func(
            cast_params,
            Term::apply(
                Term::induct_match_scoped_marked(
                    Term::apply(
                        Term::instance(types_equal_name, Universes::default()),
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
                        vec![(Plicity::Explicit, cast_value)],
                        Term::func(
                            [(identity, Term::free_var(&cast_value))],
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
            Term::instance(cast_name, Universes::default()),
            [
                Term::tuple_type_unit(),
                false_type.clone(),
                Term::tuple(Vec::<Term>::new()),
            ],
        ),
    );

    Program {
        definitions: declaring(
            Definitions {
                items: vec![unbox, boxes_equal, types_equal, cast, forged].into(),
            },
            BTreeMap::from([
                (false_name, proposition(Vec::new())),
                (box_name, box_decl),
                (equality_name, equality_decl),
            ]),
            [],
            [],
        ),
        // The program as a whole is closed, at type `False`.
        entrypoint: Entrypoint {
            body: Term::instance(forged_name, Universes::default()),
            type_: Some(false_type),
        },
    }
}

/// The binder a one-member group's scopes close over.
pub(super) fn member() -> Free {
    Free::local(900, Some("f"))
}

/// `let bad : Absurd = <the member selection of `rec f : Absurd = body`>`, with `Absurd` an empty proposition.
pub(super) fn selection_module(body: Term) -> Program {
    let name = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let f = member();

    let group = RecGroup::closing(vec![(f, absurd.clone(), body)]);

    Program {
        definitions: declaring(
            Definitions {
                items: vec![authored(
                    &Global::Authored(Qualifier::from(["bad"])),
                    absurd.clone(),
                    Term::rec_proj(group, 0),
                )]
                .into(),
            },
            BTreeMap::from([(name, proposition(Vec::new()))]),
            [],
            [],
        ),
        entrypoint: Entrypoint {
            body: Term::instance(
                Global::Authored(Qualifier::from(["bad"])),
                Universes::default(),
            ),
            type_: Some(absurd),
        },
    }
}

/// A one-constructor indexed family whose constructor aims at `target`, carried as a registry entry with no items lowering it.
pub(super) fn indexed_definitions(target: Term) -> Definitions {
    let family = Global::Authored(Qualifier::from(["Indexed"]));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        // The family states one index, because its constructor aims at one. Declaring none while a constructor targets one is a malformed declaration in its own right, which the terminal clause reports as an arity.
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(902, Some("i")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(Telescope::done(vec![target]), Vec::new()),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            items: Vec::new().into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// `let held : Type(level) = Nat`, as a whole module.
pub(super) fn level_definition(level: &Level) -> Definitions {
    sorted_definition(Term::type_at(level.clone()))
}

/// `let held : sort = Nat`, as a whole module.
pub(super) fn sorted_definition(sort: Term) -> Definitions {
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: sort,
        body: Term::intrinsic(Intrinsic::NatType),
    };

    Definitions {
        items: vec![Item::Let(Box::new(definition))].into(),
    }
}

/// A constructor-free family declared at `Type(level)`, carried as a registry entry.
pub(super) fn level_registry(level: &Level) -> Definitions {
    sorted_registry(Term::type_at(level.clone()))
}

/// A constructor-free family declared at `sort`, carried as a registry entry.
pub(super) fn sorted_registry(sort: Term) -> Definitions {
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            items: Vec::new().into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// A family indexed by the proposition `Held`, whose one constructor states its index either as a diverging `rec` or as `Held/qed()`.
pub(super) fn indexed_by_proof(diverging: bool) -> Definitions {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name, Vec::<Term>::new(), Vec::<Term>::new());
    let witness = Free::local(900, Some("p"));

    let target = match diverging {
        true => Term::rec(
            [(witness, held.clone(), Term::free_var(&witness))],
            Term::free_var(&witness),
        ),
        false => Term::variant(held_name, Vec::<Term>::new(), "qed", Vec::<Term>::new()),
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
            InductParam::new(Telescope::done(vec![target]), Vec::new()),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let qed = (
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    );

    declaring(
        Definitions {
            items: Vec::new().into(),
        },
        BTreeMap::from([(held_name, proposition(vec![qed])), (family, declaration)]),
        [],
        [],
    )
}

/// `Two`, at whatever sort its caller declares it, and `Held(t : Two)` with one constructor targeting `Two/a()`.
pub(super) fn clashing_index_decls(
    carrier_sort: Term,
) -> (Global, Global, BTreeMap<Global, InductDecl>) {
    let two_name = Global::Authored(Qualifier::from(["Two"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let two = Term::induct_type(two_name, Vec::<Term>::new(), Vec::<Term>::new());

    let nullary = |tag: &str| {
        (
            Atom::from(tag),
            InductParam::new(Telescope::done(Vec::new()), Vec::new()),
        )
    };

    let two_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![nullary("a"), nullary("b")].into(),
        result_sort: carrier_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let held_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(Free::local(700, Some("t")), two)], ())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::done(vec![Term::variant(
                    two_name,
                    Vec::<Term>::new(),
                    "a",
                    Vec::<Term>::new(),
                )]),
                Vec::new(),
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    (
        two_name,
        held_name,
        BTreeMap::from([(two_name, two_decl), (held_name, held_decl)]),
    )
}

/// The module the doc comment above describes, with `Two`'s sort written as a redex that reduces to `Prop`.
pub(super) fn aliased_sort_forgery() -> Program {
    let sort = Free::local(701, Some("s"));
    let aliased = Term::apply(
        Term::func([(sort, Term::type_ground())], Term::free_var(&sort)),
        [Term::prop()],
    );

    clashing_index_forgery(aliased)
}

/// `held : Held(Two/b()) = Held/mk()` and `forged : False`, a match on it with no arm, `Two` declared at `carrier_sort`.
pub(super) fn clashing_index_forgery(carrier_sort: Term) -> Program {
    let (two_name, held_name, mut decls) = clashing_index_decls(carrier_sort);
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
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
            Term::instance(held_value, Universes::default()),
            Scope::close(
                Many(2),
                &[&Free::local(702, Some("t")), &Free::local(703, Some("h"))],
                false_type.clone(),
            ),
            Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    Program {
        definitions: declaring(
            Definitions {
                items: vec![held, forged].into(),
            },
            decls,
            [],
            [],
        ),
        entrypoint: Entrypoint {
            body: Term::instance(forged_name, Universes::default()),
            type_: Some(false_type),
        },
    }
}

/// `induct Pin : (n : Nat) -> Prop | yes() : (0) | no() : (1) end`, and a nullary proposition `Held` with one proof, for the sole-arm fixtures below.
fn pin_decls() -> (Global, Global, BTreeMap<Global, InductDecl>) {
    let pin_name = Global::Authored(Qualifier::from(["Pin"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let targeting = |tag: &str, index: usize| {
        (
            Atom::from(tag),
            InductParam::new(
                Telescope::done(vec![Term::intrinsic(Intrinsic::Nat(Nat::new(index)))]),
                Vec::new(),
            ),
        )
    };

    let pin_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(760, Some("n")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        constructors: vec![targeting("yes", 0), targeting("no", 1)].into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let held_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    )]);

    (
        pin_name,
        held_name,
        BTreeMap::from([(pin_name, pin_decl), (held_name, held_decl)]),
    )
}

/// `Pin(n : Nat)` eliminated at `Pin(1)` with its `yes` arm alone — the shape `Cases::sole_arm` reads and `Cases::unread` fires on, where the empty elimination above is the shape `check_induct_arms` skips the large-elimination guard for.
///
/// `Pin(1)` is inhabited by `Pin/no()`, and the `yes` arm's target `0` clashes with the index, so `specialize` reports the case impossible and no rule types that arm. What stands between it and a closed inhabitant of `False` is coverage: `no` has no arm and is not impossible at `Pin(1)`. Were its absence excused, `forged(Pin/no())` would reduce to the `yes` arm's body — a closed match with one arm, no catch-all and no payload named is its arm's body with its scrutinee unevaluated, which is why the body below is the sort `Prop`, a term standing at `False` that nothing typed.
///
/// Only a fixture reaches the kernel's own coverage loop with this shape: `curios-elab`'s matrix refuses the sole-arm match with `MatrixUncovered` while the module is still being built, so no surface program leaves one behind.
pub(super) fn sole_arm_forgery() -> Program {
    let (pin_name, _, mut decls) = pin_decls();
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = || Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    decls.insert(false_name, proposition(Vec::new()));

    let at = |index: usize| {
        Term::induct_type(
            pin_name,
            Vec::<Term>::new(),
            [Term::intrinsic(Intrinsic::Nat(Nat::new(index)))],
        )
    };
    let proof = Free::local(761, Some("p"));

    // forged : (p : Pin(1)) -> False = (p) => match p at Pin(1) : (_, _) => False | yes() => Prop end
    let forged_name = Global::Authored(Qualifier::from(["forged"]));
    let forged = authored(
        &forged_name,
        Term::func_type([(proof, at(1))], false_type()),
        Term::func(
            [(proof, at(1))],
            Term::induct_match_scoped_marked(
                Term::free_var(&proof),
                Scope::constant(Many(2), false_type()),
                [(
                    Atom::from("yes"),
                    Vec::<(Plicity, Free)>::new(),
                    Term::prop(),
                )],
                None,
            )
            .stating(at(1)),
        ),
    );

    Program {
        definitions: declaring(
            Definitions {
                items: vec![forged].into(),
            },
            decls,
            [],
            [],
        ),
        entrypoint: Entrypoint {
            body: Term::apply(
                Term::instance(forged_name, Universes::default()),
                [Term::variant(
                    pin_name,
                    Vec::<Term>::new(),
                    "no",
                    Vec::<Term>::new(),
                )],
            ),
            type_: Some(false_type()),
        },
    }
}

/// The control: the same two constructors with the scrutinee at `Pin(0)`, where `no` *is* impossible and `yes` is the arm reduction takes. The sole-arm elimination must stay accepted — it is the form a cast along a proof is written in, and it runs none of the proof — so a clause that refused every match with one arm would shut the forgery above with a brick.
pub(super) fn sole_arm_control() -> Definitions {
    let (pin_name, held_name, decls) = pin_decls();
    let held_type = || Term::induct_type(held_name, Vec::<Term>::new(), Vec::<Term>::new());

    let at = |index: usize| {
        Term::induct_type(
            pin_name,
            Vec::<Term>::new(),
            [Term::intrinsic(Intrinsic::Nat(Nat::new(index)))],
        )
    };
    let (proof, carried) = (Free::local(762, Some("p")), Free::local(763, Some("h")));

    // cast : (p : Pin(0), h : Held) -> Held = (p, h) => match p at Pin(0) : (_, _) => Held | yes() => h end
    let cast_name = Global::Authored(Qualifier::from(["cast"]));
    let cast = authored(
        &cast_name,
        Term::func_type([(proof, at(0)), (carried, held_type())], held_type()),
        Term::func(
            [(proof, at(0)), (carried, held_type())],
            Term::induct_match_scoped_marked(
                Term::free_var(&proof),
                Scope::constant(Many(2), held_type()),
                [(
                    Atom::from("yes"),
                    Vec::<(Plicity, Free)>::new(),
                    Term::free_var(&carried),
                )],
                None,
            )
            .stating(at(0)),
        ),
    );

    declaring(
        Definitions {
            items: vec![cast].into(),
        },
        decls,
        [],
        [],
    )
}

/// The control: the same two families with `Two` at `Type 0`, where `Two/b()` really does clash with `Two/a()`. Nothing inhabits `Held(Two/b())`, so the module proves nothing — what it pins is that the empty elimination stays legal.
pub(super) fn relevant_index_control() -> Definitions {
    let (two_name, held_name, mut decls) = clashing_index_decls(Term::type_ground());
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
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
        Term::func_type([(subject, at_b.clone())], false_type.clone()),
        Term::func(
            [(subject, at_b)],
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

    declaring(
        Definitions {
            items: vec![vacuous].into(),
        },
        decls,
        [],
        [],
    )
}

/// `Two : Type 0` with constructors `a` and `b`, `Held(t : Two)` whose two constructors both target `Two/a()` under the given tags, and `vacuous : (x : Held(Two/b())) -> False` eliminating with no arms at all.
///
/// Both constructors target `Two/a()`, so at `Two/b()` every one of them is genuinely impossible and the elimination is legal — *when the tags are distinct*. Giving them the same tag changes nothing about the targets and everything about which entry the coverage rule reads.
pub(super) fn shadowed_constructor(tags: [&str; 2]) -> Definitions {
    let two_name = Global::Authored(Qualifier::from(["Two"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let two = Term::induct_type(two_name, Vec::<Term>::new(), Vec::<Term>::new());
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());

    let at = |tag: &str| Term::variant(two_name, Vec::<Term>::new(), tag, Vec::<Term>::new());
    let nullary = |tag: &str, targets: Vec<Term>| {
        (
            Atom::from(tag),
            InductParam::new(Telescope::done(targets), Vec::new()),
        )
    };

    let two_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![nullary("a", Vec::new()), nullary("b", Vec::new())].into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let held_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build([(Free::local(800, Some("t")), two)], ())),
        constructors: vec![
            nullary(tags[0], vec![at("a")]),
            nullary(tags[1], vec![at("a")]),
        ]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let at_b = Term::induct_type(held_name, Vec::<Term>::new(), [at("b")]);
    let vacuous_name = Global::Authored(Qualifier::from(["vacuous"]));
    let subject = Free::local(801, Some("x"));
    let vacuous = authored(
        &vacuous_name,
        Term::func_type([(subject, at_b.clone())], false_type.clone()),
        Term::func(
            [(subject, at_b)],
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

    declaring(
        Definitions {
            items: vec![vacuous].into(),
        },
        BTreeMap::from([
            (two_name, two_decl),
            (Global::Authored(Qualifier::from(["Held"])), held_decl),
            (false_name, proposition(Vec::new())),
        ]),
        [],
        [],
    )
}

/// [`level_definition`] with a universe scheme of `parameter_count` parameters.
pub(super) fn scheme_definition(level: &Level, parameter_count: usize) -> Definitions {
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(level.clone()),
        body: Term::intrinsic(Intrinsic::NatType),
    };

    Definitions {
        items: vec![Item::Let(Box::new(definition))].into(),
    }
}

/// [`level_registry`] with a universe scheme of `parameter_count` parameters.
pub(super) fn scheme_registry(level: &Level, parameter_count: usize) -> Definitions {
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: Term::type_at(level.clone()),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            items: Vec::new().into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// `let held : Type.{u} = Levelled.{…}`, where `Levelled` declares two universe parameters and the occurrence supplies `width` of them.
pub(super) fn instance_of_width(width: usize) -> Definitions {
    let family = Global::Authored(Qualifier::from(["Levelled"]));

    // Two parameters and no constraints — the shape `check_instance`'s loop never inspects.
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: Term::type_at(Level::param(UniverseParam(1))),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let levels = vec![Level::param(UniverseParam(0)); width];
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(Level::param(UniverseParam(0))),
        body: Term::induct_type_at(family, levels, Vec::<Term>::new(), Vec::<Term>::new()),
    };

    declaring(
        Definitions {
            items: vec![Item::Let(Box::new(definition))].into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// `let held : Type.{expected} = Levelled.{instance}`, where `Levelled.{u}` is a family of no constructors whose sort is `Type.{u + 1}`: an occurrence whose sort is its scheme's read at the instance it states.
pub(super) fn family_instance_at(instance: u32, expected: u32) -> Definitions {
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: Term::type_at(
            Level::param(UniverseParam(0))
                .succ()
                .expect("a parameter has a successor"),
        ),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            items: vec![authored(
                &Global::Authored(Qualifier::from(["held"])),
                Term::type_at(Level::constant(expected)),
                Term::induct_type_at(
                    family,
                    vec![Level::constant(instance)],
                    Vec::<Term>::new(),
                    Vec::<Term>::new(),
                ),
            )]
            .into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// `let lift.{u} : Type.{u + 1} = Type.{u}`, then `let held : Type.{expected} = lift.{instance}`: an occurrence whose type is its scheme's read at the instance it states.
pub(super) fn scheme_instance_at(instance: u32, expected: u32) -> Definitions {
    let lift = Global::Authored(Qualifier::from(["lift"]));
    let parameter = Level::param(UniverseParam(0));
    let scheme = Definition {
        name: lift,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(parameter.succ().expect("a parameter has a successor")),
        body: Term::type_at(parameter),
    };

    Definitions {
        items: vec![
            Item::Let(Box::new(scheme)),
            authored(
                &Global::Authored(Qualifier::from(["held"])),
                Term::type_at(Level::constant(expected)),
                Term::instance(lift, vec![Level::constant(instance)]),
            ),
        ]
        .into(),
    }
}

/// A row no store issued, returning one `Nat`.
pub(super) fn forged_row() -> ForeignFunction {
    ForeignFunction {
        name: "/forged".to_string(),
        signature: WireSignature {
            params: Vec::new(),
            returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
        },
    }
}

/// `let held : claimed = <a call to row with no operands>`, with `False` declared alongside.
pub(super) fn foreign_held_at(
    row: ForeignFunction,
    claimed: &Term,
    false_name: &Global,
) -> Definitions {
    let row = Arc::new(row);

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: claimed.clone(),
        body: Term::foreign(
            row,
            Global::Authored(Qualifier::from(["forged"])),
            Vec::new(),
        ),
    };

    declaring(
        Definitions {
            items: vec![Item::Let(Box::new(definition))].into(),
        },
        BTreeMap::from([(*false_name, proposition(Vec::new()))]),
        [],
        [],
    )
}

/// `Foo`'s registry entry and its type-former definition, declaring `registry` and `definition` universe parameters respectively.
pub(super) fn disagreeing_schemes(registry: usize, definition: usize) -> Definitions {
    let family = Global::Authored(Qualifier::from(["Foo"]));

    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: registry,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: Term::type_at(Level::param(UniverseParam(0))),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    // The type-former binding, whose body is the family's own normal form at the registry's width.
    let former = Definition {
        name: family,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: definition,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(Level::param(UniverseParam(0))),
        body: Term::induct_type_at(
            family,
            vec![Level::param(UniverseParam(0)); registry],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };

    declaring(
        Definitions {
            items: vec![Item::Let(Box::new(former))].into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// `extract : (p : P(0)) -> Nat`, eliminating the two-constructor proposition `P` under a motive whose inner switch states `sort` while every arm of it is `Nat`.
pub(super) fn lying_motive(sort: Term) -> Definitions {
    extraction(sort, None, &["mk", "mk2"])
}

/// [`lying_motive`] under an honest motive, over a `P` of the constructors `tags` that takes one universe parameter its declaration never mentions, eliminated at `instance`.
pub(super) fn levelled_extraction(instance: Level, tags: &[&str]) -> Definitions {
    extraction(Term::type_ground(), Some(instance), tags)
}

fn extraction(sort: Term, instance: Option<Level>, tags: &[&str]) -> Definitions {
    let family = Global::Authored(Qualifier::from(["P"]));
    let levels = usize::from(instance.is_some());
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));

    let nullary = |tag: &str| {
        (
            Atom::from(tag),
            InductParam::new(Telescope::done(vec![zero.clone()]), Vec::new()),
        )
    };
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: levels,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(600, Some("i")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        constructors: tags.iter().map(|tag| nullary(tag)).collect(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: vec![Variance::Irrelevant; levels],
    };

    let at_zero = Term::induct_type_at(
        family,
        instance.into_iter().collect::<Universes>(),
        Vec::<Term>::new(),
        [zero.clone()],
    );

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
            [(subject, at_zero.clone())],
            Term::intrinsic(Intrinsic::NatType),
        ),
        Term::func(
            [(subject, at_zero)],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(Many(2), &[&index, &scrutinee], motive_body),
                tags.iter()
                    .enumerate()
                    .map(|(arm, tag)| (*tag, Vec::new(), literal(7 + 2 * arm))),
                None,
            ),
        ),
    );

    declaring(
        Definitions {
            items: vec![extract].into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// The three motives [`fold_motive`] states over `match n`, for `n : Nat`.
pub(super) enum FoldMotive {
    /// `(_) => Eq()(n, 0)`: names the scrutinee instead of binding it, with the hypothesis returned whole in the successor arm.
    Captured,
    /// `(m) => Eq()(m, 0)`: binds the scrutinee, with the same successor arm — which is then the mismatch it should be.
    Honest,
    /// `(m) => Eq()(m, m)`, with `refl(k + 1)` in the successor arm: the same shape at a goal each arm can inhabit, so the module is otherwise well-formed.
    Reflexive,
}

/// `all_zero : (n : Nat) -> Eq()(n, 0)` (or `Eq()(n, n)` for [`FoldMotive::Reflexive`]) by a `Nat` fold under the given motive, with `refl` in the zero arm.
///
/// Under the captured motive every arm's goal is `Eq()(n, 0)` as it is written, which the zero arm's `refl(0)` does not inhabit. The honest motive types the hypothesis at `Eq()(k, 0)`, and the successor arm is a mismatch against `Eq()(k + 1, 0)`.
///
/// `Eq` is the two-index equality [`equality_declaration`] declares, at `Nat`, so the module needs nothing from the prelude; its `refl` payload is the value alone, the carrier being the family's parameter.
pub(super) fn fold_motive(motive: FoldMotive) -> Definitions {
    let equality = Global::Authored(Qualifier::from(["Eq"]));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let literal = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));
    let equal = |left: Term, right: Term| Term::induct_type(equality, [nat()], [left, right]);
    let refl = |value: Term| {
        Term::variant_at(
            equality,
            Vec::new(),
            vec![nat()],
            Atom::from("refl"),
            vec![value],
        )
    };

    let n = Free::local(610, Some("n"));
    let bound = Free::local(611, Some("m"));
    let pred = Free::local(612, Some("k"));
    let hypothesis = Free::local(613, Some("ih"));
    let successor = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&pred), literal(1)));

    let (goal, family, step) = match motive {
        FoldMotive::Captured => (
            equal(Term::free_var(&n), literal(0)),
            equal(Term::free_var(&n), literal(0)),
            Term::free_var(&hypothesis),
        ),
        FoldMotive::Honest => (
            equal(Term::free_var(&bound), literal(0)),
            equal(Term::free_var(&n), literal(0)),
            Term::free_var(&hypothesis),
        ),
        FoldMotive::Reflexive => (
            equal(Term::free_var(&bound), Term::free_var(&bound)),
            equal(Term::free_var(&n), Term::free_var(&n)),
            refl(successor),
        ),
    };
    let motive = Scope::close(Many(1), &[&bound], goal);
    let body = Term::nat_match_scoped(
        Term::free_var(&n),
        motive,
        refl(literal(0)),
        &pred,
        &hypothesis,
        step,
    );

    let all_zero = authored(
        &Global::Authored(Qualifier::from(["all_zero"])),
        Term::func_type([(n, nat())], family),
        Term::func([(n, nat())], body),
    );

    declaring(
        Definitions {
            items: vec![all_zero].into(),
        },
        BTreeMap::from([(equality, equality_declaration())]),
        [],
        [],
    )
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
pub(super) fn occurrence_definitions(params: Vec<Term>, indices: Vec<Term>) -> Definitions {
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
            InductParam::new(
                Telescope::build(
                    [(Free::local(952, Some("A")), Term::type_ground())],
                    vec![zero],
                ),
                vec![Plicity::Implicit],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::type_ground(),
        Term::induct_type(family, params, indices),
    );

    declaring(
        Definitions {
            items: vec![held].into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// The four ways a one-parameter nominal value can disagree with its declaration.
pub(super) fn nominal_value_cases() -> Vec<(&'static str, Definitions)> {
    let nat = Term::intrinsic(Intrinsic::NatType);
    vec![
        (
            "struct at no parameters",
            struct_value_definitions(Vec::new()),
        ),
        (
            "struct at two parameters",
            struct_value_definitions(vec![nat.clone(), nat.clone()]),
        ),
        (
            "variant at no parameters",
            variant_value_definitions(Vec::new()),
        ),
        (
            "variant at two parameters",
            variant_value_definitions(vec![nat.clone(), nat]),
        ),
    ]
}

/// `struct S(A : Type) : Type { f : A }`, with a literal at `params`.
pub(super) fn struct_value_definitions(params: Vec<Term>) -> Definitions {
    let name = Global::Authored(Qualifier::from(["S"]));
    let a = Free::local(980, Some("A"));
    let declaration = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(a, Term::type_ground())],
            Telescope::build([(Free::local(981, Some("f")), Term::free_var(&a))], ()),
        ),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let declared: Term = Subterm::StructType(StructType {
        name,
        universes: Universes::default(),
        params: vec![Term::intrinsic(Intrinsic::NatType)],
    })
    .into();

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        declared,
        Term::struct_(
            name,
            params,
            [Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)))],
        ),
    );

    declaring(
        Definitions {
            items: vec![held].into(),
        },
        [],
        BTreeMap::from([(name, declaration)]),
        [],
    )
}

/// `induct F(A : Type) : Type | mk(x : A) end`, with a constructor application at `params`.
pub(super) fn variant_value_definitions(params: Vec<Term>) -> Definitions {
    let family = Global::Authored(Qualifier::from(["F"]));
    let a = Free::local(970, Some("A"));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(a, Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [
                        (a, Term::type_ground()),
                        (Free::local(971, Some("x")), Term::free_var(&a)),
                    ],
                    Vec::new(),
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::induct_type(
            family,
            [Term::intrinsic(Intrinsic::NatType)],
            Vec::<Term>::new(),
        ),
        Term::variant(
            family,
            params,
            "mk",
            [Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)))],
        ),
    );

    declaring(
        Definitions {
            items: vec![held].into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// The two shapes: an application that under-saturates its lambda, and a neutral spine whose `plicities` are shorter than the arguments applied to it.
pub(super) fn unsaturated_cases() -> Vec<(&'static str, Definitions)> {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    let two_binder = |result: Term| Telescope::build([(a, nat.clone()), (b, nat.clone())], result);
    let definitions_of = |items: Vec<Item>| Definitions {
        items: items.into(),
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
        Term::apply(
            Term::instance(former, Universes::default()),
            [three.clone()],
        ),
        three.clone(),
    );

    vec![(
        "a lambda applied to fewer arguments than it binds",
        definitions_of(vec![former_def, under_applied]),
    )]
}

/// The three shapes: an arm over- and under-binding its payload, and a recursive application short of its member's binders.
pub(super) fn unguarded_opener_cases() -> Vec<(&'static str, Definitions)> {
    vec![
        (
            "an arm binding two components of a one-component payload",
            arm_definitions(vec![
                (Plicity::Explicit, Free::local(996, Some("a"))),
                (Plicity::Explicit, Free::local(997, Some("b"))),
            ]),
        ),
        (
            "an arm binding none of a one-component payload",
            arm_definitions(Vec::new()),
        ),
        (
            "a recursive application short of its binders",
            rec_apply_definitions(),
        ),
    ]
}

/// `held : match F/mk(3) : (s) => Type | mk(<binders>) => Nat end`, for `induct F : Type | mk(x : Nat) end`.
pub(super) fn arm_definitions(binders: Vec<(Plicity, Free)>) -> Definitions {
    let family = Global::Authored(Qualifier::from(["F"]));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build([(Free::local(995, Some("x")), nat.clone())], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let declared = Term::induct_match_scoped_marked(
        Term::variant(family, Vec::<Term>::new(), "mk", [three.clone()]),
        Scope::close(
            Many(1),
            &[&Free::local(998, Some("s"))],
            Term::type_ground(),
        ),
        [("mk", binders, nat)],
        None,
    );

    declaring(
        Definitions {
            items: vec![authored(
                &Global::Authored(Qualifier::from(["held"])),
                declared,
                three,
            )]
            .into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// `f : (a : Nat) -> Type = (a) => Nat` with `held : f(3)`, the argument written under `mark` where its parameter is plain.
pub(super) fn marked_apply_definitions(mark: Plicity) -> Definitions {
    let a = Free::local(993, Some("a"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let former = Global::Authored(Qualifier::from(["f"]));

    Definitions {
        items: vec![
            authored(
                &former,
                Term::func_type([(a, nat.clone())], Term::type_ground()),
                Term::func([(a, nat.clone())], nat),
            ),
            authored(
                &Global::Authored(Qualifier::from(["held"])),
                Term::apply_marked(
                    Term::instance(former, Universes::default()),
                    [(mark, three.clone())],
                ),
                three,
            ),
        ]
        .into(),
    }
}

/// The item [`eta_former_definitions`] declares.
pub(super) fn eta_former_held() -> Global {
    Global::Authored(Qualifier::from(["held"]))
}

/// `struct S(A : Type) : Type { f : A }` with `held : T = S(Nat) { 3 }`, `T` an η-form of `S` applied over its whole list: `((@A : Type) => S(A))(@Nat)`, its binder under a mark `S`'s plain parameter does not take, or with `lying` `((A : Nat) => S(A))(3)`, its binder at a type `S`'s parameter does not take. Handed back beside the module is `T` as it was built.
pub(super) fn eta_former_definitions(lying: bool) -> (Definitions, Term) {
    let name = Global::Authored(Qualifier::from(["S"]));
    let a = Free::local(960, Some("A"));
    let bound = Free::local(962, Some("A"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let declaration = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(a, Term::type_ground())],
            Telescope::build([(Free::local(961, Some("f")), Term::free_var(&a))], ()),
        ),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let (mark, domain, argument) = match lying {
        false => (Plicity::Implicit, Term::type_ground(), nat.clone()),
        true => (Plicity::Explicit, nat.clone(), three.clone()),
    };
    let node: Term = Subterm::StructType(StructType {
        name,
        universes: Universes::default(),
        params: vec![Term::free_var(&bound)],
    })
    .into();
    let declared = Term::apply_marked(
        Term::func_marked([(mark, bound, domain)], node),
        [(mark, argument)],
    );

    let held = authored(
        &eta_former_held(),
        declared.clone(),
        Term::struct_(name, [nat], [three]),
    );

    let definitions = declaring(
        Definitions {
            items: vec![held].into(),
        },
        [],
        BTreeMap::from([(name, declaration)]),
        [],
    );

    (definitions, declared)
}

/// The two items [`forwarding_definitions`] judges at a plain or a hidden type.
pub(super) fn forwarding_items() -> [Global; 2] {
    [
        Global::Authored(Qualifier::from(["by_name"])),
        Global::Authored(Qualifier::from(["by_literal"])),
    ]
}

/// `g : (@n : Nat) -> Nat = (@n) => ((x : Nat) => x)(n)`, a function that forwards across a mark and whose η-contraction is `(x : Nat) => x`, with `by_name : T = g` and `by_literal : T` the same lambda, `T` the function type over `n : Nat` under `declared`.
pub(super) fn forwarding_definitions(declared: Plicity) -> Definitions {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let g = Global::Authored(Qualifier::from(["g"]));
    let lambda = |n: u64, x: u64| {
        let n = Free::local(n, Some("n"));
        let x = Free::local(x, Some("x"));
        Term::func_marked(
            [(Plicity::Implicit, n, nat.clone())],
            Term::apply(
                Term::func([(x, nat.clone())], Term::free_var(&x)),
                [Term::free_var(&n)],
            ),
        )
    };
    let typed = |mark: Plicity, n: u64| {
        let n = Free::local(n, Some("n"));
        Term::func_type_marked([(mark, n, nat.clone())], nat.clone())
    };
    let [by_name, by_literal] = forwarding_items();

    Definitions {
        items: vec![
            authored(&g, typed(Plicity::Implicit, 950), lambda(951, 952)),
            authored(
                &by_name,
                typed(declared, 953),
                Term::instance(g, Universes::default()),
            ),
            authored(&by_literal, typed(declared, 954), lambda(955, 956)),
        ]
        .into(),
    }
}

/// `rec f : (a : Nat, b : Nat) -> Type = (a, b) => Nat; f(3)` as a declared type.
pub(super) fn rec_apply_definitions() -> Definitions {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let f = Free::local(992, Some("f"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let plicities = vec![Plicity::Explicit, Plicity::Explicit];

    let member_type: Term = Subterm::FuncType(FuncType::new(
        Telescope::build([(a, nat.clone()), (b, nat.clone())], Term::type_ground()),
        plicities.clone(),
    ))
    .into();
    let member_body: Term = Subterm::Func(Func::new(
        Telescope::build([(a, nat.clone()), (b, nat.clone())], nat),
        plicities,
    ))
    .into();

    let selection = Term::rec([(f, member_type, member_body)], Term::free_var(&f));

    Definitions {
        items: vec![authored(
            &Global::Authored(Qualifier::from(["held"])),
            Term::apply(selection, [three.clone()]),
            three,
        )]
        .into(),
    }
}

/// `sink : (Nat) -> Nat`, honestly stamped `Partial`: its body projects `rec f : (Nat) -> Nat = (n) => f(n)`, which does not descend.
pub(super) fn diverging_sink() -> Item {
    diverging("sink")
}

/// [`diverging_sink`] under another name.
pub(super) fn diverging(name: &str) -> Item {
    let f = Free::local(920, Some("f"));
    let n = Free::local(921, Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);

    let group = RecGroup::closing(vec![(
        f,
        Term::func_type([(n, nat.clone())], nat.clone()),
        Term::func(
            [(n, nat.clone())],
            Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
        ),
    )]);

    authored(
        &Global::Authored(Qualifier::from([name])),
        Term::func_type([(Free::local(922, Some("n")), nat.clone())], nat),
        Term::rec_proj(group, 0),
    )
}

/// `held : Nat = ((p : Vouched) => 0)(proof)` beside `sink` and `reaches`, where `proof` is `((g : (Nat) -> Nat) => Vouched/qed(()))(reaches)` when `reaching` and `Vouched/qed(())` otherwise.
///
/// The one position this records that names `reaches` is the **argument**: `held`'s own type and body sit at `Nat`, which is neither a proposition nor a sort, and every other recorded position — the binders' types, the lambdas, `qed(())` — names nothing partial. So the obligation that refuses it is (V) at an argument position and no other, which is the fixture [the soundness board's *Rejected*](../../../documentation/design/soundness/the-soundness-board.md) says has to be constructed, the corpus passing (V) inert there.
pub(super) fn proof_at_an_argument_definitions(reaching: bool) -> Definitions {
    let (vouched_name, declaration) = vouched_declaration();
    let vouched = || Term::induct_type(vouched_name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = Term::intrinsic(Intrinsic::NatType);
    let qed = || {
        Term::variant(
            vouched_name,
            Vec::<Term>::new(),
            "qed",
            [Term::tuple(Vec::<Term>::new())],
        )
    };
    let proof = match reaching {
        true => Term::apply(
            Term::func(
                [(
                    Free::local(960, Some("g")),
                    Term::func_type([(Free::local(961, Some("n")), nat.clone())], nat.clone()),
                )],
                qed(),
            ),
            [Term::instance(
                Global::Authored(Qualifier::from(["reaches"])),
                Universes::default(),
            )],
        ),
        false => qed(),
    };
    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        nat.clone(),
        Term::apply(
            Term::func(
                [(Free::local(962, Some("p")), vouched())],
                Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
            ),
            [proof],
        ),
    );

    declaring(
        Definitions {
            items: vec![diverging_sink(), reaching_definition(), held].into(),
        },
        BTreeMap::from([(vouched_name, declaration)]),
        [],
        [],
    )
}

/// `rec bad : Vouched = bad`: a group at a proposition with nothing to descend on, which `check_group`'s local gate must refuse — the verdict every budget has to reach the same way.
pub(super) fn proof_typed_nullary_group() -> Definitions {
    let (vouched_name, declaration) = vouched_declaration();
    let bad = Global::Authored(Qualifier::from(["bad"]));
    let member = fresh(Some("bad"));

    declaring(
        Definitions {
            items: vec![Item::Rec(RecItem::new(vec![(
                member,
                Definition {
                    name: bad,
                    kind: DefinitionKind::Authored,
                    universe_context: UniverseContext::empty(),
                    island: Qualifier::default(),
                    proof: false,
                    value_sort: None,
                    type_: Term::induct_type(vouched_name, Vec::<Term>::new(), Vec::<Term>::new()),
                    body: Term::free_var(&member),
                },
            )]))]
            .into(),
        },
        BTreeMap::from([(vouched_name, declaration)]),
        [],
        [],
    )
}

/// `held : Vouched = ((g : (Nat) -> Nat, h : (Nat) -> Nat) => Vouched/qed(()))(first, second)` beside the two diverging definitions it names, in that order.
pub(super) fn two_sinks_definitions(first: &str, second: &str) -> Definitions {
    let (vouched_name, declaration) = vouched_declaration();
    let vouched = Term::induct_type(vouched_name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = Term::intrinsic(Intrinsic::NatType);
    let function = || Term::func_type([(Free::local(928, Some("n")), nat.clone())], nat.clone());
    let named = |name: &str| {
        Term::instance(
            Global::Authored(Qualifier::from([name])),
            Universes::default(),
        )
    };
    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        vouched,
        Term::apply(
            Term::func(
                [
                    (Free::local(929, Some("g")), function()),
                    (Free::local(930, Some("h")), function()),
                ],
                Term::variant(
                    vouched_name,
                    Vec::<Term>::new(),
                    "qed",
                    [Term::tuple(Vec::<Term>::new())],
                ),
            ),
            [named(first), named(second)],
        ),
    );

    declaring(
        Definitions {
            items: vec![diverging(first), diverging(second), held].into(),
        },
        BTreeMap::from([(vouched_name, declaration)]),
        [],
        [],
    )
}

/// `reaches : (Nat) -> Nat = (m) => sink(m)`: partial by closure and nothing else.
pub(super) fn reaching_definition() -> Item {
    let m = Free::local(923, Some("m"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let sink = Global::Authored(Qualifier::from(["sink"]));

    authored(
        &Global::Authored(Qualifier::from(["reaches"])),
        Term::func_type([(Free::local(924, Some("m")), nat.clone())], nat.clone()),
        Term::func(
            [(m, nat)],
            Term::apply(
                Term::instance(sink, Universes::default()),
                [Term::free_var(&m)],
            ),
        ),
    )
}

/// `induct Vouched : Prop | qed(u : {})`, the proposition the proof below inhabits.
pub(super) fn vouched_declaration() -> (Global, InductDecl) {
    let name = Global::Authored(Qualifier::from(["Vouched"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("qed"),
            InductParam::new(
                Telescope::build(
                    [(Free::local(925, Some("u")), Term::tuple_type_unit())],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    (name, declaration)
}

/// `held : Vouched = ((g : (Nat) -> Nat) => Vouched/qed(()))(reaches)` — a proof whose free variables name `reaches` and nothing else, so its verdict is exactly `reaches`'s.
pub(super) fn held_proof() -> Item {
    let (vouched_name, _) = vouched_declaration();
    let vouched = Term::induct_type(vouched_name, Vec::<Term>::new(), Vec::<Term>::new());
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
            [Term::instance(
                Global::Authored(Qualifier::from(["reaches"])),
                Universes::default(),
            )],
        ),
    )
}

/// The library a proof reaches partiality through — `sink` and `reaches` — with `held` and its proposition beside them when the fixture needs the proof.
pub(super) fn reaching_library(with_proof: bool) -> Definitions {
    let mut items = vec![diverging_sink(), reaching_definition()];
    let mut induct_decls = BTreeMap::new();

    if with_proof {
        let (vouched_name, declaration) = vouched_declaration();
        induct_decls.insert(vouched_name, declaration);
        items.push(held_proof());
    }

    declaring(
        Definitions {
            items: items.into(),
        },
        induct_decls,
        [],
        [],
    )
}

/// The proof alone, as the compile path shapes it: the library lives in the environment, and only `held` is judged.
pub(super) fn carried_proof_definitions() -> Definitions {
    let (vouched_name, declaration) = vouched_declaration();

    declaring(
        Definitions {
            items: vec![held_proof()].into(),
        },
        BTreeMap::from([(vouched_name, declaration)]),
        [],
        [],
    )
}

/// What `proof_carrying_unit`'s proof carries at `Io({})`.
pub(super) enum Carried {
    /// `exit(@{}, 0)`: a host row that diverges.
    Exit,
    /// `Io/pure(())`: nothing that fails to terminate.
    Pure,
    /// `rec wait : Io({}) = wait; wait`: a group that never descends, at a type relevant enough that its own gate lets it stand.
    Wait,
}

/// `induct Held : Prop | qed(u : Io({}))`, with `bad : Held` built from what `carried` names.
pub(super) fn proof_carrying_unit(carried: Carried) -> Definitions {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name, Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("qed"),
            InductParam::new(
                Telescope::build(
                    [(
                        Free::local(910, Some("u")),
                        Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())),
                    )],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let payload = match carried {
        Carried::Exit => Term::foreign(
            Arc::new(ForeignFunction {
                name: "/exit".to_string(),
                signature: WireSignature {
                    params: vec![WireType::Byte],
                    returns: WireReturn::Never,
                },
            }),
            Global::Authored(Qualifier::from(["exit"])),
            vec![Term::tuple_type_unit(), Term::intrinsic(Intrinsic::Byte(0))],
        ),
        Carried::Pure => Term::intrinsic(Intrinsic::io_pure(
            Term::tuple_type_unit(),
            Term::tuple(Vec::<Term>::new()),
        )),
        Carried::Wait => {
            let wait = Free::local(911, Some("wait"));
            Term::rec(
                [(
                    wait,
                    Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())),
                    Term::free_var(&wait),
                )],
                Term::free_var(&wait),
            )
        }
    };

    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(held_name, declaration);

    declaring(
        Definitions {
            items: vec![authored(
                &Global::Authored(Qualifier::from(["bad"])),
                held,
                Term::variant(held_name, Vec::<Term>::new(), "qed", [payload]),
            )]
            .into(),
        },
        induct_decls,
        [],
        [],
    )
}

/// `induct Held : Type | mk(n : Nat)` with one item building `Held/mk` at `payload_count` arguments, the constructor's payload under its honest mark or a lying one.
pub(super) fn plicity_definitions(honest: bool, payload_count: usize) -> Definitions {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held = Term::induct_type(held_name, Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(
                        Free::local(920, Some("n")),
                        Term::intrinsic(Intrinsic::NatType),
                    )],
                    Vec::new(),
                ),
                // The lie is a wrong mark: a signature's marks are its entries' own, so a mark short of a binder cannot be written and the marks themselves are the only thing left that could lie.
                match honest {
                    true => vec![Plicity::Explicit],
                    false => vec![Plicity::Implicit],
                },
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let payload = (0..payload_count)
        .map(|_| Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))))
        .collect::<Vec<_>>();

    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(held_name, declaration);

    declaring(
        Definitions {
            items: vec![authored(
                &Global::Authored(Qualifier::from(["value"])),
                held,
                Term::variant(held_name, Vec::<Term>::new(), "mk", payload),
            )]
            .into(),
        },
        induct_decls,
        [],
        [],
    )
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
            InductParam::new(Telescope::done(vec![zero]), Vec::new()),
        )]
        .into(),
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// The module the doc comment below describes: four declarations, and the four definitions that close on `False`.
pub(super) fn index_forgery() -> Definitions {
    let type_0 = Term::type_ground();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let true_name = Global::Authored(Qualifier::from(["True"]));
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));

    let true_type = Term::induct_type(true_name, Vec::<Term>::new(), Vec::<Term>::new());
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(true_name, Vec::<Term>::new(), "qed", Vec::<Term>::new());
    let held_at = |index: Term| Term::induct_type(held_name, Vec::<Term>::new(), [index]);
    let yes = Term::variant(held_name, Vec::<Term>::new(), "yes", Vec::<Term>::new());
    let equality = |carrier: Term, left: Term, right: Term| {
        Term::induct_type(equality_name, [carrier], [left, right])
    };
    let reflexivity =
        |carrier: Term, value: Term| Term::variant(equality_name, [carrier], "refl", [value]);

    // induct True : Prop | qed() end, and the empty induct False : Prop end
    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
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
        Term::func_type([(carried, held_at(nat(0)))], held_at(nat(1))),
        Term::induct_match_scoped_marked(
            Term::instance(forged_name, Universes::default()),
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
                vec![(Plicity::Explicit, arm_value)],
                Term::func(
                    [(identity, held_at(Term::free_var(&arm_value)))],
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
        Term::apply(Term::instance(cast_name, Universes::default()), [yes]),
    );

    // boom : False = match held : (n, w) => False end
    let boom_name = Global::Authored(Qualifier::from(["boom"]));
    let boom_index = Free::local(90, Some("n"));
    let boom_scrutinee = Free::local(91, Some("w"));
    let boom = authored(
        &boom_name,
        false_type.clone(),
        Term::induct_match_scoped_marked(
            Term::instance(held_value_name, Universes::default()),
            Scope::close(Many(2), &[&boom_index, &boom_scrutinee], false_type.clone()),
            Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    declaring(
        Definitions {
            items: vec![forged, cast, held_value, boom].into(),
        },
        BTreeMap::from([
            (true_name, true_decl),
            (false_name, false_decl),
            (equality_name, equality_decl),
            (held_name, held_decl),
        ]),
        [],
        [],
    )
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
pub(super) fn lying_type_positions() -> Vec<(&'static str, Definitions)> {
    let boolean = Term::intrinsic(Intrinsic::BoolType);
    let unit_type = Term::tuple_type_unit();
    let unit = Term::tuple(Vec::<Term>::new());
    let zero = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let truth = Term::intrinsic(Intrinsic::Bool(true));

    let b = Free::local(300, Some("b"));
    let x = Free::local(301, Some("x"));
    let y = Free::local(302, Some("y"));
    let lie = lying_type(&b);

    let probe_module = |body: Term, type_: Term| Definitions {
        items: vec![authored(
            &Global::Authored(Qualifier::from(["probe"])),
            type_,
            body,
        )]
        .into(),
    };

    vec![
        // probe : (b : Bool, x : <lie>) -> {} = (b, x) => ()
        (
            "a function type's domain",
            probe_module(
                Term::func([(b, boolean.clone()), (x, lie.clone())], unit.clone()),
                Term::func_type([(b, boolean.clone()), (x, lie.clone())], unit_type.clone()),
            ),
        ),
        // probe : (b : Bool) -> <lie> = (b) => 0
        (
            "a function type's codomain",
            probe_module(
                Term::func([(b, boolean.clone())], zero.clone()),
                Term::func_type([(b, boolean.clone())], lie.clone()),
            ),
        ),
        // probe : (b : Bool) -> {<lie>} = (b) => (0)
        (
            "a tuple type's component",
            probe_module(
                Term::func([(b, boolean.clone())], Term::tuple([zero.clone()])),
                Term::func_type(
                    [(b, boolean.clone())],
                    Term::tuple_type(vec![(y, lie.clone())]),
                ),
            ),
        ),
        // probe : {} = ((b : Bool, x : <lie>) => ())(true, 0)
        (
            "a lambda's domain annotation",
            probe_module(
                Term::apply(
                    Term::func([(b, boolean.clone()), (x, lie.clone())], unit.clone()),
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
                    [(b, boolean.clone())],
                    Term::let_(&y, lie.clone(), zero.clone(), unit.clone()),
                ),
                Term::func_type([(b, boolean.clone())], unit_type.clone()),
            ),
        ),
        // probe : (b : Bool) -> {} = (b) => (rec y : <lie> = 0; ())
        (
            "a rec member's declared type",
            probe_module(
                Term::func(
                    [(b, boolean.clone())],
                    Term::rec([(y, lie.clone(), zero.clone())], unit.clone()),
                ),
                Term::func_type([(b, boolean)], unit_type),
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
        variances: Vec::new(),
    }
}

/// `Wrap(true)`, the instance at which the field type above reduces to its true arm.
pub(super) fn wrap_at_true(wrap_name: &Global) -> Term {
    Subterm::StructType(StructType {
        name: *wrap_name,
        universes: Universes::default(),
        params: vec![Term::intrinsic(Intrinsic::Bool(true))],
    })
    .into()
}

/// `wrapped : Wrap(true) = Wrap(true){held}`.
pub(super) fn wrapped_at_true(wrap_name: &Global, held: Term) -> Item {
    authored(
        &Global::Authored(Qualifier::from(["wrapped"])),
        wrap_at_true(wrap_name),
        Term::struct_(*wrap_name, [Term::intrinsic(Intrinsic::Bool(true))], [held]),
    )
}

/// [`index_forgery`]'s transport, reached through a `Prop`-sorted structure that really carries a `Nat`.
pub(super) fn computed_field_forgery() -> Definitions {
    let type_0 = Term::type_ground();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));

    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let held_at = |index: Term| Term::induct_type(held_name, Vec::<Term>::new(), [index]);
    let yes = Term::variant(held_name, Vec::<Term>::new(), "yes", Vec::<Term>::new());
    let wrap = wrap_at_true(&wrap_name);
    let wrapping =
        |held: Term| Term::struct_(wrap_name, [Term::intrinsic(Intrinsic::Bool(true))], [held]);

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
            equality_name,
            [wrap.clone()],
            [wrapping(nat(0)), wrapping(nat(1))],
        ),
        Term::variant(equality_name, [wrap], "refl", [wrapping(nat(0))]),
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
        Term::func_type([(carried, held_at(nat(0)))], held_at(nat(1))),
        Term::induct_match_scoped_marked(
            Term::instance(forged_name, Universes::default()),
            Scope::close(
                Many(3),
                &[&motive_left, &motive_right, &motive_proof],
                Term::func_type(
                    [(
                        carried,
                        held_at(Term::proj_struct(
                            Term::free_var(&motive_left),
                            wrap_name,
                            0,
                        )),
                    )],
                    held_at(Term::proj_struct(
                        Term::free_var(&motive_right),
                        wrap_name,
                        0,
                    )),
                ),
            ),
            [(
                "refl",
                vec![(Plicity::Explicit, arm_value)],
                Term::func(
                    [(
                        identity,
                        held_at(Term::proj_struct(Term::free_var(&arm_value), wrap_name, 0)),
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
        Term::apply(Term::instance(cast_name, Universes::default()), [yes]),
    );

    // boom : False = match held : (n, w) => False end
    let boom_index = Free::local(90, Some("n"));
    let boom_scrutinee = Free::local(91, Some("w"));
    let boom = authored(
        &Global::Authored(Qualifier::from(["boom"])),
        false_type.clone(),
        Term::induct_match_scoped_marked(
            Term::instance(held_value_name, Universes::default()),
            Scope::close(Many(2), &[&boom_index, &boom_scrutinee], false_type.clone()),
            Vec::<(&str, Vec<(Plicity, Free)>, Term)>::new(),
            None,
        ),
    );

    declaring(
        Definitions {
            items: vec![forged, cast, held_value, boom].into(),
        },
        BTreeMap::from([
            (false_name, false_decl),
            (equality_name, equality_declaration()),
            (held_name, held_decl),
        ]),
        BTreeMap::from([(
            wrap_name,
            computed_field_wrapper(nat_type.clone(), nat_type),
        )]),
        [],
    )
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
        quality_count: 0,
        constraints: vec![ordered],
    }
}

/// The same two parameters with the scheme's constraint *not* declared — the context a user of `A` stands in when it has assumed nothing about its own levels.
pub(super) fn open_context() -> UniverseContext {
    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: Vec::new(),
    }
}

/// The scheme above, followed by one `user` that spells an occurrence of it.
///
/// `user` is `(its own universe context, the term it is defined as)`, and its declared type is `Type v` throughout — so the two spellings of the occurrence differ in nothing but whether the instance is stated.
pub(super) fn universe_scheme_definitions(user: Option<(UniverseContext, Term)>) -> Definitions {
    let scheme = Definition {
        name: Global::Authored(Qualifier::from(["A"])),
        kind: DefinitionKind::Authored,
        universe_context: scheme_context(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::type_at(Level::param(UniverseParam(0))),
    };

    let mut items = vec![Item::Let(Box::new(scheme))];
    if let Some((universe_context, body)) = user {
        items.push(Item::Let(Box::new(Definition {
            name: Global::Authored(Qualifier::from(["user"])),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_: Term::type_at(Level::param(UniverseParam(1))),
            body,
        })));
    }

    Definitions {
        items: items.into(),
    }
}

/// `A` alone, and `A` at the using item's own two parameters — the two spellings of one occurrence.
pub(super) fn scheme_occurrences() -> (Term, Term) {
    let name = Global::Authored(Qualifier::from(["A"]));
    let bare = Term::instance(name, Universes::default());
    let instance = Term::instance(
        name,
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
pub(super) fn universe_refinement_definitions(target: Level, route: Route) -> Definitions {
    let one = Level::zero().succ().expect("level zero has a successor");
    let two = one.clone().succ().expect("level one has a successor");
    let three = two.clone().succ().expect("level two has a successor");

    let e_name = Global::Authored(Qualifier::from(["E"]));
    let q_name = Global::Authored(Qualifier::from(["Q"]));
    let f_name = Global::Authored(Qualifier::from(["f"]));

    let e_type = Term::induct_type(e_name, Vec::<Term>::new(), Vec::<Term>::new());

    let e_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("wrap"),
            InductParam::new(
                Telescope::build(
                    [(Free::local(610, Some("T")), Term::type_at(two.clone()))],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(three),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let q_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(611, Some("e")), e_type.clone())],
            (),
        )),
        constructors: Vec::new().into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let bounded = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .succ()
            .expect("level has a successor"),
        upper: two,
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };

    let wrap_at =
        |level: Level| Term::variant(e_name, Vec::<Term>::new(), "wrap", [Term::type_at(level)]);

    let x = Free::local(620, Some("x"));
    let s = Free::local(621, Some("s"));
    let f_item = Item::Let(Box::new(Definition {
        name: f_name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: vec![bounded],
        },
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::func_type(
            [(
                Free::local(622, Some("x")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            e_type.clone(),
        ),
        body: Term::func(
            [(x, Term::intrinsic(Intrinsic::NatType))],
            Term::switch_scoped(
                Term::free_var(&x),
                Scope::close(Many(1), &[&s], e_type.clone()),
                [(0u32, wrap_at(Level::param(UniverseParam(0))))],
                wrap_at(Level::zero()),
            ),
        ),
    }));

    let f_at = |level: Level, arg: &Free| {
        Term::apply(Term::instance(f_name, vec![level]), [Term::free_var(arg)])
    };
    let q_at = |index: Term| Term::induct_type(q_name, Vec::<Term>::new(), [index]);

    let cx = Free::local(630, Some("x"));
    let cq = Free::local(631, Some("q"));
    let cs = Free::local(632, Some("s"));
    let ct = Free::local(633, Some("T"));

    let declared = q_at(f_at(target, &cx));
    let arm = |motive: Term| {
        Term::induct_match_scoped_marked(
            f_at(Level::zero(), &cx),
            Scope::close(Many(1), &[&cs], motive),
            [("wrap", vec![(Plicity::Explicit, ct)], Term::free_var(&cq))],
            None,
        )
    };
    let body = match route {
        Route::Direct => Term::free_var(&cq),
        Route::ConstantMotive => arm(declared.clone()),
        Route::DependentMotive => arm(q_at(Term::free_var(&cs))),
    };

    let parameters = [
        (cx, Term::intrinsic(Intrinsic::NatType)),
        (cq, q_at(f_at(Level::zero(), &cx))),
    ];
    let coerce = authored(
        &Global::Authored(Qualifier::from(["coerce"])),
        Term::func_type(parameters.clone(), declared),
        Term::func(parameters, body),
    );

    declaring(
        Definitions {
            items: vec![f_item, coerce].into(),
        },
        BTreeMap::from([(e_name, e_decl), (q_name, q_decl)]),
        [],
        [],
    )
}

/// A unit a test puts in scope by hand: a module no unit holds, with the places an environment reads it by and the record filed beside it.
pub(super) struct AlreadyJudged {
    definitions: Definitions,
    places: Places,
    certification: Certification,
}

impl AlreadyJudged {
    /// `module` in scope with `certification` as the record of a walk over it.
    pub(super) fn with(definitions: &Definitions, certification: &Certification) -> Self {
        Self {
            definitions: definitions.clone(),
            places: Places::of(definitions),
            certification: certification.clone(),
        }
    }

    /// The environment a walk is handed: everything this unit puts in scope, and nothing else.
    pub(super) fn globals(&self) -> Globals<'_> {
        let mut globals = Globals::default();
        globals.mount(&self.definitions, &self.places, &self.certification);

        globals
    }
}

/// Everything `module` puts in scope, with an empty record — so a walk classifies its items' totality for itself.
pub(super) fn already_judged(definitions: &Definitions) -> AlreadyJudged {
    AlreadyJudged::with(definitions, &Certification::default())
}

/// A module carrying `items` and `induct_decls` and nothing else.
pub(super) fn collision_definitions(
    items: Vec<Item>,
    induct_decls: BTreeMap<Global, InductDecl>,
) -> Definitions {
    declaring(
        Definitions {
            items: items.into(),
        },
        induct_decls,
        [],
        [],
    )
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
pub(super) fn judged_environment() -> Definitions {
    collision_definitions(
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
                constructors: Vec::new().into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )]),
    )
}

/// `let reader : Nat = shadowed`.
pub(super) fn reader() -> Item {
    authored(
        &reader_name(),
        Term::intrinsic(Intrinsic::NatType),
        Term::instance(shadowed_name(), Universes::default()),
    )
}

/// `let shadowed : Bool = true` beside `let reader : Nat = shadowed`.
pub(super) fn shadowing_items() -> Definitions {
    collision_definitions(
        vec![
            authored(
                &shadowed_name(),
                Term::intrinsic(Intrinsic::BoolType),
                Term::intrinsic(Intrinsic::Bool(true)),
            ),
            reader(),
        ],
        BTreeMap::new(),
    )
}

/// `let Shadowed : Nat = 0`: a definition under the name [`judged_environment`] holds as a registry key and under no definition.
pub(super) fn definition_under_the_family() -> Definitions {
    collision_definitions(
        vec![authored(
            &shadowed_family(),
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        )],
        BTreeMap::new(),
    )
}

/// `name : Nat = 0`, as a group states a member.
fn constant(name: Global) -> Definition {
    Definition {
        name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
    }
}

/// A group of one member, `name : type_ = body`, marked a proof: how a lemma proved by its own recursion is stored.
pub(super) fn proof_group(name: &Global, type_: Term, body: Term) -> Item {
    Item::Rec(RecItem::new(vec![(
        fresh(None),
        Definition {
            proof: true,
            type_,
            body,
            ..constant(*name)
        },
    )]))
}

/// One group of two constants, `shadowed` and `fresh`: the first under a name [`judged_environment`] holds, the second under none.
pub(super) fn group_held_in_part() -> Definitions {
    collision_definitions(
        vec![Item::Rec(RecItem::new(vec![
            (fresh(None), constant(shadowed_name())),
            (
                fresh(None),
                constant(Global::Authored(Qualifier::from(["fresh"]))),
            ),
        ]))],
        BTreeMap::new(),
    )
}

/// One group of two constants, both named `shadowed`.
pub(super) fn group_naming_a_member_twice() -> Definitions {
    collision_definitions(
        vec![Item::Rec(RecItem::new(vec![
            (fresh(None), constant(shadowed_name())),
            (fresh(None), constant(shadowed_name())),
        ]))],
        BTreeMap::new(),
    )
}

/// The name [`statement_declared`] declares once or twice.
pub(super) fn statement_name() -> Global {
    Global::Authored(Qualifier::from(["p"]))
}

/// `p : Prop = Holds` and `thm : p = Holds/qed()`, closed with `thm` stated at `Absurd` — followed, when `twice`, by a second `p : Prop = ((x : Holds) => Absurd)(thm)`.
pub(super) fn statement_declared(twice: bool) -> Program {
    let holds_name = Global::Authored(Qualifier::from(["Holds"]));
    let absurd_name = Global::Authored(Qualifier::from(["Absurd"]));
    let theorem = Global::Authored(Qualifier::from(["thm"]));
    let holds = Term::induct_type(holds_name, Vec::<Term>::new(), Vec::<Term>::new());
    let absurd = Term::induct_type(absurd_name, Vec::<Term>::new(), Vec::<Term>::new());
    let named = |name: &Global| Term::instance(*name, Universes::default());

    let refuted = authored(
        &statement_name(),
        Term::prop(),
        Term::apply(
            Term::func(
                [(Free::local(930, Some("x")), holds.clone())],
                absurd.clone(),
            ),
            [named(&theorem)],
        ),
    );
    let proved = authored(
        &theorem,
        named(&statement_name()),
        Term::variant(holds_name, Vec::<Term>::new(), "qed", Vec::<Term>::new()),
    );
    let stated = authored(&statement_name(), Term::prop(), holds);

    let qed = (
        Atom::from("qed"),
        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
    );

    Program {
        definitions: collision_definitions(
            match twice {
                true => vec![stated, proved, refuted],
                false => vec![stated, proved],
            },
            BTreeMap::from([
                (holds_name, proposition(vec![qed])),
                (absurd_name, proposition(Vec::new())),
            ]),
        ),
        entrypoint: Entrypoint {
            body: named(&theorem),
            type_: Some(absurd),
        },
    }
}

/// The name [`record_negative_in_itself`] declares.
pub(super) fn negative_record_name() -> Global {
    Global::Authored(Qualifier::from(["Bad"]))
}

/// `struct Bad { f : (Bad) -> Absurd }`, closed with `w(Bad { w })` for `w = (b) => b.0(b)` stated at `Absurd` — beside, when `beside_a_family`, an empty `induct Bad : Type 0`.
pub(super) fn record_negative_in_itself(beside_a_family: bool) -> Program {
    let bad = negative_record_name();
    let absurd_name = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = Term::induct_type(absurd_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type: Term = Subterm::StructType(StructType {
        name: bad,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();

    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(931, Some("f")),
                Term::func_type(
                    [(Free::local(932, Some("devoured")), bad_type.clone())],
                    absurd.clone(),
                ),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    // An entry is held by its former, so the family beside the record is a second former under the record's name.
    let family = beside_a_family.then(|| {
        Item::Rec(RecItem::new(vec![(
            fresh(None),
            Definition::former(
                bad,
                Qualifier::default(),
                Nominal::Inductive(InductDecl {
                    result_sort: Term::type_ground(),
                    ..proposition(Vec::new())
                }),
            ),
        )]))
    });
    let induct_decls = BTreeMap::from([(absurd_name, proposition(Vec::new()))]);

    let subject = Free::local(933, Some("b"));
    let open = Term::func(
        [(subject, bad_type)],
        Term::apply(
            Term::proj_struct(Term::free_var(&subject), bad, 0),
            [Term::free_var(&subject)],
        ),
    );

    Program {
        definitions: collision_definitions(
            family
                .into_iter()
                .chain([Item::Let(Box::new(Definition::former(
                    bad,
                    Qualifier::default(),
                    Nominal::Struct(record),
                )))])
                .collect(),
            induct_decls,
        ),
        entrypoint: Entrypoint {
            body: Term::apply(
                open.clone(),
                [Term::struct_(bad, Vec::<Term>::new(), [open])],
            ),
            type_: Some(absurd),
        },
    }
}

/// The family [`wrapper_of_its_parameter`] declares.
pub(super) fn wrapper_family() -> Global {
    Global::Authored(Qualifier::from(["Wrapper"]))
}

/// `induct Wrapper(A : Type) | wrap(held : A) end`, or with `negatively` `| wrap(held : (A) -> Nat) end`, the entry stamped `Strict` in `A` either way: elaboration's vector, the truth of the first and a lie of the second.
pub(super) fn wrapper_of_its_parameter(negatively: bool) -> Definitions {
    wrapper_holding(|a| match negatively {
        true => taken(a),
        false => a,
    })
}

/// `(taken: of) -> Nat`.
pub(super) fn taken(of: Term) -> Term {
    Term::func_type(
        [(Free::local(991, Some("taken")), of)],
        Term::intrinsic(Intrinsic::NatType),
    )
}

/// `induct Wrapper(A : Type) | wrap(held : held(A)) end`, the entry stamped `Strict` in `A` whatever `held` makes of it.
pub(super) fn wrapper_holding(held: impl FnOnce(Term) -> Term) -> Definitions {
    let a = Free::local(990, Some("A"));
    let held = held(Term::free_var(&a));

    collision_definitions(
        Vec::new(),
        BTreeMap::from([(
            wrapper_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::build([(a, Term::type_ground())], Telescope::done(())),
                constructors: vec![(
                    Atom::from("wrap"),
                    InductParam::new(
                        Telescope::build(
                            [
                                (a, Term::type_ground()),
                                (Free::local(992, Some("held")), held),
                            ],
                            Vec::new(),
                        ),
                        vec![Plicity::Implicit, Plicity::Explicit],
                    ),
                )]
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: vec![Polarity::Strict],
                variances: Vec::new(),
            },
        )]),
    )
}

/// The family [`stored_in_the_wrapper`] declares.
pub(super) fn stored_family() -> Global {
    Global::Authored(Qualifier::from(["Outer"]))
}

/// `induct Outer | c(held : Wrapper(Outer)) end`: strictly positive exactly where [`wrapper_family`] is in its parameter.
pub(super) fn stored_in_the_wrapper() -> Definitions {
    let outer = Term::induct_type(stored_family(), Vec::<Term>::new(), Vec::<Term>::new());

    collision_definitions(
        Vec::new(),
        BTreeMap::from([(
            stored_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: vec![(
                    Atom::from("c"),
                    InductParam::new(
                        Telescope::build(
                            [(
                                Free::local(993, Some("held")),
                                Term::induct_type(wrapper_family(), [outer], Vec::<Term>::new()),
                            )],
                            Vec::new(),
                        ),
                        vec![Plicity::Explicit],
                    ),
                )]
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )]),
    )
}

/// `induct Shadowed : Type 0 | mk(a : payload) end`, carried as a registry entry with no items lowering it.
pub(super) fn shadowing_registry(payload: Term) -> Definitions {
    collision_definitions(
        Vec::new(),
        BTreeMap::from([(
            shadowed_family(),
            InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: vec![(
                    Atom::from("mk"),
                    InductParam::new(
                        Telescope::build([(Free::local(910, Some("a")), payload)], Vec::new()),
                        vec![Plicity::Explicit],
                    ),
                )]
                .into(),
                result_sort: Term::type_ground(),
                module: Qualifier::default(),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )]),
    )
}

/// The record [`two_registry_entries`] declares beside [`shadowed_family`].
pub(super) fn kept_record() -> Global {
    Global::Authored(Qualifier::from(["Kept"]))
}

/// What [`two_registry_entries`] varies of the two entries it declares.
pub(super) enum Varied {
    /// Nothing: the entries as an environment holds them.
    Nothing,
    /// Elaboration's stamps on both: a polarity vector, the owning module and the representation's privacy.
    Stamps,
    /// The family's variance vector.
    Variance,
    /// The type of the record's field.
    Field,
}

/// `induct Shadowed : Type 0 end` and `struct Kept { held : Nat }`, as registry entries with no items lowering them, differing from the unvaried pair in what `varied` names.
pub(super) fn two_registry_entries(varied: Varied) -> Definitions {
    let stamped = matches!(varied, Varied::Stamps);
    let polarities = || match stamped {
        true => vec![Polarity::Mixed],
        false => Vec::new(),
    };
    let owner = || match stamped {
        true => Qualifier::from(["elsewhere"]),
        false => Qualifier::default(),
    };

    let family = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new().into(),
        result_sort: Term::type_ground(),
        module: owner(),
        rep_public: !stamped,
        polarities: polarities(),
        variances: match varied {
            Varied::Variance => vec![Variance::Irrelevant],
            _ => Vec::new(),
        },
    };
    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(940, Some("held")),
                Term::intrinsic(match varied {
                    Varied::Field => Intrinsic::BoolType,
                    _ => Intrinsic::NatType,
                }),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: owner(),
        rep_public: !stamped,
        polarities: polarities(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            ..collision_definitions(Vec::new(), BTreeMap::from([(shadowed_family(), family)]))
        },
        [],
        BTreeMap::from([(kept_record(), record)]),
        [],
    )
}

/// What a fixture puts to the walk: a unit alone, or a program — a unit and the entry it closes with.
/// `induct Wrap.{u}(A: Type u): Type u | wrap(a: A) end`, carried with `variances`: a level only its parameter's type mentions.
pub(super) fn wrap_declaration(variances: Vec<Variance>) -> InductDecl {
    let level = Level::param(UniverseParam(0));
    let carrier = Free::local(30, Some("A"));
    let held = Free::local(31, Some("a"));

    InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::build(
            [(carrier, Term::type_at(level.clone()))],
            Telescope::done(()),
        ),
        constructors: vec![(
            Atom::from("wrap"),
            InductParam::new(
                Telescope::build(
                    [
                        (carrier, Term::type_at(level.clone())),
                        (held, Term::free_var(&carrier)),
                    ],
                    Vec::new(),
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(level),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    }
}

/// `induct Box.{u}: Type (u + 1) | box(T: Type u) end`, carried with `variances`: a level its payload's `Type` mentions.
pub(super) fn box_declaration(variances: Vec<Variance>) -> InductDecl {
    let level = Level::param(UniverseParam(0));
    let held = Free::local(32, Some("T"));

    InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("box"),
            InductParam::new(
                Telescope::build([(held, Term::type_at(level.clone()))], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(level.succ().expect("a parameter has a successor")),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    }
}

/// `induct Box.{u}: Type (u + 1) | box(T: F(Type u)) end` for an intrinsic carrier `former`, carried with `variances`: a level only the carrier's element mentions.
pub(super) fn carried_box_declaration(
    former: fn(Term) -> Intrinsic,
    variances: Vec<Variance>,
) -> InductDecl {
    let held = Free::local(34, Some("T"));
    let declaration = box_declaration(variances);
    let level = Level::param(UniverseParam(0));

    InductDecl {
        constructors: vec![(
            Atom::from("box"),
            InductParam::new(
                Telescope::build(
                    [(held, Term::intrinsic(former(Term::type_at(level))))],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        ..declaration
    }
}

/// The definition [`coercion_definitions`] puts at two instances of its family.
pub(super) fn coercion_name() -> Global {
    Global::Authored(Qualifier::from(["coerce"]))
}

/// One family and `coerce: (x: F.{from}(params)) -> F.{to}(params) = (x) => x`: the identity put at two instances of it.
pub(super) fn coercion_definitions(
    family: Global,
    declaration: InductDecl,
    params: Vec<Term>,
    from: Level,
    to: Level,
) -> Definitions {
    let at =
        |level: Level| Term::induct_type_at(family, [level], params.clone(), Vec::<Term>::new());

    definitions_of(
        vec![coercion(at(from), at(to))],
        BTreeMap::from([(family, declaration)]),
        BTreeMap::new(),
    )
}

/// A module of `items` over `induct_decls` and `struct_decls`, with nothing else in it.
fn definitions_of(
    items: Vec<Item>,
    induct_decls: BTreeMap<Global, InductDecl>,
    struct_decls: BTreeMap<Global, StructDecl>,
) -> Definitions {
    declaring(
        Definitions {
            items: items.into(),
        },
        induct_decls,
        struct_decls,
        [],
    )
}

/// `x: from` handed back at `to`: the identity under [`coercion_name`].
fn coercion(from: Term, to: Term) -> Item {
    let held = Free::local(40, Some("x"));

    authored(
        &coercion_name(),
        Term::func_type([(held, from.clone())], to),
        Term::func([(held, from)], Term::free_var(&held)),
    )
}

/// A scope holding one certified unit that declares `family` and nothing else.
pub(super) fn scope_of(family: Global, declaration: InductDecl) -> AlreadyJudged {
    already_judged(&definitions_of(
        Vec::new(),
        BTreeMap::from([(family, declaration)]),
        BTreeMap::new(),
    ))
}

/// `Held.{u}: result_sort | c(x: payload) end`, carried with `variances`, in a module of its own: a family whose one payload is an instance of another unit's.
pub(super) fn held_definitions(
    payload: Term,
    result_sort: Term,
    variances: Vec<Variance>,
) -> (Global, Definitions) {
    let family = Global::Authored(Qualifier::from(["Held"]));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build([(Free::local(50, Some("x")), payload)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    let definitions = definitions_of(
        Vec::new(),
        BTreeMap::from([(family, declaration)]),
        BTreeMap::new(),
    );

    (family, definitions)
}

/// `Box` beside `Fam.{u, w}: (D: Type w) -> Type | mk(): (Box.{u}) end` under `u + 1 ≤ w`, carried with `variances`, and `coerce` between `Fam.{0, 2}` and `Fam.{1, 2}` at the index `Box.{0}`: a level no payload and no index type of `Fam` mentions, which its one target puts at `Box`'s invariant position.
pub(super) fn index_target_definitions(variances: Vec<Variance>) -> (Global, Definitions) {
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    let family = Global::Authored(Qualifier::from(["Fam"]));
    let payload_level = Level::param(UniverseParam(0));
    let index_level = Level::param(UniverseParam(1));
    let box_at =
        |level: Level| Term::induct_type_at(boxed, [level], Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: vec![UniverseConstraint {
                lower: payload_level.succ().expect("a parameter has a successor"),
                upper: index_level.clone(),
                origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
            }],
        },
        arity: Telescope::done(Telescope::build(
            [(Free::local(33, Some("D")), Term::type_at(index_level))],
            (),
        )),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(Telescope::done(vec![box_at(payload_level)]), Vec::new()),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    let at = |level: u32| {
        Term::induct_type_at(
            family,
            [Level::constant(level), Level::constant(2)],
            Vec::<Term>::new(),
            [box_at(Level::zero())],
        )
    };

    let definitions = definitions_of(
        vec![coercion(at(0), at(1))],
        BTreeMap::from([
            (boxed, box_declaration(vec![Variance::Invariant])),
            (family, declaration),
        ]),
        BTreeMap::new(),
    );

    (family, definitions)
}

/// `struct Carrier.{u, v}(M: (Type u) -> Type v): Type max(u + 1, v) { pure: (A: Type u, x: A) -> M(A) }`, carried with `variances`, and `coerce` between its instances `from` and `to` at `M := (T) => Nat`: the level its method binds at beside the one only its parameter's type mentions.
pub(super) fn method_level_module(
    variances: Vec<Variance>,
    from: [u32; 2],
    to: [u32; 2],
) -> (Global, Definitions) {
    let name = Global::Authored(Qualifier::from(["Carrier"]));
    let method_level = Level::param(UniverseParam(0));
    let codomain_level = Level::param(UniverseParam(1));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let monad = Free::local(34, Some("M"));
    let argument = Free::local(35, Some("T"));
    let payload = Free::local(36, Some("A"));
    let value = Free::local(37, Some("x"));

    let declaration = StructDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::build(
            [(
                monad,
                Term::func_type(
                    [(argument, Term::type_at(method_level.clone()))],
                    Term::type_at(codomain_level.clone()),
                ),
            )],
            Telescope::build(
                [(
                    Free::local(38, Some("pure")),
                    Term::func_type(
                        [
                            (payload, Term::type_at(method_level.clone())),
                            (value, Term::free_var(&payload)),
                        ],
                        Term::apply(Term::free_var(&monad), [Term::free_var(&payload)]),
                    ),
                )],
                (),
            ),
        ),
        result_sort: Term::type_at(Level::max([
            method_level.succ().expect("a parameter has a successor"),
            codomain_level,
        ])),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    let at = |[method, codomain]: [u32; 2]| {
        Term::struct_type_at(
            name,
            [Level::constant(method), Level::constant(codomain)],
            [Term::func(
                [(argument, Term::type_at(Level::constant(method)))],
                nat.clone(),
            )],
        )
    };

    let definitions = definitions_of(
        vec![coercion(at(from), at(to))],
        BTreeMap::new(),
        BTreeMap::from([(name, declaration)]),
    );

    (name, definitions)
}

/// `Alias.{u} = (A: Type u) => A`, `Uses.{u}(A: Type u): Type u | c(x: Alias.{u}(A)) end` carried with `variances`, and `coerce` between `Uses.{0}(Nat)` and `Uses.{1}(Nat)`: a level a payload mentions only on an instance its reduction removes.
pub(super) fn alias_definitions(variances: Vec<Variance>) -> (Global, Definitions) {
    let alias = Global::Authored(Qualifier::from(["Alias"]));
    let family = Global::Authored(Qualifier::from(["Uses"]));
    let level = Level::param(UniverseParam(0));
    let scheme = UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    };
    let sort = Term::type_at(level.clone());
    let carrier = Free::local(41, Some("A"));
    let held = Free::local(42, Some("x"));

    let definition = Item::Let(Box::new(Definition {
        name: alias,
        kind: DefinitionKind::Authored,
        universe_context: scheme.clone(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::func_type([(carrier, sort.clone())], sort.clone()),
        body: Term::func([(carrier, sort.clone())], Term::free_var(&carrier)),
    }));
    let declaration = InductDecl {
        universe_context: scheme,
        arity: Telescope::build([(carrier, sort.clone())], Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build(
                    [
                        (carrier, sort.clone()),
                        (
                            held,
                            Term::apply(
                                Term::instance(alias, vec![level]),
                                [Term::free_var(&carrier)],
                            ),
                        ),
                    ],
                    Vec::new(),
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    let at = |level: u32| {
        Term::induct_type_at(
            family,
            [Level::constant(level)],
            [Term::intrinsic(Intrinsic::NatType)],
            Vec::<Term>::new(),
        )
    };

    let definitions = definitions_of(
        vec![definition, coercion(at(0), at(1))],
        BTreeMap::from([(family, declaration)]),
        BTreeMap::new(),
    );

    (family, definitions)
}

/// `Arm.{u, w}(b: Bool): Type w | c(T: match b : (_) => Type w | false => Nat | true => Type u end) end` under `u + 1 ≤ w`, carried with `variances`, and `coerce` between `Arm.{0, 2}(true)` and `Arm.{1, 2}(true)`: a level a payload's type states under one arm of a match stuck on the parameter, and nowhere else the walk reads.
pub(super) fn stuck_arm_definitions(variances: Vec<Variance>) -> (Global, Definitions) {
    let family = Global::Authored(Qualifier::from(["Arm"]));
    let payload_level = Level::param(UniverseParam(0));
    let sort_level = Level::param(UniverseParam(1));
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    let flag = Free::local(45, Some("b"));
    let held = Free::local(46, Some("T"));
    let payload = Term::bool_match(
        Term::free_var(&flag),
        None,
        Term::type_at(sort_level.clone()),
        Term::intrinsic(Intrinsic::NatType),
        Term::type_at(payload_level.clone()),
    );

    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: vec![UniverseConstraint {
                lower: payload_level.succ().expect("a parameter has a successor"),
                upper: sort_level.clone(),
                origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
            }],
        },
        arity: Telescope::build([(flag, bool_type.clone())], Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build([(flag, bool_type), (held, payload)], Vec::new()),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(sort_level),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    let at = |level: u32| {
        Term::induct_type_at(
            family,
            [Level::constant(level), Level::constant(2)],
            [Term::intrinsic(Intrinsic::Bool(true))],
            Vec::<Term>::new(),
        )
    };

    let definitions = definitions_of(
        vec![coercion(at(0), at(1))],
        BTreeMap::from([(family, declaration)]),
        BTreeMap::new(),
    );

    (family, definitions)
}

/// `Box` beside `Pick.{l, m}: Type (l + 1) = Box.{l}` and `Held.{u, v}: Type (u + 1) | c(x: Pick.{u, v}) end`, carried with `variances`, and `coerce` between the instances `from` and `to` of `Held`: a level a payload reaches only through a definition the walk unfolds, to another family's invariant position, beside one the unfolding drops.
pub(super) fn picked_module(
    variances: Vec<Variance>,
    from: [u32; 2],
    to: [u32; 2],
) -> (Global, Definitions) {
    let boxed = Global::Authored(Qualifier::from(["Box"]));
    let pick = Global::Authored(Qualifier::from(["Pick"]));
    let family = Global::Authored(Qualifier::from(["Held"]));
    let kept = Level::param(UniverseParam(0));
    let dropped = Level::param(UniverseParam(1));
    let scheme = UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: Vec::new(),
    };
    let lifted = Term::type_at(kept.succ().expect("a parameter has a successor"));

    let definition = Item::Let(Box::new(Definition {
        name: pick,
        kind: DefinitionKind::Authored,
        universe_context: scheme.clone(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: lifted.clone(),
        body: Term::induct_type_at(
            boxed,
            [kept.clone()],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    }));
    let declaration = InductDecl {
        universe_context: scheme,
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build(
                    [(
                        Free::local(47, Some("x")),
                        Term::instance(pick, vec![kept, dropped]),
                    )],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: lifted,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    };
    let at = |[kept, dropped]: [u32; 2]| {
        Term::induct_type_at(
            family,
            [Level::constant(kept), Level::constant(dropped)],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        )
    };

    let definitions = definitions_of(
        vec![definition, coercion(at(from), at(to))],
        BTreeMap::from([
            (boxed, box_declaration(vec![Variance::Invariant])),
            (family, declaration),
        ]),
        BTreeMap::new(),
    );

    (family, definitions)
}

/// `Wrap` beside `Ixd.{u}(A: Type u): (w: Wrap.{u}(A)) -> Type u | mk(a: A): (wrap.{u}(a)) end`, both irrelevant in their level, and `read: (x: Ixd.{0}(Nat)(wrap.{1}(3))) -> Nat` eliminating `x` with its one arm or with none: an index written at another instance of the level than its family's.
pub(super) fn excused_arm_definitions(with_arm: bool) -> (Global, Definitions) {
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));
    let family = Global::Authored(Qualifier::from(["Ixd"]));
    let level = Level::param(UniverseParam(0));
    let sort = Term::type_at(level.clone());
    let nat = Term::intrinsic(Intrinsic::NatType);
    let carrier = Free::local(43, Some("A"));
    let held = Free::local(44, Some("a"));
    let wrapped = |level: Level, carrier: Term, held: Term| {
        Term::variant_at(wrap, [level], [carrier], "wrap", [held])
    };

    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::build(
            [(carrier, sort.clone())],
            Telescope::build(
                [(
                    Free::local(45, Some("w")),
                    Term::induct_type_at(
                        wrap,
                        [level.clone()],
                        [Term::free_var(&carrier)],
                        Vec::<Term>::new(),
                    ),
                )],
                (),
            ),
        ),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(carrier, sort.clone()), (held, Term::free_var(&carrier))],
                    vec![wrapped(
                        level,
                        Term::free_var(&carrier),
                        Term::free_var(&held),
                    )],
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: vec![Variance::Irrelevant],
    };

    let subject = Free::local(46, Some("x"));
    let index = Free::local(47, Some("i"));
    let scrutinee = Free::local(48, Some("s"));
    let bound = Free::local(49, Some("b"));
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let subject_type = Term::induct_type_at(
        family,
        [Level::zero()],
        [nat.clone()],
        [wrapped(Level::constant(1), nat.clone(), three)],
    );
    let arms = match with_arm {
        true => vec![(
            "mk",
            vec![(Plicity::Explicit, bound)],
            Term::free_var(&bound),
        )],
        false => Vec::new(),
    };
    let read = authored(
        &Global::Authored(Qualifier::from(["read"])),
        Term::func_type([(subject, subject_type.clone())], nat.clone()),
        Term::func(
            [(subject, subject_type)],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(Many(2), &[&index, &scrutinee], nat),
                arms,
                None,
            ),
        ),
    );

    let definitions = definitions_of(
        vec![read],
        BTreeMap::from([
            (wrap, wrap_declaration(vec![Variance::Irrelevant])),
            (family, declaration),
        ]),
        BTreeMap::new(),
    );

    (family, definitions)
}

/// `induct Fam.{i, o | i + 1 ≤ o} : Type o | mk(A : Type i, f : (A) -> Fam.{i, o}) end`, carried with `variances`: Russell's carrier, universe-polymorphic in the level its payload holds and in the level it stands at.
///
/// The second level stands in the result sort and at `Fam`'s own position in the forward field's codomain, and nowhere else the walk reads, so the fixpoint grants it irrelevant: the two instances' telescopes convert entry by entry, which is what irrelevance claims. What keeps the self-application out is the payload's own level, which the walk calls invariant and the declaration's constraint holds a rung below the sort.
pub(super) fn russell_carrier(family: Global, variances: Vec<Variance>) -> InductDecl {
    let payload_level = Level::param(UniverseParam(0));
    let own_level = Level::param(UniverseParam(1));
    let carrier = Free::local(70, Some("A"));
    let forward = Free::local(71, Some("f"));
    let member = Free::local(72, Some("x"));

    InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            quality_count: 0,
            constraints: vec![UniverseConstraint {
                lower: payload_level.succ().expect("a parameter has a successor"),
                upper: own_level.clone(),
                origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
            }],
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [
                        (carrier, Term::type_at(payload_level.clone())),
                        (
                            forward,
                            Term::func_type(
                                [(member, Term::free_var(&carrier))],
                                russell_at(family, [payload_level, own_level.clone()]),
                            ),
                        ),
                    ],
                    Vec::new(),
                ),
                vec![Plicity::Explicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(own_level),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
    }
}

/// [`russell_carrier`]'s family at `levels`.
pub(super) fn russell_at(family: Global, levels: [Level; 2]) -> Term {
    Term::induct_type_at(family, levels, Vec::<Term>::new(), Vec::<Term>::new())
}

/// The name [`russell_definitions`] and its siblings put [`russell_carrier`] under.
pub(super) fn russell_name() -> Global {
    Global::Authored(Qualifier::from(["Fam"]))
}

/// [`russell_carrier`] beside `coerce : (x : Fam.{0, from}) -> Fam.{0, to} = (x) => x`: the identity across two instances apart in the level the walk calls irrelevant alone, both well formed since each satisfies `0 + 1 ≤ o`.
pub(super) fn russell_coercion_definitions(
    variances: Vec<Variance>,
    from: u32,
    to: u32,
) -> Definitions {
    let family = russell_name();
    let at = |level: u32| russell_at(family, [Level::zero(), Level::constant(level)]);

    definitions_of(
        vec![coercion(at(from), at(to))],
        BTreeMap::from([(family, russell_carrier(family, variances))]),
        BTreeMap::new(),
    )
}

/// [`russell_carrier`] beside `russell : Fam.{0, 1} = Fam.{0, 1}/mk(Fam.{0, 1}, (x) => x)`: the family's own type, a `Type 1`, handed to the payload that stands at `Type 0`.
///
/// Russell's self-application over a carrier whose own level conversion compares at nothing. The irrelevance makes `Fam.{0, 1}` and `Fam.{0, o}` one type for every `o`, so no instance of the family has to be spelled at the sort the payload wants; what the payload is held to is the sort the declaration states at the instance written, which the irrelevance does not move.
pub(super) fn russell_definitions(variances: Vec<Variance>) -> (Global, Definitions) {
    let family = russell_name();
    let name = Global::Authored(Qualifier::from(["russell"]));
    let fam = russell_at(family, [Level::zero(), Level::constant(1)]);
    let member = Free::local(73, Some("x"));
    let body = Term::variant_at(
        family,
        [Level::zero(), Level::constant(1)],
        Vec::<Term>::new(),
        "mk",
        [
            fam.clone(),
            Term::func([(member, fam.clone())], Term::free_var(&member)),
        ],
    );

    let definitions = definitions_of(
        vec![authored(&name, fam, body)],
        BTreeMap::from([(family, russell_carrier(family, variances))]),
        BTreeMap::new(),
    );

    (name, definitions)
}

/// [`russell_carrier`] beside `lowered : Type 1 = Fam.{0, 2}`: the higher of two instances the walk calls one type, put at the sort the lower one states.
pub(super) fn lowered_sort_definitions(variances: Vec<Variance>) -> (Global, Definitions) {
    let family = russell_name();
    let name = Global::Authored(Qualifier::from(["lowered"]));
    let definitions = definitions_of(
        vec![authored(
            &name,
            Term::type_at(Level::constant(1)),
            russell_at(family, [Level::zero(), Level::constant(2)]),
        )],
        BTreeMap::from([(family, russell_carrier(family, variances))]),
        BTreeMap::new(),
    );

    (name, definitions)
}

pub(super) trait Walked {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>);
}

impl Walked for Definitions {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>) {
        (self, None)
    }
}

impl Walked for Program {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>) {
        (&self.definitions, Some(&self.entrypoint))
    }
}

/// The whole-module walk over a bare fixture, the evidence wrapper bypassed: the modules built here are adversarial by design — some deliberately carry what `Zonked` refuses — and the kernel's own refusals are the thing under test, so nothing may stand between a forged module and the walk.
pub(super) fn fixture_verdicts(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    fixture_certified(fixture, budget, globals, syntax).verdicts
}

/// [`fixture_verdicts`], with the record the walk leaves — for a fixture that mounts what one walk concluded beneath another.
pub(super) fn fixture_certified(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Rechecked {
    let (definitions, entry) = fixture.parts();

    super::verdicts_from(Kernel::new(budget, syntax), definitions, entry, globals)
}

pub(super) const BUDGET: u64 = 1_000_000;

pub(super) fn global(symbol: &str) -> Global {
    Global::Authored(Qualifier::from([symbol]))
}

pub(super) fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub(super) fn literal(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

/// `alias : Type = <alias>`, `three : alias = 3`, `identity : (Nat) -> Nat = (x) => <returned>`, and `five : Nat = identity(5)`: `three` unfolds `alias`, and `five` types `identity` and unfolds nothing.
pub(super) fn reading_definitions(alias: Term, returned: impl Fn(&Free) -> Term) -> Definitions {
    let x = Free::local(10, Some("x"));

    Definitions {
        items: vec![
            authored(&global("alias"), Term::type_ground(), alias),
            authored(
                &global("three"),
                Term::instance(global("alias"), Universes::default()),
                literal(3),
            ),
            authored(
                &global("identity"),
                Term::func_type([(x, nat())], nat()),
                Term::func([(x, nat())], returned(&x)),
            ),
            authored(
                &global("five"),
                nat(),
                Term::apply(
                    Term::instance(global("identity"), Universes::default()),
                    [literal(5)],
                ),
            ),
        ]
        .into(),
    }
}

/// The module every fixture here is an edit of: `alias` is `Nat` and `identity` returns its argument.
pub(super) fn written() -> Definitions {
    reading_definitions(nat(), Term::free_var)
}

/// `module` walked with nothing in scope and no baseline: the walk that files a baseline's record.
pub(super) fn walked(definitions: &Definitions) -> Rechecked {
    let rechecked = fixture_certified(definitions, BUDGET, &Globals::default(), SYNTAX);
    assert_eq!(rechecked.verdicts, Vec::new());

    rechecked
}

/// `module` walked over `baseline` as `certification` records it.
pub(super) fn walked_over(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
) -> Rechecked {
    walked_among(definitions, baseline, certification, None)
}

/// [`walked_over`], taking what it can from what `store` holds.
pub(super) fn walked_among(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
    store: Option<&dyn Candidates>,
) -> Rechecked {
    let (places, now) = (Places::of(baseline), Places::of(definitions));
    let globals = Globals::default();

    super::verdicts_into(
        &mut Kernel::new(BUDGET, SYNTAX),
        definitions,
        None,
        &globals,
        Some(super::Over {
            baseline: Baseline {
                definitions: baseline,
                places: &places,
                certification,
            },
            places: &now,
            candidates: store,
        }),
    )
}

/// `certification` with `symbol`'s entry replaced by `entry`'s answer for it.
pub(super) fn doctored(
    certification: &Certification,
    symbol: &str,
    entry: impl Fn(Certified) -> Certified,
) -> Certification {
    Certification::of(
        certification.iter().map(|(name, totality)| {
            let certified = Certified {
                totality,
                reads: certification
                    .reads(name)
                    .cloned()
                    .expect("a record holds the reads of every definition it classifies"),
            };

            match *name == global(symbol) {
                true => (*name, entry(certified)),
                false => (*name, certified),
            }
        }),
        [],
        [],
        [],
    )
}

/// A record of `written` in which `five` is marked: its reads hold `alias`'s signature, which no judgment of `five` reads. A walk that hands the mark back adopted the entry, and one that judged `five` filed its own reads in its place.
pub(super) fn marked() -> Certification {
    doctored(&walked(&written()).certification, "five", |certified| {
        Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("identity"), global("alias")]),
                bodies: BTreeSet::new(),
            },
            ..certified
        }
    })
}

/// `module` with its items held as a version's: a module made of it by replacing an item shares every other item with it, as a recompile's module shares the ones it kept with its baseline's.
pub(super) fn versioned(definitions: Definitions) -> Definitions {
    Definitions {
        items: definitions.items.version(),
    }
}

/// `module`, a version, with the item declaring `name` replaced by `item`.
pub(super) fn replaced(definitions: &Definitions, name: &str, item: Item) -> Definitions {
    let place = Places::of(definitions)
        .place(&global(name))
        .expect("the module declares the name")
        .item();
    let mut items = definitions.items.clone();
    items.replace(place, item);

    Definitions { items }
}

/// `Type 5`, `Type 4` and `Type 0` as hand-built universes.
pub(super) fn type_at(level: u32) -> Term {
    Term::type_at(Level::constant(level))
}

/// `name : type_ = body`, stating the sort `value_sort` its value inhabits ([`curios_core::Definition::value_sort`]): the field a type alias carries and the one `read_digest` leaves out.
pub(super) fn aliased(name: &Global, type_: Term, body: Term, value_sort: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            value_sort: Some(value_sort),
            ..*definition
        })),
        item => item,
    }
}

/// Three aliases in a chain, each saying the sort its value inhabits: `H : Type 5 = h_body` saying `h`, `K : Type 5 = H` saying `k`, and `L : Type 5 = K` saying `l`.
///
/// The walk states `L`'s value sort by `Sort::of(K)`, which reads `K`'s *published* sort with nothing run, so what it states of `L` is a function of what it stated of `K` and of nothing else about `K`, and nothing the module says of a sort.
pub(super) fn chained_sort_definitions(h_body: Term, h: Term, k: Term, l: Term) -> Definitions {
    Definitions {
        items: vec![
            aliased(&global("H"), type_at(5), h_body, h),
            aliased(
                &global("K"),
                type_at(5),
                Term::instance(global("H"), Universes::default()),
                k,
            ),
            aliased(
                &global("L"),
                type_at(5),
                Term::instance(global("K"), Universes::default()),
                l,
            ),
        ]
        .into(),
    }
}
