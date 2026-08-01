//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    super::{derived_binder_floor, recheck_module_verdicts},
    crate::KernelError,
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Definition, DefinitionKind, Free, Global, InductDecl, InductParam, Item, Level, Many,
        Module, Prim, RecGroup, RecMemberScopes, Scope, Telescope, Term, Totality,
        UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
        UniverseParam,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// The floor must clear every local a module's terms mention, whatever `Module::binder_floor` claims.
///
/// A binder the kernel mints while comparing under a telescope or eta-contracting aliases a free local the moment the floor is too low, and two terms that differ stop being distinguishable. The carried number is the elaborator's word and nothing checks it, so the walk derives its own and the caller takes the larger.
#[test]
fn the_floor_clears_every_local_a_term_mentions() {
    let mentioned = Free::local(4_242, Some("y"));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::prim(Prim::NatType),
        body: Term::free_var(&mentioned),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        // The understated claim the walk must not believe.
        binder_floor: 0,
        type_: None,
        body: Term::prim(Prim::NatType),
    };

    assert_eq!(derived_binder_floor(&module), 4_243);
}

/// A declaration's universe context is *assumed* while checking it, so an unsatisfiable one is a hypothesis set that proves anything.
///
/// `Kernel::assume_universes` takes the item's own constraints as given, and `entails` answers `≤` questions under them — so a context containing `u + 1 ≤ u` lets every level relation through, and `check_instance` stops discharging anything. Deciding satisfiability runs a solver and lives in `curios-elab`; this asks whether the kernel notices regardless.
#[test]
fn an_unsatisfiable_universe_context_is_refused() {
    let contradiction = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .succ()
            .expect("level has a successor"),
        upper: Level::param(UniverseParam(0)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };
    let universe_context = UniverseContext {
        parameter_count: 1,
        constraints: vec![contradiction],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::prim(Prim::NatType),
        body: Term::prim(Prim::Nat(curios_core::Nat::new(0usize))),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body: Term::prim(Prim::NatType),
    };

    assert!(
        !recheck_module_verdicts(&module, 1_000_000).is_empty(),
        "the kernel assumed a contradiction as a hypothesis without noticing",
    );
}

/// A constraint may only mention parameters the context declares.
///
/// A context is closed: universe polymorphism belongs to declarations, so there is no enclosing scheme whose parameters a constraint could still reference. One that names `P3` while declaring a single parameter is not a stricter hypothesis but a meaningless one — instantiation substitutes an argument vector of the declared length, and a reference past its end has nothing to become. The elaborator refuses this as an escaping level; the kernel assumes the context, so it must refuse it too.
#[test]
fn a_constraint_naming_an_undeclared_parameter_is_refused() {
    let escaping = UniverseConstraint {
        lower: Level::param(UniverseParam(3)),
        upper: Level::param(UniverseParam(0)),
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    };
    let universe_context = UniverseContext {
        parameter_count: 1,
        constraints: vec![escaping],
    };

    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::prim(Prim::NatType),
        body: Term::prim(Prim::Nat(curios_core::Nat::new(0usize))),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body: Term::prim(Prim::NatType),
    };

    assert!(
        !recheck_module_verdicts(&module, 1_000_000).is_empty(),
        "the kernel assumed a constraint about a parameter the declaration does not have",
    );
}

/// The derivation a `Prop` carrying a type made possible, as a whole module.
///
/// `Box : Prop | mk(a : Type 0)` is a legal declaration — `Prop` is impredicative, so its payload carries no size condition, and the large-elimination guard is what is supposed to keep that sound. The guard admitted `unbox` because `carries_information` reported a universe-typed payload as carrying nothing, on the reasoning that erasure deletes a type either way. Every step after that is ordinary: irrelevance makes `mk(A)` and `mk(B)` convertible at `Box`, so `refl` inhabits `Eq(Box, mk(A), mk(B))`; congruence through `unbox` carries that to `Eq(Type 0, A, B)` for *any* two types; and transport — the licensed singleton case, `refl`'s payload being pinned by its own targets — turns `()` into a proof of `False`.
///
/// While the hole was open `recheck_module_verdicts` returned zero refusals for exactly this module, with the evaluation memos on and off, and `check_induct_decl` accepted the declaration. It never compiled and never ran: `curios-elab`'s `singleton_eliminable` refused `unbox` at every surface spelling, which is what kept the certifier's copy of the rule unobserved. The fixtures in `crate::kernel::infer::eliminate::tests` pin the predicate; this pins the consequence, and it is the reason the predicate's two call sites are worth guarding separately.
#[test]
fn a_derivation_through_a_type_carrying_proposition_is_refused() {
    let verdicts = recheck_module_verdicts(&forgery(), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::LargeElimination(_))),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// A top-level definition, as `recheck_module_verdicts` binds one.
fn authored(name: &Global, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: name.clone(),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        root: RootId::Entry,
        // Non-recursive and `Exit`-free, so the honest flag; `partial_definitions` recomputes it.
        totality: Totality::Total,
        type_,
        body,
    })
}

/// [`authored`] for a body that recurses without descending. `partial_definitions` recomputes the flag and reports a recorded `Total` it disagrees with, so a partial body must say so.
fn authored_partial(name: &Global, type_: Term, body: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Definition {
            totality: Totality::Partial,
            ..definition
        }),
        item => item,
    }
}

/// A nullary `Prop`-sorted family: `False` itself, and the shape `Box` takes but for its payload.
fn proposition(constructors: Vec<(Atom, InductParam)>) -> InductDecl {
    InductDecl {
        universe_context: UniverseContext::default(),
        params: Telescope::done(()),
        indices: Telescope::done(()),
        constructors,
        result_sort: Term::prop(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// The module the doc comment above describes: three declarations, and the five definitions that close on `False`.
fn forgery() -> Module {
    let type_0 = Term::type_ground();
    let type_1 = Term::type_at(Level::zero().succ().expect("level zero has a successor"));

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
            telescope: Telescope::build([(payload, type_0.clone())], box_type.clone()),
            plicities: vec![Plicity::Explicit],
        },
    )]);

    // induct Eq(A : Type 1) : (x : A, y : A) -> Prop | refl(z : A) : (z, z) end
    let carrier = Free::local(20, Some("A"));
    let left = Free::local(21, Some("x"));
    let right = Free::local(22, Some("y"));
    let value = Free::local(23, Some("z"));
    let mut equality_decl = proposition(vec![(
        Atom::from("refl"),
        InductParam {
            telescope: Telescope::build(
                [
                    (carrier.clone(), type_1.clone()),
                    (value.clone(), Term::free_var(&carrier)),
                ],
                Term::induct_type(
                    equality_name.clone(),
                    [Term::free_var(&carrier)],
                    [Term::free_var(&value), Term::free_var(&value)],
                ),
            ),
            plicities: vec![Plicity::Implicit, Plicity::Explicit],
        },
    )]);
    equality_decl.params = Telescope::build([(carrier.clone(), type_1.clone())], ());
    equality_decl.indices = Telescope::build(
        [
            (carrier.clone(), type_1.clone()),
            (left, Term::free_var(&carrier)),
            (right, Term::free_var(&carrier)),
        ],
        (),
    );

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
        type_: Some(false_type),
        body: Term::free_var(&Free::from(&forged_name)),
    }
}

/// `rec f : Absurd = f` reached as a member selection, which must be refused by the same rule the block spelling is refused by.
///
/// A `rec` group is checked by [`infer`](crate::infer)'s `Rec` arm: every member's declared type is verified to be a type, every body is checked against it, and a group with a proof-typed or type-yielding member must descend, since erasure deletes such a member wholesale and a non-descending one proves anything. A selection is that same node with a tail that picks one member ([`Term::rec_proj`]), so it reaches that arm and is held to that rule.
///
/// It did not always. The selection used to be `Subterm::RecMember`, a node carrying its own copy of the group — well-formed standing alone, and so gated by no scope — and its arm answered `group.member_type(index)` outright, checking nothing. Both modules below were certified with **zero refusals** while that was the representation, and `check` accepted the selection against `Absurd` directly. The second is the sharper of the two: its group never recurses at all, so no totality rule is even in play, and what went unchecked was simply whether the body inhabits the type the group claims for it. `Absurd` is a proposition with no constructors, so either module was a closed inhabitant of one.
///
/// Neither obligation caught it either. (V) seeds from the kernel's own typing, and `locally_partial` asked `group_totality` for a `Subterm::Rec` node only — so the walk descended into a selection's member scopes and found a bound variable rather than a recursion. Unifying the two spellings closed that too, without a second rule: there is now one node for the walk to recognize.
///
/// Reachable from no surface program — `curios-elab` builds a checked node — which is why this belongs here rather than in `curios/src/tests`, and why nothing in the corpus could have found it.
///
/// The control is the same construction at a *legal* group, and it is not decoration. General recursion at a relevant type is the language's design, and the group is self-referential, so a rule that certified the group at every selection *by opening its bodies over the group itself* would re-enter the check it is already inside and never terminate. It must stay accepted, and it must stay accepted quickly.
#[test]
fn a_recursive_member_is_certified_only_with_its_group() {
    for (label, body, refusal) in [
        (
            "a non-descending proof",
            Term::free_var(&member()),
            "does not descend",
        ),
        (
            "a body that is not of the declared type",
            Term::tuple(Vec::<Term>::new()),
            "expected",
        ),
    ] {
        let verdicts = recheck_module_verdicts(&selection_module(body), 1_000_000);

        assert!(
            verdicts
                .iter()
                .any(|verdict| verdict.error.to_string().contains(refusal)),
            "{label}: the kernel certified a closed inhabitant of an empty proposition: {verdicts:?}",
        );
    }
}

/// The control: a legal group reached through the same selection stays accepted.
///
/// `rec f : (Nat) -> Nat = (n) => f(n)` does not descend, and that is legal — a program that loops is a program rather than an unsoundness, and the obligations bite only where erasure deletes. Refusing every member selection would close the witness above and take this with it.
#[test]
fn a_member_of_a_legal_group_is_still_accepted() {
    let f = member();
    let n = Free::local(901, Some("n"));
    let nat = Term::prim(Prim::NatType);

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

    let module = Module {
        items: vec![authored_partial(
            &Global::Authored(Qualifier::from(["ok"])),
            Term::func_type([(n, nat.clone())], nat),
            Term::rec_proj(group, 0),
        )],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    };

    assert_eq!(
        recheck_module_verdicts(&module, 1_000_000),
        Vec::new(),
        "general recursion at a relevant type is legal however it is spelled",
    );
}

/// The binder a one-member group's scopes close over.
fn member() -> Free {
    Free::local(900, Some("f"))
}

/// `let bad : Absurd = <the member selection of `rec f : Absurd = body`>`, with `Absurd` an empty proposition.
fn selection_module(body: Term) -> Module {
    let name = Global::Authored(Qualifier::from(["Absurd"]));
    let absurd = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let f = member();

    let group = RecGroup::new(vec![RecMemberScopes {
        type_: Scope::close(Many(1), &[&f], absurd.clone()),
        body: Scope::close(Many(1), &[&f], body),
    }]);

    Module {
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
        type_: Some(absurd),
        body: Term::free_var(&Free::from(&Global::Authored(Qualifier::from(["bad"])))),
    }
}

/// A constructor's *index target* is registry data that no judgment in the walk reads.
///
/// `check_sizing` walks a constructor telescope's **domains** — each must sit at or below the family's declared level — and stops there. The terminal the telescope ends in, `Family(params, indices)`, is never visited, so the index targets a constructor states reach index inversion and the arm rule without any judgment having typed them. [`check_induct_decl`](crate::check_induct_decl) says so outright: the rest "falls out of the ordinary item walk, because a declaration lowers to a `rec` group of real definitions", and for a module the elaborator built that holds — the constructor wrapper's declared type ends in that terminal, so checking the wrapper's body against it types the targets.
///
/// The kernel never confirmed the lowering exists. The module below carries the registry entry and no items at all, so nothing types the terminal, and its index target is an unsolved metavariable — precisely what `zonk_module` promises has been eliminated, in the one position the kernel's walk did not re-derive. That made the guarantee the elaborator's word rather than the kernel's, which is the dependency the two-checker split exists to remove.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for this module. It is reachable from no surface program — the elaborator builds registry and bindings from one declaration — which is why it belongs here rather than in `curios/src/tests`, and why nothing in the corpus could have found it. The diagnostic is asserted rather than bare failure, since a module this small could fail for unrelated reasons and still look guarded.
///
/// The control is [`a_registry_index_target_of_a_real_term_is_accepted`], which is the same module with the metavariable replaced by a literal: the pass must refuse an elaboration-only node, not every registry entry.
#[test]
fn a_registry_index_target_is_checked_rather_than_believed() {
    let verdicts = recheck_module_verdicts(&indexed_module(Term::metavar(7_usize)), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotCore(_))),
        "the kernel certified a module carrying an unsolved metavariable: {verdicts:?}",
    );
}

/// The control for the fixture above: a registry entry whose index target is a real term stays accepted.
#[test]
fn a_registry_index_target_of_a_real_term_is_accepted() {
    let target = Term::prim(Prim::Nat(curios_core::Nat::new(0usize)));

    assert_eq!(
        recheck_module_verdicts(&indexed_module(target), 1_000_000),
        Vec::new(),
        "the boundary pass refused a registry entry that carries nothing elaboration-only",
    );
}

/// A one-constructor indexed family whose constructor aims at `target`, carried as a registry entry with no items lowering it.
fn indexed_module(target: Term) -> Module {
    let family = Global::Authored(Qualifier::from(["Indexed"]));
    let constructed = Term::induct_type(family.clone(), Vec::<Term>::new(), [target]);

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        params: Telescope::done(()),
        indices: Telescope::done(()),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::done(constructed),
                plicities: Vec::new(),
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };

    Module {
        items: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}
