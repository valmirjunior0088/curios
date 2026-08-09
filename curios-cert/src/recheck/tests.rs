//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    super::recheck_module_verdicts,
    crate::{Erased, KernelError, derived_binder_floor},
    curios_abi::{ForeignFunction, WireSignature, WireType},
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Definition, DefinitionKind, Free, Func, FuncType, Global, InductDecl, InductParam,
        Intrinsic, Item, Level, Many, Module, Nat, RecGroup, RecMemberScopes, Scope, StructDecl,
        StructType, Subterm, Telescope, Term, Totality, UniverseConstraint, UniverseConstraintKind,
        UniverseConstraintOrigin, UniverseContext, UniverseMetaId, UniverseParam,
    },
    std::{
        collections::{BTreeMap, BTreeSet},
        panic::{AssertUnwindSafe, catch_unwind},
        sync::Arc,
    },
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
        type_: Term::intrinsic(Intrinsic::NatType),
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
        body: Term::intrinsic(Intrinsic::NatType),
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
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
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
        body: Term::intrinsic(Intrinsic::NatType),
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
        type_: Term::intrinsic(Intrinsic::NatType),
        body: Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
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
        body: Term::intrinsic(Intrinsic::NatType),
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
        // Non-recursive and `ProcExit`-free, so the honest flag; `partial_definitions` recomputes it.
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
        arity: Telescope::done(Telescope::done(())),
        constructors,
        result_sort: Term::prop(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// `induct Eq(@A : Type 1) : (x : A, y : A) -> Prop | refl(z : A) : (z, z) end` — the equality the forgeries transport along.
fn equality_declaration() -> InductDecl {
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
fn forgery() -> Module {
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
    let target = Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));

    assert_eq!(
        recheck_module_verdicts(&indexed_module(target), 1_000_000),
        Vec::new(),
        "the boundary pass refused a registry entry that carries nothing elaboration-only",
    );
}

/// A one-constructor indexed family whose constructor aims at `target`, carried as a registry entry with no items lowering it.
fn indexed_module(target: Term) -> Module {
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

/// A *level* holding an unsolved universe metavariable is elaboration residue, and nothing in the walk refused one.
///
/// `validate_universes` is where the elaborator eliminates them, and its `validate_bound_universes` half is what walks a term's own levels and rejects a metavariable. The kernel's counterpart, [`closed`](crate::closed), inspects a [`UniverseContext`]'s *constraints* — the only place in this crate that looked for a meta level at all — and never a level sitting inside a term. So `Sort::of` read `Type(?u)` and answered `Type(?u + 1)`, and the walk carried the residue rather than refusing it.
///
/// Both positions below were certified. The registry one is the shape the metavariable pass beside it already covers: registry data no judgment types. The *definition type* one is sharper, because that position is fully walked — `check_definition` asks `Sort::of` for it and then checks the body against it — so this was not a coverage gap in which terms the walk reaches. It was the level algebra having no opinion about an unsolved level at all, which is why refusing it belongs at the boundary rather than inside a judgment.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for each of the two modules. Neither is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator — which is why they are built here and why nothing in the corpus could have found this. An unsolved level is not itself a closed inhabitant of `False`; what it is, is a level every cumulativity question is then decided against, with `entails` answering about a variable that no longer has a solver behind it. The refusal is the safe direction and the one the perimeter row already claims.
///
/// The control is [`a_ground_level_in_the_same_positions_is_accepted`], the same two modules at `Type 0`: the pass must refuse residue, not every level.
#[test]
fn a_level_holding_an_unsolved_universe_metavariable_is_refused() {
    let residue = Level::meta(UniverseMetaId::from(0usize));

    for (label, module) in [
        ("a definition's declared type", level_definition(&residue)),
        ("a registry entry's result sort", level_registry(&residue)),
    ] {
        let verdicts = recheck_module_verdicts(&module, 1_000_000);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::NotCore(_))),
            "{label}: the kernel certified a module carrying an unsolved universe metavariable: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: the same two positions at a ground level stay accepted.
#[test]
fn a_ground_level_in_the_same_positions_is_accepted() {
    let ground = Level::zero();

    for (label, module) in [
        ("a definition's declared type", level_definition(&ground)),
        ("a registry entry's result sort", level_registry(&ground)),
    ] {
        assert_eq!(
            recheck_module_verdicts(&module, 1_000_000),
            Vec::new(),
            "{label}: the boundary pass refused a level that holds no residue",
        );
    }
}

/// `let held : Type(level) = Nat`, as a whole module.
fn level_definition(level: &Level) -> Module {
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::type_at(level.clone()),
        body: Term::intrinsic(Intrinsic::NatType),
    };

    Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// A constructor-free family declared at `Type(level)`, carried as a registry entry.
fn level_registry(level: &Level) -> Module {
    let family = Global::Authored(Qualifier::from(["Levelled"]));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        result_sort: Term::type_at(level.clone()),
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

/// A constructor's index target may be a *proof*, and no judgment in the walk types it.
///
/// Definitional proof irrelevance accepts without inspecting either term, and DESIGN.md states plainly what makes that correct: every inhabitant of a proposition is total, which is (V)'s job. The premise that argument needs is that **(V) inspects every `Prop`-typed term in the accepted module** — and for the kernel's own (V), seeded from its own typing rather than from the elaborator's hook, that reduces to whether the walk types every such term.
///
/// For a time it did not, and a constructor's index target was the gap. `partial_definitions` iterates `module.items` and nothing else, and the module below has none; `derived_binder_floor` is the only other pass that reads a registry entry, and it collects free variables rather than partiality. Nor did the item walk reach these terms — `check_sizing` walks a constructor telescope's domains and stops at the terminal — so a target was typed by nothing, and `check_group`'s local gate, which refuses a proof-typed member whose group does not descend, never fired on a group no judgment met.
///
/// So the module below states its constructor's index at `rec p : Held = p`, a closed non-descending inhabitant of a proposition, and `recheck_module_verdicts` returned **zero refusals** for it. Irrelevance identifies that target with any other proof of `Held`, which is the identification (V) exists to prevent. Reachable from no surface program — the elaborator builds registry and bindings from one declaration and types the targets through the constructor wrappers it lowers — which is why it is built here.
///
/// What closed it is clause 9 of `check_induct_decl`, in `check_constructed`: every index target must inhabit the index telescope at the constructor's own parameters, which *checks* the target instead of scanning past it. That is the shape this comment predicted while the hole was open — partiality is not syntactic, so establishing it takes typing the registry rather than reading it — and typing `rec p : Held = p` against `Held` is what puts `check_group`'s gate in front of it. Nothing about the fixture changed; the clause arrived and the verdict followed.
///
/// The control is [`a_real_proof_in_a_registry_index_target_is_accepted`], the same declaration aimed at a genuine `qed()`: what establishes the target must refuse a diverging proof without refusing an ordinary one. The assertion pins the refusing item as well as the diagnostic, because this module declares two families and "does not descend" reaching the verdict list from either would otherwise read as a pass.
#[test]
fn a_partial_proof_in_a_registry_index_target_is_refused() {
    let family = Global::Authored(Qualifier::from(["Indexed"]));
    let verdicts = recheck_module_verdicts(&indexed_by_proof(true), 1_000_000);

    assert!(
        verdicts.iter().any(|verdict| {
            verdict.name.as_ref() == Some(&family)
                && verdict.error.to_string().contains("does not descend")
        }),
        "the kernel certified a non-descending proof standing as a constructor's index target: {verdicts:?}",
    );
}

/// The control for the fixture above: an ordinary proof in the same position stays accepted.
#[test]
fn a_real_proof_in_a_registry_index_target_is_accepted() {
    assert_eq!(
        recheck_module_verdicts(&indexed_by_proof(false), 1_000_000),
        Vec::new(),
        "a constructor aimed at a genuine proof was refused",
    );
}

/// A family indexed by the proposition `Held`, whose one constructor states its index either as a diverging `rec` or as `Held/qed()`.
fn indexed_by_proof(diverging: bool) -> Module {
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
        root: RootId::Entry,
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
        items: Vec::new(),
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(held_name, proposition(vec![qed])), (family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// A declared result sort that merely *reduces* to `Prop` silences the index guard that reads it syntactically.
///
/// `Sort::of` decides irrelevance by reducing `result_sort`, and so do `check_signature` and `check_non_informative`. Index inversion does not: its `Prop`-valued guard — the rule that stops a proposition's constructors from being told apart, because irrelevance says they are the same value — matches `Subterm::Prop` on the nose. A family declared at `((s : Type 0) => s)(Prop)` is therefore a proposition to every consumer except the one whose silence is unsound.
///
/// The consequence below is the vacuous-elimination route. `Two` is that family, with constructors `a` and `b`; `Held(t : Two)` has one constructor targeting `Two/a()`. Irrelevance makes `Two/a()` and `Two/b()` convertible, so `Held/mk()` inhabits `Held(Two/b())` — and then eliminating that value with *no arms at all* is accepted, because inversion clashes `Two/b()` against `Two/a()` and reports the only constructor impossible. The motive is `False`.
///
/// While the hole was open `recheck_module_verdicts` returned zero refusals for exactly this module, certifying `let forged : False` from a value that exists. It never compiled: the surface grammar admits only the literal keywords `Type` and `Prop` after a declaration's `:`, so no `.crs` file can spell the sort, and the elaborator builds no such entry.
///
/// The control is the same module with `Two` at `Type 0`, where the clash is genuine and the empty elimination must stay accepted — general vacuous elimination is how an indexed family rules its impossible cases out, and refusing it is how this hole would be shut with a brick.
#[test]
fn a_result_sort_that_only_reduces_to_a_sort_is_refused() {
    let verdicts = recheck_module_verdicts(&aliased_sort_forgery(), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotASort(_))),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_at_a_relevant_index_is_still_accepted() {
    let verdicts = recheck_module_verdicts(&relevant_index_control(), 1_000_000);

    assert!(
        verdicts.is_empty(),
        "an arm the index targets genuinely clash with was refused: {verdicts:?}",
    );
}

/// `Two`, at whatever sort its caller declares it, and `Held(t : Two)` with one constructor targeting `Two/a()`.
fn clashing_index_decls(carrier_sort: Term) -> (Global, Global, BTreeMap<Global, InductDecl>) {
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
        root: RootId::Entry,
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
        root: RootId::Entry,
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
fn aliased_sort_forgery() -> Module {
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
        items: vec![held, forged],
        universe_seeds: Vec::new(),
        induct_decls: decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: Some(false_type),
        body: Term::free_var(&Free::from(&forged_name)),
    }
}

/// The control: the same two families with `Two` at `Type 0`, where `Two/b()` really does clash with `Two/a()`. Nothing inhabits `Held(Two/b())`, so the module proves nothing — what it pins is that the empty elimination stays legal.
fn relevant_index_control() -> Module {
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
        items: vec![vacuous],
        universe_seeds: Vec::new(),
        induct_decls: decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// A family that declares one tag twice, where the shadowed entry is the one a coverage decision is about.
///
/// A tag is the elimination key and the runtime index, and every lookup resolves one by *first match*. So a repeat hides a constructor instead of adding one, and the rules that walk `constructors` entry by entry answer about the first one once per entry. Coverage is where that showed. `Held(t : Two)` below declares `mk() : (Two/a())` and then `mk() : (Two/b())`; asked whether each constructor is impossible at `Two/b()`, the rule resolved both entries to the first, compared `Two/a()` against `Two/b()` twice, and reported the family empty at an index its own second entry constructs at.
///
/// What it certified is `vacuous : (x : Held(Two/b())) -> False` — a refutation of a constructor the same declaration states. While the hole was open `recheck_module_verdicts` returned **zero refusals** for exactly that module. It was not a closed inhabitant of `False`, and the paired experiment is what established that: adding `let h : Held(Two/b()) = Held/mk()` produced one verdict and one only, a `Mismatch` of `Held(Two/a())` against `Held(Two/b())` — construction resolves by first match too, so the shadowed entry has no inhabitant to hand the refutation. The elimination rule was therefore sound because an unrelated rule happened to be lossy in the same direction, which is the dependency this clause replaces.
///
/// No `.crs` file reaches it: `curios-text` refuses both spellings with `duplicate public declaration: mk`, whether or not the representation is public. That is the `Expect::NotAsked` shape — the elaborator is the stricter of the two, so the certifier's copy of the rule is unreachable from the corpus — and it is why this belongs here rather than in `curios/src/tests`.
///
/// The control is the same shape at two *distinct* tags, both targeting `Two/a()`. It must stay accepted: ruling impossible cases out is what an indexed family's vacuous elimination is for, and a clause that refused every multi-constructor declaration, or every empty elimination over one, would shut this hole with a brick.
#[test]
fn a_family_that_declares_one_tag_twice_is_refused() {
    let verdicts = recheck_module_verdicts(&shadowed_constructor(["mk", "mk"]), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::RepeatedTag(_))),
        "the kernel certified a refutation of a constructor the declaration states: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_over_two_distinct_tags_is_still_accepted() {
    let verdicts = recheck_module_verdicts(&shadowed_constructor(["mk", "mk2"]), 1_000_000);

    assert!(
        verdicts.is_empty(),
        "an elimination whose every constructor genuinely clashes was refused: {verdicts:?}",
    );
}

/// `Two : Type 0` with constructors `a` and `b`, `Held(t : Two)` whose two constructors both target `Two/a()` under the given tags, and `vacuous : (x : Held(Two/b())) -> False` eliminating with no arms at all.
///
/// Both constructors target `Two/a()`, so at `Two/b()` every one of them is genuinely impossible and the elimination is legal — *when the tags are distinct*. Giving them the same tag changes nothing about the targets and everything about which entry the coverage rule reads.
fn shadowed_constructor(tags: [&str; 2]) -> Module {
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
        root: RootId::Entry,
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
        root: RootId::Entry,
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
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// A level naming a universe parameter its declaration does not have, in the two positions the boundary pass walks.
///
/// A declaration's universe scheme is a promise that every level it mentions is either ground or one of the parameters it declares, so a use site fully determines it. `curios-elab`'s `validate_bound_universes` is what checks that promise, and this crate had no equivalent: `universe_residue` looks for an unsolved *metavariable* in a level and nothing looks for a parameter index past the declaration's own count.
///
/// What makes that a soundness question rather than a tidiness one is what instantiation does next. `instantiate_universe_levels_scoped` substitutes the indices an instance supplies and *renumbers* the rest down by the instance's width — the correct de Bruijn shift for a well-scoped term, where an index at or above the width refers to an enclosing binder. For an ill-scoped one it is a capture: `Type.{param 1}` and `Type.{param 0}` both instantiate at `[param 0]` to the same `Type.{u}`, so two levels that were distinct become one, and the hierarchy's questions are then decided about the wrong one.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for each of the two modules below, and the renumbering was confirmed directly against `instantiate_universe_levels_scoped`. Neither module is reachable from a surface program — `validate_universes` runs before a module ever leaves the elaborator, so this is a rule the elaborator holds and the certifier did not, and the certifier was the permissive one. No closed inhabitant of `False` was built from it; what is demonstrated is the capture and the missing judgment.
///
/// The control is [`a_level_naming_a_declared_universe_parameter_is_accepted`], the same two positions with the parameter actually declared. It is not decoration: the prelude is universe-polymorphic throughout, so a check that refused every parameter-naming level would reject the standard library rather than this.
#[test]
fn a_level_naming_an_undeclared_universe_parameter_is_refused() {
    let escaping = Level::param(UniverseParam(0));

    for (label, module) in [
        (
            "a definition's declared type",
            scheme_definition(&escaping, 0),
        ),
        (
            "a registry entry's result sort",
            scheme_registry(&escaping, 0),
        ),
    ] {
        let verdicts = recheck_module_verdicts(&module, 1_000_000);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::UnclosedUniverses)),
            "{label}: the kernel certified a level naming a parameter the declaration does not have: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: the same level in the same positions, with the declaration declaring the parameter it names.
#[test]
fn a_level_naming_a_declared_universe_parameter_is_accepted() {
    let declared = Level::param(UniverseParam(0));

    for (label, module) in [
        (
            "a definition's declared type",
            scheme_definition(&declared, 1),
        ),
        (
            "a registry entry's result sort",
            scheme_registry(&declared, 1),
        ),
    ] {
        assert_eq!(
            recheck_module_verdicts(&module, 1_000_000),
            Vec::new(),
            "{label}: the boundary pass refused a parameter the declaration declares",
        );
    }
}

/// [`level_definition`] with a universe scheme of `parameter_count` parameters.
fn scheme_definition(level: &Level, parameter_count: usize) -> Module {
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::type_at(level.clone()),
        body: Term::intrinsic(Intrinsic::NatType),
    };

    Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// [`level_registry`] with a universe scheme of `parameter_count` parameters.
fn scheme_registry(level: &Level, parameter_count: usize) -> Module {
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

/// A universe instance supplying fewer levels than the scheme it instantiates has parameters.
///
/// `Kernel::check_instance` discharges a scheme's *constraints* at the levels an occurrence supplies, and that is all it does. A scheme with an empty constraint set therefore accepts an instance of any width — the loop body never runs — so nothing anywhere asked whether an occurrence supplies as many levels as the declaration declares. `curios-elab`'s `validate_instance_arities` asks exactly that, of every `UniverseInst`, `InductType`, `Variant`, `StructType` and `Struct` in the module, and this crate had no equivalent.
///
/// The consequence is the capture [`a_level_naming_an_undeclared_universe_parameter_is_refused`] records, reached from the other side. That fixture is about a declaration naming a parameter it does not have; this one is about a declaration naming a parameter it *does* have, at an occurrence that does not supply it. `instantiate_universe_levels_scoped` renumbers whatever the instance leaves unsupplied down by the instance's width, so `Levelled`'s `Type.{param 1}` at the one-level instance `[param 0]` becomes `Type.{param 0}` — and `param 0` at the use site is the *use site's* own first parameter, not the declaration's second. Two levels that were distinct are now one, and cumulativity is decided about the wrong one.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for the module below, and the renumbering was confirmed directly against `instantiate_universe_levels_scoped`. Not reachable from a surface program — `validate_universes` runs before a module leaves the elaborator — so this is again a rule the elaborator holds and the certifier did not, with the certifier the permissive one. No closed inhabitant of `False` was built from it; the capture and the missing judgment are what is demonstrated.
///
/// The control is [`a_universe_instance_of_the_declared_width_is_accepted`], the same occurrence supplying both levels. It is load-bearing: every occurrence of a universe-polymorphic declaration in the prelude carries an instance, so a width check that got the bound wrong would reject the standard library rather than this.
#[test]
fn a_universe_instance_narrower_than_its_scheme_is_refused() {
    let verdicts = recheck_module_verdicts(&instance_of_width(1), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Arity { .. })),
        "the kernel certified an occurrence that leaves a declared universe parameter unsupplied: {verdicts:?}",
    );
}

/// The control for the fixture above: the same occurrence, supplying one level per declared parameter.
#[test]
fn a_universe_instance_of_the_declared_width_is_accepted() {
    assert_eq!(
        recheck_module_verdicts(&instance_of_width(2), 1_000_000),
        Vec::new(),
        "the boundary refused an occurrence that supplies exactly the levels its scheme declares",
    );
}

/// `let held : Type.{u} = Levelled.{…}`, where `Levelled` declares two universe parameters and the occurrence supplies `width` of them.
fn instance_of_width(width: usize) -> Module {
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
        root: RootId::Entry,
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
        root: RootId::Entry,
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
        items: vec![Item::Let(definition)],
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

/// A forged ABI row cannot hand the guest an inhabitant of a proposition, because a wire signature cannot name one.
///
/// The foreign wire contract is the one perimeter row `curios/src/tests/perimeter.rs` records as enforced by the *grammar*: `parse_wire_type` is a closed keyword grammar, so `foreign bad : False` never parses and `both_checkers` returns `NotAsked` for both columns — which that file summarizes as "the rule is the parser's and neither checker backs it up". As a statement about where the rule is enforced that is right. As a soundness statement it understates the position, and it leaves open the question that matters: a host call is the one place an *embedder* supplies a value the compiler never saw, so what happens if a module reaches the kernel with the rule already broken?
///
/// It cannot be broken, and the reason is representational rather than a check. `Intrinsic::Foreign` carries a `ForeignFunction` whose `signature` is a `WireSignature` over `WireType` — a closed six-variant enum of `Nat`, `Int`, `Bool`, `Bytes`, `Handle` and `List`. No variant denotes a nominal type, so no row, forged by hand or not, can *say* its result is a proposition. And `infer`'s rule does not read a type off the term: it **constructs** the result from `wire_term` over that enum and checks each operand against its own wire type, so what the row claims about its namespace and name — the part a forgery controls — never reaches the type at all.
///
/// A null result, recorded as one: nothing here was found to be wrong. What the fixture pins is that the boundary holds from Core and not merely from the parser, which is the half no surface program can reach and no fixture covered. The row below is a forgery — a namespace and name no store would issue — and the kernel is then made to type its call.
///
/// The control is [`a_forged_foreign_row_still_inhabits_its_wire_type`], the same call at the type its signature does denote. A fixture that refused every foreign call would pin nothing about propositions.
#[test]
fn a_forged_foreign_row_cannot_inhabit_a_proposition() {
    let false_name = Global::Authored(Qualifier::from(["False"]));
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let verdicts = recheck_module_verdicts(&forged_foreign(&false_type, &false_name), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Mismatch { .. })),
        "the kernel let a forged host row inhabit a proposition: {verdicts:?}",
    );
}

/// The control for the fixture above: the same forged row at the type its own signature names, wrapped in the description every host call now returns. `wire_term` still reads `Nat` off the signature; `infer` wraps it, because a foreign call is an effect and an effect is an `Io`. Stating the control at the bare `Nat` would fail for a reason that has nothing to do with forgery.
#[test]
fn a_forged_foreign_row_still_inhabits_its_wire_type() {
    let false_name = Global::Authored(Qualifier::from(["False"]));

    assert_eq!(
        recheck_module_verdicts(
            &forged_foreign(
                &Term::intrinsic(Intrinsic::io_type(Term::intrinsic(Intrinsic::NatType))),
                &false_name
            ),
            1_000_000
        ),
        Vec::new(),
        "the boundary refused a host call at the type its own wire signature denotes",
    );
}

/// `let held : claimed = <a forged host row returning one `Nat`>`, with `False` declared alongside.
fn forged_foreign(claimed: &Term, false_name: &Global) -> Module {
    let row = Arc::new(ForeignFunction {
        namespace: "ffi",
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
        root: RootId::Entry,
        totality: Totality::Total,
        type_: claimed.clone(),
        body: Term::foreign(row, Vec::new()),
    };

    Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(false_name.clone(), proposition(Vec::new()))]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// An inductive's registry entry and its type-former definition may declare different universe schemes here, and that is not a hole.
///
/// `curios-elab`'s `validate_universes` refuses the disagreement outright — "inductive Foo and its type-former definition have different universe contexts" — and it is one of three such agreement clauses, the others pairing a `struct` with its type-former and a `concept` with its structure entry. All three are elaborator-only, and one concept stored twice with the agreement checked in one place is the shape that produced the three findings this file already records. So it was worth constructing rather than assuming.
///
/// The kernel certifies it, in both directions of the mismatch, and the reason it may is that it never reads the agreement: it reads each scheme where that scheme is *authoritative*. An `InductType` occurrence is checked and instantiated against the registry entry's context; the definition is checked under its own. Neither is ever consulted for the other's question, so there is nothing for a disagreement to corrupt.
///
/// The second fixture is what makes that a demonstration rather than a hope. It claims `Foo` at instance `[u, u]` inhabits the sort `Foo`'s registry gives instance `[u, v]`, and the kernel refuses with a `Mismatch` of `Type u` against `Type v` — the registry's `result_sort` applied faithfully to the levels the *occurrence* supplied. A kernel that read the definition's scheme here, or conflated the two, would accept it.
///
/// The verdict, stated plainly because the skill file asks for it: this is the elaborator being strict, not the certifier being permissive. The distinction is not that the elaborator is merely cautious — the agreement is load-bearing *for the elaborator*, which reads a scheme from whichever of its two copies is in hand. It is not load-bearing for a checker that reads each in its own place. No rule was added: making the kernel enforce an elaborator construction invariant it does not consult would be the kernel believing elaborator output, which is the thing this crate exists not to do.
///
/// Both routes by which a disagreement *could* have mattered are closed already, and by this same file: an ill-scoped level (`a_level_naming_an_undeclared_universe_parameter_is_refused`) and an instance of the wrong width (`a_universe_instance_narrower_than_its_scheme_is_refused`).
#[test]
fn a_registry_and_its_type_former_may_declare_different_schemes() {
    for (label, registry, definition) in
        [("registry narrower", 1, 2), ("definition narrower", 2, 1)]
    {
        assert_eq!(
            recheck_module_verdicts(&disagreeing_schemes(registry, definition), 1_000_000),
            Vec::new(),
            "{label}: the kernel refused a disagreement it never consults",
        );
    }
}

/// The demonstration that each scheme is nonetheless applied where it is authoritative: a claim about the family's sort that the *registry's* `result_sort` refutes.
#[test]
fn a_family_takes_the_sort_its_registry_gives_the_levels_supplied() {
    let family = Global::Authored(Qualifier::from(["Foo"]));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 2,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::new(),
        // The sort is the *second* parameter, so an occurrence's second level decides it.
        result_sort: Term::type_at(Level::param(UniverseParam(1))),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };

    // `Foo.{u, u}` claimed at `Type v`: its sort is `Type u`, and nothing may conflate the two.
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["held"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 2,
            constraints: Vec::new(),
        },
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::induct_type_at(
            family.clone(),
            vec![Level::param(UniverseParam(0)); 2],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };

    let module = Module {
        items: vec![Item::Let(definition)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(family, declaration)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    };

    let verdicts = recheck_module_verdicts(&module, 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::Mismatch { .. })),
        "the registry's result sort was not applied to the levels the occurrence supplied: {verdicts:?}",
    );
}

/// `Foo`'s registry entry and its type-former definition, declaring `registry` and `definition` universe parameters respectively.
fn disagreeing_schemes(registry: usize, definition: usize) -> Module {
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
        root: RootId::Entry,
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
        root: RootId::Entry,
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
        items: vec![Item::Let(former)],
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

/// A motive that lies about its own sort skips the large-elimination guard entirely.
///
/// `Sort::of` classifies a stuck `match` used as a type by reading its **motive** — "its motive is the sort, which every arm shares", as the arm says. Nothing establishes that. `check_definition` calls `Sort::of` on a declared type and never infers it, so a type-position `match` has its arms checked against its motive by no judgment at all, and the motive may claim whatever it likes.
///
/// The guard is what that buys. `check_elimination` computes `let relevant = Sort::of(result).map(|sort| !sort.is_prop())` and **returns immediately when the result is not relevant** — eliminating a proposition into a proposition needs no condition. So a motive whose body is `switch i : (_) => Prop | 0 => Nat | _ => Nat` reads as `Prop` at the abstract binder the guard opens it at, the guard is skipped, and the very same motive *reduces to `Nat`* at the concrete index each arm is checked against, so the arms typecheck as data.
///
/// `P` below is a two-constructor proposition, the shape the guard exists to refuse: `mk()` and `mk2()` both inhabit `P(0)`, proof irrelevance identifies them, and `extract` maps them to `7` and `9`. Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for exactly this module, and `Sort::of` was confirmed directly to answer `Ok(Prop)` for a switch whose every arm is `Nat` while answering `Ok(Type 0)` for the same switch with an honest motive. This is the route DESIGN.md records as having produced two closed inhabitants of `False`, reached through the classifier rather than through the guard's own condition.
///
/// Not reachable from a surface program: `curios-elab` builds a match's motive and checks the arms against it, so it never emits one that lies. That is what kept the certifier's copy of the guard unobserved, and it is why this is built here.
///
/// Closed by checking the motive itself: `check_motive` types it under its real binders and requires it to land in a sort, which is Coq's `type_of_case` clause, and hands the sort it derives to the guard so nothing re-reads the claim. The lying motive no longer typechecks — its arms inhabit `Type` while it states `Prop` — so the refusal names that rule rather than the guard it used to bypass.
///
/// The control is [`an_honest_motive_still_refuses_the_large_elimination`], the same module with the motive stating `Type` — where the motive is honest, the guard does fire, and the elimination is refused for the reason it should be. Together they pin that what was being skipped is the guard, and that closing the bypass did not close the guard itself.
#[test]
fn a_motive_that_misreports_its_sort_does_not_skip_the_large_elimination_guard() {
    let verdicts = recheck_module_verdicts(&lying_motive(Term::prop()), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotAMotive(_))),
        "the kernel eliminated a two-constructor proposition into `Nat`: {verdicts:?}",
    );
}

/// The control: the identical module whose motive states the `Type` its arms actually have. The guard sees a relevant result and refuses, which is what proves the fixture above is about the *classifier* rather than about eliminations in general.
#[test]
fn an_honest_motive_still_refuses_the_large_elimination() {
    let verdicts = recheck_module_verdicts(&lying_motive(Term::type_ground()), 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::LargeElimination(_))),
        "the guard did not fire even on an honest motive: {verdicts:?}",
    );
}

/// `extract : (p : P(0)) -> Nat`, eliminating the two-constructor proposition `P` under a motive whose inner switch states `sort` while every arm of it is `Nat`.
fn lying_motive(sort: Term) -> Module {
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
        root: RootId::Entry,
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
        items: vec![extract],
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

/// A motive is checked even where the elimination cannot run.
///
/// `check_induct_arms` skips the large-elimination guard for a vacuous elimination — no arms, no catch-all — and the reason is good: the coverage loop must then prove *every* constructor impossible at the scrutinee's indices, so the eliminated instance is uninhabited and discharging it into a relevant result leaks nothing. The question this fixture settles is whether the *motive* clause inherits that skip.
///
/// It does not, and the reason is not an exploit. No route from a vacuous elimination to a forged term was demonstrated, and the honest reading of that is the weak one: an attempt failed, which is not the same as a proof that none exists. What makes the clause unconditional is that `infer` reads the elimination's **type** off the motive and hands it to the caller whether or not the elimination can run — so a motive nothing validated means a term whose type nothing validated, and `Sort::of` will classify it downstream. The module below was certified with **zero refusals** while the clause did not run: a vacuous elimination at an uninhabited `Held(Two/b())`, whose motive states `Prop` over arms inhabiting `Type`. What keeps the fixture pointed at the motive is the order `check` and `infer` run in, not the shape of the definition around it — see the note at the definition, and the one thing about it that had to change once `infer` began typing a type former's parts rather than classifying them.
///
/// It also costs nothing. The clause sits in [`check_cases`](crate::infer) above the dispatch, where every `Cases` form shares one `motive` binding, so *not* running it here would mean pushing it down into `check_induct_arms`, guarding it with the vacuous condition, and duplicating it into the three intrinsic-carrier arms. Unconditional is the cheap implementation; the skip would have been the deliberate exception.
#[test]
fn a_vacuous_elimination_still_has_its_motive_checked() {
    let two_name = Global::Authored(Qualifier::from(["Two"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let nullary = |tag: &str, targets: Vec<Term>| {
        (
            Atom::from(tag),
            InductParam {
                telescope: Telescope::done(targets),
                plicities: Vec::new(),
            },
        )
    };
    let at = |tag: &str| {
        Term::variant(
            two_name.clone(),
            Vec::<Term>::new(),
            tag,
            Vec::<Term>::new(),
        )
    };

    // `Two : Type 0`, so `a` and `b` genuinely clash and the elimination really is vacuous.
    let two_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![nullary("a", Vec::new()), nullary("b", Vec::new())],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };
    // `Held : (t : Two) -> Prop | mk() : (Two/a())`, a proposition, so the guard would be in play.
    let held_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(900, Some("t")),
                Term::induct_type(two_name.clone(), Vec::<Term>::new(), Vec::<Term>::new()),
            )],
            (),
        )),
        constructors: vec![nullary("mk", vec![at("a")])],
        result_sort: Term::prop(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };

    let at_b = Term::induct_type(held_name.clone(), Vec::<Term>::new(), [at("b")]);
    let outer = Free::local(903, Some("n"));
    let lying = || {
        Term::switch_scoped(
            Term::free_var(&outer),
            Scope::close(Many(1), &[&Free::local(904, Some("k"))], Term::prop()),
            [(0u32, Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::NatType),
        )
    };
    let subject = Free::local(902, Some("s"));

    // The codomain is honest, and the ordering is what keeps this about the motive: `check` reaches `infer` before it can subsume, and `infer` runs `check_cases` — hence the motive clause — before there is a type to subsume with. Making the codomain *be* the lying motive, which is how this read before a former's parts were typed, now refuses a step earlier on that codomain's own arms and never reaches the body.
    let vacuous = authored(
        &Global::Authored(Qualifier::from(["vacuous"])),
        Term::func_type(
            [
                (outer.clone(), Term::intrinsic(Intrinsic::NatType)),
                (subject.clone(), at_b.clone()),
            ],
            Term::intrinsic(Intrinsic::NatType),
        ),
        Term::func(
            [
                (outer.clone(), Term::intrinsic(Intrinsic::NatType)),
                (subject.clone(), at_b),
            ],
            Term::induct_match_scoped_marked(
                Term::free_var(&subject),
                Scope::close(
                    Many(2),
                    &[&Free::local(901, Some("t")), &Free::local(905, Some("z"))],
                    lying(),
                ),
                Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
                None,
            ),
        ),
    );

    let module = Module {
        items: vec![vacuous],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(two_name, two_decl), (held_name, held_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    };

    let verdicts = recheck_module_verdicts(&module, 1_000_000);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotAMotive(_))),
        "a vacuous elimination carried a motive nothing validated: {verdicts:?}",
    );
}

/// A nominal occurrence's parameter and index counts must be the declaration's.
///
/// `Sort::of` reads an `InductType`'s declaration to answer for it — `check_instance` for the universe width, then `result_sort` instantiated at those levels. It never asked whether the occurrence supplies as many *parameters* and *indices* as the declaration declares. That is the same omission the universe width had before `check_instance` checked it: an occurrence's arity validated against itself rather than against the scheme it instantiates.
///
/// Verified while the hole was open, in both halves. Where the arity is merely carried, the module was **certified with zero refusals** — `let held : Type = F` for a one-parameter, one-index family, at no parameters, at no indices, and at two indices, all three accepted. Where the arity is *used*, the kernel **aborted**: eliminating over such a scrutinee reaches `InductDecl::indices_at`, whose `Telescope::open` asserts, and the process panicked with `telescope arity mismatch in open: expected 1, got 0`. A panic refuses rather than admits, so it is inside what DESIGN.md permits of the Rust implementation — but a malformed occurrence is the *program's* fault, and the house rule is that a program's fault is a `KernelError`. The certified half is the one that matters: a type the kernel blessed whose shape its own declaration contradicts.
///
/// Not reachable from a surface program — `curios-elab` builds a nominal occurrence saturated from the declaration it looked up — which is why this is constructed here. No inhabitant of `False` was built from it; what is demonstrated is that the arity is unchecked and that both of its consumers are wrong when it is.
///
/// The control is [`an_occurrence_at_its_declared_arity_is_accepted`], the same family at the parameters and indices it declares, which must keep passing: every nominal type in every program is such an occurrence.
#[test]
fn an_occurrence_whose_arity_is_not_its_declarations_is_refused() {
    for (label, params, indices) in arity_cases() {
        let verdicts = recheck_module_verdicts(&occurrence_module(params, indices), 1_000_000);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::Arity { .. })),
            "{label}: the kernel accepted an occurrence its declaration contradicts: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: one parameter and one index, as declared.
#[test]
fn an_occurrence_at_its_declared_arity_is_accepted() {
    let module = occurrence_module(
        vec![Term::intrinsic(Intrinsic::NatType)],
        vec![Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
    );

    assert_eq!(
        recheck_module_verdicts(&module, 1_000_000),
        Vec::new(),
        "the boundary refused an occurrence at exactly the arity its declaration states",
    );
}

/// The three ways an occurrence of a one-parameter, one-index family can disagree with it.
fn arity_cases() -> Vec<(&'static str, Vec<Term>, Vec<Term>)> {
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
fn occurrence_module(params: Vec<Term>, indices: Vec<Term>) -> Module {
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
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::type_ground(),
        Term::induct_type(family.clone(), params, indices),
    );

    Module {
        items: vec![held],
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

/// A nominal *value*'s parameter count must be its declaration's, as its type's already must be.
///
/// [`an_occurrence_whose_arity_is_not_its_declarations_is_refused`] closed this for the two type formers, where `Sort::of` consults a declaration to answer for an occurrence. It does not reach the value forms: nothing calls `Sort::of` on a `Struct` or a `Variant`, so their carried parameter list was still taken on the term's own word.
///
/// The two forms failed differently, and neither failed well. A `Struct` opens the declaration's arity with `Telescope::open`, which **asserts** — so a value at no parameters for a one-parameter structure aborted the process with `telescope arity mismatch in open: expected 1, got 0`, and at two parameters with `expected 1, got 2`. A panic refuses rather than admits, so it is inside what DESIGN.md permits of the Rust implementation, but it is the wrong shape twice over: a malformed value is the *program's* fault, which the house rule says is a `KernelError`, and `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others — an abort takes every other verdict with it, which is what makes the disagreement count a count.
///
/// A `Variant` instead opens with `open_params`, which is tolerant: too few parameters leaves the declaration's own parameter binders unopened, so they read as *payload* slots and the payload-arity check compares against the wrong number. That was refused while the hole was open, but downstream and by accident — the resulting type carried the short parameter list into a conversion that happened to reject it — rather than by any rule about the value. Sound by coincidence is the pattern this class keeps producing.
///
/// Not reachable from a surface program: `curios-elab` builds a nominal value saturated from the declaration it looked up. No inhabitant of `False` was built from either; what is demonstrated is that the count is unchecked and that both consumers are wrong when it is.
///
/// The control is [`a_nominal_value_at_its_declared_arity_is_accepted`], both forms at exactly the parameters they declare, which must keep passing: every constructor application and every record literal in every program is one.
#[test]
fn a_nominal_value_whose_arity_is_not_its_declarations_is_refused() {
    for (label, module) in nominal_value_cases() {
        let verdicts = recheck_module_verdicts(&module, 1_000_000);

        assert!(
            verdicts
                .iter()
                .any(|verdict| matches!(verdict.error, KernelError::Arity { .. })),
            "{label}: the value's parameter count was not held to its declaration: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: one parameter each, as declared.
#[test]
fn a_nominal_value_at_its_declared_arity_is_accepted() {
    let nat = Term::intrinsic(Intrinsic::NatType);

    assert_eq!(
        recheck_module_verdicts(&struct_value_module(vec![nat.clone()]), 1_000_000),
        Vec::new(),
        "a record literal at exactly its declared parameters was refused",
    );
    assert_eq!(
        recheck_module_verdicts(&variant_value_module(vec![nat]), 1_000_000),
        Vec::new(),
        "a constructor application at exactly its declared parameters was refused",
    );
}

/// The four ways a one-parameter nominal value can disagree with its declaration.
fn nominal_value_cases() -> Vec<(&'static str, Module)> {
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
fn struct_value_module(params: Vec<Term>) -> Module {
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
        root: RootId::Entry,
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
        items: vec![held],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::from([(name, declaration)]),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// `induct F(A : Type) : Type | mk(x : A) end`, with a constructor application at `params`.
fn variant_value_module(params: Vec<Term>) -> Module {
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
        root: RootId::Entry,
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
        items: vec![held],
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

/// A count carried on a term and used to *index* must be checked, not assumed — twice, in reduction and in synthesis.
///
/// Both are reached the same way. `check_definition` calls `Sort::of` on a declared type and never infers it, so a type position holds a term nothing has typed, and the two functions that walk it were written against an invariant typing would have established.
///
/// **Reduction.** `step_apply` opens a lambda's telescope at the application's arguments. `Telescope::open` asserts, so an application that does not saturate its lambda **aborted the walk** — `telescope arity mismatch in open: expected 2, got 1`. It is now stuck instead, which is the conservative direction twice over: reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than killing every other verdict. `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others, and an abort is what makes that false.
///
/// **Synthesis.** With reduction stuck, `synth_neutral`'s partial-application arm reports a spine's residual function type — and slices `plicities` at the argument count while guarding only on the *telescope's* length. A `FuncType` whose plicities are not parallel to its telescope therefore aborted there instead, at `sort.rs`'s slice. `plicities` is documented as parallel and nothing checked it; the arm now declines to report a residual type for a spine whose own type is malformed.
///
/// Verified while the holes were open: each case aborted the process rather than producing a verdict, and the two are independently reachable — the lambda cases still refuse with the reduction guard alone, and the neutral case still aborted until the slice was guarded too. Neither is reachable from a surface program: `curios-elab` emits saturated applications and plicity vectors built alongside the telescopes they parallel. No inhabitant of `False` was built from either; what is demonstrated is that a program's fault aborted the kernel where a `KernelError` belongs.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`]. It is the direction that matters: reduction must still fire on a well-formed application, and a guard that simply stopped reducing would pass every witness here while breaking every program.
///
/// The lambda case's diagnostic *improved* when `infer_type` landed: with the declared type typed rather than classified, `infer` refuses it as `Arity { expected: 2, actual: 1 }` — naming the defect — where reduction going stuck had left it a generic `Unclassified`. Both are accepted here because the two rules refuse different cases: the neutral spine's plicities are a count no typing rule reads, so it is still the guard that catches it.
#[test]
fn a_count_a_term_carries_is_refused_rather_than_indexed_with() {
    for (label, module) in unsaturated_cases() {
        let verdicts = recheck_module_verdicts(&module, 1_000_000);

        assert!(
            verdicts.iter().any(|verdict| matches!(
                verdict.error,
                KernelError::Arity { .. } | KernelError::Unclassified(_)
            )),
            "{label}: the term was indexed at a count nothing checked: {verdicts:?}",
        );
    }
}

/// The control for the fixture above: a lambda applied to exactly its binders still reduces, so the type position it stands in is classified as it always was.
#[test]
fn a_saturated_application_in_a_type_position_is_accepted() {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let former = Global::Authored(Qualifier::from(["f"]));

    let plicities = vec![Plicity::Explicit, Plicity::Explicit];
    let former_def = authored(
        &former,
        Subterm::FuncType(FuncType {
            telescope: Telescope::build(
                [(a.clone(), nat.clone()), (b.clone(), nat.clone())],
                Term::type_ground(),
            ),
            plicities: plicities.clone(),
        })
        .into(),
        Subterm::Func(Func {
            telescope: Telescope::build(
                [(a.clone(), nat.clone()), (b.clone(), nat.clone())],
                nat.clone(),
            ),
            plicities,
        })
        .into(),
    );

    // `f(3, 4)` reduces to `Nat`, so `held : f(3, 4) = 3` is an ordinary well-typed item.
    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::apply(
            Term::free_var(&Free::from(&former)),
            [
                three.clone(),
                Term::intrinsic(Intrinsic::Nat(Nat::new(4usize))),
            ],
        ),
        three,
    );

    let module = Module {
        items: vec![former_def, held],
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
        "a saturated application in a type position was refused",
    );
}

/// The two shapes: an application that under-saturates its lambda, and a neutral spine whose `plicities` are shorter than the arguments applied to it.
fn unsaturated_cases() -> Vec<(&'static str, Module)> {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let g = Free::local(992, Some("g"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    let two_binder = |result: Term| {
        Telescope::build([(a.clone(), nat.clone()), (b.clone(), nat.clone())], result)
    };
    let module_of = |items: Vec<Item>| Module {
        items,
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    };

    // `f : (a : Nat, b : Nat) -> Type`, applied to one argument in a type position.
    let former = Global::Authored(Qualifier::from(["f"]));
    let plicities = vec![Plicity::Explicit, Plicity::Explicit];
    let former_def = authored(
        &former,
        Subterm::FuncType(FuncType {
            telescope: two_binder(Term::type_ground()),
            plicities: plicities.clone(),
        })
        .into(),
        Subterm::Func(Func {
            telescope: two_binder(nat.clone()),
            plicities,
        })
        .into(),
    );
    let under_applied = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::apply(Term::free_var(&Free::from(&former)), [three.clone()]),
        three.clone(),
    );

    // `held : (g : (a : Nat, b : Nat) -> Type) -> g(3)`, where `g`'s type carries no plicities at all.
    let short_plicities: Term = Subterm::FuncType(FuncType {
        telescope: two_binder(Term::type_ground()),
        plicities: Vec::new(),
    })
    .into();
    let neutral = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Subterm::FuncType(FuncType {
            telescope: Telescope::build(
                [(g.clone(), short_plicities)],
                Term::apply(Term::free_var(&g), [three.clone()]),
            ),
            plicities: vec![Plicity::Explicit],
        })
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

/// The two reduction steps that still opened a binder set at a count the term supplied.
///
/// [`a_count_a_term_carries_is_refused_rather_than_indexed_with`] closed the β step and the partial-spine slice. It closed one of two twins and one of two openers. `whnf` opens a binder set in four places, and an enumeration of them found these two still unguarded — both reached the same way, through `check_definition` sorting a declared type it never infers.
///
/// **The arm of an elimination.** Reducing a `match` on a concrete constructor opens the matching arm at that constructor's payload. `Scope::open` asserts, so an arm binding two components of a one-component payload — or none — **aborted the walk**. The arm arity *is* checked, by `check_arm`, but only once typing reaches the elimination; reduction of a type position runs first and had no such precondition.
///
/// **The recursive twin of the β step.** `unfold_rec_apply` unfolds a folded recursive application by opening its member's telescope at the arguments, exactly as `step_apply` did, and was left behind when `step_apply` was guarded. `rec f : (a, b) -> Type = …; f(3)` in a type position aborted there.
///
/// Verified while the holes were open: each of the three cases panicked at `Scope::open` or `Telescope::open` rather than producing a verdict, and each was confirmed independently reachable. Both are stuck now, which is the same conservative direction the β step took — reduction that declines to fire can never admit anything, and the term is left for the typing rules to refuse with a diagnostic rather than aborting the walk and taking every other verdict with it.
///
/// `step_proj` was enumerated alongside them and needed nothing: every arm already guards its index (`index < fields.len()`, `(1..=payload.len()).contains(&index)`) and falls through to stuck, which is what these two now do. It is the pattern, and it was already there to copy.
///
/// The control is [`a_saturated_application_in_a_type_position_is_accepted`] together with [`an_arm_matching_its_payload_still_reduces`]: a guard that merely stopped reducing would pass every witness here while breaking every program.
#[test]
fn a_binder_set_is_not_opened_at_a_count_the_term_supplied() {
    for (label, module) in unguarded_opener_cases() {
        // The demonstrated defect is the *abort*: `recheck_module_verdicts` is documented as walking to the end with each verdict independent of the others, and a panic makes that false.
        let verdicts = catch_unwind(AssertUnwindSafe(|| {
            recheck_module_verdicts(&module, 1_000_000)
        }))
        .unwrap_or_else(|_| panic!("{label}: reduction aborted the walk instead of refusing"));

        assert!(
            !verdicts.is_empty(),
            "{label}: the module was certified rather than refused",
        );
    }
}

/// The control for the arm half: an arm binding exactly its constructor's payload still reduces, so the type position it computes is classified as it always was.
#[test]
fn an_arm_matching_its_payload_still_reduces() {
    let module = arm_module(vec![(Plicity::Explicit, Free::local(996, Some("a")))]);

    assert_eq!(
        recheck_module_verdicts(&module, 1_000_000),
        Vec::new(),
        "an arm binding exactly its payload was refused",
    );
}

/// The three shapes: an arm over- and under-binding its payload, and a recursive application short of its member's binders.
fn unguarded_opener_cases() -> Vec<(&'static str, Module)> {
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
fn arm_module(binders: Vec<(Plicity, Free)>) -> Module {
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
        root: RootId::Entry,
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
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// `rec f : (a : Nat, b : Nat) -> Type = (a, b) => Nat; f(3)` as a declared type.
fn rec_apply_module() -> Module {
    let a = Free::local(990, Some("a"));
    let b = Free::local(991, Some("b"));
    let f = Free::local(992, Some("f"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let three = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));
    let plicities = vec![Plicity::Explicit, Plicity::Explicit];

    let member_type: Term = Subterm::FuncType(FuncType {
        telescope: Telescope::build(
            [(a.clone(), nat.clone()), (b.clone(), nat.clone())],
            Term::type_ground(),
        ),
        plicities: plicities.clone(),
    })
    .into();
    let member_body: Term = Subterm::Func(Func {
        telescope: Telescope::build([(a.clone(), nat.clone()), (b.clone(), nat.clone())], nat),
        plicities,
    })
    .into();

    let selection = Term::rec([(f.clone(), member_type, member_body)], Term::free_var(&f));

    Module {
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
        type_: None,
        body: Term::tuple(Vec::<Term>::new()),
    }
}

/// (V) has two routes to a refusal and only one of them has ever fired. `check_positions` first asks whether a recorded position *reaches a definition known partial* — the named route, which blames a global — and failing that asks [`super::locally_partial`], which blames nothing: a term is partial in itself when it carries a non-descending `rec` group or an `Intrinsic::ProcExit`.
///
/// Instrumented across a kernel walk of the whole prelude and every program in `curios`'s test corpus, the named route refused 9 times — 8 at a proof position, 1 at a type position — and the anonymous route refused **zero**, with no test in this crate asserting a `NotTotal` verdict at all. The reason is the one this module documents: every surface spelling that would reach it is refused during elaboration, so no module carries it here. `rec b : False = b; b`, the shape three of `curios`'s `tests::soundness` fixtures use, never arrives.
///
/// `Intrinsic::ProcExit` is the trigger that isolates this route rather than merely reaching it. A non-descending `rec` at a proof type is refused by `check_group`'s own local gate before the position walk runs, so it demonstrates that gate instead; an exit meets no gate of its own. `exit` types at `{}` — deliberately, so that nothing is forged by a term that never returns — and `Held/qed(exit(0))` is therefore well typed at a proposition while carrying a computation that does not terminate. That is (V)'s whole subject: erasure deletes the proof, the exit never fires, and the program continues holding a certificate for something no total term established.
///
/// The control is the same module with `()` in place of the exit, which must stay accepted — a rule refusing every `Prop`-typed constructor application would satisfy the assertion above and nothing else here would notice.
#[test]
fn an_exit_inside_a_proof_is_refused_with_no_definition_to_blame() {
    let verdicts = recheck_module_verdicts(&proof_carrying_unit(true), 1_000_000);

    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            KernelError::NotTotal {
                erased: Erased::Proof,
                reached: None,
            }
        )),
        "the kernel certified a proof carrying an exit: {verdicts:?}",
    );
}

/// The control for the fixture above: the same proposition built from the unit value stays accepted.
#[test]
fn a_proof_carrying_the_unit_value_is_accepted() {
    assert_eq!(
        recheck_module_verdicts(&proof_carrying_unit(false), 1_000_000),
        Vec::new(),
        "an ordinary proof at a unit-carrying proposition was refused",
    );
}

/// `induct Held : Prop | qed(u : {})`, with `bad : Held` built either from `exit(0)` or from `()`.
fn proof_carrying_unit(exiting: bool) -> Module {
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
        root: RootId::Entry,
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
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    }
}

/// `plicities` is the one field on a registry entry that no clause of `check_induct_decl` establishes, and the stated reason it needs none is that this kernel never reads it — its only consumer is `curios-elab`'s `payload_plicities`, which slices at the parameter count, so a short vector is a panic in the elaborator rather than a judgment here.
///
/// That reason is an *inventory*: every consumer of a registry entry, read against the clauses. An inventory is exactly the kind of claim that goes stale as code moves, and the polarity vector beside it on the same entry already has `positivity::tests::a_carried_polarity_vector_is_recomputed_rather_than_believed` holding its own version of this. This holds the plicity half executably instead: the same declaration, once with the honest vector and once with a lie, must produce the same verdicts.
///
/// The lie is unusable rather than merely wrong — an empty vector where the constructor carries one payload binder, which is the shape `payload_plicities` would slice out of range on. What must not happen is this kernel quietly deciding something *differently* because of it, which is what a future nominal rule reading the field would introduce without any clause noticing.
///
/// The control is [`a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector`]: the same lie beside a genuine error. Without it, "the kernel ignores plicities" and "the kernel ignores this module" read alike.
#[test]
fn a_registry_plicity_vector_is_read_by_no_kernel_rule() {
    assert_eq!(
        recheck_module_verdicts(&plicity_module(true, 1), 1_000_000),
        recheck_module_verdicts(&plicity_module(false, 1), 1_000_000),
        "a plicity vector no kernel rule reads changed a verdict",
    );
    assert_eq!(
        recheck_module_verdicts(&plicity_module(false, 1), 1_000_000),
        Vec::new(),
        "both sides must be accepted, or the equality above is two refusals agreeing",
    );
}

/// The control for the fixture above: under the same lying plicity vector, an ordinary error is still caught.
#[test]
fn a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector() {
    assert!(
        !recheck_module_verdicts(&plicity_module(false, 0), 1_000_000).is_empty(),
        "the kernel accepted a constructor application at the wrong payload count",
    );
}

/// `induct Held : Type | mk(n : Nat)` with one item building `Held/mk` at `payload_count` arguments, the constructor's plicity vector either honest or empty.
fn plicity_module(honest: bool, payload_count: usize) -> Module {
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
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };

    let payload = (0..payload_count)
        .map(|_| Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))))
        .collect::<Vec<_>>();

    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(held_name.clone(), declaration);

    Module {
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
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    }
}

/// `Held : (n : Nat) -> Type 0 | yes() : (0)`, the family whose two instances the forged equation coerces between.
fn indexed_family(index: Free, nat_type: Term, zero: Term, result_sort: Term) -> InductDecl {
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
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// The module the doc comment below describes: four declarations, and the four definitions that close on `False`.
fn index_forgery() -> Module {
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
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    }
}

/// A nominal occurrence's parameters and indices are read by every rule that consults its declaration, and nothing typed them.
///
/// `at.rs` states the discipline this violated: an occurrence is meaningful only once what it carries has been checked against what the declaration declares, and three things had to hold. Two of them — the universe instance, and the parameter and index *counts* — moved behind the handle. The third was never written: that each argument inhabits the domain the arity states it at. Counts are the boundary's job because no typing rule reads a length; a *shape* is typing's, and this one had no rule at all.
///
/// The forgery is what reading one unestablished buys. `Eq(True, 0, 1)` is admitted as a type — `Sort::of` consults the declaration for its `result_sort` and hands back `Prop`, having checked two counts and nothing else — although `0` and `1` are `Nat`s standing in a domain the declaration says is `True`. It is then *inhabited*, and by the rule working correctly: `induct_type_args` compares the indices at the declared domain, that domain is `Prop`-sorted, and proof irrelevance discharges both without looking, so `refl(True, qed())` subsumes into it. From there every step is ordinary. Eliminating the forged equation under the motive `(s, t, q) => (Held(s)) -> Held(t)` — where the same gap lets `s` and `t`, typed `True`, stand in `Held`'s `Nat` index — yields `(Held(0)) -> Held(1)`, and `Held(1)` is uninhabited by construction, so the vacuous elimination coverage licenses (its only constructor targets `0`, which `peel_nat` clashes against `1`) proves `False`.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero** refusals for exactly this module, `let boom : False` included. No surface program reaches it — `curios-elab` elaborates a nominal occurrence as an application against the arity's telescope and checks every argument — which is why the certifier's copy of the rule went unwritten, and why the second opinion was worth nothing here.
///
/// Its control is [`an_indexed_occurrence_at_a_well_typed_index_is_accepted`], which keeps the same family at an index that genuinely inhabits `Nat`: without it, refusing every indexed occurrence would pass this.
#[test]
fn a_nominal_occurrence_types_its_arguments() {
    let verdicts = recheck_module_verdicts(&index_forgery(), 1_000_000);

    assert!(
        !verdicts.is_empty(),
        "the kernel certified a closed inhabitant of `False`",
    );
}

/// The control: an index that really is a `Nat` still types, so the guard above rejects a wrong argument rather than every argument.
#[test]
fn an_indexed_occurrence_at_a_well_typed_index_is_accepted() {
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let held_decl = indexed_family(
        Free::local(70, Some("n")),
        Term::intrinsic(Intrinsic::NatType),
        Term::intrinsic(Intrinsic::Nat(Nat::new(0usize))),
        Term::type_ground(),
    );

    let held = authored(
        &Global::Authored(Qualifier::from(["held"])),
        Term::induct_type(
            held_name.clone(),
            Vec::<Term>::new(),
            [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
        ),
        Term::variant(
            held_name.clone(),
            Vec::<Term>::new(),
            "yes",
            Vec::<Term>::new(),
        ),
    );

    let module = Module {
        items: vec![held],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(held_name, held_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    };

    assert_eq!(recheck_module_verdicts(&module, 1_000_000), Vec::new());
}

/// The same bogus occurrence, smuggled past [`a_nominal_occurrence_types_its_arguments`] through a Σ field.
///
/// Typing an occurrence's arguments closes the route only where something *types* the occurrence. A type former's parts are not typed: `infer` answers for a `FuncType` or a `TupleType` with `Sort::of`, which classifies each domain — consulting a declaration for its sort and checking nothing else — and that is the second, weaker way to accept a type `curios-cert/README.md` says this crate no longer has.
///
/// So `{Eq(True, 0, 1)}` is admitted, and the projection rule hands the field's declared type straight back: `v.0` is a scrutinee at the forged equation, and the rest of [`index_forgery`]'s derivation is unchanged. The codomain half is the same shape — `(Nat) -> Eq(True, 0, 1)` is admitted, and an application hands the codomain back — so both a `Proj` and an `Apply` reach it.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero** refusals for this module.
#[test]
fn a_bogus_occurrence_behind_a_tuple_field_is_refused() {
    let nat = |n: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(n)));

    let true_name = Global::Authored(Qualifier::from(["True"]));
    let equality_name = Global::Authored(Qualifier::from(["Eq"]));
    let true_type = Term::induct_type(true_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(
        true_name.clone(),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    );

    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam {
            telescope: Telescope::done(Vec::new()),
            plicities: Vec::new(),
        },
    )]);

    let equality_decl = equality_declaration();

    // v : {Eq(True, 0, 1)} = (refl(True, qed()))
    let bogus = Term::induct_type(equality_name.clone(), [true_type.clone()], [nat(0), nat(1)]);
    let wrapped = authored(
        &Global::Authored(Qualifier::from(["v"])),
        Term::tuple_type(vec![(Free::local(30, Some("b")), bogus)]),
        Term::tuple([Term::variant(
            equality_name.clone(),
            [true_type],
            "refl",
            [qed],
        )]),
    );

    let module = Module {
        items: vec![wrapped],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(true_name, true_decl), (equality_name, equality_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    };

    let verdicts = recheck_module_verdicts(&module, 1_000_000);

    assert!(
        !verdicts.is_empty(),
        "the kernel certified a bogus occurrence standing as a tuple field type",
    );
}

/// A declaration's domains are typed whatever sort the declaration lands in, and until they were, a `Prop`-sorted one had none of them typed at all.
///
/// `check_signature` owes clause 6 two things — that every constructor or field domain is *well-sorted*, and that a `Type`-sorted one sits at or below the family's declared level — and it ran the first inside the second. The size half is vacuous at a `Prop`-sorted result, `Prop` being impredicative, so `infer_type` sat behind `if let Some(..) = &sized` and never ran for such a declaration. Nothing else covers those positions: `check_arity` reaches an `induct`'s parameters and indices but not its constructor telescopes, a `struct` reaches no `check_arity` at all, and `check_non_informative` only *classifies* each field with `Sort::of`, the lookup that reads a claim rather than checking one.
///
/// The claim it reads is what turns the gap into a forgery. `Sort::of` classifies a stuck type-valued `match` by its **motive** — the same reading the large-elimination guard was lied to through, closed there by `check_motive` typing the motive where `infer` meets the elimination. A field type is never met by `infer`, so the lie was available again: `struct Wrap(b : Bool) : Prop { held : match b : Prop | false => Nat | true => Nat end }` classifies `Prop`, so the non-informativeness rule excuses the field, while `Wrap(true)` really carries a `Nat`.
///
/// From there the derivation is the one `check_non_informative`'s own documentation predicts. `Wrap(true)` is `Prop`-sorted, so irrelevance identifies `Wrap(true){0}` with `Wrap(true){1}`, and `refl(Wrap(true), Wrap(true){0})` therefore inhabits `Eq(Wrap(true), Wrap(true){0}, Wrap(true){1})` — the indices being compared at a `Prop`-sorted domain, which is the rule working correctly. Transporting along it under the *honest* motive `(s, t, q) => (Held(s.0)) -> Held(t.0)` yields `(Held(0)) -> Held(1)`, and `Held`'s only constructor targets `0`, so the vacuous elimination coverage licenses proves `False`.
///
/// Verified while the hole was open: `recheck_module_verdicts` returned **zero** refusals for exactly this module, `let boom : False` included. No surface program reaches it, and the reason is that `curios-elab` keeps the two clauses apart where this crate had fused them: `check_telescope_entries` types every declaration domain through `check_is_sort`, and `add_declaration_sizing` is a separate walk that returns early at a non-`Type` result. So this is constructed by hand, and the second opinion was worth nothing here.
///
/// Its control is [`a_proposition_carrying_a_computed_proof_is_still_accepted`], the same computed field type with arms that really are propositions: a fix refusing every `Prop`-sorted declaration, or every field type it could not read off syntactically, would fail it.
///
/// The refusal is required to name `Wrap` and not merely to exist, because every later item in the derivation is built on the forged field and would mismatch for a downstream reason once anything at all went wrong. What has to be refused is the *declaration*, at the arm the motive lied about: a `Nat` at `Type 0` checked against the `Prop` the motive claims.
#[test]
fn a_proposition_may_not_carry_a_computed_relevant_field() {
    let verdicts = recheck_module_verdicts(&computed_field_forgery(), 1_000_000);
    let wrap = Global::Authored(Qualifier::from(["Wrap"]));

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name.as_ref() == Some(&wrap)
                && matches!(verdict.error, KernelError::Mismatch { .. })),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The control: the same computed field type with `Prop`-sorted arms still declares, so the guard above refuses a motive that lies rather than every field a declaration has to reduce to read.
#[test]
fn a_proposition_carrying_a_computed_proof_is_still_accepted() {
    let true_name = Global::Authored(Qualifier::from(["True"]));
    let true_type = Term::induct_type(true_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let qed = Term::variant(
        true_name.clone(),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    );
    let true_decl = proposition(vec![(
        Atom::from("qed"),
        InductParam {
            telescope: Telescope::done(Vec::new()),
            plicities: Vec::new(),
        },
    )]);

    let wrap_name = Global::Authored(Qualifier::from(["Wrap"]));
    let module = Module {
        items: vec![wrapped_at_true(&wrap_name, qed)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(true_name, true_decl)]),
        struct_decls: BTreeMap::from([(
            wrap_name,
            computed_field_wrapper(true_type.clone(), true_type),
        )]),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    };

    assert_eq!(recheck_module_verdicts(&module, 1_000_000), Vec::new());
}

/// Every position at which a term stands as a *type* reaches the judgment, not the classifier — asserted at each position rather than argued once.
///
/// This crate has two ways to answer what sort a type has, and only one of them checks anything: `infer_sort` types a former's parts, `Sort::of` classifies them. `curios-cert/README.md` says the lookup "is reached only where typing has already run", and that sentence is a grep rather than a claim — which is why the same defect has now arrived three times at three different positions, as a nominal occurrence's arguments, as a type former's parts, and as a `Prop`-sorted declaration's domains.
///
/// A stuck `match` is what tells the two apart, because it is the one shape in Core that *states* its own sort instead of having one derived: `Sort::of` reads the motive, and the motive is a claim the term makes about itself. Where the judgment runs, `infer` reaches `check_motive` and the arms are checked against the motive, so `match b : Prop | false => Nat | true => Nat end` is refused by the arm — `Nat` at `Type 0` against the `Prop` the motive claims. Where only the classifier runs, that same term reads as a proposition and carries a `Nat`, which is the forgery [`a_proposition_may_not_carry_a_computed_relevant_field`] derives `False` from.
///
/// So the table below is one probe per position, each refused by that arm mismatch and by nothing else. The diagnostic is asserted rather than the mere presence of a verdict, because most of these types are uninhabitable while the lie stands: a body-driven refusal would name the body's own type and would pass a test that only counted verdicts.
///
/// Mutation-checked, which is what separates this from a fixture that asserts nothing: replacing `infer_telescope`'s `infer_type` with `Sort::of` — the exact weakening the three historical defects were — makes "a lambda's domain annotation" come back with **zero** verdicts while every other row stays refused.
///
/// A declaration's own domains are the seventh position and are not repeated here; [`a_proposition_may_not_carry_a_computed_relevant_field`] holds that one, and holds it with the derivation rather than with the position alone.
#[test]
fn no_type_position_admits_a_lying_motive() {
    for (position, module) in lying_type_positions() {
        let verdicts = recheck_module_verdicts(&module, 1_000_000);

        assert!(
            verdicts.iter().any(|verdict| matches!(
                &verdict.error,
                KernelError::Mismatch { inferred, expected }
                    if matches!(&***inferred, Subterm::Type(_))
                        && matches!(&***expected, Subterm::Prop)
            )),
            "{position} classified a lying motive instead of typing it: {verdicts:?}",
        );
    }
}

/// A stuck `match` on `scrutinee` whose motive claims `Prop` while both arms inhabit `Nat` — the one shape in Core that states its own sort instead of having one derived.
fn lying_type(scrutinee: &Free) -> Term {
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
fn lying_type_positions() -> Vec<(&'static str, Module)> {
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
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
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
fn computed_field_wrapper(false_case: Term, true_case: Term) -> StructDecl {
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
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// `Wrap(true)`, the instance at which the field type above reduces to its true arm.
fn wrap_at_true(wrap_name: &Global) -> Term {
    Subterm::StructType(StructType {
        name: wrap_name.clone(),
        universes: Vec::new(),
        params: vec![Term::intrinsic(Intrinsic::Bool(true))],
    })
    .into()
}

/// `wrapped : Wrap(true) = Wrap(true){held}`.
fn wrapped_at_true(wrap_name: &Global, held: Term) -> Item {
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
fn computed_field_forgery() -> Module {
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
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    }
}

/// A refusal names the types the way the program that produced them wrote them.
///
/// `KernelError`'s own `Display` is faithful to Core — fully qualified paths, every parameter positional — which is right for a term printed in isolation and wrong for a message a reader has to recognize their own program in. `format_with` supplies the two axes that fix it: globals shortened against the module's symbol table, and a nominal family's implicit parameters marked from the type constructor's declared plicities.
///
/// Universe instances are deliberately left alone; see `KernelError::format_with`.
#[test]
fn a_refusal_shortens_names_and_marks_implicit_parameters() {
    let name = Global::Authored(Qualifier::from(["demo", "Box", "Box"]));
    let parameter = Free::local(0, Some("A"));

    // `struct Box(@A : Type)`: one implicit parameter, so a use site writes `Box(Nat)` and never supplies it positionally.
    let constructor = Definition {
        name: name.clone(),
        kind: DefinitionKind::StructType,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        root: RootId::Entry,
        totality: Totality::Total,
        type_: Term::func_type_marked(
            [(Plicity::Implicit, parameter, Term::type_ground())],
            Term::type_ground(),
        ),
        body: Term::type_ground(),
    };

    let module = Module {
        items: vec![Item::Let(constructor)],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body: Term::intrinsic(Intrinsic::NatType),
    };

    let applied: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: vec![Term::intrinsic(Intrinsic::NatType)],
    })
    .into();
    let refusal = KernelError::Mismatch {
        inferred: Box::new(applied),
        expected: Box::new(Term::intrinsic(Intrinsic::NatType)),
    };

    assert_eq!(
        refusal.format_with(&module),
        "expected `Nat`, found `Box(@Nat)`"
    );
    // The faithful rendering keeps the qualified path and drops the mark, which is what makes the axes worth supplying.
    assert_eq!(
        refusal.to_string(),
        "expected `Nat`, found `/demo/Box/Box(Nat)`"
    );
}
