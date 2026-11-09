//! Which of a baseline's verdicts a walk adopts, and which declarations it judges again.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::test_support::*,
    curios_common::Qualifier,
    curios_core::{
        Applications, Certification, Certified, Definition, Definitions, Entrypoint, Free, Func,
        Intrinsic, Item, Level, Places, Program, Reads, Subterm, Term, Totality, UniverseParam,
        Universes, Variance,
    },
    std::collections::BTreeMap,
};
/// The fixture the trusted base is held by: a dependency's body changes, its dependent is presented as it was, and the walk judges the dependent again because its own record says it unfolded that body. `three : alias = 3` stood while `alias` was `Nat` and is refused once `alias` is `Bool`, whatever the caller took to be unchanged.
#[test]
fn a_declaration_presented_unchanged_is_judged_again_where_what_it_read_has_changed() {
    let baseline = written();
    let certification = walked(&baseline).certification;
    let edited = reading_definitions(Term::intrinsic(Intrinsic::BoolType), Term::free_var);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(
        rechecked
            .verdicts
            .iter()
            .map(|verdict| verdict.name)
            .collect::<Vec<_>>(),
        vec![Some(global("three"))],
    );
}

/// Over a baseline the walked module is a version of, the walk compares the two by what they do not share, judges the item that changed and asks what read it, and files the record a walk over every declaration of the module files: `identity` returns `7`.
#[test]
fn a_walk_over_a_version_files_what_a_walk_over_every_declaration_files() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let x = Free::local(10, Some("x"));
    let identity = authored(
        &global("identity"),
        Term::func_type([(x, nat())], nat()),
        Term::func([(x, nat())], literal(7)),
    );

    let rechecked = walked_over(
        &replaced(&baseline, "identity", identity),
        &baseline,
        &certification,
    );

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification,
        walked(&reading_definitions(nat(), |_| literal(7))).certification
    );
}

/// A declaration a recompile writes under a name a unit in scope declares is refused for the name over a baseline, as over every declaration, though the unit in scope is the one the baseline's walk read: `three` respelled as the environment's `shadowed`, the baseline's other declarations shared.
///
/// Mutation-checked: with a changed item never asked whether a unit in scope declares its name, the module is judged and `shadowed` refused where the walk writes it, twice.
#[test]
fn a_name_a_unit_in_scope_declares_written_over_a_baseline_is_refused_for_it() {
    let environment = already_judged(&judged_environment());
    let globals = environment.globals();
    let baseline = versioned(written());
    let certification = fixture_certified(&baseline, BUDGET, &globals, SYNTAX).certification;
    let edited = replaced(
        &baseline,
        "three",
        authored(&shadowed_name(), nat(), literal(3)),
    );
    let (places, now) = (Places::of(&baseline), Places::of(&edited));

    let rechecked = super::verdicts_from(
        &mut Kernel::new(BUDGET, SYNTAX),
        &edited,
        None,
        &globals,
        Some(super::Over {
            baseline: Baseline {
                definitions: &baseline,
                places: &places,
                certification: &certification,
            },
            places: &now,
            candidates: None,
        }),
    );

    assert!(
        matches!(
            rechecked.verdicts.as_slice(),
            [Verdict { name: Some(name), error: Error::Redeclared(_) }] if *name == shadowed_name()
        ),
        "a name the environment declares was written over a baseline: {:?}",
        rechecked.verdicts
    );
}

/// A scheme's constraint strengthened under the same type and body reaches the declaration that instantiated it, as a walk over every declaration does: `A<u, v | u + 2 <= v>` in place of `u + 1 <= v`, beside `user<u, v | u + 1 <= v> : Type v = A<u, v>`, whose instance discharged the constraint `A` stated and not the one it states now.
///
/// Mutation-checked: with a signature read digested without its scheme's constraints, `user` is adopted and the walk refuses nothing.
#[test]
fn a_schemes_constraint_strengthened_reaches_the_declaration_that_instantiated_it() {
    let (_, instance) = scheme_occurrences();
    let baseline = versioned(universe_scheme_definitions(Some((
        scheme_context(),
        instance,
    ))));
    let certification = walked(&baseline).certification;
    let place = Places::of(&baseline)
        .place(&global("A"))
        .expect("the module declares the scheme")
        .item();
    let Item::Let(scheme) = &baseline.items[place] else {
        panic!("the scheme is a definition");
    };
    let mut stronger = scheme_context();
    stronger.constraints[0].lower = Level::param(UniverseParam(0))
        .checked_add(2)
        .expect("level admits the offset");
    let edited = replaced(
        &baseline,
        "A",
        Item::Let(Box::new(Definition {
            universe_context: stronger,
            ..(**scheme).clone()
        })),
    );

    let whole = fixture_certified(&edited, BUDGET, &Globals::default(), SYNTAX);
    let over = walked_over(&edited, &baseline, &certification);

    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| verdict.name == Some(global("user"))
                && matches!(verdict.error, Error::UniverseInstance { .. })),
        "the control stopped refusing: `user`'s instance does not discharge `u + 2 <= v`: {:?}",
        whole.verdicts
    );
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A declaration the walk does not visit is read once the walk has passed its place: `alias` respelled to name `five`, stored after it, finds it unbound, as a walk over every declaration does, though nothing `five` read moved and the walk adopts `five` without visiting it.
///
/// Mutation-checked: with a declaration the walk does not visit read wherever it stands, `alias` finds `five` and is refused for its type instead.
#[test]
fn a_declaration_the_walk_does_not_visit_is_unbound_before_its_place() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let edited = replaced(
        &baseline,
        "alias",
        authored(
            &global("alias"),
            Term::type_ground(),
            Term::instance(global("five"), Universes::default()),
        ),
    );

    let whole = fixture_certified(&edited, BUDGET, &Globals::default(), SYNTAX);
    let over = walked_over(&edited, &baseline, &certification);

    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::Undeclared(_))),
        "a walk over every declaration finds `five` undeclared at `alias`: {:?}",
        whole.verdicts
    );
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A module that is the baseline's is adopted whole: every entry comes back as the baseline's record holds it, the mark with it.
#[test]
fn a_declaration_whose_terms_and_reads_stand_takes_the_baselines_verdict() {
    let baseline = written();
    let certification = marked();

    let rechecked = walked_over(&baseline, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(rechecked.certification, certification);
}

/// `base : (Nat) -> Nat`, total as `(n) => n` or partial as [`diverging_sink`]'s body, then `middle : Nat = base(1)`, `top : Nat = middle` and `apart : Nat = 7`: `top` reads `middle` and nothing of `base`.
fn chained_definitions(partial: bool) -> Definitions {
    let n = Free::local(10, Some("n"));
    let defined = |name: &str, type_: Term, body: Term| authored(&global(name), type_, body);
    let base = match (partial, diverging_sink()) {
        (true, Item::Let(sink)) => defined("base", sink.type_, sink.body),
        _ => defined(
            "base",
            Term::func_type([(n, nat())], nat()),
            Term::func([(n, nat())], Term::free_var(&n)),
        ),
    };

    Definitions {
        items: vec![
            base,
            defined(
                "middle",
                nat(),
                Term::apply(
                    Term::instance(global("base"), Universes::default()),
                    [literal(1)],
                ),
            ),
            defined(
                "top",
                nat(),
                Term::instance(global("middle"), Universes::default()),
            ),
            authored(&global("apart"), nat(), literal(7)),
        ]
        .into(),
    }
}

/// A verdict that moves reaches what read it through a declaration whose terms stand: `base` made partial is judged, `middle`, which read its signature, is judged and comes out partial, and so does `top`, which read `middle` alone and whose terms are the baseline's. `apart` reads nothing that moved and keeps the baseline's verdict.
///
/// Mutation-checked: with the declarations that read a moved one marked and theirs not, `top` keeps the baseline's `Total`.
#[test]
fn a_verdict_that_moves_reaches_what_read_it_through_a_declaration_whose_terms_stand() {
    let baseline = chained_definitions(false);
    let certification = walked(&baseline).certification;
    let edited = chained_definitions(true);

    let rechecked = walked_over(&edited, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    for name in ["base", "middle", "top"] {
        assert_eq!(
            rechecked.certification.totality(&global(name)),
            Some(Totality::Partial),
            "{name}",
        );
    }
    assert_eq!(
        rechecked.certification.totality(&global("apart")),
        Some(Totality::Total),
    );
}

/// An adopted verdict is the baseline's, partiality included: a record that calls `five` partial files it partial, and refuses nothing for it.
#[test]
fn an_adopted_verdict_is_the_baselines_partiality_included() {
    let baseline = written();
    let certification = doctored(&walked(&baseline).certification, "five", |certified| {
        Certified {
            totality: Totality::Partial,
            ..certified
        }
    });

    let rechecked = walked_over(&baseline, &baseline, &certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    assert_eq!(
        rechecked.certification.totality(&global("five")),
        Some(Totality::Partial),
    );
}

/// A registry entry is the baseline's by what the certifier reads of it: its terms, and the variance vector it carries, which `accept` holds to the vector the walk computes. Elaboration's other stamps take no part — the polarity vector, in whose place a walk holds the one the certifier computed, the owning module and the representation's privacy — so an entry differing from the baseline's in those alone is the baseline's, and one differing in its variance vector or in a field's type is not.
///
/// Mutation-checked against each comparison: taken over the whole entry, the stamped pair is another declaration; with the variance vector left out, the family carrying another is the baseline's; and with the arity left out the record is the baseline's.
#[test]
fn a_registry_entry_is_the_baselines_by_what_the_certifier_reads_of_it() {
    let held = two_registry_entries(Varied::Nothing);
    let family =
        |definitions: &Definitions| (*definitions.induct_decls()[&shadowed_family()]).clone();
    let record = |definitions: &Definitions| (*definitions.struct_decls()[&kept_record()]).clone();

    let stamped = two_registry_entries(Varied::Stamps);
    assert!(
        induct_alike(&family(&stamped), &family(&held))
            && struct_alike(&record(&stamped), &record(&held)),
        "an entry differing from the baseline's in elaboration's stamps alone is another declaration",
    );
    assert!(
        !induct_alike(
            &family(&two_registry_entries(Varied::Variance)),
            &family(&held)
        ),
        "a family differing from the baseline's in the variance vector it carries alone is the baseline's",
    );
    assert!(
        !struct_alike(
            &record(&two_registry_entries(Varied::Field)),
            &record(&held)
        ),
        "a record differing from the baseline's in a field's type is the baseline's",
    );
}

/// A forged `Certification` filing a genuinely partial proof as `Total`, mounted as a unit in scope, admits a looping proof of an empty proposition: the mounted record is believed without the walk that would refuse it.
///
/// `loop : Absurd = loop` recurses without descending, so a walk refuses it as a proof that may not terminate ([`totality-of-the-erased-program`]). Filed `Total` and mounted, nothing re-judges it: `Globals::mount` stands a scope entry at its type with its body, unjudged, and the reader's totality obligation reads the forged verdict (`Globals::partial`). The control is the same record filing it `Partial`, which refuses.
///
/// The input is forged — no walk of this crate produces this record — so this names what the boundary's ground, that a scope entry was judged by the walk that built it, does not itself check: the mounted `Certification`'s totality is believed, and nothing in the record (no digest, no checksum) is consulted to tell a forged verdict from one a walk filed. A stored unit's slot is guarded by the compiler digest and the verified read-set, which bind the record to this compiler over these inputs, not to a walk having concluded it.
#[test]
fn a_forged_total_verdict_mounted_in_scope_admits_a_looping_proof() {
    let loop_name = global("loop");
    let absurd_name = global("Absurd");
    let absurd = Term::induct_type(absurd_name, Vec::<Term>::new(), Vec::<Term>::new());
    let mounted = collision_definitions(
        vec![authored_proof(
            &loop_name,
            absurd.clone(),
            Term::instance(loop_name, Universes::default()),
        )],
        BTreeMap::from([(absurd_name, proposition(Vec::new()))]),
    );
    let reader = || Program {
        definitions: Definitions {
            items: Vec::new().into(),
        },
        entrypoint: Entrypoint {
            body: Term::instance(loop_name, Universes::default()),
            type_: Some(absurd.clone()),
        },
    };
    let certified = |totality: Totality| {
        Certification::of(
            mounted
                .items
                .iter()
                .flat_map(Item::declared_names)
                .map(|name| {
                    (
                        *name,
                        Certified {
                            totality,
                            applications: Applications::default(),
                            reads: Reads::default(),
                        },
                    )
                }),
            [],
            [],
            [],
        )
    };

    let forged = certified(Totality::Total);
    let scope = AlreadyJudged::with(&mounted, &forged);
    let admitted = fixture_certified(&reader(), BUDGET, &scope.globals(), SYNTAX).verdicts;
    assert_eq!(
        admitted,
        Vec::new(),
        "a forged Total verdict admitted a looping proof of Absurd: {admitted:?}",
    );

    // The control: the same record, honest, refuses — so the admission rode on the lie.
    let honest = certified(Totality::Partial);
    let honest_scope = AlreadyJudged::with(&mounted, &honest);
    let refused = fixture_certified(&reader(), BUDGET, &honest_scope.globals(), SYNTAX).verdicts;
    assert!(
        refused
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotTotal { .. })),
        "the control stopped refusing: an honest Partial must refuse the looping proof: {refused:?}",
    );
}

/// [`walked`] without its assertion, for a module a whole walk refuses.
fn walked_whole(definitions: &Definitions) -> Rechecked {
    fixture_certified(definitions, BUDGET, &Globals::default(), SYNTAX)
}

/// `Big : (T: Type 5) -> Type 6` with one constructor aiming at `L`, and `f(X: Type 0, b: Big(X)) -> X = match b at X | big() => Type 3 end`.
///
/// The arm's index inversion would refine `X`, an outer binder assumed at `Type 0`, to `L`, and keeps the refinement only where the judge holds `Sort::of(L)` below `Type 0` (`curios_analysis::invert`'s `fits_its_variable`). Kept, the arm's goal is `L`, which `Type 3` inhabits; dropped, the goal stays the abstract `X` and nothing does.
fn refining_definitions(body: Term) -> Definitions {
    refining_definitions_at(Term::instance(global("L"), Universes::default()), body)
}

/// [`refining_definitions_at`] with the variable assumed at `Type 0`.
fn refining_definitions_at(target: Term, body: Term) -> Definitions {
    refining_definitions_assumed(target, type_at(0), body)
}

/// [`refining_definitions`] with `target` as the index the constructor aims at and `assumed` as the sort the outer variable is assumed at.
fn refining_definitions_assumed(target: Term, assumed: Term, body: Term) -> Definitions {
    use curios_core::{
        Atom, Cases, InductArm, InductDecl, InductParam, Many, Scope, Telescope, UniverseContext,
        test_support::*,
    };

    let family = global("Big");
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(902, Some("i")), type_at(5))],
            (),
        )),
        constructors: vec![(
            Atom::from("big"),
            InductParam::new(Telescope::done(vec![target]), Vec::new()),
        )]
        .into(),
        result_sort: Term::type_at(Level::constant(6)),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let x = Free::local(910, Some("X"));
    let b = Free::local(911, Some("b"));
    let at = Term::induct_type(family, Vec::<Term>::new(), [Term::free_var(&x)]);
    let cases = Cases::Induct {
        cases: vec![(
            Atom::from("big"),
            InductArm::new(Scope::close(Many(0), &[], body), Vec::new()),
        )],
        default: None,
        stated: None,
    };
    let binders = [(x, assumed), (b, at.clone())];
    let item = authored(
        &global("f"),
        Term::func_type(binders.clone(), Term::free_var(&x)),
        Term::func(
            binders,
            match_ambient(Term::free_var(&b), Term::free_var(&x), cases),
        ),
    );

    declaring(
        Definitions {
            items: vec![item].into(),
        },
        BTreeMap::from([(family, declaration)]),
        [],
        [],
    )
}

/// The one gate that reads a published level and could admit a term — an index refinement the published sort makes `fits_its_variable` keep — reaches no closed term: the arm is behind a hypothesis the sort the variable is assumed at leaves uninhabited, and the arm's goal, which the publication cannot move, is the case's own target.
#[test]
fn a_kept_index_refinement_above_the_variables_sort_reaches_no_closed_term() {
    let honest = chained_sort_definitions(type_at(4), type_at(5), type_at(5), type_at(5));
    let record = walked(&honest).certification;
    let scope = AlreadyJudged::with(&honest, &record);
    let verdicts = |definitions: Definitions| {
        fixture_certified(&definitions, BUDGET, &scope.globals(), SYNTAX).verdicts
    };

    // The arm's body is typed, so what the goal is decides: a body the refined goal does not admit is refused.
    let bogus = verdicts(refining_definitions(literal(0)));
    assert!(!bogus.is_empty(), "the arm's body is typed: {bogus:?}");

    // The refinement is kept: `Type 3` inhabits the refined goal.
    assert_eq!(
        verdicts(refining_definitions(type_at(3))),
        Vec::new(),
        "the arm is admitted",
    );

    // What the publication cannot move: the arm's goal is the case's target at every sort the variable is assumed at, since `MatchResult::at` opens the goal at the targets and reads no sort. `Type 3` inhabits `Type 4` and `0` does not, whether the rule holds the target's sort below the variable's (`Type 5`) or not (`Type 0`, `Type 1`).
    for assumed in [0, 1, 5] {
        assert_eq!(
            walked_whole(&refining_definitions_assumed(
                type_at(4),
                type_at(assumed),
                type_at(3)
            ))
            .verdicts,
            Vec::new(),
            "the goal is the target with the variable assumed at Type {assumed}",
        );
        assert!(
            !walked_whole(&refining_definitions_assumed(
                type_at(4),
                type_at(assumed),
                literal(0)
            ))
            .verdicts
            .is_empty(),
            "and a body the target does not admit is refused at Type {assumed}",
        );
    }

    // The refusal that stops the route: the arm is reachable only through a value of the family at the target, and the target does not inhabit the sort the variable is assumed at, which `subsumes` decides off the declared type and not off any publication.
    let applied = walked_whole(&applying_definitions()).verdicts;
    assert!(
        applied
            .iter()
            .any(|verdict| verdict.name == Some(global("g"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "nothing closed reaches the arm: {applied:?}",
    );
}

/// [`refining_definitions_at`]'s family and function, with `g : Type 4 = f(Type 4, Big/big())` beside them: the one call the arm is reachable through.
fn applying_definitions() -> Definitions {
    let definitions = refining_definitions_at(type_at(4), type_at(3));
    let mut items = definitions.items.clone();
    items.push(authored(
        &global("g"),
        type_at(4),
        Term::apply(
            Term::instance(global("f"), Universes::default()),
            [
                type_at(4),
                Term::variant(global("Big"), Vec::<Term>::new(), "big", Vec::<Term>::new()),
            ],
        ),
    ));

    Definitions { items }
}

/// `induct Levelled.{u} : Type.{u + 1} | mk(x: Type.{u}) end`, whose entry carries `variances`: a payload's type mentions the level, so the walk computes it invariant.
fn varied_family(variances: Vec<Variance>) -> Definitions {
    use curios_common::Plicity;
    use curios_core::{Atom, InductDecl, InductParam, Telescope, UniverseContext, test_support::*};

    let family = global("Levelled");
    let level = Level::param(UniverseParam(0));
    let declaration = InductDecl {
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(Free::local(930, Some("x")), Term::type_at(level.clone()))],
                    Vec::new(),
                ),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_at(level.checked_add(1).expect("level admits the offset")),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances,
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

/// `accept` refuses a carried variance vector claiming an irrelevance the walk denies, and `induct_alike` reads the carried vector, so an item whose terms are the baseline's and whose vector is not is judged again, and refused, as a walk over every declaration refuses it: whole and baseline walks agree, and no unit is stored carrying a vector its walk denies, which a later unit's elaborator would compare instances by.
#[test]
fn a_carried_variance_the_walk_denies_is_refused_over_its_baseline_as_over_none() {
    let truthful = versioned(varied_family(vec![Variance::Invariant]));
    let certification = walked(&truthful).certification;
    let claimed = versioned(varied_family(vec![Variance::Irrelevant]));

    let whole = walked_whole(&claimed);
    assert!(
        whole
            .verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::VarianceDenied { .. })),
        "a whole walk refuses the carried irrelevance: {:?}",
        whole.verdicts,
    );

    let over = walked_over(&claimed, &truthful, &certification);
    assert!(
        over.verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::VarianceDenied { .. })),
        "and the walk over the baseline adopts the carried irrelevance: {:?}",
        over.verdicts,
    );
    assert_eq!(
        over.certification.variances(&global("Levelled")),
        Some([Variance::Invariant].as_slice()),
        "while the vector it files, which every judgment reads, is the one the baseline's walk computed",
    );
}

/// `A.{u, v} : Type v = Type u` as a one-member group under `universe_context`, the scheme [`universe_scheme_definitions`] declares as a `let`: its body inhabits `Type (u + 1)`, so it fits the declared type only where the declaration assumes `u + 1 <= v`.
fn grouped_scheme(universe_context: curios_core::UniverseContext) -> Item {
    use curios_core::{DefinitionKind, RecItem};

    let member = Free::local(70, Some("A"));
    let definition = Definition {
        name: global("A"),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::type_at(Level::param(UniverseParam(0))),
    };

    Item::Rec(RecItem::new(vec![(member, definition)]))
}

/// A group's universe context dropped: the terms are the baseline's, the context is not, and `Baseline::holds` reads a group by its members' terms, names and marks alone.
#[test]
fn a_groups_universe_context_dropped_is_judged_again_as_a_whole_compile_judges_it() {
    let baseline = versioned(Definitions {
        items: vec![grouped_scheme(scheme_context())].into(),
    });
    let certification = walked(&baseline).certification;
    let edited = replaced(&baseline, "A", grouped_scheme(open_context()));

    let whole = walked_whole(&edited);
    assert!(
        !whole.verdicts.is_empty(),
        "a whole walk refuses the group whose declaration assumes nothing: {:?}",
        whole.verdicts,
    );

    let over = walked_over(&edited, &baseline, &certification);
    assert_eq!(over.verdicts, whole.verdicts);
}

/// A binder no fixture here spells, minted far above the small indices the suites use.
fn minting() -> impl Fn(&'static str) -> Free {
    let counter = std::cell::Cell::new(2000);

    move |hint: &'static str| {
        counter.set(counter.get() + 1);
        Free::local(counter.get(), Some(hint))
    }
}

/// Hurkens' paradox with `star` its sort, over the four products a record hands back, as `xboard`'s formation ticket for a group read under its reader's constraints spells one: `bottom`, `(A: star) -> A`; `small_universe`, its `U`; `over`, the product of a family over `℘S`; and `pi`, the product of a family over `S`. A term of `bottom`, its binders minted by `fresh`.
fn hurkens(
    fresh: &impl Fn(&'static str) -> Free,
    star: &Term,
    bottom: &Term,
    small_universe: &Term,
    over: &Term,
    pi: &Term,
) -> Term {
    let star = star.clone();
    let var = Term::free_var;
    let not = |phi: Term| Term::func_type([(fresh("h"), phi)], bottom.clone());
    let power = |set: Term| Term::func_type([(fresh("s"), set)], star.clone());
    let universe = {
        let (x, f) = (fresh("X"), fresh("f"));
        let ppx = power(power(var(&x)));

        Term::func_type(
            [
                (x, star.clone()),
                (f, Term::func_type([(fresh("t"), ppx.clone())], var(&x))),
            ],
            ppx,
        )
    };
    let pu = power(universe.clone());
    let ppu = power(pu.clone());
    // over(U, (p) => body(p)), the product over ℘U at `star`.
    let forall = |body: &dyn Fn(&Free) -> Term| {
        let p = fresh("p");

        Term::apply(
            over.clone(),
            [universe.clone(), Term::func([(p, pu.clone())], body(&p))],
        )
    };
    // pi(U, (x) => body(x)), the product over U at `star`.
    let every = |body: &dyn Fn(&Free) -> Term| {
        let x = fresh("x");

        Term::apply(
            pi.clone(),
            [
                universe.clone(),
                Term::func([(x, universe.clone())], body(&x)),
            ],
        )
    };

    // τ t = (X, f) => (p) => t((x) => p(f(x(X, f)))).
    let tau = {
        let (t, x, f, p, y) = (fresh("t"), fresh("X"), fresh("f"), fresh("p"), fresh("x"));
        let ppx = power(power(var(&x)));

        Term::func(
            [(t, ppu.clone())],
            Term::func(
                [
                    (x, star.clone()),
                    (f, Term::func_type([(fresh("t"), ppx)], var(&x))),
                ],
                Term::func(
                    [(p, power(var(&x)))],
                    Term::apply(
                        var(&t),
                        [Term::func(
                            [(y, universe.clone())],
                            Term::apply(
                                var(&p),
                                [Term::apply(
                                    var(&f),
                                    [Term::apply(var(&y), [var(&x), var(&f)])],
                                )],
                            ),
                        )],
                    ),
                ),
            ),
        )
    };
    let tau_at = |t: Term| Term::apply(tau.clone(), [t]);

    // σ s = s(U, (t) => τ t).
    let sigma = {
        let (s, t) = (fresh("s"), fresh("t"));

        Term::func(
            [(s, universe.clone())],
            Term::apply(
                var(&s),
                [
                    small_universe.clone(),
                    Term::func([(t, ppu.clone())], tau_at(var(&t))),
                ],
            ),
        )
    };
    let sigma_at = |s: Term| Term::apply(sigma.clone(), [s]);
    let holds = |s: Term, p: Term| Term::apply(sigma_at(s), [p]);
    let round = |s: Term| tau_at(sigma_at(s));

    let shifted = |p: &Free| {
        let y = fresh("y");

        Term::func(
            [(y, universe.clone())],
            Term::apply(var(p), [round(var(&y))]),
        )
    };
    let claim = |s: &Term| {
        forall(&|p| {
            Term::func_type(
                [(fresh("h"), holds(s.clone(), var(p)))],
                Term::apply(var(p), [round(s.clone())]),
            )
        })
    };

    // Δ = (y) => ¬claim(y).
    let delta = {
        let y = fresh("y");

        Term::func([(y, universe.clone())], not(claim(&var(&y))))
    };

    // Ω = τ((p) => (x : U) -> (h: σ x p) -> p x).
    let omega = {
        let p = fresh("p");

        tau_at(Term::func(
            [(p, pu.clone())],
            every(&|x| {
                Term::func_type(
                    [(fresh("h"), holds(var(x), var(&p)))],
                    Term::apply(var(&p), [var(x)]),
                )
            }),
        ))
    };

    // lem1 = (p, h1) => h1(Ω)((x) => (h2) => h1(τ(σ x))(h2)).
    let lem1 = {
        let (p, h1, x, h2, x1) = (fresh("p"), fresh("h1"), fresh("x"), fresh("h2"), fresh("x"));
        let premise = Term::func_type(
            [(x1, universe.clone())],
            Term::func_type(
                [(fresh("h"), holds(var(&x1), var(&p)))],
                Term::apply(var(&p), [var(&x1)]),
            ),
        );

        Term::func(
            [(p, pu.clone()), (h1, premise)],
            Term::apply(
                Term::apply(var(&h1), [omega.clone()]),
                [Term::func(
                    [(x, universe.clone())],
                    Term::func(
                        [(h2, holds(var(&x), shifted(&p)))],
                        Term::apply(Term::apply(var(&h1), [round(var(&x))]), [var(&h2)]),
                    ),
                )],
            ),
        )
    };

    // lem2 = lem1(Δ, (x) => (h2) => (h3) => h3(Δ)(h2)((p) => (h4) => h3((y) => p(τ(σ y)))(h4))).
    let lem2 = {
        let (x, h2, h3, p, h4) = (
            fresh("x"),
            fresh("h2"),
            fresh("h3"),
            fresh("p"),
            fresh("h4"),
        );

        Term::apply(
            lem1.clone(),
            [
                delta.clone(),
                Term::func(
                    [(x, universe.clone())],
                    Term::func(
                        [(h2, holds(var(&x), delta.clone()))],
                        Term::func(
                            [(h3, claim(&var(&x)))],
                            Term::apply(
                                Term::apply(Term::apply(var(&h3), [delta.clone()]), [var(&h2)]),
                                [Term::func(
                                    [(p, pu.clone())],
                                    Term::func(
                                        [(h4, holds(round(var(&x)), var(&p)))],
                                        Term::apply(
                                            Term::apply(var(&h3), [shifted(&p)]),
                                            [var(&h4)],
                                        ),
                                    ),
                                )],
                            ),
                        ),
                    ),
                ),
            ],
        )
    };

    // lem3 = (p) => (h) => lem1((y) => p(τ(σ y)), h).
    let lem3 = {
        let (p, h) = (fresh("p"), fresh("h"));

        Term::func(
            [(p, pu.clone())],
            Term::func(
                [(h, holds(omega.clone(), var(&p)))],
                Term::apply(lem1.clone(), [shifted(&p), var(&h)]),
            ),
        )
    };

    Term::apply(lem2, [lem3])
}

/// `u + 1 <= w`, `v <= w`, `v + 1 <= x`, `w + 1 <= x`: the cumulativity the carrier's four products need, as `xboard`'s formation ticket states them.
fn carrier_context() -> curios_core::UniverseContext {
    use curios_core::{UniverseConstraint, UniverseContext};

    let param = |index: usize| Level::param(UniverseParam(index));
    let raised = |index: usize| {
        param(index)
            .checked_add(1)
            .expect("level admits the offset")
    };
    let bounded = |lower: Level, upper: Level| UniverseConstraint { lower, upper };

    UniverseContext {
        parameter_count: 4,
        quality_count: 0,
        constraints: vec![
            bounded(raised(0), param(2)),
            bounded(param(1), param(2)),
            bounded(raised(1), param(3)),
            bounded(raised(2), param(3)),
        ],
    }
}

/// The same four parameters with none of the cumulativity declared: the context the filed unit carries.
fn open_carrier_context() -> curios_core::UniverseContext {
    use curios_core::UniverseContext;

    UniverseContext {
        parameter_count: 4,
        quality_count: 0,
        constraints: Vec::new(),
    }
}

/// `carrier.{u, v, w, x} : <the record a one-member group computes> = <the four products>`, as a one-member top-level group under `universe_context`.
///
/// The declaration is `xboard`'s formation ticket's `carrier`, a `let` there and a group here: a group's context is what [`Baseline::holds`] does not read, and a `let`'s is.
fn carrier_group(universe_context: curios_core::UniverseContext) -> Item {
    use curios_core::{DefinitionKind, RecGroup, RecItem};

    let param = |index: usize| Level::param(UniverseParam(index));
    let fresh = minting();
    let var = Term::free_var;
    let small = Term::type_at(param(0));
    let sized = Term::type_at(param(1));
    let large = Term::type_at(param(2));
    let power = |set: Term, sort: &Term| Term::func_type([(fresh("s"), set)], sort.clone());

    // { bottom: Type w, universe: Type w,
    //   over: (S: Type v, F: (℘S) -> Type u) -> Type w,
    //   pi:   (S: Type v, F: (S) -> Type u) -> Type w }
    let family =
        |set: &Free| Term::func_type([(fresh("p"), power(var(set), &small))], small.clone());
    let fibre = |set: &Free| Term::func_type([(fresh("x"), var(set))], small.clone());
    let set = fresh("S");
    let over_type = Term::func_type(
        [(set, sized.clone()), (fresh("F"), family(&set))],
        large.clone(),
    );
    let set = fresh("S");
    let pi_type = Term::func_type(
        [(set, sized.clone()), (fresh("F"), fibre(&set))],
        large.clone(),
    );
    let products_type = Term::tuple_type([
        (fresh("bottom"), large.clone()),
        (fresh("universe"), large.clone()),
        (fresh("over"), over_type),
        (fresh("pi"), pi_type),
    ]);

    let products = RecGroup::closing(vec![(
        fresh("products"),
        Term::type_at(param(3)),
        products_type,
    )]);
    let carrier_type = Term::rec_proj(products, 0);

    // The four products themselves, checked against the record's field types under the declaration's constraints.
    let a = fresh("A");
    let bottom_body = Term::func_type([(a, small.clone())], var(&a));
    let (big_x, f) = (fresh("X"), fresh("f"));
    let ppx = power(power(var(&big_x), &small), &small);
    let universe_body = Term::func_type(
        [
            (big_x, small.clone()),
            (f, Term::func_type([(fresh("t"), ppx.clone())], var(&big_x))),
        ],
        ppx,
    );
    let (set, big_f, p) = (fresh("S"), fresh("F"), fresh("p"));
    let over_body = Term::func(
        [(set, sized.clone()), (big_f, family(&set))],
        Term::func_type(
            [(p, power(var(&set), &small))],
            Term::apply(var(&big_f), [var(&p)]),
        ),
    );
    let (set, big_f, big_y) = (fresh("S"), fresh("F"), fresh("x"));
    let pi_body = Term::func(
        [(set, sized.clone()), (big_f, fibre(&set))],
        Term::func_type(
            [(big_y, var(&set))],
            Term::apply(var(&big_f), [var(&big_y)]),
        ),
    );
    let carrier_body = Term::tuple([bottom_body, universe_body, over_body, pi_body]);

    let definition = Definition {
        name: global("carrier"),
        kind: DefinitionKind::Authored,
        universe_context,
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: carrier_type,
        body: carrier_body,
    };

    Item::Rec(RecItem::new(vec![(fresh("carrier"), definition)]))
}

/// `paradox : False = <Hurkens' paradox over the carrier read at `{0, 0, 0, 1}`, boxed>`, closing on it: the unit a reader of the filed group compiles.
///
/// The instance sets `u`, `v` and `w` to zero, where the declaration's own constraints say `u + 1 <= w`: the four products then quantify over `Type 0` and stand at `Type 0`, which are the impredicative products the paradox needs.
fn paradox_module() -> Program {
    use curios_core::{
        StructDecl, StructType, Subterm, Telescope, UniverseContext, test_support::*,
    };

    let fresh = minting();
    let one = Level::zero().succ().expect("level zero has a successor");
    let read = Term::instance(
        global("carrier"),
        vec![
            Level::zero(),
            one.clone(),
            Level::zero(),
            one.clone().succ().expect("a level has a successor"),
        ],
    );
    let product = |index: usize| Term::proj(read.clone(), index);

    let false_name = global("False");
    let false_type = || Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let boxed_name = global("Boxed");
    // `struct Boxed : Type 0 { proof : False }`, the carrier the paradox is instantiated at: `False` is a proposition and the products quantify over `Type 0`.
    let boxed_decl = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build([(fresh("proof"), false_type())], ())),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let boxed_type: Term = Subterm::StructType(StructType {
        name: boxed_name,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();

    let looped = hurkens(
        &fresh,
        &Term::type_ground(),
        &product(0),
        &product(1),
        &product(2),
        &product(3),
    );
    let paradox = authored(
        &global("paradox"),
        false_type(),
        Term::proj_struct(Term::apply(looped, [boxed_type]), boxed_name, 0),
    );

    Program {
        definitions: declaring(
            Definitions {
                items: vec![paradox].into(),
            },
            BTreeMap::from([(false_name, proposition(Vec::new()))]),
            BTreeMap::from([(boxed_name, boxed_decl)]),
            [],
        ),
        entrypoint: Entrypoint {
            body: Term::instance(global("paradox"), Universes::default()),
            type_: Some(false_type()),
        },
    }
}

/// Hurkens' paradox through a group whose cumulativity a recompile dropped, the route `Baseline::holds` once opened: the walk over the baseline refuses the group as a walk over every declaration refuses it, so no unit is filed carrying a context no walk assumed, and the paradox read through the carrier the baseline's walk judged is refused by the instance that cumulativity does not discharge.
#[test]
fn hurkens_paradox_through_a_group_offered_without_its_context_is_refused() {
    let baseline = versioned(Definitions {
        items: vec![carrier_group(carrier_context())].into(),
    });
    let certification = walked(&baseline).certification;
    let offered = replaced(&baseline, "carrier", carrier_group(open_carrier_context()));

    let over = walked_over(&offered, &baseline, &certification);
    assert!(
        !over.verdicts.is_empty(),
        "the walk adopted a group its context no longer licenses",
    );
    assert_eq!(over.verdicts, walked_whole(&offered).verdicts);

    let judged = AlreadyJudged::with(&baseline, &certification);
    let refused = fixture_verdicts(&paradox_module(), BUDGET, &judged.globals(), SYNTAX);
    assert!(
        refused
            .iter()
            .any(|verdict| matches!(verdict.error, Error::UniverseInstance { .. })),
        "the paradox's instance does not discharge the carrier's constraints, and was not refused for it: {refused:?}",
    );
}

/// An item the elaborator made again, its terms the baseline's but for its binders' names and marks, is handed back with what the baseline's walk stated of it under the names it was written with: `lemma`'s second binder binds a proof, and its binders are `Q` and `k` though the baseline's are `P` and `h`.
#[test]
fn an_item_made_again_is_handed_back_with_the_baselines_statements_under_its_own_names() {
    let lemma = |names: [&str; 2], proofs: &[bool]| {
        let [p, h] =
            [(20, names[0]), (21, names[1])].map(|(index, hint)| Free::local(index, Some(hint)));
        let binders = [(p, Term::prop()), (h, Term::free_var(&p))];
        let Subterm::Func(Func { telescope }) =
            Term::unwrap_or_clone(Term::func(binders.clone(), Term::free_var(&h)))
        else {
            panic!("a function literal");
        };

        authored(
            &global("lemma"),
            Term::func_type(binders, Term::free_var(&p)),
            Term::from(Subterm::Func(Func {
                telescope: telescope.with_proofs(proofs),
            })),
        )
    };
    let stated = walked(&collision_definitions(
        vec![lemma(["P", "h"], &[false, false])],
        BTreeMap::new(),
    ));
    let baseline = versioned(stated.definitions);
    let edited = replaced(&baseline, "lemma", lemma(["Q", "k"], &[true, false]));

    let rechecked = walked_over(&edited, &baseline, &stated.certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    let handed = rechecked
        .definitions
        .items
        .iter()
        .find_map(|item| match item {
            Item::Let(definition) if definition.name == global("lemma") => {
                Some(definition.body.clone())
            }
            _ => None,
        })
        .expect("the module holds `lemma`");
    let Subterm::Func(Func { telescope }) = &*handed else {
        panic!("a function literal");
    };
    assert_eq!(telescope.proofs(), [false, true]);
    assert_eq!(telescope.labels(), ["Q", "k"]);
}

/// A module that declares a name twice reaches no walk over a baseline: `three` respelled as a second `identity`, a name the baseline holds at a place the edit leaves alone and no unit in scope declares, so the clause that would answer is [`declares_a_held_name`]'s reading of the baseline's places.
///
/// It never gets there. A walk over a baseline is handed where the module holds each name, and `Places::of` asserts a name is declared once, so the duplicate refuses the compilation ahead of the walk's own `Error::Redeclared` — a panic where it refuses, as the soundness board's fourth boundary allows. The whole-module walk, handed no places, is the one `redeclarations` guards, and `xboard`'s admission ticket is its witness.
#[test]
#[should_panic(expected = "a module declares one name twice")]
fn a_module_that_declares_a_name_twice_reaches_no_walk_over_a_baseline() {
    let baseline = versioned(written());
    let certification = walked(&baseline).certification;
    let x = Free::local(11, Some("x"));
    let edited = replaced(
        &baseline,
        "three",
        authored(
            &global("identity"),
            Term::func_type([(x, nat())], nat()),
            Term::func([(x, nat())], Term::free_var(&x)),
        ),
    );

    walked_over(&edited, &baseline, &certification);
}

/// Two items the elaborator made again, each standing where the other stood, are handed back with what the baseline's walk stated of the item of their own name, under their own binders' names: `lemma`'s second binder binds a proof and `datum`'s does not, though the module states each the other way round and holds each at the other's place.
///
/// Mutation-check to make: with the item whose statements are carried taken where the baseline holds the place the module holds the item at, rather than where it holds the item's first name (`held_item`), each takes the other's.
#[test]
fn items_made_again_in_each_others_places_take_the_statements_of_their_own_names() {
    let identity = |name: &str, sort: Term, names: [&str; 2], proofs: &[bool]| {
        let [domain, value] =
            [(30, names[0]), (31, names[1])].map(|(index, hint)| Free::local(index, Some(hint)));
        let binders = [(domain, sort), (value, Term::free_var(&domain))];
        let Subterm::Func(Func { telescope }) =
            Term::unwrap_or_clone(Term::func(binders.clone(), Term::free_var(&value)))
        else {
            panic!("a function literal");
        };

        authored(
            &global(name),
            Term::func_type(binders, Term::free_var(&domain)),
            Term::from(Subterm::Func(Func {
                telescope: telescope.with_proofs(proofs),
            })),
        )
    };
    let stated = walked(&collision_definitions(
        vec![
            identity("lemma", Term::prop(), ["P", "h"], &[false, false]),
            identity("datum", Term::type_ground(), ["A", "a"], &[false, false]),
        ],
        BTreeMap::new(),
    ));
    let baseline = versioned(stated.definitions);
    let places = Places::of(&baseline);
    let at = |name: &str| {
        places
            .place(&global(name))
            .expect("the baseline declares the name")
            .item()
    };
    let mut items = baseline.items.clone();
    items.replace(
        at("lemma"),
        identity("datum", Term::type_ground(), ["B", "b"], &[false, true]),
    );
    items.replace(
        at("datum"),
        identity("lemma", Term::prop(), ["Q", "k"], &[false, false]),
    );

    let rechecked = walked_over(&Definitions { items }, &baseline, &stated.certification);

    assert_eq!(rechecked.verdicts, Vec::new());
    let statements = |name: &str| {
        let body = rechecked
            .definitions
            .items
            .iter()
            .find_map(|item| match item {
                Item::Let(definition) if definition.name == global(name) => {
                    Some(definition.body.clone())
                }
                _ => None,
            })
            .expect("the module holds the name");
        let Subterm::Func(Func { telescope }) = &*body else {
            panic!("a function literal");
        };

        (
            telescope.proofs(),
            telescope
                .labels()
                .into_iter()
                .map(str::to_string)
                .collect::<Vec<_>>(),
        )
    };
    assert_eq!(
        statements("lemma"),
        (vec![false, true], vec!["Q".to_string(), "k".to_string()])
    );
    assert_eq!(
        statements("datum"),
        (vec![false, false], vec!["B".to_string(), "b".to_string()])
    );
}
