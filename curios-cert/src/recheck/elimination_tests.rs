//! Motives and result sorts, and the large-elimination guard a vacuous elimination must not skip.

use {
    super::test_support::*,
    crate::{Counted, Error, Globals},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, Many, Module, Nat, Quality,
        Scope, Subterm, Telescope, Term, UniverseContext, test_support::declaring,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

/// A declared result sort that merely *reduces* to `Prop` silences the index guard that reads it syntactically.
///
/// `Sort::of` decides irrelevance by reducing `result_sort`, and so do `check_signature` and `check_non_informative`. Index inversion does not: its `Prop`-valued guard — the rule that stops a proposition's constructors from being told apart, because irrelevance says they are the same value — matches `Subterm::Sort(Quality::Prop, _)` on the nose. A family declared at `((s : Type 0) => s)(Prop)` is therefore a proposition to every consumer except the one whose silence is unsound.
///
/// The consequence below is the vacuous-elimination route. `Two` is that family, with constructors `a` and `b`; `Held(t : Two)` has one constructor targeting `Two/a()`. Irrelevance makes `Two/a()` and `Two/b()` convertible, so `Held/mk()` inhabits `Held(Two/b())` — and then eliminating that value with *no arms at all* is accepted, because inversion clashes `Two/b()` against `Two/a()` and reports the only constructor impossible. The motive is `False`.
///
/// Without the literal-sort clause the walk would certify `let forged : False` from a value that exists. No `.crs` file can spell the sort — the surface grammar admits only the literal keywords `Type` and `Prop` after a declaration's `:` — and the elaborator builds no such entry, so only a fixture reaches it.
///
/// The control is the same module with `Two` at `Type 0`, where the clash is genuine and the empty elimination must stay accepted — general vacuous elimination is how an indexed family rules its impossible cases out, and refusing it is how this hole would be shut with a brick.
#[test]
fn a_result_sort_that_only_reduces_to_a_sort_is_refused() {
    let verdicts = fixture_verdicts(
        &aliased_sort_forgery(),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotASort(_))),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The same route with `Two` declared at the literal `Prop`, which the inversion's tag test reads: irrelevance makes `Two/a()` and `Two/b()` one value, so `Held/mk()` is at `Held(Two/b())` and its arm cannot be excused.
#[test]
fn an_empty_elimination_where_a_propositions_two_constructors_differ_is_refused() {
    let verdicts = fixture_verdicts(
        &clashing_index_forgery(Term::prop()),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::MissingArm { .. })),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_at_a_relevant_index_is_still_accepted() {
    let verdicts = fixture_verdicts(
        &relevant_index_control(),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.is_empty(),
        "an arm the index targets genuinely clash with was refused: {verdicts:?}",
    );
}

/// Coverage holds the *sole-arm* elimination too, which is the shape reduction fires rather than leaves standing.
///
/// A closed match with one arm, no catch-all and no payload named is its arm's body with its scrutinee unevaluated (`Cases::unread`, which the kernel's `whnf`, the closed machine and `curios-elab`'s reducer each state), so the arm's body stands at the elimination's own type whatever the scrutinee is. The vacuous elimination above is the shape `check_induct_arms` deliberately skips the large-elimination guard for; this is the shape whose arm the guard does reach and whose omission coverage must still decide.
///
/// `Pin(1)` below is inhabited by `Pin/no()`, and the `yes` arm's target `0` clashes with the index, so `specialize` reports the case impossible and no rule types that arm. Only coverage is left: `no` has no arm and is not impossible at `Pin(1)`. Excused, `forged(Pin/no())` would reduce to the `yes` arm's body — the sort `Prop`, a term nothing typed standing at `False`.
///
/// Only a fixture reaches the kernel's own loop with this shape: `curios-elab` refuses the same sole-arm match with `MatrixUncovered` while the module is still being built, so no surface program leaves one behind and `curios`'s `tests::board::index_tests` reads the elaborator's matrix rather than this.
///
/// Mutation-checked, which is what makes this a forgery rather than a refusal: with the coverage loop's `MissingArm` suppressed, the walk returns **zero** verdicts over this module, so the entry is certified at `False`.
#[test]
fn a_sole_arm_elimination_must_still_rule_out_the_constructors_it_omits() {
    let verdicts = fixture_verdicts(&sole_arm_forgery(), 1_000_000, &Globals::default(), SYNTAX);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::MissingArm { .. })),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

/// The control: the same two constructors at `Pin(0)`, where `no` is impossible and `yes` is the arm reduction takes. The sole-arm elimination must stay accepted, since it is the form a cast along a proof is written in; refusing every match with one arm is how the refusal above would be reached with a brick.
#[test]
fn a_sole_arm_elimination_whose_omissions_are_ruled_out_is_still_accepted() {
    let verdicts = fixture_verdicts(&sole_arm_control(), 1_000_000, &Globals::default(), SYNTAX);

    assert!(
        verdicts.is_empty(),
        "a cast along a proof whose other case the index rules out was refused: {verdicts:?}",
    );
}

/// A family that declares one tag twice, where the shadowed entry is the one a coverage decision is about.
///
/// A tag is the elimination key and the runtime index, and every lookup resolves one by *first match*. So a repeat hides a constructor instead of adding one, and the rules that walk `constructors` entry by entry answer about the first one once per entry. Coverage is where that shows: `Held(t : Two)` below declares `mk() : (Two/a())` and then `mk() : (Two/b())`, and asked whether each constructor is impossible at `Two/b()`, a rule resolving both entries to the first would compare `Two/a()` against `Two/b()` twice and report the family empty at an index its own second entry constructs at.
///
/// What it would certify is `vacuous : (x : Held(Two/b())) -> False` — a refutation of a constructor the same declaration states. That is not a closed inhabitant of `False`: construction resolves by first match too, so `let h : Held(Two/b()) = Held/mk()` is a `Mismatch` of `Held(Two/a())` against `Held(Two/b())`, and the shadowed entry has no inhabitant to hand the refutation. Refusing the repeated tag keeps the elimination rule from being sound only because an unrelated rule is lossy in the same direction.
///
/// No `.crs` file reaches it: `curios-text` refuses both spellings with ``` `mk` is already declared in this module ```, whether or not the representation is public. The elaborator is the stricter of the two, so the certifier's copy of the rule is unreachable from the corpus, and that is why this belongs here rather than in `curios/src/tests`.
///
/// The control is the same shape at two *distinct* tags, both targeting `Two/a()`. It must stay accepted: ruling impossible cases out is what an indexed family's vacuous elimination is for, and a clause that refused every multi-constructor declaration, or every empty elimination over one, would shut this hole with a brick.
#[test]
fn a_family_that_declares_one_tag_twice_is_refused() {
    let verdicts = fixture_verdicts(
        &shadowed_constructor(["mk", "mk"]),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::RepeatedTag(_))),
        "the kernel certified a refutation of a constructor the declaration states: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_over_two_distinct_tags_is_still_accepted() {
    let verdicts = fixture_verdicts(
        &shadowed_constructor(["mk", "mk2"]),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.is_empty(),
        "an elimination whose every constructor genuinely clashes was refused: {verdicts:?}",
    );
}

/// A motive that lies about its own sort must not skip the large-elimination guard.
///
/// `Sort::of` classifies a stuck `match` used as a type by reading its **motive** — "its motive is the sort, which every arm shares", as the arm says. Nothing in `Sort::of` establishes that: the arms are checked against the motive only where a judgment meets the elimination, and the motive may claim whatever it likes.
///
/// The guard is what a lie buys. `guard_large_elimination` **returns immediately when the result is not relevant** — eliminating a proposition into a proposition needs no condition. So a motive whose body is `switch i : (_) => Prop | 0 => Nat | _ => Nat` reads as `Prop` at the abstract binder, the guard would be skipped, and the very same motive *reduces to `Nat`* at the concrete index each arm is checked against, so the arms typecheck as data.
///
/// `P` below is a two-constructor proposition, the shape the guard exists to refuse: `mk()` and `mk2()` both inhabit `P(0)`, proof irrelevance identifies them, and `extract` maps them to `7` and `9`. This is the route reached through the classifier rather than through the guard's own condition.
///
/// Not reachable from a surface program: `curios-elab` builds a match's motive and checks the arms against it, so it never emits one that lies, which is why this is built here.
///
/// Closed by checking the motive itself: `check_motive` types it under its real binders and requires it to land in a sort, which is Coq's `type_of_case` clause, and hands the sort it derives to the guard so nothing re-reads the claim. The lying motive does not typecheck — its arms inhabit `Type` while it states `Prop` — so the refusal names that rule rather than the guard it would bypass.
///
/// The control is [`an_honest_motive_still_refuses_the_large_elimination`], the same module with the motive stating `Type` — where the motive is honest, the guard does fire, and the elimination is refused for the reason it should be. Together they pin that what a lie would skip is the guard, and that closing the bypass does not close the guard itself.
#[test]
fn a_motive_that_misreports_its_sort_does_not_skip_the_large_elimination_guard() {
    let verdicts = fixture_verdicts(
        &lying_motive(Term::prop()),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotAMotive { .. })),
        "the kernel eliminated a two-constructor proposition into `Nat`: {verdicts:?}",
    );
}

/// The control: the identical module whose motive states the `Type` its arms actually have. The guard sees a relevant result and refuses, which is what proves the fixture above is about the *classifier* rather than about eliminations in general.
#[test]
fn an_honest_motive_still_refuses_the_large_elimination() {
    let verdicts = fixture_verdicts(
        &lying_motive(Term::type_ground()),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::LargeElimination(_))),
        "the guard did not fire even on an honest motive: {verdicts:?}",
    );
}

/// A motive is checked even where the elimination cannot run.
///
/// `check_induct_arms` skips the large-elimination guard for a vacuous elimination — no arms, no catch-all — and the reason is good: the coverage loop must then prove *every* constructor impossible at the scrutinee's indices, so the eliminated instance is uninhabited and discharging it into a relevant result leaks nothing. The question this fixture settles is whether the *motive* clause inherits that skip.
///
/// It does not, and the reason is not an exploit: no route from a vacuous elimination to a forged term is known, and the honest reading of that is the weak one — an attempt failed, which is not a proof that none exists. What makes the clause unconditional is that `infer` reads the elimination's **type** off the motive and hands it to the caller whether or not the elimination can run — so a motive nothing validated means a term whose type nothing validated, and `Sort::of` will classify it downstream. The module below is a vacuous elimination at an uninhabited `Held(Two/b())`, whose motive states `Prop` over arms inhabiting `Type`. What keeps the fixture pointed at the motive is the order `check` and `infer` run in, not the shape of the definition around it — see the note at the definition.
///
/// It also costs nothing. The clause sits in [`check_cases`](crate::infer) above the dispatch, where every `Cases` form shares one `motive` binding, so *not* running it here would mean pushing it down into `check_induct_arms`, guarding it with the vacuous condition, and duplicating it into the three intrinsic-carrier arms. Unconditional is the cheap implementation; the skip would be the deliberate exception.
#[test]
fn a_vacuous_elimination_still_has_its_motive_checked() {
    let two_name = Global::Authored(Qualifier::from(["Two"]));
    let held_name = Global::Authored(Qualifier::from(["Held"]));
    let nullary = |tag: &str, targets: Vec<Term>| {
        (
            Atom::from(tag),
            InductParam::new(Telescope::done(targets), Vec::new()),
        )
    };
    let at = |tag: &str| Term::variant(two_name, Vec::<Term>::new(), tag, Vec::<Term>::new());

    // `Two : Type 0`, so `a` and `b` genuinely clash and the elimination really is vacuous.
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
    // `Held : (t : Two) -> Prop | mk() : (Two/a())`, a proposition, so the guard would be in play.
    let held_decl = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(900, Some("t")),
                Term::induct_type(two_name, Vec::<Term>::new(), Vec::<Term>::new()),
            )],
            (),
        )),
        constructors: vec![nullary("mk", vec![at("a")])].into(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let at_b = Term::induct_type(held_name, Vec::<Term>::new(), [at("b")]);
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

    // The codomain is honest, and the ordering is what keeps this about the motive: `check` reaches `infer` before it can subsume, and `infer` runs `check_cases` — hence the motive clause — before there is a type to subsume with. Making the codomain *be* the lying motive would refuse a step earlier, on that codomain's own arms, and never reach the body.
    let vacuous = authored(
        &Global::Authored(Qualifier::from(["vacuous"])),
        Term::func_type(
            [
                (outer, Term::intrinsic(Intrinsic::NatType)),
                (subject, at_b.clone()),
            ],
            Term::intrinsic(Intrinsic::NatType),
        ),
        Term::func(
            [
                (outer, Term::intrinsic(Intrinsic::NatType)),
                (subject, at_b),
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

    let module = declaring(
        Module {
            mounts: Vec::new(),
            items: vec![vacuous].into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        BTreeMap::from([(two_name, two_decl), (held_name, held_decl)]),
        [],
        [],
    );

    let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotAMotive { .. })),
        "a vacuous elimination carried a motive nothing validated: {verdicts:?}",
    );
}

/// Every position at which a term stands as a *type* reaches the judgment, not the classifier — asserted at each position rather than argued once.
///
/// This crate has two ways to answer what sort a type has, and only one of them checks anything: `infer_sort` types a former's parts, `Sort::of` classifies them. `curios-cert/README.md` says the lookup is reached only where typing has run, and that sentence is a grep rather than a claim — a position reaching the classifier where the judgment belongs is the defect, and it can arrive at any position: a nominal occurrence's arguments, a type former's parts, a `Prop`-sorted declaration's domains.
///
/// A stuck `match` is what tells the two apart, because it is the one shape in Core that *states* its own sort instead of having one derived: `Sort::of` reads the motive, and the motive is a claim the term makes about itself. Where the judgment runs, `infer` reaches `check_motive` and the arms are checked against the motive, so `match b : Prop | false => Nat | true => Nat end` is refused by the arm — `Nat` at `Type 0` against the `Prop` the motive claims. Where only the classifier runs, that same term reads as a proposition and carries a `Nat`, which is the forgery [`a_proposition_may_not_carry_a_computed_relevant_field`] derives `False` from.
///
/// So the table below is one probe per position, each refused by that arm mismatch and by nothing else. The diagnostic is asserted rather than the mere presence of a verdict, because most of these types are uninhabitable while the lie stands: a body-driven refusal would name the body's own type and would pass a test that only counted verdicts.
///
/// Mutation-checked, which is what separates this from a fixture that asserts nothing: replacing `infer_telescope`'s `infer_type` with `Sort::of` — the exact weakening this guards against — makes "a lambda's domain annotation" come back with **zero** verdicts while every other row stays refused.
///
/// A declaration's own domains are the seventh position and are not repeated here; [`a_proposition_may_not_carry_a_computed_relevant_field`] holds that one, and holds it with the derivation rather than with the position alone.
#[test]
fn no_type_position_admits_a_lying_motive() {
    for (position, module) in lying_type_positions() {
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), SYNTAX);

        assert!(
            verdicts.iter().any(|verdict| matches!(
                &verdict.error,
                Error::Mismatch { inferred, expected }
                    if matches!(&***inferred, Subterm::Sort(Quality::Type, _))
                        && matches!(&***expected, Subterm::Sort(Quality::Prop, _))
            )),
            "{position} classified a lying motive instead of typing it: {verdicts:?}",
        );
    }
}

/// A fold's induction hypothesis is typed at the motive opened at the tail, and its arms learn nothing of the scrutinee. A motive that names the scrutinee instead of binding it — `(_) => Eq()(n, 0)` over `match n` — therefore states `Eq()(n, 0)` at every tail, and the zero arm's `refl(0)` does not inhabit it. The surface spelling is `let all_zero(n: Nat) -> Eq()(n, 0) = match n: (_) => Eq()(n, 0) | 0 => Eq/refl() | k + 1; ih => ih end;`.
///
/// Mutation-checked: with a fold's arms specialized as a case split's are, the zero arm stands at `Eq()(0, 0)`, the hypothesis at the successor arm's own goal, and `all_zero(1) : Eq()(1, 0)` certifies. The control is the same module with the motive binding `m` and stating `Eq()(m, 0)`, refused at the successor arm instead — its hypothesis is `Eq()(k, 0)` and its goal `Eq()(k + 1, 0)`.
#[test]
fn a_fold_motive_that_names_its_scrutinee_states_it_at_every_tail() {
    let verdicts = fixture_verdicts(
        &fold_motive(FoldMotive::Captured),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    // Refused at the zero arm, whose goal is the motive as it is written: no successor is in it.
    assert!(
        verdicts.iter().any(|verdict| match &verdict.error {
            Error::Mismatch { expected, .. } => !expected.to_string().contains("+ 1"),
            _ => false,
        }),
        "the kernel typed a fold's zero arm with its scrutinee at zero: {verdicts:?}",
    );
}

#[test]
fn a_fold_motive_that_binds_its_scrutinee_types_the_hypothesis_at_the_tail() {
    let verdicts = fixture_verdicts(
        &fold_motive(FoldMotive::Honest),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    // The successor arm's hypothesis is `Eq()(k, 0)` and its goal `Eq()(k + 1, 0)`: refused as the mismatch it is, at the successor.
    assert!(
        verdicts.iter().any(|verdict| match &verdict.error {
            Error::Mismatch { expected, .. } => expected.to_string().contains("+ 1"),
            _ => false,
        }),
        "the honest motive was not refused at the successor arm: {verdicts:?}",
    );
}

/// The same module at a goal each arm inhabits, which is what proves the two refusals above are about the motive rather than about the fixture.
#[test]
fn a_fold_motive_that_binds_its_scrutinee_is_accepted_at_an_inhabited_goal() {
    let verdicts = fixture_verdicts(
        &fold_motive(FoldMotive::Reflexive),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.is_empty(),
        "the reflexive fold was refused: {verdicts:?}"
    );
}

// The guard reads a family's declared sort and its constructors, never the instance eliminated: a two-constructor proposition is refused into `Nat` at either instance of a level it is irrelevant in, and a one-constructor one is admitted at either.
#[test]
fn an_irrelevant_level_changes_no_elimination() {
    for instance in [0, 1] {
        let two = fixture_verdicts(
            &levelled_extraction(Level::constant(instance), &["mk", "mk2"]),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );
        assert!(
            two.iter()
                .any(|verdict| matches!(verdict.error, Error::LargeElimination(_))),
            "{two:?}"
        );

        let one = fixture_verdicts(
            &levelled_extraction(Level::constant(instance), &["mk"]),
            1_000_000,
            &Globals::default(),
            SYNTAX,
        );
        assert_eq!(one, Vec::new());
    }
}

// The scrutinee's index is `wrap` at another instance than `Ixd`'s target states, which conversion calls the same value: inversion reads the tag and the payloads and no level, so the arm is solved and stays mandatory.
#[test]
fn an_arm_is_not_excused_by_an_irrelevant_level() {
    let (family, vacuous) = excused_arm_module(false);
    assert!(
        fixture_verdicts(&vacuous, 1_000_000, &Globals::default(), SYNTAX)
            .iter()
            .any(|verdict| verdict.error
                == Error::MissingArm {
                    family,
                    tag: Atom::from("mk"),
                })
    );

    let (_, covered) = excused_arm_module(true);
    assert_eq!(
        fixture_verdicts(&covered, 1_000_000, &Globals::default(), SYNTAX),
        Vec::new()
    );
}

/// `induct Pinned : (n : Nat, m : Nat) -> Prop | mk(x : Nat) : (x, x) end` beside an empty proposition, and one definition eliminating `Pinned(0, 1)` with `mk` as its only arm, `binders` naming its payload.
///
/// The stated instance is one no value of the family stands at: `mk` targets `(x, x)`, so the payload its indices pin is `x := 0` and the instance it builds at that payload is `Pinned(0, 0)`. That is what the open branch of the sole-arm rule compares — `unread_payload`'s `grounded`, which answers no here, leaving the match standing — and what the closed branch does not compare at all, `stated_payload` handing the payload over on the ground that a closed proof of a one-constructor family is that constructor.
///
/// With `over` naming a binder the definition is a function of a proof of the stated instance and the scrutinee is that binder, which is the open branch; with none the scrutinee is a metavariable, which the gate reads as closed.
fn pinned_sole_arm_module(over: Option<Free>, binders: Vec<(Plicity, Free)>) -> Module {
    let pinned = Global::Authored(Qualifier::from(["Pinned"]));
    let falsehood = Global::Authored(Qualifier::from(["False"]));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let literal = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let payload = Free::local(770, Some("x"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [
                (Free::local(771, Some("n")), nat()),
                (Free::local(772, Some("m")), nat()),
            ],
            (),
        )),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(payload, nat())],
                    vec![Term::free_var(&payload), Term::free_var(&payload)],
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
    let at = |left: usize, right: usize| {
        Term::induct_type(pinned, Vec::<Term>::new(), [literal(left), literal(right)])
    };
    let false_type = || Term::induct_type(falsehood, Vec::<Term>::new(), Vec::<Term>::new());

    let eliminated = Term::induct_match_scoped_marked(
        match over {
            Some(binder) => Term::free_var(&binder),
            None => Term::hole(0usize),
        },
        Scope::constant(Many(3), false_type()),
        [(Atom::from("mk"), binders, Term::prop())],
        None,
    )
    .stating(at(0, 1));
    let (type_, body) = match over {
        Some(binder) => (
            Term::func_type([(binder, at(0, 1))], false_type()),
            Term::func([(binder, at(0, 1))], eliminated),
        ),
        None => (false_type(), eliminated),
    };

    declaring(
        Module {
            items: vec![authored(
                &Global::Authored(Qualifier::from(["bad"])),
                type_,
                body,
            )]
            .into(),
            ..Module::default()
        },
        BTreeMap::from([(pinned, declaration), (falsehood, proposition(Vec::new()))]),
        [],
        [],
    )
}

/// A metavariable in the kernel's input must be refused, though the Match-arm rule's gate reads it as closed.
///
/// `whnf`'s gate is `!term.has_local_free()`, where the closed machine's `accelerable` and `curios-elab`'s own `closed` also require no metavariable. So a match whose scrutinee is a metavariable and whose other parts name no local is closed to this reducer and open to the other two, and the closed branch of the sole-arm rule is the one with no comparison in it: `stated_payload` hands the payload over where `unread_payload` would put the stated instance beside the one the constructor builds and decline. What keeps the two from parting over a verdict is not the gate but the boundary in front of it — `Zonked::project` refuses a program carrying elaboration's own syntax, and `infer` refuses a `Metavar` with `NotCore` where a judgment meets one — and this holds that boundary, the fixture reaching the walk with `Zonked` bypassed as every fixture here does.
#[test]
fn a_metavariable_the_match_gate_reads_as_closed_is_still_refused() {
    let verdicts = fixture_verdicts(
        &pinned_sole_arm_module(None, vec![(Plicity::Explicit, Free::local(770, Some("x")))]),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, Error::NotCore(_))),
        "elaboration's own syntax must be refused where a judgment meets it: {verdicts:?}",
    );
}

/// An arm whose arity its constructor's payload denies must be refused, which is what keeps the sole-arm rule's payload from ever being opened at the wrong count.
///
/// The open branch of the kernel's rule filters on `payload.len() == arm.arity()` before it opens the arm, where `curios-elab`'s fires `unread()` ahead of that comparison. The two can only part on a module the kernel refuses outright, and this is the refusal: the payload a stated instance yields is as long as the constructor's signature, and `check_induct_arms` holds every arm's arity to that signature. The scrutinee is a binder, so the open branch is the one reduction takes.
#[test]
fn an_arm_whose_arity_its_constructor_denies_is_refused() {
    let proof = Free::local(773, Some("p"));
    let verdicts = fixture_verdicts(
        &pinned_sole_arm_module(
            Some(proof),
            vec![
                (Plicity::Explicit, Free::local(770, Some("x"))),
                (Plicity::Explicit, Free::local(774, Some("y"))),
            ],
        ),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::Arity {
                counted: Counted::ArmBinders,
                ..
            }
        )),
        "an arm must be held to its constructor's payload: {verdicts:?}",
    );
}

/// The control: the same elimination with the arm at its constructor's arity stays refused for the stated instance alone, never for the arity.
///
/// `Pinned(0, 1)` is a proposition no value stands at, so the arm the indices rule out is what coverage decides and the arity is no part of it. Reading the refusal above as the arity's needs this beside it, or a clause that refused every sole-arm elimination would pass the one and the other alike.
#[test]
fn an_arm_at_its_constructors_arity_is_not_refused_for_its_arity() {
    let proof = Free::local(773, Some("p"));
    let verdicts = fixture_verdicts(
        &pinned_sole_arm_module(
            Some(proof),
            vec![(Plicity::Explicit, Free::local(770, Some("x")))],
        ),
        1_000_000,
        &Globals::default(),
        SYNTAX,
    );

    assert!(
        !verdicts.iter().any(|verdict| matches!(
            verdict.error,
            Error::Arity {
                counted: Counted::ArmBinders,
                ..
            }
        )),
        "an arm at its constructor's arity is held to nothing about its arity: {verdicts:?}",
    );
}
