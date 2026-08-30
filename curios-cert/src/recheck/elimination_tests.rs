//! Motives and result sorts, and the large-elimination guard a vacuous elimination must not skip.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use {
    crate::{Globals, KernelError},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Many, Module, Scope, Subterm,
        Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

use super::test_support::*;

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
    let verdicts = fixture_verdicts(
        &aliased_sort_forgery(),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotASort(_))),
        "the kernel certified a closed inhabitant of `False`: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_at_a_relevant_index_is_still_accepted() {
    let verdicts = fixture_verdicts(
        &relevant_index_control(),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts.is_empty(),
        "an arm the index targets genuinely clash with was refused: {verdicts:?}",
    );
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
    let verdicts = fixture_verdicts(
        &shadowed_constructor(["mk", "mk"]),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::RepeatedTag(_))),
        "the kernel certified a refutation of a constructor the declaration states: {verdicts:?}",
    );
}

#[test]
fn a_vacuous_elimination_over_two_distinct_tags_is_still_accepted() {
    let verdicts = fixture_verdicts(
        &shadowed_constructor(["mk", "mk2"]),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts.is_empty(),
        "an elimination whose every constructor genuinely clashes was refused: {verdicts:?}",
    );
}

/// A motive that lies about its own sort skips the large-elimination guard entirely.
///
/// `Sort::of` classifies a stuck `match` used as a type by reading its **motive** — "its motive is the sort, which every arm shares", as the arm says. Nothing establishes that. `check_definition` calls `Sort::of` on a declared type and never infers it, so a type-position `match` has its arms checked against its motive by no judgment at all, and the motive may claim whatever it likes.
///
/// The guard is what that buys. `guard_large_elimination` computes `let relevant = Sort::of(result).map(|sort| !sort.is_prop())` and **returns immediately when the result is not relevant** — eliminating a proposition into a proposition needs no condition. So a motive whose body is `switch i : (_) => Prop | 0 => Nat | _ => Nat` reads as `Prop` at the abstract binder the guard opens it at, the guard is skipped, and the very same motive *reduces to `Nat`* at the concrete index each arm is checked against, so the arms typecheck as data.
///
/// `P` below is a two-constructor proposition, the shape the guard exists to refuse: `mk()` and `mk2()` both inhabit `P(0)`, proof irrelevance identifies them, and `extract` maps them to `7` and `9`. Verified while the hole was open: `recheck_module_verdicts` returned **zero refusals** for exactly this module, and `Sort::of` was confirmed directly to answer `Ok(Prop)` for a switch whose every arm is `Nat` while answering `Ok(Type 0)` for the same switch with an honest motive. This is the route `documentation/soundness/per-term-rules/large-elimination-guard.md` records as having produced two closed inhabitants of `False`, reached through the classifier rather than through the guard's own condition.
///
/// Not reachable from a surface program: `curios-elab` builds a match's motive and checks the arms against it, so it never emits one that lies. That is what kept the certifier's copy of the guard unobserved, and it is why this is built here.
///
/// Closed by checking the motive itself: `check_motive` types it under its real binders and requires it to land in a sort, which is Coq's `type_of_case` clause, and hands the sort it derives to the guard so nothing re-reads the claim. The lying motive no longer typechecks — its arms inhabit `Type` while it states `Prop` — so the refusal names that rule rather than the guard it used to bypass.
///
/// The control is [`an_honest_motive_still_refuses_the_large_elimination`], the same module with the motive stating `Type` — where the motive is honest, the guard does fire, and the elimination is refused for the reason it should be. Together they pin that what was being skipped is the guard, and that closing the bypass did not close the guard itself.
#[test]
fn a_motive_that_misreports_its_sort_does_not_skip_the_large_elimination_guard() {
    let verdicts = fixture_verdicts(
        &lying_motive(Term::prop()),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

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
    let verdicts = fixture_verdicts(
        &lying_motive(Term::type_ground()),
        1_000_000,
        &Globals::default(),
        crate::SYNTAX,
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::LargeElimination(_))),
        "the guard did not fire even on an honest motive: {verdicts:?}",
    );
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
        mounts: Vec::new(),
        items: vec![vacuous],
        universe_seeds: Vec::new(),
        induct_decls: BTreeMap::from([(two_name, two_decl), (held_name, held_decl)]),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 1_000,
        type_: None,
        body: Some(Term::tuple(Vec::<Term>::new())),
    };

    let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

    assert!(
        verdicts
            .iter()
            .any(|verdict| matches!(verdict.error, KernelError::NotAMotive(_))),
        "a vacuous elimination carried a motive nothing validated: {verdicts:?}",
    );
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
        let verdicts = fixture_verdicts(&module, 1_000_000, &Globals::default(), crate::SYNTAX);

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
