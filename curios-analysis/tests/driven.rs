//! The shared analyses, exercised through a real checker.
//!
//! These are an integration test rather than unit tests beside the analyses for a structural reason, not a filing one. Every analysis on that seam is written against `Env`/`Judge`, and `Judge`'s one method is `convert_at` — so testing one needs a *real* implementation of conversion. The only ones in the workspace are `curios-cert`'s `Kernel` and the elaborator's `Context`, and both sit above `curios-analysis`. A test-only `Env` would be a third checker, which is exactly what nobody should write.
//!
//! A dev-dependency cycle does not solve it either: a `#[cfg(test)]` module inside `curios-analysis` compiles that crate a second time, so `Kernel`'s `Judge` implementation would be against the *other* copy and the trait bound would not hold. An integration test links the real libraries, which is why this file is here and not there.
//!
//! What stays beside the analyses is everything that needs no checker at all — the polarity lattice's own laws, the size-change matrix algebra, the readings through local definitions. Those are unit tests of pure functions and belong beside them.

use {
    curios_abi::{DeclaredForeign, ForeignFunction, WireResults, WireSignature, WireType},
    curios_analysis::{
        Answered, Coverage, Declarations, Invert, Judge, PositivityRefusal, answers,
        answers_classed, group_totality, invert_indices, positivity_vectors, solve_indices,
        struct_reaches_itself, test_support::SYNTAX,
    },
    curios_cert::Kernel,
    curios_core::{
        Apply, Argument, Atom, Bang, Carrier, Cases, Classes, Exhaustion, Field, Free, Global,
        InductArm, InductDecl, InductParam, InductType, Infix, Instance, InstanceHead, Intrinsic,
        Many, Match, MatchResult, Metavar, MetavarId, MetavarOrigin, Nat, Polarity, Proj, Rec,
        Scope, Struct, StructDecl, StructEntry, StructType, Subterm, Telescope, Term, Three,
        Totality, Transient, Tuple, Two, UniverseContext, Var, Variant,
    },
    curios_num::{Grain, Natural},
    curios_utilities::{InfixOp, Plicity, Qualifier},
    std::{collections::BTreeMap, rc::Rc, slice, sync::Arc},
};

/// The member every probe below plants a call to.
fn planted() -> Free {
    Free::local(1, Some("f"))
}

fn kernel() -> Kernel {
    Kernel::new(100_000, SYNTAX)
}

/// A nullary family at `result_sort` with two nullary constructors, `a` and `b`.
///
/// Two constructors is the point: it gives the family two closed inhabitants that are syntactically distinct, so whether they are *interchangeable* is decided by the family's sort and by nothing else.
fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Global {
    let family = Global::Authored(Qualifier::from([path]));

    kernel.declare_induct(
        &family,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: ["a", "b"]
                .into_iter()
                .map(|tag| {
                    (
                        Atom::from(tag),
                        InductParam::new(Telescope::done(Vec::new()), Vec::new()),
                    )
                })
                .collect(),
            result_sort,
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );

    family
}

/// The deletion rule (Goguen–McBride–McKinna) decides a binder forced in two index positions by *convertibility*, and this is the direction no program reaches.
///
/// Every re-forcing the corpus makes that the rule accepts has two *syntactically identical* forcings, so a plain equality test would decide every acceptance the prelude and the test programs contain. The semantic half of the rule — accepting two forcings that differ but convert — is exercised by no program, which is precisely the condition under which a rule's mistakes stay invisible.
///
/// So its answers are put to it directly. Every fixture forces one binder in the first index position and again in the second — the shape `refl(@z) : (z, z)` meets at `Eq()(a, b)` — and the first two differ in nothing but the family's sort, which is what makes them a control pair rather than two unrelated cases.
///
/// At a proposition the two forcings `a()` and `b()` convert by irrelevance, so the rule deletes the redundant constraint and one solution survives: sound because `Eq : Prop` makes the system definitionally K, and harmless because the surviving substitution is interchangeable with the one it replaced. At a relevant family the same two are constructors a program tells apart, and each forcing was reached by injective steps, so the case is reachable only if `a()` is `b()`: the verdict is `Impossible`, the conflict rule arriving through a non-linear target, and it is what lets `match h end` close a hypothesis `Eq()(false, true)`. The third fixture is the control on *that*: two forcings that do not convert and do not clash either — one opaque function at two arguments, which may well agree — are dropped, the arm still checked with the binder unsolved.
///
/// `Impossible` excuses an arm from being checked at all, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to a closed inhabitant of `False` (see `documentation/design/soundness/elimination/coverage.md`), routed there from index inversion. So the proposition's verdict is the one this holds hardest: the clash is asked of the walk a linear position is put to, whose tag test is licensed by the family's sort, and only after conversion said no — and at a proposition conversion says yes.
#[test]
fn a_binder_forced_twice_survives_only_when_its_forcings_convert() {
    enum Verdict {
        Survives,
        Dropped,
        Impossible,
    }

    for (label, sort, opaque, expected) in [
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            false,
            Verdict::Survives,
        ),
        (
            "a relevant family, whose two constructors a program tells apart",
            Term::type_ground(),
            false,
            Verdict::Impossible,
        ),
        (
            "a relevant family, at two values nothing tells apart or together",
            Term::type_ground(),
            true,
            Verdict::Dropped,
        ),
    ] {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let family_type = Term::induct_type(family, Vec::<Term>::new(), Vec::<Term>::new());
        let binder = Free::local(900, Some("p"));
        let opaque_function = Free::local(901, Some("g"));
        let argument = Free::local(902, Some("x"));

        kernel.assume(&binder, &family_type);
        kernel.assume(
            &opaque_function,
            &Term::func_type([(argument, family_type.clone())], family_type.clone()),
        );

        let inhabitant = |tag| {
            let constructed = Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new());
            match opaque {
                true => Term::apply(Term::free_var(&opaque_function), [constructed]),
                false => constructed,
            }
        };
        let target = Term::free_var(&binder);

        let outcome = invert_indices(
            &mut kernel,
            &[inhabitant("a"), inhabitant("b")],
            &[target.clone(), target],
            slice::from_ref(&binder),
        )
        .expect("inversion is a total function of finished terms");

        let surviving = match (outcome, expected) {
            (Invert::Impossible, Verdict::Impossible) => continue,
            (Invert::Impossible, _) => panic!(
                "{label}: a re-forced binder was reported unreachable, which excuses the arm from being checked"
            ),
            (Invert::Solved(_), Verdict::Impossible) => {
                panic!("{label}: two forcings that clash left the case reachable")
            }
            (Invert::Solved(solutions), Verdict::Survives) => (solutions, 1),
            (Invert::Solved(solutions), Verdict::Dropped) => (solutions, 0),
        };
        let (solutions, count) = surviving;

        assert_eq!(
            solutions.len(),
            count,
            "{label}: the deletion rule kept {} of the two forcings",
            solutions.len(),
        );
        assert!(
            solutions.iter().all(|(bound, _)| *bound == binder),
            "{label}: a solution was recorded for a binder that was never forced",
        );
    }
}

/// The rule's third answer, which the pair above does not reach: a binder whose *type* is out of scope.
///
/// `consolidate` decides a re-forcing by asking the `Judge` for the binder's assumed type and comparing the two forcings at it. With no assumption there is nothing to compare at, so it drops the binder's solutions — the conservative direction, leaving the binder unsolved and the arm still checked. No program reaches the branch: both kernel callers reach `invert_indices` through `open_payload`, which assumes every binder it opens before the body runs, so from the kernel it is unreachable by construction.
///
/// It is pinned anyway, because the conservative answer is not the obvious one to write. `None => true` — no type, so nothing to disagree about, so keep the forcing — is the plausible slip, and it would accept a re-forcing that *no convertibility test ever decided*, which is the deletion rule discharging a constraint it never checked. Nothing else in the workspace would notice.
///
/// The discrimination is against the two constructor cases above rather than beside them: identical family, identical sort, identical forcings, and the binder assumed there and not here. At a proposition that one difference must turn one surviving solution into none. At a relevant family it must turn `Impossible` into none as well: the clash is asked only after conversion has said no, and with no type conversion was never asked, so the strong verdict — the one that excuses an arm from being checked — would be decided out of blindness, which is the direction no analysis on this seam takes.
#[test]
fn a_binder_whose_type_is_out_of_scope_drops_its_forcings() {
    for (label, sort) in [
        ("a proposition", Term::prop()),
        ("a relevant family", Term::type_ground()),
    ] {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let binder = Free::local(900, Some("p"));

        // Deliberately not assumed: this is the whole of the fixture.

        let inhabitant = |tag| Term::variant(family, Vec::<Term>::new(), tag, Vec::<Term>::new());
        let target = Term::free_var(&binder);

        let outcome = invert_indices(
            &mut kernel,
            &[inhabitant("a"), inhabitant("b")],
            &[target.clone(), target],
            slice::from_ref(&binder),
        )
        .expect("inversion is a total function of finished terms");

        let Invert::Solved(solutions) = outcome else {
            panic!(
                "{label}: a binder with no assumption was reported unreachable, which excuses the arm from being checked"
            );
        };

        assert!(
            solutions.is_empty(),
            "{label}: a binder whose type is out of scope kept a forcing that no convertibility test decided",
        );
    }
}

/// The clash rule's license is the registry, and a family the registry cannot answer for must not clash.
///
/// Two different constructors of one family definitely clash only when the family is *relevant*: at a `Prop`-sorted family irrelevance makes every inhabitant the same value, so reading tag disjointness there contradicts conversion, and an arm excused as impossible on that reading is the vacuous-elimination route to a closed inhabitant of `False` (see `documentation/design/soundness/elimination/index-inversion-and-k.md`). The sort is read out of `induct_decl`, so the whole of that protection rests on the lookup answering — and a lookup that answered nothing and fell through to the tag check would answer `Impossible`: the analysis deciding the strong verdict *because* it was blind, where every other blindness on this seam — an unknown positivity name, an undecodable size, an unassumed binder — turns into a refusal. The branch is unreachable from either checker as far as the walks are known, a well-typed variant's family being registered wherever the term came from, but the fixture above pins an equally unreachable branch for the same reason: the conservative answer is not the obvious one to write, and nothing else in the workspace would notice the wrong one.
///
/// The control pair holds what the rule must not move: the same position with the declaration present still clashes at a relevant sort, and still yields nothing at `Prop`.
#[test]
fn a_family_the_registry_cannot_answer_for_does_not_clash() {
    let inhabitant = |family: &Global, tag: &str| {
        Term::variant(*family, Vec::<Term>::new(), tag, Vec::<Term>::new())
    };

    // The subject: the family was never declared, so the sort read is blind.
    let mut blind = kernel();
    let family = Global::Authored(Qualifier::from(["Blind"]));
    let outcome = invert_indices(
        &mut blind,
        &[inhabitant(&family, "a")],
        &[inhabitant(&family, "b")],
        &[],
    )
    .expect("inversion is a total function of finished terms");
    assert!(
        matches!(outcome, Invert::Solved(ref solutions) if solutions.is_empty()),
        "a family the registry cannot answer for was decided rather than refused",
    );

    for (label, sort, impossible) in [
        (
            "a relevant family's two constructors definitely clash",
            Term::type_ground(),
            true,
        ),
        (
            "a proposition's two constructors are one value and yield nothing",
            Term::prop(),
            false,
        ),
    ] {
        let mut sighted = kernel();
        let family = declare(&mut sighted, "Blind", sort);
        let outcome = invert_indices(
            &mut sighted,
            &[inhabitant(&family, "a")],
            &[inhabitant(&family, "b")],
            &[],
        )
        .expect("inversion is a total function of finished terms");
        assert_eq!(matches!(outcome, Invert::Impossible), impossible, "{label}");
    }
}

/// A single-constructor family whose one payload is `payload_type`.
fn single_payload(payload_type: Term, result_sort: Term) -> InductDecl {
    let binder = Free::local(0, Some("f"));

    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build([(binder, payload_type)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )],
        result_sort,
        module: Qualifier::from(["T"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
        plicities: Vec::new(),
    }
}

/// The four-line route to `False`, refused by the kernel running the shared analysis: `Bad`'s constructor takes `(Bad) -> False`, a negative self-occurrence.
#[test]
fn a_negative_self_occurrence_is_refused() {
    let mut kernel = kernel();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        single_payload(
            Term::func_type([(Free::local(1, Some("x")), bad_type)], false_type),
            Term::type_ground(),
        ),
    );
    for (name, entry) in &inducts {
        kernel.declare_induct(name, entry);
    }

    let refusal = positivity_vectors(
        &mut kernel,
        Declarations::of(&inducts, &BTreeMap::new()),
        Coverage::Complete,
    )
    .expect_err("a negative self-occurrence must be refused");
    assert!(
        matches!(&refusal, PositivityRefusal::NotPositive(refusal) if refusal.name == bad_name),
        "refused by the rule, at `Bad`: {refusal:?}",
    );
}

/// The same route to `False` behind an alias the driver cannot unfold: `Bad`'s constructor takes `D`, a definition standing for `(Bad) -> False`, and the kernel is given no budget to unfold it. A refused reduction has to leave the analysis in the refusing direction — the walk follows what the name defines at `Mixed` — where a bare name recording nothing would admit the declaration outright. The refusal is the budget's: the alias was never read, so the analysis has no verdict of its own to give.
#[test]
fn a_payload_type_the_driver_cannot_reduce_is_refused_not_admitted() {
    let mut kernel = Kernel::new(0, SYNTAX);

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let alias = Free::local(1, Some("D"));
    kernel.define(
        &alias,
        &Term::type_ground(),
        &Term::func_type([(Free::local(2, Some("x")), bad_type)], false_type),
        &UniverseContext::default(),
    );

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        single_payload(Term::free_var(&alias), Term::type_ground()),
    );
    for (name, entry) in &inducts {
        kernel.declare_induct(name, entry);
    }

    let refusal = positivity_vectors(
        &mut kernel,
        Declarations::of(&inducts, &BTreeMap::new()),
        Coverage::Complete,
    )
    .expect_err("an alias the driver cannot unfold is followed, not admitted");
    assert!(
        matches!(&refusal, PositivityRefusal::Exhausted { name, .. } if *name == bad_name),
        "refused for the budget `Bad`'s walk ran out of: {refusal:?}",
    );
}

/// An alias standing for the declaration itself is a strict occurrence, and with no budget to unfold it the set is still refused — the alias is read at `Mixed`, a non-strict path back to `Good` — but for the budget, which is all the verdict rests on. With the budget to unfold it the same set is admitted, and that control is what says the refusal was the budget's.
///
/// Reporting it as not strictly positive would have a kernel out of budget blame a sound declaration, as it would `/std/Toml/Toml`, whose recursion reaches the walk through `Map`.
#[test]
fn a_strict_payload_the_driver_cannot_reduce_is_refused_for_the_budget() {
    let good_name = Global::Authored(Qualifier::from(["Good"]));

    for budget in [0, 100_000] {
        let mut kernel = Kernel::new(budget, SYNTAX);

        let alias = Free::local(1, Some("D"));
        kernel.define(
            &alias,
            &Term::type_ground(),
            &Term::induct_type(good_name, Vec::<Term>::new(), Vec::<Term>::new()),
            &UniverseContext::default(),
        );

        let mut inducts = BTreeMap::new();
        inducts.insert(
            good_name,
            single_payload(Term::free_var(&alias), Term::type_ground()),
        );
        for (name, entry) in &inducts {
            kernel.declare_induct(name, entry);
        }

        let verdict = positivity_vectors(
            &mut kernel,
            Declarations::of(&inducts, &BTreeMap::new()),
            Coverage::Complete,
        );
        match budget {
            0 => assert!(
                matches!(&verdict, Err(PositivityRefusal::Exhausted { name, .. }) if *name == good_name),
                "with no budget, refused for the budget rather than by the rule: {verdict:?}",
            ),
            _ => assert!(
                verdict.is_ok(),
                "with the budget to unfold the alias, `Good` is strictly positive: {verdict:?}",
            ),
        }
    }
}

/// A strictly positive self-occurrence — the payload *is* the family — is the ordinary recursive datatype and is admitted.
#[test]
fn a_strict_self_occurrence_is_admitted() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["Chain"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let mut inducts = BTreeMap::new();
    inducts.insert(name, single_payload(self_type, Term::type_ground()));
    for (entry_name, entry) in &inducts {
        kernel.declare_induct(entry_name, entry);
    }

    let vectors = positivity_vectors(
        &mut kernel,
        Declarations::of(&inducts, &BTreeMap::new()),
        Coverage::Complete,
    )
    .expect("a strictly positive declaration is admitted");
    assert_eq!(vectors.get(&name), Some(&Vec::new()));
}

/// Declare `struct <path>(n: Nat)` with the fields given.
fn declare_struct_over_a_number(kernel: &mut Kernel, path: &str, fields: Vec<Term>) -> Global {
    let name = Global::Authored(Qualifier::from([path]));
    let fields = fields
        .into_iter()
        .zip(1..)
        .map(|(field, index)| (Free::local(index, None), field))
        .collect::<Vec<_>>();

    kernel.declare_struct(
        &name,
        &StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::build(
                [(
                    Free::local(0, Some("n")),
                    Term::intrinsic(Intrinsic::NatType),
                )],
                Telescope::build(fields, ()),
            ),
            result_sort: Term::type_ground(),
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
            plicities: Vec::new(),
        },
    );

    name
}

/// A struct reaches itself where its declaration names it — by a field of its own, in a parameter of another struct, or through another struct's fields, and whether the name is spelled as the type former's normal form or as the global that unfolds to it. One that names no struct does not, and neither does one that names only a struct that reaches itself: the question is the diagonal, and what conversion asks it for is a struct met again.
///
/// Mutation-checked: read for its own name alone, a struct that reaches itself through another's fields is missed; read for its free variables alone, one spelled as a normal form is; and read for its constructions alone, one spelled as a global is.
#[test]
fn a_struct_reaches_itself_where_its_declaration_names_it_through_whatever_structs() {
    let mut kernel = kernel();
    let named = |path: &str| Global::Authored(Qualifier::from([path]));
    let at = |path: &str, param: Term| {
        Term::from(Subterm::StructType(StructType {
            name: named(path),
            universes: Vec::new(),
            params: vec![param],
        }))
    };
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0u64)));
    let spelled = |path: &str| Term::apply(Term::free_var(&Free::Global(named(path))), [zero()]);

    declare_struct_over_a_number(&mut kernel, "Plain", vec![Term::tuple_type_unit()]);
    declare_struct_over_a_number(&mut kernel, "Own", vec![at("Own", zero())]);
    declare_struct_over_a_number(&mut kernel, "Spelled", vec![spelled("Spelled")]);
    declare_struct_over_a_number(
        &mut kernel,
        "Nested",
        vec![at("Plain", at("Nested", zero()))],
    );
    declare_struct_over_a_number(&mut kernel, "Left", vec![at("Right", zero())]);
    declare_struct_over_a_number(&mut kernel, "Right", vec![at("Left", zero())]);
    declare_struct_over_a_number(&mut kernel, "Beside", vec![at("Own", zero())]);

    assert_eq!(
        ["Own", "Spelled", "Nested", "Left", "Right"]
            .map(|path| struct_reaches_itself(&kernel, &named(path))),
        [true, true, true, true, true]
    );
    assert_eq!(
        ["Plain", "Beside", "Undeclared"].map(|path| struct_reaches_itself(&kernel, &named(path))),
        [false, false, false]
    );
}

#[test]
fn a_huge_literal_call_argument_grades_without_expansion() {
    // `rec f : (n: Nat) -> Nat = (n) => f(u64::MAX); f` — grading the literal argument must read the packed spine, not peel it: a unary expansion would loop once per successor, and the value is unbounded by the source that spelled it.
    let mut kernel = Kernel::new(100_000, SYNTAX);
    let f = Free::local(1, Some("f"));
    let n = Free::local(2, Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let rec = Term::rec(
        vec![(
            f,
            Term::func_type([(n, nat())], nat()),
            Term::func(
                [(n, nat())],
                Term::apply(
                    Term::free_var(&f),
                    [Term::intrinsic(Intrinsic::Nat(Nat::new(u64::MAX)))],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    // A constant argument never decreases, so the verdict is `Partial` — promptly.
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// Whether the engine finds a nullary self-call planted at one child position.
///
/// The group is `rec f : Nat = <plant(f)>`, whose member takes no lambda and so has an empty parameter vector: a self-call from it is a 0x0 matrix, idempotent with no diagonal, so the verdict is `Partial` exactly when [`Walk::walk`] reached the planted occurrence and `Total` exactly when it did not. Nothing here needs to be well typed — the engine is a total function of post-zonk terms and types nothing — which is what lets one probe cover every child position of every variant.
fn call_is_seen(body: Term) -> bool {
    let mut kernel = Kernel::new(100_000, SYNTAX);
    let rec = Term::rec(
        vec![(planted(), Term::intrinsic(Intrinsic::NatType), body)],
        Term::free_var(&planted()),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    group_totality(&mut kernel, group) == Ok(Totality::Partial)
}

/// The `index`th placeholder child: a literal no walk reads as anything, distinct from every other so that replacing one names one position.
fn marker(index: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(10_000 + index)))
}

/// The markers minted so far, and which of them the walk is documented not to reach.
#[derive(Default)]
struct Markers {
    minted: usize,
    unvisited: Vec<(Term, &'static str)>,
}

impl Markers {
    /// A child the walk must reach.
    fn visited(&mut self) -> Term {
        self.minted += 1;
        marker(self.minted)
    }

    /// A child on the whitelist, with the mechanism that keeps the walk from it.
    fn unvisited(&mut self, mechanism: &'static str) -> Term {
        let marker = self.visited();
        self.unvisited.push((marker.clone(), mechanism));
        marker
    }
}

/// One term whose every child is a marker, and the markers it was built from.
struct Specimen {
    term: Term,
    markers: Vec<Term>,
}

impl Specimen {
    fn of(markers: &mut Markers, build: impl FnOnce(&mut Markers) -> Term) -> Self {
        let first = markers.minted;
        let term = build(markers);
        let markers = (first + 1..=markers.minted).map(marker).collect();

        Self { term, markers }
    }
}

/// Every form a child can hang from, by the name its specimen is held under.
const FORMS: &[&str] = &[
    "a leaf",
    "a metavariable",
    "a universe instance of a variable",
    "a universe instance of a projected group",
    "a transient",
    "an intrinsic",
    "a foreign call",
    "a lambda",
    "a function type",
    "an application",
    "a tuple type",
    "a tuple",
    "a projection",
    "a nominal type",
    "a constructor",
    "a structure type",
    "a structure literal",
    "a match at a motive",
    "a match at an ambient goal",
    "a boolean match",
    "a dispatch",
    "a nominal match",
    "a `Nat` eliminator",
    "a `Bin` eliminator",
    "a `List` eliminator",
    "a let",
    "a group",
];

/// The forms a specimen stands for. Wildcard-free at every level a child can hang from, so a new former — a `Subterm` variant, a `Cases` kind, a carrier, a match result, an instance head, a transient — is a compile error here, and the arm it forces is where its specimen is owed.
fn forms(term: &Term) -> Vec<&'static str> {
    match &**term {
        Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => vec!["a leaf"],
        Subterm::Metavar(_) => vec!["a metavariable"],
        Subterm::Instance(Instance { head, .. }) => vec![match head {
            InstanceHead::Var(_) => "a universe instance of a variable",
            InstanceHead::RecProj(..) => "a universe instance of a projected group",
        }],
        Subterm::Transient(transient) => vec![match transient {
            Transient::Infix(_) | Transient::Bang(_) | Transient::Ascribed(_) => "a transient",
            Transient::NumLit(_) | Transient::Derive => "a leaf",
        }],
        Subterm::Intrinsic(_) => vec!["an intrinsic"],
        Subterm::Foreign(..) => vec!["a foreign call"],
        Subterm::Func(_) => vec!["a lambda"],
        Subterm::FuncType(_) => vec!["a function type"],
        Subterm::Apply(_) => vec!["an application"],
        Subterm::TupleType(_) => vec!["a tuple type"],
        Subterm::Tuple(_) => vec!["a tuple"],
        Subterm::Proj(_) => vec!["a projection"],
        Subterm::InductType(_) => vec!["a nominal type"],
        Subterm::Variant(_) => vec!["a constructor"],
        Subterm::StructType(_) => vec!["a structure type"],
        Subterm::Struct(_) => vec!["a structure literal"],
        Subterm::Match(Match { result, cases, .. }) => vec![
            match result {
                MatchResult::Family(_) => "a match at a motive",
                MatchResult::Ambient(_) => "a match at an ambient goal",
            },
            match cases {
                Cases::Bool { .. } => "a boolean match",
                Cases::Switch { .. } => "a dispatch",
                Cases::Induct { .. } => "a nominal match",
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat { .. } => "a `Nat` eliminator",
                    Carrier::Bin { .. } => "a `Bin` eliminator",
                    Carrier::List { .. } => "a `List` eliminator",
                },
            },
        ],
        Subterm::Let(_) => vec!["a let"],
        Subterm::Rec(_) => vec!["a group"],
    }
}

/// The position-coverage differential (see `documentation/design/soundness/totality/nothing-reachable-from-a-type-is-partial.md`): `Walk::walk` visits every child position `Subterm::any_child_term` reports, minus a named whitelist.
///
/// This matters more here than anywhere else on the soundness board because this is the only analysis whose blindness *admits*. Every other one refuses when it cannot see — positivity answers `Mixed` at an out-of-set name, `whnf` goes stuck, inversion derives nothing at a `Prop`-valued position, an under-applied call is graded `Matrix::unknown` — while a call site the walk never visits contributes no edge, and a group with no edges is `Total`. A projected inner group left unwalked is exactly that: `rec f(n) -> False = (rec g(m) -> False = f(m); g)(n)` would close to no call whatsoever, both groups would classify `Total`, and `f(0)` would diverge through `g` while (V) read the verdict.
///
/// The probe needs no instrumentation because the engine types nothing: it is a total function of post-zonk terms, so an ill-typed fixture is a legitimate input. Each row plants a *nullary* self-call at one child position — the member takes no lambda, so a self-call from it is a 0x0 matrix, idempotent with no diagonal — which makes the verdict `Partial` exactly when the walk reached the plant and `Total` exactly when it did not.
///
/// **The rows are the fold's, not a list kept here.** Each specimen is one form with a distinct marker in every child position; the fold is asked for its children, and the call is planted at each child it reports. A list written by hand holds a row for whatever its author thinks of, and passes while every position it forgot has none. Two things are asserted of every specimen before anything is planted: that the fold reports exactly the markers it was built from, so a specimen populating a field the fold does not visit fails here rather than passing unplanted, and that every form in `FORMS` has one.
///
/// **What that does and does not hold.** A term-bearing field added to a form whose specimen is a struct literal is a compile error at that literal, and the marker that repairs it is planted with no further edit. `Func`, `FuncType`, `TupleType`, `Let` and `Rec` are built through `Term`'s constructors, their fields being private, so a field added behind one of those is held only by whoever extends the constructor.
///
/// **The whitelist is three mechanisms.** A `Metavar`'s spine, which `zonk_module` refuses before this pass runs. A group member's declared type, under both spellings a group is reached by — a nested `Rec` and a universe instance's projection head. And the one the fold cannot show, because it is not a child position but a peel: `Member::of` takes the member body's *leading* lambdas and discards their domain annotations at `Telescope::Cons(_, _, rest)`, so a call planted there is invisible while the same lambda one node deeper is walked — which is why every row is wrapped in a tuple, and why the two spellings are asserted against each other at the end.
///
/// All three are type positions, and the argument that they are safe is the audit's: a call in one is consumed by β or read only by typing, never reduced, and an edge the engine misses is dangerous only where it can complete a reduction cycle. That argument is not what this test checks.
#[test]
fn the_walk_reaches_every_child_position_but_the_three_it_documents() {
    let name = || Global::Authored(Qualifier::from(["N"]));
    let binder = |index: u32| Free::local(500 + index, None);
    let call = || Term::free_var(&planted());
    let group_of = |rec: &Term| {
        let Subterm::Rec(Rec { group, .. }) = &**rec else {
            panic!("the fixture changed shape");
        };
        group.clone()
    };
    let matching = |markers: &mut Markers, cases: Cases| -> Term {
        Subterm::Match(Match {
            head: markers.visited(),
            result: MatchResult::Family(Scope::close(Many(1), &[&binder(1)], markers.visited())),
            cases,
        })
        .into()
    };

    let mut markers = Markers::default();
    let specimens = vec![
        Specimen::of(&mut markers, |_| Term::type_ground()),
        Specimen::of(&mut markers, |_| {
            Subterm::Instance(Instance {
                head: InstanceHead::Var(Var::free(binder(7))),
                levels: Vec::new(),
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Metavar(Metavar {
                id: MetavarId::from(0usize),
                spine: Rc::new(vec![markers.unvisited("a metavariable's spine")]),
                origin: MetavarOrigin::Hole,
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            let rec = Term::rec(
                vec![(
                    binder(90),
                    markers.unvisited("a projected group's member type"),
                    markers.visited(),
                )],
                Term::free_var(&binder(90)),
            );
            Subterm::Instance(Instance {
                head: InstanceHead::RecProj(group_of(&rec), 0),
                levels: Vec::new(),
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Transient(Transient::Infix(Infix {
                op: InfixOp::Add,
                left: markers.visited(),
                right: markers.visited(),
            }))
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Transient(Transient::Bang(Bang {
                action: markers.visited(),
                continuation: markers.visited(),
            }))
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Term::ascribed(markers.visited(), markers.visited())
        }),
        Specimen::of(&mut markers, |markers| {
            Term::intrinsic(Intrinsic::ListType(markers.visited()))
        }),
        Specimen::of(&mut markers, |markers| {
            let row = Arc::new(ForeignFunction::Declared(DeclaredForeign {
                name: "/planted".to_string(),
                label: "planted".to_string(),
                signature: WireSignature {
                    params: Vec::new(),
                    results: WireResults::single(WireType::Nat),
                },
            }));
            Term::foreign(row, vec![markers.visited()])
        }),
        Specimen::of(&mut markers, |markers| {
            Term::func([(binder(1), markers.visited())], markers.visited())
        }),
        Specimen::of(&mut markers, |markers| {
            Term::func_type([(binder(1), markers.visited())], markers.visited())
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Apply(Apply {
                head: markers.visited(),
                arguments: vec![Argument {
                    term: markers.visited(),
                    plicity: Plicity::Explicit,
                }],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Term::tuple_type(vec![(binder(1), markers.visited())])
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Tuple(Tuple {
                fields: vec![markers.visited()],
                names: vec![None],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Proj(Proj {
                head: markers.visited(),
                field: Field::Index(0),
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::InductType(InductType {
                name: name(),
                universes: Vec::new(),
                params: vec![markers.visited()],
                indices: vec![markers.visited()],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Variant(Variant {
                name: name(),
                universes: Vec::new(),
                params: vec![markers.visited()],
                tag: "mk".into(),
                payload: vec![markers.visited()],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::StructType(StructType {
                name: name(),
                universes: Vec::new(),
                params: vec![markers.visited()],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Struct(Struct {
                name: name(),
                universes: Vec::new(),
                params: vec![markers.visited()],
                fields: vec![markers.visited()],
                entries: vec![StructEntry::Field(None)],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            let cases = Cases::Bool {
                false_case: markers.visited(),
                true_case: markers.visited(),
            };
            matching(markers, cases)
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Match(Match {
                head: markers.visited(),
                result: MatchResult::Ambient(markers.visited()),
                cases: Cases::Bool {
                    false_case: markers.visited(),
                    true_case: markers.visited(),
                },
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            let cases = Cases::Switch {
                cases: vec![(Natural::from(0usize), markers.visited())],
                default: markers.visited(),
            };
            matching(markers, cases)
        }),
        Specimen::of(&mut markers, |markers| {
            let arm = Scope::close(Many(1), &[&binder(2)], markers.visited());
            let cases = Cases::Induct {
                cases: vec![("mk".into(), InductArm::new(arm, vec![Plicity::Explicit]))],
                default: Some(markers.visited()),
            };
            matching(markers, cases)
        }),
        Specimen::of(&mut markers, |markers| {
            let carrier = Carrier::Nat {
                empty_case: markers.visited(),
                cons_case: Scope::close(Two, &[&binder(2), &binder(3)], markers.visited()),
            };
            matching(markers, Cases::FreeMonoid { carrier })
        }),
        Specimen::of(&mut markers, |markers| {
            let carrier = Carrier::Bin {
                grain: Grain::X,
                empty_case: markers.visited(),
                cons_case: Scope::close(
                    Three,
                    &[&binder(2), &binder(3), &binder(4)],
                    markers.visited(),
                ),
            };
            matching(markers, Cases::FreeMonoid { carrier })
        }),
        Specimen::of(&mut markers, |markers| {
            let carrier = Carrier::List {
                elem: markers.visited(),
                empty_case: markers.visited(),
                cons_case: Scope::close(
                    Three,
                    &[&binder(2), &binder(3), &binder(4)],
                    markers.visited(),
                ),
            };
            matching(markers, Cases::FreeMonoid { carrier })
        }),
        Specimen::of(&mut markers, |markers| {
            Term::let_(
                &binder(1),
                markers.visited(),
                markers.visited(),
                markers.visited(),
            )
        }),
        Specimen::of(&mut markers, |markers| {
            Term::rec(
                vec![(
                    binder(1),
                    markers.unvisited("a nested group's member type"),
                    markers.visited(),
                )],
                markers.visited(),
            )
        }),
    ];

    // The harness itself, first: a bare self-call must be seen and a call-free body must not, or every row below passes for the wrong reason.
    assert!(call_is_seen(call()), "the bare self-call was not seen");
    assert!(
        !call_is_seen(Term::type_ground()),
        "a call-free body was read as recursive"
    );

    let held = specimens
        .iter()
        .flat_map(|specimen| forms(&specimen.term))
        .collect::<Vec<_>>();
    for form in FORMS {
        assert!(held.contains(form), "{form} has no specimen");
    }

    for specimen in &specimens {
        let form = forms(&specimen.term).join(", ");
        let mut children = Vec::new();
        specimen.term.any_child_term(&mut |child| {
            children.push(child.clone());
            false
        });

        // The fold and the specimen agree on what the children are, in both directions: a marker the fold does not report is a field it misses, and a child that is no marker is a position the specimen left unpopulated.
        for marker in &specimen.markers {
            assert!(
                children.contains(marker),
                "{form}: the child fold does not report {marker:?}",
            );
        }
        for child in &children {
            assert!(
                specimen.markers.contains(child),
                "{form}: the specimen leaves the child {child:?} without a marker",
            );
        }

        for child in &children {
            let plant = specimen.term.replace_term(child, &call());
            // A replacement that reached nothing would hold every whitelisted row vacuously.
            assert!(
                plant.mentions_term(&call()),
                "{form}: nothing was planted at {child:?}",
            );

            let mechanism = markers
                .unvisited
                .iter()
                .find(|(marker, _)| marker == child)
                .map(|(_, mechanism)| *mechanism);
            // Wrapped in a tuple so the member body is never itself a lambda: `Member::of` peels a *leading* lambda and discards its domain annotations, which is the third mechanism and has its own pair below, and leaving it in the way would hide what `Walk::step` does with every other position.
            assert_eq!(
                call_is_seen(Term::tuple([plant])),
                mechanism.is_none(),
                "{form}, at the child {child:?}: the walk {} the planted self-call{}",
                if mechanism.is_none() {
                    "missed"
                } else {
                    "reached"
                },
                mechanism.map_or(String::new(), |mechanism| format!(" past {mechanism}")),
            );
        }
    }

    // The variable head is the node's own data rather than a child term, so the fold reports nothing there, but it is still a call position the walk must see.
    let head: Term = Subterm::Instance(Instance {
        head: InstanceHead::Var(Var::free(planted())),
        levels: Vec::new(),
    })
    .into();
    assert!(
        call_is_seen(Term::tuple([head])),
        "a universe instance's variable head: the walk missed the planted self-call",
    );

    // The third mechanism, stated as the pair that separates it from `Walk::step`: the same lambda is invisible at the top of a member body and visited one node deeper.
    let lambda = || Term::func([(binder(1), call())], Term::type_ground());
    assert!(
        !call_is_seen(lambda()),
        "a peeled lambda domain: the walk reached the planted self-call",
    );
    assert!(
        call_is_seen(Term::tuple([lambda()])),
        "a lambda domain below the peel: the walk missed the planted self-call",
    );
}

/// A declaration's carried polarity vector is not evidence.
///
/// `InductDecl::polarities` is computed by `curios-elab` after elaboration and rides the prelude archive, so believing it would make this crate's positivity check a restatement of the other's. Here the vector *lies* — it claims the parameter is used strictly while the constructor stores a function out of it, which is the negative occurrence Cantor forbids — and the analysis must reach its own verdict from the telescopes regardless.
#[test]
fn a_carried_polarity_vector_is_recomputed_rather_than_believed() {
    let mut kernel = kernel();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name, Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name,
        InductDecl {
            constructors: Vec::new(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name,
        InductDecl {
            // The lie: every parameter claimed strictly positive, while the payload below is a function *out of* the family.
            polarities: vec![Polarity::Strict],
            variances: Vec::new(),
            plicities: Vec::new(),
            ..single_payload(
                Term::func_type([(Free::local(0, Some("f")), bad_type)], false_type),
                Term::type_ground(),
            )
        },
    );

    for (name, entry) in &inducts {
        kernel.declare_induct(name, entry);
    }

    assert!(
        positivity_vectors(
            &mut kernel,
            Declarations::of(&inducts, &BTreeMap::new()),
            Coverage::Complete
        )
        .is_err(),
        "the carried vector was believed instead of recomputed",
    );
}

/// The same claim at the branch that actually decides it, which the fixture above does not reach: there `Bad` is *inside* the analyzed set, so its polarity comes from the fixpoint and the carried vector is never consulted by any rule. What separates the two coverage modes is the lookup for a name from *outside* the set, and `Coverage::Complete` answering it `Mixed` is the whole of the kernel not inheriting an elaborator-computed vector.
///
/// So `Wrapper` is registered but withheld from the analyzed map, and its carried vector lies — `Strict` in a parameter its constructor would use negatively. `Outer` stores a `Wrapper(Outer)`. Under `Complete` the lookup declines to read the registry and returns `Mixed`, `Outer` reaches itself at `Mixed`, and the set is refused. Under `Partial` — the elaborator holding one unit, where an out-of-set name is one *this same pass* analyzed when its unit was elaborated — the registry answers `Strict` and the set is admitted. Same declarations, same kernel, opposite verdicts: that difference is the branch, and nothing else in the crate exercises it.
///
/// No other test reaches it: across a kernel walk of the whole prelude and `curios`'s whole test corpus every `Complete` lookup resolves from the computed map. A change routing `Complete` to the registry the way `Partial` does would therefore pass every other test in the workspace while making the certifier believe a conclusion it did not establish.
#[test]
fn an_out_of_set_vector_is_believed_only_under_partial_coverage() {
    let mut kernel = kernel();

    let wrapper_name = Global::Authored(Qualifier::from(["Wrapper"]));
    let outer_name = Global::Authored(Qualifier::from(["Outer"]));
    let outer_type = Term::induct_type(outer_name, Vec::<Term>::new(), Vec::<Term>::new());

    // Registered, never analyzed: the lie rides on `polarities` and only a registry lookup can read it.
    kernel.declare_induct(
        &wrapper_name,
        &InductDecl {
            arity: Telescope::build(
                [(Free::local(2, Some("A")), Term::type_ground())],
                Telescope::done(()),
            ),
            constructors: Vec::new(),
            polarities: vec![Polarity::Strict],
            variances: Vec::new(),
            plicities: Vec::new(),
            ..single_payload(Term::type_ground(), Term::type_ground())
        },
    );

    let mut inducts = BTreeMap::new();
    inducts.insert(
        outer_name,
        single_payload(
            Term::induct_type(wrapper_name, [outer_type], Vec::<Term>::new()),
            Term::type_ground(),
        ),
    );
    for (name, entry) in &inducts {
        kernel.declare_induct(name, entry);
    }

    assert!(
        positivity_vectors(
            &mut kernel,
            Declarations::of(&inducts, &BTreeMap::new()),
            Coverage::Complete
        )
        .is_err(),
        "an out-of-set name must read `Mixed` under complete coverage, not the registry's vector",
    );
    assert!(
        positivity_vectors(
            &mut kernel,
            Declarations::of(&inducts, &BTreeMap::new()),
            Coverage::Partial
        )
        .is_ok(),
        "under partial coverage the registry vector is this pass's own earlier result",
    );
}

/// A declaration whose payload type an inline `rec` computes by calling itself: `rec Count : (n : Nat) -> Type = (n) => {Nat, Count(n)}`, planted as `Count(0)`.
///
/// The walk forces the `rec` head because a mutual `induct` group lowers its type constructors into one, then descends into what forcing exposed — and `RecGroup::member_body` substitutes `Term::rec_proj` for every recursive occurrence, so what it descends into holds the same group again. Each turn of the loop opens a fresh binder, so nothing about the term repeats for a memo on terms to catch, and `unfolded` cannot catch it either: it keys on `Free`, and an inline group has no name.
///
/// Written against the kernel rather than as a Curios program because this is where the shared analysis is *the* subject: the elaborator has its own copy of the divergence, but the kernel's is the one on the soundness board, and a program-level fixture exercises the elaborator's first and aborts before reaching this one.
#[test]
fn a_self_calling_type_level_rec_leaves_the_walk_terminating() {
    let mut kernel = kernel();

    let count = Free::local(1, Some("Count"));
    let n = Free::local(2, Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);

    let rec = Term::rec(
        vec![(
            count,
            Term::func_type([(n, nat())], Term::type_ground()),
            Term::func(
                [(n, nat())],
                Term::tuple_type([
                    (Free::local(3, None), nat()),
                    (
                        Free::local(4, None),
                        Term::apply(Term::free_var(&count), [Term::free_var(&n)]),
                    ),
                ]),
            ),
        )],
        Term::free_var(&count),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let name = Global::Authored(Qualifier::from(["Boxed"]));
    let payload = Term::apply(
        Term::rec_proj(group.clone(), 0),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
    );
    let mut inducts = BTreeMap::new();
    inducts.insert(name, single_payload(payload, Term::type_ground()));
    for (entry_name, entry) in &inducts {
        kernel.declare_induct(entry_name, entry);
    }

    let vectors = positivity_vectors(
        &mut kernel,
        Declarations::of(&inducts, &BTreeMap::new()),
        Coverage::Complete,
    )
    .expect("a payload the walk cannot see through is admitted, not refused");
    assert_eq!(vectors.get(&name), Some(&Vec::new()));
}

/// Well-founded recursion's shape, put to the engine under the kernel: `rec f : (a : T) -> Nat = (a) => match a | c(g) => f(g(0)) end`, where `T`'s one constructor stores a function. The call descends on `a` only because `g(0)` reads as the payload `g` it came from — a function-typed payload is a branching node whose children are its applications. The control moves the applied function out of the constructor into a second parameter, `rec f : (a : T, g : (Nat) -> T) -> Nat = (a, g) => f(g(0), g)`, and is `Partial`: `g` is bound by no arm, so nothing relates `g(0)` to `a`, and a rule that read every application of a binder as a decrease would have accepted it.
#[test]
fn an_application_of_a_constructor_payload_descends_only_from_its_arm() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let function = || Term::func_type([(Free::local(1, Some("n")), nat())], self_type.clone());
    kernel.declare_induct(&name, &single_payload(function(), Term::type_ground()));

    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let g = Free::local(4, Some("g"));
    let applied = || Term::apply(Term::free_var(&g), [zero()]);

    let descending = Term::rec(
        vec![(
            f,
            Term::func_type([(a, self_type.clone())], nat()),
            Term::func(
                [(a, self_type.clone())],
                Term::induct_match(
                    Term::free_var(&a),
                    None,
                    nat(),
                    [("c", vec![g], Term::apply(Term::free_var(&f), [applied()]))],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*descending else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Total));

    let control = Term::rec(
        vec![(
            f,
            Term::func_type([(a, self_type.clone()), (g, function())], nat()),
            Term::func(
                [(a, self_type.clone()), (g, function())],
                Term::apply(Term::free_var(&f), [applied(), Term::free_var(&g)]),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*control else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// A lambda applied to an arm's payload is graded as its contractum: the call inside reads the payload, which is below the scrutinee. This is the shape a convoy takes — an arm generalized over a hypothesis and applied back to it — and without the rule a proof that descends through one closes to an all-unknown matrix. The control applies the same lambda to an unrelated parameter, which must stay unread: the rule reads a binder as what it stands for, never as a decrease of its own.
#[test]
fn a_lambda_applied_to_the_arm_payload_descends() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    kernel.declare_induct(
        &name,
        &single_payload(self_type.clone(), Term::type_ground()),
    );

    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let b = Free::local(4, Some("b"));
    let t = Free::local(5, Some("t"));
    let h = Free::local(6, Some("h"));

    let group_over = |argument: Term| {
        let redex = Term::apply(
            Term::func(
                [(h, self_type.clone())],
                Term::apply(Term::free_var(&f), [Term::free_var(&h), Term::free_var(&b)]),
            ),
            [argument],
        );
        Term::rec(
            vec![(
                f,
                Term::func_type([(a, self_type.clone()), (b, self_type.clone())], nat()),
                Term::func(
                    [(a, self_type.clone()), (b, self_type.clone())],
                    Term::induct_match(Term::free_var(&a), None, nat(), [("c", vec![t], redex)]),
                ),
            )],
            Term::free_var(&f),
        )
    };

    let descending = group_over(Term::free_var(&t));
    let Subterm::Rec(Rec { group, .. }) = &*descending else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Total));

    let control = group_over(Term::free_var(&b));
    let Subterm::Rec(Rec { group, .. }) = &*control else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// The same pair through a `let`: a binder aliasing the payload is below the scrutinee exactly as the payload is, and one aliasing an unrelated parameter is not.
#[test]
fn a_let_alias_of_the_arm_payload_descends() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    kernel.declare_induct(
        &name,
        &single_payload(self_type.clone(), Term::type_ground()),
    );

    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let b = Free::local(4, Some("b"));
    let t = Free::local(5, Some("t"));
    let h = Free::local(6, Some("h"));

    let group_over = |aliased: Term| {
        let arm = Term::let_(
            &h,
            self_type.clone(),
            aliased,
            Term::apply(Term::free_var(&f), [Term::free_var(&h), Term::free_var(&b)]),
        );
        Term::rec(
            vec![(
                f,
                Term::func_type([(a, self_type.clone()), (b, self_type.clone())], nat()),
                Term::func(
                    [(a, self_type.clone()), (b, self_type.clone())],
                    Term::induct_match(Term::free_var(&a), None, nat(), [("c", vec![t], arm)]),
                ),
            )],
            Term::free_var(&f),
        )
    };

    let descending = group_over(Term::free_var(&t));
    let Subterm::Rec(Rec { group, .. }) = &*descending else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Total));

    let control = group_over(Term::free_var(&b));
    let Subterm::Rec(Rec { group, .. }) = &*control else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// The outer direction reaches a variable inside an index, and only a local's.
///
/// A case whose target is `1`, met at an actual index `n + 1`, is reachable only when `n` is `0`: the peel reduces the pair to `0` against `n`, and the second direction solves the outer variable there. Both checkers get it from `solve_indices`. The control drops the one fact the rule reads from its driver: with `n` not a local the walk opened, it is a name no case can refine, and nothing is solved.
#[test]
fn the_outer_direction_solves_a_variable_inside_an_index() {
    let n = Free::local(900, Some("n"));
    let nat = |k: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(k)));
    let actual = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));

    for (label, local) in [("a local", true), ("a name no case refines", false)] {
        let mut kernel = kernel();
        if local {
            kernel.assume(&n, &Term::intrinsic(Intrinsic::NatType));
        }

        let Invert::Solved(solutions) =
            solve_indices(&mut kernel, slice::from_ref(&actual), &[nat(1)], &[])
                .expect("the kernel answers")
        else {
            panic!("{label}: a reachable case was reported unreachable");
        };

        match local {
            true => assert_eq!(solutions, vec![(n, nat(0))], "{label}"),
            false => assert!(solutions.is_empty(), "{label}: {solutions:?}"),
        }
    }
}

/// A binder the first direction pins is rewritten through what the second solves, so the arm's substitution is idempotent.
///
/// Actuals `(n + 1, n + 1)` against targets `(k, 1)`: the first direction pins `k := n + 1`, and the second learns `n := 0` from the other position. Applied one after the other the pair would leave `k` naming a variable the same substitution replaces; composed, `k`'s value no longer mentions `n` at all.
#[test]
fn a_pinned_binder_is_rewritten_through_the_outer_solution() {
    let mut kernel = kernel();
    let n = Free::local(900, Some("n"));
    let k = Free::local(901, Some("k"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    kernel.assume(&n, &nat_type);
    kernel.assume(&k, &nat_type);

    let actual = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(1)));
    let Invert::Solved(solutions) = solve_indices(
        &mut kernel,
        &[actual.clone(), actual],
        &[Term::free_var(&k), nat(1)],
        slice::from_ref(&k),
    )
    .expect("the kernel answers") else {
        panic!("a reachable case was reported unreachable");
    };

    let value_of = |name: &Free| {
        solutions
            .iter()
            .find(|(solved, _)| solved == name)
            .map(|(_, value)| value.clone())
    };
    assert_eq!(
        value_of(&n),
        Some(nat(0)),
        "the outer variable: {solutions:?}"
    );
    let pinned = value_of(&k).expect("the binder is pinned");
    assert!(
        !pinned.mentions_free(&n),
        "the pinned value still names what the substitution replaces: {pinned}"
    );
}

/// Reading an argument's size through what stands before it is a probe: where the budget cannot afford the reading, the engine answers with the refusal. Reading the argument as nothing would classify the group `Partial`, a verdict with no cause in the term that the drivers would report as non-termination. The control is the same group with the budget to read it.
#[test]
fn a_size_the_budget_cannot_read_refuses_the_classification() {
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let f = Free::local(2, Some("f"));
    let n = Free::local(3, Some("n"));
    let v = Free::local(4, Some("v"));
    let identity = Term::func([(v, nat())], Term::free_var(&v));
    let rec = Term::rec(
        vec![(
            f,
            Term::func_type([(n, nat())], nat()),
            Term::func(
                [(n, nat())],
                Term::apply(
                    Term::free_var(&f),
                    [Term::apply(identity, [Term::free_var(&n)])],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let mut starved = Kernel::new(0, SYNTAX);
    assert!(group_totality(&mut starved, group).is_err_and(|error| error.is_exhausted()));
    assert_eq!(group_totality(&mut kernel(), group), Ok(Totality::Partial));
}

/// The conversion chain's respellings are probes: a connective's leaf with no value at the type level is read as written, an atom like any other, rather than turning the comparison into the leaf's own failure. The two sides differ by a unit the truth table sees through once the leaf is an atom.
#[test]
fn a_connective_leaf_with_no_value_is_compared_as_written() {
    let mut kernel = kernel();
    let b = Free::local(900, Some("b"));
    let x = Free::local(901, Some("x"));
    let boolean = Term::intrinsic(Intrinsic::BoolType);
    kernel.assume(&b, &boolean);
    kernel.assume(&x, &Term::intrinsic(Intrinsic::NatType));
    let nat = |k: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(k)));
    let undefined = Term::intrinsic(Intrinsic::NatEql(
        Term::intrinsic(Intrinsic::NatDiv {
            dividend: nat(1),
            divisor: nat(0),
            non_zero: Term::free_var(&x),
        }),
        nat(0),
    ));
    let this = Term::intrinsic(Intrinsic::BoolAnd(Term::free_var(&b), undefined.clone()));
    let that = Term::intrinsic(Intrinsic::BoolAnd(
        Term::free_var(&b),
        Term::intrinsic(Intrinsic::BoolAnd(
            undefined,
            Term::intrinsic(Intrinsic::Bool(true)),
        )),
    ));

    assert_eq!(kernel.convert_at(&boolean, &this, &that), Ok(true));
}

/// Conversion's readers key atoms on spelling, so two sums of atoms that commute a sum inside their arguments would meet only where the positional congruence pairs them, and an `Int` sum keeps the order it was written in: `h(i + j) + e(k + l)` against `e(l + k) + h(j + i)` would pair `h` with `e` and be refused, whichever identities the heads hold. A pair the chain decides nothing about is read once more with every atom's arguments forced, and the cancellation then pairs them. The control keeps one argument genuinely different, and stays apart.
#[test]
fn atoms_equal_up_to_their_arguments_convert_in_whichever_order_their_sum_holds_them() {
    let int = || Term::intrinsic(Intrinsic::IntType);
    let [i, j, k, l] = [1, 2, 3, 4].map(|index| Free::local(index, None));
    let sum = |left: Term, right: Term| Term::intrinsic(Intrinsic::IntAdd(left, right));
    let of = |left: &Free, right: &Free| sum(Term::free_var(left), Term::free_var(right));

    for (h_index, e_index) in [(10, 11), (11, 10)] {
        let mut kernel = kernel();
        let h = Free::local(h_index, Some("h"));
        let e = Free::local(e_index, Some("e"));
        for name in [i, j, k, l] {
            kernel.assume(&name, &int());
        }
        let function = Term::func_type([(Free::local(99, None), int())], int());
        kernel.assume(&h, &function);
        kernel.assume(&e, &function);
        let at = |head: &Free, argument: Term| Term::apply(Term::free_var(head), [argument]);

        let this = sum(at(&h, of(&i, &j)), at(&e, of(&k, &l)));
        let that = sum(at(&e, of(&l, &k)), at(&h, of(&j, &i)));
        let apart = sum(at(&e, of(&l, &k)), at(&h, of(&j, &j)));

        assert_eq!(
            kernel.convert_at(&int(), &this, &that),
            Ok(true),
            "h at {h_index}, e at {e_index}"
        );
        assert_eq!(
            kernel.convert_at(&int(), &this, &apart),
            Ok(false),
            "h at {h_index}, e at {e_index}"
        );
    }
}

/// One rule says which terms an arm's equation answers, and both reducers ask it: the key itself, a term the carriers' readers hold equal to the key, and — where the equation assumes a `Bool` — a term they hold equal to the key negated, the value negated on the way.
///
/// The controls are what the rule must not answer: a comparison over another atom, and a term of another kind than the key's.
#[test]
fn an_equation_answers_a_term_the_readers_hold_equal_to_its_key() {
    let mut kernel = kernel();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let [a, b, c] = [1, 2, 3].map(|index| Free::local(index, None));
    for binder in [&a, &b, &c] {
        kernel.assume(binder, &nat_type);
    }
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let sum = |left: &Free, right: &Free| {
        Term::intrinsic(Intrinsic::nat_add(
            Term::free_var(left),
            Term::free_var(right),
        ))
    };
    let truth = |value: bool| Term::intrinsic(Intrinsic::Bool(value));

    let key = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &b), nat(10)));
    let commuted = Term::intrinsic(Intrinsic::nat_lt(sum(&b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::nat_lte(nat(10), sum(&b, &a)));
    let other = Term::intrinsic(Intrinsic::nat_lt(sum(&a, &c), nat(10)));

    assert_eq!(
        answers(&mut kernel, &key, &key, &truth(true)),
        Ok(Answered::Value(truth(true)))
    );
    assert_eq!(
        answers(&mut kernel, &commuted, &key, &truth(true)),
        Ok(Answered::Value(truth(true))),
        "a sum commuted is the key's comparison"
    );
    assert_eq!(
        answers(&mut kernel, &dual, &key, &truth(true)),
        Ok(Answered::Value(truth(false))),
        "the dual of a guard that holds does not"
    );
    assert_eq!(
        answers(&mut kernel, &dual, &key, &truth(false)),
        Ok(Answered::Value(truth(true))),
        "and the dual of one that fails holds"
    );
    assert_eq!(
        answers(&mut kernel, &other, &key, &truth(true)),
        Ok(Answered::Silent),
        "a comparison over another atom is another comparison"
    );

    // A key that is a number answers a number the readers hold equal to it, and nothing of another kind.
    let total = sum(&a, &b);
    assert_eq!(
        answers(&mut kernel, &sum(&b, &a), &total, &nat(0)),
        Ok(Answered::Value(nat(0)))
    );
    assert_eq!(
        answers(&mut kernel, &commuted, &total, &nat(0)),
        Ok(Answered::Silent)
    );
}

/// Past what the readers decide, the rule hands a question back and calls no judgment: the atoms of two operations of one kind, where some two may be one, and the whole pair where the two are no operations of the algebra and their heads say they may be one term. Asked again with the atoms classed, it reads the pair one spelling to a class, for the key and for the key negated. What cannot be one term is answered with silence, so a checker is asked nothing about it.
///
/// The judge here stands for a checker's conversion and says every pair it is asked about is one, so what is held is the rule's half: which pairs it asks about, and what it makes of the answer.
#[test]
fn an_equation_hands_back_what_its_checker_is_to_ask() {
    let mut kernel = kernel();
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let [a, b, f, g, x] = [1, 2, 3, 4, 5].map(|index| Free::local(index, None));
    for binder in [&a, &b] {
        kernel.assume(binder, &nat_type);
    }
    let unary = Term::func_type([(x, nat_type.clone())], nat_type);
    for binder in [&f, &g] {
        kernel.assume(binder, &unary);
    }
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));
    let truth = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let call = |head: &Free, left: &Free, right: &Free| {
        let sum = Intrinsic::nat_add(Term::free_var(left), Term::free_var(right));
        Term::apply(Term::free_var(head), [Term::intrinsic(sum)])
    };

    let key = Term::intrinsic(Intrinsic::nat_lt(call(&f, &a, &b), nat(10)));
    let respelled = Term::intrinsic(Intrinsic::nat_lt(call(&f, &b, &a), nat(10)));
    let dual = Term::intrinsic(Intrinsic::nat_lte(nat(10), call(&f, &b, &a)));
    let other = Term::intrinsic(Intrinsic::nat_lt(call(&g, &b, &a), nat(10)));

    let Ok(Answered::Atoms(atoms)) = answers(&mut kernel, &respelled, &key, &truth(true)) else {
        panic!("two calls of one head may be one, and which is the checker's to say");
    };
    assert!(atoms.contains(&call(&f, &b, &a)) && atoms.contains(&call(&f, &a, &b)));
    assert_eq!(
        answers(&mut kernel, &other, &key, &truth(true)),
        Ok(Answered::Silent),
        "two heads are two atoms to every checker"
    );

    let Ok(one) = Classes::of(&atoms, |_, _| Ok::<_, std::convert::Infallible>(true));
    assert_eq!(
        answers_classed(&mut kernel, &respelled, &key, &truth(true), &one),
        Ok(Some(truth(true)))
    );
    assert_eq!(
        answers_classed(&mut kernel, &dual, &key, &truth(true), &one),
        Ok(Some(truth(false))),
        "the dual over the classed atom is the key negated"
    );
    let Ok(apart) = Classes::of(&atoms, |_, _| Ok::<_, std::convert::Infallible>(false));
    assert_eq!(
        answers_classed(&mut kernel, &respelled, &key, &truth(true), &apart),
        Ok(None),
        "atoms a checker holds apart leave the pair as it was"
    );

    // No operation of the algebra on either side: the whole pair is the question, or there is none.
    assert_eq!(
        answers(&mut kernel, &call(&f, &b, &a), &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Whole)
    );
    assert_eq!(
        answers(&mut kernel, &call(&g, &b, &a), &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Silent)
    );
    assert_eq!(
        answers(&mut kernel, &Term::free_var(&a), &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Silent)
    );
    // Nor two formers: a spelling still to be unfolded is put to nothing until it is reduced, or every stuck form beside it would be a question.
    let projected = Term::proj(Term::free_var(&a), 0);
    assert_eq!(
        answers(&mut kernel, &projected, &call(&f, &a, &b), &nat(0)),
        Ok(Answered::Silent)
    );
}
