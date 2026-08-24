//! The shared analyses, exercised through a real checker.
//!
//! These live here rather than beside the analyses in `curios-analysis` for a structural reason, not a filing one. Every analysis on that seam is written against `Env`/`Judge`, and `Judge`'s one method is `convert_at` — so testing one needs a *real* implementation of conversion. The only ones in the workspace are this crate's `Kernel` and the elaborator's `Context`, and both sit above `curios-analysis`. A test-only `Env` would be a third checker, which is exactly what nobody should write.
//!
//! A dev-dependency cycle does not solve it either: a `#[cfg(test)]` module inside `curios-analysis` compiles that crate a second time, so `Kernel`'s `Judge` implementation would be against the *other* copy and the trait bound would not hold. An integration test links the real libraries, which is why this file is here and not there.
//!
//! What stays in `curios-analysis` is everything that needs no checker at all — the polarity lattice's own laws, the size-change matrix algebra, universe satisfiability. Those are unit tests of pure functions and belong beside them.

use {
    curios_analysis::{
        Coverage, Declarations, Invert, group_totality, invert_indices, positivity_vectors,
    },
    curios_cert::Kernel,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, MetaId, Metavar, Nat, Polarity,
        Rec, StructType, Subterm, Telescope, Term, Totality, UniverseContext, UniverseInst,
    },
    curios_utilities::{
        CharacterSyntax, ConceptField, LiftSyntax, MonadSyntax, OperatorSyntax, Plicity,
        ProofSyntax, Qualifier, StringSyntax, SyntaxName, SyntaxRegistry,
    },
    std::{collections::BTreeMap, rc::Rc, slice},
};

use Polarity::Strict;

const fn syntax_name(segments: &'static [&'static str]) -> SyntaxName {
    SyntaxName::new(segments)
}

const fn concept_field(segments: &'static [&'static str], label: &'static str) -> ConceptField {
    ConceptField {
        concept: syntax_name(segments),
        field: label,
    }
}

/// The registry a `Kernel` is built with. Spelled here rather than borrowed because `curios-cert`'s own fixture is `#[cfg(test)]` and an integration test links the library, not its tests. No probe below resolves one of these names — they exist so a kernel can be constructed at all.
const SYNTAX: SyntaxRegistry = SyntaxRegistry {
    monad: MonadSyntax {
        bind: syntax_name(&["syn", "Monad", "bind"]),
    },
    lift: LiftSyntax {
        lift: concept_field(&["syn", "Lift"], "lift"),
    },
    operator: OperatorSyntax {
        add: concept_field(&["syn", "Add"], "add"),
        sub: concept_field(&["syn", "Sub"], "sub"),
        mul: concept_field(&["syn", "Mul"], "mul"),
        div: concept_field(&["syn", "Div"], "div"),
        rem: concept_field(&["syn", "Rem"], "rem"),
        eql: concept_field(&["syn", "Eql", "Eql"], "eql"),
        neq: concept_field(&["syn", "Eql", "Eql"], "neq"),
        lt: concept_field(&["syn", "Cmp"], "lt"),
        gt: concept_field(&["syn", "Cmp"], "gt"),
        le: concept_field(&["syn", "Cmp"], "le"),
        ge: concept_field(&["syn", "Cmp"], "ge"),
        and: concept_field(&["syn", "And"], "and"),
        or: concept_field(&["syn", "Or"], "or"),
    },
    character: CharacterSyntax {
        character: syntax_name(&["syn", "Char", "Char"]),
        scalar_below: syntax_name(&["syn", "Char", "Scalar", "below"]),
        scalar_above: syntax_name(&["syn", "Char", "Scalar", "above"]),
    },
    string: StringSyntax {
        string: syntax_name(&["syn", "Str", "Str"]),
        of_scan_eq: syntax_name(&["syn", "Str", "of_scan_eq"]),
        refl_scan: syntax_name(&["syn", "Str", "refl_scan"]),
    },
    proof: ProofSyntax {
        true_qed: syntax_name(&["syn", "True", "True", "qed"]),
        true_type: syntax_name(&["syn", "True", "True"]),
        lt: syntax_name(&["syn", "Nat", "Lt"]),
        le: syntax_name(&["syn", "Nat", "Le"]),
        int_non_zero: syntax_name(&["syn", "Int", "NonZero"]),
        int_non_neg: syntax_name(&["syn", "Int", "NonNeg"]),
        bytes_four: syntax_name(&["syn", "Flt", "FourBytes"]),
        flt_finite: syntax_name(&["syn", "Flt", "Finite"]),
        flt_non_neg: syntax_name(&["syn", "Flt", "NonNeg"]),
    },
};

/// The member every probe below plants a call to.
fn planted() -> Free {
    Free::local(1, Some("f"))
}

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000, SYNTAX);
    kernel.set_local_floor(1_000);
    kernel
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
                        InductParam {
                            telescope: Telescope::done(Vec::new()),
                            plicities: Vec::new(),
                        },
                    )
                })
                .collect(),
            result_sort,
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    family
}

/// The deletion rule (Goguen–McBride–McKinna) decides a binder forced in two index positions by *convertibility*, and this is the direction no program reaches.
///
/// Instrumenting `consolidate` and running the whole corpus — the fixed prelude through the kernel's own walk, plus every test program through both checkers — counts 5883 inversions, 20 of which re-force a binder. Sixteen refuse, on genuinely inconvertible `Bits` spines. The four that accept are all `prior == value`: the two forcings are *syntactically identical*, so a plain equality test would have decided every acceptance the corpus contains. The semantic half of the rule — accepting two forcings that differ but convert — is exercised by nothing, which is precisely the condition under which a rule's mistakes stay invisible. `documentation/soundness/across-the-perimeter.md` collects the entries caught this way, (V)'s argument rule among them.
///
/// So the two directions are put to it directly. Both fixtures force one binder to `a()` in the first index position and to `b()` in the second, differing in nothing but the family's sort — which is what makes them a control pair rather than two unrelated cases.
///
/// At a proposition the two forcings convert by irrelevance, so the rule deletes the redundant constraint and one solution survives: sound because `Eq : Prop` makes the system definitionally K, and harmless because the surviving substitution is interchangeable with the one it replaced. At a relevant family they do not convert, and the binder's solutions are dropped rather than reconciled — the arm is still checked, with the binder simply unsolved.
///
/// What both must never be is `Impossible`. That verdict excuses an arm from being checked at all, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to a closed inhabitant of `False` that `documentation/soundness/per-term-rules/coverage.md` records, routed there from index inversion. `consolidate` returns no clash by construction; this is what holds that to account, since the refusing path removes solutions and a future rewrite could as easily report the position unreachable.
#[test]
fn a_binder_forced_twice_survives_only_when_its_forcings_convert() {
    for (label, sort, surviving) in [
        (
            "a proposition, whose two inhabitants irrelevance identifies",
            Term::prop(),
            1,
        ),
        (
            "a relevant family, whose two inhabitants a program tells apart",
            Term::type_ground(),
            0,
        ),
    ] {
        let mut kernel = kernel();
        let family = declare(&mut kernel, "Forced", sort);
        let binder = Free::local(900, Some("p"));

        kernel.assume(
            &binder,
            &Term::induct_type(family.clone(), Vec::<Term>::new(), Vec::<Term>::new()),
        );

        let inhabitant =
            |tag| Term::variant(family.clone(), Vec::<Term>::new(), tag, Vec::<Term>::new());
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
                "{label}: a re-forced binder was reported unreachable, which excuses the arm from being checked"
            );
        };

        assert_eq!(
            solutions.len(),
            surviving,
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
/// `consolidate` decides a re-forcing by asking the `Judge` for the binder's assumed type and comparing the two forcings at it. With no assumption there is nothing to compare at, so it drops the binder's solutions — the conservative direction, leaving the binder unsolved and the arm still checked. Instrumenting that branch and running the fixed prelude through the kernel's own walk, every program in `curios`'s corpus through both checkers, and this crate's own tests counts it firing **zero** times in all three: both kernel callers reach `invert_indices` through `open_payload`, which assumes every binder it opens before the body runs, so from this crate the branch is unreachable by construction.
///
/// It is pinned anyway, because the conservative answer is not the obvious one to write. `None => true` — no type, so nothing to disagree about, so keep the forcing — is the plausible slip, and it would accept a re-forcing that *no convertibility test ever decided*, which is the deletion rule discharging a constraint it never checked. Nothing else in the workspace would notice.
///
/// The discrimination is against the proposition case above rather than beside it: identical family, identical sort, identical forcings, and the binder assumed there and not here. That one difference must turn one surviving solution into none. And as there, the verdict must stay `Solved` — `Impossible` excuses the arm from being checked, and an arm excused wrongly at a `Prop`-sorted family is the vacuous-elimination route to `False`.
#[test]
fn a_binder_whose_type_is_out_of_scope_drops_its_forcings() {
    let mut kernel = kernel();
    let family = declare(&mut kernel, "Forced", Term::prop());
    let binder = Free::local(900, Some("p"));

    // Deliberately not assumed: this is the whole of the fixture.

    let inhabitant =
        |tag| Term::variant(family.clone(), Vec::<Term>::new(), tag, Vec::<Term>::new());
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
            "a binder with no assumption was reported unreachable, which excuses the arm from being checked"
        );
    };

    assert!(
        solutions.is_empty(),
        "a binder whose type is out of scope kept a forcing that no convertibility test decided",
    );
}

/// The clash rule's license is the registry, and a family the registry cannot answer for must not clash.
///
/// Two different constructors of one family definitely clash only when the family is *relevant*: at a `Prop`-sorted family irrelevance makes every inhabitant the same value, so reading tag disjointness there contradicts conversion, and an arm excused as impossible on that reading is the vacuous-elimination route to a closed inhabitant of `False` that `documentation/soundness/per-term-rules/index-inversion-and-k.md` records. The sort is read out of `induct_decl`, so the whole of that protection rests on the lookup answering — and a lookup that answered nothing used to fall through to the tag check and answer `Impossible`: the analysis deciding the strong verdict *because* it was blind, where every other blindness on this seam — an unknown positivity name, an undecodable size, an unassumed binder — turns into a refusal. The branch is unreachable from either checker as far as the walks are known, a well-typed variant's family being registered wherever the term came from, but the fixture above pins an equally unreachable branch for the same reason: the conservative answer is not the obvious one to write, and nothing else in the workspace would notice the wrong one.
///
/// Verified while the branch was wrong: the subject below answered `Impossible` for a family absent from the registry. The control pair holds what the correction must not have moved — the same position with the declaration present still clashes at a relevant sort, and still yields nothing at `Prop`.
#[test]
fn a_family_the_registry_cannot_answer_for_does_not_clash() {
    let inhabitant = |family: &Global, tag: &str| {
        Term::variant(family.clone(), Vec::<Term>::new(), tag, Vec::<Term>::new())
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
            InductParam {
                telescope: Telescope::build([(binder, payload_type)], Vec::new()),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort,
        module: Qualifier::from(["T"]),
        rep_public: true,
        polarities: Vec::new(),
    }
}

/// The four-line route to `False`, refused by the kernel running the shared analysis: `Bad`'s constructor takes `(Bad) -> False`, a negative self-occurrence.
#[test]
fn a_negative_self_occurrence_is_refused() {
    let mut kernel = kernel();

    let false_name = Global::Authored(Qualifier::from(["False"]));
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name.clone(),
        InductDecl {
            constructors: Vec::new(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name.clone(),
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
    assert_eq!(refusal.name, bad_name);
}

/// A strictly positive self-occurrence — the payload *is* the family — is the ordinary recursive datatype and is admitted.
#[test]
fn a_strict_self_occurrence_is_admitted() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["Chain"]));
    let self_type = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let mut inducts = BTreeMap::new();
    inducts.insert(name.clone(), single_payload(self_type, Term::type_ground()));
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

#[test]
fn a_huge_literal_call_argument_grades_without_expansion() {
    // `rec f : (n: Nat) -> Nat = (n) => f(u64::MAX); f` — grading the literal argument must read the packed spine, not peel it: the unary expansion this replaces would loop once per successor, and the value is unbounded by the source that spelled it.
    let mut kernel = Kernel::new(100_000, SYNTAX);
    let f = Free::local(1, Some("f"));
    let n = Free::local(2, Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let rec = Term::rec(
        vec![(
            f.clone(),
            Term::func_type([(n.clone(), nat())], nat()),
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
    assert_eq!(group_totality(&mut kernel, group), Totality::Partial);
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

    group_totality(&mut kernel, group) == Totality::Partial
}

/// The differential `documentation/soundness.md` asks for: `Walk::walk` visits every child position `Subterm::any_child_term` does, minus a named whitelist.
///
/// This matters more here than anywhere else on the perimeter because this is the only analysis whose blindness *admits*. Every other one refuses when it cannot see — positivity answers `Mixed` at an out-of-set name, `whnf` goes stuck, inversion derives nothing at a `Prop`-valued position, an under-applied call is graded `Matrix::unknown` — while a call site the walk never visits contributes no edge, and a group with no edges is `Total`. Route eight of this row's history was exactly that and nothing else: a projected inner group went unwalked, `rec f(n) -> False = (rec g(m) -> False = f(m); g)(n)` closed to no call whatsoever, both groups classified `Total`, and `f(0)` diverged through `g` while (V) read the verdict.
///
/// The probe needs no instrumentation because the engine types nothing: it is a total function of post-zonk terms, so an ill-typed fixture is a legitimate input. Each row plants a *nullary* self-call at one child position — the member takes no lambda, so a self-call from it is a 0x0 matrix, idempotent with no diagonal — which makes the verdict `Partial` exactly when the walk reached the plant and `Total` exactly when it did not.
///
/// **The whitelist is three, where the prose audit this replaces counted two.** Its two are a `Metavar`'s spine, which `zonk_module` refuses before this pass runs, and a nested `Rec` member's declared type. The third is a separate mechanism the audit names but folds into the second: `Member::of` peels the member body's *leading* lambdas and discards their domain annotations at `Telescope::Cons(_, rest)`, so a call planted there is invisible while the same lambda one node deeper is walked — which is why every row below is wrapped in a tuple, and why the two spellings are asserted against each other at the end.
///
/// All three are type positions, and the argument that they are safe is the audit's: a call in one is consumed by β or read only by typing, never reduced, and an edge the engine misses is dangerous only where it can complete a reduction cycle. That argument is not what this test checks. What it checks is that the set does not quietly grow — a new term-bearing field on an existing variant is absorbed by the `..` in both patterns without a compile error, and would otherwise widen what the engine accepts in silence.
#[test]
fn the_walk_reaches_every_child_position_but_the_three_it_documents() {
    let filler = || Term::intrinsic(Intrinsic::NatType);
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let name = || Global::Authored(Qualifier::from(["N"]));
    let binder = |index: u32| Free::local(500 + index, None);
    let call = || Term::free_var(&planted());

    // Every child position `Subterm::any_child_term` visits, and what the walk must say about it.
    let positions: Vec<(&str, Term, bool)> = vec![
        (
            "a universe instance's head",
            Subterm::UniverseInst(UniverseInst {
                head: call(),
                levels: Vec::new(),
            })
            .into(),
            true,
        ),
        (
            "an intrinsic's operand",
            Term::intrinsic(Intrinsic::ListType(call())),
            true,
        ),
        (
            "a lambda's domain",
            Term::func([(binder(1), call())], zero()),
            true,
        ),
        (
            "a lambda's body",
            Term::func([(binder(1), filler())], call()),
            true,
        ),
        (
            "a function type's domain",
            Term::func_type([(binder(1), call())], zero()),
            true,
        ),
        (
            "a function type's codomain",
            Term::func_type([(binder(1), filler())], call()),
            true,
        ),
        ("an application's head", Term::apply(call(), [zero()]), true),
        (
            "an application's argument",
            Term::apply(zero(), [call()]),
            true,
        ),
        (
            "a tuple type's component",
            Term::tuple_type(vec![(binder(1), call())]),
            true,
        ),
        ("a tuple's field", Term::tuple([call()]), true),
        ("a projection's head", Term::proj(call(), 0), true),
        (
            "a nominal type's parameter",
            Term::induct_type(name(), [call()], Vec::<Term>::new()),
            true,
        ),
        (
            "a nominal type's index",
            Term::induct_type(name(), Vec::<Term>::new(), [call()]),
            true,
        ),
        (
            "a constructor's parameter",
            Term::variant(name(), [call()], "mk", Vec::<Term>::new()),
            true,
        ),
        (
            "a constructor's payload",
            Term::variant(name(), Vec::<Term>::new(), "mk", [call()]),
            true,
        ),
        (
            "a structure type's parameter",
            Subterm::StructType(StructType {
                name: name(),
                universes: Vec::new(),
                params: vec![call()],
            })
            .into(),
            true,
        ),
        (
            "a structure literal's parameter",
            Term::struct_(name(), [call()], Vec::<Term>::new()),
            true,
        ),
        (
            "a structure literal's field",
            Term::struct_(name(), Vec::<Term>::new(), [call()]),
            true,
        ),
        (
            "a match's scrutinee",
            Term::bool_match(call(), None, filler(), zero(), zero()),
            true,
        ),
        (
            "a match's motive",
            Term::bool_match(zero(), None, call(), zero(), zero()),
            true,
        ),
        (
            "a boolean arm",
            Term::bool_match(zero(), None, filler(), call(), zero()),
            true,
        ),
        (
            "a let binding's type",
            Term::let_(&binder(1), call(), zero(), zero()),
            true,
        ),
        (
            "a let binding's value",
            Term::let_(&binder(1), filler(), call(), zero()),
            true,
        ),
        (
            "a let's tail",
            Term::let_(&binder(1), filler(), zero(), call()),
            true,
        ),
        (
            "a nested group's member body",
            Term::rec(vec![(binder(1), filler(), call())], zero()),
            true,
        ),
        (
            "a nested group's tail",
            Term::rec(vec![(binder(1), filler(), zero())], call()),
            true,
        ),
        // The two positions the audit names, and the reason each is safe to skip.
        (
            "a metavariable's spine",
            Subterm::Metavar(Metavar {
                id: MetaId::from(0usize),
                spine: Rc::new(vec![call()]),
                origin: None,
            })
            .into(),
            false,
        ),
        (
            "a nested group's member type",
            Term::rec(vec![(binder(1), call(), zero())], zero()),
            false,
        ),
    ];

    // The harness itself, first: a bare self-call must be seen and a call-free body must not, or every row below passes for the wrong reason.
    assert!(call_is_seen(call()), "the bare self-call was not seen");
    assert!(
        !call_is_seen(zero()),
        "a call-free body was read as recursive"
    );

    // Wrapped in a tuple so the member body is never itself a lambda: `Member::of` peels a *leading* lambda and discards its domain annotations, which is a separate divergence with its own row below, and leaving it in the way would hide what `Walk::step` does with every other position.
    for (position, plant, expected) in positions {
        assert_eq!(
            call_is_seen(Term::tuple([plant])),
            expected,
            "{position}: the walk {} the planted self-call",
            if expected { "missed" } else { "reached" },
        );
    }

    // The third divergence, stated as the pair that separates it from `Walk::step`: the same lambda is invisible at the top of a member body and visited one node deeper.
    let lambda = || Term::func([(binder(1), call())], zero());
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
    let false_type = Term::induct_type(false_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    let bad_type = Term::induct_type(bad_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let mut inducts = BTreeMap::new();
    inducts.insert(
        false_name.clone(),
        InductDecl {
            constructors: Vec::new(),
            ..single_payload(Term::type_ground(), Term::prop())
        },
    );
    inducts.insert(
        bad_name.clone(),
        InductDecl {
            // The lie: every parameter claimed strictly positive, while the payload below is a function *out of* the family.
            polarities: vec![Polarity::Strict],
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
/// So `Wrapper` is registered but withheld from the analyzed map, and its carried vector lies — `Strict` in a parameter its constructor would use negatively. `Outer` stores a `Wrapper(Outer)`. Under `Complete` the lookup declines to read the registry and returns `Mixed`, `Outer` reaches itself at `Mixed`, and the set is refused. Under `Partial` — the elaborator replaying a prelude, where an out-of-set name is one *this same pass* analyzed earlier — the registry answers `Strict` and the set is admitted. Same declarations, same kernel, opposite verdicts: that difference is the branch, and nothing else in the crate exercises it.
///
/// It was measured firing nowhere before this test. Across a kernel walk of the whole prelude every one of 124 lookups resolved from the computed map, and across `curios`'s whole test corpus 30,271 did, with the `Complete` fallback taken zero times and the `Partial` registry read taken 26. A change routing `Complete` to the registry the way `Partial` does would therefore have passed every test in the workspace while making the certifier believe a conclusion it did not establish.
#[test]
fn an_out_of_set_vector_is_believed_only_under_partial_coverage() {
    let mut kernel = kernel();

    let wrapper_name = Global::Authored(Qualifier::from(["Wrapper"]));
    let outer_name = Global::Authored(Qualifier::from(["Outer"]));
    let outer_type = Term::induct_type(outer_name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    // Registered, never analyzed: the lie rides on `polarities` and only a registry lookup can read it.
    kernel.declare_induct(
        &wrapper_name,
        &InductDecl {
            arity: Telescope::build(
                [(Free::local(2, Some("A")), Term::type_ground())],
                Telescope::done(()),
            ),
            constructors: Vec::new(),
            polarities: vec![Strict],
            ..single_payload(Term::type_ground(), Term::type_ground())
        },
    );

    let mut inducts = BTreeMap::new();
    inducts.insert(
        outer_name.clone(),
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
