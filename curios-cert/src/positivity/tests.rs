use {
    super::{Coverage, Occurrences, close, positivity_vectors},
    crate::Kernel,
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam,
        Polarity::{self, *},
        Telescope, Term, UniverseContext,
    },
    std::collections::BTreeMap,
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
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
        root: RootId::Entry,
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

    let refusal = positivity_vectors(&mut kernel, &inducts, &BTreeMap::new(), Coverage::Complete)
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

    let vectors = positivity_vectors(&mut kernel, &inducts, &BTreeMap::new(), Coverage::Complete)
        .expect("a strictly positive declaration is admitted");
    assert_eq!(vectors.get(&name), Some(&Vec::new()));
}

const EVERY: [Polarity; 5] = [Unused, Strict, Pos, Neg, Mixed];

fn declaration(name: &str) -> Global {
    Global::Authored(Qualifier::from([name]))
}

/// An occurrence relation from `(owner, mentioned, polarity)` triples.
fn relation(edges: &[(&str, &str, Polarity)]) -> Occurrences {
    let mut relation: Occurrences = BTreeMap::new();
    for (owner, mentioned, polarity) in edges {
        relation
            .entry(declaration(owner))
            .or_default()
            .insert(declaration(mentioned), *polarity);
    }
    relation
}

fn diagonal(closed: &BTreeMap<Global, BTreeMap<Global, Polarity>>, name: &str) -> Option<Polarity> {
    closed
        .get(&declaration(name))
        .and_then(|reached| reached.get(&declaration(name)))
        .copied()
}

// `/std/Toml`, whose recursion travels `Toml → Map → Node → Toml` with every step strict. Three hops means the direct relation says nothing about the diagonal at all — the acceptance test reads a fact only the closure knows.
#[test]
fn a_strict_cycle_closes_to_a_strict_diagonal() {
    let closed = close(&relation(&[
        ("Toml", "Map", Strict),
        ("Map", "Node", Strict),
        ("Node", "Toml", Strict),
    ]));

    assert_eq!(diagonal(&closed, "Toml"), Some(Strict));
    assert_eq!(diagonal(&closed, "Map"), Some(Strict));
    assert_eq!(diagonal(&closed, "Node"), Some(Strict));
}

// One negative step anywhere in a cycle condemns every member of it, which is what makes the check independent of which declaration the user happened to write first.
//
// The diagonal lands at `Mixed` rather than `Neg` because the closure joins over paths of *every* length: once a cycle exists it can be traversed any number of times, and a lap that is negative once is positive twice. That is the honest answer, and acceptance does not care — neither grade is accepting.
#[test]
fn one_negative_step_condemns_the_whole_cycle() {
    let closed = close(&relation(&[
        ("Left", "Right", Strict),
        ("Right", "Left", Neg),
    ]));

    assert_eq!(diagonal(&closed, "Left"), Some(Mixed));
    assert_eq!(diagonal(&closed, "Right"), Some(Mixed));
    assert!(!diagonal(&closed, "Left").unwrap().accepting());
}

// A strict cycle, by contrast, is stable under any number of laps: `Strict` is idempotent under composition, so traversing it repeatedly cannot manufacture the ambiguity above and a sound declaration is never rejected for looping.
#[test]
fn a_strict_cycle_does_not_drift_under_repeated_laps() {
    let closed = close(&relation(&[
        ("Ping", "Pong", Strict),
        ("Pong", "Ping", Strict),
    ]));

    assert_eq!(diagonal(&closed, "Ping"), Some(Strict));
    assert!(diagonal(&closed, "Ping").unwrap().accepting());
}

// Two negative steps compose back to positive — and positive is still not accepting. This is `Bad2` spread across two declarations: the sign is right but the occurrence crossed arrows, so it is not strictly positive.
#[test]
fn two_negative_steps_compose_to_a_positive_but_unaccepting_diagonal() {
    let closed = close(&relation(&[("Up", "Down", Neg), ("Down", "Up", Neg)]));

    assert_eq!(diagonal(&closed, "Up"), Some(Pos));
    assert!(!diagonal(&closed, "Up").unwrap().accepting());
}

// A declaration that reaches a cycle without being part of it is not itself rejected. Positivity is a property of the path *back*, not of having mentioned something suspicious.
#[test]
fn reaching_a_bad_cycle_without_joining_it_is_not_a_rejection() {
    let closed = close(&relation(&[
        ("Outer", "Left", Strict),
        ("Left", "Right", Strict),
        ("Right", "Left", Neg),
    ]));

    assert_eq!(diagonal(&closed, "Outer"), None);
    assert!(!diagonal(&closed, "Left").unwrap().accepting());
}

// Prelude declarations are sinks of the relation: they cannot mention user code, so a user declaration that travels through one has no way back and the closure terminates without inventing an edge.
#[test]
fn a_sink_contributes_no_path_back() {
    let closed = close(&relation(&[("User", "PreludeList", Strict)]));

    assert_eq!(diagonal(&closed, "User"), None);
    assert_eq!(diagonal(&closed, "PreludeList"), None);
}

#[test]
fn join_is_a_semilattice() {
    for p in EVERY {
        assert_eq!(p.join(p), p, "{p:?} is not idempotent under join");
        assert_eq!(p.join(Unused), p, "{p:?} lost its identity");
        assert_eq!(p.join(Mixed), Mixed, "{p:?} escaped the top");
        for q in EVERY {
            assert_eq!(
                p.join(q),
                q.join(p),
                "join is not commutative at {p:?}/{q:?}"
            );
            for r in EVERY {
                assert_eq!(
                    p.join(q).join(r),
                    p.join(q.join(r)),
                    "join is not associative at {p:?}/{q:?}/{r:?}",
                );
            }
        }
    }
}

// The two incomparable branches. A parameter used both strictly and negatively is not "negative" — it is beyond the analysis's ability to describe, and must land at the top so composition through it stays conservative.
#[test]
fn join_sends_incomparable_pairs_to_the_top() {
    assert_eq!(Strict.join(Pos), Pos);
    assert_eq!(Strict.join(Neg), Mixed);
    assert_eq!(Pos.join(Neg), Mixed);
}

#[test]
fn compose_is_sign_multiplication() {
    assert_eq!(Neg.compose(Neg), Pos);
    assert_eq!(Pos.compose(Neg), Neg);
    assert_eq!(Neg.compose(Pos), Neg);
    assert_eq!(Pos.compose(Pos), Pos);
}

#[test]
fn compose_annihilates_at_unused_and_is_identity_at_strict() {
    for p in EVERY {
        assert_eq!(p.compose(Unused), Unused, "{p:?} survived an unused former");
        assert_eq!(Unused.compose(p), Unused, "{p:?} survived an unused former");
        assert_eq!(p.compose(Strict), p, "{p:?} was not fixed by strict");
        assert_eq!(Strict.compose(p), p, "{p:?} was not fixed by strict");
    }
}

// `Mixed` absorbs every argument it is actually applied to, but must not resurrect an `Unused` one: a former that ignores its parameter ignores it however confused the analysis is about the rest of the type.
#[test]
fn compose_absorbs_into_mixed_except_at_unused() {
    for p in EVERY {
        let expected = match p {
            Unused => Unused,
            _ => Mixed,
        };
        assert_eq!(p.compose(Mixed), expected, "{p:?} composed with mixed");
        assert_eq!(Mixed.compose(p), expected, "{p:?} composed with mixed");
    }
}

#[test]
fn compose_is_commutative_and_associative() {
    for p in EVERY {
        for q in EVERY {
            assert_eq!(
                p.compose(q),
                q.compose(p),
                "compose is not commutative at {p:?}/{q:?}",
            );
            for r in EVERY {
                assert_eq!(
                    p.compose(q).compose(r),
                    p.compose(q.compose(r)),
                    "compose is not associative at {p:?}/{q:?}/{r:?}",
                );
            }
        }
    }
}

// Crossing an arrow is what costs a strict occurrence its strictness, and there is no way back: `flip` is an involution on `Pos`/`Neg`, so a strict occurrence that crosses two arrows is positive, never strict again. This is the whole reason `Bad2` — positive but not strictly positive — is rejected.
#[test]
fn flip_loses_strictness_permanently() {
    assert_eq!(Strict.flip(), Neg);
    assert_eq!(Strict.flip().flip(), Pos);
    assert_eq!(Pos.flip(), Neg);
    assert_eq!(Neg.flip(), Pos);
    assert_eq!(Unused.flip(), Unused);
    assert_eq!(Mixed.flip(), Mixed);
}

#[test]
fn flip_is_an_involution_above_strict() {
    for p in [Unused, Pos, Neg, Mixed] {
        assert_eq!(p.flip().flip(), p, "{p:?} did not survive two arrows");
    }
}

#[test]
fn only_unused_and_strict_are_accepting() {
    for p in EVERY {
        assert_eq!(p.accepting(), matches!(p, Unused | Strict), "{p:?}");
    }
}

// Acceptance is downward-closed in each branch, which is what makes joining occurrences sound: combining two admissible occurrences can only produce another admissible one when both were strict or unused.
#[test]
fn accepting_polarities_are_closed_under_join() {
    for p in EVERY {
        for q in EVERY {
            if p.accepting() && q.accepting() {
                assert!(p.join(q).accepting(), "{p:?} ⊔ {q:?} escaped acceptance");
            }
        }
    }
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
        positivity_vectors(&mut kernel, &inducts, &BTreeMap::new(), Coverage::Complete).is_err(),
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
        positivity_vectors(&mut kernel, &inducts, &BTreeMap::new(), Coverage::Complete).is_err(),
        "an out-of-set name must read `Mixed` under complete coverage, not the registry's vector",
    );
    assert!(
        positivity_vectors(&mut kernel, &inducts, &BTreeMap::new(), Coverage::Partial).is_ok(),
        "under partial coverage the registry vector is this pass's own earlier result",
    );
}
