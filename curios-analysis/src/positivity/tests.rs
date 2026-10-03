//! The polarity lattice's own laws, and the closure over the occurrence relation.
//!
//! Pure functions of the relation, so they need no checker and stay beside the analysis. The tests that drive `positivity_vectors` through a real `Env` are in `curios-analysis/tests/driven.rs`; see that file's header for why.

use {
    super::{Occurrences, close},
    curios_core::{Global, Polarity},
    curios_utilities::Qualifier,
    std::collections::BTreeMap,
};

const EVERY: [Polarity; 5] = [
    Polarity::Unused,
    Polarity::Strict,
    Polarity::Pos,
    Polarity::Neg,
    Polarity::Mixed,
];
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
        ("Toml", "Map", Polarity::Strict),
        ("Map", "Node", Polarity::Strict),
        ("Node", "Toml", Polarity::Strict),
    ]));

    assert_eq!(diagonal(&closed, "Toml"), Some(Polarity::Strict));
    assert_eq!(diagonal(&closed, "Map"), Some(Polarity::Strict));
    assert_eq!(diagonal(&closed, "Node"), Some(Polarity::Strict));
}

// One negative step anywhere in a cycle condemns every member of it, which is what makes the check independent of which declaration the user happened to write first.
//
// The diagonal lands at `Mixed` rather than `Neg` because the closure joins over paths of *every* length: once a cycle exists it can be traversed any number of times, and a lap that is negative once is positive twice. That is the honest answer, and acceptance does not care — neither grade is accepting.
#[test]
fn one_negative_step_condemns_the_whole_cycle() {
    let closed = close(&relation(&[
        ("Left", "Right", Polarity::Strict),
        ("Right", "Left", Polarity::Neg),
    ]));

    assert_eq!(diagonal(&closed, "Left"), Some(Polarity::Mixed));
    assert_eq!(diagonal(&closed, "Right"), Some(Polarity::Mixed));
    assert!(!diagonal(&closed, "Left").unwrap().accepting());
}

// A strict cycle, by contrast, is stable under any number of laps: `Strict` is idempotent under composition, so traversing it repeatedly cannot manufacture the ambiguity above and a sound declaration is never rejected for looping.
#[test]
fn a_strict_cycle_does_not_drift_under_repeated_laps() {
    let closed = close(&relation(&[
        ("Ping", "Pong", Polarity::Strict),
        ("Pong", "Ping", Polarity::Strict),
    ]));

    assert_eq!(diagonal(&closed, "Ping"), Some(Polarity::Strict));
    assert!(diagonal(&closed, "Ping").unwrap().accepting());
}

// Two negative steps compose back to positive — and positive is still not accepting. This is `Bad2` spread across two declarations: the sign is right but the occurrence crossed arrows, so it is not strictly positive.
#[test]
fn two_negative_steps_compose_to_a_positive_but_unaccepting_diagonal() {
    let closed = close(&relation(&[
        ("Up", "Down", Polarity::Neg),
        ("Down", "Up", Polarity::Neg),
    ]));

    assert_eq!(diagonal(&closed, "Up"), Some(Polarity::Pos));
    assert!(!diagonal(&closed, "Up").unwrap().accepting());
}

// A declaration that reaches a cycle without being part of it is not itself rejected. Positivity is a property of the path *back*, not of having mentioned something suspicious.
#[test]
fn reaching_a_bad_cycle_without_joining_it_is_not_a_rejection() {
    let closed = close(&relation(&[
        ("Outer", "Left", Polarity::Strict),
        ("Left", "Right", Polarity::Strict),
        ("Right", "Left", Polarity::Neg),
    ]));

    assert_eq!(diagonal(&closed, "Outer"), None);
    assert!(!diagonal(&closed, "Left").unwrap().accepting());
}

// A predecessor unit's declarations are sinks of the relation: they cannot mention a successor's, so a declaration that travels through one has no way back and the closure terminates without inventing an edge.
#[test]
fn a_sink_contributes_no_path_back() {
    let closed = close(&relation(&[("User", "PreludeList", Polarity::Strict)]));

    assert_eq!(diagonal(&closed, "User"), None);
    assert_eq!(diagonal(&closed, "PreludeList"), None);
}

#[test]
fn join_is_a_semilattice() {
    for p in EVERY {
        assert_eq!(p.join(p), p, "{p:?} is not idempotent under join");
        assert_eq!(p.join(Polarity::Unused), p, "{p:?} lost its identity");
        assert_eq!(
            p.join(Polarity::Mixed),
            Polarity::Mixed,
            "{p:?} escaped the top"
        );
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
    assert_eq!(Polarity::Strict.join(Polarity::Pos), Polarity::Pos);
    assert_eq!(Polarity::Strict.join(Polarity::Neg), Polarity::Mixed);
    assert_eq!(Polarity::Pos.join(Polarity::Neg), Polarity::Mixed);
}

#[test]
fn compose_is_sign_multiplication() {
    assert_eq!(Polarity::Neg.compose(Polarity::Neg), Polarity::Pos);
    assert_eq!(Polarity::Pos.compose(Polarity::Neg), Polarity::Neg);
    assert_eq!(Polarity::Neg.compose(Polarity::Pos), Polarity::Neg);
    assert_eq!(Polarity::Pos.compose(Polarity::Pos), Polarity::Pos);
}

#[test]
fn compose_annihilates_at_unused_and_is_identity_at_strict() {
    for p in EVERY {
        assert_eq!(
            p.compose(Polarity::Unused),
            Polarity::Unused,
            "{p:?} survived an unused former"
        );
        assert_eq!(
            Polarity::Unused.compose(p),
            Polarity::Unused,
            "{p:?} survived an unused former"
        );
        assert_eq!(
            p.compose(Polarity::Strict),
            p,
            "{p:?} was not fixed by strict"
        );
        assert_eq!(
            Polarity::Strict.compose(p),
            p,
            "{p:?} was not fixed by strict"
        );
    }
}

// `Mixed` absorbs every argument it is actually applied to, but must not resurrect an `Unused` one: a former that ignores its parameter ignores it however confused the analysis is about the rest of the type.
#[test]
fn compose_absorbs_into_mixed_except_at_unused() {
    for p in EVERY {
        let expected = match p {
            Polarity::Unused => Polarity::Unused,
            _ => Polarity::Mixed,
        };
        assert_eq!(
            p.compose(Polarity::Mixed),
            expected,
            "{p:?} composed with mixed"
        );
        assert_eq!(
            Polarity::Mixed.compose(p),
            expected,
            "{p:?} composed with mixed"
        );
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
    assert_eq!(Polarity::Strict.flip(), Polarity::Neg);
    assert_eq!(Polarity::Strict.flip().flip(), Polarity::Pos);
    assert_eq!(Polarity::Pos.flip(), Polarity::Neg);
    assert_eq!(Polarity::Neg.flip(), Polarity::Pos);
    assert_eq!(Polarity::Unused.flip(), Polarity::Unused);
    assert_eq!(Polarity::Mixed.flip(), Polarity::Mixed);
}

#[test]
fn flip_is_an_involution_above_strict() {
    for p in [
        Polarity::Unused,
        Polarity::Pos,
        Polarity::Neg,
        Polarity::Mixed,
    ] {
        assert_eq!(p.flip().flip(), p, "{p:?} did not survive two arrows");
    }
}

#[test]
fn only_unused_and_strict_are_accepting() {
    for p in EVERY {
        assert_eq!(
            p.accepting(),
            matches!(p, Polarity::Unused | Polarity::Strict),
            "{p:?}"
        );
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
