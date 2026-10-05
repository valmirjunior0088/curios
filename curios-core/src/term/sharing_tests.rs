//! Memoized rewrites keep sharing, and a deep term compares, releases and captures without native recursion.

use {
    super::test_support::*,
    crate::*,
    curios_utilities::{Source, Span},
    std::{rc::Rc, sync::Arc},
};

#[test]
fn a_memoized_rewrite_keeps_a_shared_subterm_shared() {
    let f = Free::local(0, Some("f"));
    let x = Free::local(1, Some("x"));
    let shared = Term::apply(Term::free_var(&f), [Term::free_var(&x)]);
    let term = Term::tuple([shared.clone(), shared]);

    let rewritten: Term = term.traverse(&mut Visit::rewriting_shared(
        |_, _| None,
        Box::new(|_, _| None),
    ));

    let Subterm::Tuple(tuple) = rewritten.as_ref() else {
        panic!("the rewrite changed the term's shape");
    };
    assert!(
        Rc::ptr_eq(&tuple.fields[0].inner, &tuple.fields[1].inner),
        "a memoized rewrite split one shared subterm into two nodes"
    );
}

/// A memoized walk answers a shared node once, and each occurrence under its own span: one node under two spans comes back under the same two, through a capture and through a hook that spans its answer by the occurrence it replaced, while a hook's own replacement keeps the span it was given. Handed back as stored, a hit would give the second occurrence the first one's span, so a diagnostic there would point at the other.
#[test]
fn a_memoized_walk_keeps_each_occurrence_s_span() {
    let source = Source::inline("a b c");
    let span = |start| Some(Span::new(Arc::clone(&source), start, start + 1));
    let f = Free::local(0, Some("f"));
    let x = Free::local(1, Some("x"));
    let shared = Term::apply(Term::free_var(&f), [Term::free_var(&x)]);
    let term = Term::tuple([
        shared.clone().respanned(span(0)),
        shared.clone().respanned(span(2)),
    ]);
    let spans = |walked: &Term| {
        let Subterm::Tuple(tuple) = walked.as_ref() else {
            panic!("the walk changed the term's shape");
        };
        (tuple.fields[0].span(), tuple.fields[1].span())
    };

    assert_eq!(spans(&term.capture(&[&x])), (span(0), span(2)));

    let needle = shared.clone();
    let spanned_by_occurrence: Term = term.traverse(&mut Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, node: &Term| {
            (*node == needle).then(|| Term::free_var(&f).respanned(node.span()))
        }),
    ));
    assert_eq!(spans(&spanned_by_occurrence), (span(0), span(2)));

    let replacement = Term::free_var(&x).respanned(span(4));
    let spanned_by_hook: Term = term.traverse(&mut Visit::rewriting_shared(
        |_, _| None,
        Box::new(move |_, node: &Term| (*node == shared).then(|| replacement.clone())),
    ));
    assert_eq!(spans(&spanned_by_hook), (span(4), span(4)));
}

/// A rewrite that rebuilds a shared node once per *occurrence* rather than once per node turns a DAG into its expansion. This is the shape where it matters: a string literal lowers to a chain threading a scan state, where every link mentions the previous state, so the term is linear in distinct nodes but triangular expanded. Losing the memo here costs O(n^2) nodes for an n-byte literal, and every later pass over the term inherits it.
#[test]
fn a_memoized_rewrite_keeps_a_shared_chain_linear() {
    let lead = Free::local(0, Some("lead"));
    let stop = Free::local(1, Some("stop"));
    let step = Free::local(2, Some("step"));
    let depth = 200;
    let mut state = Term::free_var(&lead);
    let mut chain = Term::free_var(&stop);
    for _ in 0..depth {
        chain = Term::tuple([state.clone(), chain]);
        state = Term::apply(Term::free_var(&step), [state]);
    }

    assert!(
        distinct_nodes(&chain) < 4 * depth,
        "the fixture itself is not shared, so the test proves nothing"
    );

    let rewritten: Term = chain.traverse(&mut Visit::rewriting_shared(
        |_, _| None,
        Box::new(|_, _| None),
    ));

    assert_eq!(
        distinct_nodes(&rewritten),
        distinct_nodes(&chain),
        "a memoized rewrite expanded the shared chain"
    );
}

#[test]
fn deep_terms_compare_without_native_recursion() {
    // Equality recursing once per link would answer a term this tall by aborting the process. Two independently built spines are structurally equal but share no node, which is exactly the case that has to walk.
    assert_eq!(deep_spine(0), deep_spine(0));
    assert_ne!(deep_spine(0), deep_spine(1));
}

#[test]
fn deep_terms_are_released_without_native_recursion() {
    // The other half: releasing a spine this tall would recurse once per link through the derived drop of the `Rc` chain. Every term built here goes out of scope at the end of the test, which is the whole point.
    let shared = deep_spine(0);
    let sharing = Term::tuple([shared.clone(), shared.clone()]);

    assert_eq!(sharing, Term::tuple([shared.clone(), shared]));
}

#[test]
fn deep_terms_are_captured_without_native_recursion() {
    // `capture` runs in `Plain` mode, which the iterative spine path is not gated for, so every link here is one native descent — the case the two fixtures above never reach, since equality walks a worklist and the drop is iterative. Under the kernel's conversion history, which captures a whole normal form to key a goal, a walk beginning inside the `grown` segment with no check per level would run it to the guard page and die as a bare `SIGBUS`; the traversal re-enters `recurse` at every level, so it maps another segment instead.
    let name = Free::local(0, None);
    let argument = Term::free_var(&name);
    let mut term = Term::free_var(&name);
    for _ in 0..TALL {
        term = Term::apply(term, [argument.clone()]);
    }

    let captured = term.capture(&[&name]);

    assert!(
        !captured.free_vars().contains(&name),
        "every occurrence was bound"
    );
}

/// Two equal graphs built apart compare equal in the size of the graph, not of its tree. Each level here is the sum of the previous level with itself, so the tree doubles per level and twenty-two levels of it is four million pairs a path-by-path walk would have masked and compared one at a time; the walk remembers each pair of shared nodes it has entered, and answers in a few dozen.
#[test]
fn equal_graphs_compare_in_their_own_size() {
    let build = || {
        let mut level = Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)));
        for _ in 0..22 {
            level = Term::intrinsic(Intrinsic::nat_add(level.clone(), level));
        }
        level
    };

    assert_eq!(build(), build());
}

/// `base` under sixty levels that each sum the one below with itself: a tree past anything a walk per path finishes, and a graph of sixty-one nodes.
fn doubled(base: Term, depth: usize) -> Term {
    let mut term = base;
    for _ in 0..depth {
        term = Term::intrinsic(Intrinsic::nat_add(term.clone(), term));
    }
    term
}

fn root_operands_shared(term: &Term) -> bool {
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = term.as_ref() else {
        panic!("the walk changed the root: {term}");
    };
    Rc::ptr_eq(&left.inner, &right.inner)
}

/// A compound needle is sought once per node, since it has no cached bit to prune by: present at the base of a shared sum it is found, and absent the whole graph is searched in its own size.
#[test]
fn a_needle_is_sought_once_per_node() {
    let f = Free::local(0, Some("f"));
    let base = Term::apply(
        Term::free_var(&f),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)))],
    );
    let absent = Term::apply(
        Term::free_var(&f),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)))],
    );
    let term = doubled(base.clone(), 60);

    assert!(term.mentions_term(&base));
    assert!(!term.mentions_term(&absent));
}

/// A replacement is made once per node, and the graph it is made in stays a graph.
#[test]
fn a_replacement_is_made_once_per_node() {
    let f = Free::local(0, Some("f"));
    let base = Term::apply(
        Term::free_var(&f),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(1usize)))],
    );
    let replacement = Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)));

    let replaced = doubled(base.clone(), 60).replace_term(&base, &replacement);

    assert!(!replaced.mentions_free(&f));
    assert!(root_operands_shared(&replaced));
}

/// The shared level walk asks its hook once per node and depth: the one level at the base of a shared sum is asked once, and the graph stays a graph.
#[test]
fn a_shared_level_walk_asks_each_level_once_per_node() {
    let meta = UniverseMetaId(0);
    let term = doubled(Term::type_at(Level::meta(meta)), 60);
    let asked = Rc::new(std::cell::Cell::new(0));
    let counter = Rc::clone(&asked);

    let rewritten: Term = rewrite_universe_levels_scoped_shared(&term, move |_, level| {
        counter.set(counter.get() + 1);
        level.substitute(|head| match head {
            LevelHead::Meta(found) if found == meta => Some(Level::zero()),
            _ => None,
        })
    })
    .expect("substituting a ground level cannot overflow");

    assert_eq!(asked.get(), 1);
    assert!(rewritten.universe_metas().is_empty());
    assert!(root_operands_shared(&rewritten));
}

/// The per-occurrence level walk asks its hook at every occurrence, in walk order: the sequence two spellings are aligned by has one entry per occurrence, so a shared level at the base of three doublings is asked eight times.
#[test]
fn the_per_occurrence_level_walk_asks_every_occurrence() {
    let term = doubled(Term::type_at(Level::meta(UniverseMetaId(0))), 3);
    let asked = Rc::new(std::cell::Cell::new(0));
    let counter = Rc::clone(&asked);

    let _: Term = rewrite_universe_levels_scoped(&term, move |_, level| {
        counter.set(counter.get() + 1);
        Ok::<_, ()>(level.clone())
    })
    .expect("the identity cannot fail");

    assert_eq!(asked.get(), 8);
}

/// Every walk this crate owns, over one doubling term — sixty levels, a tree no walk per path finishes and a graph of sixty-one nodes — answers in the graph's size, and each walk that rebuilds hands back a graph. A walk the crate adds joins this table, so one a change makes per-path again stalls its row here rather than waiting for a profile to find it; the fixtures above hold each walk's own contract beside it. `shift`, `release` and the read of a binder's use run over the same sixty levels with a loose index at their base: `reach` prunes nothing of an open term, so only the node each remembers keeps them in the graph.
#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    let meta = UniverseMetaId(0);
    let base = Term::apply(Term::free_var(&x), [Term::type_at(Level::meta(meta))]);
    let term = doubled(base.clone(), 60);
    let open = doubled(
        Term::apply(Term::free_var(&x), [Term::var(Var::bound(0))]),
        60,
    );

    let rows: Vec<(&str, bool)> = vec![
        ("equality", term == doubled(base.clone(), 60)),
        (
            "hashing",
            term.structural_hash() == doubled(base.clone(), 60).structural_hash(),
        ),
        ("free variables", term.free_vars().contains(&x)),
        ("universe metavariables", term.universe_metas().len() == 1),
        ("a needle sought", term.mentions_term(&base)),
        (
            "a needle replaced",
            root_operands_shared(&term.replace_term(&base, &Term::free_var(&y))),
        ),
        (
            "universes erased",
            root_operands_shared(&project_erased_universes(&term)),
        ),
        ("capture", root_operands_shared(&term.capture(&[&x]))),
        ("shift", root_operands_shared(&open.shift(1))),
        (
            "release",
            root_operands_shared(&open.release(&[&Term::free_var(&y)])),
        ),
        (
            "a binder's use read",
            Scope::close(Two, &[&x, &y], term.clone()).uses(0)
                && !Scope::close(Two, &[&x, &y], term.clone()).uses(1),
        ),
        (
            "level differences",
            term.level_differences(
                &doubled(
                    Term::apply(Term::free_var(&x), [Term::type_at(Level::zero())]),
                    60,
                ),
                |_| false,
            )
            .is_some_and(|pairs| pairs.len() == 1),
        ),
        (
            "the shared level rewrite",
            root_operands_shared(
                &rewrite_universe_levels_scoped_shared(&term, move |_, level| {
                    level.substitute(|head| match head {
                        LevelHead::Meta(found) if found == meta => Some(Level::zero()),
                        _ => None,
                    })
                })
                .expect("substituting a ground level cannot overflow"),
            ),
        ),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term lost its graph");
    }
}

/// A capture of local binders hands back a subterm with no local free unwalked: nothing in it is a binder's occurrence, and nothing in it is a loose index to shift. Counted in looks, since a memoized walk of the same subterm would hand back the same node.
///
/// Mutation-checked: with the capture walking what it cannot change, the looks are those of the closed subterm's sixty levels.
#[cfg(feature = "profile")]
#[test]
fn a_capture_of_local_binders_passes_over_a_closed_subterm() {
    let x = Free::local(0, Some("x"));
    let closed = doubled(Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))), 60);
    let term = Term::tuple([closed, Term::free_var(&x)]);
    // Warmed, so the count below is the capture's alone: `reach` and the flags are filled once per node.
    let _ = term.reach();

    take_looks();
    let captured = term.capture(&[&x]);
    let looks = take_looks();

    assert!(!captured.mentions_free(&x));
    assert!(looks < 20, "the capture looked at {looks} nodes");
}

/// A scope is read for a binder's use in its size, and a closed subterm of its body is not read at all: the closed type a `let` states is where a tower's tree was.
///
/// Mutation-checked: with the read rebuilding what `reach` proves it cannot find the binder in, the looks are those of the closed subterm's sixty levels.
#[cfg(feature = "profile")]
#[test]
fn a_binders_use_is_read_without_entering_a_closed_subterm() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    let closed = doubled(Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))), 60);
    let scope = Scope::close(Two, &[&x, &y], Term::tuple([closed, Term::free_var(&x)]));
    let _ = scope.body().reach();

    take_looks();
    let used = (scope.uses(0), scope.uses(1));
    let looks = take_looks();

    assert_eq!(used, (true, false));
    assert!(looks < 20, "the read looked at {looks} nodes");
}
