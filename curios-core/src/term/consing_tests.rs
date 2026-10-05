//! A hash-consed term: one node for each structure as it is spelled, built over canonical children, under no position.

use {
    super::test_support::*,
    crate::*,
    curios_utilities::{Source, Span},
    std::{rc::Rc, sync::Arc},
};

/// `f(x)` over two locals, built anew at every call: equal as a term to every other, and no node of any.
fn applied() -> Term {
    Term::apply(
        Term::free_var(&Free::local(0, Some("f"))),
        [Term::free_var(&Free::local(1, Some("x")))],
    )
}

/// The one argument of the application `term` is.
fn argument(term: &Term) -> &Term {
    let Subterm::Apply(apply) = term.as_ref() else {
        panic!("consing changed the term's shape: {term}");
    };
    apply
        .params()
        .next()
        .expect("an application of one argument")
}

/// A function type over two `Nat` binders hinted `first` and `second`, answering `Nat`.
fn binary(first: &str, second: &str) -> Term {
    let nat = || Term::intrinsic(Intrinsic::NatType);
    Term::func_type(
        [
            (Free::local(0, Some(first)), nat()),
            (Free::local(1, Some(second)), nat()),
        ],
        nat(),
    )
}

fn hints(term: &Term) -> Vec<String> {
    let Subterm::FuncType(function) = term.as_ref() else {
        panic!("consing changed the term's shape: {term}");
    };
    function
        .telescope
        .labels()
        .into_iter()
        .map(str::to_string)
        .collect()
}

/// A structure met under two parents is one node: the second parent is built over the node the first adopted, and is not handed back holding the copy it came with.
///
/// Mutation-checked: answered with the node it had whenever its rebuilt payload equals its own, as every other rebuild is, a parent met for the first time keeps every duplicate beneath it.
#[test]
fn a_structure_under_two_parents_is_one_node() {
    let g = Term::free_var(&Free::local(2, Some("g")));
    let h = Term::free_var(&Free::local(3, Some("h")));
    let sharing = Sharing::new();

    let under_g = sharing.share(&Term::apply(g, [applied()]));
    let under_h = sharing.share(&Term::apply(h, [applied()]));

    assert!(
        Rc::ptr_eq(&argument(&under_g).inner, &argument(&under_h).inner),
        "two parents hold two nodes of one structure"
    );
}

/// Two structures equal up to their binders' names are two nodes, each under the names it was written with, and a structure spelled as one already met is that one's node.
///
/// Mutation-checked: looked up by the term alone, whose equality reads no label, the second is handed the first's node and its names with it.
#[test]
fn a_structure_keeps_the_binder_names_it_was_written_with() {
    let sharing = Sharing::new();

    let min = sharing.share(&binary("a", "b"));
    let pow = sharing.share(&binary("base", "exp"));
    let max = sharing.share(&binary("a", "b"));

    assert_eq!(min, pow, "the two are one type");
    assert_eq!(hints(&min), ["a", "b"]);
    assert_eq!(hints(&pow), ["base", "exp"]);
    assert!(!Rc::ptr_eq(&min.inner, &pow.inner));
    assert!(Rc::ptr_eq(&min.inner, &max.inner));
}

/// A consed term sits under no position and holds none beneath it, so two occurrences that differ only in where they were written are one node.
#[test]
fn a_consed_term_sits_under_no_position() {
    let source = Source::inline("f x f x");
    let span = |start| Some(Span::new(Arc::clone(&source), start, start + 1));
    let written_at = |start: usize| {
        Term::apply(
            Term::free_var(&Free::local(0, Some("f"))).respanned(span(start)),
            [Term::free_var(&Free::local(1, Some("x"))).respanned(span(start + 2))],
        )
        .respanned(span(start))
    };
    let sharing = Sharing::new();

    let first = sharing.share(&written_at(0));
    let second = sharing.share(&written_at(4));

    assert_eq!(first.span(), None);
    assert_eq!(argument(&first).span(), None);
    assert!(Rc::ptr_eq(&first.inner, &second.inner));
}

/// A node met first with no span and then under one comes back without one both times: a remembered rebuild is handed to an occurrence as it was stored, not as that occurrence's own.
#[test]
fn a_node_met_first_without_a_span_comes_back_without_one() {
    let source = Source::inline("f x");
    let shared = applied();
    let term = Term::tuple([
        shared.clone(),
        shared.respanned(Some(Span::new(source, 0, 1))),
    ]);

    let consed = Sharing::new().share(&term);

    let Subterm::Tuple(tuple) = consed.as_ref() else {
        panic!("consing changed the term's shape: {consed}");
    };
    assert_eq!(tuple.fields[0].span(), None);
    assert_eq!(tuple.fields[1].span(), None);
    assert!(Rc::ptr_eq(&tuple.fields[0].inner, &tuple.fields[1].inner));
}

/// A graph is consed in its own size and stays one: a chain that threads a state through every link, linear in nodes and triangular expanded.
#[test]
fn a_shared_chain_is_consed_as_a_chain() {
    let depth = 200;
    let mut state = Term::free_var(&Free::local(0, Some("lead")));
    let mut chain = Term::free_var(&Free::local(1, Some("stop")));
    for _ in 0..depth {
        chain = Term::tuple([state.clone(), chain]);
        state = Term::apply(Term::free_var(&Free::local(2, Some("step"))), [state]);
    }

    let consed = Sharing::new().share(&chain);

    assert_eq!(consed, chain);
    assert!(
        distinct_nodes(&consed) <= distinct_nodes(&chain),
        "consing expanded the shared chain"
    );
}

/// A consed declaration's universe context says nowhere where a constraint was raised, and is the context it was: its constraints, their kinds, and the declaration and binder each names.
#[test]
fn a_universe_context_is_consed_without_its_positions() {
    let raised = UniverseConstraintOrigin {
        span: Some(Span::new(Source::inline("Type"), 0, 4)),
        kind: UniverseConstraintKind::WrittenType,
        declaration: Some("/lib/wrap".into()),
        binder: Some("A".into()),
    };
    let context = UniverseContext {
        parameter_count: 1,
        constraints: vec![UniverseConstraint {
            lower: Level::zero(),
            upper: Level::param(UniverseParam(0)),
            origin: raised.clone(),
        }],
    };

    let consed = context.unplaced();

    assert_eq!(consed, context, "the constraints are the ones it had");
    let [constraint] = consed.constraints.as_slice() else {
        panic!("one constraint, got {:?}", consed.constraints);
    };
    assert_eq!(constraint.origin.span, None);
    assert_eq!(constraint.origin.kind, raised.kind);
    assert_eq!(constraint.origin.declaration, raised.declaration);
    assert_eq!(constraint.origin.binder, raised.binder);
}

/// A spine taller than a native stack is consed, and two built apart come out the same node.
#[test]
fn a_deep_spine_is_consed_without_native_recursion() {
    let sharing = Sharing::new();

    let first = sharing.share(&deep_spine(0));
    let second = sharing.share(&deep_spine(0));

    assert!(Rc::ptr_eq(&first.inner, &second.inner));
}
