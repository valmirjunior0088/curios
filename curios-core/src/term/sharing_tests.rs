//! Memoized rewrites keep sharing, and a deep term compares, releases and captures without native recursion.

use {crate::*, std::rc::Rc};

use super::test_support::*;

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

/// A rewrite that rebuilds a shared node once per *occurrence* rather than once per node turns a DAG into its expansion. This is the shape that made it matter: a string literal lowers to a chain threading a scan state, where every link mentions the previous state, so the term is linear in distinct nodes but triangular expanded. Losing the memo here cost O(n^2) nodes for an n-byte literal, and every later pass over the term inherited it.
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
    // Equality used to recurse once per link, so a term this tall answered by aborting the process. Two independently built spines are structurally equal but share no node, which is exactly the case that has to walk.
    assert_eq!(deep_spine(0), deep_spine(0));
    assert_ne!(deep_spine(0), deep_spine(1));
}

#[test]
fn deep_terms_are_released_without_native_recursion() {
    // The other half: releasing a spine this tall used to recurse once per link through the derived drop of the `Rc` chain. Every term built here goes out of scope at the end of the test, which is the whole point.
    let shared = deep_spine(0);
    let sharing = Term::tuple([shared.clone(), shared.clone()]);

    assert_eq!(sharing, Term::tuple([shared.clone(), shared]));
}

#[test]
fn deep_terms_are_captured_without_native_recursion() {
    // `capture` runs in `Plain` mode, which the iterative spine path is not gated for, so every link here is one native descent — the case the two fixtures above never reach, since equality walks a worklist and the drop is iterative. A ten-definition numeric web died here as a bare `SIGBUS` under the kernel's conversion history, which captures a whole normal form to key a goal: the walk began inside the `grown` segment and, with no check per level, ran it to the guard page. The traversal now re-enters `recurse` at every level, so it maps another segment instead.
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
