use {
    crate::{Enter, Free, Subterm, Term},
    std::{collections::HashSet, ops::ControlFlow},
};

fn leaf(index: u32, hint: &str) -> (Free, Term) {
    let name = Free::local(index, Some(hint));
    let term = Term::free_var(&name);
    (name, term)
}

/// Collect every free identity bottom-up: exit order must present each node's children in child order.
#[test]
fn child_results_arrive_in_child_order() {
    let (f, head) = leaf(0, "f");
    let (a, first) = leaf(1, "a");
    let (b, second) = leaf(2, "b");
    let term = Term::apply(head, [first, second]);

    let names = term.walk(
        &mut (),
        |_, _| Enter::Descend,
        |_, term, children| {
            let mut names: Vec<Free> = children.flatten().collect();
            if let Subterm::Var(var) = &**term
                && let Some(name) = var.as_free()
            {
                names.push(name.clone());
            }
            names
        },
    );

    assert_eq!(names, vec![f, a, b]);
}

/// A `Skip` verdict settles the node without visiting anything below it.
#[test]
fn skip_prunes_the_subtree() {
    let (f, head) = leaf(0, "f");
    let (_, pruned) = leaf(1, "a");
    let (b, kept) = leaf(2, "b");
    let inner = Term::tuple([pruned]);
    let term = Term::apply(head, [inner.clone(), kept]);

    let names = term.walk(
        &mut (),
        |_, term| {
            if term == &inner {
                Enter::Skip(Vec::new())
            } else {
                Enter::Descend
            }
        },
        |_, term, children| {
            let mut names: Vec<Free> = children.flatten().collect();
            if let Subterm::Var(var) = &**term
                && let Some(name) = var.as_free()
            {
                names.push(name.clone());
            }
            names
        },
    );

    assert_eq!(names, vec![f, b]);
}

/// A `Break` verdict abandons the walk immediately.
#[test]
fn break_abandons_the_walk() {
    let (_, head) = leaf(0, "f");
    let (target, needle) = leaf(1, "needle");
    let term = Term::apply(head, [needle]);

    let walked = term.try_walk(
        &mut (),
        |_, term| {
            if let Subterm::Var(var) = &**term
                && var.as_free() == Some(&target)
            {
                return ControlFlow::Break(target.clone());
            }
            ControlFlow::Continue(Enter::Descend)
        },
        |_, _, _| (),
    );

    assert_eq!(walked, ControlFlow::Break(target));
}

/// A caller-owned memo in the shared state visits an `Rc`-shared node once.
// Safety: the memo is keyed on `Term`, whose interior scalar caches trip Clippy's interior-mutability warning; hashing and equality stay stable across those caches filling.
#[allow(clippy::mutable_key_type)]
#[test]
fn caller_owned_memo_visits_a_shared_node_once() {
    let (_, shared) = leaf(0, "shared");
    let term = Term::tuple([shared.clone(), shared]);

    let mut seen: HashSet<Term> = HashSet::new();
    let count = term.walk(
        &mut seen,
        |seen, term| {
            if seen.insert(term.clone()) {
                Enter::Descend
            } else {
                Enter::Skip(0)
            }
        },
        |_, _, children| 1 + children.sum::<usize>(),
    );

    assert_eq!(count, 2);
}
