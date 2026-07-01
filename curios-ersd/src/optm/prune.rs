use crate::{Item, Module, optm::call_graph::CallGraph};

/// Drop top-level items that neither contribute to the entrypoint nor perform an
/// effect.
///
/// After `erase` the module still carries the *whole* prelude — there is no
/// source-level prune, so `sys`/`syn`/`std` are elaborated and type-checked in
/// full on every compile. But `to_cont` lowers *every* item into `main`'s entry
/// region, eagerly *computing* each non-synchronous value `let` there — a
/// `Parse`/`Json`/`Http` combinator like `Json/decode` builds a web of closures
/// the optimizer then drags through lifting, specialization, and inlining, even
/// for a program that never names it.
///
/// Erasure has already type-checked everything, so it is sound to keep only what
/// the entrypoint *needs*. An item is needed when it is either:
///
/// - **reachable** — named, transitively, from the entrypoint body; or
/// - **effectful** — a *non-synchronous* item whose eager evaluation performs an
///   observable action (a host/cell op, or a call to something that does). The
///   top-level init runs these for effect even when their result is unused, e.g.
///   `let _ : False = Proc/exit(7);`. Synchronous items (closures, names, atoms)
///   allocate without acting, so they are kept only when reached.
///
/// This is the reachability/effect information that is plain at this layer and
/// lost once `to_cont` turns each item into anonymous CPS blocks — which is why
/// the prune lives here rather than in `cont::optm`, where the dead initialization is
/// indistinguishable from a live, possibly-effectful call sequence. Items keep
/// their original relative order, so `to_cont`'s dependency ordering (a
/// definition precedes its uses) is preserved.
pub fn prune_unreachable(module: &mut Module) {
    let count = module.items.len();

    // The reference graph + transitive effect taint. `tainted[i]` means evaluating
    // item `i` could perform an effect — directly, or via a reference to something
    // that does (calling/forcing which runs one).
    let graph = CallGraph::build(module);

    // Whether each item is synchronous: bound without evaluating an effect (a
    // closure, name, or atom). Non-synchronous tainted items are run for effect by
    // the eager top-level init even when their result is unused.
    let synchronous = module
        .items
        .iter()
        .map(Item::is_synchronous)
        .collect::<Vec<bool>>();

    // Keep what the entrypoint reaches, plus every item the eager top-level init
    // runs for effect (a non-synchronous tainted item) — and, transitively,
    // everything those reach, so no kept body references a dropped definition.
    let mut reachable = vec![false; count];
    let mut work = graph
        .references(&module.body.free_names())
        .into_iter()
        .chain((0..count).filter(|&i| !synchronous[i] && graph.is_tainted(i)))
        .collect::<Vec<usize>>();
    while let Some(i) = work.pop() {
        if reachable[i] {
            continue;
        }
        reachable[i] = true;
        work.extend(graph.refs_of(i).iter().copied());
    }

    let mut keep = reachable.into_iter();
    module.items.retain(|_| keep.next().unwrap());
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::{HostPrim, Name, Prim, Subterm},
    };

    fn names(module: &Module) -> Vec<String> {
        module
            .items
            .iter()
            .flat_map(|item| item.names().map(str::to_owned).collect::<Vec<_>>())
            .collect()
    }

    fn refers_to(name: &str, refers: &str) -> Item {
        // A non-trivial (non-synchronous) body that names `refers`, so reachability
        // — not the synchronous short-circuit — is what carries `refers`.
        Item::Let {
            name: name.to_owned(),
            body: Subterm::Apply(crate::Apply {
                head: Subterm::Name(Name::from(refers)).into(),
                params: vec![Subterm::Erased.into()],
            })
            .into(),
        }
    }

    fn leaf(name: &str) -> Item {
        Item::Let {
            name: name.to_owned(),
            body: Subterm::Erased.into(),
        }
    }

    #[test]
    fn keeps_the_reachable_closure_and_drops_the_rest() {
        // body → a → b (leaf); `c` is reachable from nothing and effect-free.
        let mut module = Module {
            items: vec![refers_to("a", "b"), leaf("b"), leaf("c")],
            body: Subterm::Name(Name::from("a")).into(),
        };

        prune_unreachable(&mut module);

        assert_eq!(names(&module), vec!["a", "b"]);
    }

    #[test]
    fn keeps_a_rec_group_as_a_unit_when_one_member_is_reached() {
        // body names `p`; `p` and `q` are one `rec` group, so both survive even
        // though `q` is never named directly. The unrelated `dead` item goes.
        let mut module = Module {
            items: vec![
                Item::Rec {
                    names: vec!["p".to_owned(), "q".to_owned()],
                    items: vec![
                        Subterm::Name(Name::from("q")).into(),
                        Subterm::Erased.into(),
                    ],
                },
                leaf("dead"),
            ],
            body: Subterm::Name(Name::from("p")).into(),
        };

        prune_unreachable(&mut module);

        assert_eq!(names(&module), vec!["p", "q"]);
    }

    #[test]
    fn keeps_an_unreferenced_effectful_item() {
        // `eff = Io.exit(_)`: its result is never named, but the eager top-level
        // init runs it for effect, so it must survive. The pure unused item goes.
        let mut module = Module {
            items: vec![
                Item::Let {
                    name: "eff".to_owned(),
                    body: Subterm::Prim(Prim::Host(HostPrim::IoExit(Subterm::Erased.into())))
                        .into(),
                },
                leaf("pure_unused"),
            ],
            body: Subterm::Erased.into(),
        };

        prune_unreachable(&mut module);

        assert_eq!(names(&module), vec!["eff"]);
    }

    #[test]
    fn keeps_an_item_that_only_transitively_performs_an_effect() {
        // `caller = exit(_)` names `exit`, whose body holds the host op. `caller`
        // contains no effect *directly*, but evaluating it calls one — taint
        // propagates along the reference, so the unreferenced `caller` is kept.
        let mut module = Module {
            items: vec![
                Item::Let {
                    name: "exit".to_owned(),
                    body: Subterm::Func(crate::Func {
                        captures: vec![],
                        params: vec!["c".into()],
                        body: Subterm::Prim(Prim::Host(HostPrim::IoExit(
                            Subterm::Name(Name::from("c")).into(),
                        )))
                        .into(),
                    })
                    .into(),
                },
                refers_to("caller", "exit"),
            ],
            body: Subterm::Erased.into(),
        };

        prune_unreachable(&mut module);

        // `exit` is synchronous (a closure) so it is not kept for effect on its
        // own, but `caller` (non-synchronous, tainted) is — and pulls `exit` in.
        assert_eq!(names(&module), vec!["exit", "caller"]);
    }
}
