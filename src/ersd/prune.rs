use {
    super::{Item, Module, NatMatch, Subterm, Term},
    std::collections::{BTreeSet, HashMap, HashSet},
};

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
/// the prune lives here rather than in `optm`, where the dead initialization is
/// indistinguishable from a live, possibly-effectful call sequence. Items keep
/// their original relative order, so `to_cont`'s dependency ordering (a
/// definition precedes its uses) is preserved.
pub fn prune_unreachable(module: &mut Module) {
    let count = module.items.len();

    // Every declared name to the item that owns it (a `rec` group's members all
    // map to the one group node).
    let owner = module
        .items
        .iter()
        .enumerate()
        .flat_map(|(index, item)| item_names(item).map(move |name| (name.to_owned(), index)))
        .collect::<HashMap<String, usize>>();

    let edge = |name: &str| owner.get(name).copied();
    let references = |names: BTreeSet<String>| {
        names.iter().filter_map(|name| edge(name)).collect::<HashSet<usize>>()
    };

    // The items each item references, and whether it is synchronous / directly
    // effectful (its body contains a host or cell op anywhere — even under a
    // lambda, since calling that lambda would run it).
    let refs = module
        .items
        .iter()
        .map(|item| references(item_free_names(item)))
        .collect::<Vec<HashSet<usize>>>();
    let synchronous = module.items.iter().map(item_is_synchronous).collect::<Vec<bool>>();

    // `tainted[i]`: evaluating item `i` could perform an effect — it contains one
    // directly, or it references a tainted item (calling/forcing which runs one).
    // Seed from the directly-effectful items and propagate along reverse edges.
    let mut referrers = vec![Vec::<usize>::new(); count];
    for (referrer, set) in refs.iter().enumerate() {
        for &referee in set {
            referrers[referee].push(referrer);
        }
    }
    let mut tainted = vec![false; count];
    let mut work = (0..count)
        .filter(|&i| item_contains_effect(&module.items[i]))
        .collect::<Vec<usize>>();
    while let Some(i) = work.pop() {
        if tainted[i] {
            continue;
        }
        tainted[i] = true;
        work.extend(referrers[i].iter().copied());
    }

    // Keep what the entrypoint reaches, plus every item the eager top-level init
    // runs for effect (a non-synchronous tainted item) — and, transitively,
    // everything those reach, so no kept body references a dropped definition.
    let mut reachable = vec![false; count];
    let mut work = references(module.body.free_names())
        .into_iter()
        .chain((0..count).filter(|&i| !synchronous[i] && tainted[i]))
        .collect::<Vec<usize>>();
    while let Some(i) = work.pop() {
        if reachable[i] {
            continue;
        }
        reachable[i] = true;
        work.extend(refs[i].iter().copied());
    }

    let mut keep = reachable.into_iter();
    module.items.retain(|_| keep.next().unwrap());
}

/// The names an item declares.
fn item_names(item: &Item) -> impl Iterator<Item = &str> {
    match item {
        Item::Let { name, .. } => std::slice::from_ref(name),
        Item::Rec { names, .. } => names.as_slice(),
    }
    .iter()
    .map(String::as_str)
}

/// The free names an item's body (or, for a `rec` group, every member) references.
fn item_free_names(item: &Item) -> BTreeSet<String> {
    match item {
        Item::Let { body, .. } => body.free_names(),
        Item::Rec { items, .. } => items.iter().flat_map(Term::free_names).collect(),
    }
}

/// Whether binding this item performs no action — it is a closure, name, or atom,
/// allocated without evaluating an effect. Mirrors `to_cont`'s `is_synchronous`;
/// a `rec` group is synchronous only if every member is.
fn item_is_synchronous(item: &Item) -> bool {
    match item {
        Item::Let { body, .. } => is_synchronous(body),
        Item::Rec { items, .. } => items.iter().all(is_synchronous),
    }
}

/// Whether evaluating this item could perform an effect.
fn item_contains_effect(item: &Item) -> bool {
    match item {
        Item::Let { body, .. } => contains_effect(body),
        Item::Rec { items, .. } => items.iter().any(contains_effect),
    }
}

fn is_synchronous(term: &Term) -> bool {
    matches!(
        term.as_subterm(),
        Subterm::Func(_)
            | Subterm::Erased
            | Subterm::Unreachable
            | Subterm::Atom(_)
            | Subterm::Name(_)
    )
}

/// Whether `term` contains an effectful primitive anywhere — including under a
/// lambda, since a closure that performs an effect performs it when *called*, and
/// the caller's evaluation is what we are classifying.
fn contains_effect(term: &Term) -> bool {
    match term.as_subterm() {
        Subterm::Prim(prim) => prim.is_effectful() || prim.operands().iter().any(|t| contains_effect(t)),
        Subterm::Func(func) => contains_effect(&func.body),
        Subterm::Apply(apply) => {
            contains_effect(&apply.head) || apply.params.iter().any(contains_effect)
        }
        Subterm::Tuple(tuple) => tuple.fields.iter().any(contains_effect),
        Subterm::Proj(proj) => contains_effect(&proj.head),
        Subterm::Match(m) => contains_effect(&m.head) || m.cases.iter().any(contains_effect),
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            succ_case,
            ..
        }) => {
            contains_effect(head) || contains_effect(zero_case) || contains_effect(succ_case)
        }
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => {
            contains_effect(head)
                || cases.iter().any(|(_, case)| contains_effect(case))
                || contains_effect(default)
        }
        Subterm::Let(let_) => contains_effect(&let_.body) || contains_effect(&let_.tail),
        Subterm::Rec(rec) => rec.items.iter().any(contains_effect) || contains_effect(&rec.tail),
        Subterm::Name(_) | Subterm::Atom(_) | Subterm::Erased | Subterm::Unreachable => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        super::{HostPrim, Name, Prim, Subterm},
        *,
    };

    fn names(module: &Module) -> Vec<String> {
        module
            .items
            .iter()
            .flat_map(|item| item_names(item).map(str::to_owned).collect::<Vec<_>>())
            .collect()
    }

    fn refers_to(name: &str, refers: &str) -> Item {
        // A non-trivial (non-synchronous) body that names `refers`, so reachability
        // — not the synchronous short-circuit — is what carries `refers`.
        Item::Let {
            name: name.to_owned(),
            body: Subterm::Apply(super::super::Apply {
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
                    body: Subterm::Prim(Prim::Host(HostPrim::IoExit(Subterm::Erased.into()))).into(),
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
                    body: Subterm::Func(super::super::Func {
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
