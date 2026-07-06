use {
    crate::{Module, Subterm, Term},
    std::collections::{BTreeSet, HashMap, HashSet},
};

/// The top-level item reference graph, plus the transitive *effect taint* over it.
///
/// Every declared name maps to the item that owns it; each item carries the set of
/// items it references and a flag for whether evaluating it could perform an
/// effect. Taint is seeded from the directly-effectful items (a `Host`/`Cell` prim
/// anywhere in the body, even under a lambda — see [`Term::contains_effect`]) and
/// propagated along *reverse* edges: if `a` references `b` and `b` is tainted, then
/// calling or forcing `b` through `a` runs the effect, so `a` is tainted too.
///
/// This is the purity oracle shared between two `ersd/optm` clients:
///
/// - `prune` seeds reachability with the non-synchronous tainted
///   items (the eager top-level init runs them for effect) and walks forward edges;
/// - `worker_wrapper`'s `MonoidAccumulator` gate asks
///   [`term_is_pure`](Self::term_is_pure) of each context it wants to reassociate,
///   refusing to move anything that could perform — or *call* something that
///   performs — an effect into the accumulator.
pub(crate) struct CallGraph {
    /// Each declared name to the item that owns it (a `rec` group's members all
    /// map to the one group node).
    owner: HashMap<String, usize>,
    /// `refs[i]` — the items item `i` references.
    refs: Vec<HashSet<usize>>,
    /// `tainted[i]` — evaluating item `i` could perform an effect, directly or
    /// transitively.
    tainted: Vec<bool>,
}

impl CallGraph {
    /// Build the graph and run the effect-taint fixpoint.
    pub(crate) fn build(module: &Module) -> Self {
        let count = module.items.len();

        let owner = module
            .items
            .iter()
            .enumerate()
            .flat_map(|(index, item)| item.names().map(move |name| (name.to_owned(), index)))
            .collect::<HashMap<String, usize>>();

        let refs = module
            .items
            .iter()
            .map(|item| references(&owner, item.free_names()))
            .collect::<Vec<HashSet<usize>>>();

        // Seed from the directly-effectful items and propagate along reverse edges.
        let mut referrers = vec![Vec::<usize>::new(); count];
        for (referrer, set) in refs.iter().enumerate() {
            for &referee in set {
                referrers[referee].push(referrer);
            }
        }
        let mut tainted = vec![false; count];
        let mut work = (0..count)
            .filter(|&i| module.items[i].contains_effect())
            .collect::<Vec<usize>>();
        while let Some(i) = work.pop() {
            if tainted[i] {
                continue;
            }
            tainted[i] = true;
            work.extend(referrers[i].iter().copied());
        }

        Self {
            owner,
            refs,
            tainted,
        }
    }

    /// The items a set of names references (names owned by no item are ignored).
    pub(crate) fn references(&self, names: &BTreeSet<String>) -> HashSet<usize> {
        references(&self.owner, names.clone())
    }

    /// The items item `i` references.
    pub(crate) fn refs_of(&self, item: usize) -> &HashSet<usize> {
        &self.refs[item]
    }

    /// Whether evaluating item `i` could perform an effect (directly or via a
    /// reference to a tainted item).
    pub(crate) fn is_tainted(&self, item: usize) -> bool {
        self.tainted[item]
    }

    /// Whether `name` resolves to an item whose evaluation could perform an effect.
    /// A name owned by no item (a local binder or parameter) is not effectful here.
    pub(crate) fn name_is_effectful(&self, name: &str) -> bool {
        self.owner.get(name).is_some_and(|&i| self.tainted[i])
    }

    /// Whether evaluating `term` is pure: it performs no effect, references no
    /// effect-tainted item, and makes no indirect or unresolved call.
    ///
    /// The third clause is the conservative net-new obligation (`§6`): an `Apply`
    /// whose head is not a known module item could call *anything*, so it is
    /// treated as impure. A resolved call to a non-tainted item is fine — the
    /// second clause has already established the callee performs no effect.
    pub(crate) fn term_is_pure(&self, term: &Term) -> bool {
        !term.contains_effect()
            && term
                .free_names()
                .iter()
                .all(|name| !self.name_is_effectful(name))
            && self.calls_are_resolved(term)
    }

    /// Whether every `Apply` in `term` targets a known module item — no indirect
    /// (closure-valued head) or unresolved (local name) call, which could run
    /// anything.
    fn calls_are_resolved(&self, term: &Term) -> bool {
        let resolved_here = match term.as_subterm() {
            Subterm::Apply(apply) => {
                matches!(apply.head.as_subterm(), Subterm::Name(n) if self.owner.contains_key(n.as_str()))
            }
            _ => true,
        };

        resolved_here
            && subterms(term)
                .iter()
                .all(|child| self.calls_are_resolved(child))
    }
}

/// The items a set of names references, against a given owner map.
fn references(owner: &HashMap<String, usize>, names: BTreeSet<String>) -> HashSet<usize> {
    names
        .iter()
        .filter_map(|name| owner.get(name).copied())
        .collect()
}

/// The immediate sub-terms of a term, including primitive operands — so the purity
/// walk reaches an `Apply` buried inside a `NatAdd`'s operand.
fn subterms(term: &Term) -> Vec<&Term> {
    use crate::NatMatch;
    use std::iter;

    match term.as_subterm() {
        Subterm::Erased | Subterm::Unreachable | Subterm::Atom(_) | Subterm::Name(_) => vec![],
        Subterm::Prim(prim) => prim.operands(),
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            succ_case,
            ..
        }) => vec![head, zero_case, succ_case],
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => iter::once(head)
            .chain(cases.values())
            .chain(iter::once(default))
            .collect(),
        Subterm::Func(func) => vec![&func.body],
        Subterm::Apply(apply) => iter::once(&apply.head).chain(&apply.params).collect(),
        Subterm::Tuple(tuple) => tuple.fields.iter().collect(),
        Subterm::Proj(proj) => vec![&proj.head],
        Subterm::Match(m) => iter::once(&m.head).chain(&m.cases).collect(),
        Subterm::Let(let_) => vec![&let_.body, &let_.tail],
        Subterm::Rec(rec) => rec.items.iter().chain(iter::once(&rec.tail)).collect(),
    }
}
