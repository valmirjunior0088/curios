//! Fixtures the term suites share: a name, distinct nodes, and the deep spines that must not recurse natively.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `term`, and nothing outside it.

use {
    crate::*,
    curios_utilities::Qualifier,
    std::{
        collections::{BTreeMap, HashSet},
        rc::Rc,
    },
};

/// A declaration's name, from the path a test writes. Fixture-only.
pub(super) fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// The number of distinct `Node`s reachable from `term`, counting a shared node once. Inlined here because only these tests ask the question.
pub(super) fn distinct_nodes(term: &Term) -> usize {
    let mut seen = HashSet::new();
    let mut stack = Vec::from([term.clone()]);
    while let Some(node) = stack.pop() {
        if !seen.insert(Rc::as_ptr(&node.inner)) {
            continue;
        }
        node.as_ref().any_child_term(&mut |child| {
            stack.push(child.clone());
            false
        });
    }
    seen.len()
}

/// Deep enough that a walk recursing natively per link overflows a default stack, so a regression is a stack overflow rather than a slow test.
pub(super) const DEEP: u32 = 100_000;

/// The families a test declares, as the level alignment reads a registry.
#[derive(Default)]
pub(super) struct Registry {
    pub(super) inducts: BTreeMap<Global, InductDecl>,
}

impl Families for Registry {
    fn induct(&self, name: &Global) -> Option<&InductDecl> {
        self.inducts.get(name)
    }

    fn struct_(&self, _: &Global) -> Option<&StructDecl> {
        None
    }
}

/// A family of `params` type parameters and `indices` `Nat` indices with no constructor, carrying `variances`: all the alignment reads of a declaration is its counts and its vector.
pub(super) fn shaped(params: usize, indices: usize, variances: &[Variance]) -> InductDecl {
    let binders = |from: u32, count: usize, type_: Term| {
        (from..)
            .take(count)
            .map(|index| (Free::local(index, None), type_.clone()))
            .collect::<Vec<_>>()
    };

    InductDecl {
        universe_context: UniverseContext {
            parameter_count: variances.len(),
            constraints: Vec::new(),
        },
        arity: Telescope::build(
            binders(900, params, Term::type_ground()),
            Telescope::build(
                binders(950, indices, Term::intrinsic(Intrinsic::NatType)),
                (),
            ),
        ),
        constructors: Vec::new(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: variances.to_vec(),
        plicities: Vec::new(),
    }
}

/// `path`'s former at `level` as its group's projection, applied in full to `Nat`: `rec W : (A: Type l) -> Type l = (A) => W.{l}(A); W`, the spelling reduction hands over for an instance of an `induct`'s name.
pub(super) fn former_applied(path: &str, level: u32) -> Term {
    let member = Free::local(970, Some("W"));
    let carrier = Free::local(971, Some("A"));
    let sort = Term::type_at(Level::constant(level));
    let body = Term::func(
        [(carrier, sort.clone())],
        Term::induct_type_at(
            nominal(path),
            [Level::constant(level)],
            [Term::free_var(&carrier)],
            Vec::<Term>::new(),
        ),
    );

    Term::apply(
        Term::rec(
            [(
                member,
                Term::func_type([(carrier, sort.clone())], sort),
                body,
            )],
            Term::free_var(&member),
        ),
        [Term::intrinsic(Intrinsic::NatType)],
    )
}

/// A left-nested application spine `((x a) a) …`, `DEEP` links tall.
pub(super) fn deep_spine(seed: u32) -> Term {
    let argument = Term::free_var(&Free::local(seed, None));
    let mut term = Term::free_var(&Free::local(seed, None));
    for _ in 0..DEEP {
        term = Term::apply(term, [argument.clone()]);
    }
    term
}

/// Past one 32 MiB stack segment several times over, which is what proves a walk can chain another rather than merely start on one. `DEEP` would prove the same thing at twenty segments; this asks for four.
pub(super) const TALL: u32 = 20_000;
