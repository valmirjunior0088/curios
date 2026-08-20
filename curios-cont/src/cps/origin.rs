//! Where a value comes from, followed forward: whether every flow reaching it is a tuple construction the rewrite can read fields off, and at which widths, or something it cannot see through.
//!
//! The demand lattice is the backward half — how a received value is *used*. This is the forward half the value-lifetime decision separates from it: whether a parameter's every incoming edge carries a visible construction or an alias of one, which is what makes a merged flow *exclusive* and a scalar-replacement rewrite able to say what fields exist on every path. The two halves have separate tests and separate lattices, even though the eventual rewrite consumes them together.
//!
//! A region is entered by constructions but circulates through aliases: a loop arm that passes the loop's own parameter back unchanged contributes that parameter's own fact, which the fixpoint resolves to the constructions that entered it — so an every-edge-constructs reading is not expressible here by design, because it would decline exactly the loops the specification exists for.
//!
//! **Widths merge rather than conflict, which is what makes a variant expressible.** A flow reached by constructions of several widths was `Opaque` while the fact was one arity, and a tagged family is exactly that shape: `curios-ersd`'s door lowers a nullary constructor to a one-tuple and a three-payload one to a four-tuple, so no single arity ever described the UTF-8 scan state. The fact is the *set* of widths instead, and the rewrite travels the region at the widest of them with each narrower edge filled — which is safe for the same reason the return protocol's filler is: an edge's own width is this same fact read at that edge's argument, so nothing ever projects past what a construction carries.
//!
//! **What the fact deliberately does not record is the discriminant.** A variant region's cheapness comes from its tag being a constant on each entry edge, but that is a *consequence* rather than an admission condition, and requiring it would decline the motivating flow: once `split_returns` delivers a constructor as fields, the resume rebuilds it with the tag in a parameter, so seven of the scan region's seventeen constructions carry no literal at slot zero at all. Constant discriminants are then found by the passes that already fold them — projection forwarding through a visible construction, and jump threading over a literal switch.
//!
//! Boundaries are stated by injection rather than by omission: a resume parameter receives whatever an unsplit return interface delivers, an escaping function's parameters receive whatever unknown callers pass, the entry's parameters belong to the host, and a knot-tied value is a closure — each is seeded `Opaque` so a bottom that survives the round means *unreached*, never *assumed constructed*.

use {
    super::{
        CpsAtom, CpsCallee, CpsEdge, CpsModule, CpsNode, CpsValueExpr, CpsValueId, Lattice, Solver,
        analysis::analyze_calls,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What flows into a value, ordered `Unreached < Constructed(_)/Variant(..) < Opaque`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Origin {
    /// No flow reached it in the round — an unentered parameter or an unreachable binding.
    Unreached,
    /// Every flow reaching it is a tuple construction, or an alias of one, and these are the widths they carry. One width is an exact product; several are a variant, which travels as its widest constructor with each narrower edge filled.
    Constructed(BTreeSet<usize>),
    /// Every flow reaching it is a [`CpsValueExpr::Variant`](super::CpsValueExpr::Variant) of this family, or an alias of one — all at the family's width, carried here so the rewrite needs no module access. Always settled, because the door pads every construction; a merge with a different family or with a structural tuple is `Opaque`, which upstream typing makes unreachable and this lattice makes safe anyway.
    Variant(super::CpsFamilyId, usize),
    /// Some flow is not a visible construction — a call result, a literal, a closure, a projection.
    Opaque,
}

impl Origin {
    /// One construction of `width`, the fact a `LetValue` tuple establishes.
    pub(crate) fn of_width(width: usize) -> Self {
        Origin::Constructed(BTreeSet::from([width]))
    }

    /// How wide a *region* of this origin travels: its widest construction, and `None` where no construction describes the flow at all.
    pub(crate) fn width(&self) -> Option<usize> {
        match self {
            Origin::Constructed(widths) => widths.last().copied(),
            Origin::Variant(_, width) => Some(*width),
            Origin::Unreached | Origin::Opaque => None,
        }
    }

    /// The one width every flow agrees on, and `None` where they do not.
    ///
    /// This is the fact a *site* may take a value apart by, and it is deliberately not [`Origin::width`]: a value the fixpoint reports at several widths is a variant whose constructor is undecided there, so projecting it at the widest reads past whatever the narrower constructor carries and traps. The widest is what a region travels at; the settled one is what an edge into that region may project. A [`Origin::Variant`] flow is settled by construction — every edge carries the family width, a padded slot reads null rather than out of bounds — which is what door-padding buys this analysis.
    pub(crate) fn settled_width(&self) -> Option<usize> {
        match self {
            Origin::Constructed(widths) => match widths.len() {
                1 => widths.last().copied(),
                _ => None,
            },
            Origin::Variant(_, width) => Some(*width),
            Origin::Unreached | Origin::Opaque => None,
        }
    }

    /// Whether a site may take a value of this origin apart — see [`Origin::settled_width`] for why a merged variant may not.
    pub(crate) fn is_settled(&self) -> bool {
        !matches!(self, Origin::Constructed(widths) if widths.len() > 1)
    }

    /// The family this origin's flows construct, where they are variant constructions at all.
    pub(crate) fn family(&self) -> Option<super::CpsFamilyId> {
        match self {
            Origin::Variant(family, _) => Some(*family),
            Origin::Unreached | Origin::Constructed(_) | Origin::Opaque => None,
        }
    }
}

impl Lattice for Origin {
    fn bottom() -> Self {
        Origin::Unreached
    }

    fn join(&mut self, incoming: Self) {
        *self = match (std::mem::replace(self, Origin::Unreached), incoming) {
            (Origin::Opaque, _) | (_, Origin::Opaque) => Origin::Opaque,
            (Origin::Unreached, other) | (other, Origin::Unreached) => other,
            (Origin::Constructed(mut left), Origin::Constructed(right)) => {
                left.extend(right);
                Origin::Constructed(left)
            }
            (Origin::Variant(left, width), Origin::Variant(right, _)) if left == right => {
                Origin::Variant(left, width)
            }
            // A family meeting a different family or a structural tuple is ill-typed upstream; the lattice answers the safe point rather than assuming it cannot happen.
            (Origin::Variant(..), _) | (_, Origin::Variant(..)) => Origin::Opaque,
        }
    }
}

/// Join `atom`'s fact into `target`: a value hands its own fact forward, and anything that is not a value is not a construction.
fn flow(solver: &mut Solver<Origin>, atom: &CpsAtom, target: CpsValueId) {
    let incoming = match atom {
        CpsAtom::Value(value) => solver.facts().get(value).cloned().unwrap_or(Origin::Opaque),
        CpsAtom::Fun(_) | CpsAtom::Literal(_) | CpsAtom::Filler => Origin::Opaque,
    };
    solver.join(target, incoming);
}

/// Push an edge's arguments into its target's parameters. A target without a definition is a return sentinel; its values are delivered through the return interface, whose resume parameters are already seeded opaque.
fn flow_edge(module: &CpsModule, solver: &mut Solver<Origin>, edge: &CpsEdge) {
    let Some(continuation) = module.continuation(edge.target) else {
        return;
    };
    for (atom, param) in edge.args.iter().zip(continuation.params.iter().copied()) {
        flow(solver, atom, param);
    }
}

/// What flows into every value, to its least fixpoint.
pub(crate) fn origins(module: &CpsModule) -> BTreeMap<CpsValueId, Origin> {
    let escaping = analyze_calls(module).escaping;
    let seeds = module.values.live_ids().collect::<Vec<_>>();

    Solver::solve(seeds, |solver| {
        // The injected boundaries, restated every round so a later join cannot narrow them.
        for (function, definition) in module.functions.iter_live() {
            if escaping.contains(&function) || module.entry() == Some(function) {
                for param in &definition.params {
                    solver.join(*param, Origin::Opaque);
                }
            }
        }

        for (_, node) in module.nodes.iter_live() {
            match node {
                CpsNode::LetValue { result, value, .. } => {
                    let origin = match value {
                        CpsValueExpr::Tuple(atoms) => Origin::of_width(atoms.len()),
                        CpsValueExpr::Variant(family, atoms) => {
                            Origin::Variant(*family, atoms.len())
                        }
                        CpsValueExpr::List(_) | CpsValueExpr::Literal(_) => Origin::Opaque,
                    };
                    solver.join(*result, origin);
                }
                CpsNode::LetIntrinsic { result, .. } => {
                    solver.join(*result, Origin::Opaque);
                }
                CpsNode::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => {
                    if let Some(resume) = module.continuation(*return_to) {
                        for param in &resume.params {
                            solver.join(*param, Origin::Opaque);
                        }
                    }
                    if let CpsCallee::Known(callee) = callee
                        && let Some(function) = module.function(*callee)
                    {
                        for (atom, param) in args.iter().zip(function.params.iter().copied()) {
                            flow(solver, atom, param);
                        }
                    }
                }
                CpsNode::ApplyCont(edge) => flow_edge(module, solver, edge),
                CpsNode::Switch { cases, default, .. } => {
                    for edge in cases.values().chain(default.as_ref()) {
                        flow_edge(module, solver, edge);
                    }
                }
                CpsNode::Foreign { return_to, .. }
                | CpsNode::Cell { return_to, .. }
                | CpsNode::Intrinsic { return_to, .. } => {
                    if let Some(resume) = module.continuation(*return_to) {
                        for param in &resume.params {
                            solver.join(*param, Origin::Opaque);
                        }
                    }
                }
                CpsNode::RecInit { values, .. } => {
                    for value in values {
                        solver.join(*value, Origin::Opaque);
                    }
                }
                CpsNode::LetFun { .. }
                | CpsNode::LetCont { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }
    })
}

#[cfg(test)]
mod tests;
