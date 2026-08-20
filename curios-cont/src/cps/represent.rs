//! Which values can be held in a machine register instead of an `i31`/`Flt` reference.
//!
//! Every value in an emitted module is a reference today, which is what lets one closure type serve an arity and one field shape serve a constructor. Inside a loop that uniformity buys nothing and costs a great deal: a `Nat` multiply widens both operands to 64 bits, multiplies, shifts and branches purely to check that the product still fits the 31 bits an `i31` holds, and every operand arrives through a `ref.cast` and an `i31.get_u` first.
//!
//! **The carrier is not in question; the storage is.** A value's carrier is fixed by whatever produced it, so the decision is whether to hold it raw or behind a reference — but the answer must *name* the carrier, because a local has to be declared `i32` or `f32` and a continuation parameter has no producer to read it from. Its carrier is only knowable from the demands its uses impose, which is why it is carried in the lattice rather than recovered afterwards.
//!
//! **A value is raw whenever any use demands the raw carrier**, and every disagreeing use is coerced. That is deliberately not the conservative rule "raw only when *every* use accepts it", which was specified first and measured worthless: in the `lcg` kernel `x` is used by a multiply *and* jumped out on the loop-exit edge, so the conservative join answers boxed and the 64-bit multiply survives — the single largest cost in the loop. The asymmetry that justifies preferring raw is in the instructions rather than in any loop heuristic: coercing raw to boxed is one `ref.i31`, while boxed to raw is a `ref.cast` plus an `i31.get_u`, two instructions of which one is a runtime type check.
//!
//! **An edge argument's demand is the storage of the parameter it feeds**, which is what makes this a fixpoint rather than a scan. Without that rule a loop's back-edge values have no raw use of their own — the decremented counter and the folded accumulator are *only* ever passed back round — so they would settle boxed and the loop would coerce on every iteration, losing exactly what the analysis exists to win.
//!
//! **A use can only take what the definition can give**, which is what [`Offer`] states. Demands alone would raise a function parameter the moment its body did arithmetic on it — and a function parameter arrives through a `func/N` signature that is uniformly `anyref`, with no store site the analysis controls. The same holds of a value returning from a call, a host import or a cell read. Coercing at the definition instead of excluding it was the alternative, and it buys nothing: the definition would coerce back to a register exactly what it had just been handed as a reference.

use {
    super::{
        CpsAtom, CpsContId, CpsEdge, CpsLiteral, CpsModule, CpsNode, CpsValueExpr, CpsValueId,
        Lattice, Repr, Solver, analysis::free_values,
    },
    curios_abi::WireType,
    std::collections::{BTreeMap, BTreeSet},
};

/// How one value is held between its definition and its uses.
///
/// Ordered `Boxed < Raw(_) < Conflict`, with `Conflict` meaning "no single raw carrier serves every use", which the backend reads as boxed. Only an [`Offer::Open`] value can reach the top: everything else admits exactly one carrier, so a second demand is either that same carrier or filtered out. Answering a disagreement by dropping back to `Boxed` would move a fact *down* the order, and [`Solver::solve`] terminates on nothing having changed — so the conservative answer has to be a third point above both, not a return to the bottom.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Storage {
    /// Behind a reference — an `i31`, a `Flt` struct, or a heap shape.
    Boxed,
    /// In a machine register, at the named carrier.
    Raw(Repr),
    /// Demanded raw at two carriers that cannot both be served. Held boxed.
    Conflict,
}

impl Storage {
    /// The carrier to declare this value's local at, or `None` when it stays a reference.
    pub(crate) fn raw_carrier(self) -> Option<Repr> {
        match self {
            Storage::Raw(carrier) => Some(carrier),
            Storage::Boxed | Storage::Conflict => None,
        }
    }
}

impl Lattice for Storage {
    fn bottom() -> Self {
        Storage::Boxed
    }

    fn join(&mut self, incoming: Self) {
        *self = match (*self, incoming) {
            (Storage::Conflict, _) | (_, Storage::Conflict) => Storage::Conflict,
            (Storage::Boxed, other) | (other, Storage::Boxed) => other,
            (Storage::Raw(current), Storage::Raw(incoming)) => match current == incoming {
                true => Storage::Raw(current),
                false => Storage::Conflict,
            },
        }
    }
}

/// What a value's definition can hand to a machine register.
///
/// [`Storage`] answers what the uses want; this answers what the definition can give, and a value is held raw only where the two agree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Offer {
    /// The definition produces exactly this carrier, so a use demanding it reads the register directly and every other use coerces at its own site.
    Fixed(Repr),
    /// A continuation parameter, which has no producer of its own: it holds whatever its uses agree on, and each incoming edge coerces its argument to match. This is the only offer a `Conflict` can arise from.
    Open,
    /// Nothing. Either the value arrives through a position that is uniformly a reference — a function parameter, a call, host or cell result, a recursive shell — or its definition builds a heap shape, which a register cannot hold in the first place.
    Never,
}

impl Offer {
    /// Whether this definition can hand `carrier` to a register.
    fn admits(&self, carrier: Repr) -> bool {
        match self {
            Offer::Open => true,
            Offer::Fixed(fixed) => *fixed == carrier,
            Offer::Never => false,
        }
    }
}

/// The offer a value produced at `repr` makes: its own carrier when that is a register carrier, and nothing when it is a reference.
fn offer_of(repr: Repr) -> Offer {
    match raw_carrier(&repr) {
        Some(carrier) => Offer::Fixed(carrier),
        None => Offer::Never,
    }
}

/// The representation a literal is materialised at.
fn literal_repr(literal: &CpsLiteral) -> Repr {
    match literal {
        CpsLiteral::Nat(_) => Repr::Nat,
        CpsLiteral::Int(_) => Repr::Int,
        CpsLiteral::Flt(_) => Repr::Flt,
        CpsLiteral::Bin(grain, _) => Repr::Bin(*grain),
    }
}

/// Withdraw every parameter of `cont`, which receives a result the emitter hands over as a reference.
fn withdraw_params(module: &CpsModule, cont: CpsContId, withdrawn: &mut BTreeSet<CpsValueId>) {
    let Some(continuation) = module.continuation(cont) else {
        return;
    };

    withdrawn.extend(&continuation.params);
}

/// What each value's definition can hand to a register.
///
/// Offers are collected first and withdrawals applied over them at the end, rather than both being written into one map as they are found. The order matters and this is what makes it not depend on traversal: a value can perfectly well bind a scalar an intrinsic produced *and* escape into another function's body, and it must settle on the withdrawal whichever the walk reached first.
fn offers(module: &CpsModule) -> BTreeMap<CpsValueId, Offer> {
    let mut offers = BTreeMap::new();
    let mut withdrawn = BTreeSet::new();

    for (_, continuation) in module.continuations.iter_live() {
        // A continuation parameter has no producer of its own, so it takes whatever its uses agree on.
        for &param in &continuation.params {
            offers.insert(param, Offer::Open);
        }
    }

    for (_, node) in module.nodes.iter_live() {
        match node {
            CpsNode::LetIntrinsic { result, op, .. } => {
                offers.insert(*result, offer_of(op.result_repr()));
            }

            CpsNode::LetValue { result, value, .. } => {
                let offer = match value {
                    CpsValueExpr::Literal(literal) => offer_of(literal_repr(literal)),
                    CpsValueExpr::List(_) | CpsValueExpr::Tuple(_) | CpsValueExpr::Variant(..) => {
                        Offer::Never
                    }
                };
                offers.insert(*result, offer);
            }

            // A result returning from a call, a host import, a cell operation or a call-shaped intrinsic is already a reference by the time it reaches its continuation's parameter.
            CpsNode::ApplyFun { return_to, .. }
            | CpsNode::Foreign { return_to, .. }
            | CpsNode::Cell { return_to, .. }
            | CpsNode::Intrinsic { return_to, .. } => {
                withdraw_params(module, *return_to, &mut withdrawn)
            }

            // A recursive shell is allocated empty and filled afterwards, so it is a heap value throughout.
            CpsNode::RecInit { values, .. } => withdrawn.extend(values),

            CpsNode::LetFun { .. }
            | CpsNode::LetCont { .. }
            | CpsNode::ApplyCont(_)
            | CpsNode::Switch { .. }
            | CpsNode::Exit { .. }
            | CpsNode::Unreachable => {}
        }
    }

    for (function, definition) in module.functions.iter_live() {
        // Every function is entered through a `func/N` signature whose parameters are uniformly `anyref`.
        withdrawn.extend(&definition.params);
        withdraw_params(module, definition.return_cont, &mut withdrawn);

        // A value free in this body is bound outside it, and lowering carries it in: lambda-lifted onto a directly-called function as an extra `anyref` parameter, or captured into a closure's environment as an `anyref` field. Neither survives a register, and *this* is where the locals-only scope is enforced rather than merely intended — without it a counter decided raw where it is bound is read as a register where it is used, and a `(ref any)` reaches an `i32.sub`.
        withdrawn.extend(free_values(module, function));
    }

    for value in withdrawn {
        offers.insert(value, Offer::Never);
    }

    offers
}

/// The raw carrier a representation names, or `None` when it names a reference.
fn raw_carrier(repr: &Repr) -> Option<Repr> {
    match repr {
        Repr::Nat | Repr::Int | Repr::Flt => Some(*repr),
        Repr::Bin(_) | Repr::List | Repr::Ref => None,
    }
}

/// The raw carrier a host call reads this wire type at. Mirrors the `WireType`-to-`LoadAs` mapping the emitter already applies at foreign call sites, where `Bool` crosses as a `Nat`.
fn wire_carrier(wire: &WireType) -> Option<Repr> {
    match wire {
        WireType::Nat | WireType::Bool => Some(Repr::Nat),
        WireType::Int => Some(Repr::Int),
        WireType::Bytes | WireType::Handle | WireType::List(_) => None,
    }
}

/// Decide the storage of every value in the module.
pub(crate) fn storage(module: &CpsModule) -> BTreeMap<CpsValueId, Storage> {
    let offers = offers(module);
    let seeds = module.values.live_ids().collect::<Vec<_>>();

    Solver::solve(seeds, |solver| {
        for (_, node) in module.nodes.iter_live() {
            match node {
                // The roster states what each operand position reads.
                CpsNode::LetIntrinsic { op, args, .. } => {
                    for (index, arg) in args.iter().enumerate() {
                        demand(arg, raw_carrier(&op.operand_repr(index)), &offers, solver);
                    }
                }

                // A tag is read as a raw unsigned scalar; the edges' arguments are handled below with every other edge.
                CpsNode::Switch {
                    scrutinee,
                    cases,
                    default,
                } => {
                    demand(scrutinee, Some(Repr::Nat), &offers, solver);
                    for edge in cases.values().chain(default.iter()) {
                        edge_demands(module, edge, &offers, solver);
                    }
                }

                CpsNode::ApplyCont(edge) => edge_demands(module, edge, &offers, solver),

                // The exit code crosses as a raw `i32`.
                CpsNode::Exit { value } => {
                    if let Some(value) = value {
                        demand(value, Some(Repr::Nat), &offers, solver);
                    }
                }

                // A host call reads its scalar parameters raw and its reference parameters as shapes.
                CpsNode::Foreign { function, args, .. } => {
                    for (arg, (_, wire)) in args.iter().zip(&function.signature.params) {
                        demand(arg, wire_carrier(wire), &offers, solver);
                    }
                }

                // Everything else stores or passes a reference: call arguments cross a `func/N` signature that is uniformly `anyref`, aggregate elements and closure captures are held uninterpreted, and every cell operation works on shapes. None of these demands a raw carrier, so none contributes.
                CpsNode::LetValue { value, .. } => match value {
                    CpsValueExpr::Literal(_)
                    | CpsValueExpr::List(_)
                    | CpsValueExpr::Tuple(_)
                    | CpsValueExpr::Variant(..) => {}
                },
                CpsNode::ApplyFun { .. }
                | CpsNode::Cell { .. }
                | CpsNode::Intrinsic { .. }
                | CpsNode::LetFun { .. }
                | CpsNode::LetCont { .. }
                | CpsNode::Unreachable
                | CpsNode::RecInit { .. } => {}
            }
        }
    })
}

/// Demand `carrier` of `atom`, where its definition can supply it. An atom that is not a value, a position that reads a reference, and a definition that cannot reach a register all contribute nothing.
fn demand(
    atom: &CpsAtom,
    carrier: Option<Repr>,
    offers: &BTreeMap<CpsValueId, Offer>,
    solver: &mut Solver<Storage>,
) {
    let (CpsAtom::Value(value), Some(carrier)) = (atom, carrier) else {
        return;
    };

    if offers.get(value).is_some_and(|offer| offer.admits(carrier)) {
        solver.join(*value, Storage::Raw(carrier));
    }
}

/// An edge's arguments inherit the storage of the parameters they feed — the rule that carries a decision around a loop.
fn edge_demands(
    module: &CpsModule,
    edge: &CpsEdge,
    offers: &BTreeMap<CpsValueId, Offer>,
    solver: &mut Solver<Storage>,
) {
    let Some(target) = module.continuation(edge.target) else {
        // A jump to a function's return sentinel: the result leaves through the `anyref` return, so nothing is demanded raw.
        return;
    };

    for (arg, param) in edge.args.iter().zip(&target.params) {
        let Some(&decided) = solver.facts().get(param) else {
            continue;
        };
        demand(arg, decided.raw_carrier(), offers, solver);
    }
}

#[cfg(test)]
mod tests;
