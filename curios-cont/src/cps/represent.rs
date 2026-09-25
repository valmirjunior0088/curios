//! Which values can be held in a machine register instead of an `i31`/`Flt` reference.
//!
//! Every value in an emitted module is a reference by default, which is what lets one closure type serve an arity and one field shape serve a constructor. Inside a loop that uniformity buys nothing and costs a great deal: every `Flt` operation allocates its result's box, and every read of a `Bool`, a byte or a tag arrives through a type check and an unbox first. A `Nat` or `Int` is not among what a register holds — it is a reference whatever its size, an i31 or a boxed magnitude — so the words here are the bounded scalars and the floats.
//!
//! **The carrier is not in question; the storage is.** A value's carrier is fixed by whatever produced it, so the decision is whether to hold it raw or behind a reference — but the answer must *name* the carrier, because a local has to be declared `i32` or `f64` and a continuation parameter has no producer to read it from. Its carrier is only knowable from the demands its uses impose, which is why it is carried in the lattice rather than recovered afterwards.
//!
//! **A value is raw whenever any use demands the raw carrier**, and every disagreeing use is coerced. That is deliberately not the conservative rule "raw only when *every* use accepts it", which was specified first and measured worthless while `Nat` still rode a word: in the `lcg` kernel `x` was used by a multiply *and* jumped out on the loop-exit edge, so the conservative join answered boxed and the 64-bit multiply survived — the single largest cost in the loop. The asymmetry that justifies preferring raw is in the instructions rather than in any loop heuristic: coercing raw to boxed is one `ref.i31` or one `struct.new`, while boxed to raw is a runtime type check and an unbox.
//!
//! **An edge argument's demand is the storage of the parameter it feeds**, which is what makes this a fixpoint rather than a scan. Without that rule a loop's back-edge values have no raw use of their own — the decremented counter and the folded accumulator are *only* ever passed back round — so they would settle boxed and the loop would coerce on every iteration, losing exactly what the analysis exists to win.
//!
//! **A word holds only what a word was handed.** A `Nat` is a reference whatever its size, and narrowing one to a word keeps every value below the i31 exactly but saturates the rest, so a continuation parameter coerced to a word loses what a bigger argument said. A parameter is therefore offered the word only when every argument reaching it is itself one — a small literal, a filler, a word-producing definition, or another such parameter — which [`word_params`] decides before any demand is read. A `Flt` parameter needs no such test: unboxing a float loses nothing.
//!
//! **A use can only take what the definition can give**, which is what [`Offer`] states. Demands alone would raise a function parameter the moment its body did arithmetic on it — and a function parameter arrives through a `func/N` signature that is uniformly `anyref`, with no store site the analysis controls. The same holds of a value returning from a call, a host import or a cell read. Coercing at the definition instead of excluding it was the alternative, and it buys nothing: the definition would coerce back to a register exactly what it had just been handed as a reference.

use {
    super::{
        Atom, ContinuationId, Edge, Lattice, Literal, Module, Node, Repr, Solver, ValueExpr,
        ValueId, analysis::free_values, nat_is_small,
    },
    curios_abi::WireType,
    std::collections::{BTreeMap, BTreeSet},
};

/// How one value is held between its definition and its uses.
///
/// Ordered `Boxed < Raw(_) < Conflict`, with `Conflict` meaning "no single raw carrier serves every use", which the backend reads as boxed. Only an `Offer::Open` value can reach the top: everything else admits exactly one carrier, so a second demand is either that same carrier or filtered out. Answering a disagreement by dropping back to `Boxed` would move a fact *down* the order, and `Solver::solve` terminates on nothing having changed — so the conservative answer has to be a third point above both, not a return to the bottom.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Storage {
    /// Behind a reference — an `i31`, a `Flt` struct, or a heap shape.
    Boxed,
    /// In a machine register, at the named carrier.
    Raw(Repr),
    /// Demanded raw at two carriers that cannot both be served. Held boxed.
    Conflict,
}

impl Storage {
    /// The carrier to declare this value's local at, or `None` when it stays a reference.
    pub fn raw_carrier(self) -> Option<Repr> {
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
    /// A continuation parameter, which has no producer of its own: it holds whatever its uses agree on, and each incoming edge coerces its argument to match. This is the only offer a `Conflict` can arise from. `words` says whether the machine word is among what it may hold, which [`word_params`] decides from what reaches it.
    Open { words: bool },
    /// Nothing. Either the value arrives through a position that is uniformly a reference — a function parameter, a call, host or cell result, a recursive shell — or its definition builds a heap shape, which a register cannot hold in the first place.
    Never,
}

impl Offer {
    /// Whether this definition can hand `carrier` to a register.
    fn admits(&self, carrier: Repr) -> bool {
        match self {
            Offer::Open { words } => carrier != Repr::Nat || *words,
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

/// The representation a literal is materialised at: a small `Nat` rides a machine word, since every word a use may ask it for is its value; any other `Nat`, and every `Int`, is the reference either always is, an i31 or a boxed magnitude.
fn literal_repr(literal: &Literal) -> Repr {
    match literal {
        Literal::Nat(value) if nat_is_small(value) => Repr::Nat,
        Literal::Nat(_) | Literal::Int(_) => Repr::Ref,
        Literal::Flt(_) => Repr::Flt,
        Literal::Bin(grain, _) => Repr::Bin(*grain),
    }
}

/// Withdraw every parameter of `cont`, which receives a result the emitter hands over as a reference.
fn withdraw_params(module: &Module, cont: ContinuationId, withdrawn: &mut BTreeSet<ValueId>) {
    let Some(continuation) = module.continuation(cont) else {
        return;
    };

    withdrawn.extend(&continuation.params);
}

/// What each value's definition can hand to a register.
///
/// Offers are collected first and withdrawals applied over them at the end, rather than both being written into one map as they are found. The order matters and this is what makes it not depend on traversal: a value can perfectly well bind a scalar an intrinsic produced *and* escape into another function's body, and it must settle on the withdrawal whichever the walk reached first.
fn offers(module: &Module) -> BTreeMap<ValueId, Offer> {
    let mut offers = BTreeMap::new();
    let mut withdrawn = BTreeSet::new();

    for (_, continuation) in module.continuations.iter_live() {
        // A continuation parameter has no producer of its own, so it takes whatever its uses agree on.
        for &param in &continuation.params {
            offers.insert(param, Offer::Open { words: true });
        }
    }

    for (_, node) in module.nodes.iter_live() {
        match node {
            // A row read is the one operation whose result carrier is a fact of the module rather than of the operation: the slot it names says whether a register can hold it.
            // A result its literal operands bound below the i31 is offered the word its value fits.
            Node::LetIntrinsic {
                result, op, args, ..
            } => {
                let offer = match op.bounds_result(args) {
                    true => Offer::Fixed(Repr::Nat),
                    false => offer_of(module.result_repr(op)),
                };
                offers.insert(*result, offer);
            }

            Node::LetValue { result, value, .. } => {
                let offer = match value {
                    ValueExpr::Literal(literal) => offer_of(literal_repr(literal)),
                    ValueExpr::List(_) | ValueExpr::Tuple(_) | ValueExpr::Row(..) => Offer::Never,
                };
                offers.insert(*result, offer);
            }

            // A result returning from a call, a cell operation or a call-shaped intrinsic is already a reference by the time it reaches its continuation's parameter.
            Node::ApplyFun { return_to, .. }
            | Node::Cell { return_to, .. }
            | Node::Channel { return_to, .. }
            | Node::Intrinsic { return_to, .. } => {
                withdraw_params(module, *return_to, &mut withdrawn)
            }

            // A host import's results are references too, with one exception: an `Flt` is held at its carrier. Every scalar crosses back raw, and the emitter boxes an integral one at the call — a `Nat` or `Int` past the i31 into the boxed magnitude only the guest can build — and embeds a reference into the rope; but a float's box is one allocation a use may never need. So that parameter *is* the `f64`, offered at its carrier like any other definition, and a use wanting a reference boxes at its own site through the coercion every raw carrier already has.
            //
            // Withdrawing it with the rest is what made an `Flt` result unrepresentable: the parameter became a reference the call had no way to produce, and the module failed validation with an `f64` where an `anyref` was wanted.
            Node::Foreign {
                function,
                return_to,
                ..
            } => {
                if let Some(continuation) = module.continuation(*return_to) {
                    let results = function.signature().results.iter().collect::<Vec<_>>();

                    for (index, &param) in continuation.params.iter().enumerate() {
                        match results.get(index).map(|(_, wire)| wire) {
                            Some(WireType::Flt) => {
                                offers.insert(param, Offer::Fixed(Repr::Flt));
                            }
                            _ => {
                                withdrawn.insert(param);
                            }
                        }
                    }
                }
            }

            Node::LetFun { .. }
            | Node::LetCont { .. }
            | Node::ApplyCont(_)
            | Node::Switch { .. }
            | Node::Halt { .. }
            | Node::Panic(_)
            | Node::Unreachable => {}
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

    let words = word_params(module, &offers);
    for (value, offer) in &mut offers {
        if let Offer::Open { words: admitted } = offer {
            *admitted = words.contains(value);
        }
    }

    offers
}

/// The continuation parameters a machine word may hold: those every argument reaching them is a word already — a small literal, a filler, a definition producing a word, or another such parameter.
///
/// A greatest fixpoint, because a loop parameter reaches itself around its back edge: every open parameter starts admitted, one reached by anything else is dropped, and each drop drops the parameters it reaches in turn.
fn word_params(module: &Module, offers: &BTreeMap<ValueId, Offer>) -> BTreeSet<ValueId> {
    let open = |value: &ValueId| matches!(offers.get(value), Some(Offer::Open { .. }));
    let mut words = offers.keys().copied().filter(open).collect::<BTreeSet<_>>();
    let mut reaches = BTreeMap::<ValueId, Vec<ValueId>>::new();
    let mut dropped = Vec::new();

    let mut read_edge = |edge: &Edge| {
        let Some(target) = module.continuation(edge.target) else {
            return;
        };

        for (arg, &param) in edge.args.iter().zip(&target.params) {
            match arg {
                Atom::Value(value) if open(value) => reaches.entry(*value).or_default().push(param),
                Atom::Value(value) if offers.get(value) == Some(&Offer::Fixed(Repr::Nat)) => {}
                Atom::Literal(literal) if literal_repr(literal) == Repr::Nat => {}
                Atom::Filler => {}
                Atom::Value(_) | Atom::Literal(_) | Atom::Fun(_) => dropped.push(param),
            }
        }
    };

    for (_, node) in module.nodes.iter_live() {
        match node {
            Node::ApplyCont(edge) => read_edge(edge),
            Node::Switch { cases, default, .. } => {
                cases
                    .values()
                    .chain(default.iter())
                    .for_each(&mut read_edge);
            }
            Node::LetIntrinsic { .. }
            | Node::LetValue { .. }
            | Node::ApplyFun { .. }
            | Node::Cell { .. }
            | Node::Channel { .. }
            | Node::Intrinsic { .. }
            | Node::Foreign { .. }
            | Node::LetFun { .. }
            | Node::LetCont { .. }
            | Node::Halt { .. }
            | Node::Panic(_)
            | Node::Unreachable => {}
        }
    }

    while let Some(param) = dropped.pop() {
        if words.remove(&param) {
            dropped.extend(reaches.get(&param).into_iter().flatten().copied());
        }
    }

    words
}

/// The raw carrier a representation demands, or `None` when it names a reference. A `Nat` or `Int` operand demands the word, which only a definition offering one can meet: everything else stays the reference the operand reads by default.
fn raw_carrier(repr: &Repr) -> Option<Repr> {
    match repr {
        Repr::Nat | Repr::Flt => Some(*repr),
        Repr::Number => Some(Repr::Nat),
        Repr::Bin(_) | Repr::List | Repr::Ref => None,
    }
}

/// The raw carrier a host call reads this wire type at. Mirrors the `WireType`-to-`LoadAs` mapping the emitter already applies at foreign call sites: a `Bool` or a `Byte` crosses as its word, while a `Nat` or `Int` arrives as the reference it is and is narrowed to the wire at the call, refusing a value the wire cannot carry.
fn wire_carrier(wire: &WireType) -> Option<Repr> {
    match wire {
        WireType::Bool | WireType::Byte => Some(Repr::Nat),
        WireType::Flt => Some(Repr::Flt),
        WireType::Nat
        | WireType::Int
        | WireType::Bytes
        | WireType::Bits
        | WireType::Handle
        | WireType::List(_) => None,
    }
}

/// Decide the storage of every value in the module: which values `curios-emit` may hold in a machine register, and at which carrier.
pub fn storage(module: &Module) -> BTreeMap<ValueId, Storage> {
    let offers = offers(module);
    let seeds = module.values.live_ids().collect::<Vec<_>>();

    Solver::solve(seeds, |solver| {
        for (_, node) in module.nodes.iter_live() {
            match node {
                // The roster states what each operand position reads.
                Node::LetIntrinsic { op, args, .. } => {
                    for (index, arg) in args.iter().enumerate() {
                        demand(arg, raw_carrier(&op.operand_repr(index)), &offers, solver);
                    }
                }

                // A tag is read as a raw unsigned scalar; the edges' arguments are handled below with every other edge.
                Node::Switch {
                    scrutinee,
                    cases,
                    default,
                } => {
                    demand(scrutinee, Some(Repr::Nat), &offers, solver);
                    for edge in cases.values().chain(default.iter()) {
                        edge_demands(module, edge, &offers, solver);
                    }
                }

                Node::ApplyCont(edge) => edge_demands(module, edge, &offers, solver),

                // A host call reads its scalar parameters raw and its reference parameters as shapes, whether or not it returns.
                Node::Foreign { function, args, .. } | Node::Halt { function, args } => {
                    for (arg, (_, wire)) in args.iter().zip(&function.signature().params) {
                        demand(arg, wire_carrier(wire), &offers, solver);
                    }
                }

                // A variant construction stores each atom into a slot whose carrier the row declares, so a scalar slot demands its atom raw — the store side of the same fact the read side offers above.
                Node::LetValue {
                    value: ValueExpr::Row(row, atoms),
                    ..
                } => {
                    for (index, atom) in atoms.iter().enumerate() {
                        demand(
                            atom,
                            raw_carrier(&module.slot_repr(*row, index)),
                            &offers,
                            solver,
                        );
                    }
                }

                // Everything else stores or passes a reference: call arguments cross a `func/N` signature that is uniformly `anyref`, a list's elements and a tuple's fields are held uninterpreted, and every cell operation works on shapes. None of these demands a raw carrier, so none contributes.
                Node::LetValue { value, .. } => match value {
                    ValueExpr::Literal(_)
                    | ValueExpr::List(_)
                    | ValueExpr::Tuple(_)
                    | ValueExpr::Row(..) => {}
                },
                Node::ApplyFun { .. }
                | Node::Cell { .. }
                | Node::Channel { .. }
                | Node::Intrinsic { .. }
                | Node::LetFun { .. }
                | Node::LetCont { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            }
        }
    })
}

/// Demand `carrier` of `atom`, where its definition can supply it. An atom that is not a value, a position that reads a reference, and a definition that cannot reach a register all contribute nothing.
fn demand(
    atom: &Atom,
    carrier: Option<Repr>,
    offers: &BTreeMap<ValueId, Offer>,
    solver: &mut Solver<Storage>,
) {
    let (Atom::Value(value), Some(carrier)) = (atom, carrier) else {
        return;
    };

    if offers.get(value).is_some_and(|offer| offer.admits(carrier)) {
        solver.join(*value, Storage::Raw(carrier));
    }
}

/// An edge's arguments inherit the storage of the parameters they feed — the rule that carries a decision around a loop.
fn edge_demands(
    module: &Module,
    edge: &Edge,
    offers: &BTreeMap<ValueId, Offer>,
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
