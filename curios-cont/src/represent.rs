//! Which values can be held in a machine register instead of an `i31`/`Flt` reference, and at which carriers a function the analysis sees whole takes its parameters and hands back its results.
//!
//! Every value in an emitted module is a reference by default, which is what lets one closure type serve an arity and one field shape serve a constructor. Inside a loop that uniformity buys nothing and costs a great deal: every `Flt` operation allocates its result's box, and every read of a `Bool`, a byte or a tag arrives through a type check and an unbox first. A `Nat` or `Int` is not among what a register holds — it is a reference whatever its size, an i31 or a boxed magnitude — so the words here are the bounded scalars and the floats.
//!
//! **The carrier is not in question; the storage is.** A value's carrier is fixed by whatever produced it, so the decision is whether to hold it raw or behind a reference — but the answer must *name* the carrier, because a local has to be declared `i32` or `f64` and a continuation parameter has no producer to read it from. Its carrier is only knowable from the demands its uses impose, which is why it is carried in the lattice rather than recovered afterwards.
//!
//! **A value is raw whenever any use demands the raw carrier**, and every disagreeing use is coerced. That is deliberately not the conservative rule "raw only when *every* use accepts it": a loop value used by an operation *and* jumped out on the loop-exit edge would settle boxed under it, paying the unbox and the rebox on every iteration. The asymmetry that justifies preferring raw is in the instructions rather than in any loop heuristic: coercing raw to boxed is one `ref.i31` or one `struct.new`, while boxed to raw is a runtime type check and an unbox. A value nothing reads has no use to disagree with, so a call's unread result is held as its result slot hands it over rather than boxed for no reader.
//!
//! **An argument's demand is the storage of the position it feeds**, which is what makes this a fixpoint rather than a scan. Without that rule a loop's back-edge values have no raw use of their own — the decremented counter and the folded accumulator are *only* ever passed back round — so they would settle boxed and the loop would coerce on every iteration, losing exactly what the analysis exists to win.
//!
//! **A boundary the analysis sees whole is a join like any other.** A function that neither escapes nor is the entry is called only by `Known` calls, each naming its arguments, so each of its parameters is a join whose edges are its call sites. Its results are handed back at slots `return_call` requires every member of its tail-call class to agree on ([`TailClasses`]), so each slot is a join whose edges are the class's return edges and whose uses are the resume parameters of every call into the class. Both are decided with the same lattice as a continuation parameter, and the emitter declares the function's type at what they settle to, loads a call's arguments as an edge loads its own, and coerces a result to a resume parameter held otherwise.
//!
//! **A word holds only what lies below `2³⁰`.** A `Nat` is a reference whatever its size, and narrowing one to a word keeps every value below the i31 exactly but saturates the rest, so a value a word holds has to lie below it. What every value a site may hold lies below is a lattice of its own, `Bound`, solved before any demand is read: an operation's result lies below what its operands' bounds give (`Intrinsic::result_bound`), a literal below its own value, a join below what every argument reaching it lies below, and a reference by declaration below nothing unless erasure read its type as a word. A site is offered the word where its bound is `2³⁰` or less, so a remainder by a literal, a counter counted down from one, a byte taken apart and the fields a scan carries ride words with no rule naming them; and a word read unsigned, `Repr::Length`, where its bound is `2³²` or less, so a sequence's length, a window's fields and what a monus of one leaves ride one too. A word serves where a length is asked, and arithmetic that takes an i31 apart takes a length boxed.
//!
//! **A float register holds only what a float was handed.** A parameter has one type only where no type computes one. A variant's constructors share a payload slot, and scalar replacement makes that slot a parameter, `success`'s float and `failure`'s error in the one; and a value whose type is a match on another is a float on one edge and a string on the next. What its uses read under one tag or one branch says nothing of what arrives under another, and an edge coerces its argument to the parameter's carrier before anything is consulted, so a float register there casts the reference, and loses it where it does not trap. Only a type says which reference is a float, and erasure, the one walk holding it, states it on a binder whose producer names no carrier ([`Register`]): so a position is offered the float only when every argument reaching it is one already, a float literal, a filler, a definition producing a float, a reference stating a float, or another such position, which [`float_params`] decides beside the word. A stated reference feeds the register by one unbox on the edge or the call, and keeps its own storage: the statement is what lets it feed, never what decides how it is held.
//!
//! **A use can only take what the definition can give**, which is what [`Offer`] states. What arrives through a position no single party controls is a reference by declaration: an escaping function's parameters, which its closure wrapper passes at the shared `clsr/N` type, and the entry's, which the host passes; the results of a class whose results something else fixes; and what a cell, a channel, a call-shaped intrinsic or a host import hands back, a host's `Flt` aside. Coercing at the definition instead of excluding it was the alternative, and it buys nothing: the definition would coerce back to a register exactly what it had just been handed as a reference.
//!
//! **A captured value crosses at its own carrier.** A value free in a function's body is one value wherever it is read: lowering carries it in under its own name, lambda-lifted as a leading parameter of a directly called function or captured into a field of a closure's environment, and the emitter declares the parameter and the field at the carrier decided for the value. Its uses in every body it reaches demand of the one value, so the decision is the same wherever it stands; a closure's environment is a struct of its own, so declaring a field re-keys nothing shared.

use {
    super::{
        Atom, Bound, Callee, ContinuationId, Edge, FunctionId, Intrinsic, Lattice, Literal, Module,
        Node, Register, Repr, Solver, TailClasses, ValueExpr, ValueId, nat_is_small,
    },
    curios_abi::WireType,
    std::collections::{BTreeMap, BTreeSet},
};

/// How one value is held between its definition and its uses.
///
/// Ordered `Boxed < Raw(_) < Conflict`, with `Conflict` meaning "no single raw carrier serves every use", which the backend reads as boxed; among the raw carriers `Raw(Length) < Raw(Nat)`, since a word serves where a length is asked, every word lying below `2³⁰`. Only an `Offer::Open` value can reach the top: everything else admits exactly one carrier, so a second demand is either that same carrier or filtered out. Answering a disagreement by dropping back to `Boxed` would move a fact *down* the order, and `Solver::solve` terminates on nothing having changed — so the conservative answer has to be a third point above both, not a return to the bottom.
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
            (Storage::Raw(current), Storage::Raw(incoming)) => match (current, incoming) {
                _ if current == incoming => Storage::Raw(current),
                (Repr::Nat, Repr::Length) | (Repr::Length, Repr::Nat) => Storage::Raw(Repr::Nat),
                _ => Storage::Conflict,
            },
        }
    }
}

/// What the analysis decides: how each value is held, and how each function hands back its results.
pub struct Representation {
    /// The storage of every value the module binds.
    pub values: BTreeMap<ValueId, Storage>,
    /// The storage of each live function's results, in order: the slots of its tail-call class where something it sees decides them, and every one boxed where something else fixes them.
    pub results: BTreeMap<FunctionId, Vec<Storage>>,
}

/// What one decision is about: a value, or a result slot of a tail-call class, which binds no value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Site {
    Value(ValueId),
    /// The `slot`-th value every member of tail-call class `class` hands back.
    Result {
        class: usize,
        slot: usize,
    },
}

/// What reaches a site: an atom an edge, a call or a return hands over, or a result slot a call's resume parameter receives.
#[derive(Clone, Copy)]
enum Source<'a> {
    Atom(&'a Atom),
    Site(Site),
}

/// What a site's definition can hand to a machine register.
///
/// [`Storage`] answers what the uses want; this answers what the definition can give, and a value is held raw only where the two agree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Offer {
    /// The definition produces exactly this carrier, so a use demanding it reads the register directly and every other use coerces at its own site.
    Fixed(Repr),
    /// A join — a continuation parameter, a known function's parameter, a result slot — which has no producer of its own: it holds whatever its uses agree on among what reaches it, and each incoming argument is coerced to match. This is the only offer a `Conflict` can arise from. `words`, `lengths` and `floats` say whether the machine word, the word read unsigned and the float are among what it may hold, which the bounds and [`float_params`] decide from what reaches it.
    Open {
        words: bool,
        lengths: bool,
        floats: bool,
    },
    /// Nothing. Either the value arrives through a position that is a reference by declaration, or its definition builds a heap shape, which a register cannot hold in the first place.
    Never,
}

impl Offer {
    /// Whether this definition can hand `carrier` to a register.
    fn admits(&self, carrier: Repr) -> bool {
        match self {
            Offer::Open {
                words,
                lengths,
                floats,
            } => match carrier {
                Repr::Nat => *words,
                Repr::Length => *lengths,
                Repr::Flt => *floats,
                Repr::Number | Repr::Bytes | Repr::List | Repr::Ref => true,
            },
            // A word is a length too.
            Offer::Fixed(Repr::Nat) => matches!(carrier, Repr::Nat | Repr::Length),
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
        Literal::Bytes(_) => Repr::Bytes,
    }
}

/// The boundaries the analysis sees whole: the functions every call of which it reads, and the tail-call classes whose results only their own return edges decide.
struct Boundaries {
    classes: TailClasses,
    known: BTreeSet<FunctionId>,
    /// The function each return sentinel belongs to, so an edge into one is read as a return.
    sentinels: BTreeMap<ContinuationId, FunctionId>,
    arities: BTreeMap<FunctionId, usize>,
}

impl Boundaries {
    fn of(module: &Module) -> Self {
        let escaping = module.escaping_functions();
        let classes = TailClasses::of(module, &escaping);
        let known = module
            .live_functions()
            .map(|(function, _)| function)
            .filter(|function| !escaping.contains(function) && module.entry() != Some(*function))
            .collect();
        let sentinels = module
            .live_functions()
            .map(|(function, definition)| (definition.return_cont, function))
            .collect();
        Self {
            classes,
            known,
            sentinels,
            arities: module.return_arities(),
        }
    }

    /// The class whose results `function` hands back, where nothing but the class's own return edges decides them.
    fn open_class(&self, function: FunctionId) -> Option<usize> {
        let class = *self.classes.class_of.get(&function)?;
        (!self.classes.fixed.contains(&class)).then_some(class)
    }

    /// The result slots of every open class.
    fn result_sites(&self) -> impl Iterator<Item = Site> + '_ {
        self.arities.iter().flat_map(|(&function, &arity)| {
            self.open_class(function)
                .into_iter()
                .flat_map(move |class| (0..arity).map(move |slot| Site::Result { class, slot }))
        })
    }
}

/// Withdraw every parameter of `cont`, which receives a result the emitter hands over as a reference.
fn withdraw_params(module: &Module, cont: ContinuationId, withdrawn: &mut BTreeSet<ValueId>) {
    let Some(continuation) = module.continuation(cont) else {
        return;
    };

    withdrawn.extend(&continuation.params);
}

/// What each site's definition can hand to a register.
///
/// Offers are collected first and withdrawals applied over them at the end, rather than both being written into one map as they are found. The order matters and this is what makes it not depend on traversal: a value can perfectly well bind a scalar an intrinsic produced *and* escape into another function's body, and it must settle on the withdrawal whichever the walk reached first.
fn offers(
    module: &Module,
    boundaries: &Boundaries,
    flows: &[(Source, Site)],
) -> BTreeMap<Site, Offer> {
    let open = Offer::Open {
        words: true,
        lengths: true,
        floats: true,
    };
    let mut offers = BTreeMap::new();
    let mut withdrawn = BTreeSet::new();

    // A continuation's parameters and a known function's have no producer of their own, so each takes whatever its uses agree on; so does a result slot.
    for (_, continuation) in module.live_continuations() {
        for &param in &continuation.params {
            offers.insert(Site::Value(param), open);
        }
    }
    for &function in &boundaries.known {
        for &param in &module.function(function).unwrap().params {
            offers.insert(Site::Value(param), open);
        }
    }
    for site in boundaries.result_sites() {
        offers.insert(site, open);
    }

    for (_, node) in module.live_nodes() {
        match node {
            // A row read is the one operation whose result carrier is a fact of the module rather than of the operation: the slot it names says whether a register can hold it. A `Nat` the bounds below put under `2³⁰` is offered the word further down.
            Node::LetIntrinsic { result, op, .. } => {
                offers.insert(Site::Value(*result), offer_of(module.result_repr(op)));
            }

            Node::LetValue { result, value, .. } => {
                let offer = match value {
                    ValueExpr::Literal(literal) => offer_of(literal_repr(literal)),
                    ValueExpr::List(_) | ValueExpr::Tuple(_) | ValueExpr::Row(..) => Offer::Never,
                };
                offers.insert(Site::Value(*result), offer);
            }

            // A known call into a class its own return edges decide hands its results over at the class's slots, which the flows below read; any other call's result is a reference by the time it reaches its continuation's parameter.
            Node::ApplyFun {
                callee, return_to, ..
            } => {
                let decided = match callee {
                    Callee::Known(function) => boundaries.open_class(*function).is_some(),
                    Callee::Closure(_) => false,
                };
                if !decided {
                    withdraw_params(module, *return_to, &mut withdrawn);
                }
            }

            // What a cell operation or a call-shaped intrinsic returns is a reference too.
            Node::Cell { return_to, .. }
            | Node::Channel { return_to, .. }
            | Node::Intrinsic { return_to, .. } => {
                withdraw_params(module, *return_to, &mut withdrawn)
            }

            // A host import's results are references too, with one exception: an `Flt` is held at its carrier. Every scalar crosses back raw, and the emitter boxes an integral one at the call — a `Nat` or `Int` past the i31 into the boxed magnitude only the guest can build — and embeds a reference into the rope; but a float's box is one allocation a use may never need. So that parameter *is* the `f64`, offered at its carrier like any other definition, and a use wanting a reference boxes at its own site through the coercion every raw carrier already has.
            //
            // Withdrawing it with the rest would make an `Flt` result unrepresentable: the parameter would become a reference the call has no way to produce, and the module would fail validation with an `f64` where an `anyref` is wanted.
            Node::Foreign {
                function,
                return_to,
                ..
            } => {
                if let Some(continuation) = module.continuation(*return_to) {
                    let results = function.signature().imported_results();

                    for (index, &param) in continuation.params.iter().enumerate() {
                        match results.get(index) {
                            Some(WireType::Flt) => {
                                offers.insert(Site::Value(param), Offer::Fixed(Repr::Flt));
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

    // A function nobody calls by name alone is entered through a position that is a reference by declaration: an escaping one through its closure wrapper's `clsr/N` signature, the entry by the host. A value free in a body is not withdrawn: lowering carries it in under its own name, lambda-lifted as a parameter or captured into an environment field, and the emitter declares either at the value's own carrier.
    for (function, definition) in module.live_functions() {
        if !boundaries.known.contains(&function) {
            withdrawn.extend(&definition.params);
        }
    }

    for value in withdrawn {
        offers.insert(Site::Value(value), Offer::Never);
    }

    // What the bounds put under `2³⁰` a word holds, and under `2³²` a word read unsigned: a `Nat` an operation computes, where its lowering can settle into one, and a join every argument reaching which lies there.
    let bounds = bounds(module, &offers, flows);
    for (_, node) in module.live_nodes() {
        if let Node::LetIntrinsic { result, op, .. } = node {
            let bound = bounds[&Site::Value(*result)];
            if op.may_bound_result() && bound.is_word() {
                offers.insert(Site::Value(*result), Offer::Fixed(Repr::Nat));
            } else if op.may_hold_length() && bound.is_length() {
                offers.insert(Site::Value(*result), Offer::Fixed(Repr::Length));
            }
        }
    }
    let words = offers
        .keys()
        .copied()
        .filter(|site| bounds[site].is_word())
        .collect::<BTreeSet<_>>();
    let lengths = offers
        .keys()
        .copied()
        .filter(|site| bounds[site].is_length())
        .collect::<BTreeSet<_>>();
    let floats = float_params(module, &offers, flows);
    for (site, offer) in &mut offers {
        if let Offer::Open {
            words: word,
            lengths: length,
            floats: float,
        } = offer
        {
            *word = words.contains(site);
            *length = lengths.contains(site);
            *float = floats.contains(site);
        }
    }

    offers
}

/// Every argument the module hands to a join: an edge's to a continuation's parameters, or to its function's result slots where it returns; a known call's to its callee's parameters; and a result slot's to the resume parameters of every call into its class.
fn flows<'m>(module: &'m Module, boundaries: &Boundaries) -> Vec<(Source<'m>, Site)> {
    let mut flows = Vec::new();

    let read_edge = |edge: &'m Edge, flows: &mut Vec<(Source<'m>, Site)>| {
        if let Some(target) = module.continuation(edge.target) {
            for (arg, &param) in edge.args.iter().zip(&target.params) {
                flows.push((Source::Atom(arg), Site::Value(param)));
            }
        } else if let Some(class) = boundaries
            .sentinels
            .get(&edge.target)
            .and_then(|&function| boundaries.open_class(function))
        {
            for (slot, arg) in edge.args.iter().enumerate() {
                flows.push((Source::Atom(arg), Site::Result { class, slot }));
            }
        }
    };

    for (_, node) in module.live_nodes() {
        match node {
            Node::ApplyCont(edge) => read_edge(edge, &mut flows),
            Node::Switch { cases, default, .. } => {
                for edge in cases.values().chain(default.iter()) {
                    read_edge(edge, &mut flows);
                }
            }
            Node::ApplyFun {
                callee: Callee::Known(function),
                args,
                return_to,
            } => {
                if boundaries.known.contains(function) {
                    let params = &module.function(*function).unwrap().params;
                    for (arg, &param) in args.iter().zip(params) {
                        flows.push((Source::Atom(arg), Site::Value(param)));
                    }
                }
                // A tail call returns through its caller's own sentinel, which is no continuation: the class's results pass straight through.
                if let (Some(class), Some(resume)) = (
                    boundaries.open_class(*function),
                    module.continuation(*return_to),
                ) {
                    for (slot, &param) in resume.params.iter().enumerate() {
                        flows.push((
                            Source::Site(Site::Result { class, slot }),
                            Site::Value(param),
                        ));
                    }
                }
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

    flows
}

/// What every value a site may hold lies below ([`Bound`]): what an operation computes from what reaches it, a literal its own, a join what every argument reaching it lies below, and anything else, a reference by declaration or a value no operation of this module defines, nothing. A value whose type erasure read as a word lies below `2³⁰` whatever defines it.
fn bounds(
    module: &Module,
    offers: &BTreeMap<Site, Offer>,
    flows: &[(Source, Site)],
) -> BTreeMap<Site, Bound> {
    let mut defined = BTreeSet::new();
    for (_, node) in module.live_nodes() {
        if let Node::LetIntrinsic { result, .. } | Node::LetValue { result, .. } = node {
            defined.insert(Site::Value(*result));
        }
    }

    let facts = Solver::solve(offers.keys().copied(), |rules| {
        for (_, node) in module.live_nodes() {
            match node {
                Node::LetIntrinsic {
                    result, op, args, ..
                } => rules.add(move |solver| {
                    // A row read lies below what its slot holds: a word in a word's slot, and anything in a reference's.
                    let bound = match op {
                        Intrinsic::RowGet(..) => match module.result_repr(op) {
                            Repr::Nat => Bound::WORD,
                            _ => Bound::UNBOUNDED,
                        },
                        _ => op.result_bound(args, |atom| atom_bound(module, atom, solver)),
                    };
                    solver.join(Site::Value(*result), bound);
                }),
                Node::LetValue { result, value, .. } => {
                    let bound = match value {
                        ValueExpr::Literal(Literal::Nat(value)) => Bound::of(value),
                        _ => Bound::UNBOUNDED,
                    };
                    rules.add(move |solver| solver.join(Site::Value(*result), bound));
                }
                _ => {}
            }
        }

        for (&site, offer) in offers {
            if !matches!(offer, Offer::Open { .. }) && !defined.contains(&site) {
                rules.add(move |solver| solver.join(site, Bound::UNBOUNDED));
            }
        }

        for &(source, target) in flows {
            rules.add(move |solver| {
                let bound = match source {
                    Source::Atom(atom) => atom_bound(module, atom, solver),
                    Source::Site(site) => site_bound(module, site, solver),
                };
                solver.join(target, bound);
            });
        }
    });

    facts
        .keys()
        .map(|&site| {
            let bound = match site {
                Site::Value(value) if states(module, value, Register::Word) => {
                    facts[&site].narrower(Bound::WORD)
                }
                _ => facts[&site],
            };
            (site, bound)
        })
        .collect()
}

/// What `atom` lies below, read through the solver: a value's fact, a literal its own, a filler nothing past zero, since it holds no value, and a function nothing.
fn atom_bound(module: &Module, atom: &Atom, solver: &Solver<Bound, Site>) -> Bound {
    match atom {
        Atom::Value(value) => site_bound(module, Site::Value(*value), solver),
        Atom::Literal(Literal::Nat(value)) => Bound::of(value),
        Atom::Literal(_) | Atom::Fun(_) => Bound::UNBOUNDED,
        Atom::Filler => Bound::bottom(),
    }
}

/// What `site` lies below, read through the solver, a value whose type erasure read as a word below `2³⁰` whatever reaches it.
fn site_bound(module: &Module, site: Site, solver: &Solver<Bound, Site>) -> Bound {
    let bound = solver.fact(&site).copied().unwrap_or(Bound::UNBOUNDED);
    match site {
        Site::Value(value) if states(module, value, Register::Word) => bound.narrower(Bound::WORD),
        _ => bound,
    }
}

/// The open sites a float register may hold: those every argument reaching them is a float already — a float literal, a filler, a definition producing a float, a reference stating one, or another such site.
fn float_params(
    module: &Module,
    offers: &BTreeMap<Site, Offer>,
    flows: &[(Source, Site)],
) -> BTreeSet<Site> {
    fed_only_by(offers, flows, |arg| match arg {
        Atom::Value(value) => {
            offers.get(&Site::Value(*value)) == Some(&Offer::Fixed(Repr::Flt))
                || states(module, *value, Register::Float)
        }
        Atom::Literal(literal) => literal_repr(literal) == Repr::Flt,
        Atom::Filler => true,
        Atom::Fun(_) => false,
    })
}

/// Whether `value` states `register`: its type, read by erasure, says every value it holds is one ([`Register`]).
fn states(module: &Module, value: ValueId, register: Register) -> bool {
    module
        .value(value)
        .is_some_and(|definition| definition.register == Some(register))
}

/// The open sites every argument reaching them either `feeds` or is another such site.
///
/// A greatest fixpoint, because a loop parameter reaches itself around its back edge and a recursive function's parameter through its own call: every open site starts admitted, one reached by anything else is dropped, and each drop drops the sites it reaches in turn.
fn fed_only_by(
    offers: &BTreeMap<Site, Offer>,
    flows: &[(Source, Site)],
    feeds: impl Fn(&Atom) -> bool,
) -> BTreeSet<Site> {
    let open = |site: &Site| matches!(offers.get(site), Some(Offer::Open { .. }));
    let mut admitted = offers.keys().copied().filter(open).collect::<BTreeSet<_>>();
    let mut reaches = BTreeMap::<Site, Vec<Site>>::new();
    let mut dropped = Vec::new();

    for &(source, target) in flows {
        let from = match source {
            Source::Site(site) => Some(site),
            Source::Atom(Atom::Value(value)) => Some(Site::Value(*value)),
            Source::Atom(_) => None,
        };
        // An argument that feeds the carrier itself is asked first: a stated value feeds it by its statement, whatever the fixpoint decides of its own storage.
        match (from, source) {
            (_, Source::Atom(arg)) if feeds(arg) => {}
            (Some(site), _) if open(&site) => reaches.entry(site).or_default().push(target),
            _ => dropped.push(target),
        }
    }

    while let Some(site) = dropped.pop() {
        if admitted.remove(&site) {
            dropped.extend(reaches.get(&site).into_iter().flatten().copied());
        }
    }

    admitted
}

/// The raw carrier a representation demands, or `None` when it names a reference. A `Nat` or `Int` operand demands the word, which only a definition offering one can meet: everything else stays the reference the operand reads by default.
fn raw_carrier(repr: &Repr) -> Option<Repr> {
    match repr {
        Repr::Nat | Repr::Length | Repr::Flt => Some(*repr),
        Repr::Number => Some(Repr::Nat),
        Repr::Bytes | Repr::List | Repr::Ref => None,
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
        | WireType::Handle
        | WireType::List(_)
        | WireType::Option(_) => None,
    }
}

/// Decide the storage of every value in the module and of every function's results: what `curios-emit` may hold in a machine register, and at which carrier.
pub fn representation(module: &Module) -> Representation {
    let boundaries = Boundaries::of(module);
    let flows = flows(module, &boundaries);
    let offers = offers(module, &boundaries, &flows);
    let read = module.value_use_counts();
    let seeds = module
        .value_ids()
        .map(Site::Value)
        .chain(boundaries.result_sites())
        .collect::<Vec<_>>();

    let (offers, read) = (&offers, &read);
    let facts = Solver::solve(seeds, |rules| {
        for (_, node) in module.live_nodes() {
            rules.add(move |solver| match node {
                // The roster states what each operand position reads, and a monus held as a length reads its operands as lengths, its lowering taking each as a word read unsigned.
                Node::LetIntrinsic {
                    op, args, result, ..
                } => {
                    let as_length = *op == Intrinsic::NatSub
                        && solver.fact(&Site::Value(*result)) == Some(&Storage::Raw(Repr::Length));
                    for (index, arg) in args.iter().enumerate() {
                        let carrier = match as_length {
                            true => Some(Repr::Length),
                            false => raw_carrier(&op.operand_repr(index)),
                        };
                        demand(arg, carrier, offers, solver);
                    }
                }

                // A tag is read as a raw unsigned scalar.
                Node::Switch { scrutinee, .. } => {
                    demand(scrutinee, Some(Repr::Nat), offers, solver);
                }

                // A host call reads its scalar parameters raw and its reference parameters as shapes, whether or not it returns.
                Node::Foreign { function, args, .. } | Node::Halt { function, args } => {
                    for (arg, wire) in args.iter().zip(&function.signature().imported_params()) {
                        demand(arg, wire_carrier(wire), offers, solver);
                    }
                }

                // A row construction stores each atom into a slot whose carrier the row declares, so a scalar slot demands its atom raw — the store side of the same fact the read side offers above.
                Node::LetValue {
                    value: ValueExpr::Row(row, atoms),
                    ..
                } => {
                    for (index, atom) in atoms.iter().enumerate() {
                        demand(
                            atom,
                            raw_carrier(&module.slot_repr(*row, index)),
                            offers,
                            solver,
                        );
                    }
                }

                // Everything else stores or passes a reference, or hands its arguments to a join the flows below state: a list's elements and a tuple's fields are held uninterpreted, a closure's arguments cross its arity's uniform signature, and every cell operation works on shapes. None of these demands a raw carrier, so none contributes.
                Node::LetValue { value, .. } => match value {
                    ValueExpr::Literal(_)
                    | ValueExpr::List(_)
                    | ValueExpr::Tuple(_)
                    | ValueExpr::Row(..) => {}
                },
                Node::ApplyCont(_)
                | Node::ApplyFun { .. }
                | Node::Cell { .. }
                | Node::Channel { .. }
                | Node::Intrinsic { .. }
                | Node::LetFun { .. }
                | Node::LetCont { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            });
        }

        // A resume parameter nothing reads is held as the slot feeding it hands it over: no use disagrees with the register, and holding it boxed would be a coercion no use asked for, an allocation a call where a float result goes unread.
        for &(source, target) in &flows {
            if let (Source::Site(slot), Site::Value(param)) = (source, target)
                && !read.contains_key(&param)
            {
                rules.add(move |solver| {
                    if let Some(carrier) =
                        solver.fact(&slot).and_then(|decided| decided.raw_carrier())
                    {
                        demand_site(target, carrier, offers, solver);
                    }
                });
            }
        }

        // An argument inherits the storage of the join it feeds — the rule that carries a decision around a loop, across a call and back out of a return.
        for &(source, target) in &flows {
            rules.add(move |solver| {
                let Some(carrier) = solver
                    .fact(&target)
                    .and_then(|decided| decided.raw_carrier())
                else {
                    return;
                };
                match source {
                    Source::Atom(atom) => demand(atom, Some(carrier), offers, solver),
                    Source::Site(site) => demand_site(site, carrier, offers, solver),
                }
            });
        }
    });

    let results = module
        .live_functions()
        .map(|(function, _)| {
            let arity = boundaries.arities.get(&function).copied().unwrap_or(1);
            let slots = (0..arity)
                .map(|slot| match boundaries.open_class(function) {
                    Some(class) => facts[&Site::Result { class, slot }],
                    None => Storage::Boxed,
                })
                .collect();
            (function, slots)
        })
        .collect();
    let values = facts
        .into_iter()
        .filter_map(|(site, storage)| match site {
            Site::Value(value) => Some((value, storage)),
            Site::Result { .. } => None,
        })
        .collect();

    Representation { values, results }
}

/// Demand `carrier` of `atom`, where its definition can supply it. An atom that is not a value, a position that reads a reference, and a definition that cannot reach a register all contribute nothing.
fn demand(
    atom: &Atom,
    carrier: Option<Repr>,
    offers: &BTreeMap<Site, Offer>,
    solver: &mut Solver<Storage, Site>,
) {
    let (Atom::Value(value), Some(carrier)) = (atom, carrier) else {
        return;
    };

    demand_site(Site::Value(*value), carrier, offers, solver);
}

/// Demand `carrier` of `site`, where what defines it can supply it.
fn demand_site(
    site: Site,
    carrier: Repr,
    offers: &BTreeMap<Site, Offer>,
    solver: &mut Solver<Storage, Site>,
) {
    if offers.get(&site).is_some_and(|offer| offer.admits(carrier)) {
        solver.join(site, Storage::Raw(carrier));
    }
}

#[cfg(test)]
mod tests;
