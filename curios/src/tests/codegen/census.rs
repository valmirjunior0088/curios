//! Aggregate-flow census over optimized CPS — the corpus survey behind `documentation/design/toolchain/a-value-costs-when-it-is-kept-not-when-it-is-named.md`.
//!
//! For every corpus program this classifies each tuple construction and rope-slice result by how its value travels: projection, continuation transfer, known-function transfer, return, closure capture, heap storage, unknown call, and mixed flow. Values that merge at a parameter are surveyed as one region, because eligibility in the spec is a property of the merged flow rather than of a single construction site — the fold accumulator's arm constructions and the loop parameter they meet at are one candidate, not five.
//! The census is a measurement, not an assertion: the ignored test prints the classification, and the machinery is pinned by the focused test below rather than by the survey's own figures.
//!
//! A region whose constructions disagree about width is reported as a *variant* beside its roster and class-merged width, and return components are surveyed apart from the regions — a variant-width return need contain no construction at all, an immediate family's bare constructor returning its payload, so a walk seeded at tuple sites would report its absence as its absence.

use {
    curios_cont::{
        CpsAtom, CpsCallee, CpsContId, CpsEdge, CpsFunId, CpsIntrinsic, CpsLiteral, CpsModule,
        CpsNode, CpsNodeId, CpsValueExpr, CpsValueId,
    },
    curios_pipeline::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::ArenaId,
    std::collections::{BTreeMap, BTreeSet},
};

const CORPUS: [(&str, &str); 14] = [
    (
        "dependent_vectors",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/dependent_vectors.crs"
        )),
    ),
    (
        "hello_world",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/hello_world.crs"
        )),
    ),
    (
        "monad_async",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/monad_async.crs"
        )),
    ),
    (
        "monad_io",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/monad_io.crs"
        )),
    ),
    (
        "monad_throw",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/monad_throw.crs"
        )),
    ),
    (
        "parse_bindless",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/parse_bindless.crs"
        )),
    ),
    (
        "parse_digits",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/parse_digits.crs"
        )),
    ),
    (
        "parse_manual",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/parse_manual.crs"
        )),
    ),
    (
        "parse_multibyte",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/parse_multibyte.crs"
        )),
    ),
    (
        "rng_manual",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/rng_manual.crs"
        )),
    ),
    (
        "rng_state",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/rng_state.crs"
        )),
    ),
    (
        "state_manual",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/state_manual.crs"
        )),
    ),
    (
        "state_monad",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/state_monad.crs"
        )),
    ),
    (
        "trees",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/trees/trees.crs"
        )),
    ),
];

/// The optimized CPS `source` compiles to, captured at the `cont-optm` stage.
fn cont_optm_module(source: &str) -> CpsModule {
    let entrypoint = source.parse::<Entrypoint>().expect("corpus program parses");

    let mut captured = None;
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::ContOptm(module) = stage {
                captured = Some(module.clone());
            }
        },
    )
    .expect("corpus program compiles");

    captured.expect("the pipeline observes cont-optm")
}

/// How one occurrence consumes the value it names, in the spec's classification vocabulary.
#[derive(Debug, Clone, Copy)]
enum Consumption {
    /// A field read, at this index. The index is carried because a variant's slot zero is its discriminant, so "is index zero ever read" is a different question from "is anything read".
    Projection(usize),
    RopeLen,
    RopeGet,
    RopeSlice,
    ContinuationTransfer(CpsContId, usize),
    KnownFunctionTransfer(CpsFunId, usize),
    Return,
    HeapStorage,
    UnknownCall,
    ForeignCall,
    CellOperation,
    ExitValue,
    Scrutinee,
    OpaqueIntrinsic,
}

/// One tuple construction, as the survey needs to read it.
///
/// The leading literal is what a variant classification rests on: `curios-ersd`'s door lowers a tagged constructor to `(tag, payload…)` with the tag a literal `Nat`, so a construction whose slot zero is a literal is one a family's discriminant travels in front of. A construction whose slot zero is a *value* is either an ordinary product or a variant the return protocol already rebuilt from fields — which is why the count of those is reported rather than assumed to be zero.
#[derive(Debug, Clone, Copy)]
struct TupleSite {
    node: CpsNodeId,
    arity: usize,
    tag: Option<u32>,
}

/// What one return edge hands back.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ReturnShape {
    /// A visible tuple construction of this arity — the only shape the return protocol can read fields off.
    Tuple(usize),
    /// One value that is not a visible construction: a bare immediate riding its family's collapsed edge, a call result, a parameter.
    Bare,
    /// Several values: a component the protocol has already split.
    Fields(usize),
}

/// Where a value is introduced, for the free-crossing check: the function whose frame it belongs to.
#[derive(Debug, Clone, Copy)]
enum Home {
    Fun(CpsFunId),
    Cont(CpsContId),
    Node(CpsNodeId),
}

/// The derived per-module facts the survey classifies over. Everything is read off the public module surface in one pass each.
struct Census<'m> {
    module: &'m CpsModule,
    owner_of_node: BTreeMap<CpsNodeId, CpsFunId>,
    owner_of_cont: BTreeMap<CpsContId, CpsFunId>,
    declared_in: BTreeMap<CpsFunId, CpsFunId>,
    sentinels: BTreeMap<CpsContId, CpsFunId>,
    result_conts: BTreeSet<CpsContId>,
    closure_funs: BTreeSet<CpsFunId>,
    uses: BTreeMap<CpsValueId, Vec<(CpsNodeId, Consumption)>>,
    tuple_sites: BTreeMap<CpsValueId, TupleSite>,
    slice_sites: BTreeMap<CpsValueId, CpsNodeId>,
    home_of_value: BTreeMap<CpsValueId, Home>,
    incoming_cont: BTreeMap<(CpsContId, usize), Vec<CpsAtom>>,
    incoming_fun: BTreeMap<(CpsFunId, usize), Vec<CpsAtom>>,
}

/// One merged flow: the constructions, slice results, and parameters a candidate value travels between, with everything its members' uses revealed.
#[derive(Debug, Default)]
struct Region {
    tuple_sites: Vec<CpsValueId>,
    slice_sites: Vec<CpsValueId>,
    params: Vec<CpsValueId>,
    arities: BTreeSet<usize>,
    /// The distinct literal discriminants the region's constructions lead with — its roster, as far as the flow reveals one.
    tags: BTreeSet<u32>,
    /// Constructions whose slot zero is not a literal, so no discriminant is readable off the construction itself.
    untagged: usize,
    /// Every field index anything in the region projects.
    reads: BTreeSet<usize>,
    classes: BTreeSet<&'static str>,
    rope_consumers: BTreeSet<&'static str>,
    mixed: BTreeSet<String>,
    owners: BTreeSet<String>,
}

impl Region {
    /// Whether the region's constructions disagree about width — the variant-width shape no exact product describes.
    fn variant(&self) -> bool {
        self.arities.len() > 1
    }

    /// The class-merged width one region would travel at: a discriminant slot plus the payload slots its widest constructor carries.
    ///
    /// It is the plain maximum because there is exactly one representation class to merge. `curios-cont` types every tuple field `(ref null any)`, so GHC's per-kind slot merge has nothing to distinguish here; the merge becomes interesting only when field representation stops being uniform, which the encoding decision names as `represent.rs`'s successor's subject rather than this one's.
    fn width(&self) -> usize {
        self.arities.last().copied().unwrap_or(0)
    }

    /// Whether the region's values come to rest or escape, rather than only travelling. The uniform-width alternative's reinstate gate reads this: padding at the door costs every value that rests, so a population that never rests is the one place paying at rest could be cheaper.
    fn rests(&self) -> bool {
        [
            "heap-storage",
            "closure-capture",
            "unknown-call",
            "foreign-call",
            "cell-operation",
            "exit",
        ]
        .iter()
        .any(|class| self.classes.contains(class))
    }

    /// The spec's four-way flow classification, which is *not* [`Region::bucket`]: a return is a blocker for continuation splitting alone and this campaign's M2 subject, so the two readings disagree on purpose and both are reported.
    fn flow(&self) -> &'static str {
        let eligible: BTreeSet<&'static str> = [
            "projection",
            "continuation-transfer",
            "known-function-transfer",
            "return",
        ]
        .into();
        if !self.mixed.is_empty()
            || !self.slice_sites.is_empty()
            || self.classes.iter().any(|class| !eligible.contains(class))
        {
            "blocked"
        } else if self.classes.contains("known-function-transfer") {
            "known-call"
        } else if self.classes.contains("return") {
            "return"
        } else {
            "continuation-only"
        }
    }

    /// Which of the spec's mechanisms reaches this region, if any: continuation splitting alone, splitting plus known-function workers, or neither.
    fn bucket(&self) -> &'static str {
        let eligible: BTreeSet<&'static str> = if self.slice_sites.is_empty() {
            [
                "projection",
                "continuation-transfer",
                "known-function-transfer",
            ]
            .into()
        } else {
            [
                "rope-len",
                "rope-get",
                "rope-slice",
                "continuation-transfer",
                "known-function-transfer",
            ]
            .into()
        };
        if !self.mixed.is_empty()
            || self.arities.len() > 1
            || (!self.tuple_sites.is_empty() && !self.slice_sites.is_empty())
            || self.classes.iter().any(|class| !eligible.contains(class))
        {
            "blocked"
        } else if self.classes.contains("known-function-transfer") {
            "needs-workers"
        } else {
            "continuation-only"
        }
    }

    fn outside_std(&self) -> bool {
        self.owners
            .iter()
            .any(|owner| !owner.contains("/std/") && !owner.contains("/syn/"))
    }
}

impl<'m> Census<'m> {
    fn of(module: &'m CpsModule) -> Self {
        let mut census = Self {
            module,
            owner_of_node: BTreeMap::new(),
            owner_of_cont: BTreeMap::new(),
            declared_in: BTreeMap::new(),
            sentinels: BTreeMap::new(),
            result_conts: BTreeSet::new(),
            closure_funs: BTreeSet::new(),
            uses: BTreeMap::new(),
            tuple_sites: BTreeMap::new(),
            slice_sites: BTreeMap::new(),
            home_of_value: BTreeMap::new(),
            incoming_cont: BTreeMap::new(),
            incoming_fun: BTreeMap::new(),
        };
        census.walk_owners();
        census.walk_uses();
        census
    }

    /// Assign every node and continuation to the function whose body reaches it, record return sentinels, and record parameter homes.
    fn walk_owners(&mut self) {
        let module = self.module;
        for (index, slot) in module.functions().iter().enumerate() {
            let Some(function) = slot else { continue };
            let fun = CpsFunId::from_index(index);
            self.sentinels.insert(function.return_cont, fun);
            for param in &function.params {
                self.home_of_value.insert(*param, Home::Fun(fun));
            }

            let mut stack = vec![function.body];
            while let Some(node_id) = stack.pop() {
                if self.owner_of_node.insert(node_id, fun).is_some() {
                    continue;
                }
                match module.node(node_id).expect("live node") {
                    CpsNode::LetValue { next, .. } | CpsNode::LetIntrinsic { next, .. } => {
                        stack.push(*next);
                    }
                    CpsNode::LetFun { functions, body } => {
                        for declared in functions {
                            self.declared_in.entry(*declared).or_insert(fun);
                        }
                        stack.push(*body);
                    }
                    CpsNode::LetCont {
                        continuations,
                        body,
                    } => {
                        stack.push(*body);
                        for cont in continuations {
                            self.owner_of_cont.insert(*cont, fun);
                            if let Some(continuation) = module.continuation(*cont) {
                                for param in &continuation.params {
                                    self.home_of_value.insert(*param, Home::Cont(*cont));
                                }
                                stack.push(continuation.body);
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    fn record(&mut self, value: CpsValueId, node: CpsNodeId, consumption: Consumption) {
        self.uses
            .entry(value)
            .or_default()
            .push((node, consumption));
    }

    /// One edge's arguments: continuation transfers, or the return class when the target is a sentinel. Incoming atoms are recorded per parameter position for the exclusivity check.
    fn record_edge(&mut self, node: CpsNodeId, edge: &CpsEdge) {
        let returning = self.sentinels.contains_key(&edge.target);
        for (position, atom) in edge.args.iter().enumerate() {
            match atom {
                CpsAtom::Value(value) => {
                    let consumption = if returning {
                        Consumption::Return
                    } else {
                        Consumption::ContinuationTransfer(edge.target, position)
                    };
                    self.record(*value, node, consumption);
                }
                CpsAtom::Fun(fun) => {
                    self.closure_funs.insert(*fun);
                }
                CpsAtom::Literal(_) | CpsAtom::Filler => {}
            }
            if !returning {
                self.incoming_cont
                    .entry((edge.target, position))
                    .or_default()
                    .push(atom.clone());
            }
        }
    }

    /// Classify every operand occurrence in the module, and record the candidate construction sites.
    fn walk_uses(&mut self) {
        let module = self.module;
        for (index, slot) in module.nodes().iter().enumerate() {
            let Some(node) = slot else { continue };
            let node_id = CpsNodeId::from_index(index);
            match node {
                CpsNode::LetValue { result, value, .. } => {
                    // A tagged constructor is a `Variant` since family keying, and it is exactly the shape this census counts as a tuple site — the tag still sits at slot 0.
                    if let CpsValueExpr::Tuple(atoms) | CpsValueExpr::Row(_, atoms) = value {
                        let tag = match atoms.first() {
                            Some(CpsAtom::Literal(CpsLiteral::Nat(tag))) => Some(*tag),
                            _ => None,
                        };
                        self.tuple_sites.insert(
                            *result,
                            TupleSite {
                                node: node_id,
                                arity: atoms.len(),
                                tag,
                            },
                        );
                        self.home_of_value.insert(*result, Home::Node(node_id));
                    }
                    let atoms = match value {
                        CpsValueExpr::Tuple(atoms)
                        | CpsValueExpr::List(atoms)
                        | CpsValueExpr::Row(_, atoms) => atoms.as_slice(),
                        CpsValueExpr::Literal(_) => &[],
                    };
                    for atom in atoms {
                        match atom {
                            CpsAtom::Value(operand) => {
                                self.record(*operand, node_id, Consumption::HeapStorage);
                            }
                            CpsAtom::Fun(fun) => {
                                self.closure_funs.insert(*fun);
                            }
                            CpsAtom::Literal(_) | CpsAtom::Filler => {}
                        }
                    }
                }
                CpsNode::LetIntrinsic {
                    result, op, args, ..
                } => {
                    if matches!(op, CpsIntrinsic::BinSlice(_) | CpsIntrinsic::ListSlice) {
                        self.slice_sites.insert(*result, node_id);
                        self.home_of_value.insert(*result, Home::Node(node_id));
                    }
                    for (position, atom) in args.iter().enumerate() {
                        match atom {
                            CpsAtom::Value(operand) => {
                                let consumption = match (op, position) {
                                    (CpsIntrinsic::TupleGet(field), 0) => {
                                        Consumption::Projection(*field)
                                    }
                                    (CpsIntrinsic::BinLen(_) | CpsIntrinsic::ListLen, 0) => {
                                        Consumption::RopeLen
                                    }
                                    (CpsIntrinsic::BinGet(_) | CpsIntrinsic::ListGet, 0) => {
                                        Consumption::RopeGet
                                    }
                                    (CpsIntrinsic::BinSlice(_) | CpsIntrinsic::ListSlice, 0) => {
                                        Consumption::RopeSlice
                                    }
                                    _ => Consumption::OpaqueIntrinsic,
                                };
                                self.record(*operand, node_id, consumption);
                            }
                            CpsAtom::Fun(fun) => {
                                self.closure_funs.insert(*fun);
                            }
                            CpsAtom::Literal(_) | CpsAtom::Filler => {}
                        }
                    }
                }
                CpsNode::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => {
                    self.result_conts.insert(*return_to);
                    let known = match callee {
                        CpsCallee::Known(fun) => Some(*fun),
                        CpsCallee::Closure(closure) => {
                            self.record(*closure, node_id, Consumption::UnknownCall);
                            None
                        }
                    };
                    for (position, atom) in args.iter().enumerate() {
                        match atom {
                            CpsAtom::Value(operand) => match known {
                                Some(fun) => {
                                    self.record(
                                        *operand,
                                        node_id,
                                        Consumption::KnownFunctionTransfer(fun, position),
                                    );
                                }
                                None => self.record(*operand, node_id, Consumption::UnknownCall),
                            },
                            CpsAtom::Fun(fun) => {
                                self.closure_funs.insert(*fun);
                            }
                            CpsAtom::Literal(_) | CpsAtom::Filler => {}
                        }
                        if let Some(fun) = known {
                            self.incoming_fun
                                .entry((fun, position))
                                .or_default()
                                .push(atom.clone());
                        }
                    }
                }
                CpsNode::ApplyCont(edge) => self.record_edge(node_id, edge),
                CpsNode::Switch {
                    scrutinee,
                    cases,
                    default,
                } => {
                    if let CpsAtom::Value(value) = scrutinee {
                        self.record(*value, node_id, Consumption::Scrutinee);
                    }
                    for edge in cases.values().chain(default.as_ref()) {
                        self.record_edge(node_id, edge);
                    }
                }
                CpsNode::Foreign {
                    args, return_to, ..
                } => {
                    self.result_conts.insert(*return_to);
                    self.record_atoms(node_id, args, Consumption::ForeignCall);
                }
                CpsNode::Cell {
                    args, return_to, ..
                } => {
                    self.result_conts.insert(*return_to);
                    self.record_atoms(node_id, args, Consumption::CellOperation);
                }
                CpsNode::Intrinsic {
                    args, return_to, ..
                } => {
                    self.result_conts.insert(*return_to);
                    self.record_atoms(node_id, args, Consumption::UnknownCall);
                }
                CpsNode::Exit { value } => {
                    if let Some(CpsAtom::Value(value)) = value {
                        self.record(*value, node_id, Consumption::ExitValue);
                    }
                    if let Some(CpsAtom::Fun(fun)) = value {
                        self.closure_funs.insert(*fun);
                    }
                }
                CpsNode::LetFun { .. } | CpsNode::LetCont { .. } | CpsNode::Unreachable => {}
            }
        }
    }

    fn record_atoms(&mut self, node: CpsNodeId, atoms: &[CpsAtom], consumption: Consumption) {
        for atom in atoms {
            match atom {
                CpsAtom::Value(value) => self.record(*value, node, consumption),
                CpsAtom::Fun(fun) => {
                    self.closure_funs.insert(*fun);
                }
                CpsAtom::Literal(_) | CpsAtom::Filler => {}
            }
        }
    }

    /// What each live function's return edges hand back, one entry per distinct shape.
    ///
    /// Return components are surveyed apart from the merged regions above because a variant-width *return* need not contain a construction at all: an immediate family's bare constructor returns its payload, which seeds no region and would be invisible to a walk that starts at tuple sites. This is the measurement M2's gate reads.
    fn return_shapes(&self) -> BTreeMap<CpsFunId, BTreeSet<ReturnShape>> {
        let mut shapes = BTreeMap::<CpsFunId, BTreeSet<ReturnShape>>::new();
        let mut record = |owner: CpsFunId, edge: &CpsEdge, census: &Self| {
            let shape = match edge.args.as_slice() {
                [CpsAtom::Value(value)] => match census.tuple_sites.get(value) {
                    Some(site) => ReturnShape::Tuple(site.arity),
                    None => ReturnShape::Bare,
                },
                [_] => ReturnShape::Bare,
                several => ReturnShape::Fields(several.len()),
            };
            shapes.entry(owner).or_default().insert(shape);
        };
        for slot in self.module.nodes().iter() {
            let Some(node) = slot else { continue };
            let edges: Vec<&CpsEdge> = match node {
                CpsNode::ApplyCont(edge) => vec![edge],
                CpsNode::Switch { cases, default, .. } => {
                    cases.values().chain(default.as_ref()).collect()
                }
                _ => continue,
            };
            for edge in edges {
                if let Some(owner) = self.sentinels.get(&edge.target).copied() {
                    record(owner, edge, self);
                }
            }
        }
        shapes
    }

    /// The live nodes cloning `fun` would copy: its own body's, plus — recursively — those of every function declared inside it, which is the accounting `curios-cont`'s `copied_extent` prices a specialization clone at.
    fn extent(&self, fun: CpsFunId) -> usize {
        let own = self
            .owner_of_node
            .values()
            .filter(|owner| **owner == fun)
            .count();
        own + self
            .declared_in
            .iter()
            .filter(|(_, parent)| **parent == fun)
            .map(|(child, _)| self.extent(*child))
            .sum::<usize>()
    }

    fn owner_name(&self, fun: CpsFunId) -> String {
        self.module
            .function(fun)
            .and_then(|function| function.debug_name.clone())
            .unwrap_or_else(|| fun.to_string())
    }

    fn home_owner(&self, home: Home) -> Option<CpsFunId> {
        match home {
            Home::Fun(fun) => Some(fun),
            Home::Cont(cont) => self.owner_of_cont.get(&cont).copied(),
            Home::Node(node) => self.owner_of_node.get(&node).copied(),
        }
    }

    /// Group every candidate and every parameter it flows through into merged regions, then classify each region's uses, ownership, and exclusivity.
    fn regions(&self) -> Vec<Region> {
        // Union-find over aggregate transfers: a value and the parameter receiving it are one flow.
        let mut parent = BTreeMap::<CpsValueId, CpsValueId>::new();
        fn find(parent: &mut BTreeMap<CpsValueId, CpsValueId>, value: CpsValueId) -> CpsValueId {
            let mut root = value;
            while let Some(next) = parent.get(&root).copied() {
                root = next;
            }
            let mut walk = value;
            while let Some(next) = parent.get(&walk).copied() {
                parent.insert(walk, root);
                walk = next;
            }
            root
        }
        let union = |parent: &mut BTreeMap<CpsValueId, CpsValueId>, a: CpsValueId, b| {
            let (a, b) = (find(parent, a), find(parent, b));
            if a != b {
                parent.insert(a, b);
            }
        };
        for (value, occurrences) in &self.uses {
            for (_, consumption) in occurrences {
                let param = match consumption {
                    Consumption::ContinuationTransfer(cont, position) => self
                        .module
                        .continuation(*cont)
                        .and_then(|continuation| continuation.params.get(*position))
                        .copied(),
                    Consumption::KnownFunctionTransfer(fun, position) => self
                        .module
                        .function(*fun)
                        .and_then(|function| function.params.get(*position))
                        .copied(),
                    _ => None,
                };
                if let Some(param) = param {
                    union(&mut parent, *value, param);
                }
            }
        }

        // Collect the members of every flow containing a candidate site.
        let mut members = BTreeMap::<CpsValueId, Vec<CpsValueId>>::new();
        let all: BTreeSet<CpsValueId> = self
            .tuple_sites
            .keys()
            .chain(self.slice_sites.keys())
            .chain(parent.keys())
            .copied()
            .collect();
        for value in all {
            let root = find(&mut parent, value);
            members.entry(root).or_default().push(value);
        }

        let mut regions = Vec::new();
        for group in members.into_values() {
            let mut region = Region::default();
            for value in &group {
                if let Some(site) = self.tuple_sites.get(value) {
                    region.tuple_sites.push(*value);
                    region.arities.insert(site.arity);
                    match site.tag {
                        Some(tag) => {
                            region.tags.insert(tag);
                        }
                        None => region.untagged += 1,
                    }
                    if let Some(owner) = self.owner_of_node.get(&site.node) {
                        region.owners.insert(self.owner_name(*owner));
                    }
                } else if let Some(node) = self.slice_sites.get(value) {
                    region.slice_sites.push(*value);
                    if let Some(owner) = self.owner_of_node.get(node) {
                        region.owners.insert(self.owner_name(*owner));
                    }
                } else if let Some(home) = self.home_of_value.get(value) {
                    region.params.push(*value);
                    if let Some(owner) = self.home_owner(*home) {
                        region.owners.insert(self.owner_name(owner));
                    }
                }
            }
            if region.tuple_sites.is_empty() && region.slice_sites.is_empty() {
                continue;
            }

            let group_set: BTreeSet<CpsValueId> = group.iter().copied().collect();
            for value in &group {
                let home = self.home_of_value.get(value).copied();
                for (node, consumption) in self.uses.get(value).map_or(&[][..], Vec::as_slice) {
                    // A use in a function the value does not belong to is a free-variable crossing: the emitter will capture it in a closure environment.
                    let use_owner = self.owner_of_node.get(node).copied();
                    if let (Some(home), Some(use_owner)) =
                        (home.and_then(|home| self.home_owner(home)), use_owner)
                        && home != use_owner
                    {
                        region.classes.insert("closure-capture");
                    }
                    let class = match consumption {
                        Consumption::Projection(field) => {
                            region.reads.insert(*field);
                            "projection"
                        }
                        Consumption::RopeLen => "rope-len",
                        Consumption::RopeGet => "rope-get",
                        Consumption::RopeSlice => "rope-slice",
                        Consumption::ContinuationTransfer(_, _) => "continuation-transfer",
                        Consumption::KnownFunctionTransfer(fun, _) => {
                            if self.closure_funs.contains(fun) {
                                region.mixed.insert(format!("{fun} has unknown entries"));
                            }
                            "known-function-transfer"
                        }
                        Consumption::Return => "return",
                        Consumption::HeapStorage => "heap-storage",
                        Consumption::UnknownCall => "unknown-call",
                        Consumption::ForeignCall => "foreign-call",
                        Consumption::CellOperation => "cell-operation",
                        Consumption::ExitValue => "exit",
                        Consumption::Scrutinee => "scrutinee",
                        Consumption::OpaqueIntrinsic => "opaque-intrinsic",
                    };
                    region.classes.insert(class);
                    if matches!(
                        consumption,
                        Consumption::RopeLen | Consumption::RopeGet | Consumption::RopeSlice
                    ) {
                        region.rope_consumers.insert(class);
                    }
                }
            }

            // Exclusivity: every edge into a reached parameter must carry a member; anything else is a merge with a flow the rewrite cannot see.
            for param in &region.params {
                match self.home_of_value.get(param) {
                    Some(Home::Cont(cont)) => {
                        let position = self
                            .module
                            .continuation(*cont)
                            .and_then(|continuation| {
                                continuation.params.iter().position(|p| p == param)
                            })
                            .expect("reached parameter exists");
                        if self.result_conts.contains(cont) {
                            region.mixed.insert(format!("{cont} receives call results"));
                        }
                        for atom in self
                            .incoming_cont
                            .get(&(*cont, position))
                            .map_or(&[][..], Vec::as_slice)
                        {
                            match atom {
                                CpsAtom::Value(value) if group_set.contains(value) => {}
                                other => {
                                    region
                                        .mixed
                                        .insert(format!("{cont} also receives {other:?}"));
                                }
                            }
                        }
                    }
                    Some(Home::Fun(fun)) => {
                        let position = self
                            .module
                            .function(*fun)
                            .and_then(|function| function.params.iter().position(|p| p == param))
                            .expect("reached parameter exists");
                        if self.closure_funs.contains(fun) {
                            region.mixed.insert(format!("{fun} has unknown entries"));
                        }
                        if Some(*fun) == self.module.entry() {
                            region.mixed.insert(format!("{fun} is the module entry"));
                        }
                        for atom in self
                            .incoming_fun
                            .get(&(*fun, position))
                            .map_or(&[][..], Vec::as_slice)
                        {
                            match atom {
                                CpsAtom::Value(value) if group_set.contains(value) => {}
                                other => {
                                    region
                                        .mixed
                                        .insert(format!("{fun} also receives {other:?}"));
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }

            regions.push(region);
        }
        regions
    }
}

/// One program's survey: its merged regions, and the return components beside them.
struct Survey {
    regions: Vec<Region>,
    /// Functions whose return edges disagree about shape, by the shapes they carry.
    mixed_returns: BTreeMap<String, (BTreeSet<ReturnShape>, bool)>,
}

/// The census over one program, printed one region per line.
fn survey(label: &str, source: &str) -> Survey {
    let module = cont_optm_module(source);
    let census = Census::of(&module);
    let regions = census.regions();
    println!(
        "== {label}: {tuples} tuple sites, {slices} slice sites, {count} regions",
        tuples = census.tuple_sites.len(),
        slices = census.slice_sites.len(),
        count = regions.len(),
    );
    for region in &regions {
        let tier = if region.slice_sites.is_empty() {
            "tuple"
        } else {
            "rope"
        };
        println!(
            "  [{bucket}/{flow}] {tier}{variant} arity {arities:?}{roster}: {ctors} ctors, {params} params, classes {classes:?}, owners {owners:?}{consumers}{mixed}",
            bucket = region.bucket(),
            flow = region.flow(),
            variant = if region.variant() { " variant" } else { "" },
            arities = region.arities,
            roster = if region.variant() {
                format!(
                    " width {} tags {:?}{}",
                    region.width(),
                    region.tags,
                    match region.untagged {
                        0 => String::new(),
                        n => format!(" +{n} untagged"),
                    }
                )
            } else {
                String::new()
            },
            ctors = region.tuple_sites.len() + region.slice_sites.len(),
            params = region.params.len(),
            classes = region.classes,
            owners = region.owners,
            consumers = if region.rope_consumers.is_empty() {
                String::new()
            } else {
                format!(", rope consumers {:?}", region.rope_consumers)
            },
            mixed = if region.mixed.is_empty() {
                String::new()
            } else {
                format!(", mixed {:?}", region.mixed)
            },
        );
    }

    // Escapement is reported beside the shapes because it is the *other* reason the return protocol declines a component, and a mixed-shape component that also escapes is not evidence for a variant-width mechanism.
    let mixed_returns = census
        .return_shapes()
        .into_iter()
        .filter(|(_, shapes)| shapes.len() > 1)
        .map(|(fun, shapes)| {
            let escapes = census.closure_funs.contains(&fun);
            (census.owner_name(fun), (shapes, escapes))
        })
        .collect::<BTreeMap<_, _>>();
    for (owner, (shapes, escapes)) in &mixed_returns {
        println!(
            "  [return] {owner} hands back {shapes:?}{}",
            if *escapes { " (escapes)" } else { "" }
        );
    }

    Survey {
        regions,
        mixed_returns,
    }
}

/// The M0 corpus survey. Run explicitly:
///
/// ```sh
/// cargo test --package curios --all-features -- codegen::census::aggregate_flow_census --ignored --nocapture
/// ```
///
/// # What it last printed
///
/// Taken **2026-08-17**, after the `/std` scan-spelling sweep, over the fourteen-program corpus: 852 regions — 747 blocked, 62 continuation-only, 31 needs-workers. Most regions repeat across programs because they live in `/std`'s shared plumbing; the per-program counts are stable at about 71 tuple sites and 17 slice sites each.
///
/// The readings the value-lifetime campaign proceeds on:
///
/// - The `/std/Str/fold` accumulator is `[continuation-only] tuple arity {2}: 5 ctors, 1 params, {continuation-transfer, projection}` in every string-walking program — M2's acceptance case is eligible exactly as specified.
/// - The scan-state flow is `[blocked] tuple arity {1}` with every parameter mixed by `receives call results`: after the sweep the scan circulates through `step`'s returns rather than being rebuilt, so the region dissolves through the return protocol (M1a) and continuation splitting (M2), not through worker signatures. Before the sweep the same region carried arity `{1, 4}` — the nullary constructors lower to 1-tuples and `cont` to a 4-tuple, so no exact product shape ever described it.
/// - The suffix-view rope region is `[blocked]` with `{rope-get, rope-len, rope-slice}` consumers and one continuation also receiving an empty-`Bytes` literal beside whole originals — the `Empty`/`Whole`/`Proper` mixing M4's descriptor form exists to carry.
/// - The needs-workers bucket — M3's admission gate — is owned by `/std/Async/resume_after/2`, `/std/Async/run_guards/1`, `/std/Async/serve/1/1`, `/std/Handle/write/1`, `io/bind`, and `main`; the scan state is *not* in it, for the shape reason above.
/// - Reachable regions outside `/std` and `/syn` are owned by `io/bind` and the programs' own `main`s.
///
/// Retaken after M2 landed (same day): 681 regions — 574 blocked, 50 continuation-only, 57 needs-workers. Continuation scalar replacement dissolved about 170 regions corpus-wide, the fold accumulator's among them; what it left concentrates the surviving continuation-only candidates near the growth ceiling or behind arity mixing, and promotes some previously blocked flows into needs-workers as their continuation legs cleared.
///
/// Retaken after M4 landed (same day): the buckets read the same by coincidence of sums, but the slice sites underneath them halved — 19 to 10 in each standard string program, 41 to 29 in `monad_async` — because the window split virtualizes a suffix walk's views before the survey sees them.
///
/// # The variant-width survey
///
/// Taken **2026-08-17**, over the same corpus, before anything of the variant-width campaign had landed. Thirty-eight variant regions — a region whose constructions disagree about width — in three flow classes: 14 continuation-only, 12 known-call, 12 blocked, and **none at all in the return class**.
///
/// Underneath those thirty-eight there are only **three distinct shapes**, each repeated once per program that reaches it, because every one of them lives in shared `/std` plumbing:
///
/// - `(roster 2, width 2)`, ×14, owned by `main` and `io/bind` — an `Option` merging its nullary and unary constructors at a join.
/// - `(roster 2, width 3)`, ×12, blocked by parameters that receive call results.
/// - `(roster 2, width 4)`, ×12 — **the UTF-8 scan**, printed as `tuple variant arity {1, 4} width 4 tags {0, 2} +7 untagged: 17 ctors, 2 params, classes {continuation-transfer, known-function-transfer, projection}`, owned by `/std/Str/fold`, `/std/Str/utf8/check`, `/std/Str/utf8/drop_width` and `main`. The tags are `lead` and `bad` riding 1-tuples; the seven untagged constructions are the resumes' rebuilds, whose slot zero is a *parameter* rather than a literal because `split_returns` already delivers the scan as fields.
///
/// That last line is the campaign's central reading and it corrects the specification twice. The discriminant is **not** a literal on every edge — a rebuilt return carries it as a parameter — so an origin lattice that demanded a tag-led construction would decline the one flow the campaign exists for. And the scan's flow class is **known-call**, not continuation-only: the region crosses a known call, so continuation splitting alone leaves a materialization at that boundary rather than clearing the per-character path.
///
/// The class merge is degenerate, which is why no width budget is selected here: `curios-cont` types every tuple field `(ref null any)`, so there is exactly one representation class and the class-merged width is the plain maximum arity. The largest observed is 4, against a `PARAM_SPLIT_GROWTH_LIMIT` of 16, so the existing ceiling clears every candidate and no second budget was invented to sit beside it.
///
/// **The M2 gate — variant-width return components — is three functions corpus-wide, and none of them is evidence for a return-side mechanism.** `/decode/1` and `/std/Nat/of_str/1` hand back `{Tuple(1), Tuple(2)}` and both *escape*, so the protocol declines them for the escaping reason and no width class would reach them. `/build` hands back `{Tuple(4), Bare}` — the immediate-family shape the encoding decision created, and the acceptance case M2 was written for — but a `trees` node is stored in its parent, so its values rest: splitting that return relocates the allocation into the caller rather than removing it, which is the reboxing balance the specification's own M3 clause names as disqualifying.
///
/// **The uniform-width alternative's gate** reports both never-resting populations as the two flows above (`tags {0,1} width 2` ×14, `tags {0,2} width 4` ×12) — so padding at the door would be paid by no stored value in this corpus, but it would also buy nothing that in-flight splitting does not, since neither population rests either way.
///
/// **The boxed-tag annex**, for the encoding decision's deferred item: 552 tag-led constructions, of which 429 are never read back at slot zero *within their own merged flow*. Read that as an upper bound rather than a count of dead tags — a construction that comes to rest and is discriminated after a reload is in no flow at all, and this instrument cannot see the read.
///
/// Retaken once the campaign had landed (same day): **12 variant regions, every one of them blocked.** The continuation-only and known-call classes are empty, which is the acceptance case stated as absence — every variant-width flow the corpus reaches now travels as fields. The survivors are the `(2, 3)` shape, blocked by parameters that receive call results and by an unknown call, and the buckets underneath fell with them: 573 blocked to 546 and 57 needs-workers to 28. The return gate is unchanged at three functions, which is the point of refusing that milestone rather than deferring it.
#[test]
#[ignore = "measurement: surveys the corpus rather than asserting"]
fn aggregate_flow_census() {
    let mut buckets = BTreeMap::<&'static str, usize>::new();
    let mut worker_owners = BTreeSet::new();
    let mut reachable_outside = BTreeSet::new();

    let mut variants = 0usize;
    let mut variant_flows = BTreeMap::<&'static str, usize>::new();
    let mut variant_shapes = BTreeMap::<(usize, usize), usize>::new();
    let mut variant_owners = BTreeMap::<&'static str, BTreeSet<String>>::new();
    let mut restless = BTreeMap::<String, usize>::new();
    let mut tagged_constructions = 0usize;
    let mut unread_tags = 0usize;

    let mut mixed_returns = BTreeMap::<String, (BTreeSet<ReturnShape>, bool)>::new();
    for (label, source) in CORPUS {
        let survey = survey(label, source);
        mixed_returns.extend(survey.mixed_returns);
        for region in survey.regions {
            *buckets.entry(region.bucket()).or_default() += 1;
            // The M3 admission gate reads the regions a worker signature would newly reach — not every region that happens to also cross a known call before being blocked by something else.
            if region.bucket() == "needs-workers" {
                worker_owners.extend(region.owners.iter().cloned());
            }
            if region.bucket() != "blocked" && region.outside_std() {
                reachable_outside.extend(
                    region
                        .owners
                        .iter()
                        .filter(|owner| !owner.contains("/std/") && !owner.contains("/syn/"))
                        .cloned(),
                );
            }

            // The boxed-tag annex the encoding decision deferred: a construction leading with a literal discriminant that nothing in its flow ever reads back. Counted over tag-led constructions rather than over families, because Cont holds no family identity — the tag is what survives the door.
            if !region.tags.is_empty() {
                tagged_constructions += region.tuple_sites.len() - region.untagged;
                if !region.reads.contains(&0) {
                    unread_tags += region.tuple_sites.len() - region.untagged;
                }
            }

            if !region.variant() {
                continue;
            }
            variants += 1;
            *variant_flows.entry(region.flow()).or_default() += 1;
            *variant_shapes
                .entry((region.arities.len(), region.width()))
                .or_default() += 1;
            variant_owners
                .entry(region.flow())
                .or_default()
                .extend(region.owners.iter().cloned());
            if !region.rests() {
                *restless
                    .entry(format!("tags {:?} width {}", region.tags, region.width()))
                    .or_default() += 1;
            }
        }
    }

    println!("== aggregate");
    println!("  buckets: {buckets:?}");
    println!("  needs-workers owners (M3 admission gate): {worker_owners:?}");
    println!("  reachable-region owners outside /std and /syn: {reachable_outside:?}");
    println!("== variant-width");
    println!("  variant regions: {variants}");
    println!("  by flow class: {variant_flows:?}");
    println!("  by (roster size, class-merged width): {variant_shapes:?}");
    println!(
        "  known-call owners (M3 admission gate): {:?}",
        variant_owners
            .get("known-call")
            .cloned()
            .unwrap_or_default(),
    );
    println!(
        "  continuation-only owners (M1): {:?}",
        variant_owners
            .get("continuation-only")
            .cloned()
            .unwrap_or_default(),
    );
    println!(
        "  return owners (M2): {:?}",
        variant_owners.get("return").cloned().unwrap_or_default(),
    );
    println!("  populations that never rest (uniform-width gate): {restless:?}");
    println!(
        "  tag-led constructions: {tagged_constructions}, of them never read back: {unread_tags}"
    );
    println!(
        "  variant-width return components (M2 admission gate): {}",
        mixed_returns.len()
    );
    for (owner, (shapes, escapes)) in &mixed_returns {
        println!(
            "    {owner}: {shapes:?}{}",
            if *escapes { " (escapes)" } else { "" }
        );
    }
}

/// The clone extent the specializer's budget compares, measured on the functions the value-lifetime decision names. `specialize_call_patterns` refuses a clone when the callee's copied extent plus one exceeds `BRANCH_SPECIALIZATION_GROWTH_LIMIT` (`curios-cont/src/cps/optimize.rs`), and the claim that the scan-state candidate is declined *on a budget rather than on a rule* is a claim about these numbers. Run explicitly:
///
/// ```sh
/// cargo test --package curios --lib --all-features -- --ignored --nocapture step_specialization_extent
/// ```
///
/// # What it last printed
///
/// Taken **2026-08-17**, when the limit read 24: `/syn/Str/step` extent 37, `/syn/Str/classify` 52, `/std/Str/fold` 76. So specializing `step` per tag would clone 37 nodes against a budget of 24 — a refusal by less than a factor of two, confirming the budget rather than any rule is what declines it, and that raising the limit to admit `step` would also be admitting per-tag clones of everything else this size. Retaken after M1a and M2 (same day): step 30, classify 52, fold 70 — the split protocol and the fields split slimmed both walkers, and the refusal stands. Retaken again after variant-width splitting (same day): step 26, classify 52, fold 63 — the refusal stands by a wider margin, and the reason it no longer matters is that the reconstruction the specializer was being weighed against is gone.
#[test]
#[ignore = "measurement: reports the extents the specializer's budget compares"]
fn step_specialization_extent() {
    let source = CORPUS
        .iter()
        .find(|(label, _)| *label == "parse_multibyte")
        .expect("the multi-byte fixture is in the corpus")
        .1;
    let module = cont_optm_module(source);
    let census = Census::of(&module);
    for (index, slot) in module.functions().iter().enumerate() {
        let Some(function) = slot else { continue };
        let Some(name) = &function.debug_name else {
            continue;
        };
        if ["/syn/Str/step", "/syn/Str/classify", "/std/Str/fold"]
            .iter()
            .any(|hint| name.contains(hint))
        {
            let fun = CpsFunId::from_index(index);
            println!("{name}: extent {}", census.extent(fun));
        }
    }
}

/// Pins the census machinery to the shapes the string walk still carries — and to the two it no longer does. The accumulator region dissolved when continuation scalar replacement landed (M2), and the suffix-view rope region dissolved when the window split landed (M4): both absences are the campaign's acceptance echoed by its own instrument. What survives, until a variant-width capability exists, is the scan-state flow: a blocked tuple region whose parameters receive call results.
#[test]
fn surveys_the_fold_accumulator_region() {
    let source = r#"
        use /std/{Handle, Nat, Str, Char, List, proc};
        let taint = List/len(proc/args!);
        let n : Nat = taint;
        let text : Str = Nat/to_str(n);
        /std/print(Nat/to_str(Str/fold(text, 0, (codepoint, acc) => acc + Char/to_nat(codepoint))))
        "#;
    let module = cont_optm_module(source);
    let census = Census::of(&module);
    let regions = census.regions();

    let accumulator = regions.iter().find(|region| {
        region.tuple_sites.len() >= 2
            && region.arities == BTreeSet::from([2])
            && !region.params.is_empty()
            && region.classes.contains("continuation-transfer")
            && region.classes.contains("projection")
    });
    assert!(
        accumulator.is_none(),
        "the accumulator region dissolved into fields before the survey: {accumulator:#?}",
    );

    let rope = regions.iter().find(|region| {
        region.slice_sites.len() >= 2
            && !region.params.is_empty()
            && region.classes.contains("continuation-transfer")
    });
    assert!(
        rope.is_none(),
        "the suffix-view region virtualized into window fields before the survey: {rope:#?}",
    );

    // What survives in this all-ASCII fixture is the Io plumbing no mechanism claims — enough to pin that the machinery still surveys; the variant-width scan flow needs multi-byte text and is the corpus survey's to report.
    assert!(!regions.is_empty(), "the census still surveys the walk");
}

/// The death-birth tally for one program — the churn campaign's census instrument (its verdicts retired into `the-heap-is-sized-ahead-of-its-churn.md`), the sibling of [`survey`]. Within one function, a construction of some layout beside a value of matching layout whose every use takes it apart is the pairing Perceus turns into an in-place write and a tracing collector re-allocates. The classifier locates that population per substrate; it proves no pairing — order inside the function is deliberately not consulted, so every count is an upper bound on what a reuse mechanism could establish.
#[derive(Debug, Default)]
struct Rebirth {
    /// Tuple constructions in the program.
    tuple_sites: usize,
    /// Constructions beside a co-resident dying value whose exact constructed arity matches.
    constructed_pairs: usize,
    /// Constructions beside a co-resident dying value whose projected width matches — the cross-frame shape: `chain`'s dying cell was the caller's construction, so its width arrives through the projections that take it apart.
    projected_pairs: usize,
    /// The pairs above owned under `/std/Map` — the map-spine substrate.
    spine_pairs: usize,
    /// Rope extends: appends, and every operand a concat merges.
    extends: usize,
    /// Extends whose base's only use is that extend — the linearly threaded builder lever B recognizes.
    linear_extends: usize,
    /// The functions that carry pairs, for locating concentrations.
    owners: BTreeSet<String>,
}

/// A value whose every recorded use takes it apart — projection or scrutinee — inside one function: the death half of a pair, with its width known exactly when it is itself a visible construction and by its widest projection otherwise.
struct Dying {
    value: CpsValueId,
    fun: CpsFunId,
    constructed: Option<usize>,
    projected: Option<usize>,
}

fn rebirth(census: &Census) -> Rebirth {
    let mut tally = Rebirth::default();

    let mut dying: Vec<Dying> = Vec::new();
    for (value, uses) in &census.uses {
        if uses.is_empty() {
            continue;
        }
        let deconstructive = uses
            .iter()
            .all(|(_, use_)| matches!(use_, Consumption::Projection(_) | Consumption::Scrutinee));
        if !deconstructive {
            continue;
        }
        let mut owners = uses
            .iter()
            .map(|(node, _)| census.owner_of_node.get(node).copied());
        let Some(Some(fun)) = owners.next() else {
            continue;
        };
        if !owners.all(|owner| owner == Some(fun)) {
            continue;
        }
        dying.push(Dying {
            value: *value,
            fun,
            constructed: census.tuple_sites.get(value).map(|site| site.arity),
            projected: uses
                .iter()
                .filter_map(|(_, use_)| match use_ {
                    Consumption::Projection(field) => Some(field + 1),
                    _ => None,
                })
                .max(),
        });
    }

    for (value, site) in &census.tuple_sites {
        let Some(fun) = census.owner_of_node.get(&site.node) else {
            continue;
        };
        tally.tuple_sites += 1;

        let constructed = dying.iter().any(|death| {
            death.value != *value && death.fun == *fun && death.constructed == Some(site.arity)
        });
        let projected = dying.iter().any(|death| {
            death.value != *value
                && death.fun == *fun
                && death.constructed.is_none()
                && death.projected == Some(site.arity)
        });
        if constructed {
            tally.constructed_pairs += 1;
        }
        if projected {
            tally.projected_pairs += 1;
        }
        if constructed || projected {
            let owner = census.owner_name(*fun);
            if owner.contains("/std/Map") {
                tally.spine_pairs += 1;
            }
            tally.owners.insert(owner);
        }
    }

    // The rope substrate needs no pairing search: an extend consumes its base into a same-layout successor by construction, so the only question is whether the base was threaded linearly — one use, this extend — or shared.
    for (index, slot) in census.module.nodes().iter().enumerate() {
        let Some(node) = slot else { continue };
        let node_id = CpsNodeId::from_index(index);
        let CpsNode::LetIntrinsic { op, args, .. } = node else {
            continue;
        };
        let bases: &[CpsAtom] = match op {
            CpsIntrinsic::BinAppend(_) | CpsIntrinsic::ListAppend => &args[..1],
            CpsIntrinsic::BinConcat(_, _) | CpsIntrinsic::ListConcat(_) => args.as_slice(),
            _ => continue,
        };
        for base in bases {
            let CpsAtom::Value(base) = base else { continue };
            tally.extends += 1;
            let uses = census.uses.get(base).map_or(&[][..], Vec::as_slice);
            if let [(node, _)] = uses
                && *node == node_id
            {
                tally.linear_extends += 1;
            }
        }
    }

    tally
}

/// The M0 workloads — the corpus's cross-language entries, so the classifier reads the programs the results files time.
const WORKLOADS: [(&str, &str); 3] = [
    (
        "chain",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/chain/chain.crs"
        )),
    ),
    (
        "churn",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/churn/churn.crs"
        )),
    ),
    (
        "spines",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/spines/spines.crs"
        )),
    ),
];

/// The in-corpus spine-churn consumer the specification's evidence names: a driver that pulls the whole TOML decoder — and with it `/std/Map`'s table construction — into the surveyed module. The classifier only compiles it; nothing runs.
pub(in crate::tests) const TOML_DRIVER: &str = r#"
use /std/{Str, Nat, Map, Toml, Option, Result, Io};

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) =>
        match Toml/decode(s): (_) => Io({})
        | success(m) => /std/print(Nat/to_str(Map/len(m)))
        | failure(e) => /std/print(e)
        end
    | none() => /std/print("invalid utf-8")
    end
| none() => /std/print("no input")
end
"#;

/// The death-birth census. Run explicitly:
///
/// ```sh
/// cargo test --package curios --all-features -- codegen::census::death_birth_census --ignored --nocapture
/// ```
///
/// # What it last printed
///
/// Taken **2026-08-17**, over the fourteen-program corpus, the three M0 workloads, and the TOML driver:
///
/// ```text
/// == death-birth totals: 1168 tuple sites, 0 constructed-width pairs, 509 projected-width pairs, 24 under /std/Map; 136/256 extends linear
/// ```
///
/// with `chain` at 16 pairs of 46 sites, `spines` at 24 of 59 with 9 under `/std/Map`, and the TOML driver at 313 of 470 with 70 of its 125 extends linear.
///
/// # The reading
///
/// The population is real and pervasive — 509 of 1168 constructions stand beside a dying matching-width value, so the specification's stop-evidence clause (the population rare outside the workloads) does not fire. It is also *entirely* the cross-frame shape: zero constructed-width pairs means no dying value pairs with a construction from its own function — every death arrives as a parameter taken apart where the matching birth happens, which is exactly a tail-recursive rebuild loop, and which any reuse mechanism keyed to intra-function allocation sites would miss completely. The map-spine substrate concentrates where the specification's evidence said: `/std/Map`'s `insert`/`insert_node`/`replace` and the TOML decoder's build and scan functions, the decoder alone holding three fifths of all pairs. And lever B's admission population exists: over half of all rope extends are linearly threaded — the base's only use is the extend that consumes it.
#[test]
#[ignore = "measurement: surveys the corpus rather than asserting"]
fn death_birth_census() {
    let mut owners = BTreeSet::new();
    let mut totals = Rebirth::default();

    for (label, source) in CORPUS
        .iter()
        .chain(WORKLOADS.iter())
        .chain([("toml_decode", TOML_DRIVER)].iter())
    {
        let module = cont_optm_module(source);
        let census = Census::of(&module);
        let tally = rebirth(&census);

        println!(
            "  {label}: {sites} tuple sites, {constructed} constructed-width pairs, {projected} projected-width pairs, {spines} under /std/Map; {linear}/{extends} extends linear",
            sites = tally.tuple_sites,
            constructed = tally.constructed_pairs,
            projected = tally.projected_pairs,
            spines = tally.spine_pairs,
            linear = tally.linear_extends,
            extends = tally.extends,
        );

        totals.tuple_sites += tally.tuple_sites;
        totals.constructed_pairs += tally.constructed_pairs;
        totals.projected_pairs += tally.projected_pairs;
        totals.spine_pairs += tally.spine_pairs;
        totals.extends += tally.extends;
        totals.linear_extends += tally.linear_extends;
        owners.extend(tally.owners);
    }

    println!(
        "== death-birth totals: {sites} tuple sites, {constructed} constructed-width pairs, {projected} projected-width pairs, {spines} under /std/Map; {linear}/{extends} extends linear",
        sites = totals.tuple_sites,
        constructed = totals.constructed_pairs,
        projected = totals.projected_pairs,
        spines = totals.spine_pairs,
        linear = totals.linear_extends,
        extends = totals.extends,
    );
    println!("== pair owners: {owners:?}");
}

/// The classifier pinned on the canonical pair, so the census above stays a measurement: a cons rebuilt from a predecessor dying in the same function pairs at projected width, and a program whose constructions never see a dying same-layout value reports none.
#[test]
fn death_birth_classifier_pins_the_canonical_pair() {
    let paired = r#"
        use /std/{Str, Nat, Option, Io};
        induct Chain: Type
        | nil()
        | cons(Nat, Chain)
        end
        rec step(rest: Chain, acc: Chain) -> Chain =
            match rest: (_) => Chain
            | nil() => acc
            | cons(v, tail) => step(tail, Chain/cons(v + 1, acc))
            end;
        rec build(n: Nat, acc: Chain) -> Chain =
            match n: (_) => Chain
            | 0 => acc
            | m + 1; ih => build(m, Chain/cons(n, acc))
            end;
        rec total(c: Chain, acc: Nat) -> Nat =
            match c: (_) => Nat
            | nil() => acc
            | cons(v, tail) => total(tail, (acc + v) % 1000003)
            end;
        let input = /std/read()!;
        match input: (_) => Io({})
        | some(bytes) =>
            match Str/of_bytes(bytes): (_) => Io({})
            | some(s) =>
                match Nat/of_str(Str/trim(s)): (_) => Io({})
                | some(n) => /std/print(Nat/to_str(total(step(build(n, Chain/nil()), Chain/nil()), 0)))
                | none() => /std/print("bad input")
                end
            | none() => /std/print("invalid utf-8")
            end
        | none() => /std/print("no input")
        end
    "#;
    let module = cont_optm_module(paired);
    let census = Census::of(&module);
    let tally = rebirth(&census);
    assert!(
        tally.projected_pairs >= 1,
        "the rebuilt cons beside its dying predecessor pairs: {tally:?}",
    );
}
