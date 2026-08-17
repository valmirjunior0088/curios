//! Aggregate-flow census over optimized CPS — the corpus survey `documentation/roadmap/technical_debt/03_value_lifetime_cost_spec.md` names in M0.
//!
//! For every corpus program this classifies each tuple construction and rope-slice result by how its value travels: projection, continuation transfer, known-function transfer, return, closure capture, heap storage, unknown call, and mixed flow. Values that merge at a parameter are surveyed as one region, because eligibility in the spec is a property of the merged flow rather than of a single construction site — the fold accumulator's arm constructions and the loop parameter they meet at are one candidate, not five.
//! The census is a measurement, not an assertion: the ignored test prints the classification, and the machinery is pinned by the focused test below rather than by the survey's own figures.

use {
    curios_cont::{
        CpsAtom, CpsCallee, CpsContId, CpsEdge, CpsFunId, CpsIntrinsicOp, CpsModule, CpsNode,
        CpsNodeId, CpsValueExpr, CpsValueId,
    },
    curios_pipeline::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::ArenaId,
    std::collections::{BTreeMap, BTreeSet},
};

const CORPUS: [(&str, &str); 14] = [
    (
        "binary_trees",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../programs/binary_trees.crs"
        )),
    ),
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
];

/// The optimized CPS `source` compiles to, captured at the `cont-optm` stage.
fn cont_optm_module(source: &str) -> CpsModule {
    let entrypoint = source.parse::<Entrypoint>().expect("corpus program parses");

    let mut captured = None;
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
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
    Projection,
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
    tuple_sites: BTreeMap<CpsValueId, (CpsNodeId, usize)>,
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
    classes: BTreeSet<&'static str>,
    rope_consumers: BTreeSet<&'static str>,
    mixed: BTreeSet<String>,
    owners: BTreeSet<String>,
}

impl Region {
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
                    CpsNode::RecInit { ready, body, .. } => {
                        stack.push(*ready);
                        stack.push(*body);
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
                CpsAtom::Literal(_) => {}
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
                    if let CpsValueExpr::Tuple(atoms) = value {
                        self.tuple_sites.insert(*result, (node_id, atoms.len()));
                        self.home_of_value.insert(*result, Home::Node(node_id));
                    }
                    let atoms = match value {
                        CpsValueExpr::Tuple(atoms) | CpsValueExpr::List(atoms) => atoms.as_slice(),
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
                            CpsAtom::Literal(_) => {}
                        }
                    }
                }
                CpsNode::LetIntrinsic {
                    result, op, args, ..
                } => {
                    if matches!(op, CpsIntrinsicOp::BinSlice(_) | CpsIntrinsicOp::ListSlice) {
                        self.slice_sites.insert(*result, node_id);
                        self.home_of_value.insert(*result, Home::Node(node_id));
                    }
                    for (position, atom) in args.iter().enumerate() {
                        match atom {
                            CpsAtom::Value(operand) => {
                                let consumption = match (op, position) {
                                    (CpsIntrinsicOp::TplGet(_), 0) => Consumption::Projection,
                                    (CpsIntrinsicOp::BinLen(_) | CpsIntrinsicOp::ListLen, 0) => {
                                        Consumption::RopeLen
                                    }
                                    (CpsIntrinsicOp::BinGet(_) | CpsIntrinsicOp::ListGet, 0) => {
                                        Consumption::RopeGet
                                    }
                                    (
                                        CpsIntrinsicOp::BinSlice(_) | CpsIntrinsicOp::ListSlice,
                                        0,
                                    ) => Consumption::RopeSlice,
                                    _ => Consumption::OpaqueIntrinsic,
                                };
                                self.record(*operand, node_id, consumption);
                            }
                            CpsAtom::Fun(fun) => {
                                self.closure_funs.insert(*fun);
                            }
                            CpsAtom::Literal(_) => {}
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
                            CpsAtom::Literal(_) => {}
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
                CpsNode::LetFun { .. }
                | CpsNode::LetCont { .. }
                | CpsNode::RecInit { .. }
                | CpsNode::Unreachable => {}
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
                CpsAtom::Literal(_) => {}
            }
        }
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
                if let Some((node, arity)) = self.tuple_sites.get(value) {
                    region.tuple_sites.push(*value);
                    region.arities.insert(*arity);
                    if let Some(owner) = self.owner_of_node.get(node) {
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
                        Consumption::Projection => "projection",
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

/// The census over one program, printed one region per line.
fn survey(label: &str, source: &str) -> Vec<Region> {
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
            "  [{bucket}] {tier} arity {arities:?}: {ctors} ctors, {params} params, classes {classes:?}, owners {owners:?}{consumers}{mixed}",
            bucket = region.bucket(),
            arities = region.arities,
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
    regions
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
#[test]
#[ignore = "measurement: surveys the corpus rather than asserting"]
fn aggregate_flow_census() {
    let mut buckets = BTreeMap::<&'static str, usize>::new();
    let mut worker_owners = BTreeSet::new();
    let mut reachable_outside = BTreeSet::new();
    for (label, source) in CORPUS {
        for region in survey(label, source) {
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
        }
    }
    println!("== aggregate");
    println!("  buckets: {buckets:?}");
    println!("  needs-workers owners (M3 admission gate): {worker_owners:?}");
    println!("  reachable-region owners outside /std and /syn: {reachable_outside:?}");
}

/// The clone extent the specializer's budget compares, measured on the functions the value-lifetime spec names. `specialize_call_patterns` refuses a clone when the callee's copied extent plus one exceeds `BRANCH_SPECIALIZATION_GROWTH_LIMIT` (`curios-cont/src/cps/optimize.rs`), and the spec's claim that the scan-state candidate is declined *on a budget rather than on a rule* is a claim about these numbers. Run explicitly:
///
/// ```sh
/// cargo test --package curios --lib --all-features -- --ignored --nocapture step_specialization_extent
/// ```
///
/// # What it last printed
///
/// Taken **2026-08-17**, when the limit read 24: `/syn/Str/step` extent 37, `/syn/Str/classify` 52, `/std/Str/fold` 76. So specializing `step` per tag would clone 37 nodes against a budget of 24 — a refusal by less than a factor of two, confirming the budget rather than any rule is what declines it, and that raising the limit to admit `step` would also be admitting per-tag clones of everything else this size.
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

/// Pins the census machinery to the shape it exists to see: the `/std/Str/fold` accumulator — its arm constructions, its seed, and the contified loop's parameter — is surveyed as one continuation-only region. The owner is `main` rather than the fold, because a single-call-site fold is inlined before the survey runs.
#[test]
fn census_surveys_the_fold_accumulator_region() {
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
    let fold = regions
        .iter()
        .find(|region| {
            region.tuple_sites.len() >= 2
                && region.arities == BTreeSet::from([2])
                && !region.params.is_empty()
                && region.classes.contains("continuation-transfer")
                && region.classes.contains("projection")
        })
        .unwrap_or_else(|| panic!("the fold accumulator region is surveyed; got {regions:#?}"));
    assert_eq!(
        fold.bucket(),
        "continuation-only",
        "the accumulator is reachable by continuation splitting alone: {fold:#?}",
    );
}
