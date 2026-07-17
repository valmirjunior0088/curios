//! Arena-backed high CPS.
//!
//! The surface of this module is intentionally small: Ersd lowering constructs a
//! [`CpsModule`], the optimizer mutates that graph through its checked mutation API,
//! and backend lowering consumes it. Stable integer identities, tombstoned arena
//! entries, and deterministic traversal are representation invariants rather than
//! optimizer conventions. Use information is derived on demand (see
//! [`CpsModule::value_use_counts`]) rather than maintained as a shadow arena.

use {
    curios_abi::ForeignFunction,
    curios_base::{Grain, PackedBin},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
        sync::Arc,
    },
};

macro_rules! id {
    ($name:ident, $prefix:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub(crate) u32);

        impl $name {
            pub fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl fmt::Display for $name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!($prefix, "{}"), self.0)
            }
        }
    };
}

id!(CpsNodeId, "n");
id!(CpsValueId, "%v");
id!(CpsFunId, "@f");
id!(CpsContId, "@k");

impl CpsFunId {
    pub(crate) fn from_index(index: usize) -> Self {
        Self(index as u32)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CpsLiteral {
    Nat(u32),
    Int(i32),
    Flt(f32),
    Bin(Grain, PackedBin),
}

#[derive(Debug, Clone, PartialEq)]
pub enum CpsAtom {
    Value(CpsValueId),
    Fun(CpsFunId),
    Literal(CpsLiteral),
}

#[derive(Debug, Clone)]
pub enum CpsValueExpr {
    Literal(CpsLiteral),
    List(Vec<CpsAtom>),
    Tuple(Vec<CpsAtom>),
}

/// Primitive identity without operands. Operand order and arity live on the
/// surrounding `LetPrim`, so every analysis sees one uniform operand vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CpsPrimOp {
    NatEql,
    NatNeq,
    NatAdd,
    NatSub,
    NatMul,
    NatLt,
    NatDiv,
    NatRem,
    NatGt,
    NatLte,
    NatGte,
    NatAnd,
    NatOr,
    NatXor,
    NatShl,
    NatShr,
    NatRotl,
    NatRotr,
    NatClz,
    NatCtz,
    NatPopcnt,
    NatEqz,
    NatToInt,
    NatToFlt,
    IntEql,
    IntNeq,
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntRem,
    IntLt,
    IntGt,
    IntLte,
    IntGte,
    IntAnd,
    IntOr,
    IntXor,
    IntShl,
    IntShr,
    IntRotl,
    IntRotr,
    IntClz,
    IntCtz,
    IntPopcnt,
    IntEqz,
    IntToNat,
    IntToFlt,
    FltAdd,
    FltSub,
    FltMul,
    FltDiv,
    FltRem,
    FltEql,
    FltNeq,
    FltLt,
    FltGt,
    FltLte,
    FltGte,
    FltMin,
    FltMax,
    FltNeg,
    FltAbs,
    FltSqrt,
    FltFloor,
    FltCeil,
    FltTrunc,
    FltNearest,
    FltCopysign,
    FltToNat,
    FltToLeBytes,
    FltOfLeBytes,
    FltToInt,
    BinLen(Grain),
    BinEql(Grain),
    BinGet(Grain),
    BinSlice(Grain),
    BinAppend(Grain),
    BinConcat(Grain, usize),
    LstLen,
    LstGet,
    LstSlice,
    LstAppend,
    LstConcat(usize),
    TplGet(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsPrimitiveEffect {
    Total,
    MayTrap,
    Allocates,
}

impl CpsPrimOp {
    pub fn arity(self) -> usize {
        match self {
            Self::NatClz
            | Self::NatCtz
            | Self::NatPopcnt
            | Self::NatEqz
            | Self::NatToInt
            | Self::NatToFlt
            | Self::IntClz
            | Self::IntCtz
            | Self::IntPopcnt
            | Self::IntEqz
            | Self::IntToNat
            | Self::IntToFlt
            | Self::FltNeg
            | Self::FltAbs
            | Self::FltSqrt
            | Self::FltFloor
            | Self::FltCeil
            | Self::FltTrunc
            | Self::FltNearest
            | Self::FltToNat
            | Self::FltToLeBytes
            | Self::FltOfLeBytes
            | Self::FltToInt
            | Self::BinLen(_)
            | Self::LstLen
            | Self::TplGet(_) => 1,
            Self::BinSlice(_) | Self::LstSlice => 3,
            Self::BinConcat(_, arity) | Self::LstConcat(arity) => arity,
            _ => 2,
        }
    }

    pub fn effect(self) -> CpsPrimitiveEffect {
        match self {
            Self::NatDiv
            | Self::NatRem
            | Self::IntDiv
            | Self::IntRem
            | Self::FltToNat
            | Self::FltToInt
            | Self::BinGet(_)
            | Self::BinSlice(_)
            | Self::LstGet
            | Self::LstSlice
            | Self::TplGet(_)
            | Self::NatAdd
            | Self::NatMul
            | Self::NatShl
            | Self::NatRotl
            | Self::IntAdd
            | Self::IntSub
            | Self::IntMul => CpsPrimitiveEffect::MayTrap,
            Self::BinAppend(_)
            | Self::BinConcat(_, _)
            | Self::LstAppend
            | Self::LstConcat(_)
            | Self::FltToLeBytes => CpsPrimitiveEffect::Allocates,
            _ => CpsPrimitiveEffect::Total,
        }
    }

    pub fn is_total(self) -> bool {
        self.effect() == CpsPrimitiveEffect::Total
    }

    pub fn may_trap(self) -> bool {
        self.effect() == CpsPrimitiveEffect::MayTrap
    }

    pub fn allocates(self) -> bool {
        self.effect() == CpsPrimitiveEffect::Allocates
    }

    pub fn is_commutative(self) -> bool {
        matches!(
            self,
            Self::NatEql
                | Self::NatNeq
                | Self::NatAdd
                | Self::NatMul
                | Self::NatAnd
                | Self::NatOr
                | Self::NatXor
                | Self::IntEql
                | Self::IntNeq
                | Self::IntAdd
                | Self::IntMul
                | Self::IntAnd
                | Self::IntOr
                | Self::IntXor
        )
    }

    pub fn cse_eligible(self) -> bool {
        self.is_total() && !self.allocates()
    }

    pub fn loop_motion_eligible(self) -> bool {
        self.is_total() && !self.allocates()
    }
}

#[derive(Debug, Clone)]
pub enum CpsCallee {
    Known(CpsFunId),
    Closure(CpsValueId),
}

#[derive(Debug, Clone)]
pub struct CpsEdge {
    pub target: CpsContId,
    pub args: Vec<CpsAtom>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsCellOp {
    New,
    Set,
    Get,
}

impl CpsCellOp {
    pub fn operand_arity(self) -> usize {
        match self {
            Self::New | Self::Get => 1,
            Self::Set => 2,
        }
    }

    pub fn result_arity(self) -> usize {
        match self {
            Self::New | Self::Get => 1,
            Self::Set => 0,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CpsIntrinsicOp {
    LstMap,
}

#[derive(Debug, Clone)]
pub enum CpsNode {
    LetValue {
        result: CpsValueId,
        value: CpsValueExpr,
        next: CpsNodeId,
    },
    LetPrim {
        result: CpsValueId,
        op: CpsPrimOp,
        args: Vec<CpsAtom>,
        next: CpsNodeId,
    },
    LetFun {
        functions: Vec<CpsFunId>,
        body: CpsNodeId,
    },
    LetCont {
        continuations: Vec<CpsContId>,
        body: CpsNodeId,
    },
    ApplyFun {
        callee: CpsCallee,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    ApplyCont(CpsEdge),
    Switch {
        scrutinee: CpsAtom,
        cases: BTreeMap<u32, CpsEdge>,
        default: Option<CpsEdge>,
    },
    Foreign {
        function: Arc<ForeignFunction>,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    Cell {
        op: CpsCellOp,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    Intrinsic {
        op: CpsIntrinsicOp,
        args: Vec<CpsAtom>,
        return_to: CpsContId,
    },
    Exit {
        value: Option<CpsAtom>,
    },
    Unreachable,
    RecInit {
        functions: Vec<CpsFunId>,
        values: Vec<CpsValueId>,
        ready: CpsNodeId,
        body: CpsNodeId,
    },
}

#[derive(Debug, Clone)]
pub struct CpsValueDef {
    pub debug_name: Option<String>,
    pub candidate: bool,
}

#[derive(Debug, Clone)]
pub struct CpsFunction {
    pub debug_name: Option<String>,
    pub params: Vec<CpsValueId>,
    pub return_cont: CpsContId,
    pub body: CpsNodeId,
}

#[derive(Debug, Clone)]
pub struct CpsContinuation {
    pub debug_name: Option<String>,
    pub params: Vec<CpsValueId>,
    pub body: CpsNodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum CpsUseTarget {
    Value(CpsValueId),
    Fun(CpsFunId),
    Cont(CpsContId),
}

#[derive(Debug, Clone)]
pub struct CpsVerifyError(pub String);

impl fmt::Display for CpsVerifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

impl std::error::Error for CpsVerifyError {}

/// The production Cont representation. Arena slots never move or get reused;
/// deletion writes `None` and deterministic compaction is explicit.
#[derive(Debug, Clone, Default)]
pub struct CpsModule {
    nodes: Vec<Option<CpsNode>>,
    values: Vec<Option<CpsValueDef>>,
    functions: Vec<Option<CpsFunction>>,
    continuations: Vec<Option<CpsContinuation>>,
    entry: Option<CpsFunId>,
}

impl CpsModule {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn entry(&self) -> Option<CpsFunId> {
        self.entry
    }

    pub fn set_entry(&mut self, entry: CpsFunId) {
        self.entry = Some(entry);
    }

    pub fn nodes(&self) -> &[Option<CpsNode>] {
        &self.nodes
    }

    pub fn values(&self) -> &[Option<CpsValueDef>] {
        &self.values
    }

    pub fn functions(&self) -> &[Option<CpsFunction>] {
        &self.functions
    }

    pub fn continuations(&self) -> &[Option<CpsContinuation>] {
        &self.continuations
    }

    pub fn node(&self, id: CpsNodeId) -> Option<&CpsNode> {
        self.nodes.get(id.index()).and_then(Option::as_ref)
    }

    pub fn function(&self, id: CpsFunId) -> Option<&CpsFunction> {
        self.functions.get(id.index()).and_then(Option::as_ref)
    }

    pub fn continuation(&self, id: CpsContId) -> Option<&CpsContinuation> {
        self.continuations.get(id.index()).and_then(Option::as_ref)
    }

    /// Count, per value, how many times it is referenced across the module.
    /// A value's use sites are its operand occurrences plus its use as an
    /// indirect callee; definitions (`LetValue`/`LetPrim` results, parameters)
    /// are not uses, so an unreferenced value is absent from the map. Derived on
    /// demand rather than maintained incrementally.
    pub(crate) fn value_use_counts(&self) -> BTreeMap<CpsValueId, usize> {
        let mut counts = BTreeMap::new();
        for node in self.nodes.iter().flatten() {
            for atom in atoms(node) {
                if let CpsAtom::Value(value) = atom {
                    *counts.entry(*value).or_insert(0) += 1;
                }
            }
            if let CpsNode::ApplyFun {
                callee: CpsCallee::Closure(value),
                ..
            } = node
            {
                *counts.entry(*value).or_insert(0) += 1;
            }
        }
        counts
    }

    pub fn reserve_node(&mut self) -> CpsNodeId {
        let id = CpsNodeId(self.nodes.len() as u32);
        self.nodes.push(None);
        id
    }

    pub fn add_node(&mut self, node: CpsNode) -> CpsNodeId {
        let id = self.reserve_node();
        self.define_node(id, node);
        id
    }

    pub fn define_node(&mut self, id: CpsNodeId, node: CpsNode) {
        let slot = self
            .nodes
            .get_mut(id.index())
            .unwrap_or_else(|| panic!("unknown node {id}"));
        assert!(slot.is_none(), "node {id} is already defined");
        *slot = Some(node);
    }

    pub fn add_value(&mut self, debug_name: Option<String>, candidate: bool) -> CpsValueId {
        let id = CpsValueId(self.values.len() as u32);
        self.values.push(Some(CpsValueDef {
            debug_name,
            candidate,
        }));
        id
    }

    pub fn reserve_function(&mut self, debug_name: Option<String>) -> CpsFunId {
        let id = CpsFunId(self.functions.len() as u32);
        self.functions.push(None);
        if let Some(name) = debug_name {
            // Retain the name in the eventual definition; reserving does not
            // create a parallel metadata arena.
            let _ = name;
        }
        id
    }

    pub fn define_function(&mut self, id: CpsFunId, function: CpsFunction) {
        let slot = self
            .functions
            .get_mut(id.index())
            .unwrap_or_else(|| panic!("unknown function {id}"));
        assert!(slot.is_none(), "function {id} is already defined");
        *slot = Some(function);
    }

    pub fn add_function(&mut self, function: CpsFunction) -> CpsFunId {
        let id = CpsFunId(self.functions.len() as u32);
        self.functions.push(Some(function));
        id
    }

    pub fn reserve_continuation(&mut self) -> CpsContId {
        let id = CpsContId(self.continuations.len() as u32);
        self.continuations.push(None);
        id
    }

    pub fn define_continuation(&mut self, id: CpsContId, continuation: CpsContinuation) {
        let slot = self
            .continuations
            .get_mut(id.index())
            .unwrap_or_else(|| panic!("unknown continuation {id}"));
        assert!(slot.is_none(), "continuation {id} is already defined");
        *slot = Some(continuation);
    }

    pub fn add_continuation(&mut self, continuation: CpsContinuation) -> CpsContId {
        let id = CpsContId(self.continuations.len() as u32);
        self.continuations.push(Some(continuation));
        id
    }

    pub fn remove_node(&mut self, id: CpsNodeId) -> Option<CpsNode> {
        self.nodes.get_mut(id.index())?.take()
    }

    pub fn replace_atom(&mut self, from: CpsUseTarget, replacement: CpsAtom) {
        for node in self.nodes.iter_mut().flatten() {
            visit_atoms_mut(node, &mut |atom| {
                let matches = match (&from, &*atom) {
                    (CpsUseTarget::Value(a), CpsAtom::Value(b)) => a == b,
                    (CpsUseTarget::Fun(a), CpsAtom::Fun(b)) => a == b,
                    _ => false,
                };
                if matches {
                    *atom = replacement.clone();
                }
            });
        }
    }

    pub fn tombstones(&self) -> (usize, usize, usize, usize) {
        let return_continuations = self
            .functions
            .iter()
            .flatten()
            .map(|function| function.return_cont)
            .collect::<BTreeSet<_>>();
        (
            self.nodes.iter().filter(|slot| slot.is_none()).count(),
            self.values.iter().filter(|slot| slot.is_none()).count(),
            self.functions.iter().filter(|slot| slot.is_none()).count(),
            self.continuations
                .iter()
                .enumerate()
                .filter(|(index, slot)| {
                    slot.is_none() && !return_continuations.contains(&CpsContId(*index as u32))
                })
                .count(),
        )
    }

    pub fn verify(&self) -> Result<(), CpsVerifyError> {
        let entry = self
            .entry
            .ok_or_else(|| CpsVerifyError("module has no entry function".into()))?;
        self.require_fun(entry, "entry")?;

        let mut returns = BTreeMap::<CpsContId, CpsFunId>::new();
        for (index, function) in self.functions.iter().enumerate() {
            let Some(function) = function else { continue };
            let id = CpsFunId(index as u32);
            if function.return_cont.index() >= self.continuations.len() {
                return Err(CpsVerifyError(format!(
                    "{id} return continuation {} was not minted by this module",
                    function.return_cont
                )));
            }
            if self.continuation(function.return_cont).is_some() {
                return Err(CpsVerifyError(format!(
                    "{id} return continuation {} also identifies a local continuation",
                    function.return_cont
                )));
            }
            if let Some(previous) = returns.insert(function.return_cont, id) {
                return Err(CpsVerifyError(format!(
                    "{} is the return continuation of both {previous} and {id}",
                    function.return_cont
                )));
            }
            self.require_node(function.body, "function body")?;
            for &param in &function.params {
                self.require_value(param, "function parameter")?;
            }
        }

        for continuation in self.continuations.iter().flatten() {
            self.require_node(continuation.body, "continuation body")?;
            for &param in &continuation.params {
                self.require_value(param, "continuation parameter")?;
            }
        }

        let mut node_owners = BTreeMap::<CpsNodeId, CpsFunId>::new();
        let mut bound_continuations = BTreeSet::<CpsContId>::new();
        for (index, function) in self.functions.iter().enumerate() {
            let Some(function) = function else { continue };
            self.verify_function_body(
                CpsFunId(index as u32),
                function,
                &returns,
                &mut node_owners,
                &mut bound_continuations,
            )?;
        }
        self.verify_lexical_scopes(entry)?;

        let live_nodes = self
            .nodes
            .iter()
            .enumerate()
            .filter_map(|(index, node)| node.as_ref().map(|_| CpsNodeId(index as u32)))
            .collect::<BTreeSet<_>>();
        let owned_nodes = node_owners.keys().copied().collect::<BTreeSet<_>>();
        if live_nodes != owned_nodes {
            return Err(CpsVerifyError(
                "node arena contains an unowned node or an owner references a tombstone".into(),
            ));
        }

        let live_continuations = self
            .continuations
            .iter()
            .enumerate()
            .filter_map(|(index, continuation)| {
                continuation.as_ref().map(|_| CpsContId(index as u32))
            })
            .collect::<BTreeSet<_>>();
        if live_continuations != bound_continuations {
            return Err(CpsVerifyError(
                "local-continuation arena and lexical LetCont bindings disagree".into(),
            ));
        }

        Ok(())
    }

    fn verify_lexical_scopes(&self, entry: CpsFunId) -> Result<(), CpsVerifyError> {
        type NodeTask = (
            CpsFunId,
            CpsNodeId,
            BTreeSet<CpsValueId>,
            BTreeSet<CpsFunId>,
            BTreeSet<CpsContId>,
        );

        let mut bound_functions = BTreeSet::from([entry]);
        let mut bound_values = BTreeSet::new();
        let mut pending_recursive_values = BTreeSet::new();
        let mut function_work = vec![(entry, BTreeSet::new(), BTreeSet::from([entry]))];
        let mut node_work = Vec::<NodeTask>::new();
        let mut visited_nodes = BTreeSet::new();

        while !function_work.is_empty() || !node_work.is_empty() {
            while let Some((function, mut values, functions)) = function_work.pop() {
                let definition = self.function(function).unwrap();
                for value in &definition.params {
                    if !bound_values.insert(*value) {
                        return Err(CpsVerifyError(format!(
                            "function parameter {value} is bound more than once"
                        )));
                    }
                    values.insert(*value);
                }
                node_work.push((
                    function,
                    definition.body,
                    values,
                    functions,
                    BTreeSet::new(),
                ));
            }

            let Some((owner, node_id, values, functions, continuations)) = node_work.pop() else {
                continue;
            };
            if !visited_nodes.insert(node_id) {
                continue;
            }
            let node = self.node(node_id).unwrap();
            for atom in atoms(node) {
                match atom {
                    CpsAtom::Value(value) if !values.contains(value) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {value}"
                        )));
                    }
                    CpsAtom::Fun(function) if !functions.contains(function) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} uses out-of-scope {function}"
                        )));
                    }
                    CpsAtom::Value(_) | CpsAtom::Fun(_) | CpsAtom::Literal(_) => {}
                }
            }
            if let CpsNode::ApplyFun { callee, .. } = node {
                match callee {
                    CpsCallee::Known(function) if !functions.contains(function) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {function}"
                        )));
                    }
                    CpsCallee::Closure(value) if !values.contains(value) => {
                        return Err(CpsVerifyError(format!(
                            "{owner} node {node_id} calls out-of-scope {value}"
                        )));
                    }
                    CpsCallee::Known(_) | CpsCallee::Closure(_) => {}
                }
            }

            match node {
                CpsNode::LetValue { result, next, .. } | CpsNode::LetPrim { result, next, .. } => {
                    if !bound_values.insert(*result) {
                        return Err(CpsVerifyError(format!(
                            "node result {result} is bound more than once"
                        )));
                    }
                    let mut inner = values;
                    inner.insert(*result);
                    node_work.push((owner, *next, inner, functions, continuations));
                }
                CpsNode::LetFun {
                    functions: members,
                    body,
                } => {
                    let mut inner = functions;
                    for function in members {
                        if !bound_functions.insert(*function) {
                            return Err(CpsVerifyError(format!(
                                "function {function} is bound more than once"
                            )));
                        }
                        inner.insert(*function);
                    }
                    for function in members.iter().rev() {
                        function_work.push((*function, values.clone(), inner.clone()));
                    }
                    node_work.push((owner, *body, values, inner, continuations));
                }
                CpsNode::RecInit {
                    functions: members,
                    values: recursive_values,
                    body,
                    ..
                } => {
                    let mut inner_functions = functions;
                    for function in members {
                        if !bound_functions.insert(*function) {
                            return Err(CpsVerifyError(format!(
                                "function {function} is bound more than once"
                            )));
                        }
                        inner_functions.insert(*function);
                    }
                    let mut inner_values = values;
                    for value in recursive_values {
                        if !bound_values.insert(*value) {
                            return Err(CpsVerifyError(format!(
                                "recursive value {value} is bound more than once"
                            )));
                        }
                        pending_recursive_values.insert(*value);
                        inner_values.insert(*value);
                    }
                    for function in members.iter().rev() {
                        function_work.push((
                            *function,
                            inner_values.clone(),
                            inner_functions.clone(),
                        ));
                    }
                    node_work.push((owner, *body, inner_values, inner_functions, continuations));
                }
                CpsNode::LetCont {
                    continuations: members,
                    body,
                } => {
                    let mut inner = continuations;
                    inner.extend(members.iter().copied());
                    for continuation in members.iter().rev() {
                        let definition = self.continuation(*continuation).unwrap();
                        let mut continuation_values = values.clone();
                        for value in &definition.params {
                            if !bound_values.insert(*value)
                                && !pending_recursive_values.remove(value)
                            {
                                return Err(CpsVerifyError(format!(
                                    "continuation parameter {value} is bound more than once"
                                )));
                            }
                            continuation_values.insert(*value);
                        }
                        node_work.push((
                            owner,
                            definition.body,
                            continuation_values,
                            functions.clone(),
                            inner.clone(),
                        ));
                    }
                    node_work.push((owner, *body, values, functions, inner));
                }
                CpsNode::ApplyFun { .. }
                | CpsNode::ApplyCont(_)
                | CpsNode::Switch { .. }
                | CpsNode::Foreign { .. }
                | CpsNode::Cell { .. }
                | CpsNode::Intrinsic { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }

        let live_functions = self
            .functions
            .iter()
            .enumerate()
            .filter_map(|(index, function)| function.as_ref().map(|_| CpsFunId(index as u32)))
            .collect::<BTreeSet<_>>();
        if live_functions != bound_functions {
            return Err(CpsVerifyError(
                "function arena and lexical function bindings disagree".into(),
            ));
        }
        let live_values = self
            .values
            .iter()
            .enumerate()
            .filter_map(|(index, value)| value.as_ref().map(|_| CpsValueId(index as u32)))
            .collect::<BTreeSet<_>>();
        if live_values != bound_values {
            return Err(CpsVerifyError(
                "value arena and lexical value bindings disagree".into(),
            ));
        }
        if !pending_recursive_values.is_empty() {
            return Err(CpsVerifyError(
                "recursive initializer value lacks its computed binding".into(),
            ));
        }
        Ok(())
    }

    fn verify_function_body(
        &self,
        owner: CpsFunId,
        function: &CpsFunction,
        returns: &BTreeMap<CpsContId, CpsFunId>,
        node_owners: &mut BTreeMap<CpsNodeId, CpsFunId>,
        bound_continuations: &mut BTreeSet<CpsContId>,
    ) -> Result<(), CpsVerifyError> {
        let mut work = vec![(function.body, BTreeSet::<CpsContId>::new())];
        let mut visited = BTreeSet::<CpsNodeId>::new();

        while let Some((id, scope)) = work.pop() {
            if !visited.insert(id) {
                continue;
            }
            if let Some(previous) = node_owners.insert(id, owner)
                && previous != owner
            {
                return Err(CpsVerifyError(format!(
                    "{id} is owned by both {previous} and {owner}"
                )));
            }
            let node = self
                .node(id)
                .ok_or_else(|| CpsVerifyError(format!("function body references missing {id}")))?;
            self.verify_node(owner, function.return_cont, returns, &scope, id, node)?;

            match node {
                CpsNode::LetValue { next, .. } | CpsNode::LetPrim { next, .. } => {
                    work.push((*next, scope));
                }
                CpsNode::LetFun { body, .. } | CpsNode::RecInit { body, .. } => {
                    work.push((*body, scope));
                }
                CpsNode::LetCont {
                    continuations,
                    body,
                } => {
                    let mut inner = scope;
                    for &continuation in continuations {
                        if returns.contains_key(&continuation) {
                            return Err(CpsVerifyError(format!(
                                "return ID {continuation} cannot be bound as a local continuation"
                            )));
                        }
                        self.require_cont(continuation, "LetCont member")?;
                        if !bound_continuations.insert(continuation) {
                            return Err(CpsVerifyError(format!(
                                "local continuation {continuation} is bound more than once"
                            )));
                        }
                        inner.insert(continuation);
                    }
                    work.push((*body, inner.clone()));
                    for &continuation in continuations.iter().rev() {
                        work.push((self.continuation(continuation).unwrap().body, inner.clone()));
                    }
                }
                CpsNode::ApplyFun { .. }
                | CpsNode::ApplyCont(_)
                | CpsNode::Switch { .. }
                | CpsNode::Foreign { .. }
                | CpsNode::Cell { .. }
                | CpsNode::Intrinsic { .. }
                | CpsNode::Exit { .. }
                | CpsNode::Unreachable => {}
            }
        }
        Ok(())
    }

    fn verify_node(
        &self,
        current_function: CpsFunId,
        return_cont: CpsContId,
        returns: &BTreeMap<CpsContId, CpsFunId>,
        scope: &BTreeSet<CpsContId>,
        id: CpsNodeId,
        node: &CpsNode,
    ) -> Result<(), CpsVerifyError> {
        match node {
            CpsNode::LetValue { result, next, .. } => {
                self.require_value(*result, "let-value result")?;
                self.require_node(*next, "let-value successor")?;
            }
            CpsNode::LetPrim {
                result,
                op,
                args,
                next,
            } => {
                self.require_value(*result, "let-prim result")?;
                self.require_node(*next, "let-prim successor")?;
                if args.len() != op.arity() {
                    return Err(CpsVerifyError(format!(
                        "{id} primitive {op:?} expects {} operands, got {}",
                        op.arity(),
                        args.len()
                    )));
                }
            }
            CpsNode::LetFun { functions, body } => {
                for &function in functions {
                    self.require_fun(function, "let-fun member")?;
                }
                self.require_node(*body, "let-fun body")?;
            }
            CpsNode::LetCont {
                continuations,
                body,
            } => {
                for &continuation in continuations {
                    self.require_cont(continuation, "let-cont member")?;
                }
                self.require_node(*body, "let-cont body")?;
            }
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                match callee {
                    CpsCallee::Known(function) => {
                        self.require_fun(*function, "known callee")?;
                        let arity = self.function(*function).unwrap().params.len();
                        if arity != args.len() {
                            return Err(CpsVerifyError(format!(
                                "{id} calls {function} with {} arguments; expected {arity}",
                                args.len()
                            )));
                        }
                    }
                    CpsCallee::Closure(value) => self.require_value(*value, "closure callee")?,
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    returns,
                    scope,
                    *return_to,
                )? != 1
                {
                    return Err(CpsVerifyError(format!(
                        "{id} user call return continuation {return_to} must accept one value"
                    )));
                }
            }
            CpsNode::ApplyCont(edge) => {
                self.verify_edge(current_function, return_cont, returns, scope, id, edge)?
            }
            CpsNode::Switch { cases, default, .. } => {
                for edge in cases.values() {
                    self.verify_edge(current_function, return_cont, returns, scope, id, edge)?;
                }
                if let Some(edge) = default {
                    self.verify_edge(current_function, return_cont, returns, scope, id, edge)?;
                }
            }
            CpsNode::Foreign {
                function,
                args,
                return_to,
            } => {
                if args.len() != function.signature.params.len() {
                    return Err(CpsVerifyError(format!(
                        "{id} foreign call expects {} operands, got {}",
                        function.signature.params.len(),
                        args.len()
                    )));
                }
                let results = function.signature.results.len();
                let params = self.continuation_arity(
                    current_function,
                    return_cont,
                    returns,
                    scope,
                    *return_to,
                )?;
                if results != params {
                    return Err(CpsVerifyError(format!(
                        "{id} foreign return continuation expects {params} values, call returns {results}"
                    )));
                }
            }
            CpsNode::Cell {
                op,
                args,
                return_to,
            } => {
                if args.len() != op.operand_arity() {
                    return Err(CpsVerifyError(format!(
                        "{id} cell {op:?} expects {} operands, got {}",
                        op.operand_arity(),
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    returns,
                    scope,
                    *return_to,
                )? != op.result_arity()
                {
                    return Err(CpsVerifyError(format!(
                        "{id} cell {op:?} continuation arity mismatch"
                    )));
                }
            }
            CpsNode::Intrinsic {
                op: CpsIntrinsicOp::LstMap,
                args,
                return_to,
            } => {
                if args.len() != 2 {
                    return Err(CpsVerifyError(format!(
                        "{id} LstMap expects two operands, got {}",
                        args.len()
                    )));
                }
                if self.continuation_arity(
                    current_function,
                    return_cont,
                    returns,
                    scope,
                    *return_to,
                )? != 1
                {
                    return Err(CpsVerifyError(format!(
                        "{id} LstMap continuation must accept one value"
                    )));
                }
            }
            CpsNode::Exit { .. } | CpsNode::Unreachable => {}
            CpsNode::RecInit {
                functions,
                values,
                ready,
                body,
            } => {
                if functions.is_empty() || values.is_empty() {
                    return Err(CpsVerifyError(format!(
                        "{id} recursive initializer must be a mixed function/value group"
                    )));
                }
                for &function in functions {
                    self.require_fun(function, "recursive initializer function")?;
                }
                for &value in values {
                    self.require_value(value, "recursive initializer value")?;
                }
                self.require_node(*ready, "recursive initializer ready point")?;
                self.require_node(*body, "recursive initializer body")?;
            }
        }

        for atom in atoms(node) {
            match atom {
                CpsAtom::Value(value) => self.require_value(*value, "operand")?,
                CpsAtom::Fun(function) => self.require_fun(*function, "function atom")?,
                CpsAtom::Literal(_) => {}
            }
        }
        Ok(())
    }

    fn verify_edge(
        &self,
        function: CpsFunId,
        return_cont: CpsContId,
        returns: &BTreeMap<CpsContId, CpsFunId>,
        scope: &BTreeSet<CpsContId>,
        owner: CpsNodeId,
        edge: &CpsEdge,
    ) -> Result<(), CpsVerifyError> {
        let arity = self.continuation_arity(function, return_cont, returns, scope, edge.target)?;
        if arity != edge.args.len() {
            return Err(CpsVerifyError(format!(
                "{owner} edge to {} carries {} arguments; expected {arity}",
                edge.target,
                edge.args.len()
            )));
        }
        Ok(())
    }

    fn continuation_arity(
        &self,
        function: CpsFunId,
        return_cont: CpsContId,
        returns: &BTreeMap<CpsContId, CpsFunId>,
        scope: &BTreeSet<CpsContId>,
        target: CpsContId,
    ) -> Result<usize, CpsVerifyError> {
        if target == return_cont {
            return Ok(1);
        }
        if let Some(owner) = returns.get(&target) {
            return Err(CpsVerifyError(format!(
                "{function} references {owner}'s return continuation {target}"
            )));
        }
        if !scope.contains(&target) {
            return Err(CpsVerifyError(format!(
                "{function} references undefined or out-of-scope continuation {target}"
            )));
        }
        self.continuation(target)
            .map(|continuation| continuation.params.len())
            .ok_or_else(|| CpsVerifyError(format!("undefined non-return continuation {target}")))
    }

    fn require_node(&self, id: CpsNodeId, what: &str) -> Result<(), CpsVerifyError> {
        self.node(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }

    fn require_value(&self, id: CpsValueId, what: &str) -> Result<(), CpsVerifyError> {
        self.values
            .get(id.index())
            .and_then(Option::as_ref)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }

    fn require_fun(&self, id: CpsFunId, what: &str) -> Result<(), CpsVerifyError> {
        self.function(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }

    fn require_cont(&self, id: CpsContId, what: &str) -> Result<(), CpsVerifyError> {
        self.continuation(id)
            .map(|_| ())
            .ok_or_else(|| CpsVerifyError(format!("{what} references missing {id}")))
    }
}

pub(crate) fn atoms(node: &CpsNode) -> Vec<&CpsAtom> {
    let mut output = Vec::new();
    match node {
        CpsNode::LetValue { value, .. } => match value {
            CpsValueExpr::Literal(_) => {}
            CpsValueExpr::List(values) | CpsValueExpr::Tuple(values) => output.extend(values),
        },
        CpsNode::LetPrim { args, .. }
        | CpsNode::ApplyFun { args, .. }
        | CpsNode::Foreign { args, .. }
        | CpsNode::Cell { args, .. }
        | CpsNode::Intrinsic { args, .. } => output.extend(args),
        CpsNode::ApplyCont(edge) => output.extend(&edge.args),
        CpsNode::Switch {
            scrutinee,
            cases,
            default,
        } => {
            output.push(scrutinee);
            for edge in cases.values() {
                output.extend(&edge.args);
            }
            if let Some(edge) = default {
                output.extend(&edge.args);
            }
        }
        CpsNode::Exit { value, .. } => output.extend(value),
        CpsNode::LetFun { .. }
        | CpsNode::LetCont { .. }
        | CpsNode::Unreachable
        | CpsNode::RecInit { .. } => {}
    }
    output
}

pub(crate) fn visit_atoms_mut(node: &mut CpsNode, visitor: &mut impl FnMut(&mut CpsAtom)) {
    match node {
        CpsNode::LetValue { value, .. } => match value {
            CpsValueExpr::Literal(_) => {}
            CpsValueExpr::List(values) | CpsValueExpr::Tuple(values) => {
                values.iter_mut().for_each(visitor)
            }
        },
        CpsNode::LetPrim { args, .. }
        | CpsNode::ApplyFun { args, .. }
        | CpsNode::Foreign { args, .. }
        | CpsNode::Cell { args, .. }
        | CpsNode::Intrinsic { args, .. } => args.iter_mut().for_each(visitor),
        CpsNode::ApplyCont(edge) => edge.args.iter_mut().for_each(visitor),
        CpsNode::Switch {
            scrutinee,
            cases,
            default,
        } => {
            visitor(scrutinee);
            for edge in cases.values_mut() {
                edge.args.iter_mut().for_each(&mut *visitor);
            }
            if let Some(edge) = default {
                edge.args.iter_mut().for_each(visitor);
            }
        }
        CpsNode::Exit { value, .. } => {
            if let Some(value) = value {
                visitor(value);
            }
        }
        CpsNode::LetFun { .. }
        | CpsNode::LetCont { .. }
        | CpsNode::Unreachable
        | CpsNode::RecInit { .. } => {}
    }
}

mod optimize;
pub(crate) use optimize::optimize as optimize_cps;

impl fmt::Display for CpsModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "entry {}",
            self.entry
                .map_or_else(|| "<none>".into(), |id| id.to_string())
        )?;
        for (index, function) in self.functions.iter().enumerate() {
            let Some(function) = function else { continue };
            let id = CpsFunId(index as u32);
            write!(f, "fun {id}")?;
            if let Some(name) = &function.debug_name {
                write!(f, " [{name}]")?;
            }
            write!(f, "(")?;
            separated(f, function.params.iter())?;
            writeln!(f, ") -> {} = {}", function.return_cont, function.body)?;
        }
        for (index, continuation) in self.continuations.iter().enumerate() {
            let Some(continuation) = continuation else {
                continue;
            };
            let id = CpsContId(index as u32);
            write!(f, "cont {id}(")?;
            separated(f, continuation.params.iter())?;
            writeln!(f, ") = {}", continuation.body)?;
        }
        for (index, node) in self.nodes.iter().enumerate() {
            let Some(node) = node else { continue };
            writeln!(f, "{} = {}", CpsNodeId(index as u32), CpsDisplayNode(node))?;
        }
        Ok(())
    }
}

fn separated<T: fmt::Display>(
    f: &mut fmt::Formatter<'_>,
    items: impl Iterator<Item = T>,
) -> fmt::Result {
    for (index, item) in items.enumerate() {
        if index != 0 {
            write!(f, ", ")?;
        }
        write!(f, "{item}")?;
    }
    Ok(())
}

struct CpsDisplayNode<'a>(&'a CpsNode);

impl fmt::Display for CpsDisplayNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            CpsNode::LetValue {
                result,
                value,
                next,
            } => write!(f, "let {result} = {value:?}; {next}"),
            CpsNode::LetPrim {
                result,
                op,
                args,
                next,
            } => {
                write!(f, "let {result} = {op:?}{args:?}; {next}")
            }
            CpsNode::LetFun { functions, body } => write!(f, "let-fun {functions:?}; {body}"),
            CpsNode::LetCont {
                continuations,
                body,
            } => write!(f, "let-cont {continuations:?}; {body}"),
            CpsNode::ApplyFun {
                callee,
                args,
                return_to,
            } => {
                write!(f, "apply {callee:?}{args:?} -> {return_to}")
            }
            CpsNode::ApplyCont(edge) => write!(f, "jump {}{:?}", edge.target, edge.args),
            CpsNode::Switch {
                scrutinee,
                cases,
                default,
            } => {
                write!(f, "switch {scrutinee:?} {cases:?} default {default:?}")
            }
            CpsNode::Foreign {
                function,
                args,
                return_to,
            } => {
                write!(f, "foreign {}{args:?} -> {return_to}", function.name)
            }
            CpsNode::Cell {
                op,
                args,
                return_to,
            } => write!(f, "cell.{op:?}{args:?} -> {return_to}"),
            CpsNode::Intrinsic {
                op,
                args,
                return_to,
            } => {
                write!(f, "intrinsic.{op:?}{args:?} -> {return_to}")
            }
            CpsNode::Exit { value } => write!(f, "exit {value:?}"),
            CpsNode::Unreachable => f.write_str("unreachable"),
            CpsNode::RecInit {
                functions,
                values,
                ready,
                body,
            } => {
                write!(f, "rec-init {functions:?} {values:?} ready {ready}; {body}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        CpsAtom, CpsContinuation, CpsEdge, CpsFunction, CpsLiteral, CpsModule, CpsNode, CpsNodeId,
        CpsPrimOp, CpsUseTarget, CpsValueExpr, CpsValueId,
    };

    fn minimal_module() -> CpsModule {
        let mut module = CpsModule::new();
        let fun = module.reserve_function(Some("main".into()));
        let return_cont = module.reserve_continuation();
        let result = module.add_value(Some("result".into()), false);
        let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: return_cont,
            args: vec![CpsAtom::Value(result)],
        }));
        let body = module.add_node(CpsNode::LetValue {
            result,
            value: CpsValueExpr::Literal(CpsLiteral::Nat(0)),
            next: return_node,
        });
        module.define_function(
            fun,
            CpsFunction {
                debug_name: Some("main".into()),
                params: vec![],
                return_cont,
                body,
            },
        );
        module.set_entry(fun);
        module
    }

    #[test]
    fn registers_and_rewires_uses() {
        let mut module = minimal_module();
        let old = module
            .values()
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                (value.as_ref()?.debug_name.as_deref() == Some("result"))
                    .then_some(CpsValueId(index as u32))
            })
            .unwrap();
        let replacement = module.add_value(Some("replacement".into()), false);
        let entry = module.entry().unwrap();
        module.functions[entry.index()]
            .as_mut()
            .unwrap()
            .params
            .push(replacement);
        let count = |module: &CpsModule, value| module.value_use_counts().get(&value).copied();
        assert_eq!(count(&module, old), Some(1));
        module.replace_atom(CpsUseTarget::Value(old), CpsAtom::Value(replacement));
        assert_eq!(count(&module, old), None);
        assert_eq!(count(&module, replacement), Some(1));
        module.verify().unwrap();
    }

    #[test]
    fn verifier_rejects_an_existing_but_out_of_scope_value() {
        let mut module = minimal_module();
        let result = module
            .values()
            .iter()
            .enumerate()
            .find_map(|(index, value)| {
                (value.as_ref()?.debug_name.as_deref() == Some("result"))
                    .then_some(CpsValueId(index as u32))
            })
            .unwrap();
        let orphan = module.add_value(Some("orphan".into()), false);
        module.replace_atom(CpsUseTarget::Value(result), CpsAtom::Value(orphan));

        let error = module.verify().unwrap_err();
        assert!(error.to_string().contains("out-of-scope"));
    }

    #[test]
    fn node_ids_are_not_reused_after_tombstoning() {
        let mut module = minimal_module();
        let removed = CpsNodeId(0);
        module.remove_node(removed).unwrap();
        let fresh = module.add_node(CpsNode::Unreachable);
        assert!(fresh.0 > removed.0);
    }

    #[test]
    fn verifier_rejects_primitive_arity_mismatch() {
        let mut module = minimal_module();
        let result = module.add_value(None, false);
        let next = module.add_node(CpsNode::Unreachable);
        module.add_node(CpsNode::LetPrim {
            result,
            op: CpsPrimOp::NatAdd,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
            next,
        });
        let bad = CpsNodeId((module.nodes.len() - 1) as u32);
        module.functions[0].as_mut().unwrap().body = bad;
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("expects 2 operands")
        );
    }

    #[test]
    fn lst_map_is_not_a_primitive_opcode() {
        assert!(CpsPrimOp::LstAppend.allocates());
        assert!(!CpsPrimOp::NatAdd.is_total());
    }

    #[test]
    fn return_continuation_is_a_bodyless_non_tombstone_slot() {
        let module = minimal_module();
        let function = module.function(module.entry().unwrap()).unwrap();
        assert!(module.continuation(function.return_cont).is_none());
        assert_eq!(module.tombstones().3, 0);
        module.verify().unwrap();
    }

    #[test]
    fn verifier_rejects_shared_return_continuations() {
        let mut module = minimal_module();
        let shared_return = module
            .function(module.entry().unwrap())
            .unwrap()
            .return_cont;
        let second = module.reserve_function(Some("second".into()));
        let body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: shared_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        }));
        module.define_function(
            second,
            CpsFunction {
                debug_name: Some("second".into()),
                params: vec![],
                return_cont: shared_return,
                body,
            },
        );
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("return continuation of both")
        );
    }

    #[test]
    fn verifier_rejects_another_functions_return_target() {
        let mut module = minimal_module();
        let second = module.reserve_function(Some("second".into()));
        let second_return = module.reserve_continuation();
        let second_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
            target: second_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        }));
        module.define_function(
            second,
            CpsFunction {
                debug_name: Some("second".into()),
                params: vec![],
                return_cont: second_return,
                body: second_body,
            },
        );
        let entry = module.entry().unwrap();
        let entry_body = module.function(entry).unwrap().body;
        module.nodes[entry_body.index()] = Some(CpsNode::ApplyCont(CpsEdge {
            target: second_return,
            args: vec![CpsAtom::Literal(CpsLiteral::Nat(0))],
        }));
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("references @f1's return continuation")
        );
    }

    #[test]
    fn verifier_rejects_undefined_non_return_continuation() {
        let mut module = minimal_module();
        let undefined = module.reserve_continuation();
        let entry = module.entry().unwrap();
        let entry_body = module.function(entry).unwrap().body;
        module.nodes[entry_body.index()] = Some(CpsNode::ApplyCont(CpsEdge {
            target: undefined,
            args: vec![],
        }));
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("undefined or out-of-scope continuation")
        );
    }

    #[test]
    fn verifier_rejects_local_body_at_return_id() {
        let mut module = minimal_module();
        let entry = module.entry().unwrap();
        let return_cont = module.function(entry).unwrap().return_cont;
        let local_body = module.add_node(CpsNode::Unreachable);
        module.define_continuation(
            return_cont,
            CpsContinuation {
                debug_name: Some("invalid-return-body".into()),
                params: vec![],
                body: local_body,
            },
        );
        assert!(
            module
                .verify()
                .unwrap_err()
                .0
                .contains("also identifies a local continuation")
        );
    }
}
