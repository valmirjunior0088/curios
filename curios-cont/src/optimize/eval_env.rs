//! The evaluation environment: the [`EvalEnv`] abstraction `scalar_eval` reads its
//! operands through, and its literal-map instance.
//!
//! [`Lits`] is the constant folder's environment — a flat map from value name
//! to literal `Data`, harvested per region tree by [`literals`]. The
//! interpreter's frame in [`interp`](super::interp) is the other instance.

use {super::*, std::collections::HashMap};

/// A region-tree-wide map from value name to its bound literal. Names are unique
/// per body and scoping is lexical, so a single flat map is sound.
pub(crate) type Lits = HashMap<ValueName, Data>;

/// Collect every literal-bound scalar or aggregate in the region tree.
pub(crate) fn literals(region: &Region) -> Lits {
    let mut lits = Lits::new();
    collect_literals(region, &mut lits);
    lits
}

fn collect_literals(region: &Region, lits: &mut Lits) {
    for (name, value) in &region.values {
        if let Value::Pure(data) = value {
            lits.insert(name.clone(), data.clone());
        }
    }

    for (_, block) in &region.blocks {
        collect_literals(&block.region, lits);
    }
}

/// The environment an evaluation runs against: scalar lookups by operand name,
/// aggregate lookups yielding the environment's own element representation.
pub(crate) trait EvalEnv {
    /// What an aggregate holds: a `ValueName` when folding against [`Lits`], a
    /// runtime snapshot when interpreting against a frame.
    type Elem: Clone;

    fn nat(&self, name: &ValueName) -> Option<u32>;
    fn int(&self, name: &ValueName) -> Option<i32>;
    fn flt(&self, name: &ValueName) -> Option<f32>;
    fn bin(&self, name: &ValueName) -> Option<&[u8]>;
    fn lst(&self, name: &ValueName) -> Option<&[Self::Elem]>;
    fn tpl(&self, name: &ValueName) -> Option<&[Self::Elem]>;

    /// The element handle for an operand name — `LstAppend` appends by reference.
    fn elem(&self, name: &ValueName) -> Option<Self::Elem>;

    /// The scalar behind an element handle, if it is one (`Nat`/`Int`/`Flt` —
    /// not `Bin`, which is forwarded by handle to avoid copying). Inlined when a
    /// projection forwards the element, so it cascades through further folds.
    fn scalar(&self, elem: &Self::Elem) -> Option<Scalar>;
}

impl EvalEnv for Lits {
    type Elem = ValueName;

    fn nat(&self, name: &ValueName) -> Option<u32> {
        match self.get(name)? {
            Data::Nat(value) => Some(*value),
            _ => None,
        }
    }

    fn int(&self, name: &ValueName) -> Option<i32> {
        match self.get(name)? {
            Data::Int(value) => Some(*value),
            _ => None,
        }
    }

    fn flt(&self, name: &ValueName) -> Option<f32> {
        match self.get(name)? {
            Data::Flt(value) => Some(*value),
            _ => None,
        }
    }

    fn bin(&self, name: &ValueName) -> Option<&[u8]> {
        match self.get(name)? {
            Data::Bin(bytes) => Some(bytes),
            _ => None,
        }
    }

    fn lst(&self, name: &ValueName) -> Option<&[ValueName]> {
        match self.get(name)? {
            Data::Lst(elems) => Some(elems),
            _ => None,
        }
    }

    fn tpl(&self, name: &ValueName) -> Option<&[ValueName]> {
        match self.get(name)? {
            Data::Tpl(elems) => Some(elems),
            _ => None,
        }
    }

    fn elem(&self, name: &ValueName) -> Option<ValueName> {
        Some(name.clone())
    }

    fn scalar(&self, elem: &ValueName) -> Option<Scalar> {
        match self.get(elem)? {
            Data::Nat(value) => Some(Scalar::Nat(*value)),
            Data::Int(value) => Some(Scalar::Int(*value)),
            Data::Flt(value) => Some(Scalar::Flt(*value)),
            _ => None,
        }
    }
}
