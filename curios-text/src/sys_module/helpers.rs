//! Building blocks for the `/sys` roster: the term constructors its declarations are assembled from.
//!
//! One rung below `constructors.rs`, which builds declarations out of what this builds. Nothing here knows what is in the roster, and nothing here names a declaration: everything is a term.

use {
    crate::{
        Apply, Argument, FuncType, FuncTypeParam, Intrinsic, Label, Name, Subterm, Term, TupleType,
        TupleTypeParam,
    },
    curios_num::{Grain, Natural},
    curios_utilities::Plicity,
};

pub(super) fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

pub(super) fn applied(head: Term, args: Vec<Term>) -> Term {
    Subterm::Apply(Apply {
        head,
        arguments: args
            .into_iter()
            .map(|arg| Argument {
                term: arg,
                plicity: Plicity::Explicit,
            })
            .collect(),
    })
    .into()
}

pub(super) fn intrinsic(p: Intrinsic) -> Term {
    Subterm::Intrinsic(p).into()
}

pub(super) fn nat() -> Term {
    intrinsic(Intrinsic::NatType)
}

pub(super) fn byte() -> Term {
    intrinsic(Intrinsic::ByteType)
}

// A `Byte` literal value term, for a wire code that is a byte on both sides.
pub(super) fn byte_lit(n: u8) -> Term {
    intrinsic(Intrinsic::Byte(n))
}

// A `Nat` literal value term, built exactly as the parser builds one: `0` is bare `Zero`, anything else is `Succ(n, Zero)`. Used to bake host-owned wire codes (`status`, `event`, `open_mode`, `file_kind`, `stdio_mode`, `serial_parity`, `serial_flow` and `serial_op`) into the `/sys` code modules.
pub(super) fn nat_lit(n: u64) -> Term {
    match n {
        0 => intrinsic(Intrinsic::Nat(curios_core::Nat::Zero)),
        n => intrinsic(Intrinsic::Nat(curios_core::Nat::Succ(
            Natural::from(n),
            intrinsic(Intrinsic::Nat(curios_core::Nat::Zero)),
        ))),
    }
}

pub(super) fn int() -> Term {
    intrinsic(Intrinsic::IntType)
}

pub(super) fn flt() -> Term {
    intrinsic(Intrinsic::FltType)
}

pub(super) fn bin(grain: Grain) -> Term {
    intrinsic(Intrinsic::BinType(grain))
}

pub(super) fn bool_() -> Term {
    intrinsic(Intrinsic::BoolType)
}

pub(super) fn handle() -> Term {
    intrinsic(Intrinsic::HandleType)
}

pub(super) fn unit() -> Term {
    Subterm::TupleType(TupleType { fields: vec![] }).into()
}

pub(super) fn record(fields: Vec<(&str, Term)>) -> Term {
    Subterm::TupleType(TupleType {
        fields: fields
            .into_iter()
            .map(|(label, type_)| TupleTypeParam {
                label: Some(Label::from(label)),
                func_params: None,
                type_,
            })
            .collect(),
    })
    .into()
}

pub(super) fn list_of(elem: Term) -> Term {
    intrinsic(Intrinsic::ListType(elem))
}

// A function type over several explicit parameters `(domains…) -> output`, for the stepper of `List/fold`.
pub(super) fn fn_over(domains: Vec<Term>, output: Term) -> Term {
    Subterm::FuncType(FuncType {
        params: domains
            .into_iter()
            .map(|domain| FuncTypeParam {
                plicity: Plicity::Explicit,
                label: None,
                type_: domain,
            })
            .collect(),
        output,
    })
    .into()
}

pub(super) fn io_of(result: Term) -> Term {
    intrinsic(Intrinsic::IoType(result))
}

pub(super) fn type_() -> Term {
    Subterm::Type.into()
}

// A sort its declaration leaves to each caller: what an operand a row calls a type is declared at, the node the declaration builds taking a proposition where it takes a type.
pub(super) fn sort() -> Term {
    Subterm::Sort.into()
}

// The propositions a decided bound is stated in. Built here like everything else this roster holds, and placed by the same rule: one stated over a carrier lives in that carrier's module, and one stated over none lives at the root.
//
// Authoring them in a `.crs` beside this file would read better and place worse: spliced at the root the `Flt` pair would become `/sys/NonNeg`, saying nothing about which carrier, and the range constants would collide with `/sys/Flt`'s `min` and `max` operations one segment away. Placement is what a generator is good at, so the generator keeps them.
pub(super) fn prop() -> Term {
    Subterm::Prop.into()
}
