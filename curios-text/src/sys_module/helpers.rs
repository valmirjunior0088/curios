//! Building blocks for the `/sys` roster: the term constructors its declarations are assembled from.
//!
//! One rung below `constructors.rs`, which builds declarations out of what this builds. Nothing here knows what is in the roster, and nothing here names a declaration: everything is a term.

use {
    crate::{
        Apply, Argument, FuncType, FuncTypeParam, Intrinsic, Label, Name, Nat, NatLiteral, Subterm,
        Term, TupleType, TupleTypeParam,
    },
    curios_num::Floating,
    curios_utilities::{Grain, Plicity, SyntaxName, SyntaxRegistry},
};

pub(super) fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

// A registered name, absolute so it resolves against the compilation root rather than whatever module the generated declaration lands in.
pub(super) fn registered(target: SyntaxName) -> Term {
    Subterm::Name(Name::new(true, target.qualifier())).into()
}

// One of this roster's own operations, named absolutely for the reason `registered` is: a declaration lands in whatever module the roster puts it in, so a relative name would resolve differently per site. Not a registry entry — the registry holds what a crate *below* `/sys` must be able to name, and these are `/sys` naming itself.
pub(super) fn sys_op(segments: &'static [&'static str]) -> Term {
    registered(SyntaxName::new(segments))
}

// The proposition a decided bound is stated as: `Holds` applied to the decision itself. `curios-core`'s signature table builds the same shape over the intrinsic nodes these wrappers unfold to, and elaborating a `/sys` body is what holds the two together.
pub(super) fn decided(syntax: &SyntaxRegistry, decision: Term) -> Term {
    applied(registered(syntax.proof.holds), vec![decision])
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

// A `Nat` literal value term, built exactly as the parser builds one: `0` is bare `Zero`, anything else is `Succ(n, Zero)`. Used to bake host-owned wire codes (`status`, `event`, `open_mode`, `file_kind` and `stdio_mode`) into the `/sys` code modules.
pub(super) fn nat_lit(n: u32) -> Term {
    match n {
        0 => intrinsic(Intrinsic::Nat(Nat::Zero)),
        n => intrinsic(Intrinsic::Nat(Nat::Succ(
            NatLiteral::number(n),
            intrinsic(Intrinsic::Nat(Nat::Zero)),
        ))),
    }
}

/// `left + right` as a `/sys` term. The window bound is the one precondition stated over arithmetic rather than over an operand, so it is the one that has to build a sum.
pub(super) fn nat_plus(left: Term, right: Term) -> Term {
    intrinsic(Intrinsic::NatAdd(left, right))
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

// A single-argument function type `(domain) -> output`, for higher-order intrinsics (the `f` of `List/map`).
pub(super) fn fn_of(domain: Term, output: Term) -> Term {
    Subterm::FuncType(FuncType {
        params: vec![FuncTypeParam {
            plicity: Plicity::Explicit,
            label: None,
            type_: domain,
        }],
        output,
    })
    .into()
}

pub(super) fn cell_of(elem: Term) -> Term {
    intrinsic(Intrinsic::CellType(elem))
}

pub(super) fn io_of(result: Term) -> Term {
    intrinsic(Intrinsic::IoType(result))
}

pub(super) fn type_() -> Term {
    Subterm::Type.into()
}

// The propositions a decided bound is stated in. Built here like everything else this roster holds, and placed by the same rule: one stated over a carrier lives in that carrier's module, and one stated over none lives at the root.
//
// Authoring them in a `.crs` beside this file was tried and undone. It read better and placed worse: spliced at the root the `Flt` pair became `/sys/NonNeg`, saying nothing about which carrier, and the range constants collided with `/sys/Flt`'s `min` and `max` operations one segment away. Placement is what a generator is good at, so the generator keeps them.
pub(super) fn prop() -> Term {
    Subterm::Prop.into()
}

// A `Flt` literal, for the two range bounds below. Spelled from `f32`'s own extremes rather than from a written magnitude, so the bound is the carrier's by construction and no digit string has to be kept in step with it.
pub(super) fn flt_lit(value: f32) -> Term {
    intrinsic(Intrinsic::Flt(Floating::from_f32(value)))
}
