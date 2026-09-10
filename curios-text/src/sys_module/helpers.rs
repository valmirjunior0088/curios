//! Building blocks for the `/sys` roster: the term and item constructors its declarations are assembled from.
//!
//! Split from the roster itself so that `sys_module.rs` reads as *what `/sys` declares* and this reads as *how a declaration is built*. Nothing here knows what is in the roster; everything here is a shape.

use {
    crate::{
        Apply, Argument, Doc, FuncSugarParam, FuncType, FuncTypeParam, GroupItem, Intrinsic, Label,
        LetSignature, Module, Name, Nat, NatLiteral, Pattern, Subterm, Term, TopItem, TopLet,
        TopMod, TopUse, TupleType, TupleTypeParam, UseGroup,
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

pub(super) fn pub_let(label: &str, type_: Term, body: Term) -> TopItem {
    TopItem::Let(vec![TopLet {
        doc: None,
        vis_pub: true,
        label: label.into(),
        signature: LetSignature::Name {
            type_: Some(type_),
            body,
        },
    }])
}

/// `item` under `lines`, the block a `-- |` would have put above it — written first here for the same reason it is written first there. An empty line is a paragraph break, exactly as it is in the surface syntax.
///
/// **A gloss says what the operation is, not what its carrier will not hold.** Where a value leaves the carrier is one rule stated once — `documentation/design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md`, and `curios-num`'s `scalar` per operation — and repeating it on every row would be sixty copies to keep in step. What a gloss must say is where an operation departs from the obvious reading of its name: that `sub` is monus, that `shr` divides.
///
/// **Only a lone declaration takes one.** Every builder here makes one declaration per item, so the group form would leave all but the first silently undocumented; asserted rather than assumed, since nothing else would notice.
pub(super) fn documented(lines: &[&str], item: TopItem) -> TopItem {
    let doc = Some(Doc {
        lines: lines.iter().map(|line| (*line).to_string()).collect(),
        span: None,
    });

    match item {
        TopItem::Let(mut members) => {
            assert_eq!(
                members.len(),
                1,
                "a documented `/sys` item declares one name"
            );
            members[0].doc = doc;
            TopItem::Let(members)
        }
        TopItem::Mod(mut module) => {
            module.doc = doc;
            TopItem::Mod(module)
        }
        _ => panic!("only a definition or a module carries a `/sys` gloss"),
    }
}

pub(super) fn pub_mod(label: &str, items: Vec<TopItem>) -> TopItem {
    TopItem::Mod(TopMod {
        doc: None,
        span: None,
        vis_pub: true,
        label: label.into(),
        module: Some(Module { items }),
    })
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

// `pub use Label/{let Label}` — the facade re-export that hoists a submodule's own type binding up to the library root, so `/sys/{Label}` names the type.
pub(super) fn pub_use(label: &str) -> TopItem {
    TopItem::Use(TopUse {
        span: None,
        vis_pub: true,
        name: Name::from([label.to_string()]),
        group: UseGroup::Named(vec![GroupItem::Let(label.into())]),
    })
}

// An intrinsic module's items: its type declaration first, then its operations, so the type lives *inside* its module and the root facade re-exports it.
pub(super) fn with_type(type_decl: TopItem, mut ops: Vec<TopItem>) -> Vec<TopItem> {
    let mut items = vec![type_decl];
    items.append(&mut ops);
    items
}

pub(super) fn pub_fn(label: &str, params: Vec<(&str, Term)>, output: Term, body: Term) -> TopItem {
    pub_fn_marked(
        label,
        params
            .into_iter()
            .map(|(n, t)| (Plicity::Explicit, n, t))
            .collect(),
        output,
        body,
    )
}

pub(super) fn pub_fn_marked(
    label: &str,
    params: Vec<(Plicity, &str, Term)>,
    output: Term,
    body: Term,
) -> TopItem {
    TopItem::Let(vec![fn_marked(true, label, params, output, body)])
}

pub(super) fn fn_marked(
    vis_pub: bool,
    label: &str,
    params: Vec<(Plicity, &str, Term)>,
    output: Term,
    body: Term,
) -> TopLet {
    TopLet {
        doc: None,
        vis_pub,
        label: label.into(),
        signature: LetSignature::Func {
            params: params
                .into_iter()
                .map(|(p, n, t)| FuncSugarParam {
                    plicity: p,
                    label: Pattern::Binder(Some(n.into())),
                    type_: t,
                })
                .collect(),
            output,
            body,
        },
    }
}

pub(super) fn binary(
    label: &str,
    operand: Term,
    output: Term,
    ctor: fn(Term, Term) -> Intrinsic,
) -> TopItem {
    pub_fn(
        label,
        vec![("a", operand.clone()), ("b", operand)],
        output,
        intrinsic(ctor(name("a"), name("b"))),
    )
}

// A binary operation whose second operand carries a precondition — the divisions, whose fold reports rather than answers on a zero divisor. The bound is stated the way source states it, for the reason `bin_ops` gives: a refinement is keyed on the term written, and a caller can only write the operand.
pub(super) fn guarded_binary(
    label: &str,
    operand: Term,
    output: Term,
    bound: Term,
    ctor: fn(Term, Term, Term) -> Intrinsic,
) -> TopItem {
    pub_fn_marked(
        label,
        vec![
            (Plicity::Explicit, "a", operand.clone()),
            (Plicity::Explicit, "b", operand),
            (Plicity::Implicit, "ok", bound),
        ],
        output,
        // The proof parameter is named in the body, which is what carries the bound into Core: an implicit nothing referenced would be checked here and forgotten, leaving the kernel nothing to re-verify.
        intrinsic(ctor(name("a"), name("b"), name("ok"))),
    )
}

pub(super) fn guarded_unary(
    label: &str,
    input: Term,
    output: Term,
    bound: Term,
    ctor: fn(Term, Term) -> Intrinsic,
) -> TopItem {
    pub_fn_marked(
        label,
        vec![
            (Plicity::Explicit, "a", input),
            (Plicity::Implicit, "ok", bound),
        ],
        output,
        intrinsic(ctor(name("a"), name("ok"))),
    )
}

pub(super) fn unary(
    label: &str,
    input: Term,
    output: Term,
    ctor: fn(Term) -> Intrinsic,
) -> TopItem {
    pub_fn(
        label,
        vec![("a", input)],
        output,
        intrinsic(ctor(name("a"))),
    )
}
