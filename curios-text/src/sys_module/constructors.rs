//! How a `/sys` declaration is built: the constructors a roster entry is written with, and the module those entries are grouped into.
//!
//! Split from the roster itself so that `sys_module.rs` reads as *what `/sys` declares* and this reads as *how a declaration is built*. Nothing here knows what is in the roster; everything here is a shape. Its sibling `helpers.rs` sits one rung below: that builds terms, this builds declarations out of them.

use {
    super::{helpers::*, host_fn},
    crate::{
        Doc, FuncSugarParam, GroupItem, Intrinsic, LetSignature, Module, Name, Pattern, Term,
        TopItem, TopLet, TopMod, TopUse, UseGroup,
    },
    curios_abi::ForeignStore,
    curios_utilities::Plicity,
};

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
/// One `/sys` module, before the host's rows are folded into it.
///
/// **The label is the key, and that is the whole of this restructure.** `/sys` used to be assembled by two independent passes — the carrier modules written by hand from the intrinsic table, the subject modules built from `curios-abi`'s store — emitting into one namespace with nothing to merge them. `Handle` is in both inputs, so the one collision was reconciled by lifting its rows out by string before the generic pass and appending them by hand, with a `panic!` if the row ever went missing. Keying the modules removes the removal: a host row joins the module its subject names, whether that module already exists or is created by the row, and `Handle` stops being a special case and becomes the one label that happens to have both.
pub(super) struct SysModule {
    pub(super) label: String,
    /// The carrier declarations for this label: its type former and the intrinsic operations over it. Empty for a module that is nothing but host rows.
    pub(super) items: Vec<TopItem>,
    /// Whether the root re-exports the type this module declares — every carrier a program reaches by name, and no module of operations alone.
    pub(super) hoisted: bool,
}

impl SysModule {
    /// A carrier: a type former, its documentation, and the operations over it, hoisted to the root.
    pub(super) fn carrier(label: &str, doc: &[&str], former: TopItem, ops: Vec<TopItem>) -> Self {
        Self {
            label: label.to_string(),
            items: with_type(documented(doc, former), ops),
            hoisted: true,
        }
    }

    /// A carrier whose type the root does not re-export — a packed run, reached through its own module because two of them share every operation name.
    pub(super) fn nested(self) -> Self {
        Self {
            hoisted: false,
            ..self
        }
    }

    /// A module the host's rows alone will fill.
    pub(super) fn rows(label: &str) -> Self {
        Self {
            label: label.to_string(),
            items: Vec::new(),
            hoisted: false,
        }
    }
}

/// Fold every store-described host op into the module its own row names as its subject, creating one where no carrier claims the label.
///
/// Groups keep the order their first row appears in after the carriers, and rows keep store order within a group — so a new row lands under its subject with nothing beside the table to update. The 0-arity clocks and `args` are constants rather than nullary functions: the function abstraction existed to keep an effectful intrinsic body unevaluated at definition time, and a description is already unevaluated (see `host_fn`).
pub(super) fn absorb_host_rows(modules: &mut Vec<SysModule>, foreigns: &ForeignStore) {
    for function in foreigns.iter() {
        let subject = function
            .subject
            .clone()
            .expect("a builtin host operation names its /sys subject");

        let index = match modules.iter().position(|module| module.label == subject) {
            Some(index) => index,
            None => {
                modules.push(SysModule::rows(&subject));
                modules.len() - 1
            }
        };

        modules[index]
            .items
            .push(TopItem::Let(vec![host_fn(function, true)]));
    }
}
