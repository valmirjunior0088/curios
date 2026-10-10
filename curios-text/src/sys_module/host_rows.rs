//! How a wire row becomes a Curios declaration: the surface type a [`WireType`] denotes, the declaration a [`ForeignFunction`] surfaces as, and the registration a user's own `foreign` goes through.
//!
//! Serves `into_core`, whose row is the one a `foreign` declaration wrote, the standard library's as any package's. It sits under `sys_module` because a wire type *is* a `/sys` carrier: the vocabulary this reads is the roster's own.
//!
//! **[`host_fn`] wraps every store row's result in `Io`, at this one site.** That is load-bearing rather than incidental: it is half of what narrows `documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md` from an argument about the whole library to a check over two Rust tables — a foreign row cannot introduce an eliminator for the effect type, because every row's result is built here and [`WireType`] is a closed enum with no case that could name an `Io` in a domain. A second site building a row's result would end it.

use {
    super::{
        Decl, bin, bool_, byte, flt, handle, int, io_of, list_of, name, nat, record, type_, unit,
    },
    crate::{FuncType, FuncTypeParam, LetSignature, Subterm, Term, TopForeign},
    curios_abi::{ForeignFunction, ForeignStore, ResultShape, WireType},
    curios_num::Grain,
    curios_utilities::Plicity,
    std::{iter, sync::Arc},
};

/// The surface type a host-boundary [`WireType`] denotes — the prelude's reading of the signature, mirrored by `core::wire_term` after lowering.
fn wire_type(type_: &WireType) -> Term {
    match type_ {
        WireType::Nat => nat(),
        WireType::Int => int(),
        WireType::Bool => bool_(),
        WireType::Byte => byte(),
        WireType::Flt => flt(),
        WireType::Bytes => bin(Grain::X),
        WireType::Bits => bin(Grain::B),
        WireType::Handle => handle(),
        WireType::List(element) => list_of(wire_type(&(*element).into())),
    }
}

/// The surface type of a function over `domains`, none of them named, as a declaration that writes no operand name states it.
fn function_over(domains: Vec<Term>, output: Term) -> Term {
    Subterm::FuncType(FuncType {
        params: domains
            .into_iter()
            .map(|type_| FuncTypeParam {
                plicity: Plicity::Explicit,
                label: None,
                type_,
            })
            .collect(),
        output,
    })
    .into()
}

/// The declaration a `foreign` row surfaces as: it binds the row itself, at the function type its operands and result state ([`Subterm::ForeignRow`]), since a declaration writes no operand name a call could be written over.
///
/// The result is an `Io`, and this one site is what makes that true of every row — a call across the wire is a host effect whoever declared it. The raw result shape (unit, bare type, named record) comes off the `WireSignature`, and a row that does not return yields the type its call is handed, taken as an implicit operand ahead of its wire operands, which the lowering opens a binder for.
///
/// A row with no parameters becomes a *constant* rather than a nullary function, which is not a case here but a consequence of handing [`Decl`] an empty telescope — see `Decl::signature`.
pub(super) fn host_fn(function: &Arc<ForeignFunction>, vis_pub: bool) -> Decl {
    let signature = function.signature();
    let row = Term::from(Subterm::ForeignRow(Arc::clone(function)));

    if function.diverges() {
        return Decl {
            doc: None,
            vis_pub,
            label: function.label().to_string(),
            params: Vec::new(),
            output: Subterm::FuncType(FuncType {
                params: iter::once(FuncTypeParam {
                    plicity: Plicity::Implicit,
                    label: Some("A".into()),
                    type_: type_(),
                })
                .chain(signature.params.iter().map(|operand| FuncTypeParam {
                    plicity: Plicity::Explicit,
                    label: None,
                    type_: wire_type(operand),
                }))
                .collect(),
                output: io_of(name("A")),
            })
            .into(),
            body: row,
        };
    }

    let result = match signature.results.shape() {
        ResultShape::Unit | ResultShape::Never => unit(),
        ResultShape::Single(result) => wire_type(&result),
        ResultShape::Record(fields) => record(
            fields
                .into_iter()
                .map(|(label, result)| (label, wire_type(&result)))
                .collect(),
        ),
    };
    let output = io_of(result);

    Decl {
        doc: None,
        vis_pub,
        label: function.label().to_string(),
        params: Vec::new(),
        output: match signature.params.is_empty() {
            true => output,
            false => function_over(signature.params.iter().map(wire_type).collect(), output),
        },
        body: row,
    }
}

/// Handle one user-written `foreign` declaration: register its [`ForeignFunction`] into the compilation's foreign store, and return the ordinary [`LetSignature`] `into_core` lowers it as — wire-type bookkeeping and `host_fn`'s shape stay internal to this module, so `into_core` only ever deals with the same `LetSignature` it already knows how to lower for a plain `TopItem::Let`. `name` is the declaration's fully qualified name (leading `/`, the caller's current position while walking the module tree), which becomes the wasm import string under the `ffi` namespace. Qualified names are unique per compilation (a same-scope duplicate is a binding conflict long before lowering reaches this point), so `register`'s duplicate panic stays what it is everywhere else: a construction bug.
pub(crate) fn foreign_signature(
    declaration: &TopForeign,
    foreigns: &mut ForeignStore,
    name: String,
) -> LetSignature {
    // A user's `foreign` carries its own `---` on the declaration they wrote, which is what a page reads; the row has nothing to add.
    let function = ForeignFunction {
        name,
        label: declaration.label.to_string(),
        signature: declaration.signature.clone(),
    };

    foreigns.register(function.clone());

    host_fn(&Arc::new(function), declaration.vis_pub)
        .into_let()
        .signature
}
