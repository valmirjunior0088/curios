//! How a wire row becomes a Curios declaration: the surface type a [`WireType`] denotes, the declaration a [`ForeignFunction`] surfaces as, and the registration a user's own `foreign` goes through.
//!
//! Serves two callers that are peers rather than layers — the `/sys` roster, whose rows come off `curios_abi::host_ops`, and `into_core`, whose row is the one the user wrote. Both get the same shape, which is what keeps a call across the wire a host effect whoever declared it. It sits under `sys_module` because a wire type *is* a `/sys` carrier: the vocabulary this reads is the roster's own.
//!
//! **[`host_fn`] wraps every store row's result in `Io`, at this one site.** That is load-bearing rather than incidental: it is half of what narrows `documentation/soundness/per-term-rules/a-term-outside-io-performs-no-effect.md` from an argument about the whole library to a check over two Rust tables — a foreign row cannot introduce an eliminator for the effect type, because every row's result is built here and [`WireType`] is a closed enum with no case that could name an `Io` in a domain. `curios-abi`'s `host_ops` names this function as the definition of guest result shape for the same reason. A second site building a row's result would end both.

use {
    super::{Decl, helpers::*},
    crate::{Doc, LetSignature, Subterm, Term, TopForeign},
    curios_abi::{ForeignFunction, ForeignStore, Namespace, ResultShape, WireType},
    curios_utilities::{Grain, Plicity},
    std::sync::Arc,
};

/// The surface type a host-boundary [`WireType`] denotes — the prelude's reading of the signature, mirrored by `core::wire_term` after lowering.
fn wire_type(type_: &WireType) -> Term {
    match type_ {
        WireType::Nat => nat(),
        WireType::Int => int(),
        WireType::Bool => bool_(),
        WireType::Bytes => bin(Grain::X),
        WireType::Handle => handle(),
        WireType::List(element) => list_of(wire_type(&(*element).into())),
    }
}

/// A host-function declaration generated from a foreign-store row: parameter names/types and the result shape (unit, bare type, named record) come off the `WireSignature`, and the body bakes the generic `Foreign` intrinsic applied to the parameter names. Used both for the builtin store's rows (always `pub`) and, via [`foreign_signature`], for a user's own `foreign` declaration (`vis_pub` follows what they wrote).
///
/// The result is an `Io`, and this one site is what makes that true of every row the store describes — a user's own `foreign` declaration included, since a call across the wire is a host effect whoever declared it. The wire contract does not move: `curios-abi` describes the same shapes and only the guest-facing type changes, so a multi-result row reads `Io({status : Nat, bytes : Bytes})` with the record still inside the wrapper.
///
/// A row with no parameters becomes a *constant* rather than a nullary function, which is not a case here but a consequence of handing [`Decl`] an empty telescope — see `Decl::signature`.
pub(super) fn host_fn(function: &Arc<ForeignFunction>, vis_pub: bool) -> Decl {
    // What the row says of itself, which for a builtin is the roster's own `///` and for a user's `foreign` is nothing — their prose sits on the declaration they wrote.
    let doc = match function.description.is_empty() {
        true => None,
        false => Some(Doc {
            lines: vec![function.description.clone()],
            span: None,
        }),
    };
    let signature = &function.signature;

    let result = match signature.results.shape() {
        ResultShape::Unit => unit(),
        ResultShape::Single(result) => wire_type(&result),
        ResultShape::Record(fields) => record(
            fields
                .into_iter()
                .map(|(label, result)| (label, wire_type(&result)))
                .collect(),
        ),
    };
    let output = io_of(result);

    let body = Term::from(Subterm::Foreign(
        Arc::clone(function),
        signature
            .params
            .iter()
            .map(|(param, _)| name(param))
            .collect(),
    ));

    Decl {
        doc,
        vis_pub,
        label: function.label.clone(),
        params: signature
            .params
            .iter()
            .map(|(param, type_)| (Plicity::Explicit, param.clone(), wire_type(type_)))
            .collect(),
        output,
        body,
    }
}

/// Handle one user-written `foreign` declaration: register its [`ForeignFunction`] into the compilation's (non-`host_ops`) foreign store, and return the ordinary [`LetSignature`] `into_core` lowers it as — wire-type bookkeeping and `host_fn`'s shape stay internal to this module, so `into_core` only ever deals with the same `LetSignature` it already knows how to lower for a plain `TopItem::Let`. `name` is the declaration's fully qualified name (leading `/`, the caller's current position while walking the module tree), which becomes the wasm import string under the `ffi` namespace. Qualified names are unique per compilation (a same-scope duplicate is a binding conflict long before lowering reaches this point), so `register`'s duplicate panic stays what it is everywhere else: a construction bug.
pub(crate) fn foreign_signature(
    declaration: &TopForeign,
    foreigns: &mut ForeignStore,
    name: String,
) -> LetSignature {
    let function = ForeignFunction {
        namespace: Namespace::Ffi,
        name,
        subject: None,
        label: declaration.label.to_string(),
        signature: declaration.signature.clone(),
        // A user's `foreign` carries its own `-- |` on the declaration they wrote, which is what a page reads; the row has nothing to add.
        description: String::new(),
    };

    foreigns.register(function.clone());

    host_fn(&Arc::new(function), declaration.vis_pub)
        .into_let()
        .signature
}
