//! How a wire row becomes a Curios declaration: the surface type a [`WireType`] denotes, the declaration a [`ForeignFunction`] surfaces as, and the registration a user's own `foreign` goes through.
//!
//! Serves two callers that are peers rather than layers — the `/sys` roster, whose rows come off `curios_abi::host_ops`, and `into_core`, whose row is the one the user wrote. Both get the same shape, which is what keeps a call across the wire a host effect whoever declared it. It sits under `sys_module` because a wire type *is* a `/sys` carrier: the vocabulary this reads is the roster's own.
//!
//! **[`host_fn`] wraps every store row's result in `Io`, at this one site.** That is load-bearing rather than incidental: it is half of what narrows `documentation/soundness/per-term-rules/a-term-outside-io-performs-no-effect.md` from an argument about the whole library to a check over two Rust tables — a foreign row cannot introduce an eliminator for the effect type, because every row's result is built here and [`WireType`] is a closed enum with no case that could name an `Io` in a domain. `curios-abi`'s `host_ops` names this function as the definition of guest result shape for the same reason. A second site building a row's result would end both.

use {
    super::{Decl, helpers::*},
    crate::{Doc, Intrinsic, LetSignature, Subterm, Term, TopForeign},
    curios_abi::{
        DeclaredForeign, ForeignFunction, ForeignStore, Outcome, ResultShape, WireType, status,
    },
    curios_num::Grain,
    curios_utilities::Plicity,
    std::sync::Arc,
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

/// The guest's reading of a builtin row's outcome: the type a `/sys` declaration answers and the body that answers it, from the row's raw result type and its raw call. The call is bound with `/sys/Io/bind` and answered with `/sys/Io/pure`, called as any caller calls them rather than built inline from their intrinsics — an inline bind leaves a description the erased optimizer cannot fold, and every dead top-level value holding one, `/std/Tui/Session/enter` among them, then survives pruning into every program. The status is read before anything else, so a payload is projected only under the status that makes it one — `Result(Nat, T)` for a fallible row, `ok` a success and every other status the failure it names; `Result(Nat, Option(T))` for a stream, `eof` its end; and `Option(T)` for a lookup, `not_found` its absence. `T` is the payload the status stands beside: `{}` for none, the one value, or the record of several. A row whose outcome needs no reading — one that returns, and every declared row — answers its raw call.
fn adapted(function: &ForeignFunction, raw: Term, call: Term) -> (Term, Term) {
    let outcome = match function {
        ForeignFunction::Builtin(op) => op.outcome(),
        ForeignFunction::Declared(_) => Outcome::Returns,
    };

    if matches!(outcome, Outcome::Returns | Outcome::Diverges) {
        return (io_of(raw), call);
    }

    let results = function.signature().results.iter().collect::<Vec<_>>();
    let ((label, _), payload) = results
        .split_first()
        .expect("a row with a status answers one");

    assert_eq!(
        *label,
        "status",
        "`{}` answers its status first",
        function.name()
    );

    let reply = "reply";
    let code = match payload.is_empty() {
        true => name(reply),
        false => project(name(reply), "status"),
    };
    let (payload_type, payload) = match payload {
        [] => (unit(), tuple(Vec::new())),
        [(label, wire)] => (wire_type(wire), project(name(reply), label)),
        fields => (
            record(
                fields
                    .iter()
                    .map(|(label, wire)| (*label, wire_type(wire)))
                    .collect(),
            ),
            tuple(
                fields
                    .iter()
                    .map(|(label, _)| (*label, project(name(reply), label)))
                    .collect(),
            ),
        ),
    };

    let is = |status: u64| intrinsic(Intrinsic::NatEql(code.clone(), nat_lit(status)));
    let result_of = |success| applied(sys_op(&["sys", "Result"]), vec![nat(), success]);
    let option_of = |value| applied(sys_op(&["sys", "Option"]), vec![value]);
    let success = |value| applied(sys_op(&["sys", "Result", "success"]), vec![value]);
    let failure = applied(sys_op(&["sys", "Result", "failure"]), vec![code.clone()]);
    let some = |value| applied(sys_op(&["sys", "Option", "some"]), vec![value]);
    let none = applied(sys_op(&["sys", "Option", "none"]), Vec::new());

    let (answer, value) = match outcome {
        Outcome::Fallible => (
            result_of(payload_type),
            branch(is(status::OK), success(payload), failure),
        ),
        Outcome::Stream => (
            result_of(option_of(payload_type)),
            branch(
                is(status::OK),
                success(some(payload)),
                branch(is(status::EOF), success(none), failure),
            ),
        ),
        Outcome::Lookup => (
            option_of(payload_type),
            branch(is(status::OK), some(payload), none),
        ),
        Outcome::Returns | Outcome::Diverges => unreachable!("answered above"),
    };

    let body = applied(
        sys_op(&["sys", "Io", "bind"]),
        vec![
            call,
            lambda(
                reply,
                raw,
                applied(sys_op(&["sys", "Io", "pure"]), vec![value]),
            ),
        ],
    );

    (io_of(answer), body)
}

/// A host-function declaration generated from a foreign-store row: parameter names/types and the raw result shape (unit, bare type, named record) come off the `WireSignature`, and the body bakes the generic `Foreign` intrinsic applied to the parameter names, read through [`adapted`]. Used both for the builtin store's rows (always `pub`) and, via [`foreign_signature`], for a user's own `foreign` declaration (`vis_pub` follows what they wrote).
///
/// The result is an `Io`, and this one site is what makes that true of every row the store describes — a user's own `foreign` declaration included, since a call across the wire is a host effect whoever declared it. The wire contract does not move: `curios-abi` describes the same shapes, and only the guest-facing type is read out of them, so `/sys/Handle/read` answers `Io(Result(Nat, Option(Bytes)))` where the wire carries `{status: Nat, bytes: Bytes}`.
///
/// A row with no parameters becomes a *constant* rather than a nullary function, which is not a case here but a consequence of handing [`Decl`] an empty telescope — see `Decl::signature`.
pub(super) fn host_fn(function: &Arc<ForeignFunction>, vis_pub: bool) -> Decl {
    // What the row says of itself, which for a builtin is the roster's own `///` and for a user's `foreign` is nothing — their prose sits on the declaration they wrote.
    let doc = match function.description().is_empty() {
        true => None,
        false => Some(Doc {
            lines: vec![function.description().to_string()],
            span: None,
        }),
    };
    let signature = function.signature();

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
    // A diverging row takes the type its description yields as an implicit operand ahead of its wire parameters, so an exiting arm ends a region of any type — sound because `Io` has no eliminator, and an inhabitant of `Io(False)` proves nothing.
    let yielded = function
        .diverges()
        .then(|| (Plicity::Implicit, "A".to_string(), type_()));
    let call = Term::from(Subterm::Foreign(
        Arc::clone(function),
        yielded
            .iter()
            .map(|(_, param, _)| name(param))
            .chain(signature.params.iter().map(|(param, _)| name(param)))
            .collect(),
    ));
    let (output, body) = match yielded {
        Some(_) => (io_of(name("A")), call),
        None => adapted(function, result, call),
    };

    Decl {
        doc,
        vis_pub,
        label: function.label().to_string(),
        params: yielded
            .into_iter()
            .chain(
                signature
                    .params
                    .iter()
                    .map(|(param, type_)| (Plicity::Explicit, param.clone(), wire_type(type_))),
            )
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
    // A user's `foreign` carries its own `---` on the declaration they wrote, which is what a page reads; the row has nothing to add.
    let function = ForeignFunction::Declared(DeclaredForeign {
        name,
        label: declaration.label.to_string(),
        signature: declaration.signature.clone(),
    });

    foreigns.register(function.clone());

    host_fn(&Arc::new(function), declaration.vis_pub)
        .into_let()
        .signature
}
