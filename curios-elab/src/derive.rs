//! Derived witness bodies: the body a body-less `satisfy C(T);` asks the compiler to write.
//!
//! Lowering carries the declaration into Core as the same anonymous definition a written witness produces, with [`Transient::Derive`] in body position, so the witness's telescope — its implicit binders and `use` premises — is in scope when the body is checked, and its signature registers in the witness table exactly as a written one does (orphan and duplicate-key refusals need no body). Checking the transient against the concept application is what writes the body: the derivation registered for the concept's registry slot produces the Core the lowerer would have produced for the equivalent written witness, and that Core is elaborated under the same expectation, so a derived body is typed, resolved, zonked and certified like any authored one — the kernel never sees the transient.
//!
//! The refusals are decided here, at the declaration, before any body exists. A sealed concept refuses a derived witness outside its module exactly as it refuses a written one — the same error, from the same rule — stated ahead of the derivation lookup so that derivation is never a door through representation privacy. A concept with no derivation refuses by name; derivability is a property of the concept, registered against its slot, and never inferred from its shape.

use {
    super::{Context, Error, Mode, reduce_with},
    curios_core::{Global, StructType, Subterm, Term},
    curios_utilities::SyntaxRegistry,
};

/// A concept the compiler can write a witness body for. Empty until a derivation registers.
enum Derivation {}

/// The derivation registered for `concept`'s slot, if any.
fn derivation_for(_syntax: &SyntaxRegistry, _concept: &Global) -> Option<Derivation> {
    None
}

/// Check a `Derive` transient against its expected type, writing the body the declaration asked for.
pub(crate) fn elaborate_derive(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Mode::Check(expected) = &mode else {
        return Err(Error::derive_outside_witness().at_opt(term.span()));
    };

    // The witness's declared type elaborated before its body, so the expectation reduces to the concept's record type — a `StructType` whose name is registered as a concept.
    let reduced = reduce_with(context, expected)?;
    let Subterm::StructType(StructType { name, .. }) = &*reduced else {
        return Err(Error::derive_outside_witness().at_opt(term.span()));
    };
    if context.concept(name).is_none() {
        return Err(Error::derive_outside_witness().at_opt(term.span()));
    }

    // Sealing first: the rule `elaborate_struct` applies to a written literal, decided here before any body is written.
    let record = context
        .struct_decl(name)
        .cloned()
        .expect("a registered concept has a backing struct declaration");
    if !record.rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(&record.module))
    {
        return Err(Error::private_representation(name.symbol()).at_opt(term.span()));
    }

    match derivation_for(&context.syntax(), name) {
        Some(derivation) => match derivation {},
        None => Err(Error::no_derivation(name.symbol()).at_opt(term.span())),
    }
}
