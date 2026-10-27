//! Erasure's readings through the kernel's door ([`curios_cert::Reader`]): each a judgment the kernel makes, its refusal turned into erasure's ([`Error::reading`]), and the two walks erasure makes over what the door answers — a telescope opened a binder at a time, and a type read past its parameters to the half its end is in.

use {
    crate::Error,
    curios_cert::Reader,
    curios_core::{Advance, Bound},
};

/// `term` reduced to its head, by the reduction that accepted it.
pub fn reduced(reader: &mut Reader, term: &curios_core::Term) -> Result<curios_core::Term, Error> {
    reader
        .head(term)
        .map_err(|refusal| Error::reducing(term, refusal))
}

/// The type of `term` — a name's, a neutral's, a type former's sort — as the kernel infers it.
pub fn inferred(reader: &mut Reader, term: &curios_core::Term) -> Result<curios_core::Term, Error> {
    reader
        .infer(term)
        .map_err(|refusal| Error::reading(term, refusal))
}

/// Put the case `value` of `scrutinee` in force, as the arm under it was checked.
pub fn refine(
    reader: &mut Reader,
    scrutinee: &curios_core::Term,
    value: &curios_core::Term,
) -> Result<(), Error> {
    reader
        .case(scrutinee, value)
        .map_err(|refusal| Error::reading(scrutinee, refusal))
}

/// Open the next entry of `walk` with a fresh binder assumed at `domain`, and hand the binder back.
pub fn advance_assumed(
    reader: &mut Reader,
    walk: &mut impl Advance,
    domain: &curios_core::Term,
) -> curios_core::Free {
    let binder = walk.advance_fresh(|hint| reader.fresh(hint));
    reader.assume(&binder, domain);

    binder
}

/// Whether a value of type `type_` carries nothing at run time: a type, a proof, or a function that ultimately answers one, read past its parameters, each opened as a binder, to the half the kernel classifies its end in. The empty tuple is none of them, so `{}`, `{ .., {} }` and `X -> {}` are kept.
///
/// **One reading's answer.** A domain that is a variable here is a proof where the variable is instantiated at a proposition, so a `false` says only that this reading keeps the value, and decides what a slot holds, never whether the slot is there; a `true` holds at every instantiation, since a sort, a proposition and a function into one stay what they are under any substitution.
pub fn is_erasable(reader: &mut Reader, type_: &curios_core::Term) -> Result<bool, Error> {
    reader.scoped(|reader| {
        let mut type_ = type_.clone();
        loop {
            let half = reader
                .erased(&type_)
                .map_err(|refusal| Error::reading(&type_, refusal))?;
            if half.is_some() {
                return Ok(true);
            }
            let at_head = reduced(reader, &type_)?;
            let curios_core::Subterm::FuncType(curios_core::FuncType { telescope }) = &*at_head
            else {
                return Ok(false);
            };
            let mut cursor = telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                advance_assumed(reader, &mut cursor, &domain);
            }
            type_ = cursor.body().expect("a cursor past every entry");
        }
    })
}

/// The binders `telescope` opens at, each a fresh variable assumed at its declared type: the abstract parameters a declaration's fields are read at.
pub fn open_opaque<B: Bound>(
    reader: &mut Reader,
    telescope: curios_core::Telescope<B>,
) -> Vec<curios_core::Term> {
    let mut cursor = telescope.cursor();
    while let Some((_, type_)) = cursor.entry() {
        advance_assumed(reader, &mut cursor, &type_);
    }
    cursor.into_args()
}
