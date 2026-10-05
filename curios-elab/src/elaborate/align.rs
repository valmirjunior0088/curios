//! How the members written at a site are matched to the slots of the telescope they fill.
//!
//! One rule serves a call's arguments, a lambda's binders, an arm's binders, a concept literal's entries and the entries after a spread. Plain members are always written, in order. Between two plain members the hidden ones — `@` and `use` — are written in order from the first of their run, and the rest of the run may be left out. A written member's slot is therefore its position in its run, never the next slot of its mark: over `(@A: Type, use Show(A), @B: Type, use Show(B), a: A, b: B)`, `(@B, a, b)` fills `A`'s slot, and `(use dict, a, b)` is refused, since the run opens with `@`.
//!
//! **One cursor, moving forward, is the whole rule.** At a hidden slot the next written member either carries the slot's mark and fills it, or is plain — or absent — and the slot is left to the elaborator; a hidden member of the other mark is out of order. Since the cursor never moves back, the written members of a run are a prefix of it with nothing kept to say so. Which slot a member fills depends on nothing written before it but how many there were, where three queues — one per mark — made `f(x, use d)` and `f(use d, @A, x)` the same call.
//!
//! What a slot left out is filled with is its site's to say: a call infers or resolves it, a lambda inserts the binder, an arm binds the payload, a literal resolves the edge or, after a spread, copies it from the base. A written `_` after a mark holds a slot's place and is filled as one left out is, which is how a later member of a run is written alone.

use curios_utilities::Plicity;

#[cfg(test)]
mod tests;

/// How written members failed to meet their slots. Positions are 0-based: `member` among the written members, `slot` among the telescope's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Misaligned {
    /// A plain slot with no plain member written for it.
    Missing,
    /// A plain member written with no plain slot left for it.
    Extra { member: usize },
    /// A hidden member written where a hidden slot of the other mark stands.
    Mark { member: usize, slot: usize },
    /// A hidden member with no slot left in its run: written after the run's last slot, or after the plain member the run precedes.
    Surplus { member: usize },
}

/// The plain slot a surplus hidden member stands at — the one the plain members written before it have reached — or `None` where it is written past the last plain slot. A site that binds reports the first as a mark no plain slot takes and the second as a member that claims nothing.
pub(crate) fn stands_at(slots: &[Plicity], written: &[Plicity], member: usize) -> Option<usize> {
    let plain_before = written[..member]
        .iter()
        .filter(|mark| **mark == Plicity::Explicit)
        .count();
    slots
        .iter()
        .enumerate()
        .filter(|(_, slot)| **slot == Plicity::Explicit)
        .map(|(index, _)| index)
        .nth(plain_before)
}

/// For each slot, the written member that fills it, or `None` where the slot is left to the elaborator.
pub(crate) fn align(
    slots: &[Plicity],
    written: &[Plicity],
) -> Result<Vec<Option<usize>>, Misaligned> {
    let mut fills = Vec::with_capacity(slots.len());
    let mut cursor = 0;

    for (slot, mark) in slots.iter().enumerate() {
        match (mark, written.get(cursor)) {
            (Plicity::Explicit, Some(Plicity::Explicit)) => {
                fills.push(Some(cursor));
                cursor += 1;
            }
            (Plicity::Explicit, Some(_)) => return Err(Misaligned::Surplus { member: cursor }),
            (Plicity::Explicit, None) => return Err(Misaligned::Missing),
            (_, Some(Plicity::Explicit) | None) => fills.push(None),
            (mark, Some(member)) if mark == member => {
                fills.push(Some(cursor));
                cursor += 1;
            }
            (_, Some(_)) => {
                return Err(Misaligned::Mark {
                    member: cursor,
                    slot,
                });
            }
        }
    }

    match written.get(cursor) {
        None => Ok(fills),
        Some(Plicity::Explicit) => Err(Misaligned::Extra { member: cursor }),
        Some(_) => Err(Misaligned::Surplus { member: cursor }),
    }
}
