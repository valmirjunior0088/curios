//! The sorts a form of a declaration is erased at.
//!
//! The kernel judges a declaration that leaves sorts to its caller at every assignment of `Prop` and `Type` to them, and reads no term at an open sort. Erasure follows it: such a declaration erases to a form at each assignment ([`ErasedForms`](crate::ErasedForms)), a form is known by its declaration's name and the sorts it was read at, and an occurrence links the form at the sorts its instance states.

/// One assignment of `Prop` and `Type` to the sorts a declaration leaves open: as many sorts as it leaves, bit `i` set where sort `i` is `Type`. Its index is its place in [`curios_core::Quality::assignments`]' order, and a declaration that leaves none has the one empty assignment, [`Sorts::NONE`].
///
/// Bits rather than a list, so a form's name is a copy: [`curios_core::UniverseContext::QUALITY_LIMIT`] bounds what a declaration may leave, and the kernel refuses one past it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Sorts {
    count: u8,
    types: u8,
}

impl Sorts {
    /// The assignment of a declaration that leaves no sort open.
    pub const NONE: Sorts = Sorts { count: 0, types: 0 };

    /// The assignment `qualities` states, each `Prop` or `Type`: what an occurrence's instance states, read where the kernel judged it, so none is open.
    pub fn of(qualities: &[curios_core::Quality]) -> Self {
        assert!(
            qualities.len() <= curios_core::UniverseContext::QUALITY_LIMIT,
            "an instance states no more sorts than a declaration may leave",
        );
        let types =
            qualities
                .iter()
                .enumerate()
                .fold(0u8, |types, (sort, quality)| match quality {
                    curios_core::Quality::Type => types | (1 << sort),
                    curios_core::Quality::Prop => types,
                    open => {
                        unreachable!("an instance the kernel judged states no open sort: {open:?}")
                    }
                });

        Self {
            count: qualities.len() as u8,
            types,
        }
    }

    /// Every assignment of `count` sorts, in [`curios_core::Quality::assignments`]' order: what a declaration leaving `count` erases at.
    pub fn every(count: usize) -> impl Iterator<Item = Sorts> {
        curios_core::Quality::assignments(count).map(|qualities| Sorts::of(&qualities))
    }

    /// The qualities this assignment states, sort by sort.
    pub fn qualities(self) -> Vec<curios_core::Quality> {
        (0..self.count)
            .map(|sort| match (self.types >> sort) & 1 {
                0 => curios_core::Quality::Prop,
                _ => curios_core::Quality::Type,
            })
            .collect()
    }

    /// This assignment's place among its count's, as [`Sorts::every`] yields them.
    pub fn index(self) -> usize {
        self.types as usize
    }

    /// Whether this assignment states no sort.
    pub fn is_none(self) -> bool {
        self.count == 0
    }

    /// How many sorts this assignment states.
    pub fn count(self) -> usize {
        self.count as usize
    }
}

impl std::fmt::Display for Sorts {
    /// The assignment as an instance writes it: `{Prop, Type}`, nothing where it states no sort.
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_none() {
            return Ok(());
        }
        let qualities = self
            .qualities()
            .into_iter()
            .map(|quality| match quality {
                curios_core::Quality::Prop => "Prop",
                _ => "Type",
            })
            .collect::<Vec<_>>();

        write!(formatter, "{{{}}}", qualities.join(", "))
    }
}

/// A declaration at one assignment of its sorts: what a fragment imports another item's binding and row by, and what a program links them under.
pub type AtSorts = (curios_core::Global, Sorts);
