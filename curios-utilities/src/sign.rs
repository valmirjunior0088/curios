//! The written sign of a numeric literal, as a fact rather than two booleans: a literal is unmarked, marked `+`, or marked `-`, and no fourth state exists to guard against.

/// The sign a numeric literal was written with. `Unmarked` keeps `Nat` in the realization candidate set; a written mark drops it and defaults the literal to `Int`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Sign {
    Unmarked,
    Positive,
    Negative,
}

impl Sign {
    /// Whether any mark was written — what retires the old `signed` boolean.
    pub fn is_marked(self) -> bool {
        !matches!(self, Sign::Unmarked)
    }

    /// Whether the written mark was `-` — what retires the old `negative` boolean.
    pub fn is_negative(self) -> bool {
        matches!(self, Sign::Negative)
    }

    /// The mark as written, for the printers.
    pub fn symbol(self) -> &'static str {
        match self {
            Sign::Unmarked => "",
            Sign::Positive => "+",
            Sign::Negative => "-",
        }
    }
}
