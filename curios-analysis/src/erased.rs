//! Which half of the erased program a position belongs to.
//!
//! The classification is shared; the obligations that consume it are not. `curios-cert`'s `partial_definitions` and `check_positions` decide (T) and (V) from its own typing walk, and `curios-elab` decides them from its elaborator's check sites — two independent answers to one question, deliberately. What both need to name is the same pair of halves, so the pair lives here.

/// Which half of the erased program a position belongs to. Both are deleted, so both must terminate; the distinction is only what a diagnostic should call it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Erased {
    Proof,
    Type,
}

impl std::fmt::Display for Erased {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Erased::Proof => write!(formatter, "proof"),
            Erased::Type => write!(formatter, "type"),
        }
    }
}
