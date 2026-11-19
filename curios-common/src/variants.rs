//! An enum's variants by the identifiers they are declared under: what a checker's refusal is named by, in `wonder --false`'s answer and the soundness board's records alike, and what a record of an expected error states, read by the suite that holds it to the code.

#[cfg(test)]
mod tests;

/// An enum whose variants are named as data: every one of them, and the one a value is. Implemented by [`variants!`](macro@crate::variants), whose match is exhaustive, so a variant added or removed does not compile until it is named there.
pub trait Variants {
    /// Every variant's name, in the order the implementation lists them.
    const NAMES: &'static [&'static str];

    /// The name of the variant `self` is.
    fn variant_name(&self) -> &'static str;
}

/// Implement [`Variants`] for an enum by listing every one of its variants: `variants!(Error { Mismatch, Unbound })`. A unit, tuple or struct variant is listed alike.
#[macro_export]
macro_rules! variants {
    ($type:ty { $($variant:ident),+ $(,)? }) => {
        impl $crate::Variants for $type {
            const NAMES: &'static [&'static str] = &[$(stringify!($variant)),+];

            fn variant_name(&self) -> &'static str {
                match self {
                    $(Self::$variant { .. } => stringify!($variant)),+
                }
            }
        }
    };
}
