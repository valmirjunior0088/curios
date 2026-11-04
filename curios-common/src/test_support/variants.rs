//! An enum's variants by the identifiers they are declared under, for a test that names one as data: what a record of an expected error states, read by the suite that holds it to the code.

#[cfg(test)]
mod tests;

/// An enum whose variants a test names: every one of them, and the one a value is. Implemented by [`variants!`](crate::variants), whose match is exhaustive, so a variant added or removed does not compile until it is named there.
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
        impl $crate::test_support::Variants for $type {
            const NAMES: &'static [&'static str] = &[$(stringify!($variant)),+];

            fn variant_name(&self) -> &'static str {
                match self {
                    $(Self::$variant { .. } => stringify!($variant)),+
                }
            }
        }
    };
}
