//! What a comparison of two expressions concludes, at the strength its reader may use.

/// What a comparison concludes where inversion may read it: every residual is equivalent to the equation it came from, so reading one as an equation to deduce is sound.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Deduction<R> {
    /// The two expressions are equal under the atoms' identity.
    Equal,
    /// The equation has no solution for any values of its atoms.
    Impossible,
    /// The residual holds exactly when the equation does.
    Equivalent(R),
    /// Nothing concluded: the operation made no progress it could stand on. The caller keeps its own fallback, and nothing here is a refusal.
    Undecided,
}

/// What a comparison concludes where only conversion may read it: [`Deduction`]'s verdicts, and a residual that is merely sufficient.
///
/// **Sufficient is not equivalent.** `x · f = x · g` holds when `f = g`, and at `x = 0` it holds whatever `f` and `g` are, so the residual `f = g` may be checked to establish the equation and may never be deduced from it. Conversion checks a sufficient residual; inversion is handed [`Deduction`]s only, which have no such variant, so the restriction is the type's rather than a caller's to remember. A sufficient residual that fails establishes nothing, and certainly not that the equation is impossible.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Conclusion<R> {
    /// The two expressions are equal under the atoms' identity.
    Equal,
    /// The equation has no solution for any values of its atoms.
    Impossible,
    /// The residual holds exactly when the equation does.
    Equivalent(R),
    /// Establishing the residual establishes the equation; failing to establish it establishes nothing.
    Sufficient(R),
    /// Nothing concluded.
    Undecided,
}

impl<R> Deduction<R> {
    /// The same verdict over a residual `rebuild` makes of this one's.
    pub fn map<S>(self, rebuild: impl FnOnce(R) -> S) -> Deduction<S> {
        match self {
            Deduction::Equal => Deduction::Equal,
            Deduction::Impossible => Deduction::Impossible,
            Deduction::Equivalent(residual) => Deduction::Equivalent(rebuild(residual)),
            Deduction::Undecided => Deduction::Undecided,
        }
    }
}

/// A deduction is a conclusion at the same strength: everything inversion may read, conversion may too.
impl<R> From<Deduction<R>> for Conclusion<R> {
    fn from(deduction: Deduction<R>) -> Self {
        match deduction {
            Deduction::Equal => Conclusion::Equal,
            Deduction::Impossible => Conclusion::Impossible,
            Deduction::Equivalent(residual) => Conclusion::Equivalent(residual),
            Deduction::Undecided => Conclusion::Undecided,
        }
    }
}
