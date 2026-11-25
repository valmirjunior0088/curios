//! The rounds a chain churns through, which the churn suite holds a collection to and the bench's decomposition of the chain's collections runs at.

/// The churn phase is read at this K and at none, and costs the difference, which subtracts startup, the initial build, and — in the ballast arms — the whole ballast phase.
pub const ROUNDS: u64 = 400;
