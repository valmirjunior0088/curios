//! What a row asks of a call's operands before its host is asked at all.
//!
//! A row's types say what crosses. What they cannot say of the operands, and a host must be able to rely on — two lists that go together having one length — is a [`Requirement`], and [`HostOp::admit`] holds a call to its row's before the host is asked, answering the sentence a refusal reports. A reply is held to nothing here: it is a value of the row's wire types because the adapter lowers it from the reply's own type, and what a row promises beyond that is its host's to keep and its host's tests' to hold.

use super::HostOp;

#[cfg(test)]
mod tests;

/// A relation a call's operands must hold before the host is asked at all; a call that breaks one is malformed, and refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    /// `a` and `b` have one length.
    SameLength { a: &'static str, b: &'static str },
}

impl HostOp {
    /// Refuse a call whose operands break a requirement of the row, `operands` their [`measure`](super::WireOperand::measure)s in the row's order.
    pub fn admit(self, operands: &[Option<u64>]) -> Result<(), String> {
        for requirement in self.requirements() {
            match *requirement {
                Requirement::SameLength { a, b } => {
                    let (a_length, b_length) =
                        (self.measure(operands, a), self.measure(operands, b));

                    if a_length != b_length {
                        return Err(format!(
                            "`{a}` holds {a_length} elements and `{b}` {b_length}, which must be as many"
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// The measure of the operand the row names `name`.
    fn measure(self, operands: &[Option<u64>], name: &str) -> u64 {
        operands[self.operand(name)].unwrap_or_else(|| {
            panic!(
                "`{}` reads `{name}`, which it has no measure of",
                self.name()
            )
        })
    }
}
