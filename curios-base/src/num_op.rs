/// A fixed infix operator. The surface parser maps an operator symbol (with
/// its precedence) onto one of these; elaboration dispatches it through its
/// `/syn` operator concept once the operand type is known (`elaborate_infix`,
/// `operator_concept`). Both `NumOp` and the `Infix`/`NumLit` nodes are
/// *elaboration-transient*:
/// born in `to_core`, consumed by `elaborate` (replaced with a `Prim` term), so
/// they never reach reduce/convert/zonk/erase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NumOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eql,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

impl NumOp {
    /// The operator's source spelling, for printing and error messages.
    pub fn symbol(self) -> &'static str {
        match self {
            NumOp::Add => "+",
            NumOp::Sub => "-",
            NumOp::Mul => "*",
            NumOp::Div => "/",
            NumOp::Rem => "%",
            NumOp::Eql => "==",
            NumOp::Neq => "!=",
            NumOp::Lt => "<",
            NumOp::Gt => ">",
            NumOp::Lte => "<=",
            NumOp::Gte => ">=",
            NumOp::And => "&&",
            NumOp::Or => "||",
        }
    }

    /// Comparison and equality operators yield `Bln` regardless of operand type;
    /// arithmetic operators yield the operand type.
    pub fn result_is_bln(self) -> bool {
        matches!(
            self,
            NumOp::Eql | NumOp::Neq | NumOp::Lt | NumOp::Gt | NumOp::Lte | NumOp::Gte
        )
    }
}
