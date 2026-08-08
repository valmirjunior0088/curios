//! The fixed infix operators of the surface grammar.

/// A fixed infix operator. The surface parser maps an operator symbol (with its precedence) onto one of these; elaboration dispatches it through its `/syn` operator concept once the operand type is known (`elaborate_infix`, [`OperatorSyntax::concept_field`](crate::OperatorSyntax::concept_field)). Both `NumOp` and the `Transient`-grouped `Infix`/`NumLit` nodes are *elaboration-transient*: born in `into_core`, consumed by `elaborate` (replaced with an `Intrinsic` term), so they never reach reduce/convert/zonk/erase.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
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
    /// Every operator, in declaration order — the roster the reverse lookups below derive from.
    pub(crate) const ALL: [NumOp; 13] = [
        NumOp::Add,
        NumOp::Sub,
        NumOp::Mul,
        NumOp::Div,
        NumOp::Rem,
        NumOp::Eql,
        NumOp::Neq,
        NumOp::Lt,
        NumOp::Gt,
        NumOp::Lte,
        NumOp::Gte,
        NumOp::And,
        NumOp::Or,
    ];

    /// The operator spelled `symbol`, if any — [`NumOp::symbol`]'s reverse, for display folding. Precise because operator symbols and identifiers never overlap.
    pub fn from_symbol(symbol: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|op| op.symbol() == symbol)
    }

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
}
