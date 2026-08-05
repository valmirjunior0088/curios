//! Fixed infix operators and their `/syn` concept dispatch targets.

use crate::Qualifier;

/// A fixed infix operator. The surface parser maps an operator symbol (with its precedence) onto one of these; elaboration dispatches it through its `/syn` operator concept once the operand type is known (`elaborate_infix`, [`NumOp::concept_field`]). Both `NumOp` and the `Infix`/`NumLit` nodes are *elaboration-transient*: born in `into_core`, consumed by `elaborate` (replaced with a `Prim` term), so they never reach reduce/convert/zonk/erase.
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
    const ALL: [NumOp; 13] = [
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

    /// The operator dispatching through `concept`'s `field` — [`NumOp::concept_field`]'s reverse, for display folding. `Neq` shares `Eql`'s entry, so the equality spelling wins; negating is the caller's business, exactly as it was when the call was built.
    pub fn from_concept_field(concept: &Qualifier, field: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|op| {
            let (op_concept, op_field) = op.concept_field();
            op_concept == *concept && op_field == field
        })
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

    /// The `/syn` concept (by module segments) and method field this operator dispatches through — the whole operator→concept table backing `elaborate_infix`. Every operator resolves through a witness projection of its concept: infix dispatch is one path, with no carved-out exceptions and no operator whose result the elaborator has to know on its own. `&&`/`||` are ordinary entries, and so is `!=` — it projects `Eql`'s `neq` rather than negating a rebuilt `eql`, which is what lets a carrier with a native disequality instruction name it directly.
    pub fn concept_field(self) -> (Qualifier, &'static str) {
        match self {
            NumOp::Add => (Qualifier::from(["syn", "Add"]), "add"),
            NumOp::Sub => (Qualifier::from(["syn", "Sub"]), "sub"),
            NumOp::Mul => (Qualifier::from(["syn", "Mul"]), "mul"),
            NumOp::Div => (Qualifier::from(["syn", "Div"]), "div"),
            NumOp::Rem => (Qualifier::from(["syn", "Rem"]), "rem"),
            NumOp::Eql => (Qualifier::from(["syn", "Eql", "Eql"]), "eql"),
            NumOp::Neq => (Qualifier::from(["syn", "Eql", "Eql"]), "neq"),
            NumOp::Lt => (Qualifier::from(["syn", "Cmp"]), "lt"),
            NumOp::Gt => (Qualifier::from(["syn", "Cmp"]), "gt"),
            NumOp::Lte => (Qualifier::from(["syn", "Cmp"]), "lte"),
            NumOp::Gte => (Qualifier::from(["syn", "Cmp"]), "gte"),
            NumOp::And => (Qualifier::from(["syn", "And"]), "and"),
            NumOp::Or => (Qualifier::from(["syn", "Or"]), "or"),
        }
    }
}
