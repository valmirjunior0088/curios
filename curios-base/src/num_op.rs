//! Fixed infix operators and their `/syn` concept dispatch targets.

use crate::Qualifier;

/// A fixed infix operator. The surface parser maps an operator symbol (with
/// its precedence) onto one of these; elaboration dispatches it through its
/// `/syn` operator concept once the operand type is known (`elaborate_infix`,
/// [`NumOp::concept_field`]). Both `NumOp` and the `Infix`/`NumLit` nodes are
/// *elaboration-transient*:
/// born in `into_core`, consumed by `elaborate` (replaced with a `Prim` term), so
/// they never reach reduce/convert/zonk/erase.
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

    /// Comparison and equality operators yield `Bool` regardless of operand type;
    /// arithmetic operators yield the operand type.
    pub fn result_is_bln(self) -> bool {
        matches!(
            self,
            NumOp::Eql | NumOp::Neq | NumOp::Lt | NumOp::Gt | NumOp::Lte | NumOp::Gte
        )
    }

    /// The `/syn` concept (by module segments) and method field this operator
    /// dispatches through — the whole operator→concept table backing
    /// `elaborate_infix`. Every operator, `&&`/`||` included, resolves through
    /// a witness projection of its concept: infix dispatch is one path, with
    /// no carved-out exceptions. `Neq` shares `Eql`'s entry; negating the
    /// rebuilt equality is the caller's job.
    pub fn concept_field(self) -> (Qualifier, &'static str) {
        match self {
            NumOp::Add => (Qualifier::from(["syn", "Add"]), "add"),
            NumOp::Sub => (Qualifier::from(["syn", "Sub"]), "sub"),
            NumOp::Mul => (Qualifier::from(["syn", "Mul"]), "mul"),
            NumOp::Div => (Qualifier::from(["syn", "Div"]), "div"),
            NumOp::Rem => (Qualifier::from(["syn", "Rem"]), "rem"),
            NumOp::Eql | NumOp::Neq => (Qualifier::from(["syn", "Eql"]), "eql"),
            NumOp::Lt => (Qualifier::from(["syn", "Cmp"]), "lt"),
            NumOp::Gt => (Qualifier::from(["syn", "Cmp"]), "gt"),
            NumOp::Lte => (Qualifier::from(["syn", "Cmp"]), "lte"),
            NumOp::Gte => (Qualifier::from(["syn", "Cmp"]), "gte"),
            NumOp::And => (Qualifier::from(["syn", "And"]), "and"),
            NumOp::Or => (Qualifier::from(["syn", "Or"]), "or"),
        }
    }
}
