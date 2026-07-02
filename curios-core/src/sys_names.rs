//! The `/sys` names core is allowed to know about: the operator concepts the
//! infix elaborator dispatches through. The compiler owns `/sys` — it is
//! built as AST in `curios-text`'s prelude, never parsed from user code — so
//! hardcoding these couples core to the prelude exactly the way `wire_term`
//! mirrors the prelude's host-function reading. Keep every such literal in
//! this module.

use super::NumOp;

/// The concept (by qualified name) and method field an infix operator
/// dispatches through — the whole operator→concept table. `None` for
/// `&&`/`||`: the short-circuit operators are control flow, hardcoded on
/// `Bln` and deliberately non-overloadable (the same status as `if`/`match`,
/// not a second overload standard). `!=` shares `Eql/eql` and negates the
/// rebuilt equality.
pub fn operator_concept(op: NumOp) -> Option<(&'static str, &'static str)> {
    Some(match op {
        NumOp::Add => ("/sys/Add", "add"),
        NumOp::Sub => ("/sys/Sub", "sub"),
        NumOp::Mul => ("/sys/Mul", "mul"),
        NumOp::Div => ("/sys/Div", "div"),
        NumOp::Rem => ("/sys/Rem", "rem"),
        NumOp::Eql | NumOp::Neq => ("/sys/Eql", "eql"),
        NumOp::Lt => ("/sys/Cmp", "lt"),
        NumOp::Gt => ("/sys/Cmp", "gt"),
        NumOp::Lte => ("/sys/Cmp", "lte"),
        NumOp::Gte => ("/sys/Cmp", "gte"),
        NumOp::And | NumOp::Or => return None,
    })
}
