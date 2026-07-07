//! The `/syn` names core is allowed to know about: the operator concepts the
//! infix elaborator dispatches through. `/syn` holds exactly the modules the
//! compiler emits references to (see also `to_core/lower`'s `/syn/Str` and
//! `/syn/Monad` desugaring targets) — the concepts are declared in
//! `curios-text/syn.crs`, their witnesses in the `/std` operator facades.
//! Keep every such literal in this module.

use curios_base::NumOp;

/// The concept (by qualified name) and method field an infix operator
/// dispatches through — the whole operator→concept table. `None` for
/// `&&`/`||`: the short-circuit operators are control flow, hardcoded on
/// `Bln` and deliberately non-overloadable (the same status as `if`/`match`,
/// not a second overload standard). `!=` shares `Eql/eql` and negates the
/// rebuilt equality.
pub(crate) fn operator_concept(op: NumOp) -> Option<(&'static str, &'static str)> {
    Some(match op {
        NumOp::Add => ("/syn/Add", "add"),
        NumOp::Sub => ("/syn/Sub", "sub"),
        NumOp::Mul => ("/syn/Mul", "mul"),
        NumOp::Div => ("/syn/Div", "div"),
        NumOp::Rem => ("/syn/Rem", "rem"),
        NumOp::Eql | NumOp::Neq => ("/syn/Eql", "eql"),
        NumOp::Lt => ("/syn/Cmp", "lt"),
        NumOp::Gt => ("/syn/Cmp", "gt"),
        NumOp::Lte => ("/syn/Cmp", "lte"),
        NumOp::Gte => ("/syn/Cmp", "gte"),
        NumOp::And | NumOp::Or => return None,
    })
}
