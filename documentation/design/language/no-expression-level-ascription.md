# No expression-level ascription

**Decision.** `term : type` is not an expression form and is not planned; `:` appears only in binder, signature, and motive positions.

**Rationale.** The whole-term forms — `let`, `rec`, `match`, lambdas, function types — already extend to the end of the enclosing term, so a postfix ascription would compete with them for the same tail. Where a type must be forced mid-expression, a `let` binding is the idiom.
