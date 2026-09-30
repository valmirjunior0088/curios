# Strict positivity through a type-former parameter

**Not refined yet.** This specification reserves positivity for a declaration that takes a type former as a parameter. It is not an implementation plan.

## What is missing

`induct Mu(F : (Type) -> Type) | fix(F(Mu(F))) end` cannot be judged from its own body: whether `Mu(F)` occurs positively depends on what `F` does with its argument, which only an instance knows. The analysis answers *a position the checker cannot see through* rather than `Unused`, so the declaration is refused ([Strict positivity](../../design/soundness/formation/strict-positivity.md)).

## Previously discussed

An inferred per-binder polarity obligation recorded on the declaration, generalized onto enclosing signatures that bind the parameter, instantiated freshly at each external occurrence and discharged against the argument's own vector — the architecture `UniverseContext` already has for levels — kept in a side store keyed by declaration and binder rather than as a field of `FuncType`, so conversion stays free of polarity. It waits for a consumer, and is scheduled with `Mu` itself.
