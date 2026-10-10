//! End-to-end coverage for the implicit cumulative universe hierarchy.
//!
//! The hierarchy exists to remove one source of unsoundness: under `Type : Type` a type could classify itself, which admits Girard's paradox. The solver's own rules are unit-tested in `curios-elab/src/universe_solver/tests.rs`, and the kernel's satisfiability in `curios-cert/src/satisfy/tests.rs`; these check what a *user* can observe.
//!
//! A surface program reaches `UniverseInconsistency`, and three fixtures hold where. What a program cannot do is force two *occurrences of a name* to share a level: a declaration generalizes over the levels its *interface* carries, so a self-reference like `Box/wrap(Box)` instantiates `Box` at two different levels and is admitted — correctly, since that is stratification working — and `syntax.md` gives no syntax for universe variables or explicit arguments. A bound variable and the type that binds it are another matter: they share a level by construction, and a level a declaration carries only in its body is minimized rather than generalized, so `let U: Type = (X: Type, …) -> …` fixes `X` one level below `U`, and instantiating `X` at `U` itself asks for a level strictly below itself. `a_type_quantifying_over_types_cannot_be_instantiated_at_itself` is that refusal alone, `the_self_application_girards_paradox_needs_is_refused` is the paradox's own shape, refused at the same instantiation, and `the_same_quantifier_instantiates_at_a_type_below_it` is the control. Stratification itself is covered at unit level by `a_polymorphic_definition_instantiates_at_prop_and_type`, which pins `id(Prop)` to level 1 and `id(Type)` to level 2.
//!
//! **Nor need the program be a paradox.** Two more shapes reach the same refusal from ordinary code, because generalization is a privilege of the top level and a group is monomorphic in its own levels: a *local* definition applied to itself, and a member of a recursive group called at a level above the group's. The last two fixtures here are those, each with the control admitted once the definition is hoisted or the group split, and what they assert is the *advice* — levels have no syntax, so a user who meets this has nothing to annotate and needs the message to say what to write instead.
//!
//! "Interface" is load-bearing in that sentence and is what `a_body_carried_level_is_minimized_rather_than_generalized` pins: the levels reachable only through a body are *minimized* instead, so the set of declarations a use site can instantiate at two levels is narrower than "every declaration".

mod hierarchy_tests;
mod instance_tests;
mod settlement_tests;
mod sort_tests;
mod test_support;
