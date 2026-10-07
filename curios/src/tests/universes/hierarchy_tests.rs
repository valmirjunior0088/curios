//! What the hierarchy admits and what it refuses: cumulativity, one declaration at two levels, the paradox's shape, and the two honest programs that meet the refusal with their remedies.

use crate::tests::{error, run};

// `Unit` is an ordinary level-0 type, and storing it in `Box` requires it where a higher level is expected. Cumulativity is what admits this, and it is the reason a declaration whose level is determined can be minimized rather than generalized — the lower level is usable wherever a higher one is required.
#[test]
fn cumulativity_admits_a_lower_universe_where_a_higher_is_required() {
    let source = r#"
        use /std/{Str};
        induct Unit : pub Type
        | only()
        end
        induct Box : pub Type
        | wrap(Type)
        end
        let boxed : Box = Box/wrap(Unit);
        match boxed | wrap(_) => /std/print("stored") end
        "#;

    assert_eq!(run(source), b"stored");
}

// One polymorphic declaration instantiated at two distinct levels in a single program: `pick(42)` chooses `A := Nat` (level 0), while `pick(Nat)` chooses `A := Type` (level 1). A declaration monomorphized to either level cannot serve both, so this is the direct guard on minimizing result-only levels — that change must not reach a level a use site genuinely chooses.
#[test]
fn one_declaration_serves_two_universe_levels() {
    let source = r#"
        use /std/{Nat, Str};
        let pick(@A : Type, x : A) -> A = x;
        let small : Nat = pick(7);
        let large : Type = pick(Nat);
        /std/print("both")
        "#;

    assert_eq!(run(source), b"both");
}

/// A type that quantifies over a type and answers a double powerset of it — the carrier Hurkens' form of Girard's paradox is stated over — with `tau`, the half of the paradox that stratifies. Both of `U`'s levels are carried by its body alone, so they are minimized as `a_body_carried_level_is_minimized_rather_than_generalized` pins: `X` ranges over level 0 and `U` sits at level 1.
const A_TYPE_QUANTIFYING_OVER_TYPES: &str = r#"
    let Pow(A: Type) -> Type = (A) -> Type;
    let U: Type = (X: Type, f: (Pow(Pow(X))) -> X) -> Pow(Pow(X));
    let tau(t: Pow(Pow(U))) -> U = (X, f) => (p) => t((x) => p(f(x(X, f))));
    "#;

// The refusal with nothing beside it: `s(U, f)` instantiates `U`'s bound `X` at `U`, which asks `U`, at level 1, to be a type of level 0. The diagnostic names the rule and the declaration, since a fixture that accepted any error would pass on a program broken some other way — and `tau`, which mentions `U` as often, is not where it is refused.
#[test]
fn a_type_quantifying_over_types_cannot_be_instantiated_at_itself() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let at_itself(s: U, f: (Pow(Pow(U))) -> U) -> Pow(Pow(U)) = s(U, f);
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(message.contains("strictly below itself"), "got: {message}");
    assert!(message.contains("/at_itself"), "got: {message}");
}

// The paradox's own shape: `sigma` is the self-application Girard's paradox needs, and it is refused at the same instantiation for the same reason. The hierarchy does not make this unspellable; it makes it ill-typed.
#[test]
fn the_self_application_girards_paradox_needs_is_refused() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let sigma(s: U) -> Pow(Pow(U)) = s(U, tau);
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(message.contains("strictly below itself"), "got: {message}");
    assert!(message.contains("/sigma"), "got: {message}");
}

// The control, differing in the last declaration alone: the same `s` instantiated at a type its caller supplies. `A`'s level is held to the one `X` ranges over, which is satisfiable — what the refusals above rest on is the self-instantiation, not the quantifier.
#[test]
fn the_same_quantifier_instantiates_at_a_type_below_it() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let sigma_at(s: U, A: Type, f: (Pow(Pow(A))) -> A) -> Pow(Pow(A)) = s(A, f);
        /std/print("stratified")
        "#
    );

    assert_eq!(run(&source), b"stratified");
}

// **An honest program that reaches the refusal, and what the message owes it.** Two surface programs do: a *local* polymorphic definition applied to itself, and a recursive call at a level above its group's own. Neither is a paradox — the top-level twin of the first is admitted below, and stratification is what refuses them — so each is a program a user can write by accident and has no way to annotate out of, levels having no syntax.
//
// What these assert is therefore the *advice* rather than the refusal alone. A raw constraint such as `?u784+1 ≤ ?u784` gives a reader nothing to do, so the message carries the span the constraint came from, a level numbering local to the message, and the two facts that decide what to write instead. The raw metavariable id counts every level the unit has invented and moves with an edit anywhere, which is why the numbering exists and why a fixture may assert on it at all.
#[test]
fn a_local_polymorphic_definition_applied_to_itself_is_refused_with_its_remedy() {
    let source = r#"
        use /std/{Nat};

        let self_applied: (@A: Type, x: A) -> A =
            let id(@A: Type, x: A) -> A = x;
            id(id);

        /std/print(Nat/to_str(self_applied(1)))
        "#;

    let message = error(source);
    assert!(
        message.contains("this Type would need to be strictly below itself"),
        "{message}"
    );
    assert!(
        message.contains("required constraint: ?u1+1 ≤ ?u1"),
        "the message numbers its levels within itself:\n{message}"
    );
    assert!(
        message.contains("has to be hoisted"),
        "the message says what to write instead:\n{message}"
    );
}

// The control, and the reason the refusal above is about *locality* rather than about self-application: hoisted to the top level, `id` generalizes over the level its interface carries, the two occurrences instantiate it at their own, and the program runs.
#[test]
fn the_same_definition_at_the_top_level_is_admitted() {
    let source = r#"
        use /std/{Nat};

        let id(@A: Type, x: A) -> A = x;
        let self_applied: (@A: Type, x: A) -> A = id(id);

        /std/print(Nat/to_str(self_applied(1)))
        "#;

    assert_eq!(run(source), b"1");
}

// The second shape: a recursive call at a level above its group's own. `depth` calls itself at `A := Type`, and a group is monomorphic in its universes, so the call needs the group's own level strictly above itself. The remedy clause names that case too — there is no annotation, and what the sibling needs has to be a declaration apart from the one needing it.
#[test]
fn a_recursive_call_a_level_above_its_group_is_refused_with_its_remedy() {
    let source = r#"
        use /std/{Nat};

        let depth(@A: Type, n: Nat, x: A) -> Nat =
            match n | 0 => 0 | k + 1 => depth(@Type, k, A) + 1 end;

        /std/print(Nat/to_str(depth(0, 1)))
        "#;

    let message = error(source);
    assert!(
        message.contains("this Type would need to be strictly below itself"),
        "{message}"
    );
    assert!(
        message.contains("monomorphic in its own levels"),
        "the message names the group rule:\n{message}"
    );
}

// **A group's own levels are constrained by its own recursion, and the kernel has to read that.** A member used at a type one level up — `pick(@Type, …)` for `pick(@A: Type, …)` — needs `1 ≤ u` of the group's instance, a group being monomorphic in its universes. The elaborator records exactly that in the scheme, and the kernel's entailment reaches the hypotheses before it decides a level's constant part structurally, since a parameter bounds no constant until something assumes it does.
//
// Both spellings are here because the two-member group is incidental: the same demand raised inside one self-recursive member refuses identically, so what the rule turns on is the group instance rather than the sibling.
#[test]
fn a_group_member_used_a_level_above_its_own_certifies() {
    let source = r#"
        use /std/{Nat};

        let pick(@A: Type, x: A) -> Nat = 0
        and other(n: Nat) -> Nat = pick(Nat) + n;

        /std/print(Nat/to_str(other(1)))
        "#;

    assert_eq!(run(source), b"1");
}

#[test]
fn a_self_recursive_call_a_level_above_its_own_certifies() {
    let source = r#"
        use /std/{Nat};

        let depth(@A: Type, n: Nat, x: A) -> Nat =
            match n | 0 => 0 | k + 1 => depth(@Type, k, Nat) end;

        /std/print(Nat/to_str(depth(0, 1)))
        "#;

    assert_eq!(run(source), b"0");
}
