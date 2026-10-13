//! What the hierarchy admits and what it refuses: cumulativity, one declaration at two levels, the paradox's shape, and the two honest programs that meet the refusal with their remedies.

use {
    super::test_support::universe_parameters,
    crate::tests::{error, run},
};

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

/// A type that quantifies over a type and answers a double powerset of it — the carrier Hurkens' form of Girard's paradox is stated over — with `tau`, the half of the paradox that stratifies. `U` answers a type, so the levels its body holds are its callers': the one `X` ranges over and the one each of its four powersets answers at.
const A_TYPE_QUANTIFYING_OVER_TYPES: &str = r#"
    let Pow(A: Type) -> Type = (A) -> Type;
    let U: Type = (X: Type, f: (Pow(Pow(X))) -> X) -> Pow(Pow(X));
    let tau(t: Pow(Pow(U))) -> U = (X, f) => (p) => t((x) => p(f(x(X, f))));
    "#;

// `s(U, f)` instantiates the `X` an instance of `U` binds at an instance of `U` below it, and `sigma`, the self-application Girard's paradox needs, does the same with `tau`. Both are typed: the two instances are two types, one a level under the other, and nothing here asks them to be one. The count is what says `U`'s levels are its callers', without which the instance below would not exist.
#[test]
fn a_type_quantifying_over_types_is_instantiated_at_an_instance_of_itself_below_it() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let at_itself(s: U, f: (Pow(Pow(U))) -> U) -> Pow(Pow(U)) = s(U, f);
        let sigma(s: U) -> Pow(Pow(U)) = s(U, tau);
        /std/print("stratified")
        "#
    );

    let parameters = universe_parameters(&source);
    assert_eq!(
        parameters.get("/U"),
        Some(&5),
        "the carrier stopped taking the levels its body holds, so no instance of it sits below another: {parameters:?}",
    );
    assert_eq!(run(&source), b"stratified");
}

// The paradox's own shape: it needs `sigma`'s argument and the `U` it answers over to be one type, which `omega` asks by applying `p`, a predicate over the instance below, to `x`, an inhabitant of the instance above. That is the instance below asked to be the one above it, and it is refused. The hierarchy does not make the self-application unspellable; it makes the loop ill-typed, and the diagnostic names the rule and the declaration, since a fixture that accepted any error would pass on a program broken some other way.
#[test]
fn the_self_application_girards_paradox_needs_is_refused_at_one_instance() {
    let source = format!(
        r#"{A_TYPE_QUANTIFYING_OVER_TYPES}
        let sigma(s: U) -> Pow(Pow(U)) = s(U, tau);
        let omega(x: U, p: Pow(U)) -> Type = (h: sigma(x)(p)) -> p(x);
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(message.contains("strictly below itself"), "got: {message}");
    assert!(
        message.contains("while elaborating omega:"),
        "got: {message}"
    );
}

// The control, differing in the last declaration alone: the same `s` instantiated at a type its caller supplies. `A`'s level is held to the one `X` ranges over, which is satisfiable — what the refusal above rests on is the self-instantiation at one instance, not the quantifier.
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

/// The caret line a report places under `written`, where it stands in `line` of the source.
fn caret_under(line: &str, written: &str, from: usize) -> String {
    let column = from + line[from..].find(written).expect("the line writes it");

    format!(
        "{:>5} | {}{}",
        "",
        " ".repeat(column),
        "^".repeat(written.len())
    )
}

// **A conflict a scheme's bound closes names the declaration instantiated, at a use of it.** `lt` holds the level of its first argument's type under its second's domain, a bound its scheme states and no term of `bad` writes. Restated at a use with no place and no name, it left the report wherever else on the cycle carried a position, saying nothing of `lt`.
#[test]
fn a_conflict_a_schemes_bound_closes_names_the_declaration_instantiated() {
    const BAD: &str = "        let bad(H: (Type) -> Type) -> Type = lt(H, H);";
    let source = format!(
        r#"
        let tie(@T: Type, _: T) -> Type = T;
        let lt(F: (Type) -> Type, G: (Type) -> Type) -> Type = G(tie(F));
{BAD}
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(
        message.contains("this Type would need to be strictly below itself"),
        "{message}"
    );
    assert!(
        message.contains("required by the bounds 'lt' sets on the levels it is used at"),
        "the message names whose bound the cycle runs through:\n{message}"
    );
    assert!(
        message.contains(&caret_under(BAD, "lt", 0)),
        "the report stands at the use:\n{message}"
    );
}

// Two uses whose bounds close the cycle between them, with no other constraint on it: a function's domains are equated by assignment, which raises none. The report stands at one use and holds the other as a place of its own, where it stood at the label of `bad` and named neither.
#[test]
fn a_conflict_two_uses_close_alone_stands_at_one_and_notes_the_other() {
    const BAD: &str = "        let bad: Type = let p = P; let q = Q; {lt(p, q), lt(q, p)};";
    let source = format!(
        r#"
        let tie(@T: Type, _: T) -> Type = T;
        let lt(F: (Type) -> Type, G: (Type) -> Type) -> Type = G(tie(F));
        let P(A: Type) -> Type = A;
        let Q(A: Type) -> Type = A;
{BAD}
        /std/print("no")
        "#
    );

    let message = error(&source);
    assert!(
        message.contains("required by the bounds 'lt' sets on the levels it is used at"),
        "{message}"
    );
    let first = caret_under(BAD, "lt", 0);
    let second = caret_under(BAD, "lt", BAD.find("lt(q").expect("the second use"));
    let (report, note) = message
        .split_once("'lt' is used here")
        .unwrap_or_else(|| panic!("the other use is a place of its own:\n{message}"));
    assert!(
        report.contains(&first),
        "the report stands at one use:\n{message}"
    );
    assert!(
        note.contains(&second),
        "the note stands at the other:\n{message}"
    );
}
