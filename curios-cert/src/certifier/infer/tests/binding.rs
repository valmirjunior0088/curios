//! A `let` in the certifier: bound for typing, and read by value by everything that reads a term for more than its type.

use {
    crate::certifier::infer::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::{Free, Reducer, Subterm, Term, Totality},
};

/// `match v | true => Nat | false => Bool`: a type that is `Nat` exactly where `v` is `true`, which every fixture here reads a `Bool` through.
fn family(v: &str) -> String {
    fill!(
        "#{goal = Type} match ${v} | false => #BoolType | true => #NatType end",
        v
    )
}

/// The term naming `local`, a local the certifier minted, written by its identity under `word`.
fn named(local: &Free, word: &str) -> Term {
    term(&fill!(
        "#{binding = Free(${index})} ${word}",
        index = local.index(),
        word
    ))
}

/// A `let`'s tail is typed over its binder — a variable there has the binder's type — and the `let`'s own type is stated by value, since the binder closes with it.
///
/// Mutation-checked: with conversion comparing its terms as they are handed, the second binding's annotation is a name reduction has no value for.
#[test]
fn a_lets_tail_is_typed_over_its_binder_and_its_type_stated_by_value() {
    let mut certifier = certifier();

    assert_eq!(
        infer(
            &mut certifier,
            &term("(let T: Type = #NatType; let y: T = 3; y)")
        ),
        Ok(term("#NatType"))
    );
}

/// Reduction reads by value where typing enters it: a lambda checks against a `let`-bound function type, which the rule reduces to find its telescope.
///
/// Mutation-checked: with a forced reduction taking its term as it is handed, the annotation's name reaches the reducer.
#[test]
fn a_lambda_checks_against_a_let_bound_function_type() {
    let mut certifier = certifier();

    assert_eq!(
        infer(
            &mut certifier,
            &term("(let T: Type = (x: #NatType) -> #NatType; let f: T = (x: #NatType) => x; f)")
        ),
        Ok(term("(x: #NatType) -> #NatType"))
    );
}

/// `(b: Bool) => ${lets} match b | false => 0 | true => let z: ${family} = ${value}; 0 end`: the split every fixture below reads a `let` across.
const SPLIT: &str = "(b: #BoolType) => (${lets} #{goal = #NatType} match b | false => 0 | true => (let z: ${family} = ${value}; 0) end)";

/// An arm binds again the `let` its solution moves. `c` stands for `b`, and in the arm where `b` is `true` it stands for `true`: a substituted `let` would have had the solution substituted through its value, and a bound one is bound again at it.
///
/// Mutation-checked: with a binding over a local that stood for nothing leaving a moved `let` as it was, `family(c)` is stuck on `b` inside the arm that decided it.
#[test]
fn an_arm_binds_again_the_let_its_solution_moves() {
    let split = fill!(
        SPLIT,
        lets = "let c: #BoolType = b;",
        family = family("c"),
        value = "3"
    );

    assert!(infer(&mut certifier(), &term(&split)).is_ok());
}

/// A `let` that stands for a moved `let` moves with it: `d` is `c` and `c` is `b`, so where `b` is `true` so is `d`. Each is bound again from its value as written, and `d` after `c`, so `d` reads `c` where it now stands.
///
/// Mutation-checked: with the `let`s bound again innermost first, `d` is read through the `c` the arm has not yet moved.
#[test]
fn a_let_over_a_moved_let_moves_with_it() {
    let split = fill!(
        SPLIT,
        lets = "let c: #BoolType = b; let d: #BoolType = c;",
        family = family("d"),
        value = "3"
    );

    assert!(infer(&mut certifier(), &term(&split)).is_ok());
}

/// And a `let` the solution re-types stays a `let`: bound again at its specialized type, it still stands for its value. `c` is `b` stated at a type that names `b`, and the arm reads `c`'s value through `family`.
///
/// Mutation-checked: with a re-typed `let` assumed at its new type, it stands for nothing and `family(c)` is stuck on it.
#[test]
fn a_let_an_arm_retypes_keeps_its_value() {
    // `Bool` under either case, and stuck on a variable: a type that names `b` and says nothing more.
    let carrier = |at: &str| {
        fill!(
            "#{goal = Type} match ${at} | false => #BoolType | true => #BoolType end",
            at
        )
    };
    let split = fill!(
        SPLIT,
        lets = fill!(
            "let c: ${type_} = #{goal = ${type_}} match b | false => false | true => true end;",
            type_ = carrier("b")
        ),
        family = family("c"),
        value = "3"
    );

    assert!(infer(&mut certifier(), &term(&split)).is_ok());
}

/// A `let` bound again is typed again: what was remembered of a term naming it was read off the type and the value it had, and an arm that moves it gives it others. `w` is typed at `family(b)` before the match, and is a `Nat` in the arm where `b` is `true`.
///
/// Mutation-checked: with the typings left standing where a `let` is bound again, `w` answers inside the arm at the type it had outside.
#[test]
fn a_let_bound_again_is_typed_again() {
    let split = fill!(
        SPLIT,
        lets = fill!(
            "let w: ${at_b} = #{goal = ${at_b}} match b | false => true | true => 3 end; let before: ${at_b} = w;",
            at_b = family("b")
        ),
        family = "#NatType",
        value = "w"
    );

    assert!(infer(&mut certifier(), &term(&split)).is_ok());
}

/// An elimination reads its scrutinee by value: `x` stands for `g(b)`, so the arm's equation is `g(b) = true`, kept under the spelling a substituted `let` leaves, and it answers `g(b)` written out in the arm.
///
/// Mutation-checked: with the scrutinee read as it is written, `x` is a variable the arm solves, and nothing is known of `g(b)`.
#[test]
fn an_elimination_reads_a_let_bound_scrutinee_by_value() {
    let eliminated = fill!(
        "(g: (v: #BoolType) -> #BoolType, b: #BoolType) => (let x: #BoolType = g(b); #{goal = #NatType} match x | false => 0 | true => (let z: ${family} = 3; 0) end)",
        family = family("g(b)")
    );

    assert!(infer(&mut certifier(), &term(&eliminated)).is_ok());
}

/// A guard over a `let` of a variable answers a split on the variable inside its arm, and nothing after it: `y` stands for `c`, the arm at `true` of the split binds `y` again at `true`, and the guard's spelling settled there is the arm's alone. After the split `c` is open again, and `family(k(true))` is no type `3` has.
#[test]
fn a_guard_read_through_a_let_answers_a_split_inside_its_arm_and_nothing_after_it() {
    let claimed = family("k(true)");
    // #{goal = Nat} match c | false => 0 | true => let u : family(k(true)) = 3; 0 end, inside the arm of a guard over `y`, then `after`.
    let forged = |after: &str| {
        term(&fill!(
            "(c: #BoolType, k: (v: #BoolType) -> #BoolType) => (let y: #BoolType = c; #{goal = #NatType} match #{goal = #BoolType} match y | false => k(false) | true => k(true) end | false => 0 | true => (let w: #NatType = #{goal = #NatType} match c | false => 0 | true => (let u: ${claimed} = 3; 0) end; ${after}) end)",
            claimed,
            after
        ))
    };

    assert!(infer(&mut certifier(), &forged("0")).is_ok());
    let answered_after = fill!("(let z: ${claimed} = 3; 0)", claimed);
    assert!(infer(&mut certifier(), &forged(&answered_after)).is_err());
}

/// A recorded position states what its term stands for: the obligations read a position for what it reaches, and a `let`-bound local's name reaches nothing. `h` stands for the proof `p`, and the position the tail records is `p`.
///
/// Mutation-checked: with a position recorded as it is written, one names the binder the certifier minted for `h`.
#[test]
fn a_recorded_position_is_stated_by_value() {
    let mut certifier = certifier();
    let (locals, [block]) = hypotheses(&mut certifier, "P: Prop, p: P", "(let h: P = p; h),");

    infer(&mut certifier, &block).expect("the `let` is typed");
    let (positions, failure) = certifier.take_checked();

    assert!(failure.is_none());
    assert!(!positions.is_empty());
    for position in &positions {
        assert!(
            position
                .term
                .mentions()
                .locals()
                .all(|name| locals.contains(name)),
            "a position names a binder the `let` opened: {:?}",
            position.term
        );
    }
}

/// The positions `term` records that reach a group, typed in `certifier`: each, with whether it reaches one that does not descend.
fn over_a_group(certifier: &mut Certifier, term: &Term) -> Vec<bool> {
    infer(certifier, term).expect("the term is typed");
    let (positions, _) = certifier.take_checked();

    positions
        .iter()
        .filter(|position| position.term.has_group())
        .map(|position| position.encloses_partial)
        .collect()
}

/// A position that reaches a group that does not descend says so, though the group was typed where a `let` binds it and not inside the position's term: `stuck` is a `Bool` that never arrives, and the type stated over it holds the group once it is read by value.
///
/// Mutation-checked: with no `let`-bound local read as enclosing such a group, the type over `stuck` is recorded as reaching none.
#[test]
fn a_position_over_a_let_bound_partial_group_says_so() {
    let mut certifier = certifier();
    let block = term(&fill!(
        "(let stuck: #BoolType = #{node = Rec} (let f: #BoolType = f; f); let T: Type = ${family}; 0)",
        family = family("stuck")
    ));
    let partial = over_a_group(&mut certifier, &block);

    assert!(!partial.is_empty());
    assert!(partial.into_iter().all(|partial| partial));
}

/// `countdown`, descending from its argument, whose step at `pred` is `${step}`, then `${tail}`.
const COUNTDOWN: &str = "#{node = Rec} (let countdown: (n: #NatType) -> #NatType = (n: #NatType) => #{goal = #NatType} match n | 0 => 0 | pred + 1 => ${step} end; ${tail})";

/// The verdict filed for the group `term` states, once `term` is typed in `certifier` at `Nat`.
fn verdict_once_typed(certifier: &mut Certifier, term: &Term, group: &Term) -> Option<Totality> {
    assert_eq!(infer(certifier, term), Ok(self::term("#NatType")));
    let Subterm::Rec(block) = &**group else {
        panic!("a group: {group:?}");
    };

    certifier.group_verdict(block.group())
}

/// A call is graded on what it passes: `k` stands for the predecessor, so `countdown(k)` descends as `countdown(pred)` does.
///
/// Mutation-checked: with a call's arguments graded as they are written, `k` is a binder nothing is known of and the group does not descend.
#[test]
fn a_call_through_a_let_bound_argument_is_graded_by_value() {
    let mut certifier = certifier();
    let group = term(&fill!(
        COUNTDOWN,
        step = "(let k: #NatType = pred; countdown(k))",
        tail = "countdown(3)"
    ));

    assert_eq!(
        verdict_once_typed(&mut certifier, &group, &group),
        Some(Totality::Total)
    );
}

/// A recursive group is read by value: its verdict is filed under the group a substituted `let` leaves, which is the group a recorded position holds and the obligations look up.
///
/// Mutation-checked: with the group checked as it is written, its verdict is filed under a spelling that names the binder the certifier minted for `k`.
#[test]
fn a_group_under_a_let_is_filed_by_value() {
    let mut certifier = certifier();
    let block = term("(let k: #NatType = 5; #{node = Rec} (let f: #NatType = k; f))");
    let by_value = term("#{node = Rec} (let f: #NatType = 5; f)");

    assert_eq!(
        verdict_once_typed(&mut certifier, &block, &by_value),
        Some(Totality::Total)
    );
}

/// A member of a group that stands in a term is bound in the group's tail, as a `let` binds: `countdown(3)` computes there, so a type stated over it is the type it reduces to.
///
/// Mutation-checked: with the tail typed over members that stand for nothing, `countdown(3) == 0` is a `Bool` that never arrives, and the tail's `let` is refused.
#[test]
fn a_member_stands_for_its_group_in_the_groups_tail() {
    let mut certifier = certifier();
    let group = term(&fill!(
        COUNTDOWN,
        step = "countdown(pred)",
        tail = fill!(
            "(let y: ${family} = 7; y)",
            family = family("#NatEql(countdown(3), 0)")
        )
    ));

    let inferred = infer(&mut certifier, &group).expect("the member computes in the tail");
    assert_eq!(whnf(&mut certifier, inferred).ok(), Some(term("#NatType")));
}

/// And a member not known to terminate stands for nothing in its group's own bodies: `countdown` calls itself at `3`, nothing below its argument, so its turn withholds it, and there `countdown(3) == 0` is a `Bool` that never arrives, and a type stated over it in the body is stuck where the same type in the tail is the one it reduces to. A value a body reads of a member that need not terminate is no fact the certifier holds, whoever wrote the term that reads it.
///
/// Mutation-checked: with every member read by value whatever its turn finds, `countdown(3)` computes in the body and its `let` is accepted.
#[test]
fn a_member_not_known_to_terminate_stands_for_nothing_in_its_bodies() {
    let mut certifier = certifier();
    // (n) => match n | 0 => 0 | pred + 1 => let y : family(countdown(3) == 0) = 7; countdown(pred) end
    let group = term(&fill!(
        COUNTDOWN,
        step = fill!(
            "(let y: ${family} = 7; countdown(pred))",
            family = family("#NatEql(countdown(3), 0)")
        ),
        tail = "countdown(3)"
    ));

    let inferred = infer(&mut certifier, &group);
    assert!(
        matches!(inferred, Err(Error::Mismatch { .. })),
        "{inferred:?}"
    );
}

/// A position in a group's tail that reaches the group says whether the group descends, as one over a `let`-bound group does: the type stated over `stuck` holds the group once it is read by value.
///
/// Mutation-checked: with a member bound as enclosing no such group, the type over `stuck` is recorded as reaching none.
#[test]
fn a_position_over_a_member_of_a_partial_group_says_so() {
    let mut certifier = certifier();
    let group = term(&fill!(
        "#{node = Rec} (let stuck: #BoolType = stuck; (let T: Type = ${family}; 0))",
        family = family("stuck")
    ));
    let partial = over_a_group(&mut certifier, &group);

    assert!(!partial.is_empty());
    assert!(partial.into_iter().all(|partial| partial));
}

/// Reduction reads by value where it is entered: a `let`-bound name gives way to what it stands for.
///
/// Mutation-checked: with a reduction taking its term as it is handed, the name reaches the reducer, which has no value for it.
#[test]
fn reduction_reads_a_let_bound_name_by_value() {
    let mut certifier = certifier();
    let x = certifier.fresh(Some("x"));

    let reduct = certifier.scoped(|certifier| {
        certifier.bind(&x, &term("#NatType"), &term("2"), false);
        whnf(certifier, named(&x, "x"))
    });

    assert_eq!(reduct.ok(), Some(term("2")));
}

/// Binding over a local that stood for nothing reads it again wherever the scope named it, whoever binds: `v`'s type and `c`'s value were taken while `b` stood for nothing, and once `b` stands for `true`, `v` is at `family(true)` and `c` stands for `true`.
///
/// Mutation-checked: with `Certifier::bind` leaving what named the local as it was, `v` stays at `family(b)`, and `c` hands reduction `b`, a `let`-bound name.
#[test]
fn binding_over_a_local_that_stood_for_nothing_reads_it_again_where_the_scope_named_it() {
    let mut certifier = certifier();
    let c = certifier.fresh(Some("c"));
    let boolean = term("#BoolType");

    certifier.scoped(|certifier| {
        let (locals, [b]) = hypotheses(
            certifier,
            &fill!("b: #BoolType, v: ${family}", family = family("b")),
            "b,",
        );
        certifier.bind(&c, &boolean, &b, false);
        certifier.bind(&locals[0], &boolean, &term("true"), false);

        assert_eq!(
            certifier.local_type(&locals[1]),
            Some(&term(&family("true")))
        );
        assert_eq!(whnf(certifier, named(&c, "c")).ok(), Some(term("true")));
    });
}

/// A remembered reduct is keyed on its term by value, so it never rests on what a `let`-bound name stood for when it was taken. An arm binds `x` again at another value; the reduct tables carry no binder and are not cleared when the arm is left, so an entry filed under a spelling that names `x` would answer for the outer `x` with what the inner one stood for.
///
/// Mutation-checked: with a forced reduction asking its table about the term as it is handed, the reduct taken under the arm's binding answers after the arm.
#[test]
fn a_remembered_reduct_does_not_rest_on_a_lets_binding() {
    let mut certifier = certifier();
    let (n, x) = (certifier.fresh(Some("n")), certifier.fresh(Some("x")));
    let nat = term("#NatType");
    certifier.assume(&n, &nat);
    let over_n = |operand: &str| {
        term(&fill!(
            "#NatAdd(${operand}, #{binding = Free(${n})} n)",
            operand,
            n = n.index()
        ))
    };
    let sum = over_n(&fill!("#{binding = Free(${x})} x", x = x.index()));

    certifier.scoped(|certifier| {
        certifier.bind(&x, &nat, &term("2"), false);
        let inside = certifier.scoped(|certifier| {
            certifier.bind(&x, &nat, &term("3"), false);
            certifier.reduce_forced(sum.clone())
        });
        let outside = certifier.reduce_forced(sum.clone());

        assert_eq!(inside.ok(), certifier.reduce_forced(over_n("3")).ok());
        assert_eq!(outside.ok(), certifier.reduce_forced(over_n("2")).ok());
    });
}

/// What an arm binds a moved `let` to is the arm's alone: `c` stands for `b`, so where `b` is `false` it is `false` again, whichever arm the walk takes first. Each split states `family(c)` at a value that fits its arm in one arm and the other case in the other, so a binding that outlived the arm it was made in would type the second at the first's case.
#[test]
fn a_let_an_arm_moves_stands_for_its_value_in_the_sibling_arm() {
    let at = |value: &str| {
        fill!(
            "(let z: ${family} = ${value}; 0)",
            family = family("c"),
            value
        )
    };
    let split = |if_false: &str, if_true: &str| {
        term(&fill!(
            "(b: #BoolType) => (let c: #BoolType = b; #{goal = #NatType} match b | false => ${if_false} | true => ${if_true} end)",
            if_false = at(if_false),
            if_true = at(if_true)
        ))
    };
    let (nat, boolean) = (term("#NatType"), term("#BoolType"));

    assert!(infer(&mut certifier(), &split("false", "3")).is_ok());
    assert!(matches!(
        infer(&mut certifier(), &split("3", "3")),
        Err(Error::Mismatch { inferred, .. }) if *inferred == nat
    ));
    assert!(matches!(
        infer(&mut certifier(), &split("false", "false")),
        Err(Error::Mismatch { inferred, .. }) if *inferred == boolean
    ));
}
