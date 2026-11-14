use {
    crate::*,
    curios_abi::{ForeignFunction, WireLeaf, WireResults, WireReturn, WireSignature, WireType},
    curios_num::Natural,
    std::sync::Arc,
};

/// What `f` peels: a `Nat` at `0` and its predecessor, or a list at its head and its rest.
#[derive(Clone, Copy)]
enum Peel {
    Nat,
    List,
}

/// What a step arm is built from: `f`, its peeled parameter `x`, its other parameter `a`, what the peel left — `x - 1` or the list's rest — and a host call to make.
struct Arm {
    f: FunctionId,
    x: ValueId,
    a: ValueId,
    tail: ValueId,
    poll: ForeignId,
}

/// A host row answering what `returns` names, asked with no operand but its lane.
fn row(name: &str, returns: WireType) -> Arc<ForeignFunction> {
    Arc::new(ForeignFunction {
        name: name.into(),
        signature: WireSignature {
            params: vec![],
            returns: WireReturn::Answers(WireResults::single(returns)),
        },
    })
}

/// What a host call through `foreign` answers, asked as a first call: no token, and not dropped (`curios_abi::LANE_IN`).
fn ask(builder: &mut Builder, foreign: ForeignId) -> Atom {
    let none = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![],
        },
    );
    let asking = builder.constant(Constant::Bool(false));
    Atom::Value(builder.let_value(
        None,
        Rhs::Foreign {
            foreign,
            operands: vec![Atom::Value(none), Atom::Constant(asking)],
        },
    ))
}

/// A module whose entry calls `f(x, a)` on what host calls answer, so evaluation cannot close it, and whose `f` dispatches on `x` by `peel`: the base answers `a`, and the step arm what `step` builds. The module after optimization, printed.
fn optimized(peel: Peel, step: impl FnOnce(&mut Builder, &Arm) -> Atom) -> String {
    let mut builder = Builder::new();
    let poll = builder.foreign(row("/poll", WireType::Nat));
    let f = builder.reserve_function();
    let x = builder.value(Some("x".into()));
    let a = builder.value(Some("a".into()));

    builder.open_block();
    let base = builder.seal_block(Terminator::Return(Atom::Value(a)));
    builder.open_block();
    let (tail, head) = match peel {
        Peel::Nat => {
            let one = builder.constant(Constant::Nat(Natural::one()));
            let pred = builder.let_value(
                None,
                Rhs::Operation {
                    operation: Operation::NatSub,
                    operands: vec![Atom::Value(x), Atom::Constant(one)],
                },
            );
            (pred, None)
        }
        Peel::List => (
            builder.value(Some("t".into())),
            Some(builder.value(Some("h".into()))),
        ),
    };
    let answer = step(
        &mut builder,
        &Arm {
            f,
            x,
            a,
            tail,
            poll,
        },
    );
    let arm = builder.seal_block(Terminator::Return(answer));
    builder.open_block();
    let rhs = match head {
        None => Rhs::SwitchNat {
            scrutinee: Atom::Value(x),
            cases: vec![NatCase {
                key: Natural::zero(),
                block: base,
            }],
            default: arm,
        },
        Some(element) => Rhs::UnconsSequence {
            grain: SequenceGrain::List,
            scrutinee: Atom::Value(x),
            empty: base,
            cons: UnconsSequenceStep {
                element,
                suffix: tail,
                block: arm,
            },
        },
    };
    let dispatched = builder.let_value(None, rhs);
    let body = builder.seal_block(Terminator::Return(Atom::Value(dispatched)));
    builder.define_function(f, Some("f".into()), vec![x, a], body);
    builder.item_functions(vec![f]);

    builder.open_block();
    let peeled = match peel {
        Peel::Nat => poll,
        Peel::List => builder.foreign(row("/list", WireType::List(WireLeaf::Nat))),
    };
    let x = ask(&mut builder, peeled);
    let a = ask(&mut builder, poll);
    let result = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![x, a],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize(&mut module);
    module.to_string()
}

/// `f` applied to `arguments`, and its answer.
fn call(builder: &mut Builder, f: FunctionId, arguments: Vec<Atom>) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments,
        },
    ))
}

fn operation(builder: &mut Builder, operation: Operation, left: Atom, right: Atom) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Operation {
            operation,
            operands: vec![left, right],
        },
    ))
}

fn nat(builder: &mut Builder, value: u32) -> Atom {
    Atom::Constant(builder.constant(Constant::Nat(Natural::from(value))))
}

/// `answer * 3 + plus`: an affine step on the call's answer, two operations of two monoids, so no rebase takes it first.
fn affine(builder: &mut Builder, answer: Atom, plus: Atom) -> Atom {
    let three = nat(builder, 3);
    let tripled = operation(builder, Operation::NatMul, answer, three);
    operation(builder, Operation::NatAdd, tripled, plus)
}

/// Whether the printed module dispatches by a fold, whose step arm binds what the fold of the rest answers.
fn folds(printed: &str) -> bool {
    printed
        .lines()
        .map(str::trim_start)
        .any(|line| line.starts_with('|') && (line.contains(" + 1; ") || line.contains("]; ")))
}

/// `f(x, a) = f(x - 1, a) * 3 + 1`.
#[test]
fn a_nat_recursion_its_call_answers_into_becomes_its_fold() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let answer = call(
            builder,
            arm.f,
            vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
        );
        let one = nat(builder, 1);
        affine(builder, answer, one)
    });
    assert!(folds(&printed), "{printed}");
}

/// `f(l, a) = f(rest, a) * 3 + 1`, over a list.
#[test]
fn a_list_recursion_its_call_answers_into_becomes_its_fold() {
    let printed = optimized(Peel::List, |builder, arm| {
        let answer = call(
            builder,
            arm.f,
            vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
        );
        let one = nat(builder, 1);
        affine(builder, answer, one)
    });
    assert!(folds(&printed), "{printed}");
}

/// `f(x, a) = f(x - 1, a) * 3 + x`: each level reads its own `x`, which the fold's arm would read at its outermost.
#[test]
fn a_recursion_reading_its_peeled_parameter_past_the_peel_is_left_as_written() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let answer = call(
            builder,
            arm.f,
            vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
        );
        affine(builder, answer, Atom::Value(arm.x))
    });
    assert!(!folds(&printed), "{printed}");
}

/// `f(x, a) = f(x - 1, a + 1) * 3 + 1`: the call hands another parameter on changed.
#[test]
fn a_recursion_handing_another_parameter_on_changed_is_left_as_written() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let one = nat(builder, 1);
        let next = operation(builder, Operation::NatAdd, Atom::Value(arm.a), one);
        let answer = call(builder, arm.f, vec![Atom::Value(arm.tail), next]);
        affine(builder, answer, one)
    });
    assert!(!folds(&printed), "{printed}");
}

/// `f(x, a) = f(x - 1, a) * 3 + f(x - 1, a)`: a second call would stay a recursion inside the fold.
#[test]
fn a_recursion_of_two_calls_is_left_as_written() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let arguments = vec![Atom::Value(arm.tail), Atom::Value(arm.a)];
        let first = call(builder, arm.f, arguments.clone());
        let second = call(builder, arm.f, arguments);
        affine(builder, first, second)
    });
    assert!(!folds(&printed), "{printed}");
}

/// `f(x, a) = let p = poll(); f(x - 1, a) * 3 + p`: the recursion asks the host from the outermost level in, and the fold would ask from the innermost out.
#[test]
fn a_host_call_ahead_of_the_call_keeps_the_recursion() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let asked = ask(builder, arm.poll);
        let answer = call(
            builder,
            arm.f,
            vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
        );
        affine(builder, answer, asked)
    });
    assert!(!folds(&printed), "{printed}");
}

/// `f(x, a) = let r = f(x - 1, a); r * 3 + poll()`: the recursion asks the host from the innermost level out, as the fold does.
#[test]
fn a_host_call_after_the_call_keeps_its_order_in_the_fold() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let answer = call(
            builder,
            arm.f,
            vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
        );
        let asked = ask(builder, arm.poll);
        affine(builder, answer, asked)
    });
    assert!(folds(&printed), "{printed}");
}

/// `f(x, a) = (a < 7) && f(x - 1, a)`: the recursion stops at the first level whose `a < 7` decides, where its fold would run every level.
#[test]
fn a_recursion_only_some_runs_of_its_arm_read_is_left_as_written() {
    let printed = optimized(Peel::Nat, |builder, arm| {
        let answer = call(
            builder,
            arm.f,
            vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
        );
        let seven = nat(builder, 7);
        let below = operation(builder, Operation::NatLt, Atom::Value(arm.a), seven);
        operation(builder, Operation::BoolAnd, below, answer)
    });
    assert!(!folds(&printed), "{printed}");
}

/// `f(x, a) = let r = f(x - 1, a); match a < 7 | true => r * 3 + 1 | false => …`, its other arm `r * 5 + 2` where `both`, and `a` where not.
fn dispatched(builder: &mut Builder, arm: &Arm, both: bool) -> Atom {
    let answer = call(
        builder,
        arm.f,
        vec![Atom::Value(arm.tail), Atom::Value(arm.a)],
    );
    let seven = nat(builder, 7);
    let below = operation(builder, Operation::NatLt, Atom::Value(arm.a), seven);
    builder.open_block();
    let one = nat(builder, 1);
    let tripled = affine(builder, answer, one);
    let if_true = builder.seal_block(Terminator::Return(tripled));
    builder.open_block();
    let otherwise = match both {
        true => {
            let five = nat(builder, 5);
            let two = nat(builder, 2);
            let scaled = operation(builder, Operation::NatMul, answer, five);
            operation(builder, Operation::NatAdd, scaled, two)
        }
        false => Atom::Value(arm.a),
    };
    let if_false = builder.seal_block(Terminator::Return(otherwise));
    Atom::Value(builder.let_value(
        None,
        Rhs::SwitchBool {
            scrutinee: below,
            if_false,
            if_true,
        },
    ))
}

/// An answer every arm of a dispatch reads is read on every run.
#[test]
fn an_answer_every_arm_of_a_dispatch_reads_becomes_its_fold() {
    let printed = optimized(Peel::Nat, |builder, arm| dispatched(builder, arm, true));
    assert!(folds(&printed), "{printed}");
}

/// An answer one arm of a dispatch reads is placed in that arm, and the recursion stops at the first level that takes the other.
#[test]
fn an_answer_one_arm_of_a_dispatch_reads_is_left_as_written() {
    let printed = optimized(Peel::Nat, |builder, arm| dispatched(builder, arm, false));
    assert!(!folds(&printed), "{printed}");
}
