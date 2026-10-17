use {
    crate::*,
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_num::{Binary, Grain, Natural},
    std::sync::Arc,
};

/// A module whose entry calls `f` on a `Nat` a host call answers, so evaluation cannot close it, and whose `f(n)` dispatches on `n`: `0` answers what `base` builds, and any other `n` answers what `leaf` builds, handed the builder, `f`, `n` and `n - 1`. The module after optimization, printed.
fn optimized(
    base: impl FnOnce(&mut ErsdBuilder) -> Atom,
    leaf: impl FnOnce(&mut ErsdBuilder, FunctionId, ValueId, ValueId) -> Atom,
) -> String {
    let mut builder = ErsdBuilder::new();
    let f = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let one = builder.constant(Constant::Nat(Natural::from(1u32)));

    builder.open_block();
    let base = base(&mut builder);
    let zero_arm = builder.seal_block(Terminator::Return(base));
    builder.open_block();
    let pred = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), Atom::Constant(one)],
        },
    );
    let answer = leaf(&mut builder, f, n, pred);
    let default = builder.seal_block(Terminator::Return(answer));
    builder.open_block();
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: zero_arm,
            }],
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.define_function(f, Some("f".into()), vec![n], body);
    builder.item_functions(vec![f]);

    builder.open_block();
    let row = Arc::new(ForeignFunction {
        name: "/poll".into(),
        signature: WireSignature {
            params: vec![],
            returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
        },
    });
    let foreign = builder.foreign(row);
    // A first call's lane: no token, and not dropped (`curios_abi::LANE_IN`).
    let none = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![],
        },
    );
    let asking = builder.constant(Constant::Bool(false));
    let opaque = builder.let_value(
        Some("opaque".into()),
        Rhs::Foreign {
            foreign,
            operands: vec![Atom::Value(none), Atom::Constant(asking)],
        },
    );
    let result = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![Atom::Value(opaque)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);
    let mut module = builder.finalize().expect("verifies");

    optimize(&mut module);
    module.to_string()
}

/// `n - 1` handed to `f`, and its answer.
fn recur(builder: &mut ErsdBuilder, f: FunctionId, pred: ValueId) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![Atom::Value(pred)],
        },
    ))
}

fn operation(builder: &mut ErsdBuilder, operation: Operation, left: Atom, right: Atom) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Operation {
            operation,
            operands: vec![left, right],
        },
    ))
}

fn sequence(builder: &mut ErsdBuilder, operation: SequenceOp, operands: Vec<Atom>) -> Atom {
    Atom::Value(builder.let_value(
        None,
        Rhs::Sequence {
            operation,
            operands,
        },
    ))
}

/// A base answering `value`.
fn constant(value: Constant) -> impl FnOnce(&mut ErsdBuilder) -> Atom {
    move |builder| Atom::Constant(builder.constant(value))
}

/// A base answering the empty list.
fn empty_list(builder: &mut ErsdBuilder) -> Atom {
    sequence(builder, SequenceOp::ListBuild, vec![])
}

/// The worker's parameter list, as its definition prints it, bound beside its wrapper.
fn worker_params(printed: &str) -> &str {
    let line = printed
        .lines()
        .find(|line| line.trim_start().starts_with("and ") && line.contains("$f@w("))
        .unwrap_or_else(|| panic!("a worker:\n{printed}"));
    let start = line.find("$f@w(").expect("the worker's name") + "$f@w(".len();
    let end = start + line[start..].find(')').expect("its parameters close");
    &line[start..end]
}

/// What the arm for `0` answers: the last line of the arm.
fn zero_arm_answer(printed: &str) -> &str {
    let mut lines = printed
        .lines()
        .map(str::trim)
        .skip_while(|line| !line.starts_with("| 0 =>"));
    let arm = lines
        .next()
        .unwrap_or_else(|| panic!("an arm for 0:\n{printed}"));
    match arm.strip_prefix("| 0 =>").map(str::trim) {
        Some(inline) if !inline.is_empty() => inline,
        _ => lines
            .take_while(|line| !line.starts_with('|') && *line != "end;")
            .last()
            .unwrap_or_else(|| panic!("the arm for 0 answers:\n{printed}")),
    }
}

/// `f(n) = f(n - 1) + 1`.
#[test]
fn a_monoid_deferred_recursion_gains_a_worker() {
    let printed = optimized(
        constant(Constant::Nat(Natural::zero())),
        |builder, f, _, pred| {
            let one = Atom::Constant(builder.constant(Constant::Nat(Natural::from(1u32))));
            let recursion = recur(builder, f, pred);
            operation(builder, Operation::NatAdd, recursion, one)
        },
    );
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = (f(n - 1) + n) + 1`: the chain's two addends fold into the one accumulator.
#[test]
fn a_chain_of_combines_folds_into_one_accumulator() {
    let printed = optimized(
        constant(Constant::Nat(Natural::zero())),
        |builder, f, n, pred| {
            let one = Atom::Constant(builder.constant(Constant::Nat(Natural::from(1u32))));
            let recursion = recur(builder, f, pred);
            let first = operation(builder, Operation::NatAdd, recursion, Atom::Value(n));
            operation(builder, Operation::NatAdd, first, one)
        },
    );
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = (n - 1 < n) && f(n - 1)`, the shape `List/eql` erases to: a conjunction is a monoid the oracle names.
#[test]
fn a_conjunction_around_its_recursion_gains_a_worker() {
    let printed = optimized(constant(Constant::Bool(true)), |builder, f, n, pred| {
        let below = operation(builder, Operation::NatLt, Atom::Value(pred), Atom::Value(n));
        let recursion = recur(builder, f, pred);
        operation(builder, Operation::BoolAnd, below, recursion)
    });
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = f(n - 1) + n * n`, the product computed after the call: only the combines may move across it, so the recursion is left as written.
#[test]
fn an_addend_computed_after_the_call_is_declined() {
    let printed = optimized(
        constant(Constant::Nat(Natural::zero())),
        |builder, f, n, pred| {
            let recursion = recur(builder, f, pred);
            let square = operation(builder, Operation::NatMul, Atom::Value(n), Atom::Value(n));
            operation(builder, Operation::NatAdd, recursion, square)
        },
    );
    assert!(!printed.contains("f@w"), "{printed}");
}

/// `f(n) = r + r` for `r = f(n - 1)`: a context reading the call's answer twice is no word with one hole, so the recursion is left as written.
#[test]
fn a_hole_read_twice_is_declined() {
    let printed = optimized(
        constant(Constant::Nat(Natural::zero())),
        |builder, f, _, pred| {
            let recursion = recur(builder, f, pred);
            operation(builder, Operation::NatAdd, recursion, recursion)
        },
    );
    assert!(!printed.contains("f@w"), "{printed}");
}

/// `f(n) = [n, ..f(n - 1)]`: what a cons puts before the recursion's answer is threaded on the left, `acc ++ [n]`, and the empty list a base arm answers drops out of it.
#[test]
fn a_list_built_under_a_cons_threads_its_left_side() {
    let printed = optimized(empty_list, |builder, f, n, pred| {
        let head = sequence(builder, SequenceOp::ListBuild, vec![Atom::Value(n)]);
        let recursion = recur(builder, f, pred);
        sequence(builder, SequenceOp::ListConcat, vec![head, recursion])
    });
    let params = worker_params(&printed);
    assert!(
        params.ends_with("$acc") && !params.contains("acc_after"),
        "{printed}"
    );
    assert!(
        zero_arm_answer(&printed).ends_with("$acc"),
        "the base arm answers the accumulator alone:\n{printed}"
    );
}

/// `f(n) = [..f(n - 1), n]`: what an append puts after the recursion's answer is threaded on the right, `[n] ++ acc_after`.
#[test]
fn a_list_appended_to_its_recursion_threads_its_right_side() {
    let printed = optimized(empty_list, |builder, f, n, pred| {
        let recursion = recur(builder, f, pred);
        sequence(
            builder,
            SequenceOp::ListAppend,
            vec![recursion, Atom::Value(n)],
        )
    });
    let params = worker_params(&printed);
    assert!(
        params.ends_with("$acc_after") && !params.contains("$acc,"),
        "{printed}"
    );
}

/// `f(n) = [n, ..f(n - 1), n]`, erased as an append and then a concatenation: the two links are one word around the recursion, threaded on both sides.
#[test]
fn a_word_around_the_recursion_threads_both_sides() {
    let printed = optimized(empty_list, |builder, f, n, pred| {
        let head = sequence(builder, SequenceOp::ListBuild, vec![Atom::Value(n)]);
        let recursion = recur(builder, f, pred);
        let appended = sequence(
            builder,
            SequenceOp::ListAppend,
            vec![recursion, Atom::Value(n)],
        );
        sequence(builder, SequenceOp::ListConcat, vec![head, appended])
    });
    let params = worker_params(&printed);
    assert!(
        params.contains("$acc, ") && params.ends_with("$acc_after"),
        "{printed}"
    );
}

/// `f(n) = x[0x61, ..f(n - 1)]`: a run of bytes built under its recursion is a loop too.
#[test]
fn a_run_of_bytes_built_under_its_recursion_gains_a_worker() {
    let printed = optimized(
        constant(Constant::Bin(Grain::X, Binary::empty())),
        |builder, f, _, pred| {
            let byte = Atom::Constant(
                builder.constant(Constant::Bin(Grain::X, Binary::from_bytes(vec![0x61]))),
            );
            let recursion = recur(builder, f, pred);
            sequence(
                builder,
                SequenceOp::BinConcat(Grain::X),
                vec![byte, recursion],
            )
        },
    );
    assert!(printed.contains("f@w"), "{printed}");
}

/// `f(n) = append([], f(n - 1))`: the recursion's answer kept as an element is a constructor's context, no word of the monoid, so the recursion is left as written.
#[test]
fn a_recursion_kept_as_an_element_is_declined() {
    let printed = optimized(empty_list, |builder, f, _, pred| {
        let empty = empty_list(builder);
        let recursion = recur(builder, f, pred);
        sequence(builder, SequenceOp::ListAppend, vec![empty, recursion])
    });
    assert!(!printed.contains("f@w"), "{printed}");
}
