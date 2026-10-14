use {crate::*, curios_num::Natural};

/// A module whose entry forces a description and tests what it answers, in the shape erasure binds the test: the value arm returns the answer, and the suspended arm carries the suspension on. Where `suspends`, the description also builds a suspension with `New`, which it never answers.
fn tested_entry(suspends: bool) -> Module {
    let mut builder = ErsdBuilder::new();
    let seven = builder.constant(Constant::Nat(Natural::from(7u32)));

    let description = builder.reserve_function();
    builder.open_block();
    if suspends {
        let on = builder.let_value(
            None,
            Rhs::Sequence {
                operation: SequenceOp::ListBuild,
                operands: vec![],
            },
        );
        builder.let_value(
            None,
            Rhs::Suspension {
                operation: SuspensionOperation::New,
                operands: vec![
                    Atom::Value(on),
                    Atom::Function(description),
                    Atom::Function(description),
                ],
            },
        );
    }
    let body = builder.seal_block(Terminator::Return(Atom::Constant(seven)));
    builder.define_function(description, Some("description".into()), vec![], body);
    builder.mark_description(description);
    builder.item_functions(vec![description]);

    builder.open_block();
    let answer = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(description),
            arguments: vec![],
        },
    );
    let test = builder.let_value(
        None,
        Rhs::Suspension {
            operation: SuspensionOperation::Is,
            operands: vec![Atom::Value(answer)],
        },
    );
    builder.open_block();
    let if_false = builder.seal_block(Terminator::Return(Atom::Value(answer)));
    builder.open_block();
    let carried = builder.let_value(
        None,
        Rhs::Suspension {
            operation: SuspensionOperation::Build,
            operands: vec![Atom::Value(answer); 4],
        },
    );
    let if_true = builder.seal_block(Terminator::Return(Atom::Value(carried)));
    let switched = builder.let_value(
        None,
        Rhs::SwitchBool {
            scrutinee: Atom::Value(test),
            if_false,
            if_true,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);

    builder.finalize().expect("verifies")
}

#[test]
fn a_program_that_builds_no_suspension_takes_the_value_arm_of_every_test() {
    let mut module = tested_entry(false);

    optimize(&mut module);

    let printed = module.to_string();
    assert!(!printed.contains("Suspension/"), "{printed}");
    assert!(!printed.contains("SwitchBool"), "{printed}");
}

#[test]
fn a_program_that_builds_a_suspension_keeps_its_tests() {
    let mut module = tested_entry(true);

    optimize(&mut module);

    let printed = module.to_string();
    assert!(printed.contains("Suspension/is"), "{printed}");
    assert!(printed.contains("Suspension/build"), "{printed}");
}
