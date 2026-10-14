//! The whole-program fold of suspension tests.
//!
//! Erasure tests what every non-tail force answers, since a description may suspend, and builds beside each test the arm that carries a suspension on. Only `Io/suspend` makes a suspension, so a program whose reachable code holds no [`SuspensionOperation::New`] holds none: every test in it answers no, and every arm carrying one on is dead. This pass puts the value arm in place of each switch on such a test, in the shape erasure binds them — the test, the switch, and the block returning the switch — so the program enters partial evaluation and lowering as it was before descriptions could suspend, and the functions only the dead arms defined drop at the prune that follows.

#[cfg(test)]
mod tests;

use crate::{Atom, Block, BlockId, Module, Rhs, Statement, SuspensionOperation, Terminator};

/// Take the value arm of every suspension test where nothing in `module` builds a suspension, and say whether any was taken. The arms left behind are orphaned until the next prune.
pub(super) fn fold_suspension_tests(module: &mut Module) -> bool {
    curios_profile::profile!("fold_suspension_tests");
    let builds = module.statements().iter().flatten().any(|statement| {
        matches!(
            statement,
            Statement::Let {
                rhs: Rhs::Suspension {
                    operation: SuspensionOperation::New,
                    ..
                },
                ..
            }
        )
    });
    if builds {
        return false;
    }

    let mut folded = false;
    for index in 0..module.blocks().len() {
        let id = BlockId(index as u32);
        let Some(block) = module.block(id) else {
            continue;
        };
        let Some(arm) = value_arm(module, block) else {
            continue;
        };
        let mut statements = block.statements[..block.statements.len() - 2].to_vec();
        let arm = module.block(arm).expect("a switch's arm is live").clone();
        statements.extend(arm.statements);
        module.set_block_statements(id, statements);
        module.set_block_terminator(id, arm.terminator);
        folded = true;
    }
    folded
}

/// The value arm of the switch `block` ends by returning, where the switch tests the suspension test bound just before it.
fn value_arm(module: &Module, block: &Block) -> Option<BlockId> {
    let [.., test, switch] = block.statements.as_slice() else {
        return None;
    };
    let Some(Statement::Let {
        result: tested,
        rhs:
            Rhs::Suspension {
                operation: SuspensionOperation::Is,
                ..
            },
    }) = module.statement(*test)
    else {
        return None;
    };
    let Some(Statement::Let {
        result: switched,
        rhs:
            Rhs::SwitchBool {
                scrutinee: Atom::Value(scrutinee),
                if_false,
                ..
            },
    }) = module.statement(*switch)
    else {
        return None;
    };
    let returns_switch = matches!(block.terminator, Terminator::Return(Atom::Value(returned)) if returned == *switched);

    (scrutinee == tested && returns_switch).then_some(*if_false)
}
