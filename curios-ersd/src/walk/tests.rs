use {
    super::control_blocks,
    crate::{Atom, Constant, ErsdBuilder, Terminator},
};

/// A computed member's `init` is inline control and the walk must reach it — [`control_blocks`] states what a deep copy makes of an init block it misses: a block the original and the copy own jointly, which the verifier reports as a block with more than one owner, aborting compilation of any program whose recursive group has a value member.
#[test]
fn a_recursive_value_members_init_block_is_control() {
    let mut builder = ErsdBuilder::new();
    let member = builder.value(Some("member".into()));

    builder.open_block();
    builder.open_block();
    let constant = builder.constant(Constant::Bool(true));
    let init = builder.seal_block(Terminator::Return(Atom::Constant(constant)));
    let group = builder.rec_group(vec![], vec![(member, init)]);
    builder.let_rec(group);
    let entry = builder.seal_block(Terminator::Return(Atom::Value(member)));
    builder.set_entry(entry);

    let module = builder.finalize().expect("the module verifies");

    assert!(
        control_blocks(&module, entry).contains(&init),
        "the walk must reach a recursive value member's init block",
    );
}
