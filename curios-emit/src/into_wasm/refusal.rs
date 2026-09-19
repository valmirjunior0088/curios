//! What each [`curios_cont::Panic`] class says, and the module constant that says it.
//!
//! A refusal is a call to the `sys.panic` import with one of these sentences, followed by the `unreachable` that keeps the block's type — [`Table::refuse_instrs`](super::Table::refuse_instrs) is the one spelling of that sequence, whether the class arrived as a `curios_cont::Node::Panic` or was decided while lowering an intrinsic. The vocabulary is the IR's ([`curios_cont::Panic`]); the text is this module's, so a sentence is spelled once and the IR never carries prose. Each names the rule, the carrier and the remedy, never the operation, because by the time a refusal is emitted `x * 2` may be a shift and a folded literal is no operation at all. The messages are minted once per module as byte-string constants, unconditionally: every module refuses somewhere, and a constant nothing reaches costs a data segment and nothing more.

use {
    super::{EmissionData, EmissionValueName, ImmediateLayout},
    curios_utilities::{Grain, PackedBin},
};

/// The sentence `class` reaches the user as.
pub(crate) fn refusal_message(class: curios_cont::Panic) -> &'static str {
    match class {
        curios_cont::Panic::NatCarrier => {
            "a Nat left its carrier: a Nat is held below 2^31, and this computation produced a value past that or a negative one; /std/BigNat holds larger values"
        }
        curios_cont::Panic::IntCarrier => {
            "an Int left its carrier: an Int is held between -2^30 and 2^30 - 1, and this computation produced a value outside that; /std/BigInt holds larger values"
        }
        curios_cont::Panic::OutOfBounds => {
            "a read reached past the end of a Bits, Bytes or List value"
        }
        curios_cont::Panic::FltDecode => {
            "a Flt was decoded from a byte string that is not eight bytes long"
        }
        curios_cont::Panic::Cycle => {
            "a recursive value was read while its own initializer was still running: the group's members form a cycle no forcing order can satisfy"
        }
        curios_cont::Panic::Invariant => {
            "the program reached an arm the compiler had proved unreachable; this is a compiler bug, please report the program"
        }
    }
}

/// The module const holding `class`'s message, named by the class's own spelling.
pub(crate) fn refusal_const_name(class: curios_cont::Panic) -> EmissionValueName {
    EmissionValueName::from(format!("refusal/{class}"))
}

/// The message as the byte-string constant the module carries. Every sentence is longer than the immediate envelope, so the const is minted as a rope leaf and the refusal sequence can force it to its payload directly; the assertion is what keeps a shortened sentence from silently minting an immediate the sequence cannot force.
pub(crate) fn refusal_data(class: curios_cont::Panic) -> EmissionData {
    let bytes = refusal_message(class).as_bytes().to_vec();
    debug_assert!(
        !ImmediateLayout::of(Grain::X).holds(bytes.len()),
        "a refusal message must be long enough to be a rope"
    );
    EmissionData::Bin(Grain::X, PackedBin::from_bytes(bytes))
}
