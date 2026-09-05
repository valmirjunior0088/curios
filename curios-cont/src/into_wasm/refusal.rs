//! What each [`Panic`] class says, and the module constant that says it.
//!
//! A refusal is a call to the `sys.panic` import with one of these sentences, followed by the `unreachable` that keeps the block's type — [`Table::refuse_instrs`](super::Table::refuse_instrs) is the one spelling of that sequence, whether the class arrived as a `CpsNode::Panic` or was decided while lowering an intrinsic. The vocabulary is the IR's ([`Panic`]); the text is this module's, so a sentence is spelled once and the IR never carries prose. Each names the rule, the carrier and the remedy, never the operation, because by the time a refusal is emitted `x * 2` may be a shift and a folded literal is no operation at all. The messages are minted once per module as byte-string constants, unconditionally: every module refuses somewhere, and a constant nothing reaches costs a data segment and nothing more.

use {
    super::{EmissionData, EmissionValueName, ImmediateLayout},
    crate::Panic,
    curios_utilities::{Grain, PackedBin},
};

impl Panic {
    pub(crate) const ALL: [Panic; 6] = [
        Panic::NatCarrier,
        Panic::IntCarrier,
        Panic::OutOfBounds,
        Panic::FltDecode,
        Panic::Cycle,
        Panic::Invariant,
    ];

    /// The sentence this class reaches the user as.
    pub(crate) fn message(self) -> &'static str {
        match self {
            Panic::NatCarrier => {
                "a Nat left its carrier: a Nat is held below 2^31, and this computation produced a value past that or a negative one; /std/BigNat holds larger values"
            }
            Panic::IntCarrier => {
                "an Int left its carrier: an Int is held between -2^30 and 2^30 - 1, and this computation produced a value outside that; /std/BigInt holds larger values"
            }
            Panic::OutOfBounds => "a read reached past the end of a Bits, Bytes or List value",
            Panic::FltDecode => "a Flt was decoded from a byte string that is not four bytes long",
            Panic::Cycle => {
                "a recursive value was read while its own initializer was still running: the group's members form a cycle no forcing order can satisfy"
            }
            Panic::Invariant => {
                "the program reached an arm the compiler had proved unreachable; this is a compiler bug, please report the program"
            }
        }
    }

    /// The module const holding this class's message, named by the class's own spelling.
    pub(crate) fn const_name(self) -> EmissionValueName {
        EmissionValueName::from(format!("refusal/{self}"))
    }

    /// The message as the byte-string constant the module carries. Every sentence is longer than the immediate envelope, so the const is minted as a rope leaf and the refusal sequence can force it to its payload directly; the assertion is what keeps a shortened sentence from silently minting an immediate the sequence cannot force.
    pub(crate) fn data(self) -> EmissionData {
        let bytes = self.message().as_bytes().to_vec();
        debug_assert!(
            !ImmediateLayout::of(Grain::X).holds(bytes.len()),
            "a refusal message must be long enough to be a rope"
        );
        EmissionData::Bin(Grain::X, PackedBin::from_bytes(bytes))
    }
}
