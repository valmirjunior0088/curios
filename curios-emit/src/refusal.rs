//! What each [`curios_cont::Panic`] class says, and the names of the function that says it and of the data segment it reads.
//!
//! A refusal is a call to the class's `$refuse/<class>` helper, followed by the `unreachable` that keeps the block's type — [`Table::refuse_instrs`](super::Table::refuse_instrs) is the one spelling of that sequence, whether the class arrived as a `curios_cont::Node::Panic` or was decided while lowering an intrinsic. The helper builds the sentence from a passive data segment and hands it to the `sys.panic` import, so a message exists only once a refusal fires: a module declares the helpers its code can reach and allocates nothing for them at start-up. The vocabulary is the IR's ([`curios_cont::Panic`]); the text is this module's, so a sentence is spelled once and the IR never carries prose. Each names the rule, the carrier and the remedy, never the operation, because by the time a refusal is emitted `x * 2` may be a shift and a folded literal is no operation at all.

/// The sentence `class` reaches the user as.
pub(crate) fn refusal_message(class: curios_cont::Panic) -> &'static str {
    match class {
        curios_cont::Panic::NatWire => {
            "a Nat argument to a host function is past what the wire carries: a Nat crosses to the host below 2^64, and inside the program it is unbounded"
        }
        curios_cont::Panic::IntWire => {
            "an Int argument to a host function is outside what the wire carries: an Int crosses to the host between -2^63 and 2^63 - 1, and inside the program it is unbounded"
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
        curios_cont::Panic::HostReply => {
            "the host answered a call with a value outside that call's contract, such as a Byte past 255; the program is not at fault, the host's implementation of the call is"
        }
        curios_cont::Panic::Invariant => {
            "the program reached an arm the compiler had proved unreachable; this is a compiler bug, please report the program"
        }
    }
}

/// The helper that refuses with `class`'s sentence, named by the class's own spelling.
pub(crate) fn refuse_func_name(class: curios_cont::Panic) -> curios_wasm::FuncName {
    curios_wasm::FuncName::from(format!("refuse/{class}"))
}

/// The passive data segment holding `class`'s sentence, which its helper alone reads.
pub(crate) fn refusal_data_name(class: curios_cont::Panic) -> curios_wasm::DataName {
    curios_wasm::DataName::from(format!("refusal/{class}"))
}
