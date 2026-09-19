//! What a sequence read emits beside the proof its caller discharged.

use super::{functions, wat};

/// A program reading each carrier at a run-time value, so all three `$<carrier>/read` helpers are emitted. `try_get` is the spelling because it carries its own bound — the point here is what the helper emits, not how its caller discharged the proof.
const READS_EVERY_CARRIER: &str = r#"
    use /std/{Byte, Bytes, Bits, List, Option, Io, proc};

    Io/bind(proc/args, (args) =>
        let values = List/map(args, (a) => 1);
        let bytes = Bytes/join(x[], args);
        let bits = Bits/flatten(List/map(args, (a) => b[]));
        let from_list = Option/unwrap_or(List/try_get(values, 0), 0);
        let from_bytes = Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0));
        let from_bits =
            match Option/unwrap_or(Bits/try_get(bits, 0), false)
            | true => 1
            | false => 0
            end;
        proc/exit(from_list + from_bytes + from_bits))
    "#;

/// Every carrier's read refuses a position past the value's own length, and the three agree.
///
/// A leaf would trap in the engine reading its own payload, and that is what the `List` and `Bytes` helpers rested on while the bit grain's twin opened with a compare. It is not the same guarantee: a *view* reads `base.payload[offset + i]`, so a position past the window is a position the base array still holds, and the engine sees nothing wrong with it. The read answered a neighbouring element where the bit grain refused.
///
/// No well-typed program reaches it — `get` takes a proof that its index is within the length, and `slice` supplies the window's own — so what this asserts is the backstop: the check is there for a wrong erasure or a wrong checker, and the claim is that the three carriers make the same promise rather than two of them making a weaker one. A test that compiled a read and watched it answer would be testing the proof, not the backstop.
#[test]
fn every_carrier_refuses_a_read_past_its_length() {
    let wat = wat(READS_EVERY_CARRIER);
    let emitted = functions(&wat);

    for carrier in ["list", "bytes", "bits"] {
        let name = format!("${carrier}/read");
        let helper = emitted
            .iter()
            .find(|function| function.name == name)
            .unwrap_or_else(|| panic!("the fixture emits {name}"));

        // The labelled guard rather than what it reaches: how a refusal is spelled is the emitter's own business — a hoisted constant, a call to a helper — and this is about whether the compare is there at all.
        assert!(
            helper.body.contains("if $bounds"),
            "{name} reads without refusing a position past the length:\n{}",
            helper.body
        );
    }
}
