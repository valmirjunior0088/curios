# Intrinsic signatures

**Assumes.** Every intrinsic's declared operand and result types are the ones its carrier has.

**Status.** **auditable only**, and unprobed. Typing hands each intrinsic a type off `Intrinsic::signature`, and the roster is the definition of what an intrinsic means, so a wrong row disagrees with nothing; no surface program can force one, since the spelling and the signature are read from the same table. Both checkers walk the one signature, and `/sys`'s declarations restate it, which the prelude build checks against the table. An audit is one comparison per row against the operand model the erased IR gives the same operation, the only other place those arities and carriers are written. The typing rule for a parameterized former must report the former's sort rather than its element's, or a list of proofs would infer at `Prop` ([What a proposition may carry](what-a-proposition-may-carry.md)).
