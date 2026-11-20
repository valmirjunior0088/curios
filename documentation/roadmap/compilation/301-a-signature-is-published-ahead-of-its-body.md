# A signature is published ahead of its body

**Not refined yet.** This specification reserves publishing a declaration in two parts, its signature first where that signature fixes its own universe context, so that a declaration reading only another's signature waits for nothing more. It is a refinement of [item tasks](300-item-tasks.md) for throughput and is blocked on that specification's whole-declaration scheduling model: it opens only where the split model under *What opens it* puts such a signature on the critical path. Whole-declaration scheduling is the baseline and does not wait for it.

## What is missing

**A declaration is published whole.** Its signature is published with its body, so two declarations that read each other's signatures alone are a cycle: `let neq(a: Dyadic, b: Dyadic) -> Bool = Bool/not(a == b)` beside `satisfy Eql(Dyadic) { eql = eql, neq = neq }` is refused (`curios-elab/src/elaborate/module/demand.rs`), the refusal naming the two ways around it, and `/std/Flt/internal/Dyadic`'s `neq`, `gt` and `ge` call `eql`, `lt` and `le` by name for it. That refusal is no reason to take this on by itself.

## What is known

- **The elaborator tells the two reads apart** (`Question`, `curios-elab/src/context/traced.rs`), the witness asking the signature of `neq` and never its body, and publishes the declaration whole all the same.
- **A stored unit has no form for such a pair.** A unit stores an item after every item it names (`Module::ordered`, `curios-core/src/order.rs`), and the certifier refuses a name an item reaches before it is defined; a recursive group is the one form whose members name each other, and nothing forms a group of a definition and a witness.
- **Totality reads such a pair as a recursion**: a field that names a function is that function's call ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md), *A call is read off a path*), so `neq` reaches itself through the witness with no descent.

## What opens it

A split model extending [item tasks](300-item-tasks.md)' whole-declaration graph model states which signatures can publish early, separate non-overlapping signature and body weights whose sum accounts for the declaration's work, and each task's dependencies. Compare both models at equal total work, explain any worse split schedule and distinguish list-schedule effects from a change in critical path before reporting a factor. Counted, like the model it extends; the split has no throughput claim until its weights and dependencies are so defined.

## Open questions

- Who forms the group: its author, or the compiler where a cycle closes through a witness a goal resolved to.
- What such a pair is in a stored unit, and what each part asks of the certifier, its record and erasure.
- Whether size-change reads a field that names a member as that member.
- Which signatures are published early.
