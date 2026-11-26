# A definition and a witness that read each other are one group

**Not refined yet.** This specification reserves elaborating together a definition and a witness that read each other's signatures, where today the pair is refused as a cycle. Its design is open, under *Open questions*; its consumer is `/std`'s workaround below.

## What is missing

**A declaration is published whole, so two that read each other's signatures are a cycle.** `let ord(a: Num, b: Num) -> Ordering` guarding with `a < b`, beside `satisfy Ord(Num) { ord = ord, lt = lt, le = le }`, is refused as a declaration cycle (`curios-elab/src/elaborate/module/demand.rs`), the refusal naming the two ways around it. `/std/Nat`'s, `/std/Int`'s and `/std/Byte`'s `ord` take the first: each guards with its `lt` called by name, where `<` would read as the witness's own. The lemmas a carrier's witness names for the laws `Ord` and `Eql` carry take it too: `Nat/ord`'s and `Int/ord`'s, and `Int/le`'s `trans` and `antisym` that `Int/ord`'s read, state their types through `Nat/lt`, `Int/le` and `Int/eql` where a caller writes the operator, one type by conversion.

## What is known

- **The elaborator tells the two reads apart** (`Question`, `curios-elab/src/context/traced.rs`): the witness asks the signature of `ord` and never its body, and the declaration is published whole all the same. Publishing a signature ahead of its body ([item tasks](../compilation/301-item-tasks.md)' stage 5) is one way to admit the pair, neither then waiting on the other's body.
- **A stored unit has no form for such a pair.** A unit stores an item after every item it names (`Module::ordered`, `curios-core/src/order.rs`), and the certifier refuses a name an item reaches before it is defined; a recursive group is the one form whose members name each other, and nothing forms a group of a definition and a witness.
- **Totality reads such a pair as a recursion**: a field that names a function is that function's call ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md), *A call is read off a path*), so `ord` reaches itself through the witness with no descent.
- **Haskell admits it**: a module's top-level bindings and its instances' methods are one scope, checked after every signature is known ([Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/), §4.5 and §4.3.2).

## Open questions

- Who forms the group: its author, or the compiler where a cycle closes through a witness a goal resolved to.
- What such a pair is in a stored unit, and what each part asks of the certifier, its record and erasure.
- Whether size-change reads a field that names a member as that member.

## Verification and retirement

Check, once refined: the pair above elaborates and certifies; a pair whose bodies loop with no descent is still refused; `Nat`'s, `Int`'s and `Byte`'s `ord` are written through the operators. Once it lands, the demand module's refusal is restated and this file is deleted.
