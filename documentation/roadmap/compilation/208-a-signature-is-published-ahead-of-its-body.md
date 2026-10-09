# A signature is published ahead of its body

**Not refined yet.** This specification reserves the order a declaration is elaborated in where it reads another's signature and nothing of its body. It is not an implementation plan.

## What is missing

A declaration is published whole, its signature with its body, so one that names another waits for that other's body, and two that name each other are a cycle where neither reads the other's body. `let neq(a: Dyadic, b: Dyadic) -> Bool = Bool/not(a == b)` beside `satisfy Eql(Dyadic) { eql = eql, neq = neq }` is refused (`curios-elab/src/elaborate/module/demand.rs`): the body of `neq` resolves `==` to the witness, and the witness names `neq`, of which it reads the signature alone, which is written out. The refusal says so and gives the two ways around it, the definition calling by name the function it means or its body written in the witness, which may read its own methods. So `/std`'s carriers, whose witnesses delegate to named functions, state no method by the operator of another: `/std/Dyadic/neq`, `gt` and `ge` call `eql`, `lt` and `le` by name.

## What is known

- **The elaborator tells the two reads apart and has no time to answer one alone.** What a declaration asks of another is recorded by its part, a signature or a body (`Question`, `curios-elab/src/context/traced.rs`), and the witness asks the signature of `neq` and never its body. The declaration is the unit of work all the same: it is attempted and published whole, so a signature is answered once its body is. The order that serves the pair is the signature of `neq`, the witness against it, then the body of `neq` against the witness, and [item tasks](200-item-tasks.md) names the same split as a refinement for throughput, for a declaration whose signature fixes its own universe context.
- **A stored unit has no form for the pair.** The body of `neq` names the witness and the witness names `neq`. A unit stores an item after every item it names (`Module::ordered`, `curios-core/src/module.rs`), and the certifier refuses a name an item reaches before it is defined. A recursive group is the one form whose members name each other, and it holds a witness already, one that names itself being a group of one (`elaborate_module_let`, `curios-elab/src/elaborate/module.rs`); nothing forms a group of a definition and a witness, and the surface joins declarations of one kind.
- **Totality reads the pair as a recursion.** `neq` reaches itself through the witness, whose `neq` field is the definition, unless the analysis reads `eql` projected off the dictionary as the field it selects.

## Refinement

- Who forms the group: its author, by a form the surface does not have, or the compiler, where a cycle closes through a witness a goal resolved to, which no name in the source wrote.
- What a definition and a witness that name each other are in a stored unit, a group holding both or a definition stated ahead of its body, and what each asks of the kernel, of the certifier's record and of erasure.
- Whether size-change reads a projection of a dictionary as the field it selects, and where that is no call.
- Which signatures are published early: one that fixes its own universe context, as item tasks states, or every signature written out in full.
- Whether the split is this specification's or item tasks', which would then order the pair as it schedules every other.

It retires when the pair above elaborates, both checkers accept it, and `/std/Dyadic`'s `neq`, `gt` and `ge` are written with their operators.
