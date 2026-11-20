# A signature is published ahead of its body

**Not refined yet.** This specification reserves publishing a declaration in two parts, its signature first where that signature fixes its own universe context, so that a declaration reading only another's signature waits for nothing more. It is a refinement of [item tasks](300-item-tasks.md) for throughput and is blocked on that specification's whole-declaration scheduling model: it opens only where the split model under *What opens it* puts such a signature on the critical path. Whole-declaration scheduling is the baseline and does not wait for it.

## What is missing

**A declaration is published whole.** Its signature is published with its body, so a declaration reading only another's signature waits for that body too, and a signature on the critical path holds back every item that reads it. The pair a definition and a witness form when each reads the other's signature, refused today as a cycle, is [theory/302](../theory/302-a-definition-and-a-witness-that-read-each-other-are-one-group.md)'s, which this mechanism is one way to deliver.

## What opens it

A split model extending [item tasks](300-item-tasks.md)' whole-declaration graph model states which signatures can publish early, separate non-overlapping signature and body weights whose sum accounts for the declaration's work, and each task's dependencies. Compare both models at equal total work, explain any worse split schedule and distinguish list-schedule effects from a change in critical path before reporting a factor. Counted, like the model it extends; the split has no throughput claim until its weights and dependencies are so defined.

## Open questions

- Which signatures are published early.
