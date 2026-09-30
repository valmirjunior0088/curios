# Foreign calls past scalars and byte strings

**Not refined yet.** This specification reserves the marshalling a plugin needs to speak more than scalars and byte strings. It is not an implementation plan.

## The ceiling

A `foreign` declaration answered by a plugin — a WebAssembly module a package names in its manifest, filled into `curios-runtime`'s `ForeignBindings` — may take and answer scalars and byte strings. A `Handle`, a `List` and several results at once are each refused where the signature is read, because marshalling is the host copying between the guest's GC array and the plugin's linear memory, and only those shapes have a copy.

## Refinement

Each shape states its layout on both sides, the copy the host performs, the ABI row it adds (`curios-abi` is the wire contract's source of truth), and the native and JavaScript implementations that must agree with it.
