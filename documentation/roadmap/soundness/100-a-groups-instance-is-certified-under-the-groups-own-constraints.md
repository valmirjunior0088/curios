# A group's instance is certified under the group's own constraints

**Not refined yet.** The formation ticket *A group's instance is held to the group's constraints and its bodies were checked under the enclosing declaration's* on [the soundness board](../../design/soundness/the-soundness-board.md) is a proof of `False` the kernel admits: Hurkens' paradox over the products of a group stating three levels and no constraint, read at `{0, 0, 1}` inside a declaration that assumes `u + 1 ≤ w` and `v ≤ w`. This specification reserves its fix; no shape is chosen.

## What is wrong

The instance rule (`Subterm::Instance`, `curios-cert/src/kernel/infer.rs`) certifies a `RecProj` head's group at its generic spelling with `check_group` (`curios-cert/src/kernel/module.rs`), which assumes no constraints of the group's own, so its bodies are judged under `Kernel::assumed`, the enclosing declaration's. A level parameter is positional, so the group's `UniverseParam(i)` reads as the declaration's. `synth_neutral` (`curios-cert/src/kernel/sort.rs`) then holds the instance to the group's context alone: the bodies and the instance are judged under two sets of hypotheses.

## Alternatives

- **The group's own context.** `check_group` runs with `Kernel::assumed` replaced by the group's constraints and restored after. The typing and sort memos key a term and not the constraints it was judged under, and `Type(UniverseParam(0))` names a different level on each side of the swap, so no remembered verdict may cross it.
- **The group at the instance.** The group is instantiated at the instance's levels and checked under the enclosing constraints, which no memo crosses; the group is certified once per instance, and its totality verdict is filed under the instantiated group, where a reader of the generic one does not find it.
- **An item's group only.** A `RecProj` instance is refused unless an item in scope declares its group, whose own walk certified it under its own context; how unfolding spells an instance is traced to its consumers first.

## Not known

- Whether any source program the elaborator admits builds a `RecProj` instance inside a declaration with constraints of its own.
- Whether an inline `rec`, whose levels are the enclosing declaration's, keeps today's reading under each alternative.

## Verification and retirement

The ticket's witness is refused by the kernel and the workspace suite passes; the ticket turns fixed in that commit, and this specification is deleted with it.
