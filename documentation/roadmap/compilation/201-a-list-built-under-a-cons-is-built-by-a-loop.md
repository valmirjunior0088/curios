# A list built under a cons is built by a loop

**Not refined yet.** This specification reserves a recursion whose result is built under a cons taken as a loop, as the monoid rebase takes one whose result feeds a registered monoid. It is not an implementation plan: what the row is worth is a timing that waits for a quiet machine.

## What is missing

**A list built under a cons takes a frame an element.** `curios-ersd`'s monoid rebase (`optimize/rebase.rs`) makes a recursion whose result feeds a registered monoid a loop, and registers no row that builds a sequence: its own comment gives the reason, that `ListAppend` and `BinAppend` append an element to a sequence, so the accumulator's carriers do not line up, and that a row which does not commute needs a placement rule. So `List/range`, `take`, `zip`, `take_while` and every builder written `[h, ..f(t)]` nest as deep as their list. [A module's stack](../../../curios-runtime/README.md#a-module-runs-on-a-stack-of-its-own) holds that depth, and each still pays a frame and a return an element where a loop pays neither. An accumulator written into each in `/std` is not the fix: a proof about one follows its recursion, as `curios`'s `tests::universes::instance_tests` proves the length of a `zip` by induction on it, and a builder over `Str/At/forward`, which no proof unfolds, is handed what it has built (`Str/split`, `lines`, `replace`, `escape`).

## Refinement

- **A sequence built under a cons.** The row with its placement rule, the recursion on the right of the cons and the element appended to the accumulator, the transformation OCaml calls [tail modulo cons](https://ocaml.org/manual/5.3/tail_mod_cons.html) and Koka generalizes as [tail recursion modulo context](https://www.microsoft.com/en-us/research/publication/tail-recursion-modulo-context-an-equational-approach-2/). Check: a builder's worker found by name in the optimized program, as `curios`'s `tests::runtime::loop_tests::arena_deferred_context_recursion_is_stack_safe_at_depth` finds `count`'s, and the timing with its protocol.
- **What the row is worth**, timed on a quiet machine with its protocol, before it lands.
