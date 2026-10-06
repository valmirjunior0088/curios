# Effects are descriptions, and the carrier has no eliminator

**Decision.** Every host operation and every guest cell or channel operation returns `Io(T)`, an opaque intrinsic carrier holding a *description* of a computation that yields a `T`, never the `T`. `Io` is built by two intrinsics, `pure` and `bind`, sequenced by `!` through the ordinary `Monad(Io)` witness, and eliminated by nothing. A program's tail is an `Io({})`, and the emitted entry point is the only place that forces one. An intrinsic that performs an effect returns an `Io`, which `Intrinsic::signature` states for both checkers, and nothing turns an `Io(T)` back into a `T`: there is no `IntrinsicHead` for `Io`, no match case, no projection and no `unsafe_perform`. `/sys/Io` holds `pure` and `bind` and nothing else; an operation lives with the subject its host row names. `Io(T)` erases to a zero-argument closure and forcing is a zero-argument application, and `Io/pure`'s operand is evaluated at the construction site, as an eager language evaluates every operand.

`/sys/proc/exit`, the diverging `proc_exit` row, is typed `(@A: Type, code: Byte) -> Io(A)`: the description ends the process and yields whatever its region wants, so an arm that exits ends a region of any type. The code is a `Byte` because POSIX hands a parent a status's low eight bits, so a wider code would reach it as another — `256` as the `0` of a success. There is no `Never`.

**Rationale.**

- **Every term of non-`Io` type is pure by typing.** A certifier walk deciding whether a scrutinee's spelling denotes one value fails in the accepting direction, conclusively at `match f(true)` for a parameter `f`, whose effectfulness is a fact about the environment rather than the term. Typing answers what the walk could not, and it restores refinement: a parameter-headed scrutinee refines its arm, since `(Bool) -> Bool` says the function is pure.
- **An eliminator in any spelling undoes the discipline**, which is why its absence is the invariant an intrinsic author needs.
- **A thunk is the whole runtime cost**, where an algebraic-effects design reifies a continuation and crosses to a host-side interpreter at every operation.
- **A term that never returns is unsound exactly when it inhabits a type nothing total inhabits**, and `Io(A)` is never one: an inhabitant of `Io(False)` yields nothing and proves nothing. A binding that reads nothing loses only inference — `let _ = proc/exit(1)!` leaves `A` unsolved and is written `proc/exit(@{}, 1)`. [Totality of the erased program](../soundness/totality-of-the-erased-program.md) keeps `exit` out of types and proofs on operational grounds.

**Rejected.**

- **Effect rows, effect annotations on arrows, or any effect information in types beyond `Io`**: a row sort in `curios-core`, row equality re-derived in the kernel, and a second constraint domain in the elaborator, for no written program this design accepts whose author wanted it rejected.
- **Algebraic effects with handlers and a host-side step interpreter**: reifying continuations and interpreting descriptions on the host multiplies allocation and boundary crossings for a generality the library does not use.
- **Monad laws in definitional equality, and a dependent `bind`**: `IoPure` and `IoBind` reduce their operands and rebuild, and a motive-carrying sequencing waits for a consumer.
- **Restricting which type `exit` may be given**: refusing a `Prop`-sorted result leaves an exit at a constructor-free `Empty : Type`, which eliminates into `Prop` unguarded since zero constructors leave the large-elimination guard no arm to check.
- **A core bottom `Never` with `Never ≤ A`**, which puts an uninhabited `Type`-sorted carrier into the erased program; **a unit payload, `Io({})`**, which refuses every exiting arm whose region is not unit; **a `Nat` code**, which lets `exit(256)` report a success.
