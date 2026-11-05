# Bang sequencing is governed by the region's type

**Decision.** A postfix `!` lowers to a Core transient at every lexical depth. Elaboration reads its sequencing monad from the region's expected type, resolves the `Monad` witness and any declared `Monad/Lift` edge, and replaces the transient with the corresponding `bind` application; no source-function requirement precedes that judgment. An undetermined region waits for its type, and a region whose monad is never determined or whose type admits no monad is reported at the written `!`.

A local `let`'s value, with or without an annotation, shares the surrounding sequencing region; the annotation keeps the expectation of a type expression, and its bangs never float into the value it annotates. Nested bindings contribute the lexical context their actions need, in evaluation order, and pure bindings after the last action stay inside their expression. Function bodies, `match` and `choose` branches and recursive-group members keep their own regions, so an independent local computation is a function, one of no parameters included, called where its description is needed. A top-level value, an entry point, a test body and a witness field sequence under the same rule. A type expression follows ordinary monadic typing: a pure monad may compute a type through its ordinary eliminator, an `Io(Type)` description is no `Type`, and totality checking governs type computation as it governs any.

**Rationale.**

- **The type system already judges the `bind` a `!` stands for.** A lexical permission check would refuse a well-typed description before that judgment and hide the type-directed diagnosis for an invalid one; keeping the transient's span lets elaboration name the region's type or the missing embedding where the action is written, without exposing a synthesized call's implicit arguments.
- **A binding's annotation is no region boundary**, so moving a definition between a module and a local block changes nothing of its sequencing, and a local binding sequences in place, so an unused first statement cannot change which continuation an action belongs to.
- **Each branch's monad is decided by bidirectional typing**, independently, and an action never supplies it.

**Rejected.**

- **A source-function requirement**, which duplicates a judgment monadic typing already makes and excludes declared tests despite their fixed monadic result; **an exception for tests**, which keeps the restriction everywhere else.
- **An annotated local `let` as a region of its own**, which gives adding an annotation a control-flow meaning.
- **A local-region form**, which duplicates what a function is.
- **Reading the monad from the action**, which makes one body depend on which action is written first.
- **Every `!` forced into its enclosing function's result monad**, which removes the typing a branch has as a region.
- **A ban below a type annotation**, which refuses computations an explicit `bind` admits.
