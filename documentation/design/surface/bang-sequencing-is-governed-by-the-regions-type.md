# Bang sequencing is governed by the region's type

**Decision.** A postfix `!` stands only in a body a `=>` begins: a declared function's, a local function's, a lambda's, a `match` arm's or a witness method's, and a test's body, each a function of none the runner calls, beside a program's final term. A top-level value and a witness's value hold none, and a `!` in one is refused where it is written; a guard decides which body is taken and holds none either, refused as `action in a guard`, the action bound inside the arm that tests it ([A function's body follows its arrow](a-functions-body-follows-its-arrow.md)). A `let` chain stands only as a body — a function's, a lambda's, a `match` arm's, a test's or a top-level value's, a program's final term, and a chain's own tail — so a `let`'s value is a term and never a chain, and a value that sequences is a function, one of no parameters included, called where its description is needed.

Within a function's body a postfix `!` lowers to a Core transient at every lexical depth. Elaboration reads its sequencing monad from the region's expected type, resolves the `Monad` witness and any declared `Monad/Lift` edge, and replaces the transient with the corresponding `bind` application. An undetermined region waits for its type, and a region whose monad is never determined or whose type admits no monad is reported at the written `!`.

A local `let`'s value, with or without an annotation, shares the surrounding sequencing region; the annotation keeps the expectation of a type expression, and its bangs never float into the value it annotates. Nested bindings contribute the lexical context their actions need, in evaluation order, and pure bindings after the last action stay inside their expression. Function bodies, `match` arms and recursive-group members keep their own regions. A type expression follows ordinary monadic typing: a pure monad may compute a type through its ordinary eliminator, an `Io(Type)` description is no `Type`, and totality checking governs type computation as it governs any.

**Rationale.**

- **One region per function, and one reading of a value.** Where a top-level value was a region of its own and a local value shared its function's, one text read two ways that both typed: `let later: Io({}) = let () = print("one")!; print("two");` performed its first action where it bound and bound the rest inside a function, and was one whole description at the top level, and a test binding a `Try` by `let` ran the wait in it at the binding. With a `!` only in a function's body and a chain only as a body, the second reading is no longer spelled, and nothing has to report which one was meant.
- **A function is what an independent computation already is.** A test's body and a program's final term are each one the runner calls, bound by no name and read as no value, so the rule takes nothing from either, and both sequence as a function's body does.
- **The type system still judges the `bind` a `!` stands for.** Inside a function, elaboration names the region's type or the missing embedding where the action is written, without exposing a synthesized call's implicit arguments.
- **Each branch's monad is decided by bidirectional typing**, independently, and an action never supplies it.

**Rejected.**

- **A `!` in any region its type admits**, a top-level value's among them, under which a definition read one way in a module and another in a function, both readings typing alike; **a program's final term without `!`**, which a `main()` called there says again at no gain, the tail being bound by no name; **a lint or a warning for a value that sequences**, which leaves both readings spelled where the grammar can hold one.
- **A parenthesized chain as an operand**, `1 + (let n = 2; n * n)`, a chain in a value position by another spelling, where bindings before the expression say the same.
- **An annotated local `let` as a region of its own**, which gives adding an annotation a control-flow meaning.
- **A local-region form**, which duplicates what a function is.
- **Reading the monad from the action**, which makes one body depend on which action is written first.
- **Every `!` forced into its enclosing function's result monad**, which removes the typing a branch has as a region.
- **A ban below a type annotation**, which refuses computations an explicit `bind` admits.
