# A function's body follows its arrow

**Decision.** A body over parameters follows `=>`, as a lambda's and a `match` arm's do, and a value or a pattern's binding follows `=`. A declared function, `let increment(n: Nat) -> Nat => n + 1;`, a method in a witness or a literal, `step(d) => True/qed()`, and a test, `test name => …;`, take `=>`. A witness's premises are its signature's, `satisfy (@A: Type, use Show(A)) -> Show(Bag(A)) { … }`, its body the braces of the concept literal it is, and a witness with no premises writes none, `satisfy Show(Meters) { … }`. `let x: Nat = 3`, `let (a, b) = pair` and `let refl() = p` keep `=`, so `let name(…) =` binds the case `name` and `let name(…) =>` defines a function.

A local function reads as a lambda does: each parameter's type and its result are optional where a lambda's are, `let increment(n: Nat) => n + 1;`, and one that names itself states them all, refused as `missing type` where it does not; a top-level function and every member after `and` state theirs. A function's stated result, and a function type's, is a term short of a lambda, so `-> (T) => …` reads its result as the type `(T)` and the `=>` as the body's. The old forms are refused by the grammar as any other text is, `expected: =>`, with no refusal of their own.

A `!` belongs to the body a `=>` begins, and to a program's final term ([Bang sequencing is governed by the region's type](bang-sequencing-is-governed-by-the-regions-type.md)). A guard decides which body is taken and holds none: a `!` in one is refused as `action in a guard`, with the explicit form shown, a `let` binding the action inside the arm that tests it.

**Rationale.**

- **One arrow for every body over parameters.** A lambda's body, an arm's, and a function's and a method's share the arrow, so one token tells a function from a value wherever either stands, and a definition reads as the lambda it lowers to.
- **The arrow tells a function from a case, not the result.** Where only a stated `->` marked a function, `let increment(n) = n + 1;` read as the case `increment` and was refused as a recursive pattern, and a local function could not leave its result to inference; the arrow decides it in the parse.
- **A witness's head is its type.** A witness under premises is a function from them to a dictionary, so its head is written as every function type is, and the braces are the value's.
- **A guard chooses, an arm acts.** A guard is tested to pick a body, so an action stands in the body it picks, bound there by a `let`.

**Rejected.**

- **`->` as the discriminator**: a stated result was the only mark of a function, so a forgotten `-> T` read as a case, and no local function could leave its types to inference.
- **A constructor told by scope**, as Agda's and F#'s patterns are: `let name(…) =` would mean a function or a case by what is in scope where it is written.
- **Lean's `.ctor` sigil** for a constructor pattern: a second spelling for what `let refl() = p` already writes.
- **A guard's action lifted ahead of its match**, as Lean's nested actions are: it runs the actions of arms the match never reaches.
