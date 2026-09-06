# A template is a literal whose holes are its parameters

## Status

Researched and prototyped, not designed. This specification records what a survey of the field settled, what three prototypes measured against the compiler this checkout builds, and the decisions that have to be answered before a `/std` module is written. Nothing is started, and the toolchain defects listed under prerequisites come first.

## Why it exists

Curios renders text three ways today: `Str` concatenation, `/std/Fmt`'s positional places, and the documentation generator's Askama templates, which are Rust's. Nothing lets a program hold a page of text with named places in it, fill them by name, repeat a part of it over a list, or produce HTML that a value cannot break. Every peer surveyed ships one of those; the ones that ship it well disagree about almost everything else, which is why this is a design and not a port.

## What the survey settled

Five lineages, and the one thing each got right.

- **Text substitution** (Mustache, Handlebars, Liquid, Jinja, Pandoc, Go's `text/template`): the vocabulary a reader expects — a hole, a section that repeats or vanishes, a partial, a comment, an escape for the delimiter — and, from Pandoc and Mustache, the two whitespace rules that keep generated text readable: a tag standing alone on a line consumes the line, and a hole standing alone on a line re-indents a multi-line value to its column.
- **Compiled typed templates** (Hamlet, Twirl, Askama, templ, HEEx): a template is a function with typed parameters, and a missing or mistyped value is a compile error. Every one of them needs a macro, a quasiquote or a preprocessor to get there.
- **Markup as values** (Elm's `Html`, TyXML, Lucid, Plot, kotlinx.html): there is no template language at all; markup is a value and validity is a type. One way to do it, at the price of a reader who can no longer see the page.
- **Text-mode readers** (Scribble, Verso, Isabelle's antiquotations, Typst): prose is the default mode and code is the escape. Documents are programs.
- **Dependent format strings** (Idris's `printf`, Agda's `Text.Printf`, `/std/Fmt`): the argument types are computed from the literal by reduction. No macro, no preprocessor, one language.

Two results outrank any library. Parr's *Enforcing Strict Model-View Separation in Template Engines* (WWW 2004) shows four constructs suffice for practical generation — an attribute reference, a conditional on presence, a template applied over a multi-valued attribute, and recursion — and that everything past them is where templates rot. Go's `html/template`, Closure's strict autoescaping and lit-html share one security model: the static text is trusted because the author wrote it, every value is untrusted and escaped for the context it lands in, and a value already of the output's kind passes through.

The recurring complaint against the first lineage is right: forbidding logic in the template pushes presentation logic into a controller. It dissolves when the template lives inside a typed host language, because the massaging happens at the fill site, in the same file, checked. Curios is that host.

## What is certain

Measured on a release build of 2026-09-06 with three prototype programs, each a hundred to two hundred lines of Curios needing no compiler change, on an otherwise idle machine.

- **The whole mechanism exists.** A template literal parses at the type level by reduction, exactly as `Fmt/parse` does; the holes it still needs sit in its type as a list of names, or a tree once sections exist; a name the template lacks is refused where it is written, through a decided proposition in `/std/Cli/Has`'s shape ([a bound is stated in a decided proposition](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)); and a template with a hole left does not render, since `render` takes the template at the empty list.
- **The parse folds away.** After the ersd evaluate pass nothing of the parser survives: the continuation IR of a one-line template is 68 lines with no parser function in it, the shape `curios/src/tests/fmt.rs` pins for `Fmt`.
- **Sections type.** A section with holes of its own is a slot carrying the inner slot list; filling it takes a list and a function from an element and the inner template to a filled one, and the inner template's type is computed from the outer's by a lookup decided by reduction. Two rows rendered from a two-element list, the row's holes checked.
- **Escaping selects by kind through one two-parameter concept.** `Into(A, K)` with `K` an output kind — `Html` and `Text`, sealed structs over `Str` — keys on both heads, so a `Str` is escaped into `Html`, an `Html` passes, and a `Nat` shows; the same `Str` into `Text` passes. The method's result must be `K`: a concept parameter the method type does not mention is never pinned, and resolution waits forever on it.
- **A page-size literal costs seconds at the type level, and a bind chain multiplies it.** `Fmt/render` over a 2 KB literal elaborates in 2.6 s and over 20 KB in 29 s. The prototype's 2 KB template costs 6 s bare, and every bind by name adds 6 s — four binds, 27.5 s — because every type-level mention of the literal runs the parse again; naming the text at the top level changes nothing. A one-shot fill of the same four names costs 11.5 s, two parses. At 20 KB the prototype runs out of steps at the default budget ([a reduction step costs what it builds](../design/toolchain/a-reduction-step-costs-what-it-builds.md)).
- **An indexed walk is the parse that reduces.** The same grammar written as a fold accumulating into a packed value ran out of steps at 2 KB where the indexed walk, `Fmt`'s shape, finished; and a recursion written through a fold's lambda is refused as not known to terminate where the same walk as a `match` is accepted.
- **`Show` and `Spell` already split what Idris had to invent an `Interpolation` interface for.** A hole shows; a `Str` shows verbatim.

## The design

One sentence: **a template is a literal whose holes are its parameters, and it renders only when every hole is filled.** Everything below is that sentence applied to Parr's four constructs and the Go security model, spelled the Curios way: no new syntax beyond the block string literal, which is a second spelling of the same library value, since [syntax forms are closed](../design/language/syntax-forms-are-closed-semantics-extend-by-witness.md) and [literals are library values](../design/language/literals-are-library-values.md).

**The literal.** Mustache's spellings, since they are the ones every reader already knows and they do not collide with the braces of CSS, JavaScript or JSON inside a page: `{{name}}` a hole, `{{#items}}…{{/items}}` a section repeated over a list, `{{^items}}…{{/items}}` its inverse rendered when the list is empty, `{{! note}}` a comment. The delimiter is escaped as `\{{`, consistent with `Fmt`'s `\%`, and costs the language nothing: an unrecognized escape already stands for itself. A template is written as a block string literal — `"""` and a newline to open, a newline and `"""` to close, the indentation shared with the closer stripped, as [syntax.md](../syntax.md) states it — so it reads at the indentation of the code around it and the library strips nothing. A tag alone on its line consumes the line; a hole alone on its line re-indents a multi-line value to its column.

**The type.** `Template(K, slots)`, sealed as `Test` is, where `K` is the output kind and `slots` the tree of what the literal still needs, computed once by the parse. A slot is a name, or a name with the slots of its section.

**Filling.** One call fills every hole of one template from a list of named values of the kind, under a proposition decided by reduction that refuses a missing name and a name the template lacks, each by name: `fill(t, [("name", into("Ada")), ("count", into(3))])`. A section is filled by `each(section(t, "items"), xs, (x, row) => fill(row, …))`, whose result is a value of the kind bound like any other; presence is a list of zero or one; recursion is a filled template of the same kind bound into a hole, which is also what a partial is. The one-shot shape is chosen by measurement, not taste: it mentions the parse twice where a bind-by-name chain mentions it once per bind.

**Kinds.** A template declares its kind by the function that makes it — `Html/template`, `Text/template` — and a value enters a hole through `Into(A, K)`. `/std/Html` owns `Html`, escapes the five characters into it, passes an `Html` through, and offers `raw` for an author stating trust. A program writes `Into(Its, Html)` for its own types under the orphan rule as it stands. No contextual inference across script, style and unquoted-attribute positions in the first cut; whether the parser refuses a hole in those positions is decided below.

**Where it runs.** The parse runs at the type level during elaboration and again in the ersd evaluate pass, and both fold, so a literal template reaches WebAssembly as its pieces and its fills. A template read from a file at run time is the same parser behind a `Result` and a `Map`, and is not in the first cut.

**What is left out, each for a reason a peer paid.** An expression language in the braces (Jinja's, Hugo's): the host language is the expression language. Inheritance and blocks (Jinja's most-warned feature): composition through holes is where HEEx, Vue and Go's own proposal moved. Implicit context inheritance in sections (Mustache): a section's holes are its own, and an outer value is passed in by the closure that fills the row, visibly. Lambdas that re-parse text at run time (Mustache): nothing re-parses. Indentation-significant syntax (Haml, Pug, Slim): whitespace errors are their signature complaint. An `s!`-style interpolation: `Fmt` is the one-line case already. A macro system: the mechanism Askama and Hamlet need is exactly what Curios does not.

## What has to be decided

Each with the recommendation first.

- **Delimiters.** `{{name}}` (Mustache's; clash-free in HTML, familiar) or `{name}` (Rust's, Lean's, Python's; terser, collides with CSS and JSON in a page).
- **Fill shape.** One call over named values, measured at two parses per template, or bind by name with the remaining holes tracked in the type, measured at one parse per bind until the reducer retains a closed computation across mentions, which is a prerequisite below. The second is the prettier type; the first is the one the reducer can afford today.
- **Kind per template with `Into(A, K)`**, Closure's model and a hundred lines, or contextual autoescaping per hole as Go does, a context machine over HTML, CSS and JavaScript. Whether the first cut refuses a hole inside `<script>`, `<style>` or an unquoted attribute at parse time, which is a textual check, or documents it as the author's responsibility.
- **Sections over lists only**, with `Option` through a list and an inverse section, or Mustache's truthiness. The first types; the second does not.
- **Runtime templates** in the first cut or after it.
- **Naming.** `/std/Template` with `/std/Html` beside it, or one module. Whether `Text` is a kind or the absence of one.
- **The size ceiling.** Whether a page-size template is in scope for the first cut. If a 20 KB literal must elaborate, the cheap route is a `/sys` row that finds a byte from an offset, so the type-level scan costs per hole rather than per byte, and the expensive route is a faster reducer. The first runs against [evaluating a closed term is representation, not judgment](../design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md), which refused a scan intrinsic so that the machine would accelerate every fold alike; the measurement above is the case for revisiting it, and the decision is made there or not at all.

## Prerequisites

Toolchain defects met while prototyping, each to be fixed before the module is written.

- **The language has no block string literal.** A raw newline inside `"…"` parses today by accident, its indentation is part of the value, and `curios format` rewrites the literal onto one line with `\n` escapes. The block form is what a template is written in, and lands first.
- **A stuck computed type is reported by dumping its definition.** `Fmt/render(s)` with a runtime `s` refuses with `applied a non-function` and the unfolded body of `format_type_with`, where the reader needs its spelling and the fact that `s` is not known.
- **A diagnostic quotes a long literal whole.** A budget refusal over a 20 KB literal printed the literal verbatim.
- **An undischarged decided proposition is reported by its unreduced spelling.** `nothing discharged Has(remove(remove(holes("…"), "count"), "name"), "name")` is accurate and unhelpful; `/std/Cli/get` refuses the same way.
- **A closed computation over an inline literal is re-evaluated at every mention.** `Str/len` over a 2 KB literal named by a top-level `let` costs the same mentioned once or four times, while over the literal written inline each mention pays again, and a parse mentioned four times costs four parses. This is not a prerequisite for the one-shot fill, but it is what would make the bind-by-name shape affordable, and it deserves its own investigation in the reduction cache's key and admission.

## Deliberately not specified

The escaping tables beyond HTML's five characters, which the module will document. The dynamic template's API. Whether `curios document` ever renders through this module, which is a question about the toolchain's ownership boundary rather than about the library. Any timing.
