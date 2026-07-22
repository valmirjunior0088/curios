# Anonymous match functions

Working implementation specification for a surface form analogous to OCaml's `function`, expressed in Curios as `match =>`.

This document is the implementation handoff for the feature. Its durable user-facing semantics belong in `SYNTAX.md`, while lowering invariants belong in `curios-text` module documentation and tests.

## Status and dependency

This is the second of two related projects. It should preferably follow landed monomorphic, use-driven lambda inference, allowing anonymous match functions to inherit the same behavior as ordinary lambdas.

The syntax can technically be parsed and lowered before that project lands, but it would then require an expected function type in cases where its scrutinee carrier cannot be inferred from the arms. The implementation must not add a match-function-specific inference mechanism.

## Proposed syntax

```crs
match =>
| some(x) => x
| none() => default
end
```

The expression denotes a function of exactly one explicit argument. Applying it is equivalent to applying an ordinary single-argument lambda whose body is a headed match:

```crs
(value) =>
  match value
  | some(x) => x
  | none() => default
  end
```

The spelling deliberately extends the existing `match` family instead of reserving `function` as a new keyword.

## Grammar and disambiguation

The conceptual grammar is:

```text
match-function ::= "match" "=>" matrix-arm* "end"
matrix-arm     ::= "|" match-pattern "=>" term
```

Curios then has two visually distinct forms after the `match` keyword:

| Prefix | Meaning |
| --- | --- |
| `match <term>` | Ordinary headed pattern match |
| `match =>` | Anonymous one-argument match function |

The token after `match` is therefore a complete local discriminator. No whitespace-sensitive rule is required. `choose | test => body … end` is its own reserved keyword, disjoint from `match` entirely (see `SYNTAX.md`'s `choose` section) — it needs no disambiguation against this dispatch.

## Semantics

### One explicit argument

Every `match =>` expression constructs a function with exactly one explicit parameter. A tuple pattern matches one tuple-valued argument; it does not create a multi-argument function.

```crs
match =>
| (x, y) => x
end
```

The example above has one tuple argument and is equivalent to `(pair) => match pair | (x, y) => x end`.

Additional arguments are expressed by nesting ordinary lambdas or another match function.

### Ordinary matrix semantics

The arms have exactly the semantics of an existing headed match matrix:

- Constructor coverage and exhaustiveness rules are unchanged.
- Pattern refinements and dependent typing behavior are unchanged.
- A final wildcard arm has the same meaning and restrictions as in a headed match.
- Zero arms are legal exactly when an ordinary zero-arm match is legal for the inferred scrutinee type.
- Arm order does not introduce OCaml-style first-match row priority if Curios's existing matrix semantics do not provide it.

This is syntactic abstraction over a headed match, not a second pattern-matching language.

### No condition-ladder arms

`match =>` accepts matrix patterns only. It does not accept `choose`'s condition or binding forms, including `pattern = value` arms.

The separation is important: `choose` branches on independent conditions or bindings, whereas `match =>` partitions one future scrutinee.

### No explicit motive in the initial form

The initial syntax has no explicit motive slot. Users who need to spell a dependent motive can write the equivalent ordinary lambda and headed match:

```crs
(value) =>
  match value : motive
  | pattern => body
  end
```

A motive-less anonymous match function may still check against an expected dependent function type through the ordinary elaborator. The feature adds no special dependent-motive inference guarantee beyond that equivalence.

### Effects and postfix `!`

The anonymous matcher is a lambda boundary. Constructing it performs no branch effects; a postfix-`!` action inside an arm runs only when the function is applied and that arm is selected.

Lowering must use the same region-root behavior as an ordinary lambda body so do-notation cannot hoist an arm action outside the generated function.

### Direct invocation

Normal expression precedence applies. A directly invoked anonymous matcher should be written parenthesized unless the existing application grammar proves otherwise:

```crs
(match =>
 | some(x) => x
 | none() => 0
 end)(value)
```

The project should not add a special application precedence rule for this form.

## Inference behavior

`match =>` must share the behavior of an ordinary unannotated single-argument lambda.

After parked lambda inference lands, later use in the same enclosing item may constrain the scrutinee type:

```crs
let unwrap = match =>
  | some(x) => x
  | none() => 0
  end;
unwrap(Option/some(42))
```

Primitive arm shapes may constrain their carrier immediately through the ordinary match elaborator. Inductive constructor arms must not choose an inductive by tag spelling alone; if the carrier remains unknown, the same parked inference path used by an explicit lambda and headed match applies.

If the function remains unconstrained at the enclosing item boundary, it fails with the same diagnostic as the explicit spelling. There is no implicit generalization.

## Surface AST

Retain the construct explicitly in the text AST rather than desugaring it in the parser. Every surface form is its own top-level `Subterm` variant (see `choose`'s own `Subterm::Choose` for the precedent this follows); there is no longer a `Match` enum to nest a new variant inside. A conceptual shape is:

```rust
struct MatchFunc {
    arms: Vec<MatrixArm>,
}

enum Subterm {
    …
    Match(Match),
    Choose(Choose),
    MatchFunc(MatchFunc),
    …
}
```

Names should follow the existing AST vocabulary rather than this sketch if they differ.

Keeping the sugar in the surface AST provides:

- Exact parser tests without manufacturing a source-level binder that the user never wrote.
- Canonical pretty-printing and parse-print round trips.
- Better source spans and diagnostics.
- One explicit lowering point where equivalence to a lambda can be audited.

No new core AST node is warranted.

## Parser design

The implementation belongs with the existing match parsers in `curios-text/src/parse/match_expr.rs`.

The parser should recognize the shared prefix `match =>` as the commitment point, then reuse the ordinary inductive matrix-arm parser, likely `parse_inductive_match_branch` or its current equivalent.

Only failure to see the complete `match =>` prefix may backtrack to the headed match alternative — `choose` is a distinct reserved keyword and is never a backtrack target here. Once the arrow has been consumed, a malformed arm or missing `end` must report an anonymous-match-function syntax error rather than silently reinterpret the expression as another match form.

A likely choice structure is conceptually:

```rust
parse_match_func()
    .or(parse_match())
```

(`parse_match` is the current name of the headed-only matrix parser in `curios-text/src/parse/match_expr.rs`.)

The exact placement of `catch` must follow the parser monad's current commitment conventions. The invariant, not the literal combinator sequence, is normative.

No lexer change or keyword addition is needed. In particular, `function` remains a legal identifier.

## Pretty-printing

The canonical printer should preserve the new form:

```crs
match =>
| pattern => body
end
```

It should not print a generated lambda parameter and headed match. Arm layout, indentation, multiline decisions, and `end` placement should reuse the headed matrix printer so the two forms remain visually parallel.

If the existing printer duplicates arm formatting between match variants, factoring a shared matrix-arm printer is in scope only as needed for this feature.

## Lowering

Lower entirely in `curios-text` to existing core constructs:

1. Mint a fresh internal binder with no surface spelling.
2. Create the free core variable that refers to that binder.
3. Compile the arms with the ordinary headed matrix compiler using the variable as the scrutinee and no explicit motive.
4. Infer or create the lambda domain metavariable in the same order used by ordinary unannotated lambda lowering.
5. Wrap the compiled body with the existing explicit `Term::func` representation.

The lowering should share the headed matrix compiler entry point, such as `compile_matrix_headed`, rather than reproducing coverage, refinement, or motive logic.

For ordinary term lowering, arm leaves should use the existing `MatchCompiler::term` route. For region or do-notation lowering, they should use `MatchCompiler::region` so the generated lambda remains the region root.

Freshness and metavariable allocation order should match the explicit lambda spelling as closely as practical, especially if diagnostics or golden core prints expose generated identifiers.

No change is expected in `curios-core`, erasure, continuation lowering, wasm emission, the ABI, or the runtime specifically for this syntax. Its core output is an ordinary function containing an ordinary match.

## Implementation map

The likely implementation surface is:

- `curios-text/src/term.rs`: the explicit surface AST variant and associated span behavior.
- `curios-text/src/parse.rs`: parser-module imports or dispatch wiring if required.
- `curios-text/src/parse/match_expr.rs`: `match =>` recognition, committed diagnostics, and shared arm parsing.
- `curios-text/src/print.rs`: canonical rendering and shared matrix-arm formatting.
- `curios-text/src/into_core/lowerer.rs`: dispatch from the new surface node.
- `curios-text/src/into_core/match_compile.rs`: reuse of headed matrix compilation and term/region entry points.
- Parser and printer test modules beside those components.
- `curios-text` lowering tests and `curios/src/tests/matching.rs`: semantic equivalence and cross-stage execution.
- `documentation/SYNTAX.md`, `documentation/ROADMAP.md`, and affected module rustdocs: durable documentation once the feature lands.

Before implementation, re-read `SYNTAX.md`, the `curios-text` module documentation, and the current versions of every target file, as required by `AGENTS.md`; the specification describes responsibilities and invariants rather than freezing their present internal APIs.

## Diagnostics

- `match =>` followed by a malformed matrix arm should point into that arm and remain committed to the new form.
- A missing `end` should identify the anonymous match function as the unterminated construct.
- A `choose`-style condition or bind arm written after `match =>` should receive a matrix-pattern error rather than being reinterpreted as `choose`.
- Exhaustiveness, impossible-pattern, private-representation, and branch-type errors should be the same errors produced by the equivalent explicit lambda and headed match.
- An unconstrained scrutinee type should use the ordinary lambda-inference diagnostic, preferably anchored at the `match =>` introducer because there is no written parameter.

## Acceptance tests

At minimum, the implementation should pin the following cases:

- The parser produces the explicit anonymous-match-function AST for inline and multiline forms.
- Parse-print-parse round trips preserve the form and its arms.
- `match <term>` and `match =>` remain unambiguous; `choose` needs no disambiguation against this dispatch, being a distinct reserved keyword.
- A malformed construct after the consumed arrow does not backtrack into another match parser.
- Zero-arm syntax parses and delegates legality to ordinary match elaboration.
- An annotated or expected function type checks an anonymous match function.
- Parked lambda inference allows a later call to constrain an inductive matcher after that project lands.
- A still-unconstrained matcher fails at the enclosing item boundary.
- A tuple pattern denotes one tuple argument.
- A wildcard arm and nested patterns behave exactly as in a headed match.
- Coverage, impossible-pattern, refinement, and privacy errors match the explicit spelling.
- A higher-order function can receive an anonymous matcher as an argument.
- Parenthesized direct invocation works.
- A postfix-`!` action inside an arm remains inside the generated lambda region and runs only for the selected arm.
- Compiling and running representative programs produces the same result as the explicit lambda-plus-match spelling.
- Core-print or lowering tests show no new core construct.

## Suggested implementation sequence

1. Add the surface AST variant and parser with commitment tests.
2. Add canonical printing and round-trip tests.
3. Lower term bodies through the existing headed matrix compiler and wrap them in an ordinary function.
4. Lower region bodies and pin postfix-`!` behavior.
5. Add inference, dependent/refinement, privacy, and cross-stage equivalence tests.
6. Update durable syntax and roadmap documentation, then remove this handoff specification if desired.

## Non-goals

- A `function` keyword or any new reserved word.
- OCaml-style ordered row priority distinct from Curios's current match semantics.
- Multiple implicit scrutinees or multi-argument pattern functions.
- `choose`-style condition or binding arms inside the new form.
- An explicit motive syntax in the initial version.
- A syntax-specific inference algorithm.
- A new core, erased, continuation, or wasm node.
- Changes to exhaustiveness, refinement, or representation-visibility rules.

## Verification

After implementation, run the repository's full done bar in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Because `curios-text` is in the browser compiler's dependency graph, also run the `curios-web` wasm32 build and matching `wasm-bindgen --target web` step described in `AGENTS.md`.

## Retirement criteria

- Before this specification is deleted, `match =>` grammar and semantics are recorded in `SYNTAX.md`, parsing, printing, and lowering invariants are recorded in the owning `curios-text` module documentation and tests, remaining plans refer to the landed syntax rather than this file, the roadmap entry is a checked unlinked summary, and no reference to this filename remains.
