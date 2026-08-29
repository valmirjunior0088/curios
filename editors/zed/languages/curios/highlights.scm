; Captures for the `curios` grammar in `editors/grammar`. Zed reads only this file — the grammar ships no `queries/` — so it is the single copy, and `editors/grammar`'s `npm test` compiles it against the grammar so a renamed node cannot orphan a capture silently.

(comment) @comment

; Keywords, including the contextual ones — the grammar only lexes `concept`, `satisfy` and `and` as keywords in their positions.
[
  "let"
  "and"
  "mod"
  "use"
  "pub"
  "end"
  "induct"
  "struct"
  "concept"
  "satisfy"
  "foreign"
  "match"
  "choose"
] @keyword

(sort) @type
(boolean) @boolean
(number) @number
(string) @string
(escape_sequence) @string.escape
(character) @string.special.symbol
; A goal is punctuation, not `punctuation.special`: the default themes paint the latter red, and a hole the author wrote on purpose is not an alarm.
(goal) @punctuation

; Definitions.
(let_member name: (identifier) @function)
(foreign_item name: (identifier) @function)
(let_binding pattern: (identifier) @variable)
(let_binding pattern: (identifier) @function parameters: (parameters))
(mod_item name: (identifier) @type)
(induct_body name: (identifier) @type)
(struct_item name: (identifier) @type)
(concept_item name: (identifier) @type)
(satisfy_member concept: (path (identifier) @type .))
(constructor name: (identifier) @constructor)
(constructor_pattern name: (identifier) @constructor)

; Fields and labels. `@variable.member` rather than `@property`: Zed falls back to `variable` for the suffix, so a field reads as the variable it names in every theme, where `property` shares the alarm red of `variable.parameter` in the default ones.
(field_declaration name: (identifier) @variable.member)
(field_definition name: (identifier) @variable.member)
(pattern_field name: (identifier) @variable.member)
(match_pattern_field name: (identifier) @variable.member)
(field_identifier) @variable.member

; Binders. Plain `@variable` rather than `@variable.parameter`: a binder and its uses are the same name, and a theme that paints the two apart makes every function head read as an alarm.
(type_parameter name: (identifier) @variable)
(index name: (identifier) @variable)
(payload name: (identifier) @variable)
(parameter pattern: (identifier) @variable)
(function_type_parameter name: (identifier) @variable)
(lambda_parameter pattern: (identifier) @variable)
(sugar_parameter name: (identifier) @variable)
(successor_pattern predecessor: (identifier) @variable)
(list_pattern head: (identifier) @variable)
(list_pattern tail: (identifier) @variable)
(packed_pattern head: (identifier) @variable)
(packed_pattern tail: (identifier) @variable)

; Paths: the last segment of a call head is a function, and a capitalized name is a type by the library's convention — the language itself has no lexical rule for it — in every position, so the `Eq` of `Eq/subst` reads as the `Eq` of `Eq(a, b)`. Lowercase leading segments stay `@variable`, as Zed's own Rust query leaves them: Zed has no module capture.
(path (identifier) @variable)
(call function: (path (identifier) @function .))
(struct_literal type: (path (identifier) @type .))
(struct_pattern type: (path (identifier) @type .))
(struct_match_pattern type: (path (identifier) @type .))
((path (identifier) @type)
  (#match? @type "^\\p{Lu}"))

; A group's entries are names like a path's segments, under the same convention.
(use_entry name: (identifier) @variable)
((use_entry name: (identifier) @type)
  (#match? @type "^\\p{Lu}"))

; The implicit-slot mark; the witness mark is the keyword `use` above.
"@" @attribute

[
  "->"
  "=>"
  "="
  ":"
  ";"
  "|"
  ".."
  "!"
] @operator

(binary operator: _ @operator)

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
  "b["
  "x["
  "/{"
  (use_glob)
] @punctuation.bracket

"," @punctuation.delimiter
