; Captures for the `curios` grammar in `editors/grammar`. Zed reads only this file — the grammar ships no `queries/` — so it is the single copy, and `editors/grammar`'s `npm test` compiles it against the grammar so a renamed node cannot orphan a capture silently.

(comment) @comment

; Keywords, including the contextual ones — the grammar only lexes `concept`, `satisfy` and `and` as keywords in their positions.
[
  "let"
  "rec"
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
(goal) @punctuation.special

; Definitions.
(let_item name: (identifier) @function)
(rec_binding name: (identifier) @function)
(foreign_item name: (identifier) @function)
(let_binding pattern: (identifier) @variable)
(let_binding pattern: (identifier) @function parameters: (parameters))
(mod_item name: (identifier) @type)
(induct_body name: (identifier) @type)
(struct_item name: (identifier) @type)
(concept_item name: (identifier) @type)
(satisfy_item concept: (path (identifier) @type .))
(constructor name: (identifier) @constructor)
(constructor_pattern name: (identifier) @constructor)

; Fields and labels.
(field_declaration name: (identifier) @property)
(field_definition name: (identifier) @property)
(pattern_field name: (identifier) @property)
(match_pattern_field name: (identifier) @property)
(field_identifier) @property

; Binders.
(type_parameter name: (identifier) @variable.parameter)
(index name: (identifier) @variable.parameter)
(payload name: (identifier) @variable.parameter)
(parameter pattern: (identifier) @variable.parameter)
(function_type_parameter name: (identifier) @variable.parameter)
(lambda_parameter pattern: (identifier) @variable.parameter)
(sugar_parameter name: (identifier) @variable.parameter)
(successor_pattern predecessor: (identifier) @variable)
(list_pattern head: (identifier) @variable)
(list_pattern tail: (identifier) @variable)
(packed_pattern head: (identifier) @variable)
(packed_pattern tail: (identifier) @variable)

; Paths: the last segment of a call head is a function, and a capitalized name is a type by the library's convention — the language itself has no lexical rule for it. Leading segments stay `@variable`, as Zed's own Rust query leaves them: Zed has no module capture.
(path (identifier) @variable)
(call function: (path (identifier) @function .))
(struct_literal type: (path (identifier) @type .))
(struct_pattern type: (path (identifier) @type .))
(struct_match_pattern type: (path (identifier) @type .))
((path (identifier) @type .)
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
