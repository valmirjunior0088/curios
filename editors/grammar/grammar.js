/// <reference types="tree-sitter-cli/dsl" />

// The surface grammar of `documentation/syntax.md`, mirrored from `curios-text/src/parse/*.rs`. It exists for highlighting, so it is deliberately looser than the compiler where looseness costs nothing a highlighter would notice: it does not enforce whitespace around infix operators, the six wire types of a `foreign` signature, or a `choose` bind arm's refutability.

// An identifier is a nonempty run of Unicode alphanumerics and `_` (`curios_utilities::is_identifier`).
const IDENTIFIER = /[\p{Alphabetic}\p{N}_]+/;

// Higher binds tighter, every level left-associative (`curios_text::op_precedence`).
const PRECEDENCE = {
  or: 1,
  and: 2,
  compare: 3,
  add: 4,
  multiply: 5,
  suffix: 6,
};

const separated = (rule, separator) => seq(rule, repeat(seq(separator, rule)));

// Every comma-separated list admits one trailing comma before its closing delimiter.
const commaList = (rule) => optional(seq(separated(rule, ","), optional(",")));

module.exports = grammar({
  name: "curios",

  word: ($) => $.identifier,

  extras: ($) => [/\s/, $.comment],

  conflicts: ($) => [
    [$.lambda_parameters, $.unit],
    [$.struct_literal, $.struct_pattern],
    [$.lambda_parameter, $.parenthesized_pattern],
    [$.lambda_parameter, $.pattern_field],
    [$.rec_item, $.rec_term],
    [$.function_type_parameter, $.parenthesized],
    [$.function_type_parameter, $.tuple_field],
    [$.field_definition, $.path],
    [$.path, $._pattern],
    [$.function_type_parameter, $._pattern],
    [$.field_declaration, $.path],
    [$.let_item, $._pattern],
    [$._atom, $.struct_literal],
  ],

  rules: {
    // An entrypoint is items then one final term; a module file is items only.
    source_file: ($) => seq(repeat($._item), optional($._term)),

    comment: (_) => token(seq("--", /.*/)),

    // ---- Items ----

    _item: ($) =>
      choice(
        $.let_item,
        $.rec_item,
        $.mod_item,
        $.use_item,
        $.induct_item,
        $.struct_item,
        $.concept_item,
        $.satisfy_item,
        $.foreign_item,
      ),

    // An annotated top-level `let` is an item; an unannotated one is a local `let` opening the final term (`let_term`). The dynamic precedence settles the case both grammars accept — an annotated binding followed by a term — the way the compiler does, as an item.
    let_item: ($) =>
      prec.dynamic(
        1,
        seq(optional("pub"), "let", field("name", $.identifier), $._signature, ";"),
      ),

    rec_item: ($) =>
      prec.dynamic(
        1,
        seq(
          optional("pub"),
          "rec",
          $.rec_binding,
          repeat(seq(optional("pub"), "and", $.rec_binding)),
          ";",
        ),
      ),

    rec_binding: ($) => seq(field("name", $.identifier), $._signature),

    // `(params) -> type = body` or `: type = body`.
    _signature: ($) =>
      choice(
        seq(
          field("parameters", $.parameters),
          "->",
          field("type", $._term),
          "=",
          field("body", $._term),
        ),
        seq(":", field("type", $._term), "=", field("body", $._term)),
      ),

    mod_item: ($) =>
      seq(
        optional("pub"),
        "mod",
        field("name", $.identifier),
        choice(";", seq(repeat($._item), "end")),
      ),

    use_item: ($) =>
      seq(
        optional("pub"),
        "use",
        optional(field("path", $.path)),
        choice($.use_group, $.use_glob),
        ";",
      ),

    // `/{` and `/*` are single tokens: after a path, a lone `/` would be read as one more segment.
    use_group: ($) => seq("/{", commaList($.use_entry), "}"),

    use_glob: (_) => "/*",

    use_entry: ($) => seq(optional(choice("mod", "let")), field("name", $.identifier)),

    induct_item: ($) =>
      seq(
        optional("pub"),
        "induct",
        $.induct_body,
        repeat(seq(optional("pub"), "and", $.induct_body)),
        "end",
      ),

    induct_body: ($) =>
      seq(
        field("name", $.identifier),
        optional(field("parameters", $.type_parameters)),
        ":",
        $._arity,
        repeat($.constructor),
      ),

    type_parameters: ($) => seq("(", commaList($.type_parameter), ")"),

    type_parameter: ($) =>
      seq(optional("@"), field("name", $.identifier), ":", field("type", $._term)),

    // `(indices) -> Sort` or a bare `Sort`, each with its own representation `pub`.
    _arity: ($) =>
      choice(
        seq("(", commaList($.index), ")", "->", optional("pub"), $.sort),
        seq(optional("pub"), $.sort),
      ),

    index: ($) =>
      choice(seq(field("name", $.identifier), ":", field("type", $._term)), field("type", $._term)),

    constructor: ($) =>
      seq(
        "|",
        field("name", $.identifier),
        "(",
        commaList($.payload),
        ")",
        optional(seq(":", "(", commaList($._term), ")")),
      ),

    payload: ($) =>
      choice(
        seq(optional("@"), field("name", $.identifier), ":", field("type", $._term)),
        field("type", $._term),
      ),

    struct_item: ($) =>
      seq(
        optional("pub"),
        "struct",
        field("name", $.identifier),
        optional(field("parameters", $.type_parameters)),
        ":",
        optional("pub"),
        $.sort,
        "{",
        commaList($.field_declaration),
        "}",
      ),

    // A tuple-type or struct field: `label: T`, the signature sugar `label(params) -> T`, or a bare type.
    field_declaration: ($) =>
      choice(
        seq(
          field("name", $.identifier),
          field("parameters", $.function_type_parameters),
          "->",
          field("type", $._term),
        ),
        seq(field("name", $.identifier), ":", field("type", $._term)),
        field("type", $._term),
      ),

    concept_item: ($) =>
      seq(
        optional("pub"),
        "concept",
        field("name", $.identifier),
        optional(field("parameters", $.type_parameters)),
        ":",
        optional("pub"),
        $.sort,
        "{",
        commaList($.concept_field),
        "}",
      ),

    concept_field: ($) => choice(seq("use", field("type", $._term)), $.field_declaration),

    satisfy_item: ($) =>
      seq(
        "satisfy",
        optional(seq(field("parameters", $.parameters), "=>")),
        field("concept", $.path),
        optional(field("arguments", $.type_arguments)),
        "{",
        commaList($.witness_entry),
        "}",
      ),

    type_arguments: ($) => seq("(", commaList($._term), ")"),

    witness_entry: ($) => choice(seq("use", $._term), $.field_definition),

    // `label = value` or the definition sugar `label(params) = value`.
    field_definition: ($) =>
      seq(
        field("name", $.identifier),
        optional(field("parameters", $.sugar_parameters)),
        "=",
        field("value", $._term),
      ),

    sugar_parameters: ($) => seq("(", commaList($.sugar_parameter), ")"),

    sugar_parameter: ($) =>
      seq(
        optional(choice("@", "use")),
        field("name", $.identifier),
        optional(seq(":", field("type", $._term))),
      ),

    foreign_item: ($) =>
      seq(
        optional("pub"),
        "foreign",
        field("name", $.identifier),
        ":",
        field("type", $._term),
        ";",
      ),

    // ---- Telescopes ----

    // A `let`/`rec`/`satisfy` telescope: every parameter annotated, `use` ones anonymous.
    parameters: ($) => seq("(", commaList($.parameter), ")"),

    parameter: ($) =>
      choice(
        seq("use", field("type", $._term)),
        seq(optional("@"), field("pattern", $._pattern), ":", field("type", $._term)),
      ),

    function_type_parameters: ($) => seq("(", commaList($.function_type_parameter), ")"),

    function_type_parameter: ($) =>
      choice(
        seq("use", field("type", $._term)),
        seq(optional("@"), field("name", $.identifier), ":", field("type", $._term)),
        seq(optional("@"), field("type", $._term)),
      ),

    lambda_parameters: ($) => seq("(", commaList($.lambda_parameter), ")"),

    lambda_parameter: ($) =>
      seq(
        optional(choice("@", "use")),
        field("pattern", $._pattern),
        optional(seq(":", field("type", $._term))),
      ),

    // ---- Terms ----

    _term: ($) =>
      choice(
        $.rec_term,
        $.let_term,
        $.match_term,
        $.choose_term,
        $.function_type,
        $.lambda,
        $._infix,
      ),

    rec_term: ($) => seq("rec", separated($.rec_binding, "and"), ";", field("body", $._term)),

    let_term: ($) => seq($.let_binding, field("body", $._term)),

    let_binding: ($) =>
      seq(
        "let",
        field("pattern", $._pattern),
        choice(
          seq(
            field("parameters", $.parameters),
            "->",
            field("type", $._term),
            "=",
            field("value", $._term),
          ),
          seq(optional(seq(":", field("type", $._term))), "=", field("value", $._term)),
        ),
        ";",
      ),

    match_term: ($) =>
      seq(
        "match",
        field("scrutinee", $._term),
        optional(seq(":", field("motive", $._term))),
        repeat($.match_arm),
        "end",
      ),

    match_arm: ($) =>
      seq("|", field("pattern", $._match_pattern), "=>", field("body", $._term)),

    choose_term: ($) => seq("choose", repeat($.choose_arm), "end"),

    // A bind arm's pattern is read as a term: every refutable pattern shape is also a term shape, and a highlighter has no need to tell them apart.
    choose_arm: ($) =>
      seq(
        "|",
        field("test", $._term),
        optional(seq("=", field("value", $._term))),
        "=>",
        field("body", $._term),
      ),

    function_type: ($) =>
      seq(field("parameters", $.function_type_parameters), "->", field("result", $._term)),

    lambda: ($) => seq(field("parameters", $.lambda_parameters), "=>", field("body", $._term)),

    _infix: ($) => choice($.binary, $._applied),

    binary: ($) => {
      const table = [
        [PRECEDENCE.or, "||"],
        [PRECEDENCE.and, "&&"],
        [PRECEDENCE.compare, choice("==", "!=", "<", ">", "<=", ">=")],
        [PRECEDENCE.add, choice("+", "-")],
        [PRECEDENCE.multiply, choice("*", "/", "%")],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(field("left", $._infix), field("operator", operator), field("right", $._infix)),
          ),
        ),
      );
    },

    _applied: ($) => choice($._atom, $.call, $.projection, $.bang),

    call: ($) =>
      prec.left(PRECEDENCE.suffix, seq(field("function", $._applied), field("arguments", $.arguments))),

    arguments: ($) => seq("(", commaList($.argument), ")"),

    argument: ($) => seq(optional(choice("@", "use")), $._term),

    projection: ($) =>
      prec.left(
        PRECEDENCE.suffix,
        seq(
          field("value", $._applied),
          token.immediate("."),
          field("field", alias(token.immediate(choice(/[0-9]+/, IDENTIFIER)), $.field_identifier)),
        ),
      ),

    bang: ($) => prec.left(PRECEDENCE.suffix, seq(field("action", $._applied), "!")),

    _atom: ($) =>
      choice(
        $.goal,
        $.struct_literal,
        $.path,
        $.sort,
        $.boolean,
        $.number,
        $.character,
        $.string,
        $.packed_literal,
        $.list_literal,
        $.tuple_type,
        $.unit,
        $.tuple,
        $.parenthesized,
      ),

    goal: (_) => "?",

    sort: (_) => choice("Type", "Prop"),

    boolean: (_) => choice("true", "false"),

    // A path is whitespace-free: the segments after the first are immediate tokens.
    path: ($) =>
      seq(
        choice(seq("/", alias(token.immediate(IDENTIFIER), $.identifier)), $.identifier),
        repeat(seq(token.immediate("/"), alias(token.immediate(IDENTIFIER), $.identifier))),
      ),

    struct_literal: ($) =>
      seq(
        field("type", $.path),
        optional(field("arguments", $.type_arguments)),
        "{",
        commaList($.struct_entry),
        "}",
      ),

    struct_entry: ($) =>
      choice($.spread, seq("use", $._term), $.field_definition, $._term),

    spread: ($) => seq("..", $._term),

    list_literal: ($) => seq("[", commaList(choice($.spread, $._term)), "]"),

    // The grain letter is glued to the bracket: `b[` and `x[` are single tokens, so `b [1]` stays the binder `b` before a list.
    packed_literal: ($) =>
      seq(field("grain", choice("b[", "x[")), commaList(choice($.spread, $._term)), "]"),

    tuple_type: ($) => seq("{", commaList($.field_declaration), "}"),

    unit: (_) => seq("(", ")"),

    // A first field followed by a comma, or a single labeled field, is what makes a tuple rather than a parenthesized term.
    tuple: ($) =>
      seq(
        "(",
        choice(
          seq($.tuple_field, ",", commaList($.tuple_field)),
          $.field_definition,
        ),
        ")",
      ),

    tuple_field: ($) => choice($.field_definition, $._term),

    parenthesized: ($) => seq("(", $._term, ")"),

    // ---- Literals ----

    // The sign is glued to the digits: `-42` is one literal, `- 42` a subtraction.
    number: (_) =>
      token(
        choice(
          /[+-]?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?/,
          /[+-]?0x[0-9a-fA-F]+/,
          /[+-]?0b[01]+/,
          /[+-]?[0-9]+/,
        ),
      ),

    character: (_) => token(seq("'", choice(/[^'\\]/, /\\[ntr\\']/), "'")),

    // An unrecognized escape stands for itself, so only the five recognized ones are named.
    string: ($) =>
      seq(
        '"',
        repeat(
          choice(
            $.escape_sequence,
            token.immediate(/[^"\\]+/),
            token.immediate(/\\./),
          ),
        ),
        token.immediate('"'),
      ),

    escape_sequence: (_) => token.immediate(/\\[ntr\\"]/),

    // ---- Irrefutable patterns ----

    _pattern: ($) =>
      choice($.identifier, $.tuple_pattern, $.struct_pattern, $.parenthesized_pattern),

    tuple_pattern: ($) =>
      seq(
        "(",
        choice(
          seq($.pattern_field, ",", commaList($.pattern_field)),
          seq(field("name", $.identifier), "=", $._pattern),
        ),
        ")",
      ),

    pattern_field: ($) =>
      choice(seq(field("name", $.identifier), "=", $._pattern), $._pattern),

    struct_pattern: ($) =>
      seq(field("type", $.path), "{", commaList($.pattern_field), "}"),

    parenthesized_pattern: ($) => seq("(", $._pattern, ")"),

    // ---- Refutable patterns ----

    _match_pattern: ($) =>
      choice(
        $.constructor_pattern,
        $.boolean,
        $.successor_pattern,
        $.number,
        $.list_pattern,
        $.packed_pattern,
        $.tuple_match_pattern,
        $.struct_match_pattern,
        $.parenthesized_match_pattern,
        $.identifier,
      ),

    constructor_pattern: ($) =>
      seq(field("name", $.identifier), "(", commaList($.constructor_argument), ")"),

    constructor_argument: ($) => seq(optional("@"), $._match_pattern),

    // `pred + 1`, with an optional `; hypothesis` receiving the fold result.
    successor_pattern: ($) =>
      seq(field("predecessor", $.identifier), "+", $.number, optional($.hypothesis)),

    hypothesis: ($) => seq(";", $._pattern),

    list_pattern: ($) =>
      choice(
        seq("[", "]"),
        seq(
          "[",
          field("head", $.identifier),
          ",",
          "..",
          field("tail", $.identifier),
          "]",
          optional($.hypothesis),
        ),
      ),

    packed_pattern: ($) =>
      choice(
        seq(field("grain", choice("b[", "x[")), "]"),
        seq(
          field("grain", choice("b[", "x[")),
          field("head", $.identifier),
          ",",
          "..",
          field("tail", $.identifier),
          "]",
          optional($.hypothesis),
        ),
      ),

    tuple_match_pattern: ($) =>
      seq(
        "(",
        choice(
          seq($.match_pattern_field, ",", commaList($.match_pattern_field)),
          seq(field("name", $.identifier), "=", $._match_pattern),
        ),
        ")",
      ),

    match_pattern_field: ($) =>
      choice(seq(field("name", $.identifier), "=", $._match_pattern), $._match_pattern),

    struct_match_pattern: ($) =>
      seq(field("type", $.path), "{", commaList($.match_pattern_field), "}"),

    parenthesized_match_pattern: ($) => seq("(", $._match_pattern, ")"),

    identifier: (_) => IDENTIFIER,
  },
});
