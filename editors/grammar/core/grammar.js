/// <reference types="tree-sitter-cli/dsl" />

// Core text, the `.crsc` language: Core in the program's grammar with nothing left to an elaborator (`curios-surface`'s README), mirrored from `curios-surface/src/parse/core_text.rs`. It is the program's grammar with what only Core text writes — a decorator, an intrinsic, a witness by its identity, a declaration by its absolute name, items joined into groups whatever their kinds, and the type the entry states — as the compiler reads both by one grammar under a dialect. Looser than the compiler where a highlighter would not notice, as the program's grammar is.

const curios = require("../base/grammar");
const { IDENTIFIER, commaList } = require("../common");

// A witness's name is one token: its module's path, `witness~`, and its identity — the concept and the key's heads, with its place where it is not the first at them, `/m/witness~/C(K)#1`, or its place alone, `/m/witness~3`.
const SEGMENT = IDENTIFIER.source;
const WITNESS = new RegExp(
  `/(${SEGMENT}/)*witness~([0-9]+|/${SEGMENT}(/${SEGMENT})*\\([^)]*\\)(#[0-9]+)?)`,
  "u",
);

module.exports = grammar(curios, {
  name: "curios_core",

  conflicts: ($, previous) =>
    previous.concat([
      [$.decorator_struct, $.tuple_type],
      // `test` or `concept` before a name is a form, and before a path's next segment the path's head: the token after the name decides.
      [$.test_item, $._path_head],
      [$._former, $._path_head],
      // A head's `(…)` is its indices where an arrow follows, and otherwise the sort it lands in.
      [$.index, $.parenthesized],
      // A decorator after a member opens the group's next one where `and` follows it, and the next item or the entry where anything else does; an `and` there is the group's, never a path the entry begins with.
      [$.group],
    ]),

  rules: {
    // Items, then the term the text closes with and the type it states on the line beneath it.
    source_file: ($) => seq(repeat($._core_item), optional($.entry)),

    entry: ($) =>
      seq(field("body", $._term), optional(seq(":", field("type", $._term)))),

    // A program's `mod`, `use` and `satisfy` are no Core text, read here so every capture of the program's highlight query names a node of this grammar too, and Core text's query opens with that one verbatim.
    _core_item: ($) =>
      choice(
        $.test_item,
        $.foreign_item,
        $.group,
        $.mod_item,
        $.use_item,
        $.satisfy_item,
      ),

    // Declarations of any kinds joined by `and`, each written as its own keyword writes it — a `let` member after `and` by its name alone — closed by the `;` a `let` owes where its last member is one. A decorator stands before a member, and before the `and` that opens one.
    group: ($) =>
      seq(
        optional($.decorator),
        $._group_head,
        repeat(seq(optional($.decorator), "and", $._group_member)),
        optional(";"),
      ),

    _group_head: ($) => choice(seq("let", $.let_member), $._former),

    _group_member: ($) => choice($.let_member, $._former),

    _former: ($) =>
      choice(
        seq("induct", $.induct_body, "end"),
        seq("struct", $.struct_member),
        seq("concept", $.concept_member),
      ),

    test_item: ($, original) => seq(optional($.decorator), original),

    foreign_item: ($, original) => seq(optional($.decorator), original),

    constructor: ($, original) => seq(optional($.decorator), original),

    // A declaration is named by its absolute path, a witness by its identity; a local `and` member keeps its word.
    _declared_name: ($) => choice($.path, $.witness_name, $._name),

    // The sort a head lands in, under its decorator, or a term between parentheses.
    _result_sort: ($) =>
      choice($.sort, seq($.decorator, $.sort), $.parenthesized),

    // A `use` binder's word, where something reads it.
    concept_field: ($, original) =>
      choice(
        seq("use", field("name", $._name), ":", field("type", $._term)),
        original,
      ),

    function_type_parameter: ($, original) =>
      choice(
        seq("use", field("name", $._name), ":", field("type", $._term)),
        original,
      ),

    type_parameter: ($, original) =>
      choice(
        seq("use", field("name", $._name), ":", field("type", $._term)),
        original,
      ),

    // An intrinsic's operand may be a structure value, the universe instance a cell or a channel is held at.
    argument: ($, original) => choice(original, $.decorator_struct),

    _applied: ($, original) => choice(original, $.decorated),

    _atom: ($, original) => choice(original, $.intrinsic, $.witness_name),

    // A decorator states the fields of the node its operand builds, so a call's arguments are its operand's, not a node of their own: the operand reads on through every suffix.
    decorated: ($) =>
      prec.right(
        seq($.decorator, field("operand", choice($.held, $._applied))),
      ),

    decorator: ($) => seq("#{", commaList($.decorator_field), "}"),

    decorator_field: ($) =>
      seq(
        field("name", $.identifier),
        "=",
        field("value", choice($.decorator_struct, $.root, $._term)),
      ),

    // The root module, which a declaration's island or a nominal's module is where it stands at the top.
    root: (_) => "/",

    // A struct-valued field's value: the fields of it that differ from its template's.
    decorator_struct: ($) => seq("{", commaList($.decorator_field), "}"),

    // `#` and a Core variant's name, `#NatType`; its operands are a call's arguments.
    intrinsic: (_) => token(seq("#", IDENTIFIER)),

    witness_name: (_) => WITNESS,
  },
});
