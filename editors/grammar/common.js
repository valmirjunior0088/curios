// What the program's grammar and Core text's grammar both read.

// An identifier is a nonempty run of Unicode alphanumerics and `_` (`curios_common::is_identifier`).
const IDENTIFIER = /[\p{Alphabetic}\p{N}_]+/u;
const CONTEXTUAL_WORDS = ["concept", "satisfy", "and", "test"];
const SORT_WORDS = ["Type", "Prop", "Sort"];

// Higher binds tighter, every level left-associative (`curios_module::op_precedence`).
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

module.exports = {
  IDENTIFIER,
  CONTEXTUAL_WORDS,
  SORT_WORDS,
  PRECEDENCE,
  commaList,
};
