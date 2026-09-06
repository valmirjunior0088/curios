mod literals;
use literals::*;

mod tuples;
use tuples::*;

mod patterns;
use patterns::*;

mod match_expr;
use match_expr::*;

mod expr;
use expr::*;
pub(crate) use expr::{op_precedence, parse_term};

mod top_level;
pub(crate) use top_level::parse_top_item;

#[cfg(test)]
mod concept_tests;
#[cfg(test)]
mod expr_tests;
#[cfg(test)]
mod grammar_tests;
#[cfg(test)]
mod literals_tests;
#[cfg(test)]
mod match_expr_tests;
#[cfg(test)]
mod module_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod top_level_tests;
#[cfg(test)]
mod tuples_tests;

use {
    super::{
        Apply, Argument, BinPattern, BinSegment, CasePayloadParam, Choose, ChooseArm, ChooseTest,
        ConceptField, Doc, Field, Func, FuncParam, FuncSugarParam, FuncType, FuncTypeParam,
        GroupItem, Infix, Intrinsic, Label, Let, LetBinding, LetGroup, LetSignature, ListEntry,
        ListPattern, Match, MatchPattern, MatchPatternField, MatrixArm, Module, Name, NatLiteral,
        NatPattern, NumLit, Pattern, PatternField, Proj, Radix, StrLit, StructField, StructLit,
        StructLitEntry, Subterm, Syn, Term, TopCase, TopConcept, TopForeign, TopInduct, TopItem,
        TopLet, TopMod, TopStruct, TopTest, TopUse, TopWitness, Tuple, TupleField, TupleType,
        TupleTypeParam, UseGroup, WitnessEntry, WitnessField,
    },
    curios_abi::{WireLeaf, WireResults, WireSignature, WireType},
    curios_num::{Floating, Natural},
    curios_parse::{
        Mark, Parser, catch, commit, fail, fail_from, lazy, look_ahead, many0, many1, mark,
        memoize, not_ahead, preceded_by_space, pure, sep_by0_trailing, sep_by1_trailing, spanned,
        take_eof, take_exact, take_n, take_while,
    },
    curios_utilities::{
        Grain, InfixOp, Plicity, Qualifier, Sign, Span, is_identifier_char, is_keyword,
    },
    std::{cell::RefCell, collections::BTreeMap, iter},
};

// Grammar keys for the packrat cache (see `curios_parse::memoize`). One block here rather than one per grammar file, because the table those keys index is shared: two nonterminals that pick the same number serve each other's parses at that offset, silently whenever their outputs share a type, and a key minted by reading a sibling file is a key minted from memory. A new memoized nonterminal takes the next number in this block.
//
// Only the nonterminals that overlapping alternatives re-probe at the same offset are memoized; that is enough to keep parsing linear. `parse_pattern`/`parse_match_pattern` qualify because each has its own `(...)`-grouping alternative and is re-probed by every caller that speculatively tries a lambda/match-arm shape (`parse_func`'s parameter list, `parse_ctor_match_pattern`'s argument list, …) — without them, a run of nested parens is exponential: each candidate caller re-walks the whole remaining nesting fresh.
const MEMO_TERM: u32 = 0;
const MEMO_ATOMIC_TERM: u32 = 1;
const MEMO_PATTERN: u32 = 2;
const MEMO_MATCH_PATTERN: u32 = 3;

thread_local! {
    /// Every comment the current parse run has consumed, keyed by start offset — recorded by [`parse_whitespace`], the single place comments die, and drained by the `parse_with_comments` entries. The packrat-memo pattern: per-thread and cleared per run, so runs never see each other's comments. Offset keying makes re-recording under backtracking idempotent, and memoized jumps that skip re-running whitespace are harmless because the cache-miss run already recorded.
    static COMMENTS: RefCell<BTreeMap<usize, Span>> = const { RefCell::new(BTreeMap::new()) };
}

fn record_comment(span: Span) {
    COMMENTS.with(|comments| comments.borrow_mut().insert(span.start, span));
}

pub(crate) fn clear_comments() {
    COMMENTS.with(|comments| comments.borrow_mut().clear());
}

/// The recorded comment that ends exactly at `end`, if one does — what lets a span step back over a trailing comment it consumed.
fn comment_ending_at(end: usize) -> Option<Span> {
    COMMENTS.with(|comments| {
        comments
            .borrow()
            .range(..end)
            .next_back()
            .map(|(_, span)| span.clone())
            .filter(|span| span.end == end)
    })
}

/// The recorded comments in ascending offset order, leaving the table empty.
pub(crate) fn take_comments() -> Vec<Span> {
    COMMENTS.with(|comments| {
        std::mem::take(&mut *comments.borrow_mut())
            .into_values()
            .collect()
    })
}

/// The formatter's optional tail: a whole term when one follows, `None` at end of input. The term comes first so that a tail garbled past its first token commits under [`Parser::or`]'s progress rule and is refused with the term's own message at its own position — what the compiler reports for the same program. A module file's items end at end of input, where the term fails without progress, and the alternative answers `None`.
pub(crate) fn parse_optional_term<'a>() -> Parser<'a, Option<Term>> {
    lazy(parse_term).map(Some).or(take_eof().map(|()| None))
}

/// What refuses a `--` glued to what follows it: the plain comment is `-- `, with the space, or a bare `--` ending its line.
const COMMENT_SPACING: &str = "a comment opens with `-- `, with the space, or ends its line";

/// What refuses a `-- |` glued to what follows it.
const DOC_SPACING: &str =
    "a documentation comment opens with `-- | `, with the space, or ends its line";

/// What refuses a `-- |` after code on its line.
const DOC_TRAILING: &str =
    "a documentation comment takes a line of its own; `-- |` cannot follow code";

/// What refuses two `-- |` blocks with a blank line or a plain comment between them and nothing declared in between.
const DOC_TWICE: &str = "two documentation comments precede one declaration; join them, since an empty `-- |` line is a paragraph break";

/// What refuses a `-- |` block followed by anything but what it may document.
pub(crate) const DOC_BEFORE_NOTHING: &str = "a documentation comment `-- |` must immediately precede what it documents: a declaration, a constructor, a field or a concept method";

pub(crate) fn parse_whitespace<'a>() -> Parser<'a, ()> {
    // A `many0` loop over comment-then-whitespace runs, not recursion per comment line: an N-line comment banner used to nest N native frames.
    take_while(|char| char.is_whitespace())
        .and(many0(|| {
            // The head is recoverable, because the absence of a comment is how the loop ends and a `-- |` is not a comment but the documentation syntax the caller reads next. Everything after the head is not: a `--` glued to a word is a mistake nothing else can diagnose.
            //
            // The span covers `--` through the end of the line, newline excluded. Recording is sound here because this parser never runs inside a string or character literal — literal interiors are consumed atomically by their own parsers — so every recorded span is a genuine comment of the winning parse.
            spanned(
                catch(take_exact("--").and_drop(not_ahead(" |")))
                    .and_drop(comment_spacing(COMMENT_SPACING))
                    .and_drop(take_while(|char| char != '\n')),
            )
            .map(|(span, _)| record_comment(span))
            .and_drop(take_while(|char| char.is_whitespace()))
        }))
        .map(|_| ())
}

/// After a comment's opener: a space, consumed, or the line's end, left alone — and anything else refused with `message`, fatally, since the opener was consumed.
fn comment_spacing<'a>(message: &'static str) -> Parser<'a, ()> {
    // A zero-width span: the one way to read the source at the current offset through the combinators.
    spanned(pure(())).flat_map(move |(here, ())| {
        match here.source.text[here.start..].chars().next() {
            Some(' ') => take_exact(" "),
            None | Some('\n') | Some('\r') => pure(()),
            Some(_) => fail(message),
        }
    })
}

/// One line of a documentation comment: `-- |` at the start of its line, its separator, the text to the line's end, and the line break with the indentation of the next line — so the next `-- |` is read as the same block only when it is on the very next line.
fn parse_doc_line<'a>() -> Parser<'a, String> {
    spanned(take_exact("-- |"))
        .flat_map(|(span, ())| {
            let text = &span.source.text;
            let line_start = text[..span.start].rfind('\n').map_or(0, |index| index + 1);
            match text[line_start..span.start]
                .chars()
                .all(char::is_whitespace)
            {
                true => pure(()),
                false => fail(DOC_TRAILING),
            }
        })
        .and_drop(comment_spacing(DOC_SPACING))
        .and_keep(
            take_while(|char| char != '\n').map(|line| line.trim_end_matches('\r').to_string()),
        )
        .and_drop(catch(take_exact("\n")).or(take_eof()))
        .and_drop(take_while(|char| char == ' ' || char == '\t'))
}

/// The source from the head a documentation block runs up to.
pub(crate) fn text_after(doc: &Doc) -> &str {
    match &doc.span {
        Some(span) => &span.source.text[span.end..],
        None => "",
    }
}

/// The keyword at the head a documentation block runs up to, past a `pub` if one is written, which decides whom the block belongs to: `and` opens a later member of the group being parsed, another word opens the next item, and anything else — a closing token, `end`, the end of input — is nothing the block may document.
pub(crate) fn word_after(doc: &Doc) -> &str {
    let rest = text_after(doc);
    let (first, after) = word_at(rest);
    match first {
        "pub" => word_at(after.trim_start()).0,
        other => other,
    }
}

/// The identifier `text` begins with, and the text after it.
fn word_at(text: &str) -> (&str, &str) {
    let length = text
        .chars()
        .take_while(|char| is_identifier_char(*char))
        .map(char::len_utf8)
        .sum();

    text.split_at(length)
}

/// The documentation comment above what comes next, or `None` when there is none: consecutive `-- |` lines, then the whitespace and plain comments up to the documented head. A second block before that head is refused, so a stray block far above can never be silently absorbed into a declaration's prose.
///
/// The head itself is not parsed here. Every caller reads it next and decides what a block may precede, committing the failure when there was a block: a documentation comment followed by nothing it may document is the diagnosis, never a reason to backtrack.
pub(crate) fn parse_doc<'a>() -> Parser<'a, Option<Doc>> {
    mark()
        .and(many0(parse_doc_line))
        .flat_map(|(start, lines)| match lines.is_empty() {
            true => pure(None),
            false => parse_whitespace()
                .and_keep(not_ahead("-- |").map_err(DOC_TWICE))
                .and_keep(mark())
                .map(move |end| {
                    Some(Doc {
                        lines,
                        span: Some(start.to(&end)),
                    })
                }),
        })
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

// The identifier characters alone, consuming no whitespace — the building block of the tight (whitespace-free) positions like a `Bits`/`Bytes` literal's `\..` spread operand.
fn parse_identifier_raw<'a>() -> Parser<'a, &'a str> {
    take_while(is_identifier_char).flat_map(|identifier| match identifier.is_empty() {
        true => fail("Expected identifier"),
        false => pure(identifier),
    })
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    parse_identifier_raw().and_drop(parse_whitespace())
}

// An identifier at a declaring position, carrying the span of the word alone: the trailing whitespace is consumed after the span closes, so a report about the declaration underlines the name and nothing after it.
fn parse_label<'a>() -> Parser<'a, Label> {
    spanned(parse_identifier_raw())
        .map(|(span, text)| Label::spanned(text, span))
        .and_drop(parse_whitespace())
}

fn name_from_segments<'a>(is_abs: bool, segments: Vec<String>) -> Parser<'a, Name> {
    match segments.iter().any(|segment| is_keyword(segment)) {
        true => fail(format!(
            "path '{}' contains a reserved keyword",
            segments.join("/")
        )),
        false => pure(Name::new(is_abs, Qualifier::from(segments))),
    }
}

fn parse_name<'a>() -> Parser<'a, Name> {
    // A path is whitespace-free: every separator touches both of its neighbors. That tightness is the whole disambiguation against division — the operator grammar requires whitespace on both sides of `/` (`parse_infix_op`), so `a/b` is only ever a path and `a / b` only ever a division, and the asymmetric spellings satisfy neither grammar. Trailing whitespace is consumed once, after the whole name, keeping the span tight.
    spanned(
        catch(take_exact("/"))
            .map(|()| true)
            .or(pure(false))
            .and(parse_identifier_raw().and(many0(|| {
                catch(take_exact("/").and_keep(parse_identifier_raw()))
            })))
            .flat_map(|(is_abs, (first, rest))| {
                let segments = iter::once(first)
                    .chain(rest)
                    .map(str::to_string)
                    .collect::<Vec<_>>();

                name_from_segments(is_abs, segments)
            }),
    )
    .map(|(span, name)| name.with_span(span))
    .and_drop(parse_whitespace())
}

fn parse_qualified_name<'a>() -> Parser<'a, Name> {
    catch(parse_name().flat_map(|name| match name.is_single() {
        true => fail("expected a qualified path"),
        false => pure(name),
    }))
}

// The word is read *raw* and the whitespace after it consumed only once it matched, so a mismatch is reported against the word rather than wherever that whitespace ended — which for a line-final keyword is the next line, or past the end of the file. `end` and `and` are habitually written line-final, so a misspelled one used to put its caret on the innocent declaration below it. `parse_top_item` reads its head raw for the same reason.
//
// Commitment is unchanged: `parse_identifier_raw` rejects an empty run, so a mismatch has consumed at least one character and stays fatal past the choice point, while the empty case fails *at* the choice point either way.
//
// Where no word begins at all — a `;` or a `/` standing where `end` belongs — the word run is empty and the report names the keyword and the character found, in `take_exact`'s style. Left to `parse_identifier_raw`, the report was its bare `Expected identifier`, which named neither. The empty run consumes nothing, so the failure stays recoverable at the choice point exactly as the identifier parser's did.
fn parse_keyword<'a>(expected: &'static str) -> Parser<'a, ()> {
    mark()
        .and(take_while(is_identifier_char))
        .and(look_ahead(take_while(|char| !char.is_whitespace())))
        .flat_map(
            move |((start, obtained), rest)| match (obtained, expected == obtained) {
                (_, true) => pure(()),
                ("", false) => fail(match rest.chars().next() {
                    Some(next) => format!("Expected keyword '{expected}', obtained '{next}'"),
                    None => format!("Expected keyword '{expected}', obtained 'end-of-file'"),
                }),
                // The report spans the word, so the caret underlines it rather than standing after it.
                (obtained, false) => fail_from(
                    &start,
                    format!("Expected keyword '{expected}', obtained '{obtained}'"),
                ),
            },
        )
        .and_drop(parse_whitespace())
}
