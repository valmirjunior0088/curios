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
        ConceptField, Field, Func, FuncParam, FuncSugarParam, FuncType, FuncTypeParam, GroupItem,
        Infix, Intrinsic, Let, LetBinding, LetGroup, LetSignature, ListEntry, ListPattern, Match,
        MatchPattern, MatchPatternField, MatrixArm, Module, Name, NatLiteral, NatPattern, NumLit,
        Pattern, PatternField, Proj, Radix, StructLit, StructLitEntry, Subterm, Syn, Term, TopCase,
        TopConcept, TopForeign, TopInduct, TopItem, TopLet, TopMod, TopStruct, TopTest, TopUse,
        TopWitness, Tuple, TupleField, TupleType, TupleTypeParam, UseGroup, WitnessEntry,
        WitnessField,
    },
    curios_abi::{WireLeaf, WireResults, WireSignature, WireType},
    curios_num::{Floating, Natural},
    curios_parse::{
        Mark, Parser, catch, commit, fail, lazy, many0, many1, mark, memoize, not_ahead,
        preceded_by_space, pure, sep_by0_trailing, sep_by1_trailing, spanned, take_exact, take_n,
        take_while,
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

/// The recorded comments in ascending offset order, leaving the table empty.
pub(crate) fn take_comments() -> Vec<Span> {
    COMMENTS.with(|comments| {
        std::mem::take(&mut *comments.borrow_mut())
            .into_values()
            .collect()
    })
}

/// The formatter's optional tail: a whole term when one follows, `None` at end of input. `catch` downgrades a mid-term failure so the alternative backtracks — a garbled tail then surfaces as the entry's end-of-input error at the right position.
pub(crate) fn parse_optional_term<'a>() -> Parser<'a, Option<Term>> {
    catch(lazy(parse_term)).map(Some).or(pure(None))
}

pub(crate) fn parse_whitespace<'a>() -> Parser<'a, ()> {
    // A `many0` loop over comment-then-whitespace runs, not recursion per comment line: an N-line comment banner used to nest N native frames.
    take_while(|char| char.is_whitespace())
        .and(many0(|| {
            catch(
                // The span covers `--` through the end of the line, newline excluded. Recording is sound here because this parser never runs inside a string or character literal — literal interiors are consumed atomically by their own parsers — so every recorded span is a genuine comment of the winning parse.
                spanned(take_exact("--").and_keep(take_while(|char| char != '\n')))
                    .map(|(span, _)| record_comment(span))
                    .and_drop(take_while(|char| char.is_whitespace())),
            )
        }))
        .map(|_| ())
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
fn parse_keyword<'a>(expected: &'static str) -> Parser<'a, ()> {
    parse_identifier_raw()
        .flat_map(move |obtained| match expected == obtained {
            true => pure(()),
            false => fail(format!(
                "Expected keyword '{expected}', obtained '{obtained}'"
            )),
        })
        .and_drop(parse_whitespace())
}
