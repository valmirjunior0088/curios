mod literals;
use literals::*;

mod tuples;
use tuples::*;

mod patterns;
use patterns::*;

mod match_expr;
use match_expr::*;

mod expr;
pub(crate) use expr::parse_term;
use expr::*;

mod top_level;
pub(crate) use top_level::parse_top_item;

#[cfg(test)]
mod tests;

use {
    super::{
        Apply, BinPattern, BinSegment, CasePayloadParam, Choose, ChooseArm, ChooseTest,
        ConceptField, Field, Func, FuncParam, FuncSugarParam, FuncType, FuncTypeParam, GroupItem,
        Infix, Let, LetBinding, LetSignature, LstEntry, LstPattern, Match, MatchPattern,
        MatchPatternField, MatrixArm, Module, Motive, Name, NatLiteral, NatPattern, NumLit,
        Pattern, PatternField, Prim, Proj, Radix, Rec, RecItem, StructLit, StructLitEntry, Subterm,
        Syn, Term, TopCase, TopConcept, TopForeign, TopInduct, TopItem, TopLet, TopMod, TopStruct,
        TopUse, TopWitness, Tuple, TupleField, TupleType, TupleTypeParam, UseGroup, WitnessEntry,
        WitnessField,
    },
    curios_abi::{WireSignature, WireType},
    curios_base::{
        Flt, Grain, NumOp, Plicity, Qualifier,
        parser::{
            Parser, catch, fail, lazy, many0, many1, mark, memoize, not_ahead, preceded_by_space,
            pure, sep_by0_trailing, sep_by1, sep_by1_trailing, spanned, take_exact, take_n,
            take_while,
        },
    },
    num_bigint::BigUint,
    num_traits::{ToPrimitive, Zero},
    std::iter,
};

const CHARACTERS: &[char] = &['_'];

const KEYWORDS: &[&str] = &[
    "let", "match", "choose", "rec", "mod", "use", "pub", "end", "false", "true", "induct",
    "struct", "foreign",
];

pub(crate) fn parse_whitespace<'a>() -> Parser<'a, ()> {
    take_while(|char| char.is_whitespace())
        .and(
            catch(
                take_exact("--")
                    .and_keep(take_while(|char| char != '\n'))
                    .and_keep(lazy(parse_whitespace)),
            )
            .or(pure(())),
        )
        .map(|_| ())
}

fn parse_literal<'a>(expected: &'static str) -> Parser<'a, ()> {
    take_exact(expected).and_drop(parse_whitespace())
}

// The identifier characters alone, consuming no whitespace — the building
// block of the tight (whitespace-free) positions like a `Bits`/`Bytes` literal's
// `\..` spread operand.
fn parse_identifier_raw<'a>() -> Parser<'a, &'a str> {
    take_while(|char| CHARACTERS.contains(&char) || char.is_alphanumeric()).flat_map(|identifier| {
        match identifier.is_empty() {
            true => fail("Expected identifier"),
            false => pure(identifier),
        }
    })
}

fn parse_identifier<'a>() -> Parser<'a, &'a str> {
    parse_identifier_raw().and_drop(parse_whitespace())
}

fn name_from_segments<'a>(is_abs: bool, segments: Vec<String>) -> Parser<'a, Name> {
    match segments
        .iter()
        .any(|segment| KEYWORDS.contains(&segment.as_str()))
    {
        true => fail(format!(
            "path '{}' contains a reserved keyword",
            segments.join("/")
        )),
        false => pure(Name::new(is_abs, Qualifier::from(segments))),
    }
}

fn parse_name<'a>() -> Parser<'a, Name> {
    spanned(
        catch(take_exact("/"))
            .map(|()| true)
            .or(pure(false))
            .and(parse_identifier().and(many0(|| {
                catch(take_exact("/").and_keep(parse_identifier()))
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
}

// A strictly glued name path — no whitespace anywhere, not even trailing. The
// tight sibling of [`parse_name`] (whose segments each eat trailing
// whitespace, so `Foo /bar` is the path `Foo/bar` there), used where the
// surrounding grammar is whitespace-sensitive: a `Bits`/`Bytes` literal's `\..` spread
// operand.
fn parse_name_raw<'a>() -> Parser<'a, Name> {
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
}

fn parse_qualified_name<'a>() -> Parser<'a, Name> {
    catch(parse_name().flat_map(|name| match name.is_single() {
        true => fail("expected a qualified path"),
        false => pure(name),
    }))
}

fn parse_keyword<'a>(expected: &'static str) -> Parser<'a, ()> {
    parse_identifier().flat_map(move |obtained| match expected == obtained {
        true => pure(()),
        false => fail(format!(
            "Expected keyword '{expected}', obtained '{obtained}'"
        )),
    })
}
