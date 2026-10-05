//! The mark a telescope's member is written under, and what a site does with a form its rule refuses.
//!
//! A member is plain, implicit (`@`) or a witness (`use`), and after the mark comes what the site's plain member would be: a type where the site declares, a binder where it binds, a value where it supplies. A `use` member has no binder anywhere. [`parse_mark`] is the one place a mark is read, so a site that does not take one says so by its own rule rather than leaving the next alternative to report the token it did not expect.
//!
//! **A refused form is read, held, and raised once the site is known.** `(@A, use Show(A), x)` opens a function type and a lambda alike, and only the arrow after it says which, so neither grammar may refuse a member where it stands: whichever is tried first would commit a refusal of a program that is valid as the other. A member parser therefore answers with a [`Read`] — the member, or the report [`refused`] builds over the text the form covers — and the site hands its list to [`members`] past the token that discriminates it. A site nothing else can be read as — a declaration's parameters, a payload, a field, a constructor pattern — raises where the mark stands.

use {
    super::{parse_identifier, parse_keyword, parse_literal},
    crate::{Term, parse_term},
    curios_parse::{
        Mark, Parser, ParserError, commit, fail, lazy, look_ahead, pure, raise, refusal,
    },
    curios_utilities::Plicity,
};

/// What refuses a binder after `use` where the site declares.
pub(super) const USE_HAS_NO_BINDER: &str = "a `use` member has no binder: a witness is reached by resolution, never by name, so the premise is written alone — `use Show(A)`";

/// What refuses a binder after `use` where the site binds.
pub(super) const USE_BINDS_NOTHING: &str = "a `use` member has no binder: a witness is reached by resolution, never by name, so the member is written `use _`";

/// What refuses `use _` where the site declares.
pub(super) const USE_PLACE_STATES_NO_TYPE: &str =
    "`use _` holds a place and states no type: a signature writes the premise — `use Show(A)`";

/// What refuses a plain member written as its type alone in a definition's telescope.
pub(super) const DEFINITION_NAMES_ITS_PLAIN_MEMBERS: &str = "a definition names its plain parameters: write `x: T`, or `_: T` for one the body does not use";

/// What refuses a type after `use` where the site binds.
pub(super) const BINDER_STATES_NO_USE_TYPE: &str = "a `use` member is written `use _` here: its type is the expected function type's to state, and a function that must state one is a `let` with a telescope";

/// What refuses a type after `@` where the site binds.
pub(super) const BINDER_FOLLOWS_IMPLICIT: &str =
    "after `@` a binder is read here, not a type: write `@_`, or `@_: T` to state the type";

/// What refuses a `use` member among a constructor's payload.
pub(super) const PAYLOAD_TAKES_NO_USE: &str =
    "a constructor's payload takes no `use` member: a payload is plain or `@`";

/// What refuses a mark on an index.
pub(super) const INDEX_TAKES_NO_MARK: &str =
    "an index takes no mark: a family's indices are always written";

/// What refuses a `use` member among a structure's fields.
pub(super) const STRUCT_FIELD_TAKES_NO_USE: &str = "a structure's field takes no `use` member: its premise is a `use` parameter — `struct S(K: Type, use C(K))` — and a field is plain or `@`";

/// What refuses an `@` member among a concept's fields.
pub(super) const CONCEPT_FIELD_TAKES_NO_IMPLICIT: &str =
    "a concept's field takes no `@`: it is a method, or a superclass written `use Concept(args)`";

/// What refuses a mark on a field of a tuple type.
pub(super) const TUPLE_FIELD_TAKES_NO_MARK: &str = "a tuple type's field takes no mark: a hidden field is a structure's — `struct S: Type { n: Nat, @Holds(0 < n) }`";

/// What refuses a mark on a field of a struct pattern.
pub(super) const STRUCT_PATTERN_TAKES_NO_MARK: &str = "a struct pattern takes no mark: a hidden field takes no position, and one that has a label is read by it — `label = binder`";

/// What refuses a `use` member in a constructor pattern.
pub(super) const PATTERN_TAKES_NO_USE: &str =
    "a pattern takes no `use` member: a constructor's payload is plain or `@`";

/// A member a site read, or the report of the form it read in that member's place.
pub(super) type Read<T> = Result<T, ParserError>;

/// The mark a member or an argument is written under: `use`, `@`, or nothing.
pub(super) fn parse_mark<'a>() -> Parser<'a, Plicity> {
    parse_keyword("use")
        .map(|()| Plicity::Witness)
        .or(parse_literal("@").map(|()| Plicity::Implicit))
        .or(pure(Plicity::Explicit))
}

/// The report that the text from `start` to here breaks `rule`, as the read a site holds in a member's place.
pub(super) fn refused<'a, T: 'a>(start: &Mark, rule: &'static str) -> Parser<'a, Read<T>> {
    refusal(start, rule).map(Err)
}

/// The members a site read, or the first report among them raised as the site's own. Called past the token that makes the text this site's, which is what lets the report commit.
pub(super) fn members<'a, T: 'a>(read: Vec<Read<T>>) -> Parser<'a, Vec<T>> {
    match read.into_iter().collect::<Result<Vec<_>, _>>() {
        Ok(members) => pure(members),
        Err(report) => commit(raise(report)),
    }
}

/// One read raised where it stands, for a site no other grammar reads.
pub(super) fn raised<'a, T: 'a>(read: Parser<'a, Read<T>>) -> Parser<'a, T> {
    read.flat_map(|read| match read {
        Ok(member) => pure(member),
        Err(report) => commit(raise(report)),
    })
}

/// Whether a member ends here: at the separator, or at the bracket that closes its list.
pub(super) fn member_ends<'a>() -> Parser<'a, ()> {
    look_ahead(
        parse_literal(",")
            .or(parse_literal(")"))
            .or(parse_literal("}")),
    )
}

/// The word `_`, alone.
fn parse_wildcard<'a>() -> Parser<'a, ()> {
    parse_identifier().flat_map(|word| match word {
        "_" => pure(()),
        _ => fail("not a wildcard"),
    })
}

/// What follows `use` where the site declares: the premise's type. A binder before it, or a bare `_` in its place, is read whole and reported; `start` is where the mark was written.
pub(super) fn parse_premise<'a>(start: Mark) -> Parser<'a, Read<Term>> {
    let (bound, placed) = (start.clone(), start);

    parse_identifier()
        .and_drop(parse_literal(":"))
        .and_drop(lazy(parse_term))
        .flat_map(move |_| refused(&bound, USE_HAS_NO_BINDER))
        .or(parse_wildcard()
            .and_drop(member_ends())
            .flat_map(move |()| refused(&placed, USE_PLACE_STATES_NO_TYPE)))
        .or(lazy(parse_term).map(Ok))
}

/// What follows `use` where the site binds: `_`, the member's place. A name or a type there is read whole and reported; `start` is where the mark was written.
pub(super) fn parse_place<'a>(start: Mark) -> Parser<'a, Read<()>> {
    let (named, typed) = (start.clone(), start);

    parse_wildcard()
        .and_drop(member_ends())
        .map(Ok)
        .or(parse_identifier()
            .and_drop(member_ends())
            .flat_map(move |_| refused(&named, USE_BINDS_NOTHING)))
        .or(lazy(parse_term)
            .and_drop(
                parse_literal(":")
                    .and_keep(lazy(parse_term))
                    .map(|_| ())
                    .or(pure(())),
            )
            .flat_map(move |_| refused(&typed, BINDER_STATES_NO_USE_TYPE)))
}
