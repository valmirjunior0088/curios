//! The attribute macro behind `curios-archive`. Depend on that crate, never on this one.
//!
//! A `proc-macro = true` crate can export nothing but macros, which is the only reason this is separate — the serde/serde_derive arrangement, for the same reason serde has it.

use proc_macro::{Delimiter, Group, TokenStream, TokenTree};

/// The rkyv helper clause an inert field marker stands for, or `None` if this attribute is not one of ours.
///
/// `group` is the bracketed body of a `#[…]`, so its first token is the attribute's name.
fn field_clause(group: &Group) -> Option<String> {
    if group.delimiter() != Delimiter::Bracket {
        return None;
    }

    let mut inner = group.stream().into_iter();
    let TokenTree::Ident(name) = inner.next()? else {
        return None;
    };

    match name.to_string().as_str() {
        "archived_omit_bounds" => Some("omit_bounds".to_string()),
        "archived_with" => match inner.next()? {
            TokenTree::Group(argument) if argument.delimiter() == Delimiter::Parenthesis => {
                Some(format!("with = {}", argument.stream()))
            }
            _ => None,
        },
        _ => None,
    }
}

/// Rewrite this crate's inert field markers into the rkyv helper attributes they stand for, everywhere in `stream`.
///
/// The walk is over token trees rather than a parsed item, which is what keeps this crate free of `syn`. It can afford to be: a marker is `#` followed by a bracket group whose first token is one of two idents, and recognizing that needs no grammar. Anything else is copied through untouched, so a form this does not know is a form it cannot damage.
fn rewrite_markers(stream: TokenStream, always: bool) -> TokenStream {
    let mut rewritten = Vec::new();
    let mut trees = stream.into_iter().peekable();

    while let Some(tree) = trees.next() {
        // A marker is two trees — `#` and the bracket group — so it is recognized from the first and consumed as a pair.
        if let TokenTree::Punct(ref punct) = tree
            && punct.as_char() == '#'
            && let Some(TokenTree::Group(group)) = trees.peek()
            && let Some(clause) = field_clause(group)
        {
            trees.next();

            let attribute = match always {
                true => format!("#[rkyv({clause})]"),
                false => format!("#[cfg_attr(feature = \"archive\", rkyv({clause}))]"),
            };
            rewritten.extend(
                attribute
                    .parse::<TokenStream>()
                    .expect("the generated field attribute is well-formed"),
            );

            continue;
        }

        // Fields live inside the item's brace group, so the walk has to descend.
        match tree {
            TokenTree::Group(group) => {
                let mut rebuilt =
                    Group::new(group.delimiter(), rewrite_markers(group.stream(), always));
                rebuilt.set_span(group.span());
                rewritten.push(TokenTree::Group(rebuilt));
            }
            other => rewritten.push(other),
        }
    }

    rewritten.into_iter().collect()
}

/// Mark a type as archived: gate rkyv's three derives on the crate's own `archive` feature, and redirect the paths they expand to.
///
/// ```text
/// #[curios_archive::archived]
/// pub struct Qualifier { … }
/// ```
///
/// Arguments are forwarded into the same `rkyv(…)` clause as the crate redirect, so anything rkyv's derive accepts is written where it would be otherwise:
///
/// ```text
/// #[curios_archive::archived(derive(PartialEq, Eq, Hash))]
/// pub struct Atom(String);
/// ```
///
/// # Why the expansion is `cfg_attr` rather than `cfg`
///
/// The annotation is unconditional at the call site and the *expansion* is what the feature gates. `cfg_attr` is evaluated where the macro expands, so `feature = "archive"` names the consuming crate's own feature — each crate keeps its gate, and this one neither knows nor needs to know which crates have it on.
///
/// That is also why this macro is not itself feature-gated. A macro that vanished with the feature would make every annotated type a compile error in a build with archiving off.
///
/// # Field markers
///
/// A field says `#[archived_with(SomeAdapter)]` or `#[archived_omit_bounds]`, and this rewrites it into the gated `rkyv(…)` helper. Both are inert: nothing declares them, so a field carrying one outside an `#[archived]` item is an unresolved-attribute error rather than a silently ignored line.
///
/// This is what lets rkyv be spelled nowhere but its two owning crates, and it means the macro now *does* read the item's body — reversing what this paragraph used to say. The reversal is deliberate and the reasoning is worth keeping: reading the body was declined while "field adapters are rare" stood in for a number, and the number turned out to be fifteen. What made it cheap in the end is that recognizing a marker needs no grammar — it is `#`, a bracket group, and one of two idents — so the walk is over token trees and this crate still depends on nothing. `syn` was never the price; it was only assumed to be.
/// The bounds a *recursive* archived type needs, spelled once.
///
/// rkyv cannot infer these for a type that reaches itself, so every such type carried the same three clauses copied by hand. Five copies had already drifted into two spellings of `SharedContext` — the re-export and its defining module — which is what a duplicated bound does eventually and why this now lives in exactly one place.
const RECURSIVE_BOUNDS: &str = "serialize_bounds(\
        __S: ::curios_archive::rkyv::ser::Writer \
            + ::curios_archive::rkyv::ser::Allocator \
            + ::curios_archive::rkyv::ser::Sharing, \
        __S::Error: ::curios_archive::rkyv::rancor::Source\
    ), \
    deserialize_bounds(\
        __D: ::curios_archive::rkyv::de::Pooling, \
        __D::Error: ::curios_archive::rkyv::rancor::Source\
    ), \
    bytecheck(bounds(\
        __C: ::curios_archive::rkyv::validation::ArchiveContext \
            + ::curios_archive::rkyv::validation::SharedContext, \
        __C::Error: ::curios_archive::rkyv::rancor::Source\
    ))";

/// Strip a leading bare `keyword` from an argument list, returning what follows.
///
/// Matches only a whole argument — `recursive` and `recursive, …`, never the prefix of a longer name — so a future `recursive_something` passed through to rkyv is untouched.
fn take_keyword<'a>(arguments: &'a str, keyword: &str) -> Option<&'a str> {
    let rest = arguments.strip_prefix(keyword)?.trim_start();

    match rest.strip_prefix(',') {
        Some(rest) => Some(rest.trim_start()),
        None if rest.is_empty() => Some(rest),
        None => None,
    }
}

#[proc_macro_attribute]
pub fn archived(args: TokenStream, item: TokenStream) -> TokenStream {
    let arguments = args.to_string();
    let mut arguments = arguments.trim();

    let mut always = false;
    let mut recursive = false;
    // Either order, so a reader writes what reads best rather than what the macro insists on.
    loop {
        if let Some(rest) = take_keyword(arguments, "always") {
            (always, arguments) = (true, rest);
        } else if let Some(rest) = take_keyword(arguments, "recursive") {
            (recursive, arguments) = (true, rest);
        } else {
            break;
        }
    }

    let clauses = [recursive.then_some(RECURSIVE_BOUNDS), Some(arguments)]
        .into_iter()
        .flatten()
        .filter(|clause| !clause.is_empty())
        .collect::<Vec<_>>()
        .join(", ");
    let forwarded = match clauses.is_empty() {
        true => String::new(),
        false => format!(", {clauses}"),
    };

    let derives = "derive(\
        ::curios_archive::Archive, \
        ::curios_archive::Serialize, \
        ::curios_archive::Deserialize\
    )";
    let redirect = format!("rkyv(crate = ::curios_archive::rkyv{forwarded})");

    // `always` drops the gate rather than changing what is emitted, and must split the pair into two attributes: a comma-separated list of them is `cfg_attr`'s own grammar, not an attribute's. A crate that archives unconditionally has no `archive` feature for `cfg_attr` to name anyway.
    let attribute = match always {
        true => format!("#[{derives}] #[{redirect}]"),
        false => format!("#[cfg_attr(feature = \"archive\", {derives}, {redirect})]"),
    };

    let mut expanded: TokenStream = attribute
        .parse()
        .expect("the generated attribute is well-formed");
    expanded.extend(rewrite_markers(item, always));
    expanded
}
