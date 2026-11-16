//! The language's words: every word the surface grammar reads as itself rather than as a name — the fifteen keywords, each reserved or contextual, and the three sorts — stated once, so the parser, a segment's legality and the tests holding the editors' grammars read one table. The wire words of a `foreign` signature are `curios-abi`'s, which owns the wire.
//!
//! Why the words live beside the identity rather than in the parser is `README.md`'s decision.

#[cfg(test)]
mod tests;

use crate::Class;

/// Where a word is one, and so what else it may spell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Standing {
    /// A word wherever it stands, so it spells no name: no binder, label or path segment.
    Reserved,
    /// A word at an item's head, and a name anywhere else.
    ItemHead,
    /// A word between the members of a group, and a name anywhere else.
    GroupJoin,
    /// A sort where a term stands, so it names no binder, which only a term reads; a path segment or a field's label elsewhere.
    Sort,
}

/// Declares [`Keyword`] from one table, so the roster, [`Keyword::ALL`], the spellings, the standings and the classes cannot drift apart: a new word is written exactly once.
macro_rules! keywords {
    ($($variant:ident => $spelling:literal, $standing:ident, $class:ident;)+) => {
        /// A word the surface grammar reads as itself rather than as a name.
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum Keyword {
            $($variant,)+
        }

        impl Keyword {
            /// Every word, in declaration order.
            pub const ALL: [Keyword; [$(Keyword::$variant),+].len()] = [$(Keyword::$variant),+];

            /// The word spelled `word`, if it is one — [`Keyword::spelling`]'s reverse.
            pub fn read(word: &str) -> Option<Self> {
                Self::ALL.into_iter().find(|keyword| keyword.spelling() == word)
            }

            /// How the word is written.
            pub fn spelling(self) -> &'static str {
                match self {
                    $(Keyword::$variant => $spelling,)+
                }
            }

            /// Where the word is one.
            pub fn standing(self) -> Standing {
                match self {
                    $(Keyword::$variant => Standing::$standing,)+
                }
            }

            /// What the word is painted as wherever it stands as one.
            pub fn class(self) -> Class {
                match self {
                    $(Keyword::$variant => Class::$class,)+
                }
            }
        }
    };
}

keywords! {
    Let => "let", Reserved, Keyword;
    Match => "match", Reserved, Keyword;
    Mod => "mod", Reserved, Keyword;
    Use => "use", Reserved, Keyword;
    Pub => "pub", Reserved, Keyword;
    End => "end", Reserved, Keyword;
    Induct => "induct", Reserved, Keyword;
    Struct => "struct", Reserved, Keyword;
    Foreign => "foreign", Reserved, Keyword;
    True => "true", Reserved, Boolean;
    False => "false", Reserved, Boolean;
    Concept => "concept", ItemHead, Keyword;
    Satisfy => "satisfy", ItemHead, Keyword;
    Test => "test", ItemHead, Keyword;
    And => "and", GroupJoin, Keyword;
    Type => "Type", Sort, Sort;
    Prop => "Prop", Sort, Sort;
    Sort => "Sort", Sort, Sort;
}

/// Whether `word` is reserved, and so cannot spell a name: a binder, a label or a path segment.
pub fn is_reserved(word: &str) -> bool {
    Keyword::read(word).is_some_and(|keyword| keyword.standing() == Standing::Reserved)
}
