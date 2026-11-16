//! The language's words: every word the surface grammar reads as itself rather than as a name — the fifteen keywords, each reserved or contextual, and the three sorts — stated once, so the parser, a segment's legality and the tests holding the editors' grammars read one table. The wire words of a `foreign` signature are `curios-abi`'s, which owns the wire.
//!
//! Why the words live beside the identity rather than in the parser is `README.md`'s decision.

#[cfg(test)]
mod tests;

/// Where a word is one, and so what else it may spell.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Standing {
    /// A word wherever it stands, so it spells no name: no binder, label or path segment.
    Reserved,
    /// A word at an item's head, and a name anywhere else.
    ItemHead,
    /// A word between the members of a group, and a name anywhere else.
    GroupJoin,
    /// A sort where a term stands, and a path segment elsewhere.
    Sort,
}

/// Declares [`Keyword`] from one table, so the roster, [`Keyword::ALL`], the spellings and the standings cannot drift apart: a new word is written exactly once.
macro_rules! keywords {
    ($($variant:ident => $spelling:literal, $standing:ident;)+) => {
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
        }
    };
}

keywords! {
    Let => "let", Reserved;
    Match => "match", Reserved;
    Mod => "mod", Reserved;
    Use => "use", Reserved;
    Pub => "pub", Reserved;
    End => "end", Reserved;
    Induct => "induct", Reserved;
    Struct => "struct", Reserved;
    Foreign => "foreign", Reserved;
    True => "true", Reserved;
    False => "false", Reserved;
    Concept => "concept", ItemHead;
    Satisfy => "satisfy", ItemHead;
    Test => "test", ItemHead;
    And => "and", GroupJoin;
    Type => "Type", Sort;
    Prop => "Prop", Sort;
    Sort => "Sort", Sort;
}

/// Whether `word` is reserved, and so cannot spell a name: a binder, a label or a path segment.
pub fn is_reserved(word: &str) -> bool {
    Keyword::read(word).is_some_and(|keyword| keyword.standing() == Standing::Reserved)
}
