//! The properties the tables hold, each class set an enum the build script writes the tables in, and the lookups that answer a scalar's class.

use {
    crate::{
        DEFAULT_IGNORABLE, EAST_ASIAN_WIDTH, EAST_ASIAN_WIDTH_MISSING, EMOJI_PRESENTATION,
        EXTENDED_PICTOGRAPHIC, GRAPHEME_BREAK, GRAPHEME_BREAK_MISSING, INDIC_CONJUNCT_BREAK,
        INDIC_CONJUNCT_BREAK_MISSING,
    },
    std::cmp::Ordering,
};

/// A property's classes as one enum: each variant named as the database spells the class, so the build script writes the tables in the variants themselves, and the name `/sys` gives the class's code beside it. A class's code is its place in the list.
macro_rules! classes {
    ($(#[$doc:meta])* $kind:ident { $($(#[$class:meta])* $variant:ident => $sys:literal),+ $(,)? }) => {
        $(#[$doc])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[repr(u8)]
        pub enum $kind {
            $($(#[$class])* $variant),+
        }

        impl $kind {
            /// Every class, in the order of its code.
            pub const ALL: &[Self] = &[$(Self::$variant),+];

            /// The code `/sys` gives this class: its place among [`Self::ALL`].
            pub fn code(self) -> u8 {
                self as u8
            }

            /// The name `/sys` gives this class's code.
            pub fn sys_name(self) -> &'static str {
                match self {
                    $(Self::$variant => $sys),+
                }
            }
        }
    };
}

classes! {
    /// A scalar's `Grapheme_Cluster_Break` class: what segmenting text into extended grapheme clusters reads (UAX #29).
    GraphemeBreak {
        /// Any scalar of no other class.
        Other => "other",
        /// U+000D CARRIAGE RETURN.
        Cr => "cr",
        /// U+000A LINE FEED.
        Lf => "lf",
        /// A control or a separator: always a cluster of its own.
        Control => "control",
        /// A mark or a modifier that joins the scalar before it.
        Extend => "extend",
        /// U+200D ZERO WIDTH JOINER.
        Zwj => "zwj",
        /// One of the regional indicators a flag is a pair of.
        RegionalIndicator => "regional_indicator",
        /// A scalar that joins the one after it.
        Prepend => "prepend",
        /// A spacing mark that joins the scalar before it.
        SpacingMark => "spacing_mark",
        /// A Hangul leading consonant.
        L => "l",
        /// A Hangul vowel.
        V => "v",
        /// A Hangul trailing consonant.
        T => "t",
        /// A Hangul syllable of a leading consonant and a vowel.
        Lv => "lv",
        /// A Hangul syllable of a leading consonant, a vowel and a trailing consonant.
        Lvt => "lvt",
    }
}

classes! {
    /// A scalar's `Indic_Conjunct_Break` class: what rule GB9c of UAX #29 reads to keep a conjunct in one cluster.
    IndicConjunctBreak {
        /// Any scalar of no other class.
        None => "none",
        /// A virama that joins the consonants around it.
        Linker => "linker",
        /// A consonant a linker joins.
        Consonant => "consonant",
        /// A mark that may stand between a linker and the consonant it joins.
        Extend => "extend",
    }
}

classes! {
    /// A scalar's `East_Asian_Width` class (UAX #11).
    EastAsianWidth {
        /// Neutral: no East Asian width.
        N => "neutral",
        /// Ambiguous: wide in an East Asian context, narrow elsewhere.
        A => "ambiguous",
        /// Halfwidth.
        H => "halfwidth",
        /// Wide.
        W => "wide",
        /// Fullwidth.
        F => "fullwidth",
        /// Narrow.
        Na => "narrow",
    }
}

/// How `scalar` stands against the run `first..=last`.
fn against(first: u32, last: u32, scalar: u32) -> Ordering {
    match (scalar < first, last < scalar) {
        (true, _) => Ordering::Greater,
        (_, true) => Ordering::Less,
        _ => Ordering::Equal,
    }
}

/// The class `table`'s run holding `scalar` gives it, or `missing` where no run does.
fn class_of<C: Copy>(table: &[(u32, u32, C)], missing: C, scalar: char) -> C {
    let scalar = u32::from(scalar);

    table
        .binary_search_by(|&(first, last, _)| against(first, last, scalar))
        .map_or(missing, |run| table[run].2)
}

/// Whether a run of `table` holds `scalar`.
fn held(table: &[(u32, u32)], scalar: char) -> bool {
    let scalar = u32::from(scalar);

    table
        .binary_search_by(|&(first, last)| against(first, last, scalar))
        .is_ok()
}

/// `scalar`'s `Grapheme_Cluster_Break` class.
pub fn grapheme_break(scalar: char) -> GraphemeBreak {
    class_of(GRAPHEME_BREAK, GRAPHEME_BREAK_MISSING, scalar)
}

/// `scalar`'s `Indic_Conjunct_Break` class.
pub fn indic_conjunct_break(scalar: char) -> IndicConjunctBreak {
    class_of(INDIC_CONJUNCT_BREAK, INDIC_CONJUNCT_BREAK_MISSING, scalar)
}

/// `scalar`'s `East_Asian_Width` class.
pub fn east_asian_width(scalar: char) -> EastAsianWidth {
    class_of(EAST_ASIAN_WIDTH, EAST_ASIAN_WIDTH_MISSING, scalar)
}

/// Whether `scalar` is `Extended_Pictographic`, which rule GB11 of UAX #29 reads to keep an emoji sequence in one cluster.
pub fn is_extended_pictographic(scalar: char) -> bool {
    held(EXTENDED_PICTOGRAPHIC, scalar)
}

/// Whether `scalar` presents as an emoji by default (`Emoji_Presentation`).
pub fn is_emoji_presentation(scalar: char) -> bool {
    held(EMOJI_PRESENTATION, scalar)
}

/// Whether `scalar` is `Default_Ignorable_Code_Point`: one the database says a renderer shows nothing of where it does not support it — a zero-width space or joiner, a direction mark or override, a variation selector, a Hangul filler, a tag.
pub fn is_default_ignorable(scalar: char) -> bool {
    held(DEFAULT_IGNORABLE, scalar)
}
