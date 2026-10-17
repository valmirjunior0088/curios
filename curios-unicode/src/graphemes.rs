//! Text segmented into extended grapheme clusters, by the rules of UAX #29 over the tables the database gives.

use crate::{
    GraphemeBreak, IndicConjunctBreak, grapheme_break, indic_conjunct_break,
    is_extended_pictographic,
};

/// What the scalars up to a candidate boundary leave that a rule reads past the one scalar before it.
#[derive(Clone, Copy, Default)]
struct Run {
    /// GB11: an `Extended_Pictographic` scalar and only `Extend` scalars since, up to the scalar before.
    pictographic: bool,
    /// GB11: such a run and then a ZWJ, the scalar before.
    joined_pictographic: bool,
    /// GB12 and GB13: the regional indicators that run up to the scalar before are odd in number.
    odd_indicators: bool,
    /// GB9c: an `InCB=Linker` scalar and only `InCB=Extend` scalars since, up to the scalar before.
    linked: bool,
}

impl Run {
    /// The run once `next` is the scalar before.
    fn after(self, next: char, class: GraphemeBreak) -> Self {
        let conjunct = indic_conjunct_break(next);

        Self {
            pictographic: is_extended_pictographic(next)
                || (self.pictographic && class == GraphemeBreak::Extend),
            joined_pictographic: self.pictographic && class == GraphemeBreak::Zwj,
            odd_indicators: class == GraphemeBreak::RegionalIndicator && !self.odd_indicators,
            linked: conjunct == IndicConjunctBreak::Linker
                || (self.linked && conjunct == IndicConjunctBreak::Extend),
        }
    }
}

/// Whether a cluster ends between a scalar of class `before` and `next`, of class `class`, the run up to `before` being `run`: the rules of UAX #29 in their order, the first that decides deciding.
fn breaks(before: GraphemeBreak, next: char, class: GraphemeBreak, run: Run) -> bool {
    use GraphemeBreak::*;

    match (before, class) {
        // GB3.
        (Cr, Lf) => false,
        // GB4 and GB5.
        (Control | Cr | Lf, _) | (_, Control | Cr | Lf) => true,
        // GB6, GB7 and GB8.
        (L, L | V | Lv | Lvt) | (Lv | V, V | T) | (Lvt | T, T) => false,
        // GB9, GB9a and GB9b.
        (_, Extend | Zwj | SpacingMark) | (Prepend, _) => false,
        // GB9c.
        _ if run.linked && indic_conjunct_break(next) == IndicConjunctBreak::Consonant => false,
        // GB11.
        (Zwj, _) if run.joined_pictographic && is_extended_pictographic(next) => false,
        // GB12 and GB13.
        (RegionalIndicator, RegionalIndicator) => !run.odd_indicators,
        // GB999.
        _ => true,
    }
}

/// The extended grapheme clusters of a text, in order: what [`graphemes()`] answers.
pub struct Graphemes<'a> {
    rest: &'a str,
}

impl<'a> Iterator for Graphemes<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        let mut scalars = self.rest.char_indices();
        let (_, first) = scalars.next()?;
        let mut before = grapheme_break(first);
        let mut run = Run::default().after(first, before);
        let mut end = self.rest.len();

        for (at, next) in scalars {
            let class = grapheme_break(next);
            if breaks(before, next, class, run) {
                end = at;
                break;
            }
            before = class;
            run = run.after(next, class);
        }

        let (cluster, rest) = self.rest.split_at(end);
        self.rest = rest;

        Some(cluster)
    }
}

/// `text` as its extended grapheme clusters (UAX #29).
pub fn graphemes(text: &str) -> Graphemes<'_> {
    Graphemes { rest: text }
}
