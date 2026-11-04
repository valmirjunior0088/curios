//! A sort, the levels it stands at, an instance's universes and a declaration's scheme.
//!
//! **`Type` and `Prop` build their sort at level 0, and `Sort` the first sort a declaration leaves to its caller.** A sort at another level is `#{level = u + 1} Type`, one at another quality `#{quality = Param(1)} Sort`, and an unsolved quality `#{quality = Meta(3)} Sort`.
//!
//! **A level is a term.** A constant is its numeral, a parameter its letter — `u` to `z`, then `u1` to `z1` — an offset `u + 1`, several `max(u, v + 1)`, and an unsolved level `#{id = 4} ?`. An instance's universes are `{levels = […], qualities = […]}`, each part left out where it holds nothing, and a scheme `{parameter_count = 2, quality_count = 1, constraints = [u <= v + 1]}`, each constraint's origin unwritten.

use {
    super::{Decoration, bracketed, nested},
    crate::{Frame, Spelling},
    curios_core::{Level, LevelHead, Quality, QualityParam, Sort, UniverseContext, Universes},
    curios_print::{Printer, flat, pure},
};

/// `sort` as its word, under a decorator where its level or quality is not the word's; in a reader's spelling, with its level after it where it is not 0.
pub fn sort_written(Sort { quality, level }: Sort, frame: Frame) -> Printer {
    if !frame.core_text() {
        let word = reader_quality(&quality);
        // A level is the sort's whole content, so a concrete one is shown; an unsolved one names nothing a reader can act on.
        if level.is_zero() || (frame.spelling.erases_universes() && level.metas().next().is_some())
        {
            return pure(word);
        }
        return pure(format!("{word}.{{{}}}", reader_level(&level)));
    }
    let (word, template) = match quality {
        Quality::Type => ("Type", Quality::Type),
        Quality::Prop => ("Prop", Quality::Prop),
        Quality::Param(_) | Quality::Meta(_) => ("Sort", Quality::Param(QualityParam(0))),
    };
    let mut fields = Vec::<Decoration>::new();
    if quality != template {
        fields.push(("quality", pure(quality_written(&quality))));
    }
    // A diagnostic leaves an unsolved level out: it names nothing a reader can act on.
    let erased = frame.spelling.erases_universes() && level.metas().next().is_some();
    if !level.is_zero() && !erased {
        fields.push(("level", level_written(&level)));
    }

    frame.decorated(fields, pure(word), true)
}

/// The word a reader writes a sort of `quality` by: an open one by the word a program leaves one with.
fn reader_quality(quality: &Quality) -> &'static str {
    match quality {
        Quality::Type => "Type",
        Quality::Prop => "Prop",
        Quality::Param(_) | Quality::Meta(_) => "Sort",
    }
}

/// A level as a reader is shown it: a constant, a parameter or a metavariable each under its offset, and their `max` where there are several.
fn reader_level(level: &Level) -> String {
    let mut parts = Vec::new();
    if level.constant != 0 || level.atoms.is_empty() {
        parts.push(level.constant.to_string());
    }
    for (head, offset) in &level.atoms {
        let name = match head {
            LevelHead::Param(param) => parameter_letter(param.0),
            LevelHead::Meta(meta) => meta.to_string(),
        };
        parts.push(match offset {
            0 => name,
            offset => format!("{name}+{offset}"),
        });
    }
    match parts.as_slice() {
        [single] => single.clone(),
        _ => format!("max({})", parts.join(",")),
    }
}

/// A level as `spelling` writes it: as the term it is in Core text proper, and as a reader is shown it otherwise.
pub fn level_spelled(level: &Level, spelling: &Spelling) -> String {
    match spelling.writes_core_text() {
        true => level_text(level),
        false => reader_level(level),
    }
}

/// An instance after the name it is of, as a reader is shown it: the sorts it states, then its levels, or nothing where it states none or the spelling leaves universes out.
pub fn universe_suffix(universes: &Universes, frame: Frame) -> String {
    if universes.is_empty() || frame.spelling.erases_universes() {
        return String::new();
    }
    let qualities = universes
        .qualities
        .iter()
        .map(reader_quality)
        .collect::<Vec<_>>()
        .join(",");
    let levels = universes
        .levels
        .iter()
        .map(reader_level)
        .collect::<Vec<_>>()
        .join(",");
    match (qualities.is_empty(), levels.is_empty()) {
        (true, _) => format!(".{{{levels}}}"),
        (false, true) => format!(".{{{qualities}}}"),
        (false, false) => format!(".{{{qualities}; {levels}}}"),
    }
}

/// A quality as Core names it.
fn quality_written(quality: &Quality) -> String {
    match quality {
        Quality::Type => "Type".to_string(),
        Quality::Prop => "Prop".to_string(),
        Quality::Param(param) => format!("Param({})", param.0),
        Quality::Meta(meta) => format!("Meta({})", meta.0),
    }
}

/// `level` as the term it is.
pub fn level_written(level: &Level) -> Printer {
    pure(level_text(level))
}

/// [`level_written`] as text: a level holds no break.
pub fn level_text(level: &Level) -> String {
    let mut parts = Vec::new();
    if level.constant != 0 || level.atoms.is_empty() {
        parts.push(level.constant.to_string());
    }
    for (head, offset) in &level.atoms {
        let atom = match head {
            LevelHead::Param(param) => parameter_letter(param.0),
            LevelHead::Meta(meta) => format!("#{{id = {}}} ?", meta.0),
        };
        parts.push(match offset {
            0 => atom,
            offset => format!("{atom} + {offset}"),
        });
    }

    match parts.as_slice() {
        [single] => single.clone(),
        _ => format!("max({})", parts.join(", ")),
    }
}

/// The letter a level parameter is written by: `u` to `z`, then the same letters numbered from 1.
fn parameter_letter(index: usize) -> String {
    const LETTERS: usize = (b'z' - b'u' + 1) as usize;
    let letter = char::from(b'u' + u8::try_from(index % LETTERS).expect("a letter offset"));
    match index / LETTERS {
        0 => letter.to_string(),
        cycle => format!("{letter}{cycle}"),
    }
}

/// An instance's universes as a struct-valued field's value, or `None` where it states none.
pub fn universes_written(universes: &Universes, frame: Frame) -> Option<Printer> {
    if universes.is_empty() || frame.spelling.erases_universes() {
        return None;
    }
    let mut fields = Vec::<Decoration>::new();
    if !universes.levels.is_empty() {
        fields.push((
            "levels",
            bracketed(universes.levels.iter().map(level_written).collect()),
        ));
    }
    if !universes.qualities.is_empty() {
        fields.push((
            "qualities",
            bracketed(
                universes
                    .qualities
                    .iter()
                    .map(|quality| pure(quality_written(quality)))
                    .collect(),
            ),
        ));
    }

    Some(nested(fields))
}

/// A declaration's scheme as a struct-valued field's value, or `None` where it is the empty one.
pub fn universe_context_written(context: &UniverseContext) -> Option<Printer> {
    let mut fields = Vec::<Decoration>::new();
    if context.parameter_count != 0 {
        fields.push(("parameter_count", pure(context.parameter_count.to_string())));
    }
    if context.quality_count != 0 {
        fields.push(("quality_count", pure(context.quality_count.to_string())));
    }
    if !context.constraints.is_empty() {
        fields.push((
            "constraints",
            bracketed(
                context
                    .constraints
                    .iter()
                    .map(|constraint| {
                        flat([
                            level_written(&constraint.lower),
                            pure(" <= "),
                            level_written(&constraint.upper),
                        ])
                    })
                    .collect(),
            ),
        ));
    }

    (!fields.is_empty()).then(|| nested(fields))
}
