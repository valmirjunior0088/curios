//! A sort, the levels it stands at, an instance's universes and a declaration's scheme.
//!
//! **`Type` and `Prop` build their sort at level 0, and `Sort` the first sort a declaration leaves to its caller.** A sort at another level is `#{level = u + 1} Type`, one at another quality `#{quality = Param(1)} Sort`, and an unsolved quality `#{quality = Meta(3)} Sort`.
//!
//! **A level is a term.** A constant is its numeral, a parameter its letter — `u` to `z`, then `u1` to `z1` — an offset `u + 1`, several `max(u, v + 1)`, and an unsolved level `#{id = 4} ?`. An instance's universes are `{levels = […], qualities = […]}`, each part left out where it holds nothing, and a scheme `{parameter_count = 2, quality_count = 1, constraints = [u <= v + 1]}`, each constraint's origin unwritten.

use {
    super::{
        Decoration, Fields, ReadError, Refusal, Tree, bracketed, called, decorated, malformed,
        nested, numeral, term_number, term_word, tree, value, value_fields, value_number,
        value_term, value_unmarked, value_variant, verbatim, word,
    },
    crate::{
        Apply, CoreForm, CoreText, CoreValue, Decorated, Frame, Infix, Shown, Spelling, Subterm,
    },
    curios_common::{InfixOp, Plicity, Sign},
    curios_core::{
        Level, LevelHead, Quality, QualityMetaId, QualityParam, Sort, Term, UniverseConstraint,
        UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext, UniverseMetaId,
        UniverseParam, Universes,
    },
};

/// `sort` as its word, under a decorator where its level or quality is not the word's; in a reader's spelling, with its level after it where it is not 0.
pub fn sort_written(Sort { quality, level }: Sort, frame: Frame) -> Tree {
    if !frame.core_text() {
        let word = reader_quality(&quality);
        // A level is the sort's whole content, so a concrete one is shown; an unsolved one names nothing a reader can act on.
        if level.is_zero() || (frame.spelling.erases_universes() && level.metas().next().is_some())
        {
            return quality_word(word);
        }
        return verbatim(format!("{word}.{{{}}}", reader_level(&level)));
    }
    let (word, template) = match quality {
        Quality::Type => ("Type", Quality::Type),
        Quality::Prop => ("Prop", Quality::Prop),
        Quality::Param(_) | Quality::Meta(_) => ("Sort", Quality::Param(QualityParam(0))),
    };
    let mut fields = Vec::<Decoration>::new();
    if quality != template {
        fields.push(("quality", value(quality_written(&quality))));
    }
    // A diagnostic leaves an unsolved level out: it names nothing a reader can act on.
    let erased = frame.spelling.erases_universes() && level.metas().next().is_some();
    if !level.is_zero() && !erased {
        fields.push(("level", value(level_written(&level))));
    }

    frame.decorated(fields, quality_word(word))
}

/// The sort word `word` is, as the tree's node of it.
fn quality_word(word: &str) -> Tree {
    tree(match word {
        "Type" => Subterm::Type,
        "Prop" => Subterm::Prop,
        _ => Subterm::Sort,
    })
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
fn quality_written(quality: &Quality) -> Tree {
    match quality {
        Quality::Type => word("Type"),
        Quality::Prop => word("Prop"),
        Quality::Param(param) => called(word("Param"), vec![numeral(param.0, Sign::Unmarked)]),
        Quality::Meta(meta) => called(word("Meta"), vec![numeral(meta.0, Sign::Unmarked)]),
    }
}

/// `level` as the term it is: a constant, a parameter by its letter and an unsolved level as `#{id = 4} ?`, each under its offset, and their `max` where there are several.
pub fn level_written(level: &Level) -> Tree {
    let mut parts = Vec::new();
    if level.constant != 0 || level.atoms.is_empty() {
        parts.push(numeral(level.constant, Sign::Unmarked));
    }
    for (head, offset) in &level.atoms {
        let atom = match head {
            LevelHead::Param(param) => word(parameter_letter(param.0)),
            LevelHead::Meta(meta) => decorated(
                vec![("id", value(numeral(meta.0, Sign::Unmarked)))],
                tree(Subterm::Goal),
            ),
        };
        parts.push(match offset {
            0 => atom,
            offset => tree(Subterm::Infix(Infix {
                op: InfixOp::Add,
                left: atom,
                right: numeral(*offset, Sign::Unmarked),
            })),
        });
    }

    match parts.len() {
        1 => parts.remove(0),
        _ => called(word("max"), parts),
    }
}

/// [`level_written`] as text, which a level's own print is.
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
pub fn universes_written(universes: &Universes, frame: Frame) -> Option<CoreValue<Shown>> {
    if universes.is_empty() || frame.spelling.erases_universes() {
        return None;
    }
    let mut fields = Vec::<Decoration>::new();
    if !universes.levels.is_empty() {
        fields.push((
            "levels",
            bracketed(
                universes
                    .levels
                    .iter()
                    .map(|level| value(level_written(level)))
                    .collect(),
            ),
        ));
    }
    if !universes.qualities.is_empty() {
        fields.push((
            "qualities",
            bracketed(
                universes
                    .qualities
                    .iter()
                    .map(|quality| value(quality_written(quality)))
                    .collect(),
            ),
        ));
    }

    Some(nested(fields))
}

/// A declaration's scheme as a struct-valued field's value, or `None` where it is the empty one.
pub fn universe_context_written(context: &UniverseContext) -> Option<CoreValue<Shown>> {
    let mut fields = Vec::<Decoration>::new();
    if context.parameter_count != 0 {
        fields.push((
            "parameter_count",
            value(numeral(context.parameter_count, Sign::Unmarked)),
        ));
    }
    if context.quality_count != 0 {
        fields.push((
            "quality_count",
            value(numeral(context.quality_count, Sign::Unmarked)),
        ));
    }
    if !context.constraints.is_empty() {
        fields.push((
            "constraints",
            bracketed(
                context
                    .constraints
                    .iter()
                    .map(|constraint| {
                        value(tree(Subterm::Infix(Infix {
                            op: InfixOp::Le,
                            left: level_written(&constraint.lower),
                            right: level_written(&constraint.upper),
                        })))
                    })
                    .collect(),
            ),
        ));
    }

    (!fields.is_empty()).then(|| nested(fields))
}

/// The sort a sort's word builds — `Type` and `Prop` at level 0, `Sort` at the first quality its declaration leaves to its caller — with the level and the quality its decorator sets where they differ.
pub fn sort_read(quality: Quality, fields: &mut Fields<'_>) -> Result<Term, ReadError> {
    let quality = match fields.take("quality") {
        Some(value) => quality_read(value)?,
        None => quality,
    };
    let level = match fields.take("level") {
        Some(value) => level_read(&value_term(value)?)?,
        None => Level::zero(),
    };

    Ok(Term::sort_at(quality, level))
}

/// A quality by the name Core gives it: `Type`, `Prop`, `Param(n)` or `Meta(n)`.
pub fn quality_read(value: &CoreValue) -> Result<Quality, ReadError> {
    // `Type` and `Prop` are the words a sort is written by, read as sorts by the grammar.
    if let CoreValue::Term(term) = value {
        match term.as_subterm() {
            Subterm::Type => return Ok(Quality::Type),
            Subterm::Prop => return Ok(Quality::Prop),
            _ => {}
        }
    }
    let (word, held) = value_variant(value)?;
    let index = || match held.as_slice() {
        [index] => term_number(index)
            .and_then(|index| usize::try_from(index).ok())
            .ok_or_else(|| malformed("a quality's index", value)),
        _ => Err(malformed("a quality's index", value)),
    };
    match word {
        "Type" if held.is_empty() => Ok(Quality::Type),
        "Prop" if held.is_empty() => Ok(Quality::Prop),
        "Param" => Ok(Quality::Param(QualityParam(index()?))),
        "Meta" => Ok(Quality::Meta(QualityMetaId(index()?))),
        _ => Err(malformed(
            "a quality: `Type`, `Prop`, `Param(n)` or `Meta(n)`",
            value,
        )),
    }
}

/// A level as the term it is: a constant, a parameter by its letter, an unsolved level `#{id = n} ?`, each under an offset, `u + 1`, and several as their `max`, which names each head and the constant at most once, as the printer writes one.
pub fn level_read(term: &crate::Term<CoreText>) -> Result<Level, ReadError> {
    let malformed = || {
        ReadError::at(
            Refusal::Malformed(
                "a level: a numeral, a parameter's letter, `#{id = n} ?`, an offset or a `max`",
            ),
            term.span(),
        )
    };
    let parts = match term.as_subterm() {
        Subterm::Apply(Apply { head, arguments }) if term_word(head) == Some("max") => arguments
            .iter()
            .map(|argument| match argument.plicity {
                Plicity::Explicit => Ok(&argument.term),
                _ => Err(malformed()),
            })
            .collect::<Result<Vec<_>, _>>()?,
        _ => vec![term],
    };
    let mut level = Level::zero();
    let mut constant = false;
    for part in parts {
        let repeated = match level_part(part).ok_or_else(malformed)? {
            LevelPart::Constant(value) => {
                level.constant = value;
                let repeated = constant;
                constant = true;
                repeated.then(|| "a constant".to_string())
            }
            LevelPart::Atom(head, offset) => level.atoms.insert(head, offset).map(|_| match head {
                LevelHead::Param(param) => format!("`{}`", parameter_letter(param.0)),
                LevelHead::Meta(meta) => format!("`#{{id = {}}} ?`", meta.0),
            }),
        };
        if let Some(repeated) = repeated {
            return Err(ReadError::at(
                Refusal::RepeatedLevelPart(repeated),
                part.span(),
            ));
        }
    }

    Ok(level)
}

/// One part of a level.
enum LevelPart {
    Constant(u32),
    Atom(LevelHead, u32),
}

/// One part of a level as it is written: a numeral, or an atom under its offset.
fn level_part(term: &crate::Term<CoreText>) -> Option<LevelPart> {
    if let Some(constant) = term_number(term) {
        return u32::try_from(constant).ok().map(LevelPart::Constant);
    }
    let (atom, offset) = match term.as_subterm() {
        Subterm::Infix(Infix {
            op: InfixOp::Add,
            left,
            right,
        }) => (left, u32::try_from(term_number(right)?).ok()?),
        _ => (term, 0),
    };

    Some(LevelPart::Atom(level_head(atom)?, offset))
}

/// The head of a level's atom: a parameter by its letter, `u` to `z` and then numbered, or an unsolved level, `#{id = n} ?`.
fn level_head(term: &crate::Term<CoreText>) -> Option<LevelHead> {
    if let Some(word) = term_word(term) {
        return parameter_index(word).map(|index| LevelHead::Param(UniverseParam(index)));
    }
    let Subterm::Core(CoreForm::Decorated(Decorated { fields, term: goal })) = term.as_subterm()
    else {
        return None;
    };
    let ([field], Subterm::Goal) = (fields.as_slice(), goal.as_subterm()) else {
        return None;
    };
    if field.label.as_str() != "id" {
        return None;
    }
    let CoreValue::Term(id) = &field.value else {
        return None;
    };

    Some(LevelHead::Meta(UniverseMetaId(
        usize::try_from(term_number(id)?).ok()?,
    )))
}

/// The parameter `word` is the letter of: `u` to `z`, then the same letters numbered from 1, as [`parameter_letter`] writes them.
fn parameter_index(word: &str) -> Option<usize> {
    const LETTERS: usize = (b'z' - b'u' + 1) as usize;
    let mut characters = word.chars();
    let letter = characters.next()?;
    if !('u'..='z').contains(&letter) {
        return None;
    }
    let offset = letter as usize - 'u' as usize;
    let cycle = match characters.as_str() {
        "" => 0,
        digits if !digits.starts_with('0') => digits.parse::<usize>().ok()?,
        _ => return None,
    };

    Some(cycle * LETTERS + offset)
}

/// An instance's universes as a struct-valued field holds them: `{levels = […], qualities = […]}`, each part empty where it is left out.
pub fn universes_read(value: &CoreValue) -> Result<Universes, ReadError> {
    let mut fields = value_fields(value)?;
    let levels = match fields.take("levels") {
        Some(levels) => value_unmarked(levels)?
            .into_iter()
            .map(|level| level_read(&value_term(level)?))
            .collect::<Result<_, _>>()?,
        None => Vec::new(),
    };
    let qualities = match fields.take("qualities") {
        Some(qualities) => value_unmarked(qualities)?
            .into_iter()
            .map(quality_read)
            .collect::<Result<_, _>>()?,
        None => Vec::new(),
    };
    fields.finish()?;

    Ok(Universes { levels, qualities })
}

/// A declaration's scheme as a struct-valued field holds it: `{parameter_count = 2, quality_count = 1, constraints = [u <= v + 1]}`, each part empty where it is left out, and a constraint's origin left empty.
pub fn universe_context_read(value: &CoreValue) -> Result<UniverseContext, ReadError> {
    let mut fields = value_fields(value)?;
    let count = |value: Option<&CoreValue>| match value {
        Some(value) => value_number(value)
            .and_then(|count| usize::try_from(count).map_err(|_| malformed("a count", value))),
        None => Ok(0),
    };
    let parameter_count = count(fields.take("parameter_count"))?;
    let quality_count = count(fields.take("quality_count"))?;
    let constraints = match fields.take("constraints") {
        Some(constraints) => value_unmarked(constraints)?
            .into_iter()
            .map(|constraint| {
                let term = value_term(constraint)?;
                match term.as_subterm() {
                    Subterm::Infix(Infix {
                        op: InfixOp::Le,
                        left,
                        right,
                    }) => Ok(UniverseConstraint {
                        lower: level_read(left)?,
                        upper: level_read(right)?,
                        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Other(
                            String::new(),
                        )),
                    }),
                    _ => Err(malformed("a constraint, `lower <= upper`", constraint)),
                }
            })
            .collect::<Result<_, _>>()?,
        None => Vec::new(),
    };
    fields.finish()?;

    Ok(UniverseContext {
        parameter_count,
        quality_count,
        constraints,
    })
}
