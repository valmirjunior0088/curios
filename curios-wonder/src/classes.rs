//! What a check leaves of a file, classed: each span the compiler knows the class of ([`Classed`]) — a name by what its declaration is declared as, a binder by what it binds, a field's label, a segment of a path reaching a module — which `wonder --classes` lists and the server encodes as semantic tokens. Why the server lays them over a grammar's guess is `documentation/design/tools/curios-is-coloured-from-one-class-set.md`'s.
//!
//! **A span is classed as what stands there.** Each record of a text proposes the span it would be painted over — a name its path's last segment, since each segment before it is answered on its own — and the span is classed by what [`Placed::answer`] answers at it, so a span and the hover over it never disagree, two records proposing one span are one row, and no two rows overlap. What no record answers is left to the grammar.

use {
    crate::{Diagnosed, Diagnosis, Placed, Stands, Subject, checked, unlowered},
    curios_common::{Class, Span},
    curios_module::Overlay,
    curios_pipeline::Progress,
    curios_verdict::Verdicts,
    std::{collections::BTreeMap, path::Path},
};

/// One span the compiler classes: where it lies, what it is painted as, and whether it is where what it names is declared.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Classed {
    pub span: Span,
    pub class: Class,
    /// Whether the span is where what it names is declared: a declaration's own name, a binder where it is bound, a field's label where its structure writes it, and a module's name on the `mod` line that declares it.
    pub declaration: bool,
}

impl Placed {
    /// Every span of the file this check classes, in the order they are written ([`Classed`]).
    pub fn classes(&self) -> Vec<Classed> {
        let mut classed = BTreeMap::new();
        for written in &self.texts {
            let proposed = written
                .names
                .iter()
                .map(|named| last_segment(&named.written))
                .chain(written.modules.iter().map(|(span, _)| span.clone()))
                .chain(written.fields.iter().map(|field| field.written.clone()))
                .chain(written.locals.iter().map(|local| local.written.clone()));
            for span in proposed {
                let Some(answer) = self.answer(written, span.start) else {
                    continue;
                };
                let covers = answer.range.start <= span.start && span.end <= answer.range.end;
                let Some(class) = answer.class().filter(|_| covers) else {
                    continue;
                };
                let declaration = match &answer.stands {
                    Stands::Declaration { declares, .. }
                    | Stands::Local { declares, .. }
                    | Stands::Field { declares, .. } => *declares,
                    Stands::Module { place, .. } => place.as_ref() == Some(&answer.range),
                    Stands::Term { .. } => false,
                };
                classed.entry((span.start, span.end)).or_insert(Classed {
                    span,
                    class,
                    declaration,
                });
            }
        }

        classed.into_values().collect()
    }
}

/// The last segment of the path `span` writes: the one naming what the path reaches, each segment before it being answered on its own.
fn last_segment(span: &Span) -> Span {
    let written = &span.source.text()[span.start..span.end];
    let start = written
        .rfind('/')
        .map_or(span.start, |slash| span.start + slash + 1);

    Span::new(span.source.clone(), start, span.end)
}

/// Every span of `file` the check of `subject` classes ([`Placed::classes`]), read off that check as [`placed`](crate::placed) reads a place: a file that does not elaborate classes what its lowering answers, as one that does. `overlay` and `cache` behave as they do for `diagnostics`.
///
/// The refusal is a subject that has nothing to read: one that does not parse or lower, Core text among them, or whose scope could not be compiled.
pub fn classed(
    budget: u64,
    subject: Subject,
    file: &Path,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Vec<Classed>, Vec<Diagnosis>> {
    let subject = subject.formed(overlay);
    if let Subject::CoreText { .. } = subject {
        return Err(unlowered());
    }
    let Diagnosed {
        diagnostics,
        placed,
        ..
    } = checked(budget, subject, overlay, cache, file, progress);

    placed.map(|placed| placed.classes()).ok_or(diagnostics)
}
