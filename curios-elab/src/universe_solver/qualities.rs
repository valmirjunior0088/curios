//! Quality metavariables: which of `Prop` and `Type` a sort left to inference is, and which sorts a declaration leaves to its caller.
//!
//! A sort's quality is solved by unification where its level is solved by constraints: two sorts compared are one quality or a refusal, since no proposition stands where a type is wanted. A metavariable is solved to `Prop`, to `Type`, to a parameter or to another metavariable, and one nothing decides closes as `Type` where its item has drained what it parked.
//!
//! **A written `Sort` closes as a parameter.** The lowering gives each `Sort` a declaration writes a metavariable of its own ([`UniverseSolver::seed_qualities`]), and elaboration checks the declaration with it open: every reader of a sort answers for one not known as it does for one nothing has solved. Where the declaration's levels are finalized, each written quality still open becomes a parameter of the declaration, in the order they were written ([`UniverseSolver::generalize_qualities`]), two that the declaration identified being one. One the declaration solved to `Prop` or to `Type` is no caller's to state, and is refused with what decided it.
//!
//! **A record over two open qualities has none.** A tuple type is a proposition where every field is one and a type where any is not, which over two qualities neither of which is known is neither. Its formation records the pair ([`UniverseSolver::note_quality_join`]) and goes on at `Type`; the pair is refused where the declaration closes with both of them its parameters.

use {
    super::UniverseSolver,
    curios_core::{
        Extent, Quality, QualityMetaId, QualityParam, UniverseConstraintOrigin, UniverseError,
    },
};

/// One quality metavariable: what it was solved to and where, where it was minted, and whether its declaration wrote it.
#[derive(Debug, Clone)]
pub(super) struct QualityMeta {
    pub(super) solution: Option<Quality>,
    /// The term whose check solved it, where conversion was asked about one.
    pub(super) solved_at: Option<Extent>,
    pub(super) origin: Option<UniverseConstraintOrigin>,
    /// Whether the declaration wrote it, as a `Sort`: one that closes as a parameter where nothing of the declaration decides it.
    pub(super) written: bool,
}

/// A `Sort` its declaration wrote and then decided: no caller's to state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecidedSort {
    /// What it can only be: `Prop` or `Type`.
    pub sort: Quality,
    /// Where it was written.
    pub written: Option<Extent>,
    /// The term whose check decided it, where one was recorded.
    pub decided: Option<Extent>,
}

impl UniverseSolver {
    /// Seed the qualities of the `Sort`s a declaration writes, by where each stands: the first metavariables of a fresh solver, as the lowered terms number them.
    pub fn seed_qualities(&mut self, written: &[Option<UniverseConstraintOrigin>]) {
        assert!(
            self.qualities.is_empty(),
            "qualities may only seed a fresh elaboration context"
        );
        self.qualities = written
            .iter()
            .map(|origin| QualityMeta {
                solution: None,
                solved_at: None,
                origin: origin.clone(),
                written: true,
            })
            .collect();
    }

    /// Mint the quality of a sort left to inference: an unannotated binder's type's, an elided motive's result's, a parked term's, a sort a use of another declaration states.
    pub fn fresh_quality(&mut self, origin: Option<UniverseConstraintOrigin>) -> Quality {
        let id = QualityMetaId(self.qualities.len());
        self.qualities.push(QualityMeta {
            solution: None,
            solved_at: None,
            origin,
            written: false,
        });
        Quality::Meta(id)
    }

    /// `quality` as far as it is solved: `Prop`, `Type` or a parameter where it states one, and the unsolved metavariable its chain ends in otherwise.
    pub fn quality(&self, quality: Quality) -> Quality {
        let mut quality = quality;
        while let Quality::Meta(meta) = quality {
            match self.qualities.get(meta.0).and_then(|entry| entry.solution) {
                Some(solution) => quality = solution,
                None => break,
            }
        }
        quality
    }

    /// Hold two qualities one: an unsolved one is solved to the other, and two that each state a sort are one where they state the same. Answers whether they are held one. `site` is the term whose check asked, where one did: what a `Sort` this decides is reported with.
    pub fn unify_qualities(&mut self, this: Quality, that: Quality, site: Option<Extent>) -> bool {
        match (self.quality(this), self.quality(that)) {
            (this, that) if this == that => true,
            (Quality::Meta(meta), other) | (other, Quality::Meta(meta)) => {
                self.qualities[meta.0].solution = Some(other);
                self.qualities[meta.0].solved_at = site;
                self.quality_log.push(meta);
                true
            }
            _ => false,
        }
    }

    /// Every quality metavariable as far as it is solved, by its index: what a walk over a term reads each sort's quality from. One still unsolved is itself, or `Type` where `settled`, which is what a quality nothing decides closes as and what a report shows of one.
    pub fn quality_table(&self, settled: bool) -> Vec<Quality> {
        (0..self.qualities.len())
            .map(
                |index| match self.quality(Quality::Meta(QualityMetaId(index))) {
                    Quality::Meta(_) if settled => Quality::Type,
                    quality => quality,
                },
            )
            .collect()
    }

    /// Solve every quality nothing decided to `Type`: where an item has drained what it parked, a sort left to inference that no proposition was put under is a type's.
    pub fn default_qualities(&mut self) {
        for index in 0..self.qualities.len() {
            if self.qualities[index].solution.is_none() {
                self.qualities[index].solution = Some(Quality::Type);
                self.quality_log.push(QualityMetaId(index));
            }
        }
    }

    /// Close the qualities the declaration wrote: each still open becomes a parameter of the declaration, in the order they were written, two the declaration identified being one. Answers how many parameters that is, or the first written quality the declaration decided.
    ///
    /// A quality the elaboration invented and identified with a written one is the same parameter: the chain is followed to what it ends in, and that is what is solved.
    pub fn generalize_qualities(&mut self) -> Result<usize, DecidedSort> {
        let mut count = 0;
        for index in 0..self.qualities.len() {
            if !self.qualities[index].written {
                continue;
            }
            match self.quality(Quality::Meta(QualityMetaId(index))) {
                Quality::Meta(open) => {
                    self.qualities[open.0].solution = Some(Quality::Param(QualityParam(count)));
                    self.quality_log.push(open);
                    count += 1;
                }
                Quality::Param(_) => {}
                sort @ (Quality::Prop | Quality::Type) => {
                    return Err(DecidedSort {
                        sort,
                        written: self.qualities[index]
                            .origin
                            .as_ref()
                            .and_then(|origin| origin.extent),
                        decided: self.decided_at(QualityMetaId(index)),
                    });
                }
            }
        }

        Ok(count)
    }

    /// Where the chain `meta` is solved through reached `Prop` or `Type`: the site recorded at the link that states one.
    fn decided_at(&self, meta: QualityMetaId) -> Option<Extent> {
        let mut entry = self.qualities.get(meta.0)?;
        loop {
            match entry.solution? {
                Quality::Meta(next) => entry = self.qualities.get(next.0)?,
                _ => return entry.solved_at,
            }
        }
    }

    /// Hold the qualities an occurrence of a declaration states to be that declaration's own parameters, in order: what a constructor or a method wrapper, issued in its owner's context, makes of the instance its type names the owner at.
    pub fn pin_qualities(&mut self, instance: &[Quality]) -> Result<(), UniverseError> {
        for (index, quality) in instance.iter().enumerate() {
            let parameter = Quality::Param(QualityParam(index));
            match self.quality(*quality) {
                Quality::Meta(open) => {
                    self.qualities[open.0].solution = Some(parameter);
                    self.quality_log.push(open);
                }
                held if held == parameter => {}
                // One already `Prop`, `Type` or another parameter was forced elsewhere, and nothing in the declaration can stand for the owner's parameter in its place.
                _ => return Err(UniverseError::EscapingLevel),
            }
        }

        Ok(())
    }

    /// Record that a record type was formed over fields at `this` and at `that`, two open qualities apart, and taken for a type: where `site` is the record type's own extent.
    pub fn note_quality_join(&mut self, this: Quality, that: Quality, site: Option<Extent>) {
        self.quality_joins.push((this, that, site));
    }

    /// The first record type formed over two qualities that closed as two parameters of the declaration, by its extent: one whose sort no assignment of the two states as the elaboration took it.
    pub fn open_quality_join(&self) -> Option<Option<Extent>> {
        self.quality_joins
            .iter()
            .find(|(this, that, _)| {
                matches!(
                    (self.quality(*this), self.quality(*that)),
                    (Quality::Param(this), Quality::Param(that)) if this != that
                )
            })
            .map(|(_, _, site)| *site)
    }

    /// Leave unsolved every quality solved since the journal was `len` long: the half of a rollback that is the qualities'.
    pub(super) fn unsolve_qualities(&mut self, len: usize) {
        while self.quality_log.len() > len {
            let meta = self
                .quality_log
                .pop()
                .expect("the journal is longer than its mark");
            self.qualities[meta.0].solution = None;
            self.qualities[meta.0].solved_at = None;
        }
    }

    /// Whether any quality was minted: none is where no sort was left to inference, which is most declarations.
    pub fn has_qualities(&self) -> bool {
        !self.qualities.is_empty()
    }

    /// Where the quality metavariable `meta` was minted.
    pub fn quality_origin(&self, meta: QualityMetaId) -> Option<&UniverseConstraintOrigin> {
        self.qualities
            .get(meta.0)
            .and_then(|entry| entry.origin.as_ref())
    }
}
