//! Quality metavariables: which of `Prop` and `Type` a sort left to inference is.
//!
//! A sort's quality is solved by unification where its level is solved by constraints: two sorts compared are one quality or a refusal, since no proposition stands where a type is wanted. A metavariable is solved to `Prop`, to `Type`, to a declaration's parameter, or to another metavariable it was identified with, and every reader follows the chain to its end ([`UniverseSolver::quality`]). The solutions are journaled beside the levels', so a speculative branch that is rolled back takes its qualities with it.

use {
    super::UniverseSolver,
    curios_core::{Quality, QualityMetaId, UniverseConstraintOrigin},
};

/// One quality metavariable: what it was solved to, and where it was minted.
#[derive(Debug, Clone)]
pub(super) struct QualityMeta {
    pub(super) solution: Option<Quality>,
    pub(super) origin: Option<UniverseConstraintOrigin>,
}

impl UniverseSolver {
    /// Mint the quality of a sort left to inference: an unannotated binder's type's, an elided motive's result's, a parked term's.
    pub fn fresh_quality(&mut self, origin: Option<UniverseConstraintOrigin>) -> Quality {
        let id = QualityMetaId(self.qualities.len());
        self.qualities.push(QualityMeta {
            solution: None,
            origin,
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

    /// Hold two qualities one: an unsolved one is solved to the other, and two that each state a sort are one where they state the same. Answers whether they are held one.
    pub fn unify_qualities(&mut self, this: Quality, that: Quality) -> bool {
        match (self.quality(this), self.quality(that)) {
            (this, that) if this == that => true,
            (Quality::Meta(meta), other) | (other, Quality::Meta(meta)) => {
                self.qualities[meta.0].solution = Some(other);
                self.quality_log.push(meta);
                true
            }
            _ => false,
        }
    }

    /// Every quality metavariable as far as it is solved, by its index: what a walk over a term reads each sort's quality from. One still unsolved is itself, or `Type` where `settled`, which is what a quality nothing decides closes as and what a report shows of it.
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

    /// Leave unsolved every quality solved since the journal was `len` long: the half of a rollback that is the qualities'.
    pub(super) fn unsolve_qualities(&mut self, len: usize) {
        while self.quality_log.len() > len {
            let meta = self
                .quality_log
                .pop()
                .expect("the journal is longer than its mark");
            self.qualities[meta.0].solution = None;
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
