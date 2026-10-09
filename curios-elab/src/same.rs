//! Which parts of a declaration elaborated again are the parts its baseline holds, as a unit stores them.
//!
//! A declaration kept over a baseline asked parts of the declarations around it, and stands where each is as it was; so does an erased form, by what its erasure asked. Of a declaration that was itself kept, every part is. Of one elaborated again, a part is where it came out equal — which is what lets an edit stop at the declarations that read what it changed: a body respelled to the same signature moves no declaration that read the signature alone, and a proof proved another way moves none that asked what it unfolds to, which is nothing whatever its body ([`curios_core::Definition::proof`]).
//!
//! **Equal as stored, so by spelling.** A binder's name moves no judgment, and a term's equality reads none; it is in what a unit stores all the same, the declaration's own and a dependent's that was handed its signature. So a part is compared under its binders' names ([`Term::equal_modulo_metas`], over terms that hold no metavariable), and a parameter renamed is a signature changed. A position is in no comparison: a declaration moved, or respaced, has moved no part.
//!
//! **A group is compared whole.** Its members share one knot, each one's type and body scoped over all of them, so a member of a group that moved anywhere has moved.

#[cfg(test)]
mod tests;

use {
    crate::Question,
    curios_core::{
        Bound, ConceptDecl, Definition, DefinitionKind, InductDecl, Item, MetaRenaming, RecItem,
        StructDecl, Telescope, Term,
    },
};

/// Which parts of one name elaborated again came out as the baseline holds them: what a kept declaration's question about the name is answered by.
#[derive(Clone, Copy, Default)]
pub(crate) struct Same {
    signature: bool,
    body: bool,
    kind: bool,
    totality: bool,
    variances: bool,
    induct: bool,
    struct_: bool,
    concept: bool,
}

impl Same {
    /// The parts of the definition at `member` of `made` that are those of `held`, the baseline's item under the same name. The registry entries the name keys are the caller's to compare ([`Same::keying`]).
    pub(crate) fn of(made: &Item, held: &Item, member: usize) -> Self {
        match (made, held) {
            (Item::Let(made), Item::Let(held)) => Self::of_definition(made, held),
            (Item::Rec(made), Item::Rec(held)) => Self::of_member(made, held, member),
            _ => Self::default(),
        }
    }

    fn of_definition(made: &Definition, held: &Definition) -> Self {
        let kind = made.name == held.name
            && kinds_alike(&made.kind, &held.kind)
            && made.island == held.island;
        let signature = kind
            && made.universe_context == held.universe_context
            && spelled_alike(&made.type_, &held.type_);
        let totality = made.totality == held.totality;

        Self {
            signature,
            // What a name unfolds to is read with what it is and whether it is total. A proof unfolds to nothing, so it moves with the mark and never with the body.
            body: signature
                && totality
                && made.proof == held.proof
                && (made.proof || spelled_alike(&made.body, &held.body)),
            kind,
            totality,
            variances: made.variances == held.variances,
            ..Self::default()
        }
    }

    fn of_member(made: &RecItem, held: &RecItem, member: usize) -> Self {
        let (Some(mine), Some(theirs)) =
            (made.definitions.get(member), held.definitions.get(member))
        else {
            return Self::default();
        };
        let kind = mine.name == theirs.name
            && kinds_alike(&mine.kind, &theirs.kind)
            && mine.island == theirs.island;
        let group = kind && groups_alike(made, held);
        let totality = mine.totality == theirs.totality;

        Self {
            signature: group,
            body: group && totality && mine.proof == theirs.proof,
            kind,
            totality,
            // A group's member files no vector.
            variances: true,
            ..Self::default()
        }
    }

    /// These parts, with whether each registry entry the name keys is the baseline's.
    pub(crate) fn keying(self, induct: bool, struct_: bool, concept: bool) -> Self {
        Self {
            induct,
            struct_,
            concept,
            ..self
        }
    }

    /// Whether the part `question` asks is as the baseline holds it.
    pub(crate) fn answers(&self, question: &Question) -> bool {
        match question {
            Question::Signature(_) => self.signature,
            Question::Body(_) => self.body,
            Question::Kind(_) => self.kind,
            Question::Totality(_) => self.totality,
            Question::Variances(_) => self.variances,
            Question::Induct(_) => self.induct,
            Question::Struct(_) => self.struct_,
            Question::Concept(_) => self.concept,
            Question::Witness(..)
            | Question::WitnessDeclared(..)
            | Question::Lemmas
            | Question::Erased(_) => false,
        }
    }
}

/// Whether two kinds say one thing of a name, the entry a type former carries left out. An entry is a part of its own, compared as the context holds it ([`Same::keying`]): the one a former made again carries is the lowering's until its item leaves the walk, and a former held to it would have moved for every declaration that read its signature.
fn kinds_alike(this: &DefinitionKind, that: &DefinitionKind) -> bool {
    match (this, that) {
        (DefinitionKind::Nominal(_), DefinitionKind::Nominal(_)) => {
            this.declared() == that.declared()
        }
        _ => this == that,
    }
}

/// Whether two finished terms are one as a unit stores them: equal, each binder under the name it was written with.
fn spelled_alike(this: &Term, that: &Term) -> bool {
    this.equal_modulo_metas(that, &mut MetaRenaming::default())
}

/// Whether two groups are one as a unit stores them: one scheme, and each member's type and body spelled alike.
fn groups_alike(this: &RecItem, that: &RecItem) -> bool {
    this.group.universe_context() == that.group.universe_context()
        && this.group.iter().len() == that.group.iter().len()
        && this
            .group
            .iter()
            .zip(that.group.iter())
            .all(|(this, that)| {
                this.type_.arity() == that.type_.arity()
                    && this.body.arity() == that.body.arity()
                    && spelled_alike(this.type_.body(), that.type_.body())
                    && spelled_alike(this.body.body(), that.body.body())
            })
}

/// Whether a name keys one registry entry on both sides, compared by `alike`, or none on either.
pub(crate) fn entries_alike<D>(
    this: Option<&D>,
    that: Option<&D>,
    alike: impl FnOnce(&D, &D) -> bool,
) -> bool {
    match (this, that) {
        (None, None) => true,
        (Some(this), Some(that)) => alike(this, that),
        _ => false,
    }
}

pub(crate) fn inducts_alike(this: &InductDecl, that: &InductDecl) -> bool {
    this.universe_context == that.universe_context
        && this.module == that.module
        && this.rep_public == that.rep_public
        && this.polarities == that.polarities
        && this.variances == that.variances
        && this.constructors.len() == that.constructors.len()
        && arities_alike(&this.arity, &that.arity)
        && spelled_alike(&this.result_sort, &that.result_sort)
        && this.constructors.iter().zip(&that.constructors).all(
            |((this_tag, this_payload), (that_tag, that_payload))| {
                this_tag == that_tag
                    && telescopes_alike(
                        &this_payload.telescope,
                        &that_payload.telescope,
                        &mut |these: &Vec<Term>, those: &Vec<Term>| {
                            these.len() == those.len()
                                && these
                                    .iter()
                                    .zip(those)
                                    .all(|(this, that)| spelled_alike(this, that))
                        },
                    )
            },
        )
}

pub(crate) fn structs_alike(this: &StructDecl, that: &StructDecl) -> bool {
    this.universe_context == that.universe_context
        && this.module == that.module
        && this.rep_public == that.rep_public
        && this.polarities == that.polarities
        && this.variances == that.variances
        && arities_alike(&this.arity, &that.arity)
        && spelled_alike(&this.result_sort, &that.result_sort)
}

pub(crate) fn concepts_alike(this: &ConceptDecl, that: &ConceptDecl) -> bool {
    this.universe_context == that.universe_context
        && this.fields == that.fields
        && this.supers == that.supers
        && fields_alike(&this.params, &that.params)
}

fn fields_alike(this: &Telescope<()>, that: &Telescope<()>) -> bool {
    telescopes_alike(this, that, &mut |_: &(), _: &()| true)
}

fn arities_alike(this: &Telescope<Telescope<()>>, that: &Telescope<Telescope<()>>) -> bool {
    telescopes_alike(
        this,
        that,
        &mut |these: &Telescope<()>, those: &Telescope<()>| fields_alike(these, those),
    )
}

/// Whether two telescopes agree entry by entry — each under the same mark and the same name, its type spelled alike — ending in payloads `done` agrees on. A mark is part of what an entry declares: a parameter's decides how a call writes it, and a field's what a literal leaves out and what a position counts.
fn telescopes_alike<B: Bound>(
    this: &Telescope<B>,
    that: &Telescope<B>,
    done: &mut dyn FnMut(&B, &B) -> bool,
) -> bool {
    let (mut this, mut that) = (this, that);
    loop {
        match (this, that) {
            (
                Telescope::Cons(this_mark, this_type, this_rest),
                Telescope::Cons(that_mark, that_type, that_rest),
            ) => {
                if this_mark != that_mark
                    || this_rest.arity() != that_rest.arity()
                    || !this_rest.spelled_as(that_rest)
                    || !spelled_alike(this_type, that_type)
                {
                    return false;
                }
                this = this_rest.body();
                that = that_rest.body();
            }
            (Telescope::Done(this), Telescope::Done(that)) => return done(this, that),
            _ => return false,
        }
    }
}
