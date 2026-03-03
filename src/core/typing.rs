use super::{Context, Term};

#[derive(Debug)]
pub enum Error {
    ReducePreempted { term: Term },
    ConvertPreempted { this: Term, that: Term },
    CannotInfer { term: Term },
    TypeMismatch { term: Term, type_: Term },
}

impl Error {
    pub fn reduce_preempted(term: &Term) -> Self {
        Self::ReducePreempted { term: term.clone() }
    }

    pub fn convert_preempted(this: &Term, that: &Term) -> Self {
        Self::ConvertPreempted {
            this: this.clone(),
            that: that.clone(),
        }
    }

    pub fn cannot_infer(term: impl Into<Term>) -> Self {
        Self::CannotInfer { term: term.into() }
    }

    pub fn type_mismatch(term: &Term, type_: &Term) -> Self {
        Self::TypeMismatch {
            term: term.clone(),
            type_: type_.clone(),
        }
    }
}

pub fn reduce(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term).map_err(|super::Preempted| Error::reduce_preempted(term))
}

pub fn convert(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, this, that)
        .map_err(|super::Preempted| Error::convert_preempted(this, that))
}
