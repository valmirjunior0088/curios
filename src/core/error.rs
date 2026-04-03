use super::Term;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Preempted;

#[derive(Debug)]
pub enum Error {
    ReducePreempted { term: Term },
    ConvertPreempted { this: Term, that: Term },
    CannotInfer { term: Term },
    TypeMismatch { term: Term, type_: Term },
}

impl Error {
    pub fn reduce_preempted<T>(term: T) -> Self
    where
        T: Into<Term>,
    {
        Self::ReducePreempted { term: term.into() }
    }

    pub fn convert_preempted<T, U>(this: T, that: U) -> Self
    where
        T: Into<Term>,
        U: Into<Term>,
    {
        Self::ConvertPreempted {
            this: this.into(),
            that: that.into(),
        }
    }

    pub fn cannot_infer<T>(term: T) -> Self
    where
        T: Into<Term>,
    {
        Self::CannotInfer { term: term.into() }
    }

    pub fn type_mismatch<T, U>(term: T, type_: U) -> Self
    where
        T: Into<Term>,
        U: Into<Term>,
    {
        Self::TypeMismatch {
            term: term.into(),
            type_: type_.into(),
        }
    }
}
