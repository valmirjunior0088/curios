//! A match as it is written: the form lowering hands elaboration before a pattern is compiled.
//!
//! Nothing here judges, reduces or binds. A [`Matrix`] is plain data over terms — its scrutinee, its motive, its rows' bodies and what it falls through to — and a row's binders are its body's own: the body is an ordinary function over them, as a [`Bang`](crate::Bang)'s continuation is, so no walk of Core learns a binder from a pattern. A [`Pattern`] says where each of those binders stands, counted in the order the pattern writes them.

use {
    crate::{Atom, Term},
    curios_num::{Grain, Natural},
    curios_utilities::Plicity,
};

/// A match before its patterns are compiled: the scrutinee, the motive where one is written, and the arms in the order they are written. Consumed by `elaborate_matrix`, which compiles the patterns into the single-level [`Match`](crate::Match) forms every later stage reads; the node never survives elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Matrix {
    pub head: Term,
    /// The motive as it is written, and `None` where no `:` is.
    pub motive: Option<Term>,
    pub rows: Vec<Row>,
    /// What a value no row matches falls through to: the rest of a `choose` ladder past a bind arm. A headed match has none, and writes a default as its last row.
    pub fallthrough: Option<Term>,
}

impl Matrix {
    /// The direct child terms: the scrutinee, the motive, each row's body and the fallthrough.
    pub fn subterms(&self) -> impl Iterator<Item = &Term> {
        std::iter::once(&self.head)
            .chain(&self.motive)
            .chain(self.rows.iter().map(|row| &row.body))
            .chain(&self.fallthrough)
    }

    /// This matrix with every child term mapped, its patterns as they stand.
    pub fn map_subterms(&self, f: &mut impl FnMut(&Term) -> Term) -> Matrix {
        Matrix {
            head: f(&self.head),
            motive: self.motive.as_ref().map(&mut *f),
            rows: self
                .rows
                .iter()
                .map(|row| Row {
                    pattern: row.pattern.clone(),
                    body: f(&row.body),
                })
                .collect(),
            fallthrough: self.fallthrough.as_ref().map(&mut *f),
        }
    }
}

/// One arm of a [`Matrix`]: its pattern, and its body as a function over the binders the pattern writes, in the order it writes them, placed where the pattern is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Row {
    pub pattern: Pattern,
    pub body: Term,
}

/// A match arm's pattern as it is written. A constructor is the tag it is spelled by, since the scrutinee's type supplies its namespace, and a binder is a place among its row's binders: a pattern's binders are counted left to right, a successor's predecessor and a cons's head and tail ahead of the hypothesis they stand beside.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived(recursive)]
pub enum Pattern {
    /// A name or `_`.
    Binder,
    /// A constructor over one pattern for each payload it writes, each under the mark it is written under.
    Variant {
        tag: Atom,
        #[archived_omit_bounds]
        args: Vec<(Plicity, Pattern)>,
    },
    /// A tuple pattern, each field under the label it is written with.
    Tuple(#[archived_omit_bounds] Vec<(Option<String>, Pattern)>),
    /// A struct pattern, under the head it is written with.
    Struct {
        head: String,
        #[archived_omit_bounds]
        fields: Vec<(Option<String>, Pattern)>,
    },
    Bool(bool),
    /// `0`.
    Zero,
    /// `predecessor + 1`, with the hypothesis written beside it where one is.
    Succ(Option<Bind>),
    /// A dispatch literal spelled as a numeral.
    Literal(Natural),
    /// A dispatch literal spelled as the character whose scalar value it is.
    Character(char),
    /// `[]`.
    Nil,
    /// `[head, ..tail]`, with the hypothesis written beside it where one is.
    Cons(Option<Bind>),
    /// `b[]` or `x[]`.
    End(Grain),
    /// `b[head, ..tail]` or `x[head, ..tail]`, with the hypothesis written beside it where one is.
    Atom(Grain, Option<Bind>),
}

impl Pattern {
    /// How many binders the pattern writes.
    pub fn binders(&self) -> usize {
        match self {
            Pattern::Binder => 1,
            Pattern::Variant { args, .. } => args.iter().map(|(_, arg)| arg.binders()).sum(),
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                fields.iter().map(|(_, field)| field.binders()).sum()
            }
            Pattern::Bool(_)
            | Pattern::Zero
            | Pattern::Literal(_)
            | Pattern::Character(_)
            | Pattern::Nil
            | Pattern::End(_) => 0,
            Pattern::Succ(hypothesis) => 1 + hypothesis.as_ref().map_or(0, Bind::binders),
            Pattern::Cons(hypothesis) | Pattern::Atom(_, hypothesis) => {
                2 + hypothesis.as_ref().map_or(0, Bind::binders)
            }
        }
    }

    /// Whether the pattern dispatches on a case of its scrutinee: a constructor or a carrier's leaf, where a binder takes any value and a tuple or struct pattern only projects.
    pub fn dispatches(&self) -> bool {
        !matches!(
            self,
            Pattern::Binder | Pattern::Tuple(_) | Pattern::Struct { .. }
        )
    }
}

/// What a fold's hypothesis is bound by: a name, or a tuple or struct pattern whose leaves take its projections.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived(recursive)]
pub enum Bind {
    Binder,
    Fields(#[archived_omit_bounds] Vec<(Option<String>, Bind)>),
}

impl Bind {
    /// How many binders it writes.
    pub fn binders(&self) -> usize {
        match self {
            Bind::Binder => 1,
            Bind::Fields(fields) => fields.iter().map(|(_, field)| field.binders()).sum(),
        }
    }
}
