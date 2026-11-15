//! A match as it is written: the form lowering hands elaboration before a pattern is compiled.
//!
//! Nothing here judges, reduces or binds. A [`Matrix`] is plain data over terms — its scrutinee, its rows' guards and their bodies — and a row's binders are its body's own: the body is an ordinary function over them, as a [`Bang`](crate::Bang)'s continuation is, and each guard a function over the binders written before it, so no walk of Core learns a binder from a pattern. A [`Pattern`] says where each of those binders stands, counted in the order the pattern writes them, and a bind guard's pattern after them.

use {
    crate::{Atom, Mention, Term},
    curios_common::Plicity,
    curios_num::Natural,
    std::iter,
};

/// A match before its patterns are compiled: the scrutinee and the arms in the order they are written. A match with no head is a match over `()` whose rows write `_`. Consumed by `elaborate_matrix`, which compiles the patterns into the single-level [`Match`](crate::Match) forms every later stage reads; the node never survives elaboration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Matrix {
    pub head: Term,
    pub arms: Vec<Arm>,
}

impl Matrix {
    /// The direct child terms: the scrutinee, and each row's guards and body.
    pub fn subterms(&self) -> impl Iterator<Item = &Term> {
        iter::once(&self.head).chain(self.rows().flat_map(|row| {
            row.guards
                .iter()
                .map(Guard::term)
                .chain(iter::once(&row.body))
        }))
    }

    /// Every arm's rows, arm after arm.
    pub fn rows(&self) -> impl Iterator<Item = &Row> {
        self.arms.iter().flat_map(|arm| &arm.alternatives)
    }

    /// This matrix with every child term mapped, its patterns as they stand.
    pub fn map_subterms(&self, f: &mut impl FnMut(&Term) -> Term) -> Matrix {
        Matrix {
            head: f(&self.head),
            arms: self
                .arms
                .iter()
                .map(|arm| Arm {
                    alternatives: arm
                        .alternatives
                        .iter()
                        .map(|row| Row {
                            pattern: row.pattern.clone(),
                            guards: row
                                .guards
                                .iter()
                                .map(|guard| guard.map_term(&mut *f))
                                .collect(),
                            body: f(&row.body),
                        })
                        .collect(),
                })
                .collect(),
        }
    }
}

/// One arm of a [`Matrix`]: a row for each alternative it writes, in the order it writes them. Each row's body is the arm's body lowered under that alternative's names, so each is typed apart, under its own cases, with the metavariables its own lowering minted.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Arm {
    pub alternatives: Vec<Row>,
}

/// One alternative of an [`Arm`]: its pattern, the arm's guards, and the arm's body as a function over the binders the pattern writes and then those its bind guards write, in the order they are written, placed where the pattern is written.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Row {
    pub pattern: Pattern,
    /// The guards the arm writes after its patterns, in the order they are written: the row is taken where its pattern matches and each passes.
    pub guards: Vec<Guard>,
    pub body: Term,
}

impl Row {
    /// How many binders the row writes: its pattern's, then each bind guard's.
    pub fn binders(&self) -> usize {
        self.pattern.binders() + self.guards.iter().map(Guard::binders).sum::<usize>()
    }
}

/// A guard an arm writes after its patterns, each a function over the binders written before it: the row's pattern's, then those of the bind guards before it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Guard {
    /// A `Bool`, which passes where it is `true`.
    Holds(Term),
    /// A value matched against a pattern, which passes where it matches; the pattern's binders follow the ones written before it.
    Binds { pattern: Pattern, value: Term },
}

impl Guard {
    /// The term the guard tests: its condition, or the value its pattern matches.
    pub fn term(&self) -> &Term {
        match self {
            Guard::Holds(condition) => condition,
            Guard::Binds { value, .. } => value,
        }
    }

    /// How many binders the guard writes.
    pub fn binders(&self) -> usize {
        match self {
            Guard::Holds(_) => 0,
            Guard::Binds { pattern, .. } => pattern.binders(),
        }
    }

    /// This guard with its term mapped, its pattern as it stands.
    pub fn map_term(&self, f: &mut impl FnMut(&Term) -> Term) -> Guard {
        match self {
            Guard::Holds(condition) => Guard::Holds(f(condition)),
            Guard::Binds { pattern, value } => Guard::Binds {
                pattern: pattern.clone(),
                value: f(value),
            },
        }
    }
}

/// A match arm's pattern as it is written. A constructor is the tag it is spelled by, since the scrutinee's type supplies its namespace, and a binder is a place among its row's binders: a pattern's binders are counted left to right, a successor's predecessor and a cons's head and then its tail. A `_` is a wildcard and no binder, so a row binds nothing for it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived(recursive)]
pub enum Pattern {
    /// A name, a place among its row's binders.
    Binder,
    /// `_`, which takes nothing apart and binds nothing: no place among its row's binders stands for it.
    Wildcard,
    /// A constructor over one pattern for each payload it writes, each under the mark it is written under.
    Variant {
        tag: Atom,
        #[archived_omit_bounds]
        args: Vec<(Plicity, Pattern)>,
    },
    /// A tuple pattern, each field under the label it is written with.
    Tuple(#[archived_omit_bounds] Vec<(Option<String>, Pattern)>),
    /// A struct pattern: the head it is written with, which a report prints, and the structure that head resolved to, or the binder it lowered to where it names none.
    Struct {
        head: String,
        structure: Mention,
        #[archived_omit_bounds]
        fields: Vec<(Option<String>, Pattern)>,
    },
    Bool(bool),
    /// `0`.
    Zero,
    /// `predecessor + 1`.
    Succ {
        predecessor: Slot,
    },
    /// A dispatch literal spelled as a numeral.
    Literal(Natural),
    /// A dispatch literal spelled as the character whose scalar value it is.
    Character(char),
    /// `[]`.
    Nil,
    /// `[head, ..tail]`.
    Cons {
        head: Slot,
        tail: Slot,
    },
    /// `x[]`.
    End,
    /// `x[head, ..tail]`.
    Atom {
        head: Slot,
        tail: Slot,
    },
}

/// A part a successor or a sequence's case always holds, its predecessor, its head or its tail, as a pattern writes it: a name, a place among its row's binders, or `_`, which binds nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Slot {
    Binder,
    Wildcard,
}

impl Slot {
    /// How many binders it writes.
    pub fn binders(self) -> usize {
        match self {
            Slot::Binder => 1,
            Slot::Wildcard => 0,
        }
    }
}

impl Pattern {
    /// How many binders the pattern writes.
    pub fn binders(&self) -> usize {
        match self {
            Pattern::Binder => 1,
            Pattern::Wildcard => 0,
            Pattern::Variant { args, .. } => args.iter().map(|(_, arg)| arg.binders()).sum(),
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                fields.iter().map(|(_, field)| field.binders()).sum()
            }
            Pattern::Bool(_)
            | Pattern::Zero
            | Pattern::Literal(_)
            | Pattern::Character(_)
            | Pattern::Nil
            | Pattern::End => 0,
            Pattern::Succ { predecessor } => predecessor.binders(),
            Pattern::Cons { head, tail } | Pattern::Atom { head, tail } => {
                head.binders() + tail.binders()
            }
        }
    }
}
