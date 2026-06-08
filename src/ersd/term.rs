use {
    super::{Name, Prim},
    std::collections::BTreeMap,
};

pub type Subterm = Box<Term>;

#[derive(Debug)]
pub enum NatMatch {
    Induction {
        head: Subterm,
        zero_case: Subterm,
        pred: String,
        ih: String,
        succ_case: Subterm,
    },
    Dispatch {
        head: Subterm,
        cases: BTreeMap<u32, Subterm>,
        default: Subterm,
    },
}

/// A captured or parameter binder, plus its specialization-*candidate* flag: true
/// when its (pre-erasure) type was a function, a `Type`, or unit. Computed during
/// type-directed erasure (the last point types are available) and carried to
/// `cont`, glued to the name so the two can never desync. Defaults to
/// non-candidate, so a binder built from a bare name (`"x".into()`) is not one.
#[derive(Debug)]
pub struct Argument {
    pub name: String,
    pub candidate: bool,
}

impl<S: Into<String>> From<S> for Argument {
    fn from(name: S) -> Self {
        Self {
            name: name.into(),
            candidate: false,
        }
    }
}

impl Argument {
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct Func {
    pub captures: Vec<Argument>,
    pub params: Vec<Argument>,
    pub body: Subterm,
}

#[derive(Debug)]
pub struct Apply {
    pub head: Subterm,
    pub params: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Tuple {
    pub fields: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Proj {
    pub head: Subterm,
    pub index: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Atom {
    pub index: usize,
}

#[derive(Debug)]
pub struct Match {
    pub head: Subterm,
    pub cases: Vec<Subterm>,
}

#[derive(Debug)]
pub struct Let {
    pub name: String,
    pub body: Subterm,
    pub tail: Subterm,
}

#[derive(Debug)]
pub struct Rec {
    pub names: Vec<String>,
    pub items: Vec<Subterm>,
    pub tail: Subterm,
}

#[derive(Debug)]
pub enum Term {
    Erased,
    Prim(Prim),
    NatMatch(NatMatch),
    Func(Func),
    Apply(Apply),
    Tuple(Tuple),
    Proj(Proj),
    Atom(Atom),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Name(Name),
}

/// A top-level item of a [`Module`]: a single `let` definition, or a `rec` group
/// of mutually-recursive definitions. The flat, name-keyed mirror of `core::Item`
/// after erasure — `Rec` keeps `names`/`items` as parallel vectors so the lowerer
/// can feed it straight to `lower_letrec_bindings`, exactly like a local `Rec`.
#[derive(Debug)]
pub enum Item {
    Let {
        name: String,
        body: Term,
    },
    Rec {
        names: Vec<String>,
        items: Vec<Term>,
    },
}

/// The erased program: a flat list of top-level `items` plus the entrypoint
/// `body`. Replaces the N-deep `Let`/`Rec` chain `erase` used to build, which
/// `to_cont` then recursed along (BUG.md, §scope/notes). Local `Let`/`Rec` are
/// unchanged. Bodies are unboxed `Term`s: a `Module` owns its items in a `Vec`,
/// so there is no recursive-type cycle to break with `Subterm` indirection.
#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
    pub body: Term,
}

