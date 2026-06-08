use super::Term;

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
/// unchanged.
#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
    pub body: Term,
}
