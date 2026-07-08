use {
    super::Term,
    std::{collections::BTreeSet, fmt},
};

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

impl Item {
    /// The names this item declares — one for a `let`, the whole group for a `rec`.
    pub(crate) fn names(&self) -> impl Iterator<Item = &str> {
        match self {
            Item::Let { name, .. } => std::slice::from_ref(name),
            Item::Rec { names, .. } => names.as_slice(),
        }
        .iter()
        .map(String::as_str)
    }

    /// The free names this item's body (or, for a `rec` group, every member)
    /// references. See [`Term::free_names`].
    pub(crate) fn free_names(&self) -> BTreeSet<String> {
        match self {
            Item::Let { body, .. } => body.free_names(),
            Item::Rec { items, .. } => items.iter().flat_map(Term::free_names).collect(),
        }
    }

    /// Whether evaluating this item could perform an effect — directly, anywhere in
    /// its body (or any member). See [`Term::contains_effect`].
    pub(crate) fn contains_effect(&self) -> bool {
        match self {
            Item::Let { body, .. } => body.contains_effect(),
            Item::Rec { items, .. } => items.iter().any(Term::contains_effect),
        }
    }

    /// Whether binding this item performs no action (see [`Term::is_synchronous`]);
    /// a `rec` group is synchronous only if every member is.
    pub(crate) fn is_synchronous(&self) -> bool {
        match self {
            Item::Let { body, .. } => body.is_synchronous(),
            Item::Rec { items, .. } => items.iter().all(Term::is_synchronous),
        }
    }
}

/// The erased program: a flat list of top-level `items` plus the entrypoint
/// `body`. Replaces the N-deep `Let`/`Rec` chain `erase` used to build, which
/// `into_cont` then recursed along (BUG.md, §scope/notes). Local `Let`/`Rec` are
/// unchanged.
#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
    pub body: Term,
}

impl fmt::Display for Module {
    // Iterates the flat items (each body printed via `Term`'s `Display`) — O(N),
    // so `--print ersd` cannot re-trigger the prelude-depth overflow.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.items {
            match item {
                Item::Let { name, body } => {
                    writeln!(formatter, "let #{name} =\n{body};\n")?;
                }
                Item::Rec { names, items } => {
                    write!(formatter, "rec ")?;

                    for (index, (name, body)) in names.iter().zip(items).enumerate() {
                        if index > 0 {
                            write!(formatter, "and ")?;
                        }

                        write!(formatter, "#{name} =\n{body} ")?;
                    }

                    writeln!(formatter, ";")?;
                }
            }
        }

        write!(formatter, "{}", self.body)
    }
}
