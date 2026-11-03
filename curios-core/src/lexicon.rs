//! What a report spells one unit's names by, kept with the unit: the globals it declares, its nominal families' parameter marks, and its structures' fields and constructors' payloads. The policy that reads them — how a global is shortened, how a binder is renamed, how a term is written — is `curios-surface`'s.

use {
    crate::{Atom, Global},
    curios_common::{Persistent, Plicity, Qualifier},
    std::rc::Rc,
};

/// What a report spells one unit's names by: the globals it declares (axis (b)), its nominal families' parameter marks (axis (d)), and its structures' fields and constructors' payloads.
///
/// **Made once for a unit and kept with it** ([`Definitions::lexicon`](crate::Definitions::lexicon)). A report reads each unit of its scope where it stands (`curios-surface`'s `Spelling::with_lexicons`) and builds no table over them, so what a report costs does not follow how much is in scope, and it reads no declaration it does not print.
#[derive(Clone, Default)]
pub struct Lexicon {
    pub symbols: Rc<Symbols>,
    pub plicities: Rc<Marks>,
    pub members: Rc<Members>,
}

/// Each nominal family's argument plicities, by the family's name: what a report marks an applied family by (axis (d)). Persistent, so a unit compiled over another holds the other's with the families it changed read again.
pub type Marks = Persistent<Global, Rc<Vec<Plicity>>>;

/// What a unit's structures and constructors are written by: each field's mark and label, each payload member's mark. Made once for a unit and kept with its [`Lexicon`]; persistent, as [`Marks`] are.
#[derive(Debug, Default, Clone)]
pub struct Members {
    /// Each structure's fields, a concept's among them, by the structure's name.
    pub fields: Persistent<Global, Rc<Fields>>,
    /// Each family's constructors, by the family's name.
    pub payloads: Persistent<Global, Rc<Payloads>>,
}

/// One structure's fields in slot order: the mark a field is declared under, and its label where it has one.
pub type Fields = Vec<(Plicity, Option<String>)>;

/// One family's constructors in declaration order, with the marks each one is called by: the family's parameters, every one hidden, then its payload.
pub type Payloads = Vec<(Atom, Vec<Plicity>)>;

/// The globals one unit declares, with how many of them carry each segment-suffix: what a global is shortened against (axis (b) without a reader). Made once for a unit and read where it stands ([`Lexicon`]); persistent, so a unit compiled over another holds the other's with the names it changed counted again ([`Symbols::revised`]).
#[derive(Debug, Default, Clone)]
pub struct Symbols {
    names: Persistent<Global, ()>,
    suffixes: Persistent<String, usize>,
}

impl Symbols {
    /// `names`, each counted once: one global can be listed twice, an inductive being both an `induct_decls` registry key and an `items` type-constructor definition, and such a name would otherwise look ambiguous with itself and never shorten.
    pub fn of(names: impl IntoIterator<Item = Global>) -> Self {
        let mut symbols = Self::default();
        for name in names {
            symbols.add(name);
        }

        symbols
    }

    /// These symbols with each name of `gone` taken out and each of `added` put in, every suffix counted as [`Symbols::of`] counts it.
    pub fn revised(
        &self,
        gone: impl IntoIterator<Item = Global>,
        added: impl IntoIterator<Item = Global>,
    ) -> Self {
        let mut symbols = self.clone();
        for name in gone {
            if symbols.names.remove(&name).is_some() {
                for suffix in suffixes_of(&name) {
                    let count = symbols.suffixes.get(&suffix).copied().unwrap_or(0);
                    match count {
                        0 | 1 => {
                            symbols.suffixes.remove(&suffix);
                        }
                        _ => symbols.suffixes.insert(suffix, count - 1),
                    }
                }
            }
        }
        for name in added {
            symbols.add(name);
        }

        symbols
    }

    /// `name` among these symbols, counted where it was not.
    fn add(&mut self, name: Global) {
        if self.names.contains_key(&name) {
            return;
        }
        for suffix in suffixes_of(&name) {
            let count = self.suffixes.get(&suffix).copied().unwrap_or(0);
            self.suffixes.insert(suffix, count + 1);
        }
        self.names.insert(name, ());
    }

    /// Whether `name` is one of these.
    pub fn contains(&self, name: &Global) -> bool {
        self.names.contains_key(name)
    }

    /// How many of these carry `suffix`.
    pub fn carrying(&self, suffix: &str) -> usize {
        self.suffixes.get(&suffix.to_string()).copied().unwrap_or(0)
    }
}

/// Every segment-suffix of a global's path, shortest first. Taken over the *segments* a name is made of, never over its rendered text: `/Foobar` is not a suffix of `/Foo/bar`, and only the structure says so. What a global is counted under here, and what it may shorten to.
pub fn suffixes_of(name: &Global) -> Vec<String> {
    let Some(segments) = name.qualifier().map(Qualifier::segments) else {
        return Vec::new();
    };

    (1..=segments.len())
        .map(|k| segments[segments.len() - k..].join("/"))
        .collect()
}
