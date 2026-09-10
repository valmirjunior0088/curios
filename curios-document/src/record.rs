//! What a unit's interface is, read for a page: one record per module the unit exposes, each declaration's head printed as the author wrote it with every name it mentions resolved, and the prose attached to each — plain data a renderer walks and a transport encodes. The text lowering builds it from the tables it just built, as the last thing it does, and it rides on the unit: the prelude image, a verdict slot, the browser bundle, so a unit is documented from its stored form without its sources.
//!
//! **A library is documented for its consumers.** That is the one audience this record knows: which modules and declarations appear is the export view the lowering resolved to a fixed point, so a private declaration is absent rather than hidden and a re-export is a link to the declaration it names — or, when the declaration's own module has no page, the facade pattern, the declaration itself, documented where the re-export puts it and reached by every mark that names its home; a constructor appears only when the representation is public, a field likewise, and a test never. A program has no consumer, so nothing here documents one.

use curios_utilities::Qualifier;

/// A unit's interface, for its consumers.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Documentation {
    /// The prefix the unit mounts at — `/json` for the package `json` — which every module path below begins with.
    pub prefix: Qualifier,
    /// What the unit is, in a sentence or a few, for its landing page: the manifest's `description` for a package, a constant for the standard library, nothing when neither said.
    pub description: Option<String>,
    /// Every module a consumer can reach, the root first and each parent before its children.
    pub modules: Vec<ModuleDocumentation>,
}

/// One module's page.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct ModuleDocumentation {
    pub path: Qualifier,
    /// The `-- |` block above the `mod` declaration that declares it; `None` for the root, whose prose is the manifest's.
    pub prose: Option<Vec<String>>,
    /// The public child modules, in declaration order.
    pub children: Vec<Qualifier>,
    /// Everything a consumer reaches through this module, in the order the module writes it: its own declarations where they are declared, and each name a `pub use` exposes where that `pub use` stands. A name declared elsewhere carries a [`Declaration::source`] saying so, and one declared in a module with no page carries none — for a consumer, this page is where it lives.
    pub declarations: Vec<Declaration>,
}

/// What kind of declaration a page entry is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Kind {
    Definition,
    Inductive,
    Structure,
    Concept,
    Witness,
    Foreign,
}

/// One declaration a consumer can see: its head as written, its prose, and the members its representation exposes.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Declaration {
    /// The declared label — and the anchor a link to it names. Empty for a witness, which is anonymous by design.
    pub name: String,
    /// The module a consumer names it under, which is this page's own module — for a declaration written here, one exposed out of a module with no page, and one re-exported from another page alike. Every [`Mark`] naming this declaration carries the same path, so a link finds it where it is shown and no reader is ever handed the path it was written at.
    pub home: Qualifier,
    pub kind: Kind,
    pub signature: Signature,
    pub prose: Option<Vec<String>>,
    /// Constructors, fields or concept methods: present only when the representation is public, so an opaque type shows none.
    pub members: Vec<Member>,
    /// An inductive, structure or concept whose representation is private to its declaring subtree: no constructor, field, literal or witness of it can be written by a consumer. Stated beside `members` because an empty list cannot say it — a sealed concept still lists its methods, and a public representation can have nothing to list.
    pub opaque: bool,
    /// A `satisfy` whose body the compiler writes.
    pub derived: bool,
    /// What this card came from, under the name a consumer writes it by, when that is something else this bundle shows: the declaration itself for a name re-exported off another page, and the owning declaration for a constructor or a method a module exposes beside it. `None` both for a declaration written here and for one adopted out of a module with no page — the second has no path a consumer may write, and naming the root it came from is exactly what a consumer must not be taught.
    pub source: Option<Qualifier>,
    /// What kind of thing a declaration adopted out of a root a consumer cannot name is — the word that root's owner chose, never its path. It is the only thing a card can say about a declaration whose signature says little: `let Nat: Type` lists no constructors because there are none to list, and this is what says so.
    pub chip: Option<String>,
}

/// One constructor, field or concept method — or a concept's superclass edge, `use Eql(A),`, which the language declares as an anonymous field and which stays a member so the block prints it where it was written.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Member {
    /// The label, or empty for a superclass edge: an anonymous member has no anchor, no address and no row in the search index.
    pub name: String,
    pub signature: Signature,
    pub prose: Option<Vec<String>>,
}

/// A declaration head as printed, and every name in it that resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Signature {
    pub text: String,
    /// Ascending by position, non-overlapping.
    pub marks: Vec<Mark>,
}

/// One name in a signature, resolved: the byte range of `text` it occupies and the declaration it names, under the name a consumer writes rather than the path it was declared at. A name whose declaration a consumer cannot write carries no mark at all and stays plain text.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Mark {
    pub start: usize,
    pub end: usize,
    /// The path a consumer names the declaration by — a [`Declaration::home`] with its name, never a declaration site inside a root this unit keeps to itself.
    pub referent: Qualifier,
    /// Whether the referent lies within the documented unit, and so has a page in the same bundle.
    pub within: bool,
}
