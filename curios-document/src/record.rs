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
    /// The declarations written here that a consumer can see, in source order, then — sorted by name — the ones this module exposes out of a module with no page of its own, each at its [`Declaration::home`].
    pub declarations: Vec<Declaration>,
    /// The names this module exposes that are declared on another page — a `pub use` — each a link to where the declaration lives, sorted by name. A `pub use` out of a module with no page is not listed here: its declaration is among `declarations` instead.
    pub reexports: Vec<Reexport>,
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
    /// The module that declares it, which a mark's referent names it under: the page's own module, or, for a declaration the page exposes out of a module with no page of its own, that module. This is what lets a link find the declaration where it is shown rather than where it was written.
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
}

/// One constructor, field or concept method.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Member {
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

/// One name in a signature, resolved: the byte range of `text` it occupies and the declaration it names.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Mark {
    pub start: usize,
    pub end: usize,
    /// The canonical path of the declaration named.
    pub referent: Qualifier,
    /// Whether the referent lies within the documented unit, and so has a page in the same bundle.
    pub within: bool,
}

/// A name this module exposes for a declaration made elsewhere.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Reexport {
    pub name: String,
    pub referent: Qualifier,
    pub within: bool,
}
