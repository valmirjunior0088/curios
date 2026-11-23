//! What a unit's interface is, read for a page: one record per module the unit exposes, each declaration's head printed as the author wrote it with every word it writes classed and every name it mentions resolved and classed, and the prose attached to each — plain data a renderer walks and a transport encodes. The text lowering builds it where a command asks for one, from the tables the unit's lowering stored and the surface tree its sources parse to, and no unit carries it.
//!
//! **A library is documented for its consumers.** That is the one audience this record knows: which modules and declarations appear is the export view the lowering resolved to a fixed point, so a private declaration is absent rather than hidden and a re-export is an entry of the declaration it names, by the path every mark names it by — or, when the declaration's own module has no page, the facade pattern, the declaration itself, documented where the re-export puts it and reached by every mark that names it; a constructor appears only when the representation is public, a field likewise, and a test never. A program has no consumer, so nothing here documents one.

use curios_common::{Class, Keyword, Qualifier};

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
    /// The `---` block above the `mod` declaration that declares it; `None` for the root, whose prose is the manifest's.
    pub prose: Option<Vec<String>>,
    /// The public child modules, in declaration order.
    pub children: Vec<Qualifier>,
    /// Everything a consumer reaches through this module, in the order the module writes it: its own declarations where they are declared, each name a `pub use` exposes where that `pub use` stands, and the members of a declaration such a line exposes beside it as the line that does ([`Entry`]).
    pub entries: Vec<Entry>,
}

impl ModuleDocumentation {
    /// The declarations among the module's entries, in their order.
    pub fn declarations(&self) -> impl Iterator<Item = &Declaration> {
        self.entries.iter().filter_map(|entry| match entry {
            Entry::Declaration(declaration) => Some(declaration),
            Entry::Exposure(_) => None,
        })
    }
}

/// One thing a module's page lists: a declaration a consumer reaches through the module, or the line by which the module exposes a declaration's members beside it.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Entry {
    Declaration(Declaration),
    Exposure(ExposureLine),
}

/// The members of one declaration a module exposes beside it by one `pub use` line: the constructors of an inductive, the methods of a concept. They are no declarations of their own, and each is shown in its owner's block.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct ExposureLine {
    /// The declaration whose members they are, by the path a consumer names it.
    pub owner: Qualifier,
    /// The members the line exposes, as it lists them; a glob's, in the order their owner declares them.
    pub names: Vec<String>,
    /// The line as a module writes it, `pub use Option/{some, none}`, the owner and each member marked as a signature's names are.
    pub signature: Signature,
}

/// What kind of declaration a page entry is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[curios_archive::archived]
pub enum Kind {
    Definition,
    Inductive,
    Structure,
    Concept,
    Witness,
    Foreign,
}

/// What the certifier concluded of a definition's termination, as a page states it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Totality {
    /// Proven to terminate on every input.
    Total,
    /// Not proven total, through what it reaches or a host row it calls; its own recursion descends.
    Partial,
    /// Its own recursion is not shown to descend, so no type computes through it.
    Unfounded,
}

/// How a declaration's source writes the members its representation shows: what follows its head, what leads and trails each member, what closes the block, and whether its members stand indented under the head.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block {
    pub opener: &'static str,
    pub lead: &'static str,
    pub trail: &'static str,
    pub closer: &'static str,
    pub indented: bool,
}

impl Block {
    /// The block a declaration of `kind` writes its members in, `opaque` saying its representation is kept: an inductive's constructors each behind a bar and closed by `end`, a structure's fields and a concept's methods in braces, each with its comma. None for a declaration that writes none a consumer sees: a family or a structure whose representation is kept, and every other kind. A sealed concept still writes its methods, which are reached by name either way.
    pub fn of(kind: Kind, opaque: bool) -> Option<Self> {
        match (kind, opaque) {
            (Kind::Inductive, false) => Some(Self {
                opener: "",
                lead: "| ",
                trail: "",
                closer: Keyword::End.spelling(),
                indented: false,
            }),
            (Kind::Structure, false) | (Kind::Concept, _) => Some(Self {
                opener: " {",
                lead: "",
                trail: ",",
                closer: "}",
                indented: true,
            }),
            _ => None,
        }
    }
}

/// One declaration a consumer can see: its head as written, its prose, and the members its representation exposes.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Declaration {
    /// The declared label — and the anchor a link to it names. Empty for a witness, which is anonymous by design.
    pub name: String,
    pub kind: Kind,
    pub signature: Signature,
    pub prose: Option<Vec<String>>,
    /// Constructors, fields or concept methods: present only when the representation is public, so an opaque type shows none.
    pub members: Vec<Member>,
    /// An inductive, structure or concept whose representation is private to its declaring subtree: no constructor, field, literal or witness of it can be written by a consumer. Stated beside `members` because an empty list cannot say it — a sealed concept still lists its methods, and a public representation can have nothing to list.
    pub opaque: bool,
    /// A `satisfy` whose body the compiler writes.
    pub derived: bool,
    /// Whether the declaration is a proof: one a checker marks as written at a proposition, or as a function into one. Read off the check of the unit, the one fact of a page the surface tree does not state.
    pub proof: bool,
    /// What the certifier concluded of the definition's termination, where it classifies it at all: none for what is no definition, a type or a witness.
    pub totality: Option<Totality>,
    /// What a witness takes part in: the concept it witnesses and each declaration its key names, by the path a consumer names them. Empty for anything else.
    pub parties: Vec<Qualifier>,
    /// The path every [`Mark`] names this declaration by: its own, the path of the module that lists it with its `name`, where that module's page is where a consumer finds it, and the one another page shows it by where it is shown there too, so two entries of one declaration name one path. Never a path inside a root no consumer can name. A witness, which no mark names, carries the path of the module that declares it.
    pub named: Qualifier,
    /// What kind of thing a declaration adopted out of a root a consumer cannot name is — the word that root's owner chose, never its path. It is the only thing a page can say about a declaration whose signature says little: `let Nat: Type` lists no constructors because there are none to list, and this is what says so.
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

/// One span of a signature the printer set apart: the byte range of `text` it occupies, and what it is. A name whose declaration a consumer cannot write, or a binder, carries no mark at all and stays plain text.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Mark {
    pub start: usize,
    pub end: usize,
    pub marked: Marked,
}

/// What a [`Mark`] spans: a word of the language, or a name, each with what it is painted as.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Marked {
    /// One of the language's words — a keyword, a boolean, a sort — or a wire word of a `foreign` signature, a type.
    Word(Class),
    /// A name, resolved: the declaration it names, under the name a consumer writes rather than the path it was declared at.
    Name {
        /// What the declaration is painted as, read off what it states; `None` where the compilation does not say.
        class: Option<Class>,
        /// The path a consumer names the declaration by — a [`Declaration::named`], never a declaration site inside a root this unit keeps to itself.
        referent: Qualifier,
        /// Whether the referent lies within the documented unit, and so has a page in the same bundle.
        within: bool,
    },
}
