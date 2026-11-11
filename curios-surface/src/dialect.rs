//! Which language a tree holds: a program, or Core text, which writes what a program leaves to the elaborator.
//!
//! **One tree and one grammar, marked by the dialect.** Every node a program writes is a node Core text writes too, so the tree and the parser are written once, generic over a [`Dialect`], and a dialect states each form only one of them writes as a type: Core text's a node it reads, a program's one no value inhabits. So a program's tree holds no decorator and no declared path, and its grammar has no production for one: what Core text writes and a program does not is refused in a program as the token the grammar did not expect.
//!
//! **What Core text writes beyond a program.** An operand under a decorator, `#{field = value, …} operand`, a witness named by its identity and an intrinsic node by its variant ([`CoreForm`]); a declaration's head, the decorator before it and the absolute name it declares ([`CoreHead`]); the decorator before a case; a `use` binder's word, where a program's witness binder has none; and the file a `.crsc` is ([`CoreFile`]), whose items are joined into groups by `and` whatever their kinds, and whose entry states its type.
//!
//! **What a print shows and no grammar reads back is a dialect too**, [`Shown`]: Core text's forms over its own terms, and the forms only a bounded print or a reader's spelling writes ([`ShownForm`]). A dump, a certifier's refusal and a report's term are shown, since each may elide; a tree no parser makes a shown one, so none is ever read as Core text.

use {
    crate::{Label, Term, TopConcept, TopForeign, TopInduct, TopLet, TopStruct, TopTest},
    curios_common::Plicity,
    curios_core::{Global, WitnessId},
    std::fmt,
};

/// A language a tree is written in, by the forms it writes beyond the other's.
pub trait Dialect: Clone + fmt::Debug + PartialEq + 'static {
    /// What the dialect writes in an operand's place beyond the forms both write.
    type Form: Clone + fmt::Debug + PartialEq;
    /// What a declaration's head writes beside its label.
    type Head: Clone + fmt::Debug + PartialEq;
    /// What a case states beside its tag and payload.
    type Fields: Clone + fmt::Debug + PartialEq;
}

/// The language a person writes: a `.crs` file, a program or a module.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Program;

/// Core written as text, with nothing left to an elaborator: a `.crsc` file, and what a dump and a certifier's refusal print.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CoreText;

/// A form a dialect cannot write: no value, so its tree holds none.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Unwritable {}

impl Dialect for Program {
    type Form = Unwritable;
    type Head = ();
    type Fields = ();
}

impl Dialect for CoreText {
    type Form = CoreForm;
    type Head = CoreHead;
    type Fields = Vec<DecoratorField>;
}

/// What a print shows and no grammar reads back: Core text's forms, and the ones only a bounded print or a reader's spelling writes. No parser makes a tree of it, so none is read as Core text.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Shown;

impl Dialect for Shown {
    type Form = ShownForm;
    type Head = CoreHead<Shown>;
    type Fields = Vec<DecoratorField<Shown>>;
}

/// What a shown term writes in an operand's place beyond a program's forms.
#[derive(Debug, Clone, PartialEq)]
pub enum ShownForm {
    /// A form of Core text.
    Core(CoreForm<Shown>),
    /// `…`: what a bounded print leaves out, no token of the language.
    Elided,
    /// A token as the shown spelling writes it, which no grammar reads: a name with its instance, `F.{u}/c`, a sort at a level, `Type.{u+1}`, a string past the length a report quotes.
    Spelled(String),
    /// A metavariable as a reader is shown it, `?7`, with its spine after it, `?7[x, y]`, where an entry is other than a variable.
    Metavar { id: String, spine: Vec<Term<Shown>> },
    /// A term elaboration ascribed a type, as a reader is shown it, `(e: T)`: no surface form states a type beside a bare term.
    Ascribed {
        term: Term<Shown>,
        type_: Term<Shown>,
    },
}

/// What Core text writes in an operand's place and a program cannot.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreForm<D: Dialect = CoreText> {
    /// A decorator and the operand it attaches to.
    Decorated(Decorated<D>),
    /// A witness named by its identity, `/std/Nat/witness~/std/ops/Add(Nat)`: a program reaches one by resolution alone.
    Witness(WitnessId),
    /// An intrinsic node, `#NatAdd(a, b)`: `#` and its Core variant's name, then its operands where it has any, each a value as a decorator writes one.
    Intrinsic(CoreIntrinsic<D>),
}

/// An intrinsic node as Core text writes it: what its operands mean is its variant's to say.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreIntrinsic<D: Dialect = CoreText> {
    pub variant: Label,
    pub operands: Option<Vec<CoreValue<D>>>,
}

/// `#{field = value, …} operand`: what a Core node holds beside what its surface spelling says — a level, a definition's proof mark, a match's stated type, a node other than the one the spelling suggests. It attaches to the operand an infix operator takes, a primary with its call chain, so `#{…} f(x)` states fields of the call and `(#{…} f)(x)` of its head, and a larger term is parenthesized.
#[derive(Debug, Clone, PartialEq)]
pub struct Decorated<D: Dialect = CoreText> {
    pub fields: Vec<DecoratorField<D>>,
    pub term: Term<D>,
}

/// One field a decorator states: the name of the Core field, and its value.
#[derive(Debug, Clone, PartialEq)]
pub struct DecoratorField<D: Dialect = CoreText> {
    pub label: Label,
    pub value: CoreValue<D>,
}

/// What a decorator's field holds, and an intrinsic's operand.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreValue<D: Dialect = CoreText> {
    /// A term: a name, a call, a level, a literal, a whole term a field states.
    Term(Term<D>),
    /// `{field = value, …}`: a struct-valued field, each field it omits at the template's.
    Fields(Vec<DecoratorField<D>>),
    /// `[value, @value, use value, …]`: a list, each entry under its mark.
    List(Vec<(Plicity, CoreValue<D>)>),
    /// `/`, the root module.
    Root,
}

/// What Core text writes in a declaration's head: the decorator before it, and the name it declares, which is absolute.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreHead<D: Dialect = CoreText> {
    /// What the decorator before the declaration states of it: before its declaring word for a group's first member, before `and` for a later one.
    pub fields: Vec<DecoratorField<D>>,
    /// The name declared, by its path or a witness's identity; the member's label is the path's last segment, or the identity as written.
    pub name: Global,
}

/// What a `.crsc` file holds: a module's items, then the entry a program closes with where it is one.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreFile<D: Dialect = CoreText> {
    pub items: Vec<CoreItem<D>>,
    pub entry: Option<CoreEntry<D>>,
}

/// The term a program closes with, and the type it states on the line beneath it.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreEntry<D: Dialect = CoreText> {
    pub body: Term<D>,
    pub type_: Option<Term<D>>,
}

/// One item of Core text.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreItem<D: Dialect = CoreText> {
    /// One declaration, or a group of them joined by `and`, of any kinds.
    Group(Vec<CoreMember<D>>),
    Foreign(TopForeign<D>),
    Test(TopTest<D>),
}

/// One member of a group.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreMember<D: Dialect = CoreText> {
    Let(TopLet<D>),
    Induct(TopInduct<D>),
    Struct(TopStruct<D>),
    Concept(TopConcept<D>),
}
