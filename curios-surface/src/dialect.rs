//! Which language a tree holds: a program, or Core text, which writes what a program leaves to the elaborator.
//!
//! **One tree and one grammar, marked by the dialect.** Every node a program writes is a node Core text writes too, so the tree and the parser are written once, generic over a [`Dialect`], and a dialect states each form only one of them writes as a type: Core text's a node it reads, a program's one no value inhabits. So a program's tree holds no decorator and no declared path, and its grammar has no production for one: what Core text writes and a program does not is refused in a program as the token the grammar did not expect.
//!
//! **What Core text writes beyond a program.** An operand under a decorator, `#{field = value, …} operand`, a witness named by its identity and an intrinsic node by its variant ([`CoreForm`]); a declaration's head, the decorator before it and the absolute name it declares ([`CoreHead`]); the decorator before a case; a `use` binder's word, where a program's witness binder has none; and the file a `.crsc` is ([`CoreFile`]), whose items are joined into groups by `and` whatever their kinds, and whose entry states its type.

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

/// Core written as text, with nothing left to an elaborator: a `.crsc` file, and what a dump and a kernel's refusal print.
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

/// What Core text writes in an operand's place and a program cannot.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreForm {
    /// A decorator and the operand it attaches to.
    Decorated(Decorated),
    /// A witness named by its identity, `/std/Nat/witness~/std/ops/Add(Nat)`: a program reaches one by resolution alone.
    Witness(WitnessId),
    /// An intrinsic node, `%NatAdd(a, b)`: `%` and its Core variant's name, then its operands where it has any, each a value as a decorator writes one.
    Intrinsic(CoreIntrinsic),
}

/// An intrinsic node as Core text writes it: what its operands mean is its variant's to say.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreIntrinsic {
    pub variant: Label,
    pub operands: Option<Vec<CoreValue>>,
}

/// `#{field = value, …} operand`: what a Core node holds beside what its surface spelling says — a level, a definition's proof mark, a match's stated type, a node other than the one the spelling suggests. It attaches to the operand an infix operator takes, a primary with its call chain, so `#{…} f(x)` states fields of the call and `(#{…} f)(x)` of its head, and a larger term is parenthesized.
#[derive(Debug, Clone, PartialEq)]
pub struct Decorated {
    pub fields: Vec<DecoratorField>,
    pub term: Term<CoreText>,
}

/// One field a decorator states: the name of the Core field, and its value.
#[derive(Debug, Clone, PartialEq)]
pub struct DecoratorField {
    pub label: Label,
    pub value: CoreValue,
}

/// What a decorator's field holds, and an intrinsic's operand.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreValue {
    /// A term: a name, a call, a level, a literal, a whole term a field states.
    Term(Term<CoreText>),
    /// `{field = value, …}`: a struct-valued field, each field it omits at the template's.
    Fields(Vec<DecoratorField>),
    /// `[value, @value, use value, …]`: a list, each entry under its mark.
    List(Vec<(Plicity, CoreValue)>),
    /// `/`, the root module.
    Root,
}

/// What Core text writes in a declaration's head: the decorator before it, and the name it declares, which is absolute.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreHead {
    /// What the decorator before the declaration states of it: before its declaring word for a group's first member, before `and` for a later one.
    pub fields: Vec<DecoratorField>,
    /// The name declared, by its path or a witness's identity; the member's label is the path's last segment, or the identity as written.
    pub name: Global,
}

/// What a `.crsc` file holds: a module's items, then the entry a program closes with where it is one.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreFile {
    pub items: Vec<CoreItem>,
    pub entry: Option<CoreEntry>,
}

/// The term a program closes with, and the type it states on the line beneath it.
#[derive(Debug, Clone, PartialEq)]
pub struct CoreEntry {
    pub body: Term<CoreText>,
    pub type_: Option<Term<CoreText>>,
}

/// One item of Core text.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreItem {
    /// One declaration, or a group of them joined by `and`, of any kinds.
    Group(Vec<CoreMember>),
    Foreign(TopForeign<CoreText>),
    Test(TopTest<CoreText>),
}

/// One member of a group.
#[derive(Debug, Clone, PartialEq)]
pub enum CoreMember {
    Let(TopLet<CoreText>),
    Induct(TopInduct<CoreText>),
    Struct(TopStruct<CoreText>),
    Concept(TopConcept<CoreText>),
}
