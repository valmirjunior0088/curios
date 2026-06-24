use {
    super::{LetSignature, Name, Plicity, Quantity, Term, TupleTypeParam},
    crate::Span,
};

#[derive(Debug, Clone)]
pub struct TopMod {
    pub span: Option<Span>,
    pub is_pub: bool,
    pub label: String,
    pub module: Option<Module>,
}

impl PartialEq for TopMod {
    fn eq(&self, other: &Self) -> bool {
        self.is_pub == other.is_pub && self.label == other.label && self.module == other.module
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GroupItem {
    Mod(String),
    Let(String),
    Both(String),
}

impl GroupItem {
    pub fn label(&self) -> &str {
        match self {
            GroupItem::Mod(s) | GroupItem::Let(s) | GroupItem::Both(s) => s,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UseGroup {
    Named(Vec<GroupItem>),
    Glob,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopUse {
    pub is_pub: bool,
    pub name: Name,
    pub group: UseGroup,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopLet {
    pub is_pub: bool,
    pub label: String,
    pub signature: LetSignature,
}

/// One payload binder of an `induct` case. The name is optional (`success(A)`
/// stays positional); it is required when a later payload type or the case
/// target mentions the binder. `plicity` is the `@`-on-the-name mark (implicit
/// at the value-constructor function — `cons(@m : Nat, …)`, `m` recoverable from
/// a later payload's type); `quantity` is the `@`-on-the-type mark (erased — the
/// field is dropped from the runtime variant tuple).
#[derive(Debug, Clone, PartialEq)]
pub struct CasePayloadParam {
    pub plicity: Plicity,
    pub quantity: Quantity,
    pub label: Option<String>,
    pub type_: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopCase {
    pub label: String,
    pub payload: Vec<CasePayloadParam>,
    /// The parenthesized index expressions after the payload — the case's
    /// terminal `: Vec(T, Nat/succ(m))` with the mandatory part elided to
    /// `: (Nat/succ(m))`. Present iff the inductive head declares indices.
    pub target: Option<Vec<Term>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopInductive {
    pub is_pub: bool,
    pub label: String,
    /// Inductive parameters are *implicit* on every value constructor regardless
    /// of any mark (the desugar applies those marks), with the call-site `@`
    /// available to supply one positionally when wanted. On the
    /// type-constructor function a parameter is *explicit* by default (types
    /// are written out); a declaration-site `@` makes it implicit there too
    /// (`induct Eq(@A : Type) : (x : A, y : A)` — `A` is recoverable from the
    /// indices, so types are written `Eq(x, y)`).
    pub params: Vec<(Plicity, String, Term)>,
    /// The head's index telescope, `induct Vec(T : Type) : (n : Nat)`. Names
    /// are optional and documentary — needed only when a later index's type
    /// depends on an earlier one; they are *not* in scope in the cases.
    pub indices: Vec<(Option<String>, Term)>,
    /// The arity's result sort — `Type` or `Prop`. Written after the index
    /// telescope (`: (n : Nat) -> Prop`) or in its place when there are no
    /// indices (`: Prop`); defaults to `Type` when omitted.
    pub result_sort: Term,
    pub cases: Vec<TopCase>,
}

/// A `struct` declaration: a nominal record. `is_pub` is the outer `pub` (the
/// type-former's visibility); `rep_pub` is the inner `pub` before the brace
/// (whether the representation — construction and projection — is exported).
/// `params` are written exactly like an inductive's; `fields` reuse the Σ-type field
/// grammar (label optional, like tuple-type fields).
#[derive(Debug, Clone, PartialEq)]
pub struct TopStruct {
    pub is_pub: bool,
    pub rep_pub: bool,
    pub label: String,
    pub params: Vec<(Plicity, String, Term)>,
    /// The result sort — `Type` or `Prop`, written `: Sort` after the
    /// parameters; defaults to `Type` when omitted.
    pub result_sort: Term,
    pub fields: Vec<TupleTypeParam>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Mod(TopMod),
    Use(TopUse),
    Let(TopLet),
    Rec(Vec<TopLet>),
    Inductive(Vec<TopInductive>),
    Struct(TopStruct),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<TopItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub module: Module,
    pub type_: Option<Term>,
    pub tail: Term,
}

impl Entrypoint {
    pub fn new(items: Vec<TopItem>, tail: Term) -> Self {
        Self {
            module: Module { items },
            type_: None,
            tail,
        }
    }

    pub fn with_type(self, type_: Term) -> Self {
        Self {
            type_: Some(type_),
            ..self
        }
    }
}
