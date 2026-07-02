use {
    super::{
        FuncSugarParam, FuncType, FuncTypeParam, LetSignature, Name, Plicity, Subterm, Term,
        TupleTypeParam,
    },
    curios_base::Span,
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
/// a later payload's type). Erasure is sort-driven, so no per-field mark is kept.
#[derive(Debug, Clone, PartialEq)]
pub struct CasePayloadParam {
    pub plicity: Plicity,
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
pub struct TopInduct {
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

/// A `struct`/`record` declaration: a nominal record. `is_pub` is the outer
/// `pub` (the type-former's visibility); `rep_pub` is the kind keyword —
/// `record` (representation exported, reaching wherever the type name is
/// visible) vs `struct` (representation private to the exact declaring module).
/// The two markers are orthogonal; every combination is legal. `params` are
/// written exactly like an inductive's; `fields` reuse the Σ-type field grammar
/// (label optional, like tuple-type fields).
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

/// One field of a `concept` declaration. `is_super` marks a `use`-prefixed
/// field, whose type must elaborate to a concept application (a superclass
/// edge, §4.1).
#[derive(Debug, Clone, PartialEq)]
pub struct ConceptField {
    pub is_super: bool,
    pub label: String,
    /// `Some` for the signature sugar `label(params) -> type_` — the written
    /// parameter list, kept verbatim so the printer round-trips it. `to_core`
    /// undoes the sugar, lowering the field as `label : (params) -> type_`
    /// (see [`ConceptField::desugared_type`]). Never set on a super field.
    pub func_params: Option<Vec<FuncTypeParam>>,
    pub type_: Term,
}

impl ConceptField {
    /// The field's type with the signature sugar undone: the written type when
    /// the field is plain, the Π-type `(params) -> type_` when it was written
    /// `label(params) -> type_`.
    pub fn desugared_type(&self) -> Term {
        match &self.func_params {
            Some(params) => Subterm::FuncType(FuncType {
                params: params.clone(),
                output: self.type_.clone(),
            })
            .into(),
            None => self.type_.clone(),
        }
    }
}

/// One concept parameter: a `record`-style binder plus the `out` polarity
/// marker. An `out`-marked parameter is an output position — excluded from
/// the witness key and pinned by the resolved witness (functional
/// dependencies); unmarked parameters are inputs.
#[derive(Debug, Clone, PartialEq)]
pub struct ConceptParam {
    pub plicity: Plicity,
    pub is_out: bool,
    pub label: String,
    pub type_: Term,
}

/// A `concept` declaration: a record-shaped interface. It lowers to a nominal
/// `record` (representation public) plus a concept-registry entry and, into its
/// own namespace, one method-wrapper `let` per field (§4.1). `params` are
/// written like a `record`'s, each optionally `out`-marked; the result sort is
/// `Type` or `Prop`.
#[derive(Debug, Clone, PartialEq)]
pub struct TopConcept {
    pub is_pub: bool,
    pub label: String,
    pub params: Vec<ConceptParam>,
    pub result_sort: Term,
    pub fields: Vec<ConceptField>,
}

/// One implementation field of a `witness` declaration: `label = value`, or
/// the definition sugar `label(params) = value` — the [`TupleField`] grammar
/// with the label mandatory. The sugar is kept verbatim (the printer
/// round-trips it); `to_core` undoes it when it builds the desugared
/// struct literal.
#[derive(Debug, Clone, PartialEq)]
pub struct WitnessField {
    pub label: String,
    pub func_params: Option<Vec<(String, Option<Term>)>>,
    pub value: Term,
}

/// One entry of a witness body: an implementation field, or a `use <term>`
/// fill for one of the concept's `use`-marked (superclass) field positions —
/// the same entry forms a concept struct literal admits.
#[derive(Debug, Clone, PartialEq)]
pub enum WitnessEntry {
    Field(WitnessField),
    Use(Term),
}

/// A `witness` declaration: a registered inhabitant of a concept. Witnesses
/// are anonymous — they are only ever reached through resolution (or an
/// explicit `use <term>` carrying an ordinary value), so there is no name and
/// no `pub`. The declaration desugars to a compiler-named top-level definition
/// `let witness#N(tele) -> C(args) = C(args) { … }` (§4.3) registered in the
/// program-wide witness table; diagnostics identify it by concept, key, and
/// declaring module. The telescope admits only `@` and `use` parameters
/// (explicit binders are rejected at lowering); `concept`/`args` are the
/// witnessed concept application, reused verbatim as the struct-literal head.
#[derive(Debug, Clone, PartialEq)]
pub struct TopWitness {
    pub params: Vec<FuncSugarParam>,
    pub concept: Name,
    pub args: Vec<Term>,
    pub entries: Vec<WitnessEntry>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Mod(TopMod),
    Use(TopUse),
    Let(TopLet),
    Rec(Vec<TopLet>),
    Induct(Vec<TopInduct>),
    Struct(TopStruct),
    Concept(TopConcept),
    Witness(TopWitness),
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
