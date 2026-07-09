use {
    super::{
        FuncSugarParam, FuncType, FuncTypeParam, LetSignature, LoadError, Name, Subterm, Term,
        TupleTypeParam, print_module_items, print_term,
    },
    crate::parse::{parse_term, parse_top_item, parse_whitespace},
    curios_base::{
        Plicity, Source, Span,
        parser::{ParserError, lazy, many0, run_parser, take_eof},
        printer::{flat, pure, run_printer},
    },
    std::{fmt, path::Path, rc::Rc, str::FromStr},
};

/// A `mod` declaration: `module` is `Some` for an inline body (`mod m … end`) and `None` for the file-backed form (`mod m;`), whose body module discovery loads through the active [`RootSource`](crate::RootSource). The span locates a failed load at the declaration that requested it; like `Term`'s, it is excluded from `PartialEq`.
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

/// One name in a `use` group, carrying which namespace the writer pinned: `mod x` (child module only), `let x` (binding only), or bare `x` (`Both`). Modules and bindings occupy separate namespaces, so one label can name both; a bare item imports whichever exist, requiring at least one.
#[derive(Debug, Clone, PartialEq)]
pub enum GroupItem {
    Mod(String),
    Let(String),
    Both(String),
}

impl GroupItem {
    pub(crate) fn label(&self) -> &str {
        match self {
            GroupItem::Mod(s) | GroupItem::Let(s) | GroupItem::Both(s) => s,
        }
    }
}

/// The tail of a `use` path: a braced list of named items (`use m/{a, mod b};`) or the glob `use m/*;`, which imports everything the module publicly exposes.
#[derive(Debug, Clone, PartialEq)]
pub enum UseGroup {
    Named(Vec<GroupItem>),
    Glob,
}

/// A `use` declaration: imports the `group`'s items from the module `name` names into the local scope at its own source position (scoping is point-of-use, not file-wide). `pub use` additionally re-exports the items from the declaring module — that interface effect is computed in resolution's fixed point, separately from the lexical one.
#[derive(Debug, Clone, PartialEq)]
pub struct TopUse {
    pub is_pub: bool,
    pub name: Name,
    pub group: UseGroup,
}

/// A top-level `let` (or one member of a `rec … and …;` group — see `TopItem::Rec`): a plain label, never a destructuring pattern, and a signature whose type annotation the parser makes mandatory at top level.
#[derive(Debug, Clone, PartialEq)]
pub struct TopLet {
    pub is_pub: bool,
    pub label: String,
    pub signature: LetSignature,
}

/// A `foreign` declaration: a name and a wire signature, bound to a
/// host-provided implementation at link time rather than a Curios definition.
/// `signature` is parsed directly as a [`WireSignature`](curios_abi::WireSignature) (`(Nat, Bin) -> Nat`)
/// — a closed grammar of the six wire shapes, not an ordinary Curios type —
/// so there is no name resolution to do and no `= body` form.
#[derive(Debug, Clone, PartialEq)]
pub struct TopForeign {
    pub is_pub: bool,
    pub label: String,
    pub signature: curios_abi::WireSignature,
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

/// One value constructor of an `induct` declaration: a tag label, its payload telescope (see [`CasePayloadParam`]), and — for an indexed family — the `target` pinning which indices this constructor produces.
#[derive(Debug, Clone, PartialEq)]
pub struct TopCase {
    pub label: String,
    pub payload: Vec<CasePayloadParam>,
    /// The parenthesized index expressions after the payload — the case's
    /// terminal `: Vec(T, Nat/succ(m))` with the mandatory part elided to
    /// `: (Nat/succ(m))`. Present iff the inductive head declares indices.
    pub target: Option<Vec<Term>>,
}

/// An `induct` declaration: one inductive family — a parameter telescope, an index telescope with its result sort, and the value-constructor cases. Lowering registers the family and derives both the type-constructor function and one value-constructor function per case; see the field docs for the plicity and dependency rules each part obeys.
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
    /// parameter list, kept verbatim so the printer round-trips it. `into_core`
    /// undoes the sugar, lowering the field as `label : (params) -> type_`
    /// (see `ConceptField::desugared_type`). Never set on a super field.
    pub func_params: Option<Vec<FuncTypeParam>>,
    pub type_: Term,
}

impl ConceptField {
    /// The field's type with the signature sugar undone: the written type when
    /// the field is plain, the Π-type `(params) -> type_` when it was written
    /// `label(params) -> type_`.
    pub(crate) fn desugared_type(&self) -> Term {
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
/// the definition sugar `label(params) = value` — the [`TupleField`](crate::TupleField) grammar
/// with the label mandatory. The sugar is kept verbatim (the printer
/// round-trips it); `into_core` undoes it when it builds the desugared
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

/// One top-level item of a module, one variant per declaration keyword. `Rec` and `Induct` hold groups because both keywords chain with `and` — a `rec f … and g …;` group of mutually recursive definitions, an `induct … and … end` group of mutually recursive inductive families.
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
    Foreign(TopForeign),
}

/// A parsed module body: its top-level items in source order — an order that matters, since `use` scoping is point-of-use and flat-item order is the downstream topological-sort tiebreak. Parsed via `FromStr`, loaded from disk through a [`RootSource`](crate::RootSource), or built synthetically (the embedded `sys` prelude).
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub items: Vec<TopItem>,
}

impl Module {
    pub(crate) fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and_drop(take_eof())
                .map(|items| Module { items }),
            source,
        )
    }

    pub(crate) fn from_path(path: impl AsRef<Path>) -> Result<Self, LoadError> {
        let path = path.as_ref();
        let source = Source::read(path).map_err(|error| LoadError::Read {
            path: path.into(),
            error,
        })?;

        Module::parse(&source).map_err(LoadError::Parse)
    }
}

impl FromStr for Module {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Module::parse(&Source::inline(input))
    }
}

impl fmt::Display for Module {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_module_items(self.clone().items), formatter, 2)
    }
}

/// A whole program as written: a module of top-level items followed by one tail expression — the value the program computes. Parsed via `FromStr` (inline source) or [`Entrypoint::from_path`]; the grammar has no position for `type_`, which is only ever attached afterwards via [`Entrypoint::with_type`].
#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub module: Module,
    pub type_: Option<Term>,
    pub tail: Term,
}

impl Entrypoint {
    pub(crate) fn new(items: Vec<TopItem>, tail: Term) -> Self {
        Self {
            module: Module { items },
            type_: None,
            tail,
        }
    }

    /// Attaches an expected type to the tail expression. The entrypoint grammar has no annotation position for the tail, so this is how embedders (today, the test suites) request `Check` rather than `Infer` mode — `into_core` lowers the annotation alongside the tail and the pipeline elaborates against it.
    pub fn with_type(self, type_: Term) -> Self {
        Self {
            type_: Some(type_),
            ..self
        }
    }
}

impl Entrypoint {
    fn parse(source: &Rc<Source>) -> Result<Self, ParserError> {
        run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and(lazy(parse_term))
                .and_drop(take_eof())
                .map(|(items, tail)| Entrypoint::new(items, tail)),
            source,
        )
    }

    /// Reads and parses `path` as an entrypoint (top-level items followed by a tail expression). The file-path counterpart of the `FromStr` impl below, distinguished by keeping the real path in the [`Source`](curios_base::Source) so diagnostics name the file; a parsed `Entrypoint` resolves its file-backed `mod` declarations separately, through whatever [`RootSource`](crate::RootSource) the caller pairs it with.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, LoadError> {
        let path = path.as_ref();
        let source = Source::read(path).map_err(|error| LoadError::Read {
            path: path.into(),
            error,
        })?;

        Entrypoint::parse(&source).map_err(LoadError::Parse)
    }
}

impl FromStr for Entrypoint {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Entrypoint::parse(&Source::inline(input))
    }
}

impl fmt::Display for Entrypoint {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entrypoint = self.clone();
        let printer = if entrypoint.module.items.is_empty() {
            print_term(entrypoint.tail)
        } else {
            flat([
                print_module_items(entrypoint.module.items),
                pure("\n"),
                print_term(entrypoint.tail),
            ])
        };
        run_printer(printer, formatter, 2)
    }
}
