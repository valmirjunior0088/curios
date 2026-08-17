use {
    super::{
        FuncSugarParam, FuncType, FuncTypeParam, LetSignature, LoadError, Name, RootSource,
        Subterm, Term, TupleTypeParam, print_module_items, print_term,
    },
    crate::parse::{
        clear_comments, parse_optional_term, parse_term, parse_top_item, parse_whitespace,
        take_comments,
    },
    curios_abi::WireSignature,
    curios_parse::{ParserError, lazy, many0, run_parser, spanned, take_eof},
    curios_print::{flat, pure, run_printer},
    curios_utilities::{Plicity, Source, Span},
    std::{fmt, path::Path, rc::Rc, str::FromStr},
};

/// A `mod` declaration: `module` is `Some` for an inline body (`mod m … end`) and `None` for the file-backed form (`mod m;`), whose body module discovery loads through the active [`RootSource`](crate::RootSource). The span locates a failed load at the declaration that requested it; like `Term`'s, it is excluded from `PartialEq`.
#[derive(Debug, Clone)]
pub struct TopMod {
    pub span: Option<Span>,
    pub vis_pub: bool,
    pub label: String,
    pub module: Option<Module>,
}

impl PartialEq for TopMod {
    fn eq(&self, other: &Self) -> bool {
        self.vis_pub == other.vis_pub && self.label == other.label && self.module == other.module
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
    pub vis_pub: bool,
    pub name: Name,
    pub group: UseGroup,
}

/// A top-level `let` (or one member of a `rec … and …;` group — see `TopItem::Rec`): a plain label, never a destructuring pattern, and a signature whose type annotation the parser makes mandatory at top level.
#[derive(Debug, Clone, PartialEq)]
pub struct TopLet {
    pub vis_pub: bool,
    pub label: String,
    pub signature: LetSignature,
}

/// A `foreign` declaration: a name and a wire signature, bound to a host-provided implementation at link time rather than a Curios definition. `signature` is parsed directly as a [`WireSignature`] (`(Nat, Bytes) -> Nat`) — a closed grammar of the six wire types, not an ordinary Curios type — so there is no name resolution to do and no `= body` form.
#[derive(Debug, Clone, PartialEq)]
pub struct TopForeign {
    pub vis_pub: bool,
    pub label: String,
    pub signature: WireSignature,
}

/// One payload binder of an `induct` case. The name is optional (`success(A)` stays positional); it is required when a later payload type or the case target mentions the binder. `plicity` is the `@`-on-the-name mark (implicit at the value-constructor function — `cons(@m : Nat, …)`, `m` recoverable from a later payload's type). Erasure is sort-driven, so no per-field mark is kept.
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
    /// The parenthesized index expressions after the payload — the case's terminal `: Vec(T, Nat/succ(m))` with the mandatory part elided to `: (Nat/succ(m))`. Present iff the inductive head declares indices.
    pub target: Option<Vec<Term>>,
}

/// An `induct` declaration: one inductive family — a parameter telescope, an index telescope with its result sort, and the value-constructor cases. Lowering registers the family and derives both the type-constructor function and one value-constructor function per case; see the field docs for the plicity and dependency rules each part obeys.
#[derive(Debug, Clone, PartialEq)]
pub struct TopInduct {
    pub vis_pub: bool,
    /// Whether construction and elimination are available outside the exact declaring module. Written as `pub` immediately before the result sort.
    pub rep_pub: bool,
    pub label: String,
    /// Inductive parameters are *implicit* on every value constructor regardless of any mark (the desugar applies those marks), with the call-site `@` available to supply one positionally when wanted. On the type-constructor function a parameter is *explicit* by default (types are written out); a declaration-site `@` makes it implicit there too (`induct Eq(@A : Type) : (x : A, y : A)` — `A` is recoverable from the indices, so types are written `Eq(x, y)`).
    pub params: Vec<(Plicity, String, Term)>,
    /// The head's index telescope, `induct Vec(T : Type) : (n : Nat)`. Names are optional and documentary — needed only when a later index's type depends on an earlier one; they are *not* in scope in the cases.
    pub indices: Vec<(Option<String>, Term)>,
    /// The arity's result sort — `Type` or `Prop`. Written after the index telescope (`: (n : Nat) -> Prop`) or in its place when there are no indices (`: Prop`); defaults to `Type` when omitted.
    pub result_sort: Term,
    pub cases: Vec<TopCase>,
}

/// A `struct` declaration: a nominal record. `vis_pub` is the outer `pub` (the type-former's visibility); `rep_pub` is the declaration-local `pub` before the result sort (representation exported wherever the type name is visible). The two markers are orthogonal; every combination is legal. `params` are written exactly like an inductive's; `fields` reuse the Σ-type field grammar (label optional, like tuple-type fields).
#[derive(Debug, Clone, PartialEq)]
pub struct TopStruct {
    pub vis_pub: bool,
    pub rep_pub: bool,
    pub label: String,
    pub params: Vec<(Plicity, String, Term)>,
    /// The result sort — `Type` or `Prop`, written `: Sort` after the parameters; defaults to `Type` when omitted.
    pub result_sort: Term,
    pub fields: Vec<TupleTypeParam>,
}

/// One field of a `concept` declaration. `is_super` marks a `use`-prefixed field, whose type must elaborate to a concept application (a superclass edge).
#[derive(Debug, Clone, PartialEq)]
pub struct ConceptField {
    pub is_super: bool,
    pub label: String,
    /// `Some` for the signature sugar `label(params) -> type_` — the written parameter list, kept verbatim so the printer round-trips it. `into_core` undoes the sugar, lowering the field as `label : (params) -> type_` (see `ConceptField::desugared_type`). Never set on a super field.
    pub func_params: Option<Vec<FuncTypeParam>>,
    pub type_: Term,
}

impl ConceptField {
    /// The field's type with the signature sugar undone: the written type when the field is plain, the Π-type `(params) -> type_` when it was written `label(params) -> type_`.
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

/// A `concept` declaration: a record-shaped interface. It lowers to a representation-public nominal structure plus a concept-registry entry and, into its own namespace, one method-wrapper `let` per field.
#[derive(Debug, Clone, PartialEq)]
pub struct TopConcept {
    pub vis_pub: bool,
    /// Representation visibility, independent from `vis_pub` (the name's): `: pub Type` is transparent, `: Type` is sealed — witnesses and dictionary literals only in the declaring module, exactly like a private-representation struct.
    pub rep_pub: bool,
    pub label: String,
    pub params: Vec<(Plicity, String, Term)>,
    pub result_sort: Term,
    pub fields: Vec<ConceptField>,
}

/// One implementation field of a `witness` declaration: `label = value`, or the definition sugar `label(params) = value` — the [`TupleField`](crate::TupleField) grammar with the label mandatory. The sugar is kept verbatim (the printer round-trips it); `into_core` undoes it when it builds the desugared struct literal.
#[derive(Debug, Clone, PartialEq)]
pub struct WitnessField {
    pub label: String,
    pub func_params: Option<Vec<(Plicity, String, Option<Term>)>>,
    pub value: Term,
}

/// One entry of a witness body: an implementation field, or a `use <term>` fill for one of the concept's `use`-marked (superclass) field positions — the same entry forms a concept struct literal admits.
#[derive(Debug, Clone, PartialEq)]
pub enum WitnessEntry {
    Field(WitnessField),
    Use(Term),
}

/// A `witness` declaration: a registered inhabitant of a concept. Witnesses are anonymous — they are only ever reached through resolution (or an explicit `use <term>` carrying an ordinary value), so there is no name and no `pub`. The declaration desugars to a compiler-named top-level definition `let witness@N(tele) -> C(args) = C(args) { … }` registered in the program-wide witness table; diagnostics identify it by concept, key, and declaring module. Surface syntax writes a nonempty telescope as `satisfy (tele) => C(args) { … }`; the telescope admits only `@` and `use` parameters (explicit binders are rejected at lowering). `concept`/`args` are the witnessed concept application, reused verbatim as the struct-literal head.
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
        Self::parse_with_comments(source).map(|(module, _)| module)
    }

    /// [`Module::parse`] (by way of `FromStr`), additionally returning every comment the parse consumed — spans into `source`, ascending. (The formatter itself goes through `parse_for_format`, whose `FormatInput` carries its own comment list; this pairing exists for the comment-visibility tests.) Comments are not syntax: they live beside the module, never in it, so the parsed module is identical either way and nothing downstream changes.
    pub(crate) fn parse_with_comments(
        source: &Rc<Source>,
    ) -> Result<(Self, Vec<Span>), ParserError> {
        clear_comments();
        let module = run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and_drop(take_eof())
                .map(|items| Module { items }),
            source,
        )?;
        Ok((module, take_comments()))
    }

    /// Read and parse a standalone module while retaining its source path for diagnostics. The prelude artifact builder uses this for `/syn` and `/std`; ordinary compilation reaches file-backed modules through [`RootSource`](crate::RootSource).
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, LoadError> {
        Self::read(path.as_ref()).map(|(module, _)| module)
    }

    /// [`Module::from_path`], additionally handing back the [`Source`] it parsed.
    ///
    /// For [`RootSource`](crate::RootSource), which records what it read so a cache can verify a stored unit against the exact text that produced it. Handing the source back rather than the file's bytes is what keeps that record free: the parsed module's spans already hold this `Rc`, so retaining it costs a refcount.
    pub(crate) fn read(path: &Path) -> Result<(Self, Rc<Source>), LoadError> {
        let source = Source::read(path).map_err(|error| LoadError::Read {
            path: path.into(),
            error,
        })?;

        Module::parse(&source)
            .map(|module| (module, source))
            .map_err(LoadError::Parse)
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
        run_printer(print_module_items(self.clone().items), formatter, 4)
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
        Self::parse_with_comments(source).map(|(entrypoint, _)| entrypoint)
    }

    /// The entrypoint counterpart of [`Module::parse_with_comments`]: parse plus every consumed comment as spans into `source`, ascending.
    pub(crate) fn parse_with_comments(
        source: &Rc<Source>,
    ) -> Result<(Self, Vec<Span>), ParserError> {
        clear_comments();
        let entrypoint = run_parser(
            parse_whitespace()
                .and_keep(many0(parse_top_item))
                .and(lazy(parse_term))
                .and_drop(take_eof())
                .map(|(items, tail)| Entrypoint::new(items, tail)),
            source,
        )?;
        Ok((entrypoint, take_comments()))
    }
}

/// The formatter's parse product, one grammar for both file kinds: top-level items followed by an *optional* tail expression — a program has one, a prelude-style module file does not. The item/tail interlock is the entrypoint grammar's own: a top-level `let` requires its annotation, so an unannotated binding falls through to the tail as a local `let` term.
pub(crate) struct FormatInput {
    pub(crate) module: Module,
    /// Each item's span, parallel to `module.items` — the items themselves carry none.
    pub(crate) item_spans: Vec<Span>,
    /// The tail expression when the file is a program; its own term span carries its position.
    pub(crate) tail: Option<Term>,
    /// Every comment the parse consumed, ascending.
    pub(crate) comments: Vec<Span>,
}

pub(crate) fn parse_for_format(source: &Rc<Source>) -> Result<FormatInput, ParserError> {
    clear_comments();
    let (module, item_spans, tail) = run_parser(
        parse_whitespace()
            .and_keep(many0(|| spanned(parse_top_item())))
            .and(parse_optional_term())
            .and_drop(take_eof())
            .map(|(items, tail)| {
                let (spans, items): (Vec<_>, Vec<_>) = items.into_iter().unzip();
                (Module { items }, spans, tail)
            }),
        source,
    )?;
    Ok(FormatInput {
        module,
        item_spans,
        tail,
        comments: take_comments(),
    })
}

impl Entrypoint {
    /// Reads and parses `path` as an entrypoint (top-level items followed by a tail expression). The file-path counterpart of the `FromStr` impl below, distinguished by keeping the real path in the [`Source`](Source) so diagnostics name the file; a parsed `Entrypoint` resolves its file-backed `mod` declarations separately, through whatever [`RootSource`](crate::RootSource) the caller pairs it with.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, LoadError> {
        Self::sourced(path).map(|(entrypoint, _)| entrypoint)
    }

    /// [`from_path`](Self::from_path), handing back the text that was parsed as well as what it parsed to.
    fn sourced(path: impl AsRef<Path>) -> Result<(Self, Rc<Source>), LoadError> {
        let path = path.as_ref();
        let source = Source::read(path).map_err(|error| LoadError::Read {
            path: path.into(),
            error,
        })?;

        Entrypoint::parse(&source)
            .map(|entrypoint| (entrypoint, source))
            .map_err(LoadError::Parse)
    }

    /// Reads `path` as an entrypoint, paired with the [`RootSource`](crate::RootSource) its own stem directory anchors and the text that was parsed — a bare file is a header like any other, so `mod util` in `main.crs` reads `main/util.crs`.
    ///
    /// The pairing is the point: [`from_path`](Self::from_path) leaves a parsed entrypoint's file-backed `mod` declarations unresolved, and every caller that opens a file then has to know which `RootSource` goes with it. That is one answer, not a caller's choice, so it lives beside the two calls it makes.
    ///
    /// **The source comes back because the entry's own header is the one file no loader records.** A `RootSource` logs what it resolves, and the entry's header is deliberately never resolved through it — the caller already has the body. A cache that verifies a compilation against what it read therefore has to be handed the entry separately, and it must be *this* text rather than a re-read of the path: re-reading races an edit landing between the parse and the digest, and records newer text against an older artifact, which is the one direction that admits stale.
    pub fn opened(path: &Path) -> Result<(Self, RootSource, Rc<Source>), String> {
        let (entrypoint, source) = Self::sourced(path).map_err(|error| error.format())?;

        Ok((entrypoint, RootSource::entry(path), source))
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
        run_printer(printer, formatter, 4)
    }
}
