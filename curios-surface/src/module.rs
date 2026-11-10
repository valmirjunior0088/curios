use {
    super::{
        Argument, Dialect, FieldSugarParam, FuncSugarParam, FuncTypeParam, Intrinsic, Label,
        LetSignature, Name, Program, Subterm, Term, Tuple, TupleType, TupleTypeParam,
        clear_comments, parse_file_head, parse_optional_term, parse_program_end, parse_term,
        parse_top_item, print_module_items, print_term, signature_type, take_comments,
    },
    curios_abi::WireSignature,
    curios_common::{Plicity, Report, Source, Span},
    curios_parse::{
        Parsed, Parser, ParserError, ParserState, Recovered, commit, fail, is_blank, many0, raise,
        recover, run_parser, settled, spanned, take_eof,
    },
    curios_print::{flat, pure, run_printer},
    std::{fmt, path::Path, str::FromStr, sync::Arc},
};

/// A documentation comment: the `---` block written on the lines immediately above a declaration, a constructor, a field or a concept method, attached by the parser to what it documents. A plain `-- ` comment is not syntax and lives beside the module; this is syntax and lives in it, which is what lets the formatter print it back where it was written and a generator read it off the tree.
///
/// The span runs from the block's first `---` to where the documented head begins, past the whitespace and plain comments between — two positions the formatter marks, so a plain comment written above the block is paid above it and one written between the block and its declaration is paid between them. Like every span it is excluded from equality, and `None` on a tree built without source.
#[derive(Debug, Clone)]
pub struct Doc {
    /// One entry per `---` line, holding the text after `--- ` verbatim; an empty entry is a paragraph break, written as a bare `---`.
    pub lines: Vec<String>,
    pub span: Option<Span>,
}

impl PartialEq for Doc {
    fn eq(&self, other: &Self) -> bool {
        self.lines == other.lines
    }
}

/// A `mod` declaration: `module` is `Some` for an inline body (`mod m … end`) and `None` for the file-backed form (`mod m;`), whose body module discovery loads through the active `RootSource` (`curios-module`). The span locates a failed load at the declaration that requested it; like `Term`'s, it is excluded from `PartialEq`. The documentation comment above a `mod` documents the module it declares, which is the one place a module's prose lives.
#[derive(Debug, Clone)]
pub struct TopMod {
    pub doc: Option<Doc>,
    pub span: Option<Span>,
    pub vis_pub: bool,
    pub label: Label,
    pub module: Option<Module>,
}

impl PartialEq for TopMod {
    fn eq(&self, other: &Self) -> bool {
        self.doc == other.doc
            && self.vis_pub == other.vis_pub
            && self.label == other.label
            && self.module == other.module
    }
}

/// One name in a `use` group, carrying which namespace the writer pinned: `mod x` (child module only), `let x` (binding only), or bare `x` (`Both`). Modules and bindings occupy separate namespaces, so one label can name both; a bare item imports whichever exist, requiring at least one.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum GroupItem {
    Mod(Label),
    Let(Label),
    Both(Label),
}

impl GroupItem {
    pub fn label(&self) -> &Label {
        match self {
            GroupItem::Mod(label) | GroupItem::Let(label) | GroupItem::Both(label) => label,
        }
    }
}

impl TopUse {
    /// Place this declaration in `source` — see [`Name::rebase`].
    pub fn rebase(&mut self, source: &Arc<Source>) {
        if let Some(span) = &mut self.span {
            span.source = Arc::clone(source);
        }
        self.name.rebase(source);
        if let UseGroup::Named(items) = &mut self.group {
            for item in items {
                match item {
                    GroupItem::Mod(label) | GroupItem::Let(label) | GroupItem::Both(label) => {
                        label.rebase(source)
                    }
                }
            }
        }
    }
}

/// The tail of a `use` path: a braced list of named items (`use m/{a, mod b};`) or the glob `use m/*;`, which imports everything the module publicly exposes.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum UseGroup {
    Named(Vec<GroupItem>),
    Glob,
}

/// A `use` declaration: imports the `group`'s items from the module `name` names into the local scope at its own source position (scoping is point-of-use, not file-wide). `pub use` additionally re-exports the items from the declaring module — that interface effect is computed in resolution's fixed point, separately from the lexical one. The span covers the whole declaration, `use` through `;`, and is excluded from `PartialEq` as `TopMod`'s is.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub struct TopUse {
    pub span: Option<Span>,
    pub vis_pub: bool,
    pub name: Name,
    pub group: UseGroup,
}

impl PartialEq for TopUse {
    fn eq(&self, other: &Self) -> bool {
        self.vis_pub == other.vis_pub && self.name == other.name && self.group == other.group
    }
}

/// One member of a top-level `let` item — a lone definition, or one member of a `let … and …;` group (see `TopItem::Let`): a plain label, never a destructuring pattern, and a signature whose type annotation the parser makes mandatory at top level.
#[derive(Debug, Clone, PartialEq)]
pub struct TopLet<D: Dialect = Program> {
    pub doc: Option<Doc>,
    pub core: D::Head,
    pub vis_pub: bool,
    pub label: Label,
    pub signature: LetSignature<D>,
}

/// A `foreign` declaration: a function with no body, answered where the program is linked rather than by a Curios definition. It is written as one, `foreign frobnicate(Nat, Bytes) -> Nat;`, its operands and its result read by the wire's own grammar — a closed vocabulary, not ordinary Curios types — so there is no name resolution to do. No colon stands between the name and the list: what follows a colon is a name's type, and a row's signature is not its type, whose result is an `Io`.
#[derive(Debug, Clone, PartialEq)]
pub struct TopForeign<D: Dialect = Program> {
    pub doc: Option<Doc>,
    pub core: D::Head,
    pub vis_pub: bool,
    pub label: Label,
    pub signature: WireSignature,
}

/// One payload binder of an `induct` case. The name is optional (`success(A)` stays positional); it is required when a later payload type or the case target mentions the binder. `plicity` is plain or `@` — implicit at the value-constructor function, named (`cons(@m : Nat, …)`, `m` recoverable from a later payload's type) or not (`at(n : Nat, @Holds(0 < n))`); the parser refuses `use` here. Erasure is sort-driven, so no per-field mark is kept.
#[derive(Debug, Clone, PartialEq)]
pub struct CasePayloadParam<D: Dialect = Program> {
    pub plicity: Plicity,
    pub label: Option<String>,
    pub type_: Term<D>,
}

/// One value constructor of an `induct` declaration: a tag label, its payload telescope (see [`CasePayloadParam`]), and — for an indexed family — the `target` pinning which indices this constructor produces.
#[derive(Debug, Clone, PartialEq)]
pub struct TopCase<D: Dialect = Program> {
    pub doc: Option<Doc>,
    /// What the decorator before the case states: Core text's, and nothing in a program.
    pub core: D::Fields,
    /// The constructor's name, carrying the span of the word alone. A case is reached as a path segment (`Color/red`), so this is a declaring position: it refuses a keyword, and a refusal about it — a repeated case, above all — points at the word.
    pub label: Label,
    pub payload: Vec<CasePayloadParam<D>>,
    /// The parenthesized index expressions after the payload — the case's terminal `: Vec(T, Nat/succ(m))` with the mandatory part elided to `: (Nat/succ(m))`. Present iff the inductive head declares indices.
    pub target: Option<Vec<Term<D>>>,
}

/// An `induct` declaration: one inductive family — a parameter telescope, an index telescope with its result sort, and the value-constructor cases. Lowering registers the family and derives both the type-constructor function and one value-constructor function per case; see the field docs for the plicity and dependency rules each part obeys.
#[derive(Debug, Clone, PartialEq)]
pub struct TopInduct<D: Dialect = Program> {
    pub doc: Option<Doc>,
    pub core: D::Head,
    pub vis_pub: bool,
    /// Whether construction and elimination are available outside the exact declaring module. Written as `pub` immediately before the result sort.
    pub rep_pub: bool,
    pub label: Label,
    /// The parameters of the type constructor's function type, and written as one's are: a named plain or `@` parameter, or a `use` premise, which has no name. A plain or `@` parameter is *implicit* on every value constructor regardless of its mark (the desugar applies those marks), with the call-site `@` available to supply one positionally when wanted. On the type-constructor function a parameter is *explicit* by default (types are written out); a declaration-site `@` makes it implicit there too (`induct Eq(@A : Type) : (x : A, y : A)` — `A` is recoverable from the indices, so types are written `Eq()(x, y)`). A `use` parameter is a witness slot at both: the type names the dictionary resolution found for it, and a value constructor finds the same one.
    pub params: Vec<FuncTypeParam<D>>,
    /// The head's index telescope, `induct Vec(T : Type) : (n : Nat)`. Names are optional and documentary — needed only when a later index's type depends on an earlier one; they are *not* in scope in the cases.
    pub indices: Vec<(Option<String>, Term<D>)>,
    /// The arity's result sort — `Type` or `Prop`. Written after the index telescope (`: (n : Nat) -> Prop`) or in its place when there are no indices (`: Prop`); defaults to `Type` when omitted.
    pub result_sort: Term<D>,
    pub cases: Vec<TopCase<D>>,
}

/// One field of a `struct` declaration: its mark, the Σ-type field it is written as, and the documentation comment above it. Wrapped rather than fields on [`TupleTypeParam`], because a tuple type's fields are never documented and take no mark, and a slot that only one of a type's two grammars can fill is a slot every reader of the other must ignore.
#[derive(Debug, Clone, PartialEq)]
pub struct StructField<D: Dialect = Program> {
    pub doc: Option<Doc>,
    /// Plain or `@`: a hidden field is left out of a literal and filled as a call's omitted `@` argument is. Never `use` — a structure's premise is a parameter.
    pub plicity: Plicity,
    pub param: TupleTypeParam<D>,
}

/// A `struct` declaration: a nominal record. `vis_pub` is the outer `pub` (the type-former's visibility); `rep_pub` is the declaration-local `pub` before the result sort (representation exported wherever the type name is visible). The two markers are orthogonal; every combination is legal. `params` are written exactly like an inductive's; `fields` reuse the Σ-type field grammar (label optional, like tuple-type fields).
#[derive(Debug, Clone, PartialEq)]
pub struct TopStruct<D: Dialect = Program> {
    pub doc: Option<Doc>,
    pub core: D::Head,
    pub vis_pub: bool,
    pub rep_pub: bool,
    pub label: Label,
    pub params: Vec<FuncTypeParam<D>>,
    /// The result sort — `Type` or `Prop`, written `: Sort` after the parameters; defaults to `Type` when omitted.
    pub result_sort: Term<D>,
    pub fields: Vec<StructField<D>>,
}

/// One field of a `concept` declaration: a method, or a `use`-prefixed superclass edge, whose type must elaborate to a concept application.
#[derive(Debug, Clone, PartialEq)]
pub struct ConceptField<D: Dialect = Program> {
    pub doc: Option<Doc>,
    /// Plain for a method, `use` for a superclass edge, which is the field telescope's witness member.
    pub plicity: Plicity,
    /// The method's name, carrying the span of the word alone, as [`TopCase::label`] does and for the same reasons — a field is reached as a path segment (`Show/show`). A superclass edge has none in a program, which reaches it by resolution, and the word a later field reads it by in Core text, where a binder is a variable.
    pub label: Option<Label>,
    /// `Some` for the signature sugar `label(params) -> type_` — the written parameter list, kept verbatim so the printer round-trips it. `into_core` undoes the sugar, lowering the field as `label : (params) -> type_` (see `ConceptField::desugared_type`). Never set on a superclass edge.
    pub func_params: Option<Vec<FuncTypeParam<D>>>,
    pub type_: Term<D>,
}

impl<D: Dialect> ConceptField<D> {
    /// Whether this field is a superclass edge rather than a method.
    pub fn is_super(&self) -> bool {
        self.plicity == Plicity::Witness
    }

    /// The field's type with the signature sugar undone: the written type when the field is plain, the Π-type `(params) -> type_` when it was written `label(params) -> type_`.
    pub fn desugared_type(&self) -> Term<D> {
        match &self.func_params {
            Some(params) => signature_type(self.label.as_ref(), params, &self.type_),
            None => self.type_.clone(),
        }
    }
}

/// A `concept` declaration: a record-shaped interface. It lowers to a representation-public nominal structure plus a concept-registry entry and, into its own namespace, one method-wrapper `let` per field.
#[derive(Debug, Clone, PartialEq)]
pub struct TopConcept<D: Dialect = Program> {
    pub doc: Option<Doc>,
    pub core: D::Head,
    pub vis_pub: bool,
    /// Representation visibility, independent from `vis_pub` (the name's): `: pub Type` is transparent, `: Type` is sealed — witnesses and dictionary literals only in the declaring module, exactly like a private-representation struct.
    pub rep_pub: bool,
    pub label: Label,
    /// Written as a `struct`'s are. A `use` parameter parses and is refused at lowering: a concept's premise is a superclass field.
    pub params: Vec<FuncTypeParam<D>>,
    pub result_sort: Term<D>,
    pub fields: Vec<ConceptField<D>>,
}

/// One implementation field of a `witness` declaration: `label = value`, or the definition sugar `label(params) = value` — the [`TupleField`](crate::TupleField) grammar with the label mandatory. The sugar is kept verbatim (the printer round-trips it); `into_core` undoes it when it builds the desugared struct literal.
#[derive(Debug, Clone, PartialEq)]
pub struct WitnessField {
    pub label: String,
    pub func_params: Option<Vec<FieldSugarParam>>,
    pub value: Term,
}

/// A `witness` declaration: a registered inhabitant of a concept. Witnesses are anonymous — they are only ever reached through resolution (or an explicit `use <term>` carrying an ordinary value), so there is no name and no `pub`. The declaration desugars to a compiler-named top-level definition `let witness~N(tele) -> C(args) = C { … }` registered in the program-wide witness table; diagnostics identify it by concept, key, and declaring module. Surface syntax writes a nonempty telescope as `satisfy (tele) => C(args) { … }`; the telescope admits only `@` and `use` parameters (explicit binders are rejected at lowering). `concept`/`args` are the witnessed concept application — an application like any other, so its arguments take the marks a call's do — which is the witness's declared type and what its body is checked against. The body is written, or `..` alone as `satisfy C(args) { .. }` — the derived form, whose body the compiler writes.
#[derive(Debug, Clone)]
pub struct TopWitness {
    pub doc: Option<Doc>,
    pub params: Vec<FuncSugarParam>,
    pub concept: Name,
    /// The concept's argument list where one was written: `None` for a bare concept name, and the empty list for `C()`, the application with nothing written.
    pub args: Option<Vec<Argument>>,
    /// Where the witnessed concept application was written, from the concept's name through the parenthesis that closes its arguments, or the name alone where it takes none: what a refusal about the witness points at, a witness having no name of its own. Excluded from `PartialEq`, as a `Term`'s span is.
    pub head: Option<Span>,
    /// The written fields, or `None` for the derived form. A body holds implementation fields and nothing else: the concept's `use`-marked (superclass) positions are never written in a witness, so resolution fills each one and every path to a superclass finds the one registered witness.
    pub body: Option<Vec<WitnessField>>,
}

impl PartialEq for TopWitness {
    fn eq(&self, other: &Self) -> bool {
        self.doc == other.doc
            && self.params == other.params
            && self.concept == other.concept
            && self.args == other.args
            && self.body == other.body
    }
}

/// A `test` declaration: `test name = body;` — a named description of type `/std/Test`, taking no parameters. It is not function sugar, though it lowers to a `() -> Test({})` thunk: only the selected test's body is forced.
#[derive(Debug, Clone, PartialEq)]
pub struct TopTest<D: Dialect = Program> {
    pub core: D::Head,
    pub label: Label,
    pub body: Term<D>,
}

/// One top-level item of a module, one variant per declaration keyword. Every declaration keyword chains with `and`, so every variant but `Mod`, `Use` and `Foreign` holds a group: a `let f … and g …;` group of mutually recursive definitions, an `induct … and … end` group of mutually recursive inductive families, `struct` and `concept` groups whose fields name one another, and a `satisfy C(A) { … } and D(B) { … }` group of witnesses that resolve through one another. A lone `let` is a group of one; whether it recurses is read off its body, never declared.
#[derive(Debug, Clone, PartialEq)]
pub enum TopItem {
    Mod(TopMod),
    Use(TopUse),
    Let(Vec<TopLet>),
    Induct(Vec<TopInduct>),
    Struct(Vec<TopStruct>),
    Concept(Vec<TopConcept>),
    Witness(Vec<TopWitness>),
    Foreign(TopForeign),
    Test(TopTest),
    Broken(TopBroken),
}

/// An item the parser could not read: what it reported, the text it skipped to resume past it, and the name its head declared where the head parsed that far. Kept in the list rather than beside it so the module is an honest record of the file — every consumer says what it does with one, lowering registers the name so a reference to it resolves and is withheld rather than reported unbound, and nothing that reaches erasure ever holds one. See `Module::parse` for what recovers, and what refuses instead.
#[derive(Debug, Clone, PartialEq)]
pub struct TopBroken {
    pub span: Span,
    pub report: Report,
    /// `None` when the head did not parse as far as a name, and for a `mod`, which declares a scope rather than a binding.
    pub declares: Option<Label>,
}

impl TopBroken {
    /// The text the item stood in, verbatim — what a printer shows for it, since there is no tree to print.
    pub fn text(&self) -> &str {
        &self.span.source.text[self.span.start..self.span.end]
    }
}

/// Where an item could next begin after a failure reported at `from`: the start of the next line that begins at column 0 with `pub`, a `---` documentation comment, or an item's head word, followed by whitespace. Column 0 is the formatter's convention rather than the grammar's, so code that ignores it only recovers less; a wrong candidate cannot produce a wrong tree, since the item parser runs at it and a failure that does not commit only ends the loop. `None` past the last line.
fn next_item_anchor(text: &str, from: usize) -> Option<usize> {
    let mut at = match from == 0 || text[..from].ends_with('\n') {
        true => from,
        false => from + text[from..].find('\n')? + 1,
    };

    loop {
        let rest = &text[at..];
        if rest.is_empty() {
            return None;
        }
        if begins_item(rest) {
            return Some(at);
        }
        at += rest.find('\n')? + 1;
    }
}

fn begins_item(line: &str) -> bool {
    if line.starts_with("---") {
        return true;
    }
    let word = line
        .split(|character: char| !character.is_alphanumeric() && character != '_')
        .next()
        .unwrap_or("");
    let head = matches!(
        word,
        "pub"
            | "mod"
            | "use"
            | "induct"
            | "struct"
            | "foreign"
            | "concept"
            | "satisfy"
            | "test"
            | "let"
    );

    head && line[word.len()..].starts_with(is_blank)
}

/// The item loop, recovering past a broken item ([`recover`]) or, when `strict`, refusing at the first one with its own error: the whole-or-nothing reading `str::parse` gives.
fn parse_items<'a>(strict: bool) -> impl Parser<'a, Vec<Recovered<TopItem>>> {
    move |state: ParserState<'a>| {
        recover(parse_top_item, next_item_anchor)
            .flat_map(move |recovered| {
                settled(match (strict, first_broken(&recovered)) {
                    (true, Some(error)) => Err(error),
                    _ => Ok(recovered),
                })
            })
            .parse(state)
    }
}

fn first_broken(recovered: &[Recovered<TopItem>]) -> Option<ParserError> {
    recovered.iter().find_map(|item| match item {
        Recovered::Broken { error, .. } => Some(error.clone()),
        Recovered::Item { .. } => None,
    })
}

/// The recovered list as a module, a broken item carried as [`TopItem::Broken`], each item with the text its parse consumed.
fn into_module(recovered: Vec<Recovered<TopItem>>) -> Module {
    let (items, spans) = recovered
        .into_iter()
        .map(|item| match item {
            Recovered::Item { item, span } => (item, span),
            Recovered::Broken { error, span } => (
                TopItem::Broken(TopBroken {
                    report: error.report(),
                    declares: error.tagged().map(Label::from),
                    span: span.clone(),
                }),
                span,
            ),
        })
        .unzip();

    Module { items, spans }
}

/// A parsed module body: its top-level items in source order — an order that matters, since `use` scoping is point-of-use and the flat items leave the lowering in it. Parsed via `FromStr`, loaded from disk through `curios-module`'s `RootSource`, or built synthetically (the embedded `sys` prelude).
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<TopItem>,
    /// Where each item's own text lies, parallel to `items`: from its first word, a documentation block's `---` included, to the end of its last, the whitespace and plain comments after it left out — so nothing written between two items is either's. Empty for a module built rather than parsed. Like `Term`'s span, excluded from equality.
    pub spans: Vec<Span>,
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.items == other.items
    }
}

impl Module {
    /// A module of `items` built rather than parsed, which lie nowhere.
    pub fn built(items: Vec<TopItem>) -> Self {
        Self {
            items,
            spans: Vec::new(),
        }
    }

    /// This module with each item's span cut back to its own text, in it and in every module written inline within it: an item's parse runs on past its last word, over the whitespace and the plain `comments` up to where the next item begins.
    fn trimmed(mut self, comments: &[Span]) -> Self {
        for span in &mut self.spans {
            *span = own_text(span, comments);
        }
        for item in &mut self.items {
            if let TopItem::Mod(TopMod {
                module: Some(module),
                ..
            }) = item
            {
                *module = std::mem::replace(module, Module::built(Vec::new())).trimmed(comments);
            }
        }

        self
    }
}

/// `span` without the whitespace and the plain comments that end it. `comments` are the parse's, ascending.
fn own_text(span: &Span, comments: &[Span]) -> Span {
    let text = &span.source.text;
    let mut end = span.end;
    loop {
        end = span.start + text[span.start..end].trim_end().len();
        // The last comment that begins before `end`: the one ending there, where the text ends in a comment.
        let before = comments.partition_point(|comment| comment.start < end);
        match before.checked_sub(1).map(|index| &comments[index]) {
            Some(comment) if comment.end == end && span.start <= comment.start => {
                end = comment.start;
            }
            _ => return Span::new(span.source.clone(), span.start, end),
        }
    }
}

/// End of a module's item sequence: the input is exhausted, or the item parser explains why it is not.
///
/// **A bare `take_eof` here would report the wrong thing.** A module has no tail expression, so anything left after the item loop is a failed item — but the loop drops a recoverable item failure (`curios_parse`'s repetition keeps only committed ones), which would leave `take_eof` to invent `Expected 'end-of-file'` at the item's first column. Re-running the item parser recovers the diagnosis it already had; [`Parser::or`] then keeps whichever error reached further, so a genuine trailing token still reports as end-of-input while a malformed item reports at the token that broke it. The alternative never succeeds — the loop stopped here precisely because the item parser failed — so nothing is consumed twice.
fn parse_items_end<'a>(state: ParserState<'a>) -> Parsed<'a, ()> {
    take_eof().or(parse_top_item.map(|_| ())).parse(state)
}

impl Module {
    /// The compilation's reading of a module: an item the parser could not read is kept as [`TopItem::Broken`] and parsing resumes at the next item, so one mistake costs one item's diagnosis rather than the file's. `str::parse` is the other reading — whole or nothing, refusing at the first broken item with that item's own error — for a fixture, a header a package reads to learn what it declares, and the formatter, none of which may act on a file it could not read whole.
    pub fn parse(source: &Arc<Source>) -> Result<Self, ParserError> {
        Self::parse_with_comments(source).map(|(module, _)| module)
    }

    /// [`Module::parse`], additionally returning every comment the parse consumed — spans into `source`, ascending. (The formatter itself goes through `parse_for_format`, whose `FormatInput` carries its own comment list; this pairing exists for the comment-visibility tests.) Comments are not syntax: they live beside the module, never in it, so the parsed module is identical either way and nothing downstream changes.
    pub(crate) fn parse_with_comments(
        source: &Arc<Source>,
    ) -> Result<(Self, Vec<Span>), ParserError> {
        Self::parse_reading(source, false)
    }

    fn parse_reading(source: &Arc<Source>, strict: bool) -> Result<(Self, Vec<Span>), ParserError> {
        clear_comments();
        curios_profile::profile!("parse", group = "module");
        let module = run_parser(
            parse_file_head
                .and_keep(parse_items(strict))
                .and_drop(parse_items_end)
                .map(into_module),
            source,
        )?;
        let comments = take_comments();
        Ok((module.trimmed(&comments), comments))
    }

    /// What `curios-module`'s `read_module` reads, with `text` standing in for what `path` holds — an editor's unsaved buffer.
    pub fn overlaid(path: &Path, text: &str) -> Result<Self, ParserError> {
        Self::parse(&Source::held(path, text))
    }
}

impl FromStr for Module {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Module::parse_reading(&Source::inline(input), true).map(|(module, _)| module)
    }
}

impl fmt::Display for Module {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_module_items(self.clone().items), formatter, 4)
    }
}

/// A whole program as written: a module of top-level items followed by one tail expression — the value the program computes. Parsed via `FromStr` (inline source) or read from a file by `curios-module`'s `read_entrypoint`. It carries no type: the grammar has no position for one, and what an entry is judged at is whoever compiles it's to state.
#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub module: Module,
    pub tail: Term,
}

impl Entrypoint {
    pub(crate) fn new(module: Module, tail: Term) -> Self {
        Self { module, tail }
    }
}

impl Entrypoint {
    /// The compilation's reading of a program, recovering past a broken item as [`Module::parse`] does. A broken item that stands last swallows the tail, since a tail begins with no word an anchor could find; the tail's absence is then that item's fault and reported as it, rather than as a term missing at the end of the input.
    pub fn parse(source: &Arc<Source>) -> Result<Self, ParserError> {
        Self::parse_with_comments(source).map(|(entrypoint, _)| entrypoint)
    }

    /// The entrypoint counterpart of [`Module::parse_with_comments`]: parse plus every consumed comment as spans into `source`, ascending.
    pub(crate) fn parse_with_comments(
        source: &Arc<Source>,
    ) -> Result<(Self, Vec<Span>), ParserError> {
        Self::parse_reading(source, false)
    }

    fn parse_reading(source: &Arc<Source>, strict: bool) -> Result<(Self, Vec<Span>), ParserError> {
        clear_comments();
        curios_profile::profile!("parse", group = "entrypoint");
        let entrypoint = run_parser(
            parse_file_head
                .and_keep(parse_items(strict))
                .flat_map(|items| {
                    let swallowed = match first_broken(&items) {
                        Some(error) => commit(raise(error)).boxed(),
                        None => fail("Expected a program's final term").boxed(),
                    };
                    parse_term
                        .and_drop(parse_program_end::<Program>)
                        .map(move |tail| Entrypoint::new(into_module(items), tail))
                        .or(swallowed)
                }),
            source,
        )?;
        let comments = take_comments();
        Ok((
            Entrypoint {
                module: entrypoint.module.trimmed(&comments),
                tail: entrypoint.tail,
            },
            comments,
        ))
    }
}

/// The formatter's parse product, one grammar for both file kinds: top-level items followed by an *optional* tail expression — a program has one, a prelude-style module file does not. The item/tail interlock is the entrypoint grammar's own: a top-level `let` requires its annotation, so an unannotated binding falls through to the tail as a local `let` term.
pub struct FormatInput {
    pub module: Module,
    /// Each item's span, parallel to `module.items` — the items themselves carry none.
    pub item_spans: Vec<Span>,
    /// The tail expression when the file is a program; its own term span carries its position.
    pub tail: Option<Term>,
    /// Every comment the parse consumed, ascending.
    pub comments: Vec<Span>,
}

pub fn parse_for_format(source: &Arc<Source>) -> Result<FormatInput, ParserError> {
    clear_comments();
    curios_profile::profile!("parse", group = "format");
    let (module, item_spans, tail) = run_parser(
        parse_file_head
            .and_keep(many0(|| spanned(parse_top_item)))
            .and(parse_optional_term)
            .and_drop(parse_program_end::<Program>)
            .map(|(items, tail)| {
                let (spans, items): (Vec<_>, Vec<_>) = items.into_iter().unzip();
                (Module::built(items), spans, tail)
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

/// The two kinds of file a text can be written as: a **program**, whose items a final term follows, and a **module**, which is items alone.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Form {
    Program,
    Module,
}

impl Form {
    /// The form `text`, held for `path`, is written in — read off the one grammar that admits both, which reads a file whole or not at all: items alone are a module, and anything else is a program, a text that does not parse included, since a program's own reading is the one that says where it went wrong.
    pub fn of(path: &Path, text: &str) -> Self {
        match parse_for_format(&Source::held(path, text)) {
            Ok(FormatInput { tail: None, .. }) => Self::Module,
            Ok(FormatInput { tail: Some(_), .. }) | Err(_) => Self::Program,
        }
    }
}

impl Entrypoint {
    /// The trivial program `Io/pure(())`, built rather than parsed.
    ///
    /// What a unit with no written entry is compiled through. A library has none, so one is supplied for it, and every policy that does so replaces it with a synthesized tail before anything checks it. It is a program rather than the shortest text an entrypoint parses: `()` has type `{}` and nothing lifts a pure value into `Io`, so such a placeholder would be refused the moment a policy stopped replacing it.
    ///
    /// This is a program. It needs no text, so there is nothing to parse and nothing to fail; and it is the `IoPure` intrinsic rather than a path, so there is no name to resolve and no dependence on `/std` being reachable from a placeholder.
    pub fn trivial() -> Self {
        Self {
            module: Module::built(Vec::new()),
            tail: Term::from(Subterm::Intrinsic(Intrinsic::IoPure {
                result: Term::from(Subterm::TupleType(TupleType { fields: Vec::new() })),
                value: Term::from(Subterm::Tuple(Tuple { fields: Vec::new() })),
            })),
        }
    }
}

impl FromStr for Entrypoint {
    type Err = ParserError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Entrypoint::parse_reading(&Source::inline(input), true).map(|(entrypoint, _)| entrypoint)
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

#[cfg(test)]
mod tests;
