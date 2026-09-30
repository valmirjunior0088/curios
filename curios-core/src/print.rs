use {
    super::{
        Apply, Argument, Atom, Bang, Bound, CalleeId, Carrier, Cases, Cursor, Field, Free, Func,
        FuncType, Global, InductType, Infix, Intrinsic, Label, Let, Level, Match, MatchResult,
        Metavar, MetavarOrigin, Nat, NumLit, Proj, Rec, Scope, Spellings, Struct, StructType,
        Subterm, Telescope, Term, Three, Transient, Tuple, TupleType, Two, Var, Variant,
    },
    curios_abi::stdio,
    curios_num::{Binary, Floating, Grain, Rounding},
    curios_print::{Printer, flat, group, hard_line, indent, line, pure, sep_flat, soft_line},
    curios_utilities::{InfixOp, Plicity, Qualifier, Symbol, recurse},
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap},
        rc::Rc,
    },
};

fn universe_suffix(levels: &[Level], spelling: &Rc<Spelling>) -> String {
    if levels.is_empty() || spelling.erase_universes {
        String::new()
    } else {
        format!(
            ".{{{}}}",
            levels
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

// === Source-style names (diagnostics) ========================================
//
// Core spells names for the kernel's convenience, not the reader's: every binder is opened under a `Context::fresh` gensym (`n#15`, `(n#0 : Nat) -> …`) and every global is its fully-qualified canonical path (`std/Vec/Vec`, `sys/Nat`). A [`Spelling`] rewrites both back toward what the user wrote; the default one changes nothing, so the faithful `Display` for a bare term leaves names untouched.
//
// The configuration is threaded, not ambient. `Display::fmt` has no parameter channel, so these axes were once three thread-locals installed around a render — which made a term's spelling depend on an enclosing frame nobody could see from the call, and made "should this consumer erase universes?" a question answered by accident of where the installer sat rather than by the consumer. [`Spelled`] restores the parameter: `term.spelled(&spelling)` is an ordinary value that implements `Display`, and every printer function threads a `Frame` carrying that spelling beside the binder depth.
//
// axis (a) — local binders: a *rename map* (built by `build_rename` over `display_names`) alpha-renames the whole fragment — free vars *and* binder labels. A source hint is used bare when unique; distinct names sharing a hint, or shadowing a global's displayed rendering — whatever axis (b) spells it as — take minimal `hint2`, `hint3`, … suffixes, so no two binders ever read alike. A hintless (compiler-minted) binder spells `_` — or is elided — at its label site when nothing references it, and borrows the fallback hint `x` when something does: `_` in a reference position would read as a hole and could not co-spell with its binder.
//
// axis (b) — globals. A report spells each one as name resolution would find it from where its reader stands ([`ReaderNames`]): the shortest of a declaration of the reader's module by its label, a path into one of its child modules, an import in scope at the reader's definition as it was written, and an absolute path whose audience includes the reader — the text stage's [`Spellings`], since only it sees re-exports and visibility — and in full where none reaches it. A suffix unique among the unit's symbols is not the same thing: `Ord` is such a suffix and resolves nowhere `/std/Ord` was not imported. Renders without a reader — `Module` display, `wonder stage`'s dumps, the kernel's refusals, and a report a caller could hand no spellings — take a *shorten map* instead (built by `build_shorten` over `Module::module_symbols`), each qualified path's shortest unambiguous `/`-suffix, through `build_shorten_layered` where the unit a reader wrote is known, so its declarations settle their own spelling before what surrounds them competes for it. Why reports spell for a reader is `documentation/design/toolchain/a-diagnostic-spells-what-its-reader-can-write.md`.
//
// axis (c) — universe instances: a flag suppressing the `.{…}` an instantiated nominal head carries. The surface language has no spelling for an instance — solved (`Option.{0}`) or unsolved (`Eq.{?u271}`) alike — so a diagnostic that shows one asks the reader to decode elaboration state. This is the display twin of `project_erased_universes`, which the goal-report path applies structurally; errors carry raw terms all the way to the formatter, so they suppress at the printer instead. Diagnostics set it; `wonder stage`'s dumps deliberately do not, because a dump is read *about* the compiler and its levels are the point.
//
// A `Type`'s own level is suppressed only when it is *metavariable-headed*. The level is that node's whole content, so erasing a concrete one could render two distinct sorts identically — but an unsolved level names nothing a reader can act on, and it is the common case: a diagnostic over a polymorphic head reports `(A: Type.{?u263}) -> Nat` against `((Type.{?u261}) -> Type.{?u262}) -> Nat`, three placeholders competing with the disagreement they surround. Suppressing every level was rejected for the case that cannot be ruled out — two distinct concrete levels rendering as `Type` against `Type` — and a whole-fragment "show it only when it disambiguates" pass was rejected as context-dependent rendering: unlike a binder name, which is inherently relative, a level is an absolute fact about the term.
//
// axis (e) — unsolved metavariables: a flag spelling every metavariable as a bare `?`. An id such as `?2677` is elaboration state — a counter a reader cannot decode and the surface cannot write — and a diagnostic carrying one reads as three placeholders competing with the disagreement they surround: the transcript case is `inferred: Prop, expected: Eq(@?2677)(?2679, ?2680)`, where `Eq(@?)(?, ?)` says what the reader needs, that *some* equality was expected. Two distinct metavariables rendering alike is not the hazard it is for levels: a mismatch is reported between rigid structure, and two unsolved metavariables facing each other unify rather than mismatch, so no diagnostic hinges on telling them apart. Diagnostics set it; `wonder stage`'s dumps do not, for axis (c)'s reason.
//
// axis (f) — string literals: the identity of the certified `Str` declaration, so a literal spells as the text it stands for rather than as the struct over its bytes. A `Str` is bytes beside the proof certifying them, and a report that spells one structurally — `Str { x[0x62, 0x6F, 0x64, 0x79], True/qed() }` for `"body"` — buries the one thing the reader wrote under the representation that certifies it. The identity is supplied rather than spelled: this crate sits below `curios-prelude-archive`, so it may not name a prelude declaration and takes the one the syntax registry names instead. Diagnostics and goal reports set it; `wonder stage`'s dumps do not, for axis (c)'s reason.
//
// axis (g) — grouping: a flag under which a nested concatenation spells as the operand it is, `[..[..a, ..b], ..c]`, rather than splicing its entries into the enclosing literal. The splice is right everywhere the reader wants the program quoted rather than its lowering, and it is what makes two terms that differ only in how a run is grouped render as one string — which a mismatch report cannot afford, since grouping is a difference conversion can refuse on. The report's escalation sets it, first, because it changes nothing unless a nesting is present; nothing else does.
//
// axis (h) — witnesses: the concepts and witnesses a report spells against, and the witness binders in scope where its reader stands, so a witness reads as resolution would restore it. A `use` argument resolution would put back is left out — a binder in scope that no inner binder of its concept shadows, the unique shortest superclass path off one, a global witness no binder in scope reaches ([`restorable`]) — a function type's witness binder prints unnamed, as the surface requires, and a method projected off a witness prints as the call or operator a program writes. What resolution would not restore keeps its faithful spelling, since a different program is worse than an unpasteable one. Reports set it; `wonder stage`'s dumps and the kernel's refusals do not, for axis (c)'s reason. Why is `documentation/design/toolchain/a-diagnostic-spells-what-its-reader-can-write.md`.
//
// `Spelling::label` consults the shorten map first (globals), then the rename map (locals); a name in neither renders verbatim.

/// How a term is spelled for a reader. The default spells nothing differently, which is what a bare `Display` uses.
#[derive(Clone, Default)]
pub struct Spelling {
    /// axis (a) — local binders to their source-style names.
    pretty: Option<Rc<Rename>>,
    /// axis (b) without a reader — global qualified names to their shortest unambiguous suffix.
    shorten: Option<Rc<HashMap<Global, String>>>,
    /// axis (c) — whether to suppress universe instances and metavariable-headed levels.
    erase_universes: bool,
    /// axis (d) — a nominal declaration's parameter plicities, so an applied family is marked the way a use site would write it.
    nominal_plicities: Option<Rc<BTreeMap<Global, Vec<Plicity>>>>,
    /// axis (e) — whether every metavariable spells as a bare `?`.
    anonymous_metavars: bool,
    /// axis (f) — the certified-string declaration, so a `Str` literal spells as its own text.
    string_literal: Option<Global>,
    /// axis (g) — whether a nested concatenation keeps its grouping instead of being spliced into the enclosing literal.
    grouped: bool,
    /// axis (h) — the concepts and witnesses a witness is spelled against, so it reads as resolution would restore it.
    witnesses: Option<Rc<WitnessSpelling>>,
    /// axis (b) for a reader: how the unit's names can be written and where its definitions sit, so a global is spelled as resolution would find it from where the reader stands. Takes precedence over the shorten map, which stays what a render without a reader — a dump, a kernel refusal — spells by.
    names: Option<Rc<ReaderNames>>,
    /// Where the reader stands (axes (b) and (h)).
    reader: ReaderPosition,
}

/// Where a reader stands when they read a rendered term: the definition they wrote it in (the entrypoint's final term when `None`), whose module and imports decide what reaches a name, and the witness binders in scope there, outermost first, which resolution searches before the global table.
#[derive(Clone, Debug, Default)]
pub struct ReaderPosition {
    pub owner: Option<Global>,
    pub witnesses: Rc<[(Free, Term)]>,
}

/// What axis (b) spells a global by for a reader: the unit's [`Spellings`] and the module each of its definitions sits in, with the spellings already asked for.
pub struct ReaderNames {
    spellings: Spellings,
    islands: BTreeMap<Global, Qualifier>,
    memo: RefCell<Asked>,
}

/// The spellings a render already asked for, keyed by the definition the reader stands in and the global spelled — `None` where the reader can reach it by none.
type Asked = HashMap<(Option<Global>, Global), Option<String>>;

impl ReaderNames {
    pub fn new(spellings: Spellings, islands: BTreeMap<Global, Qualifier>) -> Self {
        Self {
            spellings,
            islands,
            memo: RefCell::default(),
        }
    }

    fn spell(&self, reader: &ReaderPosition, global: &Global) -> Option<String> {
        let key = (reader.owner, *global);
        if let Some(spelled) = self.memo.borrow().get(&key) {
            return spelled.clone();
        }
        let island = reader
            .owner
            .as_ref()
            .and_then(|owner| self.islands.get(owner))
            .cloned()
            .unwrap_or_else(Qualifier::empty);
        let spelled = self.spellings.spell(&island, reader.owner.as_ref(), global);
        self.memo.borrow_mut().insert(key, spelled.clone());
        spelled
    }
}

/// What axis (h) spells a witness against: each concept's fields — a method's wrapper and the operator dispatching to it, or the concept a superclass edge reaches — and each global witness's declared type, whose terminal is the concept application it answers. Built per unit by [`Module::witness_spelling`](crate::Module::witness_spelling) and merged across a render's scope, as the plicity marks are.
#[derive(Default)]
pub struct WitnessSpelling {
    pub(crate) concepts: BTreeMap<Global, Vec<FieldSpelling>>,
    pub(crate) witnesses: BTreeMap<Global, Term>,
}

/// One field of a concept, by position.
#[derive(Clone)]
pub(crate) enum FieldSpelling {
    /// A method or value field: the wrapper a program reaches it through, whether that wrapper takes a method's own parameters in its one group, and the operator dispatching to it.
    Method {
        wrapper: Global,
        merged: bool,
        operator: Option<InfixOp>,
    },
    /// A superclass edge, to the concept it reaches.
    Super(Global),
}

impl WitnessSpelling {
    /// Add another unit's entries. A name resolves to one declaration, so the first unit to list it wins.
    pub fn merge(&mut self, other: WitnessSpelling) {
        for (name, fields) in other.concepts {
            self.concepts.entry(name).or_insert(fields);
        }
        for (name, type_) in other.witnesses {
            self.witnesses.entry(name).or_insert(type_);
        }
    }

    fn supers(&self, concept: &Global) -> impl Iterator<Item = (usize, &Global)> {
        self.concepts
            .get(concept)
            .into_iter()
            .flatten()
            .enumerate()
            .filter_map(|(index, field)| match field {
                FieldSpelling::Super(super_) => Some((index, super_)),
                FieldSpelling::Method { .. } => None,
            })
    }

    /// The concept application a type names, when it names a concept this table knows: `Show(A)` elaborated to its record, or still the concept's name applied.
    fn application(&self, type_: &Term) -> Option<(Global, Vec<Term>)> {
        let (name, arguments) = match &**type_ {
            Subterm::StructType(StructType { name, params, .. }) => (*name, params.clone()),
            Subterm::Apply(_) | Subterm::Var(_) | Subterm::Instance(_) => {
                let Free::Global(name) = type_.head_name()? else {
                    return None;
                };
                (*name, spine_arguments(type_))
            }
            _ => return None,
        };
        self.concepts
            .contains_key(&name)
            .then_some((name, arguments))
    }

    /// The concept application a global witness answers, its declared telescope opened at `arguments` — a premised witness is applied to its own hidden arguments. Without them the concept alone is known.
    fn answers(&self, witness: &Global, arguments: &[Term]) -> Option<(Global, Vec<Term>)> {
        let type_ = self.witnesses.get(witness)?;
        match &**type_ {
            Subterm::FuncType(FuncType { telescope, .. }) => {
                if telescope.len() == arguments.len() {
                    self.application(&telescope.open(&arguments.iter().collect::<Vec<_>>()))
                } else {
                    let (concept, _) = self.application(telescope.terminal())?;
                    Some((concept, Vec::new()))
                }
            }
            _ => self.application(type_),
        }
    }
}

/// Every argument along an application spine, outermost call last — `C(a)(b)` gives `[a, b]`.
fn spine_arguments(term: &Term) -> Vec<Term> {
    match &**term {
        Subterm::Apply(Apply { head, arguments }) => {
            let mut spine = spine_arguments(head);
            spine.extend(arguments.iter().map(|argument| argument.term.clone()));
            spine
        }
        _ => Vec::new(),
    }
}

impl Spelling {
    /// Rename local binders to source-style names (axis (a)).
    pub fn with_pretty_names(mut self, rename: Rc<Rename>) -> Self {
        self.pretty = Some(rename);
        self
    }

    /// Shorten global names against a module's symbol table (axis (b)).
    pub fn with_short_names(mut self, shorten: Rc<HashMap<Global, String>>) -> Self {
        self.shorten = Some(shorten);
        self
    }

    /// Suppress universe instances and metavariable-headed levels (axis (c)).
    pub fn with_erased_universes(mut self) -> Self {
        self.erase_universes = true;
        self
    }

    /// Show universe instances again, undoing axis (c).
    ///
    /// For the one report that cannot afford the suppression: two terms differing in nothing but their instances render as the same string, so a mismatch between them says `X ≠ X` and carries no information at all. A consumer that has detected that case re-renders under this.
    pub fn with_shown_universes(mut self) -> Self {
        self.erase_universes = false;
        self
    }

    /// Spell every metavariable as a bare `?` (axis (e)).
    pub fn with_anonymous_metavars(mut self) -> Self {
        self.anonymous_metavars = true;
        self
    }

    /// Keep a nested concatenation's grouping instead of splicing it (axis (g)).
    ///
    /// For the report that has found two sides rendering as one string: a nesting is a difference conversion can refuse on, and the splice is what hid it. A consumer that has detected that case re-renders under this before it reaches for the universe instances, since grouping changes nothing where no nesting is present.
    pub fn with_faithful_grouping(mut self) -> Self {
        self.grouped = true;
        self
    }

    /// Spell every witness as resolution would restore it (axis (h)), against `witnesses` — the table [`Module::witness_spelling`](crate::Module::witness_spelling) builds.
    pub fn with_witness_spelling(mut self, witnesses: Rc<WitnessSpelling>) -> Self {
        self.witnesses = Some(witnesses);
        self
    }

    /// Render for a reader standing at `position` (axis (b)'s owner and axis (h)'s witness scope).
    pub fn for_reader(mut self, position: ReaderPosition) -> Self {
        self.reader = position;
        self
    }

    /// Where this render's reader stands.
    pub fn reader(&self) -> &ReaderPosition {
        &self.reader
    }

    /// Spell every global as resolution would find it from the reader's position (axis (b) for a reader).
    pub fn with_reader_names(mut self, names: Rc<ReaderNames>) -> Self {
        self.names = Some(names);
        self
    }

    /// A built-in type or operation, named by the path under `/sys` its printer spells it by — a carrier's type `X` at `X/X`, an operation at its own path — spelled for the reader where one is set and the declaration reaches them, and by that path otherwise, as a dump reads it.
    fn intrinsic_symbol(&self, path: &str) -> String {
        let reader = self.names.as_ref().and_then(|names| {
            let mut segments = std::iter::once("sys")
                .chain(path.split('/'))
                .collect::<Vec<_>>();
            if segments.len() == 2 {
                segments.push(segments[1]);
            }
            names.spell(&self.reader, &Global::Authored(Qualifier::from(segments)))
        });
        reader.unwrap_or_else(|| path.to_string())
    }

    /// Spell a certified string literal as its own text (axis (f)). `name` is the `Str` declaration a literal's head carries — [`SyntaxRegistry`](curios_utilities::SyntaxRegistry)'s, since this crate may not spell it.
    pub fn with_string_literals(mut self, name: Global) -> Self {
        self.string_literal = Some(name);
        self
    }

    /// Mark a nominal family's implicit parameters (axis (d)), from `Module::nominal_plicities`.
    pub fn with_nominal_plicities(mut self, plicities: Rc<BTreeMap<Global, Vec<Plicity>>>) -> Self {
        self.nominal_plicities = Some(plicities);
        self
    }

    /// The display spelling of a global (axis (b)) — for a reader, as resolution would find it from where they stand, and in full where it cannot, since a suffix nothing resolves is a different program; without one, shortened against the module's other symbols when that is unambiguous, and in full otherwise. Globals never take axis (a)'s rename: their spelling is a path a programmer wrote, not a minted hint.
    pub fn symbol(&self, name: &Global) -> String {
        if let Some(names) = &self.names {
            return names
                .spell(&self.reader, name)
                .unwrap_or_else(|| name.to_string());
        }
        self.shorten
            .as_ref()
            .and_then(|map| map.get(name).cloned())
            .unwrap_or_else(|| name.to_string())
    }

    /// The display spelling of a name. A global with a shorter in-scope spelling takes it (axis (b)); a local binder gets its pretty rename (axis (a)), falling back to its minting hint. A name in neither map renders verbatim.
    fn label(&self, name: &Free) -> String {
        if let Some(global) = name.as_global() {
            return self.symbol(global);
        }
        self.pretty
            .as_ref()
            .and_then(|rename| rename.get(name).cloned())
            .unwrap_or_else(|| match name.hint() {
                Some(hint) => hint.to_string(),
                None => name.to_string(),
            })
    }

    /// The declared plicities of `name`'s arguments — parameters then indices, in the order a use site supplies them — or `None` when the declaration is not in this spelling's table, in which case an applied family renders unmarked as it always did.
    fn nominal_marks(&self, name: &Global, arity: usize) -> Option<&[Plicity]> {
        let marks = self.nominal_plicities.as_ref()?.get(name)?;
        // A declaration whose vector does not match the occurrence is not one this can speak about: render flat rather than mark the wrong argument.
        (marks.len() == arity).then_some(marks.as_slice())
    }
}

/// The prefix a plicity marks its binder or argument with — `@` for implicit, `use ` for witness, nothing for explicit.
fn plicity_mark(plicity: Option<&Plicity>) -> &'static str {
    match plicity {
        Some(Plicity::Implicit) => "@",
        Some(Plicity::Witness) => "use ",
        _ => "",
    }
}

/// One argument of an applied nominal family, marked as its declaration wrote it. Indices are always explicit, so only a parameter ever takes a mark.
fn marked_argument(printer: Printer, plicity: Option<&Plicity>) -> Printer {
    match plicity_mark(plicity) {
        "" => printer,
        mark => flat([pure(mark), printer]),
    }
}

/// A nominal type's arguments from position `offset` of its declaration on, each marked as the declaration marks it, and a witness resolution would restore left out (axis (h)).
fn nominal_arguments(
    group: Vec<Term>,
    marks: Option<&[Plicity]>,
    offset: usize,
    frame: Frame,
) -> Vec<Printer> {
    group
        .into_iter()
        .enumerate()
        .filter_map(|(index, argument)| {
            let mark = marks.and_then(|marks| marks.get(offset + index));
            (mark != Some(&Plicity::Witness) || !restorable(&argument, frame))
                .then(|| marked_argument(sub(argument, frame), mark))
        })
        .collect()
}

/// A value paired with the [`Spelling`] it renders under — the parameter channel `Display::fmt` does not have. Produced by [`Term::spelled`] and its siblings.
pub struct Spelled<'a, T> {
    value: &'a T,
    spelling: Rc<Spelling>,
    width: Option<usize>,
}

impl<'a, T> Spelled<'a, T> {
    pub(crate) fn new(value: &'a T, spelling: &Rc<Spelling>) -> Self {
        Self {
            value,
            spelling: Rc::clone(spelling),
            width: None,
        }
    }

    /// Render within `width` columns: the printer's groups fit or break against the target instead of the unbounded flat layout a plain render keeps. Diagnostics printing large terms go through this.
    pub fn within(mut self, width: usize) -> Self {
        self.width = Some(width);
        self
    }

    pub(crate) fn value(&self) -> &'a T {
        self.value
    }

    pub(crate) fn spelling(&self) -> &Rc<Spelling> {
        &self.spelling
    }

    pub(crate) fn width(&self) -> Option<usize> {
        self.width
    }
}

/// The terms a report renders, gathered so [`build_rename`] can spell every name any of them shows: their free variables, and the binders a render of each opens — which only a render can say, since a scope remembers a binder's hint and not its identity.
#[derive(Debug, Default, Clone)]
pub struct DisplayNames {
    terms: Vec<Term>,
    free: BTreeSet<Free>,
}

impl DisplayNames {
    /// Add `term` to what the report renders.
    pub fn add(&mut self, term: &Term) {
        self.free.extend(term.free_vars());
        self.terms.push(term.clone());
    }
}

/// The names a render of `term` shows ([`DisplayNames::add`]).
pub fn display_names(term: &Term) -> DisplayNames {
    let mut names = DisplayNames::default();
    names.add(term);
    names
}

/// Give every name a report shows a clean display spelling: a local its hint — or `x` where it was minted hintless — suffixed `hint2`, `hint3`, … when several distinct names would otherwise render alike, or would shadow a global's displayed rendering. The result is unambiguous by construction, so no rendered name is ever silently shared between two binders of one term.
///
/// **The binders are met by rendering.** Each term is rendered once under `spelling` into nothing, by the same printer and so in the same order, and the labels that dry run mints are the ones the real render mints — see `Minting`. `spelling` is the one the same render will apply, standing where its reader does: a global is reserved under the rendering it actually displays ([`Spelling::symbol`]), since a full path — never a bare identifier — is unshadowable by construction, while a bare label a reader reaches is exactly what a binder hint can read like.
///
/// A hintless entry's `x` is consulted only where something references the binder — the label sites spell an unreferenced unnameable binder `_` (or elide it) without the map. Hinted names are assigned first, so a synthesized `x` can never steal the spelling from a binder actually written `x`.
///
/// A tuple label is the exception: it is part of its tuple type's identity, so it keeps the spelling it was written with before anything else is assigned, and a binder that would read like it is the one suffixed — a function's parameter `frame` beside a result field `frame` reads `frame2`, since a parameter's name is no part of its type. Two labels may therefore read alike, which is what their types say.
pub fn build_rename(names: &DisplayNames, spelling: &Spelling) -> Rename {
    let spelling = Rc::new(spelling.clone());
    let mut shown = names
        .free
        .iter()
        .map(DisplayKey::of)
        .collect::<BTreeSet<_>>();
    let mut tuple_labels = BTreeSet::new();
    for term in &names.terms {
        let mint = Minting::recording();
        print_minting(term.clone(), &spelling, &mint);
        let recorded = mint
            .recorded
            .expect("a recording mint records")
            .into_inner();
        shown.extend(recorded.names);
        tuple_labels.extend(recorded.tuple_labels);
    }
    assign(&shown, &tuple_labels, &spelling)
}

/// The spellings [`build_rename`] assigns, over the names a report shows and the tuple labels among them. `shown` is sorted, so the assignment is deterministic: a free variable before the binders a render minted, and those in the order a render meets them.
fn assign(
    shown: &BTreeSet<DisplayKey>,
    tuple_labels: &BTreeSet<DisplayKey>,
    spelling: &Spelling,
) -> Rename {
    let (literal, prettifiable) = shown
        .iter()
        .partition::<Vec<&DisplayKey>, _>(|key| key.name.as_global().is_some());

    // Globals reserve the spelling they will display under.
    let mut used = literal
        .into_iter()
        .map(|key| match key.name.as_global() {
            Some(global) => spelling.symbol(global),
            None => key.name.to_string(),
        })
        .collect::<BTreeSet<_>>();

    let mut spellings = HashMap::new();
    for label in tuple_labels {
        if let Some(hint) = label.name.hint() {
            used.insert(hint.to_string());
            spellings.insert(*label, hint.to_string());
        }
    }

    let (hinted, hintless) = prettifiable
        .into_iter()
        .filter(|key| !spellings.contains_key(*key))
        .partition::<Vec<&DisplayKey>, _>(|key| key.name.hint().is_some());

    for key in hinted.into_iter().chain(hintless) {
        let hint = key.name.hint().unwrap_or("x");
        let mut candidate = hint.to_string();
        let mut next = 2;
        while used.contains(&candidate) {
            candidate = format!("{hint}{next}");
            next += 1;
        }
        used.insert(candidate.clone());
        spellings.insert(*key, candidate);
    }
    Rename { spellings }
}

/// Map each global to the shortest `/`-suffix of its path that no other global shares — the name it has in scope, since Curios has no `use … as` aliasing, so an in-scope name is always a suffix. Only entries that actually shorten are recorded; an ambiguous (or single-segment) name keeps its full path.
pub fn build_shorten(symbols: &[Global]) -> HashMap<Global, String> {
    build_shorten_layered(&[], symbols)
}

/// [`build_shorten`] for a render a reader looks at from inside `own`'s unit: a declaration sitting directly in that unit takes its bare label before anything around it may compete for the suffix.
///
/// The tier exists because a segment-suffix is not by itself a spelling anyone can write. `/std/Bool/Holds` is reachable as `Bool/Holds` and in full, never as a bare `Holds` — reaching it needs a `use` naming `Holds` itself, which only a reader's spelling ([`ReaderNames`]) can see. Counting the suffix it cannot claim against a reader's own root-declared `Holds` tied the two, so *neither* shortened and the name the reader had just written reported as `/Holds` while the one they could not reach reported as `Bool/Holds`.
///
/// Only a single-segment name gets the claim, because only its bare label is writable: reaching a reader's own `/Vec/nil` needs `Vec/nil` or an import just as the environment's does, so a nested own name has no better title to `nil` than the shared contest below gives it. Handing it one spelled a goal candidate — `? ≈ nil()` — that the reader could not paste.
pub fn build_shorten_layered(own: &[Global], scope: &[Global]) -> HashMap<Global, String> {
    // One global can be listed twice (an inductive is both an `induct_decls` registry key and an `items` type-constructor definition), and a unit listed in both tiers lists it in both; count distinct names, or such a name would look ambiguous with itself and never shorten.
    let own = own.iter().collect::<BTreeSet<_>>();
    let scope = scope
        .iter()
        .collect::<BTreeSet<_>>()
        .difference(&own)
        .copied()
        .collect::<BTreeSet<_>>();

    // Suffixes are taken over the *segments* a name is made of, never over its rendered text: `/Foobar` is not a suffix of `/Foo/bar`, and only the structure says so.
    let suffixes = |name: &Global| -> Vec<String> {
        let Some(segments) = name.qualifier().map(Qualifier::segments) else {
            return Vec::new();
        };
        (1..=segments.len())
            .map(|k| segments[segments.len() - k..].join("/"))
            .collect()
    };

    // How many distinct globals carry each segment-suffix.
    let mut count: HashMap<String, usize> = HashMap::new();
    for name in own.iter().chain(scope.iter()) {
        for suffix in suffixes(name) {
            *count.entry(suffix).or_insert(0) += 1;
        }
    }

    let mut map = HashMap::new();
    let mut claimed = BTreeSet::new();

    // Two single-segment names cannot collide — one label, one declaration — so this needs no ambiguity test of its own.
    for name in &own {
        let Some([label]) = name.qualifier().map(Qualifier::segments) else {
            continue;
        };

        claimed.insert(label.clone());
        map.insert(*(*name), label.clone());
    }

    for name in own.iter().chain(scope.iter()) {
        if map.contains_key(*name) {
            continue;
        }

        let rendered = name.to_string();
        if let Some(shortest) = suffixes(name)
            .into_iter()
            .find(|suffix| count.get(suffix) == Some(&1) && !claimed.contains(suffix))
            && shortest.len() < rendered.len()
        {
            map.insert(*(*name), shortest);
        }
    }
    map
}

/// Where the identities a render mints for the binders it opens begin: above every index a compilation mints, which counts up from zero and would exhaust its binder space long before reaching here.
const PRINTED: u32 = 1 << 31;

/// How one render labels the binders it opens. A scope remembers a binder's hint, never its identity, so the render mints one: the `k`th binder it opens is `PRINTED + k` under its hint. A dry run over the same term under the same spelling opens the same binders in the same order, which is how [`build_rename`] spells them before the render that uses the spellings.
#[derive(Default)]
struct Minting {
    next: Cell<u32>,
    /// What a dry run minted, when this is one.
    recorded: Option<RefCell<Recorded>>,
}

/// What a dry run met: every name it showed, and which of the labels it minted are a tuple type's.
#[derive(Default)]
struct Recorded {
    names: BTreeSet<DisplayKey>,
    tuple_labels: BTreeSet<DisplayKey>,
}

impl Minting {
    fn recording() -> Self {
        Self {
            next: Cell::new(0),
            recorded: Some(RefCell::new(Recorded::default())),
        }
    }

    /// The next binder's identity, rendering as `hint`.
    fn local(&self, hint: Option<Symbol>) -> Free {
        let position = self.next.get();
        self.next.set(position + 1);
        let index = PRINTED
            .checked_add(position)
            .expect("a render opens fewer binders than a compilation could mint");
        Free::local_hinted(index, hint)
    }

    fn record(&self, name: &Free) {
        if let Some(recorded) = &self.recorded {
            recorded.borrow_mut().names.insert(DisplayKey::of(name));
        }
    }

    /// Record `label` as a tuple type's, which keeps the spelling it was written with — see [`build_rename`].
    fn note_tuple_label(&self, label: &Free) {
        if let Some(recorded) = &self.recorded {
            recorded
                .borrow_mut()
                .tuple_labels
                .insert(DisplayKey::of(label));
        }
    }
}

/// A name as a rename tells it apart from the others: any name by itself, and a label a render minted by its position together with its hint. Two terms of one report restart their positions, so the hint is what keeps two binders of different names apart; two of the same name in the same position share a spelling, which is harmless, since the terms are never in scope of each other.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DisplayKey {
    name: Free,
    hint: Option<Symbol>,
}

impl DisplayKey {
    fn of(name: &Free) -> Self {
        let printed = name.local_index().is_some_and(|index| index >= PRINTED);
        Self {
            name: *name,
            hint: printed.then(|| name.hint_symbol()).flatten(),
        }
    }
}

/// Every name a render shows, with the spelling [`build_rename`] assigned it — what [`Spelling::with_pretty_names`] applies.
#[derive(Debug, Default)]
pub struct Rename {
    spellings: HashMap<DisplayKey, String>,
}

impl Rename {
    /// The spelling assigned to `name`, if it was among the names the rename was built over.
    pub(crate) fn get(&self, name: &Free) -> Option<&String> {
        self.spellings.get(&DisplayKey::of(name))
    }
}

fn label_terms(binders: &[Free]) -> Vec<Term> {
    binders.iter().map(Term::free_var).collect()
}

/// The state a recursive print call threads: the render-constant [`Spelling`], the render's [`Minting`], and the witness binders in scope (axis (h)).
#[derive(Clone, Copy)]
struct Frame<'a> {
    spelling: &'a Rc<Spelling>,
    mint: &'a Minting,
    /// The innermost witness binder in scope, chained outward. Each node lives on the stack of the arm that opened its binder: the document is built eagerly, so no frame outlives the node it borrows.
    witnesses: Option<&'a WitnessNode<'a>>,
}

/// One witness binder in scope: its label, the concept application its type names when the table knows the concept, whether a reference to it reached the rendering, and the binder outside it.
struct WitnessNode<'a> {
    label: Free,
    concept: Option<(Global, Vec<Term>)>,
    used: Cell<bool>,
    parent: Option<&'a WitnessNode<'a>>,
}

impl<'a> WitnessNode<'a> {
    fn new(label: Free, type_: &Term, frame: Frame<'a>) -> Self {
        let concept = frame
            .spelling
            .witnesses
            .as_deref()
            .and_then(|table| table.application(type_));
        Self {
            label,
            concept,
            used: Cell::new(false),
            parent: frame.witnesses,
        }
    }

    fn concept(&self) -> Option<&Global> {
        self.concept.as_ref().map(|(concept, _)| concept)
    }
}

/// The witness binders in scope, innermost first.
fn chain<'a>(first: Option<&'a WitnessNode<'a>>) -> impl Iterator<Item = &'a WitnessNode<'a>> {
    std::iter::successors(first, |node| node.parent)
}

/// What a witness argument is, as resolution sees one.
enum WitnessForm<'a> {
    /// A binder in scope, or a superclass path off one `depth` edges long, reaching `concept`.
    Local {
        node: &'a WitnessNode<'a>,
        concept: Global,
        depth: usize,
    },
    /// A witness of the global table, answering `concept` at `arguments` (empty where its declared telescope could not be opened).
    Global {
        concept: Global,
        arguments: Vec<Term>,
    },
    /// A witness goal still open, which resolution is what it waits on.
    Pending,
}

/// Classify `term` as a witness, when it is one: a binder in scope, a superclass path off one, a global witness — bare, at a universe instance, or applied to its own hidden arguments — or an open witness goal.
fn witness_form<'a>(term: &Term, frame: Frame<'a>) -> Option<WitnessForm<'a>> {
    let table = frame.spelling.witnesses.as_deref()?;
    match &**term {
        Subterm::Metavar(Metavar {
            origin: MetavarOrigin::Witness(_),
            ..
        }) => Some(WitnessForm::Pending),
        Subterm::Var(var) if matches!(var.as_free(), Some(Free::Local(_))) => {
            let label = var.as_free()?;
            let node = chain(frame.witnesses).find(|node| &node.label == label)?;
            Some(WitnessForm::Local {
                node,
                concept: *node.concept()?,
                depth: 0,
            })
        }
        Subterm::Proj(Proj {
            head,
            field: Field::Index(index),
        }) => match witness_form(head, frame)? {
            WitnessForm::Local {
                node,
                concept,
                depth,
            } => {
                let (_, reached) = table.supers(&concept).find(|(field, _)| field == index)?;
                Some(WitnessForm::Local {
                    node,
                    concept: *reached,
                    depth: depth + 1,
                })
            }
            WitnessForm::Global { .. } | WitnessForm::Pending => None,
        },
        Subterm::Var(_) | Subterm::Instance(_) | Subterm::Apply(_) => {
            let Free::Global(witness @ Global::Witness(_)) = term.head_name()? else {
                return None;
            };
            let (concept, arguments) = table.answers(witness, &spine_arguments(term))?;
            Some(WitnessForm::Global { concept, arguments })
        }
        _ => None,
    }
}

/// Whether resolution, at the reader's position, would put `argument` back if it were left out — the only case a `use` argument may be, following `documentation/syntax.md`'s order: the local witnesses innermost first, then their superclass projections breadth-first, then the global table. A binder whose concept the table does not know restores nothing and shadows nothing.
fn restorable(argument: &Term, frame: Frame) -> bool {
    let Some(table) = frame.spelling.witnesses.as_deref() else {
        return false;
    };
    match witness_form(argument, frame) {
        None => false,
        Some(WitnessForm::Pending) => true,
        // A direct match: the first binder of its concept, innermost out, is the one resolution takes.
        Some(WitnessForm::Local {
            node,
            concept,
            depth: 0,
        }) => chain(frame.witnesses)
            .find(|candidate| candidate.concept() == Some(&concept))
            .is_some_and(|first| std::ptr::eq(first, node)),
        // A superclass path: no binder of the concept itself, and this the one route of the least length.
        Some(WitnessForm::Local {
            node,
            concept,
            depth,
        }) => {
            !chain(frame.witnesses).any(|candidate| candidate.concept() == Some(&concept))
                && routes(table, frame, &concept).is_some_and(|(least, routes)| {
                    least == depth && matches!(routes[..], [only] if std::ptr::eq(only, node))
                })
        }
        // A global witness: coherence makes the table's answer this one, so long as nothing in scope answers first.
        Some(WitnessForm::Global { concept, .. }) => {
            routes(table, frame, &concept).is_none()
                && !chain(frame.witnesses).any(|candidate| candidate.concept() == Some(&concept))
        }
    }
}

/// The least superclass depth at which any binder in scope reaches `concept`, beside every binder reaching it there — one entry per path, so a diamond counts twice. `None` when no binder reaches it through a superclass.
fn routes<'a>(
    table: &WitnessSpelling,
    frame: Frame<'a>,
    concept: &Global,
) -> Option<(usize, Vec<&'a WitnessNode<'a>>)> {
    let mut level = chain(frame.witnesses)
        .filter_map(|node| node.concept().map(|reached| (node, *reached)))
        .collect::<Vec<_>>();
    let mut depth = 0;
    // The superclass graph is acyclic (checked where the registries are seeded), so the walk ends once every path has run out.
    while !level.is_empty() {
        depth += 1;
        level = level
            .into_iter()
            .flat_map(|(node, reached)| {
                table
                    .supers(&reached)
                    .map(move |(_, super_)| (node, *super_))
                    .collect::<Vec<_>>()
            })
            .collect();
        let found = level
            .iter()
            .filter(|(_, reached)| reached == concept)
            .map(|(node, _)| *node)
            .collect::<Vec<_>>();
        if !found.is_empty() {
            return Some((depth, found));
        }
    }
    None
}

/// The operator a call of the method a projection off `witness` stands for prints as: the one dispatching to that method, when the call passes it exactly two explicit arguments — an open witness goal's the operator it was inserted for. What [`method_doc`] prints infix, and so what [`print_operand`] parenthesizes: a method call spelled `b - a` is as much an operand needing parentheses as the intrinsic subtraction is.
fn method_operator(
    witness: &Term,
    index: usize,
    arguments: &[Argument],
    frame: Frame,
) -> Option<InfixOp> {
    let [_, _] = arguments else {
        return None;
    };
    if arguments
        .iter()
        .any(|argument| argument.plicity != Plicity::Explicit)
    {
        return None;
    }
    let table = frame.spelling.witnesses.as_deref()?;
    let concept = match witness_form(witness, frame)? {
        WitnessForm::Pending => {
            let Subterm::Metavar(Metavar {
                origin: MetavarOrigin::Witness(origin),
                ..
            }) = &**witness
            else {
                return None;
            };
            let CalleeId::Operator(op) = origin.func else {
                return None;
            };
            return Some(op);
        }
        WitnessForm::Local { concept, .. } | WitnessForm::Global { concept, .. } => concept,
    };
    match table.concepts.get(&concept)?.get(index)? {
        FieldSpelling::Method { operator, .. } => *operator,
        _ => None,
    }
}

/// The method a projection off a witness stands for, spelled as a program reaches it: the operator dispatching to it between exactly two operands, or its wrapper — called with the concept's parameters marked `@`, the witness only where resolution would not restore it, and, for a wrapper taking the method's own parameters in its group, those in the same call. `arguments` is the call the projection heads, when it heads one. `None` for anything else, which prints as it is.
fn method_doc(
    witness: &Term,
    index: usize,
    arguments: Option<&[Argument]>,
    frame: Frame,
) -> Option<Printer> {
    let table = frame.spelling.witnesses.as_deref()?;
    if let Some(arguments @ [left, right]) = arguments
        && let Some(op) = method_operator(witness, index, arguments, frame)
    {
        return Some(print_infix(
            op.symbol(),
            left.term.clone(),
            right.term.clone(),
            frame,
        ));
    }

    let (concept, parameters) = match witness_form(witness, frame)? {
        // An open witness goal carries only the operator it was inserted for, which spells it or nothing does.
        WitnessForm::Pending => return None,
        WitnessForm::Local {
            node,
            concept,
            depth,
        } => {
            let parameters = if depth == 0 {
                node.concept
                    .as_ref()
                    .map(|(_, arguments)| arguments.clone())
            } else {
                None
            };
            (concept, parameters.unwrap_or_default())
        }
        WitnessForm::Global { concept, arguments } => (concept, arguments),
    };

    let FieldSpelling::Method {
        wrapper, merged, ..
    } = table.concepts.get(&concept)?.get(index)?.clone()
    else {
        return None;
    };

    let restored = restorable(witness, frame);
    let reference = Term::var(Var::free(Free::Global(wrapper)));
    let leading = parameters
        .into_iter()
        .map(|parameter| (Plicity::Implicit, parameter))
        .chain((!restored).then(|| (Plicity::Witness, witness.clone())))
        .collect::<Vec<_>>();
    let own = arguments.map(|arguments| {
        arguments
            .iter()
            .map(|argument| (argument.plicity, argument.term.clone()))
            .collect::<Vec<_>>()
    });

    let term = match (merged, own) {
        (true, Some(own)) => Term::apply_marked(reference, leading.into_iter().chain(own)),
        // The method as a value: its wrapper, which a checked position instantiates and eta-expands — where resolution restores the witness, since the one call that could name it would have to carry the method's own arguments too.
        (true, None) if restored => reference,
        (true, None) => return None,
        (false, own) => {
            let field = Term::apply_marked(reference, leading);
            match own {
                Some(own) => Term::apply_marked(field, own),
                None => field,
            }
        }
    };
    Some(sub(term, frame))
}

impl<'a> Frame<'a> {
    /// This frame with `node` innermost in its witness scope.
    fn with_witness<'b>(self, node: &'b WitnessNode<'b>) -> Frame<'b>
    where
        'a: 'b,
    {
        Frame {
            spelling: self.spelling,
            mint: self.mint,
            witnesses: Some(node),
        }
    }

    /// The label the next binder this render opens is printed under: a global its own name, and a local the identity [`Minting`] mints from where the render meets it and the hint its scope remembers — `None` for a `constant` scope, which never had binders written.
    fn label(&self, label: Option<&Label>) -> Free {
        let name = match label {
            Some(Label::Global(global)) => Free::Global(*global),
            Some(Label::Local { hint, .. }) => self.mint.local(*hint),
            None => self.mint.local(None),
        };
        self.mint.record(&name);
        name
    }

    /// Every binder of a scope, labelled in order.
    fn labels<'b>(&self, binders: impl Iterator<Item = Option<&'b Label>>) -> Vec<Free> {
        binders.map(|label| self.label(label)).collect()
    }

    /// Open a two-binder scope under minted labels.
    fn open_two(&self, scope: Scope<Two>) -> ((Free, Free), Term) {
        let fst = self.label(scope.label(0));
        let snd = self.label(scope.label(1));
        let body = scope.open(&[&Term::free_var(&fst), &Term::free_var(&snd)]);

        ((fst, snd), body)
    }

    /// The three-binder counterpart of [`Frame::open_two`].
    fn open_three(&self, scope: Scope<Three>) -> ((Free, Free, Free), Term) {
        let fst = self.label(scope.label(0));
        let snd = self.label(scope.label(1));
        let thd = self.label(scope.label(2));
        let body = scope.open(&[
            &Term::free_var(&fst),
            &Term::free_var(&snd),
            &Term::free_var(&thd),
        ]);

        ((fst, snd, thd), body)
    }
}

fn print_var(var: Var, spelling: &Rc<Spelling>) -> Printer {
    pure(spelling.label(var.unwrap()))
}

fn print_atom(atom: Atom) -> Printer {
    flat([pure("'"), pure(atom.as_string())])
}

/// A `Flt` as what reads back as it, bit for bit: a finite value as its signed decimal, the four values no decimal spells as their literals — `+inf.0`, `-inf.0`, and the default NaN of either sign as `+nan.0` and `-nan.0` — and any other NaN, which no literal spells, as the call building it from its bytes, `Flt/of_le_bytes(x[…])`.
fn print_flt(flt: Floating, frame: Frame) -> Printer {
    if !flt.is_finite() {
        let sign = match flt.abs().to_bits() == flt.to_bits() {
            true => "+",
            false => "-",
        };
        if !flt.is_nan() {
            return pure(format!("{sign}inf.0"));
        }
        if flt.abs().to_bits() == Floating::nan().to_bits() {
            return pure(format!("{sign}nan.0"));
        }
        let bytes = Term::intrinsic(Intrinsic::Bin(Grain::X, flt.to_le_bytes()));
        return print_call("Flt/of_le_bytes", vec![], vec![bytes], frame);
    }

    let mut string = format!("{:+}", f64::from(flt));

    // string always starts with '+' or '-'; work on the digits after the sign
    let after_sign = &string[1..];

    if let Some(exp) = after_sign.find(['e', 'E']) {
        if !after_sign[..exp].contains('.') {
            string.insert_str(1 + exp, ".0");
        }
    } else if !after_sign.contains('.') {
        string.push_str(".0");
    }

    pure(string)
}

/// An intrinsic operation as the surface calls it — `Nat/shl(a, b)`, never `Nat.shl a b`. `path` is where `/sys` declares it, under its carrier's module; `/std` re-exports it under the same name, so for a reader the call is spelled as that declaration resolves from where they stand (`Spelling::intrinsic_symbol`) and a dump reads the path. It is also how the same term prints before reduction unfolds that `/sys` global, which is the agreement [`print_former`] states for type formers. A type argument is marked `@`, exactly as the application of the global marks it. The proof an operation carries — a bound on an index, a nonzero divisor — is not an argument here: the reader never wrote one, the operator or the elaborator inserted it, and it is erased.
fn print_call(
    path: impl AsRef<str>,
    implicits: Vec<Term>,
    explicits: Vec<Term>,
    frame: Frame,
) -> Printer {
    let name = frame.spelling.intrinsic_symbol(path.as_ref());
    print_named_call(name, implicits, explicits, frame)
}

/// [`print_call`] under a name already spelled — a host call's, which names its own subject.
fn print_named_call(
    name: impl Into<String>,
    implicits: Vec<Term>,
    explicits: Vec<Term>,
    frame: Frame,
) -> Printer {
    let arguments = implicits
        .into_iter()
        .map(|term| marked_argument(sub(term, frame), Some(&Plicity::Implicit)))
        .chain(explicits.into_iter().map(|term| sub(term, frame)))
        .collect::<Vec<_>>();
    // A row with no parameters is a constant in `/sys`, not a nullary function, and is named the way a constant is.
    if arguments.is_empty() {
        return pure(name);
    }
    flat([pure(name), listed("(".into(), false, arguments, ")")])
}

/// A parameterized intrinsic type former as the surface applies it — `List(Nat)`, never `List Nat`. The same type reaches a report two ways, as the intrinsic node and as its `/sys` global applied, and a reader shown `t : List Nat` beside `xs : List(Nat)` is being told two types where there is one.
fn print_former(name: &'static str, argument: Term, frame: Frame) -> Printer {
    print_call(name, vec![], vec![argument], frame)
}

/// A bracketed literal from its already-rendered entries: `[` and its packed cousins `b[`/`x[` opened by the caller, `]` closed here.
fn print_entries(open: &'static str, entries: Vec<Printer>) -> Printer {
    flat([pure(open), sep_flat(entries, || pure(", ")), pure("]")])
}

/// [`print_entries`] under a grain letter.
fn print_packed(grain: Grain, entries: Vec<Printer>) -> Printer {
    print_entries(
        match grain {
            Grain::B => "b[",
            Grain::X => "x[",
        },
        entries,
    )
}

/// A certified string as the literal it stands for — `"body"` in place of the struct over its bytes and the proof certifying them (axis (f)). `None` unless the spelling was told which declaration that is and the head is it, and `None` for a `Str` whose bytes are not one constant — a concatenation, a slice, a binder's — or are not text, each of which has nothing shorter to say than its own spelling.
fn string_literal(name: &Global, fields: &[Term], spelling: &Rc<Spelling>) -> Option<String> {
    if spelling.string_literal.as_ref() != Some(name) {
        return None;
    }

    let Subterm::Intrinsic(Intrinsic::Bin(Grain::X, packed)) = &**fields.first()? else {
        return None;
    };

    let text = String::from_utf8(packed.to_bytes()?).ok()?;

    // A diagnostic quoting a page of text whole says nothing a prefix does not, and a report is where this spelling is read: the rest is counted rather than shown.
    let shown = text.chars().count();
    if shown > ELIDED_LITERAL_CHARACTERS {
        let head: String = text.chars().take(ELIDED_LITERAL_CHARACTERS).collect();
        return Some(format!(
            "\"{}…\" ({} more characters)",
            escaped(&head),
            shown - ELIDED_LITERAL_CHARACTERS
        ));
    }

    Some(format!("\"{}\"", escaped(&text)))
}

/// How much of a certified string a report spells before eliding the rest.
const ELIDED_LITERAL_CHARACTERS: usize = 64;

/// A string's characters under the surface's escapes, so a rendered literal reads back as the one it came from.
fn escaped(text: &str) -> String {
    text.chars()
        .map(|character| match character {
            '\\' => "\\\\".to_string(),
            '"' => "\\\"".to_string(),
            '\n' => "\\n".to_string(),
            '\t' => "\\t".to_string(),
            '\r' => "\\r".to_string(),
            other => other.to_string(),
        })
        .collect()
}

/// The constant atoms of a packed literal, spelled as the surface writes them — `0`/`1` for bits, hexadecimal numerals for bytes.
fn bin_atoms(grain: Grain, packed: &Binary) -> Vec<Printer> {
    match grain {
        Grain::B => (0..packed.bit_length())
            .map(|index| pure(if packed.bit(index).unwrap() { "1" } else { "0" }))
            .collect(),
        Grain::X => packed
            .as_bytes()
            .unwrap()
            .iter()
            .map(|byte| pure(format!("0x{byte:X}")))
            .collect(),
    }
}

/// The entries of a list concatenation as the surface spells them: a literal operand contributes its items in place, a nested concatenation its own entries, and anything else a `..` spread. Lowering turns the `[h, ..t]` a reader wrote into a concatenation of the literal `[h]` with `t`, and substitution nests one concatenation inside another; splicing both back is what lets the report quote the program rather than its lowering. Concatenation is associative, so the splice changes no value — which is exactly why axis (g) exists: a report that has found two groupings rendering alike keeps the nesting instead.
fn list_concat_entries(operands: Vec<Term>, frame: Frame, entries: &mut Vec<Printer>) {
    for operand in operands {
        match &*operand {
            Subterm::Intrinsic(Intrinsic::List { .. }) => {
                let Subterm::Intrinsic(Intrinsic::List { items, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                entries.extend(items.into_iter().map(|item| sub(item, frame)));
            }
            Subterm::Intrinsic(Intrinsic::ListConcat { .. }) if !frame.spelling.grouped => {
                let Subterm::Intrinsic(Intrinsic::ListConcat { operands, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                list_concat_entries(operands, frame, entries);
            }
            Subterm::Intrinsic(Intrinsic::ListAppend { .. }) if !frame.spelling.grouped => {
                let Subterm::Intrinsic(Intrinsic::ListAppend { list, item, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                list_concat_entries(vec![list], frame, entries);
                entries.push(sub(item, frame));
            }
            _ => entries.push(flat([pure(".."), sub(operand, frame)])),
        }
    }
}

/// [`list_concat_entries`] for a packed concatenation: a constant operand of the same grain contributes its atoms in place.
fn bin_concat_entries(grain: Grain, operands: Vec<Term>, frame: Frame, entries: &mut Vec<Printer>) {
    for operand in operands {
        match &*operand {
            Subterm::Intrinsic(Intrinsic::Bin(g, packed)) if *g == grain => {
                entries.extend(bin_atoms(grain, packed));
            }
            Subterm::Intrinsic(Intrinsic::BinConcat { grain: g, .. })
                if *g == grain && !frame.spelling.grouped =>
            {
                let Subterm::Intrinsic(Intrinsic::BinConcat { operands, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                bin_concat_entries(grain, operands, frame, entries);
            }
            Subterm::Intrinsic(Intrinsic::BinAppend { grain: g, .. })
                if *g == grain && !frame.spelling.grouped =>
            {
                let Subterm::Intrinsic(Intrinsic::BinAppend { bin, element, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                bin_concat_entries(grain, vec![bin], frame, entries);
                entries.push(sub(element, frame));
            }
            _ => entries.push(flat([pure(".."), sub(operand, frame)])),
        }
    }
}

/// The `/sys` path of a float operation rounded in `rounding`: `Flt/add` in the default direction, `Flt/toward_zero/add` in another.
fn flt_rounded(rounding: Rounding, operation: &str) -> String {
    match rounding {
        Rounding::TiesToEven => format!("Flt/{operation}"),
        rounding => format!("Flt/{}/{operation}", rounding.label()),
    }
}

/// The surface infix symbol an operator intrinsic prints as, or `None` for an intrinsic with no infix spelling — the bitwise ops, conversions, `min`/`max`, and the `Bool.xor` that `!=` desugars through. Exactly the operators the surface language spells infix (`InfixOp::symbol`); the concept-dispatched arithmetic/comparison operators plus the two hardcoded `Bool` short-circuits.
fn infix_symbol(intrinsic: &Intrinsic) -> Option<&'static str> {
    Some(match intrinsic {
        Intrinsic::NatAdd(..)
        | Intrinsic::IntAdd(..)
        | Intrinsic::FltAdd(Rounding::TiesToEven, ..) => "+",
        Intrinsic::NatSub(..)
        | Intrinsic::IntSub(..)
        | Intrinsic::FltSub(Rounding::TiesToEven, ..) => "-",
        Intrinsic::NatMul(..)
        | Intrinsic::IntMul(..)
        | Intrinsic::FltMul(Rounding::TiesToEven, ..) => "*",
        Intrinsic::NatDiv { .. }
        | Intrinsic::IntDiv { .. }
        | Intrinsic::FltDiv(Rounding::TiesToEven, ..) => "/",
        Intrinsic::NatRem { .. } | Intrinsic::IntRem { .. } | Intrinsic::FltRem(..) => "%",
        Intrinsic::NatEql(..)
        | Intrinsic::IntEql(..)
        | Intrinsic::FltEql(..)
        | Intrinsic::BoolEql(..)
        | Intrinsic::BinEql(..) => "==",
        Intrinsic::NatNeq(..)
        | Intrinsic::IntNeq(..)
        | Intrinsic::FltNeq(..)
        | Intrinsic::BoolNeq(..) => "!=",
        Intrinsic::NatLt(..) | Intrinsic::IntLt(..) | Intrinsic::FltLt(..) => "<",
        Intrinsic::NatLe(..) | Intrinsic::IntLe(..) | Intrinsic::FltLe(..) => "<=",
        Intrinsic::BoolAnd(..) => "&&",
        Intrinsic::BoolOr(..) => "||",
        _ => return None,
    })
}

/// Render an operator intrinsic as `left <symbol> right`, each operand parenthesized when it is itself an infix operator so nesting stays unambiguous — `(a + b) * c`, never `a + b * c`.
fn print_infix(symbol: &'static str, left: Term, right: Term, frame: Frame) -> Printer {
    flat([
        print_operand(left, frame),
        pure(format!(" {symbol} ")),
        print_operand(right, frame),
    ])
}

/// A recognized type-former eta shape: the former's identity, the arguments left once its binders are stripped, and the former's whole arity, against which those arguments' declared marks are read.
enum FormerEta {
    Nominal(Global, Vec<Term>, usize),
    Intrinsic(&'static str),
}

/// Recognize a type-former eta shape on the *unopened* telescope. Two shapes contract. `(x) => T(…, x)`: one binder, whose sole occurrence is the final argument of a saturated former body — a nominal type with no indices, or a unary intrinsic carrier. And `(x₁, …, xₖ) => F(p…)(x₁, …, xₖ)`: one binder per index of an indexed family, in order — the family at its parameters, which is a function of its own because the family takes its indices in a call of their own. The binders' plicities are deliberately not inspected: the eta-lambdas this contracts are imitation solutions, which copy their plicities from the former's birth type, so the binders already mirror the declaration. The arguments left must be closed under the binders (`reach() == 0`), which is what guarantees the binders occur nowhere else.
fn former_eta(telescope: &Telescope<Term>, plicities: &[Plicity]) -> Option<FormerEta> {
    let binders = telescope.len();
    if binders == 0 || plicities.len() != binders {
        return None;
    }

    let bound = |term: &Term, index: usize| matches!(&**term, Subterm::Var(var) if var.as_bound() == Some(index));
    let closed = |terms: &[Term]| terms.iter().all(|term| term.reach() == 0);
    // Outermost binder first: under `binders` binders the first index is bound at `binders - 1` and the last at `0`.
    let binds_in_order = |terms: &[Term]| {
        terms.len() == binders
            && terms
                .iter()
                .enumerate()
                .all(|(position, term)| bound(term, binders - 1 - position))
    };

    match &**telescope.terminal() {
        Subterm::InductType(InductType {
            name,
            params,
            indices,
            ..
        }) if !indices.is_empty() => (binds_in_order(indices) && closed(params))
            .then(|| FormerEta::Nominal(*name, params.clone(), params.len() + indices.len())),
        _ if binders != 1 => None,
        Subterm::InductType(InductType { name, params, .. })
        | Subterm::StructType(StructType { name, params, .. }) => {
            let (last, prefix) = params.split_last()?;
            (bound(last, 0) && closed(prefix))
                .then(|| FormerEta::Nominal(*name, prefix.to_vec(), params.len()))
        }
        Subterm::Intrinsic(Intrinsic::IoType(payload)) => {
            bound(payload, 0).then_some(FormerEta::Intrinsic("Io"))
        }
        Subterm::Intrinsic(Intrinsic::ListType(payload)) => {
            bound(payload, 0).then_some(FormerEta::Intrinsic("List"))
        }
        Subterm::Intrinsic(Intrinsic::ChannelType(payload)) => {
            bound(payload, 0).then_some(FormerEta::Intrinsic("Channel"))
        }
        Subterm::Intrinsic(Intrinsic::CellType(payload)) => {
            bound(payload, 0).then_some(FormerEta::Intrinsic("Cell"))
        }
        _ => None,
    }
}

/// Print a recognized former: the name alone when the binders took every argument, the application to what they left otherwise, each argument marked as the declaration marks it — routed through a synthetic term so qualification and spelling stay uniform with every other reference.
fn former_doc(former: FormerEta, frame: Frame) -> Printer {
    match former {
        // Spelled as its carrier resolves from the reader, as the intrinsic's own type former is: `/std/Io` wherever `Io` is not in scope.
        FormerEta::Intrinsic(name) => pure(frame.spelling.intrinsic_symbol(name)),
        FormerEta::Nominal(name, prefix, arity) => {
            let marks = frame.spelling.nominal_marks(&name, arity);
            let reference = Term::var(Var::free(Free::Global(name)));
            let term = if prefix.is_empty() {
                reference
            } else {
                Term::apply_marked(
                    reference,
                    prefix.into_iter().enumerate().map(|(index, argument)| {
                        let mark = marks.and_then(|marks| marks.get(index)).copied();
                        (mark.unwrap_or(Plicity::Explicit), argument)
                    }),
                )
            };
            sub(term, frame)
        }
    }
}

/// A function type's parameters from `cursor` on, each rendered into `printers` in order, then its output — every entry type under the binders before it, and a witness binder's node in scope for everything after it (axis (h)).
fn parameter_types(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    frame: Frame,
    printers: &mut Vec<Printer>,
) -> Printer {
    let Some((_, ty)) = cursor.entry() else {
        return sub(cursor.body().expect("a cursor past every entry"), frame);
    };
    let raw = cursor.label();
    let label = frame.label(raw);
    let plicity = plicities.get(cursor.args().len());
    let mark = plicity_mark(plicity);
    // A hintless binder is compiler-minted (an anonymous parameter), so its label appears only when the rest of the telescope references it — `(B) -> C` renders as written, not `(#6577: B) -> C`.
    let named = match raw {
        Some(name) => name.hint().is_some() || cursor.binder_used(),
        None => false,
    };
    let typed = sub(ty.clone(), frame);
    let slot = printers.len();
    printers.push(pure(""));
    cursor.advance(Term::free_var(&label));

    let (output, named) =
        if plicity == Some(&Plicity::Witness) && frame.spelling.witnesses.is_some() {
            // A function type cannot name its witness, so the binder is spelled only where a reference resolution would not restore still names it.
            let node = WitnessNode::new(label, &ty, frame);
            let output = parameter_types(cursor, plicities, frame.with_witness(&node), printers);
            (output, node.used.get())
        } else {
            (parameter_types(cursor, plicities, frame, printers), named)
        };

    printers[slot] = if named {
        flat([
            pure(mark),
            pure(frame.spelling.label(&label)),
            pure(": "),
            typed,
        ])
    } else {
        flat([pure(mark), typed])
    };
    output
}

/// A lambda's parameters from `cursor` on, their spellings pushed onto `marked`, then its body indented under them — a witness binder's node in scope for the body (axis (h)). A lambda may name its witness, so the binder keeps its name.
fn lambda_parameters(
    mut cursor: Cursor<'_, Term>,
    plicities: &[Plicity],
    minting: Frame,
    marked: &mut Vec<String>,
) -> Printer {
    let Some((_, ty)) = cursor.entry() else {
        let body = cursor.body().expect("a cursor past every entry");
        // The body sits on the arrow's line when it fits and indents on its own line when it does not. A body that is a multi-line form of its own takes the line unconditionally: those forms spell their breaks as literal newlines, which end the fits scan within budget rather than failing it, so a group would render the arrow's line flat and leave the form's first line trailing the arrow.
        let separator = match &*body {
            Subterm::Match(_) | Subterm::Let(_) | Subterm::Rec(_) => hard_line(),
            _ => line(),
        };
        return indent(flat([separator, sub(body, minting)]));
    };
    let label = minting.label(cursor.label());
    let plicity = plicities.get(cursor.args().len());
    let shown = if label.hint().is_none() && !cursor.binder_used() {
        "_".to_string()
    } else {
        minting.spelling.label(&label)
    };
    marked.push(format!("{}{shown}", plicity_mark(plicity)));
    cursor.advance(Term::free_var(&label));

    if plicity == Some(&Plicity::Witness) && minting.spelling.witnesses.is_some() {
        let node = WitnessNode::new(label, &ty, minting);
        lambda_parameters(cursor, plicities, minting.with_witness(&node), marked)
    } else {
        lambda_parameters(cursor, plicities, minting, marked)
    }
}

/// An operand of [`print_infix`], wrapped in parentheses when it too prints infix — a nested operator intrinsic, a residual `Infix` node, or a successor over a symbolic tail, which is how reduction stores `k + 1` and which prints as `k + 1` without being an operator intrinsic; self-delimiting operands (variables, literals, applications) print bare.
fn print_operand(term: Term, frame: Frame) -> Printer {
    let parenthesize = match &*term {
        Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(_, tail))) => {
            !matches!(tail.as_ref(), Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)))
        }
        Subterm::Intrinsic(intrinsic) => infix_symbol(intrinsic).is_some(),
        Subterm::Transient(Transient::Infix(_)) => true,
        Subterm::Apply(Apply { head, arguments }) => matches!(
            &**head,
            Subterm::Proj(Proj { head: witness, field: Field::Index(index) })
                if method_operator(witness, *index, arguments, frame).is_some()
        ),
        _ => false,
    };

    if parenthesize {
        flat([pure("("), sub(term, frame), pure(")")])
    } else {
        sub(term, frame)
    }
}

fn print_intrinsic(intrinsic: Intrinsic, frame: Frame) -> Printer {
    match intrinsic {
        Intrinsic::BoolType => pure(frame.spelling.intrinsic_symbol("Bool")),
        Intrinsic::Bool(false) => pure("false"),
        Intrinsic::Bool(true) => pure("true"),
        Intrinsic::BoolAnd(l, r) => print_infix("&&", l, r, frame),
        Intrinsic::BoolOr(l, r) => print_infix("||", l, r, frame),
        Intrinsic::BoolXor(l, r) => print_call("Bool/xor", vec![], vec![l, r], frame),
        Intrinsic::BoolEql(l, r) => print_infix("==", l, r, frame),
        Intrinsic::BoolNeq(l, r) => print_infix("!=", l, r, frame),
        Intrinsic::NatType => pure(frame.spelling.intrinsic_symbol("Nat")),
        Intrinsic::Nat(Nat::Zero) => pure("0"),
        // A successor over a symbolic tail is that tail plus its literal floor — spelled infix (`n + 1`, `(n + m) + 3`) to match the operator intrinsics, its tail parenthesized when it too is an operator. A successor over `0` is a plain numeral (`{spine}`).
        Intrinsic::Nat(Nat::Succ(spine, inner)) => match inner.as_ref() {
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => pure(format!("{spine}")),
            _ => flat([
                print_operand(inner.clone(), frame),
                pure(format!(" + {spine}")),
            ]),
        },
        Intrinsic::NatEql(l, r) => print_infix("==", l, r, frame),
        Intrinsic::NatNeq(l, r) => print_infix("!=", l, r, frame),
        Intrinsic::NatAdd(l, r) => print_infix("+", l, r, frame),
        Intrinsic::NatSub(l, r) => print_infix("-", l, r, frame),
        Intrinsic::NatMul(l, r) => print_infix("*", l, r, frame),
        Intrinsic::NatLt(l, r) => print_infix("<", l, r, frame),
        Intrinsic::NatDiv {
            dividend: l,
            divisor: r,
            ..
        } => print_infix("/", l, r, frame),
        Intrinsic::NatRem {
            dividend: l,
            divisor: r,
            ..
        } => print_infix("%", l, r, frame),
        Intrinsic::NatLe(l, r) => print_infix("<=", l, r, frame),
        Intrinsic::NatAnd(l, r) => print_call("Nat/and", vec![], vec![l, r], frame),
        Intrinsic::NatOr(l, r) => print_call("Nat/or", vec![], vec![l, r], frame),
        Intrinsic::NatXor(l, r) => print_call("Nat/xor", vec![], vec![l, r], frame),
        Intrinsic::NatShl(l, r) => print_call("Nat/shl", vec![], vec![l, r], frame),
        Intrinsic::NatShr(l, r) => print_call("Nat/shr", vec![], vec![l, r], frame),
        Intrinsic::ByteType => pure(frame.spelling.intrinsic_symbol("Byte")),
        Intrinsic::Byte(value) => pure(format!("0x{value:02X}")),
        Intrinsic::ByteToNat(i) => print_call("Byte/to_nat", vec![], vec![i], frame),
        Intrinsic::NatToByte { nat, .. } => print_call("Nat/to_byte", vec![], vec![nat], frame),
        Intrinsic::IntType => pure(frame.spelling.intrinsic_symbol("Int")),
        Intrinsic::Int(value) => pure(format!("{value:+}")),
        Intrinsic::IntEql(l, r) => print_infix("==", l, r, frame),
        Intrinsic::IntNeq(l, r) => print_infix("!=", l, r, frame),
        Intrinsic::IntAdd(l, r) => print_infix("+", l, r, frame),
        Intrinsic::IntSub(l, r) => print_infix("-", l, r, frame),
        Intrinsic::IntMul(l, r) => print_infix("*", l, r, frame),
        Intrinsic::IntDiv {
            dividend: l,
            divisor: r,
            ..
        } => print_infix("/", l, r, frame),
        Intrinsic::IntRem {
            dividend: l,
            divisor: r,
            ..
        } => print_infix("%", l, r, frame),
        Intrinsic::IntLt(l, r) => print_infix("<", l, r, frame),
        Intrinsic::IntLe(l, r) => print_infix("<=", l, r, frame),
        Intrinsic::IntAnd(l, r) => print_call("Int/and", vec![], vec![l, r], frame),
        Intrinsic::IntOr(l, r) => print_call("Int/or", vec![], vec![l, r], frame),
        Intrinsic::IntXor(l, r) => print_call("Int/xor", vec![], vec![l, r], frame),
        Intrinsic::IntShl(l, r) => print_call("Int/shl", vec![], vec![l, r], frame),
        Intrinsic::IntShr(l, r) => print_call("Int/shr", vec![], vec![l, r], frame),
        Intrinsic::FltType => pure(frame.spelling.intrinsic_symbol("Flt")),
        Intrinsic::Flt(flt) => print_flt(flt, frame),
        Intrinsic::FltAdd(Rounding::TiesToEven, l, r) => print_infix("+", l, r, frame),
        Intrinsic::FltSub(Rounding::TiesToEven, l, r) => print_infix("-", l, r, frame),
        Intrinsic::FltMul(Rounding::TiesToEven, l, r) => print_infix("*", l, r, frame),
        Intrinsic::FltDiv(Rounding::TiesToEven, l, r) => print_infix("/", l, r, frame),
        Intrinsic::FltAdd(rounding, l, r) => {
            print_call(flt_rounded(rounding, "add"), vec![], vec![l, r], frame)
        }
        Intrinsic::FltSub(rounding, l, r) => {
            print_call(flt_rounded(rounding, "sub"), vec![], vec![l, r], frame)
        }
        Intrinsic::FltMul(rounding, l, r) => {
            print_call(flt_rounded(rounding, "mul"), vec![], vec![l, r], frame)
        }
        Intrinsic::FltDiv(rounding, l, r) => {
            print_call(flt_rounded(rounding, "div"), vec![], vec![l, r], frame)
        }
        Intrinsic::FltFma(rounding, a, b, c) => {
            print_call(flt_rounded(rounding, "fma"), vec![], vec![a, b, c], frame)
        }
        Intrinsic::FltRem(l, r) => print_infix("%", l, r, frame),
        Intrinsic::FltEql(l, r) => print_infix("==", l, r, frame),
        Intrinsic::FltNeq(l, r) => print_infix("!=", l, r, frame),
        Intrinsic::FltLt(l, r) => print_infix("<", l, r, frame),
        Intrinsic::FltLe(l, r) => print_infix("<=", l, r, frame),
        Intrinsic::FltMin(l, r) => print_call("Flt/min", vec![], vec![l, r], frame),
        Intrinsic::FltMax(l, r) => print_call("Flt/max", vec![], vec![l, r], frame),
        Intrinsic::FltCopysign(l, r) => print_call("Flt/copysign", vec![], vec![l, r], frame),
        Intrinsic::FltNeg(i) => print_call("Flt/neg", vec![], vec![i], frame),
        Intrinsic::FltAbs(i) => print_call("Flt/abs", vec![], vec![i], frame),
        Intrinsic::FltSqrt(rounding, i) => {
            print_call(flt_rounded(rounding, "sqrt"), vec![], vec![i], frame)
        }
        Intrinsic::FltRoundIntegral(rounding, i) => print_call(
            format!("Flt/{}", rounding.integral_label()),
            vec![],
            vec![i],
            frame,
        ),
        Intrinsic::FltToLeBytes(i) => print_call("Flt/to_le_bytes", vec![], vec![i], frame),
        Intrinsic::FltOfLeBytes { bin: i, .. } => {
            print_call("Flt/of_le_bytes", vec![], vec![i], frame)
        }
        Intrinsic::NatToInt(i) => print_call("Nat/to_int", vec![], vec![i], frame),
        Intrinsic::NatToFlt(Rounding::TiesToEven, i) => {
            print_call("Nat/to_flt", vec![], vec![i], frame)
        }
        Intrinsic::NatToFlt(rounding, i) => {
            print_call(flt_rounded(rounding, "of_nat"), vec![], vec![i], frame)
        }
        Intrinsic::IntToNat { int: i, .. } => print_call("Int/to_nat", vec![], vec![i], frame),
        Intrinsic::IntToFlt(Rounding::TiesToEven, i) => {
            print_call("Int/to_flt", vec![], vec![i], frame)
        }
        Intrinsic::IntToFlt(rounding, i) => {
            print_call(flt_rounded(rounding, "of_int"), vec![], vec![i], frame)
        }
        Intrinsic::FltToNat { flt: i, .. } => print_call("Flt/to_nat", vec![], vec![i], frame),
        Intrinsic::FltToInt { flt: i, .. } => print_call("Flt/to_int", vec![], vec![i], frame),
        Intrinsic::FltMantissa { flt: i, .. } => print_call("Flt/mantissa", vec![], vec![i], frame),
        Intrinsic::FltExponent { flt: i, .. } => print_call("Flt/exponent", vec![], vec![i], frame),
        Intrinsic::BinType(Grain::X) => pure(frame.spelling.intrinsic_symbol("Bytes")),
        Intrinsic::Bin(Grain::X, bytes) => print_packed(Grain::X, bin_atoms(Grain::X, &bytes)),
        Intrinsic::BinLen(Grain::X, b) => print_call("Bytes/len", vec![], vec![b], frame),
        Intrinsic::BinEql(Grain::X, l, r) => print_infix("==", l, r, frame),
        Intrinsic::BinGet {
            grain: Grain::X,
            bin: b,
            index: i,
            in_range: _,
        } => print_call("Bytes/get", vec![], vec![b, i], frame),
        Intrinsic::BinSlice {
            grain: Grain::X,
            bin,
            start,
            length,
            within: _,
        } => print_call("Bytes/slice", vec![], vec![bin, start, length], frame),
        Intrinsic::BinType(Grain::B) => pure(frame.spelling.intrinsic_symbol("Bits")),
        Intrinsic::Bin(Grain::B, bits) => print_packed(Grain::B, bin_atoms(Grain::B, &bits)),
        Intrinsic::BinLen(Grain::B, b) => print_call("Bits/len", vec![], vec![b], frame),
        Intrinsic::BinEql(Grain::B, l, r) => print_infix("==", l, r, frame),
        Intrinsic::BinGet {
            grain: Grain::B,
            bin: b,
            index: i,
            in_range: _,
        } => print_call("Bits/get", vec![], vec![b, i], frame),
        Intrinsic::BinSlice {
            grain: Grain::B,
            bin,
            start,
            length,
            within: _,
        } => print_call("Bits/slice", vec![], vec![bin, start, length], frame),
        // An append has no named form in the surface: `x[..acc, b]` is how a program writes one, and so how a report does.
        Intrinsic::BinAppend {
            grain,
            bin,
            element,
        } => {
            let mut entries = Vec::new();
            bin_concat_entries(grain, vec![bin], frame, &mut entries);
            entries.push(sub(element, frame));
            print_packed(grain, entries)
        }
        Intrinsic::BinConcat { grain, operands } => {
            let mut entries = Vec::new();
            bin_concat_entries(grain, operands, frame, &mut entries);
            print_packed(grain, entries)
        }
        // One arm apiece rather than one per grain, as `BinConcat` above already is: these four are the same rule at two element types, and the grain decides nothing here but which carrier's name to spell. The bound is dropped like every other proof operand — it is not what the program wrote.
        // The grain names the operand, so the spelling is the direction it is read *out* of.
        Intrinsic::BinReinterp {
            grain,
            bin,
            aligned: _,
        } => print_call(
            match grain {
                Grain::X => "Bytes/to_bits",
                Grain::B => "Bits/to_bytes",
            },
            vec![],
            vec![bin],
            frame,
        ),
        Intrinsic::BinReplicate { grain, count, atom } => print_call(
            match grain {
                Grain::X => "Bytes/replicate",
                Grain::B => "Bits/replicate",
            },
            vec![],
            vec![count, atom],
            frame,
        ),
        Intrinsic::BinAnd {
            grain,
            left,
            right,
            same_length: _,
        } => print_call(
            match grain {
                Grain::X => "Bytes/and",
                Grain::B => "Bits/and",
            },
            vec![],
            vec![left, right],
            frame,
        ),
        Intrinsic::BinOr {
            grain,
            left,
            right,
            same_length: _,
        } => print_call(
            match grain {
                Grain::X => "Bytes/or",
                Grain::B => "Bits/or",
            },
            vec![],
            vec![left, right],
            frame,
        ),
        Intrinsic::BinXor {
            grain,
            left,
            right,
            same_length: _,
        } => print_call(
            match grain {
                Grain::X => "Bytes/xor",
                Grain::B => "Bits/xor",
            },
            vec![],
            vec![left, right],
            frame,
        ),
        Intrinsic::ListType(elem) => print_former("List", elem, frame),
        Intrinsic::List {
            element: _,
            items: elems,
        } => flat([
            pure("["),
            sep_flat(elems.into_iter().map(move |e| sub(e, frame)), || pure(", ")),
            pure("]"),
        ]),
        Intrinsic::ListLen { element: ty, list } => {
            print_call("List/len", vec![ty], vec![list], frame)
        }
        Intrinsic::ListGet {
            element: ty,
            list,
            index,
            in_range: _,
        } => print_call("List/get", vec![ty], vec![list, index], frame),
        Intrinsic::ListSlice {
            element: ty,
            list,
            start,
            length,
            within: _,
        } => print_call("List/slice", vec![ty], vec![list, start, length], frame),
        Intrinsic::ListAppend {
            element: _,
            list,
            item,
        } => {
            let mut entries = Vec::new();
            list_concat_entries(vec![list], frame, &mut entries);
            entries.push(sub(item, frame));
            print_entries("[", entries)
        }
        Intrinsic::ListConcat {
            element: _,
            operands,
        } => {
            let mut entries = Vec::new();
            list_concat_entries(operands, frame, &mut entries);
            print_entries("[", entries)
        }
        Intrinsic::ListMap {
            from: a,
            to: b,
            list,
            function: f,
        } => print_call("List/map", vec![a, b], vec![list, f], frame),
        Intrinsic::ListFold {
            element,
            result,
            list,
            init,
            function,
        } => print_call(
            "List/fold",
            vec![element, result],
            vec![list, init, function],
            frame,
        ),
        Intrinsic::HandleType => pure(frame.spelling.intrinsic_symbol("Handle")),
        // The three `/sys/Handle` constants are the only handles a term ever holds: every other handle is minted by the host at run time, behind an `Io` no reduction enters. The last arm names a token no source can spell, and spells the token rather than abort the diagnostic it is inside.
        Intrinsic::Handle(stdio::STDIN) => pure(frame.spelling.intrinsic_symbol("Handle/stdin")),
        Intrinsic::Handle(stdio::STDOUT) => pure(frame.spelling.intrinsic_symbol("Handle/stdout")),
        Intrinsic::Handle(stdio::STDERR) => pure(frame.spelling.intrinsic_symbol("Handle/stderr")),
        Intrinsic::Handle(token) => pure(format!("Handle({token})")),
        Intrinsic::CellType(elem) => print_former("Cell", elem, frame),
        Intrinsic::ChannelType(elem) => print_former("Channel", elem, frame),
        Intrinsic::Cell { element } => print_call("Cell/new", vec![element], vec![], frame),
        Intrinsic::CellFill {
            element,
            cell,
            value,
        } => print_call("Cell/fill", vec![element], vec![cell, value], frame),
        Intrinsic::CellPoll { element, cell, .. } => {
            print_call("Cell/poll", vec![element], vec![cell], frame)
        }
        Intrinsic::Channel {
            element,
            capacity,
            positive,
        } => print_call(
            "Channel/new",
            vec![element],
            vec![capacity, positive],
            frame,
        ),
        Intrinsic::ChannelPush {
            element,
            channel,
            value,
        } => print_call("Channel/push", vec![element], vec![channel, value], frame),
        Intrinsic::ChannelTake {
            element, channel, ..
        } => print_call("Channel/take", vec![element], vec![channel], frame),
        Intrinsic::ChannelClose { element, channel } => {
            print_call("Channel/close", vec![element], vec![channel], frame)
        }
        Intrinsic::ChannelClosed { element, channel } => {
            print_call("Channel/closed", vec![element], vec![channel], frame)
        }
        Intrinsic::ChannelCount { element, channel } => {
            print_call("Channel/count", vec![element], vec![channel], frame)
        }
        Intrinsic::ChannelCapacity { element, channel } => {
            print_call("Channel/capacity", vec![element], vec![channel], frame)
        }
        Intrinsic::IoType(result) => print_former("Io", result, frame),
        Intrinsic::IoPure {
            result: type_,
            value,
        } => print_call("Io/pure", vec![type_], vec![value], frame),
        Intrinsic::IoBind {
            from: a,
            to: b,
            action,
            continuation: f,
        } => print_call("Io/bind", vec![a, b], vec![action, f], frame),
    }
}

/// A child document.
///
/// Every recursive call in this module goes through here, which is what makes this the one place the descent needs guarding: printing a term is a recursive function over a recursive structure, so building the document descends as deep as the term — and a diagnostic that cannot be printed is worse than no diagnostic, since it aborts the compiler while it is trying to *report* something else. [`recurse`] is what makes that depth affordable. Running and freeing the finished document stay iterative in [`Printer`] itself, for the same reason at a different layer.
fn sub(term: Term, frame: Frame) -> Printer {
    recurse(|| term_doc(term, frame))
}

/// A delimited comma-list that fits on one line or breaks one item per line, indented — `f(a, b)` against `f(\n  a,\n  b\n)`. `spaced` spells the flat padding inside the delimiters so the flat form stays byte-identical to the fixed layout it replaced: `false` for parenthesized lists, `true` for brace literals (`S { a, b }`). Behavior-neutral on the unbounded `Display` path, where every group renders flat.
fn listed(open: String, spaced: bool, items: Vec<Printer>, close: &'static str) -> Printer {
    let lead = if spaced { line } else { soft_line };
    group(flat([
        pure(open),
        indent(flat([
            lead(),
            sep_flat(items, || flat([pure(","), line()])),
        ])),
        lead(),
        pure(close),
    ]))
}

/// [`sub`] for an intrinsic's operands.
fn sub_intrinsic(intrinsic: Intrinsic, frame: Frame) -> Printer {
    recurse(|| print_intrinsic(intrinsic, frame))
}

pub(crate) fn print_term(term: Term, spelling: &Rc<Spelling>) -> Printer {
    print_minting(term, spelling, &Minting::default())
}

/// [`print_term`] under `mint`: a fresh one for a render, a recording one for [`build_rename`]'s dry run.
fn print_minting(term: Term, spelling: &Rc<Spelling>, mint: &Minting) -> Printer {
    let frame = Frame {
        spelling,
        mint,
        witnesses: None,
    };
    if spelling.witnesses.is_none() {
        return term_doc(term, frame);
    }
    within_reader(term, frame, &spelling.reader.witnesses)
}

/// [`term_doc`] under the reader's witness binders, outermost first, each node on this stack while the rest of the render borrows it.
fn within_reader(term: Term, frame: Frame, binders: &[(Free, Term)]) -> Printer {
    match binders.split_first() {
        None => term_doc(term, frame),
        Some(((label, type_), rest)) => {
            let node = WitnessNode::new(*label, type_, frame);
            within_reader(term, frame.with_witness(&node), rest)
        }
    }
}

fn term_doc(term: Term, frame: Frame) -> Printer {
    match Term::unwrap_or_clone(term) {
        Subterm::Type(level) => {
            if level.is_zero() || (frame.spelling.erase_universes && level.metas().next().is_some())
            {
                pure("Type")
            } else {
                pure(format!("Type.{{{level}}}"))
            }
        }
        Subterm::Prop => pure("Prop"),
        Subterm::Instance(instance) => flat([
            sub(instance.head.to_term(), frame),
            pure(universe_suffix(&instance.levels, frame.spelling)),
        ]),
        Subterm::Intrinsic(intrinsic) => sub_intrinsic(intrinsic, frame),
        // A builtin row surfaces under its `/sys` subject (`Handle/write`); a user's `foreign` declaration under the name they gave it.
        Subterm::Foreign(function, args) => {
            let name = match function.subject() {
                Some(subject) => format!("{subject}/{}", function.label()),
                None => function.label().to_string(),
            };
            print_named_call(name, vec![], args, frame)
        }
        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => {
            let mut printers = Vec::with_capacity(telescope.len());
            let output = parameter_types(telescope.cursor(), &plicities, frame, &mut printers);
            flat([
                listed("(".into(), false, printers, ")"),
                pure(" -> "),
                output,
            ])
        }
        Subterm::Func(Func {
            telescope,
            plicities,
        }) => {
            // A type-former lambda `(x) => T(…, x)`, or one over exactly an indexed family's indices — the shapes witness keying and goal displays materialize for a higher-kinded parameter — prints as the former itself: bare `T` when the binders took every argument, the application to what they left otherwise (`Accessible(@A, R)`). Recognition demands the exact eta shape (the binders are the final arguments and occur nowhere else), so the display never renames anything, it only hides the lambda the reader would mentally contract anyway.
            if let Some(former) = former_eta(&telescope, &plicities) {
                return former_doc(former, frame);
            }
            // Each binder carries its written/canonical mark (`@x` = implicit, `use x` = witness), matching the `FuncType` printer above. A parameter position cannot be elided, so an unnameable binder nothing references prints the way source spells it: `_`.
            let mut marked = Vec::with_capacity(telescope.len());
            let body = lambda_parameters(telescope.cursor(), &plicities, frame, &mut marked);
            // Parenthesized whatever the count: a lambda's parameter list is always written in parentheses, and a bare `x => x` is a spelling the parser refuses.
            let param_str = format!("({})", marked.join(", "));
            group(flat([pure(param_str), pure(" =>"), body]))
        }
        Subterm::Apply(Apply { head, arguments }) => {
            if let Subterm::Proj(Proj {
                head: witness,
                field: Field::Index(index),
            }) = &*head
                && let Some(method) = method_doc(witness, *index, Some(arguments.as_slice()), frame)
            {
                return method;
            }
            flat([
                sub(head, frame),
                listed(
                    "(".into(),
                    false,
                    arguments
                        .into_iter()
                        .filter(|argument| {
                            argument.plicity != Plicity::Witness
                                || !restorable(&argument.term, frame)
                        })
                        .map(|argument| {
                            marked_argument(sub(argument.term, frame), Some(&argument.plicity))
                        })
                        .collect::<Vec<_>>(),
                    ")",
                ),
            ])
        }
        Subterm::TupleType(TupleType { telescope, .. }) => {
            let mut items = Vec::with_capacity(telescope.len());
            let mut cursor = telescope.cursor();
            while let Some((_, ty)) = cursor.entry() {
                let raw = cursor.label();
                let label = frame.label(raw);
                frame.mint.note_tuple_label(&label);
                // As in the `FuncType` printer: an unnameable label nothing references is elided, so the field renders the way source wrote it.
                let named = match raw {
                    Some(name) => name.hint().is_some() || cursor.binder_used(),
                    None => false,
                };
                let typed = sub(ty, frame);
                let printer = if named {
                    flat([pure(frame.spelling.label(&label)), pure(": "), typed])
                } else {
                    typed
                };
                items.push(indent(printer));
                cursor.advance(Term::free_var(&label));
            }

            // Through `listed` like every other sequence, rather than the hand-rolled always-broken leading-comma form this used to carry: a goal report naming a tuple type is read by a person, and `{a : A, b : B}` on one line is what `documentation/syntax.md` spells. Unspaced for the same reason the surface printer is.
            listed("{".into(), false, items, "}")
        }
        Subterm::Tuple(Tuple { fields, names }) => {
            let mut names = names.into_iter().chain(std::iter::repeat(None));
            listed(
                "(".into(),
                false,
                fields
                    .into_iter()
                    .map(move |f| match names.next().flatten() {
                        Some(name) => flat([pure(name), pure(" = "), sub(f, frame)]),
                        None => sub(f, frame),
                    })
                    .collect(),
                ")",
            )
        }
        Subterm::Proj(Proj { head, field }) => {
            if let Field::Index(index) = field
                && let Some(method) = method_doc(&head, index, None, frame)
            {
                return method;
            }
            let field = match field {
                Field::Index(index) => format!(").{index}"),
                Field::Label(label) => format!(").{label}"),
            };
            flat([pure("("), sub(head, frame), pure(field)])
        }
        // The parameters, then the indices, one call each where the family has both — exactly how its type-constructor function is applied at use sites, `Sized(Nat)(1)`, and marked the same way. Without the marks this spells `Eq(Nat)(5, 5)`, an explicit argument where `Eq(@A : Type) : (A, A) -> Prop` takes an implicit one: a rendering no use site could reproduce.
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => {
            let arity = params.len() + indices.len();
            let marks = frame.spelling.nominal_marks(&name, arity);
            let label = format!(
                "{}{}",
                frame.spelling.symbol(&name),
                universe_suffix(&universes, frame.spelling)
            );
            let arguments =
                |group: Vec<Term>, offset: usize| nominal_arguments(group, marks, offset, frame);
            match (params.is_empty(), indices.is_empty()) {
                (true, true) => pure(label),
                (false, false) => {
                    let offset = params.len();
                    flat([
                        listed(format!("{label}("), false, arguments(params, 0), ")"),
                        listed("(".into(), false, arguments(indices, offset), ")"),
                    ])
                }
                (false, true) => listed(format!("{label}("), false, arguments(params, 0), ")"),
                (true, false) => listed(format!("{label}("), false, arguments(indices, 0), ")"),
            }
        }
        // Prints as the constructor-function call, instantiated type params hidden — `Result/success(42)`.
        Subterm::Variant(Variant {
            name,
            universes,
            tag,
            payload,
            ..
        }) => {
            let name = format!(
                "{}{}",
                frame.spelling.symbol(&name),
                universe_suffix(&universes, frame.spelling)
            );
            if payload.is_empty() {
                pure(format!("{name}/{tag}"))
            } else {
                listed(
                    format!("{name}/{tag}("),
                    false,
                    payload.into_iter().map(|p| sub(p, frame)).collect(),
                    ")",
                )
            }
        }
        // Like `InductType` but with no indices: `Pair(Nat, Bin)`. Concepts are struct-shaped, so a concept application marks its parameters here too.
        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => {
            let marks = frame.spelling.nominal_marks(&name, params.len());
            let label = format!(
                "{}{}",
                frame.spelling.symbol(&name),
                universe_suffix(&universes, frame.spelling)
            );
            if params.is_empty() {
                pure(label)
            } else {
                listed(
                    format!("{label}("),
                    false,
                    nominal_arguments(params, marks, 0, frame),
                    ")",
                )
            }
        }
        // Prints as the brace literal, instantiated type params hidden — `Pair { 0, "" }` — except a certified string, which prints as the literal it stands for (axis (f)).
        Subterm::Struct(Struct {
            name,
            universes,
            fields,
            ..
        }) => match string_literal(&name, &fields, frame.spelling) {
            Some(literal) => pure(literal),
            None => listed(
                format!(
                    "{}{} {{",
                    frame.spelling.symbol(&name),
                    universe_suffix(&universes, frame.spelling)
                ),
                true,
                fields.into_iter().map(|f| sub(f, frame)).collect(),
                "}",
            ),
        },
        Subterm::Match(Match {
            head,
            result,
            cases,
        }) => {
            // A family spells as `: labels => body`, arity 1 everywhere except an annotated inductive-match motive, whose pattern binders precede the scrutinee binder; an ambient goal spells as `~ goal`, with no binder to name.
            let result = match result {
                MatchResult::Family(motive) => {
                    let motive_labels = frame.labels(motive.label_iter());
                    let motive_terms = label_terms(&motive_labels);
                    let motive_refs = motive_terms.iter().collect::<Vec<_>>();
                    let motive_label = motive_labels
                        .iter()
                        .map(|label| frame.spelling.label(label))
                        .collect::<Vec<_>>()
                        .join(", ");
                    let motive = motive.open(&motive_refs);
                    flat([
                        pure(": "),
                        pure(motive_label),
                        pure(" => "),
                        sub(motive, frame),
                    ])
                }
                MatchResult::Ambient(goal) => flat([pure(" ~ "), sub(goal.clone(), frame)]),
            };

            // Shared `<keyword> head <result>;` prefix; the keyword and arm bodies depend on the case kind.
            let keyword = match &cases {
                Cases::Bool { .. } => "Bool.match ",
                Cases::Switch { .. } => "Nat.match ",
                Cases::Induct { .. } => "match ",
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat { .. } => "Nat.fold ",
                    Carrier::Bin { .. } => "Bin.fold ",
                    Carrier::List { .. } => "List.fold ",
                },
            };

            let prefix = flat([pure(keyword), sub(head, frame), result, pure(";")]);

            let arms = match cases {
                Cases::Bool {
                    false_case,
                    true_case,
                } => flat([
                    pure("\n| false =>\n"),
                    indent(flat([sub(false_case, frame), pure(";")])),
                    pure("\n| true =>\n"),
                    indent(flat([sub(true_case, frame), pure(";")])),
                ]),
                Cases::Switch { cases, default } => {
                    let case_printers = flat(
                        cases
                            .into_iter()
                            .map(|(n, body)| {
                                flat([
                                    pure(format!("\n| {n}n =>\n")),
                                    indent(flat([sub(body, frame), pure(";")])),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    );
                    flat([
                        case_printers,
                        pure("\n| _ =>\n"),
                        indent(flat([sub(default, frame), pure(";")])),
                    ])
                }
                Cases::Induct { cases, default, .. } => {
                    let case_printers = flat(
                        cases
                            .into_iter()
                            .map(|(atom, arm)| {
                                let labels = frame.labels(arm.label_iter());
                                let label_terms = label_terms(&labels);
                                let label_terms = label_terms.iter().collect::<Vec<_>>();
                                let body = arm.open(&label_terms);

                                let binders = if labels.is_empty() {
                                    pure("")
                                } else {
                                    pure(format!(
                                        "({})",
                                        labels
                                            .iter()
                                            .enumerate()
                                            .map(|(idx, l)| {
                                                let mark = plicity_mark(arm.plicities.get(idx));
                                                format!("{mark}{}", frame.spelling.label(l))
                                            })
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                    ))
                                };

                                flat([
                                    pure("\n| "),
                                    print_atom(atom),
                                    binders,
                                    pure(" =>\n"),
                                    indent(flat([sub(body, frame), pure(";")])),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    );
                    match default {
                        Some(default) => flat([
                            case_printers,
                            pure("\n| _ =>\n"),
                            indent(flat([sub(default, frame), pure(";")])),
                        ]),
                        None => case_printers,
                    }
                }
                Cases::FreeMonoid { carrier } => {
                    // The cons arm mirrors each carrier's own literal delimiters: `b[head, ..tail]; ih` for `Bin`, `[head, ..tail]; ih` for `List` — the same bracketed shape, told apart by the grain letter.
                    let cons_bin = |grain: Grain, cons_case: Scope<Three>| {
                        let ((head_label, tail_label, ih_label), cons_case) =
                            frame.open_three(cons_case);
                        flat([
                            pure(match grain {
                                Grain::B => "\n| b[",
                                Grain::X => "\n| x[",
                            }),
                            pure(frame.spelling.label(&head_label)),
                            pure(", .."),
                            pure(frame.spelling.label(&tail_label)),
                            pure("]; "),
                            pure(frame.spelling.label(&ih_label)),
                            pure(" =>\n"),
                            indent(flat([sub(cons_case, frame), pure(";")])),
                        ])
                    };
                    let cons_list = |cons_case: Scope<Three>| {
                        let ((head_label, tail_label, ih_label), cons_case) =
                            frame.open_three(cons_case);
                        flat([
                            pure("\n| ["),
                            pure(frame.spelling.label(&head_label)),
                            pure(", .."),
                            pure(frame.spelling.label(&tail_label)),
                            pure("]; "),
                            pure(frame.spelling.label(&ih_label)),
                            pure(" =>\n"),
                            indent(flat([sub(cons_case, frame), pure(";")])),
                        ])
                    };

                    // Per carrier: the identity arm's literal, its body, and the cons arm — which binds `(predecessor, ih)` for the head-less unary `Nat`, and `(head, tail), ih` for `Bin`/`List`.
                    let (empty_lit, empty_case, cons_arm) = match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => {
                            let ((pred_label, ih_label), cons_case) = frame.open_two(cons_case);
                            let cons_arm = flat([
                                pure("\n| "),
                                pure(frame.spelling.label(&pred_label)),
                                pure(" "),
                                pure(frame.spelling.label(&ih_label)),
                                pure(" =>\n"),
                                indent(flat([sub(cons_case, frame), pure(";")])),
                            ]);
                            ("\n| 0n =>\n", empty_case, cons_arm)
                        }
                        Carrier::Bin {
                            grain,
                            empty_case,
                            cons_case,
                        } => (
                            match grain {
                                Grain::B => "\n| b[] =>\n",
                                Grain::X => "\n| x[] =>\n",
                            },
                            empty_case,
                            cons_bin(grain, cons_case),
                        ),
                        Carrier::List {
                            empty_case,
                            cons_case,
                            ..
                        } => ("\n| [] =>\n", empty_case, cons_list(cons_case)),
                    };
                    flat([
                        pure(empty_lit),
                        indent(flat([sub(empty_case, frame), pure(";")])),
                        cons_arm,
                    ])
                }
            };

            flat([prefix, arms])
        }
        Subterm::Let(Let { bindings, tail, .. }) => {
            let labels = frame.labels(tail.label_iter());
            let label_terms = label_terms(&labels);
            let label_terms = label_terms.iter().collect::<Vec<_>>();

            let lines = bindings
                .iter()
                .enumerate()
                .map(|(index, binding)| {
                    let type_ = binding.type_().release(&label_terms[..index]);
                    let value = binding.value().release(&label_terms[..index]);
                    flat([
                        pure("let "),
                        pure(frame.spelling.label(&labels[index])),
                        pure(": "),
                        sub(type_, frame),
                        pure(" =\n"),
                        indent(flat([sub(value, frame), pure(";")])),
                        pure("\n"),
                    ])
                })
                .collect::<Vec<_>>();

            flat([flat(lines), sub(tail.open(&label_terms), frame)])
        }
        Subterm::Rec(Rec { group, tail }) => {
            let labels = frame.labels(tail.label_iter());
            let label_terms = label_terms(&labels);
            let label_terms = label_terms.iter().collect::<Vec<_>>();

            let bindings = group
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, member)| {
                    let type_ = member.type_.open(&label_terms);
                    let body = member.body.open(&label_terms);

                    flat([
                        pure(frame.spelling.label(&labels[index])),
                        pure(": "),
                        sub(type_, frame),
                        pure(" =\n"),
                        indent(sub(body, frame)),
                    ])
                })
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                sub(tail, frame),
            ])
        }
        Subterm::Var(var) => {
            // A reference reaching the rendering is what keeps a witness binder's name (axis (h)).
            if let Some(label) = var.as_free()
                && let Some(node) = chain(frame.witnesses).find(|node| &node.label == label)
            {
                node.used.set(true);
            }
            print_var(var, frame.spelling)
        }
        Subterm::Transient(Transient::NumLit(NumLit::Number { magnitude, sign })) => {
            pure(format!("{}{magnitude}", sign.symbol()))
        }
        Subterm::Transient(Transient::NumLit(NumLit::Character(character))) => {
            pure(format!("'{}'", escape_character(character)))
        }
        // Through `print_infix` so nested operands parenthesize — `(a + b) * c` — exactly like the intrinsic operators; display folds (`denoise`) nest these nodes.
        Subterm::Transient(Transient::Infix(Infix { op, left, right })) => {
            print_infix(op.symbol(), left, right, frame)
        }
        // A `!` sequencing site prints as the written bang followed by its hoisted continuation, so a lowered-stage dump reads close to the source region.
        Subterm::Transient(Transient::Bang(Bang {
            action,
            continuation,
        })) => flat([sub(action, frame), pure("!; "), sub(continuation, frame)]),
        // The body a derived witness asks for, before elaboration has written it: no surface form spells it, so the dump names the transient.
        Subterm::Transient(Transient::Derive) => pure("derive"),
        // Identity and renaming spines (every entry a variable) are the uninteresting common case and print as the bare id; a spine carrying anything else is exactly the one worth seeing. Under axis (e) neither is: the spine is elaboration state like the id, and the reader gets `?`.
        Subterm::Metavar(metavar) => {
            if frame.spelling.anonymous_metavars {
                pure("?")
            } else if metavar
                .spine
                .iter()
                .all(|entry| matches!(&**entry, Subterm::Var(_)))
            {
                pure(format!("?{}", metavar.id))
            } else {
                flat([
                    pure(format!("?{}[", metavar.id)),
                    sep_flat(
                        metavar
                            .spine
                            .iter()
                            .map(|entry| sub(entry.clone(), frame))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure("]"),
                ])
            }
        }
    }
}

#[cfg(test)]
mod tests;

/// A character rendered as its literal body: the five escapes by their spellings, anything else verbatim.
fn escape_character(character: char) -> String {
    match character {
        '\n' => "\\n".to_string(),
        '\t' => "\\t".to_string(),
        '\r' => "\\r".to_string(),
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        other => other.to_string(),
    }
}
