//! How Core is spelled: the naming policy a term is written by ([`Spelling`]), which the writer of Core text (`core_text`) builds every term's tree through — as Core text proper for a dump and a certifier's refusal, and in its reader's spelling for every term a reader is shown, a term the translation to the surface tree declines among them. A report's term is translated back to the surface tree ([`from_core()`]) and printed as source is, through the same policy.

use {
    crate::{
        CoreEntry, CoreHead, CoreItem, CoreMember, CoreValue, Decoration, DecoratorField, Frame,
        LetSignature, Shown, ShownOptions, TopForeign, TopLet, TopTest, Tree, bracketed,
        core_written, decoration_fields, family_declared, from_core, level_spelled, literal_of,
        nested, print_shown, print_shown_entry, print_shown_item, print_term, record_declared,
        spell, string, sub, tree, universe_context_written, value, word, word_value,
    },
    curios_abi::{ForeignFunction, StdioCodes},
    curios_common::{InfixOp, LINE_WIDTH, Plicity, Qualifier, Symbol, SyntaxRegistry},
    curios_core::{
        Arity, Atom, Bound, Cursor, Definition, DefinitionKind, Definitions, Field, Free, Func,
        Global, HeadKey, InductArm, Intrinsic, Item, Label, Level, Lexicon, Marks, Members,
        Mention, Nat, Nominal, Notation, Program, Scope, Spellings, StructDecl, Subterm, Symbols,
        Term, UniverseContext, UniverseError, WitnessKey, inconsistent_sort, suffixes_of,
    },
    curios_num::{Binary, Floating, Integer, Natural, Rounding},
    curios_print::{Printer, run_printer, run_printer_within},
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeSet, HashMap},
        fmt,
        rc::Rc,
        sync::{Arc, OnceLock},
    },
};

// === Source-style names (diagnostics) ========================================
//
// Core spells names for the certifier's convenience, not the reader's: every binder is opened under a `Context::fresh` gensym (`n#15`, `(n#0 : Nat) -> …`) and every global is its fully-qualified canonical path (`/std/Vec/Vec`, `/sys/Nat/Nat`). A [`Spelling`] rewrites both back toward what the user wrote; the default one changes nothing, so a faithful render ([`Spellable::faithful`]) leaves names untouched.
//
// The configuration is threaded, not ambient. `Display::fmt` has no parameter channel, and thread-locals installed around a render would make a term's spelling depend on an enclosing frame nobody could see from the call, and "should this consumer erase universes?" a question answered by accident of where the installer sat rather than by the consumer. [`Spelled`] restores the parameter: `term.spelled(&spelling)` is an ordinary value that implements `Display`, and every printer function threads a `Frame` carrying that spelling beside the binder depth.
//
// axis (a) — local binders: a *rename map* (built by `build_rename` over `display_names`) alpha-renames the whole fragment — free vars *and* binder labels. A source hint is used bare when unique; distinct names sharing a hint, or shadowing a global's displayed rendering — whatever axis (b) spells it as — take minimal `hint2`, `hint3`, … suffixes, so no two binders ever read alike. A hintless (compiler-minted) binder spells `_` — or is elided — at its label site when nothing references it, and borrows the fallback hint `x` when something does: `_` in a reference position would read as a hole and could not co-spell with its binder.
//
// axis (b) — globals. A report spells each one as name resolution would find it from where its reader stands ([`ReaderNames`]): the shortest of a declaration of the reader's module by its label, a path into one of its child modules, an import in scope at the reader's definition as it was written, and an absolute path whose audience includes the reader — the text stage's [`Spellings`], since only it sees re-exports and visibility. Where none reaches it this printer writes it in full, and the reader's printer declines the term ([`Spelling::reached`]). A suffix unique among the unit's symbols is not the same thing: `Ord` is such a suffix and resolves nowhere `/std/Ord` was not imported. A dump writes every global by its absolute path, as Core text proper does. Renders without a reader — the certifier's refusals, and a report a caller could hand no spellings — take a *shorten map* instead ([`Shorten`], over the symbols of the unit a reader wrote and then of the units around it, [`Spelling::with_lexicons`]), each qualified path's shortest unambiguous `/`-suffix, the reader's unit first, so its declarations settle their own spelling before what surrounds them competes for it. Why reports spell for a reader is `documentation/design/tools/a-diagnostic-spells-what-its-reader-can-write.md`.
//
// axis (c) — universe instances: a flag suppressing the `.{…}` an instantiated nominal head carries. The surface language has no spelling for an instance — solved (`Option.{0}`) or unsolved (`Eq.{?u271}`) alike — so a diagnostic that shows one asks the reader to decode elaboration state. This is the display twin of `project_erased_universes`, which the goal-report path applies structurally; errors carry raw terms all the way to the formatter, so they suppress at the printer instead. Diagnostics set it; `wonder --stage`'s dumps deliberately do not, because a dump is read *about* the compiler and its levels are the point.
//
// A `Type`'s own level is suppressed only when it is *metavariable-headed*. The level is that node's whole content, so erasing a concrete one could render two distinct sorts identically — but an unsolved level names nothing a reader can act on, and it is the common case: a diagnostic over a polymorphic head reports `(A: Type.{?u263}) -> Nat` against `((Type.{?u261}) -> Type.{?u262}) -> Nat`, three placeholders competing with the disagreement they surround. Suppressing every level was rejected for the case that cannot be ruled out — two distinct concrete levels rendering as `Type` against `Type` — and a whole-fragment "show it only when it disambiguates" pass was rejected as context-dependent rendering: unlike a binder name, which is inherently relative, a level is an absolute fact about the term.
//
// axis (e) — unsolved metavariables: a flag spelling every metavariable as a bare `?`. An id such as `?2677` is elaboration state — a counter a reader cannot decode and the surface cannot write — and a diagnostic carrying one reads as three placeholders competing with the disagreement they surround: the transcript case is `inferred: Prop, expected: Eq(@?2677)(?2679, ?2680)`, where `Eq(@?)(?, ?)` says what the reader needs, that *some* equality was expected. Two distinct metavariables rendering alike is not the hazard it is for levels: a mismatch is reported between rigid structure, and two unsolved metavariables facing each other unify rather than mismatch, so no diagnostic hinges on telling them apart. Diagnostics set it; `wonder --stage`'s dumps do not, for axis (c)'s reason.
//
// axis (f) — string literals: the identity of the certified `Str` declaration, so a literal spells as the text it stands for rather than as the struct over its bytes. A `Str` is bytes beside the proof certifying them, and a report that spells one structurally — `Str { x[0x62, 0x6F, 0x64, 0x79], True/qed() }` for `"body"` — buries the one thing the reader wrote under the representation that certifies it. The identity is supplied rather than spelled: this crate sits below `curios-module`, which holds the prelude, so it may not name a prelude declaration and takes the one the syntax registry names instead. Diagnostics and goal reports set it; `wonder --stage`'s dumps do not, for axis (c)'s reason.
//
// axis (g) — grouping: a flag under which a nested concatenation spells as the operand it is, `[..[..a, ..b], ..c]`, rather than splicing its entries into the enclosing literal. The splice is right everywhere the reader wants the program quoted rather than its lowering, and it is what makes two terms that differ only in how a run is grouped render as one string — which a mismatch report cannot afford, since grouping is a difference conversion can refuse on. The report's escalation sets it, first, because it changes nothing unless a nesting is present; nothing else does.
//
// axis (h) — witnesses: the witness binders in scope where a reader stands ([`ReaderPosition`]), so that a reader's printer declines a term that refers to one, the grammar writing no name beside the witness mark. Which witnesses a report's term leaves out is settled before the term reaches a printer: the elaborator writes it as a program writes it where the reader stands, a witness resolution puts back there an open slot (`curios-elab`'s `written`). This printer prints a witness as it stands, which is what a dump, a certifier's refusal and a term a reader's printer declines are read as. Reports set it. Why is `documentation/design/tools/a-diagnostic-spells-what-its-reader-can-write.md`.
//
// `Spelling::label` consults the shorten map first (globals), then the rename map (locals); a name in neither renders verbatim.

/// How a term is spelled for a reader. The default spells nothing differently, which is what a faithful render uses.
#[derive(Clone, Default)]
pub struct Spelling {
    /// axis (a) — local binders to their source-style names.
    pretty: Option<Rc<Rename>>,
    /// axis (b) without a reader — global qualified names to their shortest unambiguous suffix.
    shorten: Option<Rc<Shorten>>,
    /// axis (c) — whether to suppress universe instances and metavariable-headed levels.
    erase_universes: bool,
    /// axis (d) — each unit's nominal declarations' parameter plicities, the reader's own unit first, so an applied family is marked the way a use site would write it. A name resolves to one declaration, so the first unit to list it answers.
    nominal_plicities: Vec<Rc<Marks>>,
    /// axis (e) — whether every metavariable spells as a bare `?`.
    anonymous_metavars: bool,
    /// axis (f) — the certified-string declaration, so a `Str` literal spells as its own text.
    string_literal: Option<Global>,
    /// axis (g) — whether a nested concatenation keeps its grouping instead of being spliced into the enclosing literal.
    grouped: bool,
    /// axis (b) for a reader: how the unit's names can be written and where its definitions sit, so a global is spelled as resolution would find it from where the reader stands. Takes precedence over the shorten map, which stays what a render without a reader — a dump, a certifier refusal — spells by.
    names: Option<Rc<ReaderNames>>,
    /// Where the reader stands (axes (b) and (h)).
    reader: ReaderPosition,
    /// Each unit's structure fields and constructor payloads, the reader's own unit first: the marks and labels a field, a literal's entry and a constructor's argument are written by.
    members: Vec<Rc<Members>>,
    /// Whether a term shown to a reader is translated back to the surface tree and printed as a program writes it ([`from_core()`]), and printed here only where the translation declines.
    translated: bool,
    /// Whether the reader's printer declined a term this spelling rendered. Shared by a spelling's copies and begun anew where a reader is placed ([`Spelling::for_reader`]), so it is one report's to read.
    declined: Rc<Cell<bool>>,
    /// Whether a term is written as Core text proper, every field it holds stated, or in its reader's spelling: what a dump and a certifier's refusal are read as, and what reads back as the term ([`Spelling::core_text`]).
    core_text: bool,
    /// Whether a term is written whole, however far its tree outgrows its graph: Core text a reader reads back, where a dump and a report are bounded and elide what is past their bound ([`Minting::elides`]).
    whole: bool,
    /// Whether Core text is written as a text of its own — a file, a dump — rather than as a term inside a sentence: a decorated term on its line where it fits there whole, and otherwise its decorator on a line of its own above it, at the workspace's line width ([`Spelling::written_as_text`]).
    as_text: bool,
    /// What a test's and a foreign row's forms are made against, where a module's printer is handed it ([`FormScope`]).
    form_scope: Option<Rc<FormScope>>,
}

/// What a form that implies a definition is made against beside its own text: the syntax registry its terms name the compiler's declarations through, and the entries of the units it is read over, by their formers' names. A printer handed one writes a test and a foreign row in their forms, `test /t = …;` and `foreign /f(Bytes) -> Bytes;`, and a reader of Core text is handed the same to make their definitions from ([`Definition::test`], [`Definition::foreign`]).
pub struct FormScope {
    pub syntax: SyntaxRegistry,
    pub entries: HashMap<Global, Rc<Nominal>>,
    /// The foreign rows of the units it is read over, by the declarations that state them: what a call across the wire in the text is made through.
    pub rows: HashMap<Global, Arc<ForeignFunction>>,
}

impl FormScope {
    /// The scope of a text read over `units` under `syntax`: every entry their formers carry.
    pub fn over<'d>(
        syntax: SyntaxRegistry,
        units: impl IntoIterator<Item = &'d Definitions>,
    ) -> Self {
        let mut entries = HashMap::new();
        let mut rows = HashMap::new();
        for item in units
            .into_iter()
            .flat_map(|definitions| definitions.items.iter())
        {
            if let Item::Let(definition) = item
                && let Some(row) = foreign_row(definition)
            {
                rows.insert(definition.name, row);
            }
            let held = match item {
                Item::Let(definition) => vec![(definition.name, &definition.kind)],
                Item::Rec(rec) => rec
                    .definitions
                    .iter()
                    .map(|member| (member.name, &member.kind))
                    .collect(),
            };
            for (name, kind) in held {
                if let DefinitionKind::Nominal(nominal) = kind {
                    entries.insert(name, Rc::clone(nominal));
                }
            }
        }

        Self {
            syntax,
            entries,
            rows,
        }
    }
}

/// Where a reader stands when they read a rendered term: the definition they wrote it in (the entrypoint's final term when `None`), whose module and imports decide what reaches a name, and the witness binders in scope there, outermost first, which no program refers to by name.
#[derive(Clone, Debug, Default)]
pub struct ReaderPosition {
    pub owner: Option<Global>,
    pub witnesses: Rc<[Free]>,
}

/// What axis (b) spells a global by for a reader: the unit's [`Spellings`] and the module each of its definitions sits in, asked of the definition a reader stands in, with the spellings already asked for.
pub struct ReaderNames {
    spellings: Rc<Spellings>,
    islands: Islands,
    memo: RefCell<Asked>,
}

/// The module the definition a global names sits in, asked for by the global ([`Definition::island`]): what a reader standing in it reaches a name from.
pub type Islands = Box<dyn Fn(&Global) -> Option<Qualifier>>;

/// The spellings a render already asked for, keyed by the definition the reader stands in and the global spelled — `None` where the reader can reach it by none.
type Asked = HashMap<(Option<Global>, Global), Option<String>>;

impl ReaderNames {
    pub fn new(spellings: Rc<Spellings>, islands: Islands) -> Self {
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
            .and_then(|owner| (self.islands)(owner))
            .unwrap_or_else(Qualifier::empty);
        let spelled = spell(&self.spellings, &island, reader.owner.as_ref(), global);
        self.memo.borrow_mut().insert(key, spelled.clone());
        spelled
    }
}

impl Spelling {
    /// Rename local binders to source-style names (axis (a)).
    pub fn with_pretty_names(mut self, rename: Rc<Rename>) -> Self {
        self.pretty = Some(rename);
        self
    }

    /// Shorten global names against the symbols of a scope (axis (b)).
    pub fn with_short_names(mut self, shorten: Rc<Shorten>) -> Self {
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

    /// Render for a reader standing at `position` (axis (b)'s owner and axis (h)'s witness scope).
    pub fn for_reader(mut self, position: ReaderPosition) -> Self {
        self.reader = position;
        self.declined = Rc::default();
        self
    }

    /// Whether a term rendered under this spelling since its reader was placed is one the reader's printer declined, and so printed as the compiler holds it. What a report reads to say so.
    pub fn declined(&self) -> bool {
        self.declined.get()
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

    /// The name the reader reaches `global` by, `None` inside where they reach it by none. `None` outside where no reader's names are set: spelling a name for a reader is resolution in reverse ([`spell()`]).
    fn read(&self, global: &Global) -> Option<Option<String>> {
        let names = self.names.as_ref()?;

        Some(names.spell(&self.reader, global))
    }

    /// Spell a certified string literal as its own text (axis (f)). `name` is the `Str` declaration a literal's head carries — [`SyntaxRegistry`]'s, since this crate may not spell it.
    pub fn with_string_literals(mut self, name: Global) -> Self {
        self.string_literal = Some(name);
        self
    }

    /// Mark a nominal family's implicit parameters (axis (d)), from one unit's table of them ([`Lexicon`]): a table for each call, the reader's own unit's first.
    pub fn with_nominal_plicities(mut self, plicities: Rc<Marks>) -> Self {
        self.nominal_plicities.push(plicities);
        self
    }

    /// Shorten globals and mark nominal families against the lexicons of a reader's scope (axes (b) and (d)): `own`, the reader's unit's, over `scope`, each where its unit keeps it. Nothing is built over the scope: a name is looked up in each unit's own tables when a render asks.
    pub fn with_lexicons(mut self, own: &Lexicon, scope: &[&Lexicon]) -> Self {
        let around = scope.iter().map(|unit| Rc::clone(&unit.symbols));
        self.shorten = Some(Rc::new(Shorten::over(Rc::clone(&own.symbols), around)));
        self.nominal_plicities = std::iter::once(own)
            .chain(scope.iter().copied())
            .map(|unit| Rc::clone(&unit.plicities))
            .collect();
        self.members = std::iter::once(own)
            .chain(scope.iter().copied())
            .map(|unit| Rc::clone(&unit.members))
            .collect();
        self
    }

    /// The name a reader writes `global` by, for a printer that declines what no program writes where the reader stands: `None` where a reader is placed and reaches it by none, and the display spelling ([`Spelling::symbol`]) where no reader is.
    pub fn reached(&self, global: &Global) -> Option<String> {
        match self.read(global) {
            Some(spelled) => spelled,
            None => Some(self.symbol(global)),
        }
    }

    /// The display spelling of a global (axis (b)) — for a reader, as resolution would find it from where they stand, and in full where it cannot, since a suffix nothing resolves is a different program; without one, shortened against the module's other symbols when that is unambiguous, and in full otherwise. Globals never take axis (a)'s rename: their spelling is a path a programmer wrote, not a minted hint.
    pub fn symbol(&self, name: &Global) -> String {
        if let Some(spelled) = self.read(name) {
            return spelled.unwrap_or_else(|| name.faithful().to_string());
        }
        self.shorten
            .as_ref()
            .and_then(|shorten| shorten.get(name))
            .unwrap_or_else(|| name.faithful().to_string())
    }

    /// The display spelling of a local binder: its pretty rename (axis (a)), falling back to its minting hint. A name in neither renders verbatim. A declaration is spelled by [`Spelling::symbol`].
    pub fn label(&self, name: &Free) -> String {
        self.pretty
            .as_ref()
            .and_then(|rename| rename.get(name).cloned())
            .unwrap_or_else(|| match name.hint() {
                Some(hint) => hint.to_string(),
                None => name.faithful().to_string(),
            })
    }

    /// The declared plicities of `name`'s arguments — parameters then indices, in the order a use site supplies them — or `None` when the declaration is not in this spelling's table, in which case an applied family renders unmarked.
    pub fn nominal_marks(&self, name: &Global, arity: usize) -> Option<&[Plicity]> {
        let marks = self
            .nominal_plicities
            .iter()
            .find_map(|unit| unit.get(name))?;
        // A declaration whose vector does not match the occurrence is not one this can speak about: render flat rather than mark the wrong argument.
        (marks.len() == arity).then_some(marks.as_slice())
    }

    /// The fields `structure` declares, in slot order, each under its mark and with its label where it has one: what a projection out of its value and an entry of its literal are written by. `None` where no unit of the spelling's scope declares it.
    pub fn structure_fields(&self, structure: &Global) -> Option<&[(Plicity, Option<String>)]> {
        self.members
            .iter()
            .find_map(|unit| unit.fields.get(structure))
            .map(|fields| fields.as_slice())
    }

    /// The marks `family`'s constructor `tag` is called by, in order: the family's parameters, every one hidden, then its payload, which is the tail a constructor's value holds.
    pub fn payload_marks(&self, family: &Global, tag: &Atom) -> Option<&[Plicity]> {
        self.members
            .iter()
            .find_map(|unit| unit.payloads.get(family))?
            .iter()
            .find(|(declared, _)| declared == tag)
            .map(|(_, marks)| marks.as_slice())
    }

    /// A built-in type or operation, named by the path under `/sys` a reader's spelling writes it by — a carrier's type `X` at `X/X`, an operation at its own path — spelled for the reader where one is set and the declaration reaches them, and by that path otherwise.
    pub(crate) fn intrinsic_symbol(&self, path: &str) -> String {
        self.read(&intrinsic_global(path))
            .flatten()
            .unwrap_or_else(|| path.to_string())
    }

    /// `name` as a binder is written where it binds: its spelling, or `_` for a hintless binder nothing reads, to which the rename gave no word.
    pub(crate) fn word(&self, name: &Free) -> String {
        let renamed = self
            .pretty
            .as_ref()
            .is_some_and(|rename| rename.get(name).is_some());
        match name.hint().is_none() && !renamed {
            true => "_".to_string(),
            false => self.label(name),
        }
    }

    /// This spelling with every binder of `term` under a word of its own (axis (a)), where it was handed no rename: what a render with no reader writes binders by, so a binder minted with no hint is written as a word and never as the index it was minted under.
    pub(crate) fn with_words(self: &Rc<Self>, term: &Term) -> Rc<Self> {
        match self.pretty {
            Some(_) => Rc::clone(self),
            None => {
                let rename = build_rename(&display_names(term), self);
                Rc::new((**self).clone().with_pretty_names(Rc::new(rename)))
            }
        }
    }

    /// This spelling with every binder `render` opens under a word of its own, where it was handed no rename: [`Spelling::with_words`] for a render that writes several terms in one naming, a declaration's head and its cases, whose dry run is the render itself. `terms` are what it writes, whose names no word may read like.
    pub(crate) fn with_words_of<R>(
        self: &Rc<Self>,
        terms: &[Term],
        render: impl Fn(Frame) -> R,
    ) -> Rc<Self> {
        if self.pretty.is_some() {
            return Rc::clone(self);
        }
        let mint = Minting::recording();
        mint.allow_all(terms);
        render(Frame {
            spelling: self,
            mint: &mint,
        });
        let recorded = mint
            .recorded
            .expect("a recording mint records")
            .into_inner();
        let mut shown = terms
            .iter()
            .flat_map(|term| term.mentions().iter().copied().collect::<Vec<_>>())
            .map(|name| DisplayKey::mentioned(&name))
            .collect::<BTreeSet<_>>();
        shown.extend(recorded.names);
        let rename = assign(&shown, &recorded.tuple_labels, self);

        Rc::new((**self).clone().with_pretty_names(Rc::new(rename)))
    }

    /// The certified-string declaration (axis (f)), where one is set.
    pub fn string_declaration(&self) -> Option<&Global> {
        self.string_literal.as_ref()
    }

    /// Whether a nested concatenation keeps its grouping (axis (g)).
    pub fn keeps_grouping(&self) -> bool {
        self.grouped
    }

    /// Whether universe instances are left out (axis (c)): what a reader's printer asks before it writes a term, since no program writes one.
    pub fn erases_universes(&self) -> bool {
        self.erase_universes
    }

    /// Whether every metavariable is written as a bare `?` (axis (e)).
    pub fn anonymizes_metavariables(&self) -> bool {
        self.anonymous_metavars
    }

    /// Whether a term is written as Core text proper, every field it holds stated, rather than in its reader's spelling ([`Spelling::core_text`]).
    pub fn writes_core_text(&self) -> bool {
        self.core_text
    }

    /// Whether a term is written whole, eliding nothing, as Core text a reader reads back is.
    pub fn writes_whole(&self) -> bool {
        self.whole
    }

    /// Whether Core text is written as a text of its own, a decorator above what it decorates where the two do not fit one line.
    pub fn writes_as_text(&self) -> bool {
        self.as_text
    }

    /// The width a render under this spelling lays its groups out against: a text's is the workspace's line width, and a term a sentence quotes has none unless its caller sets one ([`Spelled::within`]).
    fn width(&self) -> Option<usize> {
        self.as_text.then_some(LINE_WIDTH)
    }

    /// The spelling Core text proper is written in: every global by its absolute path and every field a node holds stated, so what it writes reads back as the term. A spelling a reader is handed writes a term in that reader's spelling instead: no decorator, an intrinsic by its surface name, a level metavariable as `?u1`.
    pub fn core_text() -> Self {
        Self {
            core_text: true,
            ..Self::default()
        }
    }

    /// This spelling writing every term whole, eliding nothing, as Core text a reader reads back is written: a dump is read about the compiler and bounded as a report is, since a term's tree can outgrow any print — a tower of sixty pairs is a type of 2⁶⁰ leaves.
    pub fn written_whole(mut self) -> Self {
        self.whole = true;
        self
    }

    /// This spelling writing Core text as a text of its own — a `.crsc`, a dump — rather than a term a sentence quotes, as a certifier's refusal does: a decorated term stays on the line it is reached on where it fits there whole, and otherwise its decorator takes a line of its own and what it decorates the line beneath at its indentation, a level deeper where the decorator follows something else on its line; the text is rendered at [`LINE_WIDTH`], which decides what fits.
    pub fn written_as_text(mut self) -> Self {
        self.as_text = true;
        self
    }

    /// This spelling with `scope` to write a test's and a foreign row's forms against.
    pub fn with_form_scope(mut self, scope: Rc<FormScope>) -> Self {
        self.form_scope = Some(scope);
        self
    }

    /// How a certifier refusal of `definitions` is written: as Core text proper ([`Spelling::core_text`]), a refusal being read about the compiler, with globals shortened against the lexicons of its unit and of `scope` (axis (b)), a nominal family's implicit parameters marked (axis (d)), a `Str` literal as its text (axis (f)) — and every universe shown, unlike an elaboration diagnostic, since a certifier refusal is often about the universes.
    pub fn for_refusal(
        definitions: &Definitions,
        scope: &[&Lexicon],
        syntax: &SyntaxRegistry,
    ) -> Self {
        // A module carries only its own declarations, so the spelling is told what its environment put in scope, which each unit in scope says by the lexicon it keeps.
        let own = definitions.lexicon();

        Self::core_text()
            .with_lexicons(&own, scope)
            .with_string_literals(Global::Authored(syntax.string.string.qualifier()))
    }

    /// Print a term shown to a reader as a program writes it, translated back to the surface tree, and as the compiler holds it where the translation declines.
    pub fn with_translation(mut self) -> Self {
        self.translated = true;
        self
    }
}

/// A value paired with the [`Spelling`] it renders under — the parameter channel `Display::fmt` does not have. Produced by [`Spellable::spelled`] and [`Spellable::faithful`].
pub struct Spelled<'a, T> {
    value: &'a T,
    spelling: Rc<Spelling>,
    width: Option<usize>,
}

impl<'a, T> Spelled<'a, T> {
    fn new(value: &'a T, spelling: &Rc<Spelling>) -> Self {
        Self {
            value,
            spelling: Rc::clone(spelling),
            width: spelling.width(),
        }
    }

    /// Render within `width` columns: the printer's groups fit or break against the target instead of the unbounded flat layout a plain render keeps. Diagnostics printing large terms go through this.
    pub fn within(mut self, width: usize) -> Self {
        self.width = Some(width);
        self
    }

    fn value(&self) -> &'a T {
        self.value
    }

    fn spelling(&self) -> &Rc<Spelling> {
        &self.spelling
    }

    fn width(&self) -> Option<usize> {
        self.width
    }
}

/// The terms a report renders, gathered so [`build_rename`] can spell every name any of them shows: what they mention, and the binders a render of each opens — which only a render can say, since a scope remembers a binder's hint and not its identity.
#[derive(Debug, Default, Clone)]
pub struct DisplayNames {
    terms: Vec<Term>,
    mentioned: BTreeSet<Mention>,
}

impl DisplayNames {
    /// Add `term` to what the report renders.
    pub fn add(&mut self, term: &Term) {
        self.mentioned.extend(term.mentions().iter().copied());
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
        .mentioned
        .iter()
        .map(DisplayKey::mentioned)
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
    // Declarations reserve the spelling they will display under.
    let mut used = shown
        .iter()
        .filter_map(|key| key.name.as_declaration())
        .map(|declaration| spelling.symbol(declaration))
        .collect::<BTreeSet<_>>();
    let prettifiable = shown
        .iter()
        .filter_map(|key| Some((key, key.name.as_local()?)))
        .collect::<Vec<_>>();

    let mut spellings = HashMap::new();
    for label in tuple_labels {
        if let Some(hint) = label.name.as_local().and_then(Free::hint) {
            used.insert(hint.to_string());
            spellings.insert(*label, hint.to_string());
        }
    }

    let (hinted, hintless) = prettifiable
        .into_iter()
        .filter(|(key, _)| !spellings.contains_key(*key))
        .partition::<Vec<_>, _>(|(_, local)| local.hint().is_some());

    // Where each hint's search for a free suffix resumes. Every suffix below it was found taken, and a taken spelling stays taken, so a search begun again for each binder would re-ask what is known — for the like-named binders a large type opens, the square of how many there are.
    let mut resume: HashMap<&str, u32> = HashMap::new();
    for (key, local) in hinted.into_iter().chain(hintless) {
        let hint = local.hint().unwrap_or("x");
        let next = resume.entry(hint).or_insert(2);
        let mut candidate = hint.to_string();
        while used.contains(&candidate) {
            candidate = format!("{hint}{next}");
            *next += 1;
        }
        used.insert(candidate.clone());
        spellings.insert(*key, candidate);
    }
    Rename { spellings }
}

/// Each global's shortest `/`-suffix that no other global in scope shares — the name it has in scope, since Curios has no `use … as` aliasing, so an in-scope name is always a suffix. Only a name that actually shortens has one; an ambiguous (or single-segment) name keeps its full path.
///
/// **Answered when a render asks, from the symbols each unit keeps.** A scope's units declare disjoint names, so how many globals carry a suffix is the sum of what each unit counted for itself, and nothing is made for a scope.
///
/// **A reader's own unit is a tier of its own**: a declaration sitting directly in it takes its bare label before anything around it may compete for the suffix. The tier exists because a segment-suffix is not by itself a spelling anyone can write. `/std/Bool/Holds` is reachable as `Bool/Holds` and in full, never as a bare `Holds` — reaching it needs a `use` naming `Holds` itself, which only a reader's spelling ([`ReaderNames`]) can see. Counting the suffix it cannot claim against a reader's own root-declared `Holds` would tie the two, so *neither* would shorten, and the name the reader had just written would be reported in full.
///
/// Only a single-segment name gets the claim, because only its bare label is writable: reaching a reader's own `/Vec/nil` needs `Vec/nil` or an import just as the environment's does, so a nested own name has no better title to `nil` than the shared contest gives it. Handing it one would spell a goal candidate — `? ≈ nil()` — that the reader could not paste.
#[derive(Debug, Default)]
pub struct Shorten {
    own: Rc<Symbols>,
    scope: Vec<Rc<Symbols>>,
}

impl Shorten {
    /// Over `own`, the symbols of the unit a reader stands in, and `scope`, those of each unit around it.
    pub fn over(own: Rc<Symbols>, scope: impl IntoIterator<Item = Rc<Symbols>>) -> Self {
        Self {
            own,
            scope: scope.into_iter().collect(),
        }
    }

    /// The spelling `name` shortens to, where it shortens.
    pub fn get(&self, name: &Global) -> Option<String> {
        let own = self.own.contains(name);
        if !own && !self.scope.iter().any(|unit| unit.contains(name)) {
            return None;
        }
        // Two single-segment names cannot collide — one label, one declaration — so this needs no ambiguity test of its own.
        if own && let Some([label]) = name.qualifier().map(Qualifier::segments) {
            return Some(label.clone());
        }

        // How many distinct globals carry a segment-suffix.
        let carried = |suffix: &str| {
            let counted = |unit: &Rc<Symbols>| unit.carrying(suffix);

            counted(&self.own) + self.scope.iter().map(counted).sum::<usize>()
        };
        // A bare label a declaration at the root of the reader's own unit has taken.
        let claimed = |suffix: &str| {
            !suffix.contains('/')
                && self
                    .own
                    .contains(&Global::Authored(Qualifier::from([suffix])))
        };
        let rendered = name.faithful().to_string();

        suffixes_of(name)
            .into_iter()
            .find(|suffix| carried(suffix) == 1 && !claimed(suffix))
            .filter(|shortest| shortest.len() < rendered.len())
    }
}

/// Where the identities a render mints for the binders it opens begin: above every index a compilation mints, which counts up from zero one binder at a time and does not reach half of a `u64` in any computation that ends.
const PRINTED: u64 = 1 << 63;

/// The fewest child documents a render may build, whatever its term: a report's worth of reading.
const RENDER_FLOOR: u64 = 5_000;

/// How many child documents a render may build for each distinct node of its term. A term is a graph and its print is that graph's tree, so a render is allowed its graph this many times over and elides past that: a term that shares little prints whole, and one whose tree outgrows its graph by more — a chain of `let`s each naming the one before it twice — prints its beginning.
///
/// Set above the largest ratio of tree to graph a definition of the standard library has (`curios-prelude`'s `printed_tree_measurements`), so nothing a reader was shown before is elided.
const RENDER_PER_NODE: u64 = 64;

/// How one render labels the binders it opens. A scope remembers a binder's hint, never its identity, so the render mints one: the `k`th binder it opens is `PRINTED + k` under its hint. A dry run over the same term under the same spelling opens the same binders in the same order, which is how [`build_rename`] spells them before the render that uses the spellings.
#[derive(Default)]
pub struct Minting {
    next: Cell<u64>,
    /// How many child documents this render may still build — see [`RENDER_PER_NODE`].
    allowance: Cell<u64>,
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
            allowance: Cell::new(0),
            recorded: Some(RefCell::new(Recorded::default())),
        }
    }

    /// Allow a render of `term` its child documents: its graph [`RENDER_PER_NODE`] times over, and never fewer than [`RENDER_FLOOR`].
    fn allow(&self, term: &Term) {
        let nodes = u64::try_from(term.distinct_nodes()).unwrap_or(u64::MAX);
        self.allowance
            .set(RENDER_FLOOR.max(RENDER_PER_NODE.saturating_mul(nodes)));
    }

    /// Allow a render of `terms` together their child documents, as [`Minting::allow`] allows one term's.
    pub fn allow_all(&self, terms: &[Term]) {
        let nodes = terms
            .iter()
            .map(|term| u64::try_from(term.distinct_nodes()).unwrap_or(u64::MAX))
            .fold(0, u64::saturating_add);
        self.allowance
            .set(RENDER_FLOOR.max(RENDER_PER_NODE.saturating_mul(nodes)));
    }

    /// Spend one child document on `term`, answering whether it is elided instead: the allowance is spent and `term` has children a document would go on into. A term with no subterm is never elided, since naming it costs what eliding it would; a numeral past zero has one, the base its successors stand on, and is elided with the rest.
    pub fn elides(&self, term: &Term) -> bool {
        match self.allowance.get().checked_sub(1) {
            Some(left) => {
                self.allowance.set(left);
                false
            }
            None => term.as_ref().any_child_term(&mut |_| true),
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
    pub fn note_tuple_label(&self, label: &Free) {
        if let Some(recorded) = &self.recorded {
            recorded
                .borrow_mut()
                .tuple_labels
                .insert(DisplayKey::of(label));
        }
    }

    /// The label the next binder a render opens is printed under: the identity minted from where the render meets it and the hint its scope remembers — none for a `constant` scope, which never had binders written.
    pub fn label(&self, label: Option<&Label>) -> Free {
        let name = self.local(label.and_then(Label::hint_symbol));
        self.record(&name);
        name
    }

    /// The label the binder `cursor` stands at is opened under.
    pub fn cursor_label<B: Bound>(&self, cursor: &Cursor<'_, B>) -> Free {
        self.label(cursor.label())
    }

    /// Every binder of `scope`, labelled in order.
    pub fn scope_labels<A: Arity, B: Bound>(&self, scope: &Scope<A, B>) -> Vec<Free> {
        scope.label_iter().map(|label| self.label(label)).collect()
    }

    /// Every payload binder of `arm`, labelled in order.
    pub fn arm_labels(&self, arm: &InductArm) -> Vec<Free> {
        arm.label_iter().map(|label| self.label(label)).collect()
    }

    /// A mint a reader's printer is tried on: it labels from where this one stands and records as this one does, under an allowance of its own, so a term the printer declines leaves this mint as it was.
    fn attempt(&self, term: &Term) -> Self {
        let attempt = Self {
            next: Cell::new(self.next.get()),
            allowance: Cell::new(0),
            recorded: self.recorded.as_ref().map(|_| RefCell::default()),
        };
        attempt.allow(term);
        attempt
    }

    /// Take up what `attempt` labelled and recorded: the render it stood for is the one this mint's is.
    fn adopt(&self, attempt: Self) {
        self.next.set(attempt.next.get());
        if let (Some(recorded), Some(attempted)) = (&self.recorded, attempt.recorded) {
            let attempted = attempted.into_inner();
            let mut recorded = recorded.borrow_mut();
            recorded.names.extend(attempted.names);
            recorded.tuple_labels.extend(attempted.tuple_labels);
        }
    }
}

/// A name as a rename tells it apart from the others: any name by itself, and a label a render minted by its position together with its hint. Two terms of one report restart their positions, so the hint is what keeps two binders of different names apart; two of the same name in the same position share a spelling, which is harmless, since the terms are never in scope of each other.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct DisplayKey {
    name: Mention,
    hint: Option<Symbol>,
}

impl DisplayKey {
    fn of(name: &Free) -> Self {
        let printed = name.index() >= PRINTED;
        Self {
            name: Mention::Local(*name),
            hint: printed.then(|| name.hint_symbol()).flatten(),
        }
    }

    fn mentioned(name: &Mention) -> Self {
        match name {
            Mention::Local(local) => Self::of(local),
            Mention::Declaration(_) => Self {
                name: *name,
                hint: None,
            },
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

/// How much of a certified string a report spells before eliding the rest.
pub const ELIDED_LITERAL_CHARACTERS: usize = 64;

/// One entry of a list or a packed literal as it is written: an element, a constant atom of a packed one, or a `..` spread of a whole run.
#[derive(Debug, Clone)]
pub enum LiteralEntry<T = Term> {
    /// A constant byte of a `Bytes` literal.
    Byte(u8),
    Item(T),
    Spread(T),
}

/// An operand [`intrinsic_form`] looks into: a literal splices an operand in, and a numeral reads its tail as zero, only where that operand is an intrinsic node itself. A Core term answers it, and so does the surface term `/sys`'s own declarations are built over, which is what lets one table write both.
pub trait IntrinsicOperand: Sized {
    /// The node this operand is, where it is one.
    fn as_intrinsic(&self) -> Option<&Intrinsic<Self>>;

    /// The node this operand is, taken out of it, where it is one.
    fn into_intrinsic(self) -> Option<Intrinsic<Self>>;
}

impl IntrinsicOperand for Term {
    fn as_intrinsic(&self) -> Option<&Intrinsic> {
        match &**self {
            Subterm::Intrinsic(node) => Some(node),
            _ => None,
        }
    }

    fn into_intrinsic(self) -> Option<Intrinsic> {
        match Term::unwrap_or_clone(self) {
            Subterm::Intrinsic(node) => Some(node),
            _ => None,
        }
    }
}

/// The constant atoms of a packed literal: its bytes.
fn packed_atoms<T>(packed: &Binary) -> Vec<LiteralEntry<T>> {
    packed
        .as_bytes()
        .iter()
        .map(|byte| LiteralEntry::Byte(*byte))
        .collect()
}

/// The entries of a list concatenation as the surface spells them: a literal operand contributes its items in place, a nested concatenation its own entries, and anything else a `..` spread. Lowering turns the `[h, ..t]` a reader wrote into a concatenation of the literal `[h]` with `t`, and substitution nests one concatenation inside another; splicing both back is what lets the report quote the program rather than its lowering. Concatenation is associative, so the splice changes no value — which is exactly why axis (g) exists: a report that has found two groupings rendering alike keeps the nesting instead.
fn list_entries<T: IntrinsicOperand>(
    operands: Vec<T>,
    grouped: bool,
    entries: &mut Vec<LiteralEntry<T>>,
) {
    for operand in operands {
        match operand.as_intrinsic() {
            Some(Intrinsic::List { .. }) => {
                let Some(Intrinsic::List { items, .. }) = operand.into_intrinsic() else {
                    unreachable!()
                };
                entries.extend(items.into_iter().map(LiteralEntry::Item));
            }
            Some(Intrinsic::ListConcat { .. }) if !grouped => {
                let Some(Intrinsic::ListConcat { operands, .. }) = operand.into_intrinsic() else {
                    unreachable!()
                };
                list_entries(operands, grouped, entries);
            }
            Some(Intrinsic::ListAppend { .. }) if !grouped => {
                let Some(Intrinsic::ListAppend { list, item, .. }) = operand.into_intrinsic()
                else {
                    unreachable!()
                };
                list_entries(vec![list], grouped, entries);
                entries.push(LiteralEntry::Item(item));
            }
            _ => entries.push(LiteralEntry::Spread(operand)),
        }
    }
}

/// [`list_entries`] for a packed concatenation: a constant operand contributes its atoms in place.
fn packed_entries<T: IntrinsicOperand>(
    operands: Vec<T>,
    grouped: bool,
    entries: &mut Vec<LiteralEntry<T>>,
) {
    for operand in operands {
        match operand.as_intrinsic() {
            Some(Intrinsic::Bytes(packed)) => {
                entries.extend(packed_atoms(packed));
            }
            Some(Intrinsic::BytesConcat { .. }) if !grouped => {
                let Some(Intrinsic::BytesConcat { operands, .. }) = operand.into_intrinsic() else {
                    unreachable!()
                };
                packed_entries(operands, grouped, entries);
            }
            Some(Intrinsic::BytesAppend { .. }) if !grouped => {
                let Some(Intrinsic::BytesAppend { bytes, element, .. }) = operand.into_intrinsic()
                else {
                    unreachable!()
                };
                packed_entries(vec![bytes], grouped, entries);
                entries.push(LiteralEntry::Item(element));
            }
            _ => entries.push(LiteralEntry::Spread(operand)),
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

/// How a reader writes an intrinsic. The one table of it ([`intrinsic_form`]): this printer prints from it, a dump under the paths `/sys` declares and a report under what its reader writes, and the translation a report's term is printed through builds the surface form it names. It is written over either stage's term ([`IntrinsicOperand`]), so the surface printer writes `/sys`'s own declarations through that same translation.
#[derive(Debug, Clone)]
pub enum IntrinsicForm<T = Term> {
    /// A type former or a constant, by the path under `/sys` its declaration stands at ([`intrinsic_global`]).
    Named(&'static str),
    /// An operation called by its path under `/sys`: the type arguments it takes under `@`, then the written ones. The proof an operation carries — a bound on an index, a nonzero divisor — is no argument here: the reader never wrote one, the operator or the elaborator inserted it, and it is erased.
    Call {
        path: String,
        implicits: Vec<T>,
        explicits: Vec<T>,
    },
    /// An operator between its two operands.
    Infix(InfixOp, T, T),
    Bool(bool),
    /// A numeral of `Nat`.
    Nat(Natural),
    /// A successor over a symbolic tail: that tail plus its literal floor, `n + 1`.
    NatAbove {
        tail: T,
        floor: Natural,
    },
    Byte(u8),
    Int(Integer),
    Flt(Floating),
    /// A packed literal, `x[…]`: its entries in order.
    Bytes(Vec<LiteralEntry<T>>),
    /// A list literal: its entries in order.
    List(Vec<LiteralEntry<T>>),
    /// A handle the host minted at run time, which no source spells.
    Handle(u32),
}

/// The absolute path a reader writes an intrinsic's `path` by: an operation at its own path, and a carrier's type at the root, where `/sys` hoists it — `/sys/Bool`.
pub fn intrinsic_path(path: &str) -> Qualifier {
    Qualifier::from(std::iter::once("sys").chain(path.split('/')))
}

/// The declaration under `/sys` an intrinsic's `path` names: a carrier's type `X` at `X/X`, an operation at its own path.
pub fn intrinsic_global(path: &str) -> Global {
    let written = intrinsic_path(path);

    Global::Authored(match written.segments().len() {
        2 => written.with(written.last()),
        _ => written,
    })
}

/// The intrinsic type former `/sys` declares at `global`, over `operands`: `NatType` at `/sys/Nat/Nat` over none, `ListType(T)` at `/sys/List/List` over `T`. `None` where `global` names no intrinsic former, or `operands` are not as many as it takes. What a former written by its name and applied in full is, read off the path each former is written at ([`intrinsic_form`]) once for the process: elaboration asks it of every name it meets.
pub fn intrinsic_former(global: &Global, operands: &[Term]) -> Option<Intrinsic> {
    static FORMERS: OnceLock<HashMap<Global, IntrinsicFormer>> = OnceLock::new();

    FORMERS
        .get_or_init(|| {
            let closed: [fn() -> Intrinsic; 7] = [
                || Intrinsic::BoolType,
                || Intrinsic::NatType,
                || Intrinsic::ByteType,
                || Intrinsic::IntType,
                || Intrinsic::FltType,
                || Intrinsic::BytesType,
                || Intrinsic::HandleType,
            ];
            let over_one: [fn(Term) -> Intrinsic; 4] = [
                Intrinsic::ListType,
                Intrinsic::CellType,
                Intrinsic::ChannelType,
                Intrinsic::IoType,
            ];

            closed
                .map(IntrinsicFormer::Closed)
                .into_iter()
                .chain(over_one.map(IntrinsicFormer::OverOne))
                .map(|former| {
                    let spelled = match former {
                        IntrinsicFormer::Closed(former) => former(),
                        IntrinsicFormer::OverOne(former) => former(Term::type_ground()),
                    };
                    let path = match intrinsic_form(spelled, false) {
                        IntrinsicForm::Named(path) => path.to_owned(),
                        IntrinsicForm::Call { path, .. } => path,
                        _ => unreachable!("a type former is written by its name"),
                    };
                    (intrinsic_global(&path), former)
                })
                .collect()
        })
        .get(global)?
        .over(operands)
}

/// An intrinsic type former as [`intrinsic_former`] builds it: closed, or over its one operand. Each is its constructor, so the table kept for the process holds no term.
#[derive(Clone, Copy)]
enum IntrinsicFormer {
    Closed(fn() -> Intrinsic),
    OverOne(fn(Term) -> Intrinsic),
}

impl IntrinsicFormer {
    fn over(self, operands: &[Term]) -> Option<Intrinsic> {
        match (self, operands) {
            (IntrinsicFormer::Closed(former), []) => Some(former()),
            (IntrinsicFormer::OverOne(former), [operand]) => Some(former(operand.clone())),
            _ => None,
        }
    }
}

/// How `intrinsic` is written: see [`IntrinsicForm`]. Under `grouped` a nested concatenation stays the operand it is (axis (g)), where the splice would hide how a run is grouped.
pub fn intrinsic_form<T: IntrinsicOperand>(
    intrinsic: Intrinsic<T>,
    grouped: bool,
) -> IntrinsicForm<T> {
    let operator = |symbol: &str| InfixOp::from_symbol(symbol).expect("an operator's symbol");

    match intrinsic {
        Intrinsic::BoolType => IntrinsicForm::Named("Bool"),
        Intrinsic::Bool(value) => IntrinsicForm::Bool(value),
        Intrinsic::BoolAnd(l, r) => IntrinsicForm::Infix(operator("&&"), l, r),
        Intrinsic::BoolOr(l, r) => IntrinsicForm::Infix(operator("||"), l, r),
        Intrinsic::BoolXor(l, r) => IntrinsicForm::Call {
            path: "Bool/xor".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::BoolEql(l, r) => IntrinsicForm::Infix(operator("=="), l, r),
        Intrinsic::BoolNeq(l, r) => IntrinsicForm::Infix(operator("!="), l, r),
        Intrinsic::NatType => IntrinsicForm::Named("Nat"),
        Intrinsic::Nat(Nat::Zero) => IntrinsicForm::Nat(Natural::zero()),
        // A successor over `0` is a plain numeral; over a symbolic tail it is that tail plus its literal floor.
        Intrinsic::Nat(Nat::Succ(floor, tail)) => {
            match matches!(tail.as_intrinsic(), Some(Intrinsic::Nat(Nat::Zero))) {
                true => IntrinsicForm::Nat(floor),
                false => IntrinsicForm::NatAbove { tail, floor },
            }
        }
        Intrinsic::NatEql(l, r) => IntrinsicForm::Infix(operator("=="), l, r),
        Intrinsic::NatNeq(l, r) => IntrinsicForm::Infix(operator("!="), l, r),
        Intrinsic::NatAdd(l, r) => IntrinsicForm::Infix(operator("+"), l, r),
        Intrinsic::NatSub(l, r) => IntrinsicForm::Infix(operator("-"), l, r),
        Intrinsic::NatMul(l, r) => IntrinsicForm::Infix(operator("*"), l, r),
        Intrinsic::NatLt(l, r) => IntrinsicForm::Infix(operator("<"), l, r),
        Intrinsic::NatDiv {
            dividend: l,
            divisor: r,
            ..
        } => IntrinsicForm::Infix(operator("/"), l, r),
        Intrinsic::NatRem {
            dividend: l,
            divisor: r,
            ..
        } => IntrinsicForm::Infix(operator("%"), l, r),
        Intrinsic::NatLe(l, r) => IntrinsicForm::Infix(operator("<="), l, r),
        Intrinsic::NatAnd(l, r) => IntrinsicForm::Call {
            path: "Nat/and".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatOr(l, r) => IntrinsicForm::Call {
            path: "Nat/or".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatXor(l, r) => IntrinsicForm::Call {
            path: "Nat/xor".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatMin(l, r) => IntrinsicForm::Call {
            path: "Nat/min".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatMax(l, r) => IntrinsicForm::Call {
            path: "Nat/max".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatShl(l, r) => IntrinsicForm::Call {
            path: "Nat/shl".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatPow(l, r) => IntrinsicForm::Call {
            path: "Nat/pow".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::NatShr(l, r) => IntrinsicForm::Call {
            path: "Nat/shr".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::ByteType => IntrinsicForm::Named("Byte"),
        Intrinsic::Byte(value) => IntrinsicForm::Byte(value),
        Intrinsic::ByteToNat(i) => IntrinsicForm::Call {
            path: "Byte/to_nat".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::NatToByte { nat, .. } => IntrinsicForm::Call {
            path: "Nat/to_byte".into(),
            implicits: vec![],
            explicits: vec![nat],
        },
        Intrinsic::IntType => IntrinsicForm::Named("Int"),
        Intrinsic::Int(value) => IntrinsicForm::Int(value),
        Intrinsic::IntEql(l, r) => IntrinsicForm::Infix(operator("=="), l, r),
        Intrinsic::IntNeq(l, r) => IntrinsicForm::Infix(operator("!="), l, r),
        Intrinsic::IntAdd(l, r) => IntrinsicForm::Infix(operator("+"), l, r),
        Intrinsic::IntSub(l, r) => IntrinsicForm::Infix(operator("-"), l, r),
        Intrinsic::IntMul(l, r) => IntrinsicForm::Infix(operator("*"), l, r),
        Intrinsic::IntDiv {
            dividend: l,
            divisor: r,
            ..
        } => IntrinsicForm::Infix(operator("/"), l, r),
        Intrinsic::IntRem {
            dividend: l,
            divisor: r,
            ..
        } => IntrinsicForm::Infix(operator("%"), l, r),
        Intrinsic::IntLt(l, r) => IntrinsicForm::Infix(operator("<"), l, r),
        Intrinsic::IntLe(l, r) => IntrinsicForm::Infix(operator("<="), l, r),
        Intrinsic::IntAnd(l, r) => IntrinsicForm::Call {
            path: "Int/and".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::IntOr(l, r) => IntrinsicForm::Call {
            path: "Int/or".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::IntXor(l, r) => IntrinsicForm::Call {
            path: "Int/xor".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::IntMin(l, r) => IntrinsicForm::Call {
            path: "Int/min".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::IntMax(l, r) => IntrinsicForm::Call {
            path: "Int/max".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::IntShl(l, r) => IntrinsicForm::Call {
            path: "Int/shl".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::IntShr(l, r) => IntrinsicForm::Call {
            path: "Int/shr".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltType => IntrinsicForm::Named("Flt"),
        Intrinsic::Flt(flt) => IntrinsicForm::Flt(flt),
        Intrinsic::FltAdd(Rounding::TiesToEven, l, r) => IntrinsicForm::Infix(operator("+"), l, r),
        Intrinsic::FltSub(Rounding::TiesToEven, l, r) => IntrinsicForm::Infix(operator("-"), l, r),
        Intrinsic::FltMul(Rounding::TiesToEven, l, r) => IntrinsicForm::Infix(operator("*"), l, r),
        Intrinsic::FltDiv(Rounding::TiesToEven, l, r) => IntrinsicForm::Infix(operator("/"), l, r),
        Intrinsic::FltAdd(rounding, l, r) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "add"),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltSub(rounding, l, r) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "sub"),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltMul(rounding, l, r) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "mul"),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltDiv(rounding, l, r) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "div"),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltFma(rounding, a, b, c) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "fma"),
            implicits: vec![],
            explicits: vec![a, b, c],
        },
        Intrinsic::FltRem(l, r) => IntrinsicForm::Infix(operator("%"), l, r),
        Intrinsic::FltEql(l, r) => IntrinsicForm::Infix(operator("=="), l, r),
        Intrinsic::FltNeq(l, r) => IntrinsicForm::Infix(operator("!="), l, r),
        Intrinsic::FltLt(l, r) => IntrinsicForm::Infix(operator("<"), l, r),
        Intrinsic::FltLe(l, r) => IntrinsicForm::Infix(operator("<="), l, r),
        Intrinsic::FltMin(l, r) => IntrinsicForm::Call {
            path: "Flt/min".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltMax(l, r) => IntrinsicForm::Call {
            path: "Flt/max".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltCopysign(l, r) => IntrinsicForm::Call {
            path: "Flt/copysign".into(),
            implicits: vec![],
            explicits: vec![l, r],
        },
        Intrinsic::FltNeg(i) => IntrinsicForm::Call {
            path: "Flt/neg".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltAbs(i) => IntrinsicForm::Call {
            path: "Flt/abs".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltSqrt(rounding, i) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "sqrt"),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltRoundIntegral(rounding, i) => IntrinsicForm::Call {
            path: format!("Flt/{}", rounding.integral_label()),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltToLeBytes(i) => IntrinsicForm::Call {
            path: "Flt/to_le_bytes".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltOfLeBytes { bytes: i, .. } => IntrinsicForm::Call {
            path: "Flt/of_le_bytes".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::NatToInt(i) => IntrinsicForm::Call {
            path: "Nat/to_int".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::IntAbs(i) => IntrinsicForm::Call {
            path: "Int/abs".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::IntSign(i) => IntrinsicForm::Call {
            path: "Int/sign".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::NatToFlt(Rounding::TiesToEven, i) => IntrinsicForm::Call {
            path: "Nat/to_flt".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::NatToFlt(rounding, i) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "of_nat"),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::IntToNat { int: i, .. } => IntrinsicForm::Call {
            path: "Int/to_nat".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::IntToFlt(Rounding::TiesToEven, i) => IntrinsicForm::Call {
            path: "Int/to_flt".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::IntToFlt(rounding, i) => IntrinsicForm::Call {
            path: flt_rounded(rounding, "of_int"),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltToNat { flt: i, .. } => IntrinsicForm::Call {
            path: "Flt/to_nat".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltToInt { flt: i, .. } => IntrinsicForm::Call {
            path: "Flt/to_int".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltMantissa { flt: i, .. } => IntrinsicForm::Call {
            path: "Flt/mantissa".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::FltExponent { flt: i, .. } => IntrinsicForm::Call {
            path: "Flt/exponent".into(),
            implicits: vec![],
            explicits: vec![i],
        },
        Intrinsic::BytesType => IntrinsicForm::Named("Bytes"),
        Intrinsic::Bytes(bytes) => IntrinsicForm::Bytes(packed_atoms(&bytes)),
        Intrinsic::BytesLen(b) => IntrinsicForm::Call {
            path: "Bytes/len".into(),
            implicits: vec![],
            explicits: vec![b],
        },
        Intrinsic::BytesEql(l, r) => IntrinsicForm::Infix(operator("=="), l, r),
        Intrinsic::BytesGet {
            bytes: b,
            index: i,
            in_range: _,
        } => IntrinsicForm::Call {
            path: "Bytes/get".into(),
            implicits: vec![],
            explicits: vec![b, i],
        },
        Intrinsic::BytesSlice {
            bytes,
            start,
            length,
            within: _,
        } => IntrinsicForm::Call {
            path: "Bytes/slice".into(),
            implicits: vec![],
            explicits: vec![bytes, start, length],
        },
        // An append has no named form in the surface: `x[..acc, b]` is how a program writes one, and so how a report does.
        Intrinsic::BytesAppend { bytes, element } => {
            let mut entries = Vec::new();
            packed_entries(vec![bytes], grouped, &mut entries);
            entries.push(LiteralEntry::Item(element));
            IntrinsicForm::Bytes(entries)
        }
        Intrinsic::BytesConcat { operands } => {
            let mut entries = Vec::new();
            packed_entries(operands, grouped, &mut entries);
            IntrinsicForm::Bytes(entries)
        }
        // The bound is dropped like every other proof operand — it is not what the program wrote.
        Intrinsic::BytesReplicate { count, atom } => IntrinsicForm::Call {
            path: "Bytes/replicate".into(),
            implicits: vec![],
            explicits: vec![count, atom],
        },
        Intrinsic::BytesAnd {
            left,
            right,
            same_length: _,
        } => IntrinsicForm::Call {
            path: "Bytes/and".into(),
            implicits: vec![],
            explicits: vec![left, right],
        },
        Intrinsic::BytesOr {
            left,
            right,
            same_length: _,
        } => IntrinsicForm::Call {
            path: "Bytes/or".into(),
            implicits: vec![],
            explicits: vec![left, right],
        },
        Intrinsic::BytesXor {
            left,
            right,
            same_length: _,
        } => IntrinsicForm::Call {
            path: "Bytes/xor".into(),
            implicits: vec![],
            explicits: vec![left, right],
        },
        // A parameterized type former is applied as its `/sys` global is, `List(Nat)` and never `List Nat`: the same type reaches a report as the intrinsic node and as that global applied, and a reader shown the two spelled apart is told two types where there is one.
        Intrinsic::ListType(elem) => IntrinsicForm::Call {
            path: "List".into(),
            implicits: vec![],
            explicits: vec![elem],
        },
        Intrinsic::List {
            element: _,
            items: elems,
        } => IntrinsicForm::List(elems.into_iter().map(LiteralEntry::Item).collect()),
        Intrinsic::ListLen { element: ty, list } => IntrinsicForm::Call {
            path: "List/len".into(),
            implicits: vec![ty],
            explicits: vec![list],
        },
        Intrinsic::ListGet {
            element: ty,
            list,
            index,
            in_range: _,
        } => IntrinsicForm::Call {
            path: "List/get".into(),
            implicits: vec![ty],
            explicits: vec![list, index],
        },
        Intrinsic::ListSlice {
            element: ty,
            list,
            start,
            length,
            within: _,
        } => IntrinsicForm::Call {
            path: "List/slice".into(),
            implicits: vec![ty],
            explicits: vec![list, start, length],
        },
        Intrinsic::ListAppend {
            element: _,
            list,
            item,
        } => {
            let mut entries = Vec::new();
            list_entries(vec![list], grouped, &mut entries);
            entries.push(LiteralEntry::Item(item));
            IntrinsicForm::List(entries)
        }
        Intrinsic::ListConcat {
            element: _,
            operands,
        } => {
            let mut entries = Vec::new();
            list_entries(operands, grouped, &mut entries);
            IntrinsicForm::List(entries)
        }
        Intrinsic::ListMap {
            from: a,
            to: b,
            list,
            function: f,
        } => IntrinsicForm::Call {
            path: "List/map".into(),
            implicits: vec![a, b],
            explicits: vec![list, f],
        },
        Intrinsic::ListFold {
            element,
            result,
            list,
            init,
            function,
        } => IntrinsicForm::Call {
            path: "List/fold".into(),
            implicits: vec![element, result],
            explicits: vec![list, init, function],
        },
        Intrinsic::HandleType => IntrinsicForm::Named("Handle"),
        // The three `/sys/Handle` constants are the only handles a term ever holds: every other handle is minted by the host at run time, behind an `Io` no reduction enters. The last arm names a token no source can spell, and spells the token rather than abort the diagnostic it is inside.
        Intrinsic::Handle(StdioCodes::STDIN) => IntrinsicForm::Named("Handle/stdin"),
        Intrinsic::Handle(StdioCodes::STDOUT) => IntrinsicForm::Named("Handle/stdout"),
        Intrinsic::Handle(StdioCodes::STDERR) => IntrinsicForm::Named("Handle/stderr"),
        Intrinsic::Handle(token) => IntrinsicForm::Handle(token),
        Intrinsic::CellType(elem) => IntrinsicForm::Call {
            path: "Cell".into(),
            implicits: vec![],
            explicits: vec![elem],
        },
        Intrinsic::ChannelType(elem) => IntrinsicForm::Call {
            path: "Channel".into(),
            implicits: vec![],
            explicits: vec![elem],
        },
        Intrinsic::Cell { element } => IntrinsicForm::Call {
            path: "Cell/new".into(),
            implicits: vec![element],
            explicits: vec![],
        },
        Intrinsic::CellFill {
            element,
            cell,
            value,
        } => IntrinsicForm::Call {
            path: "Cell/fill".into(),
            implicits: vec![element],
            explicits: vec![cell, value],
        },
        Intrinsic::CellPoll { element, cell, .. } => IntrinsicForm::Call {
            path: "Cell/poll".into(),
            implicits: vec![element],
            explicits: vec![cell],
        },
        Intrinsic::Channel {
            element,
            capacity,
            positive,
        } => IntrinsicForm::Call {
            path: "Channel/new".into(),
            implicits: vec![element],
            explicits: vec![capacity, positive],
        },
        Intrinsic::ChannelPush {
            element,
            channel,
            value,
        } => IntrinsicForm::Call {
            path: "Channel/push".into(),
            implicits: vec![element],
            explicits: vec![channel, value],
        },
        Intrinsic::ChannelTake {
            element, channel, ..
        } => IntrinsicForm::Call {
            path: "Channel/take".into(),
            implicits: vec![element],
            explicits: vec![channel],
        },
        Intrinsic::ChannelClose { element, channel } => IntrinsicForm::Call {
            path: "Channel/close".into(),
            implicits: vec![element],
            explicits: vec![channel],
        },
        Intrinsic::ChannelClosed { element, channel } => IntrinsicForm::Call {
            path: "Channel/closed".into(),
            implicits: vec![element],
            explicits: vec![channel],
        },
        Intrinsic::ChannelCount { element, channel } => IntrinsicForm::Call {
            path: "Channel/count".into(),
            implicits: vec![element],
            explicits: vec![channel],
        },
        Intrinsic::ChannelCapacity { element, channel } => IntrinsicForm::Call {
            path: "Channel/capacity".into(),
            implicits: vec![element],
            explicits: vec![channel],
        },
        Intrinsic::IoType(result) => IntrinsicForm::Call {
            path: "Io".into(),
            implicits: vec![],
            explicits: vec![result],
        },
        Intrinsic::IoPure {
            result: type_,
            value,
        } => IntrinsicForm::Call {
            path: "Io/pure".into(),
            implicits: vec![type_],
            explicits: vec![value],
        },
        Intrinsic::IoBind {
            from: a,
            to: b,
            action,
            continuation: f,
        } => IntrinsicForm::Call {
            path: "Io/bind".into(),
            implicits: vec![a, b],
            explicits: vec![action, f],
        },
        Intrinsic::IoSuspend {
            result,
            on,
            filled,
            cancel,
            rest,
        } => IntrinsicForm::Call {
            path: "Io/suspend".into(),
            implicits: vec![result],
            explicits: vec![on, filled, cancel, rest],
        },
        Intrinsic::IoAdvance {
            from,
            to,
            action,
            done,
            suspended,
        } => IntrinsicForm::Call {
            path: "Io/advance".into(),
            implicits: vec![from, to],
            explicits: vec![action, done, suspended],
        },
    }
}

fn print_core(term: Term, spelling: &Rc<Spelling>) -> Printer {
    print_minting(term, spelling, &Minting::default())
}

/// [`print_core`] under `mint`: a fresh one for a render, a recording one for [`build_rename`]'s dry run — which is allowed what the render is, so the two elide at the same subterms and open the same binders in the same order.
fn print_minting(term: Term, spelling: &Rc<Spelling>, mint: &Minting) -> Printer {
    // The translation is tried on a mint of its own, taken up where it prints the term: one it declines is printed here as it would have been untranslated.
    if spelling.translated {
        let attempt = mint.attempt(&term);
        if let Some(printed) = from_core(&term, spelling, &attempt).map(print_term) {
            mint.adopt(attempt);
            return printed;
        }
        spelling.declined.set(true);
    }

    mint.allow(&term);
    print_shown(
        core_written(term, Frame { spelling, mint }),
        shown_options(spelling),
    )
}

/// What a render under `spelling` tells the printer of a shown term: a decorator above its operand where the two do not fit their line where Core text is written as a text of its own, and beside it where a sentence quotes one.
fn shown_options(spelling: &Spelling) -> ShownOptions {
    ShownOptions {
        decorators_above: spelling.writes_as_text(),
    }
}

/// `term` as the shown tree a render under `spelling` builds, on a mint of its own.
fn written_term(term: Term, spelling: &Rc<Spelling>) -> Tree {
    let mint = Minting::default();
    mint.allow(&term);

    core_written(
        term,
        Frame {
            spelling,
            mint: &mint,
        },
    )
}

/// A value of Core a report or a dump writes, under a spelling or faithfully.
pub trait Spellable: Sized {
    /// This value paired with the [`Spelling`] it renders under — the parameter `Display::fmt` cannot take. See the axes documented above.
    fn spelled<'a>(&'a self, spelling: &Rc<Spelling>) -> Spelled<'a, Self> {
        Spelled::new(self, spelling)
    }

    /// The faithful rendering: core's own names, every universe shown.
    fn faithful(&self) -> Spelled<'_, Self> {
        Spelled::new(self, &Rc::new(Spelling::default()))
    }
}

impl Spellable for Term {}

impl Spellable for Level {}

impl Spellable for WitnessKey {}

impl Spellable for UniverseError {}

/// A module's dump is Core text proper.
impl Spellable for Definitions {
    fn faithful(&self) -> Spelled<'_, Self> {
        Spelled::new(self, &Rc::new(Spelling::core_text()))
    }
}

/// A program's dump is spelled as its module's is.
impl Spellable for Program {
    fn faithful(&self) -> Spelled<'_, Self> {
        Spelled::new(self, &Rc::new(Spelling::core_text()))
    }
}

impl fmt::Display for Spelled<'_, Term> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = self.spelling().with_words(self.value());
        let printer = print_core(self.value().clone(), &spelling);
        match self.width() {
            // The width is a target, not a guarantee: content with no break point still overruns.
            Some(width) => run_printer_within(printer, formatter, 4, width),
            None => run_printer(printer, formatter, 4),
        }
    }
}

impl fmt::Display for Spelled<'_, Level> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&level_spelled(self.value(), self.spelling()))
    }
}

/// A key as a reader sees it: each nominal head spelled as the report's reader reaches it. A key is core's identity for a witness — the thing the registry is indexed by — so its own `Display` is faithful, and only a report has a reader to spell it for.
impl fmt::Display for Spelled<'_, WitnessKey> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let head = |head: &HeadKey| match head {
            HeadKey::Nominal(name) => self.spelling().symbol(name),
            head => head.to_string(),
        };
        match self.value().heads() {
            [single] => formatter.write_str(&head(single)),
            heads => {
                let heads = heads.iter().map(head).collect::<Vec<_>>();

                write!(formatter, "({})", heads.join(", "))
            }
        }
    }
}

impl fmt::Display for Spelled<'_, UniverseError> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value() {
            UniverseError::OffsetOverflow => formatter.write_str("universe level offset overflow"),
            UniverseError::EscapingLevel => {
                formatter.write_str("a universe metavariable or parameter escaped its scope")
            }
            UniverseError::InstanceArity { expected, got } => {
                write!(
                    formatter,
                    "universe instance has {got} arguments but its scheme expects {expected}"
                )
            }
            UniverseError::QualityArity { expected, got } => {
                write!(
                    formatter,
                    "universe instance states {got} sorts but its scheme leaves {expected} to its caller"
                )
            }
            UniverseError::UnknownMeta(meta) => write!(formatter, "unknown universe meta {meta}"),
            UniverseError::MismatchedRecursiveContexts => formatter
                .write_str("members of one recursive group carry different universe contexts"),
            UniverseError::SearchExhausted {
                constraints,
                branches,
                widths,
            } => write!(
                formatter,
                "universe consistency search exceeded its budget: \
                 {constraints} constraints, {branches} disjunctive clauses, widths {widths:?}"
            ),
            UniverseError::Inconsistency { lower, upper, path } => {
                write!(
                    formatter,
                    "this {} would need to be strictly below itself ({} ≤ {})",
                    inconsistent_sort(path),
                    level_spelled(lower, self.spelling()),
                    level_spelled(upper, self.spelling()),
                )?;
                for origin in path {
                    write!(formatter, "\n  required by {:?}", origin.kind)?;
                    if let Some(instantiated) = &origin.instantiated {
                        write!(formatter, " of {}", instantiated.faithful())?;
                    }
                    if let Some(declaration) = &origin.declaration {
                        write!(formatter, " in {declaration}")?;
                    }
                    if let Some(binder) = &origin.binder {
                        write!(formatter, " at binder {binder}")?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Spelled<'_, Definitions> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let items = print_items(self.value(), formatter, self.spelling())?;
        if items > 0 {
            writeln!(formatter)?;
        }

        Ok(())
    }
}

/// The module's items, then the entry, with the type it states beneath it.
impl fmt::Display for Spelled<'_, Program> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (program, spelling) = (self.value(), self.spelling());
        if print_items(&program.definitions, formatter, spelling)? > 0 {
            formatter.write_str("\n\n")?;
        }
        let written = |term: &Term| written_term(term.clone(), &spelling.with_words(term));
        let entry = CoreEntry {
            body: written(&program.entrypoint.body),
            type_: program.entrypoint.type_.as_ref().map(written),
        };

        write_printer(
            print_shown_entry(entry, shown_options(spelling)),
            formatter,
            spelling,
        )
    }
}

/// `printer` run against `formatter` at the width `spelling` lays its groups out against, or at none.
fn write_printer(
    printer: Printer,
    formatter: &mut fmt::Formatter<'_>,
    spelling: &Spelling,
) -> fmt::Result {
    match spelling.width() {
        Some(width) => run_printer_within(printer, formatter, 4, width),
        None => run_printer(printer, formatter, 4),
    }
}

/// A crate below this one writes Core through a spelling it is handed: a term as [`Spelled`] writes it, a level as a dump does.
impl Notation for Spelling {
    fn term(&self, term: &Term) -> String {
        term.spelled(&Rc::new(self.clone())).to_string()
    }

    fn level(&self, level: &Level) -> String {
        level_spelled(level, self)
    }
}

/// The items, one per declaration, each as Core text declares it and a blank line apart, answering how many it wrote — built and printed one item at a time by *iterating* the flat items (never re-folding into a nested term), so `wonder --stage core` stays O(N) and never builds the nested term a deep module overflows on. An item a declaration implies is written nowhere, where it is the one the declaration makes and stands where a reader puts it ([`Implied`]).
fn print_items(
    definitions: &Definitions,
    formatter: &mut fmt::Formatter<'_>,
    spelling: &Rc<Spelling>,
) -> Result<usize, fmt::Error> {
    let mut written = 0;
    let mut write = |formatter: &mut fmt::Formatter<'_>, item: CoreItem<Shown>| {
        if written > 0 {
            formatter.write_str("\n\n")?;
        }
        written += 1;

        write_printer(
            print_shown_item(item, shown_options(spelling)),
            formatter,
            spelling,
        )
    };
    let implied = Implied::of(definitions);
    for (place, item) in definitions.items.places().zip(definitions.items.iter()) {
        if implied.omitted.contains(&place) {
            continue;
        }
        match item {
            Item::Let(definition) => {
                // A foreign row is stated by its form, which implies the definition that calls it: one written beside it where it is not that one.
                if let Some(function) = foreign_row(definition) {
                    let made = spelling.form_scope.as_ref().map(|scope| {
                        Definition::foreign(
                            definition.name,
                            definition.island,
                            Arc::clone(&function),
                            &scope.syntax,
                        )
                    });
                    let implied = made.is_some_and(|made| **definition == made);
                    write(
                        formatter,
                        CoreItem::Foreign(foreign_declared(
                            definition, &function, implied, spelling,
                        )),
                    )?;
                    if implied {
                        continue;
                    }
                }
                if let Some(test) = test_declared(definition, spelling) {
                    write(formatter, CoreItem::Test(test))?;
                    continue;
                }
                let member = Member {
                    definition,
                    proof: definition.proof,
                    value_sort: definition.value_sort.as_ref(),
                    universe_context: Some(&definition.universe_context),
                    group: None,
                };
                write(
                    formatter,
                    CoreItem::Group(vec![member.written(&implied, spelling)]),
                )?;
            }
            Item::Rec(rec) => {
                let mut context = Some(&rec.universe_context);
                let definitions = rec.definitions();
                let members = definitions
                    .iter()
                    .zip(&rec.definitions)
                    .enumerate()
                    .map(|(index, (definition, held))| {
                        Member {
                            definition,
                            proof: held.proof,
                            value_sort: None,
                            universe_context: context.take(),
                            group: Some((index, rec.definitions.len())),
                        }
                        .written(&implied, spelling)
                    })
                    .collect();
                write(formatter, CoreItem::Group(members))?;
            }
        }
    }

    Ok(written)
}

/// The row `definition` is the declaration of, where its value beneath its own binders is a call of that row.
fn foreign_row(definition: &Definition) -> Option<Arc<ForeignFunction>> {
    let Subterm::Func(Func { telescope }) = &*definition.body else {
        return None;
    };
    match &**telescope.terminal() {
        Subterm::Foreign(foreign) if foreign.declared == definition.name => {
            Some(Arc::clone(&foreign.function))
        }
        _ => None,
    }
}

/// A foreign row in its form, `foreign /f(Bytes) -> Bytes`: the row's name where it is not its declaration's path, and where the definition it implies is the one the module holds, that definition's island where it is not its name's parent.
fn foreign_declared(
    definition: &Definition,
    function: &ForeignFunction,
    implied: bool,
    spelling: &Rc<Spelling>,
) -> TopForeign<Shown> {
    let mut fields = Vec::<Decoration>::new();
    if definition.name.qualifier().map(Qualifier::join).as_deref() != Some(function.name.as_str()) {
        fields.push(("name", value(string(function.name.as_str()))));
    }
    if implied {
        fields.extend(island_field(definition, spelling));
    }

    TopForeign {
        doc: None,
        core: CoreHead {
            fields: decorated_fields(spelling, fields),
            name: definition.name,
        },
        vis_pub: false,
        label: declared_label(&definition.name),
        signature: function.signature().clone(),
    }
}

/// A test in its form, `test /t = value`, where it is the definition the form makes against the scope the spelling holds: `None` where the spelling holds none, or the test is another.
fn test_declared(definition: &Definition, spelling: &Rc<Spelling>) -> Option<TopTest<Shown>> {
    if definition.kind != DefinitionKind::Test {
        return None;
    }
    let scope = spelling.form_scope.as_ref()?;
    let family = Global::Authored(scope.syntax.test.test_type.qualifier());
    let declaration = scope.entries.get(&family)?.inductive()?;
    let Subterm::Func(Func { telescope }) = &*definition.body else {
        return None;
    };
    if !telescope.is_empty() {
        return None;
    }
    let value = telescope.terminal().clone();
    let made = Definition::test(
        definition.name,
        definition.island,
        value.clone(),
        family,
        declaration,
    );
    if made != *definition {
        return None;
    }

    Some(TopTest {
        core: CoreHead {
            fields: decorated_fields(
                spelling,
                island_field(definition, spelling).into_iter().collect(),
            ),
            name: definition.name,
        },
        label: declared_label(&definition.name),
        body: written_term(value.clone(), &spelling.with_words(&value)),
    })
}

/// A head's decorator: its fields in Core text proper, and none in a reader's spelling, which writes no decorator.
fn decorated_fields(spelling: &Spelling, fields: Vec<Decoration>) -> Vec<DecoratorField<Shown>> {
    match spelling.writes_core_text() {
        true => decoration_fields(fields),
        false => Vec::new(),
    }
}

/// The label a declaration of `name` declares: its path's last segment, or a witness's identity.
fn declared_label(name: &Global) -> crate::Label {
    crate::Label::from(match name {
        Global::Authored(path) => path.last().to_string(),
        Global::Witness(id) => id.to_string(),
    })
}

/// A definition's island, where it is not its name's parent.
fn island_field(definition: &Definition, spelling: &Spelling) -> Option<Decoration> {
    let parent = definition.name.qualifier().map(Qualifier::without_last);
    (parent.as_ref() != Some(&definition.island))
        .then(|| ("island", module_written(&definition.island, spelling)))
}

/// What the declarations of a module imply of its items: the items left to the declaration that implies them, by their places — a version's spaced apart, so no count of the items names one — and the names a declaration implies that no item of the module declares, by the declaration.
///
/// **An implied item is written nowhere where it is the one its declaration makes and stands where a reader puts it.** A family's constructors are made by [`Definition::constructors`]; a constructor that differs from what it makes is written out, and so is every one a declaration implies where the reader's placement ([`Definitions::placed_among`]) would not stand them where the module does.
struct Implied {
    omitted: BTreeSet<usize>,
    absent: HashMap<Global, Vec<Global>>,
}

impl Implied {
    fn of(definitions: &Definitions) -> Self {
        let items = &definitions.items;
        let place = items
            .places()
            .zip(items.iter())
            .flat_map(|(at, item)| {
                item.declared_names()
                    .into_iter()
                    .map(move |name| (*name, at))
            })
            .collect::<HashMap<_, _>>();
        let mut candidates = BTreeSet::new();
        let mut absent = HashMap::<Global, Vec<Global>>::new();
        for definition in items.iter().flat_map(Item::definitions) {
            let (Some(declaration), Global::Authored(owner)) = (
                definition
                    .kind
                    .nominal()
                    .and_then(|nominal| nominal.inductive()),
                definition.name,
            ) else {
                continue;
            };
            for constructor in Definition::constructors(owner, definition.island, declaration) {
                match place.get(&constructor.name) {
                    None => absent
                        .entry(definition.name)
                        .or_default()
                        .push(constructor.name),
                    Some(&at) => {
                        if let Item::Let(stored) = &items[at]
                            && **stored == constructor
                        {
                            candidates.insert(at);
                        }
                    }
                }
            }
        }

        // Where a reader puts them is where they stand, or each is written out.
        let written = items
            .places()
            .zip(items.iter())
            .filter(|(at, _)| !candidates.contains(at))
            .map(|(_, item)| item.clone())
            .collect::<Vec<_>>();
        let implied = candidates
            .iter()
            .map(|at| items[*at].clone())
            .collect::<Vec<_>>();
        let first = |item: &Item| item.declared_names().first().map(|name| **name);
        let placed = Definitions::placed_among(written, implied);
        let stands = placed.iter().map(first).eq(items.iter().map(first));

        Self {
            omitted: match stands {
                true => candidates,
                false => BTreeSet::new(),
            },
            absent,
        }
    }
}

/// One definition of an item, as Core text declares it: with what its item holds beside it, and its place in a group where it is a member of one, with the group's size.
struct Member<'a> {
    definition: &'a Definition,
    proof: bool,
    value_sort: Option<&'a Term>,
    /// The scheme it states: its own, or its group's where it is a group's first member, and none for a later one.
    universe_context: Option<&'a UniverseContext>,
    group: Option<(usize, usize)>,
}

impl Member<'_> {
    /// This member under its decorator: a nominal declaration in its form, any other definition as a `let`.
    fn written(self, implied: &Implied, spelling: &Rc<Spelling>) -> CoreMember<Shown> {
        match self.definition.kind.nominal() {
            Some(nominal) => {
                let nominal = nominal.clone();
                let terms = declaration_terms(self.definition, &nominal);
                let words =
                    spelling.with_words_of(&terms, |frame| self.declared(&nominal, implied, frame));
                let mint = Minting::default();
                mint.allow_all(&terms);

                self.declared(
                    &nominal,
                    implied,
                    Frame {
                        spelling: &words,
                        mint: &mint,
                    },
                )
            }
            None => {
                let definition = self.definition;
                let mut fields = definition_fields(
                    &definition.name,
                    &definition.kind,
                    &definition.island,
                    self.proof,
                    self.value_sort,
                    self.universe_context.and_then(universe_context_written),
                    spelling,
                );
                // A group of one member is written as a `let` is, so it says it is a group.
                if let Some((_, 1)) = self.group {
                    fields.insert(0, ("node", word_value("Rec")));
                }
                let written = |term: &Term| written_term(term.clone(), &spelling.with_words(term));

                CoreMember::Let(TopLet {
                    doc: None,
                    core: CoreHead {
                        fields: decorated_fields(spelling, fields),
                        name: definition.name,
                    },
                    vis_pub: false,
                    label: declared_label(&definition.name),
                    signature: LetSignature::Name {
                        type_: Some(written(&definition.type_)),
                        body: written(&definition.body),
                    },
                })
            }
        }
    }

    /// A nominal declaration in its form under `frame`, its decorator stating what differs from what the form builds: the node it is where its item is not the one its keyword makes, its former's parts and its entry's beside the ones the form spells, and the items it implies that its module lacks.
    fn declared(&self, nominal: &Nominal, implied: &Implied, frame: Frame) -> CoreMember<Shown> {
        let definition = self.definition;
        let (member, words) = match nominal {
            Nominal::Inductive(declaration) => (
                CoreMember::Induct(family_declared(&definition.name, declaration, frame)),
                Vec::new(),
            ),
            Nominal::Struct(record) => {
                let written = record_declared(record, frame);
                let words = written.words.clone();
                (
                    CoreMember::Struct(written.structure(&definition.name)),
                    words,
                )
            }
            Nominal::Concept { record, .. } => {
                let written = record_declared(record, frame);
                let words = written.words.clone();
                (
                    CoreMember::Concept(written.concept(&definition.name)),
                    words,
                )
            }
        };

        // A family's item is a group, its own where it is alone; a record's and a concept's is a `let` where it is alone.
        let mut fields = Vec::<Decoration>::new();
        match (self.group, matches!(nominal, Nominal::Inductive(_))) {
            (None, true) => fields.push(("node", word_value("Let"))),
            (Some((_, 1)), false) => fields.push(("node", word_value("Rec"))),
            _ => {}
        }
        fields.extend(definition_fields(
            &definition.name,
            &definition.kind,
            &definition.island,
            self.proof,
            self.value_sort,
            self.universe_context.and_then(universe_context_written),
            frame.spelling,
        ));
        let former = Definition::former(definition.name, definition.island, nominal.clone());
        if definition.type_ != former.type_ {
            fields.push(("type_", value(sub(definition.type_.clone(), frame))));
        }
        if definition.body != former.body {
            fields.push(("body", value(sub(definition.body.clone(), frame))));
        }
        fields.extend(entry_fields(definition, nominal, &words, frame));
        if let Some(absent) = implied.absent.get(&definition.name) {
            fields.push((
                "absent",
                bracketed(
                    absent
                        .iter()
                        .map(|name| word_value(frame.spelling.symbol(name)))
                        .collect(),
                ),
            ));
        }

        let fields = decorated_fields(frame.spelling, fields);
        match member {
            CoreMember::Induct(mut family) => {
                family.core.fields = fields;
                CoreMember::Induct(family)
            }
            CoreMember::Struct(mut record) => {
                record.core.fields = fields;
                CoreMember::Struct(record)
            }
            CoreMember::Concept(mut concept) => {
                concept.core.fields = fields;
                CoreMember::Concept(concept)
            }
            CoreMember::Let(_) => unreachable!("a nominal declaration is written in its form"),
        }
    }
}

/// The terms a nominal declaration's form writes, whose names its binders' words must not read like and which bound what its render may build: its former's, and a family's constructors'.
fn declaration_terms(definition: &Definition, nominal: &Nominal) -> Vec<Term> {
    let mut terms = vec![definition.type_.clone(), definition.body.clone()];
    if let (Nominal::Inductive(declaration), Global::Authored(owner)) = (nominal, definition.name) {
        terms.extend(
            Definition::constructors(owner, definition.island, declaration)
                .into_iter()
                .map(|constructor| constructor.type_),
        );
    }
    if let Some(record) = nominal.record() {
        terms.extend(
            (0..record.fields().len())
                .filter_map(|index| record.fields().entry_type(index).cloned()),
        );
    }

    terms
}

/// What a nominal declaration's entry holds beside what its form spells and its former's: the module its representation belongs to where it is not its name's parent, the polarities and the variances its elaboration filed, and a scheme apart from its former's; and a concept's own parts where they are not read off its record — its scheme, its superclass edges and its fields' labels, which `words` are as the form wrote them.
fn entry_fields(
    definition: &Definition,
    nominal: &Nominal,
    words: &[String],
    frame: Frame,
) -> Vec<Decoration> {
    let (universe_context, module, polarities, variances) = match nominal {
        Nominal::Inductive(declaration) => (
            &declaration.universe_context,
            &declaration.module,
            &declaration.polarities,
            &declaration.variances,
        ),
        Nominal::Struct(record) | Nominal::Concept { record, .. } => (
            &record.universe_context,
            &record.module,
            &record.polarities,
            &record.variances,
        ),
    };
    let mut fields = Vec::<Decoration>::new();
    if Some(*module) != definition.name.qualifier().map(Qualifier::without_last) {
        fields.push(("module", module_written(module, frame.spelling)));
    }
    let listed = |values: Vec<String>| bracketed(values.into_iter().map(word_value).collect());
    if !polarities.is_empty() {
        fields.push((
            "polarities",
            listed(
                polarities
                    .iter()
                    .map(|polarity| format!("{polarity:?}"))
                    .collect(),
            ),
        ));
    }
    if !variances.is_empty() {
        fields.push((
            "variances",
            listed(
                variances
                    .iter()
                    .map(|variance| format!("{variance:?}"))
                    .collect(),
            ),
        ));
    }
    if *universe_context != definition.universe_context {
        let context =
            universe_context_written(universe_context).unwrap_or_else(|| nested(Vec::new()));
        fields.push(("entry", nested(vec![("universe_context", context)])));
    }
    if let Nominal::Concept { concept, record } = nominal {
        let mut own = Vec::<Decoration>::new();
        if concept.universe_context != record.universe_context {
            let context = universe_context_written(&concept.universe_context)
                .unwrap_or_else(|| nested(Vec::new()));
            own.push(("universe_context", context));
        }
        if concept.supers != concept_supers_read_off(definition.name, record) {
            own.push((
                "supers",
                bracketed(
                    concept
                        .supers
                        .iter()
                        .map(|name| word_value(frame.spelling.symbol(name)))
                        .collect(),
                ),
            ));
        }
        if concept.fields != words {
            own.push((
                "fields",
                bracketed(
                    concept
                        .fields
                        .iter()
                        .map(|label| value(string(label.as_str())))
                        .collect(),
                ),
            ));
        }
        if !own.is_empty() {
            fields.push(("concept", nested(own)));
        }
    }

    fields
}

/// A module as a decorator names it: by its path, and the root, which no path names, as the slash every absolute path leads with.
fn module_written(module: &Qualifier, spelling: &Spelling) -> CoreValue<Shown> {
    match module.is_root() {
        true => CoreValue::Root,
        false => word_value(spelling.symbol(&Global::Authored(*module))),
    }
}

/// The concepts a record's superclass edges reach, read off each edge's type, as a concept's are where its fields elaborate.
pub fn concept_supers_read_off(name: Global, record: &StructDecl) -> Vec<Global> {
    let mut parameters = Vec::new();
    let mut cursor = record.arity.cursor();
    while cursor.entry().is_some() {
        let variable = Term::free_var(&Free::local(parameters.len() as u64, None));
        cursor.advance(variable.clone());
        parameters.push(variable);
    }
    let value = Term::free_var(&Free::local(parameters.len() as u64, None));
    let fields = record.fields();

    fields
        .marks()
        .into_iter()
        .enumerate()
        .filter(|(_, mark)| *mark == Plicity::Witness)
        .filter_map(|(index, _)| {
            let field = Field::Struct(name, index);
            match &*fields.field_type_beneath(&parameters, &value, &field)? {
                Subterm::StructType(applied) => Some(applied.name),
                _ => None,
            }
        })
        .collect()
}

/// The fields a declaration's decorator sets: its scheme where it has one, its kind where it is not one a declaration writes, its island where it is not its name's parent, or a constructor's family's module, its proof mark and the sort its value inhabits where it states them. A former's entry is no decorator's: it is the declaration's own form.
fn definition_fields(
    name: &Global,
    kind: &DefinitionKind,
    island: &Qualifier,
    proof: bool,
    value_sort: Option<&Term>,
    universe_context: Option<CoreValue<Shown>>,
    spelling: &Rc<Spelling>,
) -> Vec<Decoration> {
    let mut fields = Vec::<Decoration>::new();
    if let Some(context) = universe_context {
        fields.push(("universe_context", context));
    }
    match kind {
        DefinitionKind::Authored | DefinitionKind::Nominal(_) => {}
        DefinitionKind::Witness => fields.push(("kind", word_value("Witness"))),
        DefinitionKind::Test => fields.push(("kind", word_value("Test"))),
        // A variant that holds fields is written as a structure's literal is.
        DefinitionKind::InductiveConstructor { owner, tag } => fields.push((
            "kind",
            value(literal_of(
                "InductiveConstructor",
                vec![
                    ("owner", word(spelling.symbol(&Global::Authored(*owner)))),
                    ("tag", word(tag.as_string())),
                ],
            )),
        )),
    }
    // A definition's island defaults to its name's parent, and a constructor's, whose name stands below its family's, to its family's.
    let parent = match kind {
        DefinitionKind::InductiveConstructor { owner, .. } => Some(owner.without_last()),
        _ => name.qualifier().map(Qualifier::without_last),
    };
    if parent.as_ref() != Some(island) {
        fields.push(("island", module_written(island, spelling)));
    }
    if proof {
        fields.push((
            "proof",
            value(tree(crate::Subterm::Literal(crate::Literal::Bool(true)))),
        ));
    }
    if let Some(sort) = value_sort {
        fields.push(("value_sort", value(written_term(sort.clone(), spelling))));
    }

    fields
}

#[cfg(test)]
mod form_tests;
#[cfg(test)]
mod spelling_tests;
#[cfg(test)]
mod test_support;
