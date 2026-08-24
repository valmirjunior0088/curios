use {
    super::{
        Apply, Arity, Atom, Bang, Bound, Carrier, Cases, Enter, Field, Free, Func, FuncType,
        Global, InductType, Infix, Intrinsic, Let, Level, Match, Nat, Proj, Rec, Scope, Struct,
        StructType, Subterm, Telescope, Term, Three, Transient, Tuple, TupleType, Two, Var,
        Variant,
    },
    curios_abi::stdio,
    curios_num::Floating,
    curios_print::{Printer, flat, group, indent, line, pure, sep_flat, soft_line},
    curios_utilities::{Grain, PackedBin, Plicity, Qualifier, recurse},
    std::{
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
// axis (a) — local binders: a *rename map* (built by `build_rename` over `display_names`) alpha-renames the whole fragment — free vars *and* binder labels. A source hint is used bare when unique; distinct names sharing a hint, or shadowing a global's displayed rendering — the axis-(b) shortened form where that map shortens it — take minimal `hint2`, `hint3`, … suffixes, so no two binders ever read alike. A hintless (compiler-minted) binder spells `_` — or is elided — at its label site when nothing references it, and borrows the fallback hint `x` when something does: `_` in a reference position would read as a hole and could not co-spell with its binder.
//
// axis (b) — globals: a *shorten map* (built by `build_shorten` over `Module::module_symbols`) replaces each qualified path with its shortest unambiguous `/`-suffix — the name in scope, since Curios has no `use … as` aliasing. Used by error rendering *and* `Module` display.
//
// axis (c) — universe instances: a flag suppressing the `.{…}` an instantiated nominal head carries. The surface language has no spelling for an instance — solved (`Option.{0}`) or unsolved (`Eq.{?u271}`) alike — so a diagnostic that shows one asks the reader to decode elaboration state. This is the display twin of `project_erased_universes`, which the goal-report path applies structurally; errors carry raw terms all the way to the formatter, so they suppress at the printer instead. Diagnostics set it; the `--print` stage dumps deliberately do not, because a dump is read *about* the compiler and its levels are the point.
//
// A `Type`'s own level is suppressed only when it is *metavariable-headed*. The level is that node's whole content, so erasing a concrete one could render two distinct sorts identically — but an unsolved level names nothing a reader can act on, and it is the common case: a diagnostic over a polymorphic head reports `(A: Type.{?u263}) -> Nat` against `((Type.{?u261}) -> Type.{?u262}) -> Nat`, three placeholders competing with the disagreement they surround. Suppressing every level was rejected for the case that cannot be ruled out — two distinct concrete levels rendering as `Type` against `Type` — and a whole-fragment "show it only when it disambiguates" pass was rejected as context-dependent rendering: unlike a binder name, which is inherently relative, a level is an absolute fact about the term.
//
// axis (e) — unsolved metavariables: a flag spelling every metavariable as a bare `?`. An id such as `?2677` is elaboration state — a counter a reader cannot decode and the surface cannot write — and a diagnostic carrying one reads as three placeholders competing with the disagreement they surround: the transcript case is `inferred: Prop, expected: Eq(@?2677, ?2679, ?2680)`, where `Eq(@?, ?, ?)` says what the reader needs, that *some* equality was expected. Two distinct metavariables rendering alike is not the hazard it is for levels: a mismatch is reported between rigid structure, and two unsolved metavariables facing each other unify rather than mismatch, so no diagnostic hinges on telling them apart. Diagnostics set it; the `--print` dumps do not, for axis (c)'s reason.
//
// `Spelling::label` consults the shorten map first (globals), then the rename map (locals); a name in neither renders verbatim.

/// How a term is spelled for a reader. The default spells nothing differently, which is what a bare `Display` uses.
#[derive(Clone, Default)]
pub struct Spelling {
    /// axis (a) — local binders to their source-style names.
    pretty: Option<Rc<HashMap<Free, String>>>,
    /// axis (b) — global qualified names to their shortest in-scope spelling.
    shorten: Option<Rc<HashMap<Global, String>>>,
    /// axis (c) — whether to suppress universe instances and metavariable-headed levels.
    erase_universes: bool,
    /// axis (d) — a nominal declaration's parameter plicities, so an applied family is marked the way a use site would write it.
    nominal_plicities: Option<Rc<BTreeMap<Global, Vec<Plicity>>>>,
    /// axis (e) — whether every metavariable spells as a bare `?`.
    anonymous_metavars: bool,
}

impl Spelling {
    /// Rename local binders to source-style names (axis (a)).
    pub fn with_pretty_names(mut self, rename: Rc<HashMap<Free, String>>) -> Self {
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

    /// Spell every metavariable as a bare `?` (axis (e)).
    pub fn with_anonymous_metavars(mut self) -> Self {
        self.anonymous_metavars = true;
        self
    }

    /// The axis-(b) map this spelling renders globals under, or an empty map when none was set — what a consumer hands [`build_rename`] to derive a narrower axis (a) for one fragment of a render, so the narrower map reserves the same displayed global spellings the wider one did.
    pub fn short_names(&self) -> Rc<HashMap<Global, String>> {
        self.shorten.clone().unwrap_or_default()
    }

    /// Mark a nominal family's implicit parameters (axis (d)), from `build_nominal_plicities`.
    pub fn with_nominal_plicities(mut self, plicities: Rc<BTreeMap<Global, Vec<Plicity>>>) -> Self {
        self.nominal_plicities = Some(plicities);
        self
    }

    /// The display spelling of a global — shortened against the module's other symbols (axis (b)) when that is unambiguous, and rendered in full otherwise. Globals never take axis (a)'s rename: their spelling is a path a programmer wrote, not a minted hint.
    fn symbol(&self, name: &Global) -> String {
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
            .and_then(|map| map.get(name).cloned())
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

/// Every name the printer will emit for `term`: free vars (Γ references) and the stored labels of every binder it reopens. Free vars come from the robust `Bound` traversal; binder labels — which that traversal never surfaces — from `collect_labels`, which descends scope bodies (where nested binders live).
pub fn display_names(term: &Term) -> BTreeSet<Free> {
    let mut names = term.free_vars();
    collect_labels(term, &mut names);
    names
}

/// Collect every binder label in `term` — the names [`Bound`]'s free-variable traversal cannot surface, because a scope's stored labels are bound by definition and the printer reopens them anyway.
///
/// Driven by [`Term::walk`] rather than its own worklist, so child enumeration stays in `Subterm::any_child_term` and a new term former carrying a binder cannot reach the printer while quietly missing this walk. What that would look like is not a crash but two distinct binders rendering under one spelling, which is exactly the thing [`build_rename`] promises cannot happen — so the failure mode argues for the shared fold rather than against it.
///
/// This hook adds only each node's *own* labels; the depth is [`Term::walk`]'s to absorb. `Intrinsic` and `Foreign` interiors are skipped whole: they hold no binders the diagnostics need to name, and any free vars there are already in `free_vars`.
fn collect_labels(term: &Term, out: &mut BTreeSet<Free>) {
    fn scope_names<A: Arity>(out: &mut BTreeSet<Free>, scope: &Scope<A>) {
        if let Some(names) = scope.names() {
            out.extend(names.iter().cloned());
        }
    }

    /// One binder per telescope entry. The entry *types* arrive as ordinary children, so this reads the spine for labels alone — which is what lets one function serve a `Func`/`FuncType` telescope and a `TupleType`'s, whose `Done` carries no term.
    fn telescope_binders<T: Bound>(out: &mut BTreeSet<Free>, mut cur: &Telescope<T>) {
        while let Telescope::Cons(_, rest) = cur {
            if let Some(binder) = rest.binder(0) {
                out.insert(binder.clone());
            }
            cur = rest.body();
        }
    }

    term.walk(
        out,
        |out, term| {
            match &**term {
                Subterm::Intrinsic(_) | Subterm::Foreign(..) => return Enter::Skip(()),
                Subterm::Func(Func { telescope, .. })
                | Subterm::FuncType(FuncType { telescope, .. }) => {
                    telescope_binders(out, telescope)
                }
                Subterm::TupleType(TupleType { telescope }) => telescope_binders(out, telescope),
                Subterm::Let(Let { tail, .. }) => scope_names(out, tail),
                Subterm::Rec(Rec { group, tail }) => {
                    for member in group.iter() {
                        scope_names(out, &member.type_);
                        scope_names(out, &member.body);
                    }
                    scope_names(out, tail);
                }
                Subterm::Match(Match { motive, cases, .. }) => {
                    scope_names(out, motive);

                    match cases {
                        Cases::Induct { cases, .. } => {
                            for (_, arm) in cases {
                                scope_names(out, &arm.body);
                            }
                        }
                        // The unary `Nat` cons arm binds a tail and a hypothesis; `Bin`/`List` bind a peeled generator before those, so the two arities do not share a pattern.
                        Cases::FreeMonoid { carrier } => match carrier {
                            Carrier::Nat { cons_case, .. } => scope_names(out, cons_case),
                            Carrier::Bin { cons_case, .. } | Carrier::List { cons_case, .. } => {
                                scope_names(out, cons_case)
                            }
                        },
                        Cases::Bool { .. } | Cases::Switch { .. } => {}
                    }
                }
                _ => {}
            }

            Enter::Descend
        },
        |_, _, _| (),
    )
}

/// Give every local binder a clean display spelling: its hint — or `x` where it was minted hintless — suffixed `hint2`, `hint3`, … when several distinct identities — binders *or* free vars — would otherwise render alike, or would shadow a global's displayed rendering. The result is unambiguous by construction, so no rendered name is ever silently shared between two binders.
///
/// `shorten` is the axis-(b) map the same render will apply: a global is reserved under the rendering it actually displays, since a full path — never a bare identifier — is unshadowable by construction, while a single-segment shortening is exactly what a binder hint can read like.
///
/// A hintless entry's `x` is consulted only where something references the binder — the label sites spell an unreferenced unnameable binder `_` (or elide it) without the map. Hinted names are assigned first, so a synthesized `x` can never steal the spelling from a binder actually written `x`.
pub fn build_rename(
    names: &BTreeSet<Free>,
    shorten: &HashMap<Global, String>,
) -> HashMap<Free, String> {
    // `names` is sorted, so the assignment below is deterministic.
    let (literal, prettifiable): (Vec<_>, Vec<_>) =
        names.iter().partition(|name| name.as_global().is_some());

    // Globals reserve the spelling they will display under.
    let mut used = literal
        .into_iter()
        .map(
            |name| match name.as_global().and_then(|global| shorten.get(global)) {
                Some(short) => short.clone(),
                None => name.to_string(),
            },
        )
        .collect::<BTreeSet<_>>();

    let (hinted, hintless): (Vec<_>, Vec<_>) = prettifiable
        .into_iter()
        .partition(|name| name.hint().is_some());

    let mut map = HashMap::new();
    for name in hinted.into_iter().chain(hintless) {
        let hint = name.hint().unwrap_or("x");
        let mut candidate = hint.to_string();
        let mut next = 2;
        while used.contains(&candidate) {
            candidate = format!("{hint}{next}");
            next += 1;
        }
        used.insert(candidate.clone());
        map.insert(name.clone(), candidate);
    }
    map
}

/// Map each global to the shortest `/`-suffix of its path that no other global shares — the name it has in scope, since Curios has no `use … as` aliasing, so an in-scope name is always a suffix. Only entries that actually shorten are recorded; an ambiguous (or single-segment) name keeps its full path.
pub fn build_shorten(symbols: &[Global]) -> HashMap<Global, String> {
    // One global can be listed twice (an inductive is both an `induct_decls` registry key and an `items` type-constructor definition); count distinct names, or such a name would look ambiguous with itself and never shorten.
    let symbols = symbols.iter().collect::<BTreeSet<_>>();

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
    for name in &symbols {
        for suffix in suffixes(name) {
            *count.entry(suffix).or_insert(0) += 1;
        }
    }

    let mut map = HashMap::new();
    for name in &symbols {
        let rendered = name.to_string();
        if let Some(shortest) = suffixes(name)
            .into_iter()
            .find(|suffix| count.get(suffix) == Some(&1))
            && shortest.len() < rendered.len()
        {
            map.insert((*name).clone(), shortest);
        }
    }
    map
}

/// A scope's stored binder, or a depth-positional stand-in when it has none — a `constant` scope never had binders written. The stand-in is minted at the de Bruijn level, so one printed term's placeholders stay distinct from each other.
fn binder_or(binder: Option<&Free>, depth: usize) -> Free {
    match binder {
        Some(binder) => binder.clone(),
        None => Free::local(u32::try_from(depth).unwrap_or(u32::MAX), None),
    }
}

fn label_terms(binders: &[Free]) -> Vec<Term> {
    binders.iter().map(Term::free_var).collect()
}

/// The state a recursive print call threads: the render-constant [`Spelling`] beside the binder depth descended so far. Depth exists only to position [`binder_or`] stand-ins, and the opening helpers advance it as they mint, so an arm that opens binders is handed the frame its body prints under instead of recomputing it.
#[derive(Clone, Copy)]
struct Frame<'a> {
    spelling: &'a Rc<Spelling>,
    depth: usize,
}

impl<'a> Frame<'a> {
    /// This frame, `count` binders deeper.
    fn deeper(self, count: usize) -> Self {
        Self {
            depth: self.depth + count,
            ..self
        }
    }

    /// One binder's display label minted at the current depth, the frame advanced past it — the telescope loops mint one label per entry as they walk.
    fn label(&mut self, binder: Option<&Free>) -> Free {
        let label = binder_or(binder, self.depth);
        self.depth += 1;
        label
    }

    /// Every binder of a scope, unnamed ones filled with stand-ins, beside the frame past them.
    fn labels<'b>(self, binders: impl Iterator<Item = Option<&'b Free>>) -> (Vec<Free>, Self) {
        let labels: Vec<Free> = binders
            .enumerate()
            .map(|(index, binder)| binder_or(binder, self.depth + index))
            .collect();
        let past = self.deeper(labels.len());
        (labels, past)
    }

    /// Open a two-binder scope under minted labels, beside the frame its body prints under.
    fn open_two(self, scope: Scope<Two>) -> ((Free, Free), Term, Self) {
        let fst = binder_or(scope.binder(0), self.depth);
        let snd = binder_or(scope.binder(1), self.depth + 1);
        let body = scope.open(&[&Term::free_var(&fst), &Term::free_var(&snd)]);

        ((fst, snd), body, self.deeper(2))
    }

    /// The three-binder counterpart of [`Frame::open_two`].
    fn open_three(self, scope: Scope<Three>) -> ((Free, Free, Free), Term, Self) {
        let fst = binder_or(scope.binder(0), self.depth);
        let snd = binder_or(scope.binder(1), self.depth + 1);
        let thd = binder_or(scope.binder(2), self.depth + 2);
        let body = scope.open(&[
            &Term::free_var(&fst),
            &Term::free_var(&snd),
            &Term::free_var(&thd),
        ]);

        ((fst, snd, thd), body, self.deeper(3))
    }
}

fn print_var(var: Var, spelling: &Rc<Spelling>) -> Printer {
    pure(spelling.label(var.unwrap()))
}

fn print_atom(atom: Atom) -> Printer {
    flat([pure("'"), pure(atom.as_string())])
}

fn print_flt(flt: Floating) -> Printer {
    let mut string = format!("{:+}", flt.to_f32());

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

/// An intrinsic operation as the surface calls it — `Nat/shl(a, b)`, never `Nat.shl a b`. Every operation here is declared by `/sys` under its carrier's module and re-exported by `/std` under the same name, so the path is the one a reader wrote; it is also how the same term prints before reduction unfolds that `/sys` global, which is the agreement [`print_former`] states for type formers. A type argument is marked `@`, exactly as the application of the global marks it. The proof an operation carries — a bound on an index, a nonzero divisor — is not an argument here: the reader never wrote one, the operator or the elaborator inserted it, and it is erased.
fn print_call(
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

/// The constant atoms of a packed literal, spelled as the surface writes them — `0`/`1` for bits, hexadecimal numerals for bytes.
fn bin_atoms(grain: Grain, packed: &PackedBin) -> Vec<Printer> {
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

/// The entries of a list concatenation as the surface spells them: a literal operand contributes its items in place, a nested concatenation its own entries, and anything else a `..` spread. Lowering turns the `[h, ..t]` a reader wrote into a concatenation of the literal `[h]` with `t`, and substitution nests one concatenation inside another; splicing both back is what lets the report quote the program rather than its lowering. Concatenation is associative, so the splice changes no value.
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
            Subterm::Intrinsic(Intrinsic::ListConcat { .. }) => {
                let Subterm::Intrinsic(Intrinsic::ListConcat { operands, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                list_concat_entries(operands, frame, entries);
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
            Subterm::Intrinsic(Intrinsic::BinConcat { grain: g, .. }) if *g == grain => {
                let Subterm::Intrinsic(Intrinsic::BinConcat { operands, .. }) =
                    Term::unwrap_or_clone(operand)
                else {
                    unreachable!()
                };
                bin_concat_entries(grain, operands, frame, entries);
            }
            _ => entries.push(flat([pure(".."), sub(operand, frame)])),
        }
    }
}

/// The surface infix symbol an operator intrinsic prints as, or `None` for an intrinsic with no infix spelling — the bitwise ops, conversions, `min`/`max`, and the `Bool.xor` that `!=` desugars through. Exactly the operators the surface language spells infix ([`InfixOp::symbol`](super::InfixOp::symbol)); the concept-dispatched arithmetic/comparison operators plus the two hardcoded `Bool` short-circuits.
fn infix_symbol(intrinsic: &Intrinsic) -> Option<&'static str> {
    Some(match intrinsic {
        Intrinsic::NatAdd(..) | Intrinsic::IntAdd(..) | Intrinsic::FltAdd(..) => "+",
        Intrinsic::NatSub(..) | Intrinsic::IntSub(..) | Intrinsic::FltSub(..) => "-",
        Intrinsic::NatMul(..) | Intrinsic::IntMul(..) | Intrinsic::FltMul(..) => "*",
        Intrinsic::NatDiv { .. } | Intrinsic::IntDiv { .. } | Intrinsic::FltDiv(..) => "/",
        Intrinsic::NatRem { .. } | Intrinsic::IntRem { .. } | Intrinsic::FltRem(..) => "%",
        Intrinsic::NatEql(..)
        | Intrinsic::IntEql(..)
        | Intrinsic::FltEql(..)
        | Intrinsic::BoolEql(..)
        | Intrinsic::BinEql(..)
        | Intrinsic::HandleEql(..) => "==",
        Intrinsic::NatNeq(..)
        | Intrinsic::IntNeq(..)
        | Intrinsic::FltNeq(..)
        | Intrinsic::BoolNeq(..) => "!=",
        Intrinsic::NatLt(..) | Intrinsic::IntLt(..) | Intrinsic::FltLt(..) => "<",
        Intrinsic::NatGt(..) | Intrinsic::IntGt(..) | Intrinsic::FltGt(..) => ">",
        Intrinsic::NatLe(..) | Intrinsic::IntLe(..) | Intrinsic::FltLe(..) => "<=",
        Intrinsic::NatGe(..) | Intrinsic::IntGe(..) | Intrinsic::FltGe(..) => ">=",
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

/// A recognized type-former eta shape: the former's identity and the argument prefix left after stripping the binder.
enum FormerEta {
    Nominal(Global, Vec<Term>),
    Intrinsic(&'static str),
}

/// Recognize `x => T(…, x)` on the *unopened* telescope: one binder, whose sole occurrence is the final argument of a saturated former body — a nominal type with no indices, or a unary intrinsic carrier. The binder's plicity is deliberately not inspected: the eta-lambdas this contracts are imitation solutions, which copy their plicities from the former's birth type, so the binder already mirrors the declaration. The prefix arguments must be closed under the binder (`reach() == 0`), which is what guarantees the binder occurs nowhere else.
fn former_eta(telescope: &Telescope<Term>, plicities: &[Plicity]) -> Option<FormerEta> {
    if telescope.len() != 1 || plicities.len() != 1 {
        return None;
    }
    let Telescope::Cons(_, rest) = telescope else {
        return None;
    };
    let Telescope::Done(body) = rest.body() else {
        return None;
    };

    let bound_zero =
        |term: &Term| matches!(&**term, Subterm::Var(var) if var.as_bound() == Some(0));
    let closed_prefix = |terms: &[Term]| terms.iter().all(|term| term.reach() == 0);

    match &***body {
        Subterm::InductType(InductType {
            name,
            params,
            indices,
            ..
        }) if indices.is_empty() => {
            let (last, prefix) = params.split_last()?;
            (bound_zero(last) && closed_prefix(prefix))
                .then(|| FormerEta::Nominal(name.clone(), prefix.to_vec()))
        }
        Subterm::StructType(StructType { name, params, .. }) => {
            let (last, prefix) = params.split_last()?;
            (bound_zero(last) && closed_prefix(prefix))
                .then(|| FormerEta::Nominal(name.clone(), prefix.to_vec()))
        }
        Subterm::Intrinsic(Intrinsic::IoType(payload)) => {
            bound_zero(payload).then_some(FormerEta::Intrinsic("Io"))
        }
        Subterm::Intrinsic(Intrinsic::ListType(payload)) => {
            bound_zero(payload).then_some(FormerEta::Intrinsic("List"))
        }
        Subterm::Intrinsic(Intrinsic::CellType(payload)) => {
            bound_zero(payload).then_some(FormerEta::Intrinsic("Cell"))
        }
        _ => None,
    }
}

/// Print a recognized former: the name alone when the binder was its only argument, the prefix application otherwise — routed through a synthetic term so qualification and spelling stay uniform with every other reference.
fn former_doc(former: FormerEta, frame: Frame) -> Printer {
    match former {
        FormerEta::Intrinsic(name) => pure(name),
        FormerEta::Nominal(name, prefix) => {
            let reference = Term::var(Var::free(Free::Global(name)));
            let term = if prefix.is_empty() {
                reference
            } else {
                Term::apply(reference, prefix)
            };
            sub(term, frame)
        }
    }
}

/// An operand of [`print_infix`], wrapped in parentheses when it too prints as an infix operator (a nested operator intrinsic or a residual `Infix` node); self-delimiting operands (variables, literals, applications) print bare.
fn print_operand(term: Term, frame: Frame) -> Printer {
    let parenthesize = match &*term {
        Subterm::Intrinsic(intrinsic) => infix_symbol(intrinsic).is_some(),
        Subterm::Transient(Transient::Infix(_)) => true,
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
        Intrinsic::BoolType => pure("Bool"),
        Intrinsic::Bool(false) => pure("false"),
        Intrinsic::Bool(true) => pure("true"),
        Intrinsic::BoolAnd(l, r) => print_infix("&&", l, r, frame),
        Intrinsic::BoolOr(l, r) => print_infix("||", l, r, frame),
        Intrinsic::BoolXor(l, r) => print_call("Bool/xor", vec![], vec![l, r], frame),
        Intrinsic::BoolEql(l, r) => print_infix("==", l, r, frame),
        Intrinsic::BoolNeq(l, r) => print_infix("!=", l, r, frame),
        Intrinsic::NatType => pure("Nat"),
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
        Intrinsic::HandleEql(l, r) => print_infix("==", l, r, frame),
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
        Intrinsic::NatGt(l, r) => print_infix(">", l, r, frame),
        Intrinsic::NatLe(l, r) => print_infix("<=", l, r, frame),
        Intrinsic::NatGe(l, r) => print_infix(">=", l, r, frame),
        Intrinsic::NatAnd(l, r) => print_call("Nat/and", vec![], vec![l, r], frame),
        Intrinsic::NatOr(l, r) => print_call("Nat/or", vec![], vec![l, r], frame),
        Intrinsic::NatXor(l, r) => print_call("Nat/xor", vec![], vec![l, r], frame),
        Intrinsic::NatShl(l, r) => print_call("Nat/shl", vec![], vec![l, r], frame),
        Intrinsic::NatShr(l, r) => print_call("Nat/shr", vec![], vec![l, r], frame),
        Intrinsic::ByteType => pure("Byte"),
        Intrinsic::Byte(value) => pure(format!("0x{value:02X}")),
        Intrinsic::ByteToNat(i) => print_call("Byte/to_nat", vec![], vec![i], frame),
        Intrinsic::NatToByte(i) => print_call("Nat/to_byte", vec![], vec![i], frame),
        Intrinsic::ByteEql(l, r) => print_call("Byte/eql", vec![], vec![l, r], frame),
        Intrinsic::ByteLt(l, r) => print_call("Byte/lt", vec![], vec![l, r], frame),
        Intrinsic::ByteLe(l, r) => print_call("Byte/le", vec![], vec![l, r], frame),
        Intrinsic::ByteGt(l, r) => print_call("Byte/gt", vec![], vec![l, r], frame),
        Intrinsic::ByteGe(l, r) => print_call("Byte/ge", vec![], vec![l, r], frame),
        Intrinsic::IntType => pure("Int"),
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
        Intrinsic::IntGt(l, r) => print_infix(">", l, r, frame),
        Intrinsic::IntLe(l, r) => print_infix("<=", l, r, frame),
        Intrinsic::IntGe(l, r) => print_infix(">=", l, r, frame),
        Intrinsic::IntAnd(l, r) => print_call("Int/and", vec![], vec![l, r], frame),
        Intrinsic::IntOr(l, r) => print_call("Int/or", vec![], vec![l, r], frame),
        Intrinsic::IntXor(l, r) => print_call("Int/xor", vec![], vec![l, r], frame),
        Intrinsic::IntShl(l, r) => print_call("Int/shl", vec![], vec![l, r], frame),
        Intrinsic::IntShr(l, r) => print_call("Int/shr", vec![], vec![l, r], frame),
        Intrinsic::FltType => pure("Flt"),
        Intrinsic::Flt(flt) => print_flt(flt),
        Intrinsic::FltAdd(l, r) => print_infix("+", l, r, frame),
        Intrinsic::FltSub(l, r) => print_infix("-", l, r, frame),
        Intrinsic::FltMul(l, r) => print_infix("*", l, r, frame),
        Intrinsic::FltDiv(l, r) => print_infix("/", l, r, frame),
        Intrinsic::FltRem(l, r) => print_infix("%", l, r, frame),
        Intrinsic::FltEql(l, r) => print_infix("==", l, r, frame),
        Intrinsic::FltNeq(l, r) => print_infix("!=", l, r, frame),
        Intrinsic::FltLt(l, r) => print_infix("<", l, r, frame),
        Intrinsic::FltGt(l, r) => print_infix(">", l, r, frame),
        Intrinsic::FltLe(l, r) => print_infix("<=", l, r, frame),
        Intrinsic::FltGe(l, r) => print_infix(">=", l, r, frame),
        Intrinsic::FltMin(l, r) => print_call("Flt/min", vec![], vec![l, r], frame),
        Intrinsic::FltMax(l, r) => print_call("Flt/max", vec![], vec![l, r], frame),
        Intrinsic::FltCopysign(l, r) => print_call("Flt/copysign", vec![], vec![l, r], frame),
        Intrinsic::FltNeg(i) => print_call("Flt/neg", vec![], vec![i], frame),
        Intrinsic::FltAbs(i) => print_call("Flt/abs", vec![], vec![i], frame),
        Intrinsic::FltSqrt(i) => print_call("Flt/sqrt", vec![], vec![i], frame),
        Intrinsic::FltFloor(i) => print_call("Flt/floor", vec![], vec![i], frame),
        Intrinsic::FltCeil(i) => print_call("Flt/ceil", vec![], vec![i], frame),
        Intrinsic::FltTrunc(i) => print_call("Flt/trunc", vec![], vec![i], frame),
        Intrinsic::FltNearest(i) => print_call("Flt/nearest", vec![], vec![i], frame),
        Intrinsic::FltToLeBytes(i) => print_call("Flt/to_le_bytes", vec![], vec![i], frame),
        Intrinsic::FltOfLeBytes { bin: i, .. } => {
            print_call("Flt/of_le_bytes", vec![], vec![i], frame)
        }
        Intrinsic::NatToInt(i) => print_call("Nat/to_int", vec![], vec![i], frame),
        Intrinsic::NatToFlt(i) => print_call("Nat/to_flt", vec![], vec![i], frame),
        Intrinsic::IntToNat { int: i, .. } => print_call("Int/to_nat", vec![], vec![i], frame),
        Intrinsic::IntToFlt(i) => print_call("Int/to_flt", vec![], vec![i], frame),
        Intrinsic::FltToNat(i) => print_call("Flt/to_nat", vec![], vec![i], frame),
        Intrinsic::FltToInt(i) => print_call("Flt/to_int", vec![], vec![i], frame),
        Intrinsic::BinType(Grain::X) => pure("Bytes"),
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
        Intrinsic::BinAppend {
            grain: Grain::X,
            bin: b,
            element: byte,
        } => print_call("Bytes/append", vec![], vec![b, byte], frame),
        Intrinsic::BinType(Grain::B) => pure("Bits"),
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
        Intrinsic::BinAppend {
            grain: Grain::B,
            bin: b,
            element: bit,
        } => print_call("Bits/append", vec![], vec![b, bit], frame),
        Intrinsic::BinConcat { grain, operands } => {
            let mut entries = Vec::new();
            bin_concat_entries(grain, operands, frame, &mut entries);
            print_packed(grain, entries)
        }
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
            element: ty,
            list,
            item: elem,
        } => print_call("List/append", vec![ty], vec![list, elem], frame),
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
        Intrinsic::HandleType => pure("Handle"),
        // The three `/sys/Handle` constants are the only handles a term ever holds: every other handle is minted by the host at run time, behind an `Io` no reduction enters. The last arm names a token no source can spell, and spells the token rather than abort the diagnostic it is inside.
        Intrinsic::Handle(stdio::STDIN) => pure("Handle/stdin"),
        Intrinsic::Handle(stdio::STDOUT) => pure("Handle/stdout"),
        Intrinsic::Handle(stdio::STDERR) => pure("Handle/stderr"),
        Intrinsic::Handle(token) => pure(format!("Handle({token})")),
        Intrinsic::ProcExit(code) => print_call("proc/exit", vec![], vec![code], frame),
        Intrinsic::CellType(elem) => print_former("Cell", elem, frame),
        Intrinsic::Cell {
            element: type_,
            initial: init,
        } => print_call("Cell/new", vec![type_], vec![init], frame),
        Intrinsic::CellSet {
            element: type_,
            cell,
            value,
        } => print_call("Cell/set", vec![type_], vec![cell, value], frame),
        Intrinsic::CellGet {
            element: type_,
            cell,
        } => print_call("Cell/get", vec![type_], vec![cell], frame),
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
    term_doc(term, Frame { spelling, depth: 0 })
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
        Subterm::UniverseInst(instance) => flat([
            sub(instance.head, frame),
            pure(universe_suffix(&instance.levels, frame.spelling)),
        ]),
        Subterm::Intrinsic(intrinsic) => sub_intrinsic(intrinsic, frame),
        // A builtin row surfaces under its `/sys` subject (`Handle/write`); a user's `foreign` declaration under the name they gave it.
        Subterm::Foreign(function, args) => {
            let name = match &function.subject {
                Some(subject) => format!("{subject}/{}", function.label),
                None => function.label.clone(),
            };
            print_call(name, vec![], args, frame)
        }
        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => {
            let after = frame.deeper(telescope.len());
            let mut printers = Vec::with_capacity(telescope.len());
            let mut cur = telescope;
            let mut minting = frame;
            let mut idx = 0;
            let output = loop {
                match cur {
                    Telescope::Done(body) => break *body,
                    Telescope::Cons(ty, rest) => {
                        let raw = rest.binder(0);
                        let label = minting.label(raw);
                        let mark = plicity_mark(plicities.get(idx));
                        let typed = sub(ty, after);
                        // A hintless binder is compiler-minted (an anonymous parameter), so its label appears only when the rest of the telescope references it — `(B) -> C` renders as written, not `(#6577: B) -> C`.
                        let named = match raw {
                            Some(name) => name.hint().is_some() || rest.uses(0),
                            None => false,
                        };
                        let printer = if named {
                            flat([
                                pure(mark),
                                pure(frame.spelling.label(&label)),
                                pure(": "),
                                typed,
                            ])
                        } else {
                            flat([pure(mark), typed])
                        };
                        printers.push(printer);
                        cur = rest.open(&[&Term::free_var(&label)]);
                        idx += 1;
                    }
                }
            };
            flat([
                listed("(".into(), false, printers, ")"),
                pure(" -> "),
                sub(output, after),
            ])
        }
        Subterm::Func(Func {
            telescope,
            plicities,
        }) => {
            // A type-former lambda `x => T(…, x)` — the shape witness keying and goal displays materialize for a higher-kinded parameter — prints as the former itself: bare `T` when the binder is its only argument, the prefix application otherwise. Recognition demands the exact eta shape (the binder is the final argument and occurs nowhere else), so the display never renames anything, it only hides the lambda the reader would mentally contract anyway.
            if let Some(former) = former_eta(&telescope, &plicities) {
                return former_doc(former, frame);
            }
            // Each binder carries its written/canonical mark (`@x` = implicit, `use x` = witness), matching the `FuncType` printer above. A parameter position cannot be elided, so an unnameable binder nothing references prints the way source spells it: `_`.
            let mut marked = Vec::with_capacity(telescope.len());
            let mut cur = telescope;
            let mut minting = frame;
            let mut idx = 0;
            let body = loop {
                match cur {
                    Telescope::Done(body) => break *body,
                    Telescope::Cons(_ty, rest) => {
                        let label = minting.label(rest.binder(0));
                        let mark = plicity_mark(plicities.get(idx));
                        let shown = if label.hint().is_none() && !rest.uses(0) {
                            "_".to_string()
                        } else {
                            frame.spelling.label(&label)
                        };
                        marked.push(format!("{mark}{shown}"));
                        cur = rest.open(&[&Term::free_var(&label)]);
                        idx += 1;
                    }
                }
            };
            let param_str = if marked.len() == 1 && plicities.first() == Some(&Plicity::Explicit) {
                marked.into_iter().next().unwrap()
            } else {
                format!("({})", marked.join(", "))
            };
            flat([pure(param_str), pure(" =>\n"), indent(sub(body, minting))])
        }
        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => flat([
            sub(head, frame),
            listed(
                "(".into(),
                false,
                params
                    .into_iter()
                    .zip(plicities)
                    .map(|(p, plicity)| marked_argument(sub(p, frame), Some(&plicity)))
                    .collect::<Vec<_>>(),
                ")",
            ),
        ]),
        Subterm::TupleType(TupleType { telescope, .. }) => {
            let after = frame.deeper(telescope.len());
            let mut items = Vec::with_capacity(telescope.len());
            let mut cur = telescope;
            let mut minting = frame;
            while let Telescope::Cons(ty, rest) = cur {
                let raw = rest.binder(0);
                let label = minting.label(raw);
                // As in the `FuncType` printer: an unnameable label nothing references is elided, so the field renders the way source wrote it.
                let named = match raw {
                    Some(name) => name.hint().is_some() || rest.uses(0),
                    None => false,
                };
                let typed = sub(ty, after);
                let printer = if named {
                    flat([pure(frame.spelling.label(&label)), pure(": "), typed])
                } else {
                    typed
                };
                items.push(indent(printer));
                cur = rest.open(&[&Term::free_var(&label)]);
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
            let field = match field {
                Field::Index(index) => format!(").{index}"),
                Field::Label(label) => format!(").{label}"),
            };
            flat([pure("("), sub(head, frame), pure(field)])
        }
        // Params then indices, one flat argument list — exactly how the type-constructor function is applied at use sites, and marked the same way. Without the marks this spells `Eq(Nat, 5, 5)`, three positional arguments where `Eq(@A : Type) : (A, A) -> Prop` accepts two: a rendering no use site could reproduce.
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
            if arity == 0 {
                pure(label)
            } else {
                listed(
                    format!("{label}("),
                    false,
                    params
                        .into_iter()
                        .chain(indices)
                        .enumerate()
                        .map(|(index, p)| {
                            marked_argument(sub(p, frame), marks.and_then(|marks| marks.get(index)))
                        })
                        .collect(),
                    ")",
                )
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
                    params
                        .into_iter()
                        .enumerate()
                        .map(|(index, p)| {
                            marked_argument(sub(p, frame), marks.and_then(|marks| marks.get(index)))
                        })
                        .collect(),
                    ")",
                )
            }
        }
        // Prints as the brace literal, instantiated type params hidden — `Pair { 0, "" }`.
        Subterm::Struct(Struct {
            name,
            universes,
            fields,
            ..
        }) => listed(
            format!(
                "{}{} {{",
                frame.spelling.symbol(&name),
                universe_suffix(&universes, frame.spelling)
            ),
            true,
            fields.into_iter().map(|f| sub(f, frame)).collect(),
            "}",
        ),
        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => {
            // Arity 1 everywhere except an annotated inductive-match motive, whose pattern binders precede the scrutinee binder.
            let (motive_labels, motive_frame) = frame.labels(motive.binder_iter());
            let motive_terms = label_terms(&motive_labels);
            let motive_refs = motive_terms.iter().collect::<Vec<_>>();
            let motive_label = motive_labels
                .iter()
                .map(|label| frame.spelling.label(label))
                .collect::<Vec<_>>()
                .join(", ");
            let motive = motive.open(&motive_refs);

            // Shared `<keyword> head : label => motive;` prefix; the keyword and arm bodies depend on the case kind.
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

            let prefix = flat([
                pure(keyword),
                sub(head, frame),
                pure(": "),
                pure(motive_label),
                pure(" => "),
                sub(motive, motive_frame),
                pure(";"),
            ]);

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
                                let (labels, inner) = frame.labels(arm.binder_iter());
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
                                    indent(flat([sub(body, inner), pure(";")])),
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
                        let ((head_label, tail_label, ih_label), cons_case, inner) =
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
                            indent(flat([sub(cons_case, inner), pure(";")])),
                        ])
                    };
                    let cons_list = |cons_case: Scope<Three>| {
                        let ((head_label, tail_label, ih_label), cons_case, inner) =
                            frame.open_three(cons_case);
                        flat([
                            pure("\n| ["),
                            pure(frame.spelling.label(&head_label)),
                            pure(", .."),
                            pure(frame.spelling.label(&tail_label)),
                            pure("]; "),
                            pure(frame.spelling.label(&ih_label)),
                            pure(" =>\n"),
                            indent(flat([sub(cons_case, inner), pure(";")])),
                        ])
                    };

                    // Per carrier: the identity arm's literal, its body, and the cons arm — which binds `(predecessor, ih)` for the head-less unary `Nat`, and `(head, tail), ih` for `Bin`/`List`.
                    let (empty_lit, empty_case, cons_arm) = match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => {
                            let ((pred_label, ih_label), cons_case, inner) =
                                frame.open_two(cons_case);
                            let cons_arm = flat([
                                pure("\n| "),
                                pure(frame.spelling.label(&pred_label)),
                                pure(" "),
                                pure(frame.spelling.label(&ih_label)),
                                pure(" =>\n"),
                                indent(flat([sub(cons_case, inner), pure(";")])),
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
            let (labels, inner) = frame.labels(tail.binder_iter());
            let label_terms = label_terms(&labels);
            let label_terms = label_terms.iter().collect::<Vec<_>>();

            let lines = bindings
                .iter()
                .enumerate()
                .map(|(index, binding)| {
                    let type_ = binding.type_().release(&label_terms[..index]);
                    let value = binding.value().release(&label_terms[..index]);
                    // Binding `index` sits under the `index` bindings above it.
                    let at = frame.deeper(index);

                    flat([
                        pure("let "),
                        pure(frame.spelling.label(&labels[index])),
                        pure(": "),
                        sub(type_, at),
                        pure(" =\n"),
                        indent(flat([sub(value, at), pure(";")])),
                        pure("\n"),
                    ])
                })
                .collect::<Vec<_>>();

            flat([flat(lines), sub(tail.open(&label_terms), inner)])
        }
        Subterm::Rec(Rec { group, tail }) => {
            let (labels, inner) = frame.labels(tail.binder_iter());
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
                        sub(type_, inner),
                        pure(" =\n"),
                        indent(sub(body, inner)),
                    ])
                })
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                sub(tail, inner),
            ])
        }
        Subterm::Var(var) => print_var(var, frame.spelling),
        Subterm::Transient(Transient::NumLit(num_lit)) => {
            let sign = if num_lit.negative {
                "-"
            } else if num_lit.signed {
                "+"
            } else {
                ""
            };
            pure(format!("{sign}{}", num_lit.magnitude))
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
mod tests {
    use super::*;

    #[test]
    fn a_binder_hinted_like_a_shortened_global_is_suffixed() {
        let global = Global::Authored(Qualifier::from(["main", "helper"]));
        let shorten = build_shorten(std::slice::from_ref(&global));
        assert_eq!(shorten.get(&global).map(String::as_str), Some("helper"));

        let binder = Free::local(0, Some("helper"));
        let names = BTreeSet::from([Free::Global(global), binder.clone()]);
        let rename = build_rename(&names, &shorten);
        assert_eq!(rename.get(&binder).map(String::as_str), Some("helper2"));
    }

    /// Building a document descends once per link, so this is what [`sub`]'s guard is for — and the depth a diagnostic's term can reach is the elaborator's, not the writer's. Deep enough that a regression is a stack overflow rather than a slow test. The other two walks over a document, running and freeing it, are fixtured in `curios-utilities` at the same depth.
    #[test]
    fn a_deep_term_is_printed_without_overflowing() {
        const DEEP: usize = 100_000;

        let argument = Term::free_var(&Free::local(0, None));
        let mut term = Term::free_var(&Free::local(0, None));
        for _ in 0..DEEP {
            term = Term::apply(term, [argument.clone()]);
        }

        assert_eq!(term.to_string().matches('(').count(), DEEP);
    }
}
