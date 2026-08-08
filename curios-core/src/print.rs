use {
    super::{
        Apply, Arity, Atom, Bang, Bound, Carrier, Cases, Field, Free, Func, FuncType, Global,
        InductType, Infix, Intrinsic, Let, Level, Match, Nat, Proj, Rec, Scope, Struct, StructType,
        Subterm, Telescope, Term, Three, Transient, Tuple, TupleType, Two, Var, Variant,
    },
    curios_base::{
        Flt, Grain, Plicity, Qualifier,
        printer::{Printer, deferred, flat, group, indent, line, pure, sep_flat, soft_line},
    },
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
// The configuration is threaded, not ambient. `Display::fmt` has no parameter channel, so these axes were once three thread-locals installed around a render — which made a term's spelling depend on an enclosing frame nobody could see from the call, and made "should this consumer erase universes?" a question answered by accident of where the installer sat rather than by the consumer. [`Spelled`] restores the parameter: `term.spelled(&spelling)` is an ordinary value that implements `Display`, and every printer function takes the spelling beside the depth it already threaded.
//
// axis (a) — local binders: a *rename map* (built by `build_rename` over `display_names`) alpha-renames the whole fragment — free vars *and* binder labels. A source hint is used bare when unique; distinct names sharing a hint, or shadowing a literally-rendered global, take minimal `hint2`, `hint3`, … suffixes, so no two binders ever read alike. A hintless (compiler-minted) binder spells `_` — or is elided — at its label site when nothing references it, and borrows the fallback hint `x` when something does: `_` in a reference position would read as a hole and could not co-spell with its binder.
//
// axis (b) — globals: a *shorten map* (built by `build_shorten` over `Module::module_symbols`) replaces each qualified path with its shortest unambiguous `/`-suffix — the name in scope, since Curios has no `use … as` aliasing. Used by error rendering *and* `Module` display.
//
// axis (c) — universe instances: a flag suppressing the `.{…}` an instantiated nominal head carries. The surface language has no spelling for an instance — solved (`Option.{0}`) or unsolved (`Eq.{?u271}`) alike — so a diagnostic that shows one asks the reader to decode elaboration state. This is the display twin of `project_erased_universes`, which the goal-report path applies structurally; errors carry raw terms all the way to the formatter, so they suppress at the printer instead. Diagnostics set it; the `--print` stage dumps deliberately do not, because a dump is read *about* the compiler and its levels are the point.
//
// A `Type`'s own level is suppressed only when it is *metavariable-headed*. The level is that node's whole content, so erasing a concrete one could render two distinct sorts identically — but an unsolved level names nothing a reader can act on, and it is the common case: a diagnostic over a polymorphic head reports `(A: Type.{?u263}) -> Nat` against `((Type.{?u261}) -> Type.{?u262}) -> Nat`, three placeholders competing with the disagreement they surround. Suppressing every level was rejected for the case that cannot be ruled out — two distinct concrete levels rendering as `Type` against `Type` — and a whole-fragment "show it only when it disambiguates" pass was rejected as context-dependent rendering: unlike a binder name, which is inherently relative, a level is an absolute fact about the term.
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

/// One argument of an applied nominal family, marked as its declaration wrote it. Indices are always explicit, so only a parameter ever takes a mark.
fn marked_argument(printer: Printer, plicity: Option<&Plicity>) -> Printer {
    match plicity {
        Some(Plicity::Implicit) => flat([pure("@"), printer]),
        Some(Plicity::Witness) => flat([pure("use "), printer]),
        _ => printer,
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

/// Every name the printer will emit for `term`: free vars (Γ references) and the stored labels of every binder it reopens. Free vars come from the robust `Bound` traversal; binder labels — which that traversal never surfaces — from [`collect_labels`], which descends scope bodies (where nested binders live).
pub fn display_names(term: &Term) -> BTreeSet<Free> {
    let mut names = term.free_vars();
    collect_labels(term, &mut names);
    names
}

/// Collect every binder label in `term`, recursing through scope and telescope bodies. (`Intrinsic` interiors are skipped: they hold no binders the diagnostics need to name, and any free vars there are already in `free_vars`.)
fn collect_labels(term: &Term, out: &mut BTreeSet<Free>) {
    fn push(out: &mut BTreeSet<Free>, binder: Option<&Free>) {
        if let Some(binder) = binder {
            out.insert(binder.clone());
        }
    }

    fn scope<A: Arity>(out: &mut BTreeSet<Free>, scope: &Scope<A>) {
        if let Some(names) = scope.names() {
            names.iter().for_each(|n| push(out, Some(n)));
        }
        collect_labels(scope.body(), out);
    }

    fn telescope(out: &mut BTreeSet<Free>, mut cur: &Telescope<Term>) {
        loop {
            match cur {
                Telescope::Cons(ty, rest) => {
                    push(out, rest.binder(0));
                    collect_labels(ty, out);
                    cur = rest.body();
                }
                Telescope::Done(body) => break collect_labels(body, out),
            }
        }
    }

    // A tuple type's telescope has no result term (`Telescope<()>`); only its field types and labels carry names.
    fn tuple_telescope(out: &mut BTreeSet<Free>, mut cur: &Telescope<()>) {
        while let Telescope::Cons(ty, rest) = cur {
            push(out, rest.binder(0));
            collect_labels(ty, out);
            cur = rest.body();
        }
    }

    let each = |out: &mut BTreeSet<Free>, terms: &[Term]| {
        terms.iter().for_each(|t| collect_labels(t, out))
    };

    match &**term {
        Subterm::FuncType(FuncType { telescope: t, .. }) => telescope(out, t),
        Subterm::Func(Func { telescope: t, .. }) => telescope(out, t),
        Subterm::TupleType(TupleType { telescope: t, .. }) => tuple_telescope(out, t),
        Subterm::Apply(Apply { head, params, .. }) => {
            collect_labels(head, out);
            each(out, params);
        }
        Subterm::Tuple(Tuple { fields, .. }) => each(out, fields),
        Subterm::Proj(Proj { head, .. }) => collect_labels(head, out),
        Subterm::InductType(InductType {
            params, indices, ..
        }) => {
            each(out, params);
            each(out, indices);
        }
        Subterm::Variant(Variant {
            params, payload, ..
        }) => {
            each(out, params);
            each(out, payload);
        }
        Subterm::StructType(StructType { params, .. }) => each(out, params),
        Subterm::Struct(Struct { params, fields, .. }) => {
            each(out, params);
            each(out, fields);
        }
        Subterm::Let(Let { bindings, tail, .. }) => {
            for binding in bindings {
                collect_labels(binding.type_(), out);
                collect_labels(binding.value(), out);
            }
            scope(out, tail);
        }
        Subterm::Rec(Rec { group, tail }) => {
            for member in group.iter() {
                scope(out, &member.type_);
                scope(out, &member.body);
            }
            scope(out, tail);
        }
        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => {
            collect_labels(head, out);
            scope(out, motive);
            match cases {
                Cases::Bool {
                    false_case,
                    true_case,
                } => {
                    collect_labels(false_case, out);
                    collect_labels(true_case, out);
                }
                Cases::Switch { cases, default } => {
                    cases.iter().for_each(|(_, body)| collect_labels(body, out));
                    collect_labels(default, out);
                }
                Cases::Induct { cases, default, .. } => {
                    cases.iter().for_each(|(_, s)| scope(out, &s.body));
                    default.iter().for_each(|d| collect_labels(d, out));
                }
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat {
                        empty_case,
                        cons_case,
                    } => {
                        collect_labels(empty_case, out);
                        scope(out, cons_case);
                    }
                    Carrier::Bin {
                        empty_case,
                        cons_case,
                        ..
                    } => {
                        collect_labels(empty_case, out);
                        scope(out, cons_case);
                    }
                    Carrier::Lst {
                        elem,
                        empty_case,
                        cons_case,
                    } => {
                        collect_labels(elem, out);
                        collect_labels(empty_case, out);
                        scope(out, cons_case);
                    }
                },
            }
        }
        Subterm::Metavar(metavar) => metavar.spine.iter().for_each(|t| collect_labels(t, out)),
        Subterm::UniverseInst(instance) => collect_labels(&instance.head, out),
        Subterm::Transient(transient) => {
            transient
                .subterms()
                .for_each(|child| collect_labels(child, out));
        }
        Subterm::Var(_)
        | Subterm::Type(_)
        | Subterm::Prop
        | Subterm::Intrinsic(_)
        | Subterm::Foreign(..) => {}
    }
}

/// Give every local binder a clean display spelling: its hint — or `x` where it was minted hintless — suffixed `hint2`, `hint3`, … when several distinct identities — binders *or* free vars — would otherwise render alike, or would shadow a global's literal rendering. The result is unambiguous by construction, so no rendered name is ever silently shared between two binders.
///
/// A hintless entry's `x` is consulted only where something references the binder — the label sites spell an unreferenced unnameable binder `_` (or elide it) without the map. Hinted names are assigned first, so a synthesized `x` can never steal the spelling from a binder actually written `x`.
pub fn build_rename(names: &BTreeSet<Free>) -> HashMap<Free, String> {
    // `names` is sorted, so the assignment below is deterministic.
    let (literal, prettifiable): (Vec<_>, Vec<_>) =
        names.iter().partition(|name| name.as_global().is_some());

    // Globals render as themselves and reserve their spelling up front.
    let mut used = literal
        .into_iter()
        .map(Free::to_string)
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

/// Every binder of a scope, unnamed ones filled with stand-ins positioned from `depth`.
fn scope_labels<'a>(binders: impl Iterator<Item = Option<&'a Free>>, depth: usize) -> Vec<Free> {
    binders
        .enumerate()
        .map(|(index, binder)| binder_or(binder, depth + index))
        .collect()
}

fn label_terms(binders: &[Free]) -> Vec<Term> {
    binders.iter().map(Term::free_var).collect()
}

fn open_scope_two(scope: Scope<Two>, depth: usize) -> ((Free, Free), Term) {
    let fst = binder_or(scope.binder(0), depth);
    let snd = binder_or(scope.binder(1), depth + 1);
    let body = scope.open(&[&Term::free_var(&fst), &Term::free_var(&snd)]);

    ((fst, snd), body)
}

fn open_scope_three(scope: Scope<Three>, depth: usize) -> ((Free, Free, Free), Term) {
    let fst = binder_or(scope.binder(0), depth);
    let snd = binder_or(scope.binder(1), depth + 1);
    let thd = binder_or(scope.binder(2), depth + 2);
    let body = scope.open(&[
        &Term::free_var(&fst),
        &Term::free_var(&snd),
        &Term::free_var(&thd),
    ]);

    ((fst, snd, thd), body)
}

fn print_var(var: Var, spelling: &Rc<Spelling>) -> Printer {
    pure(spelling.label(var.unwrap()))
}

fn print_atom(atom: Atom) -> Printer {
    flat([pure("'"), pure(atom.as_string())])
}

fn print_flt(flt: Flt) -> Printer {
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

/// Render a binary intrinsic as `name left right`, the shape almost every scalar arithmetic/comparison/bitwise intrinsic shares. `name` carries its own trailing space (`"Nat.add "`).
fn print_binary(
    name: &'static str,
    left: Term,
    right: Term,
    depth: usize,
    spelling: &Rc<Spelling>,
) -> Printer {
    flat([
        pure(name),
        sub(left, depth, spelling),
        pure(" "),
        sub(right, depth, spelling),
    ])
}

/// The unary counterpart of [`print_binary`]: `name inner`.
fn print_unary(name: &'static str, inner: Term, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    flat([pure(name), sub(inner, depth, spelling)])
}

/// The surface infix symbol an operator intrinsic prints as, or `None` for an intrinsic with no infix spelling — the bitwise ops, conversions, `min`/`max`, and the `Bool.xor` that `!=` desugars through. Exactly the operators the surface language spells infix ([`NumOp::symbol`](super::NumOp::symbol)); the concept-dispatched arithmetic/comparison operators plus the two hardcoded `Bool` short-circuits.
fn infix_symbol(intrinsic: &Intrinsic) -> Option<&'static str> {
    Some(match intrinsic {
        Intrinsic::NatAdd(..) | Intrinsic::IntAdd(..) | Intrinsic::FltAdd(..) => "+",
        Intrinsic::NatSub(..) | Intrinsic::IntSub(..) | Intrinsic::FltSub(..) => "-",
        Intrinsic::NatMul(..) | Intrinsic::IntMul(..) | Intrinsic::FltMul(..) => "*",
        Intrinsic::NatDiv(..) | Intrinsic::IntDiv(..) | Intrinsic::FltDiv(..) => "/",
        Intrinsic::NatRem(..) | Intrinsic::IntRem(..) | Intrinsic::FltRem(..) => "%",
        Intrinsic::NatEql(..)
        | Intrinsic::IntEql(..)
        | Intrinsic::FltEql(..)
        | Intrinsic::BoolEql(..)
        | Intrinsic::BinEql(Grain::X, ..)
        | Intrinsic::HandleEql(..) => "==",
        Intrinsic::NatNeq(..)
        | Intrinsic::IntNeq(..)
        | Intrinsic::FltNeq(..)
        | Intrinsic::BoolNeq(..) => "!=",
        Intrinsic::NatLt(..) | Intrinsic::IntLt(..) | Intrinsic::FltLt(..) => "<",
        Intrinsic::NatGt(..) | Intrinsic::IntGt(..) | Intrinsic::FltGt(..) => ">",
        Intrinsic::NatLte(..) | Intrinsic::IntLte(..) | Intrinsic::FltLte(..) => "<=",
        Intrinsic::NatGte(..) | Intrinsic::IntGte(..) | Intrinsic::FltGte(..) => ">=",
        Intrinsic::BoolAnd(..) => "&&",
        Intrinsic::BoolOr(..) => "||",
        _ => return None,
    })
}

/// Render an operator intrinsic as `left <symbol> right`, each operand parenthesized when it is itself an infix operator so nesting stays unambiguous — `(a + b) * c`, never `a + b * c`.
fn print_infix(
    symbol: &'static str,
    left: Term,
    right: Term,
    depth: usize,
    spelling: &Rc<Spelling>,
) -> Printer {
    flat([
        print_operand(left, depth, spelling),
        pure(format!(" {symbol} ")),
        print_operand(right, depth, spelling),
    ])
}

/// A recognized type-former eta shape: the former's identity and the argument prefix left after stripping the binder.
enum FormerEta {
    Nominal(Global, Vec<Term>),
    Intrinsic(&'static str),
}

/// Recognize `x => T(…, x)` on the *unopened* telescope: one explicit binder, whose sole occurrence is the final argument of a saturated former body — a nominal type with no indices, or a unary intrinsic carrier. The prefix arguments must be closed under the binder (`reach() == 0`), which is what guarantees the binder occurs nowhere else.
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
        Subterm::Intrinsic(Intrinsic::LstType(payload)) => {
            bound_zero(payload).then_some(FormerEta::Intrinsic("Lst"))
        }
        Subterm::Intrinsic(Intrinsic::CellType(payload)) => {
            bound_zero(payload).then_some(FormerEta::Intrinsic("Cell"))
        }
        _ => None,
    }
}

/// Print a recognized former: the name alone when the binder was its only argument, the prefix application otherwise — routed through a synthetic term so qualification and spelling stay uniform with every other reference.
fn former_doc(former: FormerEta, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    match former {
        FormerEta::Intrinsic(name) => pure(name),
        FormerEta::Nominal(name, prefix) => {
            let reference = Term::var(Var::free(Free::Global(name)));
            let term = if prefix.is_empty() {
                reference
            } else {
                Term::apply(reference, prefix)
            };
            sub(term, depth, spelling)
        }
    }
}

/// An operand of [`print_infix`], wrapped in parentheses when it too prints as an infix operator (a nested operator intrinsic or a residual `Infix` node); self-delimiting operands (variables, literals, applications) print bare.
fn print_operand(term: Term, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    let parenthesize = match &*term {
        Subterm::Intrinsic(intrinsic) => infix_symbol(intrinsic).is_some(),
        Subterm::Transient(Transient::Infix(_)) => true,
        _ => false,
    };

    if parenthesize {
        flat([pure("("), sub(term, depth, spelling), pure(")")])
    } else {
        sub(term, depth, spelling)
    }
}

fn print_intrinsic(intrinsic: Intrinsic, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    match intrinsic {
        Intrinsic::BoolType => pure("Bool"),
        Intrinsic::Bool(false) => pure("false"),
        Intrinsic::Bool(true) => pure("true"),
        Intrinsic::BoolAnd(l, r) => print_infix("&&", l, r, depth, spelling),
        Intrinsic::BoolOr(l, r) => print_infix("||", l, r, depth, spelling),
        Intrinsic::BoolXor(l, r) => print_binary("Bool.xor ", l, r, depth, spelling),
        Intrinsic::BoolEql(l, r) => print_infix("==", l, r, depth, spelling),
        Intrinsic::BoolNeq(l, r) => print_infix("!=", l, r, depth, spelling),
        Intrinsic::NatType => pure("Nat"),
        Intrinsic::Nat(Nat::Zero) => pure("0"),
        // A successor over a symbolic tail is that tail plus its literal floor — spelled infix (`n + 1`, `(n + m) + 3`) to match the operator intrinsics, its tail parenthesized when it too is an operator. A successor over `0` is a plain numeral (`{spine}`).
        Intrinsic::Nat(Nat::Succ(spine, inner)) => match inner.as_ref() {
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => pure(format!("{spine}")),
            _ => flat([
                print_operand(inner.clone(), depth, spelling),
                pure(format!(" + {spine}")),
            ]),
        },
        Intrinsic::NatEql(l, r) => print_infix("==", l, r, depth, spelling),
        Intrinsic::HandleEql(l, r) => print_infix("==", l, r, depth, spelling),
        Intrinsic::NatNeq(l, r) => print_infix("!=", l, r, depth, spelling),
        Intrinsic::NatAdd(l, r) => print_infix("+", l, r, depth, spelling),
        Intrinsic::NatSub(l, r) => print_infix("-", l, r, depth, spelling),
        Intrinsic::NatMul(l, r) => print_infix("*", l, r, depth, spelling),
        Intrinsic::NatLt(l, r) => print_infix("<", l, r, depth, spelling),
        Intrinsic::NatDiv(l, r) => print_infix("/", l, r, depth, spelling),
        Intrinsic::NatRem(l, r) => print_infix("%", l, r, depth, spelling),
        Intrinsic::NatGt(l, r) => print_infix(">", l, r, depth, spelling),
        Intrinsic::NatLte(l, r) => print_infix("<=", l, r, depth, spelling),
        Intrinsic::NatGte(l, r) => print_infix(">=", l, r, depth, spelling),
        Intrinsic::NatAnd(l, r) => print_binary("Nat.and ", l, r, depth, spelling),
        Intrinsic::NatOr(l, r) => print_binary("Nat.or ", l, r, depth, spelling),
        Intrinsic::NatXor(l, r) => print_binary("Nat.xor ", l, r, depth, spelling),
        Intrinsic::NatShl(l, r) => print_binary("Nat.shl ", l, r, depth, spelling),
        Intrinsic::NatShr(l, r) => print_binary("Nat.shr ", l, r, depth, spelling),
        Intrinsic::NatRotl(l, r) => print_binary("Nat.rotl ", l, r, depth, spelling),
        Intrinsic::NatRotr(l, r) => print_binary("Nat.rotr ", l, r, depth, spelling),
        Intrinsic::NatClz(i) => print_unary("Nat.clz ", i, depth, spelling),
        Intrinsic::NatCtz(i) => print_unary("Nat.ctz ", i, depth, spelling),
        Intrinsic::NatPopcnt(i) => print_unary("Nat.popcnt ", i, depth, spelling),
        Intrinsic::ByteType => pure("Byte"),
        Intrinsic::Byte(value) => pure(format!("0x{value:02X}")),
        Intrinsic::ByteToNat(i) => print_unary("Byte.to_nat ", i, depth, spelling),
        Intrinsic::NatToByte(i) => print_unary("Nat.to_byte ", i, depth, spelling),
        Intrinsic::ByteEql(l, r) => print_binary("Byte.eql ", l, r, depth, spelling),
        Intrinsic::ByteLt(l, r) => print_binary("Byte.lt ", l, r, depth, spelling),
        Intrinsic::ByteLte(l, r) => print_binary("Byte.lte ", l, r, depth, spelling),
        Intrinsic::ByteGt(l, r) => print_binary("Byte.gt ", l, r, depth, spelling),
        Intrinsic::ByteGte(l, r) => print_binary("Byte.gte ", l, r, depth, spelling),
        Intrinsic::IntType => pure("Int"),
        Intrinsic::Int(value) => pure(format!("{value:+}")),
        Intrinsic::IntEql(l, r) => print_infix("==", l, r, depth, spelling),
        Intrinsic::IntNeq(l, r) => print_infix("!=", l, r, depth, spelling),
        Intrinsic::IntAdd(l, r) => print_infix("+", l, r, depth, spelling),
        Intrinsic::IntSub(l, r) => print_infix("-", l, r, depth, spelling),
        Intrinsic::IntMul(l, r) => print_infix("*", l, r, depth, spelling),
        Intrinsic::IntDiv(l, r) => print_infix("/", l, r, depth, spelling),
        Intrinsic::IntRem(l, r) => print_infix("%", l, r, depth, spelling),
        Intrinsic::IntLt(l, r) => print_infix("<", l, r, depth, spelling),
        Intrinsic::IntGt(l, r) => print_infix(">", l, r, depth, spelling),
        Intrinsic::IntLte(l, r) => print_infix("<=", l, r, depth, spelling),
        Intrinsic::IntGte(l, r) => print_infix(">=", l, r, depth, spelling),
        Intrinsic::IntAnd(l, r) => print_binary("Int.and ", l, r, depth, spelling),
        Intrinsic::IntOr(l, r) => print_binary("Int.or ", l, r, depth, spelling),
        Intrinsic::IntXor(l, r) => print_binary("Int.xor ", l, r, depth, spelling),
        Intrinsic::IntShl(l, r) => print_binary("Int.shl ", l, r, depth, spelling),
        Intrinsic::IntShr(l, r) => print_binary("Int.shr ", l, r, depth, spelling),
        Intrinsic::IntRotl(l, r) => print_binary("Int.rotl ", l, r, depth, spelling),
        Intrinsic::IntRotr(l, r) => print_binary("Int.rotr ", l, r, depth, spelling),
        Intrinsic::IntClz(i) => print_unary("Int.clz ", i, depth, spelling),
        Intrinsic::IntCtz(i) => print_unary("Int.ctz ", i, depth, spelling),
        Intrinsic::IntPopcnt(i) => print_unary("Int.popcnt ", i, depth, spelling),
        Intrinsic::FltType => pure("Flt"),
        Intrinsic::Flt(flt) => print_flt(flt),
        Intrinsic::FltAdd(l, r) => print_infix("+", l, r, depth, spelling),
        Intrinsic::FltSub(l, r) => print_infix("-", l, r, depth, spelling),
        Intrinsic::FltMul(l, r) => print_infix("*", l, r, depth, spelling),
        Intrinsic::FltDiv(l, r) => print_infix("/", l, r, depth, spelling),
        Intrinsic::FltRem(l, r) => print_infix("%", l, r, depth, spelling),
        Intrinsic::FltEql(l, r) => print_infix("==", l, r, depth, spelling),
        Intrinsic::FltNeq(l, r) => print_infix("!=", l, r, depth, spelling),
        Intrinsic::FltLt(l, r) => print_infix("<", l, r, depth, spelling),
        Intrinsic::FltGt(l, r) => print_infix(">", l, r, depth, spelling),
        Intrinsic::FltLte(l, r) => print_infix("<=", l, r, depth, spelling),
        Intrinsic::FltGte(l, r) => print_infix(">=", l, r, depth, spelling),
        Intrinsic::FltMin(l, r) => print_binary("Flt.min ", l, r, depth, spelling),
        Intrinsic::FltMax(l, r) => print_binary("Flt.max ", l, r, depth, spelling),
        Intrinsic::FltCopysign(l, r) => print_binary("Flt.copysign ", l, r, depth, spelling),
        Intrinsic::FltNeg(i) => print_unary("Flt.neg ", i, depth, spelling),
        Intrinsic::FltAbs(i) => print_unary("Flt.abs ", i, depth, spelling),
        Intrinsic::FltSqrt(i) => print_unary("Flt.sqrt ", i, depth, spelling),
        Intrinsic::FltFloor(i) => print_unary("Flt.floor ", i, depth, spelling),
        Intrinsic::FltCeil(i) => print_unary("Flt.ceil ", i, depth, spelling),
        Intrinsic::FltTrunc(i) => print_unary("Flt.trunc ", i, depth, spelling),
        Intrinsic::FltNearest(i) => print_unary("Flt.nearest ", i, depth, spelling),
        Intrinsic::FltToLeBytes(i) => print_unary("Flt.to_le_bytes ", i, depth, spelling),
        Intrinsic::FltOfLeBytes(i) => print_unary("Flt.of_le_bytes ", i, depth, spelling),
        Intrinsic::NatToInt(i) => print_unary("Nat.to_int ", i, depth, spelling),
        Intrinsic::NatToFlt(i) => print_unary("Nat.to_flt ", i, depth, spelling),
        Intrinsic::IntToNat(i) => print_unary("Int.to_nat ", i, depth, spelling),
        Intrinsic::IntToFlt(i) => print_unary("Int.to_flt ", i, depth, spelling),
        Intrinsic::FltToNat(i) => print_unary("Flt.to_nat ", i, depth, spelling),
        Intrinsic::FltToInt(i) => print_unary("Flt.to_int ", i, depth, spelling),
        Intrinsic::BinType(Grain::X) => pure("Bytes"),
        Intrinsic::Bin(Grain::X, bytes) => pure(format!(
            "x[{}]",
            bytes
                .as_bytes()
                .unwrap()
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<Vec<_>>()
                .join(", ")
        )),
        Intrinsic::BinLen(Grain::X, b) => print_unary("Bytes.len ", b, depth, spelling),
        Intrinsic::BinEql(Grain::X, l, r) => print_binary("Bytes.eql ", l, r, depth, spelling),
        Intrinsic::BinGet(Grain::X, b, i) => print_binary("Bytes.get ", b, i, depth, spelling),
        Intrinsic::BinSlice(Grain::X, bin, start, end) => flat([
            pure("Bytes.slice "),
            sub(bin, depth, spelling),
            pure(" "),
            sub(start, depth, spelling),
            pure(" "),
            sub(end, depth, spelling),
        ]),
        Intrinsic::BinAppend(Grain::X, b, byte) => {
            print_binary("Bytes.append ", b, byte, depth, spelling)
        }
        Intrinsic::BinConcat(Grain::X, operands) => flat([
            pure("Bytes.concat "),
            sep_flat(
                operands.into_iter().map(move |e| sub(e, depth, spelling)),
                || pure(", "),
            ),
        ]),
        Intrinsic::BinType(Grain::B) => pure("Bits"),
        Intrinsic::Bin(Grain::B, bits) => pure(format!(
            "b[{}]",
            (0..bits.bit_length())
                .map(|index| match bits.bit(index).unwrap() {
                    true => "\\1",
                    false => "\\0",
                })
                .collect::<Vec<_>>()
                .join(", ")
        )),
        Intrinsic::BinLen(Grain::B, b) => print_unary("Bits.len ", b, depth, spelling),
        Intrinsic::BinEql(Grain::B, l, r) => print_binary("Bits.eql ", l, r, depth, spelling),
        Intrinsic::BinGet(Grain::B, b, i) => print_binary("Bits.get ", b, i, depth, spelling),
        Intrinsic::BinSlice(Grain::B, bin, start, end) => flat([
            pure("Bits.slice "),
            sub(bin, depth, spelling),
            pure(" "),
            sub(start, depth, spelling),
            pure(" "),
            sub(end, depth, spelling),
        ]),
        Intrinsic::BinAppend(Grain::B, b, bit) => {
            print_binary("Bits.append ", b, bit, depth, spelling)
        }
        Intrinsic::BinConcat(Grain::B, operands) => flat([
            pure("Bits.concat "),
            sep_flat(
                operands.into_iter().map(move |e| sub(e, depth, spelling)),
                || pure(", "),
            ),
        ]),
        Intrinsic::LstType(elem) => print_unary("Lst ", elem, depth, spelling),
        Intrinsic::Lst(_, elems) => flat([
            pure("["),
            sep_flat(
                elems.into_iter().map(move |e| sub(e, depth, spelling)),
                || pure(", "),
            ),
            pure("]"),
        ]),
        Intrinsic::LstLen(ty, list) => print_binary("Lst.len ", ty, list, depth, spelling),
        Intrinsic::LstGet(ty, list, index) => flat([
            pure("Lst.get "),
            sub(ty, depth, spelling),
            pure(" "),
            sub(list, depth, spelling),
            pure(" "),
            sub(index, depth, spelling),
        ]),
        Intrinsic::LstSlice(ty, list, start, end) => flat([
            pure("Lst.slice "),
            sub(ty, depth, spelling),
            pure(" "),
            sub(list, depth, spelling),
            pure(" "),
            sub(start, depth, spelling),
            pure(" "),
            sub(end, depth, spelling),
        ]),
        Intrinsic::LstAppend(ty, list, elem) => flat([
            pure("Lst.append "),
            sub(ty, depth, spelling),
            pure(" "),
            sub(list, depth, spelling),
            pure(" "),
            sub(elem, depth, spelling),
        ]),
        Intrinsic::LstConcat(ty, operands) => flat([
            pure("Lst.concat "),
            sub(ty, depth, spelling),
            pure(" "),
            sep_flat(
                operands.into_iter().map(move |e| sub(e, depth, spelling)),
                || pure(", "),
            ),
        ]),
        Intrinsic::LstMap(a, b, lst, f) => flat([
            pure("Lst.map "),
            sub(a, depth, spelling),
            pure(" "),
            sub(b, depth, spelling),
            pure(" "),
            sub(lst, depth, spelling),
            pure(" "),
            sub(f, depth, spelling),
        ]),
        Intrinsic::HandleType => pure("Handle"),
        Intrinsic::Handle(token) => pure(format!("Handle({token})")),
        Intrinsic::ProcExit(code) => print_unary("proc.exit ", code, depth, spelling),
        Intrinsic::CellType(elem) => print_unary("Cell ", elem, depth, spelling),
        Intrinsic::Cell(type_, init) => print_binary("Cell.new ", type_, init, depth, spelling),
        Intrinsic::CellSet(type_, cell, value) => flat([
            pure("Cell.set "),
            sub(type_, depth, spelling),
            pure(" "),
            sub(cell, depth, spelling),
            pure(" "),
            sub(value, depth, spelling),
        ]),
        Intrinsic::CellGet(type_, cell) => print_binary("Cell.get ", type_, cell, depth, spelling),
        Intrinsic::IoType(result) => print_unary("Io ", result, depth, spelling),
        Intrinsic::IoPure(type_, value) => print_binary("Io.pure ", type_, value, depth, spelling),
        Intrinsic::IoBind(a, b, action, f) => flat([
            pure("Io.bind "),
            sub(a, depth, spelling),
            pure(" "),
            sub(b, depth, spelling),
            pure(" "),
            sub(action, depth, spelling),
            pure(" "),
            sub(f, depth, spelling),
        ]),
    }
}

/// A child document, deferred.
///
/// Every recursive call in this module goes through here. Printing a term is a recursive function over a recursive structure, so building the document descended as deep as the term — one native frame per link of a string literal's UTF-8 derivation, which aborted the compiler while it was trying to *report* that the same literal had exhausted the reduction budget. The thunk moves that descent onto `run_printer`'s stack.
fn sub(term: Term, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    let spelling = Rc::clone(spelling);
    deferred(move || print_term(term, depth, &spelling))
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
fn sub_intrinsic(intrinsic: Intrinsic, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    let spelling = Rc::clone(spelling);
    deferred(move || print_intrinsic(intrinsic, depth, &spelling))
}

pub(crate) fn print_term(term: Term, depth: usize, spelling: &Rc<Spelling>) -> Printer {
    match Term::unwrap_or_clone(term) {
        Subterm::Type(level) => {
            if level.is_zero() || (spelling.erase_universes && level.metas().next().is_some()) {
                pure("Type")
            } else {
                pure(format!("Type.{{{level}}}"))
            }
        }
        Subterm::Prop => pure("Prop"),
        Subterm::UniverseInst(instance) => flat([
            sub(instance.head, depth, spelling),
            pure(universe_suffix(&instance.levels, spelling)),
        ]),
        Subterm::Intrinsic(intrinsic) => sub_intrinsic(intrinsic, depth, spelling),
        Subterm::Foreign(function, args) => flat(
            [pure(function.label.clone())]
                .into_iter()
                .chain(
                    args.into_iter()
                        .flat_map(|arg| [pure(" "), sub(arg, depth, spelling)]),
                )
                .collect::<Vec<_>>(),
        ),
        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => {
            fn walk(
                cur: Telescope<Term>,
                plicities: &[Plicity],
                depth: usize,
                total: usize,
                idx: usize,
                printers: &mut Vec<Printer>,
                spelling: &Rc<Spelling>,
            ) -> Term {
                match cur {
                    Telescope::Done(body) => *body,
                    Telescope::Cons(ty, rest) => {
                        let raw = rest.binder(0);
                        let label = binder_or(raw, depth + idx);
                        // Plicity marks the name (`@x` = implicit, `use x` = witness).
                        let mark = match plicities.get(idx) {
                            Some(Plicity::Implicit) => "@",
                            Some(Plicity::Witness) => "use ",
                            _ => "",
                        };
                        let typed = sub(ty, depth + total, spelling);
                        // A hintless binder is compiler-minted (an anonymous parameter), so its label appears only when the rest of the telescope references it — `(B) -> C` renders as written, not `(#6577: B) -> C`.
                        let named = match raw {
                            Some(name) => name.hint().is_some() || rest.uses(0),
                            None => false,
                        };
                        let printer = if named {
                            flat([pure(mark), pure(spelling.label(&label)), pure(": "), typed])
                        } else {
                            flat([pure(mark), typed])
                        };
                        printers.push(printer);
                        let next = rest.open(&[&Term::free_var(&label)]);
                        walk(next, plicities, depth, total, idx + 1, printers, spelling)
                    }
                }
            }

            let n = telescope.len();
            let mut printers = Vec::with_capacity(n);
            let output = walk(telescope, &plicities, depth, n, 0, &mut printers, spelling);
            flat([
                listed("(".into(), false, printers, ")"),
                pure(" -> "),
                sub(output, depth + n, spelling),
            ])
        }
        Subterm::Func(Func {
            telescope,
            plicities,
        }) => {
            // A type-former lambda `x => T(…, x)` — the shape witness keying and goal displays materialize for a higher-kinded parameter — prints as the former itself: bare `T` when the binder is its only argument, the prefix application otherwise. Recognition demands the exact eta shape (the binder is the final argument and occurs nowhere else), so the display never renames anything, it only hides the lambda the reader would mentally contract anyway.
            if let Some(former) = former_eta(&telescope, &plicities) {
                return former_doc(former, depth, spelling);
            }
            let n = telescope.len();
            // Each binder carries its written/canonical mark (`@x` = implicit, `use x` = witness), matching the `FuncType` printer above. A parameter position cannot be elided, so an unnameable binder nothing references prints the way source spells it: `_`.
            let mut marked = Vec::with_capacity(n);
            let mut cur = telescope;
            let mut idx = 0;
            let body = loop {
                match cur {
                    Telescope::Done(body) => break *body,
                    Telescope::Cons(_ty, rest) => {
                        let label = binder_or(rest.binder(0), depth + idx);
                        let mark = match plicities.get(idx) {
                            Some(Plicity::Implicit) => "@",
                            Some(Plicity::Witness) => "use ",
                            _ => "",
                        };
                        let shown = if label.hint().is_none() && !rest.uses(0) {
                            "_".to_string()
                        } else {
                            spelling.label(&label)
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
            flat([
                pure(param_str),
                pure(" =>\n"),
                indent(sub(body, depth + n, spelling)),
            ])
        }
        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => flat([
            sub(head, depth, spelling),
            listed(
                "(".into(),
                false,
                params
                    .into_iter()
                    .zip(plicities)
                    .map(|(p, plicity)| match plicity {
                        Plicity::Implicit => flat([pure("@"), sub(p, depth, spelling)]),
                        Plicity::Witness => flat([pure("use "), sub(p, depth, spelling)]),
                        Plicity::Explicit => sub(p, depth, spelling),
                    })
                    .collect::<Vec<_>>(),
                ")",
            ),
        ]),
        Subterm::TupleType(TupleType { telescope, .. }) => {
            fn walk(
                cur: Telescope<()>,
                depth: usize,
                total: usize,
                idx: usize,
                items: &mut Vec<Printer>,
                spelling: &Rc<Spelling>,
            ) {
                match cur {
                    Telescope::Done(_) => {}
                    Telescope::Cons(ty, rest) => {
                        let raw = rest.binder(0);
                        let label = binder_or(raw, depth + idx);
                        // As in the `FuncType` printer: an unnameable label nothing references is elided, so the field renders the way source wrote it.
                        let named = match raw {
                            Some(name) => name.hint().is_some() || rest.uses(0),
                            None => false,
                        };
                        let typed = sub(ty, depth + total, spelling);
                        let printer = if named {
                            flat([pure(spelling.label(&label)), pure(": "), typed])
                        } else {
                            typed
                        };
                        items.push(indent(printer));
                        let next = rest.open(&[&Term::free_var(&label)]);
                        walk(next, depth, total, idx + 1, items, spelling);
                    }
                }
            }

            let n = telescope.len();
            let mut items = Vec::with_capacity(n);
            walk(telescope, depth, n, 0, &mut items, spelling);

            // Through `listed` like every other sequence, rather than the hand-rolled always-broken leading-comma form this used to carry: a goal report naming a tuple type is read by a person, and `{a : A, b : B}` on one line is what `documentation/SYNTAX.md` spells. Unspaced for the same reason the surface printer is.
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
                        Some(name) => flat([pure(name), pure(" = "), sub(f, depth, spelling)]),
                        None => sub(f, depth, spelling),
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
            flat([pure("("), sub(head, depth, spelling), pure(field)])
        }
        // Params then indices, one flat argument list — exactly how the type-constructor function is applied at use sites, and marked the same way. Without the marks this spells `Eq(Nat, 5, 5)`, three positional arguments where `Eq(@A : Type) : (A, A) -> Prop` accepts two: a rendering no use site could reproduce.
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            indices,
        }) => {
            let arity = params.len() + indices.len();
            let marks = spelling.nominal_marks(&name, arity);
            let label = format!(
                "{}{}",
                spelling.symbol(&name),
                universe_suffix(&universes, spelling)
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
                            marked_argument(
                                sub(p, depth, spelling),
                                marks.and_then(|marks| marks.get(index)),
                            )
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
                spelling.symbol(&name),
                universe_suffix(&universes, spelling)
            );
            if payload.is_empty() {
                pure(format!("{name}/{tag}"))
            } else {
                listed(
                    format!("{name}/{tag}("),
                    false,
                    payload
                        .into_iter()
                        .map(|p| sub(p, depth, spelling))
                        .collect(),
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
            let marks = spelling.nominal_marks(&name, params.len());
            let label = format!(
                "{}{}",
                spelling.symbol(&name),
                universe_suffix(&universes, spelling)
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
                            marked_argument(
                                sub(p, depth, spelling),
                                marks.and_then(|marks| marks.get(index)),
                            )
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
                spelling.symbol(&name),
                universe_suffix(&universes, spelling)
            ),
            true,
            fields
                .into_iter()
                .map(|f| sub(f, depth, spelling))
                .collect(),
            "}",
        ),
        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => {
            // Arity 1 everywhere except an annotated inductive-match motive, whose pattern binders precede the scrutinee binder.
            let motive_labels = scope_labels(motive.binder_iter(), depth);
            let motive_terms = label_terms(&motive_labels);
            let motive_refs = motive_terms.iter().collect::<Vec<_>>();
            let motive_arity = motive_labels.len();
            let motive_label = motive_labels
                .iter()
                .map(|label| spelling.label(label))
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
                    Carrier::Lst { .. } => "Lst.fold ",
                },
            };

            let prefix = flat([
                pure(keyword),
                sub(head, depth, spelling),
                pure(": "),
                pure(motive_label),
                pure(" => "),
                sub(motive, depth + motive_arity, spelling),
                pure(";"),
            ]);

            let arms = match cases {
                Cases::Bool {
                    false_case,
                    true_case,
                } => flat([
                    pure("\n| false =>\n"),
                    indent(flat([sub(false_case, depth, spelling), pure(";")])),
                    pure("\n| true =>\n"),
                    indent(flat([sub(true_case, depth, spelling), pure(";")])),
                ]),
                Cases::Switch { cases, default } => {
                    let case_printers = flat(
                        cases
                            .into_iter()
                            .map(|(n, body)| {
                                flat([
                                    pure(format!("\n| {n}n =>\n")),
                                    indent(flat([sub(body, depth, spelling), pure(";")])),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    );
                    flat([
                        case_printers,
                        pure("\n| _ =>\n"),
                        indent(flat([sub(default, depth, spelling), pure(";")])),
                    ])
                }
                Cases::Induct { cases, default, .. } => {
                    let case_printers = flat(
                        cases
                            .into_iter()
                            .map(|(atom, arm)| {
                                let labels = scope_labels(arm.binder_iter(), depth);
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
                                                let mark = match arm.plicities.get(idx) {
                                                    Some(Plicity::Implicit) => "@",
                                                    Some(Plicity::Witness) => "use ",
                                                    _ => "",
                                                };
                                                format!("{mark}{}", spelling.label(l))
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
                                    indent(flat([
                                        sub(body, depth + labels.len(), spelling),
                                        pure(";"),
                                    ])),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    );
                    match default {
                        Some(default) => flat([
                            case_printers,
                            pure("\n| _ =>\n"),
                            indent(flat([sub(default, depth, spelling), pure(";")])),
                        ]),
                        None => case_printers,
                    }
                }
                Cases::FreeMonoid { carrier } => {
                    // The cons arm mirrors each carrier's own literal delimiters: `b[head, ..tail]; ih` for `Bin`, `[head, ..tail]; ih` for `Lst` — the same bracketed shape, told apart by the grain letter.
                    let cons_bin = |grain: Grain, cons_case: Scope<Three>| {
                        let ((head_label, tail_label, ih_label), cons_case) =
                            open_scope_three(cons_case, depth);
                        flat([
                            pure(match grain {
                                Grain::B => "\n| b[",
                                Grain::X => "\n| x[",
                            }),
                            pure(spelling.label(&head_label)),
                            pure(", .."),
                            pure(spelling.label(&tail_label)),
                            pure("]; "),
                            pure(spelling.label(&ih_label)),
                            pure(" =>\n"),
                            indent(flat([sub(cons_case, depth, spelling), pure(";")])),
                        ])
                    };
                    let cons_lst = |cons_case: Scope<Three>| {
                        let ((head_label, tail_label, ih_label), cons_case) =
                            open_scope_three(cons_case, depth);
                        flat([
                            pure("\n| ["),
                            pure(spelling.label(&head_label)),
                            pure(", .."),
                            pure(spelling.label(&tail_label)),
                            pure("]; "),
                            pure(spelling.label(&ih_label)),
                            pure(" =>\n"),
                            indent(flat([sub(cons_case, depth, spelling), pure(";")])),
                        ])
                    };

                    // Per carrier: the identity arm's literal, its body, and the cons arm — which binds `(predecessor, ih)` for the head-less unary `Nat`, and `(head, tail), ih` for `Bin`/`Lst`.
                    let (empty_lit, empty_case, cons_arm) = match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => {
                            let ((pred_label, ih_label), cons_case) =
                                open_scope_two(cons_case, depth);
                            let cons_arm = flat([
                                pure("\n| "),
                                pure(spelling.label(&pred_label)),
                                pure(" "),
                                pure(spelling.label(&ih_label)),
                                pure(" =>\n"),
                                indent(flat([sub(cons_case, depth, spelling), pure(";")])),
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
                        Carrier::Lst {
                            empty_case,
                            cons_case,
                            ..
                        } => ("\n| [] =>\n", empty_case, cons_lst(cons_case)),
                    };
                    flat([
                        pure(empty_lit),
                        indent(flat([sub(empty_case, depth, spelling), pure(";")])),
                        cons_arm,
                    ])
                }
            };

            flat([prefix, arms])
        }
        Subterm::Let(Let { bindings, tail, .. }) => {
            let labels = scope_labels(tail.binder_iter(), depth);
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
                        pure(spelling.label(&labels[index])),
                        pure(": "),
                        sub(type_, depth + index, spelling),
                        pure(" =\n"),
                        indent(flat([sub(value, depth + index, spelling), pure(";")])),
                        pure("\n"),
                    ])
                })
                .collect::<Vec<_>>();

            flat([
                flat(lines),
                sub(tail.open(&label_terms), depth + labels.len(), spelling),
            ])
        }
        Subterm::Rec(Rec { group, tail }) => {
            let labels = scope_labels(tail.binder_iter(), depth);
            let label_terms = label_terms(&labels);
            let label_terms = label_terms.iter().collect::<Vec<_>>();
            let inner_depth = depth + labels.len();

            let bindings = group
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, member)| {
                    let type_ = member.type_.open(&label_terms);
                    let body = member.body.open(&label_terms);

                    flat([
                        pure(spelling.label(&labels[index])),
                        pure(": "),
                        sub(type_, inner_depth, spelling),
                        pure(" =\n"),
                        indent(sub(body, inner_depth, spelling)),
                    ])
                })
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                sub(tail, inner_depth, spelling),
            ])
        }
        Subterm::Var(var) => print_var(var, spelling),
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
            print_infix(op.symbol(), left, right, depth, spelling)
        }
        // A `!` sequencing site prints as the written bang followed by its hoisted continuation, so a lowered-stage dump reads close to the source region.
        Subterm::Transient(Transient::Bang(Bang {
            action,
            continuation,
        })) => flat([
            sub(action.clone(), depth, spelling),
            pure("!; "),
            sub(continuation.clone(), depth, spelling),
        ]),
        // Identity and renaming spines (every entry a variable) are the uninteresting common case and print as the bare id; a spine carrying anything else is exactly the one worth seeing.
        Subterm::Metavar(metavar) => {
            if metavar
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
                            .map(|entry| sub(entry.clone(), depth, spelling))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure("]"),
                ])
            }
        }
    }
}
