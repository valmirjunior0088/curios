use {
    super::{
        Apply, Arity, Atom, Cases, Definition, Field, Flt, Func, FuncType, Item, Let, Match,
        Module, Nat, One, Plicity, Prim, Proj, Rec, Scope, Struct, StructType, Subterm, Telescope,
        Term, Tuple, TupleType, Two, UnionType, Var, Variant,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::{
        cell::RefCell,
        collections::{BTreeSet, HashMap},
        fmt::{Display, Formatter, Result},
        rc::Rc,
    },
};

// === Source-style names (diagnostics) ========================================
//
// Core spells names for the kernel's convenience, not the reader's: every binder
// is opened under a `Context::fresh` gensym (`n#15`, `(n#0 : Nat) -> …`) and
// every global is its fully-qualified canonical path (`std/Vec/Vec`, `sys/Nat`).
// Two thread-local maps, installed only while a diagnostic (or `Module`) renders,
// rewrite both back toward what the user wrote; the faithful `Display` for a bare
// term leaves names untouched.
//
//   axis (a) — local binders: a *rename map* (`with_pretty_names`, built by
//     `build_rename` over `display_names`) alpha-renames the whole fragment —
//     free vars *and* binder labels. A source hint is used bare when unique;
//     distinct names sharing a hint, or shadowing a literally-rendered name, take
//     minimal `hint2`, `hint3`, … suffixes, so no two binders ever read alike.
//
//   axis (b) — globals: a *shorten map* (`with_short_names`, built by
//     `build_shorten` over `module_symbols`) replaces each qualified path with
//     its shortest unambiguous `/`-suffix — the name in scope, since Curios has
//     no `use … as` aliasing. Installed by error rendering *and* `Module` display.
//
// `display_label` consults the shorten map first (globals), then the rename map
// (locals); a name in neither renders verbatim.

thread_local! {
    /// Local binders → source-name renaming (axis (a)); installed by error
    /// rendering only.
    static PRETTY: RefCell<Option<Rc<HashMap<String, String>>>> = const { RefCell::new(None) };
    /// Global qualified names → their shortest in-scope spelling (axis (b));
    /// installed by both error rendering and `Module` display.
    static SHORTEN: RefCell<Option<Rc<HashMap<String, String>>>> = const { RefCell::new(None) };
}

/// Install a pretty-name rename map for the duration of `f`, restoring the
/// previous state afterwards so the faithful `Display` paths are unaffected.
pub fn with_pretty_names<R>(rename: Rc<HashMap<String, String>>, f: impl FnOnce() -> R) -> R {
    let prev = PRETTY.with(|p| p.borrow_mut().replace(rename));
    let result = f();
    PRETTY.with(|p| *p.borrow_mut() = prev);
    result
}

/// Install a global-shortening map for the duration of `f`.
pub fn with_short_names<R>(shorten: Rc<HashMap<String, String>>, f: impl FnOnce() -> R) -> R {
    let prev = SHORTEN.with(|s| s.borrow_mut().replace(shorten));
    let result = f();
    SHORTEN.with(|s| *s.borrow_mut() = prev);
    result
}

/// Strip a `fresh`-generated `hint#counter` name down to its source `hint`.
/// User-written names carry no `#`; a counter-only `#7` (an unnamed binder) has
/// an empty hint and is left as-is.
fn strip_fresh(raw: &str) -> String {
    match raw.split_once('#') {
        Some((hint, _)) if !hint.is_empty() => hint.to_string(),
        _ => raw.to_string(),
    }
}

/// The display spelling of a name. A global with a shorter in-scope spelling
/// takes it (axis (b)); otherwise a local binder gets its pretty rename (axis
/// (a), with a `strip_fresh` fallback). A name in neither map renders verbatim.
fn display_label(raw: &str) -> String {
    if let Some(short) = SHORTEN.with(|s| s.borrow().as_ref().and_then(|m| m.get(raw).cloned())) {
        return short;
    }
    PRETTY.with(|p| match &*p.borrow() {
        None => raw.to_string(),
        Some(map) => map.get(raw).cloned().unwrap_or_else(|| strip_fresh(raw)),
    })
}

/// Every name the printer will emit for `term`: free vars (Γ references) and the
/// stored labels of every binder it reopens. Free vars come from the robust
/// `Bound` traversal; binder labels — which that traversal never surfaces — from
/// [`collect_labels`], which descends scope bodies (where nested binders live).
pub fn display_names(term: &Term) -> BTreeSet<String> {
    let mut names = term.free_vars();
    collect_labels(term, &mut names);
    names
}

/// Collect every binder label in `term`, recursing through scope and telescope
/// bodies. (`Prim` interiors are skipped: they hold no binders the diagnostics
/// need to name, and any free vars there are already in `free_vars`.)
fn collect_labels(term: &Term, out: &mut BTreeSet<String>) {
    fn push(out: &mut BTreeSet<String>, label: Option<&str>) {
        if let Some(label) = label.filter(|l| !l.is_empty()) {
            out.insert(label.to_string());
        }
    }

    fn scope<A: Arity>(out: &mut BTreeSet<String>, scope: &Scope<A>) {
        if let Some(names) = scope.names() {
            names.iter().for_each(|n| push(out, Some(n)));
        }
        collect_labels(scope.body(), out);
    }

    fn telescope(out: &mut BTreeSet<String>, mut cur: &Telescope<Term>) {
        loop {
            match cur {
                Telescope::Cons(ty, rest) => {
                    push(out, rest.first_label());
                    collect_labels(ty, out);
                    cur = rest.body();
                }
                Telescope::Done(body) => break collect_labels(body, out),
            }
        }
    }

    // A tuple type's telescope has no result term (`Telescope<()>`); only its
    // field types and labels carry names.
    fn tuple_telescope(out: &mut BTreeSet<String>, mut cur: &Telescope<()>) {
        while let Telescope::Cons(ty, rest) = cur {
            push(out, rest.first_label());
            collect_labels(ty, out);
            cur = rest.body();
        }
    }

    let each = |out: &mut BTreeSet<String>, terms: &[Term]| {
        terms.iter().for_each(|t| collect_labels(t, out))
    };

    match &**term {
        Subterm::FuncType(FuncType { telescope: t, .. }) => telescope(out, t),
        Subterm::Func(Func { telescope: t }) => telescope(out, t),
        Subterm::TupleType(TupleType { telescope: t }) => tuple_telescope(out, t),
        Subterm::Apply(Apply { head, params, .. }) => {
            collect_labels(head, out);
            each(out, params);
        }
        Subterm::Tuple(Tuple { fields, .. }) => each(out, fields),
        Subterm::Proj(Proj { head, .. }) => collect_labels(head, out),
        Subterm::UnionType(UnionType {
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
        Subterm::Let(Let { type_, body, tail }) => {
            collect_labels(type_, out);
            collect_labels(body, out);
            scope(out, tail);
        }
        Subterm::Rec(Rec { items, tail }) => {
            for (type_, value) in items {
                scope(out, type_);
                scope(out, value);
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
                Cases::Bln {
                    false_case,
                    true_case,
                } => {
                    collect_labels(false_case, out);
                    collect_labels(true_case, out);
                }
                Cases::Nat {
                    zero_case,
                    succ_case,
                } => {
                    collect_labels(zero_case, out);
                    scope(out, succ_case);
                }
                Cases::Switch { cases, default } => {
                    cases.iter().for_each(|(_, body)| collect_labels(body, out));
                    collect_labels(default, out);
                }
                Cases::Union { cases, .. } => cases.iter().for_each(|(_, s)| scope(out, s)),
            }
        }
        Subterm::Metavar(metavar) => metavar.spine.iter().for_each(|t| collect_labels(t, out)),
        Subterm::Var(_) | Subterm::Type | Subterm::Prim(_) => {}
    }
}

/// Assign every `hint#counter` name a clean display spelling: its source `hint`,
/// or `hint2`, `hint3`, … when several distinct names — binders *or* free vars —
/// would otherwise collide, or would shadow a name that renders literally
/// (globals, already-clean labels). The result is unambiguous by construction,
/// so no rendered name is ever silently shared between two binders.
pub fn build_rename(names: &BTreeSet<String>) -> HashMap<String, String> {
    // `names` is sorted, so the assignment below is deterministic.
    let prettifiable = names
        .iter()
        .filter(|n| matches!(n.split_once('#'), Some((hint, _)) if !hint.is_empty()))
        .collect::<Vec<_>>();

    // Names that render as themselves reserve their spelling up front.
    let mut used = names
        .iter()
        .filter(|n| !prettifiable.contains(n))
        .cloned()
        .collect::<BTreeSet<_>>();

    let mut map = HashMap::new();
    for raw in prettifiable {
        let hint = strip_fresh(raw);
        let mut candidate = hint.clone();
        let mut next = 2;
        while used.contains(&candidate) {
            candidate = format!("{hint}{next}");
            next += 1;
        }
        used.insert(candidate.clone());
        map.insert(raw.clone(), candidate);
    }
    map
}

/// Every global qualified name in `module`: each definition (`let`/`rec`), each
/// union type, each struct type. The universe a global is shortened *against*.
pub fn module_symbols(module: &Module) -> Vec<String> {
    let mut symbols = Vec::new();
    for item in &module.items {
        match item {
            Item::Let(def) => symbols.push(def.name.clone()),
            Item::Rec(defs) => symbols.extend(defs.iter().map(|def| def.name.clone())),
        }
    }
    symbols.extend(module.inductives.keys().cloned());
    symbols.extend(module.structures.keys().cloned());
    symbols
}

/// Map each global to the shortest `/`-suffix of its path that no other global
/// shares — the name it has in scope, since Curios has no `use … as` aliasing,
/// so an in-scope name is always a suffix. Only entries that actually shorten
/// are recorded; an ambiguous (or single-segment) name keeps its full path.
pub fn build_shorten(symbols: &[String]) -> HashMap<String, String> {
    // One global can be listed twice (a union is both an `inductives` registry
    // key and an `items` type-constructor definition); count distinct names, or
    // such a name would look ambiguous with itself and never shorten.
    let symbols = symbols.iter().map(String::as_str).collect::<BTreeSet<_>>();

    let suffixes = |sym: &str| -> Vec<String> {
        let segments = sym.split('/').collect::<Vec<_>>();
        (1..=segments.len())
            .map(|k| segments[segments.len() - k..].join("/"))
            .collect()
    };

    // How many distinct globals carry each segment-suffix.
    let mut count: HashMap<String, usize> = HashMap::new();
    for sym in &symbols {
        for suffix in suffixes(sym) {
            *count.entry(suffix).or_insert(0) += 1;
        }
    }

    let mut map = HashMap::new();
    for sym in &symbols {
        if let Some(shortest) = suffixes(sym)
            .into_iter()
            .find(|suffix| count.get(suffix) == Some(&1))
            && shortest.len() < sym.len()
        {
            map.insert(sym.to_string(), shortest);
        }
    }
    map
}

fn label_at(depth: usize) -> String {
    format!("#{depth}")
}

fn label_terms(labels: &[String]) -> Vec<Term> {
    labels.iter().map(Var::free).map(Term::var).collect()
}

fn open_scope_one(scope: Scope<One>, depth: usize) -> (String, Term) {
    let label = scope
        .first_label()
        .map(str::to_string)
        .unwrap_or_else(|| label_at(depth));
    let body = scope.open(&[&Term::var(Var::free(&label))]);

    (label, body)
}

fn open_telescope(telescope: Telescope<Term>, depth: usize) -> (Vec<String>, Term) {
    fn walk(cur: Telescope<Term>, depth: usize, idx: usize, labels: &mut Vec<String>) -> Term {
        match cur {
            Telescope::Done(body) => *body,
            Telescope::Cons(_ty, rest) => {
                let label = rest
                    .first_label()
                    .map(str::to_string)
                    .unwrap_or_else(|| label_at(depth + idx));
                let next = rest.open(&[&Term::var(Var::free(&label))]);
                labels.push(label);
                walk(next, depth, idx + 1, labels)
            }
        }
    }

    let mut labels = Vec::new();
    let body = walk(telescope, depth, 0, &mut labels);
    (labels, body)
}

fn open_scope_two(scope: Scope<Two>, depth: usize) -> ((String, String), Term) {
    let fst = scope
        .first_label()
        .map(str::to_string)
        .unwrap_or_else(|| label_at(depth));
    let snd = scope
        .second_label()
        .map(str::to_string)
        .unwrap_or_else(|| label_at(depth + 1));
    let body = scope.open(&[&Term::var(Var::free(&fst)), &Term::var(Var::free(&snd))]);

    ((fst, snd), body)
}

fn print_var(var: Var) -> Printer<'static> {
    pure(display_label(var.unwrap()))
}

fn print_atom(atom: Atom) -> Printer<'static> {
    flat([pure("'"), pure(atom.as_string())])
}

fn print_flt(flt: Flt) -> Printer<'static> {
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

fn print_prim(prim: Prim, depth: usize) -> Printer<'static> {
    match prim {
        Prim::BlnType => pure("Bln"),
        Prim::Bln(false) => pure("false"),
        Prim::Bln(true) => pure("true"),
        Prim::BlnAnd(left, right) => flat([
            pure("Bln.and "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::BlnOr(left, right) => flat([
            pure("Bln.or "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::BlnXor(left, right) => flat([
            pure("Bln.xor "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatType => pure("Nat"),
        Prim::Nat(Nat::Zero) => pure("0"),
        Prim::Nat(Nat::Succ(spine, inner)) => match inner.as_ref() {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => pure(format!("{spine}")),
            inner => {
                if spine == num_bigint::BigUint::from(1usize) {
                    flat([
                        pure("Nat.succ("),
                        print_term(inner.clone().into(), depth),
                        pure(")"),
                    ])
                } else {
                    flat([
                        pure(format!("Nat.succ({spine}, ")),
                        print_term(inner.clone().into(), depth),
                        pure(")"),
                    ])
                }
            }
        },
        Prim::NatEql(left, right) => flat([
            pure("Nat.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatNeq(left, right) => flat([
            pure("Nat.neq "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatAdd(left, right) => flat([
            pure("Nat.add "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatSub(left, right) => flat([
            pure("Nat.sub "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatMul(left, right) => flat([
            pure("Nat.mul "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatLt(left, right) => flat([
            pure("Nat.lt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatDiv(left, right) => flat([
            pure("Nat.div "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatRem(left, right) => flat([
            pure("Nat.rem "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatGt(left, right) => flat([
            pure("Nat.gt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatLte(left, right) => flat([
            pure("Nat.lte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatGte(left, right) => flat([
            pure("Nat.gte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatAnd(left, right) => flat([
            pure("Nat.and "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatOr(left, right) => flat([
            pure("Nat.or "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatXor(left, right) => flat([
            pure("Nat.xor "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatShl(left, right) => flat([
            pure("Nat.shl "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatShr(left, right) => flat([
            pure("Nat.shr "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatToStr(inner) => flat([pure("Nat.to_str "), print_term(inner, depth)]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{value:+}")),
        Prim::IntEql(left, right) => flat([
            pure("Int.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntNeq(left, right) => flat([
            pure("Int.neq "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntAdd(left, right) => flat([
            pure("Int.add "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntSub(left, right) => flat([
            pure("Int.sub "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntMul(left, right) => flat([
            pure("Int.mul "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntDiv(left, right) => flat([
            pure("Int.div "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntRem(left, right) => flat([
            pure("Int.rem "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntLt(left, right) => flat([
            pure("Int.lt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntGt(left, right) => flat([
            pure("Int.gt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntLte(left, right) => flat([
            pure("Int.lte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntGte(left, right) => flat([
            pure("Int.gte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntAnd(left, right) => flat([
            pure("Int.and "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntOr(left, right) => flat([
            pure("Int.or "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntXor(left, right) => flat([
            pure("Int.xor "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntShl(left, right) => flat([
            pure("Int.shl "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntShr(left, right) => flat([
            pure("Int.shr "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntToStr(inner) => flat([pure("Int.to_str "), print_term(inner, depth)]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(flt) => print_flt(flt),
        Prim::FltAdd(left, right) => flat([
            pure("Flt.add "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltSub(left, right) => flat([
            pure("Flt.sub "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltMul(left, right) => flat([
            pure("Flt.mul "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltDiv(left, right) => flat([
            pure("Flt.div "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltEql(left, right) => flat([
            pure("Flt.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltNeq(left, right) => flat([
            pure("Flt.neq "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltLt(left, right) => flat([
            pure("Flt.lt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltGt(left, right) => flat([
            pure("Flt.gt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltLte(left, right) => flat([
            pure("Flt.lte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltGte(left, right) => flat([
            pure("Flt.gte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltMin(left, right) => flat([
            pure("Flt.min "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltMax(left, right) => flat([
            pure("Flt.max "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltNeg(inner) => flat([pure("Flt.neg "), print_term(inner, depth)]),
        Prim::FltAbs(inner) => flat([pure("Flt.abs "), print_term(inner, depth)]),
        Prim::FltSqrt(inner) => flat([pure("Flt.sqrt "), print_term(inner, depth)]),
        Prim::FltFloor(inner) => flat([pure("Flt.floor "), print_term(inner, depth)]),
        Prim::FltCeil(inner) => flat([pure("Flt.ceil "), print_term(inner, depth)]),
        Prim::FltTrunc(inner) => flat([pure("Flt.trunc "), print_term(inner, depth)]),
        Prim::FltNearest(inner) => flat([pure("Flt.nearest "), print_term(inner, depth)]),
        Prim::FltToStr(inner) => flat([pure("Flt.to_str "), print_term(inner, depth)]),
        Prim::FltToLeBin(inner) => flat([pure("Flt.to_le_bin "), print_term(inner, depth)]),
        Prim::NatToInt(inner) => flat([pure("Nat.to_int "), print_term(inner, depth)]),
        Prim::NatToFlt(inner) => flat([pure("Nat.to_flt "), print_term(inner, depth)]),
        Prim::IntToNat(inner) => flat([pure("Int.to_nat "), print_term(inner, depth)]),
        Prim::IntToFlt(inner) => flat([pure("Int.to_flt "), print_term(inner, depth)]),
        Prim::FltToNat(inner) => flat([pure("Flt.to_nat "), print_term(inner, depth)]),
        Prim::FltToInt(inner) => flat([pure("Flt.to_int "), print_term(inner, depth)]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        Prim::BinLen(bin) => flat([pure("Bin.len "), print_term(bin, depth)]),
        Prim::BinEql(left, right) => flat([
            pure("Bin.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::BinGet(bin, index) => flat([
            pure("Bin.get "),
            print_term(bin, depth),
            pure(" "),
            print_term(index, depth),
        ]),
        Prim::BinSlice(bin, start, end) => flat([
            pure("Bin.slice "),
            print_term(bin, depth),
            pure(" "),
            print_term(start, depth),
            pure(" "),
            print_term(end, depth),
        ]),
        Prim::BinAppend(bin, byte) => flat([
            pure("Bin.append "),
            print_term(bin, depth),
            pure(" "),
            print_term(byte, depth),
        ]),
        Prim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(
                operands.into_iter().map(move |e| print_term(e, depth)),
                || pure(", "),
            ),
        ]),
        Prim::StrType => pure("Str"),
        Prim::Str(bytes) => pure(format!("{:?}", String::from_utf8_lossy(bytes.as_slice()))),
        Prim::StrToBin(str) => flat([pure("Str.to_bin "), print_term(str, depth)]),
        Prim::StrOfBin(bin) => flat([pure("Str.of_bin "), print_term(bin, depth)]),
        Prim::ArrType(elem) => flat([pure("Arr "), print_term(elem, depth)]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(elems.into_iter().map(move |e| print_term(e, depth)), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        Prim::ArrLen(ty, list) => flat([
            pure("Arr.len "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
        ]),
        Prim::ArrGet(ty, list, index) => flat([
            pure("Arr.get "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
            pure(" "),
            print_term(index, depth),
        ]),
        Prim::ArrSlice(ty, list, start, end) => flat([
            pure("Arr.slice "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
            pure(" "),
            print_term(start, depth),
            pure(" "),
            print_term(end, depth),
        ]),
        Prim::ArrAppend(ty, list, elem) => flat([
            pure("Arr.append "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
            pure(" "),
            print_term(elem, depth),
        ]),
        Prim::ArrConcat(ty, operands) => flat([
            pure("Arr.concat "),
            print_term(ty, depth),
            pure(" "),
            sep_flat(
                operands.into_iter().map(move |e| print_term(e, depth)),
                || pure(", "),
            ),
        ]),
        Prim::IoType => pure("Io"),
        Prim::Io(token) => pure(format!("Io({token})")),
        Prim::IoRead(handle, count) => flat([
            pure("Io.read "),
            print_term(handle, depth),
            pure(" "),
            print_term(count, depth),
        ]),
        Prim::IoWrite(handle, bytes) => flat([
            pure("Io.write "),
            print_term(handle, depth),
            pure(" "),
            print_term(bytes, depth),
        ]),
        Prim::IoOpen(path, mode) => flat([
            pure("Io.open "),
            print_term(path, depth),
            pure(" "),
            print_term(mode, depth),
        ]),
        Prim::IoConnect(host, port, connect_timeout, read_timeout, write_timeout) => flat([
            pure("Io.connect "),
            print_term(host, depth),
            pure(" "),
            print_term(port, depth),
            pure(" "),
            print_term(connect_timeout, depth),
            pure(" "),
            print_term(read_timeout, depth),
            pure(" "),
            print_term(write_timeout, depth),
        ]),
        Prim::IoListen(host, port) => flat([
            pure("Io.listen "),
            print_term(host, depth),
            pure(" "),
            print_term(port, depth),
        ]),
        Prim::IoAccept(handle) => flat([pure("Io.accept "), print_term(handle, depth)]),
        Prim::IoClose(handle) => flat([pure("Io.close "), print_term(handle, depth)]),
        Prim::IoClockWall => pure("Io.clock_wall"),
        Prim::IoClockMono => pure("Io.clock_mono"),
        Prim::IoRandom(count) => flat([pure("Io.random "), print_term(count, depth)]),
        Prim::IoArgs => pure("Io.args"),
        Prim::IoEnv(name) => flat([pure("Io.env "), print_term(name, depth)]),
        Prim::IoExit(type_, code) => flat([
            pure("Io.exit "),
            print_term(type_, depth),
            pure(" "),
            print_term(code, depth),
        ]),
    }
}

fn print_term(term: Term, depth: usize) -> Printer<'static> {
    match Term::unwrap_or_clone(term) {
        Subterm::Type => pure("Type"),
        Subterm::Prim(prim) => print_prim(prim, depth),
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
                printers: &mut Vec<Printer<'static>>,
            ) -> Term {
                match cur {
                    Telescope::Done(body) => *body,
                    Telescope::Cons(ty, rest) => {
                        let raw = rest.first_label();
                        let label = raw
                            .map(str::to_string)
                            .unwrap_or_else(|| label_at(depth + idx));
                        let mark = match plicities.get(idx) {
                            Some(Plicity::Implicit) => "@",
                            _ => "",
                        };
                        let printer = match raw {
                            Some(_) => flat([
                                pure(mark),
                                pure(display_label(&label)),
                                pure(" : "),
                                print_term(ty, depth + total),
                            ]),
                            None => flat([pure(mark), print_term(ty, depth + total)]),
                        };
                        printers.push(printer);
                        let next = rest.open(&[&Term::var(Var::free(&label))]);
                        walk(next, plicities, depth, total, idx + 1, printers)
                    }
                }
            }

            let n = telescope.len();
            let mut printers = Vec::with_capacity(n);
            let output = walk(telescope, &plicities, depth, n, 0, &mut printers);
            flat([
                pure("("),
                sep_flat(printers, || pure(", ")),
                pure(") -> "),
                print_term(output, depth + n),
            ])
        }
        Subterm::Func(Func { telescope }) => {
            let n = telescope.len();
            let (labels, body) = open_telescope(telescope, depth);
            let param_str = if labels.len() == 1 {
                display_label(&labels[0])
            } else {
                format!(
                    "({})",
                    labels
                        .iter()
                        .map(|l| display_label(l))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            flat([
                pure(param_str),
                pure(" =>\n"),
                indent(print_term(body, depth + n)),
            ])
        }
        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => flat([
            print_term(head, depth),
            pure("("),
            sep_flat(
                params
                    .into_iter()
                    .zip(plicities)
                    .map(|(p, plicity)| match plicity {
                        Plicity::Implicit => flat([pure("@"), print_term(p, depth)]),
                        Plicity::Explicit => print_term(p, depth),
                    })
                    .collect::<Vec<_>>(),
                || pure(", "),
            ),
            pure(")"),
        ]),
        Subterm::TupleType(TupleType { telescope }) => {
            fn walk(
                cur: Telescope<()>,
                depth: usize,
                total: usize,
                idx: usize,
                items: &mut Vec<Printer<'static>>,
            ) {
                match cur {
                    Telescope::Done(_) => {}
                    Telescope::Cons(ty, rest) => {
                        let label = rest
                            .first_label()
                            .map(str::to_string)
                            .unwrap_or_else(|| label_at(depth + idx));
                        items.push(indent(flat([
                            pure(display_label(&label)),
                            pure(" : "),
                            print_term(ty, depth + total),
                        ])));
                        let next = rest.open(&[&Term::var(Var::free(&label))]);
                        walk(next, depth, total, idx + 1, items);
                    }
                }
            }

            let n = telescope.len();
            let mut items = Vec::with_capacity(n);
            walk(telescope, depth, n, 0, &mut items);

            flat([pure("{ "), sep_flat(items, || pure("\n, ")), pure("\n}")])
        }
        Subterm::Tuple(Tuple { fields, names }) => {
            let mut names = names.into_iter().chain(std::iter::repeat(None));
            flat([
                pure("("),
                sep_flat(
                    fields
                        .into_iter()
                        .map(move |f| match names.next().flatten() {
                            Some(name) => flat([pure(name), pure(" = "), print_term(f, depth)]),
                            None => print_term(f, depth),
                        }),
                    || pure(", "),
                ),
                pure(")"),
            ])
        }
        Subterm::Proj(Proj { head, field }) => {
            let field = match field {
                Field::Index(index) => format!(").{index}"),
                Field::Label(label) => format!(").{label}"),
            };
            flat([pure("("), print_term(head, depth), pure(field)])
        }
        // Params then indices, one flat argument list — exactly how the
        // type-constructor function is applied at use sites.
        Subterm::UnionType(UnionType {
            name,
            params,
            indices,
        }) => {
            if params.is_empty() && indices.is_empty() {
                pure(display_label(&name))
            } else {
                flat([
                    pure(display_label(&name)),
                    pure("("),
                    sep_flat(
                        params
                            .into_iter()
                            .chain(indices)
                            .map(|p| print_term(p, depth))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure(")"),
                ])
            }
        }
        // Prints as the constructor-function call, instantiated type params
        // hidden — `Result/success(42)`.
        Subterm::Variant(Variant {
            name, tag, payload, ..
        }) => {
            if payload.is_empty() {
                pure(format!("{}/{tag}", display_label(&name)))
            } else {
                flat([
                    pure(format!("{}/{tag}", display_label(&name))),
                    pure("("),
                    sep_flat(
                        payload
                            .into_iter()
                            .map(|p| print_term(p, depth))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure(")"),
                ])
            }
        }
        // Like `UnionType` but with no indices: `Pair(Nat, Bin)`.
        Subterm::StructType(StructType { name, params }) => {
            if params.is_empty() {
                pure(display_label(&name))
            } else {
                flat([
                    pure(display_label(&name)),
                    pure("("),
                    sep_flat(
                        params
                            .into_iter()
                            .map(|p| print_term(p, depth))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure(")"),
                ])
            }
        }
        // Prints as the brace literal, instantiated type params hidden —
        // `Pair { 0, "" }`.
        Subterm::Struct(Struct { name, fields, .. }) => flat([
            pure(format!("{} {{ ", display_label(&name))),
            sep_flat(
                fields
                    .into_iter()
                    .map(|f| print_term(f, depth))
                    .collect::<Vec<_>>(),
                || pure(", "),
            ),
            pure(" }"),
        ]),
        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => {
            // Arity 1 everywhere except an annotated union-match motive,
            // whose pattern binders precede the scrutinee binder.
            let motive_labels = motive
                .label_iter()
                .enumerate()
                .map(|(i, l)| l.map(str::to_string).unwrap_or_else(|| label_at(depth + i)))
                .collect::<Vec<_>>();
            let motive_terms = label_terms(&motive_labels);
            let motive_refs = motive_terms.iter().collect::<Vec<_>>();
            let motive_arity = motive_labels.len();
            let motive_label = motive_labels
                .iter()
                .map(|l| display_label(l))
                .collect::<Vec<_>>()
                .join(", ");
            let motive = motive.open(&motive_refs);

            // Shared `<keyword> head : label => motive;` prefix; the keyword
            // and arm bodies depend on the case kind.
            let keyword = match &cases {
                Cases::Bln { .. } => "Bln.match ",
                Cases::Nat { .. } => "Nat.fold ",
                Cases::Switch { .. } => "Nat.match ",
                Cases::Union { .. } => "match ",
            };

            let prefix = flat([
                pure(keyword),
                print_term(head, depth),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(motive, depth + motive_arity),
                pure(";"),
            ]);

            let arms = match cases {
                Cases::Bln {
                    false_case,
                    true_case,
                } => flat([
                    pure("\n| false =>\n"),
                    indent(flat([print_term(false_case, depth), pure(";")])),
                    pure("\n| true =>\n"),
                    indent(flat([print_term(true_case, depth), pure(";")])),
                ]),
                Cases::Nat {
                    zero_case,
                    succ_case,
                } => {
                    let ((pred_label, ih_label), succ_case) = open_scope_two(succ_case, depth);
                    flat([
                        pure("\n| 0n =>\n"),
                        indent(flat([print_term(zero_case, depth), pure(";")])),
                        pure("\n| "),
                        pure(display_label(&pred_label)),
                        pure(" "),
                        pure(display_label(&ih_label)),
                        pure(" =>\n"),
                        indent(flat([print_term(succ_case, depth), pure(";")])),
                    ])
                }
                Cases::Switch { cases, default } => {
                    let case_printers = flat(
                        cases
                            .into_iter()
                            .map(|(n, body)| {
                                flat([
                                    pure(format!("\n| {n}n =>\n")),
                                    indent(flat([print_term(body, depth), pure(";")])),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    );
                    flat([
                        case_printers,
                        pure("\n| _ =>\n"),
                        indent(flat([print_term(default, depth), pure(";")])),
                    ])
                }
                Cases::Union { cases, .. } => flat(
                    cases
                        .into_iter()
                        .map(|(atom, scope)| {
                            let labels = scope
                                .label_iter()
                                .enumerate()
                                .map(|(i, l)| {
                                    l.map(str::to_string).unwrap_or_else(|| label_at(depth + i))
                                })
                                .collect::<Vec<_>>();
                            let label_terms = label_terms(&labels);
                            let label_terms = label_terms.iter().collect::<Vec<_>>();
                            let body = scope.open(&label_terms);

                            let binders = if labels.is_empty() {
                                pure("")
                            } else {
                                pure(format!(
                                    "({})",
                                    labels
                                        .iter()
                                        .map(|l| display_label(l))
                                        .collect::<Vec<_>>()
                                        .join(", ")
                                ))
                            };

                            flat([
                                pure("\n| "),
                                print_atom(atom),
                                binders,
                                pure(" =>\n"),
                                indent(flat([print_term(body, depth + labels.len()), pure(";")])),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
            };

            flat([prefix, arms])
        }
        Subterm::Let(Let { type_, body, tail }) => {
            let (label, tail) = open_scope_one(tail, depth);

            flat([
                pure("let "),
                pure(display_label(&label)),
                pure(" : "),
                print_term(type_, depth),
                pure(" =\n"),
                indent(flat([print_term(body, depth), pure(";")])),
                pure("\n"),
                print_term(tail, depth + 1),
            ])
        }
        Subterm::Rec(Rec { items, tail }) => {
            let labels = tail
                .label_iter()
                .enumerate()
                .map(|(i, l)| l.map(str::to_string).unwrap_or_else(|| label_at(depth + i)))
                .collect::<Vec<_>>();
            let label_terms = label_terms(&labels);
            let label_terms = label_terms.iter().collect::<Vec<_>>();
            let inner_depth = depth + labels.len();

            let bindings = items
                .into_iter()
                .enumerate()
                .map(|(index, (type_, body))| {
                    let type_ = type_.open(&label_terms);
                    let body = body.open(&label_terms);

                    flat([
                        pure(display_label(&labels[index])),
                        pure(" : "),
                        print_term(type_, inner_depth),
                        pure(" =\n"),
                        indent(print_term(body, inner_depth)),
                    ])
                })
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                print_term(tail, inner_depth),
            ])
        }
        Subterm::Var(var) => print_var(var),
        // Identity and renaming spines (every entry a variable) are the
        // uninteresting common case and print as the bare id; a spine carrying
        // anything else is exactly the one worth seeing.
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
                            .map(|entry| print_term(entry.clone(), depth))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure("]"),
                ])
            }
        }
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone(), 0), formatter, 2)
    }
}

impl Display for Subterm {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone().into(), 0), formatter, 2)
    }
}

fn print_definition(formatter: &mut Formatter<'_>, def: &Definition) -> Result {
    write!(formatter, "{} : {} = {}", def.name, def.type_, def.body)
}

impl Display for Module {
    // Printed by *iterating* the flat items (never re-folding into a nested term),
    // so `--print core` stays O(N) and cannot re-trigger the prelude-depth
    // overflow this representation removed.
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        with_short_names(Rc::new(build_shorten(&module_symbols(self))), || {
            for item in &self.items {
                match item {
                    Item::Let(def) => {
                        write!(formatter, "let ")?;
                        print_definition(formatter, def)?;
                        writeln!(formatter, ";")?;
                    }
                    Item::Rec(defs) => {
                        write!(formatter, "rec ")?;
                        for (index, def) in defs.iter().enumerate() {
                            if index > 0 {
                                write!(formatter, "and ")?;
                            }
                            print_definition(formatter, def)?;
                            write!(formatter, " ")?;
                        }
                        writeln!(formatter, ";")?;
                    }
                }
            }

            write!(formatter, "{}", self.body)?;

            if let Some(type_) = &self.type_ {
                write!(formatter, "\n: {type_}")?;
            }

            Ok(())
        })
    }
}
