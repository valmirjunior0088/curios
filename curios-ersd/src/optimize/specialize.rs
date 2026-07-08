//! Specialization of recursive functions on literal constructor spines.
//!
//! [`evaluate`](super::evaluate) leaves one recurring shape behind: a
//! recursive fold applied to a **constant** inductive value it cannot enter —
//! `Fmt/go_with` over the parsed format-string spine, with runtime hole
//! arguments and an effect-capturing `finish`, is the motivating case. The
//! spine is data the compiler already computed; only the fold's *other*
//! arguments are runtime. This pass unrolls the recursion over that data by
//! **minting one specialized item per (function, argument position, literal)**:
//!
//! ```text
//! go_with(finish, (@0, " is ", (@1, (@2))), acc)   ⇒   go_with@s0(finish, acc)
//! ```
//!
//! A specialized body is the original body with the baked parameter
//! `Let`-bound to the literal — *binding, not substitution*, so no
//! alpha-renaming is ever needed — then folded in place: projections out of
//! the known tuple become the fields, the `match` on its tag becomes the taken
//! arm, and the recursive call, now passing a strictly smaller spine literal,
//! is rewritten to the next specialization. The chain is finite because every
//! mint keys on a strict subterm of its requester's literal; the caps below
//! are belt and braces. A recursive call whose spine argument does not fold to
//! a literal falls back to the generic function — always correct, never
//! required to fire.
//!
//! Downstream, the minted items are plain first-order definitions: cont's
//! single-site inlining flattens the chain and constant folding finishes the
//! job. Every rewritten body — minted or host — gets its closure captures
//! recomputed, since call-site rewrites change which names are free under
//! enclosing closures (`into_cont` threads globals through captures).

use {
    super::{
        Loc, children, clone_args, deep_copy, members, refresh_captures, refresh_touched, root_mut,
        term_at_mut,
    },
    crate::{
        Apply, Argument, Func, Item, Let, Module, Name, NatMatch, Prim, PurePrim, Subterm, Term,
    },
    std::{
        collections::{HashMap, HashSet},
        mem,
    },
};

/// Module-wide cap on minted items. A format spine of `k` directives mints
/// `~2k + 1`; this covers pathological format strings with room to spare.
const MAX_SPECIALIZATIONS: usize = 64;

/// A literal larger than this many nodes is not a specialization key — the
/// clones would repeat it wholesale.
const MAX_SPINE_NODES: usize = 256;

/// Specialize every eligible call site, module-wide.
pub(crate) fn specialize_literal_spines(module: &mut Module) {
    let plan = plan(module);
    apply(module, plan);
}

/// One call-site rewrite: swap the head for the specialization and drop the
/// baked argument.
struct Site {
    loc: Loc,
    path: Vec<usize>,
    spec: String,
    drop: usize,
}

/// One minted item, remembered with the target it specializes (it is inserted
/// right after that item, callees first).
struct Minted {
    target: usize,
    name: String,
    body: Term,
}

struct Plan {
    sites: Vec<Site>,
    minted: Vec<Minted>,
    spec_names: HashMap<String, bool>,
}

fn plan(module: &Module) -> Plan {
    let mut minter = Minter::new(module);
    let mut sites = Vec::new();

    for (index, item) in module.items.iter().enumerate() {
        for (member, term) in members(item).into_iter().enumerate() {
            let loc = Loc::Item {
                item: index,
                member,
            };
            find_sites(
                &mut minter,
                term,
                &mut Vec::new(),
                &mut Vec::new(),
                loc,
                &mut sites,
            );
        }
    }
    find_sites(
        &mut minter,
        &module.body,
        &mut Vec::new(),
        &mut Vec::new(),
        Loc::Body,
        &mut sites,
    );

    let spec_names = minter
        .minted
        .iter()
        .map(|minted| (minted.name.clone(), true))
        .collect();

    Plan {
        sites,
        minted: minter.minted,
        spec_names,
    }
}

fn apply(module: &mut Module, plan: Plan) {
    let Plan {
        mut sites,
        minted,
        spec_names,
    } = plan;

    // Descendant sites first: rewriting an ancestor `Apply` removes one of its
    // params, which would shift a descendant's recorded child index.
    sites.sort_by(|a, b| b.path.cmp(&a.path));

    let mut touched: HashSet<(usize, usize)> = HashSet::new();
    let mut touched_body = false;

    for site in sites {
        match site.loc {
            Loc::Item { item, member } => {
                touched.insert((item, member));
            }
            Loc::Body => touched_body = true,
        }
        let root = root_mut(module, site.loc);
        let call = term_at_mut(root, &site.path);
        let Subterm::Apply(apply) = call.as_subterm_mut() else {
            unreachable!("a recorded site is an Apply");
        };
        apply.head = Subterm::Name(Name::from(site.spec.as_str())).into();
        apply.params.remove(site.drop);
    }

    // Call-site rewrites free new names (the spec items) under enclosing
    // closures; recompute their captures.
    refresh_touched(module, &touched, touched_body, &spec_names);

    // Insert each target's batch right after the target. `minted` is already
    // callee-first (a body's sub-requests push before the body itself), and
    // inserting at a fixed position reverses iteration order — so iterate the
    // batch reversed to keep every spec ahead of its callers. Targets are
    // processed back-to-front so earlier insertions don't shift later
    // positions.
    let mut batches: HashMap<usize, Vec<(String, Term)>> = HashMap::new();
    for minted in minted {
        let mut body = minted.body;
        refresh_captures(&mut body, &spec_names);
        batches
            .entry(minted.target)
            .or_default()
            .push((minted.name, body));
    }
    let mut targets: Vec<usize> = batches.keys().copied().collect();
    targets.sort_unstable_by(|a, b| b.cmp(a));
    for target in targets {
        for (name, body) in batches.remove(&target).into_iter().flatten().rev() {
            module.items.insert(target + 1, Item::Let { name, body });
        }
    }
}

// --- Site discovery -------------------------------------------------------------

/// The scope stack the walk maintains: a binding is either a known literal or
/// an opaque shadow (so lookups stop at the nearest binder either way).
type Scope = Vec<(String, Option<Term>)>;

fn lookup<'s>(scope: &'s Scope, name: &str) -> Option<&'s Term> {
    scope
        .iter()
        .rev()
        .find(|(bound, _)| bound == name)
        .and_then(|(_, literal)| literal.as_ref())
}

/// Walk a top-level term, tracking `Let`-bound literals through binders (a
/// closure's captures reference outer names verbatim, so the scope flows into
/// `Func` bodies with only the parameters shadowed), and record every eligible
/// call site.
fn find_sites(
    minter: &mut Minter<'_>,
    term: &Term,
    path: &mut Vec<usize>,
    scope: &mut Scope,
    loc: Loc,
    sites: &mut Vec<Site>,
) {
    // Record this node's site *before* descending: children keep their indices
    // regardless, and descendant-first application order is restored by the
    // path sort in `apply`.
    if let Subterm::Apply(apply) = term.as_subterm()
        && let Some((spec, drop)) = minter.specialize_call(apply, scope)
    {
        sites.push(Site {
            loc,
            path: path.clone(),
            spec,
            drop,
        });
    }

    match term.as_subterm() {
        Subterm::Let(let_) => {
            path.push(0);
            find_sites(minter, &let_.body, path, scope, loc, sites);
            path.pop();

            let known = resolve_literal(&let_.body, scope);
            scope.push((let_.name.clone(), known));
            path.push(1);
            find_sites(minter, &let_.tail, path, scope, loc, sites);
            path.pop();
            scope.pop();
        }
        Subterm::Func(func) => {
            let depth = scope.len();
            scope.extend(func.params.iter().map(|param| (param.name.clone(), None)));
            path.push(0);
            find_sites(minter, &func.body, path, scope, loc, sites);
            path.pop();
            scope.truncate(depth);
        }
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            pred,
            ih,
            succ_case,
        }) => {
            path.push(0);
            find_sites(minter, head, path, scope, loc, sites);
            path.pop();
            path.push(1);
            find_sites(minter, zero_case, path, scope, loc, sites);
            path.pop();

            let depth = scope.len();
            scope.push((pred.clone(), None));
            scope.push((ih.clone(), None));
            path.push(2);
            find_sites(minter, succ_case, path, scope, loc, sites);
            path.pop();
            scope.truncate(depth);
        }
        Subterm::Rec(rec) => {
            let depth = scope.len();
            scope.extend(rec.names.iter().map(|name| (name.clone(), None)));
            for (index, child) in children(term).into_iter().enumerate() {
                path.push(index);
                find_sites(minter, child, path, scope, loc, sites);
                path.pop();
            }
            scope.truncate(depth);
        }
        _ => {
            for (index, child) in children(term).into_iter().enumerate() {
                path.push(index);
                find_sites(minter, child, path, scope, loc, sites);
                path.pop();
            }
        }
    }
}

/// Whether `term` is a constructor-shaped literal within the size cap: a
/// (possibly nested) flat constructor tuple — field 0 a tag — over scalar and
/// bytestring literals, tags, erased slots, and literal lists.
fn is_ctor_literal(term: &Term) -> bool {
    fn size_of(term: &Term, budget: &mut usize) -> bool {
        if *budget == 0 {
            return false;
        }
        *budget -= 1;
        match term.as_subterm() {
            Subterm::Tuple(tuple) => {
                matches!(
                    tuple.fields.first().map(|field| field.as_subterm()),
                    Some(Subterm::Atom(_))
                ) && tuple
                    .fields
                    .iter()
                    .all(|field| literal_field(field, budget))
            }
            Subterm::Atom(_) => true,
            _ => false,
        }
    }

    fn literal_field(term: &Term, budget: &mut usize) -> bool {
        if *budget == 0 {
            return false;
        }
        match term.as_subterm() {
            Subterm::Tuple(_) => size_of(term, budget),
            Subterm::Atom(_) | Subterm::Erased => {
                *budget -= 1;
                true
            }
            Subterm::Prim(Prim::Pure(pure)) => match pure {
                PurePrim::Nat(_)
                | PurePrim::Int(_)
                | PurePrim::Flt(_)
                | PurePrim::Bin(_)
                | PurePrim::Io(_) => {
                    *budget -= 1;
                    true
                }
                PurePrim::Lst(elements) => {
                    *budget -= 1;
                    elements
                        .iter()
                        .all(|element| literal_field(element, budget))
                }
                _ => false,
            },
            _ => false,
        }
    }

    let mut budget = MAX_SPINE_NODES;
    size_of(term, &mut budget)
}

/// The literal `term` denotes under `scope`, if any: itself, or the literal a
/// name resolves to.
fn resolve_literal(term: &Term, scope: &Scope) -> Option<Term> {
    match term.as_subterm() {
        Subterm::Name(name) => lookup(scope, name.as_str()).map(deep_copy),
        _ => is_ctor_literal(term).then(|| deep_copy(term)),
    }
}

// --- Minting ----------------------------------------------------------------------

struct Minter<'m> {
    /// Specializable targets: single-member `rec` items whose member is a
    /// `Func` (a mutual member makes no *self*-keyed unrolling; declined).
    targets: HashMap<&'m str, (usize, &'m Func)>,
    /// `(target, position, printed literal)` → specialization name. The
    /// printed form is the structural key: terms print deterministically and
    /// spine literals are size-capped.
    memo: HashMap<(String, usize, String), String>,
    counters: HashMap<String, usize>,
    minted: Vec<Minted>,
    budget: usize,
}

impl<'m> Minter<'m> {
    fn new(module: &'m Module) -> Self {
        let mut targets = HashMap::new();
        for (index, item) in module.items.iter().enumerate() {
            if let Item::Rec { names, items } = item
                && let [name] = names.as_slice()
                && let [member] = items.as_slice()
                && let Subterm::Func(func) = member.as_subterm()
            {
                targets.insert(name.as_str(), (index, func));
            }
        }
        Self {
            targets,
            memo: HashMap::new(),
            counters: HashMap::new(),
            minted: Vec::new(),
            budget: MAX_SPECIALIZATIONS,
        }
    }

    /// If `apply` is a call to a target with a literal spine argument, the
    /// specialization to call and the argument position to drop.
    fn specialize_call(&mut self, apply: &Apply, scope: &Scope) -> Option<(String, usize)> {
        let Subterm::Name(head) = apply.head.as_subterm() else {
            return None;
        };
        let (_, func) = *self.targets.get(head.as_str())?;
        if func.params.len() != apply.params.len() {
            return None;
        }

        let (position, literal) = apply.params.iter().enumerate().find_map(|(index, param)| {
            resolve_literal(param, scope).map(|literal| (index, literal))
        })?;

        let spec = self.request(head.as_str().to_owned(), position, literal)?;
        Some((spec, position))
    }

    /// The specialization of `target` at `position` for `literal`, minting it
    /// (and, recursively, the sub-spine specializations its body requests) on
    /// first use.
    fn request(&mut self, target: String, position: usize, literal: Term) -> Option<String> {
        let key = (target.clone(), position, format!("{literal}"));
        if let Some(name) = self.memo.get(&key) {
            return Some(name.clone());
        }
        if self.budget == 0 {
            return None;
        }
        self.budget -= 1;

        let counter = self.counters.entry(target.clone()).or_insert(0);
        let name = format!("{target}@s{counter}");
        *counter += 1;
        self.memo.insert(key, name.clone());

        let (item, func) = *self.targets.get(target.as_str())?;
        let baked = func.params[position].name.clone();

        // The body: bind the literal, copy the original body verbatim, and
        // fold the projections, matches, and recursive calls it decides.
        let mut body: Term = Subterm::Let(Let {
            name: baked.clone(),
            body: literal,
            tail: deep_copy(&func.body),
        })
        .into();
        let mut scope: Scope = Vec::new();
        fold_known(self, &mut body, &mut scope);

        let spec: Term = Subterm::Func(Func {
            captures: clone_args(&func.captures),
            params: func
                .params
                .iter()
                .enumerate()
                .filter(|(index, _)| *index != position)
                .map(|(_, param)| Argument {
                    name: param.name.clone(),
                    candidate: param.candidate,
                })
                .collect(),
            body,
        })
        .into();

        self.minted.push(Minted {
            target: item,
            name: name.clone(),
            body: spec,
        });
        Some(name)
    }
}

// --- In-place folding of a minted body ---------------------------------------------

/// Fold what the baked literal decides, bottom-up-ish: known `Let` bindings
/// feed projections, a decided projection feeds the tag match, and a decided
/// recursive call re-enters [`Minter::request`]. Everything undecided is left
/// exactly as copied.
fn fold_known(minter: &mut Minter<'_>, term: &mut Term, scope: &mut Scope) {
    match term.as_subterm_mut() {
        Subterm::Let(let_) => {
            fold_known(minter, &mut let_.body, scope);
            let known = resolve_literal(&let_.body, scope);
            scope.push((let_.name.clone(), known));
            fold_known(minter, &mut let_.tail, scope);
            scope.pop();
        }
        Subterm::Func(func) => {
            let depth = scope.len();
            let params: Vec<String> = func.params.iter().map(|p| p.name.clone()).collect();
            scope.extend(params.into_iter().map(|name| (name, None)));
            fold_known(minter, &mut func.body, scope);
            scope.truncate(depth);
        }
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            pred,
            ih,
            succ_case,
        }) => {
            fold_known(minter, head, scope);
            fold_known(minter, zero_case, scope);
            let depth = scope.len();
            let (pred, ih) = (pred.clone(), ih.clone());
            scope.push((pred, None));
            scope.push((ih, None));
            fold_known(minter, succ_case, scope);
            scope.truncate(depth);
        }
        Subterm::Rec(rec) => {
            let depth = scope.len();
            let names: Vec<String> = rec.names.clone();
            scope.extend(names.into_iter().map(|name| (name, None)));
            for item in &mut rec.items {
                fold_known(minter, item, scope);
            }
            fold_known(minter, &mut rec.tail, scope);
            scope.truncate(depth);
        }
        _ => {
            for child in super::children_mut(term) {
                fold_known(minter, child, scope);
            }
        }
    }

    // Post-order rewrites: children are folded, so a projection head or match
    // scrutinee that the literal decides has already been replaced.
    let replacement = match term.as_subterm() {
        Subterm::Proj(proj) => {
            let head = match proj.head.as_subterm() {
                Subterm::Name(name) => lookup(scope, name.as_str()),
                Subterm::Tuple(_) => Some(&proj.head),
                _ => None,
            };
            match head.map(|tuple| tuple.as_subterm()) {
                Some(Subterm::Tuple(tuple)) => tuple.fields.get(proj.index).map(deep_copy),
                _ => None,
            }
        }
        Subterm::Match(m) => match m.head.as_subterm() {
            Subterm::Atom(atom) if atom.index < m.cases.len() => Some(take_case(term, atom.index)),
            _ => None,
        },
        Subterm::NatMatch(NatMatch::Dispatch { head, .. }) => match head.as_subterm() {
            Subterm::Prim(Prim::Pure(PurePrim::Nat(value))) => {
                let value = *value;
                Some(take_dispatch(term, value))
            }
            _ => None,
        },
        Subterm::Apply(apply) => minter
            .specialize_call(apply, scope)
            .map(|(spec, drop)| respecialized(apply, spec, drop)),
        _ => None,
    };

    if let Some(mut replacement) = replacement {
        // A selected arm can expose further redexes (its own projections out
        // of the same literal); fold it too.
        fold_known(minter, &mut replacement, scope);
        *term = replacement;
    }
}

/// Take `cases[index]` out of a `Match` term.
fn take_case(term: &mut Term, index: usize) -> Term {
    match mem::replace(term.as_subterm_mut(), Subterm::Erased) {
        Subterm::Match(m) => m
            .cases
            .into_iter()
            .nth(index)
            .expect("the taken arm exists"),
        _ => unreachable!("caller matched a Match"),
    }
}

/// Take the selected case (or the default) out of a `Dispatch` term.
fn take_dispatch(term: &mut Term, value: u32) -> Term {
    match mem::replace(term.as_subterm_mut(), Subterm::Erased) {
        Subterm::NatMatch(NatMatch::Dispatch {
            mut cases, default, ..
        }) => cases.remove(&value).unwrap_or(default),
        _ => unreachable!("caller matched a Dispatch"),
    }
}

/// The rewritten call: the specialization's name applied to the original
/// arguments minus the baked one.
fn respecialized(apply: &Apply, spec: String, drop: usize) -> Term {
    Subterm::Apply(Apply {
        head: Subterm::Name(Name::from(spec.as_str())).into(),
        params: apply
            .params
            .iter()
            .enumerate()
            .filter(|(index, _)| *index != drop)
            .map(|(_, param)| deep_copy(param))
            .collect(),
    })
    .into()
}

#[cfg(test)]
mod tests {
    use {super::*, crate::Atom, crate::Match, crate::Proj, crate::Tuple};

    fn nat(value: u32) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::Nat(value))).into()
    }

    fn name(named: &str) -> Term {
        Subterm::Name(Name::from(named)).into()
    }

    fn atom(index: usize) -> Term {
        Subterm::Atom(Atom { index }).into()
    }

    fn tuple(fields: Vec<Term>) -> Term {
        Subterm::Tuple(Tuple { fields }).into()
    }

    fn proj(head: Term, index: usize) -> Term {
        Subterm::Proj(Proj { head, index }).into()
    }

    fn apply(head: Term, params: Vec<Term>) -> Term {
        Subterm::Apply(Apply { head, params }).into()
    }

    fn func(captures: Vec<&str>, params: Vec<&str>, body: Term) -> Term {
        Subterm::Func(Func {
            captures: captures.into_iter().map(Argument::from).collect(),
            params: params.into_iter().map(Argument::from).collect(),
            body,
        })
        .into()
    }

    fn let_(bound: &str, body: Term, tail: Term) -> Term {
        Subterm::Let(Let {
            name: bound.to_owned(),
            body,
            tail,
        })
        .into()
    }

    fn match_(head: Term, cases: Vec<Term>) -> Term {
        Subterm::Match(Match { head, cases }).into()
    }

    /// `stop()` = `(@0)`; `step(piece, rest)` = `(@1, piece, rest)`.
    fn spine(steps: &[u32]) -> Term {
        steps.iter().rev().fold(tuple(vec![atom(0)]), |rest, step| {
            tuple(vec![atom(1), nat(*step), rest])
        })
    }

    /// The mini `go_with`:
    /// `rec walk(finish, f, acc) = match f | stop => finish(acc)
    ///  | step(p, rest) => walk(finish, rest, acc + p)` in erased form.
    fn walk() -> Item {
        Item::Rec {
            names: vec!["walk".to_owned()],
            items: vec![func(
                vec!["walk"],
                vec!["finish", "f", "acc"],
                let_(
                    "scrutinee",
                    name("f"),
                    match_(
                        proj(name("scrutinee"), 0),
                        vec![
                            apply(name("finish"), vec![name("acc")]),
                            let_(
                                "piece",
                                proj(name("scrutinee"), 1),
                                let_(
                                    "rest",
                                    proj(name("scrutinee"), 2),
                                    apply(
                                        name("walk"),
                                        vec![
                                            name("finish"),
                                            name("rest"),
                                            Subterm::Prim(Prim::Pure(PurePrim::NatAdd(
                                                name("acc"),
                                                name("piece"),
                                            )))
                                            .into(),
                                        ],
                                    ),
                                ),
                            ),
                        ],
                    ),
                ),
            )],
        }
    }

    fn fin() -> Item {
        Item::Let {
            name: "fin".to_owned(),
            body: func(vec![], vec!["a"], name("a")),
        }
    }

    fn run(mut module: Module) -> String {
        specialize_literal_spines(&mut module);
        format!("{module}")
    }

    fn unchanged(mut module: Module) {
        let before = format!("{module}");
        specialize_literal_spines(&mut module);
        assert_eq!(format!("{module}"), before, "expected the module untouched");
    }

    #[test]
    fn specializes_on_a_literal_spine() {
        let module = Module {
            items: vec![fin(), walk()],
            body: let_(
                "sp",
                spine(&[5, 7]),
                apply(name("walk"), vec![name("fin"), name("sp"), nat(0)]),
            ),
        };
        let printed = run(module);
        assert!(printed.contains("#walk@s0(#fin, 0n)"), "{printed}");
        assert!(printed.contains("let #walk@s0 ="), "{printed}");
        assert!(printed.contains("#walk@s1(#finish"), "{printed}");
        assert!(printed.contains("#walk@s2(#finish"), "{printed}");
        // The nil specialization is the finish call, no residual dispatch.
        assert!(printed.contains("#finish(#acc)"), "{printed}");
    }

    #[test]
    fn resolves_the_spine_through_a_let_under_a_closure() {
        let module = Module {
            items: vec![fin(), walk()],
            body: let_(
                "sp",
                spine(&[5]),
                func(
                    vec!["sp"],
                    vec!["x"],
                    apply(name("walk"), vec![name("fin"), name("sp"), name("x")]),
                ),
            ),
        };
        let printed = run(module);
        assert!(printed.contains("#walk@s0(#fin, #x)"), "{printed}");
    }

    #[test]
    fn leaves_a_runtime_spine_generic() {
        let module = Module {
            items: vec![fin(), walk()],
            body: func(
                vec![],
                vec!["f"],
                apply(name("walk"), vec![name("fin"), name("f"), nat(0)]),
            ),
        };
        unchanged(module);
    }

    #[test]
    fn memoizes_identical_literals() {
        let module = Module {
            items: vec![fin(), walk()],
            body: let_(
                "sp",
                spine(&[5]),
                tuple(vec![
                    apply(name("walk"), vec![name("fin"), name("sp"), nat(0)]),
                    apply(name("walk"), vec![name("fin"), name("sp"), nat(1)]),
                ]),
            ),
        };
        let printed = run(module);
        assert_eq!(printed.matches("let #walk@s0 =").count(), 1, "{printed}");
        assert_eq!(printed.matches("#walk@s0(#fin,").count(), 2, "{printed}");
    }

    #[test]
    fn declines_an_oversized_spine() {
        let steps: Vec<u32> = (0..200).collect();
        let module = Module {
            items: vec![fin(), walk()],
            body: let_(
                "sp",
                spine(&steps),
                apply(name("walk"), vec![name("fin"), name("sp"), nat(0)]),
            ),
        };
        unchanged(module);
    }

    #[test]
    fn exhausted_budget_falls_back_to_the_generic_function() {
        // 70 steps fit the node cap but out-mint the budget: the last minted
        // body keeps a generic `#walk(` call, and the module stays well-formed.
        let steps: Vec<u32> = (0..70).collect();
        let module = Module {
            items: vec![fin(), walk()],
            body: let_(
                "sp",
                spine(&steps),
                apply(name("walk"), vec![name("fin"), name("sp"), nat(0)]),
            ),
        };
        let printed = run(module);
        assert_eq!(
            printed.matches("let #walk@s").count(),
            MAX_SPECIALIZATIONS,
            "{printed}"
        );
        // The generic self-call plus the fallback inside the last spec.
        assert_eq!(printed.matches("#walk(#finish").count(), 2, "{printed}");
    }

    #[test]
    fn inserts_specs_callee_first_after_the_target() {
        let module = Module {
            items: vec![fin(), walk()],
            body: let_(
                "sp",
                spine(&[5]),
                apply(name("walk"), vec![name("fin"), name("sp"), nat(0)]),
            ),
        };
        let printed = run(module);
        let target = printed.find("rec #walk =").expect("target printed");
        let s1 = printed.find("let #walk@s1 =").expect("s1 printed");
        let s0 = printed.find("let #walk@s0 =").expect("s0 printed");
        assert!(target < s1 && s1 < s0, "{printed}");
    }
}
