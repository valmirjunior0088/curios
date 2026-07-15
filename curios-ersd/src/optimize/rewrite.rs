//! Shared term utilities for the ersd optimization passes.
//!
//! - [`children`]/[`children_mut`] — ordered child traversal. The two views
//!   enumerate children in the **identical order** (including primitive
//!   operands), so a path recorded against the immutable view applies against
//!   the mutable one; [`term_at_mut`] resolves such a path. `evaluate` plans
//!   rewrites over `&Module` and applies them over `&mut Module` through this
//!   pair.
//! - [`deep_copy`] — full structural rebuild of a term. `Term` is deliberately
//!   not `Clone` (a pass should copy consciously, not accidentally); the passes
//!   that materialize copies — reification in `evaluate`, body cloning in
//!   `specialize` — route through this one function.
//! - [`refresh_captures`] — recompute every nested closure's capture list after
//!   a rewrite changes which names are free (a `Func`'s `free_names` reads its
//!   *precomputed* captures, so they must be maintained by hand).

use {
    crate::{
        Apply, Argument, CellPrim, Func, HostPrim, Item, Let, Match, Module, NatMatch, Prim, Proj,
        PurePrim, Rec, Subterm, Term, Tuple,
    },
    std::{
        collections::{HashMap, HashSet},
        iter,
        sync::Arc,
    },
};

/// The immediate sub-terms of a term, in canonical child order, including
/// primitive operands (in [`Prim::operands`] order).
pub(super) fn children(term: &Term) -> Vec<&Term> {
    match term.as_subterm() {
        Subterm::Erased | Subterm::Unreachable | Subterm::Atom(_) | Subterm::Name(_) => vec![],
        Subterm::Prim(prim) => prim.operands(),
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            succ_case,
            ..
        }) => vec![head, zero_case, succ_case],
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => iter::once(head)
            .chain(cases.values())
            .chain(iter::once(default))
            .collect(),
        Subterm::Func(func) => vec![&func.body],
        Subterm::Apply(apply) => iter::once(&apply.head).chain(&apply.params).collect(),
        Subterm::Tuple(tuple) => tuple.fields.iter().collect(),
        Subterm::Proj(proj) => vec![&proj.head],
        Subterm::Match(m) => iter::once(&m.head)
            .chain(m.cases.values())
            .chain(m.default.iter())
            .collect(),
        Subterm::Let(let_) => let_
            .bindings
            .iter()
            .map(|(_, body)| body)
            .chain(iter::once(&let_.tail))
            .collect(),
        Subterm::Rec(rec) => rec.items.iter().chain(iter::once(&rec.tail)).collect(),
    }
}

/// The mutable mirror of [`children`] — **identical child order**, primitive
/// operands included (unlike `worker_wrapper`'s structural-only walk), so a
/// recorded path resolves to the same node through either view.
pub(super) fn children_mut(term: &mut Term) -> Vec<&mut Term> {
    match term.as_subterm_mut() {
        Subterm::Erased | Subterm::Unreachable | Subterm::Atom(_) | Subterm::Name(_) => vec![],
        Subterm::Prim(prim) => prim.operands_mut(),
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            succ_case,
            ..
        }) => vec![head, zero_case, succ_case],
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => iter::once(head)
            .chain(cases.values_mut())
            .chain(iter::once(default))
            .collect(),
        Subterm::Func(func) => vec![&mut func.body],
        Subterm::Apply(apply) => iter::once(&mut apply.head)
            .chain(&mut apply.params)
            .collect(),
        Subterm::Tuple(tuple) => tuple.fields.iter_mut().collect(),
        Subterm::Proj(proj) => vec![&mut proj.head],
        Subterm::Match(m) => iter::once(&mut m.head)
            .chain(m.cases.values_mut())
            .chain(m.default.iter_mut())
            .collect(),
        Subterm::Let(let_) => let_
            .bindings
            .iter_mut()
            .map(|(_, body)| body)
            .chain(iter::once(&mut let_.tail))
            .collect(),
        Subterm::Rec(rec) => rec
            .items
            .iter_mut()
            .chain(iter::once(&mut rec.tail))
            .collect(),
    }
}

/// Resolve a [`children`]-index path to the subterm it names, mutably.
pub(super) fn term_at_mut<'t>(root: &'t mut Term, path: &[usize]) -> &'t mut Term {
    let mut current = root;
    for &index in path {
        current = children_mut(current)
            .into_iter()
            .nth(index)
            .expect("rewrite path resolves within the tree it was recorded on");
    }
    current
}

/// Which top-level term a rewrite/site path is rooted in: one member of a
/// module item (a `Let`'s body, or one `Rec` group member), or the top-level
/// `body`.
#[derive(Clone, Copy)]
pub(super) enum Loc {
    Item { item: usize, member: usize },
    Body,
}

/// An item's directly-rewritable sub-terms: a `Let`'s body, or every member of
/// a `Rec` group.
pub(super) fn members(item: &Item) -> Vec<&Term> {
    match item {
        Item::Let { body, .. } => vec![body],
        Item::Rec { items, .. } => items.iter().collect(),
    }
}

/// Resolve a [`Loc`] to the term it names, mutably — the root [`term_at_mut`]
/// then descends a path into.
pub(super) fn root_mut(module: &mut Module, loc: Loc) -> &mut Term {
    match loc {
        Loc::Item { item, member } => match &mut module.items[item] {
            Item::Let { body, .. } => body,
            Item::Rec { items, .. } => &mut items[member],
        },
        Loc::Body => &mut module.body,
    }
}

/// Recompute captures for every root a rewrite pass touched — tracked as
/// `(item, member)` pairs plus a separate `body` flag, since [`Loc`] isn't
/// hashable and the same root can be touched by more than one rewrite.
pub(super) fn refresh_touched(
    module: &mut Module,
    touched: &HashSet<(usize, usize)>,
    touched_body: bool,
    introduced: &HashMap<String, bool>,
) {
    for &(item, member) in touched {
        refresh_captures(root_mut(module, Loc::Item { item, member }), introduced);
    }
    if touched_body {
        refresh_captures(root_mut(module, Loc::Body), introduced);
    }
}

/// A by-value copy of an argument list (`Argument` is not `Clone`).
pub(super) fn clone_args(args: &[Argument]) -> Vec<Argument> {
    args.iter()
        .map(|arg| Argument {
            name: arg.name.clone(),
            candidate: arg.candidate,
        })
        .collect()
}

/// Full structural rebuild of a term — the conscious copy `Term`'s missing
/// `Clone` forces. A `Foreign` primitive's function row is shared by
/// [`Arc::clone`], never rebuilt.
pub(super) fn deep_copy(term: &Term) -> Term {
    let subterm = match term.as_subterm() {
        Subterm::Erased => Subterm::Erased,
        Subterm::Unreachable => Subterm::Unreachable,
        Subterm::Atom(atom) => Subterm::Atom(*atom),
        Subterm::Name(name) => Subterm::Name(name.clone()),
        Subterm::Prim(prim) => Subterm::Prim(copy_prim(prim)),
        Subterm::NatMatch(NatMatch::Induction {
            head,
            zero_case,
            pred,
            ih,
            succ_case,
        }) => Subterm::NatMatch(NatMatch::Induction {
            head: deep_copy(head),
            zero_case: deep_copy(zero_case),
            pred: pred.clone(),
            ih: ih.clone(),
            succ_case: deep_copy(succ_case),
        }),
        Subterm::NatMatch(NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => Subterm::NatMatch(NatMatch::Dispatch {
            head: deep_copy(head),
            cases: cases
                .iter()
                .map(|(tag, case)| (*tag, deep_copy(case)))
                .collect(),
            default: deep_copy(default),
        }),
        Subterm::Func(func) => Subterm::Func(Func {
            captures: clone_args(&func.captures),
            params: clone_args(&func.params),
            body: deep_copy(&func.body),
        }),
        Subterm::Apply(apply) => Subterm::Apply(Apply {
            head: deep_copy(&apply.head),
            params: apply.params.iter().map(deep_copy).collect(),
        }),
        Subterm::Tuple(tuple) => Subterm::Tuple(Tuple {
            fields: tuple.fields.iter().map(deep_copy).collect(),
        }),
        Subterm::Proj(proj) => Subterm::Proj(Proj {
            head: deep_copy(&proj.head),
            index: proj.index,
        }),
        Subterm::Match(m) => Subterm::Match(Match {
            head: deep_copy(&m.head),
            cases: m
                .cases
                .iter()
                .map(|(tag, case)| (*tag, deep_copy(case)))
                .collect(),
            default: m.default.as_ref().map(deep_copy),
        }),
        Subterm::Let(let_) => Subterm::Let(Let {
            bindings: let_
                .bindings
                .iter()
                .map(|(name, body)| (name.clone(), deep_copy(body)))
                .collect(),
            tail: deep_copy(&let_.tail),
        }),
        Subterm::Rec(rec) => Subterm::Rec(Rec {
            names: rec.names.clone(),
            items: rec.items.iter().map(deep_copy).collect(),
            tail: deep_copy(&rec.tail),
        }),
    };
    subterm.into()
}

fn copy_prim(prim: &Prim) -> Prim {
    match prim {
        Prim::Pure(pure) => Prim::Pure(copy_pure(pure)),
        Prim::Host(host) => Prim::Host(copy_host(host)),
        Prim::Cell(cell) => Prim::Cell(copy_cell(cell)),
    }
}

fn copy_pure(prim: &PurePrim) -> PurePrim {
    use PurePrim::*;

    macro_rules! copy {
        // Rebuild a variant from its operand references.
        ($variant:ident $(, $operand:expr)*) => { $variant($(deep_copy($operand)),*) };
    }

    match prim {
        Nat(value) => Nat(*value),
        Int(value) => Int(*value),
        Flt(value) => Flt(*value),
        Bin(curios_base::Grain::X, bytes) => Bin(curios_base::Grain::X, bytes.clone()),
        Bin(curios_base::Grain::B, bits) => Bin(curios_base::Grain::B, bits.clone()),
        Io(token) => Io(*token),
        NatEql(a, b) => copy!(NatEql, a, b),
        NatNeq(a, b) => copy!(NatNeq, a, b),
        NatAdd(a, b) => copy!(NatAdd, a, b),
        NatSub(a, b) => copy!(NatSub, a, b),
        NatMul(a, b) => copy!(NatMul, a, b),
        NatLt(a, b) => copy!(NatLt, a, b),
        NatDiv(a, b) => copy!(NatDiv, a, b),
        NatRem(a, b) => copy!(NatRem, a, b),
        NatGt(a, b) => copy!(NatGt, a, b),
        NatLte(a, b) => copy!(NatLte, a, b),
        NatGte(a, b) => copy!(NatGte, a, b),
        NatAnd(a, b) => copy!(NatAnd, a, b),
        NatOr(a, b) => copy!(NatOr, a, b),
        NatXor(a, b) => copy!(NatXor, a, b),
        NatShl(a, b) => copy!(NatShl, a, b),
        NatShr(a, b) => copy!(NatShr, a, b),
        IntEql(a, b) => copy!(IntEql, a, b),
        IntNeq(a, b) => copy!(IntNeq, a, b),
        IntAdd(a, b) => copy!(IntAdd, a, b),
        IntSub(a, b) => copy!(IntSub, a, b),
        IntMul(a, b) => copy!(IntMul, a, b),
        IntDiv(a, b) => copy!(IntDiv, a, b),
        IntRem(a, b) => copy!(IntRem, a, b),
        IntLt(a, b) => copy!(IntLt, a, b),
        IntGt(a, b) => copy!(IntGt, a, b),
        IntLte(a, b) => copy!(IntLte, a, b),
        IntGte(a, b) => copy!(IntGte, a, b),
        IntAnd(a, b) => copy!(IntAnd, a, b),
        IntOr(a, b) => copy!(IntOr, a, b),
        IntXor(a, b) => copy!(IntXor, a, b),
        IntShl(a, b) => copy!(IntShl, a, b),
        IntShr(a, b) => copy!(IntShr, a, b),
        FltAdd(a, b) => copy!(FltAdd, a, b),
        FltSub(a, b) => copy!(FltSub, a, b),
        FltMul(a, b) => copy!(FltMul, a, b),
        FltDiv(a, b) => copy!(FltDiv, a, b),
        FltRem(a, b) => copy!(FltRem, a, b),
        FltEql(a, b) => copy!(FltEql, a, b),
        FltNeq(a, b) => copy!(FltNeq, a, b),
        FltLt(a, b) => copy!(FltLt, a, b),
        FltGt(a, b) => copy!(FltGt, a, b),
        FltLte(a, b) => copy!(FltLte, a, b),
        FltGte(a, b) => copy!(FltGte, a, b),
        FltMin(a, b) => copy!(FltMin, a, b),
        FltMax(a, b) => copy!(FltMax, a, b),
        FltNeg(a) => copy!(FltNeg, a),
        FltAbs(a) => copy!(FltAbs, a),
        FltSqrt(a) => copy!(FltSqrt, a),
        FltFloor(a) => copy!(FltFloor, a),
        FltCeil(a) => copy!(FltCeil, a),
        FltTrunc(a) => copy!(FltTrunc, a),
        FltNearest(a) => copy!(FltNearest, a),
        NatToInt(a) => copy!(NatToInt, a),
        NatToFlt(a) => copy!(NatToFlt, a),
        IntToNat(a) => copy!(IntToNat, a),
        IntToFlt(a) => copy!(IntToFlt, a),
        FltToNat(a) => copy!(FltToNat, a),
        FltToLeBytes(a) => copy!(FltToLeBytes, a),
        FltOfLeBytes(a) => copy!(FltOfLeBytes, a),
        FltToInt(a) => copy!(FltToInt, a),
        BinLen(grain, a) => BinLen(*grain, deep_copy(a)),
        BinEql(grain, a, b) => BinEql(*grain, deep_copy(a), deep_copy(b)),
        BinGet(grain, a, b) => BinGet(*grain, deep_copy(a), deep_copy(b)),
        BinSlice(grain, a, b, c) => BinSlice(*grain, deep_copy(a), deep_copy(b), deep_copy(c)),
        BinAppend(grain, a, b) => BinAppend(*grain, deep_copy(a), deep_copy(b)),
        BinConcat(grain, operands) => BinConcat(*grain, operands.iter().map(deep_copy).collect()),
        Lst(operands) => Lst(operands.iter().map(deep_copy).collect()),
        LstLen(a) => copy!(LstLen, a),
        LstGet(a, b) => copy!(LstGet, a, b),
        LstSlice(a, b, c) => copy!(LstSlice, a, b, c),
        LstAppend(a, b) => copy!(LstAppend, a, b),
        LstConcat(operands) => LstConcat(operands.iter().map(deep_copy).collect()),
        LstMap(a, b) => copy!(LstMap, a, b),
        IoEql(a, b) => copy!(IoEql, a, b),
    }
}

fn copy_host(prim: &HostPrim) -> HostPrim {
    match prim {
        HostPrim::Foreign(function, args) => {
            HostPrim::Foreign(Arc::clone(function), args.iter().map(deep_copy).collect())
        }
        HostPrim::IoExit(code) => HostPrim::IoExit(deep_copy(code)),
    }
}

fn copy_cell(prim: &CellPrim) -> CellPrim {
    match prim {
        CellPrim::New(init) => CellPrim::New(deep_copy(init)),
        CellPrim::Set(cell, value) => CellPrim::Set(deep_copy(cell), deep_copy(value)),
        CellPrim::Get(cell) => CellPrim::Get(deep_copy(cell)),
    }
}

/// Fix up closure captures bottom-up after a rewrite: each nested closure's
/// captures are exactly its body's free names minus its parameters. Candidate
/// flags are preserved from each closure's prior captures, and the `introduced`
/// names carry the flags the rewriting pass assigns. Bottom-up order matters:
/// an outer body's free names depend on its inner closures' freshly-fixed
/// captures. Unlike `worker_wrapper`'s structural walk, this one descends into
/// primitive operands too — a closure value can sit inside `LstMap`.
pub(super) fn refresh_captures(term: &mut Term, introduced: &HashMap<String, bool>) {
    for child in children_mut(term) {
        refresh_captures(child, introduced);
    }

    if let Subterm::Func(func) = term.as_subterm_mut() {
        let mut candidate = func
            .captures
            .iter()
            .map(|capture| (capture.name.clone(), capture.candidate))
            .collect::<HashMap<String, bool>>();
        candidate.extend(introduced.iter().map(|(name, cand)| (name.clone(), *cand)));

        let params = func
            .params
            .iter()
            .map(|param| param.name.as_str())
            .collect::<HashSet<&str>>();

        func.captures = func
            .body
            .free_names()
            .into_iter()
            .filter(|name| !params.contains(name.as_str()))
            .map(|name| Argument {
                candidate: candidate.get(&name).copied().unwrap_or(false),
                name,
            })
            .collect();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn nat(value: u32) -> Term {
        Subterm::Prim(Prim::Pure(PurePrim::Nat(value))).into()
    }

    fn name(name: &str) -> Term {
        Subterm::Name(crate::Name::from(name)).into()
    }

    /// A term exercising every child-bearing shape, primitive operands included.
    fn sample() -> Term {
        let map = Subterm::Prim(Prim::Pure(PurePrim::LstMap(
            name("xs"),
            Subterm::Func(Func {
                captures: vec![],
                params: vec!["e".into()],
                body: name("e"),
            })
            .into(),
        )))
        .into();

        Subterm::Let(Let {
            bindings: vec![("m".to_owned(), map)],
            tail: Subterm::Apply(Apply {
                head: name("f"),
                params: vec![
                    Subterm::Prim(Prim::Pure(PurePrim::NatAdd(nat(1), nat(2)))).into(),
                    Subterm::Match(Match {
                        head: name("m"),
                        cases: std::collections::BTreeMap::from([
                            (0, nat(3)),
                            (1, Subterm::Erased.into()),
                        ]),
                        default: Some(nat(4)),
                    })
                    .into(),
                ],
            })
            .into(),
        })
        .into()
    }

    /// `children` and `children_mut` must agree on child order at every node —
    /// `evaluate` records paths against one view and applies them against the
    /// other.
    #[test]
    fn child_views_agree_on_order() {
        fn check(term: &mut Term, path: &mut Vec<usize>, printed: &mut Vec<(Vec<usize>, String)>) {
            for (index, child) in children(term).into_iter().enumerate() {
                printed.push((
                    {
                        let mut p = path.clone();
                        p.push(index);
                        p
                    },
                    format!("{child}"),
                ));
            }
            let count = children(term).len();
            for index in 0..count {
                path.push(index);
                let child = children_mut(term).into_iter().nth(index).unwrap();
                check(child, path, printed);
                path.pop();
            }
        }

        let mut term = sample();
        let mut printed = Vec::new();
        check(&mut term, &mut Vec::new(), &mut printed);

        for (path, expected) in printed {
            let via_mut = format!("{}", term_at_mut(&mut term, &path));
            assert_eq!(via_mut, expected, "path {path:?} diverges between views");
        }
    }

    #[test]
    fn deep_copy_is_structurally_identical() {
        let term = sample();
        assert_eq!(format!("{}", deep_copy(&term)), format!("{term}"));
    }
}
