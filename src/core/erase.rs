use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Error, Func, Item, Let, Match, Module, Nat,
        NatMatch, One, Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple, TupleType, Two,
        Var, erase_prim, expect_prim_head, infer, reduce_with, refine_head,
    },
    crate::ersd,
    std::collections::BTreeMap,
};

fn erase_func(
    context: &mut Context,
    func: &Func,
    _term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    let Func { telescope } = func;

    let ft = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::FuncType(ft) => ft,
        // Elaborate already checked this function against a function type (§9).
        _ => unreachable!("erase: function checked against non-function type"),
    };

    // Walk the lambda's telescope (whose `Done` is the body) alongside the
    // checked function type's telescope (whose `Done` is the output type),
    // generating a fresh name per parameter and recording the candidate flag
    // from each expected domain. The lambda's own domains are erased away.
    fn walk(
        context: &mut Context,
        body: Telescope<Term>,
        type_: Telescope<Term>,
        names: &mut Vec<String>,
        candidates: &mut Vec<bool>,
    ) -> Result<(Term, Term), Error> {
        match (body, type_) {
            (Telescope::Done(body), Telescope::Done(output)) => Ok((*body, *output)),
            (Telescope::Cons(_domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                // The flag is read before the binder is assumed: a parameter's type
                // never depends on the parameter itself.
                candidates.push(is_candidate(context, &type_)?);
                let name = context.fresh(body_rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &type_);
                names.push(name);
                walk(
                    context,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    names,
                    candidates,
                )
            }
            _ => unreachable!("erase: function/type telescope arity mismatch"),
        }
    }

    let mut param_names = Vec::new();
    let mut candidates = Vec::new();
    let (erased_body, captures) = context.with_frame(|context| {
        let (body_opened, output_type) = walk(
            context,
            telescope.clone(),
            ft.telescope,
            &mut param_names,
            &mut candidates,
        )?;

        // Captures are the body's free variables other than the lambda's own
        // parameters (which appear as fresh frees once the body is opened). The
        // candidate flag rides from here — the last point a binder's type is
        // known — down to `cont`, where the optimizer specializes function-typed
        // arguments.
        let captures = body_opened
            .free_vars()
            .into_iter()
            .filter(|name| !param_names.contains(name))
            .map(|name| {
                let type_ = infer(context, &Term::var(Var::free(&name)))?;
                let candidate = is_candidate(context, &type_)?;
                Ok(ersd::Argument { name, candidate })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        let erased_body = erase(context, &body_opened, &output_type)?;
        Ok::<_, Error>((erased_body, captures))
    })?;

    let params = param_names
        .into_iter()
        .zip(candidates)
        .map(|(name, candidate)| ersd::Argument { name, candidate })
        .collect();

    Ok(ersd::Term::Func(ersd::Func {
        captures,
        params,
        body: erased_body.into(),
    }))
}

/// Whether an argument of type `type_` is a specialization candidate, after
/// reduction. Three erased-to-trivial shapes qualify, each a compile-time constant
/// the specializer can bake in:
///
/// - a **function type** — a first-class closure value, devirtualizable;
/// - **`Type`** — an erased type argument (a unit at runtime);
/// - the **empty tuple type `{}`** — an erased unit argument.
///
/// Reduction matters: an aliased or computed type only exposes its head in
/// weak-head normal form.
fn is_candidate(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    Ok(match &*reduce_with(context, type_)? {
        Subterm::FuncType(_) | Subterm::Type => true,
        Subterm::TupleType(tuple_type) => tuple_type.telescope.is_empty(),
        _ => false,
    })
}

fn erase_apply(
    context: &mut Context,
    apply: &Apply,
    _term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let Apply { head, params } = apply;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    // Elaborate already checked the head is a function applied to the right
    // number of arguments (§9); here the shape is re-derived only to lower.
    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft,
        _ => unreachable!("erase: applied a non-function"),
    };

    assert_eq!(
        params.len(),
        ft.telescope.len(),
        "erase: application arity disagrees with the function type",
    );

    let mut erased_params = Vec::with_capacity(params.len());
    ft.telescope.clone().walk(params, |arg, ty| {
        erased_params.push(erase(context, arg, ty)?);
        Ok(())
    })?;
    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Term::Apply(ersd::Apply {
        head: erased_head.into(),
        params: erased_params.into_iter().map(|p| p.into()).collect(),
    }))
}

fn erase_tuple(context: &mut Context, tuple: &Tuple, expected: &Term) -> Result<ersd::Term, Error> {
    let Tuple { fields } = tuple;

    // Elaborate already checked this tuple against a tuple type of matching
    // arity (§9); the telescope is re-derived here only to lower the fields.
    let type_telescope = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::TupleType(TupleType { telescope }) => telescope,
        _ => unreachable!("erase: tuple checked against non-tuple type"),
    };

    assert_eq!(
        fields.len(),
        type_telescope.len(),
        "erase: tuple width disagrees with the tuple type",
    );

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        erased: &mut Vec<ersd::Subterm>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let head = &fields[0];
                erased.push(erase(context, head, &ty)?.into());
                walk(context, rest.open(&[head]), &fields[1..], erased)
            }
        }
    }

    let mut erased_fields = Vec::<ersd::Subterm>::new();
    walk(context, type_telescope, fields, &mut erased_fields)?;

    Ok(ersd::Term::Tuple(ersd::Tuple {
        fields: erased_fields,
    }))
}

fn erase_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, term, Prim::NatType)?;

    let erased_zero_case = erase(
        context,
        zero_case,
        &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let erased_succ_case = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&pred_label))]),
        );

        erase(
            context,
            &succ_case.open(&[
                &Term::var(Var::free(&pred_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            &motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::var(Var::free(&pred_label)),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Term::NatMatch(ersd::NatMatch::Induction {
        head: erased_head.into(),
        zero_case: erased_zero_case.into(),
        pred: pred_label,
        ih: ih_label,
        succ_case: erased_succ_case.into(),
    }))
}

fn erase_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, term, Prim::NatType)?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let case_expected = motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]);
            context.with_frame(|context| {
                refine_head(
                    context,
                    head,
                    &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
                )?;
                erase(context, body, &case_expected).map(|e| (*n, e.into()))
            })
        })
        .collect::<Result<BTreeMap<_, _>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head]))?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Term::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head.into(),
        cases: erased_cases,
        default: erased_default.into(),
    }))
}

fn erase_nat_match(
    context: &mut Context,
    nm: &NatMatch,
    term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    match nm {
        NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        } => erase_nat_induction(context, head, motive, zero_case, succ_case, term, expected),
        NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        } => erase_nat_dispatch(context, head, motive, cases, default, term, expected),
    }
}

fn erase_bln_match(
    context: &mut Context,
    bm: &BlnMatch,
    term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;

    let head_type = expect_prim_head(context, head, term, Prim::BlnType)?;

    let erased_false = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into())?;
        erase(
            context,
            false_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let erased_true = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into())?;
        erase(
            context,
            true_case,
            &motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Term::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head.into(),
        cases: BTreeMap::from([(0, erased_false.into())]),
        default: erased_true.into(),
    }))
}

fn erase_proj(
    context: &mut Context,
    proj: &Proj,
    _term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let Proj { head, index } = proj;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    // Elaborate already checked the head is a tuple and the index is in range
    // (§9); the type is re-derived here only to lower the head. The head's type
    // is usually a `TupleType`, but a projection of a *union payload* whose
    // discriminant is stuck (a `parse`-style combinator run at compile time on a
    // symbolic input) has a neutral `match` type instead — every variant still
    // carries a field at `index`, in the same runtime slot, so the projection is
    // well-formed and lowers to the same `ersd::Proj` (§9).
    assert!(
        projectable_at(context, &head_type, *index)?,
        "erase: projected a non-tuple",
    );

    Ok(ersd::Term::Proj(ersd::Proj {
        head: erase(context, head, &head_type)?.into(),
        index: *index,
    }))
}

/// Whether field `index` can be projected from a value of (reduced) type `ty`.
/// A `TupleType` answers directly. A *neutral* `match` — a union payload type
/// with a stuck discriminant — answers when every branch is itself projectable
/// at `index`: at runtime the tag selects a variant, and each variant is a tuple
/// carrying that field at the shared offset, so the lowered projection is sound.
fn projectable_at(context: &mut Context, ty: &Term, index: usize) -> Result<bool, Error> {
    Ok(match Term::unwrap_or_clone(reduce_with(context, ty)?) {
        Subterm::TupleType(TupleType { telescope }) => index < telescope.len(),
        Subterm::Match(Match { cases, .. }) => {
            let mut ok = !cases.is_empty();
            for body in cases.values() {
                ok = ok && projectable_at(context, body, index)?;
            }
            ok
        }
        Subterm::BlnMatch(BlnMatch {
            false_case,
            true_case,
            ..
        }) => {
            projectable_at(context, &false_case, index)?
                && projectable_at(context, &true_case, index)?
        }
        _ => false,
    })
}

fn erase_atom(
    context: &mut Context,
    atom: &Atom,
    _term: &Term,
    expected: &Term,
) -> Result<ersd::Term, Error> {
    // Elaborate already checked this atom belongs to `expected` (§9); the atom
    // type is re-derived here only to read off the runtime tag index.
    let atoms = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        _ => unreachable!("erase: atom checked against non-atom type"),
    };

    let index = atoms
        .iter()
        .position(|candidate| candidate == atom)
        .expect("erase: atom absent from its atom type");

    Ok(ersd::Term::Atom(ersd::Atom { index }))
}

fn erase_match(
    context: &mut Context,
    m: &Match,
    _term: &Term,
    _expected: &Term,
) -> Result<ersd::Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    // Elaborate already checked the head is an atom type with exactly one body
    // per atom (§9); the atoms are re-derived here only to order and lower the
    // cases.
    let atoms = match Term::unwrap_or_clone(head_type.clone()) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        _ => unreachable!("erase: matched on a non-atom type"),
    };

    assert_eq!(
        cases.len(),
        atoms.len(),
        "erase: match arm count disagrees with the atom type",
    );

    let cases = atoms
        .iter()
        .map(|atom| {
            let body = cases
                .get(atom)
                .expect("erase: match missing an arm for an atom");

            let expected = motive.open(&[&Term::atom(atom.clone())]);

            context.with_frame(|context| {
                refine_head(context, head, &Term::atom(atom.clone()))?;
                erase(context, body, &expected).map(Into::into)
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    Ok(ersd::Term::Match(ersd::Match {
        head: erase(context, head, &head_type)?.into(),
        cases,
    }))
}

fn erase_let(context: &mut Context, let_: &Let, expected: &Term) -> Result<ersd::Term, Error> {
    let Let {
        type_: body_type,
        body,
        tail,
    } = let_;

    let name = context.fresh(tail.first_label());
    let erased_body = erase(context, body, body_type)?;
    let var_term = Term::var(Var::free(&name));
    let tail = tail.open(&[&var_term]);

    let tail = context.with_frame(|context| {
        context.define_assuming(&name, body_type, body);

        erase(context, &tail, expected)
    })?;

    Ok(ersd::Term::Let(ersd::Let {
        name,
        body: erased_body.into(),
        tail: tail.into(),
    }))
}

fn erase_rec(context: &mut Context, rec: &Rec, expected: &Term) -> Result<ersd::Term, Error> {
    let Rec { items, tail } = rec;

    let names = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = names
        .iter()
        .map(Var::free)
        .map(Term::var)
        .collect::<Vec<_>>();

    let label_terms = label_terms.iter().collect::<Vec<_>>();

    let items = items
        .iter()
        .map(|(type_, body)| (type_.open(&label_terms), body.open(&label_terms)))
        .collect::<Vec<_>>();

    let tail = tail.open(&label_terms);

    let erased = context.with_frame(|context| {
        for (name, (type_, _)) in names.iter().zip(items.iter()) {
            context.assume(name, type_);
        }

        for (name, (_, body)) in names.iter().zip(items.iter()) {
            context.define(name, body);
        }

        let erased_items = items
            .iter()
            .map(|(type_, body)| erase(context, body, type_).map(Into::into))
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(ersd::Rec {
            names,
            items: erased_items,
            tail: erase(context, &tail, expected)?.into(),
        })
    })?;

    Ok(ersd::Term::Rec(erased))
}

/// Erase a whole meta-free [`Module`] to an [`ersd::Module`] (§9). Each top-level
/// item is erased and `define`d *cumulatively in the persistent base frame* (no
/// `with_frame`), so later items, the entrypoint body, and the type annotations
/// all reduce through the accumulated definitions; then the entrypoint `body` is
/// erased against `expected`. The flat analogue of `erase_let`/`erase_rec`, minus
/// the de Bruijn open/close — top-level cross-references are already free `Var`s,
/// which erase to `ersd::Name`.
pub fn erase_module(
    context: &mut Context,
    module: &Module,
    expected: &Term,
) -> Result<ersd::Module, Error> {
    let mut items = Vec::with_capacity(module.items.len());

    for item in &module.items {
        match item {
            Item::Let(def) => {
                let body = erase(context, &def.body, &def.type_)?;
                context.define_assuming(&def.name, &def.type_, &def.body);

                items.push(ersd::Item::Let {
                    name: def.name.clone(),
                    body,
                });
            }
            Item::Rec(defs) => {
                for def in defs {
                    context.assume(&def.name, &def.type_);
                }

                for def in defs {
                    context.define(&def.name, &def.body);
                }

                let names: Vec<String> = defs.iter().map(|def| def.name.clone()).collect();

                let erased = defs
                    .iter()
                    .map(|def| erase(context, &def.body, &def.type_))
                    .collect::<Result<Vec<_>, Error>>()?;

                items.push(ersd::Item::Rec {
                    names,
                    items: erased,
                });
            }
        }
    }

    let body = erase(context, &module.body, expected)?;

    Ok(ersd::Module { items, body })
}

pub fn erase(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    // Attach this term's span to *any* error from erasing it. The dispatch lives
    // in `erase_subterm` so that its `?` short-circuits (e.g. a conversion
    // mismatch from `expect`) still flow through this wrapper rather than
    // escaping `erase` unspanned.
    let result = erase_subterm(context, term, expected);

    match term.span() {
        Some(span) => result.map_err(|error| error.at(span)),
        None => result,
    }
}

fn erase_subterm(context: &mut Context, term: &Term, expected: &Term) -> Result<ersd::Term, Error> {
    match &**term {
        Subterm::Prim(prim) => erase_prim(context, term, prim, expected),
        Subterm::BlnMatch(bm) => erase_bln_match(context, bm, term, expected),
        Subterm::NatMatch(nm) => erase_nat_match(context, nm, term, expected),
        // Type formers all erase to a runtime unit; they carry nothing to lower
        // and were already checked by `elaborate`.
        Subterm::Type | Subterm::FuncType(_) | Subterm::TupleType(_) | Subterm::AtomType(_) => {
            Ok(ersd::Term::Erased)
        }
        Subterm::Func(func) => erase_func(context, func, term, expected),
        Subterm::Apply(apply) => erase_apply(context, apply, term, expected),
        Subterm::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Subterm::Proj(proj) => erase_proj(context, proj, term, expected),
        Subterm::Atom(atom) => erase_atom(context, atom, term, expected),
        Subterm::Match(m) => erase_match(context, m, term, expected),
        Subterm::Let(let_) => erase_let(context, let_, expected),
        Subterm::Rec(lr) => erase_rec(context, lr, expected),
        Subterm::Var(var) => Ok(ersd::Term::Name(ersd::Name::from(var.unwrap()))),
        // Erase runs downstream of zonking, on a meta-free term (§9).
        Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erase"),
    }
}
