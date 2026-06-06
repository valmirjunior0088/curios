use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Error, Func, FuncType, Let, Match, Metavar, Nat,
        NatMatch, One, Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple, TupleType, Two, Var,
        check_motive, elaborate_prim, expect, expect_prim_head, reduce_with, refine_head,
    },
    std::collections::BTreeMap,
};

/// The elaboration mode (§6). `Infer` synthesizes a type; `Check(expected)`
/// drives the term against a known type, hitting `expect` at each synthesizable
/// node's turnaround and consuming `expected` directly at naturally-checked
/// nodes (`Func`, `Tuple`, `Atom`, `Metavar`).
#[derive(Debug, Clone)]
pub enum Mode {
    Infer,
    Check(Term),
}

/// Shorthand for the most common sub-obligation: drive `term` against `ty` and
/// discard the elaborated term (v1 elaboration is structure-preserving).
fn check(context: &mut Context, term: &Term, ty: Term) -> Result<(), Error> {
    elaborate(context, term, Mode::Check(ty)).map(|_| ())
}

fn elaborate_func_type(context: &mut Context, ft: &FuncType) -> Result<Term, Error> {
    fn walk(context: &mut Context, tele: Telescope<Term>) -> Result<(), Error> {
        match tele {
            Telescope::Done(body) => check(context, &body, Term::type_()),
            Telescope::Cons(ty, rest) => {
                check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &ty);
                walk(context, rest.open(&[&x]))
            }
        }
    }

    context.with_frame(|context| walk(context, ft.telescope.clone()))?;

    Ok(Term::type_())
}

fn elaborate_apply(context: &mut Context, apply: &Apply, term: &Term) -> Result<Term, Error> {
    let Apply { head, params } = apply;

    let head_type = elaborate(context, head, Mode::Infer)?.1;
    let head_type = reduce_with(context, &head_type)?;

    let ft = match &*head_type {
        Subterm::FuncType(ft) => ft.clone(),
        other => return Err(Error::not_a_function(term.clone(), other.clone())),
    };

    if params.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            term.clone(),
            ft.telescope.len(),
            params.len(),
        ));
    }

    ft.telescope
        .walk(params, |arg, ty| check(context, arg, ty.clone()))
}

fn elaborate_tuple_type(context: &mut Context, tt: &TupleType) -> Result<Term, Error> {
    fn walk(context: &mut Context, tele: Telescope<()>) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &ty);
                walk(context, rest.open(&[&x]))
            }
        }
    }

    context.with_frame(|context| walk(context, tt.telescope.clone()))?;

    Ok(Term::type_())
}

fn elaborate_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
) -> Result<Term, Error> {
    expect_prim_head(context, head, term, Prim::NatType)?;

    check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    check(
        context,
        zero_case,
        motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());
        context.assume(
            &ih_label,
            &motive.open(&[&Term::var(Var::free(&pred_label))]),
        );

        check(
            context,
            &succ_case.open(&[
                &Term::var(Var::free(&pred_label)),
                &Term::var(Var::free(&ih_label)),
            ]),
            motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::var(Var::free(&pred_label)),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
        )
    })?;

    Ok(motive.open(&[head]))
}

fn elaborate_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
) -> Result<Term, Error> {
    expect_prim_head(context, head, term, Prim::NatType)?;

    check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    for (n, body) in cases {
        context.with_frame(|context| {
            refine_head(
                context,
                head,
                &Subterm::Prim(Prim::Nat(Nat::new(*n))).into(),
            )?;
            check(
                context,
                body,
                motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(*n))).into()]),
            )
        })?;
    }

    check(context, default, motive.open(&[head]))?;

    Ok(motive.open(&[head]))
}

fn elaborate_nat_match(context: &mut Context, nm: &NatMatch, term: &Term) -> Result<Term, Error> {
    match nm {
        NatMatch::Induction {
            head,
            motive,
            zero_case,
            succ_case,
        } => elaborate_nat_induction(context, head, motive, zero_case, succ_case, term),
        NatMatch::Dispatch {
            head,
            motive,
            cases,
            default,
        } => elaborate_nat_dispatch(context, head, motive, cases, default, term),
    }
}

fn elaborate_bln_match(context: &mut Context, bm: &BlnMatch, term: &Term) -> Result<Term, Error> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;

    expect_prim_head(context, head, term, Prim::BlnType)?;

    check_motive(context, &Subterm::Prim(Prim::BlnType).into(), motive)?;

    context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into())?;
        check(
            context,
            false_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into())?;
        check(
            context,
            true_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    Ok(motive.open(&[head]))
}

fn elaborate_proj(context: &mut Context, proj: &Proj, term: &Term) -> Result<Term, Error> {
    let Proj { head, index } = proj;

    let head_type = elaborate(context, head, Mode::Infer)?.1;
    let head_type = reduce_with(context, &head_type)?;

    let TupleType { telescope } = match &*head_type {
        Subterm::TupleType(tt) => tt.clone(),
        other => return Err(Error::not_a_tuple(term.clone(), other.clone())),
    };

    if *index >= telescope.len() {
        return Err(Error::tuple_index_out_of_bounds(
            term.clone(),
            *index,
            telescope.len(),
        ));
    }

    Ok(telescope
        .nth(*index, |j| Term::proj((**head).clone(), j))
        .expect("index in range"))
}

fn elaborate_match(context: &mut Context, m: &Match, term: &Term) -> Result<Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let head_type = elaborate(context, head, Mode::Infer)?.1;
    let head_type = reduce_with(context, &head_type)?;

    let atoms = match &*head_type {
        Subterm::AtomType(AtomType { atoms }) => atoms.clone(),
        other => return Err(Error::not_an_atom_type(term.clone(), other.clone())),
    };

    check_motive(context, &Term::atom_type(atoms.iter().cloned()), motive)?;

    if cases.len() != atoms.len() {
        return Err(Error::match_arity_mismatch(
            term.clone(),
            atoms.len(),
            cases.len(),
        ));
    }

    for atom in &atoms {
        let body = if let Some(body) = cases.get(atom) {
            body
        } else {
            return Err(Error::match_case_missing(term.clone(), atom.clone()));
        };

        let expected = motive.open(&[&Term::atom(atom.clone())]);

        context.with_frame(|context| {
            refine_head(context, head, &Term::atom(atom.clone()))?;
            check(context, body, expected)
        })?;
    }

    Ok(motive.open(&[head]))
}

fn elaborate_let(
    context: &mut Context,
    let_: &Let,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Let { type_, body, tail } = let_;

    check(context, type_, Term::type_())?;
    check(context, body, type_.clone())?;

    let label = context.fresh(tail.first_label());

    // Propagate `mode` into the frame so a `Check(expected)` turnaround happens
    // where the let binding is in scope; `expected` is from the outer scope and
    // does not mention the bound name, so comparing inside the frame is sound.
    let tail_type = context.with_frame(|context| {
        context.define_assuming(&label, type_, body);

        let tail_type = elaborate(context, &tail.open(&[&Term::var(Var::free(label))]), mode)?.1;

        reduce_with(context, &tail_type)
    })?;

    Ok((term.clone(), tail_type))
}

fn elaborate_rec(
    context: &mut Context,
    rec: &Rec,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Rec { items, tail } = rec;

    let labels = tail
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();

    let label_terms = labels
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

    let tail_type = context.with_frame(|context| {
        for (label, (type_, _)) in labels.iter().zip(items.iter()) {
            context.assume(label, type_);
        }

        for (type_, _) in &items {
            check(context, type_, Term::type_())?;
        }

        for (label, (_, body)) in labels.iter().zip(items.iter()) {
            context.define(label, body);
        }

        for (_, (type_, body)) in labels.iter().zip(items.iter()) {
            check(context, body, type_.clone())?;
        }

        let tail_type = elaborate(context, &tail, mode)?.1;

        reduce_with(context, &tail_type)
    })?;

    Ok((term.clone(), tail_type))
}

fn elaborate_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Func { body } = func;

    let Mode::Check(expected) = mode else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let ft = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::FuncType(ft) => ft,
        _ => return Err(Error::not_a_function_type(term.clone(), expected.clone())),
    };

    let n = ft.telescope.len();

    let param_names = (0..n)
        .map(|i| context.fresh(body.label_iter().nth(i).flatten()))
        .collect::<Vec<_>>();
    let param_terms = param_names
        .iter()
        .map(|p| Term::var(Var::free(p)))
        .collect::<Vec<_>>();
    let param_refs = param_terms.iter().collect::<Vec<_>>();
    let body_opened = body.open(&param_refs);

    fn output_type(
        context: &mut Context,
        tele: Telescope<Term>,
        names: &[String],
        terms: &[Term],
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(body) => Ok(*body),
            Telescope::Cons(type_, rest) => {
                context.assume(&names[0], &type_);
                output_type(context, rest.open(&[&terms[0]]), &names[1..], &terms[1..])
            }
        }
    }

    context.with_frame(|context| {
        let output_type = output_type(context, ft.telescope, &param_names, &param_terms)?;
        check(context, &body_opened, output_type)
    })?;

    Ok((term.clone(), expected))
}

fn elaborate_tuple(
    context: &mut Context,
    tuple: &Tuple,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Tuple { fields } = tuple;

    let Mode::Check(expected) = mode else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let type_telescope = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::TupleType(TupleType { telescope }) => telescope,
        _ => {
            return Err(Error::not_a_tuple_type(
                Term::from(Subterm::Tuple(tuple.clone())),
                expected.clone(),
            ));
        }
    };

    if fields.len() != type_telescope.len() {
        return Err(Error::tuple_arity_mismatch(
            Term::from(Subterm::Tuple(tuple.clone())),
            type_telescope.len(),
            fields.len(),
        ));
    }

    fn walk(context: &mut Context, tele: Telescope<()>, fields: &[Term]) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let head = &fields[0];
                check(context, head, ty)?;
                walk(context, rest.open(&[head]), &fields[1..])
            }
        }
    }

    walk(context, type_telescope, fields)?;

    Ok((term.clone(), expected))
}

fn elaborate_atom(
    context: &mut Context,
    atom: &Atom,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Mode::Check(expected) = mode else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let atoms = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::AtomType(AtomType { atoms }) => atoms,
        _ => {
            return Err(Error::type_mismatch(
                term.clone(),
                Term::atom_type([atom.clone()]),
                expected.clone(),
            ));
        }
    };

    if !atoms.iter().any(|candidate| candidate == atom) {
        return Err(Error::type_mismatch(
            term.clone(),
            Term::atom_type([atom.clone()]),
            expected.clone(),
        ));
    }

    Ok((term.clone(), expected))
}

fn elaborate_metavar(
    context: &mut Context,
    metavar: &Metavar,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let id = metavar.id;

    match mode {
        // Birth (§5): freeze the local context as Γ and record the type the hole
        // is checked against. Births happen once per id, but a re-traversal in
        // the same mode is idempotent — re-check the recorded type against the
        // (identical) `expected`.
        Mode::Check(expected) => {
            if context.metavar_entry(id).is_some() {
                let result = context.metavar_entry(id).unwrap().result.clone();
                expect(context, term, &result, &expected)?;
            } else {
                let telescope = context.local_context().to_vec();
                context.birth_metavar(id, telescope, expected.clone(), term.span());
            }
            Ok((term.clone(), expected))
        }
        // A hole in synthesis position has no type to offer — unless it was
        // already born in a checking position, in which case report that type.
        Mode::Infer => match context.metavar_entry(id) {
            Some(entry) => Ok((term.clone(), entry.result.clone())),
            None => Err(Error::cannot_infer(term.clone())),
        },
    }
}

pub fn elaborate(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let result = elaborate_subterm(context, term, mode);

    match term.span() {
        Some(span) => result.map_err(|error| error.at(span)),
        None => result,
    }
}

fn elaborate_subterm(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // Synthesizable nodes compute their type and hit the `expect` turnaround in
    // `Check` mode; naturally-checked nodes (and the mode-propagating `Let`/`Rec`)
    // consume `mode` directly and return early.
    let type_ = match &**term {
        Subterm::Type => Term::type_(),
        Subterm::Prim(prim) => return elaborate_prim(context, term, prim, mode),
        Subterm::BlnMatch(bm) => elaborate_bln_match(context, bm, term)?,
        Subterm::NatMatch(nm) => elaborate_nat_match(context, nm, term)?,
        Subterm::FuncType(ft) => elaborate_func_type(context, ft)?,
        Subterm::Apply(apply) => elaborate_apply(context, apply, term)?,
        Subterm::TupleType(tt) => elaborate_tuple_type(context, tt)?,
        Subterm::Proj(proj) => elaborate_proj(context, proj, term)?,
        Subterm::AtomType(_) => Term::type_(),
        Subterm::Match(m) => elaborate_match(context, m, term)?,
        Subterm::Let(let_) => return elaborate_let(context, let_, term, mode),
        Subterm::Rec(rec) => return elaborate_rec(context, rec, term, mode),
        Subterm::Var(var) => match context.assumption(var.unwrap()) {
            Some(type_) => type_.clone(),
            None => return Err(Error::unbound_variable(Term::var(var.clone()))),
        },
        Subterm::Func(func) => return elaborate_func(context, func, term, mode),
        Subterm::Tuple(tuple) => return elaborate_tuple(context, tuple, term, mode),
        Subterm::Atom(atom) => return elaborate_atom(context, atom, term, mode),
        Subterm::Metavar(metavar) => return elaborate_metavar(context, metavar, term, mode),
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((term.clone(), type_))
}
