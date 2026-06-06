use {
    super::{
        Apply, Atom, AtomType, BlnMatch, Context, Definition, Error, Func, FuncType, Item, Let,
        Match, Metavar, Module, Nat, NatMatch, One, Prim, Proj, Rec, Scope, Subterm, Telescope,
        Term, Tuple, TupleType, Two, Var, check_motive, elaborate_prim, expect, reduce_with,
        refine_head,
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

/// Drive `term` against `ty` and return the *elaborated* term — the rebuilt,
/// de-Bruijn-correct subterm whose lambda domains are solved and whose binders
/// are re-closed (§9). Elaboration is authoritative: this output, not the
/// original lowered term, is what flows on to `zonk`/`erase`.
fn check(context: &mut Context, term: &Term, ty: Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Check(ty)).map(|(term, _)| term)
}

fn elaborate_func_type(context: &mut Context, ft: &FuncType) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<Term>,
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match tele {
            Telescope::Done(output) => check(context, &output, Term::type_()),
            Telescope::Cons(ty, rest) => {
                let domain = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &ty);
                domains.push((name, domain));
                walk(context, rest.open(&[&x]), domains)
            }
        }
    }

    let mut domains = Vec::new();
    let output =
        context.with_frame(|context| walk(context, ft.telescope.clone(), &mut domains))?;

    Ok((Term::func_type(domains, output), Term::type_()))
}

fn elaborate_apply(
    context: &mut Context,
    apply: &Apply,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let Apply { head, params } = apply;

    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
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

    let mut elaborated = Vec::with_capacity(params.len());
    let output = ft.telescope.clone().walk(params, |arg, ty| {
        elaborated.push(check(context, arg, ty.clone())?);
        Ok(())
    })?;

    Ok((Term::apply(head, elaborated), output))
}

fn elaborate_tuple_type(
    context: &mut Context,
    tt: &TupleType,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &mut Vec<(String, Term)>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let field = check(context, &ty, Term::type_())?;
                let name = context.fresh(rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &ty);
                fields.push((name, field));
                walk(context, rest.open(&[&x]), fields)
            }
        }
    }

    let mut fields = Vec::new();
    context.with_frame(|context| walk(context, tt.telescope.clone(), &mut fields))?;

    Ok((Term::tuple_type(fields), Term::type_()))
}

/// Infer and rebuild a match scrutinee, requiring its reduced type to be the
/// given primitive type. The authoritative analogue of `expect_prim_head` (kept
/// for `erase`): it returns the rebuilt head alongside its reduced type.
fn elaborate_prim_head(
    context: &mut Context,
    head: &Term,
    term: &Term,
    expected: Prim,
) -> Result<(Term, Term), Error> {
    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    match expected {
        Prim::NatType if matches!(&*head_type, Subterm::Prim(Prim::NatType)) => Ok((head, head_type)),
        Prim::BlnType if matches!(&*head_type, Subterm::Prim(Prim::BlnType)) => Ok((head, head_type)),
        Prim::NatType => Err(Error::not_nat_type(term.clone(), head_type)),
        Prim::BlnType => Err(Error::not_bln_type(term.clone(), head_type)),
        _ => unreachable!("elaborate_prim_head supports only NatType and BlnType"),
    }
}

fn elaborate_nat_induction(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, term, Prim::NatType)?;

    let motive_elaborated = check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    let zero_elaborated = check(
        context,
        zero_case,
        motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let succ_body = context.with_frame(|context| {
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

    let succ_elaborated =
        Scope::close(Two, &[pred_label.as_str(), ih_label.as_str()], succ_body);

    let rebuilt = Subterm::NatMatch(NatMatch::Induction {
        head: head_elaborated,
        motive: motive_elaborated,
        zero_case: zero_elaborated,
        succ_case: succ_elaborated,
    })
    .into();

    Ok((rebuilt, motive.open(&[head])))
}

fn elaborate_nat_dispatch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<One>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let (head_elaborated, _) = elaborate_prim_head(context, head, term, Prim::NatType)?;

    let motive_elaborated = check_motive(context, &Subterm::Prim(Prim::NatType).into(), motive)?;

    let mut cases_elaborated = BTreeMap::new();
    for (n, body) in cases {
        let body = context.with_frame(|context| {
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
        cases_elaborated.insert(*n, body);
    }

    let default_elaborated = check(context, default, motive.open(&[head]))?;

    let rebuilt = Subterm::NatMatch(NatMatch::Dispatch {
        head: head_elaborated,
        motive: motive_elaborated,
        cases: cases_elaborated,
        default: default_elaborated,
    })
    .into();

    Ok((rebuilt, motive.open(&[head])))
}

fn elaborate_nat_match(
    context: &mut Context,
    nm: &NatMatch,
    term: &Term,
) -> Result<(Term, Term), Error> {
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

fn elaborate_bln_match(
    context: &mut Context,
    bm: &BlnMatch,
    term: &Term,
) -> Result<(Term, Term), Error> {
    let BlnMatch {
        head,
        motive,
        false_case,
        true_case,
    } = bm;

    let (head_elaborated, _) = elaborate_prim_head(context, head, term, Prim::BlnType)?;

    let motive_elaborated = check_motive(context, &Subterm::Prim(Prim::BlnType).into(), motive)?;

    let false_elaborated = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(false)).into())?;
        check(
            context,
            false_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(false)).into()]),
        )
    })?;

    let true_elaborated = context.with_frame(|context| {
        refine_head(context, head, &Subterm::Prim(Prim::Bln(true)).into())?;
        check(
            context,
            true_case,
            motive.open(&[&Subterm::Prim(Prim::Bln(true)).into()]),
        )
    })?;

    let rebuilt = Subterm::BlnMatch(BlnMatch {
        head: head_elaborated,
        motive: motive_elaborated,
        false_case: false_elaborated,
        true_case: true_elaborated,
    })
    .into();

    Ok((rebuilt, motive.open(&[head])))
}

fn elaborate_proj(context: &mut Context, proj: &Proj, term: &Term) -> Result<(Term, Term), Error> {
    let Proj { head, index } = proj;

    let (head, head_type) = elaborate(context, head, Mode::Infer)?;
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

    let field_type = telescope
        .nth(*index, |j| Term::proj(head.clone(), j))
        .expect("index in range");

    Ok((Term::proj(head, *index), field_type))
}

fn elaborate_match(context: &mut Context, m: &Match, term: &Term) -> Result<(Term, Term), Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    let (head_elaborated, head_type) = elaborate(context, head, Mode::Infer)?;
    let head_type = reduce_with(context, &head_type)?;

    let atoms = match &*head_type {
        Subterm::AtomType(AtomType { atoms }) => atoms.clone(),
        other => return Err(Error::not_an_atom_type(term.clone(), other.clone())),
    };

    let motive_elaborated = check_motive(context, &Term::atom_type(atoms.iter().cloned()), motive)?;

    if cases.len() != atoms.len() {
        return Err(Error::match_arity_mismatch(
            term.clone(),
            atoms.len(),
            cases.len(),
        ));
    }

    let mut cases_elaborated = BTreeMap::new();
    for atom in &atoms {
        let body = if let Some(body) = cases.get(atom) {
            body
        } else {
            return Err(Error::match_case_missing(term.clone(), atom.clone()));
        };

        let expected = motive.open(&[&Term::atom(atom.clone())]);

        let body = context.with_frame(|context| {
            refine_head(context, head, &Term::atom(atom.clone()))?;
            check(context, body, expected)
        })?;

        cases_elaborated.insert(atom.clone(), body);
    }

    let rebuilt = Subterm::Match(Match {
        head: head_elaborated,
        motive: motive_elaborated,
        cases: cases_elaborated,
    })
    .into();

    Ok((rebuilt, motive.open(&[head])))
}

fn elaborate_let(
    context: &mut Context,
    let_: &Let,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Let { type_, body, tail } = let_;

    let type_elaborated = check(context, type_, Term::type_())?;
    let body_elaborated = check(context, body, type_.clone())?;

    let label = context.fresh(tail.first_label());

    // Propagate `mode` into the frame so a `Check(expected)` turnaround happens
    // where the let binding is in scope; `expected` is from the outer scope and
    // does not mention the bound name, so comparing inside the frame is sound.
    // The binding is `define`d with the *original* body, which `reduce`/`convert`
    // (domain-blind) treat identically to the rebuilt one.
    let (tail_elaborated, tail_type) = context.with_frame(|context| {
        context.define_assuming(&label, type_, body);

        let (tail_elaborated, tail_type) =
            elaborate(context, &tail.open(&[&Term::var(Var::free(&label))]), mode)?;

        Ok::<_, Error>((tail_elaborated, reduce_with(context, &tail_type)?))
    })?;

    let rebuilt = Term::let_(label, type_elaborated, body_elaborated, tail_elaborated);

    Ok((rebuilt, tail_type))
}

fn elaborate_rec(
    context: &mut Context,
    rec: &Rec,
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

    let (types_elaborated, bodies_elaborated, tail_elaborated, tail_type) =
        context.with_frame(|context| {
            for (label, (type_, _)) in labels.iter().zip(items.iter()) {
                context.assume(label, type_);
            }

            let mut types_elaborated = Vec::with_capacity(items.len());
            for (type_, _) in &items {
                types_elaborated.push(check(context, type_, Term::type_())?);
            }

            for (label, (_, body)) in labels.iter().zip(items.iter()) {
                context.define(label, body);
            }

            let mut bodies_elaborated = Vec::with_capacity(items.len());
            for (type_, body) in &items {
                bodies_elaborated.push(check(context, body, type_.clone())?);
            }

            let (tail_elaborated, tail_type) = elaborate(context, &tail, mode)?;

            Ok::<_, Error>((
                types_elaborated,
                bodies_elaborated,
                tail_elaborated,
                reduce_with(context, &tail_type)?,
            ))
        })?;

    let triples = labels
        .into_iter()
        .zip(types_elaborated)
        .zip(bodies_elaborated)
        .map(|((label, type_), body)| (label, type_, body));

    Ok((Term::rec(triples, tail_elaborated), tail_type))
}

fn elaborate_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Func { telescope } = func;

    let Mode::Check(expected) = mode else {
        return Err(Error::cannot_infer(term.clone()));
    };

    let ft = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::FuncType(ft) => ft,
        _ => return Err(Error::not_a_function_type(term.clone(), expected.clone())),
    };

    if telescope.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            term.clone(),
            ft.telescope.len(),
            telescope.len(),
        ));
    }

    // Walk the lambda's own telescope (whose `Done` is the body) alongside the
    // expected function type's telescope (whose `Done` is the output type) in
    // lockstep. Each parameter's domain is taken from the expected type; the
    // lambda's own domain — a hole when the surface annotation was omitted — is
    // unified against it via `expect`, which solves the hole (or, once
    // annotations exist, checks it). The rebuilt lambda then *carries* the
    // expected domain rather than the hole, so re-closing it below (and re-closing
    // every enclosing binder) captures any free names the domain mentions — this
    // is what keeps nested lambda domains de-Bruijn-correct for `zonk`/`erase`.
    fn walk(
        context: &mut Context,
        term: &Term,
        body: Telescope<Term>,
        type_: Telescope<Term>,
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match (body, type_) {
            (Telescope::Done(body), Telescope::Done(output)) => check(context, &body, *output),
            (Telescope::Cons(domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                check(context, &domain, Term::type_())?;
                expect(context, term, &domain, &type_)?;
                let name = context.fresh(body_rest.first_label());
                let x = Term::var(Var::free(&name));
                context.assume(&name, &type_);
                domains.push((name, type_.clone()));
                walk(
                    context,
                    term,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    domains,
                )
            }
            // Arities were checked equal above.
            _ => unreachable!("function/type telescope arity mismatch"),
        }
    }

    let mut domains = Vec::new();
    let body = context
        .with_frame(|context| walk(context, term, telescope.clone(), ft.telescope, &mut domains))?;

    Ok((Term::func(domains, body), expected))
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

    fn walk(
        context: &mut Context,
        tele: Telescope<()>,
        fields: &[Term],
        elaborated: &mut Vec<Term>,
    ) -> Result<(), Error> {
        match tele {
            Telescope::Done(_) => Ok(()),
            Telescope::Cons(ty, rest) => {
                let head = &fields[0];
                elaborated.push(check(context, head, ty)?);
                walk(context, rest.open(&[head]), &fields[1..], elaborated)
            }
        }
    }

    let mut elaborated = Vec::with_capacity(fields.len());
    walk(context, type_telescope, fields, &mut elaborated)?;

    Ok((Term::tuple(elaborated), expected))
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

/// Type-check a single non-recursive top-level definition, `define` it into the
/// *current* (persistent base) frame, and return its rebuilt form. The flat
/// analogue of `elaborate_let`'s per-binding work, minus the `with_frame`/tail
/// recursion: the binding must stay in scope for every later item and the
/// entrypoint body. The original body is `define`d (domain-blind reduction makes
/// it interchangeable with the rebuilt one); the rebuilt `Definition` flows on to
/// `zonk`/`erase`.
fn elaborate_module_let(context: &mut Context, def: &Definition) -> Result<Definition, Error> {
    let type_ = check(context, &def.type_, Term::type_())?;
    let body = check(context, &def.body, def.type_.clone())?;
    context.define_assuming(&def.name, &def.type_, &def.body);

    Ok(Definition {
        name: def.name.clone(),
        type_,
        body,
    })
}

/// Type-check a top-level `rec` group, `define` every member into the current
/// frame, and return their rebuilt forms. The flat analogue of `elaborate_rec` —
/// assume all signatures, check the types, define all bodies, then check the
/// bodies — but with no de Bruijn open/close: members already reference each
/// other by free name.
fn elaborate_module_rec(
    context: &mut Context,
    defs: &[Definition],
) -> Result<Vec<Definition>, Error> {
    for def in defs {
        context.assume(&def.name, &def.type_);
    }

    let mut types = Vec::with_capacity(defs.len());
    for def in defs {
        types.push(check(context, &def.type_, Term::type_())?);
    }

    for def in defs {
        context.define(&def.name, &def.body);
    }

    let mut bodies = Vec::with_capacity(defs.len());
    for def in defs {
        bodies.push(check(context, &def.body, def.type_.clone())?);
    }

    Ok(defs
        .iter()
        .zip(types)
        .zip(bodies)
        .map(|((def, type_), body)| Definition {
            name: def.name.clone(),
            type_,
            body,
        })
        .collect())
}

/// Elaborate a whole [`Module`] (§9). Each top-level item is checked and `define`d
/// *cumulatively in the persistent base frame* — never a popped `with_frame` —
/// so every definition stays in scope for later items, the entrypoint `body`, and
/// (through `mode`) its type annotation. Returns the rebuilt module (lambda
/// domains solved, binders re-closed) alongside the body's type, reduced through
/// the accumulated definitions.
///
/// Elaboration is authoritative: the returned module — not the lowered input — is
/// what `zonk_module` then makes meta-free for `erase`.
pub fn elaborate_module(
    context: &mut Context,
    module: &Module,
    mode: Mode,
) -> Result<(Module, Term), Error> {
    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        items.push(match item {
            Item::Let(def) => Item::Let(elaborate_module_let(context, def)?),
            Item::Rec(defs) => Item::Rec(elaborate_module_rec(context, defs)?),
        });
    }

    let (body, body_type) = elaborate(context, &module.body, mode)?;
    let body_type = reduce_with(context, &body_type)?;

    let module = Module {
        items,
        type_: module.type_.clone(),
        body,
    };

    Ok((module, body_type))
}

pub fn elaborate(context: &mut Context, term: &Term, mode: Mode) -> Result<(Term, Term), Error> {
    let result = elaborate_subterm(context, term, mode);

    // Carry the source span onto the rebuilt term as well as onto any error, so
    // downstream passes keep reporting against the original syntax.
    match term.span() {
        Some(span) => result
            .map(|(term, type_)| (term.with_span(span.clone()), type_))
            .map_err(|error| error.at(span)),
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
    // consume `mode` directly and return early. Every arm returns the rebuilt
    // term — binders re-closed, lambda domains solved (§9).
    let (rebuilt, type_) = match &**term {
        Subterm::Type => (term.clone(), Term::type_()),
        Subterm::Prim(prim) => return elaborate_prim(context, term, prim, mode),
        Subterm::BlnMatch(bm) => elaborate_bln_match(context, bm, term)?,
        Subterm::NatMatch(nm) => elaborate_nat_match(context, nm, term)?,
        Subterm::FuncType(ft) => elaborate_func_type(context, ft)?,
        Subterm::Apply(apply) => elaborate_apply(context, apply, term)?,
        Subterm::TupleType(tt) => elaborate_tuple_type(context, tt)?,
        Subterm::Proj(proj) => elaborate_proj(context, proj, term)?,
        Subterm::AtomType(_) => (term.clone(), Term::type_()),
        Subterm::Match(m) => elaborate_match(context, m, term)?,
        Subterm::Let(let_) => return elaborate_let(context, let_, mode),
        Subterm::Rec(rec) => return elaborate_rec(context, rec, mode),
        Subterm::Var(var) => match context.assumption(var.unwrap()) {
            Some(type_) => (term.clone(), type_.clone()),
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

    Ok((rebuilt, type_))
}
