use super::{Context, Error, Mode, One, Prim, Proj, Scope, Subterm, Term, Var, elaborate};

/// Synthesis is just `elaborate` in `Infer` mode, projecting out the type. Kept
/// as a thin shim so the many existing call sites (this module, `erase*.rs`,
/// tests) read unchanged while erase is migrated to downstream lowering (§6).
pub fn infer(context: &mut Context, term: &Term) -> Result<Term, Error> {
    elaborate(context, term, Mode::Infer).map(|(_, type_)| type_)
}

pub fn reduce_with(context: &mut Context, term: &Term) -> Result<Term, Error> {
    super::reduce(context, term.clone())
        .map_err(|error| error.into_error(|| Error::reduce_preempted(term.clone())))
}

pub fn convert_with(context: &mut Context, this: &Term, that: &Term) -> Result<bool, Error> {
    super::convert(context, &Term::type_(), this, that)
        .map_err(|error| error.into_error(|| Error::convert_preempted(this.clone(), that.clone())))
}

pub fn expect(
    context: &mut Context,
    term: &Term,
    inferred: &Term,
    expected: &Term,
) -> Result<(), Error> {
    match convert_with(context, inferred, expected)? {
        true => Ok(()),
        false => Err(Error::type_mismatch(
            term.clone(),
            inferred.clone(),
            expected.clone(),
        )),
    }
}

/// Register a counterfactual match-arm refinement of the scrutinee: a `Var`
/// scrutinee reduces to the arm's value inside the arm (`refine`), and a
/// projection scrutinee (e.g. an atom match on a tuple field) refines that
/// projection (`refine_projection`). The frame containing the refinement is
/// scoped to the arm, so the (possibly counterfactual) assumption does not
/// leak. Any other scrutinee shape records nothing — there is no stable key
/// to refine, and the arm then types against the unrefined head.
pub fn refine_head(context: &mut Context, head: &Term, value: &Term) {
    match &**head {
        Subterm::Var(var) => {
            context.refine(var.unwrap(), value);
        }
        Subterm::Proj(Proj { head, index }) => {
            context.refine_projection(head.clone(), *index, value.clone());
        }
        _ => {}
    }
}

/// Check that `motive` is a well-formed type family over a scrutinee of `head_type`,
/// returning the rebuilt motive. Shared by every match form. Runs the motive
/// through `elaborate` in `Check(Type)` mode — erase is downstream lowering now
/// and no longer type-checks (§6) — and re-closes the elaborated body so the
/// motive carries its solved/re-closed form (§9).
pub fn check_motive(
    context: &mut Context,
    head_type: &Term,
    motive: &Scope<One>,
) -> Result<Scope<One>, Error> {
    let label = context.fresh(motive.first_label());

    context.with_frame(|context| {
        context.assume(&label, head_type);
        let body = elaborate(
            context,
            &motive.open(&[&Term::var(Var::free(&label))]),
            Mode::Check(Term::type_()),
        )?
        .0;
        Ok(Scope::close(One, &[label.as_str()], body))
    })
}

/// Infer the scrutinee's type, reduce it, and require it to be the given prim type
/// (`Prim::NatType` or `Prim::BlnType`). Returns the reduced head type — used by `erase`
/// to erase the head; ignored by `infer`.
pub fn expect_prim_head(
    context: &mut Context,
    head: &Term,
    term: &Term,
    expected: Prim,
) -> Result<Term, Error> {
    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    match expected {
        Prim::NatType if matches!(&*head_type, Subterm::Prim(Prim::NatType)) => Ok(head_type),
        Prim::BlnType if matches!(&*head_type, Subterm::Prim(Prim::BlnType)) => Ok(head_type),
        Prim::NatType => Err(Error::not_nat_type(term.clone(), head_type)),
        Prim::BlnType => Err(Error::not_bln_type(term.clone(), head_type)),
        _ => unreachable!("expect_prim_head supports only NatType and BlnType"),
    }
}
