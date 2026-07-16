use super::*;

/// Elaborate a local `let` block. The bindings are a flat `Vec` in one node,
/// so this loops over them — elaborating each binding's type/body, minting its
/// binder, and defining it in a single frame — rather than recursing once per
/// binding, which a long straight-line sequence of `let`s would overflow the
/// stack with. The tail continues with one ordinary (recursive) `elaborate`,
/// its depth bounded by how often `let` and `rec` alternate, not by chain
/// length. Rebuilding folds through `Term::let_`, which merges the bindings
/// back into a single flat `Let`. The whole block is one source term with one
/// span, stamped by `elaborate`'s wrapper — no per-binding span bookkeeping.
pub(super) fn elaborate_let(
    context: &mut Context,
    let_: &Let,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    context.with_frame(|context| {
        let mut label_terms = Vec::<Term>::with_capacity(let_.bindings.len());
        let mut triples = Vec::<(String, Term, Term)>::with_capacity(let_.bindings.len());

        for (index, (type_, body)) in let_.bindings.iter().enumerate() {
            let (type_, body) = {
                let refs = label_terms.iter().collect::<Vec<_>>();
                (type_.release(&refs), body.release(&refs))
            };

            // A bare metavar annotation is the lowering of a typeless local
            // `let x = e` (equivalently `let x : _ = e`): infer the body's type
            // instead of checking the body against the hole. This is what lets a
            // lambda/tuple/atom body — which `check` against an unsolved hole
            // would reject — be bound without an annotation. Otherwise check the
            // body against the (possibly partial) annotation, as before.
            let (type_elaborated, body_elaborated, assumed) = match &*type_ {
                Subterm::Metavar(_) => {
                    let (body_elaborated, inferred) = elaborate(context, &body, Mode::Infer)?;
                    (inferred.clone(), body_elaborated, inferred)
                }
                // The body is checked against — and the binder assumed at — the
                // *rebuilt* annotation: insertion saturates applications during
                // elaboration, and a lowered (under-applied) type reaching the
                // reducer would open a telescope at the wrong arity.
                _ => {
                    let type_elaborated = check(context, &type_, Term::type_())?;
                    let body_elaborated = check(context, &body, type_elaborated.clone())?;
                    (type_elaborated.clone(), body_elaborated, type_elaborated)
                }
            };

            let label = context.fresh(let_.tail.label_iter().nth(index).flatten());

            // Define the binding with the *rebuilt* body so the tail's
            // type-level evaluation does not reduce through the lowered
            // (under-applied) original.
            context.define_assuming(&label, &assumed, &body_elaborated);

            label_terms.push(Term::free_var(&label));
            triples.push((label, type_elaborated, body_elaborated));
        }

        // Propagate `mode` into the tail: a `Check(expected)` turnaround happens
        // where the bindings are in scope; `expected` comes from the outer scope
        // and does not mention them, so comparing inside the frame is sound.
        let tail = let_.tail.open(&label_terms.iter().collect::<Vec<_>>());
        let (tail_elaborated, tail_type) = elaborate(context, &tail, mode)?;
        let tail_type = reduce_with(context, &tail_type)?;

        let rebuilt = triples
            .into_iter()
            .rev()
            .fold(tail_elaborated, |tail, (label, type_, body)| {
                Term::let_(label, type_, body, tail)
            });

        Ok((rebuilt, tail_type))
    })
}

/// Elaborate a local `rec` group and its tail. The group's mutually-recursive
/// bindings are one node, so this handles them at once; the tail recurses
/// through one ordinary `elaborate`, bounded by `let`/`rec` alternation.
pub(super) fn elaborate_rec(
    context: &mut Context,
    rec: &Rec,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    context.with_frame(|context| {
        let labels = rec
            .tail
            .label_iter()
            .map(|l| context.fresh(l))
            .collect::<Vec<_>>();

        let label_terms = labels
            .iter()
            .map(Var::free)
            .map(Term::var)
            .collect::<Vec<_>>();
        let label_refs = label_terms.iter().collect::<Vec<_>>();

        let items = rec
            .group
            .items()
            .iter()
            .map(|(type_, body)| (type_.open(&label_refs), body.open(&label_refs)))
            .collect::<Vec<_>>();

        for (label, (type_, _)) in labels.iter().zip(items.iter()) {
            context.assume(label, type_);
        }

        let mut types_elaborated = Vec::with_capacity(items.len());
        for (type_, _) in &items {
            types_elaborated.push(check(context, type_, Term::type_())?);
        }

        // Upgrade the assumptions to the *rebuilt* signatures before any body is
        // checked: a lowered (under-applied) type reaching the reducer would open
        // a telescope at the wrong arity.
        for (label, type_) in labels.iter().zip(&types_elaborated) {
            context.reassume(label, type_);
        }

        // Recursive names point at protected slots, never lowered bodies. A
        // sibling that productively needs an earlier member sees its rebuilt
        // solution; a dependency on a later member parks on the unsolved slot.
        let slots = labels
            .iter()
            .zip(&types_elaborated)
            .map(|(label, type_)| {
                let (id, slot) = context.fresh_rec_slot(type_.clone());
                context.define(label, &slot);
                id
            })
            .collect::<Vec<_>>();

        let mut bodies_elaborated = Vec::with_capacity(items.len());
        for (((_, body), type_), slot) in items.iter().zip(&types_elaborated).zip(slots) {
            let body = check(context, body, type_.clone())?;
            context.fill_rec_slot(slot, body.clone());
            bodies_elaborated.push(body);
        }
        context.retry_parked()?;

        let triples = labels
            .iter()
            .cloned()
            .zip(types_elaborated)
            .zip(bodies_elaborated)
            .map(|((label, type_), body)| (label, type_, body))
            .collect::<Vec<_>>();

        let tail = rec.tail.open(&label_refs);
        let (tail_elaborated, tail_type) = elaborate(context, &tail, mode)?;
        let tail_type = reduce_with(context, &tail_type)?;

        Ok((Term::rec(triples, tail_elaborated), tail_type))
    })
}

pub(super) fn elaborate_func(
    context: &mut Context,
    func: &Func,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Func { telescope } = func;

    match mode {
        Mode::Check(expected) => elaborate_func_check(context, telescope, term, expected),
        Mode::Infer => elaborate_func_infer(context, telescope),
    }
}

/// Park a whole *checking problem* (§8): a checked-only introduction form
/// met an expected type whose structure is still an unsolved metavariable —
/// possibly pinned by a constraint parked moments ago. A fresh placeholder
/// metavariable stands in the rebuilt tree; once the expected type's metas
/// solve, the problem re-checks under its frozen frame and the placeholder is
/// solved with the rebuilt term (the spine machinery splices it wherever the
/// occurrence travelled).
pub(super) fn park_checking(
    context: &mut Context,
    term: &Term,
    expected: &Term,
) -> Result<(Term, Term), Error> {
    let (placeholder, stand_in) = context.fresh_placeholder(expected.clone(), term.span());
    context.park(
        ParkedWork::Checking {
            term: term.clone(),
            expected: expected.clone(),
            placeholder,
        },
        term.clone(),
    );

    Ok((stand_in, expected.clone()))
}

/// Resolve a polymorphic numeric literal ([`NumLit`]) to a concrete scalar
/// primitive. In `Check` mode the expected type pins the choice; an expected
/// type that is still a bare unsolved metavar — and `Infer` mode — fall back to
/// the literal's shape default (`Int` when a sign was written, else `Nat`), and
/// the closing `expect` then solves that metavar to the chosen type. The literal
/// resolves *eagerly*: deferring it would strand downstream elaboration that
/// needs the type immediately (a projection off the literal's type, say). The
/// operator (`elaborate_infix`) pins its operand type from the non-literal side
/// first, so a literal there sees a concrete type and `1 + flt` still works.
/// Decimal literals never reach here; they parse straight to `Flt`.
pub(super) fn elaborate_num_lit(
    context: &mut Context,
    num_lit: &NumLit,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let nat_type: Term = Subterm::Prim(Prim::NatType).into();
    let byte_type: Term = Subterm::Prim(Prim::ByteType).into();
    let int_type: Term = Subterm::Prim(Prim::IntType).into();
    let flt_type: Term = Subterm::Prim(Prim::FltType).into();

    // A written sign rules out `Nat`, so the default lands on `Int`.
    let default_type: Term = if num_lit.signed {
        int_type.clone()
    } else {
        nat_type.clone()
    };

    let target = match &mode {
        Mode::Check(expected) => {
            let reduced = reduce_with(context, expected)?;
            match &*reduced {
                // Nothing concrete to resolve against yet — commit to the shape
                // default; the closing `expect` solves the metavar to it.
                Subterm::Metavar(Metavar { id, .. }) if context.metavar_solution(*id).is_none() => {
                    Term::unwrap_or_clone(default_type.clone())
                }
                _ => Term::unwrap_or_clone(reduced),
            }
        }
        Mode::Infer => Term::unwrap_or_clone(default_type.clone()),
    };

    let (prim, type_) = match &target {
        Subterm::Prim(Prim::NatType) if !num_lit.negative => {
            (Prim::Nat(Nat::new(num_lit.magnitude.clone())), nat_type)
        }
        Subterm::Prim(Prim::ByteType) if !num_lit.negative => {
            let Some(value) = num_lit.magnitude.to_u8() else {
                return Err(Error::ByteLiteralOutOfRange {
                    value: num_lit.magnitude.to_string(),
                });
            };
            (Prim::Byte(value), byte_type)
        }
        Subterm::Prim(Prim::IntType) => {
            let magnitude = BigInt::from(num_lit.magnitude.clone());
            let value = if num_lit.negative {
                -magnitude
            } else {
                magnitude
            };
            (Prim::Int(Int::new(value)), int_type)
        }
        Subterm::Prim(Prim::FltType) => {
            let magnitude = num_lit.magnitude.to_f32().unwrap_or(f32::INFINITY);
            let value = if num_lit.negative {
                -magnitude
            } else {
                magnitude
            };
            (Prim::Flt(Flt::from_f32(value)), flt_type)
        }
        // A concrete expected type that is non-numeric — or `Nat` for a negative
        // literal — has no realization: report against the literal's own shape.
        _ => {
            let Mode::Check(expected) = &mode else {
                unreachable!("Infer-mode target is always the Nat/Int shape default");
            };
            let inferred = if num_lit.negative {
                int_type
            } else {
                default_type
            };
            return Err(Error::type_mismatch(inferred, expected.clone()));
        }
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::prim(prim), type_))
}

impl Infix {
    /// The shape default for an infix operator whose operand type nothing
    /// pinned: any signed/negative literal operand forces `Int`, otherwise
    /// `Nat`.
    pub(super) fn default_type(&self) -> Prim {
        let signed =
            |operand: &Term| matches!(&**operand, Subterm::NumLit(num_lit) if num_lit.signed);

        if signed(&self.left) || signed(&self.right) {
            Prim::IntType
        } else {
            Prim::NatType
        }
    }
}

/// Elaborate an infix operator ([`Infix`]) as a concept method call. A fresh
/// operand-type metavar `?T` is pinned by the non-literal operands first (or,
/// for arithmetic operators, by the expected result type), then defaulted from
/// the operand literals if nothing constrains it; only then are the literal
/// operands checked — against a `?T` that is already concrete, so they never
/// force it to their own default. That ordering is what lets `1 + flt` resolve
/// to `Flt` rather than a `Nat`/`Flt` mismatch.
///
/// Dispatch is then **one path**: every operator, `&&`/`||` included,
/// desugars to a projection of a witness of its `/syn` concept
/// ([`NumOp::concept_field`](NumOp::concept_field)) — `a + b` ≙
/// `Add/add(a, b)`, primitives included,
/// resolved by the same engine that fills `use` slots (so `no witness of
/// Add(Point)` is the single error vocabulary, and what an operator means at
/// a type is entirely a question of which witnesses exist). `!=` rebuilds as
/// `BlnXor(Eql/eql(a, b), true)` — no `BlnNot` prim exists. The node never
/// survives elaboration; witness projections over the statically-known
/// primitive witnesses collapse back to bare `Prim` code in the backend
/// (`And(Bln)`/`Or(Bln)` collapse to `BlnAnd`/`BlnOr` exactly as `Eql(Bln)`
/// collapses to `BlnEql` — see the codegen parity tests).
pub(super) fn elaborate_infix(
    context: &mut Context,
    infix: &Infix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let bln_type: Term = Subterm::Prim(Prim::BlnType).into();

    // `?T`: the operand type shared by both sides.
    let (operand_id, operand_type) = context.fresh_placeholder(Term::type_(), term.span());

    // An arithmetic operator returns its operand type, so an expected result
    // type pins `?T` straight away; a comparison returns `Bln`, which says
    // nothing about the operands, so only the operands can pin it.
    if !infix.op.result_is_bln()
        && let Mode::Check(expected) = &mode
    {
        expect(context, term, &operand_type, expected)?;
    }

    let left_is_literal = matches!(&*infix.left, Subterm::NumLit(_));
    let right_is_literal = matches!(&*infix.right, Subterm::NumLit(_));

    // Phase 1: the non-literal operands pin `?T` from their own types.
    let mut left = match left_is_literal {
        false => Some(elaborate(context, &infix.left, Mode::Check(operand_type.clone()))?.0),
        true => None,
    };
    let mut right = match right_is_literal {
        false => Some(elaborate(context, &infix.right, Mode::Check(operand_type.clone()))?.0),
        true => None,
    };

    // Nothing pinned `?T` — every non-literal operand left it open. Default from
    // the operand shapes so the literal operands have a concrete type to take.
    if context.metavar_solution(operand_id).is_none() {
        let default = infix.default_type();
        context.solve_metavar(operand_id, Subterm::Prim(default).into());
    }

    // Phase 2: the literal operands resolve against the now-concrete `?T`.
    if left_is_literal {
        left = Some(elaborate(context, &infix.left, Mode::Check(operand_type.clone()))?.0);
    }
    if right_is_literal {
        right = Some(elaborate(context, &infix.right, Mode::Check(operand_type.clone()))?.0);
    }

    let left = left.unwrap();
    let right = right.unwrap();

    let (concept_name, field_name) = infix.op.concept_field();

    // The concept registry entry — absent only in an exotic embedding that
    // elaborates without the embedded prelude, where the operator has nothing to
    // dispatch through.
    let Some(concept) = context.concept(concept_name).cloned() else {
        let head = Term::unwrap_or_clone(reduce_with(context, &operand_type)?);
        return Err(Error::operator_undefined(
            infix.op.symbol().to_string(),
            head,
        ));
    };

    // Projection is positional over the *instantiated* field telescope
    // (`Structure::fields_at` peels the leading parameter binders, exactly as
    // `elaborate_proj` resolves a label), so the method's position among the
    // concept's fields is the index — no parameter offset.
    let projection_index = concept
        .fields
        .iter()
        .position(|field| field == field_name)
        .expect("the syn operator concepts declare their table fields");

    // Mint and attempt the witness goal exactly like an omitted `use`
    // argument: it resolves, parks on a flex operand type, or defers to a
    // later witness registration, and a definite miss reports
    // `no witness of Add(Point)` — the single operator error vocabulary.
    let goal = Term::struct_type(concept_name, vec![operand_type.clone()]);
    let provenance = WitnessOrigin {
        func: infix.op.symbol().to_string(),
        binder: field_name.to_string(),
    };
    let (slot, witness) =
        context.fresh_witness_metavar(goal.clone(), term.span(), provenance.clone());
    attempt_witness_goal(context, slot, &goal, provenance, term)?;

    let call = Term::apply(Term::proj(witness, projection_index), [left, right]);
    // No `BlnNot` prim exists; `!=` is the xor-negated equality.
    let rebuilt = match infix.op {
        NumOp::Neq => Term::prim(Prim::BlnXor(call, Term::prim(Prim::Bln(true)))),
        _ => call,
    };

    let result_type = if infix.op.result_is_bln() {
        bln_type
    } else {
        operand_type
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &result_type, expected)?;
    }

    Ok((rebuilt, result_type))
}

/// Check a lambda against an expected function type. Walk the lambda's own
/// telescope (whose `Done` is the body) alongside the expected type's telescope
/// (whose `Done` is the output type) in lockstep. Each parameter's domain is
/// taken from the expected type; the lambda's own domain — a hole when the
/// annotation was omitted, or the annotation itself — is unified against it via
/// `expect`, which solves the hole (or checks the annotation). The rebuilt lambda
/// then *carries* the expected domain rather than the hole, so re-closing it (and
/// every enclosing binder) captures any free names the domain mentions — this is
/// what keeps nested lambda domains de-Bruijn-correct for `zonk`/`erase` (§9).
pub(super) fn elaborate_func_check(
    context: &mut Context,
    telescope: &Telescope<Term>,
    term: &Term,
    expected: Term,
) -> Result<(Term, Term), Error> {
    let ft = match Term::unwrap_or_clone(reduce_with(context, &expected)?) {
        Subterm::FuncType(ft) => ft,
        Subterm::Metavar(_) if !context.parking_suppressed() => {
            return park_checking(context, term, &expected);
        }
        _ => return Err(Error::not_a_function_type(expected.clone())),
    };

    if telescope.len() != ft.telescope.len() {
        return Err(Error::wrong_number_of_arguments(
            ft.telescope.len(),
            telescope.len(),
        ));
    }

    fn walk(
        context: &mut Context,
        term: &Term,
        body: Telescope<Term>,
        type_: Telescope<Term>,
        plicities: &[Plicity],
        domains: &mut Vec<(String, Term)>,
    ) -> Result<Term, Error> {
        match (body, type_) {
            (Telescope::Done(body), Telescope::Done(output)) => check(context, &body, *output),
            (Telescope::Cons(domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                // Unify the *rebuilt* annotation against the expected domain:
                // `expect` reduces both sides, and a lowered (under-applied)
                // domain would open a telescope at the wrong arity. An omitted
                // annotation is a hole either way — `check` births it and
                // `expect` then solves it to the expected domain, as before.
                let domain = check(context, &domain, Term::type_())?;
                expect(context, term, &domain, &type_)?;
                let name = context.fresh(body_rest.first_label());
                let x = Term::free_var(&name);
                // A binder the expected type marks `use` joins the witness
                // scope: resolution inside the body finds it there.
                match plicities.get(domains.len()) {
                    Some(Plicity::Witness) => context.assume_witness(&name, &type_),
                    _ => context.assume(&name, &type_),
                }
                domains.push((name, type_.clone()));
                walk(
                    context,
                    term,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    plicities,
                    domains,
                )
            }
            // Arities were checked equal above.
            _ => unreachable!("function/type telescope arity mismatch"),
        }
    }

    let mut domains = Vec::new();
    let body = context.with_frame(|context| {
        walk(
            context,
            term,
            telescope.clone(),
            ft.telescope,
            &ft.plicities,
            &mut domains,
        )
    })?;

    Ok((Term::func(domains, body), expected))
}

/// Synthesize a function type from a lambda's own domain annotations — the mirror
/// of `elaborate_func_type`. Walk the telescope, elaborating each domain against
/// `Type`, assuming the parameter, and inferring the body at `Done`. A domain
/// that stays an unconstrained hole (the bare `(x) => …` sugar, or `(x : _)`)
/// offers nothing to synthesize from, so inference fails — exactly as a bare
/// lambda in inference position did before annotations existed. The rebuilt lambda
/// and its type share the same closed domains, so both stay de-Bruijn-correct.
pub(super) fn elaborate_func_infer(
    context: &mut Context,
    telescope: &Telescope<Term>,
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        body: Telescope<Term>,
        domains: &mut Vec<(String, Term)>,
    ) -> Result<(Term, Term), Error> {
        match body {
            Telescope::Done(body) => elaborate(context, &body, Mode::Infer),
            Telescope::Cons(domain, body_rest) => {
                let domain = check(context, &domain, Term::type_())?;

                if matches!(&*reduce_with(context, &domain)?, Subterm::Metavar(_)) {
                    return Err(Error::CannotInfer);
                }

                let name = context.fresh(body_rest.first_label());
                let x = Term::free_var(&name);
                context.assume(&name, &domain);
                domains.push((name, domain));
                walk(context, body_rest.open(&[&x]), domains)
            }
        }
    }

    let mut domains = Vec::new();
    let (body, output) =
        context.with_frame(|context| walk(context, telescope.clone(), &mut domains))?;

    Ok((
        Term::func(domains.clone(), body),
        Term::func_type(domains, output),
    ))
}
