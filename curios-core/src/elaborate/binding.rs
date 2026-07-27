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
        let mut triples = Vec::<(Free, Term, Term)>::with_capacity(let_.bindings.len());

        for (index, binding) in let_.bindings.iter().enumerate() {
            let (type_, body) = {
                let refs = label_terms.iter().collect::<Vec<_>>();
                (
                    binding.type_().release(&refs),
                    binding.value().release(&refs),
                )
            };

            // A bare metavar annotation is the lowering of a typeless local
            // `let x = e` (equivalently `let x : _ = e`): infer the body's type
            // instead of checking the body against the hole. This is what lets a
            // lambda/tuple/atom body — which `check` against an unsolved hole
            // would reject — be bound without an annotation. Otherwise check the
            // body against the (possibly partial) annotation, as before.
            let (type_elaborated, body_elaborated) = match &*type_ {
                Subterm::Metavar(_) => {
                    let (body_elaborated, inferred) = elaborate(context, &body, Mode::Infer)?;
                    (inferred, body_elaborated)
                }
                // The body is checked against — and the binder assumed at — the
                // *rebuilt* annotation: insertion saturates applications during
                // elaboration, and a lowered (under-applied) type reaching the
                // reducer would open a telescope at the wrong arity.
                _ => {
                    let type_elaborated = crate::check_is_sort(context, &type_)?.0;
                    let body_elaborated = check(context, &body, type_elaborated.clone())?;
                    (type_elaborated, body_elaborated)
                }
            };
            let label = context.fresh(let_.tail.hint_iter().nth(index).flatten());

            // Define the binding with the *rebuilt* body so the tail's
            // type-level evaluation does not reduce through the lowered
            // (under-applied) original.
            context.define_assuming(&label, &type_elaborated, &body_elaborated, None);

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
            .fold(tail_elaborated, |tail, (binder, type_, body)| {
                Term::let_(&binder, type_, body, tail)
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
        let group = &rec.group;
        let labels = rec
            .tail
            .hint_iter()
            .map(|l| context.fresh(l))
            .collect::<Vec<_>>();

        let label_terms = labels.iter().map(Term::free_var).collect::<Vec<_>>();
        let label_refs = label_terms.iter().collect::<Vec<_>>();

        let items = group
            .iter()
            .map(|member| {
                (
                    member.type_.open(&label_refs),
                    member.body.open(&label_refs),
                )
            })
            .collect::<Vec<_>>();

        for (label, (type_, _)) in labels.iter().zip(items.iter()) {
            context.assume(label, type_);
        }

        let mut types_elaborated = Vec::with_capacity(items.len());
        for (type_, _) in &items {
            types_elaborated.push(crate::check_is_sort(context, type_)?.0);
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
                context.define(label, &slot, None);
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
            .zip(types_elaborated.iter().cloned())
            .zip(bodies_elaborated.iter().cloned())
            .map(|((label, type_), body)| (label, type_, body))
            .collect::<Vec<_>>();

        let group = match Term::unwrap_or_clone(Term::rec(
            triples.clone(),
            Term::tuple(Vec::<Term>::new()),
        )) {
            Subterm::Rec(rec) => rec.group,
            _ => unreachable!("rec constructs a recursive block"),
        };
        for (index, (label, type_)) in labels.iter().zip(&types_elaborated).enumerate() {
            context.reassume(label, type_);
            context.define(label, &Term::rec_member(group.clone(), index), None);
        }

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
    let Func {
        telescope,
        plicities,
    } = func;

    match mode {
        Mode::Check(expected) => {
            elaborate_func_check(context, telescope, plicities, term, expected)
        }
        Mode::Infer => elaborate_func_infer(context, telescope, plicities),
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
/// `BoolXor(Eql/eql(a, b), true)` — no `BoolNot` prim exists. The node never
/// survives elaboration; witness projections over the statically-known
/// primitive witnesses collapse back to bare `Prim` code in the backend
/// (`And(Bool)`/`Or(Bool)` collapse to `BoolAnd`/`BoolOr` exactly as `Eql(Bool)`
/// collapses to `BoolEql` — see the codegen parity tests).
pub(super) fn elaborate_infix(
    context: &mut Context,
    infix: &Infix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let bool_type: Term = Subterm::Prim(Prim::BoolType).into();

    // `?T`: the operand type shared by both sides.
    let classifier = context.fresh_classifier_type("infix operand classifier");
    let (operand_id, operand_type) = context.fresh_placeholder(classifier, term.span());

    // An arithmetic operator returns its operand type, so an expected result
    // type pins `?T` straight away; a comparison returns `Bool`, which says
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
    let (_, universes) = context.instantiate_universe_bound(&concept.universe_context, &())?;
    let goal = Term::struct_type_at(concept_name, universes, vec![operand_type.clone()]);
    let provenance = WitnessOrigin {
        func: infix.op.symbol().to_string(),
        binder: field_name.to_string(),
    };
    let (slot, witness) =
        context.fresh_witness_metavar(goal.clone(), term.span(), provenance.clone());
    attempt_witness_goal(context, slot, &goal, provenance, term)?;

    let call = Term::apply(Term::proj(witness, projection_index), [left, right]);
    // No `BoolNot` prim exists; `!=` is the xor-negated equality.
    let rebuilt = match infix.op {
        NumOp::Neq => Term::prim(Prim::BoolXor(call, Term::prim(Prim::Bool(true)))),
        _ => call,
    };

    let result_type = if infix.op.result_is_bln() {
        bool_type
    } else {
        operand_type
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &result_type, expected)?;
    }

    Ok((rebuilt, result_type))
}

/// Check a lambda against an expected function type, aligning the lambda's own
/// binders with the expected telescope *by plicity* and inserting every omitted
/// hidden (implicit/witness) expected binder — the lambda-side counterpart of
/// application-side hidden-argument insertion.
///
/// Two queues advance together: the lambda's written telescope (whose `Done` is
/// the body) with its written plicities, and the expected type's telescope
/// (whose `Done` is the output) with its canonical plicities. At each step:
///
/// 1. matching plicities consume both — the written domain (a hole when the
///    annotation was omitted, or the annotation itself) is unified against the
///    expected domain via `expect`;
/// 2. a mismatch at a hidden expected slot inserts that binder — a real fresh
///    bound variable checked at the expected domain — and keeps the written
///    binder for the following expected slot;
/// 3. a mismatch at an *explicit* expected slot is a plicity error: an explicit
///    slot is never skipped, and a marked binder can never claim one.
///
/// Once the written binders run out, every remaining hidden expected slot is
/// synthesized; a leftover explicit slot is a missing-parameter arity error, and
/// a leftover written binder is a too-many-parameters arity error. Alignment is
/// positional by plicity, not by binder label.
///
/// The rebuilt lambda carries the *complete canonical* telescope — inserted
/// binders included — and the expected type's full plicity vector, so it re-checks
/// against the same type consuming every binder directly and inserting nothing
/// (idempotence, required for caching, parked-work replay, zonk, and archive
/// restoration). Each rebuilt domain is the *expected* domain rather than the
/// written hole, so re-closing it captures any free names it mentions — keeping
/// nested lambda domains de-Bruijn-correct for `zonk`/`erase` (§9).
pub(super) fn elaborate_func_check(
    context: &mut Context,
    telescope: &Telescope<Term>,
    written_plicities: &[Plicity],
    term: &Term,
    expected: Term,
) -> Result<(Term, Term), Error> {
    let reduced_expected = reduce_with(context, &expected)?;
    let ft = match Term::unwrap_or_clone(reduced_expected) {
        Subterm::FuncType(ft) => ft,
        Subterm::Metavar(_) if !context.parking_suppressed() => {
            return park_checking(context, term, &expected);
        }
        _ => return Err(Error::not_a_function_type(expected.clone())),
    };

    // Assume an inserted or consumed binder into the ordinary scope, joining the
    // witness scope when the *expected* slot is a `use` binder so resolution in
    // later domains and the body finds it there.
    fn assume_slot(context: &mut Context, name: &Free, plicity: Plicity, type_: &Term) {
        match plicity {
            Plicity::Witness => context.assume_witness(name, type_),
            _ => context.assume(name, type_),
        }
    }

    let mut domains: Vec<(Plicity, Free, Term)> = Vec::new();
    let body = context.with_frame(|context| {
        let mut written = telescope.clone();
        let mut expected_tele = ft.telescope;
        let e_plicities = &ft.plicities;
        let (mut w_idx, mut e_idx) = (0usize, 0usize);

        loop {
            match (written, expected_tele) {
                (Telescope::Done(body), Telescope::Done(output)) => {
                    break check(context, &body, *output);
                }
                // Written binders are exhausted: synthesize every remaining
                // expected slot, which must be hidden — an explicit slot is
                // never inserted (a missing-parameter arity error instead).
                (Telescope::Done(body), Telescope::Cons(domain, rest)) => {
                    let plicity = e_plicities[e_idx];
                    if plicity == Plicity::Explicit {
                        break Err(Error::wrong_number_of_arguments(
                            e_plicities.len(),
                            telescope.len(),
                        ));
                    }
                    let name = context.fresh(None);
                    let x = Term::free_var(&name);
                    assume_slot(context, &name, plicity, &domain);
                    domains.push((plicity, name, domain));
                    written = Telescope::Done(body);
                    expected_tele = rest.open(&[&x]);
                    e_idx += 1;
                }
                // A written binder remains but the expected telescope ended:
                // too many parameters.
                (Telescope::Cons(..), Telescope::Done(_)) => {
                    break Err(Error::wrong_number_of_arguments(
                        e_plicities.len(),
                        telescope.len(),
                    ));
                }
                (Telescope::Cons(w_domain, w_rest), Telescope::Cons(e_domain, e_rest)) => {
                    let w_plicity = written_plicities[w_idx];
                    let e_plicity = e_plicities[e_idx];
                    if w_plicity == e_plicity {
                        // Consume both. Unify the *rebuilt* written annotation
                        // against the expected domain (`expect` reduces both
                        // sides; an omitted annotation is a hole `check` births
                        // and `expect` solves to the expected domain).
                        let w_domain = crate::check_is_sort(context, &w_domain)?.0;
                        expect(context, term, &w_domain, &e_domain)?;
                        let name = context.fresh(w_rest.first_hint());
                        let x = Term::free_var(&name);
                        assume_slot(context, &name, e_plicity, &e_domain);
                        domains.push((e_plicity, name, e_domain));
                        written = w_rest.open(&[&x]);
                        expected_tele = e_rest.open(&[&x]);
                        w_idx += 1;
                        e_idx += 1;
                    } else if e_plicity != Plicity::Explicit {
                        // Insert this hidden expected slot; the written binder
                        // waits for the following expected slot.
                        let name = context.fresh(None);
                        let x = Term::free_var(&name);
                        assume_slot(context, &name, e_plicity, &e_domain);
                        domains.push((e_plicity, name, e_domain));
                        written = Telescope::Cons(w_domain, w_rest);
                        expected_tele = e_rest.open(&[&x]);
                        e_idx += 1;
                    } else {
                        // A marked written binder reached an explicit slot.
                        break Err(Error::BinderPlicityMismatch {
                            position: w_idx + 1,
                            expected: e_plicity,
                            written: w_plicity,
                        });
                    }
                }
            }
        }
    })?;

    Ok((Term::func_marked(domains, body), expected))
}

/// Synthesize a function type from a lambda's own domain annotations — the mirror
/// of `elaborate_func_type`. Without an expected type no binders can be inserted,
/// so the lambda's written plicity sequence is already canonical: the walk keeps
/// each written mark, entering a `use` binder into the witness scope for later
/// domains and the body, and the synthesized `FuncType`/rebuilt `Func` both carry
/// exactly that sequence. A domain that stays an unconstrained hole (the bare
/// `(x) => …` sugar, or `(x : _)`) offers nothing to synthesize from, so inference
/// fails — exactly as a bare lambda in inference position did before annotations
/// existed. The rebuilt lambda and its type share the same closed domains, so both
/// stay de-Bruijn-correct.
pub(super) fn elaborate_func_infer(
    context: &mut Context,
    telescope: &Telescope<Term>,
    plicities: &[Plicity],
) -> Result<(Term, Term), Error> {
    fn walk(
        context: &mut Context,
        body: Telescope<Term>,
        plicities: &[Plicity],
        domains: &mut Vec<(Plicity, Free, Term)>,
    ) -> Result<(Term, Term), Error> {
        match body {
            Telescope::Done(body) => elaborate(context, &body, Mode::Infer),
            Telescope::Cons(domain, body_rest) => {
                let domain = crate::check_is_sort(context, &domain)?.0;

                if matches!(&*reduce_with(context, &domain)?, Subterm::Metavar(_)) {
                    return Err(Error::CannotInfer);
                }

                let plicity = plicities[domains.len()];
                let name = context.fresh(body_rest.first_hint());
                let x = Term::free_var(&name);
                match plicity {
                    Plicity::Witness => context.assume_witness(&name, &domain),
                    _ => context.assume(&name, &domain),
                }
                domains.push((plicity, name, domain));
                walk(context, body_rest.open(&[&x]), plicities, domains)
            }
        }
    }

    let mut domains = Vec::new();
    let (body, output) =
        context.with_frame(|context| walk(context, telescope.clone(), plicities, &mut domains))?;

    Ok((
        Term::func_marked(domains.clone(), body),
        Term::func_type_marked(domains, output),
    ))
}
