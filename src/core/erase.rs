use {
    super::{
        Apply, Atom, Bound, Carrier, Cases, Context, Error, Field, Func, FuncType, Item, Let, Many,
        Match, Module, MotivePattern, MotiveSlot, Nat, Prim, Proj, Rec, Scope, Struct,
        StructType, Subterm, Telescope, Term, Three, Tuple, TupleType, Two, InductiveType, Var, Variant,
        erase_prim, expect_prim_head, infer, is_prop, reduce_with, refine_head,
    },
    crate::ersd,
    std::collections::BTreeMap,
};

/// Whether a value of type `type_` is dropped at runtime. Erasure is sort-driven:
/// a value erases iff it is a *type/prop-as-value* (`type_` reduces to the
/// universe `Type` or `Prop`), a *genuine proposition* — a `Prop`-sorted
/// nominal/neutral type (`Eq`, `False`, `Le`, `Utf8`, a stuck `Nat/Lt` match)
/// whose inhabitants are pure proof-irrelevant witnesses — or a *function into*
/// such a thing (a proof-/type-producing function is itself pure content-free).
///
/// [`super::Sort::of`] classifies the empty tuple `{}` as `Type`, not a prop —
/// `{}` is the result type of effects (`print`'s `let _ = write(..); ()`) and
/// must be kept — so `{}`, `{ .., {} }`, and `X -> {}` are not erased. A
/// `FuncType` erases only when its ultimate codomain does, recursing past the
/// parameters into the return type, which lands on `{}` (kept) or on a genuine
/// proposition / universe (erased).
/// Every std `@`-marker was either such a proposition/type or a function
/// returning one, so this stays output-equivalent.
///
/// CRITICAL: evaluate against the binder's *declared* (signature) type, opened
/// only with the surrounding binders as opaque variables — never with concrete
/// call arguments. A polymorphic field `value : A` is kept (its abstract `A` is
/// neither prop nor type); re-classifying it at a call where `A := SomeProp`
/// would diverge the construction's arity from the constructor function's fixed
/// arity. [`erasure_mask`] enforces the opaque-open discipline.
fn is_erasable(context: &mut Context, type_: &Term) -> Result<bool, Error> {
    match Term::unwrap_or_clone(reduce_with(context, type_)?) {
        Subterm::Type | Subterm::Prop => Ok(true),
        // A function erases iff what it ultimately returns does — a proof-/type-
        // producing function is pure, content-free; an effectful `X -> {}` is not.
        // Recurse past the parameters (opened opaquely) into the codomain.
        Subterm::FuncType(FuncType { telescope, .. }) => {
            let vars: Vec<Term> = (0..telescope.len())
                .map(|_| Term::var(Var::free(context.fresh(None))))
                .collect();
            let refs: Vec<&Term> = vars.iter().collect();
            is_erasable(context, &telescope.open(&refs))
        }
        _ => is_prop(context, type_),
    }
}

/// The per-binder erasability mask of a telescope, classifying each domain with
/// the *preceding* binders opened as fresh opaque variables — the signature-only
/// view that keeps a function's runtime arity fixed across every instantiation
/// (see [`is_erasable`]). The terminal body is ignored. Pairs with a concrete
/// walk over the actual values: the mask decides which to drop, the concrete
/// walk erases the kept ones against their (dependent, instantiated) types.
fn erasure_mask<B: Bound>(
    context: &mut Context,
    mut telescope: Telescope<B>,
) -> Result<Vec<bool>, Error> {
    let mut mask = Vec::new();
    loop {
        match telescope {
            Telescope::Cons(ty, rest) => {
                mask.push(is_erasable(context, &ty)?);
                let x = Term::var(Var::free(context.fresh(rest.first_label())));
                telescope = rest.open(&[&x]);
            }
            Telescope::Done(_) => break Ok(mask),
        }
    }
}

fn erase_func(context: &mut Context, func: &Func, expected: &Term) -> Result<ersd::Term, Error> {
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
        dropped: &mut Vec<String>,
    ) -> Result<(Term, Term), Error> {
        match (body, type_) {
            (Telescope::Done(body), Telescope::Done(output)) => Ok((*body, *output)),
            (Telescope::Cons(_domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                let name = context.fresh(body_rest.first_label());
                let x = Term::var(Var::free(&name));
                // An erasable parameter (a proof or a type) is dropped from the
                // runtime closure entirely; no runtime computation can depend on
                // it. It is still opened/assumed (de Bruijn, typing) and excluded
                // from captures below — neither a runtime param nor a capture.
                // The flag is read before the binder is assumed: a parameter's
                // type never depends on the parameter itself.
                let erasable = is_erasable(context, &type_)?;
                context.assume(&name, &type_);
                match erasable {
                    true => dropped.push(name),
                    false => {
                        candidates.push(is_candidate(context, &type_)?);
                        names.push(name);
                    }
                }
                walk(
                    context,
                    body_rest.open(&[&x]),
                    type_rest.open(&[&x]),
                    names,
                    candidates,
                    dropped,
                )
            }
            _ => unreachable!("erase: function/type telescope arity mismatch"),
        }
    }

    let mut param_names = Vec::new();
    let mut candidates = Vec::new();
    let mut dropped = Vec::new();

    let (erased_body, captures) = context.with_frame(|context| {
        let (body_opened, output_type) = walk(
            context,
            telescope.clone(),
            ft.telescope,
            &mut param_names,
            &mut candidates,
            &mut dropped,
        )?;

        // Erase the body, then collapse it to a unit only when it is both
        // proof/type-valued AND would dangle a dropped binder — the proof param of
        // an `(w : False) => w` thunk, or a type index this lambda dropped. The
        // collapse is *guarded on a live reference to a dropped binder* so an
        // effectful body that merely happens to be `False`-typed survives: a
        // process-exiting `(code) => Io/exit(code)` returns `False` through a real
        // trap, references no dropped binder, and must keep its effect. A pure
        // proof body that references nothing dropped is left as-is too (it is
        // already free of runtime content); only the dangling case is rewritten.
        let erased_body = erase(context, &body_opened, &output_type)?;
        let dangles = erased_body
            .free_names()
            .iter()
            .any(|name| dropped.contains(name));
        let erased_body = if dangles && is_erasable(context, &output_type)? {
            ersd::Subterm::Erased.into()
        } else {
            erased_body
        };

        // Captures are the *erased* body's free names — exactly what the runtime
        // closure references — other than the lambda's own parameters and its
        // dropped (erased) ones. Reading the erased body (not the pre-erasure one)
        // is what keeps a variable that survives only inside an erased position —
        // an erased constructor field or a type-level index — from being threaded
        // as a capture with no runtime value (which would leave `to_cont`
        // demanding an erased value). The candidate flag rides from here — the last
        // point a binder's type is known — down to `cont`, where the optimizer
        // specializes function-typed args.
        let captures = erased_body
            .free_names()
            .into_iter()
            .filter(|name| !param_names.contains(name) && !dropped.contains(name))
            .map(|name| {
                let type_ = infer(context, &Term::var(Var::free(&name)))?;
                let candidate = is_candidate(context, &type_)?;
                Ok(ersd::Argument { name, candidate })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        Ok::<_, Error>((erased_body, captures))
    })?;

    let params = param_names
        .into_iter()
        .zip(candidates)
        .map(|(name, candidate)| ersd::Argument { name, candidate })
        .collect();

    Ok(ersd::Subterm::Func(ersd::Func {
        captures,
        params,
        body: erased_body,
    })
    .into())
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

fn erase_apply(context: &mut Context, apply: &Apply) -> Result<ersd::Term, Error> {
    let Apply { head, params, .. } = apply;

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

    // Drop arguments to erasable parameters (a proof or a type): they are not
    // part of the runtime closure, so the argument is never evaluated. The mask
    // is the *signature* view (opaque-opened), so it agrees with the constructor
    // function's fixed arity even when a polymorphic domain is instantiated at a
    // prop here. `walk` still opens with the (un-erased) argument for dependent
    // later domains and to erase the kept ones.
    let mask = erasure_mask(context, ft.telescope.clone())?;
    ft.telescope.clone().walk(params, |i, arg, ty| {
        if !mask[i] {
            erased_params.push(erase(context, arg, ty)?);
        }
        Ok(())
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::Apply(ersd::Apply {
        head: erased_head,
        params: erased_params,
    })
    .into())
}

/// Erase each value against its telescope domain, opening the telescope with
/// the value as we go so later domains see the earlier values (the dependency).
/// The arity is checked by elaborate (§9), so a `Done` reached before the values
/// are exhausted is an internal invariant violation.
///
/// Erasable fields (a proof or a type) are dropped — not erased at all, so their
/// value (which may reference erased binders) never reaches the runtime. The
/// telescope is still opened with the un-erased value, so later dependent
/// (type-valued) domains stay correct.
fn erase_telescoped<B: Bound>(
    context: &mut Context,
    telescope: Telescope<B>,
    values: &[Term],
) -> Result<Vec<ersd::Term>, Error> {
    // The drop decision uses the signature mask (opaque-opened), so a payload
    // field's erasability matches the constructor's fixed arity even when a
    // polymorphic field is instantiated at a prop here.
    let mask = erasure_mask(context, telescope.clone())?;
    let mut telescope = telescope;
    let mut erased = Vec::with_capacity(values.len());

    for (index, value) in values.iter().enumerate() {
        match telescope {
            Telescope::Cons(ty, rest) => {
                if !mask[index] {
                    erased.push(erase(context, value, &ty)?);
                }
                telescope = rest.open(&[value]);
            }
            Telescope::Done(_) => unreachable!("erase: arity checked by elaborate"),
        }
    }

    Ok(erased)
}

fn erase_tuple(context: &mut Context, tuple: &Tuple, expected: &Term) -> Result<ersd::Term, Error> {
    let Tuple { fields, .. } = tuple;

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

    let erased_fields = erase_telescoped(context, type_telescope, fields)?;
    let dropped_any = erased_fields.len() != fields.len();

    // A subset type whose erased witnesses were dropped can collapse to its lone
    // relevant field — the same newtype collapse `erase_struct` performs. Guarded
    // on a drop having happened, so an ordinary 1-field tuple keeps its rep.
    Ok(match erased_fields.len() {
        1 if dropped_any => erased_fields.into_iter().next().expect("one field"),
        _ => ersd::Subterm::Tuple(ersd::Tuple {
            fields: erased_fields,
        })
        .into(),
    })
}

/// Erase a case body in a frame where `head` is refined to `scrutinee` (the
/// value this arm matched) and the motive is opened at that same value — the two
/// must agree, so both are derived from one `scrutinee`.
fn erase_refined_case(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    scrutinee: &Term,
    body: &Term,
) -> Result<ersd::Term, Error> {
    context.with_frame(|context| {
        refine_head(context, head, scrutinee);
        erase(context, body, &motive.open(&[scrutinee]))
    })
}

fn erase_nat_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, Prim::NatType)?;

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

    Ok(ersd::Subterm::NatMatch(ersd::NatMatch::Induction {
        head: erased_head,
        zero_case: erased_zero_case,
        pred: pred_label,
        ih: ih_label,
        succ_case: erased_succ_case,
    })
    .into())
}

fn erase_switch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, Prim::NatType)?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let scrutinee = Subterm::Prim(Prim::Nat(Nat::new(*n))).into();
            erase_refined_case(context, head, motive, &scrutinee, body).map(|e| (*n, e))
        })
        .collect::<Result<BTreeMap<_, _>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head]))?;
    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head,
        cases: erased_cases,
        default: erased_default,
    })
    .into())
}

fn erase_match(context: &mut Context, m: &Match) -> Result<ersd::Term, Error> {
    let Match {
        head,
        motive,
        cases,
    } = m;

    match cases {
        Cases::Bln {
            false_case,
            true_case,
        } => erase_bln_match(context, head, motive, false_case, true_case),
        Cases::Switch { cases, default } => erase_switch(context, head, motive, cases, default),
        Cases::Inductive { cases, pattern } => {
            erase_inductive_match(context, head, motive, cases, pattern.as_ref())
        }
        Cases::FreeMonoid {
            carrier:
                Carrier::Nat {
                    empty_case,
                    cons_case,
                },
        } => erase_nat_match(context, head, motive, empty_case, cons_case),
        Cases::FreeMonoid {
            carrier:
                Carrier::Arr {
                    elem,
                    empty_case,
                    cons_case,
                },
        } => erase_arr_match(context, head, motive, elem, empty_case, cons_case),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bin {
                    empty_case,
                    cons_case,
                },
        } => erase_bin_match(context, head, motive, empty_case, cons_case),
    }
}

/// Erase an `Arr` induction by desugaring it to `Nat` induction on the array's
/// length, reusing `erase_nat_match` and so the `Nat` loop emitter wholesale (no
/// new ersd/cont machinery). The structural fold is a `foldr`, so the loop walks
/// the buffer back-to-front: at `Nat`-step `pred = i` the original cons arm fires
/// with `head := v[len-1-i]` and `tail := v[len-i ..]`, and the induction
/// hypothesis is the accumulator. (See `reduce` for the matching type-level rule.)
fn erase_arr_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    elem: &Term,
    empty_case: &Term,
    cons_case: &Scope<Three>,
) -> Result<ersd::Term, Error> {
    let len: Term = Subterm::Prim(Prim::ArrLen(elem.clone(), head.clone())).into();
    let one: Term = Subterm::Prim(Prim::Nat(Nat::new(1usize))).into();

    // Nat motive `Q(i) = P(suffix of length i) = motive[ v[len - i ..] ]`.
    let i_label = context.fresh(None);
    let suffix_i: Term = Subterm::Prim(Prim::ArrSlice(
        elem.clone(),
        head.clone(),
        Subterm::Prim(Prim::nat_sub(len.clone(), Term::var(Var::free(&i_label)))).into(),
        len.clone(),
    ))
    .into();
    let nat_motive = Scope::close(Many(1), &[i_label.as_str()], motive.open(&[&suffix_i]));

    // Successor arm: recover the element and tail at index `len - 1 - pred`.
    let pred_label = context.fresh(cons_case.first_label());
    let ih_label = context.fresh(cons_case.third_label());

    let index: Term = Subterm::Prim(Prim::nat_sub(
        Subterm::Prim(Prim::nat_sub(len.clone(), one)),
        Term::var(Var::free(&pred_label)),
    ))
    .into();
    let head_value: Term =
        Subterm::Prim(Prim::ArrGet(elem.clone(), head.clone(), index)).into();
    let tail_value: Term = Subterm::Prim(Prim::ArrSlice(
        elem.clone(),
        head.clone(),
        Subterm::Prim(Prim::nat_sub(len.clone(), Term::var(Var::free(&pred_label)))).into(),
        len.clone(),
    ))
    .into();

    let succ_body = cons_case.open(&[
        &head_value,
        &tail_value,
        &Term::var(Var::free(&ih_label)),
    ]);
    let succ_case = Scope::close(Two, &[pred_label.as_str(), ih_label.as_str()], succ_body);

    erase_nat_match(context, &len, &nat_motive, empty_case, &succ_case)
}

/// Erase a `Bin` induction exactly as `erase_arr_match` does for `Arr` — `Nat`
/// induction on the bytestring's length, reusing the `Nat` loop emitter wholesale.
/// `Bin` carries no element type, so this is the `erase_arr_match` desugar with the
/// element parameter dropped (`BinLen`/`BinGet`/`BinSlice` for `ArrLen`/`ArrGet`/
/// `ArrSlice`); the recovered byte is a `Nat`, the `Bin`'s generator.
fn erase_bin_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
) -> Result<ersd::Term, Error> {
    let len: Term = Subterm::Prim(Prim::BinLen(head.clone())).into();
    let one: Term = Subterm::Prim(Prim::Nat(Nat::new(1usize))).into();

    // Nat motive `Q(i) = P(suffix of length i) = motive[ b[len - i ..] ]`.
    let i_label = context.fresh(None);
    let suffix_i: Term = Subterm::Prim(Prim::BinSlice(
        head.clone(),
        Subterm::Prim(Prim::nat_sub(len.clone(), Term::var(Var::free(&i_label)))).into(),
        len.clone(),
    ))
    .into();
    let nat_motive = Scope::close(Many(1), &[i_label.as_str()], motive.open(&[&suffix_i]));

    // Successor arm: recover the byte and tail at index `len - 1 - pred`.
    let pred_label = context.fresh(cons_case.first_label());
    let ih_label = context.fresh(cons_case.third_label());

    let index: Term = Subterm::Prim(Prim::nat_sub(
        Subterm::Prim(Prim::nat_sub(len.clone(), one)),
        Term::var(Var::free(&pred_label)),
    ))
    .into();
    let head_value: Term = Subterm::Prim(Prim::BinGet(head.clone(), index)).into();
    let tail_value: Term = Subterm::Prim(Prim::BinSlice(
        head.clone(),
        Subterm::Prim(Prim::nat_sub(len.clone(), Term::var(Var::free(&pred_label)))).into(),
        len.clone(),
    ))
    .into();

    let succ_body = cons_case.open(&[
        &head_value,
        &tail_value,
        &Term::var(Var::free(&ih_label)),
    ]);
    let succ_case = Scope::close(Two, &[pred_label.as_str(), ih_label.as_str()], succ_body);

    erase_nat_match(context, &len, &nat_motive, empty_case, &succ_case)
}

fn erase_bln_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
) -> Result<ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, Prim::BlnType)?;

    let erased_false = erase_refined_case(
        context,
        head,
        motive,
        &Subterm::Prim(Prim::Bln(false)).into(),
        false_case,
    )?;

    let erased_true = erase_refined_case(
        context,
        head,
        motive,
        &Subterm::Prim(Prim::Bln(true)).into(),
        true_case,
    )?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(ersd::Subterm::NatMatch(ersd::NatMatch::Dispatch {
        head: erased_head,
        cases: BTreeMap::from([(0, erased_false)]),
        default: erased_true,
    })
    .into())
}

fn erase_proj(context: &mut Context, proj: &Proj) -> Result<ersd::Term, Error> {
    let Proj { head, field } = proj;
    // Labels are resolved by elaborate; erase runs strictly downstream.
    let Field::Index(index) = field else {
        unreachable!("unresolved label projection reached erase");
    };

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    // Elaborate already checked the head is a tuple and the index is in range
    // (§9); the type is re-derived here only to lower the head. (The
    // projection-through-a-stuck-inductive-payload workaround that used to live
    // here — `projectable_at` — died with the tagged-tuple encoding: an inductive
    // payload is no longer reached by projecting a structural pair, so a
    // projection's head type is always a `TupleType` again.)
    // Erasable (proof/type) fields are dropped, so the runtime projection index
    // is the count of *relevant* fields before `index`, and a record left with a
    // single relevant field erased to that bare field (the projection then
    // vanishes — the value already *is* the field).
    let field_telescope = match &*head_type {
        Subterm::TupleType(TupleType { telescope }) => telescope.clone(),
        // A struct projects positionally with no tag offset (like a tuple, not a
        // variant).
        Subterm::StructType(StructType { name, params }) => {
            let structure = context
                .structure(name)
                .cloned()
                .expect("erase: projection head names a registered struct");
            structure.fields_at(params)
        }
        _ => unreachable!("erase: projected a non-tuple/struct"),
    };

    let field_count = field_telescope.len();
    assert!(*index < field_count, "erase: projection out of range");

    // The per-field erasability mask — the same signature view the construction
    // used, so a projection's relevant-slot arithmetic matches the laid-out
    // record exactly even under a prop-instantiated polymorphic field.
    let erasable = erasure_mask(context, field_telescope)?;

    let relevant_total = (0..field_count).filter(|&i| !erasable[i]).count();
    let relevant_before = (0..*index).filter(|&i| !erasable[i]).count();

    if relevant_total == 1 {
        return erase(context, head, &head_type);
    }

    Ok(ersd::Subterm::Proj(ersd::Proj {
        head: erase(context, head, &head_type)?,
        index: relevant_before,
    })
    .into())
}

/// Lower a primitive constructor value to its flat sum-of-products runtime
/// representation: a single allocation `(tag_index, payload...)` with the
/// payload inlined after the tag. The tag's runtime
/// index is the constructor's position in sorted (registry key) order.
fn erase_variant(context: &mut Context, uc: &Variant) -> Result<ersd::Term, Error> {
    let Variant {
        name,
        params,
        tag,
        payload,
    } = uc;

    let inductive = context
        .inductive(name)
        .cloned()
        .expect("erase: constructor names a registered inductive");

    let index = inductive
        .tag_index(tag)
        .expect("erase: constructor tag registered with its inductive");

    let telescope = inductive
        .instantiate(tag, params)
        .expect("erase: constructor instantiates at its inductive's parameters");

    // Erase the payload against the constructor telescope's (dependent) types,
    // inline after the tag. Erasable payload fields (a proof or a type) are
    // dropped from the tuple — sort-driven, read off each field's domain type.
    let mut fields = Vec::with_capacity(payload.len() + 1);
    fields.push(ersd::Subterm::Atom(ersd::Atom { index }).into());
    fields.extend(erase_telescoped(context, telescope, payload)?);

    Ok(ersd::Subterm::Tuple(ersd::Tuple { fields }).into())
}

/// Lower a struct value to its zero-cost runtime representation: a multi-field
/// struct is a *tagless* tuple (one fewer field than the equivalent
/// single-constructor inductive); a single-field struct (a newtype) is its bare
/// field — no tuple, no tag, so it is byte-identical to the field's own type.
fn erase_struct(context: &mut Context, s: &Struct) -> Result<ersd::Term, Error> {
    let Struct {
        name,
        params,
        fields,
        ..
    } = s;

    let structure = context
        .structure(name)
        .cloned()
        .expect("erase: struct names a registered struct");

    // Erase the fields against the instantiated (dependent) field telescope,
    // dropping the erasable (proof/type) ones. The single-field collapse below
    // then makes a proof-carrying record with one relevant field a bare value.
    let erased = erase_telescoped(context, structure.fields_at(params), fields)?;

    Ok(match erased.len() {
        1 => erased.into_iter().next().expect("one field"),
        _ => ersd::Subterm::Tuple(ersd::Tuple { fields: erased }).into(),
    })
}

/// Lower the primitive eliminator: an index dispatch on the scrutinee's tag
/// (field 0), each arm rebinding its payload binders to the flat record's
/// remaining fields (`head.(i + 1)`). Downstream stages
/// (`cont`/`optm`/`wasm`) see only generic tuples, projections, and an
/// Erase a match on an *erasable* (proof/type) scrutinee, which carries no
/// runtime tag. Such an inductive is a subsingleton (large-elimination
/// soundness), so the match has exactly one live arm — `Eq`'s `refl` being the
/// canonical case. The arm's body is erased with its payload binders bound (so
/// it type-checks and refines) but never projected, and the scrutinee head is
/// not erased: it is a dropped binder with no runtime value.
#[allow(clippy::too_many_arguments)]
fn erase_erasable_scrutinee_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    inductive: &super::Inductive,
    name: &str,
    params: &[Term],
    actual_indices: &[Term],
    pattern: Option<&MotivePattern>,
) -> Result<ersd::Term, Error> {
    let binder_slots: Vec<(bool, usize)> = pattern
        .map(|p| {
            let n_params = inductive.params.len();
            p.slots
                .iter()
                .enumerate()
                .filter_map(|(position, slot)| match slot {
                    MotiveSlot::Binder if position < n_params => Some((true, position)),
                    MotiveSlot::Binder => Some((false, position - n_params)),
                    MotiveSlot::Term(_) => None,
                })
                .collect()
        })
        .unwrap_or_default();

    // The single live arm. Elaborate prunes impossible arms; an erasable
    // (subsingleton) scrutinee leaves exactly one, whose body is the result.
    let (tag, scope) = cases
        .iter()
        .next()
        .expect("erase: erasable scrutinee match has its one live arm");

    let telescope = inductive
        .instantiate(tag, params)
        .expect("erase: constructor instantiates at its inductive's parameters");

    let labels = scope
        .label_iter()
        .map(|l| context.fresh(l))
        .collect::<Vec<_>>();
    let vars = labels
        .iter()
        .map(|label| Term::var(Var::free(label)))
        .collect::<Vec<_>>();

    context.with_frame(|context| {
        let mut telescope = telescope;
        for (label, var) in labels.iter().zip(&vars) {
            match telescope {
                Telescope::Cons(ty, rest) => {
                    context.assume(label, &ty);
                    telescope = rest.open(&[var]);
                }
                Telescope::Done(_) => unreachable!("erase: constructor arity checked by elaborate"),
            }
        }

        let ix_c = match &telescope {
            Telescope::Done(terminal) => match &***terminal {
                Subterm::InductiveType(InductiveType { indices, .. }) => indices.clone(),
                _ => unreachable!("erase: constructor terminal is its inductive type"),
            },
            Telescope::Cons(..) => unreachable!("erase: constructor arity checked by elaborate"),
        };

        let ctor_val = Term::variant(name.to_string(), params.to_vec(), tag.clone(), vars.clone());
        refine_head(context, head, &ctor_val);
        for (actual, target) in actual_indices.iter().zip(&ix_c) {
            refine_head(context, actual, target);
        }

        let arm_args = binder_slots
            .iter()
            .map(|&(is_param, i)| match is_param {
                true => params[i].clone(),
                false => ix_c[i].clone(),
            })
            .collect::<Vec<_>>();
        let arm_refs = arm_args.iter().chain([&ctor_val]).collect::<Vec<_>>();
        let expected = motive.open(&arm_refs);
        let var_refs = vars.iter().collect::<Vec<_>>();

        // The payload binders are all erased (the scrutinee is a prop), so the arm
        // body uses them only in erased positions — no projections are emitted.
        let body = erase(context, &scope.open(&var_refs), &expected)?;

        // If the erased arm body dangles a payload binder — an `Eq`-elimination
        // into another proof, e.g. `trans`'s `refl => p` whose `p` is the dropped
        // proof — and the result is itself proof/type-valued, collapse it to a
        // unit (it carries no runtime content). A *relevant* result (large
        // elimination into data, as in `subst`'s `refl => v`) keeps its body.
        let dangles = body.free_names().iter().any(|name| labels.contains(name));
        if dangles && is_erasable(context, &expected)? {
            return Ok(ersd::Subterm::Erased.into());
        }
        Ok(body)
    })
}

/// index-dispatched match.
fn erase_inductive_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    pattern: Option<&MotivePattern>,
) -> Result<ersd::Term, Error> {
    // A match with no arms is a vacuous elimination — of an empty inductive (`False`)
    // or of one whose every constructor inversion-clashes at the scrutinee's
    // indices. It is unreachable code that never inspects the scrutinee, which
    // elaborate placed in an erased position; erasing the head here would emit a
    // reference to an erased binder, so short-circuit to a trap.
    if cases.is_empty() {
        return Ok(ersd::Subterm::Unreachable.into());
    }

    let head_type = infer(context, head)?;
    let head_type = reduce_with(context, &head_type)?;

    let (name, params, actual_indices) = match &*head_type {
        Subterm::InductiveType(InductiveType {
            name,
            params,
            indices,
        }) => (name.clone(), params.clone(), indices.clone()),
        _ => unreachable!("erase: inductive match scrutinee checked by elaborate"),
    };

    let inductive = context
        .inductive(&name)
        .cloned()
        .expect("erase: scrutinee type names a registered inductive");

    // An erasable (proof/type) scrutinee carries no runtime tag to dispatch on.
    // For its elimination to be sound it must be a subsingleton — `Eq` (one
    // `refl` arm) is the canonical large-eliminating prop — so the match reduces
    // to its single arm. Lower that arm directly, *without* erasing the head
    // (which would reference the dropped scrutinee binder) and without payload
    // projections: every payload field is erased, used only in erased positions.
    if is_erasable(context, &head_type)? {
        return erase_erasable_scrutinee_match(
            context,
            head,
            motive,
            cases,
            &inductive,
            &name,
            &params,
            &actual_indices,
            pattern,
        );
    }

    let scrutinee_label = context.fresh(Some("scrutinee"));

    // The pattern's binder slots, positionally (validated by elaborate):
    // `true` marks a parameter position (opened with the actual parameter),
    // `false` an index position (opened with the case's target index).
    let binder_slots: Vec<(bool, usize)> = pattern
        .map(|p| {
            let n_params = inductive.params.len();

            p.slots
                .iter()
                .enumerate()
                .filter_map(|(position, slot)| match slot {
                    MotiveSlot::Binder if position < n_params => Some((true, position)),
                    MotiveSlot::Binder => Some((false, position - n_params)),
                    MotiveSlot::Term(_) => None,
                })
                .collect()
        })
        .unwrap_or_default();

    let cases_erased = inductive
        .constructors
        .keys()
        .map(|tag| {
            // A tag with no arm was pruned by elaborate (Rung C verified the
            // case impossible at the scrutinee's indices). Its dispatch slot
            // still exists positionally, but reaching it is a compiler bug or
            // corrupted runtime tag, so lower it to a real trap.
            let Some(scope) = cases.get(tag) else {
                return Ok(ersd::Subterm::Unreachable.into());
            };

            let telescope = inductive
                .instantiate(tag, &params)
                .expect("erase: constructor instantiates at its inductive's parameters");

            let hints = scope
                .label_iter()
                .map(|l| l.map(str::to_string))
                .collect::<Vec<_>>();

            let labels = hints
                .iter()
                .map(|hint| context.fresh(hint.as_deref()))
                .collect::<Vec<_>>();

            let vars = labels
                .iter()
                .map(|label| Term::var(Var::free(label)))
                .collect::<Vec<_>>();

            context.with_frame(|context| {
                let mut telescope = telescope;
                // One erasable flag per payload binder (a proof or a type),
                // computed before the binder is assumed — a binder's type never
                // depends on itself. Drives the flat-record slot assignment below.
                let mut erasable = Vec::with_capacity(labels.len());
                for (label, var) in labels.iter().zip(&vars) {
                    match telescope {
                        Telescope::Cons(ty, rest) => {
                            erasable.push(is_erasable(context, &ty)?);
                            context.assume(label, &ty);
                            telescope = rest.open(&[var]);
                        }
                        Telescope::Done(_) => {
                            unreachable!("erase: constructor arity checked by elaborate")
                        }
                    }
                }

                // This case's target indices, for opening a pattern motive.
                let ix_c = match &telescope {
                    Telescope::Done(terminal) => match &***terminal {
                        Subterm::InductiveType(InductiveType { indices, .. }) => indices.clone(),
                        _ => unreachable!("erase: constructor terminal is its inductive type"),
                    },
                    Telescope::Cons(..) => {
                        unreachable!("erase: constructor arity checked by elaborate")
                    }
                };

                let ctor_val =
                    Term::variant(name.clone(), params.clone(), tag.clone(), vars.clone());

                refine_head(context, head, &ctor_val);

                // Rung B, mirrored from elaborate: key-shaped scrutinee
                // indices reduce to the case's targets inside the arm, so
                // types erased here converge the same way they checked.
                for (actual, target) in actual_indices.iter().zip(&ix_c) {
                    refine_head(context, actual, target);
                }

                let arm_args = binder_slots
                    .iter()
                    .map(|&(is_param, i)| match is_param {
                        true => params[i].clone(),
                        false => ix_c[i].clone(),
                    })
                    .collect::<Vec<_>>();

                let arm_refs = arm_args.iter().chain([&ctor_val]).collect::<Vec<_>>();
                let expected = motive.open(&arm_refs);
                let var_refs = vars.iter().collect::<Vec<_>>();
                let body = erase(context, &scope.open(&var_refs), &expected)?;

                // Bind each *relevant* payload binder to its flat-record slot:
                // `let x_i = scrutinee.(slot); …` (innermost-last, so fold in
                // reverse). Erasable payload fields are absent from the runtime
                // tuple, so a relevant binder's slot is `1 + (relevant payload
                // before it)` (field 0 is the tag); an erasable binder gets no
                // `let` at all — the arm body only uses it in erased (→ `Erased`)
                // positions, so nothing projects it. Projections read the
                // let-bound scrutinee — never a re-erased copy of the head term,
                // which would re-execute an effectful scrutinee once per arm.
                let mut relevant = 0usize;
                let runtime_slot = labels
                    .iter()
                    .enumerate()
                    .map(|(i, _)| {
                        if erasable[i] {
                            None
                        } else {
                            let slot = 1 + relevant;
                            relevant += 1;
                            Some(slot)
                        }
                    })
                    .collect::<Vec<_>>();

                labels
                    .iter()
                    .enumerate()
                    .rev()
                    .try_fold(body, |tail, (i, label)| {
                        let Some(slot) = runtime_slot[i] else {
                            return Ok(tail);
                        };
                        Ok(ersd::Subterm::Let(ersd::Let {
                            name: label.clone(),
                            body: ersd::Subterm::Proj(ersd::Proj {
                                head: ersd::Subterm::Name(ersd::Name::from(
                                    scrutinee_label.as_str(),
                                ))
                                .into(),
                                index: slot,
                            })
                            .into(),
                            tail,
                        })
                        .into())
                    })
            })
        })
        .collect::<Result<Vec<_>, Error>>()?;

    // The head term is erased (and thus evaluated) exactly once, shared by
    // the tag dispatch and every arm's payload projections.
    Ok(ersd::Subterm::Let(ersd::Let {
        name: scrutinee_label.clone(),
        body: erase(context, head, &head_type)?,
        tail: ersd::Subterm::Match(ersd::Match {
            head: ersd::Subterm::Proj(ersd::Proj {
                head: ersd::Subterm::Name(ersd::Name::from(scrutinee_label.as_str())).into(),
                index: 0,
            })
            .into(),
            cases: cases_erased,
        })
        .into(),
    })
    .into())
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

    Ok(ersd::Subterm::Let(ersd::Let {
        name,
        body: erased_body,
        tail,
    })
    .into())
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
            .map(|(type_, body)| erase(context, body, type_))
            .collect::<Result<Vec<_>, Error>>()?;

        Ok(ersd::Rec {
            names,
            items: erased_items,
            tail: erase(context, &tail, expected)?,
        })
    })?;

    Ok(ersd::Subterm::Rec(erased).into())
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
    // Erase runs with its own `Context` (see `run::compile`); seed its
    // inductive registry from the module before any item consults it.
    for (name, inductive) in &module.inductives {
        context.register_inductive(name, inductive.clone());
    }

    // Seed the struct registry too — `erase_struct`/`erase_proj` consult it to
    // lower fields and to elide a newtype projection.
    for (name, structure) in &module.structures {
        context.register_structure(name, structure.clone());
    }

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

                let names = defs.iter().map(|def| def.name.clone()).collect::<Vec<_>>();

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
        Subterm::Match(m) => erase_match(context, m),
        // Type formers all erase to a runtime unit; they carry nothing to lower
        // and were already checked by `elaborate`.
        Subterm::Type
        | Subterm::Prop
        | Subterm::FuncType(_)
        | Subterm::TupleType(_)
        | Subterm::InductiveType(_)
        | Subterm::StructType(_) => Ok(ersd::Subterm::Erased.into()),
        Subterm::Variant(uc) => erase_variant(context, uc),
        Subterm::Struct(s) => erase_struct(context, s),
        Subterm::Func(func) => erase_func(context, func, expected),
        Subterm::Apply(apply) => erase_apply(context, apply),
        Subterm::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Subterm::Proj(proj) => erase_proj(context, proj),
        Subterm::Let(let_) => erase_let(context, let_, expected),
        Subterm::Rec(lr) => erase_rec(context, lr, expected),
        Subterm::Var(var) => Ok(ersd::Subterm::Name(ersd::Name::from(var.unwrap())).into()),
        // Erase runs downstream of zonking, on a meta-free term (§9).
        Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erase"),
    }
}
