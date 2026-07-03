use {
    super::{
        Apply, Atom, Bound, Carrier, Cases, Context, Error, Field, Func, FuncType, InductiveType,
        Item, Let, Many, Match, Module, MotivePattern, MotiveSlot, Nat, Prim, PrimHead, Proj, Rec,
        Scope, Struct, StructType, Subterm, Telescope, Term, Three, Tuple, TupleType, Two, Var,
        Variant, erase_prim, expect_prim_head, infer, is_prop, module_of, reduce_with, refine_head,
    },
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
                .map(|_| Term::free_var(context.fresh(None)))
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
                let x = Term::free_var(context.fresh(rest.first_label()));
                telescope = rest.open(&[&x]);
            }
            Telescope::Done(_) => break Ok(mask),
        }
    }
}

fn erase_func(
    context: &mut Context,
    func: &Func,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
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
                let x = Term::free_var(&name);
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
            curios_ersd::Subterm::Erased.into()
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
                let type_ = infer(context, &Term::free_var(&name))?;
                let candidate = is_candidate(context, &type_)?;
                Ok(curios_ersd::Argument { name, candidate })
            })
            .collect::<Result<Vec<_>, Error>>()?;

        Ok::<_, Error>((erased_body, captures))
    })?;

    let params = param_names
        .into_iter()
        .zip(candidates)
        .map(|(name, candidate)| curios_ersd::Argument { name, candidate })
        .collect();

    Ok(curios_ersd::Subterm::Func(curios_ersd::Func {
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

fn erase_apply(context: &mut Context, apply: &Apply) -> Result<curios_ersd::Term, Error> {
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
            erased_params.push(erase_kept(context, arg, ty)?);
        }
        Ok(())
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(curios_ersd::Subterm::Apply(curios_ersd::Apply {
        head: erased_head,
        params: erased_params,
    })
    .into())
}

/// Erase a value held in a *kept* slot — a constructor field or function
/// argument the (opaque) signature mask retains for uniform arity. The mask
/// keeps the slot, but this *instantiation* can still make it a proof or a type,
/// which carries no runtime content: proof/type irrelevance then fills the slot
/// with the trivial `Erased` rather than materialising a witness no runtime code
/// reads. Without this a `Prop`-payload constructor — `Option(Utf8)` in
/// `/std/Str`'s `check` — builds its proof at runtime, and an inductive proof
/// like `Utf8/more` drags the tail `Bin` along: an O(n²) of per-step slices in
/// `of_bin`. The slot stays (arity is fixed by the opaque mask); only its
/// contents collapse.
fn erase_kept(context: &mut Context, value: &Term, ty: &Term) -> Result<curios_ersd::Term, Error> {
    match is_erasable(context, ty)? {
        true => Ok(curios_ersd::Subterm::Erased.into()),
        false => erase(context, value, ty),
    }
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
) -> Result<Vec<curios_ersd::Term>, Error> {
    // The *drop* decision uses the signature mask (opaque-opened), so a payload
    // field's erasability matches the constructor's fixed arity even when a
    // polymorphic field is instantiated at a prop here.
    let mask = erasure_mask(context, telescope.clone())?;
    let mut telescope = telescope;
    let mut erased = Vec::with_capacity(values.len());

    for (index, value) in values.iter().enumerate() {
        match telescope {
            Telescope::Cons(ty, rest) => {
                if !mask[index] {
                    erased.push(erase_kept(context, value, &ty)?);
                }
                telescope = rest.open(&[value]);
            }
            Telescope::Done(_) => unreachable!("erase: arity checked by elaborate"),
        }
    }

    Ok(erased)
}

fn erase_tuple(
    context: &mut Context,
    tuple: &Tuple,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
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
        _ => curios_ersd::Subterm::Tuple(curios_ersd::Tuple {
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
) -> Result<curios_ersd::Term, Error> {
    context.with_frame(|context| {
        refine_head(context, head, scrutinee)?;
        erase(context, body, &motive.open(&[scrutinee]))
    })
}

/// Alias a non-variable scrutinee before its match is erased: erase the head
/// exactly once, re-run the erasure over a fresh variable defined as the head
/// (`define_assuming`, so dependent typing inside the arms still unfolds it),
/// and wrap the result in the binding `let alias = head`. Without this, the
/// projections a match substitutes into its arms (`pred := head - 1`, an
/// indexed carrier's `get`/`slice`) re-erase the head term — re-executing an
/// effectful scrutinee once per use site. The inductive path enforces the same
/// rule with its `scrutinee` let.
fn erase_aliased_match<F>(
    context: &mut Context,
    head: &Term,
    head_type: &Term,
    erase_over: F,
) -> Result<curios_ersd::Term, Error>
where
    F: FnOnce(&mut Context, &Term) -> Result<curios_ersd::Term, Error>,
{
    let label = context.fresh(Some("scrutinee"));
    let erased_head = erase(context, head, head_type)?;
    let var = Term::free_var(&label);

    let tail = context.with_frame(|context| {
        context.define_assuming(&label, head_type, head);
        erase_over(context, &var)
    })?;

    Ok(curios_ersd::Subterm::Let(curios_ersd::Let {
        name: label,
        body: erased_head,
        tail,
    })
    .into())
}

fn erase_nat_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    zero_case: &Term,
    succ_case: &Scope<Two>,
) -> Result<curios_ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, PrimHead::Nat)?;

    if !matches!(&**head, Subterm::Var(_)) {
        return erase_aliased_match(context, head, &head_type, |context, var| {
            erase_nat_match(context, var, motive, zero_case, succ_case)
        });
    }

    let erased_zero_case = erase(
        context,
        zero_case,
        &motive.open(&[&Subterm::Prim(Prim::Nat(Nat::new(0usize))).into()]),
    )?;

    // When the successor arm ignores its induction hypothesis, the eliminator is
    // a *case-split*, not a fold — so emit a single peel (`n == 0 ? zero :
    // succ[pred := n-1]`) rather than an n-step induction loop. Without this, a
    // non-tail-recursive caller that re-recurses on the peeled tail (e.g.
    // `/std/Str/count_w`) re-runs the whole fold at every level: the loop fires a
    // fresh recursion each of its n iterations, all discarded but the last, so
    // O(n) work becomes O(2^n). The `Arr`/`Bin` eliminators desugar through here
    // (their `Nat` succ arm threads `ih` iff the cons arm did), so this covers
    // them too.
    if !succ_case.uses(1) {
        let one: Term = Subterm::Prim(Prim::Nat(Nat::new(1usize))).into();
        let pred: Term = Subterm::Prim(Prim::nat_sub(head.clone(), one)).into();
        // `ih` is dead, so the term opened into it never appears — any term serves.
        let dead_ih: Term = Subterm::Prim(Prim::Nat(Nat::new(0usize))).into();
        let peeled = succ_case.open(&[&pred, &dead_ih]);

        let erased_default = erase(context, &peeled, &motive.open(&[head]))?;
        let erased_head = erase(context, head, &head_type)?;

        return Ok(
            curios_ersd::Subterm::NatMatch(curios_ersd::NatMatch::Dispatch {
                head: erased_head,
                cases: BTreeMap::from([(0, erased_zero_case)]),
                default: erased_default,
            })
            .into(),
        );
    }

    let pred_label = context.fresh(succ_case.first_label());
    let ih_label = context.fresh(succ_case.second_label());

    let erased_succ_case = context.with_frame(|context| {
        context.assume(&pred_label, &Subterm::Prim(Prim::NatType).into());

        context.assume(&ih_label, &motive.open(&[&Term::free_var(&pred_label)]));

        erase(
            context,
            &succ_case.open(&[&Term::free_var(&pred_label), &Term::free_var(&ih_label)]),
            &motive.open(&[&Subterm::Prim(Prim::nat_add(
                Term::free_var(&pred_label),
                Subterm::Prim(Prim::Nat(Nat::new(1usize))),
            ))
            .into()]),
        )
    })?;

    let erased_head = erase(context, head, &head_type)?;

    Ok(
        curios_ersd::Subterm::NatMatch(curios_ersd::NatMatch::Induction {
            head: erased_head,
            zero_case: erased_zero_case,
            pred: pred_label,
            ih: ih_label,
            succ_case: erased_succ_case,
        })
        .into(),
    )
}

fn erase_switch(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<u32, Term>,
    default: &Term,
) -> Result<curios_ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, PrimHead::Nat)?;

    let erased_cases = cases
        .iter()
        .map(|(n, body)| {
            let scrutinee = Subterm::Prim(Prim::Nat(Nat::new(*n))).into();
            erase_refined_case(context, head, motive, &scrutinee, body).map(|e| (*n, e))
        })
        .collect::<Result<BTreeMap<_, _>, Error>>()?;

    let erased_default = erase(context, default, &motive.open(&[head]))?;
    let erased_head = erase(context, head, &head_type)?;

    Ok(
        curios_ersd::Subterm::NatMatch(curios_ersd::NatMatch::Dispatch {
            head: erased_head,
            cases: erased_cases,
            default: erased_default,
        })
        .into(),
    )
}

fn erase_match(context: &mut Context, m: &Match) -> Result<curios_ersd::Term, Error> {
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
        } => erase_indexed_match(
            context,
            head,
            motive,
            empty_case,
            cons_case,
            IndexedCarrier::Arr { elem },
        ),
        Cases::FreeMonoid {
            carrier:
                Carrier::Bin {
                    empty_case,
                    cons_case,
                },
        } => erase_indexed_match(
            context,
            head,
            motive,
            empty_case,
            cons_case,
            IndexedCarrier::Bin,
        ),
    }
}

/// A borrowed view of the length-indexed carriers (`Arr`/`Bin`) — the eliminator's
/// "cons binds a head" axis, which cross-cuts the has-element axis the structural
/// traversals carve `Carrier` on. Local to `erase` because this is the only place
/// that axis matters: it bundles the carrier-specific reads behind one parameter so
/// [`erase_indexed_match`] stays carrier-agnostic, and it holds only the two indexed
/// variants, so those reads need no unreachable arm.
#[derive(Clone, Copy)]
enum IndexedCarrier<'a> {
    Arr { elem: &'a Term },
    Bin,
}

impl IndexedCarrier<'_> {
    /// The length of `head` (`Arr`/`Bin` are length-indexed).
    fn len(self, head: &Term) -> Term {
        match self {
            IndexedCarrier::Arr { elem } => {
                Subterm::Prim(Prim::ArrLen(elem.clone(), head.clone())).into()
            }
            IndexedCarrier::Bin => Subterm::Prim(Prim::BinLen(head.clone())).into(),
        }
    }

    /// The element of `head` at `index`.
    fn get(self, head: &Term, index: Term) -> Term {
        match self {
            IndexedCarrier::Arr { elem } => {
                Subterm::Prim(Prim::ArrGet(elem.clone(), head.clone(), index)).into()
            }
            IndexedCarrier::Bin => Subterm::Prim(Prim::BinGet(head.clone(), index)).into(),
        }
    }

    /// The sub-slice `head[lo .. hi]`.
    fn slice(self, head: &Term, lo: Term, hi: Term) -> Term {
        match self {
            IndexedCarrier::Arr { elem } => {
                Subterm::Prim(Prim::ArrSlice(elem.clone(), head.clone(), lo, hi)).into()
            }
            IndexedCarrier::Bin => Subterm::Prim(Prim::BinSlice(head.clone(), lo, hi)).into(),
        }
    }
}

/// Desugar a length-indexed sequence induction (`Arr`/`Bin`) to `Nat` induction
/// on the carrier's length, reusing `erase_nat_match` and so the `Nat` loop emitter
/// wholesale (no new ersd/cont machinery). The structural fold is a `foldr`, so the
/// loop walks the buffer back-to-front: at `Nat`-step `pred = i` the original cons
/// arm fires with `head := xs[len-1-i]` and `tail := xs[len-i ..]`, and the induction
/// hypothesis is the accumulator. (See `reduce` for the matching type-level rule.)
/// The carrier supplies its length, element-at, and sub-slice operations.
fn erase_indexed_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    empty_case: &Term,
    cons_case: &Scope<Three>,
    carrier: IndexedCarrier<'_>,
) -> Result<curios_ersd::Term, Error> {
    if !matches!(&**head, Subterm::Var(_)) {
        let head_type = infer(context, head)?;
        let head_type = reduce_with(context, &head_type)?;

        return erase_aliased_match(context, head, &head_type, |context, var| {
            erase_indexed_match(context, var, motive, empty_case, cons_case, carrier)
        });
    }

    let len_term = carrier.len(head);
    let one: Term = Subterm::Prim(Prim::Nat(Nat::new(1usize))).into();

    // Nat motive `Q(i) = P(suffix of length i) = motive[ xs[len - i ..] ]`.
    let i_label = context.fresh(None);
    let suffix_i = carrier.slice(
        head,
        Subterm::Prim(Prim::nat_sub(len_term.clone(), Term::free_var(&i_label))).into(),
        len_term.clone(),
    );
    let nat_motive = Scope::close(Many(1), &[i_label.as_str()], motive.open(&[&suffix_i]));

    // Successor arm: recover the element and tail at index `len - 1 - pred`.
    let pred_label = context.fresh(cons_case.first_label());
    let ih_label = context.fresh(cons_case.third_label());

    let index = Subterm::Prim(Prim::nat_sub(
        Subterm::Prim(Prim::nat_sub(len_term.clone(), one)),
        Term::free_var(&pred_label),
    ))
    .into();
    let head_value = carrier.get(head, index);
    let tail_value = carrier.slice(
        head,
        Subterm::Prim(Prim::nat_sub(len_term.clone(), Term::free_var(&pred_label))).into(),
        len_term.clone(),
    );

    let succ_body = cons_case.open(&[&head_value, &tail_value, &Term::free_var(&ih_label)]);
    let succ_case = Scope::close(Two, &[pred_label.as_str(), ih_label.as_str()], succ_body);

    erase_nat_match(context, &len_term, &nat_motive, empty_case, &succ_case)
}

fn erase_bln_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    false_case: &Term,
    true_case: &Term,
) -> Result<curios_ersd::Term, Error> {
    let head_type = expect_prim_head(context, head, PrimHead::Bln)?;

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

    Ok(
        curios_ersd::Subterm::NatMatch(curios_ersd::NatMatch::Dispatch {
            head: erased_head,
            cases: BTreeMap::from([(0, erased_false)]),
            default: erased_true,
        })
        .into(),
    )
}

fn erase_proj(context: &mut Context, proj: &Proj) -> Result<curios_ersd::Term, Error> {
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

    Ok(curios_ersd::Subterm::Proj(curios_ersd::Proj {
        head: erase(context, head, &head_type)?,
        index: relevant_before,
    })
    .into())
}

/// Lower a primitive constructor value to its flat sum-of-products runtime
/// representation: a single allocation `(tag_index, payload...)` with the
/// payload inlined after the tag. The tag's runtime
/// index is the constructor's position in sorted (registry key) order.
fn erase_variant(context: &mut Context, uc: &Variant) -> Result<curios_ersd::Term, Error> {
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
    fields.push(curios_ersd::Subterm::Atom(curios_ersd::Atom { index }).into());
    fields.extend(erase_telescoped(context, telescope, payload)?);

    Ok(curios_ersd::Subterm::Tuple(curios_ersd::Tuple { fields }).into())
}

/// Lower a struct value to its zero-cost runtime representation: a multi-field
/// struct is a *tagless* tuple (one fewer field than the equivalent
/// single-constructor inductive); a single-field struct (a newtype) is its bare
/// field — no tuple, no tag, so it is byte-identical to the field's own type.
fn erase_struct(context: &mut Context, s: &Struct) -> Result<curios_ersd::Term, Error> {
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
        _ => curios_ersd::Subterm::Tuple(curios_ersd::Tuple { fields: erased }).into(),
    })
}

/// The motive pattern's binder slots, positionally (validated by elaborate):
/// `true` marks a parameter position (opened with the actual parameter),
/// `false` an index position (opened with the case's target index). `Term`
/// slots carry no binder and are dropped.
fn pattern_binder_slots(pattern: Option<&MotivePattern>, n_params: usize) -> Vec<(bool, usize)> {
    pattern
        .map(|p| {
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
        .unwrap_or_default()
}

/// Erase a match on an *erasable* (proof/type) scrutinee, which carries no
/// runtime tag. Such an inductive is a subsingleton (large-elimination
/// soundness), so the match has exactly one live arm — `Eq`'s `refl` being the
/// canonical case. The arm's body is erased with its payload binders bound (so
/// it type-checks and refines) but never projected, and the scrutinee head is
/// not erased: it is a dropped binder with no runtime value.
/// The scrutinee's inductive type at a match site: its registered declaration
/// plus the concrete instantiation (`name`, parameters, indices) read off the
/// scrutinee's type.
struct Scrutinee<'a> {
    inductive: &'a super::Inductive,
    name: &'a str,
    params: &'a [Term],
    actual_indices: &'a [Term],
}

fn erase_erasable_scrutinee_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    scrutinee: Scrutinee<'_>,
    pattern: Option<&MotivePattern>,
) -> Result<curios_ersd::Term, Error> {
    let Scrutinee {
        inductive,
        name,
        params,
        actual_indices,
    } = scrutinee;
    let binder_slots = pattern_binder_slots(pattern, inductive.params.len());

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
    let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

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
        refine_head(context, head, &ctor_val)?;
        for (actual, target) in actual_indices.iter().zip(&ix_c) {
            refine_head(context, actual, target)?;
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
            return Ok(curios_ersd::Subterm::Erased.into());
        }
        Ok(body)
    })
}

/// Lower the primitive eliminator: an index dispatch on the scrutinee's tag
/// (field 0), each arm rebinding its payload binders to the flat record's
/// remaining fields (`head.(i + 1)`). Downstream stages
/// (`cont`/`optm`/`wasm`) see only generic tuples, projections, and an
/// index-dispatched match.
fn erase_inductive_match(
    context: &mut Context,
    head: &Term,
    motive: &Scope<Many>,
    cases: &BTreeMap<Atom, Scope<Many>>,
    pattern: Option<&MotivePattern>,
) -> Result<curios_ersd::Term, Error> {
    // A match with no arms is a vacuous elimination — of an empty inductive (`False`)
    // or of one whose every constructor inversion-clashes at the scrutinee's
    // indices. It is unreachable code that never inspects the scrutinee, which
    // elaborate placed in an erased position; erasing the head here would emit a
    // reference to an erased binder, so short-circuit to a trap.
    if cases.is_empty() {
        return Ok(curios_ersd::Subterm::Unreachable.into());
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
            Scrutinee {
                inductive: &inductive,
                name: &name,
                params: &params,
                actual_indices: &actual_indices,
            },
            pattern,
        );
    }

    let scrutinee_label = context.fresh(Some("scrutinee"));

    let binder_slots = pattern_binder_slots(pattern, inductive.params.len());

    let cases_erased = inductive
        .constructors
        .keys()
        .map(|tag| {
            // A tag with no arm was pruned by elaborate (Rung C verified the
            // case impossible at the scrutinee's indices). Its dispatch slot
            // still exists positionally, but reaching it is a compiler bug or
            // corrupted runtime tag, so lower it to a real trap.
            let Some(scope) = cases.get(tag) else {
                return Ok(curios_ersd::Subterm::Unreachable.into());
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

            let vars = labels.iter().map(Term::free_var).collect::<Vec<_>>();

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

                refine_head(context, head, &ctor_val)?;

                // Rung B, mirrored from elaborate: key-shaped scrutinee
                // indices reduce to the case's targets inside the arm, so
                // types erased here converge the same way they checked.
                for (actual, target) in actual_indices.iter().zip(&ix_c) {
                    refine_head(context, actual, target)?;
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
                        Ok(curios_ersd::Subterm::Let(curios_ersd::Let {
                            name: label.clone(),
                            body: curios_ersd::Subterm::Proj(curios_ersd::Proj {
                                head: curios_ersd::Subterm::Name(curios_ersd::Name::from(
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
    Ok(curios_ersd::Subterm::Let(curios_ersd::Let {
        name: scrutinee_label.clone(),
        body: erase(context, head, &head_type)?,
        tail: curios_ersd::Subterm::Match(curios_ersd::Match {
            head: curios_ersd::Subterm::Proj(curios_ersd::Proj {
                head: curios_ersd::Subterm::Name(curios_ersd::Name::from(scrutinee_label.as_str()))
                    .into(),
                index: 0,
            })
            .into(),
            cases: cases_erased,
        })
        .into(),
    })
    .into())
}

fn erase_let(
    context: &mut Context,
    let_: &Let,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
    let Let {
        type_: body_type,
        body,
        tail,
    } = let_;

    let name = context.fresh(tail.first_label());
    let erased_body = erase(context, body, body_type)?;
    let var_term = Term::free_var(&name);
    let tail = tail.open(&[&var_term]);

    let tail = context.with_frame(|context| {
        context.define_assuming(&name, body_type, body);

        erase(context, &tail, expected)
    })?;

    Ok(curios_ersd::Subterm::Let(curios_ersd::Let {
        name,
        body: erased_body,
        tail,
    })
    .into())
}

fn erase_rec(
    context: &mut Context,
    rec: &Rec,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
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

        Ok(curios_ersd::Rec {
            names,
            items: erased_items,
            tail: erase(context, &tail, expected)?,
        })
    })?;

    Ok(curios_ersd::Subterm::Rec(erased).into())
}

/// Erase a whole meta-free [`Module`] to an [`curios_ersd::Module`] (§9). Each top-level
/// item is erased and `define`d *cumulatively in the persistent base frame* (no
/// `with_frame`), so later items, the entrypoint body, and the type annotations
/// all reduce through the accumulated definitions; then the entrypoint `body` is
/// erased against `expected`. The flat analogue of `erase_let`/`erase_rec`, minus
/// the de Bruijn open/close — top-level cross-references are already free `Var`s,
/// which erase to `curios_ersd::Name`.
pub fn erase_module(
    context: &mut Context,
    module: &Module,
    expected: &Term,
) -> Result<curios_ersd::Module, Error> {
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
        // Mirror `elaborate_module`: set the use-site module (`island`) to the
        // item's qualifier prefix (a `rec` group shares one). `erase` re-derives
        // types via `infer` (= `elaborate` in Infer mode), which re-runs the
        // struct projection privacy check (§7); without the island an in-module
        // projection of a private-rep struct (e.g. `/std/Time`'s `Instant`)
        // would be wrongly rejected, the island defaulting to the root.
        let item_module = match item {
            Item::Let(def) => module_of(&def.name),
            Item::Rec(defs) => defs.first().map(|def| module_of(&def.name)).unwrap_or(""),
        };
        context.set_island(item_module.to_string());

        match item {
            Item::Let(def) => {
                let body = erase(context, &def.body, &def.type_)?;
                context.define_assuming(&def.name, &def.type_, &def.body);

                items.push(curios_ersd::Item::Let {
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

                items.push(curios_ersd::Item::Rec {
                    names,
                    items: erased,
                });
            }
        }
    }

    // The entrypoint body runs under the root module (mirrors `elaborate_module`).
    context.set_island(String::new());
    let body = erase(context, &module.body, expected)?;

    Ok(curios_ersd::Module { items, body })
}

pub fn erase(
    context: &mut Context,
    term: &Term,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
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

fn erase_subterm(
    context: &mut Context,
    term: &Term,
    expected: &Term,
) -> Result<curios_ersd::Term, Error> {
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
        | Subterm::StructType(_) => Ok(curios_ersd::Subterm::Erased.into()),
        Subterm::Variant(uc) => erase_variant(context, uc),
        Subterm::Struct(s) => erase_struct(context, s),
        Subterm::Func(func) => erase_func(context, func, expected),
        Subterm::Apply(apply) => erase_apply(context, apply),
        Subterm::Tuple(tuple) => erase_tuple(context, tuple, expected),
        Subterm::Proj(proj) => erase_proj(context, proj),
        Subterm::Let(let_) => erase_let(context, let_, expected),
        Subterm::Rec(lr) => erase_rec(context, lr, expected),
        Subterm::Var(var) => {
            Ok(curios_ersd::Subterm::Name(curios_ersd::Name::from(var.unwrap())).into())
        }
        // Erase runs downstream of zonking, on a meta-free term (§9).
        Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erase"),
        // `elaborate` resolves every infix/numeric-literal node to a `Prim`.
        Subterm::Infix(_) => unreachable!("infix node survived elaboration into erase"),
        Subterm::NumLit(_) => unreachable!("numeric-literal node survived elaboration into erase"),
    }
}
