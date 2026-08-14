use crate::{HeadKey, TermBuilders};
use {super::*, curios_core::Global};

/// Elaborate a local `let` block. The bindings are a flat `Vec` in one node, so this loops over them — elaborating each binding's type/body, minting its binder, and defining it in a single frame — rather than recursing once per binding, which a long straight-line sequence of `let`s would overflow the stack with. The tail continues with one ordinary (recursive) `elaborate`, its depth bounded by how often `let` and `rec` alternate, not by chain length. Rebuilding folds through `Term::let_`, which merges the bindings back into a single flat `Let`. The whole block is one source term with one span, stamped by `elaborate`'s wrapper — no per-binding span bookkeeping.
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

            // A bare metavar annotation is the lowering of a typeless local `let x = e` (equivalently `let x : _ = e`): infer the body's type instead of checking the body against the hole. This is what lets a lambda/tuple/atom body — which `check` against an unsolved hole would reject — be bound without an annotation. Otherwise check the body against the (possibly partial) annotation, as before.
            let (type_elaborated, body_elaborated) = match &*type_ {
                Subterm::Metavar(_) => {
                    let (body_elaborated, inferred) = elaborate(context, &body, Mode::Infer)?;
                    (inferred, body_elaborated)
                }
                // The body is checked against — and the binder assumed at — the *rebuilt* annotation: insertion saturates applications during elaboration, and a lowered (under-applied) type reaching the reducer would open a telescope at the wrong arity.
                _ => {
                    let type_elaborated = crate::check_is_sort(context, &type_)?.0;
                    let body_elaborated = check(context, &body, type_elaborated.clone())?;
                    (type_elaborated, body_elaborated)
                }
            };
            let label = context.fresh(let_.tail.hint_iter().nth(index).flatten());

            // Define the binding with the *rebuilt* body so the tail's type-level evaluation does not reduce through the lowered (under-applied) original.
            context.define_assuming(&label, &type_elaborated, &body_elaborated, None);

            label_terms.push(Term::free_var(&label));
            triples.push((label, type_elaborated, body_elaborated));
        }

        // Propagate `mode` into the tail: a `Check(expected)` turnaround happens where the bindings are in scope; `expected` comes from the outer scope and does not mention them, so comparing inside the frame is sound.
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

/// Elaborate a local `rec` group and its tail. The group's mutually-recursive bindings are one node, so this handles them at once; the tail recurses through one ordinary `elaborate`, bounded by `let`/`rec` alternation.
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

        // Upgrade the assumptions to the *rebuilt* signatures before any body is checked: a lowered (under-applied) type reaching the reducer would open a telescope at the wrong arity.
        for (label, type_) in labels.iter().zip(&types_elaborated) {
            context.reassume(label, type_);
        }

        // Recursive names point at protected slots, never lowered bodies. A sibling that productively needs an earlier member sees its rebuilt solution; a dependency on a later member parks on the unsolved slot.
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
        // Before the members are defined and the tail elaborated: a local `rec Bad : Type = (Bad) -> False` overflows the stack at its first use, which is in that tail. Named by the hints the program wrote, not by the gensyms elaboration minted for them.
        let names = rec
            .tail
            .hint_iter()
            .map(|hint| hint.unwrap_or("_").to_string())
            .collect::<Vec<_>>();
        crate::check_rec_totality(context, &group, &names)?;
        for (index, (label, type_)) in labels.iter().zip(&types_elaborated).enumerate() {
            context.reassume(label, type_);
            context.define(label, &Term::rec_proj(group.clone(), index), None);
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

/// Park a whole *checking problem*: a checked-only introduction form met an expected type whose structure is still an unsolved metavariable — possibly pinned by a constraint parked moments ago. A fresh placeholder metavariable stands in the rebuilt tree; once the expected type's metas solve, the problem re-checks under its frozen frame and the placeholder is solved with the rebuilt term (the spine machinery splices it wherever the occurrence travelled).
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

/// Resolve a polymorphic numeric literal ([`NumLit`]) to a concrete scalar intrinsic. In `Check` mode the expected type pins the choice; an expected type that is still a bare unsolved metavar — and `Infer` mode — fall back to the literal's shape default (`Int` when a sign was written, else `Nat`), and the closing `expect` then solves that metavar to the chosen type. The literal resolves *eagerly*: deferring it would strand downstream elaboration that needs the type immediately (a projection off the literal's type, say). The operator (`elaborate_infix`) pins its operand type from the non-literal side first, so a literal there sees a concrete type and `1 + flt` still works. Decimal literals never reach here; they parse straight to `Flt`.
pub(super) fn elaborate_num_lit(
    context: &mut Context,
    num_lit: &NumLit,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let nat_type: Term = Subterm::Intrinsic(Intrinsic::NatType).into();
    let byte_type: Term = Subterm::Intrinsic(Intrinsic::ByteType).into();
    let int_type: Term = Subterm::Intrinsic(Intrinsic::IntType).into();
    let flt_type: Term = Subterm::Intrinsic(Intrinsic::FltType).into();

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
                // Nothing concrete to resolve against yet — commit to the shape default; the closing `expect` solves the metavar to it.
                Subterm::Metavar(Metavar { id, .. }) if context.metavar_solution(*id).is_none() => {
                    Term::unwrap_or_clone(default_type.clone())
                }
                _ => Term::unwrap_or_clone(reduced),
            }
        }
        Mode::Infer => Term::unwrap_or_clone(default_type.clone()),
    };

    let (intrinsic, type_) = match &target {
        Subterm::Intrinsic(Intrinsic::NatType) if !num_lit.negative => (
            Intrinsic::Nat(Nat::new(num_lit.magnitude.clone())),
            nat_type,
        ),
        Subterm::Intrinsic(Intrinsic::ByteType) if !num_lit.negative => {
            let Some(value) = num_lit.magnitude.to_u8() else {
                return Err(Error::ByteLiteralOutOfRange {
                    value: num_lit.magnitude.to_string(),
                });
            };
            (Intrinsic::Byte(value), byte_type)
        }
        Subterm::Intrinsic(Intrinsic::IntType) => {
            let magnitude = Integer::from(num_lit.magnitude.clone());
            let value = if num_lit.negative {
                -magnitude
            } else {
                magnitude
            };
            (Intrinsic::Int(value), int_type)
        }
        Subterm::Intrinsic(Intrinsic::FltType) => {
            // `to_f32` saturates an overflowing magnitude to infinity, a value no literal can spell — refused like the `Byte` range above, never minted.
            let magnitude = num_lit.magnitude.to_f32().unwrap_or(f32::INFINITY);
            if !magnitude.is_finite() {
                return Err(Error::FltLiteralOutOfRange {
                    value: num_lit.magnitude.to_string(),
                });
            }
            let value = if num_lit.negative {
                -magnitude
            } else {
                magnitude
            };
            (Intrinsic::Flt(Flt::from_f32(value)), flt_type)
        }
        // A concrete expected type that is non-numeric — or `Nat` for a negative literal — has no realization: report against the literal's own shape.
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

    Ok((Term::intrinsic(intrinsic), type_))
}

/// The shape default for an infix operator whose operand type nothing pinned: any signed/negative literal operand forces `Int`, otherwise `Nat`.
///
/// A free function rather than an inherent method on [`Infix`]: the node is representation and lives in `curios-core`, while literal defaulting is an elaboration decision.
pub(super) fn infix_default_type(infix: &Infix) -> Intrinsic {
    let signed = |operand: &Term| matches!(&**operand, Subterm::Transient(Transient::NumLit(num_lit)) if num_lit.signed);

    if signed(&infix.left) || signed(&infix.right) {
        Intrinsic::IntType
    } else {
        Intrinsic::NatType
    }
}

/// The concept method an infix operator dispatches through, resolved against the concept's own declaration.
///
/// What the operator *returns* is read here rather than restated beside [`InfixOp`]: the method's type at the operand type `?T` comes out of the concept's lowered field telescope, so `eql(A, A) -> Bool` and `add(A, A) -> A` are told apart by what `/syn` declares and not by a Rust-side list of which operators yield `Bool`.
struct InfixMethod {
    /// The witness metavariable slot, filled by [`attempt_witness_goal`] once the operand type is pinned.
    slot: MetaId,
    goal: Term,
    witness: Term,
    provenance: WitnessOrigin,
    /// The method's position among the concept's fields — the projection index.
    index: usize,
    /// The method's type at `?T`, as a telescope: the two operand domains, any binder the declaration carries past them, and the codomain that is the operator's result type.
    signature: Telescope<Term>,
    /// One mark per binder of `signature`, so an inserted argument is filled by the convention its slot declares.
    plicities: Vec<Plicity>,
}

impl InfixMethod {
    /// Whether the method returns the concept parameter itself (`add(A, A) -> A`) rather than a fixed type (`eql(A, A) -> Bool`).
    ///
    /// Read from the terminal payload without opening the telescope: `operand_type` is a metavariable term and therefore closed, so it carries no bound variable an opening would have to substitute for, and comparing it to the codomain *under* the binders is exact. A codomain that genuinely mentions an operand binder can never equal it, and is classified as not returning the operand — the conservative answer, since all it forgoes is an early pin.
    fn returns_operand(&self, operand_type: &Term) -> bool {
        self.signature.terminal() == operand_type
    }

    /// The operator's arguments and result type: the two written operands, then one inserted argument for every binder the method declares past them.
    ///
    /// A concept method is not required to be exactly binary. `Div` states the domain its carrier's division is defined on and takes a proof of it, so `a / b` has a third slot to fill — filled here the way an omitted argument is filled at any other application, which is what routes it through the same discharge. Opening the telescope at two values regardless is what this did before, and it panicked on the arity the moment a declaration carried a third binder.
    ///
    /// Each argument comes back beside the mark its *slot* declares, not beside the mark its origin suggests. The rebuilt application is re-elaborated — by erasure's re-derivation, by zonking, by archive restoration — and the arity check at that point counts written arguments against explicit slots, so an inserted proof passed off as explicit is an arity error at every later pass. This is [`elaborate_func_check`]'s idempotence requirement on the application side.
    fn arguments(
        &self,
        context: &mut Context,
        op: InfixOp,
        left: &Term,
        right: &Term,
        origin: &Term,
    ) -> Result<(Vec<(Plicity, Term)>, Term), Error> {
        let mut telescope = self.signature.clone();
        let mut marks = self.plicities.iter().copied();
        let mut arguments = Vec::new();

        for operand in [left, right] {
            let Telescope::Cons(_, rest) = telescope else {
                panic!("a syn operator concept declares its method over both operands");
            };
            arguments.push((marks.next().unwrap_or(Plicity::Explicit), operand.clone()));
            telescope = rest.open(&[operand]);
        }

        loop {
            match telescope {
                Telescope::Done(terminal) => return Ok((arguments, *terminal)),
                Telescope::Cons(domain, rest) => {
                    let plicity = marks.next().unwrap_or(Plicity::Explicit);
                    let filled =
                        insert_auto_argument(context, plicity, &domain, None, op.symbol(), origin)?;
                    telescope = rest.open(&[&filled]);
                    arguments.push((plicity, filled));
                }
            }
        }
    }
}

/// Resolve the concept method an infix operator dispatches through, and mint its witness goal.
///
/// `None` when the concept has no registry entry — an exotic embedding that elaborates without the embedded prelude, where the operator has nothing to dispatch through. The caller reports that once the operands are elaborated, so the diagnostic still names a reduced operand type.
///
/// Minting the witness metavar here, before the operands are elaborated, is what lets the method's declared type decide whether an expected result type may pin `?T`; the witness *attempt* still waits until the operands have pinned it.
fn infix_method(
    context: &mut Context,
    op: InfixOp,
    concept_name: &Global,
    field_name: &str,
    operand_type: &Term,
    term: &Term,
) -> Result<Option<InfixMethod>, Error> {
    let (Some(concept), Some(struct_decl)) = (
        context.concept(concept_name).cloned(),
        context.struct_decl(concept_name).cloned(),
    ) else {
        return Ok(None);
    };

    // Projection is positional over the *instantiated* field telescope (`Structure::fields_at` peels the leading parameter binders, exactly as `elaborate_proj` resolves a label), so the method's position among the concept's fields is the index — no parameter offset.
    let index = concept
        .fields
        .iter()
        .position(|field| field == field_name)
        .expect("the syn operator concepts declare their table fields");

    // Mint the witness goal exactly like an omitted `use` argument.
    let (_, universes) = context.instantiate_universe_bound(&concept.universe_context, &())?;
    let goal = Term::struct_type_at(
        concept_name.clone(),
        universes.clone(),
        vec![operand_type.clone()],
    );
    let provenance = WitnessOrigin {
        func: op.symbol().to_string(),
        binder: field_name.to_string(),
    };
    let (slot, witness) =
        context.fresh_witness_metavar(goal.clone(), term.span(), provenance.clone());

    // The method's type at `?T`, read out of the concept's lowered structure through the one operation that answers this — opened at the goal's own universes and parameter, so this and `elaborate_proj` agree by construction rather than by two derivations staying in step.
    let arity = context.instantiate_universe_bound_at(
        &struct_decl.universe_context,
        &struct_decl.arity,
        &universes,
    )?;
    let method_type = arity
        .open(&[operand_type])
        .field_type_from(&witness, index)
        .expect("a concept's own field index is in range");
    let Subterm::FuncType(FuncType {
        telescope,
        plicities,
    }) = &*method_type
    else {
        panic!("a syn operator concept declares its method as an arrow");
    };

    Ok(Some(InfixMethod {
        slot,
        goal,
        witness,
        provenance,
        index,
        signature: telescope.clone(),
        plicities: plicities.clone(),
    }))
}

/// Desugar a postfix `!` sequencing site ([`Bang`]) into its `/syn/Monad/bind` application and elaborate the result. The region's monad is read from the expected type and never inferred from the action — the discipline Lean's do-elaborator enforces with `tryPostponeIfNoneOrMVar` and `extractBind`: a bang whose region type is still an unsolved metavariable parks as a whole checking problem and re-runs when it lands, and one in an inference position is refused, because sequencing has no monad until the region names one. The rebuilt application takes the node's own span, so diagnostics keep anchoring at the written `!`.
pub(super) fn elaborate_bang(
    context: &mut Context,
    bang: &Bang,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Mode::Check(expected) = &mode else {
        return Err(Error::bang_region_undetermined().at_opt(term.span()));
    };

    // Strict postponement: a region whose monad is not yet rigid parks the whole bang and re-checks when the expected type's metavariables solve; the drain reports the region as undetermined if they never do.
    let region = reduce_with(context, expected)?;
    if region_flex(context, &region) {
        return park_checking(context, term, expected);
    }

    // Auto-lift, decided before anything elaborates by reading declared shapes: both heads keyable and different means the action is wrapped in `/syn/Lift`'s `lift`, whose `use` slot resolves the declared embedding — or reports the missing edge. An unreadable action stays unwrapped and keeps the ordinary mismatch; the explicit `lift(action)` spelling always remains.
    let action = match (
        HeadKey::of_whnf(&region),
        action_result_key(context, &bang.action),
    ) {
        (Some(region_key), Some(action_key)) if region_key != action_key => {
            let field = context.syntax().lift.lift;
            let wrapper =
                Term::free_var(&Free::global(field.concept.qualifier().with(field.field)));
            let wrapped = Term::apply(wrapper, vec![bang.action.clone()]);
            match bang.action.span().or_else(|| term.span()) {
                Some(span) => Term::spanned(span, wrapped),
                None => wrapped,
            }
        }
        _ => bang.action.clone(),
    };

    let head = Term::free_var(&Free::global(context.syntax().monad.bind.qualifier()));
    let app = Term::apply(head, vec![action, bang.continuation.clone()]);
    let app = match term.span() {
        Some(span) => Term::spanned(span, app),
        None => app,
    };
    elaborate(context, &app, mode)
}

/// Whether the region's weak-head form is still headed by an unsolved metavariable — including a stuck application of one, the higher-kinded case.
fn region_flex(context: &Context, whnf: &Term) -> bool {
    match &**whnf {
        Subterm::Metavar(metavar) => context.metavar_solution(metavar.id).is_none(),
        Subterm::Apply(apply) => region_flex(context, &apply.head),
        _ => false,
    }
}

/// The action's monad head, read without elaborating: peel the explicit application spine to a named head, read the head's *declared* type from the assumption store, and key the syntactic result behind its telescope. `None` on anything unreadable — an unnamed or computed head, a spine whose explicit-argument count differs from the declared telescope's, an alias-headed or computed result — so the oracle never wraps on a guess: reads only, no elaboration, no reduction, no instantiation, and a wrong abstention costs a message, never a solution.
fn action_result_key(context: &mut Context, action: &Term) -> Option<HeadKey> {
    let mut head = action;
    let mut explicit_args = 0usize;
    loop {
        match &**head {
            Subterm::Apply(apply) => {
                explicit_args += apply
                    .plicities
                    .iter()
                    .filter(|plicity| matches!(plicity, Plicity::Explicit))
                    .count();
                head = &apply.head;
            }
            Subterm::Var(var) => {
                let declared = context.assumption(var.as_free()?)?.clone();
                return declared_result_key(context, &declared, explicit_args);
            }
            _ => return None,
        }
    }
}

/// The [`HeadKey`] of `declared`'s result when a spine of `explicit_args` explicit arguments saturates it exactly; `None` otherwise. The telescope is opened with fresh frees on the way to the result, so a *dependent* result — `Io(Cell(T))`, `Io(Future(A))` — keys on its head like any other: the head is rigid whatever the binder, and a result actually *headed* by a binder (`M(Nat)` under `(M: (Type) -> Type, …)`) still abstains, because a free-variable head has no key. The result takes one weak-head reduction before keying, because a declared type keeps its nominal spelling (`Io({})` is stored as the `/sys/Io/Io` application, aliases as their own names); the reduction is the same read `resolve`'s `node_type` performs on assumption-derived types, and a reduction failure abstains. A wrong abstention still costs a message, never a solution.
fn declared_result_key(
    context: &mut Context,
    declared: &Term,
    explicit_args: usize,
) -> Option<HeadKey> {
    let result = match &**declared {
        Subterm::FuncType(func_type) => {
            let explicit_binders = func_type
                .plicities
                .iter()
                .filter(|plicity| matches!(plicity, Plicity::Explicit))
                .count();
            if explicit_binders != explicit_args {
                return None;
            }
            let mut telescope = func_type.telescope.clone();
            loop {
                match telescope {
                    Telescope::Cons(_, rest) => {
                        let binder = context.fresh(rest.first_hint());
                        telescope = rest.open(&[&Term::free_var(&binder)]);
                    }
                    Telescope::Done(result) => break (*result).clone(),
                }
            }
        }
        // A zero-argument action (`Async/yield_now`): the declared type is the carrier itself.
        _ => match explicit_args {
            0 => declared.clone(),
            _ => return None,
        },
    };
    let whnf = reduce_with(context, &result).ok()?;
    HeadKey::of_whnf(&whnf)
}

/// Elaborate an infix operator ([`Infix`]) as a concept method call. A fresh operand-type metavar `?T` is pinned by the non-literal operands first (or, for an operator whose method returns its operand type, by the expected result type), then defaulted from the operand literals if nothing constrains it; only then are the literal operands checked — against a `?T` that is already concrete, so they never force it to their own default. That ordering is what lets `1 + flt` resolve to `Flt` rather than a `Nat`/`Flt` mismatch.
///
/// Dispatch is then **one path**: every operator desugars to a projection of a witness of its `/syn` concept ([`OperatorSyntax::concept_field`](curios_utilities::OperatorSyntax::concept_field)) — `a + b` ≙ `Add/add(a, b)`, intrinsics included, resolved by the same engine that fills `use` slots (so `no witness of Add(Point)` is the single error vocabulary, and what an operator means at a type is entirely a question of which witnesses exist). There are no carved-out operators: `&&`/`||` project `And`/`Or`, and `!=` projects `Eql`'s `neq` rather than negating a rebuilt `eql`, so this function synthesizes no term of its own and reads every result type from the declaration. The node never survives elaboration; witness projections over the statically-known intrinsic witnesses collapse back to bare `Intrinsic` code in the backend (`And(Bool)`/`Or(Bool)` collapse to `BoolAnd`/`BoolOr` exactly as `Eql(Bool)` collapses to `BoolEql` — see the codegen parity tests).
pub(super) fn elaborate_infix(
    context: &mut Context,
    infix: &Infix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    // `?T`: the operand type shared by both sides.
    let classifier = context.fresh_classifier_type("infix operand classifier");
    let (operand_id, operand_type) = context.fresh_placeholder(classifier, term.span());

    let target = context.syntax().operator.concept_field(infix.op);
    let field_name = target.field;
    let concept_name = Global::Authored(target.concept.qualifier());
    let method = infix_method(
        context,
        infix.op,
        &concept_name,
        field_name,
        &operand_type,
        term,
    )?;

    // An operator whose method returns the concept parameter is pinned by an expected result type straight away; one that returns a fixed type says nothing about its operands, so only the operands can pin it. The concept declaration is the single source of that distinction.
    if method
        .as_ref()
        .is_some_and(|method| method.returns_operand(&operand_type))
        && let Mode::Check(expected) = &mode
    {
        expect(context, term, &operand_type, expected)?;
    }

    let left_is_literal = matches!(&*infix.left, Subterm::Transient(Transient::NumLit(_)));
    let right_is_literal = matches!(&*infix.right, Subterm::Transient(Transient::NumLit(_)));

    // Phase 1: the non-literal operands pin `?T` from their own types.
    let mut left = match left_is_literal {
        false => Some(elaborate(context, &infix.left, Mode::Check(operand_type.clone()))?.0),
        true => None,
    };
    let mut right = match right_is_literal {
        false => Some(elaborate(context, &infix.right, Mode::Check(operand_type.clone()))?.0),
        true => None,
    };

    // Nothing pinned `?T` — every non-literal operand left it open. Default from the operand shapes so the literal operands have a concrete type to take.
    if context.metavar_solution(operand_id).is_none() {
        let default = infix_default_type(infix);
        context.solve_metavar(operand_id, Subterm::Intrinsic(default).into());
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

    // The registry entry was absent — report it now that the operand type has structure to name.
    let Some(method) = method else {
        let head = Term::unwrap_or_clone(reduce_with(context, &operand_type)?);
        return Err(Error::operator_undefined(
            infix.op.symbol().to_string(),
            head,
        ));
    };

    // Attempt the witness goal exactly like an omitted `use` argument: it resolves, parks on a flex operand type, or defers to a later witness registration, and a definite miss reports `no witness of Add(Point)` — the single operator error vocabulary.
    attempt_witness_goal(
        context,
        method.slot,
        &method.goal,
        method.provenance.clone(),
        term,
    )?;

    let (arguments, result_type) = method.arguments(context, infix.op, &left, &right, term)?;
    let rebuilt = Term::apply_marked(Term::proj(method.witness.clone(), method.index), arguments);

    if let Mode::Check(expected) = &mode {
        expect(context, term, &result_type, expected)?;
    }

    Ok((rebuilt, result_type))
}

/// Check a lambda against an expected function type, aligning the lambda's own binders with the expected telescope *by plicity* and inserting every omitted hidden (implicit/witness) expected binder — the lambda-side counterpart of application-side hidden-argument insertion.
///
/// Two queues advance together: the lambda's written telescope (whose `Done` is the body) with its written plicities, and the expected type's telescope (whose `Done` is the output) with its canonical plicities. At each step:
///
/// 1. matching plicities consume both — the written domain (a hole when the annotation was omitted, or the annotation itself) is unified against the expected domain via `expect`; 2. a mismatch at a hidden expected slot inserts that binder — a real fresh bound variable checked at the expected domain — and keeps the written binder for the following expected slot; 3. a mismatch at an *explicit* expected slot is a plicity error: an explicit slot is never skipped, and a marked binder can never claim one.
///
/// Once the written binders run out, every remaining hidden expected slot is synthesized; a leftover explicit slot is a missing-parameter arity error, and a leftover written binder is a too-many-parameters arity error. Alignment is positional by plicity, not by binder label.
///
/// The rebuilt lambda carries the *complete canonical* telescope — inserted binders included — and the expected type's full plicity vector, so it re-checks against the same type consuming every binder directly and inserting nothing (idempotence, required for caching, parked-work replay, zonk, and archive restoration). Each rebuilt domain is the *expected* domain rather than the written hole, so re-closing it captures any free names it mentions — keeping nested lambda domains de-Bruijn-correct for `zonk`/`erase`.
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

    // Assume an inserted or consumed binder into the ordinary scope, joining the witness scope when the *expected* slot is a `use` binder so resolution in later domains and the body finds it there.
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
                // Written binders are exhausted: synthesize every remaining expected slot, which must be hidden — an explicit slot is never inserted (a missing-parameter arity error instead).
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
                // A written binder remains but the expected telescope ended: too many parameters.
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
                        // Consume both. Unify the *rebuilt* written annotation against the expected domain (`expect` reduces both sides; an omitted annotation is a hole `check` births and `expect` solves to the expected domain).
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
                        // Insert this hidden expected slot; the written binder waits for the following expected slot.
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

/// Synthesize a function type from a lambda's own domain annotations — the mirror of `elaborate_func_type`. Without an expected type no binders can be inserted, so the lambda's written plicity sequence is already canonical: the walk keeps each written mark, entering a `use` binder into the witness scope for later domains and the body, and the synthesized `FuncType`/rebuilt `Func` both carry exactly that sequence. A domain that stays an unconstrained hole (the bare `(x) => …` sugar, or `(x : _)`) offers nothing to synthesize from, so inference fails — exactly as a bare lambda in inference position did before annotations existed. The rebuilt lambda and its type share the same closed domains, so both stay de-Bruijn-correct.
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
