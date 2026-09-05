use {
    super::*,
    crate::{
        HeadKey, TermBuilders, WitnessKey, convert::convert, typing::display_mismatch,
        zonk_solved_term_metas,
    },
    curios_core::Global,
    curios_utilities::Span,
};

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

            // A silent hole as the annotation is the lowering of a typeless local `let x = e`: infer the body's type instead of checking the body against the hole. This is what lets a lambda/tuple/atom body — which `check` against an unsolved hole would reject — be bound without an annotation. A written goal `let x : ? = e` is a bare metavariable too but not a hole (`MetavarOrigin` states the rule): it takes the annotation path below, as every other annotation site does, so the goal term is elaborated, solved by the check and reported by zonk. Otherwise check the body against the (possibly partial) annotation, as before.
            let (type_elaborated, body_elaborated) = match &*type_ {
                Subterm::Metavar(metavar) if metavar.is_hole() => {
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
                let (id, slot) = context.fresh_rec_slot(label, type_.clone());
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

        // Materialize before the group closes, as `elaborate_module_rec` does for the same reason: `Term::rec` captures the member names, so anything a committed solution still spells as a slot must become that name *here*, while the capture below can still bind it. Substituting afterwards would insert a name whose binder the capture had already consumed and whose frame has popped. Tolerant on purpose — it fixes only what is committed and leaves the rest for the final zonk, so materializing early can lose nothing.
        let types_elaborated = types_elaborated
            .iter()
            .map(|type_| zonk_solved_term_metas(context, type_))
            .collect::<Vec<_>>();
        let bodies_elaborated = bodies_elaborated
            .iter()
            .map(|body| zonk_solved_term_metas(context, body))
            .collect::<Vec<_>>();

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
            _ => unreachable!("let constructs a recursive block"),
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
    let telescope = &func.telescope;
    let plicities = func.plicities();

    match mode {
        Mode::Check(expected) => {
            elaborate_func_check(context, telescope, plicities, term, expected)
        }
        Mode::Infer => elaborate_func_infer(context, telescope, plicities, None),
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
    let bool_type: Term = Subterm::Intrinsic(Intrinsic::BoolType).into();
    let byte_type: Term = Subterm::Intrinsic(Intrinsic::ByteType).into();
    let int_type: Term = Subterm::Intrinsic(Intrinsic::IntType).into();
    let flt_type: Term = Subterm::Intrinsic(Intrinsic::FltType).into();

    // A written sign rules out `Nat`, so a marked numeral defaults to `Int`; a character-spelled literal defaults to the certified `/syn/Char` value it has always denoted.
    let default_type: Term = match num_lit {
        NumLit::Number { sign, .. } if sign.is_marked() => int_type.clone(),
        NumLit::Number { .. } => nat_type.clone(),
        NumLit::Character(_) => Term::struct_type(
            Global::Authored(context.syntax().character.character.qualifier()),
            Vec::<Term>::new(),
        ),
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

    let (intrinsic, type_) = match num_lit {
        NumLit::Number { magnitude, sign } => match &target {
            Subterm::Intrinsic(Intrinsic::NatType) if !sign.is_negative() => {
                (Intrinsic::Nat(Nat::new(magnitude.clone())), nat_type)
            }
            Subterm::Intrinsic(Intrinsic::ByteType) if !sign.is_negative() => {
                let Some(value) = magnitude.to_u8() else {
                    return Err(Error::ByteLiteralOutOfRange {
                        value: magnitude.to_string(),
                    });
                };
                (Intrinsic::Byte(value), byte_type)
            }
            // A bit is a `Bool` in this language — `Bits` is the packed carrier of `Bool` — so `0` and `1` realize where a `Bool` is expected, which is what lets a packed literal's constant atoms stay ordinary numerals.
            Subterm::Intrinsic(Intrinsic::BoolType) if !sign.is_negative() => {
                let value = match magnitude.to_u8() {
                    Some(0) => false,
                    Some(1) => true,
                    _ => {
                        return Err(Error::BoolLiteralOutOfRange {
                            value: magnitude.to_string(),
                        });
                    }
                };
                (Intrinsic::Bool(value), bool_type)
            }
            Subterm::Intrinsic(Intrinsic::IntType) => {
                let magnitude = Integer::from(magnitude.clone());
                let value = if sign.is_negative() {
                    -magnitude
                } else {
                    magnitude
                };
                (Intrinsic::Int(value), int_type)
            }
            Subterm::Intrinsic(Intrinsic::FltType) => {
                // Rounded by the model, as the decimal literal in `curios-text` is, so what a numeral means as a binary32 is stated once in this repository. A magnitude past the largest finite value rounds to infinity, a value no literal can spell — refused like the `Byte` range above, never minted.
                let value = Floating::of_natural(magnitude);
                if !value.is_finite() {
                    return Err(Error::FltLiteralOutOfRange {
                        value: magnitude.to_string(),
                    });
                }
                let value = if sign.is_negative() { -value } else { value };
                (Intrinsic::Flt(value), flt_type)
            }
            // A concrete expected type that is non-numeric — or `Nat` for a negative literal — has no realization: report against the literal's own shape, through the rendering every mismatch gets. Built from the raw expected term, the report named the placeholder an operator's operand type arrives through rather than its solution: `"a" + 1` refused `1` against `?`.
            _ => {
                let Mode::Check(expected) = &mode else {
                    unreachable!("Infer-mode target is always the Nat/Int shape default");
                };
                let inferred = if sign.is_negative() {
                    int_type
                } else {
                    default_type
                };
                return Err(display_mismatch(context, term, &inferred, expected));
            }
        },
        NumLit::Character(character) => {
            let code = *character as u32;
            match &target {
                Subterm::Intrinsic(Intrinsic::NatType) => {
                    (Intrinsic::Nat(Nat::new(code)), nat_type)
                }
                Subterm::Intrinsic(Intrinsic::ByteType) => {
                    let Ok(value) = u8::try_from(code) else {
                        return Err(Error::ByteLiteralOutOfRange {
                            value: code.to_string(),
                        });
                    };
                    (Intrinsic::Byte(value), byte_type)
                }
                Subterm::Intrinsic(Intrinsic::IntType) => {
                    (Intrinsic::Int(Integer::from(code)), int_type)
                }
                // Everything else — the `Char` default, an expected `/syn/Char`, an unsolved metavariable, or a genuine mismatch — is answered by the certified value itself: elaborating it infers the `Char` struct type, solves a waiting metavariable to it, and reports any mismatch against the type the literal has always had.
                _ => {
                    let value = character_value(context, *character);
                    return elaborate(context, &value, mode);
                }
            }
        }
    };

    if let Mode::Check(expected) = &mode {
        expect(context, term, &type_, expected)?;
    }

    Ok((Term::intrinsic(intrinsic), type_))
}

/// The certified `/syn/Char` value a character literal denotes: the code point with its `Scalar` range proof, exactly the term the lowerer's meta-emitter used to build eagerly. A Rust `char` is already a Unicode scalar, so the range constructor is selected by the code alone and the proof is one closed `qed`.
fn character_value(context: &Context, character: char) -> Term {
    let syntax = context.syntax();
    let code: Term = Subterm::Intrinsic(Intrinsic::Nat(Nat::new(character as u32))).into();
    let constructor = if (character as u32) < 0xD800 {
        syntax.character.scalar_below
    } else {
        syntax.character.scalar_above
    };
    let qed = Term::apply(
        Term::var(curios_core::Var::free(curios_core::Free::global(
            syntax.proof.true_qed.qualifier(),
        ))),
        Vec::<Term>::new(),
    );
    let scalar = Term::apply_marked(
        Term::var(curios_core::Var::free(curios_core::Free::global(
            constructor.qualifier(),
        ))),
        [
            (curios_utilities::Plicity::Implicit, code.clone()),
            (curios_utilities::Plicity::Explicit, qed),
        ],
    );
    Term::struct_(
        Global::Authored(syntax.character.character.qualifier()),
        Vec::<Term>::new(),
        [code, scalar],
    )
}

/// The shape default for an infix operator whose operand type nothing pinned: any signed/negative literal operand forces `Int`, otherwise `Nat`.
///
/// A free function rather than an inherent method on [`Infix`]: the node is representation and lives in `curios-core`, while literal defaulting is an elaboration decision.
pub(super) fn infix_default_type(infix: &Infix) -> Intrinsic {
    let signed = |operand: &Term| matches!(&**operand, Subterm::Transient(Transient::NumLit(NumLit::Number { sign, .. })) if sign.is_marked());

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
    slot: MetavarId,
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
    /// A concept method is not required to be exactly binary. `Divide` states the domain its carrier's division is defined on and takes a proof of it, so `a / b` has a third slot to fill — filled here the way an omitted argument is filled at any other application, which is what routes it through the same discharge. Opening the telescope at two values regardless is what this did before, and it panicked on the arity the moment a declaration carried a third binder.
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
                    let filled = insert_auto_argument(
                        context,
                        plicity,
                        &domain,
                        None,
                        op.symbol(),
                        origin,
                        premise_label(0),
                    )?;
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
        binder: format!("its '{field_name}' implementation"),
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
    let Subterm::FuncType(method_func_type) = &*method_type else {
        panic!("a syn operator concept declares its method as an arrow");
    };
    let telescope = &method_func_type.telescope;
    let plicities = method_func_type.plicities();

    Ok(Some(InfixMethod {
        slot,
        goal,
        witness,
        provenance,
        index,
        signature: telescope.clone(),
        plicities: plicities.to_vec(),
    }))
}

/// Desugar a postfix `!` sequencing site ([`Bang`]) into its `/syn/Monad/bind` application and elaborate the result. The region's monad is read from the expected type and never inferred from the action — the discipline Lean's do-elaborator enforces with `tryPostponeIfNoneOrMVar` and `extractBind`: a bang whose region type is still an unsolved metavariable parks as a whole checking problem and re-runs when it lands, and one in an inference position is refused, because sequencing has no monad until the region names one. The discipline is structural, not a matter of unification order: the wrapper is handed the region's monad as its `@M` (see [`region_monad`]), so every action is checked against `M(A)` with `M` the region's, and an action of another monad is reported at its own `!`. The rebuilt application takes the node's own span, so diagnostics keep anchoring at the written `!`.
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

    // Auto-lift, decided before anything elaborates by reading declared shapes: both monads keyable and different means the action is wrapped in `/syn/Lift`'s `lift`, whose `use` slot resolves the declared embedding — or reports the missing edge. An unreadable action stays unwrapped and keeps the ordinary mismatch; the explicit `lift(action)` spelling always remains.
    let action = match (
        monad_shape(context, &region),
        action_result_shape(context, &bang.action),
    ) {
        (Some(region_shape), Some(action_shape)) if embeds(&region_shape, &action_shape) => {
            lift_wrapped(context, &bang.action, term.span())
        }
        _ => bang.action.clone(),
    };

    // The bind's monad is the region's, supplied before any action is checked. Left to the wrapper's own inference, `M` is pinned by whichever side unifies first — the action's type, since arguments elaborate before the result meets the expected type — so an action of another monad the oracle could not read would fix the region to *its* monad, and the mismatch would surface at the outermost `!` of the region, against an action that was never wrong.
    //
    // **A rigid region the imitation cannot read is refused here, not left to the wrapper.** The region is already reduced and not flex, so its head is a former, a variable or an atom; a variable-headed region — `M(Nat)` under a `use Monad(M)` premise — is exactly what the imitation solves. `None` therefore means the head applies to nothing, `?M(?B)` can never meet it, and the wrapper's inference had only one outcome: `no witness of Monad(?) found`, against a premise of a call the author never wrote, showing a hole where the region's own type was already known. Acceptance is unchanged; what the reader is told is not.
    let Some(monad) = region_monad(context, &region, term.span()) else {
        return Err(Error::bang_region_not_a_monad(region).at_opt(term.span()));
    };
    let head = Term::free_var(&Free::global(context.syntax().monad.bind.qualifier()));
    let arguments = [
        (Plicity::Implicit, monad),
        (Plicity::Explicit, action),
        (Plicity::Explicit, bang.continuation.clone()),
    ];
    let app = Term::apply_marked(head, arguments);
    let app = match term.span() {
        Some(span) => Term::spanned(span, app),
        None => app,
    };
    elaborate(context, &app, mode)
}

/// The region's monad as a term — `λx. T(c̄, x)` for a region `T(c̄, v)` — read by unifying `?M(?B)` with the region: the flex-apply imitation commits the right-biased partial application, exactly the solution the wrapper's own instantiation reaches, and the pairwise equation solves `?B` to the region's value slot. `None` where the imitation does not apply — a region whose head is no nominal or intrinsic former — and the wrapper then infers as before.
fn region_monad(context: &mut Context, region: &Term, span: Option<Span>) -> Option<Term> {
    let sort = context.fresh_classifier_type("region monad");
    let binder = context.fresh(None);
    let former = Term::func_type([(binder, sort.clone())], sort.clone());
    let (_, monad) = context.fresh_placeholder(former, span.clone());
    let (_, value) = context.fresh_placeholder(sort.clone(), span);
    let applied = Term::apply(monad.clone(), [value]);
    match convert(context, &sort, &applied, region) {
        Ok(true) => Some(monad),
        _ => abstracted_monad(context, region, &sort),
    }
}

/// The region's monad read off the application directly, by abstracting its final argument — the fallback for a region whose *value slot* is still unsolved.
///
/// **Which is not the same as a region with no monad, and reporting it as one refused a program the language accepts.** A lambda handed to `File/with` checks against `(File) -> Try(Io, Io/Error, A)` with `A` the caller's implicit, so its region is `Try(Io, Io/Error, ?)`: head rigid, both context arguments rigid, only the slot the `!` itself will solve still open. Asking [`convert`] for it leaves both the abstracted slot and the argument flexible and it declines, so `Try(Io, Io/Error, ?), which is no monad` was reported at the `!` — and annotating the enclosing type, which changes nothing about the monad, made the same program compile.
///
/// The rule is the one `documentation/syntax.md` already states for witness resolution — "an under-applied shape such as `M(A) = State(S, Nat)` infers `M` right-biasedly, as `(A) => State(S, A)`: the final argument is the abstracted one". This applies it where conversion could not guess it.
///
/// Reached only after conversion has failed, so it turns refusals into readings and never changes a region that already elaborates. The head must be keyable — [`monad_shape`]'s own condition — so a flexible or computed head still declines here rather than abstracting something that names no monad; whether what is abstracted *is* a monad stays witness resolution's answer, reported as the missing `Monad` witness it is.
fn abstracted_monad(context: &mut Context, region: &Term, sort: &Term) -> Option<Term> {
    monad_shape(context, region)?;
    let binder = context.fresh(None);
    let slot = Term::free_var(&binder);

    // The value slot is the last *parameter*, which is where [`monad_shape`] reads the context arguments from too — so the two agree about which argument a right-biased abstraction takes, by reading the same field.
    let body = match &**region {
        Subterm::StructType(struct_type) => {
            let mut struct_type = struct_type.clone();
            *struct_type.params.last_mut()? = slot;
            Term::from(Subterm::StructType(struct_type))
        }
        Subterm::InductType(induct_type) => {
            let mut induct_type = induct_type.clone();
            *induct_type.params.last_mut()? = slot;
            Term::from(Subterm::InductType(induct_type))
        }
        _ => return None,
    };

    Some(Term::func_marked(
        [(Plicity::Explicit, binder, sort.clone())],
        body,
    ))
}

/// `action` wrapped in `/syn/Lift`'s `lift`, whose `use` slot resolves the declared embedding into the region or reports the missing edge; the wrapper takes the action's own span, or `fallback`, so the report anchors where the action was written.
fn lift_wrapped(context: &Context, action: &Term, fallback: Option<Span>) -> Term {
    let field = context.syntax().lift.lift;
    let wrapper = Term::free_var(&Free::global(field.concept.qualifier().with(field.field)));
    let wrapped = Term::apply(wrapper, vec![action.clone()]);
    match action.span().or(fallback) {
        Some(span) => Term::spanned(span, wrapped),
        None => wrapped,
    }
}

/// Auto-lift on a checked tail, the region-end twin of [`elaborate_bang`]'s wrap: a term whose named head is declared in a monad other than the region's, checked where the region's monad is rigid, is wrapped in `lift` before it elaborates, so the declared edge carries it or the missing edge is reported — `Some(wrapped)` to elaborate in the tail's place. It fires only where both heads are registered monads, so a mismatch between two data types stays the ordinary mismatch, and it abstains exactly where the `!` oracle does: a flexible region, an unreadable head, or equal keys. The monad gate is read before the head, since most checked nodes sit under a data type and settle on one table lookup.
pub(super) fn lift_on_check(
    context: &mut Context,
    term: &Term,
    expected: &Term,
) -> Result<Option<Term>, Error> {
    if !matches!(&**term, Subterm::Var(_) | Subterm::Apply(_)) {
        return Ok(None);
    }
    let region = reduce_with(context, expected)?;
    if region_flex(context, &region) {
        return Ok(None);
    }
    let Some(region_shape) = monad_shape(context, &region) else {
        return Ok(None);
    };
    if !is_monad(context, &region_shape.head) {
        return Ok(None);
    }
    let Some(action_shape) = action_result_shape(context, term) else {
        return Ok(None);
    };
    if !embeds(&region_shape, &action_shape) || !is_monad(context, &action_shape.head) {
        return Ok(None);
    }
    Ok(Some(lift_wrapped(context, term, None)))
}

/// What identifies a monad for the lift oracle: the rigid head, and the keys of the application's *context* arguments — every argument but the last, which is the slot a right-biased partial application abstracts and so the value slot, free to differ between an action and its region. A context argument that keys on nothing, a binder the action's own telescope will solve above all, is `None` and compatible with anything.
pub(crate) struct MonadShape {
    pub(crate) head: HeadKey,
    pub(crate) context: Vec<Option<HeadKey>>,
}

/// The shape of a weak-head-normal monad application, or `None` where its head is not keyable.
pub(crate) fn monad_shape(context: &mut Context, whnf: &Term) -> Option<MonadShape> {
    let head = HeadKey::of_whnf(whnf)?;
    let params: &[Term] = match &**whnf {
        Subterm::StructType(struct_type) => &struct_type.params,
        Subterm::InductType(induct_type) => &induct_type.params,
        _ => &[],
    };
    let context_args = params.split_last().map_or(&[][..], |(_, context)| context);
    let context = context_args
        .iter()
        .map(|arg| {
            reduce_with(context, arg)
                .ok()
                .and_then(|whnf| HeadKey::of_whnf(&whnf))
        })
        .collect();

    Some(MonadShape { head, context })
}

/// Whether an action of shape `action` belongs to another monad than a region of shape `region`, so that sequencing it needs an embedding: a different head, or a context argument both sides key and key differently — `Try(Io, E)` beside `Try(Async, E)`, which share a head and are two monads. Two shapes that agree wherever both are known are one monad as far as the oracle can read, and unification settles the rest.
pub(crate) fn embeds(region: &MonadShape, action: &MonadShape) -> bool {
    region.head != action.head
        || region
            .context
            .iter()
            .zip(&action.context)
            .any(|pair| matches!(pair, (Some(here), Some(there)) if here != there))
}

/// Whether a `Monad` witness is registered under `key` — the concept's name derives from the registry's bind wrapper, whose namespace is the concept.
pub(crate) fn is_monad(context: &Context, key: &HeadKey) -> bool {
    let monad = Global::Authored(context.syntax().monad.bind.qualifier().without_last());
    context
        .witness(&monad, &WitnessKey(vec![key.clone()]))
        .is_some()
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
fn action_result_shape(context: &mut Context, action: &Term) -> Option<MonadShape> {
    let mut head = action;
    let mut explicit: Vec<Term> = Vec::new();
    loop {
        match &**head {
            // The spine is peeled from the outside in, so an inner application's arguments go before the ones already collected.
            Subterm::Apply(apply) => {
                let mut here: Vec<Term> = apply
                    .params()
                    .zip(apply.plicities())
                    .filter(|(_, plicity)| matches!(plicity, Plicity::Explicit))
                    .map(|(argument, _)| argument.clone())
                    .collect();
                here.append(&mut explicit);
                explicit = here;
                head = &apply.head;
            }
            Subterm::Var(var) => {
                let declared = context.assumption(var.as_free()?)?.clone();
                return declared_result_shape(context, &declared, &explicit);
            }
            _ => return None,
        }
    }
}

/// The [`MonadShape`] of `declared`'s result when a spine of `explicit_args` explicit arguments saturates it exactly; `None` otherwise. The telescope is opened with fresh frees on the way to the result, so a *dependent* result — `Io(Cell(T))`, `Io(Future(A))` — keys on its head like any other: the head is rigid whatever the binder, and a result actually *headed* by a binder (`M(Nat)` under `(M: (Type) -> Type, …)`), or carrying one in a *context* argument (`Try(M, E, A)` under the same telescope), keys that binder by the argument that fixes it (see [`binder_key`]), and one no argument fixes keys on nothing there and is compatible with any region. The result takes one weak-head reduction before keying, because a declared type keeps its nominal spelling (`Io({})` is stored as the `/sys/Io/Io` application, aliases as their own names); the reduction is the same read `resolve`'s `node_type` performs on assumption-derived types, and a reduction failure abstains. A wrong abstention still costs a message, never a solution.
fn declared_result_shape(
    context: &mut Context,
    declared: &Term,
    args: &[Term],
) -> Option<MonadShape> {
    // The declared type is peeled arrow by arrow until the spine's arguments are spent, each arrow's binders opened with fresh frees: a concept method wrapper is curried — `(@S: Type, use w: Read(S)) -> (S, Nat) -> Async(Chunk)` — so its explicit binders sit behind an arrow of hidden ones. An arrow with more explicit binders than arguments remain is a partial application, which is a function and not an action, and abstains. A zero-argument action (`Async/yield_now`) has the carrier itself as its declared type.
    let mut whnf = reduce_with(context, declared).ok()?;
    let mut remaining = args;
    let mut explicit_binders = Vec::new();
    while let Subterm::FuncType(func_type) = &*whnf {
        let plicities = func_type.plicities().to_vec();
        let explicit_count = plicities
            .iter()
            .filter(|plicity| matches!(plicity, Plicity::Explicit))
            .count();
        if explicit_count > remaining.len() {
            return None;
        }
        remaining = &remaining[explicit_count..];
        let mut telescope = func_type.telescope.clone();
        let mut position = 0;
        let result = loop {
            match telescope {
                Telescope::Cons(binder_type, rest) => {
                    let binder = context.fresh(rest.first_hint());
                    if matches!(plicities.get(position), Some(Plicity::Explicit)) {
                        explicit_binders.push((binder.clone(), binder_type.clone()));
                    }
                    position += 1;
                    telescope = rest.open(&[&Term::free_var(&binder)]);
                }
                Telescope::Done(result) => break (*result).clone(),
            }
        };
        whnf = reduce_with(context, &result).ok()?;
    }
    if !remaining.is_empty() {
        return None;
    }
    if let Some(mut shape) = monad_shape(context, &whnf) {
        // A context slot that is one of the telescope's own binders — the `M` of `Try(M, E, A)` under `(@M: (Type) -> Type, …, m: Try(M, E, A), …)` — keys on nothing by itself, and is the base an argument fixes: `Try/rescue(t, h)!` lifts as the action `t` was declared over, exactly as a binder-headed result below does. A slot no argument settles stays `None`, compatible with any region.
        let params: Vec<Term> = match &*whnf {
            Subterm::StructType(struct_type) => struct_type.params.clone(),
            Subterm::InductType(induct_type) => induct_type.params.clone(),
            _ => Vec::new(),
        };
        let context_params = params.split_last().map_or(&[][..], |(_, context)| context);
        for (slot, param) in shape.context.iter_mut().zip(context_params) {
            if slot.is_none()
                && let Subterm::Var(var) = &**param
                && let Some(base) = var.as_free()
            {
                *slot = binder_key(context, base, &explicit_binders, args);
            }
        }
        return Some(shape);
    }

    // A result headed by one of the telescope's own binders — `M(Result(E, A))` under `(@M: (Type) -> Type, …, m: Try(M, E, A))` — is the base an argument fixes: the explicit parameter whose declared type mentions the binder is read against its argument's own declared shape, position for position, so `Try/run(t)!` lifts as the action `t` was declared over. Still a read: nothing elaborates, and an argument that keys on nothing at that position abstains as before.
    let Subterm::Apply(apply) = &*whnf else {
        return None;
    };
    let Subterm::Var(var) = &*apply.head else {
        return None;
    };
    let base = var.as_free()?.clone();
    let head = binder_key(context, &base, &explicit_binders, args)?;
    let arguments: Vec<Term> = apply.params().cloned().collect();
    let context_args = arguments
        .split_last()
        .map_or(&[][..], |(_, context)| context);
    let context = context_args
        .iter()
        .map(|arg| {
            reduce_with(context, arg)
                .ok()
                .and_then(|whnf| HeadKey::of_whnf(&whnf))
        })
        .collect();

    Some(MonadShape { head, context })
}

/// The key `base`, a binder of the declared telescope, is fixed at by the explicit arguments: the first explicit parameter whose declared type applies `base` takes its argument's head, and one whose declared type is a nominal application with `base` in a context slot takes the key its argument's shape has there. `None` where no argument settles it.
fn binder_key(
    context: &mut Context,
    base: &Free,
    explicit_binders: &[(Free, Term)],
    args: &[Term],
) -> Option<HeadKey> {
    let is_base = |term: &Term| matches!(&**term, Subterm::Var(var) if var.as_free() == Some(base));

    for ((_, parameter_type), arg) in explicit_binders.iter().zip(args) {
        let parameter = reduce_with(context, parameter_type).ok()?;
        let positions: Vec<Term> = match &*parameter {
            Subterm::Apply(apply) if is_base(&apply.head) => {
                return action_result_shape(context, arg).map(|shape| shape.head);
            }
            Subterm::StructType(struct_type) => struct_type.params.clone(),
            Subterm::InductType(induct_type) => induct_type.params.clone(),
            _ => continue,
        };
        let Some(parameter_head) = HeadKey::of_whnf(&parameter) else {
            continue;
        };
        let Some(shape) = action_result_shape(context, arg) else {
            continue;
        };
        if shape.head != parameter_head {
            continue;
        }
        let context_positions = positions
            .split_last()
            .map_or(&[][..], |(_, context)| context);
        for (index, position) in context_positions.iter().enumerate() {
            if is_base(position)
                && let Some(Some(key)) = shape.context.get(index)
            {
                return Some(key.clone());
            }
        }
    }

    None
}

/// Elaborate an infix operator ([`Infix`]) as a concept method call. A fresh operand-type metavar `?T` is pinned by the non-literal operands first (or, for an operator whose method returns its operand type, by the expected result type), then defaulted from the operand literals if nothing constrains it; only then are the literal operands checked — against a `?T` that is already concrete, so they never force it to their own default. That ordering is what lets `1 + flt` resolve to `Flt` rather than a `Nat`/`Flt` mismatch.
///
/// Dispatch is then **one path**: every operator desugars to a projection of a witness of its `/syn` concept ([`OperatorSyntax::concept_field`](curios_utilities::OperatorSyntax::concept_field)) — `a + b` ≙ `Add/add(a, b)`, intrinsics included, resolved by the same engine that fills `use` slots (so `no witness of Add(Point)` is the single error vocabulary, and what an operator means at a type is entirely a question of which witnesses exist). There are no carved-out operators: `&&`/`||` project `And`/`Or`, and `!=` projects `Equal`'s `neq` rather than negating a rebuilt `eql`, so this function synthesizes no term of its own and reads every result type from the declaration. The node never survives elaboration; witness projections over the statically-known intrinsic witnesses collapse back to bare `Intrinsic` code in the backend (`And(Bool)`/`Or(Bool)` collapse to `BoolAnd`/`BoolOr` exactly as `Equal(Bool)` collapses to `BoolEql` — see the codegen parity tests).
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

    // How many of these slots a lambda must write. The hidden ones are inserted from the expected type, so they are the parameters an arity refusal may not count.
    fn explicit(plicities: &[Plicity]) -> usize {
        plicities
            .iter()
            .filter(|plicity| **plicity == Plicity::Explicit)
            .count()
    }

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
        let e_plicities = ft.plicities().to_vec();
        let mut expected_tele = ft.telescope;
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
                        // Explicit slots against explicit binders, not the totals: the hidden slots are inserted rather than written, so a total names a count the author may not write — and did, pointing at the very spelling the surplus arm below refuses.
                        break Err(Error::wrong_number_of_arguments(
                            explicit(&e_plicities),
                            explicit(written_plicities),
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
                // Written binders remain but the expected telescope ended: every parameter is claimed and these claim nothing. No count pair says that — `(x, @A) => …` against `(x: Nat) -> Nat` agrees on totals *and* on explicit counts — so the surplus itself is the diagnosis.
                (Telescope::Cons(..), Telescope::Done(_)) => {
                    break Err(Error::surplus_func_binders(
                        telescope.len() - w_idx,
                        e_plicities.len(),
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

/// Synthesize a function type from a lambda's own domain annotations — the mirror of `elaborate_func_type`. Without an expected type no binders can be inserted, so the lambda's written plicity sequence is already canonical: the walk keeps each written mark, entering a `use` binder into the witness scope for later domains and the body, and the synthesized `FuncType`/rebuilt `Func` both carry exactly that sequence. A domain that stays an unconstrained hole (the bare `(x) => …` sugar, or `(x : _)`) offers nothing to synthesize from, so inference fails — unless `settle` carries the lambda term: a settle tier has established that no structure is coming from the expectation, so the hole is replaced by a metavariable named after its binder ([`MetavarOrigin::Domain`](curios_core::MetavarOrigin)), pinned by the body or by whatever the settled type later unifies with, and reported by zonk as the parameter whose type was never determined when nothing ever pins it. The rebuilt lambda and its type share the same closed domains, so both stay de-Bruijn-correct.
pub(super) fn elaborate_func_infer(
    context: &mut Context,
    telescope: &Telescope<Term>,
    plicities: &[Plicity],
    settle: Option<&Term>,
) -> Result<(Term, Term), Error> {
    // The settle scope is captured before the walk assumes a single binder: every domain metavariable is born at the settling expectation's own frame, never under the lambda's, which is what the embedded-metavariable exemption in `solve` needs to commit the settled type.
    let scope = settle.map(|_| context.domain_scope());

    fn walk(
        context: &mut Context,
        body: Telescope<Term>,
        plicities: &[Plicity],
        settle: Option<(&Term, &DomainScope)>,
        domains: &mut Vec<(Plicity, Free, Term)>,
    ) -> Result<(Term, Term), Error> {
        match body {
            Telescope::Done(body) => elaborate(context, &body, Mode::Infer),
            Telescope::Cons(domain, body_rest) => {
                let domain = crate::check_is_sort(context, &domain)?.0;

                // A domain nothing pins is refused here rather than left to fail obscurely downstream — but only a silent hole is: a written `?` domain is the author asking what the domain is, and it rides on to zonk's report (`MetavarOrigin` states the rule). A settle tier instead admits the hole as a named domain metavariable, per the function's contract above.
                let reduced = reduce_with(context, &domain)?;
                let domain = match &*reduced {
                    Subterm::Metavar(metavar) if metavar.is_hole() => match settle {
                        None => return Err(Error::CannotInfer),
                        Some((lambda, scope)) => {
                            let result = context
                                .metavar_entry(metavar.id)
                                .map(|entry| entry.result.clone())
                                .unwrap_or_else(Term::type_ground);
                            let binder = body_rest.first_hint().unwrap_or("_").to_string();
                            let named =
                                context.fresh_domain_metavar(scope, result, lambda.span(), binder);
                            context.solve_metavar(metavar.id, named.clone());
                            named
                        }
                    },
                    _ => domain,
                };

                let plicity = plicities[domains.len()];
                let name = context.fresh(body_rest.first_hint());
                let x = Term::free_var(&name);
                match plicity {
                    Plicity::Witness => context.assume_witness(&name, &domain),
                    _ => context.assume(&name, &domain),
                }
                domains.push((plicity, name, domain));
                walk(context, body_rest.open(&[&x]), plicities, settle, domains)
            }
        }
    }

    let mut domains = Vec::new();
    let settle = match (settle, &scope) {
        (Some(lambda), Some(scope)) => Some((lambda, scope)),
        _ => None,
    };
    let (body, output) = context
        .with_frame(|context| walk(context, telescope.clone(), plicities, settle, &mut domains))?;

    Ok((
        Term::func_marked(domains.clone(), body),
        Term::func_type_marked(domains, output),
    ))
}
