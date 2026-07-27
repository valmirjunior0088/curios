//! Functions and saturated application.
//!
//! A lambda reserves its arena identity, classifies each parameter against
//! the checked function type's telescope (an erasable parameter — a proof or
//! a type — is dropped from the runtime signature entirely), erases its body
//! into an owned block, and is bound by a one-member `Functions` statement at
//! its defining position; the expression's operand is the function atom. No
//! captures are stored anywhere — free values are derived by analysis.
//!
//! Application evaluates head first, then the kept arguments in order (the
//! order the legacy pipeline sequenced at CPS conversion; under the operand
//! law the erasure order *is* the runtime order). An application at an
//! erasable (proof-/type-producing) function type whose callee is not a
//! direct function reference is erased content applied to arguments — it
//! collapses to the unit constant; a direct function reference keeps its
//! call, because a never-returning host effect is proof-typed but must run.
//! Arguments to erasable parameters are dropped; a kept slot whose
//! instantiated type is a proof or a type is filled with the unit constant
//! rather than a materialized witness (proof irrelevance — the slot stays,
//! only its contents collapse).

use {
    super::{
        Apply, Context, Error, Func, FuncType, Lowering, Outcome, Subterm, Telescope, Term,
        emitted, erasure_mask, infer, is_erasable, reduce_with,
    },
    crate::DefinitionKind,
    crate::Global,
};

impl Lowering {
    pub(super) fn erase_func(
        &mut self,
        context: &mut Context,
        func: &Func,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let function = self.builder.reserve_function();
        self.define_lambda(context, function, func, expected, hint)?;
        self.builder.let_functions(vec![function]);
        Ok(Outcome::Emitted(curios_ersd::Atom::Function(function)))
    }

    /// Erase a lambda into a previously reserved function identity — shared by
    /// the expression form above (which reserves and binds it) and recursive
    /// groups (which reserve every member first for mutual visibility).
    pub(super) fn define_lambda(
        &mut self,
        context: &mut Context,
        function: curios_ersd::FunctionId,
        func: &Func,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<(), Error> {
        let ft = match Term::unwrap_or_clone(reduce_with(context, expected)?) {
            Subterm::FuncType(ft) => ft,
            // Elaborate already checked this function against a function type.
            _ => unreachable!("erase_ir: function checked against non-function type"),
        };

        let mut params = Vec::new();
        let mut dropped = Vec::new();

        let body = context.with_frame(|context| -> Result<_, Error> {
            // Walk the lambda's telescope (whose `Done` is the body) alongside
            // the checked function type's telescope (whose `Done` is the
            // output type), opening both with one fresh variable per binder.
            let mut body_telescope = func.telescope.clone();
            let mut type_telescope = ft.telescope;
            let (body, output) = loop {
                match (body_telescope, type_telescope) {
                    (Telescope::Done(body), Telescope::Done(output)) => break (*body, *output),
                    (Telescope::Cons(_domain, body_rest), Telescope::Cons(type_, type_rest)) => {
                        let label = body_rest.first_hint().map(str::to_string);
                        let name = context.fresh(label.as_deref());
                        let variable = Term::free_var(&name);
                        // The flag is read before the binder is assumed: a
                        // parameter's type never depends on the parameter.
                        let erasable = is_erasable(context, &type_)?;
                        context.assume(&name, &type_);
                        if erasable {
                            self.environment.bind_dropped(&name);
                            dropped.push(name);
                        } else {
                            let param = self.builder.value(label);
                            self.environment
                                .bind(&name, curios_ersd::Atom::Value(param));
                            params.push(param);
                        }
                        body_telescope = body_rest.open(&[&variable]);
                        type_telescope = type_rest.open(&[&variable]);
                    }
                    _ => unreachable!("erase_ir: function/type telescope arity mismatch"),
                }
            };

            self.builder.open_block();
            let outcome = self.walk(context, &body, &output, None)?;
            match outcome {
                Outcome::Emitted(atom) => {
                    // Collapse the body to a unit only when it is both
                    // proof/type-valued AND references a binder this lambda
                    // dropped — a dangling proof result. An effectful body
                    // that merely happens to be proof-typed keeps its effect,
                    // and a pure proof body referencing nothing dropped is
                    // already content-free.
                    let dangles = dropped.iter().any(|label| self.dangled.contains(label));
                    let result = if dangles && is_erasable(context, &output)? {
                        self.unit()
                    } else {
                        atom
                    };
                    Ok(self
                        .builder
                        .seal_block(curios_ersd::Terminator::Return(result)))
                }
                Outcome::Diverged(terminator) => Ok(self.builder.seal_block(terminator)),
            }
        })?;

        self.builder
            .define_function(function, hint.map(str::to_string), params, body);
        Ok(())
    }

    pub(super) fn erase_apply(
        &mut self,
        context: &mut Context,
        apply: &Apply,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let Apply { head, params, .. } = apply;

        let head_type = infer(context, head)?;
        let head_type = reduce_with(context, &head_type)?;
        let ft = match &*head_type {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.clone(),
            _ => unreachable!("erase_ir: applied a non-function"),
        };
        assert_eq!(
            params.len(),
            ft.len(),
            "erase_ir: application arity disagrees with the function type",
        );

        let callee = emitted!(self.walk(context, head, &head_type, None)?);

        // A proof-valued callee that is not a direct function reference is
        // erased content standing where a function was expected — a projection
        // of an erased field, a dropped binder, or a wrapper call returning an
        // erased method: the application is proof content and collapses to the
        // unit constant. The check must be value-driven, not type-driven: a
        // *direct* function reference keeps its call even at an erasable type,
        // because a never-returning host effect (`/std/proc/exit : (Nat) ->
        // False`, polymorphic `/sys/exit`) is proof-typed but must run.
        if !matches!(callee, curios_ersd::Atom::Function(_)) && is_erasable(context, &head_type)? {
            return Ok(Outcome::Emitted(self.unit()));
        }

        // A `Prop` family's constructor is the one direct call that *is* safe to
        // drop: it is pure, so no host effect rides on it, and its layout keeps
        // no payload (see `induct_row`). Its arguments exist only to fill fields
        // erasure has already removed — and they may mention binders erasure
        // dropped, so walking them would compute over placeholder units. The
        // never-returning host effects the check above protects are foreign
        // definitions, never constructors.
        if is_proof_constructor(context, head)? {
            return Ok(Outcome::Emitted(self.unit()));
        }

        // The drop decision is the signature mask (opaque-opened), so it
        // agrees with the function's fixed runtime arity even when a
        // polymorphic domain is instantiated at a prop here; the kept walk
        // erases against the instantiated (dependent) domain.
        let mask = erasure_mask(context, ft.clone())?;
        let arguments = match self.masked_fields(context, &mask, ft, params)? {
            Ok(arguments) => arguments,
            Err(diverged) => return Ok(diverged),
        };

        Ok(self.bind(hint, curios_ersd::Rhs::Apply { callee, arguments }))
    }

    /// Erase a value held in a kept slot. The mask keeps the slot for uniform
    /// arity, but this instantiation can still make it a proof or a type,
    /// which carries no runtime content: proof irrelevance fills the slot
    /// with the unit constant rather than materializing a witness no runtime
    /// code reads.
    pub(super) fn kept_operand(
        &mut self,
        context: &mut Context,
        value: &Term,
        type_: &Term,
    ) -> Result<Outcome, Error> {
        self.kept_operand_at(context, value, type_, None)
    }

    /// [`kept_operand`](Self::kept_operand) with a binding hint.
    pub(super) fn kept_operand_at(
        &mut self,
        context: &mut Context,
        value: &Term,
        type_: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        match is_erasable(context, type_)? {
            true => Ok(Outcome::Emitted(self.unit())),
            false => self.walk(context, value, type_, hint),
        }
    }
}

/// Whether `head` names a constructor of a `Prop`-sorted inductive.
///
/// The owning family comes from the definition's own
/// [`DefinitionKind::InductiveConstructor`], recorded by `into_core` where the
/// constructor function was generated. Splitting the name into a qualifier
/// prefix and a last segment would instead ask whether some *other*
/// declaration happens to be registered under this name's prefix — a question
/// whose answer is only accidentally the same one, and which decides whether a
/// call is dropped entirely.
fn is_proof_constructor(context: &mut Context, head: &Term) -> Result<bool, Error> {
    let Subterm::Var(var) = &**head else {
        return Ok(false);
    };
    let Some(name) = var.as_free() else {
        return Ok(false);
    };
    let Some(DefinitionKind::InductiveConstructor { owner, .. }) = context.definition_kind(name)
    else {
        return Ok(false);
    };
    let Some(declaration) = context
        .induct_decl(&Global::Authored(owner.clone()))
        .cloned()
    else {
        return Ok(false);
    };

    Ok(matches!(
        &*reduce_with(context, &declaration.result_sort)?,
        Subterm::Prop
    ))
}
