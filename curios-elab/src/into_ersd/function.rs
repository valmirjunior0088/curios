//! Functions and saturated application.
//!
//! A lambda reserves its arena identity, takes one parameter for each binder of its telescope, erases its body into an owned block, and is bound by a one-member `Functions` statement at its defining position; the expression's operand is the function atom. No captures are stored anywhere — free values are derived by analysis.
//!
//! **A function's runtime arity is its telescope's length.** Whether a binder carries anything is read off its type, and a function value stands at several types: the one it is checked against where it is written, with a caller's arguments substituted in, and the one its callee declares, with a variable where that argument goes. One reads a parameter as a proof or a type where the other reads a variable, so an arity decided by either is an arity the other does not have. Nothing is decided: every binder is a parameter, and what a proof or a type passes for one is the content-free stand-in. A parameter nothing reads is the optimizer's to remove, where it sees every call (`curios-cont`).
//!
//! Application evaluates head first, then its arguments in order (under the operand law the erasure order *is* the runtime order). An application at an erasable (proof-/type-producing) function type whose callee is not a direct function reference is erased content applied to arguments — it collapses to the unit constant; a direct function reference keeps its call, because a never-returning host effect is proof-typed but must run. Every argument has its slot; one whose type here is a proof or a type fills it with the unit constant rather than a materialized witness (proof irrelevance — the slot stays, only its contents collapse).

use {
    super::{
        Apply, Error, Func, FuncType, Lowering, Outcome, Retyping, Subterm, Term, emitted,
        is_erasable,
    },
    curios_core::{DefinitionKind, Global, Lockstep, Step},
};

impl Lowering<'_> {
    pub(super) fn erase_func(
        &mut self,
        context: &mut Retyping,
        func: &Func,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let function = self.builder.reserve_function();
        // A lambda in argument position carries no statement to be named after; it takes its owner's name instead of none at all.
        let name = hint.map(str::to_string).or_else(|| self.derived_hint());
        match name {
            Some(name) => {
                let hint = name.clone();
                self.with_owner(name, |lowering| {
                    lowering.define_lambda(context, function, func, expected, Some(&hint))
                })?;
            }
            None => self.define_lambda(context, function, func, expected, None)?,
        }
        self.builder.let_functions(vec![function]);
        Ok(Outcome::Emitted(curios_ersd::Atom::Function(function)))
    }

    /// Erase a lambda into a previously reserved function identity — shared by the expression form above (which reserves and binds it) and recursive groups (which reserve every member first for mutual visibility).
    pub(super) fn define_lambda(
        &mut self,
        context: &mut Retyping,
        function: curios_ersd::FunctionId,
        func: &Func,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<(), Error> {
        let ft = match Term::unwrap_or_clone(context.reduce_with(expected)?) {
            Subterm::FuncType(ft) => ft,
            // Elaborate already checked this function against a function type.
            _ => unreachable!("erase: function checked against non-function type"),
        };

        let mut params = Vec::new();

        let body = context.with_frame(|context| -> Result<_, Error> {
            // Walk the lambda's telescope (whose `Done` is the body) alongside the checked function type's telescope (whose `Done` is the output type), opening both with one fresh variable per binder.
            let type_telescope = ft.telescope;
            let mut walk = Lockstep::new(&func.telescope, &type_telescope);
            let (body, output) = loop {
                match walk.step() {
                    Step::Bodies(body, output) => break (body, output),
                    Step::Entries {
                        hint, right: type_, ..
                    } => {
                        // Every binder is a parameter, a proof's and a type's among them: see the module documentation.
                        let label = hint.map(str::to_string);
                        let name = context.advance_assumed(&mut walk, &type_);
                        let param = self.builder.value(label);
                        self.environment
                            .bind(&name, curios_ersd::Atom::Value(param));
                        params.push(param);
                    }
                    Step::Mismatch => unreachable!("erase: function/type telescope arity mismatch"),
                }
            };

            self.builder.open_block();
            let terminator = match self.walk(context, &body, &output, None)? {
                Outcome::Emitted(atom) => curios_ersd::Terminator::Return(atom),
                Outcome::Diverged(terminator) => terminator,
            };

            Ok(self.builder.seal_block(terminator))
        })?;

        self.builder
            .define_function(function, hint.map(str::to_string), params, body);
        Ok(())
    }

    pub(super) fn erase_apply(
        &mut self,
        context: &mut Retyping,
        apply: &Apply,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // A concept method's wrapper is erased as the projection call it forwards to. Called through the wrapper, `bind(dict, value, k)` at top level is a call whose callee calls a closure the effect summary cannot see, so pruning would judge its eager evaluation observable and keep it and everything it reaches — through `/std/Json/decode/decode`, the whole `Json`/`Parse` web in every program. Written as `dict.bind(value, k)`, closed-term evaluation resolves the method off the known dictionary before the prune.
        if let Some(forwarded) = forwarded_method(context, apply) {
            return self.walk(context, &forwarded, expected, hint);
        }

        let head = &apply.head;
        let params = apply.params().cloned().collect::<Vec<_>>();

        let head_type = self.type_of(context, head)?;
        let head_type = context.reduce_with(&head_type)?;
        let ft = match &*head_type {
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.clone(),
            _ => unreachable!("erase: applied a non-function"),
        };
        assert_eq!(
            params.len(),
            ft.len(),
            "erase: application arity disagrees with the function type",
        );

        let callee = emitted!(self.walk(context, head, &head_type, None)?);

        // A proof-valued callee that is not a direct function reference is erased content standing where a function was expected — a projection of an erased field, a dropped binder, or a wrapper call returning an erased method: the application is proof content and collapses to the unit constant. The check is value-driven, not type-driven: a *direct* function reference keeps its call even at an erasable type. Nothing that must run is proof-typed — `exit` yields `Io(A)`, which never erases, and a foreign result is a wire type — so the call kept here is to a total, pure function whose result nothing reads, left for the optimizer to drop rather than decided here.
        if !matches!(callee, curios_ersd::Atom::Function(_)) && is_erasable(context, &head_type)? {
            return Ok(Outcome::Emitted(self.unit()));
        }

        // A `Prop` family's constructor is the one direct call that *has* to be dropped here: its layout keeps no payload (see `induct_row`), and its arguments exist only to fill fields erasure has already removed — and they may mention binders erasure dropped, so walking them would compute over placeholder units.
        if is_proof_constructor(context, head)? {
            return Ok(Outcome::Emitted(self.unit()));
        }

        // Every argument has its slot, which is what makes the call's arity the callee's whatever type either is read at; each is erased against its instantiated (dependent) domain, a proof or a type there as the stand-in.
        let arguments = match self.slots(context, ft, &params)? {
            Ok(arguments) => arguments,
            Err(diverged) => return Ok(diverged),
        };

        Ok(self.bind(hint, curios_ersd::Rhs::Apply { callee, arguments }))
    }

    /// Erase a value held in a slot. A slot is there whatever it holds, but this instantiation can make what it holds a proof or a type, which carries no runtime content: proof irrelevance fills the slot with a content-free stand-in — the unit constant, or, for a *function* a callee may apply, a function of that type's arity returning it — rather than materializing a witness no runtime code reads.
    ///
    /// A `let` binding is a kept slot too: the name stays bound for the tail, and a proof or a type bound to it is not computed. Nothing is lost by that. No term outside `Io` performs an effect, so the value performed nothing; the totality obligations make every proof and every type total, so it would have returned; and what it could still have done is refuse on a run-time carrier, which is a behaviour no proof should be able to give a program.
    pub(super) fn kept_operand(
        &mut self,
        context: &mut Retyping,
        value: &Term,
        type_: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        match is_erasable(context, type_)? {
            true => Ok(Outcome::Emitted(self.proof_stub(context, type_)?)),
            false => self.walk(context, value, type_, hint),
        }
    }

    /// The content-free stand-in for an erased value in a slot, shaped by the slot's type. A proof or a type is the unit constant. A proof-valued *function* — a `Type`-valued parameter instantiated at a proposition, `Nat/descend`'s `step` at a `Prop` motive — is a function of its telescope's length whose body is the stand-in for its codomain, one layer per curried arrow: the callee applies what is in the slot, and applying the unit constant traps where applying this returns. The two sides of the seam agree as every call and its function do, on the telescope's length.
    fn proof_stub(
        &mut self,
        context: &mut Retyping,
        type_: &Term,
    ) -> Result<curios_ersd::Atom, Error> {
        let ft = match Term::unwrap_or_clone(context.reduce_with(type_)?) {
            Subterm::FuncType(ft) => ft,
            _ => return Ok(self.unit()),
        };

        let function = self.builder.reserve_function();
        let mut params = Vec::new();

        let body = context.with_frame(|context| -> Result<_, Error> {
            let telescope = ft.telescope;
            let mut cursor = telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                let name = context.fresh(None);
                context.assume(&name, &domain);
                params.push(self.builder.value(None));
                cursor.advance(Term::free_var(&name));
            }
            let output = cursor.body().expect("a cursor past every entry");

            self.builder.open_block();
            let result = self.proof_stub(context, &output)?;
            Ok(self
                .builder
                .seal_block(curios_ersd::Terminator::Return(result)))
        })?;

        self.builder.define_function(function, None, params, body);
        self.builder.let_functions(vec![function]);
        Ok(curios_ersd::Atom::Function(function))
    }
}

/// The call a concept method's wrapper forwards to, at `apply`'s arguments — `w.i(x…)` for a method, `w.i` for any other field — or `None` when the callee is not a wrapper.
///
/// `into_core` generates each wrapper as exactly that forwarding, `(@p…, use w, x…) => w.i(x…)`, and a call fills its one parameter group, so every call to one supplies the whole group and unfolds in one step: the wrapper's body opened at the call's arguments. One step and no further — reducing the call instead would go on into the method's own body wherever the dictionary is known, which is inlining arbitrary code. The kind is the definition's, read as [`is_proof_constructor`] reads its own, rather than guessed from the name.
fn forwarded_method(context: &Retyping, apply: &Apply) -> Option<Term> {
    let Subterm::Var(var) = &*apply.head else {
        return None;
    };
    let name = var.as_free()?;
    let Some(DefinitionKind::ConceptMethod { .. }) = context.definition_kind(name) else {
        return None;
    };
    let Subterm::Func(wrapper) = &**context.definition_body(name)? else {
        return None;
    };
    let arguments = apply.params().collect::<Vec<_>>();
    (wrapper.telescope.len() == arguments.len()).then(|| wrapper.telescope.open(&arguments))
}

/// Whether `head` names a constructor of a `Prop`-sorted inductive.
///
/// The owning family comes from the definition's own [`DefinitionKind::InductiveConstructor`], recorded by `into_core` where the constructor function was generated. Splitting the name into a qualifier prefix and a last segment would instead ask whether some *other* declaration happens to be registered under this name's prefix — a question whose answer is only accidentally the same one, and which decides whether a call is dropped entirely.
fn is_proof_constructor(context: &mut Retyping, head: &Term) -> Result<bool, Error> {
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
    let Some(declaration) = context.induct_decl(&Global::Authored(*owner)).cloned() else {
        return Ok(false);
    };

    Ok(matches!(
        &*context.reduce_with(&declaration.result_sort)?,
        Subterm::Prop(_)
    ))
}
