//! The lowering driver: the entry point, the expression walk, and the binding
//! forms.
//!
//! The walk mirrors the legacy recursive erasure's control structure (its
//! stack behavior is the no-regression baseline) but produces operands under
//! the operand law instead of terms: [`Outcome::Emitted`] carries the atom a
//! subexpression erased to, [`Outcome::Diverged`] carries the terminator that
//! seals the innermost block when the subexpression provably never yields a
//! value. Every non-atomic computation is bound by the builder at the point
//! the walk reaches it, so evaluation order is statement order by
//! construction.

use {
    super::{
        Binding, Bound, Context, Environment, Error, Let, Module, Qualifier, Subterm, Term,
        emitted, prim,
    },
    std::collections::BTreeSet,
};

/// What one expression erased to. See the module documentation.
#[derive(Debug)]
pub(super) enum Outcome {
    Emitted(curios_ersd::ErasedAtom),
    Diverged(curios_ersd::Terminator),
}

/// The erasure state: the checked builder constructing the module and the
/// environment mapping Core names to their operands.
#[derive(Default)]
pub(super) struct Lowering {
    pub(super) builder: curios_ersd::ErsdBuilder,
    pub(super) environment: Environment,
    /// Dropped binder labels referenced from a retained position. Consumed by
    /// the function-body collapse: a proof-valued body that dangles a binder
    /// its own lambda dropped is replaced by the unit constant.
    pub(super) dangled: BTreeSet<String>,
}

/// Erase a whole meta-free [`Module`] into a verified arena
/// [`ErasedModule`]. Top-level items are erased in dominance order as the
/// module's item chain; the entrypoint body becomes the entry block, checked
/// against `expected`.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn erase_module_to_ir(
    context: &mut Context,
    module: &Module,
    expected: &Term,
) -> Result<curios_ersd::ErasedModule, Error> {
    // Erasure runs with its own `Context`; seed the registries the re-derived
    // types consult before any item does.
    for (name, inductive) in &module.inductives {
        context.register_inductive(name, inductive.clone())?;
    }
    for (name, structure) in &module.structures {
        context.register_structure(name, structure.clone())?;
    }

    let mut lowering = Lowering::default();
    lowering.erase_items(context, module)?;

    // The entrypoint body runs under the root module (mirrors elaboration).
    context.set_island(Qualifier::empty());
    lowering.builder.open_block();
    let outcome = lowering.walk(context, &module.body, expected, None)?;
    let entry = lowering.seal(outcome);
    lowering.builder.set_entry(entry);

    // The verifier is the rejection point for the recursion classes the
    // language does not admit (a computed-only evaluation cycle); any other
    // failure here is an erasure bug, indistinguishable at this boundary.
    lowering
        .builder
        .finalize()
        .map_err(|error| Error::erased_module_invalid(error.to_string()))
}

impl Lowering {
    /// Seal the innermost open block: a computed value returns, a divergence
    /// keeps its own terminator.
    pub(super) fn seal(&mut self, outcome: Outcome) -> curios_ersd::BlockId {
        match outcome {
            Outcome::Emitted(atom) => self
                .builder
                .seal_block(curios_ersd::Terminator::Return(atom)),
            Outcome::Diverged(terminator) => self.builder.seal_block(terminator),
        }
    }

    /// Bind a compound right-hand side in the innermost open block (or as a
    /// top-level item) and hand back its result operand.
    pub(super) fn bind(&mut self, hint: Option<&str>, rhs: curios_ersd::Rhs) -> Outcome {
        let result = self.builder.let_value(hint.map(str::to_string), rhs);
        Outcome::Emitted(curios_ersd::ErasedAtom::Value(result))
    }

    /// The unit constant — the value of a retained-but-erased slot.
    pub(super) fn unit(&mut self) -> curios_ersd::ErasedAtom {
        curios_ersd::ErasedAtom::Constant(self.builder.constant(curios_ersd::Constant::Unit))
    }

    /// Erase one expression to an operand. `expected` is the type the
    /// expression was checked against, consumed where a runtime shape must be
    /// read off it; `hint` names the statement when this expression binds one.
    pub(super) fn walk(
        &mut self,
        context: &mut Context,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        // Attach this term's span to any error from erasing it, exactly like
        // the legacy wrapper.
        let result = self.walk_subterm(context, term, expected, hint);
        match term.span() {
            Some(span) => result.map_err(|error| error.at(span)),
            None => result,
        }
    }

    fn walk_subterm(
        &mut self,
        context: &mut Context,
        term: &Term,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        match &**term {
            Subterm::Prim(primitive) => prim::erase_prim(self, context, primitive, expected, hint),
            // Type formers carry nothing to lower; their value is the unit of
            // a retained-but-erased slot.
            Subterm::Type
            | Subterm::Prop
            | Subterm::FuncType(_)
            | Subterm::TupleType(_)
            | Subterm::InductiveType(_)
            | Subterm::StructType(_) => Ok(Outcome::Emitted(self.unit())),
            Subterm::Var(var) => {
                let name = var.unwrap();
                match self.environment.lookup(name) {
                    Some(Binding::Atom(atom)) => Ok(Outcome::Emitted(atom)),
                    Some(Binding::Dropped) => {
                        self.dangled.insert(name.to_string());
                        Ok(Outcome::Emitted(self.unit()))
                    }
                    None => unreachable!("erase_ir: unbound variable {name}"),
                }
            }
            Subterm::Let(binding) => self.erase_let(context, binding, expected, hint),
            Subterm::Match(m) => self.erase_match(context, m, hint),
            Subterm::Variant(variant) => self.erase_variant(context, variant, hint),
            Subterm::Struct(value) => self.erase_struct(context, value, hint),
            Subterm::Tuple(tuple) => self.erase_tuple(context, tuple, expected, hint),
            Subterm::Proj(proj) => self.erase_proj(context, proj, hint),
            Subterm::Func(func) => self.erase_func(context, func, expected, hint),
            Subterm::Apply(apply) => self.erase_apply(context, apply, hint),
            Subterm::Rec(rec) => self.erase_rec(context, rec, expected, hint),
            Subterm::RecMember(member) => self.erase_rec_member(context, member, expected, hint),
            // Erasure runs downstream of zonking and elaboration.
            Subterm::Metavar(_) => unreachable!("metavariable survived zonking into erase_ir"),
            Subterm::Infix(_) => unreachable!("infix node survived elaboration into erase_ir"),
            Subterm::NumLit(_) => {
                unreachable!("numeric-literal node survived elaboration into erase_ir")
            }
        }
    }

    /// Erase a let block binding for binding, in written order: each value is
    /// erased once (the operand law), defined in the Core context so dependent
    /// types reduce through it, and mapped to its operand; then the tail.
    fn erase_let(
        &mut self,
        context: &mut Context,
        binding: &Let,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        context.with_frame(|context| {
            let mut label_terms = Vec::<Term>::with_capacity(binding.bindings.len());

            for (index, (type_, value)) in binding.bindings.iter().enumerate() {
                let (type_, value) = {
                    let refs = label_terms.iter().collect::<Vec<_>>();
                    (type_.release(&refs), value.release(&refs))
                };

                // The arena identity uniquifies by index, so the hint stays
                // the clean source label; the `#`-uniquified fresh name is
                // only the Core context key.
                let label = binding.tail.label_iter().nth(index).flatten();
                let hint = label.map(str::to_string);
                let name = context.fresh(label);
                let outcome = self.walk(context, &value, &type_, hint.as_deref())?;
                let atom = emitted!(outcome);
                context.define_assuming(&name, &type_, &value);
                self.environment.bind(&name, atom);
                label_terms.push(Term::free_var(&name));
            }

            let tail = binding.tail.open(&label_terms.iter().collect::<Vec<_>>());
            self.walk(context, &tail, expected, hint)
        })
    }
}
