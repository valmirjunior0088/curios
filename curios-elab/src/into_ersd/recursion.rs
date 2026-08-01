//! Recursive groups — local `rec` blocks, the member selections that are one more `rec` block apiece, and the top-level recursive items.
//!
//! Members are classified syntactically: a member whose body is a lambda is a function member (a reserved arena function), anything else is an eagerly computed member (a value with an initializer block). Every member is pre-bound before any body is erased, so mutual references resolve; source order is preserved as the eager initialization order. An all-function group erases to a `Functions` statement, a mixed or value-only group to a recursive group — and the representation's verifier owns the rejection of the recursion classes the language does not admit (a computed member evaluating itself or a later member), so no diagnostic is re-derived here.

use {
    super::{Context, Error, Lowering, Outcome, Rec, Subterm, Term},
    curios_core::Free,
};

/// One classified member of a recursive group, pre-bound for mutual visibility before any body is erased.
enum Member {
    Function(curios_ersd::FunctionId),
    Computed(curios_ersd::ValueId),
}

impl Lowering {
    /// Erase a local `rec` block, then continue with its tail.
    pub(super) fn erase_rec(
        &mut self,
        context: &mut Context,
        rec: &Rec,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let Rec { group, tail } = rec;

        let hints = tail
            .hint_iter()
            .map(|label| label.map(str::to_string))
            .collect::<Vec<_>>();
        let names = hints
            .iter()
            .map(|label| context.fresh(label.as_deref()))
            .collect::<Vec<_>>();
        let name_terms = names.iter().map(Term::free_var).collect::<Vec<_>>();
        let name_refs = name_terms.iter().collect::<Vec<_>>();

        let members = group
            .iter()
            .map(|member| (member.type_.open(&name_refs), member.body.open(&name_refs)))
            .collect::<Vec<_>>();
        let tail = tail.open(&name_refs);

        context.with_frame(|context| {
            for (name, (type_, _)) in names.iter().zip(&members) {
                context.assume(name, type_);
                context.set_assumption_universe_context(name, group.universe_context().clone());
            }
            for (index, name) in names.iter().enumerate() {
                context.define(name, &Term::rec_proj(group.clone(), index), None);
            }
            self.emit_group(context, &names, &hints, &members)?;
            self.walk(context, &tail, expected, hint)
        })
    }

    /// Erase a top-level recursive item. Bindings persist in the base frame (no scoping frame), exactly like `let` items.
    pub(super) fn erase_rec_item(
        &mut self,
        context: &mut Context,
        rec: &super::RecItem,
    ) -> Result<(), Error> {
        let definitions = rec.definitions();
        for definition in &definitions {
            let name = Free::from(&definition.name);
            context.assume(&name, &definition.type_);
            context.set_assumption_universe_context(&name, rec.group.universe_context().clone());
        }
        for (index, definition) in definitions.iter().enumerate() {
            context.define(
                &Free::from(&definition.name),
                &Term::rec_proj(rec.group.clone(), index),
                Some(&definition.kind),
            );
        }

        let names = definitions
            .iter()
            .map(|definition| Free::from(&definition.name))
            .collect::<Vec<_>>();
        let hints = definitions
            .iter()
            .map(|definition| Some(definition.name.symbol()))
            .collect::<Vec<_>>();
        let members = definitions
            .iter()
            .map(|definition| (definition.type_.clone(), definition.body.clone()))
            .collect::<Vec<_>>();
        self.emit_group(context, &names, &hints, &members)
    }

    /// Pre-bind every member, erase function bodies and computed initializers, and emit the group statement (`Functions` when every member is a function, a recursive group otherwise). Statement emission falls through to the top-level item list when no block is open.
    fn emit_group(
        &mut self,
        context: &mut Context,
        names: &[Free],
        hints: &[Option<String>],
        members: &[(Term, Term)],
    ) -> Result<(), Error> {
        let classified = names
            .iter()
            .zip(hints)
            .zip(members)
            .map(|((name, hint), (_, body))| {
                let member = match &**body {
                    Subterm::Func(_) => Member::Function(self.builder.reserve_function()),
                    _ => Member::Computed(self.builder.value(hint.clone())),
                };
                let atom = match &member {
                    Member::Function(function) => curios_ersd::Atom::Function(*function),
                    Member::Computed(value) => curios_ersd::Atom::Value(*value),
                };
                self.environment.bind(name, atom);
                member
            })
            .collect::<Vec<_>>();

        let mut functions = Vec::new();
        let mut values = Vec::new();
        for (member, ((type_, body), hint)) in classified.iter().zip(members.iter().zip(hints)) {
            match member {
                Member::Function(function) => {
                    let Subterm::Func(func) = &**body else {
                        unreachable!("erase: a function member's body is a lambda")
                    };
                    self.define_lambda(context, *function, func, type_, hint.as_deref())?;
                    functions.push(*function);
                }
                Member::Computed(value) => {
                    self.builder.open_block();
                    let outcome = self.walk(context, body, type_, None)?;
                    let init = self.seal(outcome);
                    values.push((*value, init));
                }
            }
        }

        if values.is_empty() {
            self.builder.let_functions(functions);
        } else {
            let group = self.builder.rec_group(functions, values);
            self.builder.let_rec(group);
        }
        Ok(())
    }
}
