//! Recursive groups — local `rec` blocks, rec-member selections, and the
//! top-level recursive items.
//!
//! Members are classified syntactically: a member whose body is a lambda is a
//! function member (a reserved arena function), anything else is an eagerly
//! computed member (a value with an initializer block). Every member is
//! pre-bound before any body is erased, so mutual references resolve; source
//! order is preserved as the eager initialization order. An all-function
//! group erases to a `Functions` statement, a mixed or value-only group to a
//! recursive group — and the representation's verifier owns the rejection of
//! the recursion classes the language does not admit (a computed member
//! evaluating itself or a later member), so no diagnostic is re-derived here.

use super::{Context, Error, Lowering, Outcome, Rec, RecMember, Scope, Subterm, Term};

/// One classified member of a recursive group, pre-bound for mutual
/// visibility before any body is erased.
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
            .label_iter()
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
            .map(|(type_, body)| (type_.open(&name_refs), body.open(&name_refs)))
            .collect::<Vec<_>>();
        let tail = tail.open(&name_refs);

        context.with_frame(|context| {
            for (name, (type_, _)) in names.iter().zip(&members) {
                context.assume(name, type_);
                context.set_assumption_universe_context(name, group.universe_context().clone());
            }
            for (index, name) in names.iter().enumerate() {
                context.define(name, &Term::rec_member(group.clone(), index));
            }
            self.emit_group(context, &names, &hints, &members)?;
            self.walk(context, &tail, expected, hint)
        })
    }

    /// Erase a bare rec-member selection: the whole group is introduced, then
    /// the selected member's operand is the result.
    pub(super) fn erase_rec_member(
        &mut self,
        context: &mut Context,
        member: &RecMember,
        expected: &Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let rec = Rec {
            group: member.group.clone(),
            tail: Scope::constant(
                super::Many(member.group.len()),
                Term::var(super::Var::bound(member.index)),
            ),
        };
        self.erase_rec(context, &rec, expected, hint)
    }

    /// Erase a top-level recursive item. Bindings persist in the base frame
    /// (no scoping frame), exactly like `let` items.
    pub(super) fn erase_rec_item(
        &mut self,
        context: &mut Context,
        rec: &super::RecItem,
    ) -> Result<(), Error> {
        let definitions = rec.definitions();
        for definition in &definitions {
            context.assume(&definition.name, &definition.type_);
            context.set_assumption_universe_context(
                &definition.name,
                rec.group.universe_context().clone(),
            );
        }
        for (index, definition) in definitions.iter().enumerate() {
            context.define(
                &definition.name,
                &Term::rec_member(rec.group.clone(), index),
            );
        }

        let names = definitions
            .iter()
            .map(|definition| definition.name.clone())
            .collect::<Vec<_>>();
        let hints = names
            .iter()
            .map(|name| Some(name.clone()))
            .collect::<Vec<_>>();
        let members = definitions
            .iter()
            .map(|definition| (definition.type_.clone(), definition.body.clone()))
            .collect::<Vec<_>>();
        self.emit_group(context, &names, &hints, &members)
    }

    /// Pre-bind every member, erase function bodies and computed
    /// initializers, and emit the group statement (`Functions` when every
    /// member is a function, a recursive group otherwise). Statement emission
    /// falls through to the top-level item list when no block is open.
    fn emit_group(
        &mut self,
        context: &mut Context,
        names: &[String],
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
                        unreachable!("erase_ir: a function member's body is a lambda")
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
