//! Recursive groups — local `rec` blocks, the member selections that are one more `rec` block apiece, and the top-level recursive items.
//!
//! Members are classified syntactically: a member whose body is a lambda is a function member (a reserved arena function), anything else is an eagerly computed member (a value with an initializer block). Every member is pre-bound before any body is erased, so mutual references resolve; a computed member is forced by need downstream, so source order is spelling and nothing more. An all-function group erases to a `Functions` statement, a mixed or value-only group to a recursive group — and the representation's verifier owns the rejection of the recursion classes the language does not admit (a computed member evaluating itself, directly or through the functions it applies, and an initializer performing an effect), so no diagnostic is re-derived here.

use {
    super::{Lowering, Outcome},
    crate::Error,
    curios_cert::Reader,
};

/// One classified member of a recursive group, pre-bound for mutual visibility before any body is erased.
enum Member {
    Function(curios_anf::FunctionId),
    Computed(curios_anf::ValueId),
}

impl Lowering<'_> {
    /// Erase a local `rec` block, then continue with its tail.
    pub fn erase_rec(
        &mut self,
        context: &mut Reader,
        rec: &curios_core::Rec,
        expected: &curios_core::Term,
        hint: Option<&str>,
    ) -> Result<Outcome, Error> {
        let curios_core::Rec { group, tail } = rec;

        let hints = tail
            .hint_iter()
            .map(|label| label.map(str::to_string))
            .collect::<Vec<_>>();
        let names = hints
            .iter()
            .map(|label| context.fresh(label.as_deref()))
            .collect::<Vec<_>>();
        let name_terms = names
            .iter()
            .map(curios_core::Term::free_var)
            .collect::<Vec<_>>();
        let name_refs = name_terms.iter().collect::<Vec<_>>();

        let members = group
            .iter()
            .map(|member| (member.type_.open(&name_refs), member.body.open(&name_refs)))
            .collect::<Vec<_>>();
        let tail = tail.open(&name_refs);

        // Each member bound at its type to its selection, as the kernel's own group tail binds it.
        context.scoped(|context| {
            for (index, (name, (type_, _))) in names.iter().zip(&members).enumerate() {
                context.bind(
                    name,
                    type_,
                    &curios_core::Term::rec_proj(group.clone(), index),
                );
            }
            self.emit_group(context, &names, &hints, &members)?;
            self.walk(context, &tail, expected, hint)
        })
    }

    /// Erase a top-level recursive item as the fragment's item. Its members are defined in the kernel's environment already, as every item's names are, each at the group's universes: a member names another at the group's own levels and at the sorts this form is read at, which is the occurrence the kernel reads, and the form's own name for it.
    pub fn erase_rec_item(
        &mut self,
        context: &mut Reader,
        rec: &curios_core::RecItem,
    ) -> Result<(), Error> {
        let universes = curios_core::Universes {
            levels: (0..rec.group.universe_context().parameter_count)
                .map(|param| curios_core::Level::param(curios_core::UniverseParam(param)))
                .collect(),
            qualities: self.own.1.qualities(),
        };
        let names = rec
            .definitions
            .iter()
            .map(|definition| curios_core::Free::from(&definition.name))
            .collect::<Vec<_>>();
        let occurrences = names
            .iter()
            .map(|name| curios_core::Term::instance_of(name, universes.clone()))
            .collect::<Vec<_>>();
        let occurrences = occurrences.iter().collect::<Vec<_>>();
        let hints = rec
            .definitions
            .iter()
            .map(|definition| Some(definition.name.faithful().to_string()))
            .collect::<Vec<_>>();
        let members = rec
            .group
            .iter()
            .map(|member| {
                (
                    member.type_.open(&occurrences),
                    member.body.open(&occurrences),
                )
            })
            .collect::<Vec<_>>();
        self.emit_group(context, &names, &hints, &members)
    }

    /// Pre-bind every member, erase function bodies and computed initializers, and emit the group statement (`Functions` when every member is a function, a recursive group otherwise). Statement emission falls through to the top-level item list when no block is open.
    fn emit_group(
        &mut self,
        context: &mut Reader,
        names: &[curios_core::Free],
        hints: &[Option<String>],
        members: &[(curios_core::Term, curios_core::Term)],
    ) -> Result<(), Error> {
        let classified = names
            .iter()
            .zip(hints)
            .zip(members)
            .map(|((name, hint), (_, body))| {
                let member = match &**body {
                    curios_core::Subterm::Func(_) => {
                        let function = self.builder.reserve_function();
                        self.names.functions.insert(function, *name);
                        Member::Function(function)
                    }
                    _ => {
                        let value = self.builder.value(hint.clone());
                        self.names.values.insert(value, *name);
                        // Where the verifier's refusal of this member is framed, should it refuse it: the initializer, since no extent of the binder reaches this far.
                        if let Some(extent) = body.extent() {
                            self.extents.insert(value, extent);
                        }
                        Member::Computed(value)
                    }
                };
                let atom = match &member {
                    Member::Function(function) => curios_anf::Atom::Function(*function),
                    Member::Computed(value) => curios_anf::Atom::Value(*value),
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
                    let curios_core::Subterm::Func(func) = &**body else {
                        unreachable!("erase: a function member's body is a lambda")
                    };
                    // A member the group named keeps that name; one it did not takes its owner's, the same way an argument-position lambda does.
                    match hint.clone().or_else(|| self.derived_hint()) {
                        Some(name) => {
                            let named = name.clone();
                            self.with_owner(name, |lowering| {
                                lowering.define_lambda(
                                    context,
                                    *function,
                                    func,
                                    type_,
                                    Some(&named),
                                )
                            })?;
                        }
                        None => self.define_lambda(context, *function, func, type_, None)?,
                    }
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
