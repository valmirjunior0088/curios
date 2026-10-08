//! Which binders of a published item bind a proof, stated on the binder, and which of the names it declares is one, stated on its definition.
//!
//! No judgment reads a proof's value, so the closed machine leaves one as it is written ([`is_proof_in`]). It needs to know which where it substitutes, with a value and a closed body in hand and no type, so the answer is taken once, here, over an item's final terms as the item is published, and kept on each binder ([`curios_core::Scope::binds_proof`]). A binder is asked at its declared type with the binders before it opaque, and a `true` there holds at every instantiation, so no reading of the binder computes what it binds. A binder a substitution makes a proof — a `Type` variable instantiated at a proposition — is left unmarked, which costs a reduction and changes no reduct. A binder at a type is unmarked too: a type is evaluated before it is bound, since conversion and the levels it settles read the reduct they are handed.
//!
//! **It decides no runtime shape and no judgment.** A function has a parameter for each binder whatever a binder states, a declaration's stored fields are classified by erasure against its signature, and a term bound unreduced is the same term to conversion. `curios-core`'s marking walk states what is asked and what is crossed; this module is the checker's half of it.
//!
//! **A name the item declares is the one binder its terms do not hold.** It is asked here with them, at its declared type, and the answer is kept on its definition ([`curios_core::Definition::proof`]): a proof a declaration wrote is bound, from its publication on, to a body no judgment unfolds it to. That mark does decide a judgment, which a binder's never does, since a name that does not unfold is a constant to conversion. It refuses nothing a body unfolded would admit, because conversion holds two proofs of one proposition equal before it reduces either.

use {
    crate::{Context, Error, Published, is_proof_in},
    curios_core::{Advance, Bound, DefinitionKind, Free, Item, ProofBinders, Telescope, Term},
};

/// The marking walk's questions, answered by the engine's rule under the binders the walk has entered.
struct Asking<'a, 's> {
    context: &'a mut Context<'s>,
    /// The binders entered at a type, each with that type: what the rule reads a bound variable's sort off.
    opened: Vec<(Free, Term)>,
    /// Whether each binder entered came with a type, innermost last, so leaving one takes its entry out of `opened` only where it put one in.
    typed: Vec<bool>,
}

impl ProofBinders for Asking<'_, '_> {
    type Refusal = Error;

    fn proves(&mut self, type_: &Term) -> Result<bool, Error> {
        is_proof_in(self.context, &mut self.opened, type_)
    }

    fn enter(&mut self, hint: Option<&str>, type_: Option<&Term>) -> Free {
        let binder = self.context.fresh(hint);
        self.typed.push(type_.is_some());
        if let Some(type_) = type_ {
            self.opened.push((binder, type_.clone()));
        }

        binder
    }

    fn leave(&mut self, count: usize) {
        for _ in 0..count {
            if self.typed.pop() == Some(true) {
                self.opened.pop();
            }
        }
    }
}

/// `term`, a scope's body with `binders[k]` standing for its loose index `k`, with every binder a function literal or a `let` binds in it stating whether it binds a proof.
fn stated(context: &mut Context, term: &Term, binders: &[Term]) -> Result<Term, Error> {
    term.stating_proofs_under(
        &mut Asking {
            context,
            opened: Vec::new(),
            typed: Vec::new(),
        },
        binders,
    )
}

/// `telescope` with each binder stating whether it binds a proof, each asked at its type under the binders before it, opaque.
fn stated_telescope<B: Bound>(
    context: &mut Context,
    telescope: &Telescope<B>,
) -> Result<Telescope<B>, Error> {
    let mut proofs = Vec::new();
    let mut opened = Vec::new();
    let mut cursor = telescope.cursor();
    while let Some((_, type_)) = cursor.entry() {
        proofs.push(is_proof_in(context, &mut opened, &type_)?);
        opened.push((cursor.advance_fresh(|hint| context.fresh(hint)), type_));
    }

    Ok(telescope.clone().with_proofs(&proofs))
}

/// Whether the definition `kind` introduced at `type_` is a proof a declaration wrote ([`curios_core::Definition::proof`]). A generated definition is left what it unfolds to: a constructor of a proposition and a law's projection are terms the compiler writes, one node deep.
fn proves(context: &mut Context, kind: &DefinitionKind, type_: &Term) -> Result<bool, Error> {
    Ok(matches!(kind, DefinitionKind::Authored) && is_proof_in(context, &mut Vec::new(), type_)?)
}

/// State, on every binder of `published`, whether it binds a proof: each function literal's and each `let`'s in the item's terms, each constructor's payloads in the registry entries the item declares, which an arm then binds under, and each name the item declares, on its definition.
///
/// **Before the item is published**, so no declaration reads a binder that has not said. A recursive item's members are walked under the names they are published as, their bodies closed throughout, as [`curios_core::RecGroup::map_members`] rewrites them.
///
/// The questions spend a budget restored for them, as every judgment of an item's final terms does.
pub(crate) fn state_proof_binders(
    context: &mut Context,
    published: &mut Published,
) -> Result<(), Error> {
    context.restore_budget();

    match &mut published.item {
        Item::Let(definition) => {
            definition.type_ = stated(context, &definition.type_, &[])?;
            definition.body = stated(context, &definition.body, &[])?;
            definition.proof = proves(context, &definition.kind, &definition.type_)?;
        }
        Item::Rec(rec) => {
            let members = rec
                .definitions
                .iter()
                .map(|definition| Term::free_var(&Free::from(&definition.name)))
                .collect::<Vec<_>>();
            let mut refusal = None;
            let group = rec.group.map_members(|body| {
                stated(context, body, &members).unwrap_or_else(|error| {
                    refusal.get_or_insert(error);
                    body.clone()
                })
            });
            if let Some(error) = refusal {
                return Err(error);
            }
            rec.group = group;

            let names = members.iter().collect::<Vec<_>>();
            for (definition, member) in rec.definitions.iter_mut().zip(rec.group.iter()) {
                definition.proof = proves(context, &definition.kind, &member.type_.open(&names))?;
            }
        }
    }

    for (_, declaration) in &mut published.induct_decls {
        for (_, constructor) in &mut declaration.constructors {
            constructor.telescope = stated_telescope(context, &constructor.telescope)?;
        }
    }

    Ok(())
}
