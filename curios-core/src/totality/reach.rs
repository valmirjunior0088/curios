//! What the erased half of a program reaches.
//!
//! Erasure deletes two things, so there are two obligations, and they share one
//! closure over exactly one relation — "this definition mentions that one" —
//! seeded twice.
//!
//! **(T)** is seeded from every type position, and takes the *aggressive*
//! reading: everything a type reaches must be total, not merely the values a
//! type-level eliminator scrutinizes. The narrow reading is unsound, because a
//! *total* type-level function applied to a *partial value* reties the negative
//! knot that strict positivity exists to forbid.
//!
//! **(V)** is seeded from every position whose declared type is `Prop`-sorted.
//!
//! Neither obligation subsumes the other. The type-level `rec` route violates
//! only (T); the partial-carrier and `exit` routes violate only (V).
//!
//! The closure follows a definition into its **body**, not only its type. That
//! is easy to overlook and load-bearing: `/std/BigNat/Canonical` is a
//! proposition whose body eliminates `is_trimmed`, and no type annotation
//! anywhere records that dependency.
//!
//! At the replay site the module in hand is the user suffix alone, so a prelude
//! definition is a sink of this relation. That is sound for the same reason it
//! is sound for positivity — prelude items cannot mention user code — and it is
//! why partiality persists on the definition rather than being recomputed.

use {
    crate::{
        Apply, Bound, Carrier, Cases, Context, Definition, Error, Free, Func, FuncType, Global,
        Item, Let, Many, Match, Module, Rec, Scope, Struct, Subterm, Telescope, Term, Three,
        TupleType, Two, Variant, is_prop, is_prop_in, synth_neutral,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A term the erased half of the program must be total in, and what a
/// diagnostic should call it.
///
/// The obligations are stated over *terms*, not over the names those terms
/// mention, because a `rec` written inline in an erased position mentions no
/// name at all. Reporting the reached definitions alone would let
/// `rec Bad : Type = Sink(Bad)`, written as a local binding, satisfy every
/// closure while retying exactly the knot the closure exists to forbid.
pub(crate) struct Position {
    pub(crate) term: Term,
    pub(crate) site: String,
}

fn push(positions: &mut Vec<Position>, site: &str, term: &Term) {
    positions.push(Position {
        term: term.clone(),
        site: site.to_string(),
    });
}

/// Every term in a type position.
///
/// "Type position" is read syntactically and generously: a declared type, a
/// binder annotation, a match motive, a declaration telescope, a nominal type
/// former, a primitive type former, and the *body* of any definition whose own
/// type ends in a sort — the last being what reaches through
/// `/std/BigNat/Canonical` into `is_trimmed`.
pub(crate) fn type_positions(module: &Module) -> Vec<Position> {
    let mut positions = Vec::new();

    for definition in definitions(module) {
        let name = definition.name.to_string();
        push(
            &mut positions,
            &format!("the type of '{name}'"),
            &definition.type_,
        );
        if ends_in_sort(&definition.type_) {
            push(
                &mut positions,
                &format!("the body of '{name}'"),
                &definition.body,
            );
        }
        annotations(&definition.body, &format!("'{name}'"), &mut positions);
        annotations(
            &definition.type_,
            &format!("the type of '{name}'"),
            &mut positions,
        );
    }

    // The entrypoint expression and its annotation are not items, so nothing
    // above reaches them. An exploit needs only a local `rec` and one
    // construction, both of which fit in the trailing expression.
    if let Some(type_) = &module.type_ {
        push(&mut positions, "the entrypoint's type", type_);
        annotations(type_, "the entrypoint's type", &mut positions);
    }
    annotations(&module.body, "the entrypoint", &mut positions);

    // A declaration's telescopes are types by construction, and its parameter
    // and field types can name anything.
    for (name, declaration) in &module.induct_decls {
        let site = format!("a parameter of '{name}'");
        entries(&declaration.params, &site, &mut positions);
        for (tag, constructor) in &declaration.constructors {
            let site = format!("the payload of '{name}/{tag}'");
            entries(&constructor.telescope, &site, &mut positions);
        }
    }
    for (name, declaration) in &module.struct_decls {
        let site = format!("a parameter of '{name}'");
        entries(&declaration.params, &site, &mut positions);
        let site = format!("a field of '{name}'");
        entries(&declaration.fields, &site, &mut positions);
    }

    positions
}

/// The definitions these positions name — the seeds of the reachability walk.
pub(crate) fn seeds(positions: &[Position]) -> BTreeSet<Global> {
    let mut seeds = BTreeSet::new();
    for position in positions {
        mark(&position.term, &mut seeds);
    }
    seeds
}

/// Every definition named by a term checked against a `Prop`-sorted type.
///
/// Four positions carry proofs across a boundary the closure would otherwise
/// not cross: a definition that *is* a proof, a structure field declared at a
/// proposition — the certificate idiom, `Str { bytes, valid }` — a constructor
/// payload declared at one, and an argument at a `Prop`-declared parameter.
///
/// The last of those is why an inline proof inside an ordinary function is
/// seen at all. A proof inside a *proof-valued* definition needs no rule,
/// because seeding that definition takes its whole body; the same proof handed
/// to `consume(x, p)` inside a `Nat`-valued function has no such umbrella.
pub(crate) fn proof_positions(
    context: &mut Context,
    module: &Module,
) -> Result<Vec<Position>, Error> {
    let mut positions = Vec::new();

    for definition in definitions(module) {
        let name = definition.name.to_string();
        if is_prop(context, &definition.type_)? {
            push(
                &mut positions,
                &format!("the proof '{name}'"),
                &definition.body,
            );
        }
        positions.extend(certificates(
            context,
            module,
            &definition.body,
            &format!("a certificate in '{name}'"),
        )?);
    }

    // The entrypoint, for the same reason type positions cover it.
    if let Some(type_) = &module.type_
        && is_prop(context, type_)?
    {
        push(&mut positions, "the entrypoint", &module.body);
    }
    positions.extend(certificates(
        context,
        module,
        &module.body,
        "a certificate in the entrypoint",
    )?);

    Ok(positions)
}

/// Run the seeding walk over one term.
fn certificates(
    context: &mut Context,
    module: &Module,
    term: &Term,
    site: &str,
) -> Result<Vec<Position>, Error> {
    let mut walk = Certificates {
        context,
        module,
        site: site.to_string(),
        opened: Vec::new(),
        positions: Vec::new(),
    };
    walk.walk(term)?;

    Ok(walk.positions)
}

/// Close `seeds` over the definitions they name, following each into both its
/// type and its body.
pub(crate) fn reachable(module: &Module, seeds: BTreeSet<Global>) -> BTreeSet<Global> {
    let bodies = definitions(module)
        .map(|definition| {
            let mut named = definition.type_.free_vars();
            named.extend(definition.body.free_vars());
            let named = named
                .iter()
                .filter_map(|free| free.as_global().cloned())
                .collect::<BTreeSet<_>>();
            (definition.name.clone(), named)
        })
        .collect::<BTreeMap<_, _>>();

    let mut reached = seeds;
    let mut frontier = reached.iter().cloned().collect::<Vec<_>>();
    while let Some(name) = frontier.pop() {
        let Some(named) = bodies.get(&name) else {
            // Outside the module under analysis: a replayed prelude
            // definition, whose own closure was settled when the archive was
            // built.
            continue;
        };
        for next in named {
            if reached.insert(next.clone()) {
                frontier.push(next.clone());
            }
        }
    }

    reached
}

/// Every top-level definition in the module, `let` and `rec` alike.
fn definitions(module: &Module) -> impl Iterator<Item = Definition> + '_ {
    module.items.iter().flat_map(|item| match item {
        Item::Let(definition) => vec![definition.clone()],
        Item::Rec(rec) => rec.definitions(),
    })
}

/// Record every definition this term names.
fn mark(term: &Term, seeds: &mut BTreeSet<Global>) {
    seeds.extend(
        term.free_vars()
            .iter()
            .filter_map(|free| free.as_global())
            .cloned(),
    );
}

/// Take every entry of a telescope.
fn entries<B: Bound>(telescope: &Telescope<B>, site: &str, positions: &mut Vec<Position>) {
    let mut telescope = telescope;
    while let Telescope::Cons(entry, rest) = telescope {
        push(positions, site, entry);
        telescope = rest.body();
    }
}

/// Whether this type's terminal, after peeling every arrow, is a sort — the
/// test for "this definition denotes a type or a proposition".
fn ends_in_sort(type_: &Term) -> bool {
    let mut current = type_.clone();
    loop {
        let next = match &*current {
            Subterm::Type(_) | Subterm::Prop => return true,
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut telescope = telescope;
                loop {
                    match telescope {
                        Telescope::Done(inner) => break (**inner).clone(),
                        Telescope::Cons(_, rest) => telescope = rest.body(),
                    }
                }
            }
            _ => return false,
        };
        current = next;
    }
}

/// Walk a term and mark every type written inside it: binder annotations,
/// match motives, `let` and `rec` declared types, nominal and primitive type
/// formers.
fn annotations(term: &Term, site: &str, positions: &mut Vec<Position>) {
    match &**term {
        // A type former stands for its whole self; nothing inside it is a
        // value position the aggressive reading would treat differently.
        Subterm::FuncType(_)
        | Subterm::TupleType(_)
        | Subterm::InductType(_)
        | Subterm::StructType(_) => {
            push(positions, site, term);
            return;
        }

        Subterm::Func(super::super::Func { telescope, .. }) => {
            entries(telescope, site, positions);
        }

        Subterm::Match(super::super::Match { motive, .. }) => {
            push(positions, site, motive.body());
        }

        Subterm::Let(super::super::Let { bindings, .. }) => {
            for binding in bindings {
                push(positions, site, binding.type_());
            }
        }

        Subterm::Rec(super::super::Rec { group, .. })
        | Subterm::RecMember(super::super::RecMember { group, .. }) => {
            for member in group.iter() {
                push(positions, site, member.type_.body());
            }
        }

        // A nominal construction's parameters are its type arguments.
        Subterm::Variant(Variant { params, .. }) | Subterm::Struct(Struct { params, .. }) => {
            for param in params {
                push(positions, site, param);
            }
        }

        // `Prim::Exit`'s first operand is the type it inhabits; the type
        // formers name their element types.
        Subterm::Prim(
            super::super::Prim::Exit(type_, _)
            | super::super::Prim::LstType(type_)
            | super::super::Prim::CellType(type_),
        ) => push(positions, site, type_),

        _ => {}
    }

    let subterm: &Subterm = term;
    subterm.any_child_term(&mut |child| {
        annotations(child, site, positions);
        false
    });
}

/// The (V) seeding walk.
///
/// Descends the whole term, opening every binder against a fresh name so that
/// nothing it inspects carries a loose de Bruijn index. That is what an earlier
/// version could not do: it descended with `any_child_term`, which hands back a
/// scope body unopened, and guarded the resulting positions with `reach() == 0`
/// — which *skipped* every certificate built inside a function body.
///
/// The opened binders are threaded in `opened` rather than assumed into the
/// context, because `Context::assume` bumps the stamp that validates the
/// memoization caches; see [`is_prop_in`]. Binders whose type this walk
/// cannot name are opened but not recorded, which costs a seed rather than
/// risking a wrong one: an unresolvable name makes sort synthesis answer
/// conservatively.
struct Certificates<'a> {
    context: &'a mut Context,
    module: &'a Module,
    site: String,
    opened: Vec<(Free, Term)>,
    positions: Vec<Position>,
}

impl Certificates<'_> {
    /// Seed this node, then its children.
    fn walk(&mut self, term: &Term) -> Result<(), Error> {
        self.seed(term)?;

        match &**term {
            // Binder-bearing forms open as they descend.
            Subterm::Func(Func { telescope, .. })
            | Subterm::FuncType(FuncType { telescope, .. }) => {
                self.telescope_terms(telescope.clone())?;
            }

            Subterm::TupleType(TupleType { telescope }) => {
                self.telescope_units(telescope.clone())?;
            }

            Subterm::Let(Let { bindings, tail }) => {
                for binding in bindings {
                    self.walk(binding.type_())?;
                    self.walk(binding.value())?;
                }
                let body = self.open_many(tail);
                self.walk(&body)?;
            }

            Subterm::Rec(Rec { group, tail }) => {
                for index in 0..group.len() {
                    let body = group.member_body(index);
                    self.walk(&body)?;
                }
                let body = self.open_many(tail);
                self.walk(&body)?;
            }

            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                self.walk(head)?;
                let body = self.open_many(motive);
                self.walk(&body)?;
                self.arms(cases)?;
            }

            // Everything else binds nothing, so its children are already
            // closed to the same degree this node is.
            _ => {
                let subterm: &Subterm = term;
                let mut failure = None;
                subterm.any_child_term(&mut |child| match self.walk(child) {
                    Ok(()) => false,
                    Err(error) => {
                        failure = Some(error);
                        true
                    }
                });
                if let Some(error) = failure {
                    return Err(error);
                }
            }
        }

        Ok(())
    }

    /// Every proof this node hands to a `Prop`-declared position.
    ///
    /// **Known gap.** A node still carrying a loose index is skipped, because
    /// sort synthesis assumes free occurrences and asserts on a bound one. The
    /// walk opens every binder it descends through, so the only remaining
    /// source is a `Let`: its bindings are a flat vector under one tail scope,
    /// and a later binding is closed over the earlier binders, so walking them
    /// side by side hands out loose indices. Opening them one at a time is
    /// what closes this, and until it does a certificate built inside a
    /// `let`-bound value is not seeded.
    fn seed(&mut self, term: &Term) -> Result<(), Error> {
        if term.reach() != 0 {
            return Ok(());
        }

        match &**term {
            Subterm::Struct(Struct {
                name,
                params,
                fields,
                ..
            }) => {
                if let Some(declaration) = self.module.struct_decls.get(name) {
                    let declared = declaration.fields.clone().open_params(params);
                    self.prop_positions(declared, fields)?;
                }
            }

            Subterm::Variant(Variant {
                name,
                params,
                tag,
                payload,
                ..
            }) => {
                let constructor = self.module.induct_decls.get(name).and_then(|declaration| {
                    declaration
                        .constructors
                        .iter()
                        .find(|(candidate, _)| candidate == tag)
                        .map(|(_, constructor)| constructor.clone())
                });
                if let Some(constructor) = constructor {
                    let declared = constructor.telescope.open_params(params);
                    self.prop_positions(declared, payload)?;
                }
            }

            // An argument at a `Prop`-declared parameter. The head's type comes
            // from `synth_neutral`, the judgment `infer` itself is built from,
            // so a local, a global, a curried spine, and a projection are all
            // covered by one rule rather than by a lookup that would only find
            // globals. It answers `None` rather than guessing, and a `None`
            // costs a seed instead of risking a wrong one.
            Subterm::Apply(Apply { head, params, .. }) => {
                let synthesized = synth_neutral(self.context, &self.opened, head)
                    .map_err(|error| error.into_error(|| Error::reduce_exhausted(head.clone())))?;
                if let Some(type_) = synthesized
                    && let Subterm::FuncType(FuncType { telescope, .. }) = &*type_
                {
                    self.prop_positions(telescope.clone(), params)?;
                }
            }

            _ => {}
        }

        Ok(())
    }

    /// Mark the actual terms sitting at the `Prop`-sorted entries of a declared
    /// telescope.
    fn prop_positions<B: Bound>(
        &mut self,
        declared: Telescope<B>,
        actuals: &[Term],
    ) -> Result<(), Error> {
        let mut declared = declared;
        let mut index = 0;
        while let Telescope::Cons(entry, rest) = declared {
            let Some(actual) = actuals.get(index) else {
                return Ok(());
            };
            if is_prop_in(self.context, &mut self.opened, &entry)? {
                push(&mut self.positions, &self.site, actual);
            }
            declared = rest.open(&[actual]);
            index += 1;
        }
        Ok(())
    }

    fn arms(&mut self, cases: &Cases) -> Result<(), Error> {
        match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => {
                self.walk(false_case)?;
                self.walk(true_case)?;
            }
            Cases::Switch { cases, default } => {
                for body in cases.values() {
                    self.walk(body)?;
                }
                self.walk(default)?;
            }
            Cases::Induct { cases, default } => {
                for (_, arm) in cases {
                    let body = self.open_many(&arm.body);
                    self.walk(&body)?;
                }
                if let Some(default) = default {
                    self.walk(default)?;
                }
            }
            Cases::FreeMonoid { carrier } => match carrier {
                Carrier::Nat {
                    empty_case,
                    cons_case,
                } => {
                    self.walk(empty_case)?;
                    let body = self.open_two(cons_case);
                    self.walk(&body)?;
                }
                Carrier::Bin {
                    empty_case,
                    cons_case,
                    ..
                } => {
                    self.walk(empty_case)?;
                    let body = self.open_three(cons_case);
                    self.walk(&body)?;
                }
                Carrier::Lst {
                    elem,
                    empty_case,
                    cons_case,
                } => {
                    self.walk(elem)?;
                    self.walk(empty_case)?;
                    let body = self.open_three(cons_case);
                    self.walk(&body)?;
                }
            },
        }
        Ok(())
    }

    /// Fresh binders for a scope, in order.
    ///
    /// Their types are deliberately not recorded: an arm binder's type lives in
    /// the constructor telescope rather than the scope, and a binder this walk
    /// cannot name is one sort synthesis answers conservatively about — which
    /// costs a seed rather than risking a wrong one.
    fn fresh_binders(&mut self, arity: usize, hints: &[Option<&str>]) -> Vec<Term> {
        (0..arity)
            .map(|index| {
                let binder = self.context.fresh(hints.get(index).copied().flatten());
                Term::free_var(&binder)
            })
            .collect()
    }

    fn open_many(&mut self, scope: &Scope<Many>) -> Term {
        let hints = scope.hint_iter().collect::<Vec<_>>();
        let terms = self.fresh_binders(scope.arity(), &hints);
        scope.open(&terms.iter().collect::<Vec<_>>())
    }

    fn open_two(&mut self, scope: &Scope<Two>) -> Term {
        let hints = scope.hint_iter().collect::<Vec<_>>();
        let terms = self.fresh_binders(Two::ARITY, &hints);
        scope.open(&[&terms[0], &terms[1]])
    }

    fn open_three(&mut self, scope: &Scope<Three>) -> Term {
        let hints = scope.hint_iter().collect::<Vec<_>>();
        let terms = self.fresh_binders(Three::ARITY, &hints);
        scope.open(&[&terms[0], &terms[1], &terms[2]])
    }

    fn telescope_terms(&mut self, telescope: Telescope<Term>) -> Result<(), Error> {
        let mut telescope = telescope;
        loop {
            match telescope {
                Telescope::Done(terminal) => return self.walk(&terminal),
                Telescope::Cons(entry, rest) => {
                    self.walk(&entry)?;
                    let binder = self.context.fresh(rest.first_hint());
                    self.opened.push((binder.clone(), entry));
                    telescope = rest.open(&[&Term::free_var(&binder)]);
                }
            }
        }
    }

    fn telescope_units(&mut self, telescope: Telescope<()>) -> Result<(), Error> {
        let mut telescope = telescope;
        while let Telescope::Cons(entry, rest) = telescope {
            self.walk(&entry)?;
            let binder = self.context.fresh(rest.first_hint());
            self.opened.push((binder.clone(), entry));
            telescope = rest.open(&[&Term::free_var(&binder)]);
        }
        Ok(())
    }
}

/// One reachability answer: which partial definitions the seeds reach.
pub(crate) fn offenders(
    reached: &BTreeSet<Global>,
    classified: &BTreeMap<Global, super::Totality>,
) -> Vec<Global> {
    reached
        .iter()
        .filter(|name| {
            classified
                .get(*name)
                .is_some_and(|totality| !totality.is_total())
        })
        .cloned()
        .collect()
}

/// Why one position fails its obligation.
///
/// A position can fail without naming anything: an inline `rec` that does not
/// descend, or a `Prim::Exit`, is partial on its own account. Reporting only
/// reached names would miss exactly the shapes that need no name.
pub(crate) enum Fault {
    Named(Global),
    Inline,
}
