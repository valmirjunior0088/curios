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
        Apply, Atom, Bound, Carrier, Cases, Context, Definition, Error, Free, Func, FuncType,
        Global, InductType, Item, Let, Many, Match, Module, Prim, Rec, RecGroup, RecMember, Scope,
        Struct, Subterm, Telescope, Term, Three, TupleType, Two, Variant, is_prop, is_prop_in,
        reduce, synth_neutral,
    },
    curios_base::Grain,
    std::collections::{BTreeMap, BTreeSet},
};

/// How many recursive-group members [`Certificates::scrutinee_type`] will
/// unfold before giving up on naming a scrutinee's declaration.
///
/// Each unfold peels one nominal declaration, so a real chain is short; the
/// bound exists to terminate on a self-referential member rather than to
/// accommodate deep ones.
const UNFOLD_LIMIT: usize = 32;

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
    let mut groups = Groups::default();

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
            &mut groups,
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
        &mut groups,
    )?);

    groups.drain(context, module, &mut positions)?;

    Ok(positions)
}

/// Run the seeding walk over one term, recording the `rec` groups it reached.
fn certificates(
    context: &mut Context,
    module: &Module,
    term: &Term,
    site: &str,
    groups: &mut Groups,
) -> Result<Vec<Position>, Error> {
    let mut walk = Certificates {
        context,
        module,
        site: site.to_string(),
        opened: Vec::new(),
        positions: Vec::new(),
        groups: Vec::new(),
    };
    walk.walk(term)?;
    let positions = walk.positions;

    for witness in walk.groups {
        groups.enqueue(witness, site);
    }

    Ok(positions)
}

/// The `rec` groups still to walk, and those already walked.
///
/// A group is walked once for the whole module rather than once per mention,
/// and the site recorded is the first that reached it — which is the group's
/// own definition whenever that definition is reached first, and an honest
/// "reached from" otherwise.
#[derive(Default)]
struct Groups {
    seen: Vec<Term>,
    pending: Vec<(Term, String)>,
}

impl Groups {
    fn enqueue(&mut self, witness: Term, site: &str) {
        if self.seen.contains(&witness) {
            return;
        }
        self.seen.push(witness.clone());
        self.pending
            .push((witness, format!("a recursive group reached from {site}")));
    }

    /// Walk every queued group's members, following groups they reach.
    ///
    /// Both halves of each member. A member's *type* is as much a place to
    /// hand a proof to a `Prop`-declared parameter as its body is, and a
    /// group reached only from a type would otherwise never be queued at all.
    fn drain(
        &mut self,
        context: &mut Context,
        module: &Module,
        positions: &mut Vec<Position>,
    ) -> Result<(), Error> {
        while let Some((witness, site)) = self.pending.pop() {
            let Subterm::RecMember(RecMember { group, .. }) = &*witness else {
                continue;
            };
            let group = group.clone();
            for index in 0..group.len() {
                for half in [group.member_type(index), group.member_body(index)] {
                    positions.extend(certificates(context, module, &half, &site, self)?);
                }
            }
        }

        Ok(())
    }
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
/// A `rec` group is the one form this cannot open in place, because its member
/// scopes bind the group's own members and `member_body` reintroduces the
/// group; it is deferred to [`Groups`] and walked once for the module.
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
    groups: Vec<Term>,
}

impl Certificates<'_> {
    /// Seed every node of `term`, opening binders on the way down.
    ///
    /// Iterative, on an explicit stack, because a term's depth is a property of
    /// the *program*: a `Str` literal lowers to one certified-UTF-8 link per
    /// byte, so a recursive walk overflows a default test stack at a few
    /// hundred characters. Children are pushed in reverse so they pop in source
    /// order, which keeps the pre-order a recursive walk would produce and with
    /// it the first fault a diagnostic reports.
    fn walk(&mut self, term: &Term) -> Result<(), Error> {
        let mut work = vec![term.clone()];
        while let Some(term) = work.pop() {
            self.seed(&term)?;
            let mut children = Vec::new();
            self.descend(&term, &mut children)?;
            work.extend(children.into_iter().rev());
        }

        Ok(())
    }

    /// `term`'s children, in source order, each opened against a fresh binder
    /// wherever `term` introduces one.
    ///
    /// Fallible because typing a `match` scrutinee reduces, and reduction is
    /// budget-bounded: exhausting the budget is a compile error rather than a
    /// silently untyped binder.
    fn descend(&mut self, term: &Term, children: &mut Vec<Term>) -> Result<(), Error> {
        match &**term {
            // Binder-bearing forms open as they descend.
            Subterm::Func(Func { telescope, .. })
            | Subterm::FuncType(FuncType { telescope, .. }) => {
                self.telescope_terms(telescope.clone(), children);
            }

            Subterm::TupleType(TupleType { telescope }) => {
                self.telescope_units(telescope.clone(), children);
            }

            // A `let` block is flat, and binding `i` is closed over the `i`
            // binders before it — index `j` naming `bindings[j]`'s binder, the
            // same order the tail scope opens in. Collecting them side by side
            // would hand out loose indices, which is what sort synthesis
            // asserts on, so each is opened against the binders already made.
            Subterm::Let(Let { bindings, tail }) => {
                let hints = tail.hint_iter().collect::<Vec<_>>();
                let mut binders: Vec<Term> = Vec::new();
                for (index, binding) in bindings.iter().enumerate() {
                    let opened = binders.iter().collect::<Vec<_>>();
                    let type_ = binding.type_().release(&opened);
                    let value = binding.value().release(&opened);
                    children.push(type_.clone());
                    children.push(value);

                    let binder = self.context.fresh(hints.get(index).copied().flatten());
                    self.opened.push((binder.clone(), type_));
                    binders.push(Term::free_var(&binder));
                }
                children.push(tail.open(&binders.iter().collect::<Vec<_>>()));
            }

            // A `rec` group is deferred to [`Groups`] rather than descended
            // into here. Its member scopes bind the group's own members, so
            // `any_child_term` would hand them back with loose indices — which
            // is what sort synthesis asserts on — and `member_body` substitutes
            // references to the same group, so descending directly would not
            // terminate. Deferring also walks each group once instead of once
            // per mention: elaboration inlines a recursive global as a
            // `RecMember` carrying the whole group, and one prelude group is
            // carried by thirty-nine definitions.
            Subterm::Rec(Rec { group, tail }) => {
                self.note(group.clone());
                let types = (0..group.len())
                    .map(|index| group.member_type(index))
                    .collect::<Vec<_>>();
                children.push(self.open_annotated(&types, tail));
            }

            Subterm::RecMember(RecMember { group, .. }) => {
                self.note(group.clone());
            }

            // The eliminated declaration types the motive and arm binders. It
            // is looked up once here and threaded into both, because a binder
            // opened without a type is one sort synthesis answers `None`
            // about — and a `None` at an application head silently skips the
            // rule that seeds its `Prop`-declared arguments.
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                let scrutinee = self.scrutinee_type(head)?;
                children.push(head.clone());
                children.push(self.open_motive(motive, scrutinee.as_ref(), cases));
                self.arms(cases, scrutinee.as_ref(), motive, children);
            }

            // Everything else binds nothing, so its children are already
            // closed to the same degree this node is.
            _ => {
                let subterm: &Subterm = term;
                subterm.any_child_term(&mut |child| {
                    children.push(child.clone());
                    false
                });
            }
        }

        Ok(())
    }

    /// The scrutinee's inductive type, when this walk can name it.
    ///
    /// `None` where the head's type cannot be synthesized or is not a nominal
    /// inductive — every primitive carrier reaches here — and the binders are
    /// then opened untyped, exactly as they were before.
    fn scrutinee_type(&mut self, head: &Term) -> Result<Option<InductType>, Error> {
        let exhausted = || Error::reduce_exhausted(head.clone());

        let synthesized =
            synth_neutral(self.context, &self.opened, head).map_err(|e| e.into_error(exhausted))?;
        let Some(type_) = synthesized else {
            return Ok(None);
        };

        // A nominal declaration reaches here as a `RecMember`, not an
        // `InductType`: elaboration turns `induct Holder` into a recursive
        // group whose member body is the inductive. Unfold those, bounded,
        // because a member whose body is itself — `rec X : Type = X` — would
        // otherwise spin here rather than in the gate that rejects it.
        let mut current = reduce(self.context, type_).map_err(|e| e.into_error(exhausted))?;
        for _ in 0..UNFOLD_LIMIT {
            let next = match &*current {
                Subterm::InductType(induct) => return Ok(Some(induct.clone())),
                Subterm::RecMember(RecMember { group, index }) => group.member_body(*index),
                _ => return Ok(None),
            };
            current = reduce(self.context, next).map_err(|e| e.into_error(exhausted))?;
        }

        Ok(None)
    }

    /// Every proof this node hands to a `Prop`-declared position.
    ///
    /// Every node reaching here is closed, because [`Certificates::walk`] opens
    /// each binder it descends through and defers `rec` groups — the one form
    /// whose children cannot be opened in place — to [`Groups`]. That matters
    /// because sort synthesis assumes free occurrences and asserts on a bound
    /// one, so an unopened child would not merely be skipped: it would abort
    /// the walk.
    fn seed(&mut self, term: &Term) -> Result<(), Error> {
        debug_assert_eq!(term.reach(), 0, "the seeding walk opens what it descends");

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

    fn arms(
        &mut self,
        cases: &Cases,
        scrutinee: Option<&InductType>,
        motive: &Scope<Many>,
        children: &mut Vec<Term>,
    ) {
        match cases {
            Cases::Bool {
                false_case,
                true_case,
            } => {
                children.push(false_case.clone());
                children.push(true_case.clone());
            }
            Cases::Switch { cases, default } => {
                children.extend(cases.values().cloned());
                children.push(default.clone());
            }
            Cases::Induct { cases, default } => {
                for (tag, arm) in cases {
                    let declared =
                        scrutinee.and_then(|induct| self.constructor_telescope(induct, tag));
                    children.push(match declared {
                        Some(declared) => self.open_telescoped(declared, &arm.body),
                        None => self.open_many(&arm.body),
                    });
                }
                if let Some(default) = default {
                    children.push(default.clone());
                }
            }
            // A primitive carrier's cons binders are typed by the carrier
            // itself, and the induction hypothesis by the motive at the tail —
            // the same assumptions `elaborate_lst_match` and its siblings make
            // when they check these arms.
            Cases::FreeMonoid { carrier } => match carrier {
                Carrier::Nat {
                    empty_case,
                    cons_case,
                } => {
                    children.push(empty_case.clone());
                    let predecessor = Subterm::Prim(Prim::NatType).into();
                    children.push(self.open_fold_two(cons_case, predecessor, motive));
                }
                Carrier::Bin {
                    grain,
                    empty_case,
                    cons_case,
                } => {
                    children.push(empty_case.clone());
                    let atom = Subterm::Prim(match grain {
                        Grain::B => Prim::BoolType,
                        Grain::X => Prim::ByteType,
                    })
                    .into();
                    let tail = Subterm::Prim(Prim::BinType(*grain)).into();
                    children.push(self.open_fold_three(cons_case, atom, tail, motive));
                }
                Carrier::Lst {
                    elem,
                    empty_case,
                    cons_case,
                } => {
                    children.push(elem.clone());
                    children.push(empty_case.clone());
                    let tail = Subterm::Prim(Prim::LstType(elem.clone())).into();
                    children.push(self.open_fold_three(cons_case, elem.clone(), tail, motive));
                }
            },
        }
    }

    /// Record a `rec` group for the worklist, identified by its first member.
    fn note(&mut self, group: RecGroup) {
        let witness = Term::rec_member(group, 0);
        if !self.groups.contains(&witness) {
            self.groups.push(witness);
        }
    }

    /// The declared payload types of `tag`, at the scrutinee's parameters.
    ///
    /// The same telescope [`Certificates::seed`] reads to find `Prop`-declared
    /// constructor payloads, read here to type the arm binders that receive
    /// them.
    fn constructor_telescope(&self, induct: &InductType, tag: &Atom) -> Option<Telescope<Term>> {
        let declaration = self.module.induct_decls.get(&induct.name)?;
        let (_, constructor) = declaration
            .constructors
            .iter()
            .find(|(candidate, _)| candidate == tag)?;

        // `open_params` panics rather than truncating, and a declaration
        // whose telescope is shorter than the scrutinee's parameter list
        // would be malformed — decline it instead of aborting the compiler.
        (constructor.telescope.len() >= induct.params.len())
            .then(|| constructor.telescope.clone().open_params(&induct.params))
    }

    /// Open `scope` against fresh binders typed by a dependent telescope.
    ///
    /// Entries are opened progressively, as [`Certificates::telescope_terms`]
    /// does, because a later payload type may name an earlier payload. A
    /// telescope shorter than the scope leaves the remaining binders untyped
    /// rather than failing: an untyped binder costs a seed, a wrong one would
    /// produce a wrong seed silently.
    fn open_telescoped(&mut self, declared: Telescope<Term>, scope: &Scope<Many>) -> Term {
        let hints = scope.hint_iter().collect::<Vec<_>>();
        let mut declared = declared;
        let mut binders = Vec::new();

        for index in 0..scope.arity() {
            let binder = self.context.fresh(hints.get(index).copied().flatten());
            let term = Term::free_var(&binder);
            declared = match declared {
                Telescope::Cons(entry, rest) => {
                    self.opened.push((binder, entry));
                    rest.open(&[&term])
                }
                done => done,
            };
            binders.push(term);
        }

        scope.open(&binders.iter().collect::<Vec<_>>())
    }

    /// Open `scope` against fresh binders carrying `types` positionally.
    ///
    /// For binder groups whose types are already closed and independent of one
    /// another — a `rec` tail, whose binders are the group's members.
    fn open_annotated(&mut self, types: &[Term], scope: &Scope<Many>) -> Term {
        let hints = scope.hint_iter().collect::<Vec<_>>();
        let mut binders = Vec::new();

        for index in 0..scope.arity() {
            let binder = self.context.fresh(hints.get(index).copied().flatten());
            if let Some(type_) = types.get(index) {
                self.opened.push((binder.clone(), type_.clone()));
            }
            binders.push(Term::free_var(&binder));
        }

        scope.open(&binders.iter().collect::<Vec<_>>())
    }

    /// Open a match motive against typed binders.
    ///
    /// A nominal scrutinee gives one binder per index and then the scrutinee
    /// itself; a primitive carrier has no indices, so its motive binds the
    /// scrutinee alone and the carrier names its type.
    fn open_motive(
        &mut self,
        motive: &Scope<Many>,
        scrutinee: Option<&InductType>,
        cases: &Cases,
    ) -> Term {
        if let Some(opened) = self.open_induct_motive(motive, scrutinee) {
            return opened;
        }
        if motive.arity() == 1
            && let Some(type_) = primitive_scrutinee(cases)
        {
            return self.open_annotated(&[type_], motive);
        }

        self.open_many(motive)
    }

    /// The nominal half of [`Certificates::open_motive`].
    ///
    /// The index types come from the declaration's index telescope past its
    /// parameters, and the scrutinee binder is typed at those index binders.
    /// `None` on any arity disagreement, which keeps a malformed declaration
    /// from producing confidently wrong binder types — and every such check
    /// precedes the first minted binder, so declining costs nothing.
    fn open_induct_motive(
        &mut self,
        motive: &Scope<Many>,
        scrutinee: Option<&InductType>,
    ) -> Option<Term> {
        let induct = scrutinee?;
        let declaration = self.module.induct_decls.get(&induct.name)?;
        let arity = declaration.indices.len().checked_sub(induct.params.len())?;
        if arity + 1 != motive.arity() {
            return None;
        }

        let hints = motive.hint_iter().collect::<Vec<_>>();
        let mut telescope = declaration.indices.clone().open_params(&induct.params);
        let mut binders = Vec::new();

        while let Telescope::Cons(entry, rest) = telescope {
            let binder = self
                .context
                .fresh(hints.get(binders.len()).copied().flatten());
            let term = Term::free_var(&binder);
            self.opened.push((binder, entry));
            telescope = rest.open(&[&term]);
            binders.push(term);
        }

        let binder = self
            .context
            .fresh(hints.get(binders.len()).copied().flatten());
        let type_ = Term::induct_type_at(
            induct.name.clone(),
            induct.universes.clone(),
            induct.params.clone(),
            binders.clone(),
        );
        self.opened.push((binder.clone(), type_));
        binders.push(Term::free_var(&binder));

        Some(motive.open(&binders.iter().collect::<Vec<_>>()))
    }

    /// Open a `Nat` fold's cons arm: the predecessor, then the hypothesis at
    /// it.
    fn open_fold_two(
        &mut self,
        scope: &Scope<Two>,
        predecessor_type: Term,
        motive: &Scope<Many>,
    ) -> Term {
        let predecessor = self.context.fresh(scope.first_hint());
        let hypothesis = self.context.fresh(scope.second_hint());
        let value = Term::free_var(&predecessor);

        self.opened.push((predecessor, predecessor_type));
        self.note_hypothesis(hypothesis.clone(), motive, &value);

        scope.open(&[&value, &Term::free_var(&hypothesis)])
    }

    /// Open a `Lst` or `Bin` fold's cons arm: the leading element, the tail,
    /// then the hypothesis at the tail.
    fn open_fold_three(
        &mut self,
        scope: &Scope<Three>,
        atom_type: Term,
        tail_type: Term,
        motive: &Scope<Many>,
    ) -> Term {
        let atom = self.context.fresh(scope.first_hint());
        let tail = self.context.fresh(scope.second_hint());
        let hypothesis = self.context.fresh(scope.third_hint());
        let value = Term::free_var(&tail);

        self.opened.push((atom.clone(), atom_type));
        self.opened.push((tail, tail_type));
        self.note_hypothesis(hypothesis.clone(), motive, &value);

        scope.open(&[&Term::free_var(&atom), &value, &Term::free_var(&hypothesis)])
    }

    /// Record a fold hypothesis's type: the motive at the recursive argument.
    ///
    /// A primitive carrier has no indices, so its motive binds the scrutinee
    /// alone; anything else is a shape this walk does not recognize and the
    /// binder is left untyped rather than opened at a guessed arity.
    fn note_hypothesis(&mut self, binder: Free, motive: &Scope<Many>, argument: &Term) {
        if motive.arity() == 1 {
            self.opened.push((binder, motive.open(&[argument])));
        }
    }

    /// Fresh binders for a scope, in order, with no recorded types.
    ///
    /// The fallback for binder groups this walk cannot type: a primitive
    /// carrier's fold arms, and any scrutinee whose type sort synthesis
    /// declines to answer for. Sort synthesis then answers conservatively
    /// about them, which costs a seed rather than risking a wrong one.
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

    fn telescope_terms(&mut self, telescope: Telescope<Term>, children: &mut Vec<Term>) {
        let mut telescope = telescope;
        loop {
            match telescope {
                Telescope::Done(terminal) => return children.push(*terminal),
                Telescope::Cons(entry, rest) => {
                    children.push(entry.clone());
                    let binder = self.context.fresh(rest.first_hint());
                    self.opened.push((binder.clone(), entry));
                    telescope = rest.open(&[&Term::free_var(&binder)]);
                }
            }
        }
    }

    fn telescope_units(&mut self, telescope: Telescope<()>, children: &mut Vec<Term>) {
        let mut telescope = telescope;
        while let Telescope::Cons(entry, rest) = telescope {
            children.push(entry.clone());
            let binder = self.context.fresh(rest.first_hint());
            self.opened.push((binder.clone(), entry));
            telescope = rest.open(&[&Term::free_var(&binder)]);
        }
    }
}

/// The scrutinee type of a match on a primitive carrier.
///
/// `None` for a nominal inductive, whose scrutinee type carries parameters and
/// indices and is recovered from the declaration instead.
fn primitive_scrutinee(cases: &Cases) -> Option<Term> {
    let prim = match cases {
        Cases::Bool { .. } => Prim::BoolType,
        Cases::Switch { .. } => Prim::NatType,
        Cases::Induct { .. } => return None,
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat { .. } => Prim::NatType,
            Carrier::Bin { grain, .. } => Prim::BinType(*grain),
            Carrier::Lst { elem, .. } => Prim::LstType(elem.clone()),
        },
    };

    Some(Subterm::Prim(prim).into())
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
