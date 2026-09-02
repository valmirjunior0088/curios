//! Derived witness bodies: the body a body-less `satisfy C(T);` asks the compiler to write.
//!
//! Lowering carries the declaration into Core as the same anonymous definition a written witness produces, with [`Transient::Derive`](curios_core::Transient) in body position, so the witness's telescope — its implicit binders and `use` premises — is in scope when the body is checked, and its signature registers in the witness table exactly as a written one does (orphan and duplicate-key refusals need no body). Checking the transient against the concept application is what writes the body: the derivation registered for the concept's registry slot produces the Core the lowerer would have produced for the equivalent written witness, and that Core is elaborated under the same expectation, so a derived body is typed, resolved, zonked and certified like any authored one — the kernel never sees the transient.
//!
//! **Eligibility.** A derivation writes from a declaration, and only from one: the key must reduce to a registered `induct` or `struct` — not an intrinsic carrier, a tuple or function shape, or a concept's own record — that is representation-transparent at the declaring island and not `Prop`-sorted, its parameters and indices given by the key. Sealing is refused before any of that, with the rule a written literal meets, so that derivation is never a door through representation privacy; a concept with no derivation refuses by name, since derivability is registered per concept and never inferred from its shape. Every refusal is a hard error at the `satisfy` span.
//!
//! **Payloads.** Both derivations read a declaration the same way: the constructor telescopes (or the field telescope) opened at the key's parameters, one binder minted per payload, each explicit payload classified by its type under the binders before it. A payload that is itself a type is refused; a proof payload takes no part beyond what the derivation states for it; every other payload takes part through the concept's own method, `Spell/spell` or `Equal/eql`, applied with a `use` argument the body supplies as a witness goal of its own — resolved by ordinary resolution in the witness's scope (a telescope premise, the witness's own entry, an `and` sibling), and reported unresolved under a provenance naming the constructor and the payload, with the telescope premise to add when the payload's type is a telescope variable. An implicit payload is bound and never named. A field or payload the lowerer named `_{position}` had no written label, which is the one mark Core keeps of it.
//!
//! **The `Spell` body.** One match arm per constructor in declaration order, the motive omitted as a written match omits it, and per arm a single renderer application over structured pieces: `Spell/call("/Tree/node", [spell(l), …])` for a constructor, `Spell/record("/Point", [("x", spell(x)), …])` for a struct, whose fields are projected. A value therefore spells as its constructor's absolute path applied to its explicit payloads, so the text re-parses from any module that sees the names; a struct spells labeled, or positionally where its field has no label; a proof payload spells as the written goal `"?"`.
//!
//! **The `Equal` body.** `eql` matches its two arguments in turn: an arm per constructor on the first, and inside it a one-arm match on the second at the same constructor — its payloads compared pairwise through `Equal/eql` under `&&`, `true` when there is nothing to compare — with a `| _ => false` default for every other constructor. A struct compares its projections the same way, with no match. Proofs and implicit payloads do not take part. `neq` negates the same comparison, built a second time over binders of its own.

use {
    super::{
        Context, Error, Mode, Sort, TermBuilders, Underivable, elaborate, reduce_with, str_literal,
        syn_call,
    },
    curios_core::{
        Free, Global, InductDecl, InductParam, InductType, Intrinsic, Many, MetavarOrigin, Scope,
        StructDecl, StructType, Subterm, Term, WitnessOrigin,
    },
    curios_utilities::{ConceptField, InfixOp, Plicity, Qualifier, Span, SyntaxRegistry},
};

/// A concept the compiler can write a witness body for.
enum Derivation {
    Spell,
    Equal,
}

/// The derivation registered for `concept`'s slot, if any.
fn derivation_for(syntax: &SyntaxRegistry, concept: &Global) -> Option<Derivation> {
    let registered = [
        (syntax.spell.spell.concept, Derivation::Spell),
        (syntax.operator.eql.concept, Derivation::Equal),
    ];
    registered
        .into_iter()
        .find(|(name, _)| concept.qualifier() == Some(&name.qualifier()))
        .map(|(_, derivation)| derivation)
}

/// Check a `Derive` transient against its expected type, writing the body the declaration asked for.
pub(crate) fn elaborate_derive(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Mode::Check(expected) = &mode else {
        return Err(Error::derive_outside_witness().at_opt(term.span()));
    };

    // The witness's declared type elaborated before its body, so the expectation reduces to the concept's record type — a `StructType` whose name is registered as a concept.
    let reduced = reduce_with(context, expected)?;
    let Subterm::StructType(StructType { name, params, .. }) = &*reduced else {
        return Err(Error::derive_outside_witness().at_opt(term.span()));
    };
    if context.concept(name).is_none() {
        return Err(Error::derive_outside_witness().at_opt(term.span()));
    }

    // Sealing first: the rule `elaborate_struct` applies to a written literal, decided here before any body is written.
    let record = context
        .struct_decl(name)
        .cloned()
        .expect("a registered concept has a backing struct declaration");
    if !record.rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(&record.module))
    {
        return Err(Error::private_representation(name.symbol()).at_opt(term.span()));
    }

    let Some(derivation) = derivation_for(&context.syntax(), name) else {
        return Err(Error::no_derivation(name.symbol()).at_opt(term.span()));
    };
    let key = params
        .first()
        .cloned()
        .expect("a derivable concept takes the type it is derived for as its first parameter");
    let site = Site {
        concept: name,
        key: &key,
        span: term.span(),
    };
    let subject = subject(context, &site)?;
    let body = match derivation {
        Derivation::Spell => spell_body(context, &site, &subject)?,
        Derivation::Equal => eql_body(context, &site, &subject)?,
    };
    elaborate(context, &body, mode)
}

/// The declaration a derivation is writing for: the concept, the key it is derived at, and the span every refusal and goal reports at.
struct Site<'a> {
    concept: &'a Global,
    key: &'a Term,
    span: Option<Span>,
}

impl Site<'_> {
    fn at(&self, term: Term) -> Term {
        match &self.span {
            Some(span) => term.with_span(span.clone()),
            None => term,
        }
    }

    fn refuse(&self, reason: Underivable) -> Error {
        Error::underivable(self.concept.symbol(), self.key.clone(), reason)
            .at_opt(self.span.clone())
    }
}

/// What a derivation writes from, read off the key's weak head normal form: the declaration, instantiated at the key's universes, and the parameters the key applies it to.
enum Subject {
    Induct {
        name: Global,
        decl: InductDecl,
        params: Vec<Term>,
    },
    Struct {
        name: Global,
        decl: StructDecl,
        params: Vec<Term>,
    },
}

fn subject(context: &mut Context, site: &Site<'_>) -> Result<Subject, Error> {
    let reduced = reduce_with(context, site.key)?;

    match &*reduced {
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            ..
        }) => {
            let Some(decl) = context.induct_decl(name).cloned() else {
                return Err(site.refuse(Underivable::NotDeclared));
            };
            let decl = context
                .instantiate_induct_decl_at(&decl, universes)
                .map_err(Error::from)?;
            transparent(context, site, name, &decl.module, decl.rep_public)?;
            if matches!(&*decl.result_sort, Subterm::Prop) {
                return Err(site.refuse(Underivable::Proposition));
            }
            Ok(Subject::Induct {
                name: name.clone(),
                decl,
                params: params.clone(),
            })
        }
        Subterm::StructType(StructType {
            name,
            universes,
            params,
        }) => {
            if context.concept(name).is_some() {
                return Err(site.refuse(Underivable::Concept));
            }
            let Some(decl) = context.struct_decl(name).cloned() else {
                return Err(site.refuse(Underivable::NotDeclared));
            };
            let decl = context
                .instantiate_struct_decl_at(&decl, universes)
                .map_err(Error::from)?;
            transparent(context, site, name, &decl.module, decl.rep_public)?;
            if matches!(&*decl.result_sort, Subterm::Prop) {
                return Err(site.refuse(Underivable::Proposition));
            }
            Ok(Subject::Struct {
                name: name.clone(),
                decl,
                params: params.clone(),
            })
        }
        _ => Err(site.refuse(Underivable::NotDeclared)),
    }
}

/// The representation-privacy rule a written match or projection meets, applied to the declaration a derivation would eliminate.
fn transparent(
    context: &Context,
    site: &Site<'_>,
    name: &Global,
    module: &Qualifier,
    rep_public: bool,
) -> Result<(), Error> {
    if !rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(module))
    {
        return Err(Error::private_representation(name.symbol()).at_opt(site.span.clone()));
    }
    Ok(())
}

/// The absolute path a declaration or constructor re-parses from.
fn path(name: &Global, tag: Option<&str>) -> String {
    let qualifier = name
        .qualifier()
        .expect("a declared type has the path it was declared at");
    match tag {
        Some(tag) => qualifier.with(tag).join(),
        None => qualifier.join(),
    }
}

/// A list literal over `items`, its element type a hole the expected type solves — the shape the synthesized test tail already builds.
fn list(context: &mut Context, items: Vec<Term>) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: Term::hole(context.mint_metavar()),
        items,
    })
}

/// An omitted motive, as the lowerer carries one: a hole the arms pin.
fn motive(context: &mut Context) -> Scope<Many> {
    Term::match_motive_written(Term::hole(context.mint_metavar()))
}

/// Where a payload sits, for the provenance a missing witness is reported under and the label a struct field spells with.
struct Payload {
    /// The constructor's absolute path, or the struct's.
    constructor: String,
    /// The written label, empty where there was none.
    label: String,
    /// `payload 'x'`, `field #2`: how a report names it.
    described: String,
}

impl Payload {
    fn new(owner: &Global, tag: Option<&str>, label: &str, ordinal: usize) -> Self {
        let noun = match tag {
            Some(_) => "payload",
            None => "field",
        };
        let described = match label.is_empty() {
            true => format!("{noun} #{ordinal}"),
            false => format!("{noun} '{label}'"),
        };
        Self {
            constructor: path(owner, tag),
            label: label.to_string(),
            described,
        }
    }
}

/// How one explicit payload takes part in a derived body.
enum Part {
    /// Evidence: it erases, spells as the goal, and compares as nothing.
    Proof,
    /// A value, taking part through the concept's own witness — with the type to name in the premise to add when that type is a variable of the witness's telescope.
    Value { premise: Option<String> },
}

/// One explicit payload, classified: its position among the payloads (or fields), where it sits, and what it is.
struct Classified {
    position: usize,
    payload: Payload,
    part: Part,
}

/// Classify one explicit payload of type `type_`, read under the binders in `opened`.
fn classify(
    context: &mut Context,
    site: &Site<'_>,
    opened: &mut Vec<(Free, Term)>,
    position: usize,
    payload: Payload,
    type_: &Term,
) -> Result<Classified, Error> {
    let reduced = reduce_with(context, type_)?;
    if matches!(&*reduced, Subterm::Type(_) | Subterm::Prop) {
        return Err(site.refuse(Underivable::TypeValued {
            constructor: payload.constructor,
            payload: payload.described,
        }));
    }

    let sort = Sort::of_in(context, opened, type_)
        .map_err(|error| Error::from_reduce(error, || Error::reduce_exhausted(type_.clone())))?;
    if matches!(sort, Sort::Prop) {
        return Ok(Classified {
            position,
            payload,
            part: Part::Proof,
        });
    }

    // A payload typed by a variable the arm did not bind is typed by the witness's telescope, and the premise that would cover it is the one to add.
    let telescope_variable = match &*reduced {
        Subterm::Var(var) => var
            .as_free()
            .is_some_and(|free| free.is_local() && !opened.iter().any(|(bound, _)| bound == free)),
        _ => false,
    };
    Ok(Classified {
        position,
        payload,
        part: Part::Value {
            premise: telescope_variable.then(|| reduced.to_string()),
        },
    })
}

/// A label as the declaration wrote it: empty where the lowerer minted the `_{position}` an unlabeled field or payload carries.
fn written_label(label: &str, position: usize) -> &str {
    match label == format!("_{position}") {
        true => "",
        false => label,
    }
}

/// One constructor of the declaration a derivation is eliminating, at the key's parameters.
struct Constructor<'a> {
    owner: &'a Global,
    tag: &'a str,
    signature: &'a InductParam,
    param_count: usize,
    params: &'a [Term],
}

impl Constructor<'_> {
    /// One binder per payload, hinted by the written label where there is one.
    fn binders(&self, context: &mut Context) -> Vec<Free> {
        self.signature.telescope.labels()[self.param_count..]
            .iter()
            .enumerate()
            .map(|(position, label)| {
                let label = written_label(label, position);
                context.fresh((!label.is_empty()).then_some(label))
            })
            .collect()
    }

    /// The payloads' plicities, one per binder.
    fn plicities(&self) -> Vec<Plicity> {
        self.signature.plicities()[self.param_count..].to_vec()
    }

    /// The explicit payloads opened at `binders` — one per payload, after the declaration's parameters — each classified with its type read under the binders before it. `value` is the scrutinee, the one binder in scope above the arm.
    fn parts(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        binders: &[Free],
        value: &Free,
    ) -> Result<Vec<Classified>, Error> {
        let labels = self
            .signature
            .telescope
            .labels()
            .into_iter()
            .map(str::to_string)
            .collect::<Vec<_>>();
        let plicities = self.signature.plicities();
        let args = self
            .params
            .iter()
            .cloned()
            .chain(binders.iter().map(Term::free_var))
            .collect::<Vec<_>>();

        let mut opened = vec![(value.clone(), site.key.clone())];
        let mut parts = Vec::new();
        self.signature
            .telescope
            .clone()
            .walk(&args, |index, _, type_| {
                if index >= self.param_count {
                    let position = index - self.param_count;
                    if plicities[index] == Plicity::Explicit {
                        let label = written_label(&labels[index], position);
                        let payload = Payload::new(self.owner, Some(self.tag), label, position + 1);
                        parts.push(classify(
                            context,
                            site,
                            &mut opened,
                            position,
                            payload,
                            type_,
                        )?);
                    }
                    opened.push((binders[position].clone(), type_.clone()));
                }
                Ok::<(), Error>(())
            })?;
        Ok(parts)
    }
}

/// The fields of a struct at `params`, each classified with its type read through the projections off `value` before it.
fn struct_parts(
    context: &mut Context,
    site: &Site<'_>,
    name: &Global,
    decl: &StructDecl,
    params: &[Term],
    value: &Free,
) -> Result<Vec<Classified>, Error> {
    let fields = decl.arity.clone().open(&params.iter().collect::<Vec<_>>());
    let labels = fields
        .labels()
        .into_iter()
        .map(str::to_string)
        .collect::<Vec<_>>();
    let projections = (0..labels.len())
        .map(|index| Term::proj(Term::free_var(value), index))
        .collect::<Vec<_>>();

    let mut opened = vec![(value.clone(), site.key.clone())];
    let mut parts = Vec::new();
    fields.walk(&projections, |index, _, type_| {
        let label = written_label(&labels[index], index);
        let payload = Payload::new(name, None, label, index + 1);
        parts.push(classify(context, site, &mut opened, index, payload, type_)?);
        Ok::<(), Error>(())
    })?;
    Ok(parts)
}

/// `C/method(use ?w, args…)`: the concept's method applied to `args`, its `use` argument a witness goal born with the payload's provenance — how a missing witness is reported against the constructor and payload rather than against the method.
fn witness_call(
    context: &mut Context,
    site: &Site<'_>,
    field: ConceptField,
    classified: &Classified,
    premise: Option<&str>,
    args: Vec<Term>,
) -> Term {
    let binder = match premise {
        Some(type_) => format!(
            "{} — add `use {}({type_})` to the telescope",
            classified.payload.described,
            field.concept.last()
        ),
        None => classified.payload.described.clone(),
    };
    let provenance = WitnessOrigin {
        func: classified.payload.constructor.clone(),
        binder,
    };
    // Spanned like the application around it: a goal deferred to the module's drain reports at the term it was born from, and that term is the declaration.
    let goal = site.at(Term::metavar_birthed(
        context.mint_metavar(),
        MetavarOrigin::Witness(provenance),
        Vec::new(),
    ));
    let method = Term::free_var(&Free::global(field.concept.qualifier().with(field.field)));
    let arguments = std::iter::once((Plicity::Witness, goal))
        .chain(args.into_iter().map(|arg| (Plicity::Explicit, arg)))
        .collect::<Vec<_>>();
    site.at(Term::apply_marked(method, arguments))
}

/// The `Spell` witness record: `spell` as a one-parameter function over the derived rendering.
fn spell_body(context: &mut Context, site: &Site<'_>, subject: &Subject) -> Result<Term, Error> {
    let syntax = context.syntax();
    let value = context.fresh(Some("value"));

    // The spelling of one classified payload read through `read`.
    let spell = |context: &mut Context, classified: &Classified, read: Term| match &classified.part
    {
        Part::Proof => str_literal(&syntax.string, b"?"),
        Part::Value { premise } => witness_call(
            context,
            site,
            syntax.spell.spell,
            classified,
            premise.as_deref(),
            vec![read],
        ),
    };

    let rendered = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &value)?;
            let entries = parts
                .iter()
                .map(|classified| {
                    let read = Term::proj(Term::free_var(&value), classified.position);
                    Term::tuple([
                        str_literal(&syntax.string, classified.payload.label.as_bytes()),
                        spell(context, classified, read),
                    ])
                })
                .collect::<Vec<_>>();
            let items = list(context, entries);
            let head = str_literal(&syntax.string, path(name, None).as_bytes());
            site.at(syn_call(syntax.spell.record, [head, items]))
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in &decl.constructors {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    param_count: decl.param_count(),
                    params,
                };
                let binders = constructor.binders(context);
                let parts = constructor.parts(context, site, &binders, &value)?;
                let pieces = parts
                    .iter()
                    .map(|classified| {
                        let read = Term::free_var(&binders[classified.position]);
                        spell(context, classified, read)
                    })
                    .collect::<Vec<_>>();
                let items = list(context, pieces);
                let head = str_literal(&syntax.string, path(name, Some(tag.as_str())).as_bytes());
                let body = site.at(syn_call(syntax.spell.call, [head, items]));
                arms.push((
                    tag.clone(),
                    constructor
                        .plicities()
                        .into_iter()
                        .zip(binders)
                        .collect::<Vec<_>>(),
                    body,
                ));
            }

            let motive = motive(context);
            site.at(Term::induct_match_scoped_marked(
                Term::free_var(&value),
                motive,
                arms,
                None,
            ))
        }
    };

    let method = Term::func([(value, Term::hole(context.mint_metavar()))], rendered);
    Ok(site.at(Term::struct_(
        site.concept.clone(),
        Vec::<Term>::new(),
        [method],
    )))
}

/// The `Equal` witness record: `eql` over the derived comparison, `neq` over its negation.
fn eql_body(context: &mut Context, site: &Site<'_>, subject: &Subject) -> Result<Term, Error> {
    let method = |context: &mut Context, negated: bool| -> Result<Term, Error> {
        let left = context.fresh(Some("left"));
        let right = context.fresh(Some("right"));
        let compared = compare(context, site, subject, &left, &right)?;
        let body = match negated {
            false => compared,
            true => site.at(Term::bool_match_scoped(
                compared,
                motive(context),
                Term::intrinsic(Intrinsic::Bool(true)),
                Term::intrinsic(Intrinsic::Bool(false)),
            )),
        };
        Ok(Term::func(
            [
                (left, Term::hole(context.mint_metavar())),
                (right, Term::hole(context.mint_metavar())),
            ],
            body,
        ))
    };

    let eql = method(context, false)?;
    let neq = method(context, true)?;
    Ok(site.at(Term::struct_(
        site.concept.clone(),
        Vec::<Term>::new(),
        [eql, neq],
    )))
}

/// Whether `left` and `right` are equal, as a `Bool`-valued term over the two binders.
fn compare(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    left: &Free,
    right: &Free,
) -> Result<Term, Error> {
    let field = context.syntax().operator.eql;

    // The comparisons of the classified payloads read through `reads`, joined under `&&`; a proof takes no part.
    let conjunction =
        |context: &mut Context, parts: &[Classified], reads: &dyn Fn(usize) -> (Term, Term)| {
            parts
                .iter()
                .filter_map(|classified| match &classified.part {
                    Part::Proof => None,
                    Part::Value { premise } => {
                        let (this, that) = reads(classified.position);
                        Some(witness_call(
                            context,
                            site,
                            field,
                            classified,
                            premise.as_deref(),
                            vec![this, that],
                        ))
                    }
                })
                .collect::<Vec<_>>()
                .into_iter()
                .reduce(|this, that| site.at(Term::infix(InfixOp::And, this, that)))
                .unwrap_or_else(|| Term::intrinsic(Intrinsic::Bool(true)))
        };

    match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, left)?;
            Ok(conjunction(context, &parts, &|index| {
                (
                    Term::proj(Term::free_var(left), index),
                    Term::proj(Term::free_var(right), index),
                )
            }))
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in &decl.constructors {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    param_count: decl.param_count(),
                    params,
                };
                let lefts = constructor.binders(context);
                let rights = constructor.binders(context);
                let parts = constructor.parts(context, site, &lefts, left)?;
                let compared = conjunction(context, &parts, &|position| {
                    (
                        Term::free_var(&lefts[position]),
                        Term::free_var(&rights[position]),
                    )
                });
                let plicities = constructor.plicities();

                // The inner match names the one constructor the outer arm already committed to; every other tag is the default.
                let inner = site.at(Term::induct_match_scoped_marked(
                    Term::free_var(right),
                    motive(context),
                    [(
                        tag.clone(),
                        plicities.iter().copied().zip(rights).collect::<Vec<_>>(),
                        compared,
                    )],
                    Some(Term::intrinsic(Intrinsic::Bool(false))),
                ));
                arms.push((
                    tag.clone(),
                    plicities.into_iter().zip(lefts).collect::<Vec<_>>(),
                    inner,
                ));
            }

            let motive = motive(context);
            Ok(site.at(Term::induct_match_scoped_marked(
                Term::free_var(left),
                motive,
                arms,
                None,
            )))
        }
    }
}
