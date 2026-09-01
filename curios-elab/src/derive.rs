//! Derived witness bodies: the body a body-less `satisfy C(T);` asks the compiler to write.
//!
//! Lowering carries the declaration into Core as the same anonymous definition a written witness produces, with [`Transient::Derive`](curios_core::Transient) in body position, so the witness's telescope — its implicit binders and `use` premises — is in scope when the body is checked, and its signature registers in the witness table exactly as a written one does (orphan and duplicate-key refusals need no body). Checking the transient against the concept application is what writes the body: the derivation registered for the concept's registry slot produces the Core the lowerer would have produced for the equivalent written witness, and that Core is elaborated under the same expectation, so a derived body is typed, resolved, zonked and certified like any authored one — the kernel never sees the transient.
//!
//! **Eligibility.** A derivation writes from a declaration, and only from one: the key must reduce to a registered `induct` or `struct` — not an intrinsic carrier, a tuple or function shape, or a concept's own record — that is representation-transparent at the declaring island and not `Prop`-sorted, its parameters and indices given by the key. Sealing is refused before any of that, with the rule a written literal meets, so that derivation is never a door through representation privacy; a concept with no derivation refuses by name, since derivability is registered per concept and never inferred from its shape. Every refusal is a hard error at the `satisfy` span.
//!
//! **The `Spell` body.** One match arm per constructor in declaration order, the motive omitted as a written match omits it, and per arm a single renderer application over structured pieces: `Spell/call("/Tree/node", [spell(l), …])` for a constructor, `Spell/record("/Point", [("x", spell(x)), …])` for a struct, whose fields are projected. A value therefore spells as its constructor's absolute path applied to its explicit payloads, so the text re-parses from any module that sees the names; a struct spells labeled, or positionally where its field has no label. An implicit payload is bound and omitted (the re-parsed call infers it); an explicit payload that is itself a type is refused; a proof payload contributes the written goal `"?"`; every other payload is spelled by `Spell/spell`, whose `use` argument the body supplies as a witness goal of its own — resolved by ordinary resolution in the witness's scope (a telescope premise, the witness's own entry, an `and` sibling), and reported unresolved under a provenance naming the constructor and the payload, with the telescope premise to add when the payload's type is a telescope variable.

use {
    super::{
        Context, Error, Mode, Sort, TermBuilders, Underivable, elaborate, reduce_with, str_literal,
        syn_call,
    },
    curios_core::{
        Free, Global, InductDecl, InductType, Intrinsic, MetavarOrigin, StructDecl, StructType,
        Subterm, Term, WitnessOrigin,
    },
    curios_utilities::{Plicity, Span, SyntaxRegistry},
};

/// A concept the compiler can write a witness body for.
enum Derivation {
    Spell,
}

/// The derivation registered for `concept`'s slot, if any.
fn derivation_for(syntax: &SyntaxRegistry, concept: &Global) -> Option<Derivation> {
    let spell = syntax.spell.spell.concept.qualifier();
    (concept.qualifier() == Some(&spell)).then_some(Derivation::Spell)
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
    let body = match derivation {
        Derivation::Spell => {
            let subject = subject(context, name, &key, term.span())?;
            spell_body(context, name, &subject, &key, term.span())?
        }
    };
    elaborate(context, &body, mode)
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

fn subject(
    context: &mut Context,
    concept: &Global,
    key: &Term,
    span: Option<Span>,
) -> Result<Subject, Error> {
    let refuse =
        |reason| Error::underivable(concept.symbol(), key.clone(), reason).at_opt(span.clone());
    let reduced = reduce_with(context, key)?;

    match &*reduced {
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            ..
        }) => {
            let Some(decl) = context.induct_decl(name).cloned() else {
                return Err(refuse(Underivable::NotDeclared));
            };
            let decl = context
                .instantiate_induct_decl_at(&decl, universes)
                .map_err(Error::from)?;
            transparent(context, name, &decl.module, decl.rep_public, span.clone())?;
            if matches!(&*decl.result_sort, Subterm::Prop) {
                return Err(refuse(Underivable::Proposition));
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
                return Err(refuse(Underivable::Concept));
            }
            let Some(decl) = context.struct_decl(name).cloned() else {
                return Err(refuse(Underivable::NotDeclared));
            };
            let decl = context
                .instantiate_struct_decl_at(&decl, universes)
                .map_err(Error::from)?;
            transparent(context, name, &decl.module, decl.rep_public, span.clone())?;
            if matches!(&*decl.result_sort, Subterm::Prop) {
                return Err(refuse(Underivable::Proposition));
            }
            Ok(Subject::Struct {
                name: name.clone(),
                decl,
                params: params.clone(),
            })
        }
        _ => Err(refuse(Underivable::NotDeclared)),
    }
}

/// The representation-privacy rule a written match or projection meets, applied to the declaration a derivation would eliminate.
fn transparent(
    context: &Context,
    name: &Global,
    module: &curios_utilities::Qualifier,
    rep_public: bool,
    span: Option<Span>,
) -> Result<(), Error> {
    if !rep_public
        && context
            .island()
            .is_some_and(|island| !island.is_within(module))
    {
        return Err(Error::private_representation(name.symbol()).at_opt(span.clone()));
    }
    Ok(())
}

/// The absolute path a declaration or constructor re-parses from, as a string literal.
fn path_literal(syntax: &SyntaxRegistry, name: &Global, tag: Option<&str>) -> Term {
    let qualifier = name
        .qualifier()
        .expect("a declared type has the path it was declared at")
        .clone();
    let path = match tag {
        Some(tag) => qualifier.with(tag).join(),
        None => qualifier.join(),
    };
    str_literal(&syntax.string, path.as_bytes())
}

/// A list literal over `items`, its element type a hole the expected type solves — the shape the synthesized test tail already builds.
fn list(context: &mut Context, items: Vec<Term>) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: Term::hole(context.mint_metavar()),
        items,
    })
}

fn at(term: Term, span: Option<Span>) -> Term {
    match span {
        Some(span) => term.with_span(span),
        None => term,
    }
}

/// The `Spell` witness record: `spell` as a one-parameter function over the derived rendering.
fn spell_body(
    context: &mut Context,
    concept: &Global,
    subject: &Subject,
    key: &Term,
    span: Option<Span>,
) -> Result<Term, Error> {
    let syntax = context.syntax();
    let value = context.fresh(Some("value"));
    let scrutinee = Term::free_var(&value);

    let rendered = match subject {
        Subject::Struct { name, decl, params } => {
            let fields = decl.arity.clone().open(&params.iter().collect::<Vec<_>>());
            let labels = fields
                .labels()
                .into_iter()
                .map(str::to_string)
                .collect::<Vec<_>>();
            let projections = (0..labels.len())
                .map(|index| Term::proj(scrutinee.clone(), index))
                .collect::<Vec<_>>();

            // The value itself is the one opened binder: a dependent field's type names earlier fields through projections off it.
            let mut opened = vec![(value.clone(), key.clone())];
            let mut entries = Vec::new();
            fields.walk(&projections, |index, projection, type_| {
                // The lowerer names an unlabeled field `_{position}` and Core keeps no other mark of it; such a field spells positionally, which the literal grammar reads for any struct.
                let label = match labels[index] == format!("_{index}") {
                    true => "",
                    false => labels[index].as_str(),
                };
                let payload = Payload {
                    owner: name,
                    tag: None,
                    label,
                    ordinal: index + 1,
                };
                let piece = spell_piece(
                    context,
                    &mut opened,
                    &payload,
                    projection,
                    type_,
                    span.clone(),
                )?;
                entries.push(Term::tuple([
                    str_literal(&syntax.string, label.as_bytes()),
                    piece,
                ]));
                Ok::<(), Error>(())
            })?;

            let items = list(context, entries);
            at(
                syn_call(
                    syntax.spell.record,
                    [path_literal(&syntax, name, None), items],
                ),
                span.clone(),
            )
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in &decl.constructors {
                let param_count = decl.param_count();
                let labels = signature
                    .telescope
                    .labels()
                    .into_iter()
                    .map(str::to_string)
                    .collect::<Vec<_>>();
                let binders = labels[param_count..]
                    .iter()
                    .map(|label| context.fresh((!label.is_empty()).then_some(label.as_str())))
                    .collect::<Vec<_>>();
                let plicities = signature.plicities()[param_count..].to_vec();
                let args = params
                    .iter()
                    .cloned()
                    .chain(binders.iter().map(Term::free_var))
                    .collect::<Vec<_>>();

                // The arm's binders open one at a time, each joining the scope the next payload's type is read under.
                let mut opened = vec![(value.clone(), key.clone())];
                let mut pieces = Vec::new();
                signature
                    .telescope
                    .clone()
                    .walk(&args, |index, argument, type_| {
                        if index < param_count {
                            return Ok(());
                        }
                        let position = index - param_count;
                        if plicities[position] == Plicity::Explicit {
                            // An unlabeled payload carries the lowerer's `_{position}` hint, as an unlabeled field does.
                            let label = match labels[index] == format!("_{position}") {
                                true => "",
                                false => labels[index].as_str(),
                            };
                            let payload = Payload {
                                owner: name,
                                tag: Some(tag.as_str()),
                                label,
                                ordinal: position + 1,
                            };
                            let piece = spell_piece(
                                context,
                                &mut opened,
                                &payload,
                                argument,
                                type_,
                                span.clone(),
                            )?;
                            pieces.push(piece);
                        }
                        opened.push((binders[position].clone(), type_.clone()));
                        Ok::<(), Error>(())
                    })?;

                let items = list(context, pieces);
                let body = at(
                    syn_call(
                        syntax.spell.call,
                        [path_literal(&syntax, name, Some(tag.as_str())), items],
                    ),
                    span.clone(),
                );
                arms.push((
                    tag.clone(),
                    plicities.into_iter().zip(binders).collect::<Vec<_>>(),
                    body,
                ));
            }

            let motive = Term::match_motive_written(Term::hole(context.mint_metavar()));
            at(
                Term::induct_match_scoped_marked(scrutinee, motive, arms, None),
                span.clone(),
            )
        }
    };

    let method = Term::func([(value, Term::hole(context.mint_metavar()))], rendered);
    Ok(at(
        Term::struct_(concept.clone(), Vec::<Term>::new(), [method]),
        span.clone(),
    ))
}

/// Where a spelled payload sits, for the provenance a missing witness is reported under.
struct Payload<'a> {
    owner: &'a Global,
    tag: Option<&'a str>,
    label: &'a str,
    ordinal: usize,
}

impl Payload<'_> {
    fn constructor(&self) -> String {
        let qualifier = self
            .owner
            .qualifier()
            .expect("a declared type has the path it was declared at");
        match self.tag {
            Some(tag) => qualifier.with(tag).join(),
            None => qualifier.join(),
        }
    }

    fn describe(&self) -> String {
        let noun = match self.tag {
            Some(_) => "payload",
            None => "field",
        };
        match self.label.is_empty() {
            true => format!("{noun} #{}", self.ordinal),
            false => format!("{noun} '{}'", self.label),
        }
    }
}

/// The spelling of one explicit payload of type `type_`: the goal `"?"` for a proof, a refusal for a type, and `Spell/spell(use ?w, payload)` for a value — `?w` a witness goal born with this payload's provenance.
fn spell_piece(
    context: &mut Context,
    opened: &mut Vec<(Free, Term)>,
    payload: &Payload<'_>,
    value: &Term,
    type_: &Term,
    span: Option<Span>,
) -> Result<Term, Error> {
    let syntax = context.syntax();
    let reduced = reduce_with(context, type_)?;
    if matches!(&*reduced, Subterm::Type(_) | Subterm::Prop) {
        let concept = syntax.spell.spell.concept.qualifier();
        return Err(Error::underivable(
            Global::Authored(concept).symbol(),
            opened[0].1.clone(),
            Underivable::TypeValued {
                constructor: payload.constructor(),
                payload: payload.describe(),
            },
        )
        .at_opt(span.clone()));
    }

    let sort = Sort::of_in(context, opened, type_)
        .map_err(|error| Error::from_reduce(error, || Error::reduce_exhausted(type_.clone())))?;
    if matches!(sort, Sort::Prop) {
        return Ok(str_literal(&syntax.string, b"?"));
    }

    // A payload typed by a variable the arm did not bind is typed by the witness's telescope, and the premise that would spell it is the one to add.
    let telescope_variable = match &*reduced {
        Subterm::Var(var) => var
            .as_free()
            .is_some_and(|free| free.is_local() && !opened.iter().any(|(bound, _)| bound == free)),
        _ => false,
    };
    let binder = match telescope_variable {
        true => format!(
            "{} — add `use {}({})` to the telescope",
            payload.describe(),
            syntax.spell.spell.concept.last(),
            reduced
        ),
        false => payload.describe(),
    };
    let provenance = WitnessOrigin {
        func: payload.constructor(),
        binder,
    };
    // Spanned like the application around it: a goal deferred to the module's drain reports at the term it was born from, and that term is the declaration.
    let goal = at(
        Term::metavar_birthed(
            context.mint_metavar(),
            MetavarOrigin::Witness(provenance),
            Vec::new(),
        ),
        span.clone(),
    );
    let method = Term::free_var(&Free::global(
        syntax
            .spell
            .spell
            .concept
            .qualifier()
            .with(syntax.spell.spell.field),
    ));
    Ok(at(
        Term::apply_marked(
            method,
            [(Plicity::Witness, goal), (Plicity::Explicit, value.clone())],
        ),
        span.clone(),
    ))
}
