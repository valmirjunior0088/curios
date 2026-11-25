//! Derived witness bodies: the body `satisfy C(T) { .. }` asks the compiler to write.
//!
//! Lowering carries the declaration into Core as the same anonymous definition a written witness produces, with [`Transient::Derive`](curios_core::Transient) in body position, so the witness's telescope — its implicit binders and `use` premises — is in scope when the body is checked, and its signature registers in the witness table exactly as a written one does (orphan and duplicate-key refusals need no body). Checking the transient against the concept application is what writes the body: the roster row for the concept produces the Core the lowerer would have produced for the equivalent written witness, and that Core is elaborated under the same expectation, so a derived body is typed, resolved, zonked and certified like any authored one — the certifier never sees the transient.
//!
//! **The blessed set is one value.** Which concepts derive, and their specific method and renderer names, are one row of `curios_common::DerivationSyntax` — read here to pick the writer, and read by `curios-module`'s `order_flat_items` to supply the hard edges the transient hides. `Spell` also uses the registry's shared string vocabulary; both its writer and the scheduler receive that same entry.
//!
//! **Eligibility.** A derivation writes from a declaration, and only from one: the key must reduce to a registered `induct` or `struct` — not an intrinsic carrier, a tuple or function shape, or a concept's own record — that is representation-transparent at the declaring island, its parameters and indices given by the key. A proposition is no key: every concept with a derivation is over a `Type`, where none stands. Sealing is refused before any of that, with the rule a written literal meets, so that derivation is never a door through representation privacy; a concept with no derivation refuses by name, since derivability is registered per concept and never inferred from its shape. Every refusal is a hard error at the `satisfy` extent.
//!
//! **Payloads.** Every derivation reads a declaration the same way: the constructor telescopes (or the field telescope) opened at the key's parameters, one binder minted per payload, each classified by its type under the binders before it. A plain payload that is itself a type is refused, and a hidden one takes part as a plain one does where it is a value, written under its mark, and none where it is a type or a proof, which the text read back infers; a proof payload takes no part beyond what the derivation states for it; every other payload takes part through the concept's own method — `Spell/spell`, `Eql/eql`, `Ord/ord` — applied with a `use` argument the body supplies as a witness goal of its own — resolved by ordinary resolution in the witness's scope (a telescope premise, the witness's own entry, an `and` sibling), and reported unresolved under a provenance naming the constructor and the payload, with the telescope premise to add when the payload's type is a telescope variable. A field or payload the lowerer named `_{position}` had no written label, which is the one mark Core keeps of it.
//!
//! **The `Spell` body.** One match arm per constructor in declaration order, its goal a hole as a written match's is, and per arm a single renderer application over structured pieces: `Spell/call("Tree/node", [spell(l), …])` for a constructor, `Spell/record("Point", [("x", spell(x)), …])` for a struct, whose fields are projected. A value therefore spells as its type's name and constructor tag applied to its payloads, a hidden one under its mark (`Spell/hidden`), so the text re-parses wherever the type's name is visible unqualified; a struct spells labeled, or positionally where its field has no label; a proof payload spells as the written goal `"?"`.
//!
//! **The `Eql` body.** `eql` matches its two arguments in turn: an arm per constructor on the first, and inside it a one-arm match on the second at the same constructor — its payloads compared pairwise through `Eql/eql` under `&&`, `true` when there is nothing to compare — with a `| _ => false` default for every other constructor. A struct compares its projections the same way, with no match. Proofs do not take part. The two laws are proved of that comparison, each over the comparison built again, so each proof reads the method's own spelling by reduction: `eq_of_eql` by an arm for every pair of constructors ([`equal_where_eql`]), and `refl` by an arm a constructor ([`eql_to_itself`]), each payload's part through the concept's own law at that payload, a witness goal of its own as the method's is.
//!
//! **The `Ord` body.** The constructors decide before the payloads do, which that shape cannot express: its inner default is one *constant* for every mismatched tag, and a mismatched pair orders by which constructor is which. So `ord` matches each argument once for its constructor's declaration position and hands both ordinals, with the lockstep comparison, to `/std/Ord`'s `by_tag` — which answers the tag order unless they agree and the comparison otherwise. The lockstep half is then `compare`'s shape exactly, `Ordering/eq` filling a default the tag decision has already made unreachable, and the payload answers fold through `lexicographic` first-difference-wins. A proof contributes no piece, which under that fold is answering `eq`. Linear in the constructor count; an arm per pair would be quadratic. The concept's superclass slot is left unfilled, so the key's `Eql` witness is resolved as it would be for a written literal that omitted it. `lt` and `le` are `Ordering/is_lt` and `is_le` of that comparison built again, so the two laws that say so are `Eq/refl()`. The three laws of the order are proved of the comparison built again too, each payload's part through the concept's own law at that payload: `lexicographic` folds by a step, `Ordering/then`, whose laws carry `flips` ([`flipped_with_operands`]) and the chaining `transitive` closes on ([`carried_across`]) from one payload to the next, and whose two answers are `eq` where it is, which gives each payload's equation ([`equal_where_tied`]). An inductive goes through `by_tag`'s laws, which ask for the payloads' part where the two positions are one: under that equation the proof matches every value at one constructor, and two values at two constructors are closed by the equation itself ([`facing`]), so a law over three values is quadratic in the constructor count where an arm a triple would be cubic.

use {
    super::{Context, Error, Mode, Opened, Underivable, elaborate, method_type, reduce_with},
    crate::{is_prop_to, sort_of_in},
    curios_common::{
        ConceptField, Derivation, EqlSyntax, HashDerivation, InfixOp, OrdDerivation, Plicity,
        ProofSyntax, Qualifier, SpellDerivation, StringSyntax, Symbol, SyntaxName, SyntaxRegistry,
    },
    curios_core::{
        Atom, CalleeId, Extent, Free, Global, InductDecl, InductParam, InductType, Intrinsic,
        Mention, MetavarOrigin, Method, Numeral, StructDecl, StructEntry, StructType, Subterm,
        Term, WantedPremise, WitnessOrigin, WitnessRole, str_literal, syn_call,
    },
    curios_num::Natural,
    std::rc::Rc,
};

/// The roster row for `concept`, if it has one. A row carries the names its body writer applies, so that writer and `curios-module`'s scheduler read one value rather than two lists obliged to agree.
fn derivation_for<'r>(syntax: &'r SyntaxRegistry, concept: &Global) -> Option<Derivation<'r>> {
    let concept = *concept.qualifier()?;
    syntax
        .derivation_rows()
        .find(|derivation| derivation.concept_field().concept.qualifier() == concept)
}

/// Check a `Derive` transient against its expected type, writing the body the declaration asked for.
pub fn elaborate_derive(
    context: &mut Context,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let Mode::Check(expected) = &mode else {
        return Err(Error::derive_outside_witness().at_opt(term.extent()));
    };

    // The witness's declared type elaborated before its body, so the expectation reduces to the concept's record type — a `StructType` whose name is registered as a concept.
    let reduced = reduce_with(context, &expected.type_)?;
    let Subterm::StructType(StructType { name, params, .. }) = &*reduced else {
        return Err(Error::derive_outside_witness().at_opt(term.extent()));
    };
    if context.concept(name).is_none() {
        return Err(Error::derive_outside_witness().at_opt(term.extent()));
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
        return Err(Error::private_representation(*name).at_opt(term.extent()));
    }

    let syntax = context.syntax();
    let Some(derivation) = derivation_for(&syntax, name) else {
        return Err(Error::no_derivation(*name).at_opt(term.extent()));
    };
    let key = params
        .first()
        .cloned()
        .expect("a derivable concept takes the type it is derived for as its first parameter");
    // Reduced once, here, because this is the term every refusal *prints*. Reducing the whole concept application above unfolds a nullary inductive's name into its recursive definition, so a refusal of `satisfy Spell(T) { .. }` would print three lines of `rec #0: Type = T; #0` where the author wrote `T` — while a struct key (already a `StructType`) and a parameterized one (an `Apply` with a nominal head) print their names. One more reduction reaches the `InductType` all three share, which is the form every other diagnostic in the compiler prints.
    let key = reduce_with(context, &key)?;
    let site = Site {
        concept: name,
        key: &key,
        extent: term.extent(),
    };
    let subject = subject(context, &site)?;
    let body = match derivation {
        Derivation::Spell(row) => spell_body(
            context,
            &site,
            &subject,
            *row,
            &syntax.string,
            &syntax.proof,
        )?,
        Derivation::Eql(row) => eql_body(context, &site, &subject, *row)?,
        Derivation::Ord(row) => ord_body(context, &site, &subject, *row)?,
        Derivation::Hash(row) => hash_body(context, &site, &subject, *row)?,
    };
    elaborate(context, &body, mode)
}

/// The declaration a derivation is writing for: the concept, the key it is derived at, and the extent every refusal and goal reports at.
struct Site<'a> {
    concept: &'a Global,
    /// Reduced when the site was built, which is what makes it printable: an unreduced nullary inductive key is its recursion block, not its name. Every consumer wants that same form — [`subject`] classifies it, a body types its value at it, and a refusal prints it.
    key: &'a Term,
    extent: Option<Extent>,
}

impl Site<'_> {
    fn at(&self, term: Term) -> Term {
        match &self.extent {
            Some(extent) => term.with_extent(*extent),
            None => term,
        }
    }

    fn refuse(&self, reason: Underivable) -> Error {
        Error::underivable(*self.concept, self.key.clone(), reason).at_opt(self.extent)
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
    match &**site.key {
        Subterm::InductType(InductType {
            name,
            universes,
            params,
            ..
        }) => {
            let Some(decl) = context.induct_decl(name).cloned() else {
                return Err(site.refuse(Underivable::NotDeclared));
            };
            let decl = decl
                .universe_context()
                .check_instance(universes)
                .and_then(|()| decl.at(universes).whole())
                .map_err(Error::from)?;
            transparent(context, site, name, &decl.module(), decl.rep_public())?;
            Ok(Subject::Induct {
                name: *name,
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
            let decl = decl
                .universe_context
                .check_instance(universes)
                .and_then(|()| decl.at(universes).whole())
                .map_err(Error::from)?;
            transparent(context, site, name, &decl.module, decl.rep_public)?;
            Ok(Subject::Struct {
                name: *name,
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
        return Err(Error::private_representation(*name).at_opt(site.extent));
    }
    Ok(())
}

/// The path a declaration or constructor spells as: the type's own name and, for a constructor, its tag — `Ordering/lt`, `Point` — which re-parses wherever the type's name is visible unqualified, the one place a value of it can be written at all. An absolute path would re-parse everywhere but read as `/std/Ordering/Ordering/lt()` in every test report, and a report is where a spelled value is read.
fn path(name: &Global, tag: Option<&str>) -> String {
    let qualifier = name
        .qualifier()
        .expect("a declared type has the path it was declared at");
    let type_name = qualifier.last();
    match tag {
        Some(tag) => format!("{type_name}/{tag}"),
        None => type_name.to_string(),
    }
}

/// A list literal over `items`, its element type a hole the expected type solves — the shape the synthesized test tail already builds.
fn list(context: &mut Context, items: Vec<Term>) -> Term {
    Term::intrinsic(Intrinsic::List {
        element: Term::hole(context.mint_metavar()),
        items,
    })
}

/// A match's goal, as the lowerer carries one: a hole the mode decides.
fn goal(context: &mut Context) -> Term {
    Term::hole(context.mint_metavar())
}

/// Where a payload sits, for the provenance a missing witness is reported under and the label a struct field spells with.
struct Payload {
    /// The constructor's absolute path, or the struct's, as a refusal message spells it.
    constructor: String,
    /// The same declaration as an identity, for a provenance: the report then shortens the declaration under the names in scope and keeps the tag beside it.
    callee: CalleeId,
    /// The written label, empty where there was none.
    label: String,
    /// Its position among the plain payloads or fields, from one: what a report counts it by where it has no label.
    ordinal: usize,
    /// ``payload `x` ``, `field #2`: how a refusal of the derivation names it.
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
            false => format!("{noun} `{label}`"),
        };
        // A constructor's own path is its type's with the tag appended; a struct is named by its declaration directly.
        let callee = match tag {
            Some(tag) => CalleeId::Constructor {
                owner: *owner,
                tag: tag.to_string(),
            },
            None => CalleeId::Function(Mention::Declaration(*owner)),
        };
        Self {
            constructor: path(owner, tag),
            callee,
            label: label.to_string(),
            ordinal,
            described,
        }
    }
}

/// How one payload takes part in a derived body.
enum Part {
    /// Evidence: it erases, spells as the goal, and compares as nothing.
    Proof,
    /// A value, taking part through the concept's own witness — with the type to name in the premise to add when that type is a variable of the witness's telescope.
    Value { premise: Option<Term> },
}

/// One payload that takes part, classified: its position among the payloads (or fields), where it sits, what it is, and whether it is hidden, which `Spell` writes under its mark.
struct Classified {
    position: usize,
    payload: Payload,
    part: Part,
    hidden: bool,
}

/// Classify one payload of type `type_`, read under the binders in `opened`, as a plain one takes part.
fn classify(
    context: &mut Context,
    site: &Site<'_>,
    opened: &mut Opened,
    position: usize,
    payload: Payload,
    type_: &Term,
) -> Result<Classified, Error> {
    let reduced = reduce_with(context, type_)?;
    if matches!(&*reduced, Subterm::Sort(_)) {
        return Err(site.refuse(Underivable::TypeValued {
            constructor: payload.constructor,
            payload: payload.described,
        }));
    }

    let sort = sort_of_in(context, opened, type_).map_err(|error| {
        Error::from_reduce(error, |refusal| {
            Error::reduce_exhausted(type_.clone(), refusal)
        })
    })?;
    if is_prop_to(&sort, context, "derive::classify") {
        return Ok(Classified {
            position,
            payload,
            part: Part::Proof,
            hidden: false,
        });
    }

    // A payload typed by a variable the arm did not bind is typed by the witness's telescope, and the premise that would cover it is the one to add.
    let telescope_variable = match &*reduced {
        Subterm::Var(var) => var.as_free().is_some_and(|free| !opened.binds(free)),
        _ => false,
    };
    Ok(Classified {
        position,
        payload,
        part: Part::Value {
            premise: telescope_variable.then(|| reduced.clone()),
        },
        hidden: false,
    })
}

/// Classify one member of type `type_` under its mark `mark`: a plain one as [`classify`] does, and a hidden one as a plain one where it is a value. A hidden member that is a type or a proof takes no part, the text read back inferring it.
fn member(
    context: &mut Context,
    site: &Site<'_>,
    opened: &mut Opened,
    (position, payload): (usize, Payload),
    type_: &Term,
    mark: Plicity,
) -> Result<Option<Classified>, Error> {
    if mark == Plicity::Explicit {
        return classify(context, site, opened, position, payload, type_).map(Some);
    }
    if matches!(&*reduce_with(context, type_)?, Subterm::Sort(_)) {
        return Ok(None);
    }
    let classified = classify(context, site, opened, position, payload, type_)?;

    Ok(match classified.part {
        Part::Proof => None,
        Part::Value { .. } => Some(Classified {
            hidden: true,
            ..classified
        }),
    })
}

/// One constructor of the declaration a derivation is eliminating, at the key's parameters.
struct Constructor<'a> {
    owner: &'a Global,
    tag: &'a str,
    signature: &'a InductParam,
    params: &'a [Term],
}

impl Constructor<'_> {
    /// One binder per payload, hinted by the written label where there is one.
    fn binders(&self, context: &mut Context) -> Vec<Free> {
        self.signature
            .payload()
            .body()
            .hints()
            .iter()
            .map(|hint| context.fresh(*hint))
            .collect()
    }

    /// The payloads' plicities, one per binder.
    fn plicities(&self) -> Vec<Plicity> {
        self.signature.plicities()
    }

    /// The payloads that take part, opened at `binders` — one per payload, after the declaration's parameters — each classified under its mark with its type read under the binders before it ([`member`]). `value` is the scrutinee, the one binder in scope above the arm.
    fn parts(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        binders: &[Free],
        value: &Free,
    ) -> Result<Vec<Classified>, Error> {
        let labels = self
            .signature
            .payload()
            .body()
            .labels()
            .into_iter()
            .map(str::to_string)
            .collect::<Vec<_>>();
        let plicities = self.signature.plicities();
        let args = binders.iter().map(Term::free_var).collect::<Vec<_>>();

        let mut opened = Opened::default();
        opened.push(*value, site.key.clone());
        let mut parts = Vec::new();
        self.signature
            .open(self.params)
            .walk(&args, |position, _, type_| {
                let label = labels[position].as_str();
                let payload = Payload::new(self.owner, Some(self.tag), label, position + 1);
                parts.extend(member(
                    context,
                    site,
                    &mut opened,
                    (position, payload),
                    type_,
                    plicities[position],
                )?);
                opened.push(binders[position], type_.clone());
                Ok::<(), Error>(())
            })?;
        Ok(parts)
    }
}

/// The concept's literal of `methods`, written as an author writes one: its plain entries in order, every superclass edge left to resolution.
fn dictionary<const N: usize>(site: &Site<'_>, methods: [Term; N]) -> Term {
    Term::struct_entries(
        *site.concept,
        Vec::<Term>::new(),
        methods
            .into_iter()
            .map(|method| (StructEntry::Field(None), method)),
    )
}

/// The fields of a struct at `params` that take part, each classified under its mark with its type read through the projections off `value` before it ([`member`]): a hidden field as a hidden payload is.
fn struct_parts(
    context: &mut Context,
    site: &Site<'_>,
    name: &Global,
    decl: &StructDecl,
    params: &[Term],
    value: &Free,
) -> Result<Vec<Classified>, Error> {
    let width = decl
        .arity
        .clone()
        .open(&params.iter().collect::<Vec<_>>())
        .len();
    let projections = (0..width)
        .map(|index| Term::proj_struct(Term::free_var(value), *name, index))
        .collect::<Vec<_>>();
    let mut opened = Opened::default();
    opened.push(*value, site.key.clone());

    struct_parts_read(
        context,
        site,
        (name, decl, params),
        &projections,
        opened,
        None,
    )
}

/// [`struct_parts`] with a binder standing for each field, hidden ones included, where a body is written over the fields and no value of the struct is in scope: each field's type read under the binders before it, as a constructor's payloads are ([`Constructor::parts`]). Answers each field's type beside the parts, the type its binder is bound at.
fn struct_parts_over(
    context: &mut Context,
    site: &Site<'_>,
    name: &Global,
    decl: &StructDecl,
    params: &[Term],
    binders: &[Free],
) -> Result<(Vec<Classified>, Vec<Term>), Error> {
    let fields = binders.iter().map(Term::free_var).collect::<Vec<_>>();
    let mut types = Vec::new();
    let parts = struct_parts_read(
        context,
        site,
        (name, decl, params),
        &fields,
        Opened::default(),
        Some((binders, &mut types)),
    )?;

    Ok((parts, types))
}

/// The walk [`struct_parts`] and [`struct_parts_over`] share: the struct's field telescope opened at `fields`, the terms its fields are read as, each classified under `opened`. `bound` is the binders those terms are, where they are binders, each entered in scope at its field's type once it is classified, with the types kept.
fn struct_parts_read(
    context: &mut Context,
    site: &Site<'_>,
    (name, decl, params): (&Global, &StructDecl, &[Term]),
    fields: &[Term],
    mut opened: Opened,
    mut bound: Option<(&[Free], &mut Vec<Term>)>,
) -> Result<Vec<Classified>, Error> {
    let telescope = decl.arity.clone().open(&params.iter().collect::<Vec<_>>());
    let labels = telescope
        .labels()
        .into_iter()
        .map(str::to_string)
        .collect::<Vec<_>>();

    let marks = telescope.marks();
    let mut parts = Vec::new();
    let mut ordinal = 0;
    telescope.walk(fields, |index, _, type_| {
        // A field is described by its place among the plain fields, which is what a literal counts.
        if marks[index] == Plicity::Explicit {
            ordinal += 1;
        }
        let label = labels[index].as_str();
        let payload = Payload::new(name, None, label, ordinal);
        parts.extend(member(
            context,
            site,
            &mut opened,
            (index, payload),
            type_,
            marks[index],
        )?);
        if let Some((binders, types)) = &mut bound {
            opened.push(binders[index], type_.clone());
            types.push(type_.clone());
        }
        Ok::<(), Error>(())
    })?;
    Ok(parts)
}

/// The witness of `concept` a payload's call takes: a goal born with the payload's provenance, so a missing witness is reported against the constructor and the payload.
fn witness_goal(
    context: &mut Context,
    site: &Site<'_>,
    concept: SyntaxName,
    classified: &Classified,
    premise: Option<&Term>,
) -> Term {
    let payload = &classified.payload;
    let role = WitnessRole::Payload {
        label: (!payload.label.is_empty()).then(|| Symbol::new(&payload.label)),
        ordinal: payload.ordinal,
        premise: premise.map(|variable| {
            Rc::new(WantedPremise {
                concept: Global::Authored(concept.qualifier()),
                variable: variable.clone(),
            })
        }),
    };
    let provenance = WitnessOrigin {
        func: payload.callee.clone(),
        role: Some(role),
    };
    // Placed like the application around it: a goal deferred to the module's drain reports at the term it was born from, and that term is the declaration.
    site.at(Term::metavar_birthed(
        context.mint_metavar(),
        MetavarOrigin::Witness(provenance),
        Vec::new(),
    ))
}

/// `lemma(@_, use ?w, args…)`: a lemma over a witness of `concept` at one type, which it takes ahead of its arguments as a method does, applied at a payload with the goal a method's call there takes ([`witness_goal`]).
fn lemma_call(
    context: &mut Context,
    site: &Site<'_>,
    (lemma, concept): (SyntaxName, SyntaxName),
    classified: &Classified,
    premise: Option<&Term>,
    args: Vec<Term>,
) -> Term {
    let goal = witness_goal(context, site, concept, classified, premise);
    let arguments = [
        (Plicity::Implicit, Term::hole(context.mint_metavar())),
        (Plicity::Witness, goal),
    ]
    .into_iter()
    .chain(args.into_iter().map(|arg| (Plicity::Explicit, arg)))
    .collect::<Vec<_>>();
    site.at(Term::apply_marked(
        Term::written(Global::Authored(lemma.qualifier())),
        arguments,
    ))
}

/// `C/method(@_, …, use ?w, args…)`: the concept's method applied to `args`, its `use` argument a witness goal born with the payload's provenance — how a missing witness is reported against the constructor and payload rather than against the method. A method's call takes the concept's parameters ahead of its witness, and hidden members are written in order from the first of their run, so each parameter is written as the placeholder that leaves it to the elaborator.
fn witness_call(
    context: &mut Context,
    site: &Site<'_>,
    field: ConceptField,
    classified: &Classified,
    premise: Option<&Term>,
    args: Vec<Term>,
) -> Term {
    let goal = witness_goal(context, site, field.concept, classified, premise);
    let parameters = match method_type(context, &Method::from(field)).as_deref() {
        Some(Subterm::FuncType(function)) => function
            .plicities()
            .iter()
            .take_while(|mark| **mark == Plicity::Implicit)
            .count(),
        _ => 0,
    };
    let method = Term::registered_method(field);
    let arguments = (0..parameters)
        .map(|_| (Plicity::Implicit, Term::hole(context.mint_metavar())))
        .chain(std::iter::once((Plicity::Witness, goal)))
        .chain(args.into_iter().map(|arg| (Plicity::Explicit, arg)))
        .collect::<Vec<_>>();
    site.at(Term::apply_marked(method, arguments))
}

/// The `Spell` witness record: `spell` as a one-parameter function over the derived rendering.
fn spell_body(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: SpellDerivation,
    string: &StringSyntax,
    proof: &ProofSyntax,
) -> Result<Term, Error> {
    let value = context.fresh(None);

    // The spelling of one classified payload read through `read`.
    let spell = |context: &mut Context, classified: &Classified, read: Term| match &classified.part
    {
        Part::Proof => str_literal(string, proof, b"?"),
        Part::Value { premise } => witness_call(
            context,
            site,
            row.spell,
            classified,
            premise.as_ref(),
            vec![read],
        ),
    };

    let rendered = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &value)?;
            let entries = parts
                .iter()
                .map(|classified| {
                    let read =
                        Term::proj_struct(Term::free_var(&value), *name, classified.position);
                    let spelled = spell(context, classified, read);
                    let label = &classified.payload.label;
                    // A hidden field is written under its mark: `@label = value` where it has a label, `@value` where it has none.
                    let (label, spelled) = match (classified.hidden, label.is_empty()) {
                        (true, false) => (format!("@{label}"), spelled),
                        (true, true) => (String::new(), syn_call(row.hidden, [spelled])),
                        (false, _) => (label.clone(), spelled),
                    };
                    Term::tuple([str_literal(string, proof, label.as_bytes()), spelled])
                })
                .collect::<Vec<_>>();
            let items = list(context, entries);
            let head = str_literal(string, proof, path(name, None).as_bytes());
            site.at(syn_call(row.record, [head, items]))
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in decl.constructors() {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    params,
                };
                let binders = constructor.binders(context);
                let parts = constructor.parts(context, site, &binders, &value)?;
                let pieces = parts
                    .iter()
                    .map(|classified| {
                        let read = Term::free_var(&binders[classified.position]);
                        let spelled = spell(context, classified, read);
                        match classified.hidden {
                            true => syn_call(row.hidden, [spelled]),
                            false => spelled,
                        }
                    })
                    .collect::<Vec<_>>();
                let items = list(context, pieces);
                let head = str_literal(string, proof, path(name, Some(tag.as_str())).as_bytes());
                let body = site.at(syn_call(row.call, [head, items]));
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

            let goal = goal(context);
            site.at(Term::induct_match_marked(
                Term::free_var(&value),
                goal,
                arms,
                None,
            ))
        }
    };

    let method = Term::func([(value, Term::hole(context.mint_metavar()))], rendered);
    Ok(site.at(dictionary(site, [method])))
}

/// The `Hash` witness record: `hash` over the derived encoding.
///
/// One arm per constructor, each `Hash/tagged(ordinal, [hash(p₀), …])` — the constructor's position followed by the encodings of its payloads that take part, every part framed by `tagged` so that no two distinct values share a byte string. A struct has one shape and takes ordinal zero.
///
/// **A proof payload is omitted rather than encoded.** `Spell` renders one as `"?"` because a reader wants to see it; here two values differing only in a proof *are* the same value, so giving them different encodings would be wrong — and erasure drops the payload anyway, so nothing could produce the bytes at run time.
fn hash_body(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: HashDerivation,
) -> Result<Term, Error> {
    let value = context.fresh(None);

    // The encoding of one classified payload read through `read`, or nothing where the payload is a proof.
    let encode = |context: &mut Context, classified: &Classified, read: Term| match &classified.part
    {
        Part::Proof => None,
        Part::Value { premise } => Some(witness_call(
            context,
            site,
            row.hash,
            classified,
            premise.as_ref(),
            vec![read],
        )),
    };

    let ordinal = |index: usize| Term::num_lit(Numeral::Natural(Natural::from(index)));

    let encoded = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &value)?;
            let pieces = parts
                .iter()
                .filter_map(|classified| {
                    let read =
                        Term::proj_struct(Term::free_var(&value), *name, classified.position);
                    encode(context, classified, read)
                })
                .collect::<Vec<_>>();
            let items = list(context, pieces);
            site.at(syn_call(row.tagged, [ordinal(0), items]))
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (index, (tag, signature)) in decl.constructors().iter().enumerate() {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    params,
                };
                let binders = constructor.binders(context);
                let parts = constructor.parts(context, site, &binders, &value)?;
                let pieces = parts
                    .iter()
                    .filter_map(|classified| {
                        let read = Term::free_var(&binders[classified.position]);
                        encode(context, classified, read)
                    })
                    .collect::<Vec<_>>();
                let items = list(context, pieces);
                let body = site.at(syn_call(row.tagged, [ordinal(index), items]));
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

            let goal = goal(context);
            site.at(Term::induct_match_marked(
                Term::free_var(&value),
                goal,
                arms,
                None,
            ))
        }
    };

    let method = Term::func([(value, Term::hole(context.mint_metavar()))], encoded);
    Ok(site.at(dictionary(site, [method])))
}

/// The `Eql` witness record: `eql` over the derived comparison, and its two laws, each proved through its payloads' own.
fn eql_body(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: EqlSyntax,
) -> Result<Term, Error> {
    let left = context.fresh(None);
    let right = context.fresh(None);
    let compared = compare(context, site, subject, row, &left, &right)?;
    let eql = Term::func(
        [
            (left, Term::hole(context.mint_metavar())),
            (right, Term::hole(context.mint_metavar())),
        ],
        compared,
    );
    let eq_of_eql = equal_where_eql(context, site, subject, row)?;
    let refl = eql_to_itself(context, site, subject, row)?;

    Ok(site.at(dictionary(site, [eql, eq_of_eql, refl])))
}

/// The comparisons `eql` joins for one constructor or one struct: the parts that are values, in order, and the two terms each is compared at. A proof takes no part.
struct Compared<'c> {
    row: EqlSyntax,
    values: Vec<(&'c Classified, Option<&'c Term>)>,
    reads: &'c dyn Fn(usize) -> (Term, Term),
}

impl<'c> Compared<'c> {
    fn new(
        row: EqlSyntax,
        parts: &'c [Classified],
        reads: &'c dyn Fn(usize) -> (Term, Term),
    ) -> Self {
        let values = parts
            .iter()
            .filter_map(|classified| match &classified.part {
                Part::Proof => None,
                Part::Value { premise } => Some((classified, premise.as_ref())),
            })
            .collect();

        Self { row, values, reads }
    }

    /// The two terms the value at `index` is compared at.
    fn pair(&self, index: usize) -> (Term, Term) {
        (self.reads)(self.values[index].0.position)
    }

    /// The concept's `field` applied at the value at `index` over `args`, its witness goal born at this call: a comparison written twice is two goals, so none stands under two scopes.
    fn applied(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        field: ConceptField,
        index: usize,
        args: Vec<Term>,
    ) -> Term {
        let (classified, premise) = self.values[index];
        witness_call(context, site, field, classified, premise, args)
    }

    /// The comparison of the value at `index`, as `eql` writes it.
    fn comparison(&self, context: &mut Context, site: &Site<'_>, index: usize) -> Term {
        let (this, that) = self.pair(index);
        self.applied(context, site, self.row.eql, index, vec![this, that])
    }

    /// The first `count` comparisons joined under `&&`, the leftmost innermost, `count` at least one: the spelling [`Compared::joined`] gives the whole, which the laws' proofs take apart and put together a conjunct at a time.
    fn conjunction(&self, context: &mut Context, site: &Site<'_>, count: usize) -> Term {
        let mut joined = self.comparison(context, site, 0);
        for index in 1..count {
            let next = self.comparison(context, site, index);
            joined = site.at(Term::infix(InfixOp::And, joined, next));
        }

        joined
    }

    /// Every comparison joined, `true` where there is nothing to compare.
    fn joined(&self, context: &mut Context, site: &Site<'_>) -> Term {
        match self.values.len() {
            0 => Term::intrinsic(Intrinsic::Bool(true)),
            count => self.conjunction(context, site, count),
        }
    }
}

/// Whether `left` and `right` are equal, as a `Bool`-valued term over the two binders.
fn compare(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: EqlSyntax,
    left: &Free,
    right: &Free,
) -> Result<Term, Error> {
    match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, left)?;
            let reads = |index: usize| {
                (
                    Term::proj_struct(Term::free_var(left), *name, index),
                    Term::proj_struct(Term::free_var(right), *name, index),
                )
            };

            Ok(Compared::new(row, &parts, &reads).joined(context, site))
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in decl.constructors() {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    params,
                };
                let lefts = constructor.binders(context);
                let rights = constructor.binders(context);
                let parts = constructor.parts(context, site, &lefts, left)?;
                let reads = |position: usize| {
                    (
                        Term::free_var(&lefts[position]),
                        Term::free_var(&rights[position]),
                    )
                };
                let compared = Compared::new(row, &parts, &reads).joined(context, site);
                let plicities = constructor.plicities();

                // The inner match names the one constructor the outer arm already committed to; every other tag is the default.
                let inner = site.at(Term::induct_match_marked(
                    Term::free_var(right),
                    goal(context),
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

            let goal = goal(context);
            Ok(site.at(Term::induct_match_marked(
                Term::free_var(left),
                goal,
                arms,
                None,
            )))
        }
    }
}

/// `eq_of_eql`: that two values `eql` holds equal are one, as a function of the two and of the proof that it does.
///
/// **An inductive is matched on both sides, an arm for every pair of constructors.** At two different constructors `eql` answered `false`, so the hypothesis reduces to a proposition nothing proves and the arm is its zero-arm match; at the same constructor the comparisons hold part by part ([`equated`]). A default cannot stand for the mismatched arms as it does in `eql`: the hypothesis reduces only where the second constructor is known.
///
/// **A struct's proof is a congruence written over its fields and applied at the projections.** A struct has no arm to bind its fields, and an equation between two projections solves nothing, so the proof is a helper over a binder for each field of each side and an equation for each field that is a value, which states that the two literals are one, applied at the two values' projections, where eta holds a value one with the literal of its projections. The concept's law is applied outside it, at the projections: a field is below its record, so a family nested through the struct descends through the law as it does through `eql`, which a call at the helper's own binders would not show.
fn equal_where_eql(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: EqlSyntax,
) -> Result<Term, Error> {
    let left = context.fresh(None);
    let right = context.fresh(None);
    let held = context.fresh(None);

    let proof = match subject {
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in decl.constructors() {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    params,
                };
                let lefts = constructor.binders(context);
                let parts = constructor.parts(context, site, &lefts, &left)?;

                let mut facing = Vec::new();
                for (other, other_signature) in decl.constructors() {
                    let faced = Constructor {
                        owner: name,
                        tag: other.as_str(),
                        signature: other_signature,
                        params,
                    };
                    let rights = faced.binders(context);
                    let body = match other == tag {
                        true => {
                            let reads = |position: usize| {
                                (
                                    Term::free_var(&lefts[position]),
                                    Term::free_var(&rights[position]),
                                )
                            };
                            equated(context, site, &Compared::new(row, &parts, &reads), &held)
                        }
                        false => refuted(context, site, &held),
                    };
                    facing.push((
                        other.clone(),
                        faced
                            .plicities()
                            .into_iter()
                            .zip(rights)
                            .collect::<Vec<_>>(),
                        body,
                    ));
                }

                let inner = site.at(Term::induct_match_marked(
                    Term::free_var(&right),
                    goal(context),
                    facing,
                    None,
                ));
                arms.push((
                    tag.clone(),
                    constructor
                        .plicities()
                        .into_iter()
                        .zip(lefts)
                        .collect::<Vec<_>>(),
                    inner,
                ));
            }

            let goal = goal(context);
            site.at(Term::induct_match_marked(
                Term::free_var(&left),
                goal,
                arms,
                None,
            ))
        }
        Subject::Struct { name, decl, params } => {
            // The laws, each applied at the two values' own projections.
            let parts = struct_parts(context, site, name, decl, params, &left)?;
            let reads = |index: usize| {
                (
                    Term::proj_struct(Term::free_var(&left), *name, index),
                    Term::proj_struct(Term::free_var(&right), *name, index),
                )
            };
            let equal = equations(context, site, &Compared::new(row, &parts, &reads), &held);
            congruent(context, site, (name, decl, params), (&left, &right), equal)?
        }
    };

    Ok(Term::func(
        [
            (left, Term::hole(context.mint_metavar())),
            (right, Term::hole(context.mint_metavar())),
            (held, Term::hole(context.mint_metavar())),
        ],
        proof,
    ))
}

/// That two values of a struct are one, from an equation for each field that is a value, in the fields' order, under the proofs `bound` binds.
///
/// The proof is a helper over a binder for each field of each side and an equation for each field that is a value, which states that the two literals are one and matches each equation in turn ([`matched`]), applied at the two values' projections, where eta holds a value one with the literal of its projections. The equations are written outside it, at the projections.
fn congruent(
    context: &mut Context,
    site: &Site<'_>,
    (name, decl, params): (&Global, &StructDecl, &[Term]),
    (left, right): (&Free, &Free),
    (bound, equations): (Vec<(Free, Term)>, Vec<Term>),
) -> Result<Term, Error> {
    let Subterm::StructType(StructType { universes, .. }) = &**site.key else {
        unreachable!("a struct is derived for at a struct type");
    };
    let width = decl
        .arity
        .clone()
        .open(&params.iter().collect::<Vec<_>>())
        .len();
    let lefts = (0..width).map(|_| context.fresh(None)).collect::<Vec<_>>();
    let rights = (0..width).map(|_| context.fresh(None)).collect::<Vec<_>>();
    let literal = |fields: &[Free]| {
        Term::struct_at(
            *name,
            universes.clone(),
            params.to_vec(),
            fields.iter().map(Term::free_var).collect::<Vec<_>>(),
        )
    };

    // Each field's type under the fields before it, and the other side's the same types at its own binders.
    let (fields, left_types) = struct_parts_over(context, site, name, decl, params, &lefts)?;
    let renamed = lefts
        .iter()
        .zip(&rights)
        .map(|(left, right)| (*left, Term::free_var(right)))
        .collect::<Vec<_>>();
    let right_types = left_types.iter().map(|type_| type_.substitute(&renamed));
    let family = context.syntax().equality.family;
    let equal =
        |this: Term, that: Term| site.at(Term::apply(syn_call(family, []), vec![this, that]));

    // The helper's type takes an equation for each field that is a value and states the two literals one, and its body, a lambda checked against that type, matches each equation in turn.
    let premises = fields
        .iter()
        .filter(|classified| matches!(classified.part, Part::Value { .. }))
        .map(|classified| {
            let equation = equal(
                Term::free_var(&lefts[classified.position]),
                Term::free_var(&rights[classified.position]),
            );
            (context.fresh(None), equation)
        })
        .collect::<Vec<_>>();
    let signature = Term::func_type(
        lefts
            .iter()
            .copied()
            .zip(left_types.iter().cloned())
            .chain(rights.iter().copied().zip(right_types))
            .chain(premises.iter().cloned())
            .collect::<Vec<_>>(),
        equal(literal(&lefts), literal(&rights)),
    );
    let closing = reflexive(context, site);
    let proved = matched(
        context,
        site,
        premises
            .iter()
            .map(|(premise, _)| Term::free_var(premise))
            .collect(),
        closing,
    );
    let binders = lefts
        .iter()
        .chain(&rights)
        .chain(premises.iter().map(|(premise, _)| premise))
        .map(|binder| (*binder, Term::hole(context.mint_metavar())))
        .collect::<Vec<_>>();

    let projections = |value: &Free| {
        (0..width)
            .map(|index| Term::proj_struct(Term::free_var(value), *name, index))
            .collect::<Vec<_>>()
    };
    let helper = context.fresh(None);
    let applied = site.at(Term::apply(
        Term::free_var(&helper),
        projections(left)
            .into_iter()
            .chain(projections(right))
            .chain(equations)
            .collect::<Vec<_>>(),
    ));

    Ok(Term::let_(
        &helper,
        signature,
        Term::func(binders, proved),
        bound_around(context, bound, applied),
    ))
}

/// The zero-arm match on `held`: the arm of two constructors `eql` holds apart, where the hypothesis reduces to a proposition with no proof.
fn refuted(context: &mut Context, site: &Site<'_>, held: &Free) -> Term {
    site.at(Term::induct_match_marked(
        Term::free_var(held),
        goal(context),
        Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
        None,
    ))
}

/// A proof that the two sides `compared` reads are one value, from `held`, a proof that every comparison holds: each part's equation ([`equations`]) matched in turn ([`matched`]).
fn equated(context: &mut Context, site: &Site<'_>, compared: &Compared<'_>, held: &Free) -> Term {
    let (bound, equations) = equations(context, site, compared, held);
    let closing = reflexive(context, site);
    let proof = matched(context, site, equations, closing);

    bound_around(context, bound, proof)
}

/// Each part's equation from `held`, a proof that every comparison of `compared` holds, in the parts' order, beside the proofs it binds on the way, outermost first: the concept's own law at a part gives that part's equation from its comparison.
///
/// **The hypothesis is taken apart a conjunct at a time.** `eql` joins the comparisons leftmost innermost, so each shorter prefix is the first conjunct of the next, bound once, and a part's own comparison is the second conjunct of its prefix.
fn equations(
    context: &mut Context,
    site: &Site<'_>,
    compared: &Compared<'_>,
    held: &Free,
) -> (Vec<(Free, Term)>, Vec<Term>) {
    let row = compared.row;
    let conjunction = context.syntax().conjunction;
    let count = compared.values.len();

    // `prefixes[n]` proves the first `count - n` comparisons joined.
    let mut prefixes = vec![*held];
    let mut bound = Vec::new();
    for length in (1..count).rev() {
        let longer = Term::free_var(&prefixes[prefixes.len() - 1]);
        let shorter = compared.conjunction(context, site, length);
        let last = compared.comparison(context, site, length);
        let binder = context.fresh(None);
        bound.push((
            binder,
            site.at(syn_call(conjunction.first, [shorter, last, longer])),
        ));
        prefixes.push(binder);
    }

    let equations = (0..count)
        .map(|index| {
            let prefix = Term::free_var(&prefixes[count - index - 1]);
            let evidence = match index {
                0 => prefix,
                _ => {
                    let before = compared.conjunction(context, site, index);
                    let own = compared.comparison(context, site, index);
                    site.at(syn_call(conjunction.second, [before, own, prefix]))
                }
            };
            let (this, that) = compared.pair(index);
            compared.applied(
                context,
                site,
                row.eq_of_eql,
                index,
                vec![this, that, evidence],
            )
        })
        .collect();

    (bound, equations)
}

/// `body` under the proofs `bound` binds, outermost first, each at a type its value gives.
fn bound_around(context: &mut Context, bound: Vec<(Free, Term)>, body: Term) -> Term {
    bound.into_iter().rev().fold(body, |body, (binder, value)| {
        Term::let_(&binder, Term::hole(context.mint_metavar()), value, body)
    })
}

/// **Each equation is matched to `refl()` in turn, and `closing` closes.** Matching a part's equation solves the second side's part to the first's, so the parts after it, whose types may name it, stand at one type, and once every part is solved the two sides are one term, which `closing` is checked at. A proof part needs no equation: two proofs of one proposition are one.
fn matched(context: &mut Context, site: &Site<'_>, equations: Vec<Term>, closing: Term) -> Term {
    let same = context.syntax().equality.refl;
    let mut proof = closing;
    for equation in equations.into_iter().rev() {
        // The constructor's one payload is hidden, the value both sides are.
        let payload = context.fresh(None);
        proof = site.at(Term::induct_match_marked(
            equation,
            goal(context),
            [(
                Atom::from(same.qualifier().last()),
                vec![(Plicity::Implicit, payload)],
                proof,
            )],
            None,
        ));
    }

    proof
}

/// `Eq/refl()`: what a proof that two values are one closes on, once every part's equation is matched.
fn reflexive(context: &Context, site: &Site<'_>) -> Term {
    site.at(syn_call(context.syntax().equality.refl, []))
}

/// `refl`: that a value is `eql` to itself, as a function of the value. An inductive is matched once, an arm a constructor; a struct reads its projections.
fn eql_to_itself(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: EqlSyntax,
) -> Result<Term, Error> {
    let value = context.fresh(None);

    let proof = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &value)?;
            let reads = |index: usize| {
                let field = Term::proj_struct(Term::free_var(&value), *name, index);
                (field.clone(), field)
            };

            held_of_itself(context, site, &Compared::new(row, &parts, &reads))
        }
        Subject::Induct { name, decl, params } => {
            let mut arms = Vec::new();
            for (tag, signature) in decl.constructors() {
                let constructor = Constructor {
                    owner: name,
                    tag: tag.as_str(),
                    signature,
                    params,
                };
                let binders = constructor.binders(context);
                let parts = constructor.parts(context, site, &binders, &value)?;
                let reads = |position: usize| {
                    let payload = Term::free_var(&binders[position]);
                    (payload.clone(), payload)
                };
                let proved = held_of_itself(context, site, &Compared::new(row, &parts, &reads));
                arms.push((
                    tag.clone(),
                    constructor
                        .plicities()
                        .into_iter()
                        .zip(binders)
                        .collect::<Vec<_>>(),
                    proved,
                ));
            }

            let goal = goal(context);
            site.at(Term::induct_match_marked(
                Term::free_var(&value),
                goal,
                arms,
                None,
            ))
        }
    };

    Ok(Term::func(
        [(value, Term::hole(context.mint_metavar()))],
        proof,
    ))
}

/// A proof that every comparison of `compared` holds where both sides are one term: each part's by the concept's own law at that part, joined a conjunct at a time as `eql` joins the comparisons, and `True/qed()` where there is nothing to compare.
fn held_of_itself(context: &mut Context, site: &Site<'_>, compared: &Compared<'_>) -> Term {
    let row = compared.row;
    let syntax = context.syntax();
    let count = compared.values.len();
    if count == 0 {
        return site.at(syn_call(syntax.proof.true_qed, []));
    }

    let own = |context: &mut Context, index: usize| {
        let (this, _) = compared.pair(index);
        compared.applied(context, site, row.refl, index, vec![this])
    };
    let mut proof = own(context, 0);
    for index in 1..count {
        let before = compared.conjunction(context, site, index);
        let last = compared.comparison(context, site, index);
        let next = own(context, index);
        proof = site.at(syn_call(
            syntax.conjunction.both,
            [before, last, proof, next],
        ));
    }

    proof
}

/// The `Ord` witness record: `ord` over the derived comparison; `lt` and `le` its `Ordering/is_lt` and `is_le`, each the comparison built again over binders of its own, so each agrees with `ord` by reduction, which is the whole of the two laws that say so; and the order's three laws, each proved through its payloads' own.
///
/// The superclass `Eql` slot is deliberately left unfilled — `elaborate_struct` pairs written fields with the plain positions and mints a resolution goal for every `use` position left over, so the key's own equality witness is found exactly as it would be for a written literal that omitted it.
fn ord_body(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: OrdDerivation,
) -> Result<Term, Error> {
    let method = |context: &mut Context, read: Option<SyntaxName>| -> Result<Term, Error> {
        let left = context.fresh(None);
        let right = context.fresh(None);
        let ordered = order(context, site, subject, row, &left, &right)?;
        let body = match read {
            None => ordered,
            Some(read) => site.at(syn_call(read, [ordered])),
        };
        Ok(Term::func(
            [
                (left, Term::hole(context.mint_metavar())),
                (right, Term::hole(context.mint_metavar())),
            ],
            body,
        ))
    };
    // `lt` and `le` are `ord` read through `is_lt` and `is_le`, so each side of the law that says so is one term.
    let agreed = |context: &mut Context| {
        let same = reflexive(context, site);
        ignoring(context, 2, same)
    };

    let ord = method(context, None)?;
    let lt = method(context, Some(row.is_lt))?;
    let le = method(context, Some(row.is_le))?;
    let flips = flipped_with_operands(context, site, subject, row)?;
    let transitive = carried_across(context, site, subject, row)?;
    let eq_of_ord = equal_where_tied(context, site, subject, row)?;
    let lt_of_ord = agreed(context);
    let le_of_ord = agreed(context);
    Ok(site.at(dictionary(
        site,
        [
            ord, lt, le, flips, transitive, eq_of_ord, lt_of_ord, le_of_ord,
        ],
    )))
}

/// One side of a comparison: the term a payload or a field is read as, by its position.
type Side<'a> = &'a dyn Fn(usize) -> Term;

/// An inductive a derivation is eliminating, at the key's parameters.
type Family<'a> = (&'a Global, &'a InductDecl, &'a [Term]);

/// The side that reads a struct's fields off `value`.
fn projected<'a>(value: &'a Free, name: &'a Global) -> impl Fn(usize) -> Term + 'a {
    move |index| Term::proj_struct(Term::free_var(value), *name, index)
}

/// A function of `count` arguments it does not read, answering `body`: the tie a step's law hands the proof of its second half, where a payload's own law needs none.
fn ignoring(context: &mut Context, count: usize, body: Term) -> Term {
    let binders = (0..count)
        .map(|_| (context.fresh(None), Term::hole(context.mint_metavar())))
        .collect::<Vec<_>>();
    Term::func(binders, body)
}

/// The answers `ord` folds for one constructor or one struct: the parts that are values, in order. A proof takes no part.
struct Pieces<'c> {
    row: OrdDerivation,
    values: Vec<(&'c Classified, Option<&'c Term>)>,
}

impl<'c> Pieces<'c> {
    fn new(row: OrdDerivation, parts: &'c [Classified]) -> Self {
        let values = parts
            .iter()
            .filter_map(|classified| match &classified.part {
                Part::Proof => None,
                Part::Value { premise } => Some((classified, premise.as_ref())),
            })
            .collect();

        Self { row, values }
    }

    /// A concept's `field` applied at the value at `index` over `args`, its witness goal born at this call: an answer written twice is two goals, so none stands under two scopes.
    fn applied(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        field: ConceptField,
        index: usize,
        args: Vec<Term>,
    ) -> Term {
        let (classified, premise) = self.values[index];
        witness_call(context, site, field, classified, premise, args)
    }

    /// The value at `index` as each of `sides` reads it.
    fn read<const N: usize>(&self, index: usize, sides: [Side<'_>; N]) -> [Term; N] {
        let position = self.values[index].0.position;
        sides.map(|side| side(position))
    }

    /// The answer at the value at `index`, as `ord` writes it.
    fn piece(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        index: usize,
        sides: [Side<'_>; 2],
    ) -> Term {
        let operands = self.read(index, sides).to_vec();
        self.applied(context, site, self.row.ord, index, operands)
    }

    /// The first `count` answers folded by `lexicographic`, first difference wins: at every value the whole comparison, and at fewer the answer the next one is stepped after, which the laws' proofs take a step at a time.
    fn folded(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        count: usize,
        sides: [Side<'_>; 2],
    ) -> Term {
        let pieces = (0..count)
            .map(|index| self.piece(context, site, index, sides))
            .collect::<Vec<_>>();
        let items = list(context, pieces);
        site.at(syn_call(self.row.lexicographic, [items]))
    }

    /// The whole comparison.
    fn compared(&self, context: &mut Context, site: &Site<'_>, sides: [Side<'_>; 2]) -> Term {
        self.folded(context, site, self.values.len(), sides)
    }

    /// A proof that the comparison with its sides swapped is the comparison flipped: the first value's own law, and for each value after it the step's law over the proof so far and that value's own, and `Eq/refl()` where there is nothing to compare.
    ///
    /// **A step's answers are written.** The step's law takes the fold so far and the value's answer, each way round, as arguments: left to inference, one that stands under `Ordering/flip` is solved as the comparison unfolded, a recursion written out that the totality check follows no witness into, and a family nested through a container's witness would have no law.
    fn flipped(&self, context: &mut Context, site: &Site<'_>, sides: [Side<'_>; 2]) -> Term {
        let laws = self.row.laws;
        let count = self.values.len();
        if count == 0 {
            return reflexive(context, site);
        }

        let [this, that] = sides;
        let own = |context: &mut Context, index: usize| {
            let operands = self.read(index, sides).to_vec();
            self.applied(context, site, laws.flips, index, operands)
        };
        let mut proof = own(context, 0);
        for index in 1..count {
            let answers = vec![
                self.folded(context, site, index, [this, that]),
                self.piece(context, site, index, [this, that]),
                self.folded(context, site, index, [that, this]),
                self.piece(context, site, index, [that, this]),
            ];
            let next = own(context, index);
            let rest = ignoring(context, 1, next);
            proof = site.at(syn_call(
                laws.then_flips,
                answers.into_iter().chain([proof, rest]),
            ));
        }

        proof
    }

    /// A proof that the comparison's answers at the three sides chain: the first value's answers, by the law every order has, and for each value after it the step's law over the proof so far and that value's, its answers written as [`Pieces::flipped`]'s are, and `True/qed()` where there is nothing to compare, three `eq`s chaining by reduction.
    ///
    /// A value's part is that law at its three sides and nothing else: the law takes no bound, so a law that recurses through its witness applies it at parameters alone.
    fn chained(&self, context: &mut Context, site: &Site<'_>, sides: [Side<'_>; 3]) -> Term {
        let row = self.row;
        let count = self.values.len();
        if count == 0 {
            return site.at(syn_call(context.syntax().proof.true_qed, []));
        }

        let [first, middle, last] = sides;
        let pairs = [[first, middle], [middle, last], [first, last]];
        let own = |context: &mut Context, index: usize| {
            let (classified, premise) = self.values[index];
            let operands = self.read(index, sides).to_vec();
            lemma_call(
                context,
                site,
                (row.laws.chains, row.ord.concept),
                classified,
                premise,
                operands,
            )
        };
        let mut proof = own(context, 0);
        for index in 1..count {
            let mut answers = Vec::new();
            for pair in pairs {
                answers.push(self.folded(context, site, index, pair));
            }
            for pair in pairs {
                answers.push(self.piece(context, site, index, pair));
            }
            let next = own(context, index);
            let rest = ignoring(context, 2, next);
            proof = site.at(syn_call(
                row.laws.chains_then,
                answers.into_iter().chain([proof, rest]),
            ));
        }

        proof
    }

    /// Each value's equation from `held`, a proof that the comparison answers `eq`, in the values' order, beside the proofs it binds on the way, outermost first: the concept's own law at a value gives that value's equation from its answer being `eq`.
    ///
    /// **The hypothesis is taken apart a step at a time.** `lexicographic` steps each answer after the fold of the ones before it, so each shorter fold is the first answer of the next step, bound once, and a value's own answer is the second.
    fn equations(
        &self,
        context: &mut Context,
        site: &Site<'_>,
        sides: [Side<'_>; 2],
        held: &Free,
    ) -> (Vec<(Free, Term)>, Vec<Term>) {
        let laws = self.row.laws;
        let count = self.values.len();

        // `prefixes[n]` proves the first `count - n` answers fold to `eq`.
        let mut prefixes = vec![*held];
        let mut bound = Vec::new();
        for length in (1..count).rev() {
            let longer = Term::free_var(&prefixes[prefixes.len() - 1]);
            let before = self.folded(context, site, length, sides);
            let last = self.piece(context, site, length, sides);
            let binder = context.fresh(None);
            bound.push((
                binder,
                site.at(syn_call(laws.then_first, [before, last, longer])),
            ));
            prefixes.push(binder);
        }

        let equations = (0..count)
            .map(|index| {
                let prefix = Term::free_var(&prefixes[count - index - 1]);
                let evidence = match index {
                    0 => prefix,
                    _ => {
                        let before = self.folded(context, site, index, sides);
                        let own = self.piece(context, site, index, sides);
                        site.at(syn_call(laws.then_second, [before, own, prefix]))
                    }
                };
                let arguments = self
                    .read(index, sides)
                    .into_iter()
                    .chain([evidence])
                    .collect::<Vec<_>>();
                self.applied(context, site, laws.eq_of_ord, index, arguments)
            })
            .collect();

        (bound, equations)
    }
}

/// How `left` orders against `right`, as an `Ordering`-valued term over the two binders.
///
/// **The constructors decide first, and the payloads only where they agree.** [`compare`]'s shape cannot serve here: its inner match names the one constructor the outer arm committed to and gives every other tag a *constant* default, and a constant cannot name the tag the answer depends on — a mismatched pair orders by which constructor is which. Deciding by ordinal up front costs two extra matches and stays linear in the constructor count, where an arm per pair would be quadratic in it.
fn order(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: OrdDerivation,
    left: &Free,
    right: &Free,
) -> Result<Term, Error> {
    match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, left)?;
            let sides: [Side<'_>; 2] = [&projected(left, name), &projected(right, name)];
            Ok(Pieces::new(row, &parts).compared(context, site, sides))
        }
        Subject::Induct { name, decl, params } => {
            let family = (name, decl, params.as_slice());
            let left_tag = ordinal(context, site, family, left);
            let right_tag = ordinal(context, site, family, right);
            let same = lockstep(context, site, family, row, left, right)?;
            Ok(site.at(syn_call(row.by_tag, [left_tag, right_tag, same])))
        }
    }
}

/// `value`'s constructor as its declaration position, one arm per constructor: the ordinal `by_tag` orders on, and the thing a constant default has no way to carry.
fn ordinal(
    context: &mut Context,
    site: &Site<'_>,
    (name, decl, params): Family<'_>,
    value: &Free,
) -> Term {
    let mut arms = Vec::new();
    for (index, (tag, signature)) in decl.constructors().iter().enumerate() {
        let constructor = Constructor {
            owner: name,
            tag: tag.as_str(),
            signature,
            params,
        };
        let binders = constructor.binders(context);
        arms.push((
            tag.clone(),
            constructor
                .plicities()
                .into_iter()
                .zip(binders)
                .collect::<Vec<_>>(),
            Term::num_lit(Numeral::Natural(Natural::from(index))),
        ));
    }

    let goal = goal(context);
    site.at(Term::induct_match_marked(
        Term::free_var(value),
        goal,
        arms,
        None,
    ))
}

/// The comparison of two values at one constructor: the payloads' answers folded, read where both are matched at it, and `Ordering/eq` anywhere else, which `by_tag` consults only where the ordinals already agreed.
fn lockstep(
    context: &mut Context,
    site: &Site<'_>,
    (name, decl, params): Family<'_>,
    row: OrdDerivation,
    left: &Free,
    right: &Free,
) -> Result<Term, Error> {
    let mut arms = Vec::new();
    for (tag, signature) in decl.constructors() {
        let constructor = Constructor {
            owner: name,
            tag: tag.as_str(),
            signature,
            params,
        };
        let lefts = constructor.binders(context);
        let rights = constructor.binders(context);
        let parts = constructor.parts(context, site, &lefts, left)?;
        let sides: [Side<'_>; 2] = [&|position| Term::free_var(&lefts[position]), &|position| {
            Term::free_var(&rights[position])
        }];
        let compared = Pieces::new(row, &parts).compared(context, site, sides);
        let plicities = constructor.plicities();

        // The default is unreachable — `by_tag` consults this term only where the ordinals already agreed, so the right is at the same constructor — but a one-arm match still owes one.
        let inner = site.at(Term::induct_match_marked(
            Term::free_var(right),
            goal(context),
            [(
                tag.clone(),
                plicities
                    .iter()
                    .copied()
                    .zip(rights.iter().copied())
                    .collect::<Vec<_>>(),
                compared,
            )],
            Some(Term::apply(
                Term::written(Global::Authored(row.eq.qualifier())),
                Vec::<Term>::new(),
            )),
        ));
        arms.push((
            tag.clone(),
            plicities.into_iter().zip(lefts).collect::<Vec<_>>(),
            inner,
        ));
    }

    let goal = goal(context);
    Ok(site.at(Term::induct_match_marked(
        Term::free_var(left),
        goal,
        arms,
        None,
    )))
}

/// The constructor `tag` of an inductive, at the key's parameters.
fn constructor_of<'a>(
    (name, _, params): Family<'a>,
    tag: &'a Atom,
    signature: &'a InductParam,
) -> Constructor<'a> {
    Constructor {
        owner: name,
        tag: tag.as_str(),
        signature,
        params,
    }
}

/// A match on the first of `values` with an arm a constructor, and inside each arm a match on every other value in turn, around the body `at` writes where all of them stand at that constructor, over the payloads each binds there.
///
/// **Two values at two constructors are closed by the equation of their positions.** `ties` holds, for each value after the first, a proof that its position is the one before it's, which `by_tag`'s laws hand the body or the hypothesis gives. At two different constructors that is an equation of two different numerals, so the arm is its match with no arm: no payload is read there, and a law over three values has two such arms a pair of constructors, where an arm a triple would be cubic in them.
fn facing<const N: usize>(
    context: &mut Context,
    site: &Site<'_>,
    (name, decl, params): Family<'_>,
    row: OrdDerivation,
    (values, ties): ([&Free; N], &[&Free]),
    at: &mut dyn FnMut(&mut Context, &Pieces<'_>, [Side<'_>; N]) -> Term,
) -> Result<Term, Error> {
    let constructor = |tag, signature| constructor_of((name, decl, params), tag, signature);
    let arm = |constructor: &Constructor<'_>, tag: &Atom, binders: &[Free], body: Term| {
        (
            tag.clone(),
            constructor
                .plicities()
                .into_iter()
                .zip(binders.iter().copied())
                .collect::<Vec<_>>(),
            body,
        )
    };

    let mut arms = Vec::new();
    for (tag, signature) in decl.constructors() {
        let agreed = constructor(tag, signature);
        let bound = values.map(|_| agreed.binders(context));
        let parts = agreed.parts(context, site, &bound[0], values[0])?;
        let readers = bound
            .each_ref()
            .map(|binders| move |position: usize| Term::free_var(&binders[position]));
        let sides = readers.each_ref().map(|reader| reader as Side<'_>);
        let mut body = at(context, &Pieces::new(row, &parts), sides);

        for ((value, binders), tie) in values.iter().zip(&bound).skip(1).zip(ties).rev() {
            let mut facing = Vec::new();
            for (other, other_signature) in decl.constructors() {
                if other == tag {
                    facing.push(arm(&agreed, tag, binders, body.clone()));
                    continue;
                }
                let apart = constructor(other, other_signature);
                let payloads = apart.binders(context);
                let closed = site.at(Term::induct_match_marked(
                    Term::free_var(tie),
                    goal(context),
                    Vec::<(Atom, Vec<(Plicity, Free)>, Term)>::new(),
                    None,
                ));
                facing.push(arm(&apart, other, &payloads, closed));
            }
            body = site.at(Term::induct_match_marked(
                Term::free_var(value),
                goal(context),
                facing,
                None,
            ));
        }
        arms.push(arm(&agreed, tag, &bound[0], body));
    }

    let goal = goal(context);
    Ok(site.at(Term::induct_match_marked(
        Term::free_var(values[0]),
        goal,
        arms,
        None,
    )))
}

/// `flips`: that `ord` answers the flipped answer with its operands swapped, as a function of the two.
///
/// A struct's is its fields' ([`Pieces::flipped`]). An inductive's is `by_tag`'s law at the two positions, handed the payloads' part where the positions are one ([`facing`]).
fn flipped_with_operands(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: OrdDerivation,
) -> Result<Term, Error> {
    let left = context.fresh(None);
    let right = context.fresh(None);

    let proof = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &left)?;
            let sides: [Side<'_>; 2] = [&projected(&left, name), &projected(&right, name)];
            Pieces::new(row, &parts).flipped(context, site, sides)
        }
        Subject::Induct { name, decl, params } => {
            let family = (name, decl, params.as_slice());
            let arguments = vec![
                ordinal(context, site, family, &left),
                ordinal(context, site, family, &right),
                lockstep(context, site, family, row, &left, &right)?,
                lockstep(context, site, family, row, &right, &left)?,
            ];
            let tie = context.fresh(None);
            let payloads = facing(
                context,
                site,
                family,
                row,
                ([&left, &right], &[&tie]),
                &mut |context, pieces, sides| pieces.flipped(context, site, sides),
            )?;
            let tied = Term::func([(tie, Term::hole(context.mint_metavar()))], payloads);
            site.at(syn_call(
                row.laws.by_tag_flips,
                arguments.into_iter().chain([tied]),
            ))
        }
    };

    Ok(Term::func(
        [left, right].map(|binder| (binder, Term::hole(context.mint_metavar()))),
        proof,
    ))
}

/// `transitive`: that `ord` as a relation carries across a middle value, as a function of the three and of the two bounds.
///
/// The three answers chain ([`Pieces::chained`]), a struct's through its fields and an inductive's through `by_tag`'s law at the three positions, handed the payloads' part where the positions are one ([`facing`]); and the third of three answers that chain is `lt` or `eq` where the first two are. The three comparisons that law is over are built again and written, as a step's answers are.
fn carried_across(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: OrdDerivation,
) -> Result<Term, Error> {
    let first = context.fresh(None);
    let middle = context.fresh(None);
    let last = context.fresh(None);
    let below = context.fresh(None);
    let above = context.fresh(None);

    let chained = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &first)?;
            let sides: [Side<'_>; 3] = [
                &projected(&first, name),
                &projected(&middle, name),
                &projected(&last, name),
            ];
            Pieces::new(row, &parts).chained(context, site, sides)
        }
        Subject::Induct { name, decl, params } => {
            let family = (name, decl, params.as_slice());
            let arguments = vec![
                ordinal(context, site, family, &first),
                ordinal(context, site, family, &middle),
                ordinal(context, site, family, &last),
                lockstep(context, site, family, row, &first, &middle)?,
                lockstep(context, site, family, row, &middle, &last)?,
                lockstep(context, site, family, row, &first, &last)?,
            ];
            let ties = [context.fresh(None), context.fresh(None)];
            let payloads = facing(
                context,
                site,
                family,
                row,
                ([&first, &middle, &last], &[&ties[0], &ties[1]]),
                &mut |context, pieces, sides| pieces.chained(context, site, sides),
            )?;
            let tied = Term::func(
                ties.map(|tie| (tie, Term::hole(context.mint_metavar()))),
                payloads,
            );
            site.at(syn_call(
                row.laws.by_tag_chains,
                arguments.into_iter().chain([tied]),
            ))
        }
    };
    let compared = [
        order(context, site, subject, row, &first, &middle)?,
        order(context, site, subject, row, &middle, &last)?,
        order(context, site, subject, row, &first, &last)?,
    ];
    let proof = site.at(syn_call(
        row.laws.chains_le,
        compared
            .into_iter()
            .chain([chained, Term::free_var(&below), Term::free_var(&above)]),
    ));

    Ok(Term::func(
        [first, middle, last, below, above]
            .map(|binder| (binder, Term::hole(context.mint_metavar()))),
        proof,
    ))
}

/// `eq_of_ord`: that two values `ord` answers `eq` at are one, as a function of the two and of the proof that it does.
///
/// Each value's equation comes from its answer being `eq` ([`Pieces::equations`]). A struct's are handed to its congruence ([`congruent`]). An inductive's are matched in turn at one constructor ([`matched`]), under the equation of the two positions `by_tag`'s law reads off the hypothesis ([`facing`]).
fn equal_where_tied(
    context: &mut Context,
    site: &Site<'_>,
    subject: &Subject,
    row: OrdDerivation,
) -> Result<Term, Error> {
    let left = context.fresh(None);
    let right = context.fresh(None);
    let held = context.fresh(None);

    let proof = match subject {
        Subject::Struct { name, decl, params } => {
            let parts = struct_parts(context, site, name, decl, params, &left)?;
            let sides: [Side<'_>; 2] = [&projected(&left, name), &projected(&right, name)];
            let equal = Pieces::new(row, &parts).equations(context, site, sides, &held);
            congruent(context, site, (name, decl, params), (&left, &right), equal)?
        }
        Subject::Induct { name, decl, params } => {
            let family = (name, decl, params.as_slice());
            let left_tag = ordinal(context, site, family, &left);
            let right_tag = ordinal(context, site, family, &right);
            let same = lockstep(context, site, family, row, &left, &right)?;
            let positions = site.at(syn_call(
                row.laws.by_tag_tied,
                [left_tag, right_tag, same, Term::free_var(&held)],
            ));
            let tie = context.fresh(None);
            let payloads = facing(
                context,
                site,
                family,
                row,
                ([&left, &right], &[&tie]),
                &mut |context, pieces, sides| {
                    let (bound, equations) = pieces.equations(context, site, sides, &held);
                    let closing = reflexive(context, site);
                    let proof = matched(context, site, equations, closing);
                    bound_around(context, bound, proof)
                },
            )?;
            Term::let_(
                &tie,
                Term::hole(context.mint_metavar()),
                positions,
                payloads,
            )
        }
    };

    Ok(Term::func(
        [left, right, held].map(|binder| (binder, Term::hole(context.mint_metavar()))),
        proof,
    ))
}
