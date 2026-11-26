//! A file of Curios/Core read back into the module it writes, and the entry a program closes with.
//!
//! **Each item stands where it is written.** A group's members, a test and a foreign row's definition each make the item the text writes at its place. A nominal declaration's forms make the item that holds their entries, and its formers and a family's constructors are what those entries make of them ([`Item::made`]), so a module neither lacks one nor holds one apart from its declaration, and a definition is never declared one.
//!
//! **A group's members name one another by their paths at the group's own instance**, as a group opened to be written names them ([`RecItem::definitions`]); each such reference is the group's own binder again where the group is made.

use {
    super::{
        Fields, ReadError, Reader, Refusal, concept_entry_read, core_read, family_entry_read,
        malformed, record_entry_read, universe_context_read, value_bool, value_global, value_list,
        value_module, value_string, value_term, value_variant, value_word,
    },
    crate::{
        CoreFile, CoreHead, CoreItem, CoreNominals, CoreText, CoreValue, FormScope, LetSignature,
        Spellable, Spelling, TopConcept, TopForeign, TopLet, TopTest, concept_supers_read_off,
    },
    curios_abi::ForeignFunction,
    curios_common::{Qualifier, Span, SyntaxRegistry},
    curios_core::{
        ConceptDecl, Definition, DefinitionKind, Definitions, Entrypoint, Global, Item, Nominal,
        Nominals, RecItem, Term, UniverseContext,
    },
    std::{rc::Rc, sync::Arc},
};

/// The module `file` writes, and the entry it closes with where it is a program's: read over `scope` where it is handed one, which a test's and a foreign row's forms are made against and a call across the wire is made through.
pub fn to_core(
    file: &CoreFile,
    scope: Option<&FormScope>,
) -> Result<(Definitions, Option<Entrypoint>), ReadError> {
    let mut reader = Reader::new(scope);
    // A row is named by every call across the wire in the text, wherever its form stands.
    for item in &file.items {
        if let CoreItem::Foreign(row) = item {
            reader.declare_row(row.core.name, Arc::new(row_read(row)?));
        }
    }
    let mut written = Vec::new();
    let mut items = file.items.iter().peekable();
    while let Some(item) = items.next() {
        match item {
            CoreItem::Lets(members) => written.push(group_read(members, &mut reader)?),
            CoreItem::Nominals(nominals) => {
                written.push(Item::Nominal(nominals_read(nominals, &mut reader)?))
            }
            CoreItem::Test(test) => {
                written.push(Item::Let(Box::new(test_read(test, &mut reader)?)))
            }
            CoreItem::Foreign(row) => {
                // The definition the row implies stands in its place, where the text writes no other under its name beside it.
                let stated = matches!(
                    items.peek(),
                    Some(CoreItem::Lets(members))
                        if matches!(members.as_slice(), [member] if member.core.name == row.core.name)
                );
                if let Some(definition) = foreign_definition_read(row, stated, &mut reader)? {
                    written.push(Item::Let(Box::new(definition)));
                }
            }
        }
    }
    let definitions = Definitions {
        items: written.into(),
    };
    let entry = file
        .entry
        .as_ref()
        .map(|entry| {
            Ok(Entrypoint {
                body: core_read(&entry.body, &mut reader)?,
                type_: entry
                    .type_
                    .as_ref()
                    .map(|type_| core_read(type_, &mut reader))
                    .transpose()?,
            })
        })
        .transpose()?;

    Ok((definitions, entry))
}

/// [`to_core`], the closing term put at `at`, the type a question puts it at: where the text states a type for it, that type held to `at` by Core's equality, which reads no position, and refused at the place it is written where it is another.
pub fn to_core_at(
    file: &CoreFile,
    scope: Option<&FormScope>,
    at: &Term,
    syntax: &SyntaxRegistry,
) -> Result<(Definitions, Option<Entrypoint>), ReadError> {
    let (definitions, entry) = to_core(file, scope)?;
    let entry = entry
        .map(|entry| match entry.type_ {
            Some(stated) if stated != *at => Err(ReadError::at(
                Refusal::PutElsewhere(at.spelled(&Rc::new(Spelling::new(syntax))).to_string()),
                file.entry
                    .as_ref()
                    .and_then(|written| written.type_.as_ref())
                    .and_then(crate::Term::span),
            )),
            _ => Ok(Entrypoint {
                body: entry.body,
                type_: Some(at.clone()),
            }),
        })
        .transpose()?;

    Ok((definitions, entry))
}

/// The row a foreign form states: its name, the path it is declared at unless `#{name = "…"}` states another, and its signature.
fn row_read(row: &TopForeign<CoreText>) -> Result<ForeignFunction, ReadError> {
    let name = match row
        .core
        .fields
        .iter()
        .find(|field| field.label.as_str() == "name")
    {
        Some(field) => value_string(&field.value)?,
        None => row
            .core
            .name
            .qualifier()
            .map(Qualifier::join)
            .unwrap_or_default(),
    };

    Ok(ForeignFunction {
        name,
        signature: row.signature.clone(),
    })
}

/// The definition a foreign row's form implies, made against the scope's registry, or none where the text states another beside it.
fn foreign_definition_read(
    row: &TopForeign<CoreText>,
    stated: bool,
    reader: &mut Reader<'_>,
) -> Result<Option<Definition>, ReadError> {
    let span = row.label.span();
    let mut fields = Fields::of(&row.core.fields, span)?;
    fields.take("name");
    if stated {
        fields.finish()?;
        return Ok(None);
    }
    let island = island_read(&mut fields, &row.core.name)?;
    fields.finish()?;
    let scope = reader.scope("a foreign row's form", span)?;
    let function = reader
        .row(&row.core.name)
        .cloned()
        .expect("every row of the text is declared before it is read");

    Ok(Some(Definition::foreign(
        row.core.name,
        island,
        function,
        &scope.syntax,
    )))
}

/// The definition a test's form implies, `test /t = value`, made against the scope's test family.
fn test_read(test: &TopTest<CoreText>, reader: &mut Reader<'_>) -> Result<Definition, ReadError> {
    let span = test.label.span();
    let mut fields = Fields::of(&test.core.fields, span)?;
    let island = island_read(&mut fields, &test.core.name)?;
    fields.finish()?;
    let scope = reader.scope("a test's form", span)?;
    let family = Global::Authored(scope.syntax.test.test_type.qualifier());
    let declaration = scope
        .entries
        .get(&family)
        .and_then(|entry| entry.inductive())
        .ok_or_else(|| ReadError::at(Refusal::Undeclared(family.faithful().to_string()), span))?;
    let value = core_read(&test.body, reader)?;

    Ok(Definition::test(
        test.core.name,
        island,
        value,
        family,
        declaration,
    ))
}

/// The module a definition's decorator places it in: its name's parent where it states none.
fn island_read(fields: &mut Fields<'_>, name: &Global) -> Result<Qualifier, ReadError> {
    match fields.take("island") {
        Some(value) => value_module(value),
        None => Ok(match name {
            Global::Authored(path) => path.without_last(),
            Global::Witness(id) => *id.module(),
        }),
    }
}

/// What a group's member makes: its definition, and the node its item is where the decorator states one.
struct Member {
    definition: Definition,
    node: Option<String>,
}

/// A group of definitions as the item it writes: a lone one a `let` unless its decorator's `node` says it is a group, and several a group.
fn group_read(members: &[TopLet<CoreText>], reader: &mut Reader<'_>) -> Result<Item, ReadError> {
    let mut scheme = None;
    let (mut read, held) = reader.grouped(|reader| {
        members
            .iter()
            .enumerate()
            .map(|(index, member)| member_read(member, index, &mut scheme, reader))
            .collect::<Result<Vec<_>, _>>()
    })?;
    // What a member names of another by an instance is the global, put back once the group has bound what it names by its path.
    let instances = |term: &Term| {
        held.iter().fold(term.clone(), |term, (local, instance)| {
            term.replace_term(&Term::free_var(local), instance)
        })
    };
    let grouped = match read.as_slice() {
        [lone] => lone.node.as_deref() == Some("Rec"),
        _ => true,
    };
    if !grouped {
        let mut lone = read.pop().expect("a group has a member").definition;
        lone.type_ = instances(&lone.type_);
        lone.body = instances(&lone.body);
        return Ok(Item::Let(Box::new(lone)));
    }
    let context = scheme.unwrap_or_default();
    let instance = context.identity_instance();
    let locals = read
        .iter()
        .map(|member| reader.fresh(Some(&member.definition.name.faithful().to_string())))
        .collect::<Vec<_>>();
    let references = read
        .iter()
        .zip(&locals)
        .map(|(member, local)| {
            (
                Term::instance(member.definition.name, instance.clone()),
                Term::free_var(local),
            )
        })
        .collect::<Vec<_>>();
    let grouped = read
        .into_iter()
        .zip(locals)
        .map(|(member, local)| {
            let mut definition = member.definition;
            for (reference, binder) in &references {
                definition.type_ = definition.type_.replace_term(reference, binder);
                definition.body = definition.body.replace_term(reference, binder);
            }
            definition.type_ = instances(&definition.type_);
            definition.body = instances(&definition.body);
            definition.universe_context = context.clone();
            (local, definition)
        })
        .collect();
    let item = RecItem::try_new(grouped).map_err(|_| {
        ReadError::at(
            Refusal::Malformed("one scheme over a group, stated by its first member"),
            members.first().and_then(|member| member.label.span()),
        )
    })?;

    Ok(Item::Rec(item))
}

/// A nominal declaration's forms as the item holding their entries, each form's read under the scheme its group's first form states.
fn nominals_read(nominals: &CoreNominals, reader: &mut Reader<'_>) -> Result<Nominals, ReadError> {
    let mut scheme = None;
    let entries = match nominals {
        CoreNominals::Induct(forms) => forms
            .iter()
            .enumerate()
            .map(|(index, form)| {
                entry_read(
                    &form.core,
                    form.label.span(),
                    index,
                    &mut scheme,
                    |name, scheme, fields| {
                        family_entry_read(form, name, scheme, fields, reader)
                            .map(Nominal::Inductive)
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?,
        CoreNominals::Struct(forms) => forms
            .iter()
            .enumerate()
            .map(|(index, form)| {
                entry_read(
                    &form.core,
                    form.label.span(),
                    index,
                    &mut scheme,
                    |name, scheme, fields| {
                        record_entry_read(form, name, scheme, fields, reader).map(Nominal::Struct)
                    },
                )
            })
            .collect::<Result<Vec<_>, _>>()?,
        CoreNominals::Concept(forms) => forms
            .iter()
            .enumerate()
            .map(|(index, form)| {
                entry_read(
                    &form.core,
                    form.label.span(),
                    index,
                    &mut scheme,
                    |name, scheme, fields| concept_read(form, name, scheme, fields, reader),
                )
            })
            .collect::<Result<Vec<_>, _>>()?,
    };

    Ok(Nominals::new(entries))
}

/// A nominal declaration's form, headed by `core` and written at `span`, as the name it declares and its entry, which `read` reads of it: under the scheme `scheme` its group shares, which its first member states. A former is made of its entry, so its decorator states nothing of one.
fn entry_read(
    core: &CoreHead,
    span: Option<&Span>,
    index: usize,
    scheme: &mut Option<UniverseContext>,
    read: impl FnOnce(&Global, &UniverseContext, &mut Fields<'_>) -> Result<Nominal, ReadError>,
) -> Result<(Global, Nominal), ReadError> {
    let mut fields = Fields::of(&core.fields, span)?;
    let scheme = group_scheme(&mut fields, index, scheme)?;
    let entry = read(&core.name, &scheme, &mut fields)?;
    fields.finish()?;

    Ok((core.name, entry))
}

/// One definition of a group under its decorator, by its name, type and body. `scheme` is the group's, which its first member states.
fn member_read(
    member: &TopLet<CoreText>,
    index: usize,
    scheme: &mut Option<UniverseContext>,
    reader: &mut Reader<'_>,
) -> Result<Member, ReadError> {
    let (core, span) = (&member.core, member.label.span());
    let name = core.name;
    let mut fields = Fields::of(&core.fields, span)?;
    let node = match index {
        0 => fields
            .take("node")
            .map(value_word)
            .transpose()?
            .map(str::to_string),
        _ => None,
    };
    let context = group_scheme(&mut fields, index, scheme)?;
    let kind = fields.take("kind").map(kind_read).transpose()?;
    let island = island_read(&mut fields, &name)?;
    let proof = fields
        .take("proof")
        .map(value_bool)
        .transpose()?
        .unwrap_or(false);
    let value_sort = fields
        .take("value_sort")
        .map(|value| core_read(&value_term(value)?, reader))
        .transpose()?;
    let LetSignature::Name {
        type_: Some(type_),
        body,
    } = &member.signature
    else {
        return Err(ReadError::at(
            Refusal::ProgramForm("a definition's sugar"),
            span,
        ));
    };
    let definition = Definition {
        name,
        kind: kind.unwrap_or(DefinitionKind::Authored),
        universe_context: context,
        island,
        proof,
        value_sort,
        type_: core_read(type_, reader)?,
        body: core_read(body, reader)?,
    };
    fields.finish()?;

    Ok(Member { definition, node })
}

/// The scheme the member at `index` of a group is read under: the one its decorator states where it is the first, which `scheme` keeps for the rest, each of which states none.
fn group_scheme(
    fields: &mut Fields<'_>,
    index: usize,
    scheme: &mut Option<UniverseContext>,
) -> Result<UniverseContext, ReadError> {
    let context = match (index, fields.take("universe_context")) {
        (0, Some(value)) => universe_context_read(value)?,
        (0, None) => UniverseContext::empty(),
        (_, Some(value)) => {
            return Err(malformed("a scheme its group's first member states", value));
        }
        (_, None) => scheme.clone().unwrap_or_default(),
    };
    if index == 0 {
        *scheme = Some(context.clone());
    }

    Ok(context)
}

/// A concept's entry from its form: its record, and its superclass edges read off the record where its `concept` decorator states none.
fn concept_read(
    concept: &TopConcept<CoreText>,
    name: &Global,
    scheme: &UniverseContext,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Nominal, ReadError> {
    let record = concept_entry_read(concept, name, scheme, fields, reader)?;
    let mut declaration = ConceptDecl {
        supers: concept_supers_read_off(*name, &record),
    };
    if let Some(value) = fields.take("concept") {
        let mut own = super::value_fields(value)?;
        if let Some(supers) = own.take("supers") {
            declaration.supers = value_list(supers)?
                .iter()
                .map(value_global)
                .collect::<Result<_, _>>()?;
        }
        own.finish()?;
    }

    Ok(Nominal::Concept {
        concept: declaration,
        record,
    })
}

/// What a definition is declared as, where it is not what its keyword declares: `Witness` or `Test`. A constructor is no kind a definition states: it is its family's, which the family's form declares, so a definition stating itself one is refused here.
fn kind_read(value: &CoreValue) -> Result<DefinitionKind, ReadError> {
    let refused = || malformed("a definition's kind: `Witness` or `Test`", value);
    match value_variant(value).map_err(|_| refused())? {
        ("Witness", held) if held.is_empty() => Ok(DefinitionKind::Witness),
        ("Test", held) if held.is_empty() => Ok(DefinitionKind::Test),
        _ => Err(refused()),
    }
}
