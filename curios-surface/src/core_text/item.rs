//! A file of Core text read back into the module it writes, and the entry a program closes with.
//!
//! **Each item stands where it is written.** A group's members, a test and a foreign row's definition each make the item the text writes at its place. A family's constructors are no item: its form declares them, and they are what its entry makes of them ([`Item::constructors`]), so a module neither lacks one nor holds one apart from its family, and a definition is never declared one.
//!
//! **A group's members name one another by their paths at the group's own instance**, as a group opened to be written names them ([`RecItem::definitions`]); each such reference is the group's own binder again where the group is made.

use {
    super::{
        Fields, ReadError, Reader, Refusal, concept_entry_read, core_read, family_entry_read,
        malformed, record_entry_read, universe_context_read, value_bool, value_global, value_list,
        value_module, value_string, value_term, value_variant, value_word,
    },
    crate::{
        CoreFile, CoreItem, CoreMember, CoreText, CoreValue, FormScope, LetSignature, TopForeign,
        TopTest, concept_supers_read_off,
    },
    curios_abi::ForeignFunction,
    curios_common::Qualifier,
    curios_core::{
        ConceptDecl, Definition, DefinitionKind, Definitions, Entrypoint, Global, Item, Nominal,
        RecItem, Term, UniverseContext,
    },
    std::sync::Arc,
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
            CoreItem::Group(members) => written.push(group_read(members, &mut reader)?),
            CoreItem::Test(test) => {
                written.push(Item::Let(Box::new(test_read(test, &mut reader)?)))
            }
            CoreItem::Foreign(row) => {
                // The definition the row implies stands in its place, where the text writes no other under its name beside it.
                let stated = matches!(
                    items.peek(),
                    Some(CoreItem::Group(members))
                        if matches!(members.as_slice(), [CoreMember::Let(member)] if member.core.name == row.core.name)
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

/// A group of members as the item it writes: a lone definition a `let`, a lone family a group, a lone record or concept a `let`, each unless its decorator's `node` says otherwise, and several members a group.
fn group_read(members: &[CoreMember], reader: &mut Reader<'_>) -> Result<Item, ReadError> {
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
        [lone] => {
            let family = matches!(lone.definition.kind.nominal(), Some(Nominal::Inductive(_)));
            match lone.node.as_deref() {
                Some("Rec") => true,
                Some("Let") => false,
                _ => family,
            }
        }
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
            members.first().and_then(member_span),
        )
    })?;

    Ok(Item::Rec(item))
}

/// Where a member's name is written.
fn member_span(member: &CoreMember) -> Option<&curios_common::Span> {
    match member {
        CoreMember::Let(member) => member.label.span(),
        CoreMember::Induct(member) => member.label.span(),
        CoreMember::Struct(member) => member.label.span(),
        CoreMember::Concept(member) => member.label.span(),
    }
}

/// One member under its decorator: a definition by its name, type and body, or a nominal declaration in its form. `scheme` is the group's, which its first member states.
fn member_read(
    member: &CoreMember,
    index: usize,
    scheme: &mut Option<UniverseContext>,
    reader: &mut Reader<'_>,
) -> Result<Member, ReadError> {
    let (core, span) = match member {
        CoreMember::Let(member) => (&member.core, member.label.span()),
        CoreMember::Induct(member) => (&member.core, member.label.span()),
        CoreMember::Struct(member) => (&member.core, member.label.span()),
        CoreMember::Concept(member) => (&member.core, member.label.span()),
    };
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
    let definition = match member {
        CoreMember::Let(member) => {
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
            Definition {
                name,
                kind: kind.unwrap_or(DefinitionKind::Authored),
                universe_context: context,
                island,
                proof,
                value_sort,
                type_: core_read(type_, reader)?,
                body: core_read(body, reader)?,
            }
        }
        nominal => {
            if kind.is_some() {
                return Err(ReadError::at(
                    Refusal::UnknownField("kind".to_string()),
                    span,
                ));
            }
            let entry = nominal_read(nominal, &name, &context, &mut fields, reader)?;
            let mut definition = Definition::former(name, island, entry);
            definition.universe_context = context;
            definition.proof = proof;
            definition.value_sort = value_sort;
            if let Some(value) = fields.take("type_") {
                definition.type_ = core_read(&value_term(value)?, reader)?;
            }
            if let Some(value) = fields.take("body") {
                definition.body = core_read(&value_term(value)?, reader)?;
            }
            definition
        }
    };
    fields.finish()?;

    Ok(Member { definition, node })
}

/// A nominal declaration's entry from its form.
fn nominal_read(
    member: &CoreMember,
    name: &Global,
    scheme: &UniverseContext,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<Nominal, ReadError> {
    match member {
        CoreMember::Induct(family) => Ok(Nominal::Inductive(family_entry_read(
            family, name, scheme, fields, reader,
        )?)),
        CoreMember::Struct(record) => Ok(Nominal::Struct(record_entry_read(
            record, name, scheme, fields, reader,
        )?)),
        CoreMember::Concept(concept) => {
            let (record, words) = concept_entry_read(concept, name, scheme, fields, reader)?;
            let mut declaration = ConceptDecl {
                universe_context: record.universe_context.clone(),
                fields: words,
                supers: concept_supers_read_off(*name, &record),
            };
            if let Some(value) = fields.take("concept") {
                let mut own = super::value_fields(value)?;
                if let Some(context) = own.take("universe_context") {
                    declaration.universe_context = universe_context_read(context)?;
                }
                if let Some(supers) = own.take("supers") {
                    declaration.supers = value_list(supers)?
                        .iter()
                        .map(value_global)
                        .collect::<Result<_, _>>()?;
                }
                if let Some(labels) = own.take("fields") {
                    declaration.fields = value_list(labels)?
                        .iter()
                        .map(value_string)
                        .collect::<Result<_, _>>()?;
                }
                own.finish()?;
            }
            Ok(Nominal::Concept {
                concept: declaration,
                record,
            })
        }
        CoreMember::Let(_) => unreachable!("a definition is read by its name, type and body"),
    }
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
