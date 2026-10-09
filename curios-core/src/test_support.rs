//! Re-folding a finished [`Program`] into one nested term, for the suites that assert against that shape.
//!
//! A namespace rather than a root export, for `curios-runtime`'s `test_support` reason: `curios_core::test_support::into_nested_term(module)` says at its use site that the caller reached for scaffolding rather than product API, which a `Module::into_nested_term` method sitting beside `nominal_plicities` would not. The path is the warning label.
//!
//! **Behind `test-support`, not `#[cfg(test)]`.** The caller is `curios-text`'s lowering suite, a different crate, and that cfg is set only while *this* crate is its own test harness — so a `cfg(test)` item would be invisible to it. The gate is also what keeps a shape no compiler stage produces out of every build that ships.

use {
    crate::{
        ConceptDecl, Definition, DefinitionKind, Free, Global, InductDecl, Item, Module, Nominal,
        Program, RecItem, StructDecl, Term,
    },
    curios_utilities::Qualifier,
    std::{collections::BTreeMap, rc::Rc},
};

/// Re-fold the program's flat module into a nested `Let`/`Rec` [`Term`] around the entry's body (items are already in binding order).
///
/// Lets the `into_core` suite assert against a single term. Drops the entry's type, which that suite's `run` helper does not return.
pub fn into_nested_term(program: Program) -> Term {
    program
        .module
        .items
        .into_iter()
        .rev()
        .fold(program.entry.body, |acc, item| match item {
            Item::Let(def) => Term::let_(&Free::from(&def.name), def.type_, def.body, acc),
            Item::Rec(rec) => Term::rec(
                rec.definitions()
                    .into_iter()
                    .map(|def| (Free::from(&def.name), def.type_, def.body)),
                acc,
            ),
        })
}

/// `module` declaring each of `inducts`, `structs` and `concepts`: carried by the definition of its name where the module holds one, and where it holds none by the former spelled from it ([`Definition::former`]), a family's in a group of its own as the lowering writes one, stored as a module stores an item ([`Module::ordered`]): after the items its entry names, so ahead of the module's own or just past the last of them it reaches. A concept takes the record `structs` holds under its name.
///
/// How a fixture that names a registry entry builds its module: an entry is carried by a former and by nothing else, so a fixture that wants one and no former of its own is handed the one the entry spells.
pub fn declaring(
    module: Module,
    inducts: impl IntoIterator<Item = (Global, InductDecl)>,
    structs: impl IntoIterator<Item = (Global, StructDecl)>,
    concepts: impl IntoIterator<Item = (Global, ConceptDecl)>,
) -> Module {
    let mut records = structs.into_iter().collect::<BTreeMap<_, _>>();
    let mut entries = BTreeMap::new();
    for (name, concept) in concepts {
        let record = records
            .remove(&name)
            .expect("a concept is declared with the record its witnesses are values of");
        entries.insert(name, Nominal::Concept { concept, record });
    }
    entries.extend(
        records
            .into_iter()
            .map(|(name, record)| (name, Nominal::Struct(record))),
    );
    for (name, declaration) in inducts {
        let earlier = entries.insert(name, Nominal::Inductive(declaration));
        assert!(earlier.is_none(), "{name} is declared by one entry");
    }

    let carried = |name: &Global, kind: &mut DefinitionKind, entries: &mut BTreeMap<_, _>| {
        if let Some(nominal) = entries.remove(name) {
            *kind = DefinitionKind::Nominal(Rc::new(nominal));
        }
    };
    let mut items = module.items.to_vec();
    for item in &mut items {
        match item {
            Item::Let(definition) => carried(&definition.name, &mut definition.kind, &mut entries),
            Item::Rec(rec) => {
                for member in &mut rec.definitions {
                    carried(&member.name, &mut member.kind, &mut entries);
                }
            }
        }
    }
    // Each former by the place it is stored ahead of: a walk takes the items as they come, and an entry may name a definition the module holds.
    let mut formers: BTreeMap<usize, Vec<Item>> = BTreeMap::new();
    for (name, nominal) in entries {
        let reach = nominal.reaches();
        let place = items
            .iter()
            .rposition(|item| {
                item.declared_names()
                    .into_iter()
                    .any(|declared| reach.contains(declared))
            })
            .map_or(0, |last| last + 1);
        let grouped = matches!(nominal, Nominal::Inductive(_));
        let island = name
            .qualifier()
            .map(Qualifier::without_last)
            .unwrap_or_default();
        let former = Definition::former(name, island, nominal);

        formers.entry(place).or_default().push(match grouped {
            true => Item::Rec(RecItem::new(vec![former])),
            false => Item::Let(former),
        });
    }
    let mut stored = Vec::new();
    for (place, item) in items.into_iter().enumerate() {
        stored.extend(formers.remove(&place).into_iter().flatten());
        stored.push(item);
    }
    stored.extend(formers.into_values().flatten());

    Module {
        items: stored.into(),
        ..module
    }
}
