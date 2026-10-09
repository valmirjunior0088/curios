//! Naming a unit's witnesses, once its items are lowered.
//!
//! A `satisfy` declaration is anonymous, and the name the compiler gives it is what it witnesses: its module, the concept its signature applies and the key that signature is spelled at (`curios_core::WitnessId`). The key is read through the definitions a signature names, in any file of the unit, so it cannot be read where the witness is lowered: a witness is lowered under its place among its module's witnesses, and this pass gives every one whose signature spells a key its name before an item leaves the lowering.
//!
//! **A key is read off lowered forms alone**, the unit's and those of the units in scope: a `let` by its body, a group's member not at all, a concept's marks off its record entry's arity. A unit in scope is asked by name ([`PreparedText::written`]), so naming a witness reads the lowered form of each definition its signature names and of no other. How a declaration is written is the lowering's to say, so a witness's name is a function of the text in scope and of nothing that elaborated.
//!
//! **No lowered term names a witness** — one is reached by resolution, which is elaboration's — so a name changes in the tables that are keyed by it and nowhere else: the items, the unit's witness set, what each declaration minted, the import scope of each, and the binders a lint names.

use {
    super::{FlatItem, FlatLet},
    crate::{Lint, PreparedText},
    curios_utilities::{Plicity, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

/// A name's declaration as a key reads it: its kind, and its body where it is a `let`'s.
type Declared<'a> = (
    &'a curios_core::DefinitionKind,
    Option<&'a curios_core::Term>,
);

/// What a declaration is, as far as a key reads it: a type former, a definition with its body, or anything a key is not read through.
fn written<'a>(declared: Option<Declared<'a>>) -> curios_core::Written<'a> {
    match declared {
        Some((
            curios_core::DefinitionKind::InductiveType
            | curios_core::DefinitionKind::StructType
            | curios_core::DefinitionKind::ConceptType,
            _,
        )) => curios_core::Written::Former,
        Some((curios_core::DefinitionKind::Authored, Some(body))) => {
            curios_core::Written::Defined(body)
        }
        _ => curios_core::Written::Opaque,
    }
}

/// The name each witness of `items` takes where its signature spells a concept and a key, by the name it was lowered under. Its place counts the witnesses its module declares ahead of it at that concept and key: none in a coherent unit.
///
/// `scope` holds every unit in scope, in dependency order.
fn witness_names(
    items: &[FlatItem],
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: &BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    scope: &[&PreparedText],
) -> BTreeMap<curios_core::Global, curios_core::Global> {
    let witnesses = items
        .iter()
        .flat_map(FlatItem::lets)
        .filter(|let_| matches!(let_.kind, curios_core::DefinitionKind::Witness))
        .collect::<Vec<_>>();
    if witnesses.is_empty() {
        return BTreeMap::new();
    }

    let mut unit = BTreeMap::<curios_core::Global, Declared<'_>>::new();
    for item in items {
        match item {
            FlatItem::Let(let_) => {
                unit.insert(let_.name, (&let_.kind, Some(&let_.body)));
            }
            // A member's body is closed over its group, which a key is not read through.
            FlatItem::Rec(lets) => {
                for let_ in lets {
                    unit.insert(let_.name, (&let_.kind, None));
                }
            }
            FlatItem::Kept(rec) => {
                for definition in &rec.definitions {
                    unit.insert(definition.name, (&definition.kind, None));
                }
            }
        }
    }
    // A name the unit does not declare is read as the nearest unit in scope declares it.
    let read = |name: &curios_core::Global| {
        let declared = unit
            .get(name)
            .copied()
            .or_else(|| scope.iter().rev().find_map(|earlier| earlier.written(name)));

        written(declared)
    };
    // A concept's parameters are its record entry's, whose arity states the mark each is declared under.
    let marks = |name: &curios_core::Global| -> Option<Vec<Plicity>> {
        let declared = |module: &curios_core::Module| {
            module.concepts.get(name)?;

            Some(module.struct_decls.get(name)?.arity.marks())
        };
        match concepts.contains_key(name) {
            true => Some(struct_decls.get(name)?.arity.marks()),
            false => scope
                .iter()
                .rev()
                .find_map(|earlier| declared(earlier.entries())),
        }
    };
    let spelled = curios_core::Keying {
        written: &read,
        marks: &marks,
    };

    let mut alike = BTreeMap::<(Qualifier, Qualifier, curios_core::WitnessKey), u32>::new();
    let mut names = BTreeMap::new();
    for witness in witnesses {
        let Some((concept, key)) = spelled.key(&witness.type_) else {
            continue;
        };
        // A concept is an authored declaration; a signature that applied anything else spelled none.
        let Some(concept) = concept.qualifier().copied() else {
            continue;
        };
        let module = module_of(witness);
        let place = alike.entry((module, concept, key)).or_default();
        names.insert(
            witness.name,
            curios_core::Global::Witness(curios_core::WitnessId::Spelled {
                concept,
                key,
                module,
                place: *place,
            }),
        );
        *place += 1;
    }

    names
}

/// The module declaring a witness, which its lowering wrote into the name it was lowered under.
fn module_of(witness: &FlatLet) -> Qualifier {
    match witness.name {
        curios_core::Global::Witness(id) => *id.module(),
        curios_core::Global::Authored(_) => {
            unreachable!("a witness is lowered under a witness's name")
        }
    }
}

/// The tables a unit's lowering keys by a declaration's name, each as it stands once every item is lowered.
pub(super) struct Named<'a> {
    pub items: &'a mut [FlatItem],
    pub witnesses: &'a mut BTreeSet<curios_core::Global>,
    pub minted: &'a mut curios_core::Minted,
    pub imports: &'a mut curios_core::Imports,
    pub lints: &'a mut [Lint],
}

/// Give every witness of the unit whose signature spells a concept and a key the name that says so ([`witness_names`]), in each table that holds the name it was lowered under.
pub(super) fn name_witnesses(
    named: Named<'_>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    concepts: &BTreeMap<curios_core::Global, curios_core::ConceptDecl>,
    scope: &[&PreparedText],
) {
    curios_profile::profile!("name_witnesses");
    let names = witness_names(named.items, struct_decls, concepts, scope);
    if names.is_empty() {
        return;
    }
    let renamed = |name: curios_core::Global| names.get(&name).copied().unwrap_or(name);

    for let_ in named.items.iter_mut().flat_map(FlatItem::lets_mut) {
        let_.name = renamed(let_.name);
    }
    *named.witnesses = std::mem::take(named.witnesses)
        .into_iter()
        .map(renamed)
        .collect();
    named.minted.from = std::mem::take(&mut named.minted.from)
        .into_iter()
        .map(|(name, declaration)| (renamed(name), declaration))
        .collect();
    named.imports.by_item = std::mem::take(&mut named.imports.by_item)
        .into_iter()
        .map(|(name, in_scope)| (renamed(name), in_scope))
        .collect();
    for binder in named
        .lints
        .iter_mut()
        .filter_map(|lint| lint.binder.as_mut())
    {
        binder.declaration = binder.declaration.map(renamed);
    }
}
