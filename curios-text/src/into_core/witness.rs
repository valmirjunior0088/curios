//! Naming a unit's witnesses, once its items are lowered.
//!
//! A `satisfy` declaration is anonymous, and the name the compiler gives it is what it witnesses: its module, the concept its signature applies and the key that signature is spelled at (`curios_core::WitnessId`). The key is read through the definitions a signature names, in any file of the unit, so it cannot be read where the witness is lowered: a witness is lowered under its place among its module's witnesses, and this pass gives every one whose signature spells a key its name before an item leaves the lowering.
//!
//! **A key is read off lowered forms alone**, the unit's and those of the units in scope: a `let` by its body, a group's member not at all, a concept's marks off its record entry's arity. A unit in scope is asked by name ([`PreparedText::written`]), so naming a witness reads the lowered form of each definition its signature names and of no other. How a declaration is written is the lowering's to say, so a witness's name is a function of the text in scope and of nothing that elaborated.
//!
//! **Over a baseline, a kept witness keeps its name** unless its module holds a witness lowered now, whose place among the module's may come before it, or its key read a declaration lowered now or taken away ([`Naming`]): what each key read is recorded with the unit for that ([`BaselineHalf::keyed`](super::BaselineHalf::keyed)).
//!
//! **No lowered term names a witness** — one is reached by resolution, which is elaboration's — so a name changes in the tables that are keyed by it and nowhere else: the items, the unit's witness set, what each declaration minted, the import scope of each, and the binders a lint names.

use {
    super::{FlatItem, FlatLet},
    crate::{Lint, PreparedText},
    curios_utilities::{Plicity, Qualifier},
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashSet},
    },
};

/// A name's declaration as a key reads it: its kind, and its body where it is a `let`'s.
pub(super) type Keyed<'a> = (curios_core::Declared, Option<&'a curios_core::Term>);

/// How `item` declares each of its names, as a key reads it: a `let` with its body, a group's members without theirs.
pub(super) fn keyed_of(item: &FlatItem) -> Vec<(curios_core::Global, Keyed<'_>)> {
    match item {
        FlatItem::Let(let_) => vec![(let_.name, (let_.kind.declared(), Some(&let_.body)))],
        // A member's body is closed over its group, which a key is not read through.
        FlatItem::Rec(lets) => lets
            .iter()
            .map(|let_| (let_.name, (let_.kind.declared(), None)))
            .collect(),
        FlatItem::Kept(rec) => rec
            .definitions
            .iter()
            .map(|definition| (definition.name, (definition.kind.declared(), None)))
            .collect(),
    }
}

/// What a declaration is, as far as a key reads it: a type former, a definition with its body, or anything a key is not read through.
fn written(declared: Option<Keyed<'_>>) -> curios_core::Written {
    match declared {
        Some((declared, body)) => curios_core::Written::of(declared, body),
        None => curios_core::Written::Opaque,
    }
}

/// How a unit's own declarations are read where a key names one: how each is written, and the marks of a concept's parameters, `None` for a name the unit does not declare, which is read as the units in scope declare it.
pub(super) struct Own<'o> {
    pub written: &'o dyn Fn(&curios_core::Global) -> Option<Keyed<'o>>,
    pub marks: &'o dyn Fn(&curios_core::Global) -> Option<Option<Vec<Plicity>>>,
}

/// Which witnesses a lowering names: every one, where the unit is lowered whole; over a baseline, those of `modules`, every other keeping the name it holds, and each kept one of them whose key `spelled` holds, read again where it is not there.
pub(super) struct Naming<'n> {
    pub modules: &'n HashSet<Qualifier>,
    /// What the baseline spells each kept witness at, by the name it holds, for each whose key reads nothing that moved.
    pub spelled: &'n dyn Fn(&curios_core::Global) -> Option<(Qualifier, curios_core::WitnessKey)>,
}

/// The name each witness of `items` takes, by the name it holds, where that is another, beside what each named witness's key read, by the name it takes. A witness whose signature spells a concept and a key is named by them and its place among the witnesses its module declares ahead of it at that concept and key — none in a coherent unit; one whose signature spells none keeps its name.
///
/// `scope` holds every unit in scope, in dependency order.
pub(super) fn witness_names(
    items: &[FlatItem],
    own: &Own<'_>,
    scope: &[&PreparedText],
    naming: Option<&Naming<'_>>,
) -> (
    BTreeMap<curios_core::Global, curios_core::Global>,
    BTreeMap<curios_core::Global, Vec<curios_core::Global>>,
) {
    let witnesses = items
        .iter()
        .flat_map(FlatItem::lets)
        .filter(|let_| matches!(let_.kind, curios_core::DefinitionKind::Witness))
        .filter(|let_| naming.is_none_or(|naming| naming.modules.contains(&module_of(let_))))
        .collect::<Vec<_>>();
    if witnesses.is_empty() {
        return (BTreeMap::new(), BTreeMap::new());
    }

    // What a key reads, noted name by name for the witness being named.
    let read_names = RefCell::new(Vec::new());
    // A name the unit does not declare is read as the nearest unit in scope declares it.
    let read = |name: &curios_core::Global| {
        read_names.borrow_mut().push(*name);
        let declared = (own.written)(name)
            .or_else(|| scope.iter().rev().find_map(|earlier| earlier.written(name)));

        written(declared)
    };
    // A concept's parameters are its record entry's, whose arity states the mark each is declared under.
    let marks = |name: &curios_core::Global| -> Option<Vec<Plicity>> {
        read_names.borrow_mut().push(*name);
        let declared = |earlier: &PreparedText| match earlier.nominal(name)? {
            curios_core::Nominal::Concept { record, .. } => Some(record.arity.marks()),
            _ => None,
        };
        match (own.marks)(name) {
            Some(marks) => marks,
            None => scope.iter().rev().find_map(|earlier| declared(earlier)),
        }
    };
    let spelled = curios_core::Keying {
        written: &read,
        marks: &marks,
    };

    let mut alike = BTreeMap::<(Qualifier, Qualifier, curios_core::WitnessKey), u32>::new();
    let mut names = BTreeMap::new();
    let mut keyed = BTreeMap::new();
    for witness in witnesses {
        read_names.borrow_mut().clear();
        let held = naming.and_then(|naming| (naming.spelled)(&witness.name));
        let key = match held {
            Some(held) => Some(held),
            None => spelled
                .key(&witness.type_)
                // A concept is an authored declaration; a signature that applied anything else spelled none.
                .and_then(|(concept, key)| Some((concept.qualifier().copied()?, key))),
        };
        let reads = std::mem::take(&mut *read_names.borrow_mut());
        let Some((concept, key)) = key else {
            continue;
        };
        let module = module_of(witness);
        let place = alike.entry((module, concept, key)).or_default();
        let name = curios_core::Global::Witness(curios_core::WitnessId::Spelled {
            concept,
            key,
            module,
            place: *place,
        });
        *place += 1;
        if held.is_none() {
            keyed.insert(name, reads);
        }
        if name != witness.name {
            names.insert(witness.name, name);
        }
    }

    (names, keyed)
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

/// Give each witness `names` holds the name it takes there ([`witness_names`]), in each table that holds the name it holds.
pub(super) fn rename_witnesses(
    named: Named<'_>,
    names: &BTreeMap<curios_core::Global, curios_core::Global>,
) {
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
    // One witness may take the name another held, its place among its module's moved: every entry is taken under the name it was held by before any is placed under the one it takes.
    let held = names
        .keys()
        .map(|name| {
            let mints = named.minted.shared(name).cloned();
            let in_scope = named.imports.scoped(name);
            named.minted.forget(name);
            named.imports.forget(name);
            (*name, mints, in_scope)
        })
        .collect::<Vec<_>>();
    for (name, mints, in_scope) in held {
        let taken = renamed(name);
        if let Some(mints) = mints {
            named.minted.declare(&mints, [taken]);
        }
        if let Some(in_scope) = in_scope {
            named.imports.scope(taken, in_scope);
        }
    }
    for binder in named
        .lints
        .iter_mut()
        .filter_map(|lint| lint.binder.as_mut())
    {
        binder.declaration = binder.declaration.map(renamed);
    }
}
