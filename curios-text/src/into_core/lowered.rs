//! What one source declaration lowered to beside its items, and what it asked of its scope to get there: the record a later lowering of the same sources keeps the declaration's lowering by.
//!
//! **A lowering is a function of its declaration's text and the names it resolved.** Every counter a lowering mints from starts at zero for each declaration, and a declaration reads its scope through two doors alone: `Context::resolve_term_name` for a name written as a path, `Context::resolve_bare` for a bare one no binder of its own binds. So a declaration whose text is the one a baseline lowered, and whose every resolution answers as it did, lowers to what it lowered to, and is not lowered: its items, its registry entries, what it minted, its import scopes and its lints are taken from the baseline ([`Baseline::kept`]).
//!
//! **Decided by asking again, through the same doors.** A resolution marks the `use` it came through as used and the mount it was spelled under as reached, so a trace asked again leaves the unit's tables as lowering the declaration would have. An answer that differs, or a resolution the scope now refuses, lowers the declaration from its text, where the refusal is raised as it always was.
//!
//! **Found by its text, confirmed by its text.** A lowered declaration is held under its anchor and the stamp of the text it was lowered from, which is how a position finds its declaration too; a stamp is thirty-two bits, so the text itself is compared before a lowering is believed.

use {
    super::{Context, FlatItem, FlatLet, Journal, LoweredItem, Lowering},
    crate::{Lint, LintKind, LintedBinder, PreparedText},
    curios_utilities::{Qualifier, Report},
    std::{collections::HashMap, rc::Rc},
};

/// A name a declaration's lowering resolved through its scope, as it was written: a path of several segments or an absolute one through the modules it may see, a bare one through the bindings in scope where it stands.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub(crate) struct Lookup {
    pub absolute: bool,
    pub path: Qualifier,
}

/// An `unused-binder` lint as a lowered declaration holds it: placed in the declaration's text, and naming the definition it is about by its place among the declaration's, since a witness's name is given anew by every lowering.
#[derive(Debug, Clone)]
#[curios_archive::archived]
struct Unused {
    message: String,
    at: curios_core::Extent,
    definition: u32,
    ordinal: u32,
}

/// What one source declaration lowered to, beside the items themselves, which are its unit's: see the module documentation.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(crate) struct LoweredDeclaration {
    /// The text it was lowered from, by its anchor and the text's stamp. `None` for a declaration no later lowering may keep: one that was built rather than written, one a name of which resolved to nothing, and one whose findings could not be placed in its text.
    key: Option<(curios_core::Anchor, u32)>,
    /// What the unit-wide passes read of each item it lowered to, in the unit's order.
    items: Vec<LoweredItem>,
    /// The definitions it recorded an import scope for, each by its place among the declaration's.
    scoped: Vec<u32>,
    lints: Vec<Unused>,
    /// Every name it resolved through its scope, with what each resolved to.
    trace: curios_core::Trace<Lookup, Qualifier>,
}

impl LoweredDeclaration {
    /// What the declaration `context` has just lowered to `items` is recorded as, `facts` having been taken of them.
    pub(super) fn made(context: &Context, items: &[FlatItem], facts: Vec<LoweredItem>) -> Self {
        let Journal {
            trace,
            scoped,
            lints,
            unbound,
        } = context.journal();
        let names = items.iter().flat_map(FlatItem::names).collect::<Vec<_>>();
        let place = |name: &curios_core::Global| {
            let place = names.iter().position(|declared| declared == name)?;
            u32::try_from(place).ok()
        };

        let scoped = scoped.iter().map(place).collect::<Option<Vec<_>>>();
        let lints = lints
            .iter()
            .map(|lint| {
                let binder = lint.binder?;

                Some(Unused {
                    message: lint.report.message.clone(),
                    at: context.extent(lint.report.span.as_ref()?)?,
                    definition: place(&binder.declaration?)?,
                    ordinal: binder.ordinal,
                })
            })
            .collect::<Option<Vec<_>>>();
        // A name that was written and could not be placed is one the `unused-declaration` lint would lose with a kept lowering.
        let placed = items
            .iter()
            .flat_map(FlatItem::lets)
            .zip(facts.iter().flat_map(|lowered| &lowered.definitions))
            .all(|(let_, declared)| let_.span.is_none() || declared.label.is_some());

        let key = match (unbound, placed, &scoped, &lints) {
            (false, true, Some(_), Some(_)) => context.sited(|key, _| key),
            _ => None,
        };

        Self {
            key,
            items: facts,
            scoped: scoped.unwrap_or_default(),
            lints: lints.unwrap_or_default(),
            trace,
        }
    }

    /// What the unit-wide passes read of each item this lowered to.
    pub(super) fn facts(&self) -> &[LoweredItem] {
        &self.items
    }
}

/// A unit an earlier text of the same sources lowered to, as a later one is lowered over it: its declarations by the text each was lowered from, and where each one's items lie in its module.
pub(super) struct Baseline<'a> {
    unit: &'a PreparedText,
    by_text: HashMap<(curios_core::Anchor, u32), usize>,
    /// The place of each declaration's first item among the unit's.
    first: Vec<usize>,
    /// Whether the unit declares a foreign function at all, which the prelude does not: a row is looked up by its import name, spelled for the asking.
    foreign: bool,
}

impl<'a> Baseline<'a> {
    pub(super) fn of(unit: &'a PreparedText) -> Self {
        let mut by_text = HashMap::with_capacity(unit.lowerings().len());
        let mut first = Vec::with_capacity(unit.lowerings().len());
        let mut next = 0;
        for (index, declaration) in unit.lowerings().iter().enumerate() {
            first.push(next);
            next += declaration.items.len();
            if let Some(key) = declaration.key {
                by_text.insert(key, index);
            }
        }

        Self {
            unit,
            by_text,
            first,
            foreign: unit.foreigns.iter().next().is_some(),
        }
    }

    /// The declaration of this unit that the one `context` stands at was lowered as, by its place: the one lowered from the text that declaration is written in, where every name it resolved resolves here as it did.
    pub(super) fn standing(&self, context: &Context) -> Option<usize> {
        let index = context.sited(|key, text| {
            let index = *self.by_text.get(&key)?;

            (self.unit.anchors.text(key.0, key.1) == Some(text)).then_some(index)
        })??;

        let stands = self.unit.lowerings()[index]
            .trace
            .stands(|lookup, found| context.answers(lookup, found));
        context.forget_asked();

        stands.then_some(index)
    }

    /// Take the declaration at `index` whole into `lowered`, as lowering it again would have written it: its items under the names this lowering gives them, their registry entries and rows, what it minted, its import scopes and its lints.
    pub(super) fn kept(&self, index: usize, context: &Context, lowered: &mut Lowering) {
        let declaration = &self.unit.lowerings()[index];
        let first = self.first[index];

        let mut names = Vec::new();
        for item in &self.unit.core.items[first..first + declaration.items.len()] {
            let kept = match item {
                curios_core::Item::Let(definition) => {
                    FlatItem::Let(self.kept_let(definition, context, lowered))
                }
                // A group of witnesses is closed again under the names this lowering gives its members, which the scope it was closed under spells.
                curios_core::Item::Rec(rec) if rec.definitions.iter().any(is_witness) => {
                    FlatItem::Rec(
                        rec.definitions()
                            .iter()
                            .map(|definition| self.kept_let(definition, context, lowered))
                            .collect(),
                    )
                }
                curios_core::Item::Rec(rec) => {
                    for definition in &rec.definitions {
                        self.keep_entries(
                            &definition.name,
                            definition.name,
                            &definition.kind,
                            lowered,
                        );
                    }

                    FlatItem::Kept(rec.clone())
                }
            };
            names.extend(kept.names());
            lowered.items.push(kept);
        }

        for &place in &declaration.scoped {
            context.record_import_scope(Some(&names[place as usize]));
        }
        context.report(declaration.lints.iter().map(|unused| Lint {
            kind: LintKind::UnusedBinder,
            report: Report {
                span: context.resolved(&unused.at),
                message: unused.message.clone(),
            },
            binder: Some(LintedBinder {
                declaration: Some(names[unused.definition as usize]),
                ordinal: unused.ordinal,
            }),
        }));
        context.keep_declaration(self.unit.minted.declarations[index].clone(), names);
        lowered.declarations.push(Rc::clone(declaration));
    }

    /// One definition of a kept item as the walk holds one it lowered: a witness under the next name of its module, as a lowering gives it before its signature is read.
    fn kept_let(
        &self,
        definition: &curios_core::Definition,
        context: &Context,
        lowered: &mut Lowering,
    ) -> FlatLet {
        let name = match definition.kind {
            curios_core::DefinitionKind::Witness => {
                curios_core::Global::Witness(context.fresh_witness())
            }
            _ => definition.name,
        };
        self.keep_entries(&definition.name, name, &definition.kind, lowered);

        FlatLet {
            name,
            kind: definition.kind.clone(),
            // What is read of where it was written was taken where it was lowered.
            span: None,
            island: definition.island,
            type_: definition.type_.clone(),
            body: definition.body.clone(),
        }
    }

    /// Take what the baseline's tables hold under `held`, a name one of its definitions declares, into `lowered` under `name`.
    fn keep_entries(
        &self,
        held: &curios_core::Global,
        name: curios_core::Global,
        kind: &curios_core::DefinitionKind,
        lowered: &mut Lowering,
    ) {
        let core = &self.unit.core;
        if let Some(entry) = core.induct_decls.get(held) {
            lowered.induct_decls.insert(name, entry.clone());
        }
        if let Some(entry) = core.struct_decls.get(held) {
            lowered.struct_decls.insert(name, entry.clone());
        }
        if let Some(entry) = core.concepts.get(held) {
            lowered.concepts.insert(name, entry.clone());
        }
        match kind {
            curios_core::DefinitionKind::Witness => {
                lowered.witnesses.insert(name);
            }
            curios_core::DefinitionKind::Test => lowered.tests.push(name),
            _ => {}
        }
        if self.foreign
            && let Some(path) = held.qualifier()
            && let Some(row) = self.unit.foreigns.get(&path.join())
        {
            lowered.foreigns.register((**row).clone());
        }
    }
}

fn is_witness(definition: &curios_core::RecDefinition) -> bool {
    matches!(definition.kind, curios_core::DefinitionKind::Witness)
}
