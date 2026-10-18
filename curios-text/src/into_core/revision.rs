//! A unit lowered over a baseline, made a version of the baseline's: its items, its records, where it holds each name and the declarations its items make are the baseline's, changed where the walk lowered a declaration again, took one away or named a kept witness otherwise ([`revise`]).
//!
//! **A declaration's items stand in the order the walk meets them.** The walk passes each declaration in the order a whole lowering writes its items. What it did not keep is taken out first, and an item lowered now is put midway between the last item the walk passed and the next it keeps, so a declaration lowered again at every keystroke stands at one place, between the same two. A kept declaration written ahead of what the walk has passed, its text moved, is moved after it. So the unit is stored as a whole lowering of its text stores it.
//!
//! **Room runs out only where items pile up between two kept ones**, each halving what is left: the version is then placed apart again, every item at its rank, and the walk's changes made over that.

#[cfg(test)]
mod tests;

use {
    super::{Key, LoweredDeclaration, PreparedText, Records},
    curios_utilities::{Persistent, Qualifier, Sorted},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        rc::Rc,
    },
};

/// One declaration as a walk over a baseline met it, in the order it met them: what its record and its first item are among the walk's, and where the baseline holds its first item, for a declaration the walk kept. Or a module the walk passed unentered, with everything under it, by where the baseline holds the first item of its last declaration (`passed`): what is put in after it stands after that.
#[derive(Debug, Clone, Copy)]
pub(super) struct Step {
    pub kept: Option<i64>,
    pub passed: Option<i64>,
    pub record: usize,
    pub from: usize,
}

impl Step {
    /// Whether the walk lowered this declaration now.
    pub(super) fn made(&self) -> bool {
        self.kept.is_none() && self.passed.is_none()
    }
}

/// What a unit lowered over a baseline holds otherwise than the baseline does.
#[derive(Debug, Clone, Default)]
pub(super) struct Revision {
    /// Every name declared by an item the unit does not hold as the baseline holds it: lowered now, or a kept witness named otherwise.
    pub anew: BTreeSet<curios_core::Global>,
    /// Every name the baseline declares and the unit does not.
    pub dropped: BTreeSet<curios_core::Global>,
}

/// A unit lowered over a baseline, as a version of the baseline's ([`revise`]).
pub(super) struct Revised {
    pub items: curios_core::Items,
    pub records: Records,
    pub places: curios_core::Places,
    pub declarations: curios_core::Declarations,
    pub bodies: Bodies,
    pub texts: Persistent<Key, i64>,
    pub revision: Revision,
    /// Each kept witness the walk named otherwise, by the name the baseline gives it and the one it takes now.
    pub renamed: Vec<(curios_core::Global, curios_core::Global)>,
}

/// The unit the walk over `baseline` made, `steps` being the declarations it met in order, `records` their records and `made` their items, each lowered or kept, under the names the walk gives them: the baseline's tables with what the walk changed.
pub(super) fn revise(
    baseline: &PreparedText,
    steps: &[Step],
    records: &[Rc<LoweredDeclaration>],
    made: &[curios_core::Item],
    gone: &[Gone<'_>],
) -> Revised {
    let ends = steps
        .iter()
        .skip(1)
        .map(|step| step.from)
        .chain([made.len()])
        .collect::<Vec<_>>();
    // What each kept declaration's items are named now against what the baseline names them: a witness is named by what its signature spells and where it stands among its module's, which an edit elsewhere can move.
    let earlier = &baseline.core().items;
    let mut anew = BTreeSet::new();
    let mut renamed = Vec::new();
    let mut named = HashSet::new();
    for (step, end) in steps.iter().zip(&ends) {
        let made = &made[step.from..*end];
        if step.passed.is_some() {
            continue;
        }
        let Some(first) = step.kept else {
            anew.extend(made.iter().flat_map(|item| item.declared_names()).copied());
            continue;
        };
        for (index, (place, item)) in run(earlier, place_of(first), made.len())
            .into_iter()
            .zip(made)
            .enumerate()
        {
            let now = item.declared_names();
            let then = earlier.names(place);
            if now != then {
                renamed.extend(
                    then.iter()
                        .map(|name| **name)
                        .zip(now.iter().map(|name| **name)),
                );
                anew.extend(now.into_iter().copied());
                named.insert(step.from + index);
            }
        }
    }

    let tables = Tables::of(baseline);
    let walk = Walk {
        steps,
        ends: &ends,
        records,
        made,
        named: &named,
        gone,
    };
    let (items, records, places, declarations, bodies, texts, removed) = walk
        .over(&tables)
        .or_else(|| walk.over(&tables.respaced()))
        .expect("a version placed apart again has room for one walk's items");
    let dropped = removed
        .iter()
        .flat_map(|(_, names)| names)
        .filter(|name| places.place(name).is_none())
        .copied()
        .collect();

    Revised {
        items,
        records,
        places,
        declarations,
        bodies,
        texts,
        revision: Revision { anew, dropped },
        renamed,
    }
}

/// One of a baseline's declarations a walk did not keep: its record, the place of its first item in the baseline, and the places and the names of its items there.
pub(super) struct Gone<'b> {
    pub record: &'b Rc<LoweredDeclaration>,
    pub first: i64,
    pub places: Vec<usize>,
    pub names: Vec<curios_core::Global>,
}

/// Each of `baseline`'s declarations the walk did not keep ([`Gone`]): of each body of `walked`, the ones none of `steps` keeps, and every one of each body of `removed`, which the unit no longer holds. Every other body's are kept, the walk having passed it.
pub(super) fn gone<'b>(
    baseline: &'b PreparedText,
    steps: &[Step],
    walked: impl IntoIterator<Item = Qualifier>,
    removed: &[Qualifier],
) -> Vec<Gone<'b>> {
    let kept = steps
        .iter()
        .filter_map(|step| step.kept)
        .collect::<HashSet<_>>();
    let items = &baseline.core().items;
    let bodies = walked
        .into_iter()
        .chain(removed.iter().copied())
        .collect::<BTreeSet<_>>();

    let mut gone = bodies
        .iter()
        .filter_map(|body| baseline.bodies().get(body))
        .flat_map(|body| body.firsts.iter().copied())
        .filter(|first| !kept.contains(first))
        .filter_map(|first| {
            let record = baseline.lowerings().get(first)?;
            let places = run(items, place_of(first), record.facts().len());
            let names = places
                .iter()
                .flat_map(|place| names_at(items, *place))
                .collect();
            Some(Gone {
                record,
                first,
                places,
                names,
            })
        })
        .collect::<Vec<_>>();
    gone.sort_by_key(|gone| gone.first);

    gone
}

/// The texts of the declarations `gone` takes away that no record of `records` was lowered from or kept by: what the unit's anchors no longer hold.
pub(super) fn texts_gone(gone: &[Gone<'_>], records: &[Rc<LoweredDeclaration>]) -> Vec<Key> {
    let sited = records
        .iter()
        .filter_map(|record| record.text())
        .collect::<HashSet<_>>();

    gone.iter()
        .filter_map(|gone| gone.record.text())
        .filter(|text| !sited.contains(text))
        .collect()
}

/// The declarations of `records` a later lowering may keep, by the text each was lowered from: the place of each one's first item.
pub(super) fn texts_of(records: &Records) -> Persistent<Key, i64> {
    let mut texts = Persistent::new();
    for (first, record) in records.iter() {
        if let Some(key) = record.key() {
            texts.insert(key, first);
        }
    }

    texts
}

/// Each module body's declarations, by the place of each one's first item, with how many of them no later lowering may keep: what a lowering over the unit finds a body's declarations by, and whether it may pass the body unwalked.
#[derive(Clone, Default)]
pub(crate) struct Bodies {
    by: Sorted<Qualifier, Rc<Body>>,
}

/// One module body's declarations ([`Bodies`]).
#[derive(Clone, Debug, Default)]
pub(crate) struct Body {
    pub firsts: BTreeSet<i64>,
    pub unkeyable: usize,
    /// How many of them registered a `foreign` row.
    pub foreign: usize,
}

impl Bodies {
    /// The bodies of the declarations `records` holds.
    pub(super) fn of(records: &Records) -> Self {
        let mut bodies = Self::default();
        let mut gathered = BTreeMap::<Qualifier, Body>::new();
        for (first, record) in records.iter() {
            let body = gathered.entry(record.body()).or_default();
            body.firsts.insert(first);
            body.unkeyable += usize::from(!record.keyable());
            body.foreign += usize::from(record.foreign());
        }
        for (module, body) in gathered {
            bodies.by.insert(module, Rc::new(body));
        }

        bodies
    }

    /// The declarations of the body `module`, where it holds any.
    pub(super) fn get(&self, module: &Qualifier) -> Option<&Body> {
        self.by.get(module).map(Rc::as_ref)
    }

    /// Every body that holds a declaration, with its declarations.
    pub(super) fn iter(&self) -> impl Iterator<Item = (&Qualifier, &Body)> {
        self.by.iter().map(|(module, body)| (module, &**body))
    }

    /// The first item's place of the last declaration `root` or a module under it holds, where they hold any: what a walk that passes them unentered places what follows after.
    pub(super) fn last_within(&self, root: &Qualifier) -> Option<i64> {
        let mut last = self.get(root).and_then(|body| body.firsts.last().copied());
        let mut at = *root;
        while let Some((module, body)) = self.by.after(&at) {
            if !module.is_within(root) {
                break;
            }
            last = last.max(body.firsts.last().copied());
            at = *module;
        }

        last
    }

    /// `record`, whose first item stands at `first`, among its body's declarations.
    fn add(&mut self, first: i64, record: &LoweredDeclaration) {
        let mut body = self
            .by
            .get(&record.body())
            .map(|body| (**body).clone())
            .unwrap_or_default();
        body.firsts.insert(first);
        body.unkeyable += usize::from(!record.keyable());
        body.foreign += usize::from(record.foreign());
        self.by.insert(record.body(), Rc::new(body));
    }

    /// `record`, whose first item stood at `first`, taken out of its body's declarations.
    fn remove(&mut self, first: i64, record: &LoweredDeclaration) {
        let Some(held) = self.by.get(&record.body()) else {
            return;
        };
        let mut body = (**held).clone();
        body.firsts.remove(&first);
        body.unkeyable -= usize::from(!record.keyable());
        body.foreign -= usize::from(record.foreign());
        match body.firsts.is_empty() {
            true => {
                self.by.remove(&record.body());
            }
            false => self.by.insert(record.body(), Rc::new(body)),
        }
    }
}

/// A baseline's tables at the places a version of its items holds them at, with where it holds what the baseline holds at each of its own places.
struct Tables {
    items: curios_core::Items,
    records: Records,
    places: curios_core::Places,
    declarations: curios_core::Declarations,
    bodies: Bodies,
    texts: Persistent<Key, i64>,
    /// The place of the version's item for each place of the baseline's own: `None` where the two are one.
    from: Option<HashMap<usize, usize>>,
}

impl Tables {
    /// `baseline`'s tables, at the places of a version of its items: its own, where its items are a version's already.
    fn of(baseline: &PreparedText) -> Self {
        let earlier = &baseline.core().items;
        if earlier.is_version() {
            return Self {
                items: earlier.clone(),
                records: baseline.lowerings().clone(),
                places: baseline.places().clone(),
                declarations: baseline.declarations().clone(),
                bodies: baseline.bodies().clone(),
                texts: baseline.by_text().clone(),
                from: None,
            };
        }

        let items = earlier.version();
        let from = earlier
            .places()
            .map(|place| (place, earlier.place_in_version(place)))
            .collect::<HashMap<_, _>>();
        Self::at(items, baseline.lowerings(), from)
    }

    /// These tables, every item placed apart again at its rank.
    fn respaced(&self) -> Self {
        let items = self.items.respaced();
        let moved = self
            .items
            .places()
            .zip(items.places())
            .collect::<HashMap<_, _>>();
        let from = match &self.from {
            Some(from) => from
                .iter()
                .map(|(own, place)| (*own, moved[place]))
                .collect(),
            None => moved.clone(),
        };
        let mut records = Records::new();
        for (first, record) in self.records.iter() {
            records.insert(position(moved[&place_of(first)]), Rc::clone(record));
        }
        let module = curios_core::Module {
            items,
            ..curios_core::Module::default()
        };

        Self {
            places: curios_core::Places::of(&module),
            declarations: curios_core::Declarations::of(&module),
            items: module.items,
            bodies: Bodies::of(&records),
            texts: texts_of(&records),
            records,
            from: Some(from),
        }
    }

    /// The tables of `items`, a version, `records` keyed by the baseline's own places, which `from` takes to the version's.
    fn at(items: curios_core::Items, records: &Records, from: HashMap<usize, usize>) -> Self {
        let mut placed = Records::new();
        for (first, record) in records.iter() {
            placed.insert(position(from[&place_of(first)]), Rc::clone(record));
        }
        let module = curios_core::Module {
            items,
            ..curios_core::Module::default()
        };

        Self {
            places: curios_core::Places::of(&module),
            declarations: curios_core::Declarations::of(&module),
            items: module.items,
            bodies: Bodies::of(&placed),
            texts: texts_of(&placed),
            records: placed,
            from: Some(from),
        }
    }

    /// Where the version holds what the baseline holds at its own place `place`.
    fn place(&self, place: usize) -> usize {
        match &self.from {
            Some(from) => from[&place],
            None => place,
        }
    }
}

/// What the walk did, to be made over a baseline's tables.
struct Walk<'w> {
    steps: &'w [Step],
    ends: &'w [usize],
    records: &'w [Rc<LoweredDeclaration>],
    made: &'w [curios_core::Item],
    /// The walk's items, by their place among them, that are kept items named otherwise.
    named: &'w HashSet<usize>,
    /// The baseline's declarations the walk did not keep.
    gone: &'w [Gone<'w>],
}

/// A version's items, records, places, declarations and bodies, with every place an item was taken out of or replaced at and the names it held there.
type Made = (
    curios_core::Items,
    Records,
    curios_core::Places,
    curios_core::Declarations,
    Bodies,
    Persistent<Key, i64>,
    Vec<(usize, Vec<curios_core::Global>)>,
);

impl Walk<'_> {
    /// The walk's changes made over `tables`. `None` where no place is left between two items for one put between them.
    fn over(&self, tables: &Tables) -> Option<Made> {
        let mut items = tables.items.clone();
        let mut records = tables.records.clone();
        let mut bodies = tables.bodies.clone();
        let mut texts = tables.texts.clone();
        let (mut removed, mut placed) = (Vec::new(), Vec::new());

        // What the walk did not keep is taken out first, so what is put in has the room between kept items.
        for gone in self.gone {
            let first = position(tables.place(place_of(gone.first)));
            records.remove(first);
            bodies.remove(first, gone.record);
            if let Some(key) = gone.record.key()
                && texts.get(&key) == Some(&first)
            {
                texts.remove(&key);
            }
            for place in run(&tables.items, place_of(first), gone.record.facts().len()) {
                removed.push((place, names_at(&items, place)));
                items.remove(place);
            }
        }

        let mut cursor: Option<usize> = None;
        let mut index = 0;
        while let Some(step) = self.steps.get(index) {
            // A module passed unentered stands where it stood, and what follows stands after it.
            if let Some(last) = step.passed {
                let first = tables.place(place_of(last));
                let count = tables
                    .records
                    .get(position(first))
                    .map_or(1, |record| record.facts().len());
                cursor = run(&tables.items, first, count).last().copied().max(cursor);
                index += 1;
                continue;
            }
            let Some(first) = step.kept else {
                // A run of declarations lowered now, put between the last item passed and the next kept one together, so the room between the two is shared among them.
                let run = index;
                while self.steps.get(index).is_some_and(Step::made) {
                    index += 1;
                }
                let steps = &self.steps[run..index];
                let made = steps
                    .iter()
                    .zip(&self.ends[run..index])
                    .flat_map(|(step, end)| self.made[step.from..*end].iter().cloned())
                    .collect::<Vec<_>>();
                let mut at = items.insert_run(cursor, made)?.into_iter();
                for (step, end) in steps.iter().zip(&self.ends[run..index]) {
                    let held = at.by_ref().take(end - step.from).collect::<Vec<_>>();
                    if let Some(&head) = held.first() {
                        let record = &self.records[step.record];
                        records.insert(position(head), Rc::clone(record));
                        bodies.add(position(head), record);
                        if let Some(key) = record.key() {
                            texts.insert(key, position(head));
                        }
                    }
                    placed.extend(&held);
                    cursor = held.last().copied().or(cursor);
                }
                continue;
            };
            let record = &self.records[step.record];
            let made = &self.made[step.from..self.ends[index]];
            let first = tables.place(place_of(first));
            let mut held = run(&tables.items, first, made.len());
            if let (Some(passed), Some(&head)) = (cursor, held.first())
                && head <= passed
            {
                // Written ahead of what the walk has passed: moved after it.
                records.remove(position(first));
                bodies.remove(position(first), record);
                if let Some(key) = record.key() {
                    texts.remove(&key);
                }
                let mut after = cursor;
                for place in &mut held {
                    let names = names_at(&items, *place);
                    let moved = items.relocate(*place, after)?;
                    removed.push((*place, names));
                    placed.push(moved);
                    *place = moved;
                    after = Some(moved);
                }
                if let Some(&head) = held.first() {
                    records.insert(position(head), Rc::clone(record));
                    bodies.add(position(head), record);
                    if let Some(key) = record.key() {
                        texts.insert(key, position(head));
                    }
                }
            }
            for (offset, (place, item)) in held.iter().zip(made).enumerate() {
                if self.named.contains(&(step.from + offset)) {
                    removed.push((*place, names_at(&items, *place)));
                    items.replace(*place, item.clone());
                    placed.push(*place);
                }
            }
            cursor = held.last().copied().or(cursor);
            index += 1;
        }

        let mut places = tables.places.clone();
        for (_, names) in &removed {
            for name in names {
                places.forget(name);
            }
        }
        for &place in &placed {
            places.place_item(&items, place);
        }
        let mut declarations = tables.declarations.clone();
        declarations.revise(&items, &removed, &placed);

        Some((items, records, places, declarations, bodies, texts, removed))
    }
}

/// The places of the `count` items from `first` on.
fn run(items: &curios_core::Items, first: usize, count: usize) -> Vec<usize> {
    let mut places = Vec::with_capacity(count);
    let mut place = Some(first);
    for _ in 0..count {
        let at = place.expect("a declaration's items stand together");
        places.push(at);
        place = items.place_after(Some(at));
    }

    places
}

/// The names the item at `place` declares.
fn names_at(items: &curios_core::Items, place: usize) -> Vec<curios_core::Global> {
    items.names(place).into_iter().copied().collect()
}

/// A place as a record's key.
fn position(place: usize) -> i64 {
    i64::try_from(place).expect("a place is counted in an i64")
}

/// The place a record's key is.
fn place_of(position: i64) -> usize {
    usize::try_from(position).expect("a place is not negative")
}
