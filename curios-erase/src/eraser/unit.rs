//! The eraser's entry points: a unit's items erased through the kernel that certified them, each into a form at every assignment of the sorts it leaves open and on a budget restored for it; a unit over a baseline keeping the forms the baseline has; a program's entry erased into a fragment of its own and linked; and what each item read of the declarations around it, recorded for a later erasure to ask.

use {
    super::{Eraser, Outcome},
    crate::{
        Asked, AskedBy, AskedItems, AskedRecord, AtSorts, ErasedBaseline, ErasedForms, ErasedItem,
        ErasedOver, ErasedUnit, Erasures, Error, Part, Sorts, declared, emitted, evaluate, link,
        reduced,
    },
    curios_cert::Reader,
    curios_utilities::grown,
    std::{
        borrow::Cow,
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// Erase the entrypoint body into the entry block of a fragment of its own — [`erase_program`]'s tail. The program's own body owns what it mints, the same way an item owns what its body mints: the entry is emitted as `func/main`, so that is the name its lifted lambdas descend from. The verifier is the rejection point for the recursion classes the language does not admit — an evaluation cycle, an initializer that performs an effect — and hands those back as the refusal; a malformed fragment is an erasure fault, which the verifier panics on rather than returning here.
fn seal_entry(
    mut lowering: Eraser<'_>,
    context: &mut Reader,
    body: &curios_core::Term,
    expected: &curios_core::Term,
) -> Result<ErasedItem, Error> {
    lowering.types.clear();
    lowering.builder.open_block();
    let outcome = lowering.with_owner("main".to_string(), |lowering| {
        lowering.walk(context, body, expected, None)
    })?;
    let outcome = force_entry(&mut lowering, context, expected, outcome)?;
    let entry = lowering.seal(outcome);
    lowering.builder.set_entry(entry);

    lowering.into_erased(&[])
}

/// The entrypoint boundary: an `Io(T)` tail is a *description*, and the emitted `func/main` is the one place anything forces one, handing back what it yields — or the suspension it answers, surfaced for the host to wait on and advance ([`Eraser::yield_to_host`]).
///
/// Nothing else in the language may: there is no eliminator from `Io(T)` to `T`, which is what makes every term of non-`Io` type pure by typing. The force is type-directed rather than unconditional so a non-`Io` tail erases unforced — the erasure unit tests state such tails directly. What makes it mandatory in production is `curios-pipeline`, which checks an authored tail against `Io({})` and a test program's against `Io(Bool)`. A program's meaning is the effects its description performs, so the unit it yields is read by no host; a test program's yield is its verdict, which the runner reads ([`ENTRY`](curios_abi::ENTRY)).
fn force_entry(
    lowering: &mut Eraser<'_>,
    context: &mut Reader,
    expected: &curios_core::Term,
    outcome: Outcome,
) -> Result<Outcome, Error> {
    let Outcome::Emitted(description) = outcome else {
        return Ok(outcome);
    };
    if !matches!(
        &*reduced(context, expected)?,
        curios_core::Subterm::Intrinsic(curios_core::Intrinsic::IoType(_))
    ) {
        return Ok(Outcome::Emitted(description));
    }

    let answer = emitted!(lowering.bind(
        None,
        curios_anf::Rhs::Apply {
            callee: description,
            arguments: Vec::new(),
        },
    ));

    let polled = lowering.suspended_poll(context)?;
    Ok(Outcome::Emitted(lowering.yield_to_host(answer, polled)))
}

/// A program erased and linked: one module holding what its entry reaches, and where the function each linked declaration defines landed.
#[derive(Debug)]
pub struct ErasedProgram {
    module: curios_anf::Module,
    /// The function each name that defines one is bound to, at the sorts of the form it was linked as. A function an item computed is bound under a name and defined by none, so it is not among them.
    defined: BTreeMap<AtSorts, curios_anf::FunctionId>,
}

impl ErasedProgram {
    pub(crate) fn new(
        module: curios_anf::Module,
        defined: BTreeMap<AtSorts, curios_anf::FunctionId>,
    ) -> Self {
        Self { module, defined }
    }

    /// Mark every function a definition `certification` classifies `Total` erased to as total, at every form — see [`curios_anf::Function::total`]. The kernel judged the definition at each assignment of its sorts, so its totality holds of each form. Each is found through where its name was linked, so a definition that erased to anything but a function, or that the program does not reach, has nothing to mark, and a function no record names keeps the conservative `false`: a closure a top-level value was computed to among them, which its name is bound to and no declaration defines.
    ///
    /// Run on the linked program, once per record of what it was erased from: every unit's in scope and the entry's own. Nothing reads the flag before the back half lowers what this leaves.
    ///
    /// **Asked by the names the program linked, never walked.** A record classifies every definition of its unit and a program links a few of them, so each linked name is asked of the record, and the record is read for what the program took of its unit.
    pub fn mark_total(&mut self, certification: &curios_core::Certification) {
        let total = self
            .defined
            .iter()
            .filter(|((name, _), _)| {
                certification
                    .totality(name)
                    .is_some_and(|totality| totality.is_total())
            })
            .map(|(_, function)| *function)
            .collect::<Vec<_>>();
        for id in total {
            self.module.mark_total(id);
        }
    }

    /// The finished erased module — what the back half of the pipeline lowers.
    pub fn into_module(self) -> curios_anf::Module {
        self.module
    }
}

/// Erase one unit's items through `kernel`, the kernel that certified the unit, each into its forms, over what its scope erased to.
///
/// What every reading reads is the kernel's: the unit's definitions as its walk defined them and the units in scope it mounted, each item read at each assignment of the sorts it leaves open, as the walk judged it. The items erase in the order the module stores them, every item after the items it reaches — so the erased form of every name an item uses is in hand when it is asked for.
///
/// **A unit and a program are erased by one walk, and only a program is linked.** The entry is not a unit's to carry: a program is its module and the term it closes with ([`Program`](curios_core::Program)), so [`erase_program`] runs this same walk, erases the entry at the type it states, which [`Zonked`](curios_core::Zonked) holds, and links what the entry reaches.
pub fn erase_unit(
    kernel: &mut curios_cert::Kernel<'_>,
    scope: Erasures<'_>,
    module: &curios_core::Zonked<curios_core::Module>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = module.as_module();
        let places = curios_core::Places::of(module);
        kernel.read(|reader| erase_items(reader, scope, (module, &places), None))
    })
}

/// [`erase_unit`] for a unit compiled over a baseline: an item keeps the erased forms `baseline` has for it, the baseline's own and no copy of them, where the elaborator kept its declaration and everything its erasure read there digests as it did (`over`), and every other is erased. The walk visits the items an edit could reach and no other; where the module is a version of the baseline's, the unit is a version of the baseline's unit.
pub fn erase_unit_over(
    kernel: &mut curios_cert::Kernel<'_>,
    scope: Erasures<'_>,
    module: &curios_core::Zonked<curios_core::Module>,
    baseline: ErasedBaseline<'_>,
) -> Result<ErasedUnit, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = module.as_module();
        let over = ErasedOver::new(baseline, module, scope);
        kernel.read(|reader| erase_items(reader, scope, (module, baseline.now), Some(over)))
    })
}

/// [`erase_unit`] for a program: its module erased over its scope through `kernel`, the kernel that certified the program, its entry erased at the type it states, and the two linked with what they reach of the scope — the module whose `ErasedProgram::into_module` the back half lowers.
pub fn erase_program(
    kernel: &mut curios_cert::Kernel<'_>,
    scope: Erasures<'_>,
    program: &curios_core::Zonked<curios_core::Program>,
) -> Result<ErasedProgram, Error> {
    curios_profile::profile!("erase_unit");
    grown(|| {
        let module = &program.as_program().module;
        let places = curios_core::Places::of(module);
        let (body, type_) = program.entry();
        kernel.read(|reader| {
            let own = erase_items(reader, scope, (module, &places), None)?;
            let entry = {
                let externals = |name: &curios_core::Global, sorts: Sorts| {
                    own.find(name, sorts).or_else(|| scope.find(name, sorts))
                };
                let declared = |name: &curios_core::Global| {
                    declared(module, &places, name).or_else(|| scope.definition(name))
                };
                reader.restore_budget();
                seal_entry(
                    Eraser::over(&externals, &declared, (&[], Sorts::NONE)),
                    reader,
                    body,
                    type_,
                )?
            };

            link(scope, &own, &entry)
        })
    })
}

/// Erase `module`'s items in the order it stores them, each into its forms, but for those `over` keeps the baseline's erased forms of, which are not read. `places` says where `module` holds each name. What erasing each item read is kept with the unit ([`AskedItems`]): of the declarations around it, their signatures and bodies through the kernel's reads and their kinds off their items, of each erased form it read how a name is bound in, and of each erased form its evaluation ran, the form whole.
///
/// **Over a baseline whose module this one is a version of, the unit begins as the baseline's**, every item standing where the baseline holds it with what its erasure read, and the walk visits the items an edit could reach alone ([`ErasedOver`]); over any other, it is made item by item.
pub fn erase_items(
    reader: &mut Reader,
    scope: Erasures<'_>,
    (module, places): (&curios_core::Module, &curios_core::Places),
    mut over: Option<ErasedOver<'_>>,
) -> Result<ErasedUnit, Error> {
    let shared = over.as_ref().and_then(|over| {
        over.changes()
            .map(|changes| (over.baseline(), changes.clone()))
    });
    let (mut unit, mut asked) = match &shared {
        Some((baseline, changes)) => {
            // What the baseline held where the module holds another item or none is taken out, names and all: every item the module holds anew is one the walk visits, made again by the elaborator.
            let mut unit = baseline.erased.version();
            for place in changes.replaced.iter().chain(&changes.removed) {
                unit.forget(*place, &baseline.core.items.names(*place));
            }

            (unit, baseline.erased.asked_items().clone())
        }
        None => (ErasedUnit::default(), AskedItems::default()),
    };
    if shared.is_some()
        && let Some(over) = &over
    {
        for name in over.lost() {
            asked.forget(name);
        }
    }
    let walk = match (&over, &shared) {
        (Some(over), Some(_)) => over.visited().collect::<Vec<_>>(),
        _ => module.items.places().collect(),
    };
    // The first name of each item erased here, whose trace is its own, and every other name such an item declares.
    let (mut made, mut others) = (Vec::new(), Vec::new());
    for place in walk {
        let item = &module.items[place];
        let names = item.declared_names();
        let kept = over.as_mut().and_then(|over| over.keep(place, item, &unit));
        let taken = match kept {
            Some(_) => None,
            None => over
                .as_ref()
                .and_then(|over| over.taken(place, item, &unit)),
        };
        let record = match (kept, taken) {
            (Some((held, trace)), _) => {
                unit.keep(place, item, held);
                if let Some(name) = names.first() {
                    asked.share(**name, trace);
                }
                continue;
            }
            (None, Some((held, record))) => {
                curios_profile::sample!("erase::taken", record.len());
                unit.keep(place, item, held);

                record
            }
            (None, None) => {
                let (erased, read) = erase_forms(reader, scope, &unit, (module, places), item)?;
                // One sample for each item erased, with how much its erasure read.
                curios_profile::sample!("erase::asked", read.len());
                unit.push(place, item, erased);

                Rc::new(AskedRecord::new(read))
            }
        };
        made.extend(names.first().map(|name| **name));
        // A record is filed under the first name, so one the baseline filed under another is an item this one now holds the name of.
        for name in names.iter().skip(1) {
            asked.forget(name);
            others.push(**name);
        }
        if let Some(name) = names.first() {
            asked.share(**name, record);
        }
    }
    // Who read what of the unit's records: the baseline's, with the edges of every item erased again, or no longer held, replaced.
    let readers = over.as_ref().map(|over| {
        let earlier = over.earlier();
        let mut readers = earlier.readers().clone();
        for name in made.iter().chain(&others).chain(over.lost()) {
            readers.replace(
                *name,
                earlier.asked(name).map(|held| &***held),
                asked.held(name).map(|held| &***held),
            );
        }

        readers
    });
    match (&over, &readers) {
        (Some(over), Some(readers)) => {
            revise_foreign(&mut asked, &unit, scope, over, &made, &others, readers);
        }
        _ => note_foreign(&mut asked, &unit, scope),
    }
    let unit = unit.asking(asked);

    Ok(match readers {
        Some(readers) => unit.reading(readers),
        None => unit,
    })
}

/// Record what erasing `unit`'s items read of declarations the unit does not hold, each part by the digest it has in `scope`: what a later erasure over this unit asks a read of another unit again by. What will not digest is recorded nowhere, and a read with no record does not stand.
fn note_foreign(asked: &mut AskedItems, unit: &ErasedUnit, scope: Erasures<'_>) {
    let parts = asked
        .records()
        .flat_map(|(_, record)| foreign_parts(record, unit, scope))
        .collect::<BTreeSet<_>>();
    for part in parts {
        if let Ok(digest) = scope.part_digest(part) {
            asked.record_abroad(part, digest);
        }
    }
}

/// [`note_foreign`] for an erasure over a baseline: the baseline's table, revised where an item was erased again or dropped. Each part an item erased again reads now is recorded, digested again where nothing was recorded of it or the unit declaring it moved, and taken out where it will not digest; each part an item erased again or dropped read before is taken out where no item reads it now; and each part of a name the unit declares now and the baseline did not is taken out. A part a kept form's erasure read of a unit that moved digests to what it did, or the form would have been erased again, so its record stands; and an erasure that erased and dropped nothing, in a scope that did not move, holds the baseline's table.
fn revise_foreign(
    asked: &mut AskedItems,
    unit: &ErasedUnit,
    scope: Erasures<'_>,
    over: &ErasedOver<'_>,
    made: &[curios_core::Global],
    others: &[curios_core::Global],
    readers: &AskedBy,
) {
    let (earlier, baseline) = (over.earlier(), over.baseline());
    asked.read_abroad_of(earlier.asked_items());
    if scope.settled() && made.is_empty() && over.lost().is_empty() {
        return;
    }

    let read = made
        .iter()
        .filter_map(|name| asked.held(name))
        .flat_map(|record| foreign_parts(record, unit, scope))
        .collect::<BTreeSet<_>>();
    let dropped = made
        .iter()
        .chain(others)
        .chain(over.lost())
        .filter_map(|name| earlier.asked(name))
        .flat_map(|record| foreign_parts(record, unit, scope))
        .collect::<BTreeSet<_>>();
    for (name, part) in read {
        if asked.abroad(&name, part).is_some() && scope.unmoved(&name) {
            continue;
        }
        match scope.part_digest((name, part)) {
            Ok(digest) => asked.record_abroad((name, part), digest),
            Err(_) => asked.forget_abroad((name, part)),
        }
    }
    for (name, part) in dropped {
        if !readers.asks(&name, part) {
            asked.forget_abroad((name, part));
        }
    }
    // A name the unit declares now and the baseline did not is read of the unit, no other.
    for name in baseline
        .made
        .iter()
        .filter(|name| baseline.places.place(name).is_none())
    {
        for part in Part::ALL {
            asked.forget_abroad((*name, part));
        }
    }
}

/// The parts `asked` reads of declarations `unit` does not hold, as the table of them records each: none of a declaration of a fixed unit.
fn foreign_parts<'t>(
    asked: &'t Asked,
    unit: &'t ErasedUnit,
    scope: Erasures<'t>,
) -> impl Iterator<Item = (curios_core::Global, Part)> + 't {
    asked
        .reads()
        .filter(move |(name, _)| unit.place(name).is_none() && scope.recorded(name))
}

/// Erase `item` into a form at each assignment of the sorts it leaves to its caller, each read at its assignment as the kernel judged it, with what erasing them asked between them.
fn erase_forms(
    reader: &mut Reader,
    scope: Erasures<'_>,
    unit: &ErasedUnit,
    (module, places): (&curios_core::Module, &curios_core::Places),
    item: &curios_core::Item,
) -> Result<(ErasedForms, Asked), Error> {
    let names = item.declared_names();
    let mut asked = Asking::of(&names);
    let mut forms = Vec::new();
    for sorts in Sorts::every(left_open(item)) {
        let (erased_read, declared_read) = (RefCell::new(Vec::new()), RefCell::new(Vec::new()));
        let externals = |name: &curios_core::Global, sorts: Sorts| {
            erased_read.borrow_mut().push(*name);
            unit.find(name, sorts).or_else(|| scope.find(name, sorts))
        };
        let declared = |name: &curios_core::Global| {
            declared_read.borrow_mut().push(*name);
            declared(module, places, name).or_else(|| scope.definition(name))
        };
        reader.restore_budget();
        reader.take_reads();
        let erased = erase_item(
            reader,
            Eraser::over(&externals, &declared, (&names, sorts)),
            &at_sorts(item, sorts),
        )?;
        asked.read(reader.take_reads(), declared_read.take());
        for name in erased_read.take() {
            asked.note(name, Part::Binding);
        }
        // What the item's top-level statements compute is computed here, and its trace says what that ran whether or not it could be finished.
        let erased = match evaluate(scope, unit, (&erased, sorts)) {
            Some(evaluation) => {
                for name in &evaluation.ran {
                    asked.note(*name, Part::Forms);
                }
                match evaluation.erased {
                    Some(evaluated) => {
                        for (name, _) in evaluated.environment().imported_rows() {
                            asked.note(*name, Part::Binding);
                        }

                        evaluated
                    }
                    None => erased,
                }
            }
            None => erased,
        };
        forms.push(erased);
    }
    let forms = ErasedForms::of(forms)
        .unwrap_or_else(|error| panic!("an erased form failed to archive: {error}"));

    Ok((forms, asked.asked))
}

/// How many sorts `item` leaves to its caller: its definition's or its group's.
fn left_open(item: &curios_core::Item) -> usize {
    universe_context(item).quality_count
}

/// The universe context `item` is judged under.
fn universe_context(item: &curios_core::Item) -> &curios_core::UniverseContext {
    match item {
        curios_core::Item::Let(definition) => &definition.universe_context,
        curios_core::Item::Rec(rec) => &rec.universe_context,
    }
}

/// `item` at the assignment `sorts` of the sorts it leaves open, as the kernel judges it there: every term it holds read at the assignment, and no sort left. An item that leaves none, as it stands.
fn at_sorts(item: &curios_core::Item, sorts: Sorts) -> Cow<'_, curios_core::Item> {
    if sorts.is_none() {
        return Cow::Borrowed(item);
    }
    let qualities = sorts.qualities();

    Cow::Owned(match item {
        curios_core::Item::Let(definition) => {
            let mut at = curios_core::Definition::clone(definition);
            at.type_ = curios_core::instantiate_qualities(&at.type_, &qualities);
            at.body = curios_core::instantiate_qualities(&at.body, &qualities);
            at.value_sort = at
                .value_sort
                .map(|sort| curios_core::instantiate_qualities(&sort, &qualities));
            at.universe_context.quality_count = 0;

            curios_core::Item::Let(Box::new(at))
        }
        curios_core::Item::Rec(rec) => {
            let mut at = curios_core::RecItem::clone(rec);
            at.group = rec
                .group
                .map_members(|body| curios_core::instantiate_qualities(body, &qualities));
            at.universe_context.quality_count = 0;

            curios_core::Item::Rec(at)
        }
    })
}

/// What erasing an item's forms asked, as it is asked: the kernel's reads of the declarations around it, read as the parts they are, the kinds and bodies erasure read off their items, and the erased forms it read. Nothing asked of a name the item declares is kept, as the item's own.
struct Asking<'a> {
    own: &'a [&'a curios_core::Global],
    asked: Asked,
}

impl<'a> Asking<'a> {
    fn of(own: &'a [&'a curios_core::Global]) -> Self {
        Self {
            own,
            asked: Asked::default(),
        }
    }

    fn note(&mut self, name: curios_core::Global, part: Part) {
        if !self.own.contains(&&name) {
            self.asked.note(name, part);
        }
    }

    /// What the kernel read while erasure read through it, and the definitions erasure read the kind and body of off their items.
    fn read(&mut self, reads: curios_core::Reads, declared: Vec<curios_core::Global>) {
        for name in reads.signatures {
            self.note(name, Part::Signature);
        }
        for name in reads.bodies {
            self.note(name, Part::Body);
        }
        for name in declared {
            self.note(name, Part::Kind);
            self.note(name, Part::Body);
        }
    }
}

/// Erase one item, at one assignment of its sorts, into a form of its own: on a budget restored for it and under the constraints its universe context states.
fn erase_item(
    reader: &mut Reader,
    mut lowering: Eraser<'_>,
    item: &curios_core::Item,
) -> Result<ErasedItem, Error> {
    reader.assume_universes(universe_context(item));
    match item {
        curios_core::Item::Let(definition) => lowering.erase_definition(reader, definition)?,
        curios_core::Item::Rec(rec) => lowering.erase_rec_item(reader, rec)?,
    }
    // The row of each registry entry the item declares is registered here, whether or not anything of the item constructs or matches it: every other item holds the row as an import of this one.
    for name in item.declared_names() {
        if reader.struct_decl(name).is_some() {
            lowering.struct_row(reader, (*name, Sorts::NONE))?;
        }
        if reader.induct_decl(name).is_some() {
            lowering.induct_family(reader, (*name, Sorts::NONE))?;
        }
    }

    lowering.into_erased(&item.declared_names())
}
