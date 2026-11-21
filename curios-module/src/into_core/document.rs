//! The builder of a unit's [`Documentation`] record — `curios-document`'s plain data: one record per module the unit exposes, each declaration's head printed as the author wrote it with every name it mentions resolved, and the prose attached to each — built where a command asks for it, by `document_unit`, and carried by no unit.
//!
//! **Built from the compilation that builds the unit: the tables its lowering stored.** Which modules and declarations a page shows is the export view resolution built to a fixed point, so a private declaration is absent rather than hidden and a re-export is listed as a link to the declaration it names — unless the declaration's own module has no page, the facade pattern, where the declaration is read off that module and filed under the re-exporting one, since the re-export is the only way it reaches a consumer. A written name's referent is the one the lowering resolved it to, read off the lowered unit at the place it was written ([`crate::Text::mentions`]), and a name the printer wrote, which has no place, is looked up with the visibility functions the lowering resolves a name with, over the same tables — nothing here resolves a name by a rule of its own. Nothing is read from the elaborated module either: every declaration states its signature, so the surface tree the lowering parsed is the whole of what a page prints, and a signature is printed by the printers `curios format` prints it with.
//!
//! **What an item states of a definition is read once** ([`stated`]): its head's printer, its prose and its members. A page shows of it what a consumer sees, and a place in a file is answered with whatever is written there ([`heading`]), from the declaration's own text and the same printers.
//!
//! **A library is documented for its consumers.** That is the one audience this record knows: a constructor appears only when the representation is public, a field likewise, and a test never. A program has no consumer, so nothing here documents one; which mount is documented, and with what description, is the resolver's to say.

#[cfg(test)]
mod tests;

use {
    super::{ModuleInfo, PreparedText, PublicInterface, Scoped, reached_from, visible_path},
    curios_abi::{NEVER, WireType},
    curios_common::{
        Class, Keyword, MAX_LINE_WIDTH, Mount, Qualifier, RootKind, Source, SourceName, Span,
    },
    curios_document::{
        Block, Declaration, Documentation, Entry, ExposureLine, Kind, Mark, Marked, Member,
        ModuleDocumentation, Signature,
    },
    curios_print::{Annotated, Printer, each_named, render_annotated},
    curios_surface::{
        Doc, Module, TopItem, UseGroup, carrier_former, print_case_head, print_case_result_head,
        print_concept_field_head, print_concept_head, print_exposure_head, print_foreign_head,
        print_induct_head, print_let_head, print_struct_field_head, print_struct_head,
        print_test_head, print_witness_head,
    },
    std::{
        collections::{HashMap, HashSet},
        path::PathBuf,
        rc::Rc,
        sync::Arc,
    },
};

/// Where a name is written: the name and the file of the source it lies in, and its byte range there — what a name a signature prints is found by among the unit's mentions.
pub type Written = (Option<SourceName>, Option<PathBuf>, usize, usize);

/// The indent a broken signature continues at — the formatter's.
const INDENT: usize = 4;

/// The mounts whose declarations reach a consumer only through the documented one, each with the word a page chips onto them.
///
/// **Decided here, from the prefixes this unit may name.** A root is adopted because a consumer cannot name it, this unit can, and it is not the one being documented — a fact about what this compilation mounts, not about the source the documented unit was read from. That distinction is what the prelude's split forces: the adopted root is a *different unit's* mount, so no source could claim it and no assertion against its own bases could hold.
///
/// **`mounts` is what this unit declared, not the whole compilation.** Every unit in a fold has `/sys` mounted, so the internal-root test alone makes every package adopt it — and a package that merely depends on `/std` would then show the intrinsics on its own pages as if it wrapped them. Adoption belongs to the facade in front of a closed root, and the facade is the one unit that declared a dependency on it.
fn adopted_mounts(mounts: &[Mount], documented: &Qualifier) -> Vec<(Qualifier, Option<String>)> {
    mounts
        .iter()
        .filter(|mount| mount.kind == RootKind::Internal && &mount.prefix != documented)
        .map(|mount| (mount.prefix, chip(&mount.prefix)))
        .collect()
}

/// The word a page shows on every declaration it adopts out of `prefix`, where there is one to show.
///
/// Spelled here because this crate holds the registry of the names the compiler knows in `/sys` (`registry.rs`), so the word for what that root holds is this crate's to know. A root with no entry is still adopted and merely says nothing about itself.
fn chip(prefix: &Qualifier) -> Option<String> {
    // Compared as a qualifier, not as text: `join` writes a canonical identity, which is absolute, so the text `"sys"` would match nothing it is handed.
    match *prefix == Qualifier::from(["sys"]) {
        true => Some("intrinsic".to_string()),
        false => None,
    }
}

/// What the checks of a unit established of its declarations: its checked module, where each of its items stands in it, and the certifier's record of it. The one thing a page shows that the surface tree does not state: that a declaration is a proof, and whether it is proven total.
pub struct Checked<'a> {
    definitions: &'a curios_core::Definitions,
    places: curios_core::Places,
    certification: &'a curios_core::Certification,
}

impl<'a> Checked<'a> {
    /// The checks of the unit whose checked module is `definitions` and whose certifier's record is `certification`.
    pub fn of(
        definitions: &'a curios_core::Definitions,
        certification: &'a curios_core::Certification,
    ) -> Self {
        Self {
            definitions,
            places: curios_core::Places::of(definitions),
            certification,
        }
    }

    /// Whether a checker marked `name` a proof.
    fn proof(&self, name: &curios_core::Global) -> bool {
        self.places
            .place(name)
            .and_then(|place| {
                self.definitions.items[place.item()]
                    .definitions()
                    .into_iter()
                    .find(|definition| definition.name == *name)
            })
            .is_some_and(|definition| definition.proof)
    }

    /// Whether the certifier proved `name` total, where it classifies it.
    fn total(&self, name: &curios_core::Global) -> Option<bool> {
        self.certification
            .totality(name)
            .map(curios_core::Totality::is_total)
    }
}

/// The interface of the unit mounted at `prefix`, read off the tables its lowering built: `modules` are the file-backed modules discovery parsed, `table` and `public` the direct interface and the export view over the whole scope, and `mentions` what each name written in the unit resolved to, where it was written.
///
/// Infallible, because every module it visits is one discovery loaded a moment ago: a prefix without a module in the map is a broken invariant of this stage, not a condition a caller can meet.
///
/// # Panics
///
/// Where `mounts` adopts a root no record of `records` documents.
#[expect(
    clippy::too_many_arguments,
    reason = "three over the line: each argument is a table the lowering built, a fact about its scope or what its checks established, and the one caller holds each under these names, so a struct would be a second spelling of the same ten"
)]
pub fn document(
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &Scoped<'_, ModuleInfo>,
    public: &Scoped<'_, PublicInterface>,
    mentions: &HashMap<Written, Qualifier>,
    prefix: &Qualifier,
    mounts: &[Mount],
    records: &[&Documentation],
    units: &[&PreparedText],
    checked: &Checked<'_>,
    description: Option<String>,
) -> Documentation {
    curios_profile::profile!("document");
    let adopted = adopted_mounts(mounts, prefix);
    for (root, _) in &adopted {
        assert!(
            records.iter().any(|record| record.prefix == *root),
            "'{}' adopts '{}', whose record it was not handed",
            prefix.join(),
            root.join()
        );
    }
    let internal = mounts
        .iter()
        .filter(|mount| mount.kind == RootKind::Internal)
        .map(|mount| mount.prefix)
        .collect::<Vec<_>>();
    let mut reader = Reader {
        modules,
        table,
        public,
        mentions,
        prefix,
        adopted: &adopted,
        internal: &internal,
        records,
        units,
        checked,
        public_names: HashMap::new(),
    };
    // Read before the walk, because a signature anywhere in the unit may name an adopted declaration, and taken through the reader because knowing which modules have pages is its own rule.
    reader.public_names = reader.exposed_names();

    let mut pages = Vec::new();
    reader.visit(*prefix, None, &mut pages);

    Documentation {
        prefix: *prefix,
        description,
        modules: pages,
    }
}

/// The tables a page is read from, and the walk over the unit's modules.
struct Reader<'a> {
    modules: &'a HashMap<Qualifier, Rc<Module>>,
    table: &'a Scoped<'a, ModuleInfo>,
    public: &'a Scoped<'a, PublicInterface>,
    /// What each name written in the unit resolved to, where it was written: the lowering's own answer, the one an editor's go-to-definition reads ([`crate::Text::mentions`]).
    mentions: &'a HashMap<Written, Qualifier>,
    prefix: &'a Qualifier,
    /// The unit's own roots that no consumer may name — its internal mounts. Their declarations reach a consumer only through a `pub use` in the documented mount, so a page shows them and no page names where they were written.
    adopted: &'a [(Qualifier, Option<String>)],
    /// Every root this unit mounts that no consumer may name, the documented one among them where it is one: what a path the compiler wrote into is shown below ([`Reader::shown`]). `/sys` documents itself under this rule, which is what spells the declarations `/std` adopts out of its record.
    internal: &'a [Qualifier],
    /// The records of the units this one adopts a root of, which is where an adopted declaration is read from.
    ///
    /// **An adopted root is a different unit, and a unit keeps no surface tree.** Its declarations are rendered from its own source, by its own record, which is made before this one. So a `pub use` into an adopted root is answered out of that record rather than re-derived from items nothing here can reach; re-derived, the adopted half would go missing silently, because every check of it asserts the absence of a path rather than the presence of a card.
    records: &'a [&'a Documentation],
    /// The units this one was lowered over and itself: what says what each declaration a name resolves to is, with nothing read ([`PreparedText::declared_as`]).
    units: &'a [&'a PreparedText],
    /// What the unit's checks established of its declarations ([`Checked`]).
    checked: &'a Checked<'a>,
    /// What a consumer calls each declaration that no page of its own shows: its declaration site, to the path of the page that exposes it. One entry per declaration a private child or an adopted root holds and a page re-exports.
    public_names: HashMap<Qualifier, Qualifier>,
}

impl Reader<'_> {
    /// What a consumer calls every declaration a page exposes out of a module with no page of its own — a private child, or a root this unit keeps to itself. Both are the same situation and take the same answer: the declaration is named for the page that shows it.
    ///
    /// **A second name is a tie this breaks and does not report.** One declaration exposed under two paths is legal, and both pages document it; what needs a single answer is which page a *mark* naming it links to. The first in sorted order wins — sorted because the maps underneath are `HashMap`s, and a tie broken by their iteration order would move between compilations of the same source.
    fn exposed_names(&self) -> HashMap<Qualifier, Qualifier> {
        let mut pages = self
            .public
            .iter()
            .filter(|(module, _)| self.has_page(module))
            .collect::<Vec<_>>();
        pages.sort_by_key(|(left, _)| *left);

        let mut names = HashMap::new();
        for (module, interface) in pages {
            let mut bindings = interface.bindings.iter().collect::<Vec<_>>();
            bindings.sort_by_key(|(left, _)| *left);

            for (label, entry) in bindings {
                let declaring = entry.target.without_last();
                if !self.has_page(&declaring) && self.has_declarations(&declaring) {
                    names
                        .entry(entry.target)
                        .or_insert_with(|| module.with(label));
                }
            }
        }
        names
    }

    /// The path a consumer writes for the declaration at `referent`.
    ///
    /// **A member is addressed under the declaration that holds it, never under a name a glob gave it.** A constructor and a concept method live in their owner's block on their owner's page, so `/std/Ordering/Ordering/lt` is where a mark finds `lt` even where `pub use Ordering/*` also spells it `/std/Ordering/lt` — the shorter spelling is a way to write the name, not a second place it lives.
    ///
    /// **A declaration whose own module has a page is already named the way a consumer writes it.** One whose module has none — a private child, or a root this unit keeps to itself — is the consumer's only through the re-export that shows it, so it takes that page's name. And one this bundle exposes nowhere has no consumer-facing name at all, which is a hole in the documented interface rather than a fact to render: it is refused here, naming both ends, rather than quietly rendered as a path the reader is not allowed to write. A name belonging to neither this unit's documented mount nor a root it adopts is nobody here's to rename, and renders as written.
    fn public_name(&self, referent: &Qualifier) -> Qualifier {
        let declaring = referent.without_last();
        let ours = referent.is_within(self.prefix) || self.adopted_root(referent).is_some();
        if !ours || self.has_page(&declaring) {
            return *referent;
        }

        if let Some(public) = self.public_names.get(referent) {
            return *public;
        }

        // **A member follows the declaration that holds it.** `Scan` moves to the page that exposes it, so `Scan/lead` is written under that page too — the constructor namespace is the declaration's, not a module anyone re-exports on its own.
        if let Some(owner) = self.public_names.get(&declaring) {
            return owner.with(referent.last());
        }

        // A member of a declaration that never moved is already addressed under it, and a name outside every module this unit shows is nobody here's to rename.
        if !self.has_declarations(&declaring) {
            return *referent;
        }

        panic!(
            "{} is named by the interface of {} but exposed by no page of it, so a consumer has no way to write it",
            referent.join(),
            self.prefix.join()
        )
    }

    /// Whether this bundle can render the declarations of `module`: its own, out of the items discovery parsed, or an adopted unit's, out of its record.
    fn has_declarations(&self, module: &Qualifier) -> bool {
        self.items_of(module).is_some() || self.adopted_page(module).is_some()
    }

    /// The page `module` has in the record of a unit this one adopts from.
    ///
    /// Gated on adoption rather than open to every record in scope: a visible unit documents itself, and its declarations belong to its own bundle under its own paths. Only a root a consumer cannot name has to be shown here instead.
    fn adopted_page(&self, module: &Qualifier) -> Option<&ModuleDocumentation> {
        self.adopted_root(module)?;
        self.records
            .iter()
            .find_map(|record| record.modules.iter().find(|page| page.path == *module))
    }

    /// The declaration `target` names in an adopted unit's record.
    fn adopted_declaration(&self, target: &Qualifier) -> Option<Declaration> {
        let label = target.last();
        let found = self
            .adopted_page(&target.without_last())?
            .declarations()
            .find(|declaration| declaration.name == label)?;

        Some(self.adopt(found))
    }

    /// One declaration of an adopted unit, with every name in it put through this unit's spellings.
    fn adopt(&self, declaration: &Declaration) -> Declaration {
        Declaration {
            signature: self.adopt_signature(&declaration.signature),
            members: declaration
                .members
                .iter()
                .map(|member| Member {
                    signature: self.adopt_signature(&member.signature),
                    ..member.clone()
                })
                .collect(),
            ..declaration.clone()
        }
    }

    /// `signature` as this unit shows it: its text as the root that wrote it rendered it, and every referent under the name a consumer writes here.
    ///
    /// **The text is kept, because the layout is.** Where a signature breaks was decided when it was rendered, over the text it renders to ([`Reader::shown`]), so splicing other text into a mark would keep the breaks of the old one.
    fn adopt_signature(&self, signature: &Signature) -> Signature {
        Signature {
            text: signature.text.clone(),
            marks: signature
                .marks
                .iter()
                .map(|mark| Mark {
                    start: mark.start,
                    end: mark.end,
                    marked: match &mark.marked {
                        Marked::Word(class) => Marked::Word(*class),
                        Marked::Name {
                            class, referent, ..
                        } => {
                            let referent = self.public_name(referent);
                            Marked::Name {
                                class: *class,
                                within: referent.is_within(self.prefix),
                                referent,
                            }
                        }
                    },
                })
                .collect(),
        }
    }

    /// What a name written `spelling` shows on a page — decided before the signature is laid out, so it breaks where its shown text does.
    ///
    /// **Everything an author wrote is shown as written, and a path the compiler wrote is spelled for a consumer.** `/sys` is generated, and its declarations spell what they name by absolute `/sys` paths, which resolve against the compilation root and which no consumer may write. Such a spelling — absolute, into a root no consumer can name — shows the path below that root, as the printer's table writes it (`curios_surface::intrinsic_form`): `Option`, `Bool/Holds`, `Bytes/len`. `/std` re-exports what `/sys` declares under those same paths, so it is what a consumer writes once the first segment is in scope, and the mark links the declaration wherever a page shows it.
    fn shown(&self, spelling: &str) -> String {
        let Some(path) = spelling.strip_prefix('/') else {
            return spelling.to_string();
        };
        let written = Qualifier::from(path.split('/'));

        match self.internal.iter().find(|root| written.is_within(root)) {
            Some(root) => written.segments()[root.segments().len()..].join("/"),
            None => spelling.to_string(),
        }
    }

    /// The adopted root `name` lies within, when it lies within one.
    fn adopted_root(&self, name: &Qualifier) -> Option<&(Qualifier, Option<String>)> {
        self.adopted.iter().find(|(root, _)| name.is_within(root))
    }

    /// The file-backed module `qualifier` names, then its public children after it.
    fn visit(
        &self,
        qualifier: Qualifier,
        prose: Option<Vec<String>>,
        out: &mut Vec<ModuleDocumentation>,
    ) {
        let module = self.modules.get(&qualifier).unwrap_or_else(|| {
            panic!("discovery loaded every module it declared, including {qualifier:?}")
        });
        self.visit_items(qualifier, prose, &module.items, out);
    }

    fn visit_items(
        &self,
        qualifier: Qualifier,
        prose: Option<Vec<String>>,
        items: &[TopItem],
        out: &mut Vec<ModuleDocumentation>,
    ) {
        let mut page = ModuleDocumentation {
            path: qualifier,
            prose,
            children: Vec::new(),
            entries: Vec::new(),
        };
        let mut children = Vec::new();
        // Every name this page has already placed, so the sweep below adds each exposed name once.
        let mut placed = HashSet::new();
        // What each witness this module declares takes part in, in the order it declares them.
        let mut parties = self.parties_of(&qualifier).into_iter();

        for item in items {
            match item {
                TopItem::Mod(declaration) => {
                    if declaration.vis_pub {
                        let child = qualifier.with(&declaration.label);
                        page.children.push(child);
                        children.push((child, lines(&declaration.doc), &declaration.module));
                    }
                }
                // A `pub use` puts what it exposes on this page, where it is written: a name a consumer reaches through this module is documented where the module offers it, whatever module declared it.
                TopItem::Use(use_) if use_.vis_pub => {
                    let labels = match &use_.group {
                        UseGroup::Named(group) => group
                            .iter()
                            .map(|entry| entry.label().to_string())
                            .collect(),
                        // A glob over a declaration exposes its members, which stand where the line is written; one over a module writes no order, and what it exposes is swept below.
                        UseGroup::Glob => self.globbed(&qualifier, &use_.name.join()),
                    };
                    self.expose_line(&qualifier, &labels, &mut placed, &mut page.entries);
                }
                // Every other item is a declaration of this module, or nothing.
                item => {
                    let mut declared = Vec::new();
                    self.declare(&qualifier, item, &mut declared);
                    for mut declaration in declared {
                        placed.insert(declaration.name.clone());
                        if declaration.kind == Kind::Witness {
                            declaration.parties = parties.next().unwrap_or_default();
                        }
                        page.entries.push(Entry::Declaration(declaration));
                    }
                }
            }
        }

        // What a glob over a module exposed, and anything else resolution reached that no item above placed. A glob writes no order, so these take the one a reader can predict.
        if let Some(interface) = self.public.get(&qualifier) {
            let mut rest = interface
                .bindings
                .keys()
                .filter(|label| !placed.contains(*label))
                .cloned()
                .collect::<Vec<_>>();
            rest.sort();
            self.expose_line(&qualifier, &rest, &mut placed, &mut page.entries);
        }

        out.push(page);

        // An inline module's items are in the tree; a file-backed one's are in the map, where discovery filed them.
        for (child, prose, inline) in children {
            match inline {
                Some(module) => self.visit_items(child, prose, &module.items, out),
                None => self.visit(child, prose, out),
            }
        }
    }

    /// What one line of `module` exposes as `labels`, appended to `out` in their order: a declaration each, and the members of one declaration together as the line that exposes them ([`ExposureLine`]), where the first of them stands.
    fn expose_line(
        &self,
        module: &Qualifier,
        labels: &[String],
        placed: &mut HashSet<String>,
        out: &mut Vec<Entry>,
    ) {
        // Where the line's exposure of each owner's members stands among the entries.
        let mut exposures = HashMap::<Qualifier, usize>::new();
        for label in labels {
            self.expose(module, label, placed, out, &mut exposures);
        }

        // A line's head is printed once every member it exposes is known, since the names decide where it breaks.
        for at in exposures.into_values() {
            let Entry::Exposure(exposure) = &mut out[at] else {
                unreachable!("an exposure is filed where its line's first member stands");
            };
            let owner = self.spelled_from(module, &exposure.owner);
            exposure.signature =
                self.signature(module, None, print_exposure_head(&owner, &exposure.names));
        }
    }

    /// The name `module` exposes as `label`, appended to `out`: a declaration of this page, or one more member of the exposure `exposures` holds for its owner.
    ///
    /// **A page shows what it offers, not what it wrote.** Where the declaration lives decides only what its entry says of it: every mark names it by one path ([`Declaration::named`]), its own where this page is where a consumer finds it, and a module with no page — a private child, or a root a consumer cannot name — is not named at all, because for a consumer this page is where the declaration lives. A name declared on this page is skipped, since its own item already placed it, and one declared outside this unit is skipped because nothing here can read it.
    ///
    /// **A member is no declaration of its own.** A constructor or a concept's method a module exposes beside its owner is shown in its owner's block, on its owner's page, so the page that exposes it says the line that does, with the member linked there.
    fn expose(
        &self,
        module: &Qualifier,
        label: &str,
        placed: &mut HashSet<String>,
        out: &mut Vec<Entry>,
        exposures: &mut HashMap<Qualifier, usize>,
    ) {
        if placed.contains(label) {
            return;
        }
        let Some(entry) = self
            .public
            .get(module)
            .and_then(|interface| interface.bindings.get(label))
        else {
            return;
        };

        let declaring = entry.target.without_last();
        if declaring == *module {
            return;
        }

        if self.is_member(&entry.target) {
            let owner = self.public_name(&declaring);
            match exposures.get(&owner) {
                Some(&at) => {
                    let Entry::Exposure(exposure) = &mut out[at] else {
                        unreachable!("an exposure is filed where its line's first member stands");
                    };
                    exposure.names.push(label.to_string());
                }
                None => {
                    exposures.insert(owner, out.len());
                    out.push(Entry::Exposure(ExposureLine {
                        owner,
                        names: vec![label.to_string()],
                        signature: Signature {
                            text: String::new(),
                            marks: Vec::new(),
                        },
                    }));
                }
            }
            placed.insert(label.to_string());
            return;
        }

        let (mut declaration, chip) = match self.declaration_at(&entry.target) {
            // A declaration of another module: adopted and chipped with what its root holds where it has no page to be named by.
            Some(found) => {
                let chip = match self.has_page(&declaring) {
                    true => None,
                    false => self
                        .adopted_root(&entry.target)
                        .and_then(|(_, chip)| chip.clone()),
                };
                (found, chip)
            }
            // A declaration of a unit this one adopts, read from that unit's record: it has no page here and no path a consumer may write, so it is chipped with what its root holds exactly as a page-less module of this unit's own is.
            None => match self.adopted_declaration(&entry.target) {
                Some(found) => (
                    found,
                    self.adopted_root(&entry.target)
                        .and_then(|(_, chip)| chip.clone()),
                ),
                None => return,
            },
        };

        declaration.name = label.to_string();
        declaration.home = *module;
        declaration.named = self.public_name(&entry.target);
        declaration.chip = chip;

        placed.insert(label.to_string());
        out.push(Entry::Declaration(declaration));
    }

    /// Whether `target` is a member a `use` selects out of its owner's namespace: an inductive's constructor or a concept's method, by what the units in scope declare it as.
    fn is_member(&self, target: &Qualifier) -> bool {
        let name = curios_core::Global::Authored(*target);

        self.units
            .iter()
            .find_map(|unit| unit.declared_as(&name))
            .is_some_and(|declared| {
                matches!(
                    declared,
                    curios_core::Declared::Constructor | curios_core::Declared::Method
                )
            })
    }

    /// The members a glob written `spelling` in `module` exposes of the declaration it names, in the order that declaration lists them. None where the path names a module, or a declaration with no member a consumer sees.
    fn globbed(&self, module: &Qualifier, spelling: &str) -> Vec<String> {
        let (Some(owner), Some(interface)) =
            (self.resolve(spelling, module), self.public.get(module))
        else {
            return Vec::new();
        };

        self.members_of(&owner)
            .into_iter()
            .filter(|label| {
                interface
                    .bindings
                    .get(label)
                    .is_some_and(|entry| entry.target == owner.with(label))
            })
            .collect()
    }

    /// The names of the members the declaration at `owner` lists, in its order: read off the item that declares it, or off the record of a unit this one adopts.
    fn members_of(&self, owner: &Qualifier) -> Vec<String> {
        let declaring = owner.without_last();
        let named = |names: Vec<String>| {
            names
                .into_iter()
                .filter(|name| !name.is_empty())
                .collect::<Vec<_>>()
        };

        if let Some(items) = self.items_of(&declaring) {
            return items
                .iter()
                .find(|item| declares(item, owner.last()))
                .into_iter()
                .flat_map(stated)
                .find(|stated| stated.name == owner.last())
                .map(|stated| {
                    named(
                        stated
                            .members
                            .into_iter()
                            .map(|member| member.name)
                            .collect(),
                    )
                })
                .unwrap_or_default();
        }

        self.adopted_page(&declaring)
            .and_then(|page| {
                page.declarations()
                    .find(|declaration| declaration.name == owner.last())
            })
            .map(|declaration| {
                named(
                    declaration
                        .members
                        .iter()
                        .map(|member| member.name.clone())
                        .collect(),
                )
            })
            .unwrap_or_default()
    }

    /// How a line of `module` names the declaration a consumer names `owner`: by the name the module exposes it under, and by its whole path where it exposes it under none.
    fn spelled_from(&self, module: &Qualifier, owner: &Qualifier) -> String {
        let exposed = self.public.get(module).and_then(|interface| {
            let mut labels = interface
                .bindings
                .iter()
                .filter(|(_, entry)| self.public_name(&entry.target) == *owner)
                .map(|(label, _)| label.clone())
                .collect::<Vec<_>>();
            labels.sort();
            labels.into_iter().next()
        });

        exposed.unwrap_or_else(|| owner.join())
    }

    /// What each witness `module` declares takes part in, in the order the module declares them: the concept it witnesses and the declarations its key names ([`curios_core::WitnessKey`]), a carrier by the declaration that forms it, each by the path a consumer names it.
    fn parties_of(&self, module: &Qualifier) -> Vec<Vec<Qualifier>> {
        let Some(own) = self.units.last() else {
            return Vec::new();
        };
        let mut witnesses = own
            .places()
            .witnesses()
            .filter_map(|witness| match witness {
                curios_core::Global::Witness(id) if id.module() == module => Some((
                    own.anchors().written(witness).map(|(_, label)| label.start),
                    id,
                )),
                curios_core::Global::Witness(_) | curios_core::Global::Authored(_) => None,
            })
            .collect::<Vec<_>>();
        witnesses.sort_by_key(|(written, _)| *written);

        witnesses
            .into_iter()
            .map(|(_, id)| {
                let Some((concept, key)) = id.spelled() else {
                    return Vec::new();
                };
                let heads = key.heads().iter().filter_map(|head| match head {
                    curios_core::HeadKey::Nominal(nominal) => Some(*nominal),
                    carrier => carrier_former(carrier),
                });

                [concept]
                    .into_iter()
                    .chain(heads)
                    .filter_map(|party| match party {
                        curios_core::Global::Authored(path) => Some(self.public_name(&path)),
                        curios_core::Global::Witness(_) => None,
                    })
                    .collect()
            })
            .collect()
    }

    /// The declaration written at `target`, read from the module that declares it — `None` when that module is not one this unit holds.
    ///
    /// **Only the item that declares it is read.** Reading the whole module and picking afterwards would print, resolve and check a signature for every other declaration the module makes, including ones this bundle exposes nowhere — so a name only those may write would be judged against an interface that does not contain them, and the work to reach one declaration would scale with the module holding it.
    fn declaration_at(&self, target: &Qualifier) -> Option<Declaration> {
        let declaring = target.without_last();
        let items = self.items_of(&declaring)?;
        let label = target.last();

        let item = items.iter().find(|item| declares(item, label))?;
        let mut found = Vec::new();
        self.declare(&declaring, item, &mut found);
        found
            .into_iter()
            .find(|declaration| declaration.name == label)
    }

    /// The declarations `item` makes in `home` that a consumer can see, appended to `out`: of what it states ([`stated`]), each definition written `pub` and every witness, with the members its representation exposes — a concept's methods either way, being reached by name whether or not its representation is sealed.
    fn declare(&self, home: &Qualifier, item: &TopItem, out: &mut Vec<Declaration>) {
        for stated in stated(item).into_iter().filter(|stated| stated.exposed) {
            // The file the item is written in, which its members share: what a written name is looked up in.
            let source = stated.label.map(|label| &label.source);
            let members = match stated.shown {
                true => stated
                    .members
                    .into_iter()
                    .map(|member| Member {
                        name: member.name,
                        signature: self.signature(home, source, member.head),
                        prose: member.prose,
                    })
                    .collect(),
                false => Vec::new(),
            };

            // What the checks established of a definition, by the name it is declared under; a type and a witness are neither proof nor total.
            let defined = (stated.kind == Kind::Definition)
                .then(|| curios_core::Global::Authored(home.with(&stated.name)));

            out.push(Declaration {
                home: *home,
                kind: stated.kind,
                signature: self.signature(home, source, stated.head),
                prose: stated.prose,
                members,
                opaque: stated.opaque,
                derived: stated.derived,
                proof: defined
                    .as_ref()
                    .is_some_and(|name| self.checked.proof(name)),
                total: defined.as_ref().and_then(|name| self.checked.total(name)),
                parties: Vec::new(),
                // A witness has no name, and no mark names one.
                named: match stated.name.is_empty() {
                    true => *home,
                    false => home.with(&stated.name),
                },
                chip: None,
                name: stated.name,
            });
        }
    }

    /// Whether `module` has a page of its own: every hop below the prefix is a `pub mod`. A private module, and every module beneath one, has none.
    fn has_page(&self, module: &Qualifier) -> bool {
        module.is_within(self.prefix)
            && reached_from(self.prefix, module, |module| self.table.get(module))
    }

    /// The items of `module`: a file-backed module's from the map discovery filed it in, an inline module's from its parent's tree. `None` for a path that names no module — a constructor namespace, say.
    fn items_of(&self, module: &Qualifier) -> Option<&[TopItem]> {
        if let Some(file) = self.modules.get(module) {
            return Some(&file.items);
        }
        if module.segments().len() <= self.prefix.segments().len() {
            return None;
        }
        self.items_of(&module.without_last())?
            .iter()
            .find_map(|item| match item {
                TopItem::Mod(declaration) if declaration.label.as_str() == module.last() => {
                    declaration
                        .module
                        .as_ref()
                        .map(|inline| inline.items.as_slice())
                }
                _ => None,
            })
    }

    /// `head`, written in `module` in the file `source`, rendered with every name in it that resolves marked.
    ///
    /// **A written name names what the lowering resolved it to** ([`Reader::mentions`]), read at the place it was written, so a page links exactly what the compiler took it for: a binder, a name that did not resolve and one no lowered term holds have no answer there, and stay plain text. A name the printer wrote, which has no place — one `/sys` generates, a family a constructor's own head names — is resolved by [`Reader::resolve`].
    ///
    /// **Each name is resolved and spelled before the layout is decided** ([`each_named`]), so the signature breaks where the text it shows does. A render emits the names in the order the walk met them, which is what pairs each with its referent.
    fn signature(
        &self,
        module: &Qualifier,
        source: Option<&Arc<Source>>,
        mut head: Printer,
    ) -> Signature {
        curios_profile::profile!("document::signature");
        let mut referents = Vec::new();
        each_named(&mut head, |text, written| {
            let referent = match written {
                Some(written) => source.and_then(|source| {
                    let place = (
                        source.name().cloned(),
                        source.file().map(ToOwned::to_owned),
                        written.start,
                        written.end,
                    );
                    self.mentions.get(&place).copied()
                }),
                None => self.resolve(text, module),
            };
            if referent.is_some() {
                *text = self.shown(text);
            }
            referents.push(
                referent.map(|referent| (self.public_name(&referent), self.class_of(&referent))),
            );
        });
        let (text, annotations) = render_annotated(head, INDENT, MAX_LINE_WIDTH);

        let mut referents = referents.into_iter();
        let marks = annotations
            .into_iter()
            .filter_map(|annotation| {
                let marked = match annotation.annotated {
                    Annotated::Word => Marked::Word(word_class(&annotation.text)),
                    Annotated::Name { .. } => {
                        let (referent, class) = referents
                            .next()
                            .expect("a render emits every name the walk met, once")?;
                        Marked::Name {
                            class,
                            within: referent.is_within(self.prefix),
                            referent,
                        }
                    }
                };

                Some(Mark {
                    start: annotation.start,
                    end: annotation.end,
                    marked,
                })
            })
            .collect();
        assert!(
            referents.next().is_none(),
            "a render emits every name the walk met, once"
        );

        Signature { text, marks }
    }

    /// What the declaration at `referent` is painted as, read off what the units in scope outline it as: a name's class is what its declaration states, never where it lives.
    fn class_of(&self, referent: &Qualifier) -> Option<Class> {
        let name = curios_core::Global::Authored(*referent);
        self.units
            .iter()
            .find_map(|unit| unit.declared_as(&name))?
            .class()
    }

    /// The declaration a name the printer wrote — one with no place, so no mention — names from `module`, by the lowering's visibility functions: an absolute path walks the public tree from the root, and a relative one is the module's own declaration or a path through its own children. No `use` brought it into scope, since nobody wrote it. `None` is a name that does not resolve or is out of view, which a page leaves as plain text.
    fn resolve(&self, spelling: &str, module: &Qualifier) -> Option<Qualifier> {
        let start = match spelling.starts_with('/') {
            true => Qualifier::empty(),
            false => *module,
        };
        let segments = spelling
            .trim_start_matches('/')
            .split('/')
            .collect::<Vec<_>>();

        visible_path(self.public, self.table, module, start, &segments)
    }
}

/// What an item states of one definition it declares: the head it is printed by, the prose above it, and the members stated under it. The one reading of an item's declarations, which a page takes what a consumer sees of ([`Reader::declare`]) and a place is answered from whole ([`heading`]).
struct Stated<'a> {
    /// Its name as a page lists it: none for a witness, and none for a member written without one.
    name: String,
    /// Where its name is written, a witness's being its head.
    label: Option<&'a Span>,
    kind: Kind,
    /// Whether a consumer sees it: one written `pub`, and every witness.
    exposed: bool,
    /// Whether a consumer sees its members: its representation is written `pub`, or it is a concept, whose methods are reached by name either way.
    shown: bool,
    /// Whether its representation is kept from a consumer.
    opaque: bool,
    /// Whether it is a witness the compiler writes.
    derived: bool,
    head: Printer,
    /// The head it has standing apart from its owner, where that says more than the one its owner's block lists it by: a constructor's, with the family it builds.
    apart: Option<Printer>,
    prose: Option<Vec<String>>,
    members: Vec<Stated<'a>>,
}

impl<'a> Stated<'a> {
    /// A definition with no member and no representation, stated by `head`.
    fn plain(
        name: String,
        label: Option<&'a Span>,
        kind: Kind,
        exposed: bool,
        head: Printer,
        doc: &Option<Doc>,
    ) -> Self {
        Self {
            name,
            label,
            kind,
            exposed,
            shown: false,
            opaque: false,
            derived: false,
            head,
            apart: None,
            prose: lines(doc),
            members: Vec::new(),
        }
    }
}

/// What `item` states of each definition it declares, in the order they are written, a type's constructors, a structure's fields and a concept's fields each under their owner. A module and an import declare none, and a broken item is read by nobody.
fn stated(item: &TopItem) -> Vec<Stated<'_>> {
    match item {
        TopItem::Mod(_) | TopItem::Use(_) | TopItem::Broken(_) => Vec::new(),
        TopItem::Let(members) => members
            .iter()
            .map(|member| {
                Stated::plain(
                    member.label.to_string(),
                    member.label.span(),
                    Kind::Definition,
                    member.vis_pub,
                    print_let_head(member),
                    &member.doc,
                )
            })
            .collect(),
        TopItem::Induct(members) => members
            .iter()
            .map(|member| {
                let cases = member.cases.iter().map(|case| Stated {
                    apart: Some(print_case_result_head(member, case)),
                    ..Stated::plain(
                        case.label.to_string(),
                        case.label.span(),
                        Kind::Definition,
                        false,
                        print_case_head(case),
                        &case.doc,
                    )
                });

                Stated {
                    // Constructors are the representation: shown exactly when it is public.
                    shown: member.rep_pub,
                    opaque: !member.rep_pub,
                    members: cases.collect(),
                    ..Stated::plain(
                        member.label.to_string(),
                        member.label.span(),
                        Kind::Inductive,
                        member.vis_pub,
                        print_induct_head(member),
                        &member.doc,
                    )
                }
            })
            .collect(),
        TopItem::Struct(members) => members
            .iter()
            .map(|member| {
                let fields = member
                    .fields
                    .iter()
                    .map(|field| {
                        Stated::plain(
                            field.param.label.as_deref().unwrap_or_default().to_string(),
                            None,
                            Kind::Definition,
                            false,
                            print_struct_field_head(field),
                            &field.doc,
                        )
                    })
                    .collect();

                Stated {
                    shown: member.rep_pub,
                    opaque: !member.rep_pub,
                    members: fields,
                    ..Stated::plain(
                        member.label.to_string(),
                        member.label.span(),
                        Kind::Structure,
                        member.vis_pub,
                        print_struct_head(member),
                        &member.doc,
                    )
                }
            })
            .collect(),
        TopItem::Concept(members) => members
            .iter()
            .map(|member| {
                let fields = member.fields.iter().map(|field| {
                    Stated::plain(
                        // A superclass edge has no name, and a page lists it by its signature alone.
                        field
                            .label
                            .as_ref()
                            .map(|label| label.to_string())
                            .unwrap_or_default(),
                        field.label.as_ref().and_then(|label| label.span()),
                        Kind::Definition,
                        false,
                        print_concept_field_head(field),
                        &field.doc,
                    )
                });

                Stated {
                    // A concept's methods are its interface whether or not its representation is sealed: they are reached by name either way.
                    shown: true,
                    opaque: !member.rep_pub,
                    members: fields.collect(),
                    ..Stated::plain(
                        member.label.to_string(),
                        member.label.span(),
                        Kind::Concept,
                        member.vis_pub,
                        print_concept_head(member),
                        &member.doc,
                    )
                }
            })
            .collect(),
        TopItem::Witness(members) => members
            .iter()
            .map(|member| Stated {
                derived: member.body.is_none(),
                ..Stated::plain(
                    String::new(),
                    member.head.as_ref(),
                    Kind::Witness,
                    true,
                    print_witness_head(member),
                    &member.doc,
                )
            })
            .collect(),
        TopItem::Foreign(declaration) => vec![Stated::plain(
            declaration.label.to_string(),
            declaration.label.span(),
            Kind::Foreign,
            declaration.vis_pub,
            print_foreign_head(declaration),
            &declaration.doc,
        )],
        // A test is no part of an interface, and takes no prose.
        TopItem::Test(test) => vec![Stated::plain(
            test.label.to_string(),
            test.label.span(),
            Kind::Definition,
            false,
            print_test_head(test),
            &None,
        )],
    }
}

/// What a declaration states of one definition: its head as the author wrote it, the prose above it, a line an entry, what kind of declaration it is, whether its representation is kept, and the members its representation shows, each as its owner's block lists it — none for a member, which has none, and none where the representation is kept.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Heading {
    pub head: String,
    pub prose: Vec<String>,
    pub kind: Kind,
    pub opaque: bool,
    pub members: Vec<String>,
}

impl Heading {
    /// The declaration as its source writes it, with what a body elides left out ([`written`]).
    pub fn written(&self) -> String {
        written(self.kind, self.opaque, &self.head, &self.members)
    }
}

/// A declaration as its source writes it, with what a body elides left out: `head`, then `members`, the ones its representation shows, in the block a declaration of `kind` writes them in ([`Block`]) — a member that runs over lines keeping the block's indent on each. A declaration that writes no block a consumer sees is its head alone.
pub fn written(kind: Kind, opaque: bool, head: &str, members: &[impl AsRef<str>]) -> String {
    let Some(block) = Block::of(kind, opaque) else {
        return head.to_string();
    };
    let indent = match block.indented {
        true => " ".repeat(INDENT),
        false => String::new(),
    };

    let mut lines = vec![format!("{head}{}", block.opener)];
    lines.extend(members.iter().map(|member| {
        let member = member.as_ref().replace('\n', &format!("\n{indent}"));

        format!("{indent}{}{member}{}", block.lead, block.trail)
    }));
    lines.push(block.closer.to_string());

    lines.join("\n")
}

/// What `declaration`, the text of one source declaration, states of the definition whose name is written `label` bytes into it: the head a page would print it by, from the printers a page prints by, with the prose above it, whether or not a consumer sees the definition. A member is stated as it stands apart from its owner, a constructor with the family it builds. `None` where the text does not parse as an item, and where no definition it declares is named there.
pub fn heading(declaration: &str, label: usize) -> Option<Heading> {
    let module = Module::parse(&Source::inline(declaration)).ok()?;
    let named = |stated: &Stated| stated.label.is_some_and(|span| span.start == label);

    module
        .items
        .iter()
        .flat_map(stated)
        .find_map(|stated| match named(&stated) {
            true => Some(stated),
            false => stated.members.into_iter().find(|member| named(member)),
        })
        .map(|stated| Heading {
            kind: stated.kind,
            opaque: stated.opaque,
            members: match stated.shown {
                true => stated
                    .members
                    .into_iter()
                    .map(|member| render_annotated(member.head, INDENT, MAX_LINE_WIDTH).0)
                    .collect(),
                false => Vec::new(),
            },
            head: render_annotated(stated.apart.unwrap_or(stated.head), INDENT, MAX_LINE_WIDTH).0,
            prose: stated.prose.unwrap_or_default(),
        })
}

/// Whether `item` declares `label` as something a consumer can see — asked of the tree alone, so finding one declaration never costs the printing and resolving of its neighbours.
fn declares(item: &TopItem, label: &str) -> bool {
    let named = |vis_pub: bool, declared: &str| vis_pub && declared == label;
    match item {
        TopItem::Let(members) => members
            .iter()
            .any(|member| named(member.vis_pub, member.label.as_str())),
        TopItem::Induct(members) => members
            .iter()
            .any(|member| named(member.vis_pub, member.label.as_str())),
        TopItem::Struct(members) => members
            .iter()
            .any(|member| named(member.vis_pub, member.label.as_str())),
        TopItem::Concept(members) => members
            .iter()
            .any(|member| named(member.vis_pub, member.label.as_str())),
        TopItem::Foreign(declaration) => named(declaration.vis_pub, declaration.label.as_str()),
        // A witness is anonymous, and neither a module, an import, a test nor a broken item declares a name a `use` can select.
        TopItem::Witness(_)
        | TopItem::Mod(_)
        | TopItem::Use(_)
        | TopItem::Test(_)
        | TopItem::Broken(_) => false,
    }
}

/// A documentation comment's lines, or `None` when there is none.
fn lines(doc: &Option<Doc>) -> Option<Vec<String>> {
    doc.as_ref().map(|doc| doc.lines.clone())
}

/// What a word the printer wrote is painted as: one of the language's ([`Keyword`]), or a wire word of a `foreign` signature, which `curios-abi` states and which names a type. The printer writes no other word.
fn word_class(word: &str) -> Class {
    let wire = || word == NEVER || WireType::words().any(|wire| wire == word);
    match Keyword::read(word) {
        Some(keyword) => keyword.class(),
        None if wire() => Class::Type,
        None => panic!(
            "the printer writes `{word}` as a word, which is neither the language's nor the wire's"
        ),
    }
}
