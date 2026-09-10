//! The builder of a unit's [`Documentation`] record — `curios-document`'s plain data: one record per module the unit exposes, each declaration's head printed as the author wrote it with every name it mentions resolved, and the prose attached to each — built by the lowering as the last thing it does and carried on the unit it lowered.
//!
//! **Built by the compilation that builds the unit, from the tables it just built.** Which modules and declarations a page shows is the export view resolution built to a fixed point, so a private declaration is absent rather than hidden and a re-export is listed as a link to the declaration it names — unless the declaration's own module has no page, the facade pattern, where the declaration is read off that module and filed under the re-exporting one, since the re-export is the only way it reaches a consumer. A referent is looked up with the visibility functions the lowering resolves a name with, over the same tables, seeded by the import scopes the lowering recorded per definition — nothing here resolves a name by a rule of its own. Nothing is read from the elaborated module either: every declaration states its signature, so the surface tree the lowering parsed is the whole of what a page prints, and a signature is printed by the printers `curios format` prints it with. Riding on the unit, the record travels wherever the unit does — the prelude image, a verdict slot, the browser bundle — so a unit is documented from its stored form without its sources.
//!
//! **A library is documented for its consumers.** That is the one audience this record knows: a constructor appears only when the representation is public, a field likewise, and a test never. A program has no consumer, so nothing here documents one; which mount is documented, and with what description, is the resolver's to say.

use {
    super::*,
    crate::{
        print_case_head, print_case_result_head, print_concept_field_head, print_concept_head,
        print_foreign_head, print_induct_head, print_let_head, print_struct_field_head,
        print_struct_head, print_witness_head,
    },
    curios_core::{Global, Imports},
    curios_document::{
        Declaration, Documentation, Kind, Mark, Member, ModuleDocumentation, Signature,
    },
    curios_print::{Printer, render_annotated},
    std::collections::{HashMap, HashSet},
};

/// The width a signature is rendered within — the formatter's, so a page and a file agree on where a long telescope breaks.
const WIDTH: usize = 100;

/// The indent a broken signature continues at — the formatter's.
const INDENT: usize = 4;

/// The interface of the unit mounted at `prefix`, read off the tables the lowering just built: `modules` are the file-backed modules discovery parsed, `table` and `public` the direct interface and the export view over the whole scope, and `imports` what each definition's `use` lines brought into scope.
///
/// The mounts whose declarations reach a consumer only through the documented one, each with the word a page chips onto them.
///
/// **Decided here, from the prefixes this unit may name.** A root is adopted because a consumer cannot name it, this unit can, and it is not the one being documented — a fact about what this compilation mounts, not about the source the documented unit was read from. That distinction is what the prelude's split forces: the adopted root is a *different unit's* mount, so no source could claim it and no assertion against its own bases could hold.
///
/// **`mounts` is what this unit declared, not the whole compilation.** Every unit in a fold has `/sys` mounted, so the internal-root test alone makes every package adopt it — and a package that merely depends on `/std` would then show the intrinsics on its own pages as if it wrapped them. Adoption belongs to the facade in front of a closed root, and the facade is the one unit that declared a dependency on it.
fn adopted_mounts(mounts: &[Mount], documented: &Qualifier) -> Vec<(Qualifier, Option<String>)> {
    mounts
        .iter()
        .filter(|mount| mount.kind == RootKind::Internal && &mount.prefix != documented)
        .map(|mount| (mount.prefix.clone(), chip(&mount.prefix)))
        .collect()
}

/// The word a page shows on every declaration it adopts out of `prefix`, where there is one to show.
///
/// Spelled here because `/sys` is this crate's own root — `sys_module` builds it — so the word for what it holds is this crate's to know. A root with no entry is still adopted and merely says nothing about itself.
fn chip(prefix: &Qualifier) -> Option<String> {
    // Compared as a qualifier, not as text: `join` writes a canonical identity, which is absolute, so `"sys"` matched nothing it was ever handed.
    match *prefix == Qualifier::from(["sys"]) {
        true => Some("intrinsic".to_string()),
        false => None,
    }
}

/// Infallible, because every module it visits is one discovery loaded a moment ago: a prefix without a module in the map is a broken invariant of this stage, not a condition a caller can meet.
// One over the lint's line, and every argument is a table the lowering just built or a fact about the scope it built them in — there is one caller, which has each of them to hand under these names, so a struct here would be a second spelling of the same eight things.
#[allow(clippy::too_many_arguments)]
pub(super) fn document(
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &Scoped<'_, ModuleInfo>,
    public: &Scoped<'_, PublicInterface>,
    imports: &Imports,
    prefix: &Qualifier,
    mounts: &[Mount],
    records: &[&Documentation],
    description: Option<String>,
) -> Documentation {
    let adopted = adopted_mounts(mounts, prefix);
    let mut reader = Reader {
        modules,
        table,
        public,
        imports,
        prefix,
        adopted: &adopted,
        records,
        public_names: HashMap::new(),
    };
    // Read before the walk, because a signature anywhere in the unit may name an adopted declaration, and taken through the reader because knowing which modules have pages is its own rule.
    reader.public_names = reader.exposed_names();

    let mut pages = Vec::new();
    reader.visit(prefix.clone(), None, &mut pages);

    Documentation {
        prefix: prefix.clone(),
        description,
        modules: pages,
    }
}

/// The tables a page is read from, and the walk over the unit's modules.
struct Reader<'a> {
    modules: &'a HashMap<Qualifier, Rc<Module>>,
    table: &'a Scoped<'a, ModuleInfo>,
    public: &'a Scoped<'a, PublicInterface>,
    imports: &'a Imports,
    prefix: &'a Qualifier,
    /// The unit's own roots that no consumer may name — its internal mounts. Their declarations reach a consumer only through a `pub use` in the documented mount, so a page shows them and no page names where they were written.
    adopted: &'a [(Qualifier, Option<String>)],
    /// The records the units already compiled carry, which is where an adopted declaration is read from.
    ///
    /// **An adopted root is a different unit, and a unit keeps no surface tree.** Its declarations were rendered while it was being lowered and its items were in hand; by the time this unit is lowered they exist only in the record it carried away. So a `pub use` into an adopted root is answered out of that record rather than re-derived from items nothing here can reach — which is what stopped working the day the prelude became two units, silently, because every check of the adopted half asserts the absence of a path rather than the presence of a card.
    records: &'a [&'a Documentation],
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
                        .entry(entry.target.clone())
                        .or_insert_with(|| module.with(label));
                }
            }
        }
        names
    }

    /// The path a consumer writes for the declaration at `referent`.
    ///
    /// **A member is addressed under the declaration that holds it, never under a name a glob gave it.** A constructor and a concept method live in their owner's block on their owner's page, so `/std/Result/Result/success` is where a mark finds `success` even where `pub use Result/*` also spells it `/std/Result/success` — the shorter spelling is a way to write the name, not a second place it lives.
    ///
    /// **A declaration whose own module has a page is already named the way a consumer writes it.** One whose module has none — a private child, or a root this unit keeps to itself — is the consumer's only through the re-export that shows it, so it takes that page's name. And one this bundle exposes nowhere has no consumer-facing name at all, which is a hole in the documented interface rather than a fact to render: it is refused here, naming both ends, rather than quietly rendered as a path the reader is not allowed to write. A name belonging to neither this unit's documented mount nor a root it adopts is nobody here's to rename, and renders as written.
    fn public_name(&self, referent: &Qualifier) -> Qualifier {
        let declaring = referent.without_last();
        let ours = referent.is_within(self.prefix) || self.adopted_root(referent).is_some();
        if !ours || self.has_page(&declaring) {
            return referent.clone();
        }

        if let Some(public) = self.public_names.get(referent) {
            return public.clone();
        }

        // **A member follows the declaration that holds it.** `Scan` moves to the page that exposes it, so `Scan/lead` is written under that page too — the constructor namespace is the declaration's, not a module anyone re-exports on its own.
        if let Some(owner) = self.public_names.get(&declaring) {
            return owner.with(referent.last());
        }

        // A member of a declaration that never moved is already addressed under it, and a name outside every module this unit shows is nobody here's to rename.
        if !self.has_declarations(&declaring) {
            return referent.clone();
        }

        panic!(
            "{} is named by the interface of {} but exposed by no page of it, so a consumer has no way to write it",
            referent.join(),
            self.prefix.join()
        )
    }

    /// Whether this bundle can render the declarations of `module`: its own, out of the items discovery parsed, or an adopted unit's, out of the record it carried.
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
            .declarations
            .iter()
            .find(|declaration| declaration.name == label)?;

        Some(self.adopt(found))
    }

    /// The member `target` names beside its owner in an adopted unit's record — a constructor or a concept method, which a page shows in its owner's block rather than as a declaration of its own.
    fn adopted_member(&self, target: &Qualifier) -> Option<Declaration> {
        let owner = target.without_last();
        let label = target.last();
        let holder = self
            .adopted_page(&owner.without_last())?
            .declarations
            .iter()
            .find(|declaration| declaration.name == owner.last())?;
        let member = holder.members.iter().find(|member| member.name == label)?;

        Some(Declaration {
            name: label.to_string(),
            home: owner.without_last(),
            kind: Kind::Definition,
            signature: self.adopt_signature(&member.signature),
            prose: member.prose.clone(),
            members: Vec::new(),
            opaque: false,
            derived: false,
            source: None,
            chip: None,
        })
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

    /// `signature` as this unit shows it: every referent under the name a consumer writes here, and every absolute spelling into an adopted root replaced by it.
    ///
    /// **A record is rendered text, so this rewrites over marks where the walk rewrites over annotations.** A mark carries the byte range its name occupies and the declaration it names, which is the same pair an annotation carries — so mapping each referent through [`Reader::public_name`] and splicing its range states exactly the rule the walk states: only an absolute spelling into a root this unit keeps to itself is rewritten, and everything an author wrote is shown as written.
    fn adopt_signature(&self, signature: &Signature) -> Signature {
        let mut text = String::new();
        let mut marks = Vec::new();
        let mut at = 0;

        for mark in &signature.marks {
            text.push_str(&signature.text[at..mark.start]);
            at = mark.end;

            let spelling = &signature.text[mark.start..mark.end];
            let referent = self.public_name(&mark.referent);
            let start = text.len();
            match spelling.starts_with('/') && self.adopted_root(&mark.referent).is_some() {
                true => text.push_str(&referent.join()),
                false => text.push_str(spelling),
            }

            marks.push(Mark {
                start,
                end: text.len(),
                within: referent.is_within(self.prefix),
                referent,
            });
        }
        text.push_str(&signature.text[at..]);

        Signature { text, marks }
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
        let imports = self.imports_of(&qualifier);
        let mut page = ModuleDocumentation {
            path: qualifier.clone(),
            prose,
            children: Vec::new(),
            declarations: Vec::new(),
        };
        let mut children = Vec::new();
        // Every name this page has already placed, so the sweep below adds each exposed name once.
        let mut placed = HashSet::new();

        for item in items {
            match item {
                TopItem::Mod(declaration) => {
                    if declaration.vis_pub {
                        let child = qualifier.with(&declaration.label);
                        page.children.push(child.clone());
                        children.push((child, lines(&declaration.doc), &declaration.module));
                    }
                }
                // A `pub use` puts what it exposes on this page, where it is written: a name a consumer reaches through this module is documented where the module offers it, whatever module declared it.
                TopItem::Use(use_) if use_.vis_pub => {
                    if let UseGroup::Named(group) = &use_.group {
                        for entry in group {
                            let label = entry.label().to_string();
                            self.expose(&qualifier, &label, &mut placed, &mut page.declarations);
                        }
                    }
                }
                // Every other item is a declaration of this module, or nothing.
                item => {
                    let before = page.declarations.len();
                    self.declare(&qualifier, &imports, item, &mut page.declarations);
                    for declaration in &page.declarations[before..] {
                        placed.insert(declaration.name.clone());
                    }
                }
            }
        }

        // What a glob exposed, and anything else resolution reached that no item above placed. A glob writes no order, so these take the one a reader can predict.
        if let Some(interface) = self.public.get(&qualifier) {
            let mut rest = interface
                .bindings
                .keys()
                .filter(|label| !placed.contains(*label))
                .cloned()
                .collect::<Vec<_>>();
            rest.sort();
            for label in rest {
                self.expose(&qualifier, &label, &mut placed, &mut page.declarations);
            }
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

    /// The name `module` exposes as `label`, appended to `out` as a declaration of this page.
    ///
    /// **A page shows what it offers, not what it wrote.** Where the declaration lives decides only what the card says about it: another page of this bundle is named as its source, and a module with no page — a private child, or a root a consumer cannot name — is not named at all, because for a consumer this page is where the declaration lives. A name declared on this page is skipped, since its own item already placed it, and one declared outside this unit is skipped because nothing here can read it.
    fn expose(
        &self,
        module: &Qualifier,
        label: &str,
        placed: &mut HashSet<String>,
        out: &mut Vec<Declaration>,
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
        let (mut declaration, source, chip) = match self.declaration_at(&entry.target) {
            // A declaration of another module: named for the page that holds it, or, where it has none, adopted and chipped with what its root holds.
            Some(found) => {
                let source = self
                    .has_page(&declaring)
                    .then(|| self.public_name(&entry.target));
                let chip = match source {
                    Some(_) => None,
                    None => self
                        .adopted_root(&entry.target)
                        .and_then(|(_, chip)| chip.clone()),
                };
                (found, source, chip)
            }
            // A member of a declaration: its owner's namespace is a path a consumer writes, and the row inside that owner's block is where the member is shown, so the card names it and links there.
            None => match self.member_at(&entry.target) {
                // The owner under the name a consumer writes, which is what the card points at: a member is shown in its owner's block, and its own path is this very card.
                Some(found) => (found, Some(self.public_name(&declaring)), None),
                // A declaration of a unit this one adopts, read from that unit's record: it has no page here and no path a consumer may write, so it is chipped with what its root holds exactly as a page-less module of this unit's own is.
                None => match self.adopted_declaration(&entry.target) {
                    Some(found) => (
                        found,
                        None,
                        self.adopted_root(&entry.target)
                            .and_then(|(_, chip)| chip.clone()),
                    ),
                    None => match self.adopted_member(&entry.target) {
                        Some(found) => (found, Some(self.public_name(&declaring)), None),
                        None => return,
                    },
                },
            },
        };

        declaration.name = label.to_string();
        declaration.home = module.clone();
        declaration.source = source;
        declaration.chip = chip;

        placed.insert(label.to_string());
        out.push(declaration);
    }

    /// The member a module exposes beside its owner: a constructor of an inductive, a method of a concept.
    ///
    /// **The head states what the member takes and what it produces, and the card links to the owner for the rest.** What a consumer reaches through `pub use Option/*` is the value constructor, whose full type also carries the family's parameters implicitly in front of the payload — but *that* is the lowering's rule, and restating it here would put a second statement of it on a page the compiler never checks. The payload and the result are written in the declaration itself, so they are read rather than derived; the parameters are left to the owner's own card, which the source names.
    fn member_at(&self, target: &Qualifier) -> Option<Declaration> {
        let owner = target.without_last();
        let declaring = owner.without_last();
        let items = self.items_of(&declaring)?;
        let imports = self.imports_of(&declaring);
        let label = target.last();
        let item = items.iter().find(|item| declares(item, owner.last()))?;

        let (signature, prose) = match item {
            TopItem::Induct(members) => {
                let holder = members
                    .iter()
                    .find(|member| member.label.as_str() == owner.last())?;
                let case = holder.cases.iter().find(|case| case.label == label)?;
                // A payload's label binds for the payloads and the target after it, exactly as it does inside the block.
                let mut binders = param_binders(&holder.params);
                binders.extend(case.payload.iter().filter_map(|param| param.label.clone()));
                (
                    self.signature(
                        &declaring,
                        &imports,
                        &binders,
                        print_case_result_head(holder, case),
                    ),
                    lines(&case.doc),
                )
            }
            TopItem::Concept(members) => {
                let holder = members
                    .iter()
                    .find(|member| member.label.as_str() == owner.last())?;
                let field = holder.fields.iter().find(|field| field.label == label)?;
                (
                    self.method(&declaring, &imports, &param_binders(&holder.params), field)
                        .signature,
                    lines(&field.doc),
                )
            }
            // A struct's fields are projections rather than a namespace a `use` selects from, and nothing else holds members at all.
            _ => return None,
        };

        Some(Declaration {
            name: label.to_string(),
            home: declaring,
            kind: Kind::Definition,
            signature,
            prose,
            members: Vec::new(),
            opaque: false,
            derived: false,
            source: None,
            chip: None,
        })
    }

    /// The declaration written at `target`, read from the module that declares it — `None` when that module is not one this unit holds.
    ///
    /// **Only the item that declares it is read.** Reading the whole module and picking afterwards would print, resolve and check a signature for every other declaration the module makes, including ones this bundle exposes nowhere — so a name only those may write would be judged against an interface that does not contain them, and the work to reach one declaration would scale with the module holding it.
    fn declaration_at(&self, target: &Qualifier) -> Option<Declaration> {
        let declaring = target.without_last();
        let items = self.items_of(&declaring)?;
        let imports = self.imports_of(&declaring);
        let label = target.last();

        let item = items.iter().find(|item| declares(item, label))?;
        let mut found = Vec::new();
        self.declare(&declaring, &imports, item, &mut found);
        found
            .into_iter()
            .find(|declaration| declaration.name == label)
    }

    /// The declarations `item` makes in `home` that a consumer can see, appended to `out`: a `let` group's `pub` members, a `pub` inductive, structure or concept with the members its representation exposes, every witness, a `pub` foreign. A module, an import and a test declare nothing here.
    fn declare(
        &self,
        home: &Qualifier,
        imports: &HashMap<String, Qualifier>,
        item: &TopItem,
        out: &mut Vec<Declaration>,
    ) {
        match item {
            TopItem::Mod(_) => {}
            // An import is not a declaration, and a test is not part of the interface.
            TopItem::Use(_) | TopItem::Test(_) => {}
            TopItem::Let(members) => {
                for member in members.iter().filter(|member| member.vis_pub) {
                    let binders = sugar_binders(match &member.signature {
                        LetSignature::Func { params, .. } => params,
                        LetSignature::Name { .. } => &[],
                    });
                    out.push(Declaration {
                        name: member.label.to_string(),
                        home: home.clone(),
                        kind: Kind::Definition,
                        signature: self.signature(home, imports, &binders, print_let_head(member)),
                        prose: lines(&member.doc),
                        members: Vec::new(),
                        opaque: false,
                        derived: false,
                        source: None,
                        chip: None,
                    });
                }
            }
            TopItem::Induct(members) => {
                for member in members.iter().filter(|member| member.vis_pub) {
                    let binders = param_binders(&member.params);
                    // Constructors are the representation: shown exactly when it is public.
                    let cases = match member.rep_pub {
                        true => member
                            .cases
                            .iter()
                            .map(|case| self.case(home, imports, &binders, case))
                            .collect(),
                        false => Vec::new(),
                    };
                    out.push(Declaration {
                        name: member.label.to_string(),
                        home: home.clone(),
                        kind: Kind::Inductive,
                        signature: self.signature(
                            home,
                            imports,
                            &binders,
                            print_induct_head(member),
                        ),
                        prose: lines(&member.doc),
                        members: cases,
                        opaque: !member.rep_pub,
                        derived: false,
                        source: None,
                        chip: None,
                    });
                }
            }
            TopItem::Struct(members) => {
                for member in members.iter().filter(|member| member.vis_pub) {
                    let mut binders = param_binders(&member.params);
                    let fields = match member.rep_pub {
                        true => member
                            .fields
                            .iter()
                            .map(|field| {
                                let shown = self.field(home, imports, &binders, field);
                                // A field's label binds for the fields after it.
                                if let Some(label) = &field.param.label {
                                    binders.insert(label.to_string());
                                }
                                shown
                            })
                            .collect(),
                        false => Vec::new(),
                    };
                    out.push(Declaration {
                        name: member.label.to_string(),
                        home: home.clone(),
                        kind: Kind::Structure,
                        signature: self.signature(
                            home,
                            imports,
                            &param_binders(&member.params),
                            print_struct_head(member),
                        ),
                        prose: lines(&member.doc),
                        members: fields,
                        opaque: !member.rep_pub,
                        derived: false,
                        source: None,
                        chip: None,
                    });
                }
            }
            TopItem::Concept(members) => {
                for member in members.iter().filter(|member| member.vis_pub) {
                    let binders = param_binders(&member.params);
                    // A concept's methods are its interface whether or not its representation is sealed: they are reached by name either way.
                    let fields = member
                        .fields
                        .iter()
                        .map(|field| self.method(home, imports, &binders, field))
                        .collect();
                    out.push(Declaration {
                        name: member.label.to_string(),
                        home: home.clone(),
                        kind: Kind::Concept,
                        signature: self.signature(
                            home,
                            imports,
                            &binders,
                            print_concept_head(member),
                        ),
                        prose: lines(&member.doc),
                        members: fields,
                        opaque: !member.rep_pub,
                        derived: false,
                        source: None,
                        chip: None,
                    });
                }
            }
            TopItem::Witness(members) => {
                for member in members {
                    let binders = sugar_binders(&member.params);
                    out.push(Declaration {
                        name: String::new(),
                        home: home.clone(),
                        kind: Kind::Witness,
                        signature: self.signature(
                            home,
                            imports,
                            &binders,
                            print_witness_head(member),
                        ),
                        prose: lines(&member.doc),
                        members: Vec::new(),
                        opaque: false,
                        derived: member.body.is_none(),
                        source: None,
                        chip: None,
                    });
                }
            }
            TopItem::Foreign(declaration) => {
                if declaration.vis_pub {
                    out.push(Declaration {
                        name: declaration.label.to_string(),
                        home: home.clone(),
                        kind: Kind::Foreign,
                        signature: self.signature(
                            home,
                            imports,
                            &HashSet::new(),
                            print_foreign_head(declaration),
                        ),
                        prose: lines(&declaration.doc),
                        members: Vec::new(),
                        opaque: false,
                        derived: false,
                        source: None,
                        chip: None,
                    });
                }
            }
        }
    }

    /// Whether `module` has a page of its own: every hop below the prefix is a `pub mod`. A private module, and every module beneath one, has none.
    fn has_page(&self, module: &Qualifier) -> bool {
        let prefix = self.prefix.segments().len();
        let segments = module.segments();
        if segments.len() < prefix || segments[..prefix] != self.prefix.segments()[..] {
            return false;
        }
        let mut current = self.prefix.clone();
        for segment in &segments[prefix..] {
            match self
                .table
                .get(&current)
                .and_then(|info| info.get_child(segment))
            {
                Some(true) => current = current.with(segment),
                _ => return false,
            }
        }
        true
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

    fn case(
        &self,
        module: &Qualifier,
        imports: &HashMap<String, Qualifier>,
        binders: &HashSet<String>,
        case: &TopCase,
    ) -> Member {
        // A payload's label binds for the payloads and the target after it.
        let mut binders = binders.clone();
        binders.extend(case.payload.iter().filter_map(|param| param.label.clone()));
        Member {
            name: case.label.to_string(),
            signature: self.signature(module, imports, &binders, print_case_head(case)),
            prose: lines(&case.doc),
        }
    }

    fn field(
        &self,
        module: &Qualifier,
        imports: &HashMap<String, Qualifier>,
        binders: &HashSet<String>,
        field: &StructField,
    ) -> Member {
        Member {
            name: field.param.label.as_deref().unwrap_or_default().to_string(),
            signature: self.signature(module, imports, binders, print_struct_field_head(field)),
            prose: lines(&field.doc),
        }
    }

    fn method(
        &self,
        module: &Qualifier,
        imports: &HashMap<String, Qualifier>,
        binders: &HashSet<String>,
        field: &ConceptField,
    ) -> Member {
        Member {
            name: field.label.to_string(),
            signature: self.signature(module, imports, binders, print_concept_field_head(field)),
            prose: lines(&field.doc),
        }
    }

    /// `head` rendered, with every name in it that resolves marked.
    fn signature(
        &self,
        module: &Qualifier,
        imports: &HashMap<String, Qualifier>,
        binders: &HashSet<String>,
        head: Printer,
    ) -> Signature {
        let (rendered, annotations) = render_annotated(head, INDENT, WIDTH);

        let mut text = String::new();
        let mut marks = Vec::new();
        let mut at = 0;
        for annotation in annotations {
            text.push_str(&rendered[at..annotation.start]);
            at = annotation.end;

            let Some(referent) = self.resolve(&annotation.name, module, imports, binders) else {
                text.push_str(&rendered[annotation.start..annotation.end]);
                continue;
            };
            let referent = self.public_name(&referent);

            // **A declaration may spell a name no consumer may write.** `/sys`'s rows state their preconditions as absolute `/sys` paths, because the module is generated and resolves against the compilation root — so the page shows the name that reaches the same declaration through the standard library instead of the one the compiler emitted. Only an absolute spelling into a root this unit keeps to itself is rewritten; everything an author wrote is shown as written.
            let start = text.len();
            match annotation.name.starts_with('/')
                && self.adopted.iter().any(|(root, _)| {
                    Qualifier::from(annotation.name.trim_start_matches('/').split('/'))
                        .is_within(root)
                }) {
                true => text.push_str(&referent.join()),
                false => text.push_str(&rendered[annotation.start..annotation.end]),
            }

            marks.push(Mark {
                start,
                end: text.len(),
                within: referent.is_within(self.prefix),
                referent,
            });
        }
        text.push_str(&rendered[at..]);

        Signature { text, marks }
    }

    /// The declaration `spelling` names as written in `module`, by the lowering's own rule: an absolute path walks the public tree from the root; a relative one is what a `use` in this module brought into scope under that spelling, else — when its head is no binder of the declaration — the module's own declaration, or a path through its own children. `None` is a binder, a name that does not resolve, or one out of view, and a page leaves each of those as plain text.
    fn resolve(
        &self,
        spelling: &str,
        module: &Qualifier,
        imports: &HashMap<String, Qualifier>,
        binders: &HashSet<String>,
    ) -> Option<Qualifier> {
        let absolute = spelling.starts_with('/');
        let segments = spelling
            .trim_start_matches('/')
            .split('/')
            .collect::<Vec<_>>();
        let (last, parents) = segments.split_last()?;

        if absolute {
            let mut current = Qualifier::empty();
            for segment in parents {
                current = visible_child(self.public, self.table, module, &current, segment)?;
            }
            return visible_binding(self.public, self.table, module, &current, last);
        }

        if let Some(target) = imports.get(spelling) {
            return Some(target.clone());
        }
        if binders.contains(segments[0]) {
            return None;
        }

        let mut current = module.clone();
        for segment in parents {
            current = visible_child(self.public, self.table, module, &current, segment)?;
        }
        visible_binding(self.public, self.table, module, &current, last)
    }

    /// Every spelling a `use` in `module` brought into scope, with what it resolved to — the union over the module's definitions of the scopes the lowering recorded for them. `use` is point-of-use, so two definitions may differ in what they see, but a spelling that resolves two ways in one module is a program nobody writes, and the first recorded wins.
    fn imports_of(&self, module: &Qualifier) -> HashMap<String, Qualifier> {
        let mut spellings = HashMap::new();
        for (owner, indices) in &self.imports.by_item {
            let Global::Authored(owner) = owner else {
                continue;
            };
            if owner.without_last() != *module {
                continue;
            }
            for index in indices {
                let import = &self.imports.entries[*index];
                let Global::Authored(target) = &import.global else {
                    continue;
                };
                spellings
                    .entry(import.spelling.clone())
                    .or_insert_with(|| target.clone());
            }
        }
        spellings
    }
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
        // A witness is anonymous, and neither a module, an import nor a test declares a name a `use` can select.
        TopItem::Witness(_) | TopItem::Mod(_) | TopItem::Use(_) | TopItem::Test(_) => false,
    }
}

/// A documentation comment's lines, or `None` when there is none.
fn lines(doc: &Option<Doc>) -> Option<Vec<String>> {
    doc.as_ref().map(|doc| doc.lines.clone())
}

/// The labels a telescope of function-sugar parameters binds — what shadows a like-named declaration in the signature they open.
fn sugar_binders(params: &[FuncSugarParam]) -> HashSet<String> {
    params
        .iter()
        .filter_map(|param| match &param.label {
            Pattern::Binder(label) => label.as_ref().map(|label| label.to_string()),
            Pattern::Tuple(_) | Pattern::Struct { .. } => None,
        })
        .collect()
}

/// The labels a declaration's parameter telescope binds.
fn param_binders(params: &[(Plicity, String, Term)]) -> HashSet<String> {
    params.iter().map(|(_, label, _)| label.clone()).collect()
}
