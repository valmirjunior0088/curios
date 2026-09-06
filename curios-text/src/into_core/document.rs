//! The builder of a unit's [`Documentation`] record — `curios-document`'s plain data: one record per module the unit exposes, each declaration's head printed as the author wrote it with every name it mentions resolved, and the prose attached to each — built by the lowering as the last thing it does and carried on the unit it lowered.
//!
//! **Built by the compilation that builds the unit, from the tables it just built.** Which modules and declarations a page shows is the export view resolution built to a fixed point, so a private declaration is absent rather than hidden and a re-export is listed as a link to the declaration it names — unless the declaration's own module has no page, the facade pattern, where the declaration is read off that module and filed under the re-exporting one, since the re-export is the only way it reaches a consumer. A referent is looked up with the visibility functions the lowering resolves a name with, over the same tables, seeded by the import scopes the lowering recorded per definition — nothing here resolves a name by a rule of its own. Nothing is read from the elaborated module either: every declaration states its signature, so the surface tree the lowering parsed is the whole of what a page prints, and a signature is printed by the printers `curios format` prints it with. Riding on the unit, the record travels wherever the unit does — the prelude image, a verdict slot, the browser bundle — so a unit is documented from its stored form without its sources.
//!
//! **A library is documented for its consumers.** That is the one audience this record knows: a constructor appears only when the representation is public, a field likewise, and a test never. A program has no consumer, so nothing here documents one; which mount is documented, and with what description, is the resolver's to say.

use {
    super::*,
    crate::{
        print_case_head, print_concept_field_head, print_concept_head, print_foreign_head,
        print_induct_head, print_let_head, print_struct_field_head, print_struct_head,
        print_witness_head,
    },
    curios_core::{Global, Imports},
    curios_document::{
        Declaration, Documentation, Kind, Mark, Member, ModuleDocumentation, Reexport, Signature,
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
/// Infallible, because every module it visits is one discovery loaded a moment ago: a prefix without a module in the map is a broken invariant of this stage, not a condition a caller can meet.
pub(super) fn document(
    modules: &HashMap<Qualifier, Rc<Module>>,
    table: &Scoped<'_, ModuleInfo>,
    public: &Scoped<'_, PublicInterface>,
    imports: &Imports,
    prefix: &Qualifier,
    description: Option<String>,
) -> Documentation {
    let reader = Reader {
        modules,
        table,
        public,
        imports,
        prefix,
    };

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
}

impl Reader<'_> {
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
            reexports: self.reexports(&qualifier),
        };
        let mut children = Vec::new();

        for item in items {
            match item {
                TopItem::Mod(declaration) => {
                    if declaration.vis_pub {
                        let child = qualifier.with(&declaration.label);
                        page.children.push(child.clone());
                        children.push((child, lines(&declaration.doc), &declaration.module));
                    }
                }
                // Every other item is a declaration of this module, or nothing.
                item => self.declare(&qualifier, &imports, item, &mut page.declarations),
            }
        }

        // The facade: a `pub use` out of a module with no page of its own is the one way that declaration reaches a consumer, so it is documented here, after the module's own declarations and sorted by name, at the home a mark names it under.
        let mut facades = Vec::new();
        if let Some(interface) = self.public.get(&qualifier) {
            for (label, entry) in &interface.bindings {
                let Some(home) = self.facade_home(&qualifier, label, &entry.target) else {
                    continue;
                };
                let items = self.items_of(&home).expect("a facade home is a module");
                let imports = self.imports_of(&home);
                let mut found = Vec::new();
                for item in items {
                    self.declare(&home, &imports, item, &mut found);
                }
                facades.extend(
                    found
                        .into_iter()
                        .filter(|declaration| declaration.name == entry.target.last()),
                );
            }
        }
        facades.sort_by(|left, right| left.name.cmp(&right.name));
        page.declarations.extend(facades);

        out.push(page);

        // An inline module's items are in the tree; a file-backed one's are in the map, where discovery filed them.
        for (child, prose, inline) in children {
            match inline {
                Some(module) => self.visit_items(child, prose, &module.items, out),
                None => self.visit(child, prose, out),
            }
        }
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
                                    binders.insert(label.clone());
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

    /// The module a binding `module` exposes as `label` was declared in, when that module has no page of its own and the binding is therefore documented on `module`'s page: a `pub use` out of a private child, or out of a module below one. `None` for a declaration of `module` itself, one outside the unit, or one whose home has a page and so is listed as a link.
    fn facade_home(
        &self,
        module: &Qualifier,
        label: &str,
        target: &Qualifier,
    ) -> Option<Qualifier> {
        if *target == module.with(label) || !target.is_within(self.prefix) {
            return None;
        }
        let home = target.without_last();
        match !self.has_page(&home) && self.items_of(&home).is_some() {
            true => Some(home),
            false => None,
        }
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
            name: case.label.clone(),
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
            name: field.param.label.clone().unwrap_or_default(),
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
            name: field.label.clone(),
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
        let (text, annotations) = render_annotated(head, INDENT, WIDTH);
        let marks = annotations
            .into_iter()
            .filter_map(|annotation| {
                self.resolve(&annotation.name, module, imports, binders)
                    .map(|referent| Mark {
                        start: annotation.start,
                        end: annotation.end,
                        within: referent.is_within(self.prefix),
                        referent,
                    })
            })
            .collect();
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

    /// The names `module`'s export view exposes for declarations made on other pages. One made in a module with no page is not a link but a declaration of this page, and is left to `facade_home`.
    fn reexports(&self, module: &Qualifier) -> Vec<Reexport> {
        let Some(interface) = self.public.get(module) else {
            return Vec::new();
        };
        let mut reexports = interface
            .bindings
            .iter()
            .filter(|(label, entry)| entry.target != module.with(label))
            .filter(|(label, entry)| self.facade_home(module, label, &entry.target).is_none())
            .map(|(label, entry)| Reexport {
                name: label.clone(),
                referent: entry.target.clone(),
                within: entry.target.is_within(self.prefix),
            })
            .collect::<Vec<_>>();
        reexports.sort_by(|left, right| left.name.cmp(&right.name));
        reexports
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
