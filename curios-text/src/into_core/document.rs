//! What a unit's interface is, read for a page: one record per module the unit exposes, each declaration's head printed as the author wrote it with every name it mentions resolved, and the prose attached to each — plain data a renderer walks and a transport encodes, built by the lowering as the last thing it does and carried on the unit it lowered.
//!
//! **Built by the compilation that builds the unit, from the tables it just built.** Which modules and declarations a page shows is the export view resolution built to a fixed point, so a private declaration is absent rather than hidden and a re-export is listed as a link to the declaration it names. A referent is looked up with the visibility functions the lowering resolves a name with, over the same tables, seeded by the import scopes the lowering recorded per definition — nothing here resolves a name by a rule of its own. Nothing is read from the elaborated module either: every declaration states its signature, so the surface tree the lowering parsed is the whole of what a page prints, and a signature is printed by the printers `curios format` prints it with. Riding on the unit, the record travels wherever the unit does — the prelude image, a verdict slot, the browser bundle — so a unit is documented from its stored form without its sources.
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
    curios_print::{Printer, render_annotated},
    std::collections::{HashMap, HashSet},
};

/// The width a signature is rendered within — the formatter's, so a page and a file agree on where a long telescope breaks.
const WIDTH: usize = 100;

/// The indent a broken signature continues at — the formatter's.
const INDENT: usize = 4;

/// A unit's interface, for its consumers.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Documentation {
    /// The prefix the unit mounts at — `/json` for the package `json` — which every module path below begins with.
    pub prefix: Qualifier,
    /// What the unit is, in a sentence or a few, for its landing page: the manifest's `description` for a package, a constant for the standard library, nothing when neither said.
    pub description: Option<String>,
    /// Every module a consumer can reach, the root first and each parent before its children.
    pub modules: Vec<ModuleDocumentation>,
}

/// One module's page.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct ModuleDocumentation {
    pub path: Qualifier,
    /// The `-- |` block above the `mod` declaration that declares it; `None` for the root, whose prose is the manifest's.
    pub prose: Option<Vec<String>>,
    /// The public child modules, in declaration order.
    pub children: Vec<Qualifier>,
    /// The declarations written here that a consumer can see, in source order.
    pub declarations: Vec<Declaration>,
    /// The names this module exposes that are declared elsewhere — a `pub use` — each a link to where the declaration lives, sorted by name.
    pub reexports: Vec<Reexport>,
}

/// What kind of declaration a page entry is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Kind {
    Definition,
    Inductive,
    Structure,
    Concept,
    Witness,
    Foreign,
}

/// One declaration a consumer can see: its head as written, its prose, and the members its representation exposes.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Declaration {
    /// The declared label — and the anchor a link to it names. Empty for a witness, which is anonymous by design.
    pub name: String,
    pub kind: Kind,
    pub signature: Signature,
    pub prose: Option<Vec<String>>,
    /// Constructors, fields or concept methods: present only when the representation is public, so an opaque type shows none.
    pub members: Vec<Member>,
    /// A `satisfy` whose body the compiler writes.
    pub derived: bool,
}

/// One constructor, field or concept method.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Member {
    pub name: String,
    pub signature: Signature,
    pub prose: Option<Vec<String>>,
}

/// A declaration head as printed, and every name in it that resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Signature {
    pub text: String,
    /// Ascending by position, non-overlapping.
    pub marks: Vec<Mark>,
}

/// One name in a signature, resolved: the byte range of `text` it occupies and the declaration it names.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Mark {
    pub start: usize,
    pub end: usize,
    /// The canonical path of the declaration named.
    pub referent: Qualifier,
    /// Whether the referent lies within the documented unit, and so has a page in the same bundle.
    pub within: bool,
}

/// A name this module exposes for a declaration made elsewhere.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Reexport {
    pub name: String,
    pub referent: Qualifier,
    pub within: bool,
}

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
                // An import is not a declaration, and a test is not part of the interface.
                TopItem::Use(_) | TopItem::Test(_) => {}
                TopItem::Let(members) => {
                    for member in members.iter().filter(|member| member.vis_pub) {
                        let binders = sugar_binders(match &member.signature {
                            LetSignature::Func { params, .. } => params,
                            LetSignature::Name { .. } => &[],
                        });
                        page.declarations.push(Declaration {
                            name: member.label.to_string(),
                            kind: Kind::Definition,
                            signature: self.signature(
                                &qualifier,
                                &imports,
                                &binders,
                                print_let_head(member),
                            ),
                            prose: lines(&member.doc),
                            members: Vec::new(),
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
                                .map(|case| self.case(&qualifier, &imports, &binders, case))
                                .collect(),
                            false => Vec::new(),
                        };
                        page.declarations.push(Declaration {
                            name: member.label.to_string(),
                            kind: Kind::Inductive,
                            signature: self.signature(
                                &qualifier,
                                &imports,
                                &binders,
                                print_induct_head(member),
                            ),
                            prose: lines(&member.doc),
                            members: cases,
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
                                    let shown = self.field(&qualifier, &imports, &binders, field);
                                    // A field's label binds for the fields after it.
                                    if let Some(label) = &field.param.label {
                                        binders.insert(label.clone());
                                    }
                                    shown
                                })
                                .collect(),
                            false => Vec::new(),
                        };
                        page.declarations.push(Declaration {
                            name: member.label.to_string(),
                            kind: Kind::Structure,
                            signature: self.signature(
                                &qualifier,
                                &imports,
                                &param_binders(&member.params),
                                print_struct_head(member),
                            ),
                            prose: lines(&member.doc),
                            members: fields,
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
                            .map(|field| self.method(&qualifier, &imports, &binders, field))
                            .collect();
                        page.declarations.push(Declaration {
                            name: member.label.to_string(),
                            kind: Kind::Concept,
                            signature: self.signature(
                                &qualifier,
                                &imports,
                                &binders,
                                print_concept_head(member),
                            ),
                            prose: lines(&member.doc),
                            members: fields,
                            derived: false,
                        });
                    }
                }
                TopItem::Witness(members) => {
                    for member in members {
                        let binders = sugar_binders(&member.params);
                        page.declarations.push(Declaration {
                            name: String::new(),
                            kind: Kind::Witness,
                            signature: self.signature(
                                &qualifier,
                                &imports,
                                &binders,
                                print_witness_head(member),
                            ),
                            prose: lines(&member.doc),
                            members: Vec::new(),
                            derived: member.body.is_none(),
                        });
                    }
                }
                TopItem::Foreign(declaration) => {
                    if declaration.vis_pub {
                        page.declarations.push(Declaration {
                            name: declaration.label.to_string(),
                            kind: Kind::Foreign,
                            signature: self.signature(
                                &qualifier,
                                &imports,
                                &HashSet::new(),
                                print_foreign_head(declaration),
                            ),
                            prose: lines(&declaration.doc),
                            members: Vec::new(),
                            derived: false,
                        });
                    }
                }
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

    /// The names `module`'s export view exposes for declarations made elsewhere.
    fn reexports(&self, module: &Qualifier) -> Vec<Reexport> {
        let Some(interface) = self.public.get(module) else {
            return Vec::new();
        };
        let mut reexports = interface
            .bindings
            .iter()
            .filter(|(label, entry)| entry.target != module.with(label))
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
