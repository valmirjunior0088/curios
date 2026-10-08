//! What a top-level item declares, apart from what it says.
//!
//! Everything a lowering decides before it lowers a declaration is read off heads: which names a module binds and which it exports, which modules it declares and where each is written, what each `use` brings into scope, and where each declaration's own text lies. Only a declaration's lowering reads its tree, so the passes over a whole unit take heads and never an item.

use {
    crate::{Label, Module, TopItem, TopUse},
    curios_utilities::{Report, Span},
};

/// A name an item declares, and whether it is `pub`.
#[derive(Debug, Clone)]
pub(crate) struct Labelled {
    pub label: Label,
    pub vis_pub: bool,
}

/// One `induct` of a group: its name, whether the name and the representation are `pub`, and its constructors' names.
#[derive(Debug, Clone)]
pub(crate) struct Family {
    pub label: Label,
    pub vis_pub: bool,
    pub rep_pub: bool,
    pub cases: Vec<Label>,
}

/// One `struct` of a group: its name, and whether the name and the representation are `pub`.
#[derive(Debug, Clone)]
pub(crate) struct Nominal {
    pub label: Label,
    pub vis_pub: bool,
    pub rep_pub: bool,
}

/// One `concept` of a group: its name, whether it is `pub`, and its methods' names. A superclass edge has none.
#[derive(Debug, Clone)]
pub(crate) struct Interface {
    pub label: Label,
    pub vis_pub: bool,
    pub fields: Vec<Label>,
}

/// What one top-level item declares: one variant for each kind of item, holding what the passes over a unit read of it.
#[derive(Debug, Clone)]
pub(crate) enum Head {
    /// A `mod`: its name, whether it is `pub`, where the whole declaration is written, and its items' heads where its body is written inline. A file-backed module's are its file's.
    Mod {
        label: Label,
        vis_pub: bool,
        span: Option<Span>,
        inline: Option<Vec<Headed>>,
    },
    /// A `use`, whole: it is the scope the declarations after it are lowered in.
    Use(TopUse),
    Let(Vec<Labelled>),
    Induct(Vec<Family>),
    Struct(Vec<Nominal>),
    Concept(Vec<Interface>),
    /// A `satisfy`, which declares no name.
    Witness,
    Foreign(Labelled),
    Test(Label),
    /// An item the parser could not read: the name its head declared, where it got that far, and the parser's report.
    Broken {
        declares: Option<Label>,
        report: Report,
    },
}

/// One item's head, and where the item's own text lies in its file. `None` for an item that was built rather than written.
#[derive(Debug, Clone)]
pub(crate) struct Headed {
    pub head: Head,
    pub own: Option<Span>,
}

impl Headed {
    /// The heads of `module`'s items, in the items' order.
    pub(super) fn of(module: &Module) -> Vec<Headed> {
        module
            .items
            .iter()
            .enumerate()
            .map(|(index, item)| Headed {
                head: Head::of(item),
                own: module.spans.get(index).cloned(),
            })
            .collect()
    }
}

impl Head {
    fn of(item: &TopItem) -> Self {
        match item {
            TopItem::Mod(declaration) => Head::Mod {
                label: declaration.label.clone(),
                vis_pub: declaration.vis_pub,
                span: declaration.span.clone(),
                inline: declaration.module.as_ref().map(Headed::of),
            },
            TopItem::Use(use_item) => Head::Use(use_item.clone()),
            TopItem::Let(group) => Head::Let(
                group
                    .iter()
                    .map(|let_| Labelled {
                        label: let_.label.clone(),
                        vis_pub: let_.vis_pub,
                    })
                    .collect(),
            ),
            TopItem::Induct(group) => Head::Induct(
                group
                    .iter()
                    .map(|induct| Family {
                        label: induct.label.clone(),
                        vis_pub: induct.vis_pub,
                        rep_pub: induct.rep_pub,
                        cases: induct.cases.iter().map(|case| case.label.clone()).collect(),
                    })
                    .collect(),
            ),
            TopItem::Struct(group) => Head::Struct(
                group
                    .iter()
                    .map(|struct_| Nominal {
                        label: struct_.label.clone(),
                        vis_pub: struct_.vis_pub,
                        rep_pub: struct_.rep_pub,
                    })
                    .collect(),
            ),
            TopItem::Concept(group) => Head::Concept(
                group
                    .iter()
                    .map(|concept| Interface {
                        label: concept.label.clone(),
                        vis_pub: concept.vis_pub,
                        fields: concept
                            .fields
                            .iter()
                            .filter_map(|field| field.label.clone())
                            .collect(),
                    })
                    .collect(),
            ),
            TopItem::Witness(_) => Head::Witness,
            TopItem::Foreign(foreign) => Head::Foreign(Labelled {
                label: foreign.label.clone(),
                vis_pub: foreign.vis_pub,
            }),
            TopItem::Test(test) => Head::Test(test.label.clone()),
            TopItem::Broken(broken) => Head::Broken {
                declares: broken.declares.clone(),
                report: broken.report.clone(),
            },
        }
    }
}
