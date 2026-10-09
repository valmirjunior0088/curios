//! What a declaration references: the names the unused-declaration lint follows from one item to the next, and the alias edges a public signature is audited through.
//!
//! Nothing here orders anything. A unit's items leave the lowering as they are written, and which of them elaborates first is elaboration's to say: it asks for each declaration when it is read, and refuses the ones that need one another by name.

use {
    super::{FlatItem, FlatLet},
    curios_core::Bound,
    curios_utilities::{Span, SyntaxRegistry},
    std::collections::{BTreeMap, HashMap, HashSet},
};

/// What the unit-wide passes read of one lowered item, taken once, where it is lowered and its terms are open: the passes then read no term, so an item a later lowering keeps answers them as one it lowers does.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(crate) struct LoweredItem {
    /// Every name the item's declaration references ([`node_reference_names`]), in the names' order.
    pub reaches: Vec<curios_core::Global>,
    /// What each of its definitions declares, in the item's order.
    pub definitions: Vec<Declared>,
}

/// One definition of a lowered item, as the exposure audit and the `unused-declaration` lint read it.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(crate) struct Declared {
    /// Where its name was written, in the text of its declaration: what the `unused-declaration` lint underlines. `None` for a definition the compiler named — a constructor, a method wrapper, a witness, a test — and for one that was built rather than written.
    pub label: Option<curios_core::Extent>,
    /// The top-level definitions its declared type names, in the order a term lists its free variables. Taken of a definition written under its own name alone, the only kind the audit asks of.
    pub signature: Vec<curios_core::Global>,
    /// What it is a bare, transparent alias of, where it is one.
    pub alias: Option<AliasEdge>,
}

impl LoweredItem {
    /// What the passes read of `item`, whose registry entries `induct_decls` and `struct_decls` already hold. `placed` gives the extent of a span in the text of the declaration the item was lowered from.
    pub(super) fn of(
        item: &FlatItem,
        placed: impl Fn(&Span) -> Option<curios_core::Extent>,
        induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
        struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
        syntax: &SyntaxRegistry,
    ) -> Self {
        let mut reaches =
            node_reference_names(item, &item.names(), induct_decls, struct_decls, syntax)
                .into_iter()
                .collect::<Vec<_>>();
        reaches.sort();

        Self {
            reaches,
            definitions: item
                .lets()
                .iter()
                .map(|let_| Declared {
                    label: let_.span.as_ref().and_then(&placed),
                    signature: match written_under_its_name(let_) {
                        true => globals(let_.type_.free_vars()).collect(),
                        false => Vec::new(),
                    },
                    alias: alias_of(let_),
                })
                .collect(),
        }
    }
}

/// Whether the source wrote this definition under its own name, in the module that declares it. A member synthesized into a nested namespace — an inductive's constructor, a concept's method wrapper — sits below its declaring module, and a witness has no authored path at all.
fn written_under_its_name(let_: &FlatLet) -> bool {
    let_.name
        .qualifier()
        .is_some_and(|path| path.without_last() == let_.island)
}

/// The top-level definitions among `names`. A binder is nobody's dependency: it is introduced and discharged inside the very signature being read.
pub(super) fn globals(
    names: impl IntoIterator<Item = curios_core::Free>,
) -> impl Iterator<Item = curios_core::Global> {
    names
        .into_iter()
        .filter_map(|name| name.as_global().cloned())
}

/// The full set of names one node's declaration references: its own free vars, plus (for a declared inductive/struct) its registry entry's free vars. An inductive's declaration is wider than its items: the registry entry's constructor payload and target types are elaborated alongside the type-binding group (`curios_elab::elaborate_module_rec` rebuilds the registry telescopes there), so a node declaring a registered name references everything its registry entry does — those names live nowhere in the type binding's own `type_`/`body`. Struct field types live in the registry too.
pub(super) fn node_reference_names(
    item: &FlatItem,
    declared: &[curios_core::Global],
    induct_decls: &BTreeMap<curios_core::Global, curios_core::InductDecl>,
    struct_decls: &BTreeMap<curios_core::Global, curios_core::StructDecl>,
    syntax: &SyntaxRegistry,
) -> HashSet<curios_core::Global> {
    let mut names = item.free_vars();
    for name in declared {
        if let Some(induct_decl) = induct_decls.get(name) {
            names.extend(induct_free_vars(induct_decl));
        }
        if let Some(struct_decl) = struct_decls.get(name) {
            names.extend(struct_free_vars(struct_decl));
        }
    }
    names.extend(derived_vocabulary(item, syntax));
    names
}

/// The names a derived witness body will reference once elaboration writes it — the vocabulary of the derivation registered for the concept its signature names, which the `Derive` transient stands for without spelling. References, as the written body's `Var`s would be: a renderer, a method wrapper or a piece of the string machinery that only a derived witness applies is used. Restricted to the signature's concept so one derivation's names never count for the other's rows, and empty for an item that derives nothing.
///
/// **Every name a body *writes*, and nothing it merely dispatches through.** A concept reached by infix operator is not here and must not be: `&&` in the equality body is rebuilt by `elaborate_infix` as a projection off a resolved witness, never a wrapper `Var`, so it names no global at all.
fn derived_vocabulary(item: &FlatItem, syntax: &SyntaxRegistry) -> Vec<curios_core::Global> {
    let mut vocabulary = Vec::new();
    for let_ in item.lets().iter().filter(|let_| let_.body.has_derive()) {
        let Some(concept) = witness_concept(let_).and_then(|name| name.qualifier().cloned()) else {
            continue;
        };
        let Some(derivation) = syntax
            .derivations
            .rows()
            .find(|derivation| derivation.concept_field().concept.qualifier() == concept)
        else {
            continue;
        };
        vocabulary.extend(
            derivation
                .vocabulary(&syntax.string)
                .into_iter()
                .map(curios_core::Global::Authored),
        );
    }
    vocabulary
}

/// The concept a witness row registers into — the head of its signature's terminal concept application, peeled through the premise telescope. LoweredItem form only: pre-elaboration the terminal is an `Apply`/`Var` spine, so the head is a free global rather than a `StructType` normal form (kept as a fallback for synthetic inputs).
fn witness_concept(let_: &FlatLet) -> Option<curios_core::Global> {
    fn head_of(term: &curios_core::Term) -> Option<curios_core::Global> {
        match &**term {
            curios_core::Subterm::FuncType(func_type) => {
                let mut telescope = &func_type.telescope;
                loop {
                    match telescope {
                        curios_core::Telescope::Done(body) => return head_of(body),
                        curios_core::Telescope::Cons(_, _, scope) => telescope = scope.body(),
                    }
                }
            }
            curios_core::Subterm::Apply(apply) => head_of(&apply.head),
            curios_core::Subterm::Instance(inst) => inst
                .head
                .head_name()
                .and_then(|free| free.as_global())
                .cloned(),
            curios_core::Subterm::Var(var) => {
                var.as_free().and_then(|free| free.as_global()).cloned()
            }
            curios_core::Subterm::StructType(struct_type) => Some(struct_type.name),
            _ => None,
        }
    }

    matches!(let_.kind, curios_core::DefinitionKind::Witness)
        .then(|| head_of(&let_.type_))
        .flatten()
}

/// Owner index (declared name → node) over the given nodes only.
pub(super) fn owner_of(items: &[FlatItem], nodes: &[usize]) -> HashMap<curios_core::Global, usize> {
    nodes
        .iter()
        .flat_map(|&n| items[n].names().into_iter().map(move |name| (name, n)))
        .collect()
}

/// The external references of an inductive registry entry: every free var of its telescopes. Binder names (parameters, payload binders) are captured by `Telescope::build` and never appear here; the index types' references also live in the type binding's own signature, but are included for robustness.
fn induct_free_vars(induct_decl: &curios_core::InductDecl) -> HashSet<curios_core::Global> {
    induct_decl
        .arity
        .free_vars()
        .into_iter()
        .chain(
            induct_decl
                .constructors
                .iter()
                .flat_map(|(_, param)| param.telescope.free_vars()),
        )
        .filter_map(|name| name.as_global().cloned())
        .collect()
}

/// The external references of a struct registry entry: every free var of its arity — its parameter domains and the field telescope they terminate in. Like `induct_free_vars`, this is what makes a struct's type-former node depend on the (e.g. intrinsic) types its fields mention — they live nowhere in the type-former's own body, which is just the `StructType` normal form.
fn struct_free_vars(struct_decl: &curios_core::StructDecl) -> HashSet<curios_core::Global> {
    struct_decl
        .arity
        .free_vars()
        .into_iter()
        .filter_map(|name| name.as_global().cloned())
        .collect()
}

/// A definition whose body is a bare top-level name: what it stands for and, where it is a type alias written out in full, every top-level name its body mentions.
#[derive(Debug, Clone)]
#[curios_archive::archived]
pub(crate) struct AliasEdge {
    pub target: curios_core::Global,
    pub dependencies: Option<Vec<curios_core::Global>>,
}

/// The alias `let_` is, where its body is one.
fn alias_of(let_: &FlatLet) -> Option<AliasEdge> {
    // An alias target is a top-level definition. A body that is a bare *local* is not an alias — a discriminant test, not a leading-`/` test on the spelling.
    let direct = let_.body.direct_type_alias_target(&let_.type_);
    let target = *direct
        .or_else(|| let_.body.transparent_alias_target())
        .and_then(curios_core::Free::as_global)?;

    Some(AliasEdge {
        target,
        dependencies: direct.map(|_| globals(let_.body.free_vars()).collect()),
    })
}

/// Every alias among `items`, by the name that is one, read off the facts taken of each.
pub(super) fn aliases<'a>(
    items: &[FlatItem],
    facts: &[&'a LoweredItem],
) -> BTreeMap<curios_core::Global, &'a AliasEdge> {
    items
        .iter()
        .zip(facts)
        .flat_map(|(item, lowered)| item.declared().zip(&lowered.definitions))
        .filter_map(|((name, _, _), declared)| Some((name, declared.alias.as_ref()?)))
        .collect()
}
