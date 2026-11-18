//! What another crate's suite reads of the certifier and no judgment does: its errors by name, which the soundness board's records name a refusal by, a walk with its memos off, and the one door a candidate is put to the certifier through ([`Put`]).
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {
    crate::{
        Baseline, Certifier, Error, Globals, Rechecked, Verdict, certify_definitions_over,
        recheck::verdicts_from,
    },
    curios_common::{Candidates, SyntaxRegistry, fill, variants},
    curios_core::{
        Certification, Definitions, Entrypoint, Free, Global, Places, Subterm, Term, Universes,
    },
    curios_surface::{CoreEntry, CoreFile, FormScope, ReadError, to_core},
};

/// What a candidate is put to the certifier under: the forms its text is read against, the units in scope, the registry the walk judges under, and the budget each judgment is restored to.
///
/// **One door for every attack on Core.** The soundness board puts a ticket's module through here and this crate's suites put theirs, so a ticket's record and a test's text are two ways of writing one candidate for one walk: what the board says of a ticket is what a suite would say of its text.
pub struct Put<'a> {
    /// What a text's forms are made against, where it is read over units in scope.
    pub scope: Option<&'a FormScope>,
    /// The units in scope, read where they stand; none where nothing is in scope.
    pub globals: Option<&'a Globals<'a>>,
    pub syntax: SyntaxRegistry,
    pub budget: u64,
}

/// Why a text put to the certifier wrote no module for it to walk.
#[derive(Debug)]
pub enum Unread {
    /// The text does not parse, or does not close as what it is put as: as the parser reports it, or what it closes with.
    Text(String),
    /// The reader refused the text, where the refusal stands in it.
    Read(ReadError),
}

impl Put<'_> {
    /// What `text` writes, Core text read against [`Put::scope`]: the module, and the term it closes with where it closes with one.
    pub fn read(&self, text: &str) -> Result<(Definitions, Option<Entrypoint>), Unread> {
        let file = text
            .parse::<CoreFile>()
            .map_err(|error| Unread::Text(error.format()))?;

        to_core(&file, self.scope).map_err(Unread::Read)
    }

    /// What the walk says of `text` put as a proof of `False`: Core text closing with a term that states no type, put at the `False` the registry names.
    pub fn core_text(&self, text: &str) -> Result<Rechecked<Definitions>, Unread> {
        let file = text
            .parse::<CoreFile>()
            .map_err(|error| Unread::Text(error.format()))?;
        match &file.entry {
            Some(CoreEntry { type_: None, .. }) => {}
            Some(CoreEntry { type_: Some(_), .. }) => {
                return Err(Unread::Text(
                    "the text states the type its term is put at, which is `False`".to_string(),
                ));
            }
            None => {
                return Err(Unread::Text(
                    "the text closes with no term to put".to_string(),
                ));
            }
        }
        let (definitions, entry) = to_core(&file, self.scope).map_err(Unread::Read)?;
        let entry = Entrypoint {
            body: entry.expect("an entry the text writes is read").body,
            type_: Some(Term::instance(
                Global::Authored(self.syntax.proof.false_type.qualifier()),
                Universes::default(),
            )),
        };

        Ok(self.module(&definitions, Some(&entry)))
    }

    /// What the walk says of `definitions`, and of `entry` where the module closes with one: the walk a compilation certifies a unit or a program by, answered as the module it accepted, its closing term checked among it.
    pub fn module(
        &self,
        definitions: &Definitions,
        entry: Option<&Entrypoint>,
    ) -> Rechecked<Definitions> {
        self.under(|globals| {
            verdicts_from(
                &mut Certifier::new(self.budget, self.syntax),
                definitions,
                entry,
                globals,
                None,
            )
            .module()
        })
    }

    /// [`Put::module`] over `baseline` as `certification` records it, taking what it can from what `store` holds: the walk a recompilation certifies a unit by.
    pub fn over(
        &self,
        definitions: &Definitions,
        baseline: &Definitions,
        certification: &Certification,
        store: Option<&dyn Candidates>,
    ) -> Rechecked<Definitions> {
        let (places, now) = (Places::of(baseline), Places::of(definitions));
        let baseline = Baseline {
            definitions: baseline,
            places: &places,
            certification,
        };

        self.under(|globals| {
            certify_definitions_over(
                definitions,
                self.budget,
                globals,
                baseline,
                (&now, store),
                self.syntax,
            )
            .0
        })
    }

    /// `walk` run over the units in scope.
    fn under<R>(&self, walk: impl FnOnce(&Globals) -> R) -> R {
        match self.globals {
            Some(globals) => walk(globals),
            None => walk(&Globals::default()),
        }
    }
}

/// What `text` writes, Core text read with nothing in scope, panicking where it does not read.
fn read_alone(text: &str) -> (Definitions, Option<Entrypoint>) {
    let file = text
        .parse::<CoreFile>()
        .unwrap_or_else(|error| panic!("{}", error.format()));

    to_core(&file, None).unwrap_or_else(|refusal| panic!("{refusal:?}"))
}

/// The module `text` writes, Core text read with nothing in scope and closing with no term: what a suite below the walk hands a judgment of a whole item or entry.
///
/// # Panics
///
/// Where the text does not read or closes with a term.
pub fn read_module(text: &str) -> Definitions {
    let (definitions, entry) = read_alone(text);
    assert!(entry.is_none(), "a module's text closes with no term");

    definitions
}

/// The term `text` closes with, Core text read with nothing in scope and declaring nothing: a candidate a suite below the walk puts to one judgment.
///
/// # Panics
///
/// Where the text does not read, declares anything, or closes with no term.
pub fn term(text: &str) -> Term {
    let (definitions, entry) = read_alone(text);
    assert!(
        definitions.items.is_empty(),
        "a term's text declares nothing"
    );

    entry.expect("a term's text closes with it").body
}

/// The term `text` closes with, Core text read with nothing in scope beside the declarations it names, which no judgment is handed: a row a foreign call is made through, and the definition its form implies, which a text read with no scope states.
///
/// # Panics
///
/// Where the text does not read or closes with no term.
pub fn closing(text: &str) -> Term {
    let (_, entry) = read_alone(text);

    entry.expect("a term's text closes with it").body
}

/// Declare in `certifier` what `text` declares, Core text read with nothing in scope, judging none of it: each family's and record's entry, and each definition, a family's constructors among them, at its type with its body. What a suite below the walk reads its declarations from.
///
/// # Panics
///
/// Where the text does not read, closes with a term, or declares a name `certifier` holds.
pub fn declared(certifier: &mut Certifier, text: &str) {
    let (definitions, entry) = read_alone(text);
    assert!(entry.is_none(), "a declarations' text closes with no term");
    for item in definitions.items.iter() {
        for (name, nominal) in item.nominals() {
            if let Some(declaration) = nominal.inductive() {
                certifier
                    .declare_induct(name, declaration)
                    .expect("a name is declared once");
            }
            if let Some(record) = nominal.record() {
                certifier
                    .declare_struct(name, record)
                    .expect("a name is declared once");
            }
        }
        for definition in item.definitions() {
            certifier
                .define(
                    &definition.name,
                    &definition.type_,
                    &definition.body,
                    &definition.universe_context,
                )
                .expect("a name is declared once");
        }
        for made in item.formers().into_iter().chain(item.constructors()) {
            certifier
                .define(&made.name, &made.type_, &made.body, &made.universe_context)
                .expect("a name is declared once");
        }
    }
}

/// The term `text` closes with ([`term`]), its leading function literal opened at fresh locals, each assumed in `certifier` at its domain: the locals, and the body under them. How a suite below the walk puts a term to a judgment under hypotheses, each written as a binder of the literal.
pub fn opened(certifier: &mut Certifier, text: &str) -> (Vec<Free>, Term) {
    let term = term(text);
    let Subterm::Func(function) = &*term else {
        return (Vec::new(), term);
    };
    let mut cursor = function.telescope.cursor();
    let mut locals = Vec::new();
    while let Some((_, domain)) = cursor.entry() {
        locals.push(certifier.advance_assumed(&mut cursor, &domain));
    }
    let body = cursor
        .body()
        .expect("a cursor past every entry holds its body");

    (locals, body)
}

/// The terms the tuple `term` holds, in order: how a text puts several terms under one context, the body of a literal [`opened`] opens.
///
/// # Panics
///
/// Where `term` is no tuple.
pub fn components(term: &Term) -> Vec<Term> {
    match &**term {
        Subterm::Tuple(tuple) => tuple.fields.clone(),
        _ => panic!("a tuple of the terms put under one context"),
    }
}

/// The terms the tuple `text` writes, Core text read as [`term`] reads one: several terms over one set of free words, each word one local in every term.
///
/// # Panics
///
/// Where the text does not read, or writes other than a tuple of `N` terms.
pub fn terms<const N: usize>(text: &str) -> [Term; N] {
    components(&term(text))
        .try_into()
        .unwrap_or_else(|_| panic!("{N} terms"))
}

/// The locals the hypotheses `binders` writes open at in `certifier` ([`opened`]), and `terms`, several terms written over them ([`components`]): each, in order. A lone term is written with a trailing comma, a tuple of one.
///
/// # Panics
///
/// Where the text does not read, or holds other than `N` terms.
pub fn hypotheses<const N: usize>(
    certifier: &mut Certifier,
    binders: &str,
    terms: &str,
) -> (Vec<Free>, [Term; N]) {
    let (locals, opened) = opened(certifier, &format!("({binders}) => ({terms})"));
    let terms = components(&opened)
        .try_into()
        .unwrap_or_else(|_| panic!("{N} terms"));

    (locals, terms)
}

/// The terms [`hypotheses`] opens, where no test reads the locals themselves.
///
/// # Panics
///
/// Where the text does not read, or holds other than `N` terms.
pub fn under<const N: usize>(certifier: &mut Certifier, binders: &str, terms: &str) -> [Term; N] {
    hypotheses(certifier, binders, terms).1
}

/// `depth` levels of `node` over `base`, the text a doubling graph is written in: each level's operand, which `node` writes at `${below}` and again at `${again}`, written once and shared by its second occurrence under the share keys from `keys` on. A tree of `2^depth` leaves, and a graph of `depth + 1` nodes.
pub fn doubled(node: &str, base: &str, depth: usize, keys: usize) -> String {
    (keys..keys + depth).fold(base.to_string(), |operand, key| {
        fill!(
            node,
            below = fill!("#{share = ${key}} (${operand})", key, operand),
            again = fill!("#{shared = ${key}} _", key)
        )
    })
}

variants!(Error {
    Reduce,
    Unbound,
    NotClosed,
    Undeclared,
    UndeclaredCase,
    Redeclared,
    Unclassified,
    NotASort,
    OpenSort,
    AtSorts,
    TooManySorts,
    NotAGoal,
    Mismatch,
    NotAFunction,
    NotATuple,
    NotASlotOf,
    Arity,
    Mark,
    LargeElimination,
    NotCore,
    IndexedByItsGroup,
    NeedsItsOwnTyping,
    MissingArm,
    NotDescending,
    NotPositive,
    HoldsItself,
    VarianceDenied,
    Oversized,
    NotTotal,
    Informative,
    UntypedEntry,
    UnclosedUniverses,
    UnsatisfiableUniverses,
    UniverseInstance,
});

/// [`certify_definitions`](crate::certify_definitions)'s verdicts with the certifier's evaluation memos off.
///
/// Exists for one purpose: asserting that memoization changes no *semantic* verdict — the property that makes a memo an evaluation strategy rather than a store. A resource verdict may differ, since a term-keyed hit is free.
pub fn recheck_definitions_verdicts_uncached(
    definitions: &Definitions,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    verdicts_from(
        &mut Certifier::uncached(budget, syntax),
        definitions,
        None,
        globals,
        None,
    )
    .module()
    .into_verdicts()
}
