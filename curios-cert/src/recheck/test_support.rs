//! The modules the recheck suites put to the walk, written as Core text, and the walks they put them through.
//!
//! Written as Core text because a refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program. Reaching it means writing the finished module and putting it to the walk directly.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `recheck`, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::SYNTAX,
    curios_common::{Candidates, Qualifier, SyntaxRegistry, fill},
    curios_core::{
        Certification, Certified, Definitions, Entrypoint, Global, Item, Places, Program, Reads,
        Term,
    },
    std::collections::BTreeSet,
};

/// A unit a test puts in scope: a module no unit holds, with the places an environment reads it by and the record filed beside it.
pub struct AlreadyJudged {
    definitions: Definitions,
    places: Places,
    certification: Certification,
}

impl AlreadyJudged {
    /// `module` in scope beside `certification`, whichever walk filed it: what a walk over it accepted beside the record it filed, as [`already_judged`] mounts, or bytes no walk wrote, which a test mounts to state the claim's edge as a forger would, and nothing else mounts.
    pub fn with(definitions: &Definitions, certification: &Certification) -> Self {
        Self {
            definitions: definitions.clone(),
            places: Places::of(definitions),
            certification: certification.clone(),
        }
    }

    /// The environment a walk is handed: everything this unit puts in scope, and nothing else.
    pub fn globals(&self) -> Globals<'_> {
        let mut globals = Globals::default();
        globals.mount(Filed {
            definitions: &self.definitions,
            places: &self.places,
            certification: &self.certification,
        });

        globals
    }
}

/// Everything `module` puts in scope as a walk over it with nothing in scope files it: the module as the walk stated it, beside the record the walk filed, as a unit a build made of it holds them. A fixture the walk refuses is a fixture bug.
pub fn already_judged(definitions: &Definitions) -> AlreadyJudged {
    let Rechecked::Accepted(accepted) = walked(definitions) else {
        unreachable!("`walked` holds the walk to have accepted the module");
    };
    let (definitions, certification) = accepted.into_parts();

    AlreadyJudged {
        places: Places::of(&definitions),
        definitions,
        certification,
    }
}

/// `induct Shadowed : Type 0 end` beside `let shadowed : Nat = 0` — the two entries an earlier walk is taken to have judged.
pub const JUDGED_ENVIRONMENT: &str = r#"
induct /Shadowed: pub Type
end

let /shadowed: #NatType =
    0;
"#;

/// [`JUDGED_ENVIRONMENT`]'s module.
pub fn judged_environment() -> Definitions {
    core_text_module(JUDGED_ENVIRONMENT)
}

pub trait Walked {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>);
}

impl Walked for Definitions {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>) {
        (self, None)
    }
}

impl Walked for Program {
    fn parts(&self) -> (&Definitions, Option<&Entrypoint>) {
        (&self.definitions, Some(&self.entrypoint))
    }
}

/// The whole-module walk over a fixture: the modules built here are adversarial by design, some carrying what only elaboration leaves, and the certifier's own refusals are the thing under test.
pub fn fixture_refusals(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Refusal> {
    fixture_certified(fixture, budget, globals, syntax).into_refusals()
}

/// [`fixture_refusals`], with the record the walk leaves — for a fixture that mounts what one walk concluded beneath another.
pub fn fixture_certified(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Rechecked<Definitions> {
    let (definitions, entry) = fixture.parts();

    Put {
        scope: None,
        globals: Some(globals),
        syntax,
        budget,
    }
    .module(definitions, entry)
}

/// The door this crate's suites put a candidate through, as the soundness board puts its tickets: nothing in scope, the registry they judge under, and their budget.
pub fn put() -> Put<'static> {
    Put {
        scope: None,
        globals: None,
        syntax: SYNTAX,
        budget: BUDGET,
    }
}

pub const BUDGET: u64 = 1_000_000;

pub fn global(symbol: &str) -> Global {
    Global::Authored(Qualifier::from([symbol]))
}

/// `alias : Type = ${alias}`, `three : alias = 3`, `identity : (Nat) -> Nat = (x) => ${returned}`, and `five : Nat = identity(5)`: `three` unfolds `alias`, and `five` types `identity` and unfolds nothing.
pub const READING: &str = r#"
let /alias: Type =
    ${alias};

let /three: /alias =
    3;

let /identity: (x: #NatType) -> #NatType =
    (x: #NatType) => ${returned};

let /five: #NatType =
    /identity(5);
"#;

/// [`READING`]'s module at `alias` and `returned`.
pub fn reading_definitions(alias: &str, returned: &str) -> Definitions {
    core_text_module(&fill!(READING, alias, returned))
}

/// The module every fixture here is an edit of: `alias` is `Nat` and `identity` returns its argument.
pub fn written() -> Definitions {
    reading_definitions("#NatType", "x")
}

/// The propositions a module written as Core text here uses, declared where the registry this crate's suites judge under names them: `False`, `True` and `Holds`, the reflection of a decision a bound states.
pub const PROPOSITIONS: &str = r#"
induct /std/Bool/False: pub Prop
end

induct /std/Bool/True: pub Prop
| qed()
end

let /std/Bool/Holds : (b: #BoolType) -> Prop =
    (b: #BoolType) =>
        #{goal = Prop}
        match b
        | false =>
            #{node = InductType}
            /std/Bool/False()
        | true =>
            #{node = InductType}
            /std/Bool/True()
        end;
"#;

/// What `text` writes, Core text read with nothing in scope: the module, and the term it closes with where it closes with one.
fn core_text_read(text: &str) -> (Definitions, Option<Entrypoint>) {
    put()
        .read(text)
        .unwrap_or_else(|unread| panic!("{unread:?}"))
}

/// A family of one case over one `Nat`, `F` with `mk(x : Nat)`, and `held : T = 3` whose type eliminates `mk(3)` through the arm `${arm}`.
pub const ARM: &str = r#"
induct /F: pub Type
| mk(x: #NatType)
end

let /held: #{goal = Type} match #{node = Variant} /F/mk(3) | ${arm} => #NatType end =
    3;
"#;

/// What the walk says of `text`, Core text read with nothing in scope and walked as it closes: a module, or a program at the type it states.
pub fn text_certified(text: &str) -> Rechecked<Definitions> {
    text_certified_over(text, &Globals::default())
}

/// [`text_certified`] over the units `globals` holds.
pub fn text_certified_over(text: &str, globals: &Globals) -> Rechecked<Definitions> {
    let (definitions, entry) = core_text_read(text);

    Put {
        globals: Some(globals),
        ..put()
    }
    .module(&definitions, entry.as_ref())
}

/// The verdicts of [`text_certified`].
pub fn text_refusals(text: &str) -> Vec<Refusal> {
    text_certified(text).into_refusals()
}

/// The verdicts of [`text_certified_over`].
pub fn text_refusals_over(text: &str, globals: &Globals) -> Vec<Refusal> {
    text_certified_over(text, globals).into_refusals()
}

/// [`text_certified`] with each judgment restored to `budget`.
pub fn text_certified_within(text: &str, budget: u64) -> Rechecked<Definitions> {
    let (definitions, entry) = core_text_read(text);

    Put { budget, ..put() }.module(&definitions, entry.as_ref())
}

/// Whether `verdicts` refuse the declaration named `name` at the root, or the closing term where it is `None`, by an error `refusal` admits.
pub fn refuses(verdicts: &[Refusal], name: Option<&str>, refusal: impl Fn(&Error) -> bool) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name == name.map(global) && refusal(&verdict.error))
}

/// A count refused as `counted`, `actual` where `expected` was.
pub fn miscounted(counted: Counted, expected: usize, actual: usize) -> impl Fn(&Error) -> bool {
    move |error| {
        matches!(error, Error::Arity { counted: found, expected: e, actual: a }
            if *found == counted && *e == expected && *a == actual)
    }
}

/// The module `text` writes, read with nothing in scope and closing with no term: an environment another text is put over.
pub fn core_text_module(text: &str) -> Definitions {
    let (definitions, entry) = core_text_read(text);
    assert!(entry.is_none(), "an environment closes with no term");

    definitions
}

/// What the certifier says of `text`, Core text read with nothing in scope and closing with a term it states no type for, put at `False` as the registry this crate's suites judge under names it, over `globals`.
pub fn core_text_refusals_over(text: &str, globals: &Globals) -> Vec<Refusal> {
    Put {
        globals: Some(globals),
        ..put()
    }
    .core_text(text)
    .unwrap_or_else(|unread| panic!("{unread:?}"))
    .into_refusals()
}

/// What the certifier says of `text`, Core text read with nothing in scope and closing with a term it states no type for, put at `False` as the registry this crate's suites judge under names it: how a module built by hand is written, declaring every name it uses.
pub fn core_text_refusals(text: &str) -> Vec<Refusal> {
    core_text_refusals_over(text, &Globals::default())
}

/// `module` walked with nothing in scope and no baseline: the walk that files a baseline's record.
pub fn walked(definitions: &Definitions) -> Rechecked<Definitions> {
    let rechecked = fixture_certified(definitions, BUDGET, &Globals::default(), SYNTAX);
    assert_eq!(rechecked.refusals(), Vec::new());

    rechecked
}

/// What a suite reads of a walk it holds to have accepted its module, each a refusal where the walk refused it.
pub trait Answered {
    /// What the walk concluded about the module it accepted.
    fn certification(&self) -> &Certification;

    /// The module the walk accepted, as it stated it.
    fn definitions(&self) -> &Definitions;
}

impl Answered for Rechecked<Definitions> {
    fn certification(&self) -> &Certification {
        self.accepted()
            .unwrap_or_else(|| panic!("the walk refused the module: {:?}", self.refusals()))
            .certification()
    }

    fn definitions(&self) -> &Definitions {
        self.accepted()
            .unwrap_or_else(|| panic!("the walk refused the module: {:?}", self.refusals()))
            .as_definitions()
    }
}

/// `module` walked over `baseline` as `certification` records it.
pub fn walked_over(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
) -> Rechecked<Definitions> {
    walked_among(definitions, baseline, certification, None)
}

/// [`walked_over`], taking what it can from what `store` holds.
pub fn walked_among(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
    store: Option<&dyn Candidates>,
) -> Rechecked<Definitions> {
    put().over(definitions, baseline, certification, store)
}

/// `certification` with `symbol`'s entry replaced by `entry`'s answer for it.
pub fn doctored(
    certification: &Certification,
    symbol: &str,
    entry: impl Fn(Certified) -> Certified,
) -> Certification {
    Certification::of(
        certification.iter().map(|(name, totality)| {
            let certified = Certified {
                totality,
                applications: certification
                    .applications(name)
                    .cloned()
                    .unwrap_or_default(),
                reads: certification
                    .reads(name)
                    .cloned()
                    .expect("a record holds the reads of every definition it classifies"),
            };

            match *name == global(symbol) {
                true => (*name, entry(certified)),
                false => (*name, certified),
            }
        }),
        [],
        [],
        [],
    )
}

/// A record of `written` in which `five` is marked: its reads hold `alias`'s signature, which no judgment of `five` reads. A walk that hands the mark back adopted the entry, and one that judged `five` filed its own reads in its place.
pub fn marked() -> Certification {
    doctored(walked(&written()).certification(), "five", |certified| {
        Certified {
            reads: Reads {
                signatures: BTreeSet::from([global("identity"), global("alias")]),
                bodies: BTreeSet::new(),
            },
            ..certified
        }
    })
}

/// `module` with its items held as a version's: a module made of it by replacing an item shares every other item with it, as a recompile's module shares the ones it kept with its baseline's.
pub fn versioned(definitions: Definitions) -> Definitions {
    Definitions {
        items: definitions.items.version(),
    }
}

/// `module`, a version, with the item declaring `name` replaced by `item`.
pub fn replaced(definitions: &Definitions, name: &str, item: Item) -> Definitions {
    let place = Places::of(definitions)
        .place(&global(name))
        .expect("the module declares the name")
        .item();
    let mut items = definitions.items.clone();
    items.replace(place, item);

    Definitions { items }
}

/// `Type ${level}`, a universe at a constant level.
pub fn type_at(level: u32) -> Term {
    term(&fill!("#{level = ${level}} Type", level))
}

/// Three aliases in a chain at `Type 5`, each saying the sort its value inhabits: `H = ${h_body}` saying `${h}`, `K = H` saying `${k}`, and `L = K` saying `${l}`.
///
/// The walk states `L`'s value sort by `Sort::of(K)`, which reads `K`'s *published* sort with nothing run, so what it states of `L` is a function of what it stated of `K` and of nothing else about `K`, and nothing the module says of a sort.
pub const CHAINED_SORTS: &str = r#"
#{value_sort = ${h}}
let /H: #{level = 5} Type =
    ${h_body};

#{value_sort = ${k}}
let /K: #{level = 5} Type =
    /H;

#{value_sort = ${l}}
let /L: #{level = 5} Type =
    /K;
"#;

/// [`CHAINED_SORTS`]'s module at `h_body`, `h`, `k` and `l`.
pub fn chained_sort_definitions(h_body: &str, h: &str, k: &str, l: &str) -> Definitions {
    core_text_module(&fill!(CHAINED_SORTS, h_body, h, k, l))
}

/// The item `text` declares `symbol` by, read with nothing in scope: an edit [`replaced`] puts in a version, the text declaring beside it whatever its forms name.
pub fn declared_item(text: &str, symbol: &str) -> Item {
    core_text_module(text)
        .items
        .iter()
        .find(|item| item.declared_names().contains(&global(symbol)))
        .cloned()
        .unwrap_or_else(|| panic!("the text declares `{symbol}`"))
}
