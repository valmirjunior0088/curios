//! The hand-built adversarial modules the recheck suites forge, and the declarations they forge them from.
//!
//! Built by hand because a refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program. Reaching it means constructing the finished module here and asking `certify_definitions` directly.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `recheck`, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::SYNTAX,
    curios_common::{Candidates, Qualifier, SyntaxRegistry, fill},
    curios_core::{
        Atom, Certification, Certified, Definition, DefinitionKind, Definitions, Entrypoint, Free,
        Global, InductDecl, InductParam, Intrinsic, Item, Level, Nat, Places, Polarity, Program,
        Reads, RecGroup, StructDecl, Telescope, Term, UniverseConstraint, UniverseContext,
        UniverseParam, Universes, Variance, test_support::*,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A top-level definition, as `certify_definitions` binds one.
pub fn authored(name: &Global, type_: Term, body: Term) -> Item {
    Item::Let(Box::new(Definition {
        name: *name,
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_,
        body,
    }))
}

/// [`authored`] marked a proof, as the module a walk hands back marks one its declaration wrote at a proposition: what a unit in scope holds.
pub fn authored_proof(name: &Global, type_: Term, body: Term) -> Item {
    match authored(name, type_, body) {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            proof: true,
            ..*definition
        })),
        item => item,
    }
}

/// A nullary `Prop`-sorted family: `False` itself, and the shape `Box` takes but for its payload.
pub fn proposition(constructors: Vec<(Atom, InductParam)>) -> InductDecl {
    InductDecl::new(
        UniverseContext::default(),
        Telescope::done(Telescope::done(())),
        constructors.into(),
        Term::prop(),
        Qualifier::default(),
        true,
    )
}

/// `sink : (Nat) -> Nat`, honestly stamped `Partial`: its body projects `rec f : (Nat) -> Nat = (n) => f(n)`, which does not descend.
pub fn diverging_sink() -> Item {
    diverging("sink")
}

/// [`diverging_sink`] under another name.
pub fn diverging(name: &str) -> Item {
    let f = Free::local(920, Some("f"));
    let n = Free::local(921, Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);

    let group = RecGroup::closing(vec![(
        f,
        Term::func_type([(n, nat.clone())], nat.clone()),
        Term::func(
            [(n, nat.clone())],
            Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
        ),
    )]);

    authored(
        &Global::Authored(Qualifier::from([name])),
        Term::func_type([(Free::local(922, Some("n")), nat.clone())], nat),
        Term::rec_proj(group, 0),
    )
}

/// The scheme both universe-occurrence fixtures below are built on: `def A<u, v | u + 1 <= v> : Type v = Type u`.
///
/// Its constraint is load-bearing rather than decorative, which is the whole point of the shape: checking the body needs `Type (u + 1) <= Type v`, and that holds under this hypothesis and under nothing else. So an occurrence of `A` is legitimate exactly where it discharges the constraint at its own levels, and [`Certifier::check_instance`](crate::Certifier) is what does that.
pub fn scheme_context() -> UniverseContext {
    let ordered = UniverseConstraint {
        lower: Level::param(UniverseParam(0))
            .checked_add(1)
            .expect("level admits the offset"),
        upper: Level::param(UniverseParam(1)),
    };

    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![ordered],
    }
}

/// The same two parameters with the scheme's constraint *not* declared — the context a user of `A` stands in when it has assumed nothing about its own levels.
pub fn open_context() -> UniverseContext {
    UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: Vec::new(),
    }
}

/// The scheme above, followed by one `user` that spells an occurrence of it.
///
/// `user` is `(its own universe context, the term it is defined as)`, and its declared type is `Type v` throughout — so the two spellings of the occurrence differ in nothing but whether the instance is stated.
pub fn universe_scheme_definitions(user: Option<(UniverseContext, Term)>) -> Definitions {
    let scheme = Definition {
        name: Global::Authored(Qualifier::from(["A"])),
        kind: DefinitionKind::Authored,
        universe_context: scheme_context(),
        island: Qualifier::default(),
        proof: false,
        value_sort: None,
        type_: Term::type_at(Level::param(UniverseParam(1))),
        body: Term::type_at(Level::param(UniverseParam(0))),
    };

    let mut items = vec![Item::Let(Box::new(scheme))];
    if let Some((universe_context, body)) = user {
        items.push(Item::Let(Box::new(Definition {
            name: Global::Authored(Qualifier::from(["user"])),
            kind: DefinitionKind::Authored,
            universe_context,
            island: Qualifier::default(),
            proof: false,
            value_sort: None,
            type_: Term::type_at(Level::param(UniverseParam(1))),
            body,
        })));
    }

    Definitions {
        items: items.into(),
    }
}

/// `A` alone, and `A` at the using item's own two parameters — the two spellings of one occurrence.
pub fn scheme_occurrences() -> (Term, Term) {
    let name = Global::Authored(Qualifier::from(["A"]));
    let bare = Term::instance(name, Universes::default());
    let instance = Term::instance(
        name,
        vec![
            Level::param(UniverseParam(0)),
            Level::param(UniverseParam(1)),
        ],
    );

    (bare, instance)
}

/// A unit a test puts in scope by hand: a module no unit holds, with the places an environment reads it by and the record filed beside it.
pub struct AlreadyJudged {
    definitions: Definitions,
    places: Places,
    certification: Certification,
}

impl AlreadyJudged {
    /// `module` in scope with `certification` as the record of a walk over it.
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
        globals.mount(&self.definitions, &self.places, &self.certification);

        globals
    }
}

/// Everything `module` puts in scope, with an empty record — so a walk classifies its items' totality for itself.
pub fn already_judged(definitions: &Definitions) -> AlreadyJudged {
    AlreadyJudged::with(definitions, &Certification::default())
}

/// A module carrying `items` and `induct_decls` and nothing else.
pub fn collision_definitions(
    items: Vec<Item>,
    induct_decls: BTreeMap<Global, InductDecl>,
) -> Definitions {
    declaring(
        Definitions {
            items: items.into(),
        },
        induct_decls,
        [],
        [],
    )
}

/// The definition name the environment and the module both declare.
pub fn shadowed_name() -> Global {
    Global::Authored(Qualifier::from(["shadowed"]))
}

/// The family name the environment and the module both declare.
pub fn shadowed_family() -> Global {
    Global::Authored(Qualifier::from(["Shadowed"]))
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

/// The record [`two_registry_entries`] declares beside [`shadowed_family`].
pub fn kept_record() -> Global {
    Global::Authored(Qualifier::from(["Kept"]))
}

/// What [`two_registry_entries`] varies of the two entries it declares.
pub enum Varied {
    /// Nothing: the entries as an environment holds them.
    Nothing,
    /// Elaboration's stamps on both: a polarity vector, the owning module and the representation's privacy.
    Stamps,
    /// The family's variance vector.
    Variance,
    /// The type of the record's field.
    Field,
}

/// `induct Shadowed : Type 0 end` and `struct Kept { held : Nat }`, as registry entries with no items lowering them, differing from the unvaried pair in what `varied` names.
pub fn two_registry_entries(varied: Varied) -> Definitions {
    let stamped = matches!(varied, Varied::Stamps);
    let polarities = || match stamped {
        true => vec![Polarity::Mixed],
        false => Vec::new(),
    };
    let owner = || match stamped {
        true => Qualifier::from(["elsewhere"]),
        false => Qualifier::default(),
    };

    let family = InductDecl::new(
        UniverseContext::default(),
        Telescope::done(Telescope::done(())),
        Vec::new().into(),
        Term::type_ground(),
        owner(),
        !stamped,
    )
    .with_polarities(polarities())
    .with_variances(match varied {
        Varied::Variance => vec![Variance::Irrelevant],
        _ => Vec::new(),
    });
    let record = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(940, Some("held")),
                Term::intrinsic(match varied {
                    Varied::Field => Intrinsic::BoolType,
                    _ => Intrinsic::NatType,
                }),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: owner(),
        rep_public: !stamped,
        polarities: polarities(),
        variances: Vec::new(),
    };

    declaring(
        Definitions {
            ..collision_definitions(Vec::new(), BTreeMap::from([(shadowed_family(), family)]))
        },
        [],
        BTreeMap::from([(kept_record(), record)]),
        [],
    )
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
pub fn fixture_verdicts(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Vec<Verdict> {
    fixture_certified(fixture, budget, globals, syntax).verdicts
}

/// [`fixture_verdicts`], with the record the walk leaves — for a fixture that mounts what one walk concluded beneath another.
pub fn fixture_certified(
    fixture: &impl Walked,
    budget: u64,
    globals: &Globals,
    syntax: SyntaxRegistry,
) -> Rechecked {
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

pub fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub fn literal(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
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
        #{result = Ambient(Prop)}
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

let /held: match #{node = Variant} /F/mk(3): (s) => Type | ${arm} => #NatType end =
    3;
"#;

/// What the walk says of `text`, Core text read with nothing in scope and walked as it closes: a module, or a program at the type it states.
pub fn text_certified(text: &str) -> Rechecked {
    text_certified_over(text, &Globals::default())
}

/// [`text_certified`] over the units `globals` holds.
pub fn text_certified_over(text: &str, globals: &Globals) -> Rechecked {
    let (definitions, entry) = core_text_read(text);

    Put {
        globals: Some(globals),
        ..put()
    }
    .module(&definitions, entry.as_ref())
}

/// The verdicts of [`text_certified`].
pub fn text_verdicts(text: &str) -> Vec<Verdict> {
    text_certified(text).verdicts
}

/// The verdicts of [`text_certified_over`].
pub fn text_verdicts_over(text: &str, globals: &Globals) -> Vec<Verdict> {
    text_certified_over(text, globals).verdicts
}

/// [`text_certified`] with each judgment restored to `budget`.
pub fn text_certified_within(text: &str, budget: u64) -> Rechecked {
    let (definitions, entry) = core_text_read(text);

    Put { budget, ..put() }.module(&definitions, entry.as_ref())
}

/// Whether `verdicts` refuse the declaration named `name` at the root, or the closing term where it is `None`, by an error `refusal` admits.
pub fn refuses(verdicts: &[Verdict], name: Option<&str>, refusal: impl Fn(&Error) -> bool) -> bool {
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
pub fn core_text_verdicts_over(text: &str, globals: &Globals) -> Vec<Verdict> {
    Put {
        globals: Some(globals),
        ..put()
    }
    .core_text(text)
    .unwrap_or_else(|unread| panic!("{unread:?}"))
    .verdicts
}

/// What the certifier says of `text`, Core text read with nothing in scope and closing with a term it states no type for, put at `False` as the registry this crate's suites judge under names it: how a module built by hand is written, declaring every name it uses.
pub fn core_text_verdicts(text: &str) -> Vec<Verdict> {
    core_text_verdicts_over(text, &Globals::default())
}

/// `module` walked with nothing in scope and no baseline: the walk that files a baseline's record.
pub fn walked(definitions: &Definitions) -> Rechecked {
    let rechecked = fixture_certified(definitions, BUDGET, &Globals::default(), SYNTAX);
    assert_eq!(rechecked.verdicts, Vec::new());

    rechecked
}

/// `module` walked over `baseline` as `certification` records it.
pub fn walked_over(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
) -> Rechecked {
    walked_among(definitions, baseline, certification, None)
}

/// [`walked_over`], taking what it can from what `store` holds.
pub fn walked_among(
    definitions: &Definitions,
    baseline: &Definitions,
    certification: &Certification,
    store: Option<&dyn Candidates>,
) -> Rechecked {
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
    doctored(&walked(&written()).certification, "five", |certified| {
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

/// `Type 5`, `Type 4` and `Type 0` as hand-built universes.
pub fn type_at(level: u32) -> Term {
    Term::type_at(Level::constant(level))
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
