//! The contexts: one for each child a term former holds, so that every seed is stated under every position conversion compares a child at.
//!
//! A context is a term with a hole. Conversion is a congruence, so a seed that holds, holds under each of them, and each of them keeps its hole — nothing here discards what fills it — so a near miss stays one. A row under a context states no side of its own: its side is its seed's, and a checker that puts it on the other is listed in the table of parted rows.
//!
//! **Where a hole lands is read back.** A context names the former its hole sits directly under, and `every_context_places_its_hole_under_the_former_it_names` reads that off the term the elaborator builds for it around a marker. [`former`] names a former by a `match` over `Subterm` with no wildcard, and over `Cases` beneath it, so a former added to Core does not compile here until it says whether a context holds a hole under it. What the lint holds is the former and the kind of elimination; which of a former's children a context's hole is, is the context's to state.

use {
    super::{Answers, Audit, SEEDS, STRUCTURAL, asked_alone, claim, hold_to_the_table},
    curios_core::{Bound, Carrier, Cases, Free, Global, Item, Match, Subterm, Term, Visit},
    curios_pipeline::{DEFAULT_STEP_BUDGET, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_utilities::{Qualifier, recurse},
};

/// The former a hole sits directly under, in the term the elaborator builds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Former {
    Apply,
    Func,
    FuncType,
    Tuple,
    TupleType,
    Struct,
    Proj,
    Let,
    Intrinsic,
    BoolMatch,
    NatMatch,
    LiteralMatch,
    ListMatch,
    FamilyMatch,
}

/// The former `parent` is, where a context holds a hole under it, named with no wildcard so that a former added to Core is placed here before this compiles.
fn former(parent: &Subterm) -> Option<Former> {
    match parent {
        Subterm::Apply(_) => Some(Former::Apply),
        Subterm::Func(_) => Some(Former::Func),
        Subterm::FuncType(_) => Some(Former::FuncType),
        Subterm::Tuple(_) => Some(Former::Tuple),
        Subterm::TupleType(_) => Some(Former::TupleType),
        Subterm::Struct(_) => Some(Former::Struct),
        Subterm::Proj(_) => Some(Former::Proj),
        Subterm::Let(_) => Some(Former::Let),
        Subterm::Intrinsic(_) => Some(Former::Intrinsic),
        Subterm::Match(Match { cases, .. }) => match cases {
            Cases::Bool { .. } => Some(Former::BoolMatch),
            Cases::Switch { .. } => Some(Former::LiteralMatch),
            Cases::Induct { .. } => Some(Former::FamilyMatch),
            Cases::FreeMonoid { carrier } => match carrier {
                Carrier::Nat { .. } => Some(Former::NatMatch),
                Carrier::List { .. } => Some(Former::ListMatch),
                // A packed word's fold. No seed is a word, and the words' own laws are the grid's rows.
                Carrier::Bin { .. } => None,
            },
        },
        // Written as a call of its former or its constructor, which reduction rebuilds as this node: the contexts under an application are the ones that reach it.
        Subterm::InductType(_) | Subterm::StructType(_) | Subterm::Variant(_) => None,
        // A local recursive group and a host call: `tests::recursion` and `tests::host` hold what conversion makes of them.
        Subterm::Rec(_) | Subterm::Foreign(..) => None,
        // No term beneath it.
        Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) | Subterm::Instance(_) => None,
        // No part of a finished term.
        Subterm::Metavar(_) | Subterm::Transient(_) => None,
    }
}

/// A term with a hole, as source: `{T}` stands for the type of what fills the hole, `{hole}` for what fills it, and `{rest}` for another inhabitant of that type, the same on both sides of a row.
pub(super) struct Context {
    pub(super) name: &'static str,
    under: Former,
    /// The binders it adds to a seed's.
    binders: &'static str,
    /// The type of the term around the hole.
    type_: &'static str,
    around: &'static str,
    /// The seed types its hole takes: every type, where empty.
    only: &'static [&'static str],
}

const fn any(
    name: &'static str,
    under: Former,
    binders: &'static str,
    type_: &'static str,
    around: &'static str,
) -> Context {
    at(&[], name, under, binders, type_, around)
}

const fn at(
    only: &'static [&'static str],
    name: &'static str,
    under: Former,
    binders: &'static str,
    type_: &'static str,
    around: &'static str,
) -> Context {
    Context {
        name,
        under,
        binders,
        type_,
        around,
        only,
    }
}

const FUNCTION: &[&str] = &["(Nat) -> Nat"];
const RECORD: &[&str] = &["{Nat, Nat}"];
const NUMBER: &[&str] = &["Nat"];
const TYPE: &[&str] = &["Type"];

/// A hole where each checker has the type to compare it at: an argument, a payload, a field, a component, an element, a body.
pub(super) const TYPED: &[Context] = &[
    any(
        "a variable's argument",
        Former::Apply,
        "on: ({T}) -> Nat",
        "Nat",
        "on({hole})",
    ),
    any(
        "a definition's argument",
        Former::Apply,
        "count: Nat",
        "{T}",
        "hold({hole}, count)",
    ),
    any(
        "a recursive call's argument",
        Former::Apply,
        "count: Nat",
        "{T}",
        "carry(count, {hole})",
    ),
    any(
        "a constructor's payload",
        Former::Apply,
        "",
        "Option({T})",
        "Option/some({hole})",
    ),
    any(
        "a struct's field",
        Former::Struct,
        "",
        "Box({T})",
        "Box({T}) { held = {hole} }",
    ),
    any(
        "a tuple's component",
        Former::Tuple,
        "",
        "{{T}, Nat}",
        "({hole}, 0)",
    ),
    any(
        "a list's element",
        Former::Intrinsic,
        "",
        "List({T})",
        "[{hole}]",
    ),
    any(
        "a lambda's body",
        Former::Func,
        "",
        "(Nat) -> {T}",
        "(z: Nat) => {hole}",
    ),
    any(
        "a let's value",
        Former::Let,
        "",
        "{T}",
        "(let z = {hole}; z)",
    ),
];

/// A hole in an arm of a stuck elimination, which the kernel compares at `Type`.
pub(super) const ARMS: &[Context] = &[
    any(
        "an arm of a Bool match",
        Former::BoolMatch,
        "flag: Bool",
        "{T}",
        "match flag: (_) => {T} | true => {hole} | false => {rest} end",
    ),
    any(
        "the zero arm of a Nat match",
        Former::NatMatch,
        "count: Nat",
        "{T}",
        "match count: (_) => {T} | 0 => {hole} | pred + 1 => {rest} end",
    ),
    any(
        "the successor arm of a Nat match",
        Former::NatMatch,
        "count: Nat",
        "{T}",
        "match count: (_) => {T} | 0 => {rest} | pred + 1 => {hole} end",
    ),
    any(
        "an arm of a literal match",
        Former::LiteralMatch,
        "count: Nat",
        "{T}",
        "match count: (_) => {T} | 3 => {hole} | _ => {rest} end",
    ),
    any(
        "the default of a literal match",
        Former::LiteralMatch,
        "count: Nat",
        "{T}",
        "match count: (_) => {T} | 3 => {rest} | _ => {hole} end",
    ),
    any(
        "the empty arm of a List match",
        Former::ListMatch,
        "items: List(Nat)",
        "{T}",
        "match items: (_) => {T} | [] => {hole} | [head, ..tail] => {rest} end",
    ),
    any(
        "the cons arm of a List match",
        Former::ListMatch,
        "items: List(Nat)",
        "{T}",
        "match items: (_) => {T} | [] => {rest} | [head, ..tail] => {hole} end",
    ),
    any(
        "an arm of a family's match",
        Former::FamilyMatch,
        "maybe: Option(Nat)",
        "{T}",
        "match maybe: (_) => {T} | some(got) => {hole} | none() => {rest} end",
    ),
    any(
        "the default of a family's match",
        Former::FamilyMatch,
        "maybe: Option(Nat)",
        "{T}",
        "match maybe: (_) => {T} | some(got) => {rest} | _ => {hole} end",
    ),
];

/// A hole that is what its former takes apart or computes on: a head, a scrutinee, an operand.
const HEADS: &[Context] = &[
    at(
        RECORD,
        "a projection's head",
        Former::Proj,
        "",
        "Nat",
        "({hole}).0",
    ),
    at(
        FUNCTION,
        "an application's head",
        Former::Apply,
        "",
        "Nat",
        "({hole})(7)",
    ),
    at(
        NUMBER,
        "a match's scrutinee",
        Former::NatMatch,
        "",
        "Nat",
        "match {hole}: (_) => Nat | 0 => 0 | pred + 1 => 1 end",
    ),
    at(
        NUMBER,
        "an operation's operand",
        Former::Apply,
        "",
        "Nat",
        "({hole}) + 1",
    ),
];

/// A hole that is a type, in each place a type is written.
pub(super) const TYPES: &[Context] = &[
    at(
        TYPE,
        "a function type's domain",
        Former::FuncType,
        "",
        "Type",
        "({hole}) -> Nat",
    ),
    at(
        TYPE,
        "a function type's codomain",
        Former::FuncType,
        "",
        "Type",
        "(z: Nat) -> {hole}",
    ),
    at(
        TYPE,
        "a tuple type's field",
        Former::TupleType,
        "",
        "Type",
        "{{hole}, Nat}",
    ),
    at(
        TYPE,
        "a family's parameter",
        Former::Apply,
        "",
        "Type",
        "Option({hole})",
    ),
    at(
        TYPE,
        "a struct type's parameter",
        Former::Apply,
        "",
        "Type",
        "Box({hole})",
    ),
    at(
        TYPE,
        "a list type's element",
        Former::Apply,
        "",
        "Type",
        "List({hole})",
    ),
];

impl Context {
    /// A template with its type, its hole and its other inhabitant filled in.
    fn fill(template: &str, type_: &str, hole: &str, rest: &str) -> String {
        template
            .replace("{T}", type_)
            .replace("{hole}", hole)
            .replace("{rest}", rest)
    }

    fn takes(&self, type_: &str) -> bool {
        self.only.is_empty() || self.only.contains(&type_)
    }
}

/// Every seed this context's hole takes, under it: the row's name, the row, and whether its seed holds.
///
/// A hole that is a type takes every seed, as the argument of a family over the seed's type: `on(left)` against `on(right)`. A seed that was itself a type would write a `Type` on each side, each settling a level of its own where the elaborator is not asked to compare them, and a row keeps its levels out of what it asks: a level is a question the elaborator solves and the kernel judges.
fn rows_under(context: &Context) -> Vec<(String, (String, String), bool)> {
    let lifts = context.only == TYPE;
    SEEDS
        .iter()
        .filter(|seeds| lifts || context.takes(seeds.type_))
        .flat_map(|seeds| {
            seeds.seeds.iter().map(move |seed| {
                let (type_, left, right, family) = match lifts {
                    true => (
                        "Type",
                        format!("on({})", seed.left),
                        format!("on({})", seed.right),
                        format!("on: ({}) -> Type", seeds.type_),
                    ),
                    false => (
                        seeds.type_,
                        seed.left.to_owned(),
                        seed.right.to_owned(),
                        String::new(),
                    ),
                };
                let fill =
                    |template: &str, hole: &str| Context::fill(template, type_, hole, &right);
                let binders = [
                    seeds.binders.to_owned(),
                    family,
                    fill(context.binders, &left),
                ]
                .into_iter()
                .filter(|binders| !binders.is_empty())
                .collect::<Vec<_>>()
                .join(", ");
                (
                    format!("{} under {}", seed.rule, context.name),
                    (
                        binders,
                        claim(
                            &fill(context.type_, &left),
                            &fill(context.around, &left),
                            &fill(context.around, &right),
                        ),
                    ),
                    seed.holds,
                )
            })
        })
        .collect()
}

/// Hold every seed to its own side under each of `contexts`, in both checkers asked alone, but for the rows the table lists.
fn keeps_its_side_under(audit: Audit, contexts: &[Context]) {
    let found = contexts
        .iter()
        .flat_map(|context| {
            let stated = rows_under(context);
            let rows = stated
                .iter()
                .map(|(_, row, _)| row.clone())
                .collect::<Vec<_>>();
            let answers = asked_alone(STRUCTURAL, context.name, &rows);
            stated
                .into_iter()
                .zip(answers)
                .filter(|((_, _, holds), answers)| *answers != Answers::both(*holds))
                .map(|((name, _, _), answers)| (name, answers))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    hold_to_the_table(audit, &found);
}

#[test]
fn every_seed_keeps_its_side_where_a_child_is_typed() {
    keeps_its_side_under(Audit::Typed, TYPED);
}

#[test]
fn every_seed_keeps_its_side_in_an_arm() {
    keeps_its_side_under(Audit::Arms, ARMS);
}

#[test]
fn every_seed_keeps_its_side_at_a_head() {
    keeps_its_side_under(Audit::Heads, HEADS);
}

#[test]
fn every_seed_keeps_its_side_in_a_type() {
    keeps_its_side_under(Audit::Types, TYPES);
}

/// The marker a context is read back around, by the type its hole takes. Each is a top-level name, so it stands as one free variable wherever the elaborator leaves it, whatever binders it sits under.
const MARKERS: &[(&str, &str)] = &[
    ("Nat", "marker_number"),
    ("(Nat) -> Nat", "marker_function"),
    ("{Nat, Nat}", "marker_record"),
    ("Type", "Marker"),
];

const MARKED: &str = "let marker_number: Nat = 0;
let marker_function(x: Nat) -> Nat = x;
let marker_record: {Nat, Nat} = (0, 0);
let Marker: Type = Nat;";

/// Every node of `term` that holds `marker` as a child.
fn parents_of(term: &Term, marker: &Term, parents: &mut Vec<Term>) {
    recurse(|| {
        for child in children_of(term) {
            if child == *marker {
                parents.push(term.clone());
            }
            parents_of(&child, marker, parents);
        }
    })
}

/// The terms `term` holds directly, as Core's own masking stands them down.
fn children_of(term: &Term) -> Vec<Term> {
    let mut visit = Visit::masking(|_, _| None, Term::from(Subterm::Prop));
    let _ = (**term).traverse(&mut visit);
    visit.take_masked_children()
}

#[test]
fn every_context_places_its_hole_under_the_former_it_names() {
    // Mutation-checked: a list type's element declared under an intrinsic is named with the application of `/sys/List/List` it landed under.
    let contexts = TYPED
        .iter()
        .chain(ARMS)
        .chain(HEADS)
        .chain(TYPES)
        .collect::<Vec<_>>();
    let marked = |context: &Context| {
        *MARKERS
            .iter()
            .find(|(type_, _)| context.takes(type_))
            .expect("a context takes a type a marker has")
    };
    let items = contexts
        .iter()
        .enumerate()
        .map(|(index, context)| {
            let (type_, marker) = marked(context);
            let fill = |template: &str| Context::fill(template, type_, marker, marker);
            format!(
                "let probe{index}({}) -> {} = {};",
                fill(context.binders),
                fill(context.type_),
                fill(context.around)
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    let entrypoint = format!("{STRUCTURAL}\n{MARKED}\n{items}\nIo/pure(())")
        .parse::<Entrypoint>()
        .expect("the probes parse");
    let program = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
        .unwrap_or_else(|refused| {
            panic!("a context does not elaborate around its marker:\n{refused}")
        })
        .program;
    let named = |name: String| Global::Authored(Qualifier::from([name]));

    let placed = contexts
        .iter()
        .map(|context| context.under)
        .collect::<Vec<_>>();
    for (index, context) in contexts.iter().enumerate() {
        let name = named(format!("probe{index}"));
        let body = program
            .module
            .items
            .iter()
            .find_map(|item| match item {
                Item::Let(definition) if definition.name == name => Some(&definition.body),
                _ => None,
            })
            .unwrap_or_else(|| panic!("the program defines {name}"));
        let marker = Term::free_var(&Free::from(&named(marked(context).1.to_owned())));

        let mut parents = Vec::new();
        parents_of(body, &marker, &mut parents);
        assert!(
            !parents.is_empty(),
            "`{}`: the elaborator left its marker nowhere in {body}",
            context.name
        );
        for parent in &parents {
            assert_eq!(
                former(parent),
                Some(context.under),
                "`{}` places its hole under `{parent}`",
                context.name
            );
        }

        // Every former a context is written in is one some context holds a hole under, so none is placed in [`former`] and left without one.
        let mut within = vec![body.clone()];
        while let Some(term) = within.pop() {
            let children = children_of(&term);
            if let (false, Some(former)) = (children.is_empty(), former(&term)) {
                assert!(
                    placed.contains(&former),
                    "`{}` is written in a former no context holds a hole under: `{term}`",
                    context.name
                );
            }
            within.extend(children);
        }
    }
}
