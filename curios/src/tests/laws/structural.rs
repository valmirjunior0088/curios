//! The seeds: one equation for each rule of conversion no law table states — beta, delta, zeta, iota at each elimination, eta at a function, a record, a struct and a unit, and irrelevance — and the near miss beside it.
//!
//! A seed is two sides at one type, under one list of binders. A held seed's right side is a neutral of that type, a binder or a call of one, so two held seeds at one type meet at it and chain, and a seed placed under a context keeps a side the context cannot reduce. A near miss differs from the held seed beside it in the one thing its rule must not let pass: a binder dropped, two components swapped, the other arm taken.
//!
//! No seed states a verdict. Which side a checker puts it on is read from each checker asked by itself, and a seed a checker puts on the other side is listed in [`PARTED`] with the finding that holds why.

use super::{Answers, Audit, asked_alone, closes, hold_to_the_table, parted};

/// What a seed's program states ahead of its rows: its imports, and the declarations the seeds are stated over.
pub(super) const STRUCTURAL: &str =
    "use /std/{Eq, Nat, Bool, List, Option, Io}; use /std/Bool/{Holds};
struct Record: pub Type { a: Nat, b: Nat }
struct Empty: pub Type {}
struct Bounded: pub Type { n: Nat, ok: Holds(n < 10) }
let forward(h: (Nat) -> Nat) -> (Nat) -> Nat = h;
let shifted(h: (Nat) -> Nat) -> (Nat) -> Nat = (x: Nat) => h(x + 1);";

/// The seeds at one type, under the binders they share.
pub(super) struct Seeds {
    pub(super) type_: &'static str,
    pub(super) binders: &'static str,
    pub(super) seeds: &'static [Seed],
}

/// One equation: the rule it is a seed of, its two sides, and whether the rule holds it.
pub(super) struct Seed {
    pub(super) rule: &'static str,
    pub(super) left: &'static str,
    pub(super) right: &'static str,
    pub(super) holds: bool,
}

const fn held(rule: &'static str, left: &'static str, right: &'static str) -> Seed {
    Seed {
        rule,
        left,
        right,
        holds: true,
    }
}

const fn miss(rule: &'static str, left: &'static str, right: &'static str) -> Seed {
    Seed {
        rule,
        left,
        right,
        holds: false,
    }
}

pub(super) const SEEDS: &[Seeds] = &[
    Seeds {
        type_: "(Nat) -> Nat",
        binders: "g: (Nat) -> Nat, k: (Nat) -> Nat",
        seeds: &[
            held("function eta", "(x: Nat) => g(x)", "g"),
            held("function eta past a fold", "(x: Nat) => g(x + 0)", "g"),
            miss("function eta, its binder dropped", "(x: Nat) => g(0)", "g"),
            held("beta", "((h: (Nat) -> Nat) => h)(g)", "g"),
            miss(
                "beta, its argument dropped",
                "((h: (Nat) -> Nat) => k)(g)",
                "g",
            ),
            held("delta", "forward(g)", "g"),
            miss("delta, another definition", "shifted(g)", "g"),
            held("zeta", "(let h = g; h)", "g"),
            miss("zeta, another value", "(let h = k; h)", "g"),
            held(
                "iota at Bool",
                "match true: (_) => (Nat) -> Nat | true => g | false => k end",
                "g",
            ),
            miss(
                "iota at Bool, the other arm",
                "match false: (_) => (Nat) -> Nat | true => g | false => k end",
                "g",
            ),
            held(
                "iota at Nat",
                "match 0: (_) => (Nat) -> Nat | 0 => g | m + 1 => k end",
                "g",
            ),
            miss(
                "iota at Nat, the other arm",
                "match 1: (_) => (Nat) -> Nat | 0 => g | m + 1 => k end",
                "g",
            ),
            held(
                "iota at a literal",
                "match 3: (_) => (Nat) -> Nat | 3 => g | _ => k end",
                "g",
            ),
            miss(
                "iota at a literal, the default",
                "match 4: (_) => (Nat) -> Nat | 3 => g | _ => k end",
                "g",
            ),
            held(
                "iota at List",
                "match [1]: (_) => (Nat) -> Nat | [] => k | [h, ..t] => g end",
                "g",
            ),
            miss(
                "iota at List, the other arm",
                "match [1]: (_) => (Nat) -> Nat | [] => g | [h, ..t] => k end",
                "g",
            ),
            held(
                "iota at a family",
                "match Option/some(0): (_) => (Nat) -> Nat | some(v) => g | none() => k end",
                "g",
            ),
            miss(
                "iota at a family, the other arm",
                "match Option/none(@Nat): (_) => (Nat) -> Nat | some(v) => g | none() => k end",
                "g",
            ),
        ],
    },
    Seeds {
        type_: "{Nat, Nat}",
        binders: "p: {Nat, Nat}",
        seeds: &[
            held("record eta", "(p.0, p.1)", "p"),
            miss("record eta, its components swapped", "(p.1, p.0)", "p"),
        ],
    },
    Seeds {
        type_: "Record",
        binders: "s: Record",
        seeds: &[
            held("struct eta", "Record { a = s.a, b = s.b }", "s"),
            miss(
                "struct eta, its fields swapped",
                "Record { a = s.b, b = s.a }",
                "s",
            ),
        ],
    },
    Seeds {
        type_: "{}",
        binders: "u: {}, v: {}",
        seeds: &[
            held("unit eta, the literal", "()", "u"),
            held("unit eta, two neutrals", "v", "u"),
        ],
    },
    Seeds {
        type_: "Empty",
        binders: "u: Empty, v: Empty",
        seeds: &[
            held("unit eta at a struct, the literal", "Empty {}", "u"),
            held("unit eta at a struct, two neutrals", "v", "u"),
        ],
    },
    Seeds {
        type_: "Nat",
        binders: "a: Nat, b: Nat, f: (Holds(a < 10)) -> Nat, c: (Nat) -> Nat, p1: Holds(a < 10), p2: Holds(a < 10)",
        seeds: &[
            held("irrelevance through a variable head", "f(p1)", "f(p2)"),
            miss("relevance through a variable head", "c(a)", "c(b)"),
        ],
    },
    Seeds {
        type_: "Bounded",
        binders: "a: Nat, b: Nat, p1: Holds(a < 10), p2: Holds(a < 10), q: Holds(b < 10)",
        seeds: &[
            held(
                "irrelevance in a proof field",
                "Bounded { n = a, ok = p1 }",
                "Bounded { n = a, ok = p2 }",
            ),
            miss(
                "relevance beside a proof field",
                "Bounded { n = a, ok = p1 }",
                "Bounded { n = b, ok = q }",
            ),
        ],
    },
];

impl Seeds {
    /// One seed as a row, under the binders these seeds share.
    pub(super) fn row(&self, seed: &Seed) -> (String, String) {
        (
            self.binders.to_owned(),
            claim(self.type_, seed.left, seed.right),
        )
    }

    /// These seeds as rows.
    pub(super) fn rows(&self) -> Vec<(String, String)> {
        self.seeds.iter().map(|seed| self.row(seed)).collect()
    }
}

/// The claim that `left` and `right` are equal at `type_`. It states the type, where a carrier's row leaves it to inference, because `Eq/refl()`'s implicit is not solved against a lambda while its own type is unsolved: a row that left the type out would ask the elaborator for a solution, and what a seed asks for is a comparison.
pub(super) fn claim(type_: &str, left: &str, right: &str) -> String {
    format!("Eq(@({type_}))({left}, {right})")
}

#[test]
fn every_seed_is_on_the_side_each_checker_puts_it() {
    // Mutation-checked both ways: a near miss stated as held is named as refused by both and unlisted, and a row taken off the table is named as parting and unlisted.
    let found = SEEDS
        .iter()
        .flat_map(|seeds| {
            let answers = asked_alone(STRUCTURAL, seeds.type_, &seeds.rows());
            seeds
                .seeds
                .iter()
                .zip(answers)
                .filter(|(seed, answers)| *answers != Answers::both(seed.holds))
                .map(|(seed, answers)| (seed.rule.to_owned(), answers))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    hold_to_the_table(Audit::Seeds, &found);
}

#[test]
fn every_held_seed_the_table_does_not_list_closes_by_refl() {
    // The admitting direction as an author meets it: a compilation, the elaborator and then the kernel over the proof it built.
    let listed = parted();
    for seeds in SEEDS {
        let rows = seeds
            .seeds
            .iter()
            .filter(|seed| seed.holds)
            .filter(|seed| {
                !listed
                    .iter()
                    .any(|parted| parted.audit == Audit::Seeds && parted.row == seed.rule)
            })
            .map(|seed| seeds.row(seed))
            .collect::<Vec<_>>();
        if let Err(failures) = closes(STRUCTURAL, &rows) {
            panic!(
                "a held seed at `{}` no longer closes:\n{}",
                seeds.type_,
                failures.join("\n")
            );
        }
    }
}
