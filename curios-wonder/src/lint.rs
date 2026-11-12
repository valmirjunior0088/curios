//! `curios lint`: the gate over what `wonder` reports.
//!
//! The same subjects, the same records, the same rendering: a lint is a diagnostic the compilation reports and nothing stops on, and this subcommand is where it turns into an exit code. What it adds is the one lint no unit can decide alone — a declared dependency nothing in the package wrote, read off the union of the prefixes every unit spelled — and it adds it only when the subject is the package entire, because a dependency is a fact of the package and a file asked about alone reaches what it reaches.

use {
    crate::{Asked, Diagnosed, Severity, Telling, answer},
    curios_common::{Kind, Qualifier, Report, Said},
    curios_module::Overlay,
    curios_package::Selection,
    std::collections::BTreeSet,
};

/// What `curios lint` found, each distinct fact counted once: its lints, an unused dependency among them, the errors beside them, and the written goals. A lint or an error is exit 1, goals and nothing else the incomplete state `run` exits 2 on, and none of the three exit 0.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Linted {
    pub lints: usize,
    pub errors: usize,
    pub goals: usize,
}

/// `curios lint [SUBJECT]`: every diagnostic, goal and lint of what `selection` selects rendered to stdout, each distinct fact once, and for the package entire every dependency nothing reached. `telling` is told of each subject's compile as it goes and why, where a store took nothing of what the lint compiled, which changes neither the report nor what is returned.
pub fn lint(
    budget: u64,
    selection: Selection,
    telling: &mut dyn Telling,
) -> Result<Linted, String> {
    let overlay = Overlay::default();
    // A dependency is a fact of the package, so only the package entire is asked which of its dependencies nothing reached.
    let dependencies = match &selection {
        Selection::Entire(entire) => Some(
            entire
                .governing
                .package
                .dependencies
                .keys()
                .cloned()
                .collect::<Vec<_>>(),
        ),
        Selection::Library(_) | Selection::Program(_) => None,
    };

    let mut seen = Renderings::default();
    let reader = telling.reader();
    let mut reached = BTreeSet::new();
    let mut linted = Linted::default();
    let mut unstored = None;
    for asked in Asked::every(selection)? {
        let Diagnosed {
            diagnostics,
            reached: unit_reached,
            unfiled: refusal,
            ..
        } = asked.diagnosed(budget, &overlay, telling);
        reached.extend(unit_reached);
        // The first, since every subject of a package asks one store.
        unstored = unstored.or(refusal);
        for diagnostic in diagnostics {
            if seen.insert_rendered(diagnostic.rendered(&reader)) {
                match diagnostic.severity {
                    Severity::Lint => linted.lints += 1,
                    Severity::Error => linted.errors += 1,
                    Severity::Goal => linted.goals += 1,
                    // A note says how the question was answered, which is nothing to fix.
                    Severity::Note => {}
                }
            }
        }
    }

    if let Some(dependencies) = dependencies {
        for name in unused_dependencies(&dependencies, &reached) {
            let mut said = Said::line("unused dependency");
            said.stating(format_args!("`{name}` is never used"));
            said.stating("delete its row");
            let rendered = Report::of(None, said).rendered(Kind::Lint, &reader);
            if seen.insert_rendered(rendered) {
                linted.lints += 1;
            }
        }
    }

    if !seen.rendered.is_empty() {
        telling.answering();
        answer(format_args!("{}\n", seen.rendered.join("\n\n")));
    }
    if let Some(unstored) = unstored {
        telling.unfiled(unstored);
    }

    Ok(linted)
}

/// The declared dependencies whose prefix nothing in the package *wrote*. Exact, where a language with externals could only guess: a dependency mounts at its name and is reached only by a reference that names it.
///
/// Spelling rather than resolution, which is the same basis the refusal uses — one rule enforced one way. Resolution would report almost nothing unused: a re-export means the path a reference resolves to need not be the path it was written under, so a program naming `/std/Nat` resolves into `/sys` without having spelled it.
fn unused_dependencies<'a>(
    declared: impl IntoIterator<Item = &'a String>,
    reached: &BTreeSet<Qualifier>,
) -> Vec<&'a String> {
    declared
        .into_iter()
        .filter(|name| !reached.contains(&Qualifier::from([name.as_str()])))
        .collect()
}

/// Renderings in the order they arrived, each distinct one once — the collapse `wonder` applies, since the subjects of a package overlap on their library.
#[derive(Default)]
struct Renderings {
    rendered: Vec<String>,
    distinct: BTreeSet<String>,
}

impl Renderings {
    /// Keep `rendering` where it is the first of its text, answering whether it is.
    fn insert_rendered(&mut self, rendering: String) -> bool {
        let first = self.distinct.insert(rendering.clone());
        if first {
            self.rendered.push(rendering);
        }

        first
    }
}

#[cfg(test)]
mod tests;
