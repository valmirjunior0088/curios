//! `curios lint`: the gate over what `wonder diagnostics` reports.
//!
//! The same subjects, the same records, the same rendering: a lint is a diagnostic the compilation reports and nothing stops on, and this subcommand is where it turns into an exit code. What it adds is the one lint no unit can decide alone — a declared dependency nothing in the package reached, read off the union of what every unit resolved into — and it adds it only when the target is the package entire, because a dependency is a fact of the package and a file asked about alone reaches what it reaches.

use {
    crate::{Diagnosed, Severity, resolve},
    curios_package::{Form, Governing},
    curios_text::Overlay,
    curios_utilities::{Qualifier, Report},
    std::{
        collections::BTreeSet,
        path::{Path, PathBuf},
    },
};

/// What `curios lint` found, in the order its exit code ranks them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Linted {
    /// Nothing reported: exit 0.
    Clean,
    /// Goals and nothing else: the incomplete state `run` exits 2 on.
    Goals,
    /// A lint or an error: exit 1.
    Findings,
}

/// `curios lint [TARGET]`: every diagnostic, goal and lint of the target rendered to stdout, each distinct fact once, and for the package entire every dependency nothing reached.
pub fn lint(
    budget: u64,
    mounted: &[PathBuf],
    manifest: Option<&Path>,
    target: Option<&str>,
) -> Result<Linted, String> {
    let overlay = Overlay::default();
    let package_entire = matches!(Form::of(target), Form::Named(None));

    let mut seen = Renderings::default();
    let mut reached = BTreeSet::new();
    let mut linted = Linted::Clean;
    for asked in resolve(mounted, manifest, target)? {
        let Diagnosed {
            diagnostics,
            reached: unit_reached,
        } = asked.diagnosed(budget, &overlay);
        reached.extend(unit_reached);
        for diagnostic in diagnostics {
            linted = linted.max(match diagnostic.severity {
                Severity::Goal => Linted::Goals,
                Severity::Error | Severity::Lint => Linted::Findings,
            });
            seen.insert_rendered(diagnostic.render());
        }
    }

    if package_entire {
        let governing = Governing::here(manifest)?;
        for name in unused_dependencies(governing.package.dependencies.keys(), &reached) {
            linted = Linted::Findings;
            seen.insert_rendered(
                Report::unlocated(format!("unused dependency `{name}`; delete its row")).render(),
            );
        }
    }

    if !seen.rendered.is_empty() {
        println!("{}", seen.rendered.join("\n\n"));
    }

    Ok(linted)
}

/// The declared dependencies whose prefix nothing resolved into. Exact, where a language with externals could only guess: a dependency mounts at its name and is reached only by a reference that names it.
fn unused_dependencies<'a>(
    declared: impl IntoIterator<Item = &'a String>,
    reached: &BTreeSet<Qualifier>,
) -> Vec<&'a String> {
    declared
        .into_iter()
        .filter(|name| !reached.contains(&Qualifier::from([name.as_str()])))
        .collect()
}

/// Renderings in the order they arrived, each distinct one once — the collapse `wonder diagnostics` applies, since the subjects of a package overlap on their library.
#[derive(Default)]
struct Renderings {
    rendered: Vec<String>,
    distinct: BTreeSet<String>,
}

impl Renderings {
    fn insert_rendered(&mut self, rendering: String) {
        if self.distinct.insert(rendering.clone()) {
            self.rendered.push(rendering);
        }
    }
}

#[cfg(test)]
mod tests;
