//! The command line, read off the tree: a subcommand for each group and step by its path, and the flags and arguments the variables of its steps take.
//!
//! **A path the tree does not hold cannot be written.** Each group is a subcommand whose own subcommands are what it holds, so `cargo xtask bundle lint` is the parser's own error, and `--help` at any depth lists what is there, each step described by its `description` and each group inside a file by its steps'. A variable given on the command line is a flag or an argument of every subcommand whose steps use it. `release` and `installer` are the two commands outside the tree, and the reader refuses a task of either name.

use {
    crate::{Given, Name, Tree, Variable, dotted},
    clap::{Arg, ArgMatches, Command, parser::ValueSource},
    std::collections::BTreeSet,
};

#[cfg(test)]
mod tests;

/// What the command line asked for.
#[derive(Debug, PartialEq, Eq)]
pub enum Asked {
    /// A step or a group, by its path, with the variables the command line gave.
    Task {
        name: Name,
        given: Given,
    },
    Release {
        bump: Option<String>,
    },
    Installer {
        version: String,
    },
}

/// The command line the tree reads as.
pub fn command(tree: &Tree) -> Command {
    let top = &tree.groups[&Vec::new()];
    let mut command = Command::new("cargo xtask")
        .bin_name("cargo xtask")
        .version(env!("CARGO_PKG_VERSION"))
        .about(top.description.clone().unwrap_or_default())
        .subcommand_required(true)
        .arg_required_else_help(true)
        .help_template(
            "\
{name} {version}
{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}",
        );
    for child in &top.children {
        command = command.subcommand(task(tree, child));
    }

    command
        .subcommand(
            Command::new("release")
                .about("Bump the workspace version and bundled library's compiler minimum, commit them, tag the release, and push main and the tag")
                .arg(
                    Arg::new("bump")
                        .value_name("BUMP")
                        .help("patch, minor, major, or the version itself; omitted, the three candidates are named"),
                ),
        )
        .subcommand(
            Command::new("installer")
                .about("Render the installer script for one release version under xtask/.artifacts/install.sh")
                .arg(
                    Arg::new("version")
                        .value_name("VERSION")
                        .required(true)
                        .help("The release's version: the part of its tag after release/"),
                ),
        )
}

/// The subcommand of the task `name`: what it holds, and the variables its steps take from the command line.
fn task(tree: &Tree, name: &Name) -> Command {
    let word = name.last().expect("a task has a name").clone();
    let about = match tree.groups.get(name) {
        Some(group) => group.description.clone().unwrap_or_else(|| {
            group
                .steps
                .iter()
                .map(|step| tree.steps[*step].description.as_str())
                .collect::<Vec<_>>()
                .join("; ")
        }),
        None => tree.steps[tree.steps_of(name)[0]].description.clone(),
    };

    let mut command = Command::new(word).about(about);
    if let Some(group) = tree.groups.get(name) {
        for child in &group.children {
            command = command.subcommand(task(tree, child));
        }
    }
    // A group takes the flags its steps do; a positional argument is a step's alone, since on a group it would read as a word naming what the group holds.
    let is_group = tree.groups.contains_key(name);
    let given = tree
        .steps_of(name)
        .into_iter()
        .flat_map(|step| tree.given_to(step))
        .filter(|variable| !is_group || matches!(tree.variables[variable], Variable::Flag { .. }))
        .collect::<BTreeSet<_>>();
    for variable in given {
        command = command.arg(argument(tree, &variable));
    }

    command
}

/// How the command line gives the variable `name`.
fn argument(tree: &Tree, name: &Name) -> Arg {
    let id = dotted(name);
    match &tree.variables[name] {
        Variable::Flag { flag, help } => {
            let value = name.last().expect("a variable has a name").to_uppercase();
            let argument = Arg::new(id).value_name(value).help(help.clone());
            match flag.strip_prefix("--") {
                Some(long) => argument.long(long.to_string()),
                None => argument.short(
                    flag.trim_start_matches('-')
                        .chars()
                        .next()
                        .expect("the reader refuses an empty flag"),
                ),
            }
        }
        Variable::Argument {
            placeholder, help, ..
        } => Arg::new(id)
            .value_name(placeholder.clone())
            .help(help.clone()),
        Variable::Words(_) | Variable::Output(_) => {
            unreachable!("only a flag or an argument is given on the command line")
        }
    }
}

/// What `matches` asked for.
pub fn asked(matches: &ArgMatches) -> Asked {
    match matches.subcommand() {
        Some(("release", release)) => {
            return Asked::Release {
                bump: release.get_one::<String>("bump").cloned(),
            };
        }
        Some(("installer", installer)) => {
            return Asked::Installer {
                version: installer
                    .get_one::<String>("version")
                    .expect("the version is required")
                    .clone(),
            };
        }
        _ => {}
    }

    let mut name = Vec::new();
    let mut given = Given::new();
    let mut current = matches;
    while let Some((word, below)) = current.subcommand() {
        name.push(word.to_string());
        for id in below.ids() {
            if below.value_source(id.as_str()) == Some(ValueSource::CommandLine)
                && let Some(value) = below.get_one::<String>(id.as_str())
            {
                given.insert(
                    id.as_str().split('.').map(str::to_string).collect(),
                    value.clone(),
                );
            }
        }
        current = below;
    }

    Asked::Task { name, given }
}
