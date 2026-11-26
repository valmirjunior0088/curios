//! The command line, read off the tree: a subcommand for each group and step by its path, and the flags and arguments the variables of its steps take.
//!
//! **A path the tree does not hold cannot be written.** Each group is a subcommand whose own subcommands are what it holds, so `cargo xtask bundle lint` is the parser's own error, and `--help` at any depth lists what is there, each step described by its `description` and each group inside a file by its steps'. A variable given on the command line is a flag or an argument of every subcommand whose steps use it.

use {
    crate::{Given, GivenValues, Name, Tree, dotted},
    clap::{Arg, ArgAction, ArgMatches, Command, parser::ValueSource},
    std::collections::BTreeSet,
};

#[cfg(test)]
mod tests;

/// What the command line asked for: a step or a group, by its path, with the variables the command line gave.
#[derive(Debug, PartialEq, Eq)]
pub struct Asked {
    pub name: Name,
    pub given: GivenValues,
}

/// The command line the tree reads as.
pub fn command(tree: &Tree) -> Command {
    let top = &tree.groups[&Vec::new()];
    let mut command = Command::new("cargo xtask")
        .bin_name("cargo xtask")
        .version(env!("CARGO_PKG_VERSION"))
        .about(top.description.clone())
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
}

/// The subcommand of the task `name`: what it holds, and the variables its steps take from the command line.
fn task(tree: &Tree, name: &Name) -> Command {
    let word = name.last().expect("a task has a name").clone();
    let about = match tree.groups.get(name) {
        Some(group) => group.description.clone(),
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
        .filter(|given| !is_group || matches!(tree.given[given], Given::Flag { .. }))
        .collect::<BTreeSet<_>>();
    for given in given {
        command = command.arg(argument(tree, &given));
    }

    command
}

/// How the command line gives the value `name`.
fn argument(tree: &Tree, name: &Name) -> Arg {
    let id = dotted(name);
    match &tree.given[name] {
        Given::Flag { flags, help } => {
            let value = name
                .last()
                .expect("a given value has a name")
                .to_uppercase();
            flags.iter().fold(
                Arg::new(id)
                    .value_name(value)
                    .help(help.clone())
                    .action(ArgAction::Append),
                |argument, flag| match flag.strip_prefix("--") {
                    Some(long) => argument.long(long.to_string()),
                    None => argument.short(
                        flag.chars()
                            .nth(1)
                            .expect("the reader takes a short flag as a dash and one character"),
                    ),
                },
            )
        }
        Given::Argument {
            placeholder,
            else_,
            help,
        } => Arg::new(id)
            .value_name(placeholder.clone())
            .required(else_.is_none())
            .help(help.clone()),
    }
}

/// What `matches` asked for.
pub fn asked(matches: &ArgMatches) -> Asked {
    let mut name = Vec::new();
    let mut given = GivenValues::new();
    let mut current = matches;
    while let Some((word, below)) = current.subcommand() {
        name.push(word.to_string());
        for id in below.ids() {
            if below.value_source(id.as_str()) == Some(ValueSource::CommandLine)
                && let Some(values) = below.get_many::<String>(id.as_str())
            {
                given.insert(
                    id.as_str().split('.').map(str::to_string).collect(),
                    values.cloned().collect(),
                );
            }
        }
        current = below;
    }

    Asked { name, given }
}
