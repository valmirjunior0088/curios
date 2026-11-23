//! What a command's words launch.
//!
//! **A command is the words a person would type.** Leading `NAME=value` words set the environment, as a shell reads them, and the next word names the program: a tool's executable where the step names one, `{.tools.npm}`, and otherwise a program the `PATH` finds as written. `cargo` is the cargo that launched this run: `cargo xtask` is `cargo run`, and cargo sets `CARGO` to the binary performing the build — the toolchain's own, the rustup shim already out of the picture — so a step cannot resolve a second time and land somewhere else.

use std::{env, process::Command};

#[cfg(test)]
mod tests;

/// What a command's words launch.
pub struct Launch {
    pub command: Command,
    pub arguments: Vec<String>,
}

/// Whether `word` sets the environment, as a leading `NAME=value` word does.
fn is_assignment(word: &str) -> bool {
    word.split_once('=').is_some_and(|(name, _)| {
        !name.is_empty()
            && name.chars().all(|character| {
                character.is_ascii_uppercase() || character.is_ascii_digit() || character == '_'
            })
    })
}

/// What `words` launch: the environment their leading assignments set, and the program the first other word names, with the rest its arguments.
pub fn launch(words: &[String]) -> Result<Launch, String> {
    let assignments = words.iter().take_while(|word| is_assignment(word)).count();
    let Some(program) = words.get(assignments) else {
        return Err("a command sets its environment and names no program".to_string());
    };

    let mut command = match program.as_str() {
        "cargo" => Command::new(env::var_os("CARGO").unwrap_or_else(|| "cargo".into())),
        program => Command::new(program),
    };
    for assignment in &words[..assignments] {
        let (name, value) = assignment
            .split_once('=')
            .expect("an assignment holds an equals sign");
        command.env(name, value);
    }

    Ok(Launch {
        command,
        arguments: words[assignments + 1..].to_vec(),
    })
}
