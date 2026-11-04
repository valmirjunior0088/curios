//! The board read off its records, `xboard/tickets/`: a directory per part holding its `metadata.toml`, and in it a directory per ticket holding the ticket's `metadata.toml` and its witnesses' texts.
//!
//! The loader holds a record to the words it knows: a ticket's directory is named for its title, its day is one the calendar has, its status is `open` or `fixed`, a witness's text is a program, `.crs`, or Core text, `.crsc`, every file is one a witness puts, and a refusal names a checker the witness is put to, an error that checker's crate states, and where it stands by `entry` or an absolute path. A record that breaks any of them fails the run.

#[cfg(test)]
mod tests;

use {
    crate::{Checker, Date, Expected, Part, Proof, Status, Ticket, Within, Witness},
    serde::Deserialize,
    std::collections::{BTreeMap, BTreeSet},
    toml::value::Datetime,
};

/// What every record of the board is named.
const METADATA: &str = "metadata.toml";

/// A part's record.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PartRecord {
    description: String,
}

/// A ticket's record.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TicketRecord {
    title: String,
    found: Datetime,
    status: Status,
    witness: Vec<WitnessRecord>,
}

/// A witness's entry in its ticket's record.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct WitnessRecord {
    what: String,
    /// The file of the ticket's directory its text is.
    source: String,
    refused: Vec<RefusedRecord>,
}

/// One refusal a witness expects.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RefusedRecord {
    by: Checker,
    error: Option<String>,
    within: Option<String>,
}

/// Every part the files of a board hold, each by its path below the board's directory, in the order of their directories' names.
pub(crate) fn load_board<'f>(
    files: impl IntoIterator<Item = (&'f str, &'f str)>,
) -> Result<Vec<Part>, String> {
    let mut parts = BTreeMap::<&str, (Option<&str>, BTreeMap<&str, BTreeMap<&str, &str>>)>::new();
    for (path, text) in files {
        match path.split('/').collect::<Vec<_>>()[..] {
            [part, METADATA] => parts.entry(part).or_default().0 = Some(text),
            [part, ticket, file] => {
                parts
                    .entry(part)
                    .or_default()
                    .1
                    .entry(ticket)
                    .or_default()
                    .insert(file, text);
            }
            _ => {
                return Err(format!(
                    "{path}: the board holds a directory per part, its `{METADATA}` and a directory per ticket, and nothing else"
                ));
            }
        }
    }

    parts
        .into_iter()
        .map(|(name, (record, tickets))| {
            let record = record
                .ok_or_else(|| format!("{name}: a part's directory holds its `{METADATA}`"))?;
            let PartRecord { description } =
                toml::from_str(record).map_err(|error| format!("{name}/{METADATA}: {error}"))?;
            let tickets = tickets
                .into_iter()
                .map(|(directory, files)| {
                    load_ticket(directory, &files).map_err(|why| format!("{name}/{why}"))
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Part {
                name: name.to_string(),
                description,
                tickets,
            })
        })
        .collect()
}

/// The ticket filed in the directory named `directory`, read from the files it holds, each by its name.
pub(crate) fn load_ticket(directory: &str, files: &BTreeMap<&str, &str>) -> Result<Ticket, String> {
    let in_record = |why: String| format!("{directory}/{METADATA}: {why}");
    let record = files
        .get(METADATA)
        .ok_or_else(|| format!("{directory}: a ticket's directory holds its `{METADATA}`"))?;
    let TicketRecord {
        title,
        found,
        status,
        witness,
    } = toml::from_str(record).map_err(|error| in_record(error.to_string()))?;

    let filed = filed_as(&title);
    if filed != directory {
        return Err(in_record(format!(
            "its title files the ticket as `{filed}`"
        )));
    }
    let found_on =
        day(&found).ok_or_else(|| in_record(format!("`{found}` is no day the calendar has")))?;
    if witness.is_empty() {
        return Err(in_record("a ticket carries a witness".to_string()));
    }

    let mut put = BTreeSet::new();
    let witnesses = witness
        .into_iter()
        .map(|record| {
            let text = files.get(record.source.as_str()).ok_or_else(|| {
                in_record(format!(
                    "no file `{}` holds a witness's text",
                    record.source
                ))
            })?;
            if !put.insert(record.source.clone()) {
                return Err(in_record(format!(
                    "`{}` is put by two witnesses",
                    record.source
                )));
            }
            let proof = match record.source.rsplit_once('.') {
                Some((_, "crs")) => Proof::Program(text.to_string()),
                Some((_, "crsc")) => Proof::Module(text.to_string()),
                _ => {
                    return Err(in_record(format!(
                        "`{}` is neither a program, `.crs`, nor Core text, `.crsc`",
                        record.source
                    )));
                }
            };
            if record.refused.is_empty() {
                return Err(in_record(format!(
                    "the witness of `{}` states what refuses it",
                    record.source
                )));
            }
            let expect = record
                .refused
                .into_iter()
                .map(|refused| expected(refused, &proof).map_err(in_record))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Witness {
                what: record.what,
                proof,
                expect,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    if let Some(stray) = files
        .keys()
        .find(|file| **file != METADATA && !put.contains(**file))
    {
        return Err(format!("{directory}/{stray}: no witness puts it"));
    }

    Ok(Ticket {
        title,
        found_on,
        status,
        witnesses,
    })
}

/// The refusal `record` expects of a witness put as `proof`.
fn expected(record: RefusedRecord, proof: &Proof) -> Result<Expected, String> {
    let RefusedRecord { by, error, within } = record;
    if !proof.asked().contains(&by) {
        return Err(format!("a {} is not put to the {by}", proof.kind()));
    }
    let error = error
        .map(|name| {
            by.errors()
                .iter()
                .find(|known| **known == name)
                .copied()
                .ok_or_else(|| format!("the {by} raises no error named `{name}`"))
        })
        .transpose()?;
    let within = match within.as_deref() {
        None => None,
        Some(_) if by == Checker::Reader => {
            return Err("a reader's refusal stands in no declaration".to_string());
        }
        Some("entry") => Some(Within::Entry),
        Some(path) if path.starts_with('/') => Some(Within::Declaration(path.to_string())),
        Some(other) => {
            return Err(format!(
                "`{other}` is neither `entry` nor a declaration's absolute path"
            ));
        }
    };

    Ok(Expected { by, error, within })
}

/// The day `found` states, where it states a day alone and the calendar has it.
fn day(found: &Datetime) -> Option<Date> {
    match found {
        Datetime {
            date: Some(date),
            time: None,
            offset: None,
        } => Date::new(date.year, date.month, date.day),
        _ => None,
    }
}

/// The name a ticket's directory takes from its title: lowercase, a backtick or an apostrophe dropped, and every other run of what is not a letter or a digit one `_`.
fn filed_as(title: &str) -> String {
    let mut name = String::new();
    for character in title
        .chars()
        .filter(|character| !matches!(character, '`' | '\''))
    {
        match character.is_ascii_alphanumeric() {
            true => name.push(character.to_ascii_lowercase()),
            false if !name.ends_with('_') => name.push('_'),
            false => {}
        }
    }

    name.trim_matches('_').to_string()
}
