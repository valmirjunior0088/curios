//! The board read off its records, `xboard/tickets/`: a directory per part holding its `metadata.toml`, and in it a directory per ticket holding the ticket's `metadata.toml` and its witnesses' texts.
//!
//! The loader holds a record to the words it knows: a ticket's directory is named for its title, its day is one the calendar has, its status is `open` or `fixed`, a witness states its `source` or the steps of its history, closing with its proof's, every text is a program, `.crs`, or Core text, `.crsc`, every file is one a witness puts, a step mounts a unit at a path of one name and withholds only a unit an earlier step mounted, and a refusal names a checker the witness is put to, an error that checker's crate states, and where it stands by `entry` or an absolute path. A record that breaks any of them fails the run.

#[cfg(test)]
mod tests;

use {
    crate::{Checker, Date, Expected, Part, Proof, Status, Step, Ticket, Within, Witness},
    curios_common::Qualifier,
    curios_core::Global,
    curios_verdict::test_support::Written,
    serde::Deserialize,
    std::collections::{BTreeMap, BTreeSet},
    toml::value::Datetime,
};

/// What every record of the board is named.
pub(crate) const METADATA: &str = "metadata.toml";

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

/// A witness's entry in its ticket's record: its proof's file, or the steps of its history, the last of them its proof's.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct WitnessRecord {
    what: String,
    /// The file of the ticket's directory its text is.
    source: Option<String>,
    #[serde(default)]
    step: Vec<StepRecord>,
    refused: Vec<RefusedRecord>,
}

/// One step of a witness's history: a unit, by the path it is mounted at and its file; a record withheld, by the unit's path and the names withheld, all of them where none is listed; or the proof, its file alone.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StepRecord {
    mount: Option<String>,
    source: Option<String>,
    withhold: Option<String>,
    verdicts: Option<Vec<String>>,
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
            let mut read = BTreeSet::new();
            let mut text = |source: &str| {
                read.insert(source.to_string());
                text_of(files, source)
            };
            let (steps, proof) = match (record.source, record.step) {
                (Some(source), steps) if steps.is_empty() => (Vec::new(), proof_of(text(&source)?)),
                (None, steps) if !steps.is_empty() => history(steps, &mut text)?,
                (Some(_), _) => {
                    return Err(format!(
                        "the witness `{}` states its `source` or its steps, not both",
                        record.what
                    ));
                }
                (None, _) => {
                    return Err(format!(
                        "the witness `{}` states its `source` or its steps",
                        record.what
                    ));
                }
            };
            if let Some(shared) = read.iter().find(|source| put.contains(*source)) {
                return Err(format!("`{shared}` is put by two witnesses"));
            }
            put.extend(read);

            let witness = Witness {
                what: record.what,
                steps,
                proof,
                expect: Vec::new(),
            };
            if record.refused.is_empty() {
                return Err(format!(
                    "the witness `{}` states what refuses it",
                    witness.what
                ));
            }
            let expect = record
                .refused
                .into_iter()
                .map(|refused| expected(refused, &witness))
                .collect::<Result<Vec<_>, _>>()?;

            Ok(Witness { expect, ..witness })
        })
        .collect::<Result<Vec<_>, String>>()
        .map_err(in_record)?;

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

/// The text the file `source` of a ticket's directory holds, with what it is written in, read off its extension.
fn text_of<'f>(
    files: &BTreeMap<&str, &'f str>,
    source: &str,
) -> Result<(Written, &'f str), String> {
    let text = files
        .get(source)
        .ok_or_else(|| format!("no file `{source}` holds a witness's text"))?;
    match source.rsplit_once('.') {
        Some((_, "crs")) => Ok((Written::Program, text)),
        Some((_, "crsc")) => Ok((Written::CoreText, text)),
        _ => Err(format!(
            "`{source}` is neither a program, `.crs`, nor Core text, `.crsc`"
        )),
    }
}

/// The proof a text is, by what it is written in.
fn proof_of((written, text): (Written, &str)) -> Proof {
    match written {
        Written::Program => Proof::Program(text.to_string()),
        Written::CoreText => Proof::Module(text.to_string()),
    }
}

/// The steps a history's records state before its last, and the proof its last puts, each text read through `text`.
fn history<'f>(
    records: Vec<StepRecord>,
    text: &mut impl FnMut(&str) -> Result<(Written, &'f str), String>,
) -> Result<(Vec<Step>, Proof), String> {
    let shape = "a step is a unit, `mount` and `source`; a record withheld, `withhold` and its `verdicts` where it withholds some; or the proof, `source` alone";
    let mut records = records;
    let last = records.pop().expect("a history states a step");
    let proof = match last {
        StepRecord {
            mount: None,
            source: Some(source),
            withhold: None,
            verdicts: None,
        } => proof_of(text(&source)?),
        _ => {
            return Err(
                "a history closes with its proof, a step with a `source` and no `mount`"
                    .to_string(),
            );
        }
    };
    if records.is_empty() {
        return Err("a history of its proof alone is a witness's `source`".to_string());
    }

    let mut mounted = BTreeSet::new();
    let steps = records
        .into_iter()
        .map(|record| match record {
            StepRecord {
                mount: Some(mount),
                source: Some(source),
                withhold: None,
                verdicts: None,
            } => {
                let mount = mount_of(&mount)?;
                let (written, text) = text(&source)?;
                mounted.insert(mount.clone());

                Ok(Step::Unit {
                    mount,
                    written,
                    text: text.to_string(),
                })
            }
            StepRecord {
                mount: None,
                source: None,
                withhold: Some(withheld),
                verdicts,
            } => {
                let mount = mount_of(&withheld)?;
                if !mounted.contains(&mount) {
                    return Err(format!("`{withheld}` is withheld before a step mounts it"));
                }
                let names = verdicts
                    .map(|verdicts| verdicts.iter().map(|path| name_of(path)).collect())
                    .transpose()?;

                Ok(Step::Withhold { mount, names })
            }
            StepRecord {
                mount: None,
                source: Some(_),
                withhold: None,
                verdicts: None,
            } => Err("only a history's last step is its proof".to_string()),
            _ => Err(shape.to_string()),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok((steps, proof))
}

/// The mount a step's path names, `/lib` mounting `lib`: a path of one name.
fn mount_of(path: &str) -> Result<String, String> {
    match path.strip_prefix('/') {
        Some(name) if !name.is_empty() && !name.contains('/') => Ok(name.to_string()),
        _ => Err(format!(
            "`{path}` is no path of one name a unit is mounted at"
        )),
    }
}

/// The declaration an absolute path names.
fn name_of(path: &str) -> Result<Global, String> {
    match path.strip_prefix('/') {
        Some(rest) if !rest.is_empty() && rest.split('/').all(|part| !part.is_empty()) => {
            Ok(Global::Authored(Qualifier::from(rest.split('/'))))
        }
        _ => Err(format!("`{path}` is no declaration's absolute path")),
    }
}

/// The refusal `record` expects of `witness`.
fn expected(record: RefusedRecord, witness: &Witness) -> Result<Expected, String> {
    let RefusedRecord { by, error, within } = record;
    if !witness.asked().contains(&by) {
        return Err(format!("a {} is not put to the {by}", witness.kind()));
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
