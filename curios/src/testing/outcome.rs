//! What a test found, as its program answers it: the checks that failed, each the steps that reached it, what it states and the values it compared, read back from the text `/std/Test` writes of them. The one reading of an answer, which a native run and a page's post are each read through, so the two runners present a test alike.

#[cfg(test)]
mod tests;

use curios_surface::{ListEntry, Literal, ProofLiteral, StructLitEntry, Subterm, Term};

/// What a test found: each check that failed, in the order the test met them, and none where the test held.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Outcome {
    pub failures: Vec<FailedCheck>,
}

/// One check that failed: the steps that reached it, the outermost first, what it states, and the values it compared, each behind its label and spelled as Curios writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FailedCheck {
    pub steps: Vec<String>,
    pub statement: String,
    pub facts: Vec<(String, String)>,
}

impl Outcome {
    /// The outcome `answer` spells: the text a derived `Spell` writes of `/std/Test`'s failures, a list of structure literals read back as Curios. `Err` says what of it is no outcome, which a test program never answers, so its runner reports it as its own fault.
    pub fn read(answer: &[u8]) -> Result<Self, String> {
        let text = str::from_utf8(answer).map_err(|_| "the answer is no text".to_string())?;
        let term = text
            .parse::<Term>()
            .map_err(|error| format!("the answer is no term: {}", error.format()))?;
        let failures = listed(&term)?
            .into_iter()
            .map(failed_check)
            .collect::<Result<_, _>>()?;

        Ok(Self { failures })
    }

    /// Whether the test held: no check of it failed.
    pub fn held(&self) -> bool {
        self.failures.is_empty()
    }
}

/// The elements of the list literal `term` is.
fn listed(term: &Term) -> Result<Vec<&Term>, String> {
    let Subterm::Literal(Literal::List(entries)) = term.as_subterm() else {
        return Err(format!("`{term}` is no list"));
    };

    entries
        .iter()
        .map(|entry| match entry {
            ListEntry::Elem(element) => Ok(element),
            ListEntry::Spread(_) => Err(format!("`{term}` spreads a list")),
        })
        .collect()
}

/// The text the string literal `term` is.
fn text(term: &Term) -> Result<String, String> {
    match term.as_subterm() {
        Subterm::ProofLiteral(ProofLiteral::Str(literal)) => Ok(literal.value.clone()),
        _ => Err(format!("`{term}` is no text")),
    }
}

/// The two texts the pair `term` is.
fn pair(term: &Term) -> Result<(String, String), String> {
    match term.as_subterm() {
        Subterm::Tuple(tuple) => match tuple.fields.as_slice() {
            [label, value] if label.label.is_none() && value.label.is_none() => {
                Ok((text(&label.value)?, text(&value.value)?))
            }
            _ => Err(format!("`{term}` is no pair")),
        },
        _ => Err(format!("`{term}` is no pair")),
    }
}

/// The failed check the structure literal `term` is, read by its fields' labels.
fn failed_check(term: &Term) -> Result<FailedCheck, String> {
    let Subterm::StructLit(literal) = term.as_subterm() else {
        return Err(format!("`{term}` is no failed check"));
    };
    let field = |label: &str| {
        literal
            .entries
            .iter()
            .find_map(|entry| match entry {
                StructLitEntry::Field(field) if field.label.as_deref() == Some(label) => {
                    Some(&field.value)
                }
                _ => None,
            })
            .ok_or_else(|| format!("`{term}` has no `{label}`"))
    };

    Ok(FailedCheck {
        steps: listed(field("steps")?)?
            .into_iter()
            .map(text)
            .collect::<Result<_, _>>()?,
        statement: text(field("statement")?)?,
        facts: listed(field("facts")?)?
            .into_iter()
            .map(pair)
            .collect::<Result<_, _>>()?,
    })
}
