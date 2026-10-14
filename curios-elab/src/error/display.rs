//! Rendering an [`Error`] under a [`Spelling`]: the per-arm prose every diagnostic carries, and the goal turnstile the elaborator's goal batches print.
//!
//! The rendering is separated from the error roster for size alone — [`Error`] and its inherent impl are already a long file — and the seam is exact: nothing here decides *what* went wrong, only how it reads. [`Displayed`] is the whole mechanism, and [`goal_text`] is the one piece the roster itself reaches for, when it turns a goal batch into per-goal [`Report`](curios_utilities::Report)s.

#[cfg(test)]
mod tests;

use {
    super::{
        BinderSite, Callee, CycleMember, DeadArm, Declined, Erased, ErasedName, ErasedSite, Error,
        GoalReport, Need, ShapeDiagnosis, Underivable, Watched,
    },
    crate::{Conclusion, Origin, Refusal, ordinal},
    curios_algebra::BOOL_ATOM_CAP,
    curios_analysis::{DeclaredPart, MemberLabel},
    curios_core::{
        Anchors, CalleeId, Free, Global, Level, Quality, ReaderPosition, Spelling, Subterm, Term,
        UniverseMetaId, WitnessKey,
    },
    curios_num::Grain,
    curios_utilities::{Plicity, Qualifier},
    std::{collections::HashMap, fmt, rc::Rc},
};

/// What a report on a dead arm adds: that the arm is never taken and what says so — its own guard, the guard of an arm around it that its case contradicts, or its scrutinee's indices, which also say it may be left out.
pub(super) fn unreachable_arm_note(dead: &DeadArm, spelling: &Rc<Spelling>) -> String {
    match dead {
        DeadArm::Guard { guard, case } => format!(
            "  this arm is never taken: its guard {} is always {}",
            guard.spelled(spelling),
            case.spelled(spelling),
        ),
        DeadArm::EnclosingGuard { guard, case } => format!(
            "  this arm is never taken: in it the guard {} is always {}",
            guard.spelled(spelling),
            case.spelled(spelling),
        ),
        DeadArm::Indices { case, scrutinee } => format!(
            "  this arm is never taken: its case has type {} and its scrutinee has type {}\n  the arm may be left out",
            case.spelled(spelling),
            scrutinee.spelled(spelling),
        ),
    }
}

/// One goal's report without its snippet: the turnstile idiom under its own rename map (see [`GoalReport::rename_map`]) — the batch-wide one the caller installed is replaced, not extended, since every name this report shows is in the narrower map by construction. The message half of a goal's [`Report`](curios_utilities::Report), and what the batch's `Display` writes before each snippet.
pub(super) fn goal_text(report: &GoalReport, spelling: &Rc<Spelling>) -> String {
    // A report's terms render within a fixed width — the pipeline is pure and stays terminal-blind, so the target is a constant — and a broken term's continuation lines re-indent under the clause body rather than restarting at column zero.
    const WIDTH: usize = 100;

    let spelling = Rc::new(
        spelling
            .as_ref()
            .clone()
            .with_pretty_names(report.rename_map(spelling)),
    );
    let clause = |term: &Term| {
        term.spelled(&spelling)
            .within(WIDTH)
            .to_string()
            .replace('\n', "\n    ")
    };

    let mut text = String::from("goal `?`");
    for (name, type_) in &report.scope {
        let shown = match unnameable_binder(name) {
            true => "_".to_string(),
            false => clause(name),
        };
        text.push_str(&format!("\n  {shown} : {}", clause(type_)));
    }
    text.push_str(&format!("\n  ? : {}", clause(&report.goal)));
    if let Some(refusal) = &report.refusal {
        text.push_str(&refused(refusal, &spelling));
    }
    if let Some(solution) = &report.solution {
        text.push_str(&format!("\n  ? = {}", clause(solution)));
    }
    for (this, that) in &report.obligations {
        text.push_str(&format!(
            "\n  ? such that {} \u{2261} {}",
            clause(this),
            clause(that)
        ));
    }
    for candidate in &report.candidates {
        text.push_str(&format!("\n  ? \u{2248} {}", clause(candidate)));
    }
    text
}

/// Whether a goal-scope binder is unnameable — a hintless local no written expression can reference. Its scope line spells `_` the way source does, instead of the synthesized name the rename map would mint for it.
fn unnameable_binder(name: &Term) -> bool {
    match &**name {
        Subterm::Var(var) => var.as_free().is_some_and(|free| !free.nameable()),
        _ => false,
    }
}

/// How a diagnostic names a declaring module. The root qualifier is the entry module — a legitimate value, not a missing one.
fn declaring_module(module: &Qualifier) -> String {
    match module.is_root() {
        true => "the entry module".to_string(),
        false => format!("module '{}'", module.join()),
    }
}

/// What a diagnostic calls a witness key: a head when it has one parameter, a key when it has several.
fn key_noun(key: &WitnessKey) -> &'static str {
    match key.heads().len() {
        1 => "head",
        _ => "key",
    }
}

/// One short name per level metavariable the message shows, numbered in the order the reader meets them.
///
/// A metavariable's own id counts every level the unit has invented, so the same two-line program reports `?u784` on one day and `?u791` on the next. The reader needs only to see which occurrences are the *same* level, which is what numbering within the message gives, and what makes the message something a fixture can asservate on.
fn level_meta_names<'a>(
    levels: impl IntoIterator<Item = &'a Level>,
) -> HashMap<UniverseMetaId, String> {
    let mut names = HashMap::new();
    for level in levels {
        for meta in level.metas() {
            let next = names.len() + 1;
            names.entry(meta).or_insert_with(|| format!("?u{next}"));
        }
    }
    names
}

/// A level under those names, the rest of its spelling left to [`Level`]'s own.
fn renamed_level(level: &Level, names: &HashMap<UniverseMetaId, String>) -> String {
    let mut text = level.to_string();
    // Longest first: `?u12` contains `?u1`, and replacing the shorter one first would leave `?u1` followed by a stray `2`.
    let mut metas = names.iter().collect::<Vec<_>>();
    metas.sort_by_key(|(meta, _)| std::cmp::Reverse(meta.to_string().len()));
    for (meta, name) in metas {
        text = text.replace(&meta.to_string(), name);
    }
    text
}

/// What the procedure that proves a bound from the facts in scope says beside a bound it refused, one line per clause under the bound's own: the facts it read, those it could not, and what its search concluded. A refusal that read nothing and searched nothing says nothing, so a bound the procedure reads nothing in reports as it would with no procedure.
fn refused(refusal: &Refusal, spelling: &Rc<Spelling>) -> String {
    if refusal.is_silent() {
        return String::new();
    }
    let origin = |origin: &Origin| match origin {
        Origin::Hypothesis(name) => name.spelled(spelling).to_string(),
        Origin::Field(name, label) => format!("{}.{label}", name.spelled(spelling)),
        Origin::Guard(written, true) => format!("a guard, {}", written.spelled(spelling)),
        Origin::Guard(written, false) => {
            format!("a guard's false arm, {}", written.spelled(spelling))
        }
        Origin::Natural | Origin::Definition | Origin::Product(..) | Origin::Negated => {
            String::new()
        }
    };
    // A guard read as it was written states itself, and its line names it once.
    let fact = |from: &Origin, statement: &Term| {
        let stated = statement.spelled(spelling).to_string();
        match from {
            Origin::Guard(written, _) if written.spelled(spelling).to_string() == stated => {
                origin(from)
            }
            _ => format!("{}: {stated}", origin(from)),
        }
    };
    let mut lines = String::new();
    if !refusal.considered.is_empty() {
        let heading = match refusal.conclusion {
            Conclusion::Counterexample(_) | Conclusion::Unsearched if refusal.absurd => {
                "the facts in scope do not refute each other:"
            }
            Conclusion::Exhausted(_) => "the facts in scope the search ran over:",
            Conclusion::Unwritten | Conclusion::Rejected => "it follows from the facts in scope:",
            Conclusion::Unsearched | Conclusion::Counterexample(_) => {
                "it does not follow from the facts in scope:"
            }
        };
        lines += &format!("\n  {heading}");
        for (from, statement) in &refusal.considered {
            lines += &format!("\n    {}", fact(from, statement));
        }
    }
    for (from, statement) in &refusal.unread {
        lines += &format!("\n  not read: {}", fact(from, statement));
    }
    match &refusal.conclusion {
        Conclusion::Counterexample(assignment) if !assignment.is_empty() => {
            let values = assignment
                .iter()
                .map(|(atom, value)| format!("{} = {value}", atom.spelled(spelling)))
                .collect::<Vec<_>>()
                .join(", ");
            lines += &match refusal.absurd {
                true => format!("\n  they all hold at {values}"),
                false => format!("\n  it fails at {values}"),
            };
        }
        Conclusion::Exhausted(cap) => {
            lines +=
                &format!("\n  the search for a proof ran out of its bound of {cap} derived rows");
        }
        Conclusion::Unwritten => lines += "\n  the lemmas a proof would use are not in scope here",
        Conclusion::Rejected => {
            lines += "\n  a proof was found and did not check, which is the compiler's fault: please report it";
        }
        Conclusion::Unsearched | Conclusion::Counterexample(_) => {}
    }
    lines
}

fn spelled_free(name: &Free, spelling: &Spelling) -> String {
    match name {
        Free::Global(global) => spelling.symbol(global),
        Free::Local(_) => name.hint().unwrap_or("_").to_string(),
    }
}

impl ErasedName {
    /// This name as its reader writes it: a declaration as resolution reaches it, a local binder by its label.
    fn spelled(&self, spelling: &Spelling) -> String {
        match self {
            ErasedName::Named(name) => spelled_free(name, spelling),
            ErasedName::Erased(name) => name.clone(),
        }
    }
}

impl ErasedSite {
    /// This site as a refusal's sentence names it, each declaration spelled for the reader.
    pub(crate) fn phrase(&self, spelling: &Rc<Spelling>) -> String {
        let quoted = |name: &Global| format!("'{}'", spelling.symbol(name));
        match self {
            ErasedSite::TypeOf(name) => format!("the type of {}", quoted(name)),
            ErasedSite::BodyOf(name) => format!("the body of {}", quoted(name)),
            ErasedSite::Within {
                definition,
                settled,
            } => {
                let place = match definition {
                    Some(name) => quoted(name),
                    None => "the entrypoint".to_string(),
                };
                match settled {
                    Some(what) => format!("{what} in {place}"),
                    None => place,
                }
            }
            ErasedSite::EntryType => "the entrypoint's type".to_string(),
            ErasedSite::ParameterOf(name) => format!("a parameter of {}", quoted(name)),
            ErasedSite::PayloadOf {
                family,
                constructor,
            } => format!("the payload of '{}/{constructor}'", spelling.symbol(family)),
            ErasedSite::FieldOf(name) => format!("a field of {}", quoted(name)),
            ErasedSite::Recursive(names) => format!(
                "the recursive definition {}",
                names.iter().map(quoted).collect::<Vec<_>>().join(", ")
            ),
            ErasedSite::LocallyRecursive(labels) => format!(
                "the recursive definition {}",
                labels
                    .iter()
                    .map(|label| format!("'{label}'"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ErasedSite::Bound { callee, binder } => callee.slot("bound", binder, spelling),
            ErasedSite::OperandBound(operation) => {
                format!("the bound of '{}'", operation.spelled(spelling))
            }
        }
    }
}

/// The declaration a report arose in, by the names it declares, each as resolution reaches it from where the reader stands. A witness has no name and is described: by its concept and key, or by its module where its signature spells neither.
pub(super) fn declared(names: &[Global], spelling: &Spelling) -> String {
    names
        .iter()
        .map(|name| match name {
            Global::Authored(_) => spelling.symbol(name),
            Global::Witness(id) => match id.spelled() {
                Some((concept, key)) => Callee::Witness { concept, key }.phrase(spelling),
                None => format!("the witness in {}", declaring_module(id.module())),
            },
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// A callee identity spelled the same way, for the reports that carry one whole rather than destructured.
fn spelled_callee(callee: &CalleeId, spelling: &Spelling) -> String {
    match callee {
        CalleeId::Function(name) => spelled_free(name, spelling),
        CalleeId::Constructor { owner, tag } => format!("{}/{tag}", spelling.symbol(owner)),
        CalleeId::Witness(global) | CalleeId::Structure(global) => spelling.symbol(global),
        CalleeId::Operator(op) => op.symbol().to_string(),
        CalleeId::Anonymous => "<function>".to_string(),
    }
}

impl Callee {
    /// This callee as a report's sentence names it: a function quoted as written, an operator and a witness described, since neither has a name a program writes.
    pub(crate) fn phrase(&self, spelling: &Spelling) -> String {
        match self {
            Callee::Function(name) => format!("'{}'", spelled_free(name, spelling)),
            Callee::Constructor { owner, tag } => {
                format!("'{}/{tag}'", spelling.symbol(owner))
            }
            Callee::Anonymous => "'<function>'".to_string(),
            Callee::Structure(name) => format!("'{}'", spelling.symbol(name)),
            Callee::Operator { op, .. } => format!("the '{}' operator", op.symbol()),
            Callee::Witness { concept, key } => format!(
                "the witness of '{}' for {} '{}'",
                spelling.symbol(concept),
                key_noun(key),
                key.spelled(spelling)
            ),
        }
    }

    /// One of this callee's slots as a report names it, `the bound 'ok' of '/f'`. An operator's slot is named by the operator alone: its binder is minted, and `'_'` names nothing a reader can find.
    pub(crate) fn slot(&self, noun: &str, binder: &str, spelling: &Spelling) -> String {
        match self {
            Callee::Operator { .. } => format!("the {noun} of {}", self.phrase(spelling)),
            Callee::Function(_)
            | Callee::Constructor { .. }
            | Callee::Witness { .. }
            | Callee::Structure(_)
            | Callee::Anonymous => {
                format!("the {noun} '{binder}' of {}", self.phrase(spelling))
            }
        }
    }
}

/// An error paired with the [`Spelling`] its terms render under — the parameter `Display::fmt` cannot take, and the reason every arm below rebinds its term fields before interpolating them. A field left unrebound would silently render core's own spelling, which is why the rebinding is per-arm and mechanical rather than left to each `write!`.
///
/// The third is where the declarations a position lies in were written, for the three messages that state one in their own text: a goal batch rendered whole, a written goal a postponed conversion watched, and the other of two terms that decide one `Sort` apart. Empty where nobody knows, and the position is then left out.
pub(super) struct Displayed<'a>(
    pub(super) &'a Error,
    pub(super) Rc<Spelling>,
    pub(super) &'a [&'a Anchors],
);

impl fmt::Display for Displayed<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = &self.1;
        let anchors = self.2;
        // A watched metavariable by what it is.
        let named = |watched: &Watched| match watched {
            Watched::Slot {
                callee,
                noun,
                binder,
            } => callee.slot(noun, binder, spelling),
            Watched::Described(description) => description.clone(),
            // Where it was written is the report's note.
            Watched::Goal(_) => "a written goal `?`".to_string(),
        };
        match self.0 {
            Error::ReduceExhausted { term, .. } => {
                let term = term.spelled(spelling);
                write!(f, "reduction ran out of steps on: {term}")
            }
            Error::ConvertExhausted { this, that, .. } => {
                let that = that.spelled(spelling);
                let this = this.spelled(spelling);
                write!(f, "conversion ran out of steps between {this} and {that}")
            }
            Error::TypeMismatch {
                inferred,
                expected,
                declined,
                terms,
            } => {
                // Reports erase universe instances and splice nested concatenations, which reads better everywhere except here: when the instances or the grouping *are* the disagreement, both sides render as one string and the message states nothing. Detected on the rendering rather than on the terms, so it covers every axis that could collapse two sides into one spelling and not merely the ones that are known to. Grouping is tried first because it changes nothing where no nesting is present, whereas two polymorphic instances always differ once their levels are shown, so trying the universes first would answer with two metavariable ids where the nesting was the difference.
                let plain = (
                    inferred.spelled(spelling).to_string(),
                    expected.spelled(spelling).to_string(),
                );
                let detailed = (plain.0 == plain.1).then(|| {
                    let render = |spelling: Spelling| {
                        let spelling = Rc::new(spelling);
                        (
                            inferred.spelled(&spelling).to_string(),
                            expected.spelled(&spelling).to_string(),
                        )
                    };
                    let grouped = render(spelling.as_ref().clone().with_faithful_grouping());
                    if grouped.0 != grouped.1 {
                        return grouped;
                    }
                    let shown = render(spelling.as_ref().clone().with_shown_universes());
                    // Neither grouping nor universes was the axis that collapsed them: fall back to core's own spelling, which abbreviates nothing. Unreadable beside the shortened form, and better than a message that says two things are unequal in identical words.
                    match shown.0 == shown.1 {
                        false => shown,
                        true => render(Spelling::default()),
                    }
                });
                let (shown_inferred, shown_expected) = match detailed {
                    Some(detailed) => detailed,
                    None => plain,
                };
                match terms {
                    true => write!(
                        f,
                        "terms differ\n  written: {shown_inferred}\n  expected: {shown_expected}"
                    )?,
                    false => write!(
                        f,
                        "type mismatch\n  inferred: {shown_inferred}\n  expected: {shown_expected}"
                    )?,
                }
                // The collision every beginner meets once: a type handed over where a value of it belonged — `Eq/cong(f, Eq()(a, b))` with the statement in the proof's seat. The mismatch line already says `Prop` against `Eq()(…)`, but only to a reader who knows a proposition is a type and a proof is its inhabitant, which is the very thing they have not learned yet. One sentence names the level confusion; the `Type` twin is the same mistake with a type where a value belonged.
                let inferred_is_sort = matches!(&***inferred, Subterm::Sort(..));
                let expected_is_sort = matches!(&***expected, Subterm::Sort(..));
                if !terms && inferred_is_sort && !expected_is_sort {
                    let (what, held) = match &***inferred {
                        Subterm::Sort(Quality::Prop, _) => ("a proposition", "a proof of it"),
                        _ => ("a type", "a value of it"),
                    };
                    write!(f, "\n  {what} was given where {held} was expected")?;
                }
                match declined {
                    Some(Declined::Unpaired) => write!(
                        f,
                        "\n  not compared: no operand of one side of a commutative operation is an operand of the other, and operands are paired where they are one term, never by the order they stand in"
                    )?,
                    Some(Declined::PastCap) => write!(
                        f,
                        "\n  not compared: two Boolean terms over more than {BOOL_ATOM_CAP} atoms are past what the truth table that decides them reads"
                    )?,
                    None => {}
                }
                Ok(())
            }
            Error::RejectedCandidate {
                watched,
                candidate,
                error,
            } => {
                let candidate = candidate.spelled(spelling);
                let born = match watched {
                    Watched::Slot { .. } => "where the argument was left out",
                    Watched::Described(_) | Watched::Goal(_) => "where it stands",
                };
                write!(
                    f,
                    "{} cannot be {candidate}\n  it does not check {born}:",
                    named(watched)
                )?;
                // The check's own report, under whatever located or scoped it: the candidate is no term of the program, and where it failed is this report's place.
                let reason = Displayed(error.unwrapped(), Rc::clone(spelling), anchors).to_string();
                for line in reason.lines() {
                    write!(f, "\n    {line}")?;
                }
                Ok(())
            }
            // "this sequencing", not "this '!'": a hand-written '/std/Monad/bind' call reaches the same report, and nothing on the term records which spelling produced it.
            Error::StrandedSequencing { sequenced, region } => {
                let region = region.spelled(spelling);
                let needed = match sequenced {
                    Some(sequenced) => sequenced.spelled(spelling).to_string(),
                    None => "a monad".to_string(),
                };
                write!(
                    f,
                    "this sequencing needs a region whose type is {needed}\n  the region here has type {region}\n  '!' sequences within the nearest enclosing value body; a 'let' with a type annotation is a declaration, and its body is its own region"
                )
            }
            Error::UnembeddedAction { action, expected } => {
                let action = action.spelled(spelling);
                let expected = expected.spelled(spelling);
                write!(
                    f,
                    "an action of one monad where another is expected\n  action: {action}\n  expected: {expected}\n  an action is embedded through the declared Lift witness when its monad can be read from its head's declaration\n  write lift(action) to embed this one explicitly; that resolves the witness, or reports the edge to declare"
                )
            }
            Error::UniverseInconsistency { lower, upper, path } => {
                let names = level_meta_names([lower, upper]);
                write!(
                    f,
                    "this {} would need to be strictly below itself\n  required constraint: {} ≤ {}",
                    curios_core::inconsistent_sort(path),
                    renamed_level(lower, &names),
                    renamed_level(upper, &names),
                )?;
                for instantiated in curios_core::instantiated_on(path) {
                    write!(
                        f,
                        "\n  required by the bounds '{}' sets on the levels it is used at",
                        spelling.symbol(&instantiated)
                    )?;
                }
                if curios_core::names_a_proposition(path) {
                    write!(
                        f,
                        "\n  a proposition sits at the level of what it quantifies over and of what it holds, so it is none of the propositions it ranges over"
                    )?;
                }

                write!(
                    f,
                    "\n  levels are inferred and there is no syntax for one, so this cannot be annotated away\n  a definition is generalized only at the top level, so a local one used at two levels has to be hoisted to be used at both; a recursive group is monomorphic in its own levels, so a member's own calls constrain those levels rather than choosing them afresh",
                )
            }
            Error::UniverseInvariant(message) => {
                write!(f, "invalid inferred universe state: {message}")
            }
            Error::NotAFunction { head_type, reduced } => {
                let reduced = reduced.spelled(spelling).to_string();
                let head_type = head_type.spelled(spelling).to_string();
                write!(f, "applied a non-function\n  head has type: {head_type}")?;
                // The reduced form is not spelled: it is the unfolded definition stuck on its argument, and the written type is what the reader can act on.
                if reduced != head_type {
                    write!(f, "\n  which does not reduce to a function type")?;
                }
                Ok(())
            }
            Error::NotAFunctionType { expected, argument } => {
                let expected = expected.spelled(spelling);
                write!(
                    f,
                    "introduced a lambda where the expected type is not a function type\n  expected: {expected}"
                )?;
                if let Some(site) = argument {
                    let parameter = match &site.parameter {
                        Some(name) => format!("`{name}`, "),
                        None => String::new(),
                    };
                    let mark = match site.plicity {
                        Plicity::Explicit => "",
                        Plicity::Implicit => "'@' ",
                        Plicity::Witness => "'use' ",
                    };
                    write!(
                        f,
                        "\n  checked as {parameter}the {} {mark}argument of '{}'",
                        ordinal(site.position),
                        spelled_callee(&site.function, spelling)
                    )?;
                    if let Some((name, position)) = &site.function_typed {
                        let name = match name {
                            Some(name) => format!("`{name}`, "),
                            None => String::new(),
                        };
                        write!(
                            f,
                            "\n  '{}' takes a function as {name}its {} argument",
                            spelled_callee(&site.function, spelling),
                            ordinal(*position)
                        )?;
                    }
                }
                Ok(())
            }
            Error::NotATuple { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(
                    f,
                    "projected from a non-tuple\n  head has type: {head_type}"
                )
            }
            Error::NotATupleType {
                expected,
                unit_for_a_type,
            } => {
                let expected = expected.spelled(spelling);
                write!(
                    f,
                    "introduced a tuple where the expected type is not a tuple type\n  expected: {expected}"
                )?;
                match unit_for_a_type {
                    true => write!(
                        f,
                        "\n  `()` is the unit value, and a type stands here: the unit type is written `{{}}`"
                    ),
                    false => Ok(()),
                }
            }
            Error::TupleArityMismatch { expected, got } => {
                write!(
                    f,
                    "tuple has {got} field(s) but expected type has {expected}"
                )
            }
            Error::TupleIndexOutOfBounds { index, arity } => {
                write!(f, "tuple index {index} out of bounds (arity {arity})")
            }
            Error::UnknownTupleLabel { label, available } => {
                if available.is_empty() {
                    write!(
                        f,
                        "no field named '{label}' (the tuple type has no labeled fields)"
                    )
                } else {
                    write!(
                        f,
                        "no field named '{label}' (available: {})",
                        available.join(", ")
                    )
                }
            }
            Error::DuplicateTupleLabel { label } => {
                write!(f, "duplicate field label '{label}' in tuple type")
            }
            Error::DuplicateInduct { name } => {
                let name = spelling.symbol(name);
                write!(f, "duplicate inductive declaration '{name}'")
            }
            Error::DuplicateStruct { name } => {
                let name = spelling.symbol(name);
                write!(f, "duplicate struct declaration '{name}'")
            }
            Error::DuplicateConcept { name } => {
                let name = spelling.symbol(name);
                write!(f, "duplicate concept declaration '{name}'")
            }
            Error::TupleFieldNameMismatch {
                written,
                expected,
                position,
            } => {
                if expected.is_empty() {
                    write!(
                        f,
                        "field {position} is named '{written}' but the expected type has no label there"
                    )
                } else {
                    write!(
                        f,
                        "field {position} is named '{written}' but the expected type calls it '{expected}'"
                    )
                }
            }
            Error::NotNatType { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(f, "expected Nat but got {head_type}")
            }
            Error::NotBoolType { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(f, "expected Bool but got {head_type}")
            }
            Error::NotListType { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(f, "expected List but got {head_type}")
            }
            Error::NotBinType { grain, head_type } => {
                let head_type = head_type.spelled(spelling);
                let expected = match grain {
                    Grain::B => "Bits",
                    Grain::X => "Bytes",
                };

                write!(f, "expected {expected} but got {head_type}")
            }
            Error::WrongNumberOfArguments {
                expected,
                got,
                answers_a_function,
            } => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, got {got}"
                )?;
                match answers_a_function {
                    true => write!(
                        f,
                        "\n  a call fills one parameter list, and what this one answers is itself a function: the rest are a call of their own, `f(a)(b)`"
                    ),
                    false => Ok(()),
                }
            }
            Error::SurplusFuncBinders { surplus, slots } => {
                let binders = match surplus {
                    1 => "1 binder that claims no parameter".to_string(),
                    surplus => format!("{surplus} binders that claim no parameter"),
                };
                let claimed = match slots {
                    0 => "the expected type has no parameters".to_string(),
                    1 => "the expected type's only parameter is already claimed".to_string(),
                    slots => {
                        format!("all {slots} of the expected type's parameters are already claimed")
                    }
                };

                write!(
                    f,
                    "this function writes {binders}: a binder binds the next parameter of its run, and {claimed}"
                )
            }
            Error::BinderPlicityMismatch {
                site,
                position,
                binder,
                expected,
                written,
            } => {
                let member = match site {
                    BinderSite::Parameter => "parameter",
                    BinderSite::Payload { .. } => "payload",
                };
                let requirement = match expected {
                    Plicity::Explicit => format!("an explicit {member} (written with no mark)"),
                    Plicity::Implicit => format!("an implicit {member} (written with `@`)"),
                    Plicity::Witness => format!("a witness {member} (written with `use`)"),
                };
                let written = match written {
                    Plicity::Explicit => "written with no mark",
                    Plicity::Implicit => "written with `@`",
                    Plicity::Witness => "written with `use`",
                };
                let named = match binder.is_empty() {
                    true => String::new(),
                    false => format!(" '{binder}'"),
                };
                match site {
                    BinderSite::Parameter => write!(
                        f,
                        "function parameter {position}{named} is {requirement}, but was {written}"
                    ),
                    BinderSite::Payload { constructor } => write!(
                        f,
                        "payload {position}{named} of '{constructor}' is {requirement}, but was {written}"
                    ),
                }
            }
            Error::UnknownMatchConstructor {
                type_name,
                tag,
                constructors,
            } => {
                let type_name = spelling.symbol(type_name);
                write!(f, "match arm '{tag}' is not a constructor of '{type_name}'")?;
                match constructors.is_empty() {
                    true => write!(f, "\n  it has no constructors"),
                    false => write!(f, "\n  its constructors are {}", constructors.join(", ")),
                }
            }
            Error::MatchCaseMissing { type_name, tag } => {
                let type_name = spelling.symbol(type_name);
                write!(
                    f,
                    "missing match case for constructor '{tag}' of '{type_name}'"
                )
            }
            Error::NotAInductType { head_type, no_arms } => {
                let head_type = head_type.spelled(spelling);
                match no_arms {
                    true => write!(
                        f,
                        "a match with no arms eliminates only a type with no constructors\n  head has type: {head_type}"
                    ),
                    false => write!(
                        f,
                        "matched inductive constructors on a non-inductive type\n  head has type: {head_type}"
                    ),
                }
            }
            Error::SortDecided { prop, at_decider } => {
                let sort = match prop {
                    true => "Prop",
                    false => "Type",
                };
                match at_decider {
                    true => write!(
                        f,
                        "this decides a `Sort` its declaration leaves to a caller: it can only be `{sort}`"
                    )?,
                    false => write!(
                        f,
                        "this `Sort` can only be `{sort}`: its declaration decides it"
                    )?,
                }
                write!(
                    f,
                    "\n  a sort left to a caller is one each use states, so one the declaration decides is written `{sort}`"
                )
            }
            // The other term is the report's note, where its check was recorded.
            Error::SortDecidedTwice { prop, .. } => {
                let (here, there) = match prop {
                    true => ("Prop", "Type"),
                    false => ("Type", "Prop"),
                };
                write!(
                    f,
                    "this decides a `Sort` its declaration leaves to a caller as `{here}`, and another of its terms decides it as `{there}`\n  a sort is one of the two wherever it is stated, so no `Sort` serves both terms"
                )
            }
            Error::TooManySorts { stated } => {
                write!(
                    f,
                    "this declaration leaves {stated} sorts to its caller, and one leaves at most {}\n  a declaration is checked once at every way of stating them",
                    curios_core::UniverseContext::QUALITY_LIMIT
                )
            }
            Error::RecordOverTwoSorts => {
                write!(
                    f,
                    "this tuple type stands over two sorts each left to a caller\n  it is a proposition where both are `Prop` and a type otherwise, so it has no one sort: state one of the two as `Type` or `Prop`"
                )
            }
            Error::LargeElimOfProp { name } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "cannot eliminate the proposition '{name}' into a relevant result\n  a proposition is matched into data only where no constructor applies, or where it declares one whose every payload its indices determine"
                )
            }
            Error::InformativePropStruct {
                name,
                field,
                field_type,
            } => {
                let name = spelling.symbol(name);
                let field_type = field_type.spelled(spelling);
                write!(
                    f,
                    "struct '{name}' is declared at sort 'Prop' but field '{field} : {field_type}' is informative\n  a 'Prop' struct's fields must all be propositions (or forced by indices)"
                )
            }
            Error::NotStrictlyPositive {
                name,
                part,
                site_type,
                polarity,
            } => {
                let name = spelling.symbol(name);
                let site_type = site_type.spelled(spelling);
                let through = match part {
                    DeclaredPart::Payload {
                        constructor,
                        member: MemberLabel::Written(label),
                    } => format!("the payload '{label}' of '{constructor}', a '{site_type}',"),
                    DeclaredPart::Payload {
                        constructor,
                        member: MemberLabel::Place(place),
                    } => format!(
                        "the {} payload of '{constructor}', a '{site_type}',",
                        ordinal(*place)
                    ),
                    DeclaredPart::Field(MemberLabel::Written(label)) => {
                        format!("its field '{label}', a '{site_type}',")
                    }
                    DeclaredPart::Field(MemberLabel::Place(place)) => {
                        format!("its {} field, a '{site_type}',", ordinal(*place))
                    }
                    DeclaredPart::Whole => "its own declaration".to_string(),
                };
                write!(
                    f,
                    "'{name}' is not strictly positive: through {through} it occurs in itself {polarity}\n  a recursive occurrence must be a plain payload, never left of an arrow"
                )
            }
            Error::PartialInErasedPosition {
                erased,
                site,
                offender,
            } => {
                let advice = match erased {
                    Erased::Type => {
                        "everything a type reaches must terminate, or type formation may not"
                    }
                    Erased::Proof => {
                        "erasure deletes proofs, so a proof that may not terminate proves anything"
                    }
                };
                let site = site.phrase(spelling);
                match offender {
                    Some(offender) => write!(
                        f,
                        "{site} is a {erased} position but reaches '{}', which is not known to terminate\n  {advice}",
                        spelling.symbol(offender)
                    ),
                    None => write!(
                        f,
                        "{site} is a {erased} position but does not terminate on every input\n  {advice}"
                    ),
                }
            }
            Error::NotAStructType { found } => {
                let found = found.spelled(spelling);
                write!(f, "expected a struct type here\n  found: {found}")
            }
            Error::StructArityMismatch {
                name,
                expected,
                got,
            } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "struct '{name}' takes {expected} type argument(s) but got {got}"
                )
            }
            Error::WrongNumberOfFields {
                name,
                expected,
                got,
                missing,
                surplus,
            } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "struct '{name}' has {expected} field(s) but the literal supplies {got}"
                )?;
                if !missing.is_empty() {
                    write!(f, "\n  missing: {}", missing.join(", "))?;
                }
                if !surplus.is_empty() {
                    write!(f, "\n  no such field: {}", surplus.join(", "))?;
                }
                Ok(())
            }
            Error::UnknownDeclaration { name } => {
                let name = spelling.symbol(name);
                write!(f, "no declaration for '{name}'")
            }
            Error::UnknownStructField {
                name,
                label,
                available,
            } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "struct '{name}' has no field '{label}' at that position (fields in order: {})",
                    available.join(", ")
                )
            }
            Error::UseEntryOutsideConcept { name } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "'use' entries are only legal in concept literals — struct '{name}' is not a concept"
                )
            }
            Error::SpreadNotFirst { name } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "a '..' spread must be the first entry of the '{name}' literal"
                )
            }
            Error::MultipleSpreads { name } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "a struct literal takes at most one '..' spread, but the '{name}' literal has more"
                )
            }
            Error::SpreadBaseTypeMismatch { name, found } => {
                let name = spelling.symbol(name);
                let found = found.spelled(spelling);
                write!(
                    f,
                    "the '..' base of a '{name}' literal must itself be a '{name}'\n  found: {found}"
                )
            }
            Error::UnlabeledSpreadOverride { name } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "overrides after a '..' spread must be labeled ('field = value') in the '{name}' literal"
                )
            }
            Error::SpreadOverrideOutOfOrder { name, label, order } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "overrides after a '..' spread must follow '{name}''s declared field order ({}); '{label}' is repeated or out of place",
                    order.join(", ")
                )
            }
            Error::PrivateField { name, field } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "field '{field}' of struct '{name}' is private to its declaring module and its descendants"
                )
            }
            Error::PrivateRepresentation { name } => {
                let name = spelling.symbol(name);
                write!(
                    f,
                    "the representation of type '{name}' is private to its declaring module and its descendants"
                )
            }
            Error::NoDerivation { concept } => {
                let concept = spelling.symbol(concept);
                write!(
                    f,
                    "no derivation exists for '{concept}'; write the body\n  `{{ .. }}` asks the compiler to write the witness, which it does only for a concept registered as derivable"
                )
            }
            Error::DeriveOutsideWitness => {
                write!(
                    f,
                    "a derived body is legal only as a witness body checked against its concept application"
                )
            }
            Error::Underivable {
                concept,
                key,
                reason,
            } => {
                let concept = spelling.symbol(concept);
                let key = key.spelled(spelling);
                write!(f, "cannot derive '{concept}' for {key}")?;
                match reason {
                    Underivable::NotDeclared => write!(
                        f,
                        "\n  {key} is not a declared `induct` or `struct`, and a derivation writes out a declaration's constructors and nothing else; write the body"
                    ),
                    Underivable::Concept => {
                        write!(f, "\n  {key} is a concept's record; write the body")
                    }
                    Underivable::TypeValued {
                        constructor,
                        payload,
                    } => write!(
                        f,
                        "\n  {payload} of '{constructor}' is a type, which no value spells; write the body"
                    ),
                }
            }
            Error::CtorArityMismatch {
                atom,
                expected,
                got,
            } => {
                write!(
                    f,
                    "constructor '{atom}' takes {expected} argument(s) but the match arm binds {got}"
                )
            }
            Error::MatrixInconsistentShape => {
                write!(
                    f,
                    "match arm patterns disagree on shape for the same column"
                )
            }
            Error::MatrixDuplicateRow { covered } => {
                write!(
                    f,
                    "duplicate or overlapping match arm: every arm must be reachable and distinct"
                )?;
                match covered {
                    Some(covered) => write!(
                        f,
                        ", and no value reaches this one past the arms before it, `{covered}` among them"
                    ),
                    None => Ok(()),
                }
            }
            Error::MatrixUncovered { value, indexed } => {
                write!(f, "no arm matches `{value}`")?;
                match indexed {
                    true => write!(
                        f,
                        ": its index target is not provably impossible at the scrutinee's indices — write the arm"
                    ),
                    false => Ok(()),
                }
            }
            Error::MatrixArmsMeet {
                value,
                first,
                second,
            } => {
                write!(
                    f,
                    "the arms `{first}` and `{second}` both match `{value}`, and neither is a case of the other: arm order decides nothing, so write an arm for `{value}` before them"
                )
            }
            Error::MatrixStructHead { head, scrutinee } => {
                let scrutinee = scrutinee.spelled(spelling);
                write!(
                    f,
                    "a `{head}` pattern matches a value of that structure, and this one is of type {scrutinee}"
                )
            }
            Error::MatrixMixedNatDispatch => {
                write!(
                    f,
                    "a `Nat` match arm mixes successor-peeling (`n + 1; ih`) with literal dispatch (`5`) in one column; use one or the other"
                )
            }
            Error::MatrixMixedBinGrain => {
                write!(f, "binary match arms mix `b[…]` and `x[…]` patterns")
            }
            // A name a program wrote is reported by the reason the lowering filed for it, in that value's own words, so a misspelled name reads the same whichever stage refuses for it.
            Error::UnboundVariable { why: Some(why), .. } => write!(f, "{why}"),
            Error::UnboundVariable { term, why: None } => {
                let term = term.spelled(spelling);
                write!(f, "`{term}` is not in scope")
            }
            Error::CannotInfer => {
                write!(f, "cannot infer type of expression")
            }
            Error::PostponedCheck { expected } => {
                let expected = expected.spelled(spelling);
                write!(
                    f,
                    "cannot check expression: its expected type never gained structure: {expected}"
                )
            }
            Error::PostponedConversion {
                this,
                that,
                watching,
                under_refinements,
            } => {
                let this = this.spelled(spelling).to_string();
                let that = that.spelled(spelling).to_string();
                // Two bare holes name nothing a reader can act on; the metavariables that never solved are then the whole message.
                let both_bare = this == "?" && that == "?";
                match both_bare && !watching.is_empty() {
                    true => write!(f, "cannot decide a postponed conversion")?,
                    false => write!(
                        f,
                        "cannot decide a postponed conversion\n  between: {this}\n      and: {that}"
                    )?,
                }
                if !watching.is_empty() {
                    let watching = watching.iter().map(named).collect::<Vec<_>>();
                    write!(f, "\n  never solved: {}", watching.join(", "))?;
                }
                if *under_refinements {
                    write!(
                        f,
                        "\n  the goal sits under match-arm refinements; a solution holding only under them is never committed"
                    )?;
                }
                Ok(())
            }
            Error::AmbiguousSpines {
                watched,
                one,
                another,
            } => {
                let one = one.spelled(spelling);
                let another = another.spelled(spelling);
                write!(
                    f,
                    "two types each give {} a value, and the values differ\n      one: {one}\n  another: {another}\n  writing it says which is meant",
                    named(watched)
                )
            }
            Error::BangRegionUndetermined => {
                write!(
                    f,
                    "the monad of this region was never determined\n  a '!' sequences in its region's monad, which is read from the region's type\n  annotate the enclosing result type to fix the monad"
                )
            }
            Error::BangRegionNotAMonad { region } => {
                let region = region.spelled(spelling);

                write!(
                    f,
                    "this region's type is {region}, which is no monad\n  a '!' sequences in its region's monad, which is read from the region's type\n  a function whose result is not a monad has nowhere to sequence one"
                )
            }
            Error::OperatorUndefined { symbol, type_ } => {
                let type_ = type_.spelled(spelling);
                write!(f, "operator '{symbol}' is not defined for type {type_}")
            }
            Error::UninferredImplicit {
                callee,
                binder,
                bound,
                proposition,
                reduct,
                refusal,
            } => {
                let bound = bound.spelled(spelling);
                // A proposition is a bound, and what was asked for is the fact nothing established. Anything else is a value or a type the arguments and the expectation left undetermined, which is a different fault with a different remedy.
                let why = match proposition {
                    true => {
                        let nothing = match reduct {
                            // An inductive proposition as written reduces to itself, and a report that said so would say the bound twice.
                            Some(reduct) => {
                                let reduct = reduct.spelled(spelling).to_string();
                                match reduct == bound.to_string() {
                                    true => format!("nothing discharged {bound}"),
                                    false => format!(
                                        "nothing discharged {bound}, which reduces to {reduct}"
                                    ),
                                }
                            }
                            None => format!("nothing discharged {bound}"),
                        };
                        match refusal {
                            Some(refusal) => nothing + &refused(refusal, spelling),
                            None => nothing,
                        }
                    }
                    false => {
                        // A literal has fields where a call has arguments.
                        let given = match callee {
                            Callee::Structure(_) => "field",
                            _ => "argument",
                        };
                        format!("no {given} or expected type determined it (its type is {bound})")
                    }
                };
                // A bound the search read is one the facts in scope may prove, so the way out named first is a fact: a guard. One it read nothing in has the written proof alone.
                let searched =
                    *proposition && refusal.as_ref().is_some_and(|refusal| !refusal.is_silent());
                match callee {
                    // A literal's slot is a field, written in braces under `@`. One declared as its type alone has no name to quote.
                    Callee::Structure(name) => {
                        let name = spelling.symbol(name);
                        let field = match binder.as_str() {
                            "_" => "a hidden field".to_string(),
                            binder => format!("hidden field '{binder}'"),
                        };
                        let guard = match searched {
                            true => "decide the bound with a guard before the literal, or ",
                            false => "",
                        };
                        write!(
                            f,
                            "{field} of '{name}' was not filled\n  {why}\n  {guard}write it explicitly: {name} {{ @... }}"
                        )
                    }
                    Callee::Function(func) => {
                        let func = spelled_free(func, spelling);
                        let guard = match searched {
                            true => "decide the bound with a guard before the call, or ",
                            false => "",
                        };
                        write!(
                            f,
                            "implicit argument '{binder}' of '{func}' was not inferred\n  {why}\n  {guard}supply it explicitly: {func}(@...)"
                        )
                    }
                    // An operator has no argument list to write the bound into, so the report names the two places it can be established: a guard before the operation, or the method call the operator stands for — with its type argument first, since a leading `@` fills that slot and not the bound.
                    Callee::Operator { method, .. } => write!(
                        f,
                        "{} was not discharged\n  {why}\n  an operator takes no written arguments: decide the bound with a guard before the operation, or call {}(@T, a, b, @proof)",
                        callee.slot("bound", binder, spelling),
                        spelling.symbol(method)
                    ),
                    Callee::Constructor { .. } => write!(
                        f,
                        "implicit argument '{binder}' of {} was not inferred\n  {why}\n  supply it explicitly",
                        callee.phrase(spelling)
                    ),
                    // A head the program gave no name to: a computed function, or a projection out of a recursive group. There is nothing to quote, so the advice names the slot rather than a callee it cannot spell.
                    Callee::Anonymous => write!(
                        f,
                        "{} was not inferred\n  {why}\n  supply it explicitly at the call site",
                        callee.slot("implicit argument", binder, spelling)
                    ),
                    // A witness is never called, so there is no argument to supply: its parameters are filled from the goal it answers.
                    Callee::Witness { .. } => write!(
                        f,
                        "{} was not inferred\n  {why}\n  a witness is never called: its parameters are filled from the goal it is resolved for",
                        callee.slot("implicit parameter", binder, spelling)
                    ),
                }
            }
            Error::ImplicitAtOtherSort {
                callee,
                binder,
                given,
                proposition,
            } => {
                let given = given.spelled(spelling);
                let slot = callee.slot("implicit argument", binder, spelling);
                match proposition {
                    true => write!(
                        f,
                        "a proposition was given where {slot} takes a type\n  {given} is a proposition, and no proposition stands where a Type is declared"
                    ),
                    false => write!(
                        f,
                        "a type was given where {slot} takes a proposition\n  {given} is a type, and no type stands where a Prop is declared"
                    ),
                }
            }
            Error::DomainNeverDetermined { binder } => {
                write!(
                    f,
                    "the type of parameter '{binder}' was never determined\n  a lambda's parameter type comes from its annotation, its body, or its position's expected type, and none supplied one\n  annotate the parameter: ({binder}: T) => ..."
                )
            }
            Error::WitnessMemberNeverStated => {
                write!(
                    f,
                    "this lambda writes `use _` and nothing states the member\n  a `use` member holds a place: its type is the expected function type's, and this lambda has none\n  write the function as a `let` with a telescope: let f(use C(A), ...) -> ... = ..."
                )
            }
            Error::HiddenMemberOutOfOrder {
                written,
                slot,
                binder,
            } => {
                let mark = |plicity: &Plicity| match plicity {
                    Plicity::Witness => "use",
                    Plicity::Implicit | Plicity::Explicit => "@",
                };
                let placeholder = match slot {
                    Plicity::Witness => "use _",
                    Plicity::Implicit | Plicity::Explicit => "@_",
                };
                let (written, slot) = (mark(written), mark(slot));
                let stands = match binder.is_empty() {
                    true => format!("a `{slot}` member"),
                    false => format!("the `{slot}` member '{binder}'"),
                };
                write!(
                    f,
                    "a `{written}` member is written where {stands} stands\n  hidden members are written in order from the first of their run, and the rest of the run may be left out\n  write `{placeholder}` first to leave that one to the elaborator"
                )
            }
            Error::HiddenMemberWithoutSlot { written } => {
                let written = match written {
                    Plicity::Witness => "use",
                    Plicity::Implicit | Plicity::Explicit => "@",
                };
                write!(
                    f,
                    "this `{written}` member has no slot\n  hidden members are written before the plain member they precede, and every hidden slot there is already written or left out"
                )
            }
            Error::DeclarationCycle { members } => {
                let named = members
                    .iter()
                    .map(|member| match member {
                        CycleMember::Named(name) => format!("'{}'", spelling.symbol(name)),
                        CycleMember::Witness { type_, .. } => {
                            format!("the witness of {}", type_.spelled(spelling))
                        }
                    })
                    .collect::<Vec<_>>();
                // Neither group's spelling holds a definition beside a witness, so a cycle through both is pointed at the two ways out it does have.
                let across = members
                    .iter()
                    .any(|member| matches!(member, CycleMember::Named(_)))
                    && members
                        .iter()
                        .any(|member| matches!(member, CycleMember::Witness { .. }));
                let way_out = match across {
                    true => {
                        "no group holds a definition beside a witness: let the definition call by its name the function it means, the witness delegating to it, or write its body in the witness, which may read its own methods"
                    }
                    false => {
                        "declare definitions that call one another as one group, 'let ... and ...', and witnesses that resolve through one another as 'satisfy C(A) { ... } and D(B) { ... }'"
                    }
                };
                match named.split_last() {
                    Some((last, rest)) if !rest.is_empty() => write!(
                        f,
                        "{} and {last} need one another\n  a declaration elaborates after what it reads, and each of these reads another before it can finish — by a name it writes, or by the witness a goal of its resolves to\n  a definition may call itself and a witness may resolve through its own entry, but a cycle through two has no order to elaborate them in\n  {way_out}",
                        rest.join(", ")
                    ),
                    _ => write!(
                        f,
                        "{} needs itself before it can be read\n  what its signature reads leads back to it",
                        named.join(", ")
                    ),
                }
            }
            Error::NoWitness {
                goal,
                callee,
                binder,
                need,
                shape,
            } => {
                let written = match need {
                    Some(Need::Embedding(diagnosis)) => diagnosis.goal.as_deref(),
                    _ => None,
                };
                let goal = written.unwrap_or(goal).spelled(spelling);
                write!(f, "no witness of {goal} found")?;
                if let Some(diagnosis) = shape {
                    let ShapeDiagnosis { wanted, bare } = &**diagnosis;
                    // A tuple shape is the only identity a twin can drop, so the sentence names labels and nothing decides between two of them: `diagnose_shape` bares tuple labels alone and answers `None` when that leaves the key unchanged.
                    write!(
                        f,
                        "\n  labels are part of the type: the witness for {bare} does not cover {wanted}\n  name a struct for the labeled product, or declare the witness for this shape"
                    )?;
                }
                match need {
                    None => write!(f, "\n  needed by {} for {binder}", callee.phrase(spelling)),
                    Some(Need::Closing(type_)) => write!(
                        f,
                        "\n  a program closes with a term whose type has one, and this one's type is {}",
                        type_.spelled(spelling)
                    ),
                    Some(Need::Embedding(diagnosis)) => {
                        let source = diagnosis.source.spelled(spelling).to_string();
                        let target = diagnosis.target.spelled(spelling);
                        let article = match source.trim_start_matches('/').chars().next() {
                            Some('A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u') => "an",
                            _ => "a",
                        };
                        write!(
                            f,
                            "\n  needed to sequence {article} {source} action in this {target} region"
                        )?;
                        if !diagnosis.source_is_monad {
                            write!(
                                f,
                                "\n  {source} is not a monad — no Monad witness exists for it, so no embedding out of it can be declared"
                            )?;
                        }
                        if !diagnosis.chain.is_empty() {
                            write!(
                                f,
                                "\n  declared embeddings chain from {source} to {target}:"
                            )?;
                            for (pair, module) in &diagnosis.chain {
                                let module = declaring_module(module);
                                write!(f, "\n    Lift{pair} — declared in {module}")?;
                            }
                            write!(
                                f,
                                "\n  embeddings never chain automatically; declare the composite Lift({source}, {target}) edge beside one of the two monads"
                            )?;
                        }
                        Ok(())
                    }
                }
            }
            Error::Goal {
                scope,
                goal,
                solution,
            } => {
                // Rendered as a one-element batch so the safety-net spelling and the compile path's [`Error::Goals`] can never drift; this form carries no occurrence extent or candidates of its own.
                // Its reader position is the enclosing `InDeclaration`'s and `InScope`'s, which the spelling handed here already stands at.
                let reader = spelling.reader();
                let report = GoalReport {
                    extent: None,
                    owner: reader.owner,
                    witnesses: Rc::clone(&reader.witnesses),
                    scope: scope.clone(),
                    goal: (**goal).clone(),
                    solution: solution.as_deref().cloned(),
                    obligations: Vec::new(),
                    candidates: Vec::new(),
                    refusal: None,
                };
                Displayed(&Error::Goals(vec![report]), Rc::clone(spelling), anchors).fmt(f)
            }
            Error::Goals(reports) => {
                // Each entry is the single-goal turnstile idiom followed by its own snippet — message first, then location, matching how `reports` orders a `Located` diagnostic. Entries are separated by a blank line.
                for (index, report) in reports.iter().enumerate() {
                    if index > 0 {
                        write!(f, "\n\n")?;
                    }
                    let spelling = Rc::new(spelling.as_ref().clone().for_reader(ReaderPosition {
                        owner: report.owner,
                        witnesses: Rc::clone(&report.witnesses),
                    }));
                    f.write_str(&goal_text(report, &spelling))?;
                    if let Some(span) = report.extent.and_then(|extent| extent.resolved(anchors)) {
                        write!(f, "\n\n{}", span.render_snippet())?;
                    }
                }
                Ok(())
            }
            Error::DuplicateWitness {
                concept,
                key,
                first,
                second,
                ..
            } => {
                let concept = spelling.symbol(concept);
                let noun = key_noun(key);
                let key = key.spelled(spelling);
                // One clause when both sit in the same module, which is the common case while a program is being written: naming that module twice reads as two coordinates and is one. The carets on each declaration are what separate them.
                let where_ = match first == second {
                    true => format!("both are declared in {}", declaring_module(first)),
                    false => format!(
                        "one is declared in {}, another in {}",
                        declaring_module(first),
                        declaring_module(second)
                    ),
                };

                write!(
                    f,
                    "duplicate witness of '{concept}' for {noun} '{key}'\n  {where_}\n  every concept-{noun} pair has at most one witness, program-wide"
                )
            }
            Error::OrphanWitness {
                concept,
                key,
                witness,
            } => {
                let concept = spelling.symbol(concept);
                let noun = key_noun(key);
                let key = key.spelled(spelling);
                write!(
                    f,
                    "orphan witness of '{concept}' for {noun} '{key}', declared in {}\n  a witness may only be declared where the concept or a type in its {noun} is already declared",
                    declaring_module(witness)
                )
            }
            Error::AmbiguousWitness {
                goal,
                first,
                second,
            } => {
                let first = first.spelled(spelling);
                let goal = goal.spelled(spelling);
                let second = second.spelled(spelling);
                write!(
                    f,
                    "ambiguous witness of {goal}\n  both {first} and {second} match at the same superclass depth"
                )
            }
            Error::CyclicSuperclass { concept } => {
                let concept = spelling.symbol(concept);
                write!(
                    f,
                    "concept '{concept}' participates in a superclass cycle ('use'-marked fields must form an acyclic graph)"
                )
            }
            Error::UnknownSuperclass { concept, target } => {
                let concept = spelling.symbol(concept);
                let target = target.spelled(spelling);
                write!(
                    f,
                    "concept '{concept}' names '{target}' as a superclass, but '{target}' is not a concept application"
                )
            }
            Error::InvalidWitnessHead { position, head } => {
                let head = head.spelled(spelling);
                write!(
                    f,
                    "this witness cannot be keyed: its concept's parameter {n} reduces to {head}\n  every parameter's head must be an inductive, a struct, an intrinsic type, or a tuple type",
                    n = position + 1
                )
            }
            Error::WitnessKeyNotSpelled {
                concept,
                key,
                spelled,
            } => {
                let concept = spelling.symbol(concept);
                let noun = key_noun(key);
                let key = key.spelled(spelling);
                let spelled = match spelled {
                    Some(spelled) => format!(
                        "\n  as it is written, it would be keyed at '{}'",
                        spelled.spelled(spelling)
                    ),
                    None => String::new(),
                };
                write!(
                    f,
                    "this witness of '{concept}' is for {noun} '{key}', which its signature does not spell\n  a witness is known by its key before anything is elaborated, so each parameter of its concept is written as the type it is: a declared type, applied or not, a name for one, a tuple type, or a function to an applied type{spelled}\n  write '{key}' where the signature computes it"
                )
            }
            Error::ParameterlessWitnessConcept { concept } => {
                let concept = spelling.symbol(concept);
                write!(
                    f,
                    "this witness cannot be registered: concept '{concept}' has no parameters to key on\n  a global witness keys on its concept's parameter heads; supply a parameterless concept through a local 'use' binder instead"
                )
            }
            Error::WitnessNotAConcept { found } => {
                let found = found.spelled(spelling);
                write!(
                    f,
                    "this witness does not witness a concept\n  its annotation elaborates to: {found}"
                )
            }
            Error::UseParameterNotAConcept { found, proposition } => {
                let found = found.spelled(spelling);
                write!(
                    f,
                    "a 'use' parameter's type must be a concept application\n  found: {found}"
                )?;
                if *proposition {
                    write!(
                        f,
                        "\n  a proof is discharged through an implicit parameter: write '@' in place of 'use'"
                    )?;
                }
                Ok(())
            }
            Error::ImplicitConceptMemberUnfillable { found } => {
                let found = found.spelled(spelling);
                write!(
                    f,
                    "an '@' parameter at a concept's type that no later type mentions can never be filled\n  found: {found}\n  an '@' parameter is inferred from the types after it, and resolution fills only 'use' parameters\n  write 'use' in place of '@' to have it resolved, or no mark to have it passed"
                )
            }
            Error::NonRegularWitnessPremise { premise } => {
                let premise = premise.spelled(spelling);
                write!(
                    f,
                    "this witness has a non-regular premise: {premise}\n  every 'use' premise must be smaller than the witness's own concept application: its variables bound by the witness's parameters, none used more often than there, and fewer nodes in all"
                )
            }
            Error::ExplicitWitnessParam => {
                write!(
                    f,
                    "this witness declares an explicit parameter\n  witness parameters must be implicit ('@') or 'use' premises — nothing supplies explicit arguments during resolution"
                )
            }
            Error::NatCaseKeyOverflow { value } => {
                write!(
                    f,
                    "natural dispatch key {value} does not fit a branch table"
                )
            }
            Error::EvaluationCycle { steps } => {
                let quoted = steps
                    .iter()
                    .map(|(member, _)| format!("`{}`", member.spelled(spelling)))
                    .collect::<Vec<_>>();
                let listed = match quoted.split_last() {
                    Some((last, rest)) if !rest.is_empty() => {
                        format!("{} and {last}", rest.join(", "))
                    }
                    _ => quoted.join(", "),
                };
                let calls = steps
                    .iter()
                    .filter_map(|(_, through)| through.as_ref())
                    .map(|callee| format!("`{}`", callee.spelled(spelling)))
                    .collect::<Vec<_>>();
                let through = match calls.as_slice() {
                    [] => String::new(),
                    [call] => format!(" through a call to {call}"),
                    _ => format!(" through calls to {}", calls.join(", ")),
                };
                match steps.len() {
                    1 => write!(
                        f,
                        "{listed} evaluates itself{through} as it is initialized; a value may mention itself only under a lambda, where it is computed the first time it is read"
                    ),
                    _ => write!(
                        f,
                        "{listed} evaluate each other{through} as they are initialized; values may mention one another only under a lambda, where each is computed the first time it is read"
                    ),
                }
            }
            Error::InitializerPerformsEffect { member, through } => {
                let through = match through {
                    Some(callee) => {
                        format!(" through a call to `{}`", callee.spelled(spelling))
                    }
                    None => String::new(),
                };
                write!(
                    f,
                    "the initializer of `{}` performs an effect{through}; a recursive value is computed when it is first read, or never, so its initializer may perform none",
                    member.spelled(spelling)
                )
            }
            Error::MotiveBinderCount {
                name,
                expected,
                written,
            } => {
                let subject = match name {
                    Some(name) => format!("eliminating '{}'", spelling.symbol(name)),
                    None => "this eliminator".to_string(),
                };
                let shape = match expected {
                    1 => "just the scrutinee".to_string(),
                    2 => "one index, then the scrutinee".to_string(),
                    n => format!("{} indices, then the scrutinee", n - 1),
                };
                write!(
                    f,
                    "motive binds {written} name(s), but {subject} needs {expected} ({shape})"
                )
            }
            Error::MissingArmNotImpossible { tag } => {
                write!(
                    f,
                    "missing arm '{tag}': its index target is not provably impossible at the scrutinee's indices — write the arm"
                )
            }
            Error::BinGetOutOfBounds { len, index } => {
                write!(f, "Bin.get index {index} out of bounds (length {len})")
            }
            Error::BinSliceOutOfRange { len, start, length } => {
                write!(
                    f,
                    "Bin.slice window of {length} at {start} out of range (length {len})"
                )
            }
            Error::ListGetOutOfBounds { len, index } => {
                write!(f, "List.get index {index} out of bounds (length {len})")
            }
            Error::ListSliceOutOfRange { len, start, length } => {
                write!(
                    f,
                    "List.slice window of {length} at {start} out of range (length {len})"
                )
            }
            Error::DivisionByZero { kind } => {
                write!(f, "division by zero in {kind}")
            }
            Error::IntToNatNegative { value } => {
                write!(f, "Int/to_nat of {value}, a value no Nat holds")
            }
            Error::NatToByteAbove { value } => {
                write!(f, "Nat/to_byte of {value}, a value no Byte holds")
            }
            Error::ByteLiteralOutOfRange { value } => {
                write!(
                    f,
                    "Byte literal {value} is out of range: a Byte is 0 through 255"
                )
            }
            Error::BoolLiteralOutOfRange { value } => {
                write!(f, "Bool literal {value} is out of range: a Bool is 0 or 1")
            }
            Error::FltLiteralOutOfRange { value } => {
                write!(f, "Flt literal {value} overflows the finite range")
            }
            Error::Poisoned => {
                write!(f, "a witness a refused declaration held was needed")
            }
            Error::Unfinished => {
                write!(f, "a declaration that has not elaborated was read")
            }
            // `render_body` intercepts both wrappers before a real spelling ever reaches this match, but these arms must not rely on that: interpolating `{error}` would route through `Display for Error`, silently resetting a nested term's spelling to core's default. Recurse with the spelling in hand instead.
            Error::InDeclaration { names, error } => {
                writeln!(f, "while elaborating {}:", declared(names, spelling))?;
                Displayed(error, Rc::clone(spelling), anchors).fmt(f)
            }
            Error::Located { error, .. } | Error::InScope { error, .. } => {
                Displayed(error, Rc::clone(spelling), anchors).fmt(f)
            }
            Error::InUnreachableArm { dead, error } => {
                Displayed(error, Rc::clone(spelling), anchors).fmt(f)?;
                write!(f, "\n{}", unreachable_arm_note(dead, spelling))
            }
            Error::Batch(errors) => {
                // Every member's own rendering, separated by a blank line as a goal batch's entries are. Nothing is added around them: each member carries its own declaration and location.
                for (index, error) in errors.iter().enumerate() {
                    if index > 0 {
                        write!(f, "\n\n")?;
                    }
                    Displayed(error, Rc::clone(spelling), anchors).fmt(f)?;
                }
                Ok(())
            }
        }
    }
}
