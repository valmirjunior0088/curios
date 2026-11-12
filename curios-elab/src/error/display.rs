//! Rendering an [`Error`] under a [`Spelling`]: the per-arm prose every diagnostic carries, and the goal turnstile the elaborator's goal batches print.
//!
//! The rendering is separated from the error roster for size alone — [`Error`] and its inherent impl are already a long file — and the seam is exact: nothing here decides *what* went wrong, only how it reads. [`Displayed`] is the whole mechanism, and [`goal_said`] is the one piece the roster itself reaches for, when it turns a goal batch into per-goal [`Report`]s.

#[cfg(test)]
mod tests;

use {
    super::{
        BinderSite, Callee, CycleMember, DeadArm, Declined, ErasedSite, Error, GoalReport, Need,
        RegionOwner, ShapeDiagnosis, Underivable, Watched,
    },
    crate::{Conclusion, Origin, Refusal, ordinal},
    curios_analysis::{DeclaredPart, Erased, MemberLabel},
    curios_common::{Kind, LINE_WIDTH, Plicity, Qualifier, Report, Said, Style},
    curios_core::{
        Anchors, CalleeId, Global, Level, Mention, Quality, ReduceError, Sort, Subterm, Term,
        UniverseMetaId, WitnessKey,
    },
    curios_surface::{ReaderPosition, Spellable, Spelling},
    std::{collections::HashMap, fmt, rc::Rc},
};

/// What a spent budget was refused, beneath the report it ends: the row of the charge it could not afford, with what that charge was and what was left, as the certifier names it.
fn spent(f: &mut Said, refusal: &ReduceError) {
    if let ReduceError::Exhausted {
        category,
        remaining,
        attempted,
    } = refusal
    {
        f.stating(format_args!(
            "{category} needed {attempted} units with {remaining} left"
        ));
    }
}

/// What a report on a dead arm adds, a fact each: that the arm is never taken and what says so — its own guard, the guard of an arm around it that its case contradicts, or its scrutinee's indices, which also say it may be left out.
pub fn unreachable_arm_note(said: &mut Said, dead: &DeadArm, spelling: &Rc<Spelling>) {
    said.stating("this arm is never taken");
    match dead {
        DeadArm::Guard { guard, case } => said.stating(format_args!(
            "its guard {} is always {}",
            guard.spelled(spelling),
            case.spelled(spelling),
        )),
        DeadArm::EnclosingGuard { guard, case } => said.stating(format_args!(
            "in it the guard {} is always {}",
            guard.spelled(spelling),
            case.spelled(spelling),
        )),
        DeadArm::Indices { case, scrutinee } => {
            said.stating(format_args!(
                "its case has type {} and its scrutinee has type {}",
                case.spelled(spelling),
                scrutinee.spelled(spelling),
            ));
            said.stating("the arm may be left out");
        }
    }
}

/// What a report on an error one alternative of an arm raised adds, a fact each: which alternative the arm's body was typed under, and that it is typed under each.
pub fn alternative_note(said: &mut Said, written: &str) {
    said.stating(format_args!("under the alternative `{written}`"));
    said.stating("an arm's body is typed for each alternative apart from the others");
}

/// `count` of what `noun` names, the noun in the number the count takes.
fn counted(count: usize, noun: &str) -> String {
    match count {
        1 => format!("1 {noun}"),
        count => format!("{count} {noun}s"),
    }
}

/// What one goal's report says, under its own rename map (see [`GoalReport::rename_map`]) — the batch-wide one the caller installed is replaced, not extended, since every name this report shows is in the narrower map by construction.
///
/// **What is said is the answer to what a goal asks.** A goal asks what goes where it stands, so the line is the type expected there, or, where the compiler worked out what stands there, what it inferred; a term too wide for a line is said as a fact beneath a line that says only which of the two it is. The facts are the scope, what the search for a bound said, the equations the goal is held to, and the candidates that fit.
pub fn goal_said(report: &GoalReport, spelling: &Rc<Spelling>) -> Said {
    // A report's terms render within the width a line is written within — the pipeline is pure and stays terminal-blind, so the target is a constant — and a broken term's continuation lines indent under the clause that holds it rather than restarting at its margin.

    let spelling = Rc::new(
        spelling
            .as_ref()
            .clone()
            .with_pretty_names(report.rename_map(spelling)),
    );
    let clause = |term: &Term| {
        term.spelled(&spelling)
            .within(LINE_WIDTH)
            .to_string()
            .replace('\n', "\n  ")
    };
    let goal = clause(&report.goal);
    let solution = report.solution.as_ref().map(clause);
    let narrow = |term: &String| !term.contains('\n');

    // Each of the two is said in the line where it fits one, and as a fact where it does not.
    let (mut said, typed, solved) = match &solution {
        Some(solution) if narrow(solution) => {
            (Said::line(format!("`{solution}` inferred")), true, false)
        }
        Some(_) => (Said::line("a term is inferred"), true, true),
        None if narrow(&goal) => (Said::line(format!("`{goal}` expected")), false, false),
        None => (Said::line("a term is expected"), true, false),
    };
    for (name, type_) in &report.scope {
        let shown = match unnameable_binder(name) {
            true => "_".to_string(),
            false => clause(name),
        };
        said.stating(format_args!("{shown} : {}", clause(type_)));
    }
    if typed {
        said.stating(format_args!("? : {goal}"));
    }
    if let Some(refusal) = &report.refusal {
        refused(&mut said, refusal, &spelling);
    }
    if let (true, Some(solution)) = (solved, &solution) {
        said.stating(format_args!("? = {solution}"));
    }
    for (this, that) in &report.obligations {
        said.stating(format_args!(
            "? such that {} \u{2261} {}",
            clause(this),
            clause(that)
        ));
    }
    for candidate in &report.candidates {
        said.stating(format_args!("? \u{2248} {}", clause(candidate)));
    }

    said
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
        false => format!("the module `{}`", module.join()),
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
    let mut text = level.faithful().to_string();
    // Longest first: `?u12` contains `?u1`, and replacing the shorter one first would leave `?u1` followed by a stray `2`.
    let mut metas = names.iter().collect::<Vec<_>>();
    metas.sort_by_key(|(meta, _)| std::cmp::Reverse(meta.to_string().len()));
    for (meta, name) in metas {
        text = text.replace(&meta.to_string(), name);
    }
    text
}

/// What the procedure that proves a bound from the facts in scope says beside a bound it refused, one line per clause under the bound's own: the facts it read, those it could not, and what its search concluded. A refusal that read nothing and searched nothing says nothing, so a bound the procedure reads nothing in reports as it would with no procedure.
fn refused(said: &mut Said, refusal: &Refusal, spelling: &Rc<Spelling>) {
    if refusal.is_silent() {
        return;
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
    if !refusal.considered().is_empty() {
        // A counterexample no program takes says what the search found, and nothing of the facts themselves.
        let heading = match refusal.conclusion() {
            Conclusion::Counterexample(_) if refusal.absurd() && refusal.refuted() => {
                "the search finds no contradiction among the facts in scope:"
            }
            Conclusion::Counterexample(_) | Conclusion::Unsearched if refusal.absurd() => {
                "the facts in scope do not refute each other:"
            }
            Conclusion::Counterexample(_) if refusal.refuted() => {
                "the search finds no proof from the facts in scope:"
            }
            Conclusion::Exhausted(_) => "the facts in scope the search ran over:",
            Conclusion::Unwritten | Conclusion::Rejected => "it follows from the facts in scope:",
            Conclusion::Unsearched | Conclusion::Counterexample(_) => {
                "it does not follow from the facts in scope:"
            }
        };
        // One fact: the heading, and each fact it heads on a line beneath it.
        said.stating(heading);
        for (from, statement) in refusal.considered() {
            said.text(format_args!("\n  {}", fact(from, statement)));
        }
    }
    for (from, statement) in refusal.unread() {
        said.stating(format_args!("not read: {}", fact(from, statement)));
    }
    match refusal.conclusion() {
        Conclusion::Counterexample(assignment) if !assignment.is_empty() => {
            let values = assignment
                .iter()
                .map(|(atom, value)| format!("{} = {value}", atom.spelled(spelling)))
                .collect::<Vec<_>>()
                .join(", ");
            said.stating(match (refusal.absurd(), refusal.refuted()) {
                (true, false) => format!("they all hold at {values}"),
                (false, false) => format!("it fails at {values}"),
                (true, true) => format!("the search finds them all holding at {values}"),
                (false, true) => format!("the search finds it failing at {values}"),
            });
            if refusal.refuted() {
                said.stating("each atom is read as a number of its own");
            }
        }
        Conclusion::Exhausted(cap) => said.stating(format_args!(
            "the search for a proof ran out of its bound of {cap} derived rows"
        )),
        Conclusion::Unwritten => {
            said.stating("the lemmas a proof would use are not in scope here");
        }
        Conclusion::Rejected => {
            said.stating("a proof was found and did not check");
            said.stating("that is the compiler's fault: please report it");
        }
        Conclusion::Unsearched | Conclusion::Counterexample(_) => {}
    }
}

fn spelled_mention(name: &Mention, spelling: &Spelling) -> String {
    match name {
        Mention::Declaration(declaration) => spelling.symbol(declaration),
        Mention::Local(local) => local.hint().unwrap_or("_").to_string(),
    }
}

impl ErasedSite {
    /// This site as a refusal's sentence names it, each declaration spelled for the reader.
    pub fn phrase(&self, spelling: &Rc<Spelling>) -> String {
        let quoted = |name: &Global| format!("`{}`", spelling.symbol(name));
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
            } => format!("the payload of `{}/{constructor}`", spelling.symbol(family)),
            ErasedSite::FieldOf(name) => format!("a field of {}", quoted(name)),
            ErasedSite::Recursive(names) => format!(
                "the recursive definition {}",
                names.iter().map(quoted).collect::<Vec<_>>().join(", ")
            ),
            ErasedSite::LocallyRecursive(labels) => format!(
                "the recursive definition {}",
                labels
                    .iter()
                    .map(|label| format!("`{label}`"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ErasedSite::Bound { callee, binder } => {
                callee.slot("bound", binder.as_deref(), spelling)
            }
            ErasedSite::OperandBound(operation) => {
                format!("the bound of `{}`", operation.spelled(spelling))
            }
        }
    }
}

/// A callee identity spelled the same way, for the reports that carry one whole rather than destructured.
fn spelled_callee(callee: &CalleeId, spelling: &Spelling) -> String {
    match callee {
        CalleeId::Function(name) => spelled_mention(name, spelling),
        CalleeId::Constructor { owner, tag } => format!("{}/{tag}", spelling.symbol(owner)),
        CalleeId::Method { concept, field } => format!("{}/{field}", spelling.symbol(concept)),
        CalleeId::Witness(global) | CalleeId::Structure(global) => spelling.symbol(global),
        CalleeId::Operator(op) => op.symbol().to_string(),
        CalleeId::Anonymous => "<function>".to_string(),
    }
}

impl Callee {
    /// This callee as a report's sentence names it: a function quoted as written, an operator and a witness described, since neither has a name a program writes.
    pub fn phrase(&self, spelling: &Spelling) -> String {
        match self {
            Callee::Function(name) => format!("`{}`", spelled_mention(name, spelling)),
            Callee::Constructor { owner, tag } => {
                format!("`{}/{tag}`", spelling.symbol(owner))
            }
            Callee::Anonymous => "`<function>`".to_string(),
            Callee::Structure(name) => format!("`{}`", spelling.symbol(name)),
            Callee::Operator { op, .. } => format!("the `{}` operator", op.symbol()),
            Callee::Witness { concept, key } => format!(
                "the witness of `{}` for {} `{}`",
                spelling.symbol(concept),
                key_noun(key),
                key.spelled(&Rc::new(spelling.clone()))
            ),
        }
    }

    /// One of this callee's slots as a report names it, `the bound 'ok' of '/f'`, and `the bound of '/f'` where the slot has no name. An operator's slot is named by the operator alone: its binder is minted, and names nothing a reader can find.
    pub fn slot(&self, noun: &str, binder: Option<&str>, spelling: &Spelling) -> String {
        match (self, binder) {
            (Callee::Operator { .. }, _) | (_, None) => {
                format!("the {noun} of {}", self.phrase(spelling))
            }
            (
                Callee::Function(_)
                | Callee::Constructor { .. }
                | Callee::Witness { .. }
                | Callee::Structure(_)
                | Callee::Anonymous,
                Some(binder),
            ) => format!("the {noun} `{binder}` of {}", self.phrase(spelling)),
        }
    }
}

/// An error paired with the [`Spelling`] its terms render under — the parameter `Display::fmt` cannot take, and the reason every arm below rebinds its term fields before interpolating them. A field left unrebound would silently render core's own spelling, which is why the rebinding is per-arm and mechanical rather than left to each `write!`.
///
/// The third is where the declarations a position lies in were written, for the three messages that state one in their own text: a goal batch rendered whole, a written goal a postponed conversion watched, and the other of two terms that decide one `Sort` apart. Empty where nobody knows, and the position is then left out.
pub struct Displayed<'a>(pub &'a Error, pub Rc<Spelling>, pub &'a [&'a Anchors]);

impl fmt::Display for Displayed<'_> {
    /// What the error says as text: the line and each fact beneath it, and for a batch each member's own, a blank line between.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Displayed(error, spelling, anchors) = self;

        match error {
            // Each goal is a report of its own, at its own place.
            Error::Goals(reports) => {
                let rendered = reports
                    .iter()
                    .map(|report| {
                        let spelling =
                            Rc::new(spelling.as_ref().clone().for_reader(ReaderPosition {
                                owner: report.owner,
                                witnesses: Rc::clone(&report.witnesses),
                            }));
                        let span = report.extent.and_then(|extent| extent.resolved(anchors));

                        Report::of(span, goal_said(report, &spelling))
                            .rendered(Kind::Goal, &Style::plain())
                    })
                    .collect::<Vec<_>>();

                formatter.write_str(&rendered.join("\n\n"))
            }
            // Every member's own rendering, a blank line between: each carries its own declaration and place.
            Error::Batch(errors) => {
                for (index, error) in errors.iter().enumerate() {
                    if index > 0 {
                        formatter.write_str("\n\n")?;
                    }
                    Displayed(error, Rc::clone(spelling), anchors).fmt(formatter)?;
                }

                Ok(())
            }
            _ => self.said().fmt(formatter),
        }
    }
}

impl Displayed<'_> {
    /// What the error says: a line, and the facts it rests on.
    pub fn said(&self) -> Said {
        let mut said = Said::default();
        self.say(&mut said);

        said
    }

    /// Say the error onto `f`: each arm its tag, the kind of thing refused, then a fact a line — what stands here, what it is held to, the way out.
    fn say(&self, f: &mut Said) {
        let spelling = &self.1;
        let anchors = self.2;
        // A watched metavariable by what it is.
        let named = |watched: &Watched| match watched {
            Watched::Implicit { callee, binder } => {
                callee.slot("implicit argument", binder.as_deref(), spelling)
            }
            Watched::Witness { callee, role } => match role {
                Some(role) => format!(
                    "the witness argument of {} for {role}",
                    callee.phrase(spelling)
                ),
                None => format!("the witness argument of {}", callee.phrase(spelling)),
            },
            Watched::Described(description) => description.clone(),
            // Where it was written is the report's note.
            Watched::Goal(_) => "a written goal `?`".to_string(),
        };
        // A term of one type where a carrier's was wanted is a type mismatch, said as one.
        let carried = |f: &mut Said, head_type: &Term, expected: &str| {
            f.text("type mismatch");
            f.stating(format_args!("inferred: {}", head_type.spelled(spelling)));
            f.stating(format_args!("expected: {expected}"));
        };
        match self.0 {
            Error::ReduceExhausted { term, refusal } => {
                f.text("budget spent");
                f.stating(format_args!("reducing: {}", term.spelled(spelling)));
                spent(f, refusal)
            }
            Error::ConvertExhausted {
                this,
                that,
                refusal,
            } => {
                f.text("budget spent");
                f.stating(format_args!("comparing: {}", this.spelled(spelling)));
                f.stating(format_args!("with: {}", that.spelled(spelling)));
                spent(f, refusal)
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
                let (tag, side) = match terms {
                    true => ("term mismatch", "written"),
                    false => ("type mismatch", "inferred"),
                };
                f.text(tag);
                f.stating(format_args!("{side}: {shown_inferred}"));
                f.stating(format_args!("expected: {shown_expected}"));
                // The collision every beginner meets once: a type handed over where a value of it belonged — `Eq/cong(f, Eq()(a, b))` with the statement in the proof's seat. The mismatch line already says `Prop` against `Eq()(…)`, but only to a reader who knows a proposition is a type and a proof is its inhabitant, which is the very thing they have not learned yet. One sentence names the level confusion; the `Type` twin is the same mistake with a type where a value belonged.
                let inferred_is_sort = matches!(&***inferred, Subterm::Sort(_));
                let expected_is_sort = matches!(&***expected, Subterm::Sort(_));
                if !terms && inferred_is_sort && !expected_is_sort {
                    let (what, held) = match &***inferred {
                        Subterm::Sort(Sort {
                            quality: Quality::Prop,
                            level: _,
                        }) => ("a proposition", "a proof of it"),
                        _ => ("a type", "a value of it"),
                    };
                    f.stating(format_args!("{what} was given where {held} was expected"));
                }
                if let Some(Declined::Unpaired) = declined {
                    f.stating(
                        "not compared: the two sides of a commutative operation share no operand",
                    );
                    f.stating(
                        "operands are paired where they are one term, never by the order they stand in",
                    );
                }
            }
            Error::RejectedCandidate {
                watched,
                candidate,
                error,
            } => {
                let candidate = candidate.spelled(spelling);
                let born = match watched {
                    Watched::Implicit { .. } | Watched::Witness { .. } => {
                        "where the argument was left out"
                    }
                    Watched::Described(_) | Watched::Goal(_) => "where it stands",
                };
                f.text("rejected candidate");
                f.stating(format_args!("{} cannot be {candidate}", named(watched)));
                f.stating(format_args!("it does not check {born}"));
                // The check's own report, under whatever located or scoped it: the candidate is no term of the program, and where it failed is this report's place.
                // One fact: what the check said, its own facts beneath its tag.
                f.stating(Displayed(error.unwrapped(), Rc::clone(spelling), anchors).said());
            }
            // "this sequencing", not "this `!`": a hand-written `/std/Monad/bind` call reaches the same report, and nothing on the term records which spelling produced it.
            Error::StrandedSequencing { sequenced, region } => {
                let region = region.spelled(spelling);
                let needed = match sequenced {
                    Some(sequenced) => sequenced.spelled(spelling).to_string(),
                    None => "a monad".to_string(),
                };
                f.text("region mismatch");
                f.stating(format_args!(
                    "this sequencing needs a region whose type is {needed}"
                ));
                f.stating(format_args!("the region here has type {region}"));
                f.stating("`!` sequences within the nearest enclosing value body");
                f.stating(
                    "a `let` with a type annotation is a declaration, and its body is its own region",
                );
            }
            Error::UnembeddedAction { action, expected } => {
                f.text("monad mismatch");
                f.stating(format_args!("action: {}", action.spelled(spelling)));
                f.stating(format_args!("expected: {}", expected.spelled(spelling)));
                f.stating("an action is embedded through a declared `Lift` witness");
                f.stating("that needs its monad to be read from its head's declaration");
                f.stating("write `lift(action)` to embed this one explicitly");
                f.stating("that resolves the witness, or reports the edge to declare");
            }
            Error::UniverseInconsistency { lower, upper, path } => {
                let names = level_meta_names([lower, upper]);
                f.text("universe inconsistency");
                f.stating(format_args!(
                    "this {} would need to be strictly below itself",
                    curios_core::inconsistent_sort(path)
                ));
                f.stating(format_args!(
                    "required: {} ≤ {}",
                    renamed_level(lower, &names),
                    renamed_level(upper, &names)
                ));
                for instantiated in curios_core::instantiated_on(path) {
                    f.stating(format_args!(
                        "required by the bounds `{}` sets on the levels it is used at",
                        spelling.symbol(&instantiated)
                    ));
                }
                if curios_core::names_a_proposition(path) {
                    f.stating(
                        "a proposition sits at the level of what it quantifies over and of what it holds",
                    );
                    f.stating("so it is none of the propositions it ranges over");
                }
                f.stating("levels are inferred and have no syntax, so no annotation removes this");
                f.stating("a definition is generalized only at the top level");
                f.stating("hoist a local one that is used at two levels");
                f.stating("a recursive group is monomorphic in its own levels");
                f.stating(
                    "a member's own calls constrain those levels, and never choose them afresh",
                );
            }
            Error::UniverseInvariant(message) => {
                f.text("compiler fault");
                f.stating("the inferred universe state is invalid");
                f.stating(message);
            }
            Error::NotAFunction { head_type, reduced } => {
                let reduced = reduced.spelled(spelling).to_string();
                let head_type = head_type.spelled(spelling).to_string();
                f.text("not a function");
                f.stating(format_args!("applied: a term of type {head_type}"));
                // The reduced form is not spelled: it is the unfolded definition stuck on its argument, and the written type is what the reader can act on.
                if reduced != head_type {
                    f.stating("which does not reduce to a function type");
                }
            }
            Error::NotAFunctionType { expected, argument } => {
                f.text("unexpected function");
                f.stating(format_args!("expected: {}", expected.spelled(spelling)));
                if let Some(site) = argument {
                    let parameter = match &site.parameter {
                        Some(name) => format!("`{name}`, "),
                        None => String::new(),
                    };
                    let mark = match site.plicity {
                        Plicity::Explicit => "",
                        Plicity::Implicit => "`@` ",
                        Plicity::Witness => "`use` ",
                    };
                    f.stating(format_args!(
                        "checked as {parameter}the {} {mark}argument of `{}`",
                        ordinal(site.position),
                        spelled_callee(&site.function, spelling)
                    ));
                    if let Some((name, position)) = &site.function_typed {
                        let name = match name {
                            Some(name) => format!("`{name}`, "),
                            None => String::new(),
                        };
                        f.stating(format_args!(
                            "`{}` takes a function as {name}its {} argument",
                            spelled_callee(&site.function, spelling),
                            ordinal(*position)
                        ));
                    }
                }
            }
            Error::NotATuple { head_type } => {
                f.text("not a tuple");
                f.stating(format_args!(
                    "projected from: a term of type {}",
                    head_type.spelled(spelling)
                ));
            }
            Error::NotATupleType {
                expected,
                unit_for_a_type,
            } => {
                f.text("unexpected tuple");
                f.stating(format_args!("expected: {}", expected.spelled(spelling)));
                if *unit_for_a_type {
                    f.stating("`()` is the unit value, and a type stands here");
                    f.stating("the unit type is written `{}`");
                }
            }
            Error::TupleArityMismatch { expected, got } => {
                f.text("field count mismatch");
                f.stating(format_args!("written: {got}"));
                f.stating(format_args!("expected: {expected}"));
            }
            Error::TupleIndexOutOfBounds { index, arity } => {
                f.text("index out of range");
                f.stating(format_args!("index: {index}"));
                f.stating(format_args!("fields: {arity}"));
            }
            Error::UnknownTupleLabel { label, available } => {
                f.text("unknown field");
                f.stating(format_args!("no field is named `{label}`"));
                match available.is_empty() {
                    true => f.stating("the tuple type has no labeled fields"),
                    false => f.stating(format_args!("available: {}", available.join(", "))),
                }
            }
            Error::DuplicateTupleLabel { label } => {
                f.text("duplicate field");
                f.stating(format_args!(
                    "`{label}` labels two fields of this tuple type"
                ));
            }
            Error::DuplicateInduct { name } => {
                f.text("duplicate declaration");
                f.stating(format_args!(
                    "the inductive type `{}` is already declared",
                    spelling.symbol(name)
                ));
            }
            Error::DuplicateStruct { name } => {
                f.text("duplicate declaration");
                f.stating(format_args!(
                    "the struct `{}` is already declared",
                    spelling.symbol(name)
                ));
            }
            Error::DuplicateConcept { name } => {
                f.text("duplicate declaration");
                f.stating(format_args!(
                    "the concept `{}` is already declared",
                    spelling.symbol(name)
                ));
            }
            Error::TupleFieldNameMismatch {
                written,
                expected,
                position,
            } => {
                f.text("field name mismatch");
                f.stating(format_args!("field {position} is written `{written}`"));
                match expected.is_empty() {
                    true => f.stating("the expected type has no label there"),
                    false => f.stating(format_args!("expected: `{expected}`")),
                }
            }
            Error::NotNatType { head_type } => carried(f, head_type, "Nat"),
            Error::NotBoolType { head_type } => carried(f, head_type, "Bool"),
            Error::NotListType { head_type } => carried(f, head_type, "a List"),
            Error::NotBytesType { head_type } => carried(f, head_type, "Bytes"),
            Error::WrongNumberOfArguments {
                expected,
                got,
                answers_a_function,
            } => {
                f.text("argument count mismatch");
                f.stating(format_args!("written: {got}"));
                f.stating(format_args!("expected: {expected}"));
                if *answers_a_function {
                    f.stating(
                        "a call fills one parameter list, and what this one answers is itself a function",
                    );
                    f.stating("write the rest as a call of their own, `f(a)(b)`");
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
                f.text("surplus binder");
                f.stating(format_args!("this function writes {binders}"));
                f.stating("a binder binds the next parameter of its run");
                f.stating(claimed);
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
                    Plicity::Explicit => format!("an explicit {member}, written with no mark"),
                    Plicity::Implicit => format!("an implicit {member}, written with `@`"),
                    Plicity::Witness => format!("a witness {member}, written with `use`"),
                };
                let written = match written {
                    Plicity::Explicit => "written with no mark",
                    Plicity::Implicit => "written with `@`",
                    Plicity::Witness => "written with `use`",
                };
                let named = match binder {
                    None => String::new(),
                    Some(binder) => format!(", `{binder}`,"),
                };
                f.text("mark mismatch");
                match site {
                    BinderSite::Parameter => f.stating(format_args!(
                        "function parameter {position}{named} is {requirement}"
                    )),
                    BinderSite::Payload { constructor } => f.stating(format_args!(
                        "payload {position}{named} of `{constructor}` is {requirement}"
                    )),
                }
                f.stating(format_args!("it is {written}"));
            }
            Error::UnknownMatchConstructor {
                type_name,
                tag,
                constructors,
            } => {
                let type_name = spelling.symbol(type_name);
                f.text("unknown constructor");
                f.stating(format_args!("`{tag}` is no constructor of `{type_name}`"));
                match constructors.is_empty() {
                    true => f.stating(format_args!("`{type_name}` has no constructors")),
                    false => {
                        f.stating(format_args!(
                            "its constructors: {}",
                            constructors.join(", ")
                        ));
                    }
                }
            }
            Error::MatchCaseMissing { type_name, tag } => {
                f.text("missing arm");
                f.stating(format_args!(
                    "no arm matches the constructor `{tag}` of `{}`",
                    spelling.symbol(type_name)
                ));
            }
            Error::NotAInductType { head_type, no_arms } => {
                f.text("not an inductive type");
                f.stating(match no_arms {
                    true => "a match with no arms eliminates only a type with no constructors",
                    false => "constructor arms match a value of an inductive type",
                });
                f.stating(format_args!("scrutinee: {}", head_type.spelled(spelling)));
            }
            Error::SortDecided { prop, at_decider } => {
                let sort = match prop {
                    true => "Prop",
                    false => "Type",
                };
                f.text("sort decided");
                match at_decider {
                    true => {
                        f.stating("this decides a `Sort` its declaration leaves to a caller");
                        f.stating(format_args!("it can only be `{sort}`"));
                    }
                    false => {
                        f.stating(format_args!("this `Sort` can only be `{sort}`"));
                        f.stating("its declaration decides it");
                    }
                }
                f.stating("a sort left to a caller is one each use states");
                f.stating(format_args!(
                    "write `{sort}` where the declaration decides it"
                ));
            }
            // The other term is the report's note, where its check was recorded.
            Error::SortDecidedTwice { prop, .. } => {
                let (here, there) = match prop {
                    true => ("Prop", "Type"),
                    false => ("Type", "Prop"),
                };
                f.text("sort decided twice");
                f.stating(format_args!(
                    "this decides a `Sort` its declaration leaves to a caller as `{here}`"
                ));
                f.stating(format_args!("another of its terms decides it as `{there}`"));
                f.stating(
                    "a sort is one of the two wherever it is stated, so no `Sort` serves both terms",
                );
            }
            Error::TooManySorts { stated } => {
                f.text("too many sorts");
                f.stating(format_args!(
                    "this declaration leaves {stated} sorts to its caller"
                ));
                f.stating(format_args!(
                    "a declaration leaves at most {}",
                    curios_core::UniverseContext::QUALITY_LIMIT
                ));
                f.stating("it is checked once at every way of stating them");
            }
            Error::RecordOverTwoSorts => {
                f.text("ambiguous sort");
                f.stating("this tuple type stands over two sorts each left to a caller");
                f.stating(
                    "it is a proposition where both are `Prop` and a type otherwise, so it has no one sort",
                );
                f.stating("state one of the two as `Type` or `Prop`");
            }
            Error::LargeElimOfProp { name } => {
                f.text("proof matched into data");
                f.stating(format_args!(
                    "the proposition `{}` is matched into a relevant result",
                    spelling.symbol(name)
                ));
                f.stating("a proposition is matched into data only where no constructor applies");
                f.stating("or where it declares one whose every payload its indices determine");
            }
            Error::InformativePropStruct {
                name,
                keyword,
                mark,
                field,
                field_type,
            } => {
                let name = spelling.symbol(name);
                let field_type = field_type.spelled(spelling);
                // The field as it is written: a superclass edge under `use`, a hidden field under `@`, and a positional one as its type alone.
                let field = match (mark, field.is_empty()) {
                    (Plicity::Witness, _) => format!("use {field_type}"),
                    (Plicity::Implicit, _) => format!("@{field}: {field_type}"),
                    (Plicity::Explicit, true) => field_type.to_string(),
                    (Plicity::Explicit, false) => format!("{field}: {field_type}"),
                };
                f.text("informative field");
                f.stating(format_args!("the {keyword} `{name}` is declared at `Prop`"));
                f.stating(format_args!("its field `{field}` is no proposition"));
                f.stating(format_args!(
                    "every field of a `Prop` {keyword} is a proposition"
                ));
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
                    } => format!("the payload `{label}` of `{constructor}`, a `{site_type}`"),
                    DeclaredPart::Payload {
                        constructor,
                        member: MemberLabel::Place(place),
                    } => format!(
                        "the {} payload of `{constructor}`, a `{site_type}`",
                        ordinal(*place)
                    ),
                    DeclaredPart::Field(MemberLabel::Written(label)) => {
                        format!("its field `{label}`, a `{site_type}`")
                    }
                    DeclaredPart::Field(MemberLabel::Place(place)) => {
                        format!("its {} field, a `{site_type}`", ordinal(*place))
                    }
                    DeclaredPart::Whole => "its own declaration".to_string(),
                };
                f.text("not strictly positive");
                f.stating(format_args!("`{name}` occurs in itself {polarity}"));
                f.stating(format_args!("through {through}"));
                f.stating("a recursive occurrence is a plain payload, never left of an arrow");
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
                f.text("may not terminate");
                f.stating(format_args!(
                    "{} is a {erased} position",
                    site.phrase(spelling)
                ));
                match offender {
                    Some(offender) => f.stating(format_args!(
                        "it reaches `{}`, which is not known to terminate",
                        spelling.symbol(offender)
                    )),
                    None => f.stating("it does not terminate on every input"),
                }
                f.stating(advice);
            }
            Error::NotAStructType { found } => {
                f.text("not a struct");
                f.stating(format_args!("found: {}", found.spelled(spelling)));
            }
            Error::StructArityMismatch {
                name,
                expected,
                got,
            } => {
                f.text("argument count mismatch");
                f.stating(format_args!(
                    "`{}` takes {}",
                    spelling.symbol(name),
                    counted(*expected, "type argument")
                ));
                f.stating(format_args!("written: {got}"));
            }
            Error::WrongNumberOfFields {
                name,
                expected,
                got,
                missing,
                surplus,
            } => {
                f.text("field count mismatch");
                f.stating(format_args!(
                    "`{}` has {}",
                    spelling.symbol(name),
                    counted(*expected, "field")
                ));
                f.stating(format_args!("written: {got}"));
                if !missing.is_empty() {
                    f.stating(format_args!("missing: {}", missing.join(", ")));
                }
                if !surplus.is_empty() {
                    f.stating(format_args!("no such field: {}", surplus.join(", ")));
                }
            }
            Error::UnknownDeclaration { name } => {
                f.text("unknown declaration");
                f.stating(format_args!("`{}` is not declared", spelling.symbol(name)));
            }
            Error::UnknownStructField {
                name,
                label,
                available,
            } => {
                f.text("unknown field");
                f.stating(format_args!(
                    "`{}` has no field `{label}` at that position",
                    spelling.symbol(name)
                ));
                f.stating(format_args!("fields in order: {}", available.join(", ")));
            }
            Error::UseEntryOutsideConcept { name } => {
                f.text("not a concept");
                f.stating(format_args!(
                    "a `use` entry is written in a literal of the struct `{}`",
                    spelling.symbol(name)
                ));
                f.stating("only a concept's literal takes `use` entries");
            }
            Error::SpreadNotFirst { name } => {
                f.text("misplaced spread");
                f.stating(format_args!(
                    "a `..` spread is the first entry of the `{}` literal",
                    spelling.symbol(name)
                ));
            }
            Error::MultipleSpreads { name } => {
                f.text("duplicate spread");
                f.stating(format_args!(
                    "the `{}` literal writes more than one `..` spread",
                    spelling.symbol(name)
                ));
                f.stating("a struct literal takes at most one");
            }
            Error::SpreadBaseTypeMismatch { name, found } => {
                let name = spelling.symbol(name);
                f.text("type mismatch");
                f.stating(format_args!("inferred: {}", found.spelled(spelling)));
                f.stating(format_args!("expected: {name}"));
                f.stating(format_args!(
                    "the `..` base of a `{name}` literal is itself a `{name}`"
                ));
            }
            Error::UnlabeledSpreadOverride { name } => {
                f.text("unlabeled override");
                f.stating(format_args!(
                    "an override after the `..` spread of the `{}` literal is labeled, `field = value`",
                    spelling.symbol(name)
                ));
            }
            Error::SpreadOverrideOutOfOrder { name, label, order } => {
                f.text("override out of order");
                f.stating(format_args!("`{label}` is repeated or out of place"));
                f.stating(format_args!(
                    "overrides after a `..` spread follow the field order of `{}`",
                    spelling.symbol(name)
                ));
                f.stating(format_args!("order: {}", order.join(", ")));
            }
            Error::PrivateField { name, field } => {
                f.text("private field");
                f.stating(format_args!(
                    "`{field}` of `{}` is private to its declaring module and its descendants",
                    spelling.symbol(name)
                ));
            }
            Error::PrivateRepresentation { name } => {
                f.text("private representation");
                f.stating(format_args!(
                    "the representation of `{}` is private to its declaring module and its descendants",
                    spelling.symbol(name)
                ));
            }
            Error::NoDerivation { concept } => {
                f.text("no derivation");
                f.stating(format_args!(
                    "`{}` is not registered as derivable",
                    spelling.symbol(concept)
                ));
                f.stating("`{ .. }` asks the compiler to write the witness");
                f.stating("write the body");
            }
            Error::DeriveOutsideWitness => {
                f.text("misplaced derivation");
                f.stating("a derived body is written only as a witness body");
                f.stating("it is checked against the witness's concept application");
            }
            Error::Underivable {
                concept,
                key,
                reason,
            } => {
                let key = key.spelled(spelling);
                f.text("no derivation");
                f.stating(format_args!(
                    "`{}` cannot be derived for {key}",
                    spelling.symbol(concept)
                ));
                match reason {
                    Underivable::NotDeclared => {
                        f.stating(format_args!("{key} is no declared `induct` or `struct`"));
                        f.stating(
                            "a derivation writes out a declaration's constructors and nothing else",
                        );
                    }
                    Underivable::Concept => {
                        f.stating(format_args!("{key} is a concept's record"));
                    }
                    Underivable::TypeValued {
                        constructor,
                        payload,
                    } => f.stating(format_args!(
                        "{payload} of `{constructor}` is a type, which no value spells"
                    )),
                }
                f.stating("write the body");
            }
            Error::CtorArityMismatch {
                atom,
                expected,
                got,
            } => {
                f.text("payload count mismatch");
                f.stating(format_args!(
                    "`{atom}` takes {}",
                    counted(*expected, "payload")
                ));
                f.stating(format_args!("written: {got}"));
            }
            Error::MatrixInconsistentShape => {
                f.text("shape mismatch");
                f.stating("the arms' patterns disagree on shape in one column");
            }
            Error::MatrixDuplicateRow { covered } => {
                f.text("unreachable arm");
                match covered {
                    Some(covered) => {
                        f.stating("no value reaches this arm past the arms before it");
                        f.stating(format_args!("`{covered}` is among them"));
                    }
                    None => {
                        f.stating("this arm duplicates or overlaps another");
                        f.stating("every arm is reachable and distinct");
                    }
                }
            }
            Error::MatrixUncovered { value, indexed } => {
                f.text("missing arm");
                f.stating(format_args!("no arm matches `{value}`"));
                if *indexed {
                    f.stating("its indices are not provably impossible at the scrutinee's");
                    f.stating("write the arm");
                }
            }
            Error::MatrixArmsMeet {
                value,
                first,
                second,
            } => {
                f.text("overlapping arms");
                f.stating(format_args!(
                    "the arms `{first}` and `{second}` both match `{value}`"
                ));
                f.stating("neither is a case of the other, and arm order decides nothing");
                f.stating(format_args!("write an arm for `{value}` before them"));
            }
            Error::MatrixAlternativesMeet {
                value,
                first,
                second,
                contained,
            } => {
                f.text("overlapping alternatives");
                match contained {
                    true => {
                        f.stating(format_args!(
                            "the alternative `{first}` is a case of `{second}` in the same arm"
                        ));
                        f.stating(format_args!(
                            "`{second}` matches every value `{first}` does"
                        ));
                        f.stating(format_args!("write `{second}` alone"));
                    }
                    false => {
                        f.stating(format_args!(
                            "the alternatives `{first}` and `{second}` of one arm both match `{value}`"
                        ));
                        f.stating("an arm's alternatives match no value in common");
                        f.stating(format_args!("write one of them to leave `{value}` out"));
                    }
                }
            }
            Error::MatrixStructHead { head, scrutinee } => {
                f.text("pattern mismatch");
                f.stating(format_args!(
                    "a `{head}` pattern matches a value of that structure"
                ));
                f.stating(format_args!("scrutinee: {}", scrutinee.spelled(spelling)));
            }
            Error::MatrixMixedNatDispatch {
                successor, literal, ..
            } => {
                f.text("mixed patterns");
                f.stating(format_args!(
                    "`{successor}` peels a successor and `{literal}` matches a value"
                ));
                f.stating("a `Nat` is taken apart one of the two ways");
                f.stating("write `0` beside successor arms, or literals beside `_`");
                f.stating("match a value past `0` on the predecessor, inside the successor arm");
            }
            // A name a program wrote is reported by the reason the lowering filed for it, in that value's own words, so a misspelled name reads the same whichever stage refuses for it.
            Error::UnboundVariable { why: Some(why), .. } => why.say(f),
            Error::UnboundVariable { term, why: None } => {
                f.text("unknown name");
                f.stating(format_args!("`{}` is not in scope", term.spelled(spelling)));
            }
            Error::CannotInfer => {
                f.text("undetermined type");
                f.stating("the type of this expression cannot be inferred");
            }
            Error::PostponedCheck { expected, watching } => {
                f.text("undetermined type");
                f.stating("the expected type of this expression never gained structure");
                f.stating(format_args!("expected: {}", expected.spelled(spelling)));
                if !watching.is_empty() {
                    let watching = watching.iter().map(named).collect::<Vec<_>>();
                    f.stating(format_args!("never solved: {}", watching.join(", ")));
                }
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
                f.text("undecided conversion");
                if !(both_bare && !watching.is_empty()) {
                    f.stating(format_args!("between: {this}"));
                    f.stating(format_args!("and: {that}"));
                }
                if !watching.is_empty() {
                    let watching = watching.iter().map(named).collect::<Vec<_>>();
                    f.stating(format_args!("never solved: {}", watching.join(", ")));
                }
                if *under_refinements {
                    f.stating("the goal sits under match-arm refinements");
                    f.stating("a solution holding only under them is never committed");
                }
            }
            Error::AmbiguousSpines {
                watched,
                one,
                another,
            } => {
                f.text("ambiguous argument");
                f.stating(format_args!(
                    "two types each give {} a value, and the values differ",
                    named(watched)
                ));
                f.stating(format_args!("one: {}", one.spelled(spelling)));
                f.stating(format_args!("another: {}", another.spelled(spelling)));
                f.stating("write it to say which is meant");
            }
            Error::BangRegionUndetermined => {
                f.text("undetermined monad");
                f.stating("the monad of this region was never determined");
                f.stating("`!` sequences in its region's monad, read from the region's type");
                f.stating("annotate the enclosing result type to fix the monad");
            }
            Error::BangRegionNotAMonad { region, owner } => {
                f.text("not a monad");
                f.stating(format_args!("region: {}", region.spelled(spelling)));
                f.stating("`!` sequences in its region's monad, read from the region's type");
                if let RegionOwner::Function = owner {
                    f.stating("a function whose result is not a monad has nowhere to sequence one");
                }
            }
            Error::OperatorUndefined { symbol, type_ } => {
                f.text("undefined operator");
                f.stating(format_args!(
                    "`{symbol}` is not defined for the type {}",
                    type_.spelled(spelling)
                ));
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
                        let mut why = Said::line(nothing);
                        if let Some(refusal) = refusal {
                            refused(&mut why, refusal, spelling);
                        }

                        why
                    }
                    false => {
                        // A literal has fields where a call has arguments.
                        let given = match callee {
                            Callee::Structure(_) => "field",
                            _ => "argument",
                        };
                        let mut why =
                            Said::line(format!("no {given} or expected type determined it"));
                        why.stating(format_args!("its type: {bound}"));

                        why
                    }
                };
                // A bound the search read is one the facts in scope may prove, so the way out named first is a fact: a guard. One it read nothing in has the written proof alone.
                let searched =
                    *proposition && refusal.as_ref().is_some_and(|refusal| !refusal.is_silent());
                // The way out by writing the slot, after the guard where there is one.
                let written = |f: &mut Said, before: &str, explicit: String| match searched {
                    true => {
                        f.stating(format_args!(
                            "decide the bound with a guard before the {before}"
                        ));
                        f.stating(format_args!("or {explicit}"));
                    }
                    false => f.stating(explicit),
                };
                match callee {
                    // A literal's slot is a field, written in braces under `@`. One declared as its type alone has no name to quote.
                    Callee::Structure(name) => {
                        let name = spelling.symbol(name);
                        let field = match binder {
                            None => "a hidden field".to_string(),
                            Some(binder) => format!("the hidden field `{binder}`"),
                        };
                        f.text("unfilled field");
                        f.stating(format_args!("{field} of `{name}` was not filled"));
                        f.extend(why);
                        written(
                            f,
                            "literal",
                            format!("write it explicitly, `{name} {{ @... }}`"),
                        );
                    }
                    Callee::Function(func) => {
                        let func = spelled_mention(func, spelling);
                        let argument = match binder {
                            None => "an implicit argument".to_string(),
                            Some(binder) => format!("the implicit argument `{binder}`"),
                        };
                        f.text("uninferred argument");
                        f.stating(format_args!("{argument} of `{func}` was not inferred"));
                        f.extend(why);
                        written(f, "call", format!("supply it explicitly, `{func}(@...)`"));
                    }
                    // An operator has no argument list to write the bound into, so the report names the two places it can be established: a guard before the operation, or the method call the operator stands for — with its type argument first, since a leading `@` fills that slot and not the bound.
                    Callee::Operator { method, .. } => {
                        f.text("undischarged bound");
                        f.stating(format_args!(
                            "{} was not discharged",
                            callee.slot("bound", binder.as_deref(), spelling)
                        ));
                        f.extend(why);
                        f.stating("an operator takes no written arguments");
                        f.stating("decide the bound with a guard before the operation");
                        f.stating(format_args!(
                            "or call `{}(@T, a, b, @proof)`",
                            spelling.symbol(method)
                        ));
                    }
                    Callee::Constructor { .. } => {
                        let argument = match binder {
                            None => "an implicit argument".to_string(),
                            Some(binder) => format!("the implicit argument `{binder}`"),
                        };
                        f.text("uninferred argument");
                        f.stating(format_args!(
                            "{argument} of {} was not inferred",
                            callee.phrase(spelling)
                        ));
                        f.extend(why);
                        f.stating("supply it explicitly");
                    }
                    // A head the program gave no name to: a computed function, or a projection out of a recursive group. There is nothing to quote, so the advice names the slot rather than a callee it cannot spell.
                    Callee::Anonymous => {
                        f.text("uninferred argument");
                        f.stating(format_args!(
                            "{} was not inferred",
                            callee.slot("implicit argument", binder.as_deref(), spelling)
                        ));
                        f.extend(why);
                        f.stating("supply it explicitly at the call site");
                    }
                    // A witness is never called, so there is no argument to supply: its parameters are filled from the goal it answers.
                    Callee::Witness { .. } => {
                        f.text("uninferred argument");
                        f.stating(format_args!(
                            "{} was not inferred",
                            callee.slot("implicit parameter", binder.as_deref(), spelling)
                        ));
                        f.extend(why);
                        f.stating("a witness is never called");
                        f.stating("its parameters are filled from the goal it is resolved for");
                    }
                }
            }
            Error::ImplicitAtOtherSort {
                callee,
                binder,
                given,
                proposition,
            } => {
                let slot = callee.slot("implicit argument", binder.as_deref(), spelling);
                let (given_as, taken_as, declared) = match proposition {
                    true => ("proposition", "type", "Type"),
                    false => ("type", "proposition", "Prop"),
                };
                f.text("sort mismatch");
                f.stating(format_args!(
                    "a {given_as} was given where {slot} takes a {taken_as}"
                ));
                f.stating(format_args!("given: {}", given.spelled(spelling)));
                f.stating(format_args!(
                    "no {given_as} stands where a `{declared}` is declared"
                ));
            }
            Error::DomainNeverDetermined { binder } => {
                let (parameter, written) = match binder {
                    None => ("a parameter".to_string(), "_"),
                    Some(binder) => (format!("the parameter `{binder}`"), binder.as_str()),
                };
                f.text("undetermined type");
                f.stating(format_args!("the type of {parameter} was never determined"));
                f.stating("it comes from its annotation, its body or its position's expected type");
                f.stating("none supplied one");
                f.stating(format_args!(
                    "annotate the parameter, `({written}: T) => ...`"
                ));
            }
            Error::WitnessMemberNeverStated => {
                f.text("unstated member");
                f.stating("this lambda writes `use _` and nothing states the member");
                f.stating("a `use` member holds a place, typed by the expected function type");
                f.stating("this lambda has none");
                f.stating(
                    "write the function as a `let` with a telescope, `let f(use C(A), ...) -> ... = ...`",
                );
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
                let stands = match binder {
                    None => format!("a `{slot}` member"),
                    Some(binder) => format!("the `{slot}` member `{binder}`"),
                };
                f.text("member out of order");
                f.stating(format_args!(
                    "a `{written}` member is written where {stands} stands"
                ));
                f.stating("hidden members are written in order from the first of their run");
                f.stating("the rest of the run may be left out");
                f.stating(format_args!(
                    "write `{placeholder}` first to leave that one to the elaborator"
                ));
            }
            Error::HiddenMemberWithoutSlot { written } => {
                let written = match written {
                    Plicity::Witness => "use",
                    Plicity::Implicit | Plicity::Explicit => "@",
                };
                f.text("surplus member");
                f.stating(format_args!("this `{written}` member has no slot"));
                f.stating("hidden members are written before the plain member they precede");
                f.stating("every hidden slot there is already written or left out");
            }
            Error::DeclarationCycle { members } => {
                let named = members
                    .iter()
                    .map(|member| member.spelled(spelling))
                    .collect::<Vec<_>>();
                // Neither group's spelling holds a definition beside a witness, so a cycle through both is pointed at the two ways out it does have.
                let across = members
                    .iter()
                    .any(|member| matches!(member, CycleMember::Named(_)))
                    && members
                        .iter()
                        .any(|member| matches!(member, CycleMember::Witness { .. }));
                f.text("declaration cycle");
                match named.split_last() {
                    Some((last, rest)) if !rest.is_empty() => {
                        f.stating(format_args!(
                            "{} and {last} need one another",
                            rest.join(", ")
                        ));
                        f.stating("a declaration elaborates after what it reads");
                        f.stating("each of these reads another before it can finish");
                        f.stating(
                            "by a name it writes, or by the witness a goal of its resolves to",
                        );
                        f.stating("a definition may call itself, and a witness may resolve through its own entry");
                        f.stating("a cycle through two has no order to elaborate them in");
                        match across {
                            true => {
                                f.stating("no group holds a definition beside a witness");
                                f.stating("let the definition call by its name the function it means, the witness delegating to it");
                                f.stating("or write its body in the witness, which may read its own methods");
                            }
                            false => {
                                f.stating("declare definitions that call one another as one group, `let ... and ...`");
                                f.stating("and witnesses that resolve through one another as `satisfy C(A) { ... } and D(B) { ... }`");
                            }
                        }
                    }
                    _ => {
                        f.stating(format_args!(
                            "{} needs itself before it can be read",
                            named.join(", ")
                        ));
                        f.stating("what its signature reads leads back to it");
                    }
                }
            }
            Error::GroupApart { parts } => {
                // `a`, `a and b`, `a, b and c`: the members of one part, or every member.
                let listed = |names: Vec<String>| match names.split_last() {
                    Some((last, rest)) if !rest.is_empty() => {
                        format!("{} and {last}", rest.join(", "))
                    }
                    _ => names.concat(),
                };
                let grouped = listed(
                    parts
                        .iter()
                        .flatten()
                        .map(|member| member.spelled(spelling))
                        .collect(),
                );
                let each = parts
                    .iter()
                    .map(|part| match part.as_slice() {
                        [alone] => format!("{} alone", alone.spelled(spelling)),
                        _ => format!(
                            "{} as one group",
                            listed(part.iter().map(|member| member.spelled(spelling)).collect())
                        ),
                    })
                    .collect::<Vec<_>>();
                // A part's own list holds an `and`, so the parts are told apart by a comma before the last.
                let each = match each.split_last() {
                    Some((last, rest)) if !rest.is_empty() => {
                        format!("{}, and {last}", rest.join(", "))
                    }
                    _ => each.concat(),
                };
                f.text("needless group");
                f.stating(format_args!(
                    "{grouped} are grouped, and do not all need one another"
                ));
                f.stating("a group ties declarations that each read another of them before they can finish");
                f.stating("a declaration reads any other wherever it is written");
                f.stating(format_args!("declare each part on its own: {each}"));
            }
            Error::IndexedByItsGroup { name, member } => {
                f.text("indexed by its group");
                f.stating(format_args!(
                    "`{}` is indexed by `{}`, which is declared in its own group",
                    spelling.symbol(name),
                    spelling.symbol(member)
                ));
                f.stating("a family's parameters and indices, and a record's parameters, name only declarations outside their group");
            }
            Error::NoWitness {
                goal,
                callee,
                role,
                need,
                shape,
            } => {
                let needed = match role {
                    Some(role) => format!("needed by {} for {role}", callee.phrase(spelling)),
                    None => format!("needed by {}", callee.phrase(spelling)),
                };
                let written = match need {
                    Some(Need::Embedding(diagnosis)) => diagnosis.goal.as_deref(),
                    _ => None,
                };
                let goal = written.unwrap_or(goal).spelled(spelling);
                f.text("no witness");
                f.stating(format_args!("wanted: {goal}"));
                if let Some(diagnosis) = shape {
                    // A tuple shape is the only identity a twin can drop, so the sentence names labels and nothing decides between two of them: `diagnose_shape` bares tuple labels alone and answers `None` when that leaves the key unchanged.
                    let ShapeDiagnosis { wanted, bare } = &**diagnosis;
                    f.stating("labels are part of the type");
                    f.stating(format_args!(
                        "the witness for {bare} does not cover {wanted}"
                    ));
                    f.stating("name a struct for the labeled product, or declare the witness for this shape");
                }
                match need {
                    None => f.stating(needed),
                    Some(Need::Closing(type_)) => {
                        f.stating("a program closes with a term whose type has one");
                        f.stating(format_args!(
                            "this one's type is {}",
                            type_.spelled(spelling)
                        ));
                    }
                    Some(Need::Embedding(diagnosis)) => {
                        let source = diagnosis.source.spelled(spelling).to_string();
                        let target = diagnosis.target.spelled(spelling);
                        let article = match source.trim_start_matches('/').chars().next() {
                            Some('A' | 'E' | 'I' | 'O' | 'U' | 'a' | 'e' | 'i' | 'o' | 'u') => "an",
                            _ => "a",
                        };
                        f.stating(format_args!(
                            "needed to sequence {article} {source} action in this {target} region"
                        ));
                        if let Some(runner) = &diagnosis.runner {
                            f.stating(format_args!(
                                "{} hands the action's {} back to this region",
                                runner.run.spelled(spelling),
                                runner.result.spelled(spelling)
                            ));
                            f.stating(format_args!(
                                "a region whose own type is a {} sequences it",
                                runner.family.spelled(spelling)
                            ));
                        }
                        if !diagnosis.source_is_monad {
                            f.stating(format_args!("{source} is not a monad"));
                            f.stating("no `Monad` witness exists for it, so no embedding out of it can be declared");
                        }
                        if !diagnosis.chain.is_empty() {
                            // One fact: the heading, and each embedding it heads on a line beneath it.
                            f.stating(format_args!(
                                "declared embeddings chain from {source} to {target}:"
                            ));
                            for (pair, module) in &diagnosis.chain {
                                let module = declaring_module(module);
                                f.text(format_args!("\n  Lift{pair}, declared in {module}"));
                            }
                            f.stating("embeddings never chain automatically");
                            f.stating(format_args!(
                                "declare the composite `Lift({source}, {target})` edge beside one of the two monads"
                            ));
                        }
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
                let report = GoalReport::new(
                    None,
                    reader.owner,
                    Rc::clone(&reader.witnesses),
                    scope.clone(),
                    (**goal).clone(),
                    solution.as_deref().cloned(),
                );
                f.extend(goal_said(&report, spelling));
            }
            // A batch of goals is a report each, which `Display` and `Error::reports` make; said as one, each goal's line and facts follow the last's.
            Error::Goals(reports) => {
                for report in reports {
                    let spelling = Rc::new(spelling.as_ref().clone().for_reader(ReaderPosition {
                        owner: report.owner,
                        witnesses: Rc::clone(&report.witnesses),
                    }));
                    f.extend(goal_said(report, &spelling));
                }
            }
            Error::DuplicateWitness {
                concept,
                key,
                first,
                second,
                ..
            } => {
                let noun = key_noun(key);
                // One clause when both sit in the same module, which is the common case while a program is being written: naming that module twice reads as two coordinates and is one. The carets on each declaration are what separate them.
                let where_ = match first == second {
                    true => format!("both are declared in {}", declaring_module(first)),
                    false => format!(
                        "one is declared in {}, another in {}",
                        declaring_module(first),
                        declaring_module(second)
                    ),
                };
                f.text("duplicate witness");
                f.stating(format_args!(
                    "`{}` has two witnesses for the {noun} `{}`",
                    spelling.symbol(concept),
                    key.spelled(spelling)
                ));
                f.stating(where_);
                f.stating(format_args!(
                    "every concept-{noun} pair has at most one witness, program-wide"
                ));
            }
            Error::OrphanWitness {
                concept,
                key,
                witness,
            } => {
                let noun = key_noun(key);
                f.text("orphan witness");
                f.stating(format_args!(
                    "a witness of `{}` for the {noun} `{}` is declared in {}",
                    spelling.symbol(concept),
                    key.spelled(spelling),
                    declaring_module(witness)
                ));
                f.stating(format_args!(
                    "a witness is declared where its concept or a type in its {noun} is declared"
                ));
            }
            Error::AmbiguousWitness {
                goal,
                first,
                second,
            } => {
                f.text("ambiguous witness");
                f.stating(format_args!("wanted: {}", goal.spelled(spelling)));
                f.stating(format_args!(
                    "both {} and {} match at the same superclass depth",
                    first.spelled(spelling),
                    second.spelled(spelling)
                ));
            }
            Error::CyclicSuperclass { concept } => {
                f.text("superclass cycle");
                f.stating(format_args!(
                    "the concept `{}` is on a cycle of superclasses",
                    spelling.symbol(concept)
                ));
                f.stating("`use` fields form no cycle");
            }
            Error::UnknownSuperclass { concept, target } => {
                let target = target.spelled(spelling);
                f.text("not a concept");
                f.stating(format_args!(
                    "the concept `{}` names `{target}` as a superclass",
                    spelling.symbol(concept)
                ));
                f.stating(format_args!("`{target}` is no concept application"));
            }
            Error::InvalidWitnessHead { position, head } => {
                f.text("unkeyed witness");
                f.stating(format_args!(
                    "parameter {} of its concept reduces to {}",
                    position + 1,
                    head.spelled(spelling)
                ));
                f.stating("a parameter's head is an inductive, a struct, an intrinsic type or a tuple type");
            }
            Error::WitnessKeyNotSpelled {
                concept,
                key,
                spelled,
            } => {
                let noun = key_noun(key);
                let key = key.spelled(spelling);
                f.text("key not spelled");
                f.stating(format_args!(
                    "this witness of `{}` is for the {noun} `{key}`, which its signature does not spell",
                    spelling.symbol(concept)
                ));
                f.stating("a witness is known by its key before anything is elaborated");
                f.stating("so each parameter of its concept is written as the type it is");
                f.stating("a declared type, applied or not, a name for one, a tuple type, or a function to an applied type");
                if let Some(spelled) = spelled {
                    f.stating(format_args!(
                        "as it is written, it would be keyed at `{}`",
                        spelled.spelled(spelling)
                    ));
                }
                f.stating(format_args!(
                    "write `{key}` where the signature computes it"
                ));
            }
            Error::ParameterlessWitnessConcept { concept } => {
                f.text("unkeyed witness");
                f.stating(format_args!(
                    "the concept `{}` has no parameters to key on",
                    spelling.symbol(concept)
                ));
                f.stating("a global witness keys on its concept's parameter heads");
                f.stating("supply a parameterless concept through a local `use` binder");
            }
            Error::WitnessNotAConcept { found } => {
                f.text("not a concept");
                f.stating("this witness witnesses no concept");
                f.stating(format_args!(
                    "its annotation elaborates to: {}",
                    found.spelled(spelling)
                ));
            }
            Error::UseParameterNotAConcept { found, proposition } => {
                f.text("not a concept");
                f.stating("the type of a `use` parameter is a concept application");
                f.stating(format_args!("found: {}", found.spelled(spelling)));
                if *proposition {
                    f.stating("a proof is discharged through an implicit parameter");
                    f.stating("write `@` in place of `use`");
                }
            }
            Error::ImplicitConceptMemberUnfillable { found } => {
                f.text("unfillable parameter");
                f.stating("an `@` parameter at a concept's type that no later type mentions is never filled");
                f.stating(format_args!("found: {}", found.spelled(spelling)));
                f.stating("an `@` parameter is inferred from the types after it");
                f.stating("resolution fills only `use` parameters");
                f.stating(
                    "write `use` in place of `@` to have it resolved, or no mark to have it passed",
                );
            }
            Error::NonRegularWitnessPremise { premise } => {
                f.text("non-regular premise");
                f.stating(format_args!("premise: {}", premise.spelled(spelling)));
                f.stating(
                    "every `use` premise is smaller than the witness's own concept application",
                );
                f.stating("its variables are bound by the witness's parameters, none used more often than there");
                f.stating("and it has fewer nodes in all");
            }
            Error::ExplicitWitnessParam => {
                f.text("explicit witness parameter");
                f.stating("a witness's parameters are implicit, `@`, or `use` premises");
                f.stating("nothing supplies explicit arguments during resolution");
            }
            Error::MotiveBinderCount {
                name,
                expected,
                written,
            } => {
                let subject = match name {
                    Some(name) => format!("eliminating `{}`", spelling.symbol(name)),
                    None => "this eliminator".to_string(),
                };
                let shape = match expected {
                    1 => "just the scrutinee".to_string(),
                    2 => "one index, then the scrutinee".to_string(),
                    n => format!("{} indices, then the scrutinee", n - 1),
                };
                f.text("binder count mismatch");
                f.stating(format_args!(
                    "the motive binds {}",
                    counted(*written, "name")
                ));
                f.stating(format_args!("{subject} needs {expected}: {shape}"));
            }
            Error::MissingArmNotImpossible { tag } => {
                f.text("missing arm");
                f.stating(format_args!("no arm matches `{tag}`"));
                f.stating("its indices are not provably impossible at the scrutinee's");
                f.stating("write the arm");
            }
            Error::ByteLiteralOutOfRange { value } => {
                f.text("literal out of range");
                f.stating(format_args!("`{value}` is no `Byte`"));
                f.stating("a `Byte` is 0 through 255");
            }
            Error::BoolLiteralOutOfRange { value } => {
                f.text("literal out of range");
                f.stating(format_args!("`{value}` is no `Bool`"));
                f.stating("a `Bool` is 0 or 1");
            }
            Error::FltLiteralOutOfRange { value } => {
                f.text("literal out of range");
                f.stating(format_args!(
                    "`{value}` overflows the finite range of `Flt`"
                ));
            }
            Error::Poisoned => {
                f.text("refused dependency");
                f.stating("a witness held by a refused declaration was needed");
            }
            Error::Unfinished => {
                f.text("unfinished declaration");
                f.stating("a declaration that has not elaborated was read");
            }
            Error::Pending(_) => {
                f.text("compiler fault");
                f.stating("a refusal's report was not built");
            }
            // A wrapper says what it wraps, and what it adds of its own after.
            Error::InDeclaration { error, .. }
            | Error::Located { error, .. }
            | Error::InScope { error, .. } => {
                Displayed(error, Rc::clone(spelling), anchors).say(f);
            }
            Error::InUnreachableArm { dead, error } => {
                Displayed(error, Rc::clone(spelling), anchors).say(f);
                unreachable_arm_note(f, dead, spelling);
            }
            Error::InAlternative { written, error } => {
                Displayed(error, Rc::clone(spelling), anchors).say(f);
                alternative_note(f, written);
            }
            // A batch is a report each, as a batch of goals is.
            Error::Batch(errors) => {
                for error in errors {
                    f.extend(Displayed(error, Rc::clone(spelling), anchors).said());
                }
            }
        }
    }
}
