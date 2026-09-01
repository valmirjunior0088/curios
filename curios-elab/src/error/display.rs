//! Rendering an [`Error`] under a [`Spelling`]: the per-arm prose every diagnostic carries, and the goal turnstile the elaborator's goal batches print.
//!
//! The rendering is separated from the error roster for size alone — [`Error`] and its inherent impl are already a long file — and the seam is exact: nothing here decides *what* went wrong, only how it reads. [`Displayed`] is the whole mechanism, and [`goal_text`] is the one piece the roster itself reaches for, when it turns a goal batch into per-goal [`Report`]s.

#[cfg(test)]
mod tests;

use {
    super::{Erased, Error, GoalReport, HeadKey, ShapeDiagnosis},
    curios_core::{Spelling, Subterm, Term},
    curios_utilities::{Grain, Plicity, Qualifier},
    std::{fmt, rc::Rc},
};

/// Whether a goal-scope binder is unnameable — a hintless local no written expression can reference. Its scope line spells `_` the way source does, instead of the synthesized name the rename map would mint for it.
/// One goal's report without its snippet: the turnstile idiom under its own rename map (see [`GoalReport::rename_map`]) — the batch-wide one the caller installed is replaced, not extended, since every name this report shows is in the narrower map by construction. The message half of a goal's [`Report`], and what the batch's `Display` writes before each snippet.
pub(super) fn goal_text(report: &GoalReport, spelling: &Rc<Spelling>) -> String {
    // A report's terms render within a fixed width — the pipeline is pure and stays terminal-blind, so the target is a constant — and a broken term's continuation lines re-indent under the clause body rather than restarting at column zero.
    const WIDTH: usize = 100;

    let shorten = spelling.short_names();
    let spelling = Rc::new(
        spelling
            .as_ref()
            .clone()
            .with_pretty_names(report.rename_map(&shorten)),
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

/// An error paired with the [`Spelling`] its terms render under — the parameter `Display::fmt` cannot take, and the reason every arm below rebinds its term fields before interpolating them. A field left unrebound would silently render core's own spelling, which is why the rebinding is per-arm and mechanical rather than left to each `write!`.
pub(super) struct Displayed<'a>(pub(super) &'a Error, pub(super) Rc<Spelling>);

impl fmt::Display for Displayed<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = &self.1;
        match self.0 {
            Error::ReduceExhausted { term } => {
                let term = term.spelled(spelling);
                write!(f, "reduction ran out of steps on: {term}")
            }
            Error::ConvertExhausted { this, that } => {
                let that = that.spelled(spelling);
                let this = this.spelled(spelling);
                write!(f, "conversion ran out of steps between {this} and {that}")
            }
            Error::TypeMismatch { inferred, expected } => {
                // Reports erase universe instances, which reads better everywhere except here: when the instances *are* the disagreement, both sides render as one string and the message states nothing. Detected on the rendering rather than on the terms, so it covers every axis that could collapse two sides into one spelling and not merely the one that is known to.
                let plain = (
                    inferred.spelled(spelling).to_string(),
                    expected.spelled(spelling).to_string(),
                );
                let detailed = (plain.0 == plain.1).then(|| {
                    let shown = Rc::new(spelling.as_ref().clone().with_shown_universes());
                    let pair = (
                        inferred.spelled(&shown).to_string(),
                        expected.spelled(&shown).to_string(),
                    );
                    // Universes were not the axis that collapsed them: fall back to core's own spelling, which abbreviates nothing. Unreadable beside the shortened form, and better than a message that says two things are unequal in identical words.
                    match pair.0 == pair.1 {
                        false => pair,
                        true => {
                            let bare = Rc::new(Spelling::default());
                            (
                                inferred.spelled(&bare).to_string(),
                                expected.spelled(&bare).to_string(),
                            )
                        }
                    }
                });
                let (shown_inferred, shown_expected) = match detailed {
                    Some(detailed) => detailed,
                    None => plain,
                };
                write!(
                    f,
                    "type mismatch\n  inferred: {shown_inferred}\n  expected: {shown_expected}"
                )?;
                // The collision every beginner meets once: a type handed over where a value of it belonged — `Eq/cong(f, Eq(a, b))` with the statement in the proof's seat. The mismatch line already says `Prop` against `Eq(…)`, but only to a reader who knows a proposition is a type and a proof is its inhabitant, which is the very thing they have not learned yet. One sentence names the level confusion; the `Type` twin is the same mistake with a type where a value belonged.
                let inferred_is_sort = matches!(&***inferred, Subterm::Type(_) | Subterm::Prop);
                let expected_is_sort = matches!(&***expected, Subterm::Type(_) | Subterm::Prop);
                if inferred_is_sort && !expected_is_sort {
                    let (what, held) = match &***inferred {
                        Subterm::Prop => ("a proposition", "a proof of it"),
                        _ => ("a type", "a value of it"),
                    };
                    write!(f, "\n  {what} was given where {held} was expected")?;
                }
                Ok(())
            }
            // "this sequencing", not "this '!'": a hand-written '/syn/Monad/bind' call reaches the same report, and nothing on the term records which spelling produced it.
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
            Error::UniverseInconsistency { lower, upper, path } => {
                write!(
                    f,
                    "this Type would need to be strictly below itself\n  required constraint: {lower} ≤ {upper}"
                )?;
                if path.len() > 1 {
                    write!(f, "\n  inconsistency path has {} steps", path.len())?;
                }
                Ok(())
            }
            Error::UniverseInvariant(message) => {
                write!(f, "invalid inferred universe state: {message}")
            }
            Error::NotAFunction { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(f, "applied a non-function\n  head has type: {head_type}")
            }
            Error::NotAFunctionType { expected } => {
                let expected = expected.spelled(spelling);
                write!(
                    f,
                    "introduced a lambda where the expected type is not a function type\n  expected: {expected}"
                )
            }
            Error::NotATuple { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(
                    f,
                    "projected from a non-tuple\n  head has type: {head_type}"
                )
            }
            Error::NotATupleType { expected } => {
                let expected = expected.spelled(spelling);
                write!(
                    f,
                    "introduced a tuple where the expected type is not a tuple type\n  expected: {expected}"
                )
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
                write!(f, "duplicate inductive declaration '{name}'")
            }
            Error::DuplicateStruct { name } => {
                write!(f, "duplicate struct declaration '{name}'")
            }
            Error::DuplicateConcept { name } => {
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
            Error::WrongNumberOfArguments { expected, got } => {
                write!(
                    f,
                    "wrong number of arguments: expected {expected}, got {got}"
                )
            }
            Error::BinderPlicityMismatch {
                position,
                expected,
                written,
            } => {
                let requirement = match expected {
                    Plicity::Explicit => "an explicit parameter (written with no mark)",
                    Plicity::Implicit => "an implicit parameter (written with `@`)",
                    Plicity::Witness => "a witness parameter (written with `use`)",
                };
                let written = match written {
                    Plicity::Explicit => "written with no mark",
                    Plicity::Implicit => "written with `@`",
                    Plicity::Witness => "written with `use`",
                };
                write!(
                    f,
                    "function parameter {position} is {requirement}, but was {written}"
                )
            }
            Error::UnknownMatchConstructor { type_name, tag } => {
                write!(f, "match arm '{tag}' is not a constructor of '{type_name}'")
            }
            Error::MatchCaseMissing { term, atom } => {
                let term = term.spelled(spelling);
                write!(f, "missing match case for constructor '{atom}': {term}")
            }
            Error::NotAInductType { head_type } => {
                let head_type = head_type.spelled(spelling);
                write!(
                    f,
                    "matched inductive constructors on a non-inductive type\n  head has type: {head_type}"
                )
            }
            Error::LargeElimOfProp { name } => {
                write!(
                    f,
                    "cannot eliminate the proposition '{name}' into a relevant result\n  a strict proposition admits large elimination only when empty or singleton"
                )
            }
            Error::InformativePropStruct {
                name,
                field,
                field_type,
            } => {
                let field_type = field_type.spelled(spelling);
                write!(
                    f,
                    "struct '{name}' is declared at sort 'Prop' but field '{field} : {field_type}' is informative\n  a 'Prop' struct's fields must all be propositions (or forced by indices)"
                )
            }
            Error::NotStrictlyPositive {
                name,
                site,
                site_type,
                polarity,
            } => {
                let site_type = site_type.spelled(spelling);
                write!(
                    f,
                    "'{name}' is not strictly positive: through '{site} : {site_type}' it occurs in itself {polarity}\n  a recursive occurrence must be a plain payload, never left of an arrow"
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
                match offender {
                    Some(offender) => write!(
                        f,
                        "{site} is a {erased} position but reaches '{offender}', which is not known to terminate\n  {advice}"
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
                write!(
                    f,
                    "struct '{name}' takes {expected} type argument(s) but got {got}"
                )
            }
            Error::WrongNumberOfFields {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "struct '{name}' has {expected} field(s) but the literal supplies {got}"
                )
            }
            Error::UnknownDeclaration { name } => {
                write!(f, "no declaration for '{name}'")
            }
            Error::UnknownStructField {
                name,
                label,
                available,
            } => {
                write!(
                    f,
                    "struct '{name}' has no field '{label}' at that position (fields in order: {})",
                    available.join(", ")
                )
            }
            Error::UseEntryOutsideConcept { name } => {
                write!(
                    f,
                    "'use' entries are only legal in concept literals — struct '{name}' is not a concept"
                )
            }
            Error::TooManyUseEntries {
                name,
                expected,
                got,
            } => {
                write!(
                    f,
                    "literal supplies {got} 'use' entr{} but concept '{name}' has only {expected} 'use' field(s)",
                    if *got == 1 { "y" } else { "ies" }
                )
            }
            Error::SpreadNotFirst { name } => {
                write!(
                    f,
                    "a '..' spread must be the first entry of the '{name}' literal"
                )
            }
            Error::MultipleSpreads { name } => {
                write!(
                    f,
                    "a struct literal takes at most one '..' spread, but the '{name}' literal has more"
                )
            }
            Error::SpreadBaseTypeMismatch { name, found } => {
                let found = found.spelled(spelling);
                write!(
                    f,
                    "the '..' base of a '{name}' literal must itself be a '{name}'\n  found: {found}"
                )
            }
            Error::UnlabeledSpreadOverride { name } => {
                write!(
                    f,
                    "overrides after a '..' spread must be labeled ('field = value') in the '{name}' literal"
                )
            }
            Error::SpreadOverrideOutOfOrder { name, label, order } => {
                write!(
                    f,
                    "overrides after a '..' spread must follow '{name}''s declared field order ({}); '{label}' is repeated or out of place",
                    order.join(", ")
                )
            }
            Error::PrivateField { name, field } => {
                write!(
                    f,
                    "field '{field}' of struct '{name}' is private to its declaring module and its descendants"
                )
            }
            Error::PrivateRepresentation { name } => {
                write!(
                    f,
                    "the representation of type '{name}' is private to its declaring module and its descendants"
                )
            }
            Error::NoDerivation { concept } => {
                write!(
                    f,
                    "no derivation exists for '{concept}'; write the body\n  a body-less `satisfy` asks the compiler to write the witness, which it does only for a concept registered as derivable"
                )
            }
            Error::DeriveOutsideWitness => {
                write!(
                    f,
                    "a derived body is legal only as a witness body checked against its concept application"
                )
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
            Error::UnboundVariable { term } => {
                let term = term.spelled(spelling);
                write!(f, "unbound variable: {term}")
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
                deferred_witnesses,
            } => {
                let this = this.spelled(spelling);
                let that = that.spelled(spelling);
                write!(
                    f,
                    "cannot decide a postponed conversion\n  between: {this}\n      and: {that}"
                )?;
                if !watching.is_empty() {
                    write!(f, "\n  never solved: {}", watching.join(", "))?;
                }
                if *under_refinements {
                    write!(
                        f,
                        "\n  the goal sits under match-arm refinements; a solution holding only under them is never committed"
                    )?;
                }
                for goal in deferred_witnesses {
                    write!(
                        f,
                        "\n  a witness for '{goal}' is not declared by this point in elaboration order, and this conversion must unfold through it within its own declaration\n  name the operation the witness supplies directly, or declare the witness where this declaration's dependencies can order it first"
                    )?;
                }
                Ok(())
            }
            Error::BangRegionUndetermined => {
                write!(
                    f,
                    "the monad of this region was never determined\n  a '!' sequences in its region's monad, which is read from the region's type\n  annotate the enclosing result type to fix the monad"
                )
            }
            Error::OperatorUndefined { symbol, type_ } => {
                let type_ = type_.spelled(spelling);
                write!(f, "operator '{symbol}' is not defined for type {type_}")
            }
            Error::UninferredImplicit { func, binder } => {
                write!(
                    f,
                    "implicit argument '{binder}' of '{func}' was not inferred; supply it explicitly: {func}(@...)"
                )
            }
            Error::DomainNeverDetermined { binder } => {
                write!(
                    f,
                    "the type of parameter '{binder}' was never determined\n  a lambda's parameter type comes from its annotation, its body, or its position's expected type, and none supplied one\n  annotate the parameter: ({binder}: T) => ..."
                )
            }
            Error::TooManyImplicits { expected, got } => {
                write!(
                    f,
                    "call supplies {got} '@' argument(s) but the function has only {expected} implicit parameter(s)"
                )
            }
            Error::TooManyWitnessArgs { expected, got } => {
                write!(
                    f,
                    "call supplies {got} 'use' argument(s) but the function has only {expected} 'use' parameter(s)"
                )
            }
            Error::WitnessCycle { this, that } => {
                let this = this.spelled(spelling);
                let that = that.spelled(spelling);
                write!(
                    f,
                    "witnesses for {this} and {that} resolve each other\n  a witness may recurse through its own entry, but a cycle between two has no order to declare them in — whichever comes first names the other before it exists\n  declare them as one group, 'satisfy C(A) {{ ... }} and D(B) {{ ... }}', whose members resolve through one another"
                )
            }
            Error::NoWitness {
                goal,
                func,
                binder,
                embedding,
                shape,
            } => {
                let goal = goal.spelled(spelling);
                write!(f, "no witness of {goal} found")?;
                if let Some(diagnosis) = shape {
                    let ShapeDiagnosis { wanted, bare } = &**diagnosis;
                    // Which identity the twin dropped decides the sentence: a differing tuple pair means labels were dropped, otherwise the difference sits in a function type's marks. A key that mixes both surprises takes the label sentence — the remedy line names declaring the shape's witness either way.
                    let labels = wanted.0.iter().zip(bare.0.iter()).any(|(wanted, bare)| {
                        matches!(wanted, HeadKey::TupleType(_)) && wanted != bare
                    });
                    match labels {
                        true => write!(
                            f,
                            "\n  labels are part of the type: the witness for {bare} does not cover {wanted}\n  name a struct for the labeled product, or declare the witness for this shape"
                        )?,
                        false => write!(
                            f,
                            "\n  plicity marks are part of the type: the witness for {bare} does not cover {wanted}\n  declare the witness for this shape"
                        )?,
                    }
                }
                match embedding {
                    None => write!(f, "\n  needed by '{func}' for {binder}"),
                    Some(diagnosis) => {
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
                // Rendered as a one-element batch so the safety-net spelling and the compile path's [`Error::Goals`] can never drift; this form carries no occurrence span or candidates of its own.
                let report = GoalReport {
                    span: None,
                    scope: scope.clone(),
                    goal: (**goal).clone(),
                    solution: solution.as_deref().cloned(),
                    obligations: Vec::new(),
                    candidates: Vec::new(),
                };
                Displayed(&Error::Goals(vec![report]), Rc::clone(spelling)).fmt(f)
            }
            Error::Goals(reports) => {
                // Each entry is the single-goal turnstile idiom followed by its own snippet — message first, then location, matching how `reports` orders a `Located` diagnostic. Entries are separated by a blank line.
                for (index, report) in reports.iter().enumerate() {
                    if index > 0 {
                        write!(f, "\n\n")?;
                    }
                    f.write_str(&goal_text(report, spelling))?;
                    if let Some(span) = &report.span {
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
            } => {
                let noun = match key.0.len() {
                    1 => "head",
                    _ => "key",
                };
                write!(
                    f,
                    "duplicate witness of '{concept}' for {noun} '{key}'\n  one is declared in {}, another in {}\n  every concept-{noun} pair has at most one witness, program-wide",
                    declaring_module(first),
                    declaring_module(second)
                )
            }
            Error::OrphanWitness {
                concept,
                key,
                witness,
            } => {
                let noun = match key.0.len() {
                    1 => "head",
                    _ => "key",
                };
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
                write!(
                    f,
                    "concept '{concept}' participates in a superclass cycle ('use'-marked fields must form an acyclic graph)"
                )
            }
            Error::UnknownSuperclass { concept, target } => {
                write!(
                    f,
                    "concept '{concept}' names '{target}' as a superclass, but '{target}' is not a registered concept"
                )
            }
            Error::InvalidWitnessHead {
                witness,
                position,
                head,
            } => {
                let head = head.spelled(spelling);
                write!(
                    f,
                    "witness '{witness}' cannot be keyed: its concept's parameter {n} reduces to {head}\n  every parameter's head must be an inductive, a struct, an intrinsic type, a tuple type, or a function type",
                    n = position + 1
                )
            }
            Error::ParameterlessWitnessConcept { witness, concept } => {
                write!(
                    f,
                    "witness '{witness}' cannot be registered: concept '{concept}' has no parameters to key on\n  a global witness keys on its concept's parameter heads; supply a parameterless concept through a local 'use' binder instead"
                )
            }
            Error::NotAConcept { witness, found } => {
                let found = found.spelled(spelling);
                write!(
                    f,
                    "witness '{witness}' does not witness a concept\n  its annotation elaborates to: {found}"
                )
            }
            Error::NonRegularWitnessPremise { witness, premise } => {
                let premise = premise.spelled(spelling);
                write!(
                    f,
                    "witness '{witness}' has a non-regular premise: {premise}\n  every 'use' premise must apply its concept to variables bound by the witness's own parameters"
                )
            }
            Error::ExplicitWitnessParam { witness } => {
                write!(
                    f,
                    "witness '{witness}' declares an explicit parameter\n  witness parameters must be implicit ('@') or 'use' premises — nothing supplies explicit arguments during resolution"
                )
            }
            Error::NatOverflow { value } => {
                write!(f, "Nat literal {value} overflows u32 at the erase boundary")
            }
            Error::ErasedModuleInvalid { detail } => {
                write!(f, "the erased module failed verification: {detail}")
            }
            Error::IntOverflow { value } => {
                write!(
                    f,
                    "Int literal {value:+} overflows i32 at the erase boundary"
                )
            }
            Error::MotiveBinderCount {
                name,
                expected,
                written,
            } => {
                let subject = match name {
                    Some(name) => format!("eliminating '{name}'"),
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
            Error::ByteLiteralOutOfRange { value } => {
                write!(f, "Byte literal {value} is out of range (expected 0..=255)")
            }
            Error::BoolLiteralOutOfRange { value } => {
                write!(f, "Bool literal {value} is out of range (expected 0..=1)")
            }
            Error::FltLiteralOutOfRange { value } => {
                write!(f, "Flt literal {value} overflows the finite range")
            }
            // `render_body` intercepts both wrappers before a real spelling ever reaches this match, but these arms must not rely on that: interpolating `{error}` would route through `Display for Error`, silently resetting a nested term's spelling to core's default. Recurse with the spelling in hand instead.
            Error::InDeclaration { name, error } => {
                writeln!(f, "while elaborating {name}:")?;
                Displayed(error, Rc::clone(spelling)).fmt(f)
            }
            Error::Located { error, .. } => Displayed(error, Rc::clone(spelling)).fmt(f),
        }
    }
}
