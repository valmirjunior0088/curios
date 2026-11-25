//! Size-change termination: which `rec` groups descend.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in the places erasure *removes*, because a divergent type breaks type formation and a divergent proof proves anything, while a program that loops is only a program that loops. This module supplies the fact both checkers need: whether a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in the shape Abel's `foetus` gave it for dependent types). Each recursive call contributes a **call matrix** grading every callee argument against every caller parameter as strictly smaller, equal, or unrelated. The matrices are closed transitively under composition, and the group is accepted when every **idempotent** matrix in that closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the standard library needs it. `/std/List/sort_by`'s `merge` descends on *either* of two lists depending on the arm, and `/std/Cli/check`'s `problems` and `beneath` close only across their two members, as `/std/Cli/parse`'s `command` and `chosen`, `/std/Cli`'s `variables` and `variables_of`, and `/std/Tui`'s `asked` and `all` do. A rule keyed to one designated argument rejects all of them, and a fold cannot express them either, because a fold cannot short-circuit. `problems` is reached by a type — `/std/Cli/WellFormed` is `Clear(check/problems(command, version))`, a proposition every `Cli` holds a proof of — so a structural rule would leave a declared command line's type unformed.
//!
//! **Match refinement is part of the size order, not an optimization.** A parameter scrutinized by an enclosing arm is expanded through that arm's constructor before it is compared, so in `merge`'s `[y, ..ys]` arm the tail `ys` grades *smaller* than `b`, and in an arm that refined `x` to the empty run a literal `0x[]` argument grades *equal* to `x` rather than unknown. Without that, matrices of calls that cannot follow each other compose to one with no decrease anywhere on its diagonal, and the group is rejected on a call path that cannot actually occur; most of `/std`'s recursive groups would be.
//!
//! **An application of a constructor payload is a child of the constructor, and that is the whole of what lets a proof recurse along an accessibility predicate.** The size order reads a value as a finite tree, and a function-typed payload is a branching node whose children are its applications, so `below(y, r)` sits below `intro(x, below)` for the reason `below` itself does. The premise — that a payload's value *is* a finite tree — is what type totality and proof totality already guarantee at every position this verdict is consulted, since a partial value stored in a payload is refused by reachability before the verdict matters. Agda's structural order states the same rule for any function-typed constructor argument; Rocq's guard states a narrower one keyed to the payload's codomain, which refuses no cycle this admits: every payload is sized under its family, a proposition's as a type's, so none quantifies over the sort its family is in and none is applied at the family to hand a value of it back.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* engine, through [`Driver`]: it is a total function of post-zonk terms, so a second implementation would be a second run of the same function on the same input rather than a second opinion. What differs is the obligation each driver hangs on the verdict. The elaborator's is positional and whole-module — type totality and proof totality, seeded from what elaboration settled, turning a classification short of `Total` into a rejection only where erasure deletes. The certifier's is local and self-derivable: a `rec` member whose declared type is a proof or yields a sort must descend, because assuming it at that type otherwise certifies `rec f : False = f`. Rejection by the *engine* is a classification, not an error — a corecursive or productive group classifies `Unfounded` and stays usable everywhere erasure keeps it.
//!
//! # Discovery, grading, and closure are three things
//!
//! Finding a group's calls, grading each against its caller's parameters, and closing the graded calls into a verdict are separate here. [`grade()`] reads one call's arguments under a [`SizeContext`] — what the arms enclosing it established — and [`decide`] closes a group's calls and demands a descent on every cycle. [`group_totality`] is one way to feed them: the discovery walk, which traverses member bodies for calls on its own. A walk that meets calls another way, while typing the bodies, grades and closes them with the same two functions, so the two differ only in how calls are found.

mod guard;
use guard::*;

mod matrix;
pub use matrix::Call;
use matrix::*;

mod part;
use part::*;
pub use part::{
    Holding, PartShape, Segment, base_part, call_path, group_parts, handed_member,
    held_application, part_calls, peel,
};

mod shape;
use shape::*;

mod grade;
use grade::*;
pub use grade::{
    Answer, Applied, SizeContext, Standing, binder_bounds, grade, handed, handed_within,
    nonzero_by, refined_by_arm, ruled_out,
};

#[cfg(test)]
mod tests;

use {
    crate::Driver,
    curios_common::recurse,
    curios_core::{
        Advance, Applications, Arity, Bound, CallMatrix, Carrier, Cases, FieldApplications,
        Foreign, Free, Func, FuncType, Global, InductType, Intrinsic, Many, Match, One,
        PartApplications, Probe, Proj, RecGroup, Scope, Size, Struct, StructType, Subterm,
        Telescope, Term, Totality, Tuple, TupleType, Two, Variant,
    },
    curios_num::Natural,
    std::{
        collections::{BTreeSet, HashMap, hash_map::Entry},
        slice,
    },
};

/// Whether every recursive call path in `group` descends, discovering the calls by walking its member bodies: `Total` where it does, and `Unfounded` where it does not, a group's verdict being of its own recursion alone.
///
/// This is the whole of size-change termination as the elaborator applies it: collect one matrix per call site, close them under composition, and demand a decrease on the diagonal of every idempotent result — [`decide`]. A group with no recursive call at all closes to nothing and is accepted, which is how the prelude's call-free `; ih` folds pass — their recursion is the intrinsic eliminator's, already structural by construction.
pub fn group_totality<D: Driver>(driver: &mut D, group: &RecGroup) -> Result<Totality, D::Error> {
    curios_common::profile!("group_totality");
    let parts = open_parts(driver, group);
    let shapes = parts
        .iter()
        .map(|part| part.shape.clone())
        .collect::<Vec<_>>();
    let arities = shapes.iter().map(|shape| shape.arity).collect();

    let mut calls = Vec::new();
    for (index, part) in parts.iter().enumerate() {
        let mut walk = Walk {
            driver,
            group: Some(group),
            parts: &shapes,
            arities: &arities,
            caller: index,
            params: &part.params,
            context: SizeContext::default(),
            entered: Vec::new(),
            walked: HashMap::new(),
            calls: Vec::new(),
            tracked: None,
        };
        walk.walk(part)?;
        calls.extend(walk.calls);
    }

    Ok(decide(calls))
}

/// How many matrices a parameter's applications may close to before the parameter is summarized as used otherwise: the conservative direction, as a closure past its bound is `Unfounded`.
const APPLICATIONS_LIMIT: usize = 64;

/// How each part of each member of `group` applies its parameters ([`Applications`]): for a parameter at a position no part uses otherwise than as the head of an application, a field of it applied, or handed on unchanged, itself or a field of it — at that position to a member taking a parameter there, called or itself handed where its callee applies it, or to a definition whose summary applies what it is handed — every application of it the part makes, graded against the part's parameters, and every one reached through a path of the group's calls, graded through the path. A member's leading lambdas are one part, and each field lambda of the record under them another ([`PartShape`]), over the member's parameters and then its own; each states what its own call applies, the leading lambdas' what they apply as they answer the record whatever a field lambda applies, which a reader of a path into a field adds to the field's ([`handed`]).
///
/// **Only where every use is seen.** A use the walk cannot follow — the parameter stored, returned, handed to a definition that states nothing of it, handed to a member at another position or past its parameters, or named in a type — leaves the position unsummarized in every part, and a call through it is graded as it is without a summary. A callee's applications at a place are counted in a part's summary only where the part hands its parameter on there; a member call handing that place something other than the parameter, from a part that hands the parameter on there in another call, hands the callee another function, whose applications the summary then counts as well: more matrices claim less, never more.
pub fn group_applications<D: Driver>(
    driver: &mut D,
    group: &RecGroup,
) -> Result<Vec<Applications>, D::Error> {
    curios_common::profile!("group_applications");
    let parts = open_parts(driver, group);
    summaries(driver, Some(group), parts, group.length())
}

/// How a definition that is no recursive group applies its parameters, as [`group_applications`] reads a member's: `None` where it applies none, and where no parameter of its leading lambdas is of a type that is applied or projected, whose body is not walked.
pub fn definition_applications<D: Driver>(
    driver: &mut D,
    body: &Term,
) -> Result<Option<Applications>, D::Error> {
    curios_common::profile!("definition_applications");
    let Some(parts) = open_definition(driver, body)? else {
        return Ok(None);
    };
    let applications = summaries(driver, None, parts, 1)?;
    Ok(applications
        .into_iter()
        .next()
        .filter(|applications| !applications.is_empty()))
}

/// The summaries of `members` members whose parts are `parts`, walked as parts of `group` where there is one, and as parts of no group otherwise ([`group_applications`]).
fn summaries<D: Driver>(
    driver: &mut D,
    group: Option<&RecGroup>,
    parts: Vec<Part>,
    members: usize,
) -> Result<Vec<Applications>, D::Error> {
    let shapes = parts
        .iter()
        .map(|part| part.shape.clone())
        .collect::<Vec<_>>();
    let arities = shapes.iter().map(|shape| shape.arity).collect::<Vec<_>>();

    let mut calls = Vec::new();
    let mut uses = Vec::new();
    for (index, part) in parts.iter().enumerate() {
        let mut walk = Walk {
            driver,
            group,
            parts: &shapes,
            arities: &arities,
            caller: index,
            params: &part.params,
            context: SizeContext::default(),
            entered: Vec::new(),
            walked: HashMap::new(),
            calls: Vec::new(),
            tracked: Some(Tracked::new(arities[index])),
        };
        walk.walk(part)?;
        calls.extend(walk.calls);
        uses.push(walk.tracked.expect("a tracking walk keeps its record"));
    }

    let width = arities.iter().copied().max().unwrap_or(0);
    let seen = (0..width)
        .map(|position| {
            uses.iter()
                .all(|used| !used.escaped.get(position).copied().unwrap_or(false))
        })
        .collect::<Vec<_>>();
    let closed = close(calls);

    Ok((0..members)
        .map(|member| {
            let summaries = member_parts(&shapes, member)
                .filter_map(|part| {
                    let leading = shapes[part].path.is_empty();
                    let mut fields = Vec::new();
                    let applied = (0..arities[part])
                        .map(|position| {
                            let closed = closed.as_ref().filter(|_| seen[position])?;
                            // The applications of the parameter, or of its field at `path`, the part's own call makes: its own, and each reached through a path of the group's calls, a field read out of a member's record calling the leading lambdas that answered it too ([`part_calls`]). A callee's applications at the parameter's place are composed only into a part that hands the parameter on there, being of what it handed, and a field is applied through such a call as the parameter is. What the leading lambdas apply as they answer the record a field lambda is called on is theirs, which a reader of the path adds ([`handed`]).
                            let reached = |path: Option<&[usize]>| {
                                let mut matrices = BTreeSet::new();
                                for applied in uses[part].at(position, path) {
                                    matrices.insert(applied.over(arities[part]));
                                }
                                let handed =
                                    uses[part].handed.get(position).copied().unwrap_or(false);
                                for call in
                                    closed.iter().filter(|call| handed && call.caller == part)
                                {
                                    for applied in uses[call.callee].at(position, path) {
                                        if let Some(through) = call.matrix.compose(&applied) {
                                            matrices.insert(through.over(arities[part]));
                                        }
                                    }
                                }
                                (matrices.len() <= APPLICATIONS_LIMIT)
                                    .then(|| matrices.into_iter().collect::<Vec<_>>())
                            };
                            let own = reached(None)?;
                            let paths = uses
                                .iter()
                                .flat_map(|used| used.paths(position))
                                .collect::<BTreeSet<_>>();
                            let mut found = Vec::new();
                            for path in paths {
                                let applied = reached(Some(&path))?;
                                found.push(FieldApplications {
                                    position,
                                    path,
                                    applied,
                                });
                            }
                            fields.extend(found);
                            Some(own)
                        })
                        .collect();
                    let summary = PartApplications::new(shapes[part].path.clone(), applied, fields);
                    (leading || !summary.is_empty()).then_some(summary)
                })
                .collect();
            Applications::new(summaries)
        })
        .collect())
}

/// What a walk summarizing its member's parameters found of each ([`group_applications`]): the matrices of its applications and of its fields', and whether it is used otherwise.
struct Tracked {
    applied: Vec<Vec<CallMatrix>>,
    /// Each field path of a parameter applied, with the matrix grading that application.
    fields: Vec<Vec<(Vec<usize>, CallMatrix)>>,
    /// Whether a parameter is handed on at its own place to a member taking one there: the place the summary composes the group's calls through, a callee's applications there being of what this part handed it.
    handed: Vec<bool>,
    escaped: Vec<bool>,
}

impl Tracked {
    fn new(params: usize) -> Self {
        Self {
            applied: vec![Vec::new(); params],
            fields: vec![Vec::new(); params],
            handed: vec![false; params],
            escaped: vec![false; params],
        }
    }

    /// The matrices of the applications of the parameter at `position`, or, given `path`, of its field there.
    fn at(&self, position: usize, path: Option<&[usize]>) -> Vec<CallMatrix> {
        match path {
            None => self.applied.get(position).cloned().unwrap_or_default(),
            Some(path) => self
                .fields
                .get(position)
                .into_iter()
                .flatten()
                .filter(|(at, _)| at == path)
                .map(|(_, matrix)| matrix.clone())
                .collect(),
        }
    }

    /// Each field path of the parameter at `position` applied.
    fn paths(&self, position: usize) -> Vec<Vec<usize>> {
        self.fields
            .get(position)
            .into_iter()
            .flatten()
            .map(|(path, _)| path.clone())
            .collect()
    }
}

/// A parameter's field applied, read off its path's steps: the fields projected first, at least one, and every step after them arguments, at least one — `e.0(x, y)`, `e.1.0(y)`. A path any other way, a field of what an application answers among them, is no such application.
fn field_application(steps: &[Segment]) -> Option<(Vec<usize>, Vec<Term>)> {
    let fields = steps
        .iter()
        .take_while(|step| matches!(step, Segment::Field(_)))
        .map(|step| match step {
            Segment::Field(slot) => *slot,
            Segment::Arguments(_) => unreachable!("taken while fields"),
        })
        .collect::<Vec<_>>();
    let rest = &steps[fields.len()..];
    if fields.is_empty()
        || rest.is_empty()
        || rest.iter().any(|step| matches!(step, Segment::Field(_)))
    {
        return None;
    }
    let arguments = rest.iter().flat_map(Segment::arguments).cloned().collect();
    Some((fields, arguments))
}

/// Whether `calls`, every graded call of one group, close to no call path that can repeat forever without a strict decrease: `Total` where they do, `Unfounded` where they do not.
///
/// The closure and the verdict are shared by every walk that finds a group's calls, whichever walk met them.
pub fn decide(calls: Vec<Call>) -> Totality {
    curios_common::profile!("decide");
    match close(calls) {
        // A call from a part to itself whose matrix is not square was graded over other than the part's parameters, so it claims nothing, as a closure that cannot compose it does.
        Some(closed) => match closed.iter().all(|call| {
            call.caller != call.callee
                || (call.matrix.rows() == call.matrix.columns()
                    && (!call.matrix.is_idempotent() || call.matrix.descends()))
        }) {
            true => Totality::Total,
            false => Totality::Unfounded,
        },
        // The closure outgrew its bound; claim nothing.
        None => Totality::Unfounded,
    }
}

/// One member body's traversal: it finds the recursive calls, and has [`grade()`] grade each under the context its arms built.
struct Walk<'a, D: Driver> {
    driver: &'a mut D,
    /// The group whose members the walk reads as callees, and none where it walks a definition that is no group, whose calls are all of other declarations.
    group: Option<&'a RecGroup>,
    /// The group's parts, which a call's path is read against ([`part_calls`]).
    parts: &'a [PartShape],
    /// Each part's arity — the columns a call to it is graded over.
    arities: &'a Vec<usize>,
    /// The part whose runs are walked, and its parameters: its member's, and a field lambda's after them.
    caller: usize,
    params: &'a [Free],
    /// What the arms enclosing the position being walked have established, entered on the way into an arm and left on the way out.
    context: SizeContext,
    /// The nested groups whose bodies the walk is currently inside, entered and left with them. A group reached from within itself would regenerate its own bodies without end, since every member reference materializes as a projection carrying the whole group.
    entered: Vec<RecGroup>,
    /// The `let` values the walk is past, in a later binding of their `let` or in its tail, each walked where its `let` binds it, and how many `let`s bind each. The walk reads a later binding and the tail with each binder standing for its value, so they hold a copy at every use; a copy is the value's calls again, with the same arguments, and what held where the `let` binds it still holds wherever one stands. So the matrix graded at the binding describes every copy, and a copy is passed over. Scoped to the `let`: past it the same term is another position, read under what holds there.
    walked: HashMap<Term, usize>,
    calls: Vec<Call>,
    /// The record of its part's parameters a walk for [`group_applications`] keeps, and `None` for a walk deciding descent alone, which grades nothing but the group's calls and so spends exactly what the decision does.
    tracked: Option<Tracked>,
}

impl<D: Driver> Walk<'_, D> {
    /// Record a call to member `callee` with `arguments`, graded against the caller's parameters under what the enclosing arms established.
    fn call(&mut self, callee: usize, arguments: &[Term]) -> Result<(), D::Error> {
        let call = grade(
            self.driver,
            &self.context,
            self.caller,
            self.params,
            callee,
            self.arities[callee],
            arguments,
        )?;
        self.calls.push(call);
        Ok(())
    }

    /// Where a walk summarizing its member's parameters keeps a record, the position of `name` among them.
    fn tracked_position(&self, name: Option<&Free>) -> Option<usize> {
        let tracked = self.tracked.as_ref()?;
        self.params
            .iter()
            .position(|param| Some(param) == name)
            .filter(|position| *position < tracked.escaped.len())
    }

    /// Where `term` is a parameter a summarizing walk keeps a record of, or a field of one, `e.1.0`: the parameter's position, and the slots the term projects out of it, in order.
    fn tracked_path(&self, term: &Term) -> Option<(usize, Vec<usize>)> {
        let (root, steps) = call_path(term);
        let Subterm::Var(var) = &*root else {
            return None;
        };
        let position = self.tracked_position(var.as_free())?;
        let slots = steps
            .iter()
            .map(|step| match step {
                Segment::Field(slot) => Some(*slot),
                Segment::Arguments(_) => None,
            })
            .collect::<Option<Vec<_>>>()?;
        Some((position, slots))
    }

    /// Whether `term` names a parameter a summarizing walk keeps a record of.
    fn names_tracked(&self, term: &Term) -> bool {
        self.tracked.is_some() && self.params.iter().any(|param| term.mentions_free(param))
    }

    /// The member of the group walked that `term` names, where it names one: a projection of any other group names none, and so does every term where no group is walked.
    fn member(&self, term: &Term) -> Option<usize> {
        let (group, member) = term.as_rec_proj()?;
        (Some(group) == self.group).then_some(member)
    }

    /// Record an application of the parameter at `position` to `arguments`, or of its field at `path`, graded against the member's parameters under what the enclosing arms established.
    fn applied(
        &mut self,
        position: usize,
        path: Option<Vec<usize>>,
        arguments: &[Term],
    ) -> Result<(), D::Error> {
        let graded = grade(
            self.driver,
            &self.context,
            self.caller,
            self.params,
            self.caller,
            arguments.len(),
            arguments,
        )?;
        self.recorded(position, path, graded.matrix);
        Ok(())
    }

    /// Record an application a list operation makes of the parameter at `position`, its function operand, on each element of `list`: the element below `list` as `list` stands to the member's parameters, and the rest of its `arity` arguments, a fold's accumulator, claiming nothing.
    fn element_applied(
        &mut self,
        position: usize,
        list: &Term,
        arity: usize,
    ) -> Result<(), D::Error> {
        let graded = grade(
            self.driver,
            &self.context,
            self.caller,
            self.params,
            self.caller,
            1,
            slice::from_ref(list),
        )?;
        let mut matrix = CallMatrix::unknown(self.params.len(), arity);
        for row in 0..self.params.len() {
            matrix.set(row, 0, Size::Less.compose(graded.matrix.entry(row, 0)));
        }
        self.recorded(position, None, matrix);
        Ok(())
    }

    /// Keep `matrix`, an application of the parameter at `position` or of its field at `path`, in the record a summarizing walk keeps.
    fn recorded(&mut self, position: usize, path: Option<Vec<usize>>, matrix: CallMatrix) {
        if let Some(tracked) = &mut self.tracked {
            match path {
                None => tracked.applied[position].push(matrix),
                Some(path) => tracked.fields[position].push((path, matrix)),
            }
        }
    }

    /// Record that each parameter `handed` lists is handed on at its own place, where a summarizing walk keeps a record of its parameters.
    fn record_handed_on(&mut self, handed: &[(usize, &Term)]) {
        if let Some(tracked) = &mut self.tracked {
            for (position, _) in handed {
                if let Some(slot) = tracked.handed.get_mut(*position) {
                    *slot = true;
                }
            }
        }
    }

    /// Record a use of `name` the walk does not follow, where it is a parameter a summarizing walk keeps a record of.
    fn escaped(&mut self, name: Option<&Free>) {
        if let Some(position) = self.tracked_position(name)
            && let Some(tracked) = &mut self.tracked
        {
            tracked.escaped[position] = true;
        }
    }

    /// Walk a lambda handed where its callee applies it as `handed` grades: each entry type, then the body with every binder standing to this member's parameters as [`binder_bounds`] reads them.
    fn walk_handed(
        &mut self,
        telescope: Telescope<Term>,
        handed: &[CallMatrix],
    ) -> Result<(), D::Error> {
        let mut cursor = telescope.cursor();
        let mut binders = Vec::new();
        while let Some((_, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            binders.push(cursor.advance_fresh(|hint| self.driver.fresh(hint)));
        }
        let body = cursor.body().expect("a cursor past every entry");
        self.context
            .assume(binder_bounds(handed, self.params, &binders));
        let walked = self.walk_term(&body);
        self.context.exit();
        walked
    }

    /// Walk an argument a call hands where its callee applies it as `applied` grades, and as any term where nothing applies it: a lambda's binders stand as its applications establish ([`Walk::walk_handed`]), a record literal hands each field what its path is applied with, a member of this group, named bare or applied to its own arguments, makes the calls of its parts each application reaches ([`handed_member`]), and a definition applied to its leading lambdas' arguments hands them what its parts apply of them ([`Walk::walk_within`]).
    fn walk_argument(
        &mut self,
        argument: &Term,
        applied: Option<&Applied>,
    ) -> Result<(), D::Error> {
        let Some(applied) = applied.filter(|applied| !applied.is_empty()) else {
            return self.walk_term(argument);
        };
        // A parameter handed whole, or a field of one, is applied where the callee applies what it is handed, as its summary states, through this call: those applications are this member's own, already graded against its parameters ([`handed`]), of the parameter at the path the argument projects, and of its fields at that path continued.
        if let Some((position, projected)) = self.tracked_path(argument) {
            let at = |path: &[usize]| {
                let at = [projected.as_slice(), path].concat();
                (!at.is_empty()).then_some(at)
            };
            for matrix in &applied.itself {
                self.recorded(position, at(&[]), matrix.clone());
            }
            for (path, matrices) in &applied.fields {
                for matrix in matrices {
                    self.recorded(position, at(path), matrix.clone());
                }
            }
            return Ok(());
        }
        match &**argument {
            Subterm::Func(Func { telescope, .. }) => {
                return self.walk_handed(telescope.clone(), &applied.itself);
            }
            Subterm::Struct(Struct { params, fields, .. }) => {
                self.walks(params)?;
                return self.walk_fields(fields, applied);
            }
            Subterm::Tuple(Tuple { fields, .. }) => return self.walk_fields(fields, applied),
            _ => {}
        }
        let (root, steps) = call_path(argument);
        let member = self.member(&root);
        let given = steps
            .iter()
            .map(|step| match step {
                Segment::Arguments(arguments) => Some(arguments.iter().cloned()),
                Segment::Field(_) => None,
            })
            .collect::<Option<Vec<_>>>()
            .map(|lists| lists.into_iter().flatten().collect::<Vec<_>>());
        let Some(given) = given else {
            return self.walk_term(argument);
        };
        let Some(member) = member else {
            return self.walk_within(argument, &root, &steps, &given, applied);
        };
        let base = base_part(self.parts, member);
        let graded = match given.len() {
            0 => None,
            count if count == self.arities[base] => Some(
                grade(
                    self.driver,
                    &self.context,
                    self.caller,
                    self.params,
                    base,
                    count,
                    &given,
                )?
                .matrix,
            ),
            _ => return self.walk_term(argument),
        };
        let rows = self.params.len();
        for (callee, matrix) in handed_member(self.parts, member, rows, graded.as_ref(), applied) {
            self.calls.push(Call {
                caller: self.caller,
                callee,
                matrix,
            });
        }
        // A parameter the member is handed at its own place, where its arguments fill its leading lambdas, is the member's parameter there, whose applications the summary reaches through the calls above, as it reaches one a call hands on: a witness that hands itself, applied to its own parameters, to a container's publishes how it applies them.
        let (handed, rest): (Vec<_>, Vec<_>) =
            given.iter().enumerate().partition(|(position, argument)| {
                graded.is_some()
                    && self.tracked.is_some()
                    && matches!(&***argument, Subterm::Var(var) if var.as_free() == self.params.get(*position))
            });
        self.record_handed_on(&handed);
        let rest = rest
            .into_iter()
            .map(|(_, argument)| argument.clone())
            .collect::<Vec<_>>();
        self.walks(&rest)
    }

    /// Walk an application of a definition to every argument its leading lambdas take, `given`, where it is handed to a definition that applies it as `applied` grades: each argument is handed what the part each application reaches applies of it ([`handed_within`]), beside what the application's own call hands it, and is walked as any term is where that claims nothing. An argument any other way is walked as it stands.
    fn walk_within(
        &mut self,
        argument: &Term,
        root: &Term,
        steps: &[Segment],
        given: &[Term],
        applied: &Applied,
    ) -> Result<(), D::Error> {
        let (Subterm::Instance(instance), [Segment::Arguments(_)]) = (&**root, steps) else {
            return self.walk_term(argument);
        };
        let Some(within) = handed_within(
            self.driver,
            &self.context,
            self.params,
            &instance.name,
            given,
            applied,
        )?
        else {
            return self.walk_term(argument);
        };
        let own = handed(
            self.driver,
            &self.context,
            self.params,
            &instance.name,
            steps,
            Answer::Held,
        )?;
        for ((argument, own), within) in given.iter().zip(own).zip(within) {
            let handed = within.map(|within| within.beside(own.unwrap_or_default()));
            // An application nests as deep as the type it is the witness of is written.
            recurse(|| self.walk_argument(argument, handed.as_ref()))?;
        }
        Ok(())
    }

    /// Walk `callee` applied to `given`, held at a field of the record this part answers: the leading lambdas answer it held, each argument handed what they apply of it, and its parts are this member's ([`Held`]). A parameter, or a field of one, their summary states and applies nothing of is applied by those parts alone; any other argument is walked as it stands, a member among them, which every call reaching one of those parts reaches here too.
    fn walk_answering(&mut self, callee: &Global, given: &[Term]) -> Result<(), D::Error> {
        let steps = [Segment::Arguments(given.to_vec())];
        let handed = handed(
            self.driver,
            &self.context,
            self.params,
            callee,
            &steps,
            Answer::Held,
        )?;
        let leading = self
            .driver
            .applications(callee)
            .and_then(|applications| applications.part(&[]).cloned());
        for (position, (argument, applied)) in given.iter().zip(handed).enumerate() {
            let stated = leading
                .as_ref()
                .is_some_and(|leading| leading.of(position).is_some());
            if applied.is_none() && stated && self.tracked_path(argument).is_some() {
                continue;
            }
            recurse(|| self.walk_argument(argument, applied.as_ref()))?;
        }
        Ok(())
    }

    /// Walk a part a field holds by reference, the call of its definition's part: each argument that is a parameter, a field of one or the part's own is handed what that part applies of it. Any other argument is walked where the record is answered ([`Walk::walk_answering`]), whose leading lambdas every call reaching this part reaches too, so this part makes no call of the group.
    fn walk_held(&mut self, held: &Held) -> Result<(), D::Error> {
        let own = self.params[self.params.len() - held.own..]
            .iter()
            .map(Term::free_var)
            .collect::<Vec<_>>();
        let mut steps = vec![Segment::Arguments(held.given.clone())];
        steps.extend(held.path.iter().map(|slot| Segment::Field(*slot)));
        if !own.is_empty() {
            steps.push(Segment::Arguments(own.clone()));
        }
        let handed = handed(
            self.driver,
            &self.context,
            self.params,
            &held.callee,
            &steps,
            Answer::Escapes,
        )?;
        for (argument, applied) in held.given.iter().chain(&own).zip(handed) {
            if self.tracked_path(argument).is_some() {
                recurse(|| self.walk_argument(argument, applied.as_ref()))?;
            }
        }
        Ok(())
    }

    /// Walk a record literal's fields where a callee applies the record as `applied` grades, each handed what its own path is applied with.
    fn walk_fields(&mut self, fields: &[Term], applied: &Applied) -> Result<(), D::Error> {
        for (slot, field) in fields.iter().enumerate() {
            self.walk_argument(field, Some(&applied.field(slot)))?;
        }
        Ok(())
    }

    /// The reader of terms as sizes, under the context this walk has built so far.
    fn grader(&mut self) -> Grader<'_, D> {
        Grader::new(self.driver, &self.context)
    }

    /// One fresh binder per position of a scope, carrying the written hints so a shape reads in the user's own names.
    fn binders<A: Arity>(&mut self, scope: &Scope<A>) -> Vec<Free> {
        (0..scope.arity())
            .map(|index| self.driver.fresh(scope.hint(index)))
            .collect()
    }

    /// Open a runtime-arity scope — an inductive arm, a `let` tail, a `rec` tail — against fresh binders.
    ///
    /// The three openers here stay separate on purpose. `Scope<A>::open` takes `A::Params`, a *fixed-size* type per arity, which is what stops a two-binder arm from being opened at three terms; collapsing them would mean a slice-taking opener in `curios-core` that re-admits exactly that mistake, to save three lines apiece.
    fn open_many(&mut self, scope: &Scope<Many>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = terms.iter().collect::<Vec<_>>();
        let body = scope.open(&refs);
        (binders, body)
    }

    /// Open the predecessor arm of the `Nat` case split.
    fn open_one(&mut self, scope: &Scope<One>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0]]);
        (binders, body)
    }

    /// Open the `(head, tail)` arm of the `Bytes`/`List` case splits.
    fn open_two(&mut self, scope: &Scope<Two>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1]]);
        (binders, body)
    }

    /// Find the recursive calls in what `part`'s call runs, tracking the refinements that make their arguments comparable: its terms, each definition its record holds answered held ([`Walk::walk_answering`]), and, for a part a field holds, its call of that definition's part ([`Walk::walk_held`]).
    ///
    /// Scope discipline holds by construction: an arm's refinement is taken on by [`Walk::scoped`], which walks the body and puts it back, so no path can drop a refinement or carry one out.
    fn walk(&mut self, part: &Part) -> Result<(), D::Error> {
        self.walks(&part.runs)?;
        for (callee, given) in &part.answers {
            self.walk_answering(callee, given)?;
        }
        if let Some(held) = &part.held {
            self.walk_held(held)?;
        }

        assert!(
            self.context.is_balanced() && self.entered.is_empty(),
            "the walk left a scope open"
        );
        Ok(())
    }

    /// Walk one term, scheduling its children by descending into them.
    ///
    /// Guarded by [`recurse`] because a member body nests as deep as the source that spelled it, and a generated term is bounded by nothing this analysis controls.
    ///
    /// **Arms are reached one at a time, and that is what makes this walk faithful.** Every arm's guard read, shape read, binder minting and body materialization happens in the arm's own call, never batched ahead of it — because those are effects on the checker driving the analysis: [`Driver::force`] spends a reduction budget and [`Driver::fresh`] mints an identity, so the order the arms are reached in *is* the order those effects land in. A differently-ordered spend against a nearly exhausted budget reads a different shape, which would be a verdict change with no cause in the term. The recursion gives that ordering.
    fn walk_term(&mut self, term: &Term) -> Result<(), D::Error> {
        recurse(|| self.step(term))
    }

    /// Take on what an arm establishes, walk its body under it, and put it back.
    fn scoped(
        &mut self,
        refines: impl IntoIterator<Item = (Free, Shape)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
        body: &Term,
    ) -> Result<(), D::Error> {
        self.context.open(refines, nonzero, payloads);
        self.walk_term(body)?;
        self.context.exit();
        Ok(())
    }

    /// What an arm whose case opens `binders` refines: the binder its scrutinee is, to the case's `shape`, or where the scrutinee `head` is an expression, each of `binders` to below the expression's value ([`below_scrutinee`]), read in the arm's own call.
    fn arm_refines(
        &mut self,
        head: &Term,
        scrutinee: Option<Free>,
        shape: Shape,
        binders: &[Free],
    ) -> Result<Vec<(Free, Shape)>, D::Error> {
        if let Some(scrutinee) = scrutinee {
            return Ok(vec![(scrutinee, shape)]);
        }
        if binders.is_empty() {
            return Ok(Vec::new());
        }
        let below = below_scrutinee(&self.grader().shape_of(head)?);

        Ok(below
            .map(|below| {
                binders
                    .iter()
                    .map(|binder| (*binder, below.clone()))
                    .collect()
            })
            .unwrap_or_default())
    }

    /// Walk each of `terms`, in order.
    fn walks<'t>(&mut self, terms: impl IntoIterator<Item = &'t Term>) -> Result<(), D::Error> {
        for term in terms {
            self.walk_term(term)?;
        }
        Ok(())
    }

    /// Open a runtime-arity scope against fresh binders and walk its body.
    fn open_many_walk(&mut self, scope: &Scope<Many>) -> Result<(), D::Error> {
        let (_, body) = self.open_many(scope);
        self.walk_term(&body)
    }

    /// Walk a lambda's telescope applied to `arguments`: each entry type, then the body with every binder standing for its argument. A binder past the last argument is minted fresh as `walk_terms` would; an argument past the last binder stays applied to the body, which may be a lambda in its turn.
    fn walk_redex(
        &mut self,
        telescope: Telescope<Term>,
        arguments: &[Term],
    ) -> Result<(), D::Error> {
        let mut remaining = arguments.iter();
        let mut cursor = telescope.cursor();
        while let Some((hint, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            let argument = match remaining.next() {
                Some(argument) => argument.clone(),
                None => Term::free_var(&self.driver.fresh(hint)),
            };
            cursor.advance(argument);
        }

        let body = cursor.body().expect("a cursor past every entry");
        let leftover = remaining.cloned().collect::<Vec<_>>();
        match leftover.is_empty() {
            true => self.walk_term(&body),
            false => self.walk_term(&Term::apply(body, leftover)),
        }
    }

    /// Walk a `Func`/`FuncType` telescope: each entry, then the terminal.
    ///
    /// A loop rather than a recursion, because a telescope is a list and this iterates it. Each binder is minted only after the entry before it has been walked, which is where the recursive walk minted it.
    fn walk_terms(&mut self, telescope: Telescope<Term>) -> Result<(), D::Error> {
        let mut cursor = telescope.cursor();
        while let Some((_, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            cursor.advance_fresh(|hint| self.driver.fresh(hint));
        }
        let terminal = cursor.body().expect("a cursor past every entry");
        self.walk_term(&terminal)
    }

    /// [`Walk::walk_terms`] for a `TupleType`, which has no terminal to walk — a tuple type's payload is its fields.
    fn walk_units(&mut self, telescope: Telescope<()>) -> Result<(), D::Error> {
        let mut cursor = telescope.cursor();
        while let Some((_, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            cursor.advance_fresh(|hint| self.driver.fresh(hint));
        }
        Ok(())
    }

    /// Walk a match's arms, in order, each under what it establishes.
    ///
    /// Nothing is read ahead: an arm's guard, shape and binders are taken in the arm's own call, which is the ordering [`Walk::walk_term`] documents.
    ///
    /// **An arm of another case than the one the scrutinee already is holds no call.** No reduction takes it where it stands, so the term makes none of the calls it writes, and the arm is not walked, as the certifier types no such arm. The scrutinee is read as it stands ([`standing`]), so nothing is forced for it; a default arm states no case and is always walked.
    fn walk_arms(&mut self, head: &Term, cases: &Cases) -> Result<(), D::Error> {
        let scrutinee = refined_by_arm(head);
        let nominal = matches!(cases, Cases::Induct { .. });
        let stands = standing(&*self.driver, &self.context, head, nominal);
        let reached = |case: Case| stands.as_ref().is_none_or(|stands| *stands == case);

        match cases {
            // A boolean arm carries no binder, but when the scrutinee compares a binder against a literal the arm still settles whether that binder can be zero — which is what an arithmetic decrease on it needs. The scrutinee is re-read as a guard per arm, because what a guard establishes depends on which way it went.
            Cases::Bool {
                false_case,
                true_case,
            } => {
                for (taken, body) in [(false, false_case), (true, true_case)] {
                    if !reached(Case::Tag(Tag::Bool(taken))) {
                        continue;
                    }
                    let atom = self
                        .grader()
                        .guard(head)?
                        .filter(|guard| guard.establishes_nonzero(taken))
                        .map(|guard| guard.atom);
                    let shape = Shape::node(Tag::Bool(taken), []);
                    self.scoped(refine(scrutinee, shape), atom, Vec::new(), body)?;
                }
            }

            Cases::Switch { keys, default } => {
                for (literal, body) in keys.literals() {
                    let shape = self.grader().shape_of(&literal)?;
                    if shape.case().is_some_and(|case| !reached(case)) {
                        continue;
                    }
                    self.scoped(refine(scrutinee, shape), None, Vec::new(), body)?;
                }
                // The default arm stands for every value *not* enumerated, so it refines the scrutinee to nothing — but enumerating zero is exactly what rules zero out everywhere else.
                let atom = scrutinee.filter(|_| keys.names_zero());
                self.scoped(None, atom, Vec::new(), default)?;
            }

            // The arm's binders are the constructor's payloads, whether or not the scrutinee is a binder the arm can refine: what an application of one reads as is a fact about the payload, not about what it was projected from.
            Cases::Induct {
                cases,
                default,
                stated,
            } => {
                for (tag, case) in cases {
                    if !reached(Case::Tag(Tag::Variant(tag.clone()))) {
                        continue;
                    }
                    let (binders, body) = self.open_many(case.body());
                    let shape = Shape::node(
                        Tag::Variant(tag.clone()),
                        binders.iter().map(|b| Shape::Atom(*b)),
                    );
                    let refines = self.arm_refines(head, scrutinee, shape, &binders)?;
                    self.scoped(refines, None, binders, &body)?;
                }
                if let Some(default) = default {
                    self.walk_term(default)?;
                }
                // The instance a match states is no type position to pass over: where the match fires, its arm is opened at the payload this instance's indices state, so a call in one is a call the reduct holds.
                if let Some(stated) = stated {
                    self.walk_term(stated)?;
                }
            }

            // The cons arm binds the generator and the tail; the scrutinee is the generator consed onto the tail.
            Cases::FreeMonoid { carrier } => match carrier {
                Carrier::Nat {
                    empty_case,
                    cons_case,
                } => {
                    if reached(Case::Tag(Tag::Empty(Carriers::Unary))) {
                        self.empty_arm(&scrutinee, Carriers::Unary, empty_case)?;
                    }
                    if reached(Case::Run(Carriers::Unary)) {
                        let (binders, body) = self.open_one(cons_case);
                        let shape =
                            Shape::unary_run(Natural::from(1usize), Shape::Atom(binders[0]));
                        let refines = self.arm_refines(head, scrutinee, shape, &binders)?;
                        self.scoped(refines, None, Vec::new(), &body)?;
                    }
                }
                Carrier::Bytes {
                    empty_case,
                    cons_case,
                    ..
                } => {
                    if reached(Case::Tag(Tag::Empty(Carriers::Bytes))) {
                        self.empty_arm(&scrutinee, Carriers::Bytes, empty_case)?;
                    }
                    if reached(Case::Run(Carriers::Bytes)) {
                        self.elem_cons(head, scrutinee, Carriers::Bytes, cons_case)?;
                    }
                }
                Carrier::List {
                    elem,
                    empty_case,
                    cons_case,
                } => {
                    self.walk_term(elem)?;
                    if reached(Case::Tag(Tag::Empty(Carriers::List))) {
                        self.empty_arm(&scrutinee, Carriers::List, empty_case)?;
                    }
                    if reached(Case::Run(Carriers::List)) {
                        self.elem_cons(head, scrutinee, Carriers::List, cons_case)?;
                    }
                }
            },
        }
        Ok(())
    }

    /// The identity arm of a free-monoid eliminator, refining the scrutinee to that carrier's empty value — a shape stated without opening anything.
    fn empty_arm(
        &mut self,
        scrutinee: &Option<Free>,
        carriers: Carriers,
        empty_case: &Term,
    ) -> Result<(), D::Error> {
        let shape = Shape::node(Tag::Empty(carriers), []);
        self.scoped(refine(*scrutinee, shape), None, Vec::new(), empty_case)
    }

    /// The cons arm of a `Bytes`/`List` case split of `head`, binding the generator and the tail.
    fn elem_cons(
        &mut self,
        head: &Term,
        scrutinee: Option<Free>,
        carriers: Carriers,
        cons_case: &Scope<Two>,
    ) -> Result<(), D::Error> {
        let (binders, body) = self.open_two(cons_case);
        let shape = Shape::elem_run(
            carriers,
            vec![Shape::Atom(binders[0])],
            Shape::Atom(binders[1]),
        );
        let refines = self.arm_refines(head, scrutinee, shape, &binders)?;
        self.scoped(refines, None, Vec::new(), &body)
    }

    fn step(&mut self, term: &Term) -> Result<(), D::Error> {
        // A call is a member reference, so a term that holds no group holds no call, and what an arm inside it establishes is scoped to that arm: there is nothing in it for the walk to find or to carry out. Asked of the node's own cache, so a value a `let` puts at every use costs one question each. And a `let`'s value walked where it is bound is passed over in the tail — see `walked`.
        if (!term.has_group() && !self.names_tracked(term)) || self.walked.contains_key(term) {
            return Ok(());
        }

        match &**term {
            // A parameter named where nothing above took it as an application's head or handed it on is a use the walk does not follow.
            Subterm::Var(var) => {
                self.escaped(var.as_free());
                Ok(())
            }

            // Nothing here can contain a call.
            Subterm::Sort(_) | Subterm::Instance(_) | Subterm::Metavar(_) => Ok(()),

            // A member reference at the head of a spine is a call with those arguments; anywhere else it is a call of each of its parts the analysis cannot grade, which an all-unknown matrix records faithfully ([`part_calls`]) — save where it is handed to a definition that applies it as its summary states, which the application arm below grades through each application. The guard names *this* group alone: a projection of any other group is that group's only appearance in the term, so it falls through to the general `rec` arm and its bodies are walked like any nested group's.
            Subterm::Rec(_) if self.member(term).is_some() => {
                let index = self.member(term).expect("a member");
                for (part, given) in part_calls(self.parts, index, &[]) {
                    self.call(part, &given)?;
                }
                Ok(())
            }

            // A call is read off its path: a member of this group, its own arguments, a field of the record its leading lambdas answer, the field's arguments — the parts it reaches ([`part_calls`]) are the callees.
            Subterm::Apply(apply) => {
                let (spine_head, steps) = call_path(term);
                let projected = steps.iter().any(|step| matches!(step, Segment::Field(_)));
                let arguments = steps
                    .iter()
                    .flat_map(Segment::arguments)
                    .cloned()
                    .collect::<Vec<_>>();
                if let Some(member) = self.member(&spine_head) {
                    for (part, given) in part_calls(self.parts, member, &steps) {
                        self.call(part, &given)?;
                    }
                    // A parameter handed on at its own position, where the callee takes one, is the callee's parameter there, whose applications the summary reaches through this call, whatever the path projects after the callee's own arguments: a field part's parameters open with its member's, so the parameter stands at its own place in the part the path reaches too. One handed past the callee's parameters goes to what the callee answers, or is a field lambda's own argument, which applies it where no summary looks, so it is walked as any other use.
                    let arity = self.arities[base_part(self.parts, member)];
                    let handed_on = |(position, argument): &(usize, &Term)| {
                        self.tracked.is_some()
                            && *position < arity
                            && matches!(&***argument, Subterm::Var(var) if var.as_free() == self.params.get(*position))
                    };
                    let (handed, rest): (Vec<_>, Vec<_>) =
                        arguments.iter().enumerate().partition(handed_on);
                    self.record_handed_on(&handed);
                    let rest = rest
                        .into_iter()
                        .map(|(_, argument)| argument.clone())
                        .collect::<Vec<_>>();
                    return self.walks(&rest);
                }
                // A call of a definition, or of a field lambda of the record its leading lambdas answer, that applies a parameter, or a field of one, as its summary states hands each argument there what each application of it establishes ([`Walk::walk_argument`]).
                if let Subterm::Instance(instance) = &*spine_head {
                    let handed = handed(
                        self.driver,
                        &self.context,
                        self.params,
                        &instance.name,
                        &steps,
                        Answer::Escapes,
                    )?;
                    if handed.iter().any(Option::is_some) {
                        for (argument, applied) in arguments.iter().zip(handed) {
                            self.walk_argument(argument, applied.as_ref())?;
                        }
                        return Ok(());
                    }
                }
                if projected {
                    // A parameter's field applied, `e.0(x, y)`, is an application the summary states of that field.
                    if let Subterm::Var(var) = &*spine_head
                        && let Some(position) = self.tracked_position(var.as_free())
                        && let Some((path, applied)) = field_application(&steps)
                    {
                        self.applied(position, Some(path), &applied)?;
                        return self.walks(&applied);
                    }
                    self.walk_term(&apply.head)?;
                    return self.walks(apply.params());
                }
                if let Subterm::Var(var) = &*spine_head
                    && let Some(position) = self.tracked_position(var.as_free())
                {
                    self.applied(position, None, &arguments)?;
                    return self.walks(&arguments);
                }
                // A lambda at the head of a spine is graded as its contractum: the body is walked with each binder standing for the argument it was applied to, so a call inside reads its arguments as what they are rather than as fresh binders nothing is below. This is what keeps a convoy — an arm generalized over a hypothesis and applied back to it — from hiding the descent it carries. The arguments are walked on their own first, because a binder the body never uses would otherwise drop a call from the walk.
                if let Subterm::Func(Func { telescope, .. }) = &*spine_head {
                    self.walks(&arguments)?;
                    return self.walk_redex(telescope.clone(), &arguments);
                }
                self.walk_term(&apply.head)?;
                self.walks(apply.params())
            }

            Subterm::Match(Match { head, goal, cases }) => {
                self.walk_term(head)?;
                self.walk_term(goal)?;
                self.walk_arms(head, cases)
            }

            Subterm::Func(Func { telescope, .. })
            | Subterm::FuncType(FuncType { telescope, .. }) => self.walk_terms(telescope.clone()),

            Subterm::TupleType(TupleType { telescope }) => self.walk_units(telescope.clone()),

            Subterm::Tuple(Tuple { fields, .. }) => self.walks(fields),

            // A path of this group's member that ends in a projection names the parts it reaches without calling them, which are calls the analysis cannot grade past the arguments it was handed.
            Subterm::Proj(Proj { head, .. }) => {
                let (root, steps) = call_path(term);
                if let Some(member) = self.member(&root) {
                    for (part, given) in part_calls(self.parts, member, &steps) {
                        self.call(part, &given)?;
                    }
                    let arguments = steps
                        .iter()
                        .flat_map(Segment::arguments)
                        .cloned()
                        .collect::<Vec<_>>();
                    return self.walks(&arguments);
                }
                self.walk_term(head)
            }

            Subterm::InductType(InductType {
                params, indices, ..
            }) => self.walks(params.iter().chain(indices)),

            Subterm::Variant(Variant {
                params, payload, ..
            }) => self.walks(params.iter().chain(payload)),

            Subterm::StructType(StructType { params, .. }) => self.walks(params),

            Subterm::Struct(Struct { params, fields, .. }) => {
                self.walks(params.iter().chain(fields))
            }

            // The early-mention net walks *written* (lowered, pre-elaboration) terms, so transients are legitimate here; their children may name definitions.
            Subterm::Transient(transient) => self.walks(transient.subterms()),

            Subterm::Intrinsic(intrinsic) => {
                // A list operation applies its function operand to each element of its list, so a parameter standing there is applied, as a summary states it, to something below the list.
                let elementwise = match intrinsic {
                    Intrinsic::ListMap {
                        from,
                        to,
                        list,
                        function,
                    } => Some((list, function, 1, vec![from, to, list])),
                    Intrinsic::ListFold {
                        element,
                        result,
                        list,
                        init,
                        function,
                    } => Some((list, function, 2, vec![element, result, list, init])),
                    _ => None,
                };
                if let Some((list, function, arity, rest)) = elementwise
                    && let Subterm::Var(var) = &**function
                    && let Some(position) = self.tracked_position(var.as_free())
                {
                    self.element_applied(position, list, arity)?;
                    return self.walks(&rest.into_iter().cloned().collect::<Vec<_>>());
                }
                let mut terms = Vec::new();
                intrinsic.any_term(&mut |child| {
                    terms.push(child.clone());
                    false
                });
                self.walks(&terms)
            }

            Subterm::Foreign(Foreign {
                arguments: args, ..
            }) => self.walks(args),

            // A binding's type and value, and then the tail, are each walked with every binder before them standing for its value, for the reason an applied lambda is: a `let` is a redex, and a binder aliasing an arm's payload is below the scrutinee exactly as the payload is. Binding `i` is stored under the `i` binders before it, so it is released against the values already in hand before it is walked, as the certifier checks it against the binders before it: a call in a binding's value reads what an earlier binding stands for as a call in the tail does.
            Subterm::Let(block) => {
                let (bindings, tail) = (block.bindings(), block.tail());
                let mut values: Vec<Term> = Vec::with_capacity(bindings.len());
                // Only a value that holds a group, or names a parameter a summarizing walk keeps a record of, is worth remembering: the rest are passed over as they stand. A copy of one names the parameter again with the same arguments, applied or used otherwise where the `let` binds it. Counted, since two `let`s may bind one term and the inner one leaves first.
                let mut remembered: Vec<Term> = Vec::new();
                let mut walked = Ok(());
                for binding in bindings {
                    let refs = values.iter().collect::<Vec<_>>();
                    let type_ = binding.type_().release(&refs);
                    let value = binding.value().release(&refs);
                    walked = self.walks([&type_, &value]);
                    if walked.is_err() {
                        break;
                    }
                    if value.has_group() || self.names_tracked(&value) {
                        *self.walked.entry(value.clone()).or_default() += 1;
                        remembered.push(value.clone());
                    }
                    values.push(value);
                }
                if walked.is_ok() {
                    let refs = values.iter().collect::<Vec<_>>();
                    walked = self.walk_term(&tail.open(&refs));
                }
                for value in &remembered {
                    if let Entry::Occupied(mut standing) = self.walked.entry(value.clone()) {
                        *standing.get_mut() -= 1;
                        if *standing.get() == 0 {
                            standing.remove();
                        }
                    }
                }

                walked
            }

            // An inner group is classified on its own, but its bodies may still call *this* group, and such a call is a real edge of this group's call graph.
            //
            // `entered` is what keeps the descent finite. `RecGroup::member_body` materializes every member reference as a projection carrying the whole group, so a group reached from inside its own bodies would regenerate them without end — which is why a projection of *this* group is answered above rather than descended into, and why any other group is walked at most once per path. It is entered and left around the bodies, so a group met twice in sibling positions is still walked under each one's refinements.
            Subterm::Rec(block) => {
                let (group, tail) = (block.group(), block.tail());
                if !self.entered.contains(group) {
                    self.entered.push(group.clone());
                    // One body at a time: each materializes a projection carrying the whole group, so holding them together would hold every member at once.
                    for index in 0..group.length() {
                        self.walk_term(&group.member_body(index))?;
                    }
                    self.entered.pop();
                }
                self.open_many_walk(tail)
            }
        }
    }
}

/// What an arm refines, for a scrutinee that may not be a binder at all — a match on anything else establishes nothing to remember.
fn refine(scrutinee: Option<Free>, shape: Shape) -> Option<(Free, Shape)> {
    scrutinee.map(|scrutinee| (scrutinee, shape))
}

/// An application spine as its head and its arguments in order, so an over-applied or curried call is graded as the one call it is — whichever walk meets it.
pub fn spine(term: &Term) -> (Term, Vec<Term>) {
    let mut arguments = Vec::new();
    let mut head = term.clone();
    loop {
        match &*head.clone() {
            Subterm::Apply(apply) => {
                let mut prefix = apply.params().cloned().collect::<Vec<_>>();
                prefix.extend(arguments);
                arguments = prefix;
                head = apply.head.clone();
            }
            _ => return (head, arguments),
        }
    }
}

/// Whether a member of a recursive group is known to terminate at its turn: `known` holds the members known to terminate so far and the member itself, each its name, type and body over the group's names, and every call among them descends. A name of the group outside `known` is no member of the group this closes, so a call to it is none and nothing here unfolds it: it stands for a member withheld or not yet reached.
///
/// Both checkers take each member's turn by this, in the order the members are written, and read a member by value in its group's signatures and bodies once its turn finds it known (`curios-cert`'s `check_group`, `curios-elab`'s `Turns`). A member unfolds only where the members it reaches are known too or stand for nothing, so every unfolding either checker makes inside the group's judgment is of a definition that terminates, as every one past it is, an item whose recursion does not descend being defined with no value.
pub fn known_to_terminate<D: Driver>(
    driver: &mut D,
    known: Vec<(Free, Term, Term)>,
) -> Result<bool, D::Error> {
    Ok(group_totality(driver, &RecGroup::closing(known))?.is_total())
}

/// What a value of `type_` answers once applied to everything it takes: `type_` reduced, and past every function type it reduces to, each binder opened over a fresh identity, the codomain reduced again. `None` where a reduction has no value.
///
/// **Reduced at every step rather than read.** A declared type is a term like any other, so `U` — a definition whose value is `Type` — answers a sort only once forced, and so does `(n : Nat) -> U`: an alias is not a different claim.
///
/// Each binder is opened over a fresh identity before the walk descends past it, because a scope's body carries loose indices and reducing one would be reducing a term that is not there. Nothing is assumed at those binders: reduction meets an unknown name as a stuck neutral, which is all this needs. The reduction is a [`Probe`], so the budget's refusal is no answer at all and propagates.
pub fn codomain<D: Driver>(driver: &mut D, type_: &Term) -> Result<Option<Term>, D::Error> {
    // Peeling to a codomain answers the same question about a smaller type, so it iterates rather than re-entering: an arrow's spine is as long as its type is written.
    let mut type_ = type_.clone();

    loop {
        let Some(reduced) = driver.force(&type_).probed()? else {
            return Ok(None);
        };
        let Subterm::FuncType(FuncType { telescope, .. }) = &*reduced else {
            return Ok(Some(reduced));
        };
        let mut cursor = telescope.cursor();
        while !cursor.is_done() {
            cursor.advance_fresh(|hint| driver.fresh(hint));
        }
        type_ = cursor.body().expect("a cursor past every entry");
    }
}

/// Whether a sort is extractable from this type: its `codomain` is one, or a tuple a component of which yields one. A sort in a *parameter* is not extractable — `(A : Type) -> A` denotes a value, not a type.
///
/// Read through `codomain`, reduced at every step, so declaring a member through an alias is no escape from the descent gate, which is what stops a member erasure deletes from being assumed at its own type before its body is checked against it.
///
/// `undetermined` answers for a codomain it cannot settle: one with no value, as `None`, and one that reduces to no sort or tuple type, as what it reduces to. A type stuck on a member not known to terminate may be a sort once that member's value is, which no checker will ever read, so a group asking whether it owes descent answers yes where the codomain names one; and a signature asked before its group's bodies are elaborated may hold a level they settle, so no value there is no answer yet (`curios-cert`'s `check_group`, `curios-elab`'s `Turns`).
pub fn yields_a_sort_where<D: Driver>(
    driver: &mut D,
    type_: &Term,
    undetermined: &dyn Fn(Option<&Term>) -> bool,
) -> Result<bool, D::Error> {
    let Some(codomain) = codomain(driver, type_)? else {
        return Ok(undetermined(None));
    };

    match &*codomain {
        Subterm::Sort(_) => Ok(true),
        Subterm::TupleType(tuple) => {
            let mut cursor = tuple.telescope.cursor();
            while let Some((_, entry)) = cursor.entry() {
                if yields_a_sort_where(driver, &entry, undetermined)? {
                    return Ok(true);
                }
                cursor.advance_fresh(|hint| driver.fresh(hint));
            }
            Ok(false)
        }
        _ => Ok(undetermined(Some(&codomain))),
    }
}

/// What a weak-head normal form waits on: what it applies, eliminates or projects, followed down to a variable, a metavariable or an intrinsic operation, or nothing where it reaches a former, a value or anything else no solution or definition could move. A type stuck on a member not known to terminate waits on that member, which only the head and the scrutinee say: a name an argument or a spine holds is no reason it is stuck.
pub fn blocker(reduced: &Term) -> Option<&Term> {
    let mut term = reduced;
    loop {
        term = match &**term {
            Subterm::Apply(apply) => &apply.head,
            Subterm::Match(matched) => &matched.head,
            Subterm::Proj(proj) => &proj.head,
            Subterm::Var(_) | Subterm::Metavar(_) | Subterm::Intrinsic(_) => return Some(term),
            _ => return None,
        };
    }
}
