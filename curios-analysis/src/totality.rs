//! Size-change termination: which `rec` groups descend.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in the places erasure *removes*, because a divergent type breaks type formation and a divergent proof proves anything, while a program that loops is only a program that loops. This module supplies the fact both checkers need: whether a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in the shape Abel's `foetus` gave it for dependent types). Each recursive call contributes a **call matrix** grading every callee argument against every caller parameter as strictly smaller, equal, or unrelated. The matrices are closed transitively under composition, and the group is accepted when every **idempotent** matrix in that closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the standard library needs it. `/std/List/sort_by`'s `merge` descends on *either* of two lists depending on the arm, and `/std/Cli/check`'s `problems` and `beneath` close only across their two members, as `/std/Cli/parse`'s `command` and `chosen`, `/std/Cli`'s `variables` and `variables_of`, and `/std/Tui`'s `asked` and `all` do. A rule keyed to one designated argument rejects all of them, and a fold cannot express them either, because a fold cannot short-circuit. `problems` is reached by a type — `/std/Cli/WellFormed` is `Clear(check/problems(command, version))`, a proposition every `Cli` holds a proof of — so a structural rule would leave a declared command line's type unformed.
//!
//! **Match refinement is part of the size order, not an optimization.** A parameter scrutinized by an enclosing arm is expanded through that arm's constructor before it is compared, so in `merge`'s `[y, ..ys]` arm the tail `ys` grades *smaller* than `b`, and in an arm that refined `x` to the empty run a literal `x[]` argument grades *equal* to `x` rather than unknown. Without that, matrices of calls that cannot follow each other compose to one with no decrease anywhere on its diagonal, and the group is rejected on a call path that cannot actually occur; most of `/std`'s recursive groups would be.
//!
//! **An application of a constructor payload is a child of the constructor, and that is the whole of what lets a proof recurse along an accessibility predicate.** The size order reads a value as a finite tree, and a function-typed payload is a branching node whose children are its applications, so `below(y, r)` sits below `intro(x, below)` for the reason `below` itself does. The premise — that a payload's value *is* a finite tree — is what obligations (T) and (V) already guarantee at every position this verdict is consulted, since a partial value stored in a payload is refused by reachability before the verdict matters. Agda's structural order states the same rule for any function-typed constructor argument; Rocq's guard states a narrower one keyed to the payload's codomain, which refuses no cycle this admits: every payload is sized under its family, a proposition's as a type's, so none quantifies over the sort its family is in and none is applied at the family to hand a value of it back.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* engine, through [`Env`]: it is a total function of post-zonk terms, so a second implementation would be a second run of the same function on the same input rather than a second opinion. What differs is the obligation each driver hangs on the verdict. The elaborator's is positional and whole-module — obligations (T) and (V), seeded from what elaboration settled, turning a `Partial` classification into a rejection only where erasure deletes. The kernel's is local and self-derivable: a `rec` member whose declared type is a proof or yields a sort must descend, because assuming it at that type otherwise certifies `rec f : False = f`. Rejection by the *engine* is a classification, not an error — corecursive and productive definitions classify `Partial` and stay usable everywhere erasure keeps them.
//!
//! # Discovery, grading, and closure are three things
//!
//! Finding a group's calls, grading each against its caller's parameters, and closing the graded calls into a verdict are separate here. [`grade()`] reads one call's arguments under a [`SizeContext`] — what the arms enclosing it established — and [`decide`] closes a group's calls and demands a descent on every cycle. [`group_totality`] is one way to feed them: the discovery walk, which traverses member bodies for calls on its own. A walk that meets calls another way, while typing the bodies, grades and closes them with the same two functions, so the two differ only in how calls are found.

mod guard;
use guard::*;

mod matrix;
pub use matrix::Call;
use matrix::*;

mod shape;
use shape::*;

mod grade;
use grade::*;
pub use grade::{SizeContext, binder_bounds, grade, handed, nonzero_by, refined_by_arm};

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_common::recurse,
    curios_core::{
        Advance, Applications, Arity, Bound, CallMatrix, Carrier, Cases, Foreign, Free, Func,
        FuncType, InductType, Intrinsic, Let, Many, Match, MatchResult, Nat, Probe, Proj, Rec,
        RecGroup, Scope, Struct, StructType, Subterm, Telescope, Term, Three, Totality, Tuple,
        TupleType, Two, Variant,
    },
    curios_num::Natural,
    std::collections::{BTreeSet, HashMap, hash_map::Entry},
};

/// Whether every recursive call path in `group` descends, discovering the calls by walking its member bodies.
///
/// This is the whole of size-change termination as the elaborator applies it: collect one matrix per call site, close them under composition, and demand a decrease on the diagonal of every idempotent result — [`decide`]. A group with no recursive call at all closes to nothing and is accepted, which is how the prelude's call-free `; ih` folds pass — their recursion is the intrinsic eliminator's, already structural by construction.
pub fn group_totality<E: Env>(env: &mut E, group: &RecGroup) -> Result<Totality, E::Error> {
    curios_profile::profile!("group_totality");
    let mut members = Vec::new();
    for index in 0..group.length() {
        members.push(Member::of(env, group, index));
    }
    let arities = members.iter().map(|member| member.params.len()).collect();

    let mut calls = Vec::new();
    for (index, member) in members.iter().enumerate() {
        let mut walk = Walk {
            env,
            group,
            arities: &arities,
            caller: index,
            params: &member.params,
            context: SizeContext::default(),
            entered: Vec::new(),
            walked: HashMap::new(),
            calls: Vec::new(),
            tracked: None,
        };
        walk.walk(&member.body)?;
        calls.extend(walk.calls);
    }

    Ok(decide(calls))
}

/// How many matrices a parameter's applications may close to before the parameter is summarized as used otherwise: the conservative direction, as a closure past its bound is `Partial`.
const APPLICATIONS_LIMIT: usize = 64;

/// How each member of `group` applies its parameters ([`Applications`]): for a parameter at a position no member uses otherwise than as the head of an application or handed on unchanged at that position to a member, every application of it a member body makes, graded against that member's parameters, and every one reached through a path of the group's calls, graded through the path.
///
/// **Only where every use is seen.** A use the walk cannot follow — the parameter stored, returned, handed to another function, handed to a member at another position, or named in a type — leaves the position unsummarized in every member, and a call through it is graded as it is without a summary. A member call handing a position something other than the caller's own parameter there hands the callee another function, whose applications the summary then counts as well: more matrices claim less, never more.
pub fn group_applications<E: Env>(
    env: &mut E,
    group: &RecGroup,
) -> Result<Vec<Applications>, E::Error> {
    curios_profile::profile!("group_applications");
    let mut members = Vec::new();
    for index in 0..group.length() {
        members.push(Member::of(env, group, index));
    }
    let arities = members
        .iter()
        .map(|member| member.params.len())
        .collect::<Vec<_>>();

    let mut calls = Vec::new();
    let mut uses = Vec::new();
    for (index, member) in members.iter().enumerate() {
        let mut walk = Walk {
            env,
            group,
            arities: &arities,
            caller: index,
            params: &member.params,
            context: SizeContext::default(),
            entered: Vec::new(),
            walked: HashMap::new(),
            calls: Vec::new(),
            tracked: Some(Tracked::new(member.params.len())),
        };
        walk.walk(&member.body)?;
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

    Ok((0..members.len())
        .map(|member| {
            let applied = (0..arities[member])
                .map(|position| {
                    let closed = closed.as_ref().filter(|_| seen[position])?;
                    let mut matrices = uses[member].applied[position]
                        .iter()
                        .cloned()
                        .collect::<BTreeSet<_>>();
                    for path in closed.iter().filter(|path| path.caller == member) {
                        for applied in uses[path.callee]
                            .applied
                            .get(position)
                            .into_iter()
                            .flatten()
                        {
                            if let Some(through) = path.matrix.compose(applied) {
                                matrices.insert(through);
                            }
                        }
                    }
                    (matrices.len() <= APPLICATIONS_LIMIT).then(|| matrices.into_iter().collect())
                })
                .collect();
            Applications::new(applied)
        })
        .collect())
}

/// What a walk summarizing its member's parameters found of each ([`group_applications`]): the matrices of its applications, and whether it is used otherwise.
struct Tracked {
    applied: Vec<Vec<CallMatrix>>,
    escaped: Vec<bool>,
}

impl Tracked {
    fn new(params: usize) -> Self {
        Self {
            applied: vec![Vec::new(); params],
            escaped: vec![false; params],
        }
    }
}

/// Whether `calls`, every graded call of one group, close to no call path that can repeat forever without a strict decrease.
///
/// The closure and the verdict are shared by every walk that finds a group's calls, whichever walk met them.
pub fn decide(calls: Vec<Call>) -> Totality {
    curios_profile::profile!("decide");
    match close(calls) {
        Some(closed) => match closed.iter().all(|call| {
            call.caller != call.callee || !call.matrix.is_idempotent() || call.matrix.descends()
        }) {
            true => Totality::Total,
            false => Totality::Partial,
        },
        // The closure outgrew its bound; claim nothing.
        None => Totality::Partial,
    }
}

/// Each member's parameter count, as its leading lambdas bind them — the columns a call to it is graded over.
pub fn member_arities<E: Env>(env: &mut E, group: &RecGroup) -> Vec<usize> {
    (0..group.length())
        .map(|index| Member::of(env, group, index).params.len())
        .collect()
}

/// One member of a group, opened for analysis: the parameter binders the size order is measured against, and the body under them.
struct Member {
    params: Vec<Free>,
    body: Term,
}

impl Member {
    /// Peel the member's leading lambdas, minting one binder per parameter.
    ///
    /// A member with no lambda — `rec inf : F = F/more(inf)`, or any of `/std/Json/decode`'s nullary parsers — has an empty parameter vector, and its self-call therefore contributes a 0×0 matrix. That matrix is idempotent and has no diagonal to descend on, so a nullary self-call is rejected, which is exactly right: nothing about it can get smaller.
    fn of<E: Env>(env: &mut E, group: &RecGroup, index: usize) -> Self {
        let mut params = Vec::new();
        let mut body = group.member_body(index);

        while let Subterm::Func(Func { telescope, .. }) = &*body {
            let mut cursor = telescope.cursor();
            while !cursor.is_done() {
                params.push(cursor.advance_fresh(|hint| env.fresh(hint)));
            }
            let inner = cursor.body().expect("a cursor past every entry");
            body = inner;
        }

        Self { params, body }
    }
}

/// One member body's traversal: it finds the recursive calls, and has [`grade()`] grade each under the context its arms built.
struct Walk<'a, E: Env> {
    env: &'a mut E,
    group: &'a RecGroup,
    arities: &'a Vec<usize>,
    caller: usize,
    params: &'a [Free],
    /// What the arms enclosing the position being walked have established, entered on the way into an arm and left on the way out.
    context: SizeContext,
    /// The nested groups whose bodies the walk is currently inside, entered and left with them. A group reached from within itself would regenerate its own bodies without end, since every member reference materializes as a projection carrying the whole group.
    entered: Vec<RecGroup>,
    /// The `let` values the walk is past, in a later binding of their `let` or in its tail, each walked where its `let` binds it, and how many `let`s bind each. The walk reads a later binding and the tail with each binder standing for its value, so they hold a copy at every use; a copy is the value's calls again, with the same arguments, and what held where the `let` binds it still holds wherever one stands. So the matrix graded at the binding describes every copy, and a copy is passed over. Scoped to the `let`: past it the same term is another position, read under what holds there.
    walked: HashMap<Term, usize>,
    calls: Vec<Call>,
    /// The record of its member's parameters a walk for [`group_applications`] keeps, and `None` for a walk deciding descent alone, which grades nothing but the group's calls and so spends exactly what the decision does.
    tracked: Option<Tracked>,
}

impl<E: Env> Walk<'_, E> {
    /// Record a call to member `callee` with `arguments`, graded against the caller's parameters under what the enclosing arms established.
    fn call(&mut self, callee: usize, arguments: &[Term]) -> Result<(), E::Error> {
        let call = grade(
            self.env,
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
        self.tracked.as_ref()?;
        self.params.iter().position(|param| Some(param) == name)
    }

    /// Whether `term` names a parameter a summarizing walk keeps a record of.
    fn names_tracked(&self, term: &Term) -> bool {
        self.tracked.is_some() && self.params.iter().any(|param| term.mentions_free(param))
    }

    /// Record an application of the parameter at `position` to `arguments`, graded against the member's parameters under what the enclosing arms established.
    fn applied(&mut self, position: usize, arguments: &[Term]) -> Result<(), E::Error> {
        let graded = grade(
            self.env,
            &self.context,
            self.caller,
            self.params,
            self.caller,
            arguments.len(),
            arguments,
        )?;
        if let Some(tracked) = &mut self.tracked {
            tracked.applied[position].push(graded.matrix);
        }
        Ok(())
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
    ) -> Result<(), E::Error> {
        let mut cursor = telescope.cursor();
        let mut binders = Vec::new();
        while let Some((_, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            binders.push(cursor.advance_fresh(|hint| self.env.fresh(hint)));
        }
        let body = cursor.body().expect("a cursor past every entry");
        self.context
            .assume(binder_bounds(handed, self.params, &binders));
        let walked = self.walk_term(&body);
        self.context.exit();
        walked
    }

    /// Record the calls of member `callee` a definition makes where it applies it as `handed` grades: one for each application, and one claiming nothing where an application hands it other than all its parameters.
    fn call_handed(&mut self, callee: usize, handed: &[CallMatrix]) {
        let arity = self.arities[callee];
        for matrix in handed {
            let matrix = match matrix.columns() == arity {
                true => matrix.clone(),
                false => CallMatrix::unknown(self.params.len(), arity),
            };
            self.calls.push(Call {
                caller: self.caller,
                callee,
                matrix,
            });
        }
    }

    /// The reader of terms as sizes, under the context this walk has built so far.
    fn grader(&mut self) -> Grader<'_, E> {
        Grader::new(self.env, &self.context)
    }

    /// One fresh binder per position of a scope, carrying the written hints so a shape reads in the user's own names.
    fn binders<A: Arity>(&mut self, scope: &Scope<A>) -> Vec<Free> {
        (0..scope.arity())
            .map(|index| self.env.fresh(scope.hint(index)))
            .collect()
    }

    /// Open a runtime-arity scope — a motive, an inductive arm, a `let` tail, a `rec` tail — against fresh binders.
    ///
    /// The three openers here stay separate on purpose. `Scope<A>::open` takes `A::Params`, a *fixed-size* type per arity, which is what stops a two-binder arm from being opened at three terms; collapsing them would mean a slice-taking opener in `curios-core` that re-admits exactly that mistake, to save three lines apiece.
    fn open_many(&mut self, scope: &Scope<Many>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let refs = terms.iter().collect::<Vec<_>>();
        let body = scope.open(&refs);
        (binders, body)
    }

    /// Open the `(pred, ih)` arm of the `Nat` eliminator.
    fn open_two(&mut self, scope: &Scope<Two>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1]]);
        (binders, body)
    }

    /// Open the `(head, tail, ih)` arm of the `Bytes`/`List` eliminators.
    fn open_three(&mut self, scope: &Scope<Three>) -> (Vec<Free>, Term) {
        let binders = self.binders(scope);
        let terms = binders.iter().map(Term::free_var).collect::<Vec<_>>();
        let body = scope.open(&[&terms[0], &terms[1], &terms[2]]);
        (binders, body)
    }

    /// Find the recursive calls in `body`, tracking the refinements that make their arguments comparable.
    ///
    /// Scope discipline holds by construction: an arm's refinement is taken on by [`Walk::scoped`], which walks the body and puts it back, so no path can drop a refinement or carry one out.
    fn walk(&mut self, body: &Term) -> Result<(), E::Error> {
        self.walk_term(body)?;

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
    /// **Arms are reached one at a time, and that is what makes this walk faithful.** Every arm's guard read, shape read, binder minting and body materialization happens in the arm's own call, never batched ahead of it — because those are effects on the checker driving the analysis: [`Env::force`] spends a reduction budget and [`Env::fresh`] mints an identity, so the order the arms are reached in *is* the order those effects land in. A differently-ordered spend against a nearly exhausted budget reads a different shape, which would be a verdict change with no cause in the term. The recursion gives that ordering.
    fn walk_term(&mut self, term: &Term) -> Result<(), E::Error> {
        recurse(|| self.step(term))
    }

    /// Take on what an arm establishes, walk its body under it, and put it back.
    fn scoped(
        &mut self,
        refine: Option<(Free, Shape)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
        body: &Term,
    ) -> Result<(), E::Error> {
        self.context.open(refine, nonzero, payloads);
        self.walk_term(body)?;
        self.context.exit();
        Ok(())
    }

    /// Walk each of `terms`, in order.
    fn walks<'t>(&mut self, terms: impl IntoIterator<Item = &'t Term>) -> Result<(), E::Error> {
        for term in terms {
            self.walk_term(term)?;
        }
        Ok(())
    }

    /// Open a runtime-arity scope against fresh binders and walk its body.
    fn open_many_walk(&mut self, scope: &Scope<Many>) -> Result<(), E::Error> {
        let (_, body) = self.open_many(scope);
        self.walk_term(&body)
    }

    /// Walk a lambda's telescope applied to `arguments`: each entry type, then the body with every binder standing for its argument. A binder past the last argument is minted fresh as `walk_terms` would; an argument past the last binder stays applied to the body, which may be a lambda in its turn.
    fn walk_redex(
        &mut self,
        telescope: Telescope<Term>,
        arguments: &[Term],
    ) -> Result<(), E::Error> {
        let mut remaining = arguments.iter();
        let mut cursor = telescope.cursor();
        while let Some((hint, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            let argument = match remaining.next() {
                Some(argument) => argument.clone(),
                None => Term::free_var(&self.env.fresh(hint)),
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
    fn walk_terms(&mut self, telescope: Telescope<Term>) -> Result<(), E::Error> {
        let mut cursor = telescope.cursor();
        while let Some((_, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            cursor.advance_fresh(|hint| self.env.fresh(hint));
        }
        let terminal = cursor.body().expect("a cursor past every entry");
        self.walk_term(&terminal)
    }

    /// [`Walk::walk_terms`] for a `TupleType`, which has no terminal to walk — a tuple type's payload is its fields.
    fn walk_units(&mut self, telescope: Telescope<()>) -> Result<(), E::Error> {
        let mut cursor = telescope.cursor();
        while let Some((_, entry)) = cursor.entry() {
            self.walk_term(&entry)?;
            cursor.advance_fresh(|hint| self.env.fresh(hint));
        }
        Ok(())
    }

    /// Walk a match's arms, in order, each under what it establishes.
    ///
    /// Nothing is read ahead: an arm's guard, shape and binders are taken in the arm's own call, which is the ordering [`Walk::walk_term`] documents.
    fn walk_arms(&mut self, head: &Term, cases: &Cases) -> Result<(), E::Error> {
        let scrutinee = refined_by_arm(head);

        match cases {
            // A boolean arm carries no binder, but when the scrutinee compares a binder against a literal the arm still settles whether that binder can be zero — which is what an arithmetic decrease on it needs. The scrutinee is re-read as a guard per arm, because what a guard establishes depends on which way it went.
            Cases::Bool {
                false_case,
                true_case,
            } => {
                for (taken, body) in [(false, false_case), (true, true_case)] {
                    let atom = self
                        .grader()
                        .guard(head)?
                        .filter(|guard| guard.establishes_nonzero(taken))
                        .map(|guard| guard.atom);
                    let shape = Shape::node(Tag::Bool(taken), []);
                    self.scoped(refine(scrutinee, shape), atom, Vec::new(), body)?;
                }
            }

            Cases::Switch { cases, default } => {
                for (value, body) in cases {
                    let literal = Term::intrinsic(Intrinsic::Nat(Nat::new(value.clone())));
                    let shape = self.grader().shape_of(&literal)?;
                    self.scoped(refine(scrutinee, shape), None, Vec::new(), body)?;
                }
                // The default arm stands for every value *not* enumerated, so it refines the scrutinee to nothing — but enumerating zero is exactly what rules zero out everywhere else.
                let atom = scrutinee.filter(|_| cases.iter().any(|(key, _)| key.is_zero()));
                self.scoped(None, atom, Vec::new(), default)?;
            }

            // The arm's binders are the constructor's payloads, whether or not the scrutinee is a binder the arm can refine: what an application of one reads as is a fact about the payload, not about what it was projected from.
            Cases::Induct {
                cases,
                default,
                stated,
            } => {
                for (tag, case) in cases {
                    let (binders, body) = self.open_many(case.body());
                    let shape = Shape::node(
                        Tag::Variant(tag.clone()),
                        binders.iter().map(|b| Shape::Atom(*b)),
                    );
                    self.scoped(refine(scrutinee, shape), None, binders, &body)?;
                }
                if let Some(default) = default {
                    self.walk_term(default)?;
                }
                // The instance a match states is no type position to pass over: where the match fires, its arm is opened at the payload this instance's indices state, so a call in one is a call the reduct holds.
                if let Some(stated) = stated {
                    self.walk_term(stated)?;
                }
            }

            // The cons arm binds the generator, the tail, and the induction hypothesis; the scrutinee is the generator consed onto the tail, and the hypothesis is not part of the value's shape.
            Cases::FreeMonoid { carrier } => match carrier {
                Carrier::Nat {
                    empty_case,
                    cons_case,
                } => {
                    self.empty_arm(&scrutinee, Carriers::Unary, empty_case)?;
                    let (binders, body) = self.open_two(cons_case);
                    let shape = Shape::unary_run(Natural::from(1usize), Shape::Atom(binders[0]));
                    self.scoped(refine(scrutinee, shape), None, Vec::new(), &body)?;
                }
                Carrier::Bytes {
                    empty_case,
                    cons_case,
                    ..
                } => {
                    self.empty_arm(&scrutinee, Carriers::Bytes, empty_case)?;
                    self.elem_cons(scrutinee, Carriers::Bytes, cons_case)?;
                }
                Carrier::List {
                    elem,
                    empty_case,
                    cons_case,
                } => {
                    self.walk_term(elem)?;
                    self.empty_arm(&scrutinee, Carriers::List, empty_case)?;
                    self.elem_cons(scrutinee, Carriers::List, cons_case)?;
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
    ) -> Result<(), E::Error> {
        let shape = Shape::node(Tag::Empty(carriers), []);
        self.scoped(refine(*scrutinee, shape), None, Vec::new(), empty_case)
    }

    /// The cons arm of a `Bytes`/`List` eliminator, binding the generator, the tail, and the induction hypothesis.
    fn elem_cons(
        &mut self,
        scrutinee: Option<Free>,
        carriers: Carriers,
        cons_case: &Scope<Three>,
    ) -> Result<(), E::Error> {
        let (binders, body) = self.open_three(cons_case);
        let shape = Shape::elem_run(
            carriers,
            vec![Shape::Atom(binders[0])],
            Shape::Atom(binders[1]),
        );
        self.scoped(refine(scrutinee, shape), None, Vec::new(), &body)
    }

    fn step(&mut self, term: &Term) -> Result<(), E::Error> {
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

            // A member reference at the head of a spine is a call with those arguments; anywhere else it is a call the analysis cannot grade, which an all-unknown matrix records faithfully — save where it is handed to a definition that applies it as its summary states, which the application arm below grades through each application. The guard names *this* group alone: a projection of any other group is that group's only appearance in the term, so it falls through to the general `rec` arm and its bodies are walked like any nested group's.
            Subterm::Rec(_)
                if term
                    .as_rec_proj()
                    .is_some_and(|(group, _)| group == self.group) =>
            {
                let (_, index) = term.as_rec_proj().expect("a projection");
                self.call(index, &[])
            }

            Subterm::Apply(apply) => {
                let (spine_head, arguments) = spine(term);
                if let Some((group, index)) = spine_head.as_rec_proj()
                    && group == self.group
                {
                    self.call(index, &arguments)?;
                    // A parameter handed on at its own position is the callee's parameter there, whose applications the summary reaches through this call.
                    let handed_on = |(position, argument): &(usize, &Term)| {
                        self.tracked.is_some()
                            && matches!(&***argument, Subterm::Var(var) if var.as_free() == self.params.get(*position))
                    };
                    let rest = arguments
                        .iter()
                        .enumerate()
                        .filter(|entry| !handed_on(entry))
                        .map(|(_, argument)| argument.clone())
                        .collect::<Vec<_>>();
                    return self.walks(&rest);
                }
                if let Subterm::Var(var) = &*spine_head
                    && let Some(position) = self.tracked_position(var.as_free())
                {
                    self.applied(position, &arguments)?;
                    return self.walks(&arguments);
                }
                // A call of a definition that applies a parameter as its summary states hands a lambda there binders standing to this member's parameters as each application of it stands, and hands a member of this group there a call graded through each.
                if let Subterm::Instance(instance) = &*spine_head {
                    let handed = handed(
                        self.env,
                        &self.context,
                        self.params,
                        &instance.name,
                        &arguments,
                    )?;
                    if handed.iter().any(Option::is_some) {
                        for (argument, handed) in arguments.iter().zip(handed) {
                            let member = argument
                                .as_rec_proj()
                                .filter(|(group, _)| *group == self.group)
                                .map(|(_, index)| index);
                            match (handed, &**argument, member) {
                                (Some(handed), Subterm::Func(Func { telescope, .. }), _) => {
                                    self.walk_handed(telescope.clone(), &handed)?;
                                }
                                (Some(handed), _, Some(member)) => {
                                    self.call_handed(member, &handed)
                                }
                                _ => self.walk_term(argument)?,
                            }
                        }
                        return Ok(());
                    }
                }
                // A lambda at the head of a spine is graded as its contractum: the body is walked with each binder standing for the argument it was applied to, so a call inside reads its arguments as what they are rather than as fresh binders nothing is below. This is what keeps a convoy — an arm generalized over a hypothesis and applied back to it — from hiding the descent it carries. The arguments are walked on their own first, because a binder the body never uses would otherwise drop a call from the walk.
                if let Subterm::Func(Func { telescope, .. }) = &*spine_head {
                    self.walks(&arguments)?;
                    return self.walk_redex(telescope.clone(), &arguments);
                }
                self.walk_term(&apply.head)?;
                self.walks(apply.params())
            }

            Subterm::Match(Match {
                head,
                result,
                cases,
            }) => {
                self.walk_term(head)?;
                match result {
                    MatchResult::Family(motive) => self.open_many_walk(motive)?,
                    MatchResult::Ambient(goal) => self.walk_term(goal)?,
                }
                self.walk_arms(head, cases)
            }

            Subterm::Func(Func { telescope, .. })
            | Subterm::FuncType(FuncType { telescope, .. }) => self.walk_terms(telescope.clone()),

            Subterm::TupleType(TupleType { telescope }) => self.walk_units(telescope.clone()),

            Subterm::Tuple(Tuple { fields, .. }) => self.walks(fields),

            Subterm::Proj(Proj { head, .. }) => self.walk_term(head),

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

            // A binding's type and value, and then the tail, are each walked with every binder before them standing for its value, for the reason an applied lambda is: a `let` is a redex, and a binder aliasing an arm's payload is below the scrutinee exactly as the payload is. Binding `i` is stored under the `i` binders before it, so it is released against the values already in hand before it is walked, as the kernel checks it against the binders before it: a call in a binding's value reads what an earlier binding stands for as a call in the tail does.
            Subterm::Let(Let { bindings, tail }) => {
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
            Subterm::Rec(Rec { group, tail }) => {
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
/// Both checkers take each member's turn by this, in the order the members are written, and read a member by value in its group's signatures and bodies once its turn finds it known (`curios-cert`'s `check_group`, `curios-elab`'s `Turns`). A member unfolds only where the members it reaches are known too or stand for nothing, so every unfolding either checker makes is of a definition that terminates.
pub fn known_to_terminate<E: Env>(
    env: &mut E,
    known: Vec<(Free, Term, Term)>,
) -> Result<bool, E::Error> {
    Ok(group_totality(env, &RecGroup::closing(known))?.is_total())
}

/// Whether a sort is extractable from this type: reachable by peeling arrows to the codomain, or by projecting a tuple component. A sort in a *parameter* is not extractable — `(A : Type) -> A` denotes a value, not a type.
///
/// **Reduced at every step rather than read.** A declared type is a term like any other, so `U` — a definition whose value is `Type` — yields a sort and says so only once forced, and so does `(n : Nat) -> U`. Reading the spelling let a member escape the descent gate by being declared through an alias, and the gate is what stops a member erasure deletes from being assumed at its own type before its body is checked against it. An alias is not a different claim.
///
/// Each binder is opened over a fresh identity before the walk descends past it, because a scope's body carries loose indices and reducing one would be reducing a term that is not there. Nothing is assumed at those binders: reduction meets an unknown name as a stuck neutral, which is all this needs.
///
/// `undetermined` answers for a codomain it cannot settle: one with no value, as `None`, and one that reduces to no sort, arrow or tuple type, as what it reduces to. A type stuck on a member not known to terminate may be a sort once that member's value is, which no checker will ever read, so a group asking whether it owes descent answers yes where the codomain names one; and a signature asked before its group's bodies are elaborated may hold a level they settle, so no value there is no answer yet (`curios-cert`'s `check_group`, `curios-elab`'s `Turns`). The reduction is a [`Probe`], so the budget's refusal is no answer at all and propagates.
pub fn yields_a_sort_where<E: Env>(
    env: &mut E,
    type_: &Term,
    undetermined: &dyn Fn(Option<&Term>) -> bool,
) -> Result<bool, E::Error> {
    // Peeling to a codomain answers the same question about a smaller type, so it iterates here rather than re-entering: an arrow's spine is as long as its type is written, and the answer is the codomain's own.
    let mut type_ = type_.clone();

    loop {
        let Some(reduced) = env.force(&type_).probed()? else {
            return Ok(undetermined(None));
        };

        match &*reduced {
            Subterm::Sort(_) => return Ok(true),
            Subterm::FuncType(FuncType { telescope, .. }) => {
                let mut cursor = telescope.cursor();
                while !cursor.is_done() {
                    cursor.advance_fresh(|hint| env.fresh(hint));
                }
                type_ = cursor.body().expect("a cursor past every entry");
            }
            Subterm::TupleType(tuple) => {
                let mut cursor = tuple.telescope.cursor();
                while let Some((_, entry)) = cursor.entry() {
                    if yields_a_sort_where(env, &entry, undetermined)? {
                        return Ok(true);
                    }
                    cursor.advance_fresh(|hint| env.fresh(hint));
                }
                return Ok(false);
            }
            _ => return Ok(undetermined(Some(&reduced))),
        }
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
