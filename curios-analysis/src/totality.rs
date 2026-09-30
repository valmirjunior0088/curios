//! Size-change termination: which `rec` groups descend.
//!
//! Curios keeps general recursion. What it cannot keep is general recursion in the places erasure *removes*, because a divergent type breaks type formation and a divergent proof proves anything, while a program that loops is only a program that loops. This module supplies the fact both checkers need: whether a recursive group terminates on every call path.
//!
//! A group is accepted by the size-change principle (Lee–Jones–Ben-Amram, in the shape Abel's `foetus` gave it for dependent types). Each recursive call contributes a **call matrix** grading every callee argument against every caller parameter as strictly smaller, equal, or unrelated. The matrices are closed transitively under composition, and the group is accepted when every **idempotent** matrix in that closure carries a strict decrease on its diagonal — an idempotent matrix describes a call path that can repeat forever, so a decrease on it is a decrease that cannot be sustained.
//!
//! Size-change rather than structural recursion because the corpus needs it. `/big_nat/add/raw` — the corpus fixture that was `/std/BigNat` — descends on *either* of two `Bits` arguments depending on the arm, `add/raw_assoc` does the same over three, and `add/raw_comm` needs the mutual closure across two members. A rule keyed to one designated argument rejects all of them, and a fold cannot express them either, because a fold cannot short-circuit.
//!
//! **Match refinement is part of the size order, not an optimization.** A parameter scrutinized by an enclosing arm is expanded through that arm's constructor before it is compared, so in `add/raw`'s empty-`x` arm the literal argument `b[]` grades *equal* to `x` rather than unknown. Without that, the two nil-arm matrices compose to an all-unknown matrix, which is idempotent with no decrease anywhere on its diagonal, and `add/raw` is rejected on a call path that cannot actually occur.
//!
//! **An application of a constructor payload is a child of the constructor, and that is the whole of what lets a proof recurse along an accessibility predicate.** The size order reads a value as a finite tree, and a function-typed payload is a branching node whose children are its applications, so `below(y, r)` sits below `intro(x, below)` for the reason `below` itself does. The premise — that a payload's value *is* a finite tree — is what obligations (T) and (V) already guarantee at every position this verdict is consulted, since a partial value stored in a payload is refused by reachability before the verdict matters. Agda's structural order states the same rule for any function-typed constructor argument; Rocq's guard states a narrower one keyed to the payload's codomain, which refuses no cycle this admits, because a payload of a foreign type cannot reach the parameter's column.
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
pub use matrix::*;

mod shape;
use shape::*;

mod grade;
pub use grade::*;

#[cfg(test)]
mod tests;

use {
    crate::Env,
    curios_core::{
        Advance, Arity, Bound, Carrier, Cases, Free, Func, FuncType, InductType, Instance,
        Intrinsic, Let, Many, Match, MatchResult, Nat, Probe, Proj, Rec, RecGroup, Scope, Struct,
        StructType, Subterm, Telescope, Term, Three, Totality, Tuple, TupleType, Two, Variant,
    },
    curios_num::Natural,
    curios_utilities::recurse,
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
            calls: Vec::new(),
        };
        walk.walk(&member.body)?;
        calls.extend(walk.calls);
    }

    Ok(decide(calls))
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
    calls: Vec<Call>,
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

    /// The reader of terms as sizes, under the context this walk has built so far.
    fn grader(&mut self) -> Grader<'_, E> {
        Grader {
            env: self.env,
            context: &self.context,
        }
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

    /// Open the `(head, tail, ih)` arm of the `Bin`/`List` eliminators.
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
    /// **Arms are reached one at a time, and that is what makes this walk faithful.** Every arm's guard read, shape read, binder minting and body materialization happens in the arm's own call, never batched ahead of it — because those are effects on the checker driving the analysis: [`Env::force`] spends a reduction budget and [`Env::fresh`] mints an identity, so the order the arms are reached in *is* the order those effects land in. A differently-ordered spend against a nearly exhausted budget reads a different shape, which would be a verdict change with no cause in the term. The recursion gives that ordering; the frame machine this replaces had to argue for it.
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
        let scrutinee = match &**head {
            Subterm::Var(var) => var.as_free().cloned(),
            _ => None,
        };

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
                    let shape = Shape::Node(Tag::Bool(taken), Vec::new());
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
            Cases::Induct { cases, default } => {
                for (tag, case) in cases {
                    let (binders, body) = self.open_many(&case.body);
                    let shape = Shape::Node(
                        Tag::Variant(tag.clone()),
                        binders.iter().map(|b| Shape::Atom(*b)).collect(),
                    );
                    self.scoped(refine(scrutinee, shape), None, binders, &body)?;
                }
                if let Some(default) = default {
                    self.walk_term(default)?;
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
                Carrier::Bin {
                    empty_case,
                    cons_case,
                    ..
                } => {
                    self.empty_arm(&scrutinee, Carriers::Bin, empty_case)?;
                    self.elem_cons(scrutinee, Carriers::Bin, cons_case)?;
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
        let shape = Shape::Node(Tag::Empty(carriers), Vec::new());
        self.scoped(refine(*scrutinee, shape), None, Vec::new(), empty_case)
    }

    /// The cons arm of a `Bin`/`List` eliminator, binding the generator, the tail, and the induction hypothesis.
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
        match &**term {
            // Nothing here can contain a call.
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) | Subterm::Metavar(_) => Ok(()),

            // A member reference at the head of a spine is a call with those arguments; anywhere else it is a call the analysis cannot grade, which an all-unknown matrix records faithfully. The guard names *this* group alone: a projection of any other group is that group's only appearance in the term, so it falls through to the general `rec` arm and its bodies are walked like any nested group's.
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

            Subterm::Instance(Instance { head, .. }) => self.walk_term(&head.to_term()),

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

            Subterm::Foreign(_, args) => self.walks(args),

            // The tail is walked with each binder standing for its value, for the reason an applied lambda is: a `let` is a redex, and a binder aliasing an arm's payload is below the scrutinee exactly as the payload is. Binding `i` is stored under the `i` binders before it, so each value is released against the values already in hand.
            Subterm::Let(Let { bindings, tail }) => {
                let mut values: Vec<Term> = Vec::with_capacity(bindings.len());
                for binding in bindings {
                    self.walk_term(binding.type_())?;
                    self.walk_term(binding.value())?;
                    let refs = values.iter().collect::<Vec<_>>();
                    values.push(binding.value().release(&refs));
                }
                let refs = values.iter().collect::<Vec<_>>();
                self.walk_term(&tail.open(&refs))
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
            Subterm::Instance(Instance { head: inner, .. }) => head = inner.to_term(),
            _ => return (head, arguments),
        }
    }
}

/// Whether a sort is extractable from this type: reachable by peeling arrows to the codomain, or by projecting a tuple component. A sort in a *parameter* is not extractable — `(A : Type) -> A` denotes a value, not a type.
///
/// **Reduced at every step rather than read.** A declared type is a term like any other, so `U` — a definition whose value is `Type` — yields a sort and says so only once forced, and so does `(n : Nat) -> U`. Reading the spelling let a member escape the descent gate by being declared through an alias, and the gate is what stops a member erasure deletes from being assumed at its own type before its body is checked against it. An alias is not a different claim.
///
/// Each binder is opened over a fresh identity before the walk descends past it, because a scope's body carries loose indices and reducing one would be reducing a term that is not there. Nothing is assumed at those binders: reduction meets an unknown name as a stuck neutral, which is all this needs.
///
/// A reduction with no value answers **yes**. The gate then demands descent, which is the refusing direction, and a declared type that cannot be reduced is not one to take on trust. The reduction is a [`Probe`], so the budget's refusal is no answer at all and propagates.
pub fn yields_a_sort<E: Env>(env: &mut E, type_: &Term) -> Result<bool, E::Error> {
    // Peeling to a codomain answers the same question about a smaller type, so it iterates here rather than re-entering: an arrow's spine is as long as its type is written, and the answer is the codomain's own.
    let mut type_ = type_.clone();

    loop {
        let Some(reduced) = env.force(&type_).probed()? else {
            return Ok(true);
        };

        match &*reduced {
            Subterm::Type(_) | Subterm::Prop => return Ok(true),
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
                    if yields_a_sort(env, &entry)? {
                        return Ok(true);
                    }
                    cursor.advance_fresh(|hint| env.fresh(hint));
                }
                return Ok(false);
            }
            _ => return Ok(false),
        }
    }
}
