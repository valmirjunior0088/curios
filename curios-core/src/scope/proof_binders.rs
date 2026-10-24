//! The walk that states, on every binder a function literal or a `let` binds, whether it binds a proof ([`Scope::binds_proof`]).
//!
//! **Why a binder states it.** The closed machine substitutes a value for a binder with nothing in hand but the value and a closed body, and whether that position holds a proof is a question about the binder's type under the binders before it. So the answer is decided where those binders can be stood for, once, when the term's declaration is published, and kept on the binder for whoever substitutes.
//!
//! **The term is walked closed.** Opening each scope to reach the next rewrites everything under it, once per binder, and re-closing rebuilds every node, where all this walk changes is a label. It keeps instead the variables standing for the binders entered and releases one binder's type against them, the type alone, to put the question; the body is never opened. A node none of whose binders changed is handed back as it was, so a term keeps its sharing and whatever its nodes had cached.
//!
//! **What is asked, what is entered, and what is crossed.** A function literal's binders and a `let`'s are asked, since those are the positions a reducer substitutes at. A function type's and a tuple type's are entered at their types and not asked: nothing substitutes there, but a literal further in may name them. Every other scope — a motive, an arm, a recursive group's members — is crossed with a variable standing for each binder and no type, so a question about a type that reads one of those binders is answered by what its head's declaration says or conservatively: a binder left unmarked costs a reduction and changes no reduct. An arm takes its marks where it is built, from its constructor's signature, and this walk leaves them standing.

#[cfg(test)]
mod tests;

use {
    crate::{
        Bound, Free, Func, FuncType, Let, LetBinding, One, Scope, Subterm, Telescope, Term,
        TupleType, Visit,
    },
    curios_utilities::recurse,
    std::{
        cell::RefCell,
        collections::{HashMap, VecDeque},
        ops::Range,
        rc::Rc,
    },
};

/// What the marking walk asks of the checker that runs it: the question itself, and a variable to stand for each binder it enters.
pub trait ProofBinders {
    /// Why a question went unanswered: a budget that ran out deciding it.
    type Refusal;

    /// Whether a value of `type_` is a proof: `type_`, a binder's type as it stands under the binders entered before it, is a proposition or a function that ultimately answers one.
    fn proves(&mut self, type_: &Term) -> Result<bool, Self::Refusal>;

    /// A binder is entered, and the variable answered stands for it until it is left. `type_` is its type as it stands under the binders entered before it, or `None` where the walk crosses a scope it does not read the types of.
    fn enter(&mut self, hint: Option<&str>, type_: Option<&Term>) -> Free;

    /// The `count` binders entered last are left.
    fn leave(&mut self, count: usize);
}

impl Term {
    /// This term with every binder a function literal or a `let` binds stating whether it binds a proof, as `asked` answers of the binder's type. The module documentation states what is asked and what a binder left unasked costs.
    pub fn stating_proofs<E: ProofBinders>(&self, asked: &mut E) -> Result<Term, E::Refusal> {
        self.stating_proofs_under(asked, &[])
    }

    /// [`Term::stating_proofs`] for a scope's body read where it stands, `binders[k]` standing for its loose index `k`: how a recursive group's member is walked under the names its members are published as, with no scope opened or closed for it.
    pub fn stating_proofs_under<E: ProofBinders>(
        &self,
        asked: &mut E,
        binders: &[Term],
    ) -> Result<Term, E::Refusal> {
        Marking {
            asked,
            entered: binders.iter().rev().cloned().collect(),
            blocks: Vec::new(),
            closed: HashMap::new(),
        }
        .term(self)
    }
}

struct Marking<'a, E> {
    asked: &'a mut E,
    /// The variables standing for the binders entered, outermost first: a loose index `k` names the `k`th from the end, a `let` block's run excepted ([`Marking::blocks`]).
    entered: Vec<Term>,
    /// The runs of `entered` each `let` block being walked entered its binders as, which a loose index reads in the block's own order, its first binder innermost. A block's binders are entered as each is minted and stay entered, so a binding is walked under the ones before it without entering them again.
    blocks: Vec<Range<usize>>,
    /// What a term with no loose index came back as, by node. Its binders' types name nothing outside it, so one answer serves every occurrence, and a type shared across a web of definitions is walked once.
    closed: HashMap<usize, Term>,
}

impl<E: ProofBinders> Marking<'_, E> {
    fn term(&mut self, term: &Term) -> Result<Term, E::Refusal> {
        if matches!(&**term, Subterm::Var(_) | Subterm::Sort(..)) {
            return Ok(term.clone());
        }
        let closed = term.reach() == 0;
        if closed && let Some(marked) = self.closed.get(&term.identity()) {
            return Ok(marked.clone());
        }

        let marked = recurse(|| match &**term {
            Subterm::Func(Func { telescope }) => {
                let marked = self.telescope(telescope, true, |marking, body| {
                    marking.term(body).map(|marked| {
                        let changed = marked.identity() != body.identity();
                        (marked, changed)
                    })
                })?;

                Ok(marked.map_or_else(
                    || term.clone(),
                    |telescope| term.respelled(Subterm::Func(Func { telescope })),
                ))
            }
            Subterm::FuncType(FuncType { telescope }) => {
                let marked = self.telescope(telescope, false, |marking, body| {
                    marking.term(body).map(|marked| {
                        let changed = marked.identity() != body.identity();
                        (marked, changed)
                    })
                })?;

                Ok(marked.map_or_else(
                    || term.clone(),
                    |telescope| term.respelled(Subterm::FuncType(FuncType { telescope })),
                ))
            }
            Subterm::TupleType(TupleType { telescope }) => {
                let marked = self.telescope(telescope, false, |_, ()| Ok(((), false)))?;

                Ok(marked.map_or_else(
                    || term.clone(),
                    |telescope| term.respelled(Subterm::TupleType(TupleType { telescope })),
                ))
            }
            Subterm::Let(let_) => self.let_(term, let_),
            _ => self.children(term),
        })?;

        if closed {
            self.closed.insert(term.identity(), marked.clone());
        }

        Ok(marked)
    }

    /// `value` as it stands under the binders entered, each loose index it reaches read off them, and no more of them read than it reaches.
    fn released<B: Bound>(&self, value: &B) -> B {
        value.release(&self.loose(value.reach()))
    }

    /// The variables standing for the first `count` loose indices, innermost first: from the end of `entered`, with a `let` block's run read in the block's order.
    fn loose(&self, count: usize) -> Vec<&Term> {
        let mut loose = Vec::with_capacity(count);
        let mut end = self.entered.len();
        for block in self.blocks.iter().rev() {
            let past = &self.entered[block.end..end];
            loose.extend(past.iter().rev().take(count - loose.len()));
            loose.extend(self.entered[block.clone()].iter().take(count - loose.len()));
            end = block.start;
        }
        loose.extend(self.entered[..end].iter().rev().take(count - loose.len()));
        loose
    }

    /// A telescope's entries entered in order, each type walked and, where `asked`, each binder stating whether it binds a proof; `payload` walks what the telescope ends in, under every binder, and says whether that changed. `None` where nothing did.
    fn telescope<B: Bound>(
        &mut self,
        telescope: &Telescope<B>,
        asked: bool,
        payload: impl FnOnce(&mut Self, &B) -> Result<(B, bool), E::Refusal>,
    ) -> Result<Option<Telescope<B>>, E::Refusal> {
        let before = self.entered.len();
        let mut entries = Vec::new();
        let mut changed = false;
        let mut current = telescope;

        let outcome = loop {
            match current {
                Telescope::Done(body) => break payload(self, body),
                Telescope::Cons(mark, type_, rest, ..) => {
                    let walked = match self.term(type_) {
                        Ok(walked) => walked,
                        Err(refusal) => break Err(refusal),
                    };
                    let opened = self.released(type_);
                    let proof = match asked {
                        true => match self.asked.proves(&opened) {
                            Ok(proof) => proof,
                            Err(refusal) => break Err(refusal),
                        },
                        false => rest.binds_proof(0),
                    };
                    changed |=
                        walked.identity() != type_.identity() || proof != rest.binds_proof(0);

                    let binder = self.asked.enter(rest.first_hint(), Some(&opened));
                    self.entered.push(Term::free_var(&binder));
                    entries.push((*mark, walked, rest, proof));
                    current = rest.body();
                }
            }
        };

        self.asked.leave(self.entered.len() - before);
        self.entered.truncate(before);
        let (body, body_changed) = outcome?;
        if !changed && !body_changed {
            return Ok(None);
        }

        Ok(Some(entries.into_iter().rev().fold(
            Telescope::Done(Box::new(body)),
            |marked, (mark, type_, rest, proof)| {
                let scope = Scope {
                    arity: One,
                    labels: rest.labels.clone(),
                    body: Box::new(marked),
                };

                Telescope::cons(mark, type_, scope.with_proofs(&[proof]))
            },
        )))
    }

    /// A `let`: each binding walked under the bindings before it, its binder asked at its type and entered, and the tail walked under all of them.
    fn let_(&mut self, term: &Term, Let { bindings, tail }: &Let) -> Result<Term, E::Refusal> {
        let before = self.entered.len();
        self.blocks.push(before..before);
        let mut walked = Vec::with_capacity(bindings.len());
        let mut proofs = Vec::with_capacity(bindings.len());
        let mut changed = false;

        let outcome = 'walk: {
            for (index, binding) in bindings.iter().enumerate() {
                let (type_, value) = match (self.term(binding.type_()), self.term(binding.value()))
                {
                    (Ok(type_), Ok(value)) => (type_, value),
                    (Err(refusal), _) | (_, Err(refusal)) => break 'walk Err(refusal),
                };
                let opened = self.released(binding.type_());
                let proof = match self.asked.proves(&opened) {
                    Ok(proof) => proof,
                    Err(refusal) => break 'walk Err(refusal),
                };
                changed |= type_.identity() != binding.type_().identity()
                    || value.identity() != binding.value().identity()
                    || proof != tail.binds_proof(index);

                let binder = self.asked.enter(tail.hint(index), Some(&opened));
                self.entered.push(Term::free_var(&binder));
                self.blocks
                    .last_mut()
                    .expect("the block this walk pushed")
                    .end += 1;
                walked.push(LetBinding::new(type_, value));
                proofs.push(proof);
            }

            self.term(tail.body())
        };

        self.asked.leave(self.entered.len() - before);
        self.entered.truncate(before);
        self.blocks.pop();
        let body = outcome?;
        if !changed && body.identity() == tail.body().identity() {
            return Ok(term.clone());
        }

        let tail = Scope {
            arity: tail.arity,
            labels: tail.labels.clone(),
            body: Box::new(body),
        }
        .with_proofs(&proofs);

        Ok(term.respelled(Subterm::Let(Let {
            bindings: walked,
            tail,
        })))
    }

    /// Any other node: each child term walked where it stands, under a variable of no type for every binder between the node and it, and the node rebuilt over what came back where a child changed.
    fn children(&mut self, term: &Term) -> Result<Term, E::Refusal> {
        let found = Rc::new(RefCell::new(Vec::new()));
        let sink = Rc::clone(&found);
        let mut collect = Visit::rewriting(
            |_, _| None,
            Box::new(move |depth, child| {
                sink.borrow_mut().push((depth, child.clone()));
                Some(child.clone())
            }),
        );
        let _: Subterm = (**term).traverse(&mut collect);
        let children = found.take();

        let before = self.entered.len();
        let mut walked = VecDeque::with_capacity(children.len());
        let mut changed = false;
        for (depth, child) in &children {
            for _ in 0..*depth {
                let binder = self.asked.enter(None, None);
                self.entered.push(Term::free_var(&binder));
            }
            let marked = self.term(child);
            self.asked.leave(*depth);
            self.entered.truncate(before);

            let marked = marked?;
            changed |= marked.identity() != child.identity();
            walked.push_back(marked);
        }
        if !changed {
            return Ok(term.clone());
        }

        let walked = Rc::new(RefCell::new(walked));
        let mut rebuild = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, _| walked.borrow_mut().pop_front()),
        );

        Ok(term.respelled((**term).traverse(&mut rebuild)))
    }
}
