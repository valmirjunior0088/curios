//! What the machine's suite and the bench's measurement of it share: the minimal host the machine runs a closed term under, and the terms they put to it.

use {crate::*, curios_num::Binary, std::collections::BTreeMap};

/// The minimal host: a definition store and a budget. Its own reduction *is* the machine, which is exactly the position the real hosts put it in for closed terms — anything the intrinsic folds hand back through the [`Reducer`] seam lands here and re-enters the machine.
pub struct MachineHost {
    pub definitions: BTreeMap<Free, Term>,
    pub budget: u64,
    pub by_category: BTreeMap<Category, u64>,
    pub minted: u64,
    /// What this host reads for a payload off a scrutinee's type: nothing, unless a test states one.
    pub stated: Option<Vec<Term>>,
    /// Which payloads of any constructor this host's entries state are proofs: none, unless a test states some.
    pub proofs: Vec<bool>,
    /// The bodies of the declarations an instance names, as stated, before its levels are read into them: none, unless a test states some.
    pub declared: BTreeMap<Global, Term>,
    pub memo: ClosedMemo,
}

impl MachineHost {
    pub fn new(budget: u64) -> Self {
        Self {
            definitions: BTreeMap::new(),
            budget,
            by_category: BTreeMap::new(),
            minted: 0,
            stated: None,
            proofs: Vec::new(),
            declared: BTreeMap::new(),
            memo: ClosedMemo::default(),
        }
    }
}

impl Reducer for MachineHost {
    fn reduce(&mut self, term: Term) -> Result<Term, ReduceError> {
        reduce_closed(self, term, Demand::Whnf)
    }

    fn reduce_forced(&mut self, term: Term) -> Result<Term, ReduceError> {
        reduce_closed(self, term, Demand::Forced)
    }

    fn spend(&mut self, cost: Cost) -> Result<(), ReduceError> {
        if cost.is_refused() || cost.get() > self.budget {
            return Err(ReduceError::exhausted(self.budget, cost));
        }

        *self.by_category.entry(cost.category()).or_default() += cost.get();
        self.budget -= cost.get();
        Ok(())
    }

    fn fresh_binder(&mut self, _hint: Option<&str>) -> Free {
        self.minted += 1;
        Free::local(1_000_000 + self.minted, Some("fresh"))
    }
}

impl ClosedHost for MachineHost {
    fn closed_body(&self, name: &Free) -> Option<&Term> {
        self.definitions.get(name)
    }

    fn closed_body_at(&self, name: &Global, _: &Universes) -> Option<&Term> {
        self.declared.get(name)
    }

    fn stated_payload(
        &mut self,
        _head: &Term,
        _tag: &crate::Atom,
    ) -> Result<Option<Vec<Term>>, ReduceError> {
        Ok(self.stated.clone())
    }

    fn payload_proofs(&self, _family: &Global, _tag: &crate::Atom) -> Vec<bool> {
        self.proofs.clone()
    }

    fn closed_memo(&mut self) -> &mut ClosedMemo {
        &mut self.memo
    }
}

pub fn binder(index: u32, hint: &str) -> Free {
    Free::local(u64::from(index), Some(hint))
}

pub fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(crate::Nat::new(n)))
}

pub fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub fn bytes_type() -> Term {
    Term::intrinsic(Intrinsic::BytesType)
}

pub fn bytes(data: Vec<u8>) -> Term {
    Term::intrinsic(Intrinsic::Bytes(Binary::from_bytes(data)))
}

/// `count(0x[]) = empty`, `count(0x[h, ..t]) = cons(count(t))` over `data`.
pub fn non_tail(data: Vec<u8>, empty: Term, cons: impl Fn(Term) -> Term) -> Term {
    let (count, b) = (binder(0, "count"), binder(1, "b"));
    let (h, t) = (binder(3, "h"), binder(4, "t"));

    let body = Term::func(
        [(b, bytes_type())],
        Term::bytes_match(
            Term::free_var(&b),
            nat_type(),
            empty,
            &[&h, &t],
            cons(Term::apply(Term::free_var(&count), [Term::free_var(&t)])),
        ),
    );

    Term::rec(
        [(
            count,
            Term::func_type([(b, bytes_type())], nat_type()),
            body,
        )],
        Term::apply(Term::free_var(&count), [bytes(data)]),
    )
}

/// `go(acc, 0x[]) = acc`, `go(acc, 0x[h, ..t]) = go(combine(acc, h), t)` over `data`, applied to an accumulator of zero.
pub fn tail_fold(data: Vec<u8>, combine: impl Fn(Term, Term) -> Term) -> Term {
    let (go, acc, b) = (binder(0, "go"), binder(1, "acc"), binder(2, "b"));
    let (h, t) = (binder(3, "h"), binder(4, "t"));

    let body = Term::func(
        [(acc, nat_type()), (b, bytes_type())],
        Term::bytes_match(
            Term::free_var(&b),
            nat_type(),
            Term::free_var(&acc),
            &[&h, &t],
            Term::apply(
                Term::free_var(&go),
                [
                    combine(Term::free_var(&acc), Term::free_var(&h)),
                    Term::free_var(&t),
                ],
            ),
        ),
    );

    Term::rec(
        [(
            go,
            Term::func_type([(acc, nat_type()), (b, bytes_type())], nat_type()),
            body,
        )],
        Term::apply(Term::free_var(&go), [nat(0), bytes(data)]),
    )
}
