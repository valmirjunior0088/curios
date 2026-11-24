//! What a pass reads off a module, filed with the generation of the module it was read off and handed again at that generation alone: a fact lives as long as the module it was read off, and no pass states what it preserves, since every write advances the generation ([`Written::write`](super::Written::write)).

use {
    super::Module,
    crate::{
        CallAnalysis, ContinuationId, Demand, FunctionId, NodeId, Origin, Scopes, ValueFlow,
        ValueId, analyze_calls, continuation_entries, continuation_transfers, demands,
        escaping_in_bodies, origins,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        fmt,
        rc::Rc,
    },
};

/// The facts read off a module at one generation, each where a pass asked for it.
#[derive(Clone, Default)]
pub struct Readings(RefCell<Filed>);

impl fmt::Debug for Readings {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "Readings(at {})", self.0.borrow().generation)
    }
}

/// Every fact a pass asks of a module, each filed where it was first asked at `generation`.
#[derive(Clone, Default)]
struct Filed {
    generation: u64,
    calls: Option<Rc<CallAnalysis>>,
    scopes: Option<Rc<Scopes>>,
    demands: Option<Rc<BTreeMap<ValueId, Demand>>>,
    origins: Option<Rc<BTreeMap<ValueId, Origin>>>,
    escaping: Option<Rc<BTreeSet<FunctionId>>>,
    entries: Option<Rc<BTreeMap<ContinuationId, Vec<Option<FunctionId>>>>>,
    transfers: Option<Rc<BTreeMap<ContinuationId, Vec<NodeId>>>>,
    flow: Option<Rc<ValueFlow>>,
}

impl Module {
    /// The call analysis of the module as it stands ([`analyze_calls`]).
    pub(crate) fn calls(&self) -> Rc<CallAnalysis> {
        self.read(|filed| &mut filed.calls, analyze_calls)
    }

    /// The scopes of the module as it stands ([`Scopes::of`]).
    pub(crate) fn scopes(&self) -> Rc<Scopes> {
        self.read(
            |filed| &mut filed.scopes,
            |module| Scopes::of(module, &module.calls()),
        )
    }

    /// What each value's uses ask of it, over the module as it stands ([`demands`]).
    pub(crate) fn demands(&self) -> Rc<BTreeMap<ValueId, Demand>> {
        self.read(|filed| &mut filed.demands, demands)
    }

    /// What flows into each value, over the module as it stands ([`origins`]).
    pub(crate) fn origins(&self) -> Rc<BTreeMap<ValueId, Origin>> {
        self.read(|filed| &mut filed.origins, origins)
    }

    /// The functions some live body names as a value ([`escaping_in_bodies`]).
    pub(crate) fn escaping_in_bodies(&self) -> Rc<BTreeSet<FunctionId>> {
        self.read(|filed| &mut filed.escaping, escaping_in_bodies)
    }

    /// What transfers into each continuation ([`continuation_entries`]).
    pub(crate) fn continuation_entries(
        &self,
    ) -> Rc<BTreeMap<ContinuationId, Vec<Option<FunctionId>>>> {
        self.read(|filed| &mut filed.entries, continuation_entries)
    }

    /// The nodes transferring into each continuation ([`continuation_transfers`]).
    pub(crate) fn continuation_transfers(&self) -> Rc<BTreeMap<ContinuationId, Vec<NodeId>>> {
        self.read(|filed| &mut filed.transfers, continuation_transfers)
    }

    /// The value flow of the module as it stands ([`ValueFlow::of`]).
    pub(crate) fn value_flow(&self) -> Rc<ValueFlow> {
        self.read(|filed| &mut filed.flow, ValueFlow::of)
    }

    /// The fact `slot` holds where it was read at the module's generation, and otherwise the one `build` reads now, filed there. A build may ask for another fact, so the store is not held across one; this crate's tests, which drive every pass, hold a fact handed again to the one built fresh.
    fn read<T: PartialEq>(
        &self,
        slot: fn(&mut Filed) -> &mut Option<Rc<T>>,
        build: impl Fn(&Module) -> T,
    ) -> Rc<T> {
        let generation = self.graph.generation();
        let filed = {
            let mut filed = self.readings.0.borrow_mut();
            if filed.generation != generation {
                *filed = Filed {
                    generation,
                    ..Filed::default()
                };
            }
            slot(&mut filed).clone()
        };
        if let Some(fact) = filed {
            #[cfg(test)]
            assert!(
                *fact == build(self),
                "a fact handed again at a generation is the one built at it"
            );
            return fact;
        }
        let fact = Rc::new(build(self));
        *slot(&mut self.readings.0.borrow_mut()) = Some(Rc::clone(&fact));
        fact
    }
}
