use {
    super::{Name, Prim, print_term},
    curios_base::printer::run_printer,
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
        ops::Deref,
    },
};

/// An owned term of the erased IR: the boxed handle around one [`Subterm`] (it `Deref`s to it; build one with `Subterm::….into()`). Deliberately not `Clone` — a pass that wants a copy must route through `optimize`'s `deep_copy`, so duplicating a term is a conscious act rather than an accidental `.clone()`.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
#[cfg_attr(
    feature = "archive",
    rkyv(
        serialize_bounds(__S: rkyv::ser::Writer + rkyv::ser::Allocator + rkyv::ser::Sharing, __S::Error: rkyv::rancor::Source),
        deserialize_bounds(__D: rkyv::de::Pooling, __D::Error: rkyv::rancor::Source),
        bytecheck(bounds(__C: rkyv::validation::ArchiveContext + rkyv::validation::SharedContext, __C::Error: rkyv::rancor::Source))
    )
)]
pub struct Term {
    #[cfg_attr(feature = "archive", rkyv(omit_bounds))]
    inner: Box<Subterm>,
}

impl Term {
    /// Unbox into the owned [`Subterm`] — the by-value counterpart of the `Deref` view, for consumers that take a term apart to rebuild it (e.g. `optimize::worker_wrapper`'s body splitter).
    pub fn into_subterm(self) -> Subterm {
        *self.inner
    }

    /// Build a [`Let`] binding `bindings` over `tail`, merging into `tail` when
    /// it is itself a `Let` so an adjacent run of bindings collapses into one
    /// flat block rather than nesting. `bindings` are the outer (earlier) ones;
    /// `tail`'s own bindings stay after them, which preserves their sequential
    /// scoping (names are fresh, so the concatenation never captures). Empty
    /// `bindings` returns `tail` unchanged.
    pub fn let_(mut bindings: Vec<(String, Term)>, tail: Term) -> Term {
        if bindings.is_empty() {
            return tail;
        }

        match tail.into_subterm() {
            Subterm::Let(inner) => {
                bindings.extend(inner.bindings);
                Subterm::Let(Let {
                    bindings,
                    tail: inner.tail,
                })
                .into()
            }
            other => Subterm::Let(Let {
                bindings,
                tail: other.into(),
            })
            .into(),
        }
    }

    pub(crate) fn as_subterm(&self) -> &Subterm {
        &self.inner
    }

    pub(crate) fn as_subterm_mut(&mut self) -> &mut Subterm {
        &mut self.inner
    }

    /// Free names of this term, treating a nested `Func` as contributing its
    /// precomputed `captures` (the closure's free variables) rather than
    /// descending into its body. Used to build the `rec` dependency graph in
    /// `into_cont` and to compute a closure's captures during erasure — reading the
    /// *erased* body's free names is what keeps an erased-only reference from
    /// being threaded as a runtime capture.
    pub fn free_names(&self) -> BTreeSet<String> {
        let mut names = BTreeSet::new();

        match &**self {
            Subterm::Erased | Subterm::Unreachable | Subterm::Atom(_) => {}
            Subterm::Name(name) => {
                names.insert(name.as_str().to_owned());
            }
            Subterm::Func(func) => names.extend(func.captures.iter().map(|c| c.name.clone())),
            Subterm::Apply(apply) => {
                names.extend(apply.head.free_names());
                apply
                    .params
                    .iter()
                    .for_each(|param| names.extend(param.free_names()));
            }
            Subterm::Tuple(tuple) => tuple
                .fields
                .iter()
                .for_each(|field| names.extend(field.free_names())),
            Subterm::Proj(proj) => names.extend(proj.head.free_names()),
            Subterm::Match(m) => {
                names.extend(m.head.free_names());
                m.cases
                    .iter()
                    .for_each(|(_, case)| names.extend(case.free_names()));
                if let Some(default) = &m.default {
                    names.extend(default.free_names());
                }
            }
            Subterm::NatMatch(NatMatch::Induction {
                head,
                zero_case,
                pred,
                ih,
                succ_case,
            }) => {
                names.extend(head.free_names());
                names.extend(zero_case.free_names());

                let mut succ = succ_case.free_names();
                succ.remove(pred);
                succ.remove(ih);
                names.extend(succ);
            }
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                cases,
                default,
            }) => {
                names.extend(head.free_names());
                cases
                    .iter()
                    .for_each(|(_, case)| names.extend(case.free_names()));
                names.extend(default.free_names());
            }
            Subterm::Let(let_) => {
                // Sequential scoping: binding `i` is bound for every later
                // binding and the tail but not for its own body, so fold from
                // the tail inward, dropping each name before adding its body's
                // frees (which may reference the bindings before it).
                let mut free = let_.tail.free_names();
                for (name, body) in let_.bindings.iter().rev() {
                    free.remove(name);
                    free.extend(body.free_names());
                }
                names.extend(free);
            }
            Subterm::Rec(rec) => {
                let mut inner = BTreeSet::new();
                rec.items
                    .iter()
                    .for_each(|item| inner.extend(item.free_names()));
                inner.extend(rec.tail.free_names());
                rec.names.iter().for_each(|name| {
                    inner.remove(name);
                });
                names.extend(inner);
            }
            Subterm::Prim(prim) => names.extend(prim.free_names()),
        }

        names
    }

    /// Whether this term contains an effectful primitive anywhere — including
    /// under a lambda, since a closure that performs an effect performs it when
    /// *called*, and the caller's evaluation is what this classifies. The
    /// impurity boundary is exactly [`Prim::is_effectful`] (`Host`/`Cell`);
    /// everything in `PurePrim` is pure by construction.
    ///
    /// This is the intra-term half of the purity question shared between
    /// `optimize::prune` (which seeds its effect taint with it) and
    /// `optimize::worker_wrapper` (whose `MonoidAccumulator` gate rejects an
    /// absorbed context that is not pure). The transitive half — following an
    /// `Apply` to another module item — lives in `optimize::call_graph`.
    pub(crate) fn contains_effect(&self) -> bool {
        match self.as_subterm() {
            Subterm::Prim(prim) => {
                prim.is_effectful() || prim.operands().iter().any(|t| t.contains_effect())
            }
            Subterm::Func(func) => func.body.contains_effect(),
            Subterm::Apply(apply) => {
                apply.head.contains_effect() || apply.params.iter().any(Term::contains_effect)
            }
            Subterm::Tuple(tuple) => tuple.fields.iter().any(Term::contains_effect),
            Subterm::Proj(proj) => proj.head.contains_effect(),
            Subterm::Match(m) => {
                m.head.contains_effect()
                    || m.cases.iter().any(|(_, case)| case.contains_effect())
                    || m.default.iter().any(Term::contains_effect)
            }
            Subterm::NatMatch(NatMatch::Induction {
                head,
                zero_case,
                succ_case,
                ..
            }) => {
                head.contains_effect() || zero_case.contains_effect() || succ_case.contains_effect()
            }
            Subterm::NatMatch(NatMatch::Dispatch {
                head,
                cases,
                default,
            }) => {
                head.contains_effect()
                    || cases.iter().any(|(_, case)| case.contains_effect())
                    || default.contains_effect()
            }
            Subterm::Let(let_) => {
                let_.bindings.iter().any(|(_, body)| body.contains_effect())
                    || let_.tail.contains_effect()
            }
            Subterm::Rec(rec) => {
                rec.items.iter().any(Term::contains_effect) || rec.tail.contains_effect()
            }
            Subterm::Name(_) | Subterm::Atom(_) | Subterm::Erased | Subterm::Unreachable => false,
        }
    }

    /// Whether binding this term performs no action — it is a closure, name, or
    /// atom, allocated without evaluating an effect. `into_cont` lowers such terms
    /// into the flat top-level loop rather than the CPS path, and `optimize::prune`
    /// keeps a non-synchronous tainted item for its eager init effect.
    pub(crate) fn is_synchronous(&self) -> bool {
        matches!(
            self.as_subterm(),
            Subterm::Func(_)
                | Subterm::Erased
                | Subterm::Unreachable
                | Subterm::Atom(_)
                | Subterm::Name(_)
        )
    }
}

impl Deref for Term {
    type Target = Subterm;

    fn deref(&self) -> &Subterm {
        &self.inner
    }
}

impl From<Subterm> for Term {
    fn from(subterm: Subterm) -> Self {
        Self {
            inner: Box::new(subterm),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(print_term(self), formatter, 2)
    }
}

/// The two erased shapes of a `Nat` eliminator (and, through the length desugaring, of the `Lst`/`Bin` eliminators too). `Induction` is a genuine fold — `succ_case` binds the predecessor `pred` and the induction hypothesis `ih`, and lowers to an n-iteration loop. `Dispatch` is a literal-keyed switch with a `default` arm: the form `switch` matches erase to, and what the case-split erasure emits as a single peel when the successor arm ignores its `ih` (emitting a fold there would make a re-recursing caller O(2^n)).
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum NatMatch {
    Induction {
        head: Term,
        zero_case: Term,
        pred: String,
        ih: String,
        succ_case: Term,
    },
    Dispatch {
        head: Term,
        cases: BTreeMap<u32, Term>,
        default: Term,
    },
}

/// A captured or parameter binder, plus its specialization-*candidate* flag: true
/// when its (pre-erasure) type was a function, a `Type`, or unit. Computed during
/// type-directed erasure (the last point types are available) and carried to
/// `cont`, glued to the name so the two can never desync. Defaults to
/// non-candidate, so a binder built from a bare name (`"x".into()`) is not one.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Argument {
    pub name: String,
    pub candidate: bool,
}

impl Argument {
    pub(crate) fn as_str(&self) -> &str {
        &self.name
    }
}

impl<S: Into<String>> From<S> for Argument {
    fn from(name: S) -> Self {
        Self {
            name: name.into(),
            candidate: false,
        }
    }
}

/// A closure with its environment made explicit: `params` is the uncurried runtime parameter telescope (erasable binders already dropped) and `captures` is exactly the erased body's free names minus those params — precomputed by erasure and thereafter maintained by hand, since [`Term::free_names`] reads a `Func`'s capture list *instead of* descending into its body. A rewrite that changes which names the body frees must refresh the captures (`optimize`'s `refresh_captures`) or the closure threads the wrong environment.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Func {
    pub captures: Vec<Argument>,
    pub params: Vec<Argument>,
    pub body: Term,
}

/// Application of `head` to its full argument list — saturated against the callee's type, with arguments in erasable (proof/type) slots already dropped by erasure, so `params` lines up one-to-one with the callee [`Func`]'s `params`.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Apply {
    pub head: Term,
    pub params: Vec<Term>,
}

/// The one aggregate data shape of the erased IR: a flat record of runtime-relevant fields. An inductive value is its constructor's [`Atom`] tag at field 0 followed by the kept payload fields; a multi-field struct is the fields alone (tagless). A single-field struct never reaches here — it erases to its bare field.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Tuple {
    pub fields: Vec<Term>,
}

/// Positional field read `head.(index)`. On a variant tuple field 0 is the tag, so payload projections start at 1 — and since erasable payload fields are absent from the runtime tuple, a binder's slot is 1 plus the count of *relevant* fields before it, not its source position.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Proj {
    pub head: Term,
    pub index: usize,
}

/// A constructor tag: the constructor's index within its inductive. Erasure seats one at field 0 of every variant [`Tuple`], and [`Match`] dispatches on it.
#[derive(Debug, Clone, Copy)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Atom {
    pub index: usize,
}

/// Constructor dispatch: `head` evaluates to an [`Atom`] and selects `cases` by its index, falling through to `default` when no key matches. Sparse, mirroring [`NatMatch::Dispatch`]: only constructors with a distinct arm appear in `cases`, sorted iteration preserving their positional order. `default: None` is the invariant that the arms cover *every* constructor — a pruned (elaboration-proved-impossible) arm then still occupies its slot as `Unreachable`; `Some` supplies a `| _ =>` catch-all for every omitted constructor, so omission from `cases` is used only when a default exists. Payload access is not part of the node: erasure let-binds the scrutinee and each arm projects the fields it uses from that binding.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Match {
    pub head: Term,
    pub cases: BTreeMap<usize, Term>,
    pub default: Option<Term>,
}

/// A sequential block of bindings: evaluate each `body` in order — performing any effect it contains — binding its result in scope as `name` for the later bindings and the `tail`. A whole run of source `let`s is one node, not a nest, so every walk over it is a loop over `bindings` rather than one native stack frame per binding. Beyond user `let`s, erasure leans on it for sharing: a matched scrutinee is bound once, then dispatched on and projected from many times without re-evaluating.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Let {
    pub bindings: Vec<(String, Term)>,
    pub tail: Term,
}

/// A block of mutually-recursive bindings: `names` and `items` are parallel vectors, every name in scope in every item and in `tail`. The local twin of the top-level `Item::Rec`; `into_cont` computes the group's initialization order from the dependency graph over each item's [`Term::free_names`].
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub struct Rec {
    pub names: Vec<String>,
    pub items: Vec<Term>,
    pub tail: Term,
}

/// One node of the erased term language. Control is [`Match`]/[`NatMatch`]/[`Let`]/[`Rec`], data is [`Tuple`]/[`Atom`]/[`Proj`] plus the [`Prim`] alphabet, and [`Func`]/[`Apply`] carry closures with explicit captures. `Erased` is the residue a proof or type leaves behind in a relevant slot (never inspected at runtime), and `Unreachable` is a trap seated where elaboration proved an arm impossible. [`Term`] is the boxed handle; build one with `Subterm::….into()`.
#[derive(Debug)]
#[cfg_attr(
    feature = "archive",
    derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize)
)]
pub enum Subterm {
    Erased,
    Unreachable,
    Prim(Prim),
    NatMatch(NatMatch),
    Func(Func),
    Apply(Apply),
    Tuple(Tuple),
    Proj(Proj),
    Atom(Atom),
    Match(Match),
    Let(Let),
    Rec(Rec),
    Name(Name),
}
