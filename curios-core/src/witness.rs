//! What a witness is known by: the heads it is keyed on, the key its signature is spelled at, and the name it is declared under.
//!
//! **A witness is named by what it witnesses, never by where it sits.** A `satisfy` declaration is anonymous, so its name is an identity the compiler gives it; the one no edit elsewhere can move is the module declaring it, the concept its signature applies, and the key that signature is spelled at ([`WitnessId`]). Its place among its module's witnesses is no part of it: a witness moved past another is the same declaration, and so is every declaration that resolves through it.
//!
//! **The key is read off how the declaration is written** ([`Keying`]), before anything elaborates, so the lowering can name a witness and a question about a concept is answered the same whichever witnesses have elaborated. Registration reads the key again off the elaborated signature ([`HeadKey::of_whnf`]) and refuses a witness the two disagree on, so the reading decides only what it is certain of, and everything else is no key at all.
//!
//! **A key is a copyable identity** ([`WitnessKey`]), interned once per process as a path is, so a name stays `Copy`: every free variable of every term holds one.

#[cfg(test)]
mod keying_tests;

#[cfg(test)]
mod tests;

use {
    crate::{Free, Func, Global, Intrinsic, Spelling, Subterm, Telescope, Term, TupleType},
    curios_num::Grain,
    curios_utilities::{Plicity, Qualifier, Table, intern},
    std::{
        cmp::Ordering,
        collections::HashMap,
        fmt,
        hash::{Hash, Hasher},
        ptr,
        sync::{LazyLock, Mutex},
    },
};

/// The tuple of rigid heads a witness is keyed on: one [`HeadKey`] per concept parameter, in declaration order. Displays bare for arity one (`Nat`) and as a tuple otherwise (`(Nat, Str)`).
///
/// **A reference to the one allocation of its heads**, interned once per process as a [`Qualifier`] is, and for its reason: a witness's name holds its key ([`WitnessId`]), a name is copied and compared far more often than it is built, and an owned key would make [`Global`] a clone at every free variable. Two keys of equal heads are one allocation, so equality is the pointer's.
#[derive(Clone, Copy)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct WitnessKey {
    /// Archived as the bare heads, so the archived comparisons agree with the live ones by construction; read back through the process table — see [`InternedHeads`].
    #[archived_with(InternedHeads)]
    heads: &'static Vec<HeadKey>,
}

/// Every key this process has named. See `curios_utilities`' interner for why nothing is freed.
static KEYS: LazyLock<Table<[HeadKey], Vec<HeadKey>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// The one allocation of `heads`, copied into the table only when this process has not named the key before.
fn interned(heads: &[HeadKey]) -> &'static Vec<HeadKey> {
    intern(&KEYS, heads, || heads.to_vec(), Vec::as_slice)
}

impl WitnessKey {
    /// The heads, one per concept parameter.
    pub fn heads(&self) -> &'static [HeadKey] {
        self.heads.as_slice()
    }

    /// This key as a reader sees it: [`Display`](fmt::Display)'s rendering with each nominal head shortened against what the report's module has in scope.
    ///
    /// `Display` stays faithful, as `Term`'s does. A key is core's identity for a witness — the thing the registry is indexed by — so a rendering of one must not depend on where it is being looked at from; only a *diagnostic* has a reader to spell it for.
    pub fn spelled(&self, spelling: &Spelling) -> String {
        match self.heads() {
            [single] => single.spelled(spelling),
            heads => {
                let heads = heads
                    .iter()
                    .map(|head| head.spelled(spelling))
                    .collect::<Vec<_>>();

                format!("({})", heads.join(", "))
            }
        }
    }
}

impl<I: IntoIterator<Item = HeadKey>> From<I> for WitnessKey {
    fn from(heads: I) -> Self {
        Self {
            heads: interned(&heads.into_iter().collect::<Vec<_>>()),
        }
    }
}

/// Identity is the heads, and interning makes the allocation answer for it.
impl PartialEq for WitnessKey {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.heads, other.heads)
    }
}

impl Eq for WitnessKey {}

/// Ordered by the heads, never by the address, so a map of names comes out the same in every process.
impl Ord for WitnessKey {
    fn cmp(&self, other: &Self) -> Ordering {
        match ptr::eq(self.heads, other.heads) {
            true => Ordering::Equal,
            false => self.heads.cmp(other.heads),
        }
    }
}

impl PartialOrd for WitnessKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Hashes the heads, not the address: a term's structural hash is taken over the names it mentions, and a witness's name holds its key.
impl Hash for WitnessKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.heads.hash(state);
    }
}

/// The sharing is noise; a key prints as its heads.
impl fmt::Debug for WitnessKey {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_tuple("WitnessKey")
            .field(&self.heads)
            .finish()
    }
}

impl fmt::Display for WitnessKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.heads() {
            [single] => write!(f, "{single}"),
            heads => {
                write!(f, "(")?;
                for (index, head) in heads.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{head}")?;
                }
                write!(f, ")")
            }
        }
    }
}

/// Reads an archived key back into the process table every other occurrence of the same heads already uses.
pub struct InterningHeads;

/// The field adapter: `#[archived_with(InternedHeads)]`.
pub type InternedHeads = curios_archive::Via<InterningHeads>;

impl curios_archive::Proxy<&'static Vec<HeadKey>> for InterningHeads {
    type Archivable = Vec<HeadKey>;

    /// Borrowed, never cloned: the archived form is the `Vec` the table already holds.
    fn to_archivable(heads: &&'static Vec<HeadKey>) -> impl std::borrow::Borrow<Vec<HeadKey>> {
        *heads
    }

    fn from_archivable(heads: Vec<HeadKey>) -> Result<&'static Vec<HeadKey>, String> {
        Ok(interned(&heads))
    }
}

/// One rigid head inside a [`WitnessKey`]: the nominal (inductive or struct) qualified name, an intrinsic type constructor, or an anonymous product's shape. A function type has no head here and is refused as unkeyable. Parameters past the heads are checked by unification at resolution time, not by the key.
///
/// A nominal head holds a name, and a witness's name holds a key: the type reaches itself, so its archived bounds are the ones a recursive type states.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(recursive, derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum HeadKey {
    Nominal(#[archived_omit_bounds] Global),
    Nat,
    Byte,
    Int,
    Flt,
    Bool,
    Bin(Grain),
    Handle,
    List,
    Cell,
    Channel,
    Io,
    /// A tuple type, keyed by its *shape*: the label at each field position, `""` where the field is positional, arity implied by the length. A tuple type has no name to be headed by, so the shape plays the role a nominal name plays — it is precisely the half of the type's identity that conversion does not delegate to the fields (`compare_tuple_type` refuses differing labels before enqueuing one), so keying here splits the type along the seam conversion already splits it on. `{Nat, Bool}` and `{x: Nat, y: Bool}` are two keys because they are two types.
    TupleType(Vec<String>),
}

impl HeadKey {
    /// This head as a reader sees it, for [`WitnessKey::spelled`]. Only a nominal head carries a name to shorten; every other case is a fixed word or a shape, and renders as [`Display`](fmt::Display) does.
    fn spelled(&self, spelling: &Spelling) -> String {
        match self {
            HeadKey::Nominal(name) => spelling.symbol(name),
            head => head.to_string(),
        }
    }

    /// The key of a term already in weak-head normal form, if its head is rigid and nominal, intrinsic or a tuple shape. A `Func` head is the higher-kinded case (a type constructor like `Option` reduces to `λA. Option-normal-form`): its *body* supplies the key, so `Monad(Option)` keys on `Option`. `None` for anything else — variables, metavariables, `Type`/`Prop` — which are not keyable.
    pub fn of_whnf(term: &Term) -> Option<HeadKey> {
        match &**term {
            Subterm::InductType(induct_decl) => Some(HeadKey::Nominal(induct_decl.name)),
            Subterm::StructType(struct_decl) => Some(HeadKey::Nominal(struct_decl.name)),
            Subterm::Intrinsic(intrinsic) => Self::of_intrinsic(intrinsic),
            // The weak-head form of a tuple type is the node itself and its labels are structural, so keying costs a walk down the spine and reduces no field type.
            Subterm::TupleType(tuple_type) => Some(Self::of_tuple_type(tuple_type)),
            // The higher-kinded head: the type-constructor function's body is the normal form the applied constructor would reduce to (`λA. InductType(Option, [A])`, or `λT. ListType(T)` for an intrinsic former like `/sys/List`) — through a second function where an indexed family takes its indices in a call of their own (`λT. λn. InductType(Vec, [T], [n])`). The binders need not be opened — the name/former sits on the node.
            Subterm::Func(func) => {
                let mut body = func.telescope.terminal();
                while let Subterm::Func(inner) = &**body {
                    body = inner.telescope.terminal();
                }
                match &**body {
                    Subterm::InductType(induct_decl) => Some(HeadKey::Nominal(induct_decl.name)),
                    Subterm::StructType(struct_decl) => Some(HeadKey::Nominal(struct_decl.name)),
                    Subterm::Intrinsic(intrinsic) => Self::of_intrinsic(intrinsic),
                    // A constructor whose body is an anonymous product — `let Pair(A: Type) -> Type = {Nat, A};` reduces to `(A: Type) => {Nat, A}` — keys on that body's shape, so `Functor(Pair)` registers where `Monad(Option)` does. Symmetry with the nominal case, not a consumer's demand; it does not extend imitation, since `?M(?A) ≡ {Nat, Nat}` has no unique solution.
                    Subterm::TupleType(tuple_type) => Some(Self::of_tuple_type(tuple_type)),
                    // A *partially applied* family: `(A : Type) => State(S, A)` leaves the body a stuck application under the binder, since weak-head reduction never descends into a `Func`. The head of its application spine names the former — through a curried family's calls, `(n : Nat) => Sized(T)(n)` names `Sized` — a registry entry and its type-former definition share one finalized context, so the reference's global *is* the declaration's key, and the universes riding an `Instance` wrapper are irrelevant to keying, which reads names alone. Arguments below the head stay unification's job at resolution time, exactly as for a saturated node.
                    Subterm::Apply(_) => match body.head_name()? {
                        Free::Global(global) => Some(HeadKey::Nominal(*global)),
                        Free::Local(_) => None,
                    },
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// The key of a tuple type, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf). `labels` walks the spine without opening a binder, which is the same read `TupleType`'s own `Eq` performs.
    fn of_tuple_type(tuple_type: &TupleType) -> HeadKey {
        HeadKey::TupleType(
            tuple_type
                .telescope
                .labels()
                .into_iter()
                .map(str::to_owned)
                .collect(),
        )
    }

    /// The key of an intrinsic type former, shared by the first-order and higher-kinded (`Func`-body) positions of [`of_whnf`](Self::of_whnf).
    fn of_intrinsic(intrinsic: &Intrinsic) -> Option<HeadKey> {
        match intrinsic {
            Intrinsic::NatType => Some(HeadKey::Nat),
            Intrinsic::ByteType => Some(HeadKey::Byte),
            Intrinsic::IntType => Some(HeadKey::Int),
            Intrinsic::FltType => Some(HeadKey::Flt),
            Intrinsic::BoolType => Some(HeadKey::Bool),
            Intrinsic::BinType(grain) => Some(HeadKey::Bin(*grain)),
            Intrinsic::HandleType => Some(HeadKey::Handle),
            Intrinsic::ListType(_) => Some(HeadKey::List),
            Intrinsic::CellType(_) => Some(HeadKey::Cell),
            Intrinsic::ChannelType(_) => Some(HeadKey::Channel),
            Intrinsic::IoType(_) => Some(HeadKey::Io),
            _ => None,
        }
    }
}

impl fmt::Display for HeadKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HeadKey::Nominal(name) => write!(f, "{name}"),
            HeadKey::Nat => write!(f, "Nat"),
            HeadKey::Byte => write!(f, "Byte"),
            HeadKey::Int => write!(f, "Int"),
            HeadKey::Flt => write!(f, "Flt"),
            HeadKey::Bool => write!(f, "Bool"),
            HeadKey::Bin(Grain::B) => write!(f, "Bits"),
            HeadKey::Bin(Grain::X) => write!(f, "Bytes"),
            HeadKey::Handle => write!(f, "Handle"),
            HeadKey::List => write!(f, "List"),
            HeadKey::Cell => write!(f, "Cell"),
            HeadKey::Channel => write!(f, "Channel"),
            HeadKey::Io => write!(f, "Io"),
            // A shape displays as the type it stands for with every field type elided: `{}`, `{_, _}`, `{x: _, y: _}`. The field types are not in the key, so there is nothing truthful to print in their place.
            HeadKey::TupleType(labels) => {
                write!(f, "{{")?;
                for (index, label) in labels.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    match label.is_empty() {
                        true => write!(f, "_")?,
                        false => write!(f, "{label}: _")?,
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

/// A witness's identity: the module that declares it, and what it witnesses there.
///
/// **Not a place.** A witness named by its ordinal among its module's witnesses is renamed by every witness written ahead of it, and with it every declaration that resolves through it: a unit stores other items for the same text reordered, and an edit that inserts one witness redoes the dependents of every later one. Its module, its concept and its key are what the declaration says of itself.
///
/// **The module, though coherence makes a concept and a key one witness's.** Two units that each declare a witness at one concept and key are refused by the elaborator, and a name is stored before it runs: with the module in it, the two are two names for the refusal to cite, disjoint by the argument mount disjointness already carries. It is also what lets a witness be *placed*: an import scope is keyed by `Global`, and a documentation page asks which declarations belong to the module it is rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum WitnessId {
    /// A witness whose signature spells its concept and key.
    ///
    /// The concept and the key come first, so the names a unit's witnesses are stored under are in the order a question about a concept at a key reads them: the witnesses a unit declares there are neighbours, whatever modules declare them.
    Spelled {
        /// The concept's path. A concept is an authored declaration, so its path is the whole of its name.
        concept: Qualifier,
        key: WitnessKey,
        module: Qualifier,
        /// Its place among the witnesses of `module` spelled at this concept and key: zero for the one a coherent unit has, and the later ones are the duplicates the elaborator refuses, which need a name of their own to be refused under.
        place: u32,
    },
    /// A witness whose signature spells no key, under the name it was lowered by: its place among the witnesses `module` declares. Registration refuses it, so no stored unit holds one, and a place is all that tells two of them apart until then.
    Unspelled { module: Qualifier, place: u32 },
}

impl WitnessId {
    /// The module that declares it.
    pub fn module(&self) -> &Qualifier {
        match self {
            WitnessId::Spelled { module, .. } | WitnessId::Unspelled { module, .. } => module,
        }
    }

    /// The concept and the key its signature is spelled at, where it spells them.
    pub fn spelled(&self) -> Option<(Global, WitnessKey)> {
        match self {
            WitnessId::Spelled { concept, key, .. } => Some((Global::Authored(*concept), *key)),
            WitnessId::Unspelled { .. } => None,
        }
    }
}

impl fmt::Display for WitnessId {
    /// Module-qualified, naming the module a reader would look in rather than the mount, then what it witnesses there.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WitnessId::Spelled {
                concept,
                key,
                module,
                place,
            } => {
                write!(formatter, "{}/witness@{}(", module.join(), concept.join())?;
                for (index, head) in key.heads().iter().enumerate() {
                    if index > 0 {
                        write!(formatter, ", ")?;
                    }
                    write!(formatter, "{head}")?;
                }
                write!(formatter, ")")?;
                match place {
                    0 => Ok(()),
                    place => write!(formatter, "#{place}"),
                }
            }
            WitnessId::Unspelled { module, place } => {
                write!(formatter, "{}/witness@{place}", module.join())
            }
        }
    }
}

/// What a name is declared as, as far as a key reads it.
pub enum Written<'a> {
    /// A type former: an inductive type, a struct or a concept.
    Former,
    /// A definition, with the body it is bound to.
    Defined(&'a Term),
    /// Anything a key is not read through.
    Opaque,
}

/// The reading of one unit's witness signatures.
pub struct Keying<'a> {
    /// What each name is declared as: the unit's own by its lowered form, another unit's by what it published.
    pub written: &'a dyn Fn(&Global) -> Written<'a>,
    /// The marks the concept `name` declares its parameters under, where it is one.
    pub marks: &'a dyn Fn(&Global) -> Option<Vec<Plicity>>,
}

impl Keying<'_> {
    /// The concept and the key the witness declared at `type_` is spelled at: the application its signature ends in, behind its premise telescope, read parameter by parameter. `None` where the spelling decides either of them no further than elaboration would.
    pub fn key(&self, type_: &Term) -> Option<(Global, WitnessKey)> {
        let Subterm::Apply(apply) = &**terminal(type_) else {
            return None;
        };
        let concept = self.concept(&apply.head, &mut Vec::new())?;
        // Every parameter written, each under the mark the concept declares it by: the arguments are then the parameters in order, and nothing is left for elaboration to place or to infer.
        if !apply.plicities().eq((self.marks)(&concept)?) {
            return None;
        }
        let heads = apply
            .params()
            .map(|param| self.head(param, 0, &mut Vec::new()))
            .collect::<Option<Vec<_>>>()?;

        Some((concept, WitnessKey::from(heads)))
    }

    /// The concept `head` names: a concept's own name, or a definition bound to one and to nothing more.
    fn concept(&self, head: &Term, seen: &mut Vec<Global>) -> Option<Global> {
        let name = named(head)?;
        match (self.written)(&name) {
            Written::Former => (self.marks)(&name).map(|_| name),
            Written::Defined(body) if !seen.contains(&name) => {
                seen.push(name);
                self.concept(body, seen)
            }
            Written::Defined(_) | Written::Opaque => None,
        }
    }

    /// The head `term` is spelled by where it stands applied to `applied` arguments.
    fn head(&self, term: &Term, applied: usize, seen: &mut Vec<Global>) -> Option<HeadKey> {
        match &**term {
            Subterm::Apply(apply) => self.head(&apply.head, applied + apply.arguments.len(), seen),
            Subterm::Var(_) | Subterm::Instance(_) => {
                let name = named(term)?;
                match (self.written)(&name) {
                    Written::Former => Some(HeadKey::Nominal(name)),
                    Written::Defined(body) if !seen.contains(&name) => {
                        seen.push(name);
                        self.head(body, applied, seen)
                    }
                    Written::Defined(_) | Written::Opaque => None,
                }
            }
            // A function applied to none of its parameters is read under its binders. Applied to every one, each written out, it is its body, read as the body stands with what is left of the arguments; applied to some of them, or past a parameter the call leaves to be inferred, it is not read.
            Subterm::Func(func) => match applied {
                0 => self.under_binders(func),
                _ if applied >= func.telescope.len()
                    && func
                        .plicities()
                        .iter()
                        .all(|plicity| matches!(plicity, Plicity::Explicit)) =>
                {
                    let rest = applied - func.telescope.len();
                    self.head(func.telescope.terminal(), rest, seen)
                }
                _ => None,
            },
            _ if applied > 0 => None,
            Subterm::InductType(induct_type) => Some(HeadKey::Nominal(induct_type.name)),
            Subterm::StructType(struct_type) => Some(HeadKey::Nominal(struct_type.name)),
            Subterm::Intrinsic(intrinsic) => HeadKey::of_intrinsic(intrinsic),
            Subterm::TupleType(tuple_type) => Some(HeadKey::of_tuple_type(tuple_type)),
            _ => None,
        }
    }

    /// The head a function's body names: a type's own node, the former it applies, or the intrinsic former a definition it applies is a name for. Reduction does not descend under a binder, so a body is read as it is written, and any other definition applied there is not read through.
    fn under_binders(&self, func: &Func) -> Option<HeadKey> {
        let body = innermost(func);
        match &**body {
            Subterm::InductType(induct_type) => Some(HeadKey::Nominal(induct_type.name)),
            Subterm::StructType(struct_type) => Some(HeadKey::Nominal(struct_type.name)),
            Subterm::Intrinsic(intrinsic) => HeadKey::of_intrinsic(intrinsic),
            Subterm::TupleType(tuple_type) => Some(HeadKey::of_tuple_type(tuple_type)),
            Subterm::Apply(_) => {
                let name = *body.head_name().and_then(Free::as_global)?;
                match (self.written)(&name) {
                    Written::Former => Some(HeadKey::Nominal(name)),
                    Written::Defined(defined) => match &**defined {
                        Subterm::Func(former) => match &**innermost(former) {
                            Subterm::Intrinsic(intrinsic) => HeadKey::of_intrinsic(intrinsic),
                            _ => None,
                        },
                        _ => None,
                    },
                    Written::Opaque => None,
                }
            }
            _ => None,
        }
    }
}

/// The body under every binder `func` and the functions it returns take.
fn innermost(func: &Func) -> &Term {
    let mut body = func.telescope.terminal();
    while let Subterm::Func(inner) = &**body {
        body = inner.telescope.terminal();
    }

    body
}

/// The application a witness's lowered type ends in, behind its premise telescope.
fn terminal(type_: &Term) -> &Term {
    match &**type_ {
        Subterm::FuncType(func_type) => {
            let mut telescope = &func_type.telescope;
            loop {
                match telescope {
                    Telescope::Done(body) => return terminal(body),
                    Telescope::Cons(_, _, scope) => telescope = scope.body(),
                }
            }
        }
        _ => type_,
    }
}

/// The top-level name `term` is, the universes it is instantiated at left aside.
fn named(term: &Term) -> Option<Global> {
    match &**term {
        Subterm::Var(_) | Subterm::Instance(_) => {
            term.head_name().and_then(Free::as_global).copied()
        }
        _ => None,
    }
}
