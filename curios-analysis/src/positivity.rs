//! Strict positivity modulo polarity: the declaration-time gate that makes `induct` and `struct` sound.
//!
//! An `induct` declaration is a claim that a functor has an initial algebra, and the eliminator it hands back is the statement that the algebra is initial. Strict positivity is a syntactic sufficient condition for that functor being polynomial, hence accessible, hence for the initial algebra existing. Without it, `induct Bad | c(f : (Bad) -> False) end` yields a closed inhabitant of `False` in four lines with no recursion at all.
//!
//! "Modulo polarity" is load-bearing, not a refinement. A check that only recognizes recursive occurrences in immediate payload positions rejects `/std/Toml`, whose recursion travels `Toml → Map(Toml) → struct field → Option → Node → leaf`. Polarity does not weaken the rule — every functor it admits was polynomial already; it only lets the checker recognize that fact through abstraction.
//!
//! The pass computes two facts over the same walk (`occurrences`). The **parameter polarities** — how each type former uses its *i*th parameter — are what composition consumes, and they persist on [`curios_core::InductDecl`] and [`curios_core::StructDecl`] so the prelude's are computed once at archive-build time rather than re-derived per compilation. The **occurrence relation** — at what polarity declaration `D` appears inside declaration `E`, transitively closed — is transient; it exists to answer the acceptance test and is discarded.
//!
//! Acceptance is one predicate: the diagonal of the closed occurrence relation must be [`Polarity::Strict`] or [`Polarity::Unused`] for every declaration — no declaration reaches itself through a non-strict path. That predicate does not change. Every future improvement to the analysis changes how precisely `occurrences` computes, never what the test admits.
//!
//! A structure meets a second rule once it is strictly positive: it reaches itself through no path of record fields alone, a field of a structure or a component of a tuple at every step ([`Holding`]). Such a structure has no finite value, and the size order reads a field strictly below its record, so a recursion on one would descend without end on the open value a checker reduces a type under.
//!
//! # Read through what a type computes
//!
//! A type-level definition is forced to the form it reduces to, and a `match` that form is stuck on is read as the arms it reduces to at every instance ([`crate::opened`]): each at the polarity the `match` stands at, and what supplies an arm's binders, the head or the instance definitional K fires on, at `Mixed` where an arm reads one. Its annotations — the goal, a list's element type, and a supplier no arm reads — stand in no reduct, so no parameter is read in them; a declaration they name is read at `Mixed`, since a `match` does not say which family it takes apart and an annotation naming one is the only witness that a type computes by taking apart a value of it. Every other form the walk cannot see through is read whole at `Mixed` ([`Walk::opaque`]).
//!
//! A value standing where data is expected is a datum ([`crate::Data`]): an index of a family occurrence at its index telescope's type, a value parameter of a family or record occurrence at its parameter telescope's, the head of a `match` whose cases state a data carrier, and a `match` whose goal is data. A datum is the same value at every instance of the declaration's type parameters — no part of it is a type, and no term computes on one — so no type parameter is read inside it ([`answers_a_type`]); a value parameter and a declaration are read there as anywhere, a datum being free to compute on the one and to take apart a value of the other.
//!
//! A recursive call reduction leaves folded — stuck, no unfolding of it getting anywhere — and a member the walk meets again on its own path, a call read as reduction reads one ([`curios_core::Subterm::called_member`]), are not forced further, which would expose the same node one level deeper forever. Each is read by the least fixpoint of its group's unfolding ([`Summary`]): the polarity each of its parameters stands at in its body, and every other target the body reaches, its type read as an annotation, computed for the group's members together from `Unused` and kept for the walk alone, since a group may close over binders the walk opened. Only a group that terminates is read so ([`crate::read_by_unfolding`]): one that may not is read as written, at `Mixed`, its unfolding being one neither checker makes.
//!
//! # Asked before the pass has run
//!
//! Conversion asks whether a struct reaches itself while a unit's items are still being checked, to end its reading of a type's inhabitants, and the relation above does not exist yet: the pass runs over finished declarations, after the items whose conversions ask. [`struct_reaches_itself`] answers from the declarations as they are written. It reads the same diagonal coarsely — any mention counts, at whatever polarity and whatever it would reduce to — so it answers yes wherever the pass would and in more places besides. Its answer ends a walk early and admits nothing: whatever it says, a type judged to have one inhabitant is one whose every field was judged.
//!
//! # Shared, not duplicated
//!
//! Both checkers run *this* analysis, through [`Driver`]. It is a total function of post-zonk declarations, so a second implementation would be a second run of the same function on the same input rather than a second opinion; what each side supplies for itself is reduction, unfolding, and the registry fallback for declarations outside the analyzed set. A reduction the driver refuses does not stop the walk — the term is read as opaque, which is the conservative direction — but it is kept, and the set is refused for it: reading the term at `Mixed` may be all that refuses a declaration, and all that puts a `Mixed` in a vector the analysis would hand back, which a family declared later would then be refused by with no word of the budget. So the analysis fails in exactly two ways, the refusal it exists to produce and a budget that ran out before it could decide.
//!
//! Two obligations are deliberately out of scope. A declaration that takes a *type-former* parameter — `induct Mu(F : (Type) -> Type) | fix(F(Mu(F))) end` — cannot be checked from its own body, because `F` is a binder with no known polarity; discharging that needs an inferred per-binder obligation in a side store, never a field on `FuncType`, and nothing in the corpus takes such a parameter. And whether a `rec` terminates is size-change totality's question (`totality`), a separate mechanism rather than a refinement of this one.

#[cfg(test)]
mod tests;

use {
    crate::{
        Data, Driver, Segment, call_path, codomain, forceable, least_fixpoint, opened, peel,
        read_by_unfolding,
    },
    curios_common::recurse,
    curios_core::{
        Advance, Atom, Bound, Carrier, Cases, Free, FuncType, Global, InductDecl, InductType,
        Intrinsic, Match, Mention, Polarity, RecGroup, StructDecl, StructType, Subterm, Telescope,
        Term, TupleType,
    },
    std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        hash::{Hash, Hasher},
    },
};

/// Why a declaration set was refused.
#[derive(Debug, Clone, PartialEq)]
pub enum PositivityRefusal<D> {
    /// A declaration reaches itself through a non-strict path.
    NotPositive(NotPositive),
    /// A structure reaches itself through record fields alone, so it has no finite value.
    HoldsItself(HoldsItself),
    /// The driver refused a reduction while `name` was being walked — its budget ran out — and the set is refused for it.
    ///
    /// Reported instead of what the analysis computed past it, which may be the refusal's own doing: the term the driver could not reduce was read at `Mixed`, so a recursion through it that would have read `Strict` reads as a non-strict path back, and a vector that would have read `Strict` holds a `Mixed` no declaration put there. `name` is where the budget went, not necessarily a declaration a verdict would have fallen on, since a `Mixed` vector reaches every declaration that composes through it.
    Exhausted { name: Global, error: D },
}

/// A member of a telescope as its declaration names it: by the label it was written under, or by its place among the members where it was written with none.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberLabel {
    Written(String),
    Place(usize),
}

/// The piece of a declaration a refused path starts from. An identity, which each driver words for its own reader.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DeclaredPart {
    /// A payload of the constructor `constructor`.
    Payload {
        constructor: Atom,
        member: MemberLabel,
    },
    /// A structure's field.
    Field(MemberLabel),
    /// The declaration as a whole, where no one piece reproduces the path.
    Whole,
}

/// A declaration refused by the rule itself: `name`'s `part` reaches back to `name` at a non-accepting `polarity`. The driver owns the rendering; `type_` is the offending part's type, for a diagnostic that points at real source.
#[derive(Debug, Clone, PartialEq)]
pub struct NotPositive {
    pub name: Global,
    pub part: DeclaredPart,
    pub type_: Term,
    pub polarity: Polarity,
}

/// A structure refused by the second formation rule: `name`'s `part` leads back to `name` through a field of a structure or a component of a tuple at every step, so `name` has no finite value. The driver owns the rendering; `type_` is the part's type, for a diagnostic that points at real source.
#[derive(Debug, Clone, PartialEq)]
pub struct HoldsItself {
    pub name: Global,
    pub part: DeclaredPart,
    pub type_: Term,
}

/// The declaration set an analysis runs over: the `induct` and `struct` entries of one declaration, a group's taken together.
///
/// **These are analyzed; every other is consulted.** A family the walk meets outside the set answers from its driver's registry ([`Driver::induct_decl`], [`Driver::struct_decl`]), by the vector the entry held there carries. Which vector that is is the driver's to say, and each checker reads only its own: the elaborator's entry carries the vector it wrote as that declaration was published, and the certifier's the vector its own walk computed and filed, never the one elaboration stamped on the entry a module carries. An entry with no vector answers `Mixed`.
///
/// A name is one declaration, and the set is read by name. A name in both registries would be read as the inductive wherever it is asked for, the record never analysed, so the certifier refuses a module that holds one, and lowering refuses the declaration that would produce one.
#[derive(Clone, Copy)]
pub struct Declarations<'a> {
    inducts: &'a BTreeMap<Global, InductDecl>,
    structs: &'a BTreeMap<Global, StructDecl>,
}

impl<'a> Declarations<'a> {
    /// These declarations.
    pub fn of(
        inducts: &'a BTreeMap<Global, InductDecl>,
        structs: &'a BTreeMap<Global, StructDecl>,
    ) -> Self {
        Self { inducts, structs }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.inducts.is_empty() && self.structs.is_empty()
    }

    /// Every name to analyze: the inductives in name order, then the structs. Grouped that way rather than globally sorted because the refusal reported is the first in this order.
    pub(crate) fn names(&self) -> Vec<Global> {
        self.inducts
            .keys()
            .chain(self.structs.keys())
            .cloned()
            .collect()
    }

    pub(crate) fn induct(&self, name: &Global) -> Option<&InductDecl> {
        self.inducts.get(name)
    }

    pub(crate) fn struct_(&self, name: &Global) -> Option<&StructDecl> {
        self.structs.get(name)
    }
}

/// Analyze every declaration in `declarations` for strict positivity modulo polarity, returning each declaration's parameter-polarity vector, or the first refusal in name order — and [`PositivityRefusal::Exhausted`] wherever the driver refused a reduction, so that a vector handed back is the declarations' own.
///
/// The set is exactly what to analyze: the entries one declaration declares, in either checker. Anything the walk reaches outside it answers from the driver's registry ([`Driver::induct_decl`] / [`Driver::struct_decl`]), whose vector was computed when that declaration was judged — sound because a declaration judged before the one analysed cannot mention it, so every out-of-set declaration the driver judged ([`Driver::judged`]) is a sink of the occurrence relation. One it has not judged is none: the certifier registers every family of a module before it judges any, so a family stored after the one analysed and outside its declaration may reach it again through what nothing has read, and its occurrence is read as reaching the declaration walked at [`Polarity::Mixed`]. The elaborator never meets one, refusing a declaration that reaches one stored after it outside its group.
pub fn positivity_vectors<D: Driver>(
    driver: &mut D,
    declarations: Declarations<'_>,
) -> Result<BTreeMap<Global, Vec<Polarity>>, PositivityRefusal<D::Error>> {
    curios_common::profile!("positivity_vectors");
    if declarations.is_empty() {
        return Ok(BTreeMap::new());
    }

    // Opening a telescope mints binders and is the expensive half of the pass, while the fixpoint below re-walks the same pieces several times — so split every declaration exactly once, up front.
    let names = declarations.names();
    let mut split = BTreeMap::new();
    for name in &names {
        // Which parameters are type parameters is read by reduction, so a refused one is reported as every other is.
        match Split::of(driver, declarations, name) {
            Ok(opened) => split.insert(*name, opened),
            Err(error) => {
                return Err(PositivityRefusal::Exhausted { name: *name, error });
            }
        };
    }

    let mut data = Data::new(declarations);
    let (vectors, closed, exhausted) = fixpoint(driver, &mut data, &split);

    // Nothing computed past a refused reduction is handed back: an acceptance would file a vector that may hold the budget's `Mixed`, and a refusal may be that reading's alone.
    if let Some((walked, error)) = exhausted {
        return Err(PositivityRefusal::Exhausted {
            name: walked,
            error,
        });
    }

    for name in &names {
        let diagonal = closed
            .get(name)
            .and_then(|reached| reached.get(name))
            .copied()
            .unwrap_or(Polarity::Unused);
        if diagonal.accepting() {
            continue;
        }

        return Err(PositivityRefusal::NotPositive(refusal(
            driver, &mut data, &split, &vectors, &closed, name, diagonal,
        )));
    }

    // A structure strictly positive in itself may still hold itself through its fields alone, which no finite value does.
    let mut holding = Holding {
        driver,
        declarations,
        held: BTreeMap::new(),
        underway: Vec::new(),
    };
    for name in names
        .iter()
        .filter(|name| declarations.struct_(name).is_some())
    {
        match holding.held(name) {
            Ok(_) => {}
            Err(Stop::Holds(holds)) => return Err(PositivityRefusal::HoldsItself(holds)),
            Err(Stop::Refused { name, error }) => {
                return Err(PositivityRefusal::Exhausted { name, error });
            }
        }
    }

    Ok(vectors.computed)
}

/// What each structure holds through record fields alone, read off its declaration where the walk first meets it: the second formation rule, that no structure reaches itself so.
///
/// A position is field-only where every step to it is a field of a structure or a component of a tuple. A parameter at one is held; a structure at one is entered, and an argument it is given at a parameter it holds continues the position; anything else — a family, a function, an intrinsic, a stuck form — ends it. A family between ends a path because a case of a variable is stuck where a projection of one reduces, and an applied field is read no further (the `Subterm::Proj` arm of totality's `Grader::shape_read`), so it is fields alone that the size order reads below a binder without end.
///
/// Meeting a structure whose own walk is underway is a path back to it, so the walk is exact and it ends: a structure is entered once, its summary kept for the rest of the pass, and one that grows its own arguments, `struct Q(A) { q: Q({A, A}) }`, meets itself on its first step. A structure outside the set is read off its declaration as the driver holds it, which a structure judged earlier cannot name one of the set through but at the arguments it is handed.
///
/// A node one structure's walk has entered is not entered again, told apart by its address and held so the address stays the node's, as the strict-positivity walk's [`Entered`] is: the walk of one structure reads its parameters alone, so a node a second path reaches holds what it held on the first, and a field typed by a tower of tuples, each level naming the one below twice, is walked in its size and not in its paths.
struct Holding<'d, 'e, D: Driver> {
    driver: &'e mut D,
    declarations: Declarations<'d>,
    /// The parameters each structure walked so far holds at a field-only position.
    held: BTreeMap<Global, BTreeSet<usize>>,
    /// The structures whose walks are underway, outermost first, each with the field it is in and that field's type.
    underway: Vec<(Global, DeclaredPart, Term)>,
}

/// Why a [`Holding`] walk stopped short of a summary.
enum Stop<D> {
    Holds(HoldsItself),
    Refused { name: Global, error: D },
}

impl<D: Driver> Holding<'_, '_, D> {
    /// The parameters the structure `name` holds at a field-only position, walked where it is first met. A name the driver holds no structure for holds nothing.
    fn held(&mut self, name: &Global) -> Result<BTreeSet<usize>, Stop<D::Error>> {
        if let Some(held) = self.held.get(name) {
            return Ok(held.clone());
        }
        let declaration = match self.declarations.struct_(name) {
            Some(declaration) => declaration.clone(),
            None => match self.driver.struct_decl(name) {
                Some(declaration) => declaration.clone(),
                None => return Ok(BTreeSet::new()),
            },
        };
        let params = binders(self.driver, &declaration.arity);
        let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
        let mut held = BTreeSet::new();
        let mut entered = HashMap::new();
        for (member, type_) in labelled(self.driver, &declaration.fields_at(&arguments)) {
            self.underway
                .push((*name, DeclaredPart::Field(member), type_.clone()));
            self.position(&type_, &params, &mut held, &mut entered)?;
            self.underway.pop();
        }
        self.held.insert(*name, held.clone());

        Ok(held)
    }

    /// Walk the field-only position `type_` of the structure whose parameters are `params`, into `held`, past every node `entered` holds, by its address.
    fn position(
        &mut self,
        type_: &Term,
        params: &[Free],
        held: &mut BTreeSet<usize>,
        entered: &mut HashMap<usize, Term>,
    ) -> Result<(), Stop<D::Error>> {
        recurse(|| self.position_within(type_, params, held, entered))
    }

    fn position_within(
        &mut self,
        type_: &Term,
        params: &[Free],
        held: &mut BTreeSet<usize>,
        entered: &mut HashMap<usize, Term>,
    ) -> Result<(), Stop<D::Error>> {
        if entered.insert(type_.identity(), type_.clone()).is_some() {
            return Ok(());
        }
        let type_ = match forceable(type_) {
            true => self.driver.force(type_).map_err(|error| Stop::Refused {
                name: self.underway.last().expect("a walk is underway").0,
                error,
            })?,
            false => type_.clone(),
        };
        match &*type_ {
            Subterm::Var(var) => {
                if let Some(free) = var.as_free()
                    && let Some(index) = params.iter().position(|param| param == free)
                {
                    held.insert(index);
                }
            }
            Subterm::TupleType(TupleType { telescope }) => {
                let mut cursor = telescope.cursor();
                while let Some((_, component)) = cursor.entry() {
                    self.position(&component, params, held, entered)?;
                    cursor.advance_fresh(|hint| self.driver.fresh(hint));
                }
            }
            Subterm::StructType(StructType {
                name,
                params: arguments,
                ..
            }) => {
                if let Some((name, part, type_)) = self
                    .underway
                    .iter()
                    .find(|(underway, ..)| underway == name)
                    .cloned()
                {
                    return Err(Stop::Holds(HoldsItself { name, part, type_ }));
                }
                for index in self.held(name)? {
                    if let Some(argument) = arguments.get(index) {
                        self.position(argument, params, held, entered)?;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }
}

/// Whether the struct `name` reaches itself: whether its own name is among the globals its parameters' and fields' types name, or among those the structs they name do, each declaration read once.
///
/// Read off the declarations as they are stored, with nothing opened or reduced, so any mention counts and the answer errs toward yes. It follows struct declarations alone, a struct's fields being all its caller's walk opens: a path back through a definition is not seen here, and is a walk the caller's budget ends. The module documentation says why this is not the pass's own diagonal.
pub fn struct_reaches_itself<D: Driver>(driver: &D, name: &Global) -> bool {
    let mut pending = vec![*name];
    let mut met = BTreeSet::new();

    while let Some(declared) = pending.pop() {
        let Some(declaration) = driver.struct_decl(&declared) else {
            continue;
        };

        let mut named = BTreeSet::new();
        let mut params = &declaration.arity;
        while let Telescope::Cons(_, type_, rest, ..) = params {
            globals_named(type_, &mut named);
            params = rest.body();
        }
        let mut fields = declaration.fields();
        while let Telescope::Cons(_, type_, rest, ..) = fields {
            globals_named(type_, &mut named);
            fields = rest.body();
        }

        if named.contains(name) {
            return true;
        }
        pending.extend(named.into_iter().filter(|named| met.insert(*named)));
    }

    false
}

/// The globals `type_` names: the declarations it references, and the head of each construction and type former in it ([`Subterm::construction_names`]), which no reference names.
fn globals_named(type_: &Term, named: &mut BTreeSet<Global>) {
    named.extend(type_.mentions().declarations().copied());
    named.extend(type_.construction_names());
}

/// One walkable piece of a declaration: a constructor payload binder, a struct field, or an index binder's type — named so a rejection can point at it.
#[derive(Debug)]
struct Split {
    params: Vec<Free>,
    /// Which of `params` is a type parameter ([`answers_a_type`]), which a datum carries none of.
    type_params: Vec<bool>,
    parts: Vec<Piece>,
}

/// A declaration opened for analysis: its parameter binders, and every piece of it the walk consumes.
#[derive(Debug)]
struct Piece {
    part: DeclaredPart,
    type_: Term,
}

impl Split {
    fn of<D: Driver>(
        driver: &mut D,
        declarations: Declarations<'_>,
        name: &Global,
    ) -> Result<Self, D::Error> {
        if let Some(declaration) = declarations.induct(name) {
            let params = binders(driver, declaration.arity());
            let type_params = type_parameters(driver, declaration.arity())?;
            let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
            let mut parts = Vec::new();

            // The index *binder types* are deliberately not walked. They describe the family's arity, not its carrier: they exist to typecheck the index arguments a constructor targets and store nothing, so what a declaration contains is witnessed entirely by its constructor payloads. `Eq(@A : Type) : (x : A, y : A)` is `Strict` in `A` because `refl(@z : A)` has an `A` payload, and walking `x : A` on top of that would only cost the vector its precision — enough to reject the sound `induct Wit | tied(a : Wit, b : Wit, p : Eq()(a, b)) end`.
            //
            // Nor do they need to guard a self-reference. `induct Foo : (x : Foo) -> Type` does not elaborate: `x : Foo` requires `Foo` to already be a type, and it is a family until applied to the very index being declared. The elaborator rejects it on kinding before positivity ever runs.
            //
            // Index *arguments* at an occurrence site are a different position that shares the word, and stay `Mixed` — see `walk`.
            for (tag, constructor) in declaration.constructors() {
                // The terminal is the constructed type, not a payload: it is *always* a recursive occurrence and is skipped, which `labelled` does by ignoring the telescope's `Done`.
                let payload = constructor.open(&arguments);
                for (member, type_) in labelled(driver, &payload) {
                    parts.push(Piece {
                        part: DeclaredPart::Payload {
                            constructor: tag.clone(),
                            member,
                        },
                        type_,
                    });
                }
            }

            return Ok(Self {
                params,
                type_params,
                parts,
            });
        }

        let declaration = declarations
            .struct_(name)
            .expect("every analyzed name is an inductive or a struct");
        let params = binders(driver, &declaration.arity);
        let type_params = type_parameters(driver, &declaration.arity)?;
        let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();
        let parts = labelled(driver, &declaration.fields_at(&arguments))
            .into_iter()
            .map(|(member, type_)| Piece {
                part: DeclaredPart::Field(member),
                type_,
            })
            .collect();

        Ok(Self {
            params,
            type_params,
            parts,
        })
    }
}

/// One fresh binder per entry of a parameter telescope, carrying the declared hints so a diagnostic reads in the user's own names.
pub fn binders<D: Driver, B: Bound>(driver: &mut D, params: &Telescope<B>) -> Vec<Free> {
    params
        .labels()
        .into_iter()
        .map(|label| {
            let hint = (!label.is_empty()).then_some(label);
            driver.fresh(hint)
        })
        .collect::<Vec<_>>()
}

/// Split an opened telescope into `(label, type)` entries, minting a fresh binder per entry so each later type holds free occurrences rather than dangling de Bruijn indices — the precondition for reducing it at all.
pub fn labelled<D: Driver, B: Bound>(
    driver: &mut D,
    telescope: &Telescope<B>,
) -> Vec<(MemberLabel, Term)> {
    let mut entries = Vec::new();
    let mut cursor = telescope.cursor();
    while let Some((hint, type_)) = cursor.entry() {
        let label = match hint.filter(|hint| !hint.is_empty()) {
            Some(hint) => MemberLabel::Written(hint.to_string()),
            None => MemberLabel::Place(cursor.args().len()),
        };
        entries.push((label, type_));
        cursor.advance_fresh(|hint| driver.fresh(hint.filter(|hint| !hint.is_empty())));
    }
    entries
}

/// One declaration's occurrences under the current parameter estimates, and the first reduction the driver refused while finding them.
fn sweep<D: Driver>(
    driver: &mut D,
    data: &mut Data<'_>,
    vectors: &Vectors,
    split: &Split,
) -> (BTreeMap<Target, Polarity>, Option<D::Error>) {
    let mut found = BTreeMap::new();
    let mut refused = None;
    for part in &split.parts {
        let mut walk = Walk::over(driver, data, vectors, split);
        walk.walk(&part.type_, Polarity::Strict);
        for (target, polarity) in walk.found {
            found
                .entry(target)
                .and_modify(|entry: &mut Polarity| *entry = entry.join(polarity))
                .or_insert(polarity);
        }
        refused = refused.or(walk.refused);
    }
    (found, refused)
}

/// The two fixpoints, run together over the whole declaration set to stability, then the occurrence relation closed transitively — beside the first reduction the driver refused, and the declaration whose walk it was.
///
/// Whole-set rather than declaration-by-declaration because a single ordered pass gets the wrong answer twice over. `Node(V)` mentions itself, so its own vector is an input to computing it; and in the mutual pair `A(X) | a(B(X))` / `B(X) | b(f : (X) -> Nat)`, `A` only learns that it is negative in `X` on the round after `B` does. Seeding at `Unused` and joining upward computes the least fixpoint, which is the right one: an inductive declaration *is* the least fixpoint of its own unfolding.
fn fixpoint<D: Driver>(
    driver: &mut D,
    data: &mut Data<'_>,
    split: &BTreeMap<Global, Split>,
) -> (Vectors, Occurrences, Option<(Global, D::Error)>) {
    let mut vectors = Vectors {
        computed: split
            .iter()
            .map(|(name, entry)| (*name, vec![Polarity::Unused; entry.params.len()]))
            .collect(),
    };
    let mut direct: Occurrences = BTreeMap::new();
    let mut exhausted = None;

    loop {
        let mut changed = false;
        for (name, entry) in split {
            let (found, refused) = sweep(driver, data, &vectors, entry);
            if exhausted.is_none()
                && let Some(error) = refused
            {
                exhausted = Some((*name, error));
            }

            let mut vector = vec![Polarity::Unused; entry.params.len()];
            let mut edges: BTreeMap<Global, Polarity> = BTreeMap::new();
            for (target, polarity) in found {
                match target {
                    Target::Param(index) => vector[index] = vector[index].join(polarity),
                    // A member's binder is tracked only inside a summary, which records it under the member.
                    Target::Local(_) | Target::PassedOver(_) => {}
                    Target::Decl(other) => {
                        // A family outside the set its driver has not judged is no sink: what it reaches is unread, so it is read as reaching this declaration again, at `Mixed`.
                        let (other, polarity) =
                            match split.contains_key(&other) || driver.judged(&other) {
                                true => (other, polarity),
                                false => (*name, polarity.compose(Polarity::Mixed)),
                            };
                        edges
                            .entry(other)
                            .and_modify(|entry| *entry = entry.join(polarity))
                            .or_insert(polarity);
                    }
                }
            }

            if vectors.computed.get(name) != Some(&vector) {
                vectors.computed.insert(*name, vector);
                changed = true;
            }
            if direct.get(name) != Some(&edges) {
                direct.insert(*name, edges);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    (vectors, close(&direct), exhausted)
}

/// Transitively close the occurrence relation under composition: if `middle` occurs in `owner` at `first` and `target` occurs in `middle` at `second`, then `target` occurs in `owner` at `first ∘ second`.
///
/// This is what removes any need for the check to know which declarations were declared as one mutually recursive group. That grouping lives on the item holding the entries (`Item::Nominal`) and not on an entry, so asking "who else is in my group" would mean reaching outside the registry; closing the relation instead reaches every member of every cycle by construction.
fn close(direct: &Occurrences) -> Occurrences {
    let mut closed = direct.clone();
    loop {
        let mut changed = false;
        let reached = closed.clone();
        for (owner, edges) in direct {
            for (middle, first) in edges {
                let Some(onward) = reached.get(middle) else {
                    continue;
                };
                for (target, second) in onward {
                    let composed = first.compose(*second);
                    let slot = closed
                        .entry(*owner)
                        .or_default()
                        .entry(*target)
                        .or_insert(Polarity::Unused);
                    let joined = slot.join(composed);
                    if *slot != joined {
                        *slot = joined;
                        changed = true;
                    }
                }
            }
        }
        if !changed {
            return closed;
        }
    }
}

/// Name the piece of `name` that a non-strict path back to `name` starts from.
///
/// Runs only on the error path, which is why the fixpoint above can stay a plain polarity join with no provenance threaded through it. Re-walking one declaration part by part is cheap here and gives a message that points at real source: `Toml`, rejected through `Toml → Map → Node → Toml`, still names the `table` payload the path leaves from.
///
/// Reached only when the fixpoint refused no reduction, so the verdict is the rule's. A reduction this re-walk has refused can only cost the message its part, which the fallback below covers.
fn refusal<D: Driver>(
    driver: &mut D,
    data: &mut Data<'_>,
    split: &BTreeMap<Global, Split>,
    vectors: &Vectors,
    closed: &Occurrences,
    name: &Global,
    diagonal: Polarity,
) -> NotPositive {
    let entry = &split[name];
    for part in &entry.parts {
        let mut walk = Walk::over(driver, data, vectors, entry);
        walk.walk(&part.type_, Polarity::Strict);

        for (target, polarity) in walk.found {
            let Target::Decl(other) = target else {
                continue;
            };
            // A direct self-occurrence stands on its own; anything else has to find its way back, and `closed` already knows how.
            let back = match other == *name {
                true => Polarity::Strict,
                // A family outside the set its driver has not judged reaches back as the fixpoint read it.
                false if !split.contains_key(&other) && !driver.judged(&other) => Polarity::Mixed,
                false => match closed.get(&other).and_then(|reached| reached.get(name)) {
                    Some(back) => *back,
                    None => continue,
                },
            };
            let through = polarity.compose(back);
            if !through.accepting() {
                return NotPositive {
                    name: *name,
                    part: part.part.clone(),
                    type_: part.type_.clone(),
                    polarity: through,
                };
            }
        }
    }

    // The parts were re-walked under the final vectors, so one of them always reproduces the offending path. Fall back to the declaration alone rather than panicking on a diagnostic.
    NotPositive {
        name: *name,
        part: DeclaredPart::Whole,
        type_: Term::type_ground(),
        polarity: diagonal,
    }
}

/// What an occurrence the walk found refers to.
///
/// The two live in one relation because they come from one traversal: the parameter fixpoint consumes the `Param` entries and the occurrence relation consumes the `Decl` entries, so there is exactly one place in the compiler that knows the per-`Subterm` polarity rules.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Target {
    /// The `index`th parameter binder of the declaration being walked.
    Param(usize),
    /// Another declaration, by qualified name — including the walked declaration itself, which is how a recursive occurrence is recorded.
    Decl(Global),
    /// A parameter of a recursive member whose unfolding a walk summarizes, by the binder it was opened at ([`Summary`]): no declaration's target, recorded only inside a summary.
    Local(Free),
    /// Such a parameter standing where a parameter is passed over — an annotation, or a datum where it is a type parameter — which a declaration standing there is not ([`Summary::passed_over`]). Its polarity says nothing; that it was met does.
    PassedOver(Free),
}

/// A recursive member opened for summarizing: its parameters' binders, which of them is a type parameter, its body at those binders, and its type.
struct OpenedMember {
    params: Vec<Free>,
    type_params: Vec<bool>,
    body: Term,
    type_: Term,
}

/// What a recursive member's unfolding holds, read where a walk meets the member again on its own path: the polarity each of its parameters stands at in its body, which of them stand where a parameter is passed over, and every other target it reaches, a declaration, the declaration's parameter or an enclosing member's, its type read as an annotation. The least fixpoint over its group's members, so it bounds every finite unfolding.
#[derive(Debug, Clone, PartialEq)]
struct Summary {
    params: Vec<Polarity>,
    /// Which parameters stand in an annotation, or as a type parameter in a datum, where a parameter is passed over and a declaration is read: an argument for one is read as an annotation, as its unfolding would read what the argument names there.
    passed_over: Vec<bool>,
    found: BTreeMap<Target, Polarity>,
}

/// At what polarity each declaration occurs inside each other declaration — `relation[owner][mentioned]`. Transient: it exists to answer the acceptance test and is discarded, which is why nothing persists it the way parameter polarities are persisted.
type Occurrences = BTreeMap<Global, BTreeMap<Global, Polarity>>;

/// Each analyzed declaration's parameter polarities as the fixpoint currently estimates them.
///
/// A name absent from the map is outside the set under analysis, and answers as its driver holds it ([`Driver::polarity`]): [`Polarity::Mixed`], never `Unused`, where the driver holds no vector for it.
#[derive(Debug)]
struct Vectors {
    computed: BTreeMap<Global, Vec<Polarity>>,
}

impl Vectors {
    fn at<D: Driver>(&self, driver: &D, name: &Global, index: usize) -> Polarity {
        match self.computed.get(name) {
            Some(vector) => vector.get(index).copied().unwrap_or(Polarity::Mixed),
            None => driver.polarity(name, index),
        }
    }
}

/// One declaration's traversal: where it mentions its own parameters, and where it mentions other declarations.
struct Walk<'a, 'd, D: Driver> {
    driver: &'a mut D,
    /// The declaration's parameter binders as this traversal opened them, in declaration order. `Target::Param(i)` is a free occurrence of `params[i]`.
    params: Vec<Free>,
    vectors: &'a Vectors,
    /// Which of `params` is a type parameter, which a datum carries none of.
    type_params: Vec<bool>,
    /// The classification of data, kept for the analysis this traversal is part of.
    data: &'a mut Data<'d>,
    /// The parameters of the recursive members whose unfoldings this traversal summarizes, as their bodies were opened: what [`Target::Local`] records.
    locals: Vec<Free>,
    /// Which of `locals` is a type parameter, which a datum carries none of.
    type_locals: BTreeSet<Free>,
    /// The summaries this traversal computed, by group: a group it meets again is read by them ([`Summary`]). Kept for the traversal alone, since a group may close over the binders it opened.
    summaries: HashMap<RecGroup, Option<Vec<Summary>>>,
    /// The group whose summaries this traversal is a step of estimating, with the estimates so far, which a meet of one of its members reads.
    estimates: Option<(RecGroup, Vec<Summary>)>,
    /// Definitions already unfolded on this traversal. A type-level `let` can be mentioned from many positions, and a recursive group can mention itself; without this the sweep in `blocked` would not terminate.
    unfolded: BTreeSet<Mention>,
    /// The `rec` members this walk is already inside, innermost last — the path, not the traversal.
    ///
    /// [`Self::unfolded`] cannot serve. It keys on a name, and the recursive occurrence [`curios_core::RecGroup::member_body`] substitutes is an inline `rec` node with no name to key on. A stack rather than a set because it is popped on the way out: two *sibling* occurrences of one member sit on different paths and must each still force, or an occurrence that is `Strict` would read `Mixed` and the declaration holding it would be refused.
    forcing: Vec<(RecGroup, usize)>,
    /// The nodes this traversal has entered. A node entered again at the same polarity inside the same members, read the same way, records what it recorded, so a node a reduct shares is walked once however many paths reach it — where [`Self::forcing`]'s sibling occurrences, on paths inside different members, are each still walked.
    entered: HashSet<Entered>,
    /// How what is walked now is read: as what a type holds, or as an annotation no reduct holds.
    reading: Reading,
    found: BTreeMap<Target, Polarity>,
    /// The first reduction the driver refused on this traversal, kept for [`positivity_vectors`] to report.
    refused: Option<D::Error>,
}

/// The type each of `arguments` stands at in `telescope`, each entry instantiated at the arguments before it; `None` past the telescope's last entry.
fn along<B: Bound>(telescope: &Telescope<B>, arguments: &[Term]) -> Vec<Option<Term>> {
    let mut types = Vec::with_capacity(arguments.len());
    let mut cursor = telescope.cursor();
    for argument in arguments {
        types.push(cursor.entry().map(|(_, type_)| type_));
        cursor.advance(argument.clone());
    }
    types
}

/// Whether a binder at `type_` is a type parameter: every value of it a type, or a function answering one once applied to everything it takes ([`codomain`]). A datum can apply such a value but never take apart what it answers. A tuple holding a type is none: a datum can project its other components and compute on them, as it can on a value parameter.
fn answers_a_type<D: Driver>(driver: &mut D, type_: &Term) -> Result<bool, D::Error> {
    Ok(matches!(
        codomain(driver, type_)?.as_deref(),
        Some(Subterm::Sort(_))
    ))
}

/// Which of a telescope's entries is a type parameter ([`answers_a_type`]): what a datum carries none of.
fn type_parameters<D: Driver, B: Bound>(
    driver: &mut D,
    params: &Telescope<B>,
) -> Result<Vec<bool>, D::Error> {
    labelled(driver, params)
        .into_iter()
        .map(|(_, type_)| answers_a_type(driver, &type_))
        .collect()
}

/// How a walk reads what it is walking, each reading reading less than the one after it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Reading {
    /// What a type holds: every target where it stands.
    Content,
    /// A value standing where data is expected ([`Data`]): one value at every instance of the declaration's type parameters, so none of them is recorded; a value parameter and a declaration are, where they stand.
    Datum,
    /// What annotates a term and stands in none of its reducts — a stuck `match`'s goal and a list's element type, and a head no arm reads a binder of. No parameter reaches a type through it, so none is recorded; a declaration is, at `Mixed`, since an annotation naming a family is the only witness that a type takes apart a value of it.
    Annotation,
}

/// The `rec` member `term` is a folded call to, if it is one ([`Subterm::called_member`]).
///
/// These are the shapes a member's unfolding reproduces, so they are the shapes a cycle in the walk is built from, and the call is the one reduction, conversion and totality read: a member applied past its own parameters, or called through a field of the record it answers, `W(x).0(l)`, is one call. A head that has yet to unfold to one — a definition's name, a group with a computed tail — carries no key and forces unguarded.
pub fn rec_member(term: &Term) -> Option<(RecGroup, usize)> {
    term.called_member()
        .map(|(group, index)| (group.clone(), index))
}

/// A node as a walk entered it, at a polarity, read a way, inside the `rec` members of its path. Told apart by the node's address, and holding the node, so the address is the node's for as long as the walk keys by it: the walk forces and opens what it reads and drops what it made, and a node met later could take a freed one's address.
struct Entered {
    node: Term,
    polarity: Polarity,
    reading: Reading,
    inside: Vec<(RecGroup, usize)>,
}

impl PartialEq for Entered {
    fn eq(&self, other: &Self) -> bool {
        self.node.identity() == other.node.identity()
            && self.polarity == other.polarity
            && self.reading == other.reading
            && self.inside == other.inside
    }
}

impl Eq for Entered {}

impl Hash for Entered {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.node.identity().hash(state);
        self.polarity.hash(state);
        self.reading.hash(state);
        self.inside.hash(state);
    }
}

impl<'a, 'd, D: Driver> Walk<'a, 'd, D> {
    /// A traversal of one of `split`'s parts, with nothing found yet.
    fn over(
        driver: &'a mut D,
        data: &'a mut Data<'d>,
        vectors: &'a Vectors,
        split: &Split,
    ) -> Self {
        Self {
            driver,
            params: split.params.clone(),
            vectors,
            type_params: split.type_params.clone(),
            data,
            locals: Vec::new(),
            type_locals: BTreeSet::new(),
            summaries: HashMap::new(),
            estimates: None,
            unfolded: BTreeSet::new(),
            forcing: Vec::new(),
            entered: HashSet::new(),
            reading: Reading::Content,
            found: BTreeMap::new(),
            refused: None,
        }
    }

    fn record(&mut self, target: Target, polarity: Polarity) {
        let skipped = match (&target, self.reading) {
            (Target::Decl(_) | Target::PassedOver(_), _) | (_, Reading::Content) => false,
            (_, Reading::Annotation) => true,
            (Target::Param(index), Reading::Datum) => {
                self.type_params.get(*index).copied().unwrap_or(false)
            }
            (Target::Local(free), Reading::Datum) => self.type_locals.contains(free),
        };
        if skipped {
            // A member's parameter passed over here names, at a meet, whatever its argument is, and a declaration the argument names would be read here: the meet reads that off this ([`Summary::passed_over`]).
            if let Target::Local(free) = target {
                self.record(Target::PassedOver(free), Polarity::Mixed);
            }
            return;
        }
        self.found
            .entry(target)
            .and_modify(|found| *found = found.join(polarity))
            .or_insert(polarity);
    }

    /// Weak-head-reduce `term` when its head could reduce, which is what unfolds a type-level `let` — `Valid(bytes)` inside `struct Str` — into the type former it actually names.
    ///
    /// Forces a `rec` head rather than leaving it stuck, because a mutually recursive `induct` group lowers its *type constructors* into a top-level `rec`: `Pause` inside `step(pause : Pause, …)` elaborates to a universe instance of a projection, and without forcing it the whole `Pause`/`Step` group reads as `Mixed` and is rejected.
    ///
    /// Declines in two cases, both of which leave the term to be treated as opaque, which is conservative. [`forceable`] owns the first — a head that cannot move, or a term still under enclosing binders. And a reduction the driver refuses (an exhausted budget on a type-level `rec` that will not converge) does not stop the walk: the refusal is kept in [`Walk::refused`], and the flag says so, because [`Walk::walk`] then follows the term's definitions at `Mixed` itself — a refused name would otherwise stand as a bare `Var` that records nothing, and a payload type the driver could not read would be admitted through.
    fn forced(&mut self, term: &Term) -> (Term, bool) {
        if !forceable(term) {
            return (term.clone(), false);
        }
        match self.driver.force(term) {
            Ok(reduced) => (reduced, false),
            Err(error) => {
                if self.refused.is_none() {
                    self.refused = Some(error);
                }
                (term.clone(), true)
            }
        }
    }

    /// A function type: each domain flips, the terminal keeps its polarity.
    ///
    /// A nullary `() -> X` is `Done(X)` — an *empty* domain telescope — so this loop never runs and `X` keeps the polarity it arrived with. That is not an edge case to tolerate but the rule that admits `later(next : () -> Susp(A))`: a zero-argument function is a thunk of its result, so `Susp(A) = now(A) | later(X)` is the polynomial functor it looks like.
    fn arrow(&mut self, telescope: &Telescope<Term>, polarity: Polarity) {
        let flipped = polarity.flip();
        let mut cursor = telescope.cursor();
        // Each binder is fresh so what follows holds free occurrences rather than dangling indices and can be reduced; it is never assumed a type, since the walk reads structure, not sorts.
        while let Some((_, domain)) = cursor.entry() {
            self.walk(&domain, flipped);
            cursor.advance_fresh(|hint| self.driver.fresh(hint));
        }
        let terminal = cursor.body().expect("a cursor past every entry");
        self.walk(&terminal, polarity)
    }

    /// A field telescope — a tuple type, a struct's fields, a constructor's payload. Every component keeps the polarity it arrived with: a product is as positive as its factors.
    fn fields(&mut self, telescope: &Telescope<()>, polarity: Polarity) {
        let mut cursor = telescope.cursor();
        while let Some((_, field)) = cursor.entry() {
            self.walk(&field, polarity);
            cursor.advance_fresh(|hint| self.driver.fresh(hint));
        }
    }

    /// A nominal type former's arguments, composed against how that former uses each one, each a datum where the parameter it fills is typed at data. `Toml/table(Map(Toml))` is admitted here and nowhere else: `Map` is `Strict` in its parameter, so the `Toml` inside stays strict.
    fn arguments(
        &mut self,
        name: &Global,
        arguments: &[Term],
        types: &[Option<Term>],
        polarity: Polarity,
    ) {
        for (index, argument) in arguments.iter().enumerate() {
            let used = self.vectors.at(self.driver, name, index);
            let type_ = types.get(index).and_then(Option::as_ref);
            self.value_at(argument, type_, polarity.compose(used));
        }
    }

    /// A stuck `match`, read as what it reduces to: every instance reduces it to one arm, its binders supplied by the head or by the instance the `match` states, so each arm is read at the polarity the `match` stands at, opened against fresh binders, and the suppliers at `Mixed` where an arm reads one of its binders. Where none does they only select an arm, whose reading the arms' join bounds, and are read as annotations, as the goal and a list's element type are.
    ///
    /// A `match` whose goal is data is a datum whole, and a head whose cases state a data carrier — a `Bool`, a `Nat`, `Bytes`, a list of data — is one.
    fn stuck(&mut self, matched: &Match, polarity: Polarity) {
        let reading = match self.is_data(&matched.goal) {
            true => Reading::Datum,
            false => Reading::Content,
        };
        let head_type = match &matched.cases {
            Cases::Bool { .. } => Some(Term::intrinsic(Intrinsic::BoolType)),
            Cases::Switch { keys, .. } => Some(keys.carrier()),
            Cases::FreeMonoid { carrier } => Some(match carrier {
                Carrier::Nat { .. } => Term::intrinsic(Intrinsic::NatType),
                Carrier::Bytes { .. } => Term::intrinsic(Intrinsic::BytesType),
                Carrier::List { elem, .. } => Term::intrinsic(Intrinsic::ListType(elem.clone())),
            }),
            Cases::Induct { .. } => None,
        };

        self.within(reading, |walk| {
            let opened = opened(walk.driver, matched);
            for arm in &opened.arms {
                walk.walk(arm, polarity);
            }
            for (position, supplier) in opened.suppliers.iter().enumerate() {
                match opened.reads_a_binder {
                    // The head is the first supplier, and the one its cases type.
                    true => {
                        let type_ = head_type.as_ref().filter(|_| position == 0);
                        walk.value_at(supplier, type_, Polarity::Mixed);
                    }
                    false => walk.annotation(supplier),
                }
            }
            for annotation in &opened.annotations {
                walk.annotation(annotation);
            }
        });
    }

    /// Walk `term` as an annotation ([`Reading::Annotation`]), at `Mixed`.
    fn annotation(&mut self, term: &Term) {
        self.within(Reading::Annotation, |walk| walk.walk(term, Polarity::Mixed));
    }

    /// Walk `term` at `polarity`, as a datum ([`Reading::Datum`]) where `type_` is data, the type its position states.
    fn value_at(&mut self, term: &Term, type_: Option<&Term>, polarity: Polarity) {
        match type_.is_some_and(|type_| self.is_data(type_)) {
            true => self.within(Reading::Datum, |walk| walk.walk(term, polarity)),
            false => self.walk(term, polarity),
        }
    }

    /// Run `read` reading no more than `reading` reads: a reading entered inside another that reads less keeps the outer one.
    fn within(&mut self, reading: Reading, read: impl FnOnce(&mut Self)) {
        let outer = self.reading;
        self.reading = outer.max(reading);
        read(self);
        self.reading = outer;
    }

    /// Whether `type_` is data, a reduction the classification refuses kept as the walk keeps its own.
    fn is_data(&mut self, type_: &Term) -> bool {
        self.data.is_data(self.driver, type_, &mut self.refused)
    }

    /// The type each of a family's or record's parameters and indices stands at in this occurrence, by position, each instantiated at the arguments before it; `None` past what the entry states, or for a name with no entry. Read off the entry where it is held, the declarations analysed first.
    fn argument_types(
        &self,
        name: &Global,
        params: &[Term],
        indices: &[Term],
    ) -> (Vec<Option<Term>>, Vec<Option<Term>>) {
        let declarations = self.data.declarations();
        let induct = declarations
            .induct(name)
            .or_else(|| self.driver.induct_decl(name));
        if let Some(declaration) = induct {
            return (
                along(declaration.arity(), params),
                along(&declaration.indices_at(params), indices),
            );
        }
        let record = declarations
            .struct_(name)
            .or_else(|| self.driver.struct_decl(name));
        match record {
            Some(declaration) => (along(&declaration.arity, params), Vec::new()),
            None => (vec![None; params.len()], vec![None; indices.len()]),
        }
    }

    /// A recursive member met again on its own path, read through its [`Summary`]: each argument at the polarity its parameter stands at in the member's body composed with where the call stands, and as an annotation too where that parameter stands where a parameter is passed over, as the unfolding would read what the argument names there; an argument past the member's parameters at `Mixed`, and every other target its unfolding reaches at where the call stands composed with its own.
    fn meet(&mut self, term: &Term, group: &RecGroup, index: usize, polarity: Polarity) {
        // A group not read by its unfolding is read as written ([`read_by_unfolding`]).
        let Some(summary) = self.summary(group, index) else {
            self.opaque(term);
            return;
        };
        // The member's own arguments are the path's before its first field; a field's own, past it, are read at `Mixed`, as an argument past the member's parameters is.
        let (_, steps) = call_path(term);
        let mut position = 0;
        let mut past_a_field = false;
        for step in &steps {
            past_a_field |= matches!(step, Segment::Field(_));
            for argument in step.arguments() {
                let used = match past_a_field {
                    true => Polarity::Mixed,
                    false => summary
                        .params
                        .get(position)
                        .copied()
                        .unwrap_or(Polarity::Mixed),
                };
                let passed_over =
                    !past_a_field && summary.passed_over.get(position).copied().unwrap_or(false);
                position += 1;
                self.walk(argument, polarity.compose(used));
                if passed_over {
                    self.annotation(argument);
                }
            }
        }
        for (target, found) in summary.found {
            self.record(target, polarity.compose(found));
        }
    }

    /// The summary of `group`'s member at `index`: the estimate where this traversal is a step of computing it, and otherwise the fixpoint, computed once a traversal. `None` for a group not read by its unfolding.
    fn summary(&mut self, group: &RecGroup, index: usize) -> Option<Summary> {
        if let Some((estimating, estimates)) = &self.estimates
            && estimating == group
        {
            return Some(estimates[index].clone());
        }
        if let Some(summaries) = self.summaries.get(group) {
            return summaries.as_ref().map(|summaries| summaries[index].clone());
        }
        let summaries = self.summarize(group);
        let summary = summaries.as_ref().map(|summaries| summaries[index].clone());
        self.summaries.insert(group.clone(), summaries);
        summary
    }

    /// The least fixpoint of what `group`'s members' unfoldings hold ([`least_fixpoint`]): every member's body walked at `Strict` under the others' current summaries, from every parameter `Unused` and nothing found, each step's reading joined with the estimate it read under, so the estimates only climb. Each step is a traversal of its own, with every member of the group on its path, so a member it meets is read by the estimate; it tracks the member's parameters beside this traversal's targets, and reads the member's type as an annotation.
    fn summarize(&mut self, group: &RecGroup) -> Option<Vec<Summary>> {
        // A refused reduction reads the group as written, and is kept for the analysis to report.
        match read_by_unfolding(self.driver, group) {
            Ok(true) => {}
            Ok(false) => return None,
            Err(error) => {
                self.refused.get_or_insert(error);
                return None;
            }
        }
        // A member's parameters are its leading lambdas', as totality counts them ([`peel`]); its type is read as an annotation.
        let members = (0..group.length())
            .map(|index| {
                let (params, domains, body) = peel(self.driver, group.member_body(index));
                // A refused reduction reads the binder as a value, and is kept for the analysis to report.
                let type_params = domains
                    .iter()
                    .map(|domain| {
                        answers_a_type(self.driver, domain).unwrap_or_else(|error| {
                            self.refused.get_or_insert(error);
                            false
                        })
                    })
                    .collect::<Vec<_>>();
                OpenedMember {
                    params,
                    type_params,
                    body,
                    type_: group.member_type(index),
                }
            })
            .collect::<Vec<_>>();
        let seed = members
            .iter()
            .map(|member| Summary {
                params: vec![Polarity::Unused; member.params.len()],
                passed_over: vec![false; member.params.len()],
                found: BTreeMap::new(),
            })
            .collect::<Vec<_>>();

        Some(least_fixpoint(seed, |estimates| {
            let mut climbed = estimates.clone();
            for (index, member) in members.iter().enumerate() {
                let mut locals = self.locals.clone();
                locals.extend(&member.params);
                let mut type_locals = self.type_locals.clone();
                type_locals.extend(
                    member
                        .params
                        .iter()
                        .zip(&member.type_params)
                        .filter(|(_, type_param)| **type_param)
                        .map(|(param, _)| *param),
                );
                let mut step = Walk {
                    driver: &mut *self.driver,
                    params: self.params.clone(),
                    vectors: self.vectors,
                    type_params: self.type_params.clone(),
                    data: &mut *self.data,
                    locals,
                    type_locals,
                    summaries: HashMap::new(),
                    estimates: Some((group.clone(), estimates.clone())),
                    unfolded: BTreeSet::new(),
                    forcing: (0..group.length())
                        .map(|member| (group.clone(), member))
                        .collect(),
                    entered: HashSet::new(),
                    reading: Reading::Content,
                    found: BTreeMap::new(),
                    refused: None,
                };
                step.walk(&member.body, Polarity::Strict);
                step.annotation(&member.type_);
                let refused = step.refused.take();
                let mut found = step.found;
                if self.refused.is_none() {
                    self.refused = refused;
                }

                let summary = &mut climbed[index];
                for (position, param) in member.params.iter().enumerate() {
                    summary.passed_over[position] |=
                        found.remove(&Target::PassedOver(*param)).is_some();
                    let read = found
                        .remove(&Target::Local(*param))
                        .unwrap_or(Polarity::Unused);
                    summary.params[position] = summary.params[position].join(read);
                }
                for (target, polarity) in found {
                    summary
                        .found
                        .entry(target)
                        .and_modify(|entry| *entry = entry.join(polarity))
                        .or_insert(polarity);
                }
            }
            climbed
        }))
    }

    /// A form the walk cannot see through: descend into every direct child at `Mixed`.
    ///
    /// `Mixed` is a *result*, never a stop. Halting here would let a recursive occurrence hidden inside a stuck `match` arm or a lambda body pass unseen, which is a soundness hole rather than a missed refinement. The descent reuses `Subterm::any_child_term` so a new term former cannot be added without this walk following it.
    fn opaque(&mut self, term: &Term) {
        let subterm: &Subterm = term;
        subterm.any_child_term(&mut |child| {
            self.walk(child, Polarity::Mixed);
            false
        });
        self.blocked(term);
    }

    /// Follow the definitions of an unreducible term's free variables.
    ///
    /// `any_child_term` visits scope bodies closed, so a child that still sits under a binder is one `forced` refused to reduce — and a definition mentioned there would otherwise hide whatever its body names. Walking each such definition's body (which is closed, hence safe to reduce) at `Mixed` closes that gap. A term under no binder was forced, so it needs no following here; the one that was forced and refused is [`Walk::definitions`]'s, from `walk`.
    fn blocked(&mut self, term: &Term) {
        if term.reach() == 0 {
            return;
        }
        self.definitions(term);
    }

    /// Walk the body of every definition `term` names, at `Mixed`. `unfolded` keeps a definition that is mentioned many times, or that mentions itself, from being followed twice.
    fn definitions(&mut self, term: &Term) {
        for name in term.mentions().iter().copied() {
            if !self.unfolded.insert(name) {
                continue;
            }
            let body = match &name {
                Mention::Local(local) => self.driver.unfold(local),
                Mention::Declaration(declaration) => self.driver.body(declaration),
            };
            let Some(body) = body.cloned() else {
                continue;
            };
            self.walk(&body, Polarity::Mixed);
        }
    }

    /// Where `term` mentions this walk's targets, given that `term` itself sits at `polarity`.
    ///
    /// Depth is bought with stack, as every walk over data-shaped depth buys it: a part's reduct is descended into one level per arrow, nothing between the levels is a loop, and the budget is spent only through [`Driver::force`], where a `FuncType` is already a weak-head value. Without the bracket the native stack goes first and the process dies, which is no refusal ([The soundness board](../../documentation/design/soundness/the-soundness-board.md)).
    fn walk(&mut self, term: &Term, polarity: Polarity) {
        recurse(|| self.walk_within(term, polarity));
    }

    fn walk_within(&mut self, term: &Term, polarity: Polarity) {
        // Nothing inside a position the enclosing former never inspects can occur at all — `Unused` composes to `Unused` however deep the walk would go.
        if polarity == Polarity::Unused {
            return;
        }
        if !self.entered.insert(Entered {
            node: term.clone(),
            polarity,
            reading: self.reading,
            inside: self.forcing.clone(),
        }) {
            return;
        }

        // A `rec` member this path is already inside is not forced again. Forcing it exposes the same node one level deeper — `member_body` substitutes the group into its own body, and every turn opens a fresh binder, so no memo on *terms* would ever see a repeat — and the descent would not terminate. It is read by the least fixpoint of its unfolding instead ([`Summary`]), which bounds every finite unfolding.
        let member = rec_member(term);
        if let Some((group, index)) = member
            .as_ref()
            .filter(|member| self.forcing.contains(member))
        {
            self.meet(term, group, *index, polarity);
            return;
        }
        let (term, refused) = self.forced(term);

        // A recursive call reduction left folded is one no unfolding of gets anywhere, the canonical normal form of a recursion stuck on what it is handed: it is read by the least fixpoint of its group's unfolding, as a call met again on its own path is, and never forced further.
        if !refused && let Some((group, index)) = rec_member(&term) {
            self.meet(&term, &group, index, polarity);
            return;
        }

        // A reduction the driver refused leaves the term as written, and what it names is then read here rather than lost: a bare name would fall to the `Var` arm below and record nothing, which would admit `c(f : D)` with `D` an alias of `(Bad) -> False` whenever unfolding `D` ran out of budget. Following its definitions at `Mixed` is what `opaque` does for every other stuck shape, and it keeps a refusal in the refusing direction.
        if refused {
            self.definitions(&term);
        }
        let entered = match member {
            Some(member) => {
                self.forcing.push(member);
                true
            }
            None => false,
        };

        match &*term {
            // Sorts name nothing, and an instance names a declaration, which no parameter is.
            Subterm::Sort(_) | Subterm::Instance(_) => {}

            // A variable is the occurrence itself, and the one place `opaque` cannot serve: `any_child_term` reports no child here, a `Var`'s identity being the node's own data, so the descent would find nothing and a parameter named this way would keep the `Unused` the fixpoint seeds it at.
            Subterm::Var(var) => {
                if let Some(free) = var.as_free() {
                    if let Some(index) = self.params.iter().position(|param| param == free) {
                        self.record(Target::Param(index), polarity);
                    } else if self.locals.contains(free) {
                        self.record(Target::Local(*free), polarity);
                    }
                }
            }

            Subterm::FuncType(FuncType { telescope, .. }) => self.arrow(telescope, polarity),
            Subterm::TupleType(TupleType { telescope }) => self.fields(telescope, polarity),

            // A nominal occurrence is recorded at the polarity it stands at — `Strict` at the top of a payload, but `Neg` for the `Waker` in `park((Waker) -> {})`. Its arguments are walked *as well*, not instead: `Vec(T, n)` inside `Vec`'s own payload is both a recursive occurrence and a use of `T`.
            Subterm::InductType(InductType {
                name,
                params,
                indices,
                ..
            }) => {
                self.record(Target::Decl(*name), polarity);
                let (param_types, index_types) = self.argument_types(name, params, indices);
                self.arguments(name, params, &param_types, polarity);
                // Indices are not parameters: an inductive is not uniform in them, so nothing can be composed and they stay opaque. The corpus never recurses through one. An index at a data type is a datum.
                for (index, type_) in indices.iter().zip(&index_types) {
                    self.value_at(index, type_.as_ref(), Polarity::Mixed);
                }
            }

            Subterm::StructType(StructType { name, params, .. }) => {
                self.record(Target::Decl(*name), polarity);
                let (param_types, _) = self.argument_types(name, params, &[]);
                self.arguments(name, params, &param_types, polarity);
            }

            Subterm::Intrinsic(intrinsic) => self.intrinsic(intrinsic, polarity, &term),

            // A host call is opaque for the same reason an effectful intrinsic is: its result comes from outside the program, so nothing about the declaration being checked can be read out of it.
            Subterm::Foreign(_) => self.opaque(&term),

            // A `match` the reduction left stuck, under no binder this walk has not opened, is read as the arms it reduces to at every instance.
            Subterm::Match(matched) if term.reach() == 0 => self.stuck(matched, polarity),

            Subterm::Apply(_)
            | Subterm::Func(_)
            | Subterm::Tuple(_)
            | Subterm::Variant(_)
            | Subterm::Match(_)
            | Subterm::Struct(_)
            | Subterm::Proj(_)
            | Subterm::Let(_)
            | Subterm::Rec(_)
            | Subterm::Metavar(_)
            | Subterm::Transient(_) => self.opaque(&term),
        }

        // Every arm above falls through — the match holds no early return — so the path is popped however the walk leaves it.
        if entered {
            self.forcing.pop();
        }
    }

    /// Intrinsic type formers, matched exhaustively rather than consulted through a side table — so adding an intrinsic forces a polarity decision at compile time instead of inheriting a default.
    fn intrinsic(&mut self, intrinsic: &Intrinsic, polarity: Polarity, term: &Term) {
        match intrinsic {
            // The nullary type formers name nothing.
            Intrinsic::BoolType
            | Intrinsic::NatType
            | Intrinsic::ByteType
            | Intrinsic::IntType
            | Intrinsic::FltType
            | Intrinsic::BytesType
            | Intrinsic::HandleType
            | Intrinsic::LatchType => {}

            // `List` is covariant: a list of `T`s is a finite product of `T`s, so a strict occurrence in the element stays strict. This is the rule that admits `Json/arr(List(Json))` and `Toml/arr(List(Toml))`.
            Intrinsic::ListType(element) => self.walk(element, polarity),

            // A box is read *and* written, so it is invariant in its element.
            Intrinsic::BoxType(element) => self.walk(element, Polarity::Mixed),

            // `Io` is covariant, and by the same reading as `List` rather than by analogy to `Box`.
            //
            // A description of a `T` is operationally the delayed `T` its thunk erasure makes it — `{} -> T`, whose codomain is a strictly positive position. What decides the polarity is what the eliminations can extract, and `Io`'s are *weaker* than `List`'s: `List/at` hands back a `T` outright, while `bind` never exposes the `A` except inside another `Io`, and there is no `Io(T) -> T` for any other rule to lean on. A carrier whose only reader is stricter than a covariant carrier's cannot be less positive than it.
            //
            // This is the rule that admits a step whose continuation is computed by performing an effect — `Step(A) | step(next: Io(Step(A)))`.
            Intrinsic::IoType(result) => self.walk(result, polarity),

            // Everything remaining is a value or an operation over values, not a type former. Its operands are terms; whatever they mention, they mention at `Mixed`.
            Intrinsic::Bool(_)
            | Intrinsic::BoolAnd(..)
            | Intrinsic::BoolOr(..)
            | Intrinsic::BoolXor(..)
            | Intrinsic::BoolEql(..)
            | Intrinsic::BoolNeq(..)
            | Intrinsic::Nat(_)
            | Intrinsic::NatEql(..)
            | Intrinsic::NatNeq(..)
            | Intrinsic::NatAdd(..)
            | Intrinsic::NatSub(..)
            | Intrinsic::NatMul(..)
            | Intrinsic::NatLt(..)
            | Intrinsic::NatDiv { .. }
            | Intrinsic::NatRem { .. }
            | Intrinsic::NatLe(..)
            | Intrinsic::NatAnd(..)
            | Intrinsic::NatOr(..)
            | Intrinsic::NatXor(..)
            | Intrinsic::NatMin(..)
            | Intrinsic::NatMax(..)
            | Intrinsic::NatShl(..)
            | Intrinsic::NatPow(..)
            | Intrinsic::NatShr(..)
            | Intrinsic::Byte(_)
            | Intrinsic::ByteToNat(..)
            | Intrinsic::NatToByte { .. }
            | Intrinsic::Int(_)
            | Intrinsic::IntEql(..)
            | Intrinsic::IntNeq(..)
            | Intrinsic::IntAdd(..)
            | Intrinsic::IntSub(..)
            | Intrinsic::IntMul(..)
            | Intrinsic::IntDiv { .. }
            | Intrinsic::IntRem { .. }
            | Intrinsic::IntLt(..)
            | Intrinsic::IntLe(..)
            | Intrinsic::IntAnd(..)
            | Intrinsic::IntOr(..)
            | Intrinsic::IntXor(..)
            | Intrinsic::IntMin(..)
            | Intrinsic::IntMax(..)
            | Intrinsic::IntShl(..)
            | Intrinsic::IntShr(..)
            | Intrinsic::Flt(_)
            | Intrinsic::FltAdd(..)
            | Intrinsic::FltSub(..)
            | Intrinsic::FltMul(..)
            | Intrinsic::FltDiv(..)
            | Intrinsic::FltRem(..)
            | Intrinsic::FltEql(..)
            | Intrinsic::FltNeq(..)
            | Intrinsic::FltLt(..)
            | Intrinsic::FltLe(..)
            | Intrinsic::FltMin(..)
            | Intrinsic::FltMax(..)
            | Intrinsic::FltNeg(..)
            | Intrinsic::FltAbs(..)
            | Intrinsic::FltSqrt(..)
            | Intrinsic::FltFma(..)
            | Intrinsic::FltRoundIntegral(..)
            | Intrinsic::FltCopysign(..)
            | Intrinsic::NatToInt(..)
            | Intrinsic::IntAbs(..)
            | Intrinsic::IntSign(..)
            | Intrinsic::NatToFlt(..)
            | Intrinsic::IntToNat { .. }
            | Intrinsic::IntToFlt(..)
            | Intrinsic::FltToNat { .. }
            | Intrinsic::FltMantissa { .. }
            | Intrinsic::FltExponent { .. }
            | Intrinsic::FltToLeBytes(..)
            | Intrinsic::FltOfLeBytes { .. }
            | Intrinsic::FltToInt { .. }
            | Intrinsic::Bytes(..)
            | Intrinsic::BytesLen(..)
            | Intrinsic::BytesEql(..)
            | Intrinsic::BytesGet { .. }
            | Intrinsic::BytesSlice { .. }
            | Intrinsic::BytesAppend { .. }
            | Intrinsic::BytesConcat { .. }
            | Intrinsic::BytesReplicate { .. }
            | Intrinsic::BytesAnd { .. }
            | Intrinsic::BytesOr { .. }
            | Intrinsic::BytesXor { .. }
            | Intrinsic::List { .. }
            | Intrinsic::ListLen { .. }
            | Intrinsic::ListGet { .. }
            | Intrinsic::ListSlice { .. }
            | Intrinsic::ListSet { .. }
            | Intrinsic::ListAppend { .. }
            | Intrinsic::ListConcat { .. }
            | Intrinsic::ListMap { .. }
            | Intrinsic::ListFold { .. }
            | Intrinsic::Handle(_)
            | Intrinsic::Latch
            | Intrinsic::LatchTrip { .. }
            | Intrinsic::LatchTripped { .. }
            | Intrinsic::Box { .. }
            | Intrinsic::BoxHeld { .. }
            | Intrinsic::BoxSet { .. }
            | Intrinsic::IoPure { .. }
            | Intrinsic::IoBind { .. }
            | Intrinsic::IoSuspend { .. }
            | Intrinsic::IoAdvance { .. } => self.opaque(term),
        }
    }
}
