//! Grading a call: what its arguments are, measured against the caller's parameters under what the arms enclosing it established.
//!
//! Shared by every walk that finds calls — the elaborator's discovery [`Walk`](super::Walk), and the certifier's own typing walk — so a call is graded by one function whichever walk met it. What a walk contributes is its [`SizeContext`], built as it enters and leaves arms, and the arguments it saw at the call.

use {
    super::{Call, Carriers, Guard, Segment, Shape, Tag, spine},
    crate::{Driver, forceable},
    curios_core::{
        CallMatrix, Free, FreeMonoid, Global, Intrinsic, Layer, Nat, Probe, Proj, Size, Struct,
        Subterm, Term, Tuple, Variant,
    },
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

/// How a binder stands to the parameters of the member whose body holds it, each parameter it is graded against beside the relation it bears to it: what a summary hands a lambda's binder ([`binder_bounds`]).
pub type Standing = Vec<(Free, Size)>;

/// Whether shape reading may force `term`.
///
/// [`forceable`] answers the two halves every analysis on this seam shares — whether the head could move, and whether the term is closed enough to reduce. This adds totality's own, and it is what bounds the read in place of a step count: **a `rec` head is refused.** The group under analysis is the one whose termination is being decided, and a group already in scope may be legitimately partial — `check_rec_group` demands descent only where a member is erased, so `/std/Json/decode` classifies `Unfounded` and is still defined. Unfolding it can spin, and refusing makes the read stop on its own terms instead of by exhausting the reduction budget.
///
/// Measured inert — no load-bearing unfolding in the corpus has a `rec` head — so this is a determinism guarantee rather than a restriction.
fn readable(term: &Term) -> bool {
    forceable(term) && !matches!(&**term, Subterm::Rec(_))
}

/// How deep a refinement chain may be expanded.
///
/// Each nested `match` on a binder introduced by an outer arm adds one level — `/std/Str/utf8`'s `advanced` reaches three — so this is generous. It exists only so a pathological or cyclic refinement map cannot loop.
pub const EXPAND_FUEL: usize = 16;

/// The binder an arm refines for the size order, read off its scrutinee: the arm's case stands for what a binder scrutinee is within the arm. Where the scrutinee is an expression no binder stands for the case, and the case's own binders stand below the expression's value instead (`below_scrutinee`).
///
/// **An index inversion's solutions are no part of it.** A solution is an equation conversion concluded — [`solve_indices`](crate::solve_indices) is the one analysis that borrows a driver's conversion — and at a `Prop`-sorted type conversion is proof irrelevance, which every proof of a proposition satisfies. A target holding a proposition's constructor is therefore one spelling of the scrutinee among all of them, and a payload read below the binder through it grades below a value no reduction took apart. The size order reads what reduction took apart and nothing conversion concluded, which is the line [`Driver::force`] already draws by reducing plainly.
///
/// Both drivers read it here, so the rule has one home. What each supplies for itself is the case's shape: the discovery walk builds one from the binders it minted, and the certifier reads the arm's own spelling of the case value.
pub fn refined_by_arm(scrutinee: &Term) -> Option<Free> {
    match &**scrutinee {
        Subterm::Var(var) => var.as_free().cloned(),
        _ => None,
    }
}

/// The shape each binder an arm opens stands at where its scrutinee is an expression, the expression read as `scrutinee`: strictly below the value the expression reads at or below, a case's binders being its value's children. `None` where the expression reads at or below no value a call is measured against, and the binders stand for nothing.
///
/// A case of an expression takes apart the expression's value as a case of a binder takes apart the binder's, so its binders stand below that value as a binder's payloads stand below the binder: a payload of a case on `x.next` is below `x`. The expression is read as a graded call reads its arguments, so nothing an index inversion concluded enters through it.
pub fn below_scrutinee(scrutinee: &Shape) -> Option<Shape> {
    match scrutinee {
        Shape::Atom(_) => Some(Shape::smaller(scrutinee.clone())),
        Shape::Smaller(below) | Shape::AtMost(below) => Some(Shape::Smaller(below.clone())),
        Shape::Node(..) | Shape::UnaryRun { .. } | Shape::ElemRun { .. } | Shape::Opaque => None,
    }
}

/// What the arms enclosing a position have established about the binders in scope: what each has been refined to, which are known nonzero, and which are constructor payloads.
///
/// Built by whichever walk reaches the position — entered on the way into an arm and left on the way out, so knowledge never escapes the branch that established it — and read by [`grade`] at each call.
#[derive(Default)]
pub struct SizeContext {
    /// What each binder has been refined to by the arms enclosing the position being walked.
    refined: BTreeMap<Free, Shape>,
    /// The binders an enclosing arm has established are not zero.
    ///
    /// This is what makes an arithmetic decrease sound rather than merely plausible: `n / k` is below `n` only when `n` is nonzero, and without the guard `let loop(n: Nat) -> Nat = loop(n / 10);` would be accepted while looping forever at zero.
    nonzero: BTreeSet<Free>,
    /// The binders the enclosing inductive arms bound as constructor payloads.
    ///
    /// An application whose head is one of these reads as the payload itself, which is what grades `below(y, r)` below the `intro(x, below)` an arm refined the scrutinee to. A head bound anywhere else — a parameter, a lambda binder — is not in the set and reads as any other application does.
    payloads: BTreeSet<Free>,
    /// The binders of a lambda a call hands a parameter its callee applies as a summary states ([`Applications`](curios_core::Applications)), each with how it stands to the caller's parameters it was graded against: what an application of the parameter hands it, wherever the callee applies it.
    assumed: BTreeMap<Free, Standing>,
    /// One entry per scope [`SizeContext::enter`] has opened and [`SizeContext::exit`] has yet to close.
    scopes: Vec<Undo>,
}

/// What one [`SizeContext::enter`] changed, so its [`SizeContext::exit`] puts back exactly that and nothing else.
///
/// Recorded on the way in rather than recomputed on the way out, because only the entering side can tell the difference that matters: a binder an outer arm had already refined, or already knew nonzero, must be left standing as that outer arm left it.
pub struct Undo {
    /// The refined binders, each with what it stood for before, empty when the scope refined nothing.
    refined: Vec<(Free, Option<Shape>)>,
    /// The binders this scope added to `nonzero`, empty when it added none — an already-known binder included.
    nonzero: Vec<Free>,
    /// The binders this scope added to `payloads`, empty when it added none.
    payloads: Vec<Free>,
    /// The binders this scope assumed bounds of, each with what it stood for before.
    assumed: Vec<(Free, Option<Standing>)>,
}

impl SizeContext {
    /// Enter an arm that establishes `binder` is `value`, that `nonzero` cannot be zero, and that `payloads` are the constructor's payloads — each where given.
    ///
    /// `value` is read as a shape against the context as it stands before the arm, which is where the value was built. A read the budget refuses opens nothing, so the refusal leaves the context as balanced as it found it.
    pub fn enter<D: Driver>(
        &mut self,
        driver: &mut D,
        refine: Option<(Free, &Term)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
    ) -> Result<(), D::Error> {
        let refine = match refine {
            Some((binder, value)) => {
                let shape = Grader::new(driver, self).shape_of(value)?;
                Some((binder, shape))
            }
            None => None,
        };
        self.open(refine, nonzero, payloads);
        Ok(())
    }

    /// Enter an arm whose scrutinee is an expression: each of `binders`, the case's own, stands below the value `scrutinee` reads at or below (`below_scrutinee`), `scrutinee` read against the context as it stands before the arm. A read the budget refuses opens nothing, as [`SizeContext::enter`]'s does.
    pub fn enter_below<D: Driver>(
        &mut self,
        driver: &mut D,
        scrutinee: &Term,
        binders: &[Free],
    ) -> Result<(), D::Error> {
        let shape = Grader::new(driver, self).shape_of(scrutinee)?;
        let refines = below_scrutinee(&shape)
            .map(|below| {
                binders
                    .iter()
                    .map(|binder| (*binder, below.clone()))
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        self.open(refines, None, Vec::new());
        Ok(())
    }

    /// [`SizeContext::enter`] with each refinement already a shape — the discovery walk builds an arm's shape from the binders it opened.
    ///
    /// The three kinds of scoped knowledge open together because they close together: a boolean arm refines its scrutinee *and* rules out zero, an inductive arm refines its scrutinee *and* binds payloads, and one bracket is one thing to keep balanced instead of three.
    pub(super) fn open(
        &mut self,
        refines: impl IntoIterator<Item = (Free, Shape)>,
        nonzero: Option<Free>,
        payloads: Vec<Free>,
    ) {
        // A binder that stands for another, bare — a lambda's binder applied on the spot to it — is one value with it wherever the lambda's body runs, so a case refining the one refines the other in the same bracket.
        let mut refined = Vec::new();
        for (binder, shape) in refines {
            let mut next = Some(binder);
            while let Some(binder) = next.take() {
                next = match self.refined.get(&binder) {
                    Some(Shape::Atom(stood_for))
                        if *stood_for != binder
                            && !refined.iter().any(|(seen, _)| seen == stood_for) =>
                    {
                        Some(*stood_for)
                    }
                    _ => None,
                };
                let previous = self.refined.insert(binder, shape.clone());
                refined.push((binder, previous));
            }
        }
        // Zero ruled out of a binder is ruled out of what it stands for bare, in the same bracket, for the reason a case refining the one refines the other.
        let mut ruled = Vec::new();
        let mut next = nonzero;
        while let Some(atom) = next.take() {
            if self.nonzero.insert(atom) {
                ruled.push(atom);
            }
            next = match self.refined.get(&atom) {
                Some(Shape::Atom(stood_for))
                    if *stood_for != atom && !ruled.contains(stood_for) =>
                {
                    Some(*stood_for)
                }
                _ => None,
            };
        }
        let nonzero = ruled;
        let payloads = payloads
            .into_iter()
            .filter(|binder| self.payloads.insert(*binder))
            .collect();

        self.scopes.push(Undo {
            refined,
            nonzero,
            payloads,
            assumed: Vec::new(),
        });
    }

    /// Open a scope in which each of `bounds`' binders stands to the caller's parameters as it states ([`binder_bounds`]): the body of a lambda handed where its callee applies it.
    pub fn assume(&mut self, bounds: Vec<(Free, Standing)>) {
        let assumed = bounds
            .into_iter()
            .map(|(binder, bound)| (binder, self.assumed.insert(binder, bound)))
            .collect();
        self.scopes.push(Undo {
            refined: Vec::new(),
            nonzero: Vec::new(),
            payloads: Vec::new(),
            assumed,
        });
    }

    /// Close the innermost open scope, putting back exactly what its [`SizeContext::enter`] recorded.
    pub fn exit(&mut self) {
        let undo = self
            .scopes
            .pop()
            .expect("every exit is bracketed with its own enter");

        for (binder, previous) in undo.refined.into_iter().rev() {
            match previous {
                Some(previous) => self.refined.insert(binder, previous),
                None => self.refined.remove(&binder),
            };
        }
        for atom in undo.nonzero {
            self.nonzero.remove(&atom);
        }
        for binder in undo.payloads {
            self.payloads.remove(&binder);
        }
        for (binder, previous) in undo.assumed.into_iter().rev() {
            match previous {
                Some(previous) => self.assumed.insert(binder, previous),
                None => self.assumed.remove(&binder),
            };
        }
    }

    /// Whether every scope entered has been left.
    pub fn is_balanced(&self) -> bool {
        self.scopes.is_empty()
    }

    /// How many scopes are open — a mark [`SizeContext::exit_to`] returns to.
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Close every scope opened since `depth` was the [`SizeContext::depth`], innermost first — for a walk that brackets scopes by marks rather than pairing each entry with its exit.
    pub fn exit_to(&mut self, depth: usize) {
        while self.scopes.len() > depth {
            self.exit();
        }
    }
}

/// Grade a call from the member whose parameters are `params` to a member taking `callee_arity` parameters, with `arguments`, under `context`: each argument against each parameter, as strictly smaller, equal, or unrelated.
pub fn grade<D: Driver>(
    driver: &mut D,
    context: &SizeContext,
    caller: usize,
    params: &[Free],
    callee: usize,
    callee_arity: usize,
    arguments: &[Term],
) -> Result<Call, D::Error> {
    curios_common::profile!("grade");
    let mut grader = Grader::new(driver, context);
    let mut matrix = CallMatrix::unknown(params.len(), callee_arity);

    let expanded = params
        .iter()
        .map(|param| grader.expand(param, EXPAND_FUEL))
        .collect::<Vec<_>>();
    // A binder a summary bounds is graded against as a parameter is, and what an argument is of it composes with where it stands to the caller's: `f(c)` with `c` below `t` is below `t`, and so is a payload an arm takes `c` apart into.
    let bounded = context
        .assumed
        .iter()
        .filter_map(|(binder, bound)| {
            let rows = bound
                .iter()
                .filter_map(|(param, size)| Some((params.iter().position(|p| p == param)?, *size)))
                .collect::<Vec<_>>();
            (!rows.is_empty()).then(|| (grader.expand(binder, EXPAND_FUEL), rows))
        })
        .collect::<Vec<_>>();

    for (column, argument) in arguments.iter().enumerate().take(callee_arity) {
        let shape = grader.shape_of(argument)?;
        for (row, parameter) in expanded.iter().enumerate() {
            matrix.set(row, column, shape.against(parameter));
        }
        for (binder, rows) in &bounded {
            let below = shape.against(binder);
            for (row, bound) in rows {
                let through = below.compose(*bound);
                matrix.set(*row, column, matrix.entry(*row, column).join(through));
            }
        }
    }

    Ok(Call {
        caller,
        callee,
        matrix,
    })
}

/// How a callee applies one argument a call hands it, graded against the caller's parameters: every application of the argument itself, and of each field path of it, as the callee's summary states them ([`Applications`](curios_core::Applications)).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Applied {
    pub itself: Vec<CallMatrix>,
    pub fields: Vec<(Vec<usize>, Vec<CallMatrix>)>,
}

impl Applied {
    /// What the value at `slot` of a record handed here is handed: the applications of that field, and of each path under it.
    pub fn field(&self, slot: usize) -> Applied {
        let mut field = Applied::default();
        for (path, applied) in &self.fields {
            match path.split_first() {
                Some((first, [])) if *first == slot => field.itself.extend(applied.iter().cloned()),
                Some((first, rest)) if *first == slot => {
                    field.fields.push((rest.to_vec(), applied.clone()))
                }
                _ => {}
            }
        }
        field
    }

    /// Whether nothing applies the argument or any field of it.
    pub fn is_empty(&self) -> bool {
        self.itself.is_empty() && self.fields.iter().all(|(_, applied)| applied.is_empty())
    }
}

/// What a call of `callee`'s part at `path` with `arguments` — its leading lambdas at the empty path, a field lambda of the record they answer otherwise, its arguments the leading lambdas' and then the field's — from a member whose parameters are `params`, hands each argument at a parameter the part applies as its summary states ([`Applications`](curios_core::Applications)): every matrix grading an application of that parameter, or of a field of it, against `params`, through what the call hands the part under `context`. `None` at a position the part summarizes nothing at or applies nowhere, and at every position where nothing is filed of the part or the call does not hand it every parameter.
pub fn handed<D: Driver>(
    driver: &mut D,
    context: &SizeContext,
    params: &[Free],
    callee: &Global,
    steps: &[Segment],
) -> Result<Vec<Option<Applied>>, D::Error> {
    let arguments = steps
        .iter()
        .flat_map(Segment::arguments)
        .cloned()
        .collect::<Vec<_>>();
    let arguments = arguments.as_slice();
    let mut handed = vec![None; arguments.len()];
    let Some(applications) = driver.applications(callee) else {
        return Ok(handed);
    };
    // The part the steps reach: the leading lambdas where they apply arguments alone, and the field lambda at the slots they project where the arguments before those fill the leading lambdas and arguments alone follow.
    let is_arguments = |step: &&Segment| matches!(step, Segment::Arguments(_));
    let leading = steps
        .iter()
        .take_while(is_arguments)
        .flat_map(Segment::arguments)
        .count();
    let path = steps
        .iter()
        .skip_while(is_arguments)
        .map_while(|step| match step {
            Segment::Field(slot) => Some(*slot),
            Segment::Arguments(_) => None,
        })
        .collect::<Vec<_>>();
    let after = steps.iter().skip_while(is_arguments).skip(path.len());
    let reaches = path.is_empty()
        || (after.clone().all(|step| is_arguments(&step))
            && applications
                .part(&[])
                .is_some_and(|part| part.arity() == leading));
    let Some(part) = applications.part(&path).filter(|_| reaches) else {
        return Ok(handed);
    };
    let applies = |position: usize| {
        part.of(position).is_some_and(|applied| !applied.is_empty())
            || part
                .fields_of(position)
                .any(|(_, applied)| !applied.is_empty())
    };
    if !(0..arguments.len()).any(applies) {
        return Ok(handed);
    }
    let call = grade(driver, context, 0, params, 0, arguments.len(), arguments)?.matrix;
    let through = |applied: &[CallMatrix]| {
        applied
            .iter()
            .map(|application| call.compose(application))
            .collect::<Option<Vec<_>>>()
    };
    for (position, handed) in handed.iter_mut().enumerate() {
        if !applies(position) {
            continue;
        }
        let Some(itself) = part.of(position).and_then(through) else {
            continue;
        };
        let fields = part
            .fields_of(position)
            .map(|(path, applied)| Some((path.to_vec(), through(applied)?)))
            .collect::<Option<Vec<_>>>();
        *handed = fields.map(|fields| Applied { itself, fields });
    }

    Ok(handed)
}

/// How each of `binders`, a lambda's handed where its callee applies it as `handed` grades, stands to `params`: at each parameter, what every application establishes of the binder at its place, and nothing where one hands it less than that.
pub fn binder_bounds(
    handed: &[CallMatrix],
    params: &[Free],
    binders: &[Free],
) -> Vec<(Free, Standing)> {
    binders
        .iter()
        .enumerate()
        .map(|(column, binder)| {
            let bound = params
                .iter()
                .enumerate()
                .filter_map(|(row, param)| {
                    let size = handed
                        .iter()
                        .map(|application| match column < application.columns() {
                            true => application.entry(row, column),
                            false => Size::Unknown,
                        })
                        .min()
                        .unwrap_or(Size::Unknown);
                    (size != Size::Unknown).then_some((*param, size))
                })
                .collect::<Vec<_>>();
            (*binder, bound)
        })
        .filter(|(_, bound)| !bound.is_empty())
        .collect()
}

/// The binder a boolean arm taken `taken` rules zero out for, when `head` compares a binder against a literal in a way that does.
pub fn nonzero_by<D: Driver>(
    driver: &mut D,
    head: &Term,
    taken: bool,
) -> Result<Option<Free>, D::Error> {
    let context = SizeContext::default();
    let guard = Grader::new(driver, &context).guard(head)?;
    Ok(guard
        .filter(|guard| guard.establishes_nonzero(taken))
        .map(|guard| guard.atom))
}

/// The reader of terms as sizes: a checker to force through, the context the arms established, and what it has read.
pub struct Grader<'a, D: Driver> {
    driver: &'a mut D,
    context: &'a SizeContext,
    /// The shape of each term read so far. The context stands for as long as the reader does, so a term reads as one shape however often it is met: an argument that holds a subterm twice — sixty `let`s each pairing the one before it with itself, handed whole to the member — is read in its size, where a reading per occurrence builds every path through it.
    read: HashMap<Term, Shape>,
}

impl<'a, D: Driver> Grader<'a, D> {
    /// A reader under `context` that has read nothing.
    pub fn new(driver: &'a mut D, context: &'a SizeContext) -> Self {
        Self {
            driver,
            context,
            read: HashMap::new(),
        }
    }

    /// The value a binder currently stands for, following the refinements the enclosing arms established.
    pub fn expand(&self, var: &Free, fuel: usize) -> Shape {
        if fuel == 0 {
            return Shape::Atom(*var);
        }
        match self.context.refined.get(var) {
            None => Shape::Atom(*var),
            Some(shape) => self.expand_shape(shape, fuel - 1),
        }
    }

    fn expand_shape(&self, shape: &Shape, fuel: usize) -> Shape {
        match shape {
            Shape::Atom(var) => self.expand(var, fuel),
            // Left as read: the value each holds was read where its operand stood, and a refinement made since is no part of that claim.
            Shape::Smaller(_) | Shape::AtMost(_) => shape.clone(),
            Shape::Opaque => Shape::Opaque,
            Shape::Node(tag, kids) => Shape::node(
                tag.clone(),
                kids.iter().map(|kid| self.expand_shape(kid, fuel)),
            ),
            // Rebuilt through the constructors so a tail that expands into a run of the same carrier merges back into one canonical run.
            Shape::UnaryRun { count, tail } => {
                Shape::unary_run(count.clone(), self.expand_shape(tail, fuel))
            }
            Shape::ElemRun {
                carrier,
                heads,
                tail,
            } => Shape::elem_run(
                *carrier,
                heads
                    .iter()
                    .map(|head| self.expand_shape(head, fuel))
                    .collect(),
                self.expand_shape(tail, fuel),
            ),
        }
    }

    /// Read a term as a constructor tree.
    ///
    /// A constructor, a free-monoid layer, and a recognised arithmetic decrease read directly; everything else goes to [`Grader::unfolded_shape`], which sees through the definitions standing between a term and its shape. That fallback carries most of what the size order knows — an operator resolves a witness, so even `n - 1` reaches here as a projection — and it is not bounded by a step count; see [`readable`] for what bounds it instead.
    ///
    /// Each term is read once ([`Grader::read`]): a binder is looked up as it stands, and anything else is remembered.
    pub fn shape_of(&mut self, term: &Term) -> Result<Shape, D::Error> {
        if let Subterm::Var(var) = &**term {
            return Ok(match var.as_free() {
                Some(free) => self.expand(free, EXPAND_FUEL),
                None => Shape::Opaque,
            });
        }
        if let Some(read) = self.read.get(term) {
            return Ok(read.clone());
        }
        let shape = self.shape_read(term)?;
        self.read.insert(term.clone(), shape.clone());

        Ok(shape)
    }

    /// [`Grader::shape_of`] for a term that is no binder and has not been read.
    fn shape_read(&mut self, term: &Term) -> Result<Shape, D::Error> {
        Ok(match &**term {
            Subterm::Variant(Variant { tag, payload, .. }) => {
                let kids = self.shapes_of(payload)?;
                Shape::node(Tag::Variant(tag.clone()), kids)
            }

            Subterm::Struct(Struct { name, fields, .. }) => {
                let kids = self.shapes_of(fields)?;
                Shape::node(Tag::Struct(*name), kids)
            }

            Subterm::Tuple(Tuple { fields, .. }) => {
                let kids = self.shapes_of(fields)?;
                Shape::node(Tag::Tuple, kids)
            }

            Subterm::Intrinsic(Intrinsic::Bool(value)) => Shape::node(Tag::Bool(*value), []),

            Subterm::Intrinsic(Intrinsic::Nat(_)) => self.monoid_shape(FreeMonoid::Unary, term)?,

            Subterm::Intrinsic(
                Intrinsic::Bytes(_)
                | Intrinsic::BytesAppend { .. }
                | Intrinsic::BytesConcat { .. }
                | Intrinsic::BytesSlice { .. },
            ) => self.monoid_shape(FreeMonoid::Bytes, term)?,

            Subterm::Intrinsic(
                Intrinsic::List { .. }
                | Intrinsic::ListAppend { .. }
                | Intrinsic::ListConcat { .. }
                | Intrinsic::ListSlice { .. },
            ) => self.monoid_shape(FreeMonoid::List, term)?,

            // An element a list read hands out is one of the list's, below it as a case of the list hands one out; read out of a list literal it is whichever element the index picks, which nothing here reads.
            Subterm::Intrinsic(Intrinsic::ListGet { list, .. }) => {
                let list = self.shape_of(list)?;
                below_scrutinee(&list).unwrap_or(Shape::Opaque)
            }

            // Arithmetic descent. Both operations are monotone and floor-like on Core's unbounded `Nat` — `NatDiv` folds through `Natural` division and `NatSub` truncates at zero — so each is below its left operand whenever that operand is nonzero.
            Subterm::Intrinsic(Intrinsic::NatDiv {
                dividend: left,
                divisor: right,
                ..
            }) => self.arithmetic_shape(left, right, &Natural::from(2usize))?,

            Subterm::Intrinsic(Intrinsic::NatSub(left, right)) => {
                self.arithmetic_shape(left, right, &Natural::from(1usize))?
            }

            // A record is the node of its fields, by eta, so a field of one is strictly below it, and a field of a proof record ranks below the record as each of its least derivation's premises does: a projection of a record literal reads as its field, and one of a binder, or of what is already below one, as below that binder. A field applied is read no further, a record being no family whose function-typed fields sizing keeps below it.
            Subterm::Proj(Proj { head, field }) if field.slot().is_some() => {
                let slot = field.slot().expect("a slot");
                match self.shape_of(head)? {
                    Shape::Node(Tag::Tuple | Tag::Struct(_), kids) if slot < kids.len() => {
                        kids[slot].clone()
                    }
                    atom @ Shape::Atom(_) => Shape::smaller(atom),
                    Shape::Smaller(below) => Shape::Smaller(below),
                    _ => self.unfolded_shape(term)?,
                }
            }

            // An application of a constructor payload reads as the payload it came from: a function-typed payload is a branching node whose children are its applications, so `below(y, r)` grades below `intro(x, below)` for the reason `below` does. The head is read through the same refinement expansion a parameter gets, so a payload bound by a nested pattern reads the same. Any other head — a parameter, a lambda binder, a global — falls through to unfolding.
            Subterm::Apply(_) => {
                let (head, _) = spine(term);
                if let Subterm::Var(var) = &*head
                    && let Some(free) = var.as_free()
                    && self.context.payloads.contains(free)
                {
                    return Ok(self.expand(free, EXPAND_FUEL));
                }
                self.unfolded_shape(term)?
            }

            _ => self.unfolded_shape(term)?,
        })
    }

    /// Each of `terms` read as a shape, in order.
    fn shapes_of(&mut self, terms: &[Term]) -> Result<Vec<Shape>, D::Error> {
        terms.iter().map(|term| self.shape_of(term)).collect()
    }

    /// Read `left op right` as below the value `left` is, or at most it.
    ///
    /// `least` is the smallest literal right-hand operand that makes the operation strictly decreasing: `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. A non-literal operand or an operand below `least` reads as unread. A left side known nonzero — a binder an arm ruled zero out of, a successor run, or already a decrease — makes a decrease; any other is at most what it was, as a fixed point at zero.
    fn arithmetic_shape(
        &mut self,
        left: &Term,
        right: &Term,
        least: &Natural,
    ) -> Result<Shape, D::Error> {
        let Some(divisor) = right.as_nat().and_then(|nat| nat.to_natural()) else {
            return Ok(Shape::Opaque);
        };
        if divisor < *least {
            return Ok(Shape::Opaque);
        }
        Ok(match self.shape_of(left)? {
            // `n` itself, and an arm has ruled out zero.
            Shape::Atom(atom) if self.context.nonzero.contains(&atom) => {
                Shape::smaller(Shape::Atom(atom))
            }
            // A successor run is not zero by its shape: `n` where an arm refined it to `pred + 1`, or what a case equation rewrote it to.
            run @ Shape::UnaryRun { .. } => Shape::smaller(run),
            // Already below `below`, or at most `above`, and these operations never grow: dividing or subtracting again keeps it so.
            Shape::Smaller(below) => Shape::Smaller(below),
            Shape::AtMost(above) => Shape::AtMost(above),
            Shape::Opaque => Shape::Opaque,
            // Not known nonzero, so possibly zero, where the operation is a fixed point, and never above `left`: what a parameter `left` is below is above it too. A case equation is where this is met, `(x + 1) - 2` cancelling to `x - 1` before it is read.
            operand => Shape::at_most(operand),
        })
    }

    /// Decode a whole free-monoid prefix into one packed run over the shape of whatever stops the peel.
    ///
    /// The run mirrors the carrier's own representation instead of re-expanding it: a `Nat`'s successor count is read wholesale off the packed spine, and a `Bytes`/`List` prefix is peeled breadth-wise into one head vector. One node per layer would recurse — in construction and in every later comparison — as deep as the literal is large, and a `Nat` literal's value is unbounded by the source that spelled it.
    fn monoid_shape(&mut self, carrier: FreeMonoid, term: &Term) -> Result<Shape, D::Error> {
        let carriers = match carrier {
            FreeMonoid::Unary => {
                return match &**term {
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => {
                        Ok(Shape::node(Tag::Empty(Carriers::Unary), []))
                    }
                    Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine, inner))) => {
                        Ok(Shape::unary_run(spine.clone(), self.shape_of(inner)?))
                    }
                    _ => self.unfolded_shape(term),
                };
            }
            FreeMonoid::Bytes => Carriers::Bytes,
            FreeMonoid::List => Carriers::List,
        };

        let mut heads = Vec::new();
        let mut rest = Term::unwrap_or_clone(term.clone());
        loop {
            match carrier.uncons(rest) {
                Layer::Empty => {
                    break Ok(Shape::elem_run(
                        carriers,
                        heads,
                        Shape::node(Tag::Empty(carriers), []),
                    ));
                }
                Layer::Cons { head, tail } => {
                    if let Some(head) = head {
                        heads.push(self.shape_of(&head)?);
                    }
                    rest = Term::unwrap_or_clone(tail);
                }
                Layer::Stuck(stuck) => {
                    let stuck = Term::from(stuck);
                    let tail = match heads.is_empty() {
                        // Nothing peeled: the whole term is what stuck, and dispatching it back through `shape_of` would land right here again — force it instead.
                        true => self.unfolded_shape(&stuck)?,
                        // The remainder after a peeled prefix is an arbitrary term — a binder, another literal spelling, an application — and gets the full dispatch.
                        false => self.shape_of(&stuck)?,
                    };
                    break Ok(Shape::elem_run(carriers, heads, tail));
                }
            }
        }
    }

    /// Unfold weak-head steps until the term reads as a shape, or stops moving.
    ///
    /// Definitions stand between a term and its constructor shape, and no enumeration of *which* closes the set: witness projections (an operator resolves a witness, so `n - 1` arrives as `(w).0(n, 1)`), `/sys` intrinsic wrappers and ordinary definitions all do. Unfolding is uniform over all of them because δ and β preserve meaning: a decrease visible after unfolding is a decrease in the term's value.
    ///
    /// There is no step count. Termination rests on what this pass is handed rather than on a budget: every walk that grades a call has typed the terms it reads before asking — the discovery walk runs after the bodies are checked, and the certifier grades a call once its arguments are — positivity refuses a negative occurrence, and the universe hierarchy refuses `Type : Type` — so a well-typed rec-free term normalizes. [`readable`] keeps `rec` out, and the checker's own reduction budget remains the backstop for anything that still fails to settle: the force is a [`Probe`], so a term with no reading is opaque and the budget's refusal propagates as the analysis's own.
    ///
    /// A count here would bound *re-entries* rather than unfoldings: [`Driver::force`] is a full weak-head normalization, so it walks an entire forwarder chain in one call. The stated condition above is what bounds the read instead, and no verdict in the corpus depends on a number.
    fn unfolded_shape(&mut self, term: &Term) -> Result<Shape, D::Error> {
        if !readable(term) {
            return Ok(Shape::Opaque);
        }
        let Some(reduced) = self.driver.force_graded(term).probed()? else {
            return Ok(Shape::Opaque);
        };
        if reduced == *term {
            return Ok(Shape::Opaque);
        }
        self.shape_of(&reduced)
    }

    /// Read a boolean scrutinee as a comparison against a literal.
    ///
    /// Neither spelling arrives as an intrinsic: an operator (`n < 10`) resolves a witness and comes through as a projection out of it, and a named comparison (`Nat/lt(n, 10)`) stays an application of a one-line `/sys` wrapper. The same unfolding [`Grader::shape_of`] uses is what exposes the intrinsic under both, and it is the same [`Probe`].
    pub fn guard(&mut self, head: &Term) -> Result<Option<Guard>, D::Error> {
        if let Some(guard) = Guard::read(head) {
            return Ok(Some(guard));
        }
        // `n != 0` elaborates to the negation of `n == 0`, `xor(n == 0, true)`, which is also what `Bool/not` unfolds to: read as the comparison it negates, its relation negated, as conversion reads a negated comparison as its dual.
        if let Subterm::Intrinsic(Intrinsic::BoolXor(compared, negation)) = &**head
            && negation.as_bool() == Some(true)
        {
            return Ok(self.guard(compared)?.map(Guard::negated));
        }
        if !readable(head) {
            return Ok(None);
        }
        let Some(reduced) = self.driver.force_graded(head).probed()? else {
            return Ok(None);
        };
        if reduced == *head {
            return Ok(None);
        }
        self.guard(&reduced)
    }
}
