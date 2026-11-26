//! The matrix compiler: a match as it is written, compiled into the single-level matches the eliminators rebuild.
//!
//! [`elaborate_matrix`] takes the [`Matrix`] lowering wrote and builds the decision tree its rows come to (Maranget, "Compiling Pattern Matching to Good Decision Trees"), with each column's cases read off its type. Columns are taken left to right. A column no row takes apart is retired, each name written there bound to it. A tuple or struct column is flattened into its fields' projections, and into the fields themselves where the column is a tuple written in place. Any other is a dispatch: the rows that name a case are its arm, a row that writes a binder there joins the arm of every case some row names, and where the type has a case no row names, the rows that write a binder are what any other value takes.
//!
//! Each dispatch is an eliminator's own, taken a step at a time ([`NominalSplit`] and its kin): the column is resolved as a scrutinee, each case is entered as an arm of it is, with the rows of that case over the columns left as the arm's body, and the match is rebuilt from the arms. So what an arm assumes is stated once, for the tree and for a match written one level deep.
//!
//! **A row is typed once, where its own pattern states it.** A row whose every case on the way to its leaf is one it states is elaborated at that leaf, whose frames are the row's own. A row that a binder of its own put under a case another row wrote reaches a leaf whose frames hold more than it states, and may reach several. It is elaborated once, by its own walk ([`Compiler::walk`]): its cases entered alone, standing on the frames held where the match is entered. Each leaf it reaches holds that body with the leaf's binders in the places of the walk's ([`Term::substitute`]), which the elaborator does not type again and the certifier does.
//!
//! **The row that cannot fail**, a final row that takes nothing apart and has no guard, after rows of which one takes the scrutinee apart, states no case, so it is read where the match stands and at the match's own type, once, after the rows before it: written in place where one leaf reaches it, and bound once as a function of no argument, called from each, where more do. Where no row takes the scrutinee apart there is one leaf, and every row is read there.
//!
//! **Arm order decides nothing between patterns.** The rows that reach a leaf are its candidates, and the first written is taken. Each of the others either has it as a case, a general arm standing after its exception, or is a case of it, which no value reaches there; two that are neither would mean one thing in one order and another in the other, and the match is refused. A case no row reaches is entered all the same and closed by the contradiction the facts in scope prove there, as an omitted bound at `False` is proved ([`Compiler::unreached`]); where they prove none it is a value no arm matches, reported as its pattern. A row no leaf takes is an arm no value reaches.
//!
//! **A row with guards takes nothing for itself** ([`Compiler::guarded`]). Where it is the first candidate, its guards run, and where one fails the candidates after it are what the value takes. A row whose one guard is a `Bool` is a `Bool` split at its leaf, its own body the true case and the candidates after it the false one, so they are read under the condition's failure; any other row with guards fails in more ways than one, so the candidates after it are read once, at the leaf, and stand wherever one of its guards fails.
//!
//! **An arm of several alternatives is a row for each**, typed apart as arms of their own are, so two of them that reach one leaf are refused, whichever is a case of the other: an arm's alternatives match no value in common, and no order among them says which typing a value takes.

use {
    super::{
        BoolSplit, Expectation, Mode, NatSplit, NominalSplit, Scrutinee, Sequence, SequenceSplit,
        SwitchSplit, absurd, elaborate, payload_fills, read_fields, realize, record_keyword,
        reference,
    },
    crate::{
        Context, Entailed, Error, HeldFrames, PatternShape, Refusal, Resolved, WrittenLiteral,
        check, entail, expect_as, pending, reduce_with, resolved_for_display, stuck_on_metavar,
    },
    curios_common::{Plicity, SyntaxRegistry, recurse},
    curios_core::{
        Arm, Atom, Elements, Ending, Free, Global, Guard, InductArm, Intrinsic, IntrinsicHead,
        Keyed, Keys, Label, Matrix, Mention, Nat, NumLit, Numeral, Pattern, Row, Slot, StructType,
        Subterm, Telescope, Term, Tuple, TupleType, WrittenPlace,
    },
    curios_num::{Integer, Natural},
    curios_surface::written_pattern,
    std::{
        collections::{BTreeMap, BTreeSet},
        convert,
        rc::Rc,
        sync::OnceLock,
    },
};

/// Compile `matrix`, which `term` holds, into the tree of matches it comes to, each elaborated as it is built.
pub fn elaborate_matrix(
    context: &mut Context,
    matrix: &Matrix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let written = matrix.rows().collect::<Vec<_>>();
    let rows = written.len();
    let compiler = Compiler {
        matrix,
        term,
        labels: written.iter().copied().map(labels).collect(),
        arm_of: matrix
            .arms
            .iter()
            .enumerate()
            .flat_map(|(arm, written)| std::iter::repeat_n(arm, written.alternatives.len()))
            .collect(),
        rows: written,
        mode: mode.clone(),
        held: context.hold_frames(),
        fallthrough: None,
        outermost: None,
        strays: vec![false; rows],
        walks: vec![None; rows],
        taken: vec![false; rows],
        covers: vec![None; rows],
        seen: vec![Seen::Any],
        texts: BTreeSet::new(),
        syntax: context.syntax(),
    };

    compiler.whole(context, mode)
}

/// What a row writes at one column beside the place of the first binder it writes there among its row's, or nothing where the compiler put the column under a pattern another row wrote, which the row reads as a binder that binds nothing.
type Cell<'m> = Option<(Shape<'m>, usize)>;

/// What a row writes at a column: a pattern, what is left of a run once a split has read elements off its front, which is a run of its own, or what is left of a successor's step once splits have peeled successors off its value, which is a successor of its own.
#[derive(Clone, Copy)]
enum Shape<'m> {
    Pattern(&'m Pattern),
    /// The elements of a run after the ones read, and how it ends, over the sequence it is a run of.
    Remains(Sequence, &'m [Pattern], Ending),
    /// What a string pattern spells past the bytes read: a closed run of those bytes.
    Text(&'m [u8]),
    /// A successor pattern or a literal with this many successors of its value peeled by the splits around it: what is left of a successor's step, always short of the whole step since one peeled to its step is its predecessor's binder, or a literal's value less them ([`Shape::peel`]).
    Peeled(&'m Pattern, usize),
}

impl<'m> Shape<'m> {
    /// What a successor pattern or a literal past `0` leaves past one more successor of its value: what is left of a successor's step, or its predecessor's binder once its step is spent, and a literal one less.
    fn peel(self) -> Shape<'m> {
        let (pattern, peeled) = match self {
            Shape::Pattern(pattern) => (pattern, 1),
            Shape::Peeled(pattern, peeled) => (pattern, peeled + 1),
            Shape::Remains(..) | Shape::Text(_) => unreachable!("only a number is peeled"),
        };

        match pattern {
            Pattern::Succ { predecessor, step } if Natural::from(peeled) == *step => {
                Shape::Pattern(slot_pattern(*predecessor))
            }
            _ => Shape::Peeled(pattern, peeled),
        }
    }

    /// The value of a literal at its column, where this is one: the value it is written as, less what the splits around it peeled.
    fn value(self) -> Option<Natural> {
        let (pattern, peeled) = match self {
            Shape::Pattern(pattern) => (pattern, 0usize),
            Shape::Peeled(pattern, peeled) => (pattern, peeled),
            Shape::Remains(..) | Shape::Text(_) => return None,
        };
        // A column reads a value only once its rows have been realized at its type, a signed numeral being no natural's ([`Compiler::numbers`]), so a numeral's value is the natural it spells and a character's its code point.
        let written = match pattern {
            Pattern::Literal(NumLit::Number(Numeral::Natural(natural))) => natural.clone(),
            Pattern::Literal(NumLit::Character(character)) => Natural::from(*character as u32),
            _ => return None,
        };

        Some(written.monus(&Natural::from(peeled)))
    }

    /// Whether this is what a split peeling successors leaves of a literal: no part of the value is the row's to bind, so the successors peeled off it are the compiler's, and the row states no case by them, as its own walk, which reads the literal by its value, states none ([`Compiler::alone`]).
    fn is_peeled_literal(self) -> bool {
        matches!(self, Shape::Peeled(pattern, _) if !matches!(pattern, Pattern::Succ { .. }))
    }

    /// What is left of a successor pattern's step at its column, where this is one: its step, less what the splits around it peeled.
    fn step_left(self) -> Option<Natural> {
        match self {
            Shape::Pattern(Pattern::Succ { step, .. }) => Some(step.clone()),
            Shape::Peeled(Pattern::Succ { step, .. }, peeled) => {
                Some(step.monus(&Natural::from(peeled)))
            }
            _ => None,
        }
    }

    /// What a run leaves past an element: its elements after it and its end, or its rest itself where no element is left of an open one.
    fn after(sequence: Sequence, then: &'m [Pattern], ending: Ending) -> Shape<'m> {
        match (then, ending) {
            ([], Ending::Open(rest)) => Shape::Pattern(slot_pattern(rest)),
            _ => Shape::Remains(sequence, then, ending),
        }
    }

    /// The run this is, where it is one: its sequence, and its first element beside what follows it, or nothing where it is empty.
    fn run(self) -> Option<(Sequence, Option<(&'m Pattern, Shape<'m>)>)> {
        let split = |sequence, elements: &'m Elements| {
            elements
                .split()
                .map(|(first, then, ending)| (first, Shape::after(sequence, then, ending)))
        };

        // A string is the closed run of its bytes, each a byte's literal.
        let text = |bytes: &'m [u8]| {
            bytes
                .split_first()
                .map(|(first, then)| (byte_pattern(*first), Shape::Text(then)))
        };

        match self {
            Shape::Pattern(Pattern::List(elements)) => {
                Some((Sequence::List, split(Sequence::List, elements)))
            }
            Shape::Pattern(Pattern::Bytes(elements)) => {
                Some((Sequence::Bytes, split(Sequence::Bytes, elements)))
            }
            Shape::Pattern(Pattern::Str(value)) => Some((Sequence::Bytes, text(value.as_bytes()))),
            Shape::Text(bytes) => Some((Sequence::Bytes, text(bytes))),
            Shape::Pattern(_) | Shape::Peeled(..) => None,
            Shape::Remains(sequence, elements, ending) => Some((
                sequence,
                elements
                    .split_first()
                    .map(|(first, then)| (first, Shape::after(sequence, then, ending))),
            )),
        }
    }

    /// The pattern this is, where it is no run's remainder: a successor peeled short of its step is the successor it was written as.
    fn pattern(self) -> Option<&'m Pattern> {
        match self {
            Shape::Pattern(pattern) | Shape::Peeled(pattern, _) => Some(pattern),
            Shape::Remains(..) | Shape::Text(_) => None,
        }
    }
}

/// The pattern of a byte's literal `byte`, as a string pattern's run reads each of its bytes.
fn byte_pattern(byte: u8) -> &'static Pattern {
    static BYTES: OnceLock<Vec<Pattern>> = OnceLock::new();

    &BYTES.get_or_init(|| {
        (0..=u8::MAX)
            .map(|byte| {
                Pattern::Literal(NumLit::Number(Numeral::Natural(Natural::from(u32::from(
                    byte,
                )))))
            })
            .collect()
    })[usize::from(byte)]
}

/// What a literal pattern or a successor writes, which says the carriers it takes apart ([`Error::MatrixLiteralColumn`]).
fn written_literal(pattern: &Pattern) -> WrittenLiteral {
    match pattern {
        Pattern::Literal(NumLit::Number(Numeral::Integer(_))) => WrittenLiteral::Signed,
        Pattern::Literal(NumLit::Character(_)) => WrittenLiteral::Character,
        Pattern::Succ { .. } => WrittenLiteral::Successor,
        _ => WrittenLiteral::Numeral,
    }
}

/// The pattern of the truth value `value`, which a numeral realized at a `Bool` is read as.
fn truth_pattern(value: bool) -> &'static Pattern {
    const FALSE: &Pattern = &Pattern::Bool(false);
    const TRUE: &Pattern = &Pattern::Bool(true);

    match value {
        false => FALSE,
        true => TRUE,
    }
}

/// A key a dispatch reads off its rows: a natural's, which a byte's and a character's are read as, or an integer's. One statement of a dispatch serves every carrier ([`Compiler::dispatch`]), each saying how its keys are read, written and typed, and which value no row writes.
trait DispatchKey: Ord + Clone + Sized {
    /// The carrier the scrutinee is held to.
    const CARRIER: IntrinsicHead;

    /// The key `shape` writes, where it writes one of this carrier's.
    fn of(shape: Shape<'_>) -> Option<Self>;

    /// The key as its first row writes it, which a report names its case by.
    fn spelled(&self, shape: Shape<'_>) -> String;

    /// The key as the literal its arm is typed at.
    fn literal(&self) -> Term;

    /// The switch's keys over `keyed`.
    fn keys(keyed: Keyed<Self>) -> Keys;

    /// The value no row writes where no row writes a binder, `written` saying which keys are written: the one a report names as a value no arm matches.
    fn missing(context: &Context, scrutinee: &Scrutinee, written: impl Fn(&Self) -> bool) -> Seen;

    /// The scrutinee as the dispatch reads it.
    fn viewed(context: &Context, scrutinee: Scrutinee) -> Scrutinee;
}

impl DispatchKey for Natural {
    const CARRIER: IntrinsicHead = IntrinsicHead::Nat;

    fn of(shape: Shape<'_>) -> Option<Self> {
        shape.value()
    }

    fn spelled(&self, shape: Shape<'_>) -> String {
        match shape {
            Shape::Pattern(Pattern::Literal(NumLit::Character(character))) => {
                format!("{character:?}")
            }
            _ => self.to_string(),
        }
    }

    fn literal(&self) -> Term {
        Term::intrinsic(Intrinsic::Nat(Nat::new(self.clone())))
    }

    fn keys(keyed: Keyed<Self>) -> Keys {
        Keys::Nat(keyed)
    }

    // No set of literals covers every natural: the least one no row writes is a value no arm matches; and a character no row writes is any of them.
    fn missing(context: &Context, scrutinee: &Scrutinee, written: impl Fn(&Self) -> bool) -> Seen {
        match scrutinee.is_char(context) {
            true => Seen::Any,
            false => {
                let least = (0u32..)
                    .map(Natural::from)
                    .find(|literal| !written(literal))
                    .expect("a match writes finitely many literals");

                Seen::Leaf(least.to_string())
            }
        }
    }

    fn viewed(context: &Context, scrutinee: Scrutinee) -> Scrutinee {
        dispatched(context, scrutinee)
    }
}

impl DispatchKey for u8 {
    const CARRIER: IntrinsicHead = IntrinsicHead::Byte;

    fn of(shape: Shape<'_>) -> Option<Self> {
        u8::try_from(&shape.value()?).ok()
    }

    fn spelled(&self, shape: Shape<'_>) -> String {
        Natural::from(u32::from(*self)).spelled(shape)
    }

    fn literal(&self) -> Term {
        Term::intrinsic(Intrinsic::Byte(*self))
    }

    fn keys(keyed: Keyed<Self>) -> Keys {
        Keys::Byte(keyed)
    }

    // A byte's other values are taken at a binder, written `_` where every byte has an arm.
    fn missing(_: &Context, _: &Scrutinee, written: impl Fn(&Self) -> bool) -> Seen {
        (0..=u8::MAX)
            .find(|byte| !written(byte))
            .map_or(Seen::Any, |least| Seen::Leaf(least.to_string()))
    }

    fn viewed(_: &Context, scrutinee: Scrutinee) -> Scrutinee {
        scrutinee
    }
}

impl DispatchKey for Integer {
    const CARRIER: IntrinsicHead = IntrinsicHead::Int;

    fn of(shape: Shape<'_>) -> Option<Self> {
        match shape.pattern()? {
            Pattern::Literal(NumLit::Number(Numeral::Integer(integer))) => Some(integer.clone()),
            _ => None,
        }
    }

    fn spelled(&self, _: Shape<'_>) -> String {
        format!("{self:+}")
    }

    fn literal(&self) -> Term {
        Term::intrinsic(Intrinsic::Int(self.clone()))
    }

    fn keys(keyed: Keyed<Self>) -> Keys {
        Keys::Int(keyed)
    }

    // The key of least magnitude no row writes, the nonnegative first.
    fn missing(_: &Context, _: &Scrutinee, written: impl Fn(&Self) -> bool) -> Seen {
        let least = (0u32..)
            .flat_map(|magnitude| [Integer::from(magnitude), -Integer::from(magnitude)])
            .find(|key| !written(key))
            .expect("a match writes finitely many keys");

        Seen::Leaf(format!("{least:+}"))
    }

    fn viewed(_: &Context, scrutinee: Scrutinee) -> Scrutinee {
        scrutinee
    }
}

/// Whether the cells a row writes for a case's parts are what a split left of a literal, which states no case of the row's ([`Shape::is_peeled_literal`]).
fn states_no_case(cells: &[Cell<'_>]) -> bool {
    cells
        .iter()
        .flatten()
        .any(|(shape, _)| shape.is_peeled_literal())
}

/// What `shape` takes apart, as a report on a column whose patterns disagree names it: what is left of a run is a run of its sequence, and what is left of a number a number.
fn said_shape(shape: Shape<'_>) -> PatternShape {
    match shape {
        Shape::Pattern(pattern) | Shape::Peeled(pattern, _) => shape_of(pattern),
        Shape::Remains(Sequence::List, ..) => PatternShape::List,
        Shape::Remains(Sequence::Bytes, ..) => PatternShape::Bytes,
        Shape::Text(_) => PatternShape::Text,
    }
}

/// What `pattern` takes apart, as a report on a column whose patterns disagree names it. A binder and a wildcard take nothing apart, so no column disagrees over one.
fn shape_of(pattern: &Pattern) -> PatternShape {
    match pattern {
        Pattern::Tuple(fields) => PatternShape::Tuple(fields.len()),
        Pattern::Struct { head, .. } => PatternShape::Structure(head.clone()),
        Pattern::Variant { .. } => PatternShape::Constructor,
        Pattern::Bool(_) => PatternShape::Truth,
        Pattern::Succ { .. } | Pattern::Literal(_) => PatternShape::Number,
        Pattern::Str(_) => PatternShape::Text,
        Pattern::List(_) => PatternShape::List,
        Pattern::Bytes(_) => PatternShape::Bytes,
        Pattern::Binder | Pattern::Wildcard(_) => {
            unreachable!("a binder or a wildcard is no pattern a column is split by")
        }
    }
}

/// The pattern a part a case always holds is read as at a column of its own, a predecessor or an open run's rest: a binder, or `_`.
fn slot_pattern(slot: Slot) -> &'static Pattern {
    const BINDER: &Pattern = &Pattern::Binder;
    const WILDCARD: &Pattern = &Pattern::WILDCARD;

    match slot {
        Slot::Binder => BINDER,
        Slot::Wildcard => WILDCARD,
    }
}

/// One column: the term it is, and the place of the value it holds in the pattern the match's scrutinee is being read as ([`Compiler::seen`]).
#[derive(Clone)]
struct Column {
    term: Term,
    seen: usize,
}

/// One row while its columns are compiled.
#[derive(Clone)]
struct Line<'m> {
    /// Which of the matrix's rows it is.
    row: usize,
    /// What it writes at each column left.
    cells: Vec<Cell<'m>>,
    /// What each of the binders the row's pattern writes stands for, once its pattern is read. Its bind guards' binders are bound where those guards run.
    taken: Vec<Option<Term>>,
    /// Whether the row's guards are still to run, where it is the candidate a leaf takes.
    guarded: bool,
    /// The binders bound ahead of the body, each to what it stands for, in the order they were read: the first is outermost.
    lets: Vec<(Free, Term)>,
    /// The binders of the cases the row states, in the order it states them: what the binders of its own walk are replaced by where a leaf holds its instance.
    cases: Vec<Free>,
}

/// What is known, at the place the compiler stands, of one part of the value the match's scrutinee holds: the pattern a value reaching that place matches, part by part.
#[derive(Clone)]
enum Seen {
    /// Nothing: no case of it was read.
    Any,
    /// A carrier's case, as a pattern writes it.
    Leaf(String),
    /// A natural's successor, over the place of its predecessor.
    Successor(usize),
    /// A constructor over its plain payloads.
    Variant(String, Vec<usize>),
    /// A tuple, or a struct under its head, over its fields, each behind what it is written behind: its mark and its label, or nothing.
    Fields(Option<String>, Vec<(String, usize)>),
    /// An empty sequence.
    Empty(Sequence),
    /// A nonempty sequence over its first element and what follows it.
    Cons(Sequence, usize, usize),
}

/// The outermost dispatch, resolved once: what a row's own walk enters its first case by, so the row and the tree are typed against one result.
#[derive(Clone)]
enum Outermost {
    Nominal(Rc<NominalSplit>),
    Bool(Rc<BoolSplit>),
    Switch(Rc<SwitchSplit>),
    Nat(Rc<NatSplit>),
    Sequence(Rc<SequenceSplit>),
}

/// A row elaborated by its own walk: the binders of the cases it states, in the order it states them, and its body under them.
struct Walked {
    cases: Vec<Free>,
    body: Term,
}

struct Compiler<'m> {
    matrix: &'m Matrix,
    /// Every arm's rows, arm after arm, each an alternative of its arm in the order the arm writes them: a row is one of these by its place here.
    rows: Vec<&'m Row>,
    /// Which arm each row is an alternative of.
    arm_of: Vec<usize>,
    /// The match as it is written, which a report of one of its dispatches is placed at.
    term: &'m Term,
    /// Each row's binders, as its body's telescope labels them: the name each is written with, and where the lowering wrote it among its declaration's binders, or `None` for a body built without labels.
    labels: Vec<Vec<Option<Label>>>,
    /// How the match is read where it stands, which a row's own walk reads its outermost case under.
    mode: Mode,
    /// The frames that stand where the match is entered, which a row's own walk stands on.
    held: HeldFrames,
    /// The row that cannot fail, where the matrix ends in one.
    fallthrough: Option<Fallthrough>,
    /// The outermost dispatch, once it is resolved, with the column it is of and the type the match stands at.
    outermost: Option<(Outermost, Term, Term)>,
    /// The rows a binder of their own put under a case another row wrote.
    strays: Vec<bool>,
    /// Each such row as its own walk elaborated it, once a leaf takes it.
    walks: Vec<Option<Rc<Walked>>>,
    /// The rows some leaf takes.
    taken: Vec<bool>,
    /// For a row that reached a leaf another row took, that row: the arm written before it that covers it there.
    covers: Vec<Option<usize>>,
    /// The value the scrutinee holds at the place the compiler stands, as a pattern: the first is the scrutinee's, and a part read apart names the places of its parts.
    seen: Vec<Seen>,
    /// The places that hold a `Str` its bytes were read apart at, which a value prints as the string they spell, or `_` where they spell none yet.
    texts: BTreeSet<usize>,
    /// The registry the match is elaborated under, which a row's pattern is printed under where a report names it ([`Compiler::written`]).
    syntax: SyntaxRegistry,
}

/// The contradiction the facts prove where a match stands, bound once around it for the cases it closes.
struct Contradiction {
    /// What it is bound to.
    binder: Free,
    /// The empty proposition it proves, as it is referenced.
    stated: Term,
    /// That proposition reduced, which a zero-arm match is read at.
    reduced: Term,
    proof: Term,
}

/// The row that cannot fail, while the rows before it are read.
struct Fallthrough {
    /// Which row it is.
    row: usize,
    /// What stands at each leaf that takes it until it is read.
    stand_in: Free,
    /// How many leaves take it.
    reached: usize,
}

impl<'m> Compiler<'m> {
    /// The whole matrix: its rows over its scrutinee, every one of them reached.
    fn whole(mut self, context: &mut Context, mode: Mode) -> Result<(Term, Term), Error> {
        let matrix = self.matrix;
        let rows = self.rows.len();

        // A final row that takes nothing apart and has no guard states no case. A lone one is a binding, read where its one leaf stands, and so is one after rows none of which takes the scrutinee apart: there is one leaf, and the row is read there, after the guards of the rows before it.
        if rows > 1
            && let Some(last) = self.rows.last()
            && refutations(&last.pattern) == 0
            && last.guards.is_empty()
            && (0..rows).any(|row| refutations(&self.row(row).pattern) > 0)
        {
            self.fallthrough = Some(Fallthrough {
                row: rows - 1,
                stand_in: context.fresh(None),
                reached: 0,
            });
        }

        let head = Column {
            term: matrix.head.clone(),
            seen: 0,
        };
        let compiled = match rows {
            // With no row the match is a vacuous elimination, which has no column to read a dispatch off and is the nominal one.
            0 => self.variants(context, vec![head], Vec::new(), true, mode)?,
            _ => {
                let lines = (0..rows).map(|row| self.line(row)).collect();

                self.columns(context, vec![head], lines, true, mode)?
            }
        };

        // An arm is reached by some value: one written after an arm it is a case of is not, and neither is a final `_` after arms that cover every case.
        match (0..rows).find(|row| !self.taken[*row]) {
            Some(row) => Err(Error::MatrixDuplicateRow {
                covered: self.covers[row].map(|by| self.written(by)),
            }
            .at_opt(self.row(row).body.extent())),
            None => Ok(compiled),
        }
    }

    /// The row at `row`, among every arm's rows.
    fn row(&self, row: usize) -> &'m Row {
        self.rows[row]
    }

    /// `error`, raised where the row at `row` was typed, naming the alternative it is where its arm writes more than one.
    fn in_alternative(&self, row: usize, error: Error) -> Error {
        match self.matrix.arms[self.arm_of[row]].alternatives.len() {
            1 => error,
            _ => error.in_alternative(self.written(row)),
        }
    }

    /// The row at `row` with nothing of it read: its whole pattern over the scrutinee.
    fn line(&self, row: usize) -> Line<'m> {
        Line {
            row,
            cells: vec![Some((Shape::Pattern(&self.row(row).pattern), 0))],
            taken: vec![None; self.row(row).pattern.binders()],
            guarded: !self.row(row).guards.is_empty(),
            lets: Vec::new(),
            cases: Vec::new(),
        }
    }

    /// One step: the leftmost column is retired where no row takes it apart, flattened where the rows that do write a tuple or a struct, and otherwise split by what they write. `top` says no dispatch stands above the column: its dispatch is where the row that cannot fail is read, and `mode` is how what the rows come to is read: as the match is at its scrutinee, and against the type of its case beneath a dispatch.
    fn columns(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        if columns.is_empty() {
            return self.leaf(context, lines, mode);
        }
        // A `_` the lowering placed binds the column it stands at, which binds nothing for it: what a lint asks the class of.
        for line in &lines {
            if let Some((Shape::Pattern(Pattern::Wildcard(WrittenPlace(Some(place)))), _)) =
                line.cells[0]
            {
                context.open_column_site(*place, &columns[0].term);
            }
        }

        let written = lines
            .iter()
            .filter_map(|line| line.cells[0].map(|(shape, _)| shape))
            .filter(|shape| {
                !matches!(
                    shape,
                    Shape::Pattern(Pattern::Binder | Pattern::Wildcard(_))
                )
            })
            .collect::<Vec<_>>();
        let Some(first) = written.first().copied() else {
            let column = columns.remove(0);
            for line in &mut lines {
                if let Some((Shape::Pattern(Pattern::Binder), place)) = line.cells.remove(0) {
                    self.named(context, line, place, column.term.clone());
                }
            }

            return recurse(|| self.columns(context, columns, lines, top, mode));
        };

        if let Some((sequence, _)) = first.run() {
            if let Some(other) = written
                .iter()
                .find(|other| !other.run().is_some_and(|(other, _)| other == sequence))
            {
                return Err(Error::matrix_inconsistent_shape(Some((
                    said_shape(first),
                    said_shape(*other),
                ))));
            }

            return self.sequence(context, sequence, columns, lines, top, mode);
        }
        // What is left of a run is a run, so every other shape is a pattern as written.
        let Some(written) = written
            .iter()
            .map(|shape| shape.pattern())
            .collect::<Option<Vec<_>>>()
        else {
            let other = written.iter().find(|shape| shape.pattern().is_none());
            return Err(Error::matrix_inconsistent_shape(
                other.map(|other| (said_shape(first), said_shape(*other))),
            ));
        };
        let first = written[0];

        // The first written pattern that is not `alike` the first, which a refusal names beside it.
        let disagreeing = |alike: fn(&Pattern, &Pattern) -> bool| {
            written.iter().find(|other| !alike(first, other)).copied()
        };
        let apart = |other: &Pattern| {
            Error::matrix_inconsistent_shape(Some((shape_of(first), shape_of(other))))
        };
        match first {
            Pattern::Variant { .. } => {
                if let Some(other) =
                    disagreeing(|_, other| matches!(other, Pattern::Variant { .. }))
                {
                    return Err(apart(other));
                }

                self.variants(context, columns, lines, top, mode)
            }
            Pattern::Tuple(_) => {
                if let Some(other) = disagreeing(
                    |first, other| matches!((first, other), (Pattern::Tuple(first), Pattern::Tuple(other)) if first.len() == other.len()),
                ) {
                    return Err(apart(other));
                }

                self.fields(context, columns, lines, top, mode)
            }
            Pattern::Struct { .. } => {
                if let Some(other) = disagreeing(|first, other| {
                    matches!(
                        (first, other),
                        (
                            Pattern::Struct { structure: first_structure, fields: first, .. },
                            Pattern::Struct { structure: other_structure, fields: other, .. },
                        ) if first_structure == other_structure && first.len() == other.len()
                    )
                }) {
                    return Err(apart(other));
                }

                self.fields(context, columns, lines, top, mode)
            }
            // A numeral is realized at its column's type, so `0` and `1` share a `Bool`'s column with its truth values.
            Pattern::Bool(_) => {
                if let Some(other) =
                    disagreeing(|_, other| matches!(other, Pattern::Bool(_) | Pattern::Literal(_)))
                {
                    return Err(apart(other));
                }

                self.booleans(context, columns, lines, top, mode)
            }
            // A character is a dispatch case spelled by its scalar value, so it shares a column with the numerals.
            Pattern::Succ { .. } | Pattern::Literal(_) => {
                if let Some(other) = disagreeing(|_, other| {
                    matches!(
                        other,
                        Pattern::Succ { .. } | Pattern::Literal(_) | Pattern::Bool(_)
                    )
                }) {
                    return Err(apart(other));
                }

                let truths = written
                    .iter()
                    .any(|other| matches!(other, Pattern::Bool(_)));
                let peels = written
                    .iter()
                    .any(|other| matches!(other, Pattern::Succ { .. }));
                match (truths, peels) {
                    (true, true) => Err(Error::matrix_inconsistent_shape(Some((
                        PatternShape::Truth,
                        PatternShape::Number,
                    )))),
                    (true, false) => self.booleans(context, columns, lines, top, mode),
                    (false, _) => self.numbers(context, columns, lines, top, mode),
                }
            }
            Pattern::List(_) | Pattern::Bytes(_) | Pattern::Str(_) => {
                unreachable!("a run is read above")
            }
            Pattern::Binder | Pattern::Wildcard(_) => {
                unreachable!("a binder or a wildcard is no pattern a column is split by")
            }
        }
    }

    /// The arm of one case: its rows over the columns left, read against the type the case is typed at, and where no row reaches the case, the leaf no arm reaches ([`Compiler::unreached`]). Guarded by [`recurse`]: a row is read a pattern at a time, so the depth is that of the patterns a reader wrote.
    fn case(
        &mut self,
        context: &mut Context,
        columns: Vec<Column>,
        lines: Vec<Line<'m>>,
        expected: Term,
    ) -> Result<Term, Error> {
        let mode = Mode::Check(self.mode.at(expected));
        if lines.is_empty() {
            return self
                .unreached(context, mode, |value, refusal| Error::MatrixUncovered {
                    value,
                    indexed: false,
                    refusal,
                })
                .map(|(arm, _)| arm);
        }
        recurse(|| self.columns(context, columns, lines, false, mode)).map(|(arm, _)| arm)
    }

    /// The rows of a column a dispatch is about to split, with the column read by each row that writes a binder there: its name is bound to the column, and the row is under whichever case of the column the compiler puts it, none of which it states. Which of them are strays is the column's to say once it knows its cases ([`Compiler::strayed`]).
    fn joined(&mut self, context: &mut Context, lines: &mut [Line<'m>], column: &Column) {
        for line in lines {
            match line.cells[0] {
                Some((Shape::Pattern(Pattern::Binder), place)) => {
                    self.named(context, line, place, column.term.clone());
                    line.cells[0] = None;
                }
                Some((Shape::Pattern(Pattern::Wildcard(_)), _)) => line.cells[0] = None,
                _ => {}
            }
        }
    }

    /// How many of the `places` a column puts a joined row under each such row is still reached at: every place but the ones a row before it closes, a row closing the case its pattern takes whole (`closes`) where it has no guard and nothing else it writes can fail. A row a binder of its own puts under one case alone is that case's arm, typed under its facts as a row writing the case's pattern is; one put under several stands on none of them ([An arm is taken by its patterns, then its guards](../../../documentation/design/surface/an-arm-is-taken-by-its-patterns-then-its-guards.md)).
    fn reached<K: Ord>(
        &self,
        lines: &[Line<'m>],
        closes: impl Fn(Shape<'m>) -> Option<K>,
        places: usize,
    ) -> Vec<(usize, usize)> {
        let mut closed = BTreeSet::new();
        let mut reached = Vec::new();
        for line in lines {
            if let Some((shape, _)) = line.cells[0] {
                let unfailing = self.row(line.row).guards.is_empty()
                    && line.cells[1..].iter().all(|cell| match cell {
                        None => true,
                        Some((Shape::Pattern(pattern), _)) => refutations(pattern) == 0,
                        Some(_) => false,
                    });
                if unfailing && let Some(case) = closes(shape) {
                    closed.insert(case);
                }
            } else {
                reached.push((line.row, places.saturating_sub(closed.len())));
            }
        }

        reached
    }

    /// Mark each joined row of [`Compiler::reached`]'s count a stray where it is reached at more than one place.
    fn strayed(&mut self, reached: &[(usize, usize)]) {
        for &(row, places) in reached {
            if places > 1 {
                self.strays[row] = true;
            }
        }
    }

    /// A leaf no row reaches, entered as an arm of its case is: the zero-arm match on the contradiction the facts in scope prove where it stands, the proof an omitted bound at `False` is given ([`crate::entail`]), read at `mode`. Where the facts prove none, `refused` is handed the value the leaf stands for, as a pattern with `_` wherever no case was read, and what the procedure said of the facts.
    ///
    /// Only a match that leaves a case to no arm reaches one, so a match that misses no arm asks nothing of the facts. The arm is written into the match and checked as one a reader wrote is: the certifier's rule for an arm left out stays inversion's.
    fn unreached(
        &mut self,
        context: &mut Context,
        mode: Mode,
        refused: impl FnOnce(String, Box<Refusal>) -> Error,
    ) -> Result<(Term, Term), Error> {
        let value = self.value(0);
        let (bound, reduced) = empty(context)?;

        // The facts a refusal names are terms of the case's own frame, its binders among them, so it records the scope its reader stands in here, which a report writes them in.
        match entail(context, &bound, &reduced)? {
            Entailed::Proved(proof) => absurd(context, proof, reduced, mode, self.term),
            Entailed::Refused(refusal) => {
                Err(refused(value, Box::new(refusal)).in_scope(context.reader_scope()))
            }
        }
    }

    /// The bytes the run at `place` holds, where every one of them and its end were read.
    fn spelled_bytes(&self, place: usize) -> Option<Vec<u8>> {
        let mut bytes = Vec::new();
        let mut at = place;
        loop {
            match &self.seen[at] {
                Seen::Empty(_) => return Some(bytes),
                Seen::Cons(_, head, tail) => {
                    let Seen::Leaf(byte) = &self.seen[*head] else {
                        return None;
                    };
                    bytes.push(byte.parse().ok()?);
                    at = *tail;
                }
                _ => return None,
            }
        }
    }

    /// The refusal of the row at `row`, which writes `literal` at a column whose type, the scrutinee's, takes no literal of its kind.
    fn literal_refused(
        &self,
        context: &mut Context,
        scrutinee: &Scrutinee,
        row: usize,
        literal: WrittenLiteral,
        instead: Option<String>,
    ) -> Error {
        let pattern = self.written(row);

        pending(context, [scrutinee.type_()], move |reporting, [type_]| {
            Error::MatrixLiteralColumn {
                pattern,
                literal,
                scrutinee: Box::new(resolved_for_display(reporting, &type_)),
                instead,
            }
        })
        .at_opt(self.row(row).body.extent())
    }

    /// The value at `place` as a pattern.
    fn value(&self, place: usize) -> String {
        let fields = |fields: &[(String, usize)]| {
            fields
                .iter()
                .map(|(written, field)| format!("{written}{}", self.value(*field)))
                .collect::<Vec<_>>()
                .join(", ")
        };

        if self.texts.contains(&place) {
            return self
                .spelled_bytes(place)
                .and_then(|bytes| String::from_utf8(bytes).ok())
                .map_or_else(|| "_".to_string(), |text| format!("{text:?}"));
        }

        match &self.seen[place] {
            Seen::Any => "_".to_string(),
            Seen::Leaf(leaf) => leaf.clone(),
            // A run of successors is written as the number it reaches, `2` and not `0 + 1 + 1`, over a predecessor that was read, and as `_ + 2` over one that was not.
            Seen::Successor(predecessor) => {
                let mut steps = 1usize;
                let mut at = *predecessor;
                while let Seen::Successor(next) = &self.seen[at] {
                    steps += 1;
                    at = *next;
                }
                let past = self.value(at);

                match Natural::parse_bytes(past.as_bytes(), 10) {
                    Some(value) => (value + Natural::from(steps)).to_string(),
                    None => format!("{past} + {steps}"),
                }
            }
            Seen::Variant(tag, payloads) => {
                let payloads = payloads
                    .iter()
                    .map(|payload| self.value(*payload))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{tag}({payloads})")
            }
            Seen::Fields(None, written) => format!("({})", fields(written)),
            Seen::Fields(Some(head), written) => format!("{head} {{ {} }}", fields(written)),
            Seen::Empty(sequence) => format!("{}]", opening(*sequence)),
            Seen::Cons(sequence, head, tail) => {
                // A run is written flat, as the literal that builds the value is: its tail's elements follow its head, and an open end is `.._`.
                let mut elements = vec![self.value(*head)];
                let mut tail = *tail;
                let end = loop {
                    match &self.seen[tail] {
                        Seen::Cons(_, head, next) => {
                            elements.push(self.value(*head));
                            tail = *next;
                        }
                        Seen::Empty(_) => break None,
                        _ => break Some(self.value(tail)),
                    }
                };
                if let Some(rest) = end {
                    elements.push(format!("..{rest}"));
                }

                format!("{}{}]", opening(*sequence), elements.join(", "))
            }
        }
    }

    /// A place of the value for each of `count` parts of one being read apart.
    fn parts(&mut self, count: usize) -> Vec<usize> {
        let first = self.seen.len();
        self.seen.resize(first + count, Seen::Any);

        (first..first + count).collect()
    }

    /// The pattern of the row at `row` as it is written.
    fn written(&self, row: usize) -> String {
        let mut names = self.labels[row].iter().map(|label| {
            curios_surface::Label::from(label.as_ref().and_then(Label::hint).unwrap_or("_")).into()
        });

        written_pattern(&self.row(row).pattern, &mut names, convert::identity)
            .printed(&self.syntax)
            .to_string()
    }

    /// The rows that reach a place where every column is read: the first written is the arm a value there takes, and it is a case of every other, since nothing else orders two arms that match one value. No two of them are alternatives of one arm, which match no value in common. Where the first has guards, it takes the value only where they pass ([`Compiler::guarded`]).
    fn leaf(
        &mut self,
        context: &mut Context,
        mut lines: Vec<Line<'m>>,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let line = lines.remove(0);
        for other in &lines {
            let (first, later) = (&self.row(line.row).pattern, &self.row(other.row).pattern);
            // Each alternative of an arm is typed under its own cases, so the one a value both match takes would be decided by the order they are written in.
            if self.arm_of[line.row] == self.arm_of[other.row] {
                let (case, general) = match (
                    instance(context, first, later),
                    instance(context, later, first),
                ) {
                    (true, _) => (Some(line.row), other.row),
                    (_, true) => (Some(other.row), line.row),
                    _ => (None, other.row),
                };

                return Err(Error::MatrixAlternativesMeet {
                    value: self.value(0),
                    first: self.written(case.unwrap_or(line.row)),
                    second: self.written(general),
                    contained: case.is_some(),
                }
                .at_opt(self.row(case.unwrap_or(other.row)).body.extent()));
            }
            // An arm written after one it is a case of is no second claim on the value: the first takes it, and the later one is reached by nothing here. Guards order no pattern, so two that meet are refused whether or not the first is guarded.
            if !instance(context, first, later) && !instance(context, later, first) {
                return Err(Error::MatrixArmsMeet {
                    value: self.value(0),
                    first: self.written(line.row),
                    second: self.written(other.row),
                }
                .at_opt(self.row(other.row).body.extent()));
            }
            // A guard orders no pattern: a guarded arm before one that is a case of it would hand the case on where its guards fail, where with no guard the case would be reached by nothing — so it is held to the order an arm with no guard is.
            if line.guarded && !instance(context, first, later) {
                return Err(Error::MatrixGuardedBeforeCase {
                    general: self.written(line.row),
                    case: self.written(other.row),
                }
                .at_opt(self.row(other.row).body.extent()));
            }
            if !line.guarded {
                self.covers[other.row].get_or_insert(line.row);
            }
        }
        self.taken[line.row] = true;

        if line.guarded {
            return self.guarded(context, line, lines, mode);
        }
        if self
            .fallthrough
            .as_ref()
            .is_some_and(|fallthrough| fallthrough.row == line.row)
        {
            if self.strays[line.row] {
                return self.fallen(context, mode);
            }
            // Reached at one case alone, it is that case's arm, read here under the case's facts.
            let row = line.row;
            let body = self.unfailing(context, row)?;

            return elaborate(context, &body, mode)
                .map_err(|error| self.in_alternative(row, error));
        }
        if !self.strays[line.row] {
            let row = line.row;
            self.watch_lone(context, row);
            let leaf = self.body_of(context, line);

            return elaborate(context, &leaf, mode)
                .map_err(|error| self.in_alternative(row, error));
        }

        let Mode::Check(expected) = mode else {
            unreachable!("a row is put under a case beneath a dispatch");
        };

        Ok((self.walked(context, &line)?, expected.type_))
    }

    /// Watch the row at `row` where it is its match's one arm, written as one alternative with no guard and taking something apart: the frames that stand at its leaf, above the match's own, are the cases it states, which the `unused-case` lint asks whether its body read ([`Context::watch_lone_arm`]). A match written nowhere, or a row whose binders the lowering did not write, is no written arm a lint can name.
    fn watch_lone(&self, context: &mut Context, row: usize) {
        let written = self.row(row);
        if self.rows.len() != 1 || !written.guards.is_empty() || refutations(&written.pattern) == 0
        {
            return;
        }
        let Some(at) = self.matrix.head.extent() else {
            return;
        };
        let Some(places) = self.labels[row]
            .iter()
            .map(|label| label.as_ref().and_then(Label::written))
            .collect::<Option<Vec<_>>>()
        else {
            return;
        };

        context.watch_lone_arm(&self.held, at, places);
    }

    /// The first candidate at a leaf when it has guards to run, and `rest`, the candidates after it: what a value there takes where one of its guards fails.
    ///
    /// A row whose one guard is a `Bool`, typed at this leaf, is a `Bool` split on that condition: its body the true case and `rest` the false one, so each row of `rest` typed at this leaf is read where the condition is false. Any other fails in more ways than one: `rest` is read once, here, and the row is read as the function of it ([`Compiler::failing`]), applied where it stands — `rest` in place where the row has one guard, and a function of no argument, called at each failure, where it has more. A row typed once by its own walk is read so too, its walk the function. Where no candidate follows the row, what its guards refuse reaches no arm ([`Compiler::failed`]).
    fn guarded(
        &mut self,
        context: &mut Context,
        mut line: Line<'m>,
        rest: Vec<Line<'m>>,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let row = line.row;
        if let [Guard::Holds(condition)] = self.row(row).guards.as_slice()
            && !self.strays[row]
        {
            let condition = Self::opened_at(condition, &line);
            line.guarded = false;
            let scrutinee = Scrutinee::read(context, &condition)?;
            let goal = Term::hole(context.mint_metavar());

            return context.awaiting(|context| {
                let split = Rc::new(BoolSplit::resolve(context, scrutinee, &goal, &mode)?);
                let false_case = split.arm(context, false, |context, expected| {
                    let mode = Mode::Check(self.mode.at(expected));
                    self.failed(context, row, rest, mode).map(|(arm, _)| arm)
                })?;
                let true_case = split.arm(context, true, |context, expected| {
                    let mode = Mode::Check(self.mode.at(expected));
                    recurse(|| self.leaf(context, vec![line], mode)).map(|(arm, _)| arm)
                })?;

                Ok(split.rebuild(false_case, true_case))
            });
        }

        let (rest, type_) = self.failed(context, row, rest, mode)?;
        let function = match self.strays[row] {
            true => self.walked(context, &line)?,
            false => {
                let function = self.failing(context, line);
                let expected = failing_type(context, &type_);
                let (function, _) =
                    elaborate(context, &function, Mode::Check(Expectation::own(expected)))
                        .map_err(|error| self.in_alternative(row, error))?;

                function
            }
        };
        let Subterm::Func(function) = &*function else {
            unreachable!("a row with guards is read as the function of what its failures take");
        };
        let taken = match self.row(row).guards.len() {
            1 => function.telescope.open(&[&rest]),
            _ => {
                let bound = context.fresh(None);
                let call = Term::apply(Term::free_var(&bound), Vec::<Term>::new());
                let thunk_type = Term::func_type(Vec::<(Free, Term)>::new(), type_.clone());
                let thunk = Term::func(Vec::<(Free, Term)>::new(), rest);

                Term::let_block(
                    vec![(bound, thunk_type, thunk)],
                    function.telescope.open(&[&call]),
                )
            }
        };

        Ok((taken, type_))
    }

    /// What a value takes where the guards of the row at `row` fail: `rest`, the candidates after it, and where there are none, the leaf no arm reaches ([`Compiler::unreached`]), whose refusal names the row.
    fn failed(
        &mut self,
        context: &mut Context,
        row: usize,
        rest: Vec<Line<'m>>,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        if !rest.is_empty() {
            return recurse(|| self.leaf(context, rest, mode));
        }
        let guarded = self.written(row);
        let at = self.row(row).body.extent();

        self.unreached(context, mode, |value, refusal| {
            Error::MatrixGuarded {
                value,
                guarded,
                refusal,
            }
            .at_opt(at)
        })
    }

    /// The instance a leaf holds of a row its own walk elaborated: the walk's body with the leaf's binders in the places of the walk's.
    fn walked(&mut self, context: &mut Context, line: &Line<'m>) -> Result<Term, Error> {
        let walked = match &self.walks[line.row] {
            Some(walked) => Rc::clone(walked),
            None => {
                let walked = Rc::new(
                    self.walk(context, line.row)
                        .map_err(|error| self.in_alternative(line.row, error))?,
                );
                self.walks[line.row] = Some(Rc::clone(&walked));

                walked
            }
        };
        debug_assert_eq!(walked.cases.len(), line.cases.len());
        let renamed = walked
            .cases
            .iter()
            .copied()
            .zip(line.cases.iter().map(Term::free_var))
            .collect::<Vec<_>>();

        Ok(walked.body.substitute(&renamed))
    }

    /// The stand-in for the row that cannot fail, which is read once the rows before it are ([`Compiler::settled`]). The match's own type, which that row is read at, is held to the type of the case here, where the cases above are in view.
    fn fallen(&mut self, context: &mut Context, mode: Mode) -> Result<(Term, Term), Error> {
        let fallthrough = self
            .fallthrough
            .as_mut()
            .expect("the row that cannot fail is the one a leaf took");
        let Mode::Check(expected) = mode else {
            unreachable!("the row that cannot fail is reached beneath a dispatch");
        };
        fallthrough.reached += 1;
        let stand_in = Term::free_var(&fallthrough.stand_in);
        let (_, _, stands) = self
            .outermost
            .as_ref()
            .expect("a case is entered beneath the outermost dispatch");
        let stands = stands.clone();
        expect_as(context, self.term, &stands, &expected)?;

        Ok((stand_in, expected.type_))
    }

    /// Note the outermost dispatch where it is resolved, with the column it is of and the type the match stands at.
    fn resolved(&mut self, top: bool, split: impl FnOnce() -> (Outermost, Term, Term)) {
        if top {
            self.outermost = Some(split());
        }
    }

    /// The match a dispatch rebuilt, at the type `type_` it stands at. Beneath another dispatch, and where the matrix ends in no row that cannot fail, it is the match as rebuilt. The outermost one takes that row: it states no case, so it is typed where the match stands and against the match's own type, once, after the rows before it. It is written in place where one leaf takes it; where more do, it is bound once as a function of no argument and called from each, so it is written once and evaluated only where a value falls through to it.
    fn settled(
        &mut self,
        context: &mut Context,
        top: bool,
        rebuilt: Term,
        type_: Term,
    ) -> Result<(Term, Term), Error> {
        let Some(fallthrough) = self.fallthrough.take_if(|_| top) else {
            return Ok((rebuilt, type_));
        };
        // No leaf takes it, which is refused once every row has been read.
        if fallthrough.reached == 0 {
            return Ok((rebuilt, type_));
        }

        let body = self.unfailing(context, fallthrough.row)?;
        let body = check(context, &body, type_.clone())?;
        if fallthrough.reached == 1 {
            return Ok((rebuilt.substitute(&[(fallthrough.stand_in, body)]), type_));
        }

        let bound = context.fresh(None);
        let call = Term::apply(Term::free_var(&bound), Vec::<Term>::new());
        let thunk_type = Term::func_type(Vec::<(Free, Term)>::new(), type_.clone());
        let thunk = Term::func(Vec::<(Free, Term)>::new(), body);
        let rebuilt = Term::let_block(
            vec![(bound, thunk_type, thunk)],
            rebuilt.substitute(&[(fallthrough.stand_in, call)]),
        );

        Ok((rebuilt, type_))
    }

    /// The row that cannot fail as a term over the scrutinee: its body under each name its pattern writes, bound to the part of the scrutinee it stands at.
    fn unfailing(&mut self, context: &mut Context, row: usize) -> Result<Term, Error> {
        let head = &self.matrix.head;
        let pattern = &self.row(row).pattern;
        let mut line = self.line(row);
        line.cells.clear();
        self.unfailing_parts(context, &mut line, pattern, 0, head.clone())?;

        Ok(self.body_of(context, line))
    }

    /// Bind each name `pattern`, which takes nothing apart, writes to the part of `value` it stands at.
    fn unfailing_parts(
        &mut self,
        context: &mut Context,
        line: &mut Line<'m>,
        pattern: &Pattern,
        mut place: usize,
        value: Term,
    ) -> Result<(), Error> {
        match pattern {
            Pattern::Binder => self.bind(context, line, place, value),
            Pattern::Wildcard(_) => {}
            Pattern::Tuple(_) | Pattern::Struct { .. } => {
                for (index, (read, _, field)) in parts(context, pattern)?.into_iter().enumerate() {
                    let projected = read.of(&value, index);
                    self.unfailing_parts(context, line, field, place, projected)?;
                    place += field.binders();
                }
            }
            _ => unreachable!("the row that cannot fail takes nothing apart"),
        }

        Ok(())
    }

    /// The refusal of the arms' default where its name is a constructor of the family the outermost dispatch takes apart, the scrutinee itself ([`Error::MatrixDefaultNamesAConstructor`]): `| none =>` after `| some(x) =>` binds what the arms leave as `none`, where the case was meant. A literal's dispatch names no constructor, so over one any name stands; and a default over a tuple or a struct binds the whole, so a constructor of a field's family is no case it could mean.
    fn default_named_as_a_constructor(
        &self,
        context: &mut Context,
        split: &NominalSplit,
    ) -> Option<Error> {
        let row = self.fallthrough.as_ref()?.row;
        if !matches!(self.row(row).pattern, Pattern::Binder) {
            return None;
        }
        let label = self.labels[row].first()?.as_ref()?;
        let binder = label.hint()?.to_string();
        let constructor = split.constructor_named(context, &binder)?;
        // The refusal stands on the binder it is about, and speaks for it: no lint says of it again what to write.
        let at = label
            .written()
            .and_then(|ordinal| context.refused_binder(ordinal));

        Some(
            Error::MatrixDefaultNamesAConstructor {
                binder,
                constructor,
            }
            .at_opt(at),
        )
    }

    /// Rows grouped by the constructor they name, each group the arm of its case, over the payload's columns and then the columns left. A row that writes a binder joins every group, and where the family has a constructor no row names that inversion leaves possible, the rows that write a binder are the one arm of them all. The arms are elaborated in the order the family declares its constructors, as a match written one level deep has them.
    fn variants(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        let rest = columns;
        self.joined(context, &mut lines, &column);

        let mut tags: BTreeMap<String, Atom> = BTreeMap::new();
        for line in &lines {
            if let Some((Shape::Pattern(Pattern::Variant { tag, .. }), _)) = line.cells[0] {
                tags.entry(tag.as_string()).or_insert_with(|| tag.clone());
            }
        }
        let named = self.reached(
            &lines,
            |shape| match shape.pattern()? {
                Pattern::Variant { tag, args, .. }
                    if args.iter().all(|(_, arg)| refutations(arg) == 0) =>
                {
                    Some(tag.as_string())
                }
                _ => None,
            },
            tags.len(),
        );
        // Each group in the order its rows are written, which is the order a leaf reads its candidates in.
        let mut groups: BTreeMap<String, Vec<Line<'m>>> =
            tags.keys().map(|tag| (tag.clone(), Vec::new())).collect();
        let mut binders = Vec::new();
        for mut line in lines {
            match line.cells[0] {
                Some((Shape::Pattern(Pattern::Variant { tag, .. }), _)) => groups
                    .get_mut(&tag.as_string())
                    .expect("a group stands for each constructor a row names")
                    .push(line),
                _ => {
                    for group in groups.values_mut() {
                        group.push(line.clone());
                    }
                    // Under the arm of the constructors no row names it reads the columns left alone.
                    line.cells.remove(0);
                    binders.push(line);
                }
            }
        }

        let scrutinee = Scrutinee::read(context, &column.term)?;
        let goal = Term::hole(context.mint_metavar());
        let term = self.term;

        context.awaiting(|context| {
            let vacuous = groups.is_empty() && binders.is_empty();
            let split = Rc::new(NominalSplit::resolve(
                context, scrutinee, &goal, &mode, vacuous,
            )?);
            self.resolved(top, || {
                (
                    Outermost::Nominal(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });
            if top
                && column.seen == 0
                && let Some(refused) = self.default_named_as_a_constructor(context, &split)
            {
                return Err(refused);
            }

            // Whether a constructor no row names is left possible, which the rows that write a binder are the one arm of. With no such row each is asked as its turn comes, as a match written one level deep asks.
            let defaulted = !binders.is_empty() && split.leaves_unnamed(context, tags.values())?;
            let reached = named
                .iter()
                .map(|&(row, places)| (row, places + usize::from(defaulted)))
                .collect::<Vec<_>>();
            self.strayed(&reached);
            // Where a row writes a binder it takes every case no row names, so only the named ones are walked. A case no row names is left out where inversion rules it out, and is otherwise entered and closed by the facts in scope ([`Compiler::unnamed`]): an arm of the match all the same, which the large-elimination guard counts as the certifier does.
            let cases = split.cases(context, tags.values(), !binders.is_empty());
            let mut unnamed = BTreeSet::new();
            for tag in &cases {
                if !groups.contains_key(&tag.as_string()) && !split.ruled_out(context, tag)? {
                    unnamed.insert(tag.as_string());
                }
            }
            let undecided =
                split.admit(context, groups.is_empty() && unnamed.is_empty(), defaulted)?;
            split.declared(context, tags.values())?;

            // The contradiction the facts prove where the match stands, asked at the first case no row names that the facts of its own arm leave standing, and bound once around the match for every such case.
            let mut contradiction = None;
            let mut arms = Vec::with_capacity(cases.len());
            for tag in &cases {
                if let Some(group) = groups.remove(&tag.as_string()) {
                    arms.push(self.variant(context, &split, tag, group, &column, &rest)?);
                } else if unnamed.contains(&tag.as_string()) {
                    arms.push(self.unnamed(context, &split, tag, &column, &mut contradiction)?);
                }
            }

            let default = match defaulted {
                true => Some(split.default(context, |context, expected| {
                    self.case(context, rest, binders, expected)
                })?),
                false => None,
            };
            let (rebuilt, type_) = split.rebuild(context, arms, default, undecided, term)?;
            let rebuilt = match contradiction {
                Some(Contradiction {
                    binder,
                    stated,
                    proof,
                    ..
                }) => Term::let_block(vec![(binder, stated, proof)], rebuilt),
                None => rebuilt,
            };

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// The arm of `tag`'s case over the rows that reach it: the ones that name it, and the ones that write a binder where they do.
    ///
    /// The one row that names a case names a payload it binds by its own binder, as a match written one level deep does; a payload a pattern takes apart, a payload no row writes, and every payload of a case several rows name, is a binder of the compiler's. Each row's patterns meet the payloads by its own marks ([`payload_fills`]), so two rows may write different hidden payloads.
    fn variant(
        &mut self,
        context: &mut Context,
        split: &NominalSplit,
        tag: &Atom,
        group: Vec<Line<'m>>,
        column: &Column,
        rest: &[Column],
    ) -> Result<(Atom, InductArm), Error> {
        let payload = split.payload(context, tag)?;
        let marks = payload.marks();
        let places = self.parts(marks.len());

        let mut written = Vec::with_capacity(group.len());
        for mut line in group {
            let cells = match line.cells.remove(0) {
                Some((Shape::Pattern(Pattern::Variant { at, args, .. }), first)) => {
                    if let Some(constructor) = split.constructor(tag) {
                        context.resolved(*at, Resolved::Declaration(constructor));
                    }
                    Some(payload_cells(tag, &payload, args, first)?)
                }
                _ => None,
            };
            written.push((cells, line));
        }
        let (binders, mut columns, lines) = self.case_parts(context, written, &places);
        columns.extend(rest.iter().cloned());

        self.seen[column.seen] = Seen::Variant(
            tag.as_string(),
            places
                .iter()
                .zip(&marks)
                .filter(|(_, mark)| **mark == Plicity::Explicit)
                .map(|(place, _)| *place)
                .collect(),
        );
        let arm = split.arm(context, tag, payload, &binders, |context, expected| {
            self.case(context, columns, lines, expected)
        });
        self.seen[column.seen] = Seen::Any;

        arm
    }

    /// The arm of `tag`'s case where no row names it and inversion leaves it possible, closed by a contradiction: the one the facts prove where the arm stands ([`Compiler::unreached`]), and where they prove none there, the one they prove where the match stands, held in `contradiction` once it is asked and named in each arm it closes. Its refusal is the arm's, and says whether the family is indexed, which inversion alone excuses a case of.
    ///
    /// **What the scrutinee's own type states is a fact where the match stands.** In its own arm the scrutinee is typed as its case is ([`NominalSplit::arm`]), so the equation of an `Eq` the unifier leaves standing, `g(n) = g(n) + 1`, is stated by no local there, and a proof from it is typed where its hypothesis has that type: bound around the match, as a reader binds it by hand. It is asked only of a case its own arm's facts leave standing, so a match those facts close asks nothing more than it did.
    fn unnamed(
        &mut self,
        context: &mut Context,
        split: &NominalSplit,
        tag: &Atom,
        column: &Column,
        contradiction: &mut Option<Contradiction>,
    ) -> Result<(Atom, InductArm), Error> {
        let indexed = split.indexed();
        // Whether the arm's refusal is the facts' own, the one the scope the match stands in may answer.
        let mut left = false;
        let arm = self.entered(context, split, tag, column, |compiler, context, mode| {
            compiler.unreached(context, mode, |value, refusal| {
                left = true;
                Error::MatrixUncovered {
                    value,
                    indexed,
                    refusal,
                }
            })
        });
        let refused = match arm {
            Err(refused) if left => refused,
            arm => return arm,
        };

        if contradiction.is_none() {
            let (stated, reduced) = empty(context)?;
            if let Entailed::Proved(proof) = entail(context, &stated, &reduced)? {
                *contradiction = Some(Contradiction {
                    binder: context.fresh(None),
                    stated,
                    reduced,
                    proof,
                });
            }
        }
        let Some(bound) = contradiction else {
            return Err(refused);
        };

        context.with_frame(|context| {
            context.assume(&bound.binder, &bound.stated);
            self.entered(context, split, tag, column, |compiler, context, mode| {
                absurd(
                    context,
                    Term::free_var(&bound.binder),
                    bound.reduced.clone(),
                    mode,
                    compiler.term,
                )
            })
        })
    }

    /// The arm of `tag`'s case, which no row names: entered as an arm of it is, each payload bound by a binder of the compiler's, and closed by `close`, which is handed the mode the arm is read at.
    fn entered(
        &mut self,
        context: &mut Context,
        split: &NominalSplit,
        tag: &Atom,
        column: &Column,
        close: impl FnOnce(&mut Self, &mut Context, Mode) -> Result<(Term, Term), Error>,
    ) -> Result<(Atom, InductArm), Error> {
        let payload = split.payload(context, tag)?;
        let marks = payload.marks();
        let places = self.parts(marks.len());
        let binders = places
            .iter()
            .map(|_| context.fresh(None))
            .collect::<Vec<_>>();

        self.seen[column.seen] = Seen::Variant(
            tag.as_string(),
            places
                .iter()
                .zip(&marks)
                .filter(|(_, mark)| **mark == Plicity::Explicit)
                .map(|(place, _)| *place)
                .collect(),
        );
        let arm = split.arm(context, tag, payload, &binders, |context, expected| {
            let mode = Mode::Check(self.mode.at(expected));
            close(self, context, mode).map(|(arm, _)| arm)
        });
        self.seen[column.seen] = Seen::Any;

        arm
    }

    /// The rows that reach a case over its parts, each beside the cells it writes for them — none where it wrote a binder at the column the case is of — as the binders the case is entered under, the columns of the parts left to read, each at its place among `places`, and the rows over those columns and then the ones they had left.
    ///
    /// The one row that writes the case names a part it binds by its own binder, as a match written one level deep does ([`Compiler::alone`]); a part a pattern takes apart, a part no row writes, and every part of a case several rows write, is a binder of the compiler's.
    fn case_parts(
        &mut self,
        context: &mut Context,
        mut group: Vec<(Option<Vec<Cell<'m>>>, Line<'m>)>,
        places: &[usize],
    ) -> (Vec<Free>, Vec<Column>, Vec<Line<'m>>) {
        let column = |binders: &[Free], slot: usize| Column {
            term: Term::free_var(&binders[slot]),
            seen: places[slot],
        };
        let writing = group
            .iter()
            .enumerate()
            .filter(|(_, (cells, _))| cells.is_some())
            .map(|(at, _)| at)
            .collect::<Vec<_>>();
        if let &[at] = writing.as_slice() {
            let (cells, line) = &mut group[at];
            let cells = cells
                .take()
                .expect("the row that writes the case writes its parts");
            let (binders, slots) = self.alone(context, line, cells);
            // A row that wrote a binder at the column reads no part.
            let lines = group
                .into_iter()
                .enumerate()
                .map(|(index, (_, mut line))| {
                    if index != at {
                        line.cells
                            .splice(0..0, std::iter::repeat_n(None, slots.len()));
                    }

                    line
                })
                .collect();
            let columns = slots
                .into_iter()
                .map(|slot| column(&binders, slot))
                .collect();

            return (binders, columns, lines);
        }

        let binders = places
            .iter()
            .map(|_| context.fresh(None))
            .collect::<Vec<_>>();
        let mut read = vec![false; places.len()];
        let mut rows = Vec::with_capacity(group.len());
        for (cells, mut line) in group {
            let cells = match cells {
                Some(cells) => {
                    if !states_no_case(&cells) {
                        line.cases.extend(binders.iter().copied());
                    }

                    cells
                }
                None => vec![None; places.len()],
            };
            for (slot, cell) in cells.iter().enumerate() {
                read[slot] |= cell.is_some();
            }
            rows.push((cells, line));
        }

        // A column for each part some row writes: what no row writes, no row reads.
        let slots = (0..places.len())
            .filter(|slot| read[*slot])
            .collect::<Vec<_>>();
        let lines = rows
            .into_iter()
            .map(|(cells, mut line)| {
                let mut front = slots.iter().map(|slot| cells[*slot]).collect::<Vec<_>>();
                front.append(&mut line.cells);
                line.cells = front;

                line
            })
            .collect();
        let columns = slots
            .into_iter()
            .map(|slot| column(&binders, slot))
            .collect();

        (binders, columns, lines)
    }

    /// A case as `line`, the one row that writes it, reads it, over the `cells` it writes for the case's parts: the binder of each part — the row's own where it writes a name, and one of the compiler's where it writes a pattern or nothing — and the parts whose pattern is left to read, which are the columns the row's cells are put ahead for.
    fn alone(
        &mut self,
        context: &mut Context,
        line: &mut Line<'m>,
        cells: Vec<Cell<'m>>,
    ) -> (Vec<Free>, Vec<usize>) {
        let states = !states_no_case(&cells);
        let mut binders = Vec::with_capacity(cells.len());
        let mut slots = Vec::new();
        let mut front = Vec::new();
        for (slot, cell) in cells.into_iter().enumerate() {
            let binder = match cell {
                Some((Shape::Pattern(Pattern::Binder), place)) => {
                    let name = self.name(context, line, place);
                    line.taken[place] = Some(Term::free_var(&name));

                    name
                }
                // A `_` the lowering placed is read as a column, which binds nothing for it, so its column's type is asked where columns are read ([`Compiler::columns`]).
                Some((Shape::Pattern(Pattern::Wildcard(WrittenPlace(None))), _)) | None => {
                    context.fresh(None)
                }
                Some(cell) => {
                    slots.push(slot);
                    front.push(Some(cell));

                    context.fresh(None)
                }
            };
            binders.push(binder);
        }
        front.append(&mut line.cells);
        line.cells = front;
        if states {
            line.cases.extend(binders.iter().copied());
        }

        (binders, slots)
    }

    /// The numerals the rows write at a `Bool` column as the truth values they realize as, `0` and `1` its `false` and `true`, as a numeral term is realized where a `Bool` is expected ([`realize`]). A column whose type is no `Bool` takes no numeral beside a truth value, and a numeral past `1`, a signed one and a character are refused where their row is written.
    fn truths(
        &self,
        context: &mut Context,
        lines: &mut [Line<'m>],
        column: &Column,
    ) -> Result<(), Error> {
        let literal = |line: &Line<'m>| {
            matches!(
                line.cells[0],
                Some((Shape::Pattern(Pattern::Literal(_)), _))
            )
        };
        if !lines.iter().any(literal) {
            return Ok(());
        }
        // A truth value beside the numerals decides a column of a type nothing has decided yet as a `Bool`, which its split then holds it to.
        let scrutinee = Scrutinee::read(context, &column.term)?;
        if !scrutinee.is_bool() && !scrutinee.is_undecided(context) {
            return Err(Error::matrix_inconsistent_shape(Some((
                PatternShape::Truth,
                PatternShape::Number,
            ))));
        }
        let carrier = Subterm::Intrinsic(Intrinsic::BoolType);
        for line in lines.iter_mut() {
            let Some((Shape::Pattern(pattern @ Pattern::Literal(literal)), place)) = line.cells[0]
            else {
                continue;
            };
            let written = written_literal(pattern);
            match (written, realize(literal, &carrier)) {
                (WrittenLiteral::Signed, _) | (_, Ok(None)) => {
                    return Err(self.literal_refused(context, &scrutinee, line.row, written, None));
                }
                (_, Err(error)) => return Err(error.at_opt(self.row(line.row).body.extent())),
                (_, Ok(Some(Intrinsic::Bool(value)))) => {
                    line.cells[0] = Some((Shape::Pattern(truth_pattern(value)), place));
                }
                (_, Ok(Some(_))) => unreachable!("a literal realized at a `Bool` is a truth value"),
            }
        }

        Ok(())
    }

    /// Rows split by the `Bool` they write, a row that writes a binder under both.
    fn booleans(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        let rest = columns;
        self.joined(context, &mut lines, &column);
        self.truths(context, &mut lines, &column)?;
        let reached = self.reached(
            &lines,
            |shape| match shape.pattern()? {
                Pattern::Bool(value) => Some(*value),
                _ => None,
            },
            2,
        );
        self.strayed(&reached);

        let mut falses = Vec::new();
        let mut trues = Vec::new();
        for mut line in lines {
            match line.cells.remove(0) {
                Some((Shape::Pattern(Pattern::Bool(false)), _)) => falses.push(line),
                Some((Shape::Pattern(Pattern::Bool(true)), _)) => trues.push(line),
                _ => {
                    falses.push(line.clone());
                    trues.push(line);
                }
            }
        }

        let scrutinee = Scrutinee::read(context, &column.term)?;
        let goal = Term::hole(context.mint_metavar());

        context.awaiting(|context| {
            let split = Rc::new(BoolSplit::resolve(context, scrutinee, &goal, &mode)?);
            self.resolved(top, || {
                (
                    Outermost::Bool(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });

            self.seen[column.seen] = Seen::Leaf("false".to_string());
            let false_case = split.arm(context, false, |context, expected| {
                self.case(context, rest.clone(), falses, expected)
            })?;
            self.seen[column.seen] = Seen::Leaf("true".to_string());
            let true_case = split.arm(context, true, |context, expected| {
                self.case(context, rest, trues, expected)
            })?;
            self.seen[column.seen] = Seen::Any;
            let (rebuilt, type_) = split.rebuild(false_case, true_case);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// Rows over a column of numbers: naturals, bytes or integers. A natural's column takes one of two forms the rows that write a case choose: a case split, `0` and `n + 1`, where a row writes a successor or only `0` beside binders, and dispatch otherwise, over the literals written, with the rows that write a binder as what any other value takes, as a byte's column and an integer's always are. A case split takes each literal apart as the successors it is, `0` at its zero case and a literal past it one less at the predecessor, so a column peels as many successors as its largest step and dispatches on the literals past it there. A byte is dispatched on its values, an integer on its signed keys, and a column of any other type takes no literal.
    fn numbers(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        let rest = columns;
        self.joined(context, &mut lines, &column);

        let first = |alike: fn(&Pattern) -> bool| {
            lines
                .iter()
                .find(|line| {
                    line.cells[0].is_some_and(|(shape, _)| shape.pattern().is_some_and(alike))
                })
                .map(|line| line.row)
        };
        let peels = first(|pattern| matches!(pattern, Pattern::Succ { .. }));
        // A literal past `0`, or a character, makes the column a dispatch where no successor makes it a case split.
        let dispatches = first(|pattern| match pattern {
            Pattern::Literal(NumLit::Number(numeral)) => !numeral.magnitude().is_zero(),
            Pattern::Literal(NumLit::Character(_)) => true,
            _ => false,
        });
        let place = |row: usize| self.row(row).body.extent();

        let scrutinee = Scrutinee::read(context, &column.term)?;
        // A literal is realized at its column's type, so a `Bool`'s numerals are its two cases.
        if scrutinee.is_bool() && peels.is_none() {
            let mut columns = rest;
            columns.insert(0, column);

            return self.booleans(context, columns, lines, top, mode);
        }
        // What a column's literals are is its type's: a byte's are its values, a character's its code point, which only a character's literal names, a natural's its values, an integer's the keys written with their sign, and a type that is none of these takes none.
        let byte = scrutinee.is_byte();
        let character = scrutinee.is_char(context);
        let integer = scrutinee.is_int();
        let natural = byte || character || scrutinee.takes_naturals(context);
        let carrier = match () {
            () if byte => Some(Subterm::Intrinsic(Intrinsic::ByteType)),
            () if integer => Some(Subterm::Intrinsic(Intrinsic::IntType)),
            () if natural => Some(Subterm::Intrinsic(Intrinsic::NatType)),
            // A `Bool` column reaches here only where a row peels a successor, which is what is refused.
            () if scrutinee.is_bool() => Some(Subterm::Intrinsic(Intrinsic::BoolType)),
            () => None,
        };
        for line in &lines {
            let Some((shape, _)) = line.cells[0] else {
                continue;
            };
            let Some(pattern) = shape.pattern() else {
                continue;
            };
            let written = written_literal(pattern);
            let taken = match written {
                WrittenLiteral::Numeral => carrier.is_some() && !character && !integer,
                WrittenLiteral::Signed => integer,
                WrittenLiteral::Successor => natural && !character,
                WrittenLiteral::Character => natural,
                WrittenLiteral::String => false,
            };
            if !taken {
                // At an integer's column, the key a reader means is the value with its sign.
                let instead = match pattern {
                    Pattern::Literal(NumLit::Number(numeral)) if integer => {
                        Some(format!("+{}", numeral.magnitude()))
                    }
                    Pattern::Literal(NumLit::Character(character)) if integer => {
                        Some(format!("+{}", u32::from(*character)))
                    }
                    _ => None,
                };
                return Err(self.literal_refused(context, &scrutinee, line.row, written, instead));
            }
            // A literal is realized where it is first read, at the column it is written at; what a split peels off it is the same value less what was peeled.
            if let (Some(carrier), Shape::Pattern(Pattern::Literal(literal))) = (&carrier, shape) {
                realize(literal, carrier).map_err(|error| error.at_opt(place(line.row)))?;
            }
        }
        if integer {
            let mut columns = rest;
            columns.insert(0, column);

            return self.dispatch::<Integer>(context, columns, lines, top, mode, scrutinee);
        }
        if byte && let Some(successor) = peels {
            return Err(Error::MatrixByteSuccessor {
                successor: self.written(successor),
            }
            .at_opt(place(successor)));
        }
        // `0` and a binder are induction's two cases, so `| 0 => … | _ => …` is the term `| 0 => … | _ + 1 => …` is, and takes its successor arm at a successor whose predecessor is unknown, where a dispatch takes an arm only at a literal. A byte has no successor, so its `0` is a literal.
        if peels.is_none() && (byte || character || dispatches.is_some()) {
            let mut columns = rest;
            columns.insert(0, column);

            return match byte {
                true => self.dispatch::<u8>(context, columns, lines, top, mode, scrutinee),
                false => self.dispatch::<Natural>(context, columns, lines, top, mode, scrutinee),
            };
        }

        // A literal of `0` closes the zero case, and a successor the case of every successor where a step of one is left of it; a larger step leaves the successor of `0` to the rows after it, and a literal past `0` closes no case of the split.
        let reached = self.reached(
            &lines,
            |shape| match shape.value() {
                Some(value) => value.is_zero().then_some(false),
                None => shape.step_left().filter(Natural::is_one).map(|_| true),
            },
            2,
        );
        self.strayed(&reached);
        let mut zeros = Vec::new();
        let mut successors = Vec::new();
        for mut line in lines {
            let cell = line.cells.remove(0);
            let value = cell.and_then(|(shape, _)| shape.value());
            match (cell, value) {
                // A literal is the successors it builds over zero: `0` is the zero case,
                (_, Some(value)) if value.is_zero() => zeros.push(line),
                // and one past it the successor case, one less at the predecessor, `7` at `n` being `6` at `n - 1`.
                (Some((shape, place)), Some(_)) => {
                    successors.push((Some(vec![Some((shape.peel(), place))]), line));
                }
                // A successor of a step past one is one of a step less over the predecessor, `p + 2` at `n` being `p + 1` at `n - 1`.
                (Some((shape, place)), None) if shape.step_left().is_some() => {
                    successors.push((Some(vec![Some((shape.peel(), place))]), line));
                }
                _ => {
                    zeros.push(line.clone());
                    successors.push((None, line));
                }
            }
        }
        // A case no row reaches is closed by the facts or refused, a row that writes `0` alone leaving every successor to no arm, the least of them `1`, which its report names.
        let unreached = successors.is_empty();

        let goal = Term::hole(context.mint_metavar());

        context.awaiting(|context| {
            let split = Rc::new(NatSplit::resolve(context, scrutinee, &goal, &mode)?);
            self.resolved(top, || {
                (
                    Outermost::Nat(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });

            self.seen[column.seen] = Seen::Leaf("0".to_string());
            let zero = split.zero(context, |context, expected| {
                self.case(context, rest.clone(), zeros, expected)
            })?;

            let places = self.parts(1);
            let (binders, mut columns, group) = self.case_parts(context, successors, &places);
            columns.extend(rest);
            let predecessor = binders[0];
            self.seen[column.seen] = match unreached {
                true => Seen::Leaf("1".to_string()),
                false => Seen::Successor(places[0]),
            };
            let successor = split.successor(context, &predecessor, |context, expected| {
                self.case(context, columns, group, expected)
            })?;
            self.seen[column.seen] = Seen::Any;
            let (rebuilt, type_) = split.rebuild(zero, &predecessor, successor);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// The case split of a run's column, a list's or a packed sequence's: the rows of its empty case, and of its nonempty one over its first element and what follows it, each a column of its own as a constructor's payload is; a row that writes a binder is under both.
    fn sequence(
        &mut self,
        context: &mut Context,
        sequence: Sequence,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        let rest = columns;
        self.joined(context, &mut lines, &column);
        // A run's cases are its lengths, which no row closes one at a time here.
        let reached = self.reached(&lines, |_| None::<()>, usize::MAX);
        self.strayed(&reached);

        // A string pattern takes a `Str` apart by its bytes, and a column a row writes one at holds strings alone: the row that writes the first, where one does.
        let whole =
            |line: &Line<'m>| matches!(line.cells[0], Some((Shape::Pattern(Pattern::Str(_)), _)));
        let texts = lines.iter().find(|line| whole(line)).map(|line| line.row);
        if texts.is_some()
            && lines
                .iter()
                .any(|line| line.cells[0].is_some() && !whole(line))
        {
            return Err(Error::matrix_inconsistent_shape(None));
        }
        // Read before a case is missed, so a column no string fits is refused as one and a missing case is spelled as the string it is.
        let read = match texts {
            Some(text) => {
                let scrutinee = Scrutinee::read(context, &column.term)?;
                if !scrutinee.is_text(context) && !stuck_on_metavar(context, scrutinee.type_()) {
                    return Err(self.literal_refused(
                        context,
                        &scrutinee,
                        text,
                        WrittenLiteral::String,
                        None,
                    ));
                }
                self.texts.insert(column.seen);

                Some(scrutinee.of_text(context)?)
            }
            None => None,
        };

        let mut empties = Vec::new();
        let mut conses = Vec::new();
        for mut line in lines {
            match line.cells.remove(0) {
                Some((shape, place)) => {
                    let (_, split) = shape
                        .run()
                        .expect("every row that takes the column apart writes a run");
                    match split {
                        None => empties.push(line),
                        Some((first, after)) => {
                            let cells = vec![
                                Some((Shape::Pattern(first), place)),
                                Some((after, place + first.binders())),
                            ];
                            conses.push((Some(cells), line));
                        }
                    }
                }
                None => {
                    empties.push(line.clone());
                    conses.push((None, line));
                }
            }
        }
        let scrutinee = match read {
            Some(scrutinee) => scrutinee,
            None => Scrutinee::read(context, &column.term)?,
        };
        let goal = Term::hole(context.mint_metavar());

        context.awaiting(|context| {
            let split = Rc::new(SequenceSplit::resolve(
                context, sequence, scrutinee, &goal, &mode,
            )?);
            self.resolved(top, || {
                (
                    Outermost::Sequence(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });

            self.seen[column.seen] = Seen::Empty(sequence);
            let empty = split.empty(context, |context, expected| {
                self.case(context, rest.clone(), empties, expected)
            })?;

            let places = self.parts(2);
            let (binders, mut columns, group) = self.case_parts(context, conses, &places);
            columns.extend(rest);
            let binders = [binders[0], binders[1]];
            self.seen[column.seen] = Seen::Cons(sequence, places[0], places[1]);
            let cons = split.cons(context, &binders, |context, expected| {
                self.case(context, columns, group, expected)
            })?;
            self.seen[column.seen] = Seen::Any;
            let (rebuilt, type_) = split.rebuild(empty, &binders, cons);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// A tuple or struct column flattened into one column for each field, a projection of the scrutinee: a value of either has one shape, so matching one is reading its fields, and no row is put under a case by it. The rows that write the shape agree, field by field, on the label each is written with; a row that writes a binder binds its name to the column and reads no field. A struct pattern's head is the structure the column's type is, and a tuple pattern's fields are every field of the tuple it is.
    fn fields(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        let rest = columns;

        let written = |context: &Context, shape: Shape<'m>| match shape {
            Shape::Pattern(pattern @ (Pattern::Tuple(_) | Pattern::Struct { .. })) => {
                parts(context, pattern)
            }
            _ => unreachable!("every row that takes the column apart writes a tuple or a struct"),
        };
        let reads = |parts: &[(Read<'m>, String, &'m Pattern)]| {
            parts.iter().map(|(read, ..)| *read).collect::<Vec<_>>()
        };
        let shape = lines
            .iter()
            .find_map(|line| {
                line.cells[0].filter(|(shape, _)| {
                    !matches!(
                        shape,
                        Shape::Pattern(Pattern::Binder | Pattern::Wildcard(_))
                    )
                })
            })
            .map(|(shape, _)| shape)
            .expect("a column is flattened for a row that writes its shape");
        let head = match shape {
            Shape::Pattern(Pattern::Struct {
                head, structure, ..
            }) => {
                held_to_structure(context, &column.term, head, structure)?;

                Some(head.clone())
            }
            Shape::Pattern(Pattern::Tuple(fields)) => {
                held_to_arity(context, &column.term, fields.len())?;

                None
            }
            _ => None,
        };
        // Every row that takes the column apart reads its fields as this one does, each where the literal would write it.
        let first = written(context, shape)?;
        let canonical = reads(&first);

        let mut flattened = Vec::with_capacity(lines.len());
        for mut line in lines {
            let cells = match line.cells.remove(0) {
                Some((Shape::Pattern(Pattern::Binder), place)) => {
                    self.named(context, &mut line, place, column.term.clone());

                    vec![None; canonical.len()]
                }
                Some((Shape::Pattern(Pattern::Wildcard(_)), _)) => vec![None; canonical.len()],
                Some((shape, mut place)) => {
                    let parts = written(context, shape)?;
                    if reads(&parts) != canonical {
                        return Err(Error::matrix_inconsistent_shape(None));
                    }
                    // What each label names, at the row's own place for it.
                    if let Some(Pattern::Struct {
                        structure: Mention::Declaration(structure),
                        fields,
                        ..
                    }) = shape.pattern()
                    {
                        for (_, label, _) in fields {
                            if let Some(label) = label {
                                context.resolved_label(label, *structure);
                            }
                        }
                    }

                    parts
                        .iter()
                        .map(|(_, _, field)| {
                            let cell = Some((Shape::Pattern(field), place));
                            place += field.binders();

                            cell
                        })
                        .collect()
                }
                None => vec![None; canonical.len()],
            };
            let mut front: Vec<Cell<'m>> = cells;
            front.append(&mut line.cells);
            line.cells = front;
            flattened.push(line);
        }

        let places = self.parts(canonical.len());
        let mut projections = canonical
            .iter()
            .enumerate()
            .map(|(index, read)| Column {
                term: read.of(&column.term, index),
                seen: places[index],
            })
            .collect::<Vec<_>>();
        projections.extend(rest);
        self.seen[column.seen] = Seen::Fields(
            head,
            first
                .into_iter()
                .map(|(_, spelled, _)| spelled)
                .zip(places)
                .collect(),
        );

        let compiled = recurse(|| self.columns(context, projections, flattened, top, mode));
        self.seen[column.seen] = Seen::Any;

        compiled
    }

    /// A row with every column read, as the term it comes to where it stands: its body over what its binders stand for, under the binders bound ahead of it.
    fn body_of(&mut self, context: &mut Context, line: Line<'m>) -> Term {
        let values = line
            .taken
            .iter()
            .map(|value| {
                value
                    .as_ref()
                    .expect("a row's every binder is read ahead of its body")
            })
            .collect::<Vec<_>>();
        let body = self.body(line.row).open(&values);

        Self::bound_ahead(context, line.lets, body)
    }

    /// `body` under what a row binds ahead of it: each name to what it stands for, the first outermost.
    fn bound_ahead(context: &mut Context, lets: Vec<(Free, Term)>, body: Term) -> Term {
        lets.into_iter().rev().fold(body, |tail, (name, value)| {
            let hole = Term::hole(context.mint_metavar());

            Term::let_(&name, hole, value, tail)
        })
    }

    /// A row with guards as the function of what a value takes where one of them fails, `(failed) => …`: each guard a `Bool` match or a match over its value, the row's body past the last, and `failed` wherever one fails — under what the row binds ahead of it, as [`Compiler::body_of`] writes a body. It is checked at [`failing_type`], whose domain gives `failed` its type: the type the leaf answers is elaborated already, and written in again as an annotation it would be elaborated a second time, in the match's module, where a projection reduction left in it may be private (a sealed concept's method, as `Parse(I, A)` reduces to `Input/Admits` of `I`'s witness).
    fn failing(&mut self, context: &mut Context, line: Line<'m>) -> Term {
        let failed = context.fresh(None);
        let mut values = line
            .taken
            .iter()
            .map(|value| {
                value
                    .clone()
                    .expect("a row's every binder is read ahead of its guards")
            })
            .collect::<Vec<_>>();
        let passing = self.passing(context, line.row, 0, &mut values, &Term::free_var(&failed));
        let chain = Self::bound_ahead(context, line.lets, passing);

        Term::func(vec![(failed, Term::hole(context.mint_metavar()))], chain)
    }

    /// The guards of the row at `row` from the one at `index`, with `values` standing for the binders written before it, as the term a value that passed the ones before comes to: the row's body where none is left, and `failed` where this one fails.
    fn passing(
        &mut self,
        context: &mut Context,
        row: usize,
        index: usize,
        values: &mut Vec<Term>,
        failed: &Term,
    ) -> Term {
        let written = self.row(row);
        let Some(guard) = written.guards.get(index) else {
            return opened(&written.body, values);
        };

        match guard {
            Guard::Holds(condition) => {
                let condition = opened(condition, values);
                let passed = self.passing(context, row, index + 1, values, failed);

                Term::bool_match(
                    condition,
                    Term::hole(context.mint_metavar()),
                    failed.clone(),
                    passed,
                )
            }
            Guard::Binds { pattern, value } => {
                let value = opened(value, values);
                let first = values.len();
                let names = (first..first + pattern.binders())
                    .map(|place| context.fresh_written(self.labels[row][place].as_ref()))
                    .collect::<Vec<_>>();
                values.extend(names.iter().map(Term::free_var));
                let passed = self.passing(context, row, index + 1, values, failed);
                values.truncate(first);
                // A value the pattern refuses takes the `_`, and learns nothing of the pattern.

                Term::matrix(Matrix {
                    head: value,
                    arms: vec![
                        Arm {
                            alternatives: vec![Row {
                                pattern: pattern.clone(),
                                guards: Vec::new(),
                                body: Term::func(
                                    names
                                        .into_iter()
                                        .map(|name| (name, Term::hole(context.mint_metavar())))
                                        .collect::<Vec<_>>(),
                                    passed,
                                ),
                            }],
                        },
                        Arm {
                            alternatives: vec![Row {
                                pattern: Pattern::WILDCARD,
                                guards: Vec::new(),
                                body: Term::func(Vec::<(Free, Term)>::new(), failed.clone()),
                            }],
                        },
                    ],
                })
            }
        }
    }

    /// A guard of the row `line` is, a function over the binders its pattern writes, opened where the row stands: each binder in place of what it stands for there.
    fn opened_at(guard: &Term, line: &Line<'m>) -> Term {
        let values = line
            .taken
            .iter()
            .map(|value| {
                value
                    .clone()
                    .expect("a row's every binder is read ahead of its guards")
            })
            .collect::<Vec<_>>();

        opened(guard, &values).substitute(&line.lets)
    }

    /// The row at `row` elaborated by its own walk: its cases entered alone, standing on the frames held where the match is entered.
    fn walk(&mut self, context: &mut Context, row: usize) -> Result<Walked, Error> {
        let held = self.held.clone();
        let line = self.line(row);
        let columns = vec![self.matrix.head.clone()];
        let mode = self.mode.clone();

        context.standing_on(&held, |context| {
            self.alone_over(context, columns, line, true, mode)
        })
    }

    /// How a row alone enters a case it states of `column`, where `top` says it has stated none yet: by the tree's outermost dispatch where that is of this column, and otherwise by a dispatch of its own. A row that wrote a binder at the tree's outermost dispatch states no case of it, so its own first one is read against the type the match stands at. Answers the dispatch to enter by where it is the tree's, how the case is read, and whether it awaits as a dispatch beneath another does.
    fn entering(&self, top: bool, column: &Term, mode: Mode) -> (Option<Outermost>, Mode, bool) {
        if !top {
            return (None, mode, true);
        }

        let (outermost, at, stands) = self
            .outermost
            .as_ref()
            .expect("a row is walked beneath the outermost dispatch");
        match at == column {
            true => (Some(outermost.clone()), mode, false),
            false => (None, Mode::Check(self.mode.at(stands.clone())), true),
        }
    }

    /// One step of a row's own walk: `line` alone over `columns`. A name it writes is bound to its column, a tuple or a struct it writes is flattened, and a case it states is entered by the eliminator's own step, with the columns left as the arm; nothing is rebuilt. `top` says it has stated no case yet. Answers the row's body elaborated where every column is read, with the binders of the cases it stated.
    fn alone_over(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        mut line: Line<'m>,
        top: bool,
        mode: Mode,
    ) -> Result<Walked, Error> {
        if columns.is_empty() {
            // A row that states no case at all is read as the row that cannot fail is: against the type the match stands at.
            let mode = match (top, &self.outermost) {
                (true, Some((_, _, stands))) => Mode::Check(self.mode.at(stands.clone())),
                _ => mode,
            };
            let cases = line.cases.clone();
            // A row with guards is read as the function of what its failures take, which each leaf it reaches supplies.
            if line.guarded {
                let Mode::Check(type_) = mode else {
                    unreachable!("a row is walked against the type its case stands at");
                };
                let function = self.failing(context, line);
                let expected = failing_type(context, &type_.type_);
                let (body, _) =
                    elaborate(context, &function, Mode::Check(Expectation::own(expected)))?;

                return Ok(Walked { cases, body });
            }
            let leaf = self.body_of(context, line);
            let (body, _) = elaborate(context, &leaf, mode)?;

            return Ok(Walked { cases, body });
        }

        let column = columns.remove(0);
        let (shape, place) = line
            .cells
            .remove(0)
            .expect("a row alone writes every column it reads");

        match shape {
            Shape::Pattern(Pattern::Binder) => {
                self.bind(context, &mut line, place, column);

                return recurse(|| self.alone_over(context, columns, line, top, mode));
            }
            Shape::Pattern(Pattern::Wildcard(written)) => {
                if let WrittenPlace(Some(place)) = written {
                    context.open_column_site(*place, &column);
                }

                return recurse(|| self.alone_over(context, columns, line, top, mode));
            }
            Shape::Pattern(pattern @ (Pattern::Tuple(_) | Pattern::Struct { .. })) => {
                let parts = parts(context, pattern)?;
                let mut cells = Vec::with_capacity(parts.len() + line.cells.len());
                let mut projections = Vec::with_capacity(parts.len() + columns.len());
                let mut place = place;
                for (index, (read, _, field)) in parts.into_iter().enumerate() {
                    cells.push(Some((Shape::Pattern(field), place)));
                    place += field.binders();
                    projections.push(read.of(&column, index));
                }
                cells.append(&mut line.cells);
                line.cells = cells;
                projections.extend(columns);

                return recurse(|| self.alone_over(context, projections, line, top, mode));
            }
            _ => {}
        }

        let (shared, mode, awaits) = self.entering(top, &column, mode);

        // What the arm of the row's case comes to: the walk of the columns left, kept apart from the arm the eliminator closes over it.
        let mut walked = None;
        let mut arm = |compiler: &mut Self,
                       context: &mut Context,
                       columns: Vec<Term>,
                       line: Line<'m>,
                       expected: Term| {
            let reached = recurse(|| {
                let mode = Mode::Check(compiler.mode.at(expected));
                compiler.alone_over(context, columns, line, false, mode)
            })?;
            let body = reached.body.clone();
            walked = Some(reached);

            Ok(body)
        };

        if let Some((sequence, run)) = shape.run() {
            let split = match shared {
                Some(Outermost::Sequence(split)) => split,
                Some(_) => unreachable!("a column is split by one shape"),
                None => {
                    let scrutinee = Scrutinee::read(context, &column)?;
                    // A string pattern reads its `Str` through its bytes, as the tree's split does.
                    let scrutinee = match shape {
                        Shape::Pattern(Pattern::Str(_)) => scrutinee.of_text(context)?,
                        _ => scrutinee,
                    };
                    let goal = Term::hole(context.mint_metavar());

                    Rc::new(SequenceSplit::resolve(
                        context, sequence, scrutinee, &goal, &mode,
                    )?)
                }
            };

            match run {
                Some((first, after)) => {
                    let cells = vec![
                        Some((Shape::Pattern(first), place)),
                        Some((after, place + first.binders())),
                    ];
                    let (binders, slots) = self.alone(context, &mut line, cells);
                    let mut parts = slots
                        .into_iter()
                        .map(|slot| Term::free_var(&binders[slot]))
                        .collect::<Vec<_>>();
                    parts.extend(columns);
                    let binders = [binders[0], binders[1]];

                    walk_awaited(context, awaits, |context| {
                        split.cons(context, &binders, |context, expected| {
                            arm(self, context, parts, line, expected)
                        })
                    })?;
                }
                None => {
                    walk_awaited(context, awaits, |context| {
                        split.empty(context, |context, expected| {
                            arm(self, context, columns, line, expected)
                        })
                    })?;
                }
            }

            return Ok(walked.expect("the arm of a row's case is its walk"));
        }
        let Some(pattern) = shape.pattern() else {
            unreachable!("what is left of a run is a run");
        };

        match pattern {
            Pattern::Binder | Pattern::Wildcard(_) | Pattern::Tuple(_) | Pattern::Struct { .. } => {
                unreachable!("a pattern that states no case is read above")
            }
            Pattern::List(_) | Pattern::Bytes(_) | Pattern::Str(_) => {
                unreachable!("a run is read above")
            }
            Pattern::Variant { tag, at, args } => {
                let split = match shared {
                    Some(Outermost::Nominal(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());

                        Rc::new(NominalSplit::resolve(
                            context, scrutinee, &goal, &mode, false,
                        )?)
                    }
                };
                let payload = split.payload(context, tag)?;
                if let Some(constructor) = split.constructor(tag) {
                    context.resolved(*at, Resolved::Declaration(constructor));
                }
                let cells = payload_cells(tag, &payload, args, place)?;
                let (binders, slots) = self.alone(context, &mut line, cells);
                let mut payloads = slots
                    .into_iter()
                    .map(|slot| Term::free_var(&binders[slot]))
                    .collect::<Vec<_>>();
                payloads.extend(columns);

                walk_awaited(context, awaits, |context| {
                    split.arm(context, tag, payload, &binders, |context, expected| {
                        arm(self, context, payloads, line, expected)
                    })
                })?;
            }
            Pattern::Bool(value) => {
                let split = match shared {
                    Some(Outermost::Bool(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());

                        Rc::new(BoolSplit::resolve(context, scrutinee, &goal, &mode)?)
                    }
                };

                walk_awaited(context, awaits, |context| {
                    split.arm(context, *value, |context, expected| {
                        arm(self, context, columns, line, expected)
                    })
                })?;
            }
            Pattern::Literal(_) if Integer::of(shape).is_some() => {
                let key = Integer::of(shape).expect("an integer's key, as read");
                let split = match shared {
                    Some(Outermost::Switch(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());

                        Rc::new(SwitchSplit::resolve(
                            context,
                            scrutinee,
                            IntrinsicHead::Int,
                            &goal,
                            &mode,
                        )?)
                    }
                };

                walk_awaited(context, awaits, |context| {
                    split.arm(context, key.literal(), |context, expected| {
                        arm(self, context, columns, line, expected)
                    })
                })?;
            }
            Pattern::Literal(_) => {
                let value = shape.value().expect("a literal has a value");
                // A numeral at a `Bool` is the truth value it realizes as, which the tree held to `0` or `1` where it read the column ([`Compiler::truths`]).
                let mut truth = |context: &mut Context, split: &BoolSplit, line, columns| {
                    walk_awaited(context, awaits, |context| {
                        split.arm(context, !value.is_zero(), |context, expected| {
                            arm(self, context, columns, line, expected)
                        })
                    })
                };
                let split = match shared {
                    Some(Outermost::Switch(split)) => split,
                    Some(Outermost::Bool(split)) => {
                        truth(context, &split, line, columns)?;

                        return Ok(walked.expect("the arm of a row's case is its walk"));
                    }
                    // The tree peels successors at this column, so a literal is the successors it builds over zero there: `0` the zero case, and one past it the successor case, one less at the predecessor, which states no case of the row's.
                    Some(Outermost::Nat(split)) => {
                        match value.is_zero() {
                            true => {
                                walk_awaited(context, awaits, |context| {
                                    split.zero(context, |context, expected| {
                                        arm(self, context, columns, line, expected)
                                    })
                                })?;
                            }
                            false => {
                                let cells = vec![Some((shape.peel(), place))];
                                let (binders, slots) = self.alone(context, &mut line, cells);
                                let predecessor = binders[0];
                                let mut parts = slots
                                    .into_iter()
                                    .map(|slot| Term::free_var(&binders[slot]))
                                    .collect::<Vec<_>>();
                                parts.extend(columns);

                                walk_awaited(context, awaits, |context| {
                                    split.successor(context, &predecessor, |context, expected| {
                                        arm(self, context, parts, line, expected)
                                    })
                                })?;
                            }
                        }

                        return Ok(walked.expect("the arm of a row's case is its walk"));
                    }
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());
                        if scrutinee.is_bool() {
                            let split = BoolSplit::resolve(context, scrutinee, &goal, &mode)?;
                            truth(context, &split, line, columns)?;

                            return Ok(walked.expect("the arm of a row's case is its walk"));
                        }
                        // A `0` written alone is induction's zero case, as a column whose rows write no literal past it reads it; a byte has no successor, so its `0` is a literal's case.
                        let zero = matches!(
                            shape,
                            Shape::Pattern(Pattern::Literal(NumLit::Number(Numeral::Natural(natural))))
                                if natural.is_zero()
                        );
                        if zero && !scrutinee.is_byte() {
                            let split = NatSplit::resolve(context, scrutinee, &goal, &mode)?;
                            walk_awaited(context, awaits, |context| {
                                split.zero(context, |context, expected| {
                                    arm(self, context, columns, line, expected)
                                })
                            })?;

                            return Ok(walked.expect("the arm of a row's case is its walk"));
                        }

                        let carrier = match scrutinee.is_byte() {
                            true => IntrinsicHead::Byte,
                            false => IntrinsicHead::Nat,
                        };
                        let scrutinee = dispatched(context, scrutinee);

                        Rc::new(SwitchSplit::resolve(
                            context, scrutinee, carrier, &goal, &mode,
                        )?)
                    }
                };

                let literal = split.key(value);
                walk_awaited(context, awaits, |context| {
                    split.arm(context, literal, |context, expected| {
                        arm(self, context, columns, line, expected)
                    })
                })?;
            }
            // What is left of a step past one is read at the predecessor, as a column of its own.
            Pattern::Succ { .. } => {
                let split = match shared {
                    Some(Outermost::Nat(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());

                        Rc::new(NatSplit::resolve(context, scrutinee, &goal, &mode)?)
                    }
                };

                let cells = vec![Some((shape.peel(), place))];
                let (binders, slots) = self.alone(context, &mut line, cells);
                let predecessor = binders[0];
                let mut parts = slots
                    .into_iter()
                    .map(|slot| Term::free_var(&binders[slot]))
                    .collect::<Vec<_>>();
                parts.extend(columns);

                walk_awaited(context, awaits, |context| {
                    split.successor(context, &predecessor, |context, expected| {
                        arm(self, context, parts, line, expected)
                    })
                })?;
            }
        }

        Ok(walked.expect("the arm of a row's case is its walk"))
    }

    /// A dispatch on the literals the rows write at a column, each a key of `K`'s carrier: a case for each key, and what any other value takes, the rows that write a binder there, typed where each key's test failed. Where no row writes a binder, the value no row writes is refused as one no arm matches.
    fn dispatch<K: DispatchKey>(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
        scrutinee: Scrutinee,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        let rest = columns;
        let key = |shape: Shape<'m>| K::of(shape).expect("every row of a dispatch writes a key");
        let mut groups: BTreeMap<K, Vec<Line<'m>>> = lines
            .iter()
            .filter_map(|line| line.cells[0].map(|(shape, _)| (key(shape), Vec::new())))
            .collect();
        // Each case as its first row writes it.
        let mut spelled = BTreeMap::new();
        for line in &lines {
            if let Some((shape, _)) = line.cells[0] {
                let written = key(shape);
                let spelling = written.spelled(shape);
                spelled.entry(written).or_insert(spelling);
            }
        }
        let reached = self.reached(&lines, K::of, groups.len() + 1);
        self.strayed(&reached);
        let mut others = Vec::new();
        for mut line in lines {
            match line.cells.remove(0) {
                Some((shape, _)) => groups
                    .get_mut(&key(shape))
                    .expect("a group stands for each key a row writes")
                    .push(line),
                None => {
                    for group in groups.values_mut() {
                        group.push(line.clone());
                    }
                    others.push(line);
                }
            }
        }
        // Where no row writes a binder, what any other value takes is a leaf no row reaches, which its report names by the value no row writes.
        let unreached = others
            .is_empty()
            .then(|| K::missing(context, &scrutinee, |key| groups.contains_key(key)));

        let scrutinee = K::viewed(context, scrutinee);
        let goal = Term::hole(context.mint_metavar());

        context.awaiting(|context| {
            let split = Rc::new(SwitchSplit::resolve(
                context,
                scrutinee,
                K::CARRIER,
                &goal,
                &mode,
            )?);
            self.resolved(top, || {
                (
                    Outermost::Switch(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });

            let mut cases = Vec::with_capacity(groups.len());
            for (value, group) in groups {
                self.seen[column.seen] = Seen::Leaf(spelled[&value].clone());
                let arm = split.arm(context, value.literal(), |context, expected| {
                    self.case(context, rest.clone(), group, expected)
                })?;
                cases.push((value, arm));
            }
            self.seen[column.seen] = unreached.unwrap_or(Seen::Any);
            let keys = K::keys(
                Keyed::new(cases)
                    .expect("a dispatch's keys are its groups', one at least, ascending"),
            );
            let default = split.default(context, &keys, |context, expected| {
                self.case(context, rest, others, expected)
            })?;
            self.seen[column.seen] = Seen::Any;
            let (rebuilt, type_) = split.rebuild(keys, default);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// The body of the row at `row`, as the function over its binders it is.
    fn body(&self, row: usize) -> &'m Telescope<Term> {
        match &*self.row(row).body {
            Subterm::Func(func) => &func.telescope,
            _ => unreachable!("a row's body is a function over its binders"),
        }
    }

    /// A fresh local for the binder at `place` of the row `line` is, under the label the row wrote it with.
    fn name(&self, context: &mut Context, line: &Line<'m>, place: usize) -> Free {
        context.fresh_from(self.labels[line.row][place].as_ref())
    }

    /// A name a row writes at a column the tree reads, bound to the column. The row that cannot fail is read apart, once the rows before it are ([`Compiler::unfailing`]), so nothing is bound for it here.
    fn named(&self, context: &mut Context, line: &mut Line<'m>, place: usize, value: Term) {
        if self
            .fallthrough
            .as_ref()
            .is_some_and(|fallthrough| fallthrough.row == line.row)
        {
            return;
        }

        self.bind(context, line, place, value);
    }

    /// Bind the binder at `place` to `value` ahead of the row's body.
    fn bind(&self, context: &mut Context, line: &mut Line<'m>, place: usize, value: Term) {
        let label = self.labels[line.row][place].as_ref();
        let name = context.fresh_written(label);
        // The `let` it is bound by is the matrix's, and a pattern's binder a slot, which a lint asks only whether it binds `{}`.
        context.note_pattern_site(label.and_then(Label::written));
        line.taken[place] = Some(Term::free_var(&name));
        line.lets.push((name, value));
    }
}

/// A case of a row's own walk entered as a term that awaits where the tree's dispatch of it does: beneath a dispatch it completes where its arm is read, and the outermost is the matrix's own, which the walk runs inside ([`Compiler::entering`]).
fn walk_awaited<R>(
    context: &mut Context,
    awaits: bool,
    enter: impl FnOnce(&mut Context) -> Result<R, Error>,
) -> Result<R, Error> {
    match awaits {
        true => context.awaiting(enter),
        false => enter(context),
    }
}

/// The labels of a row's binders, off its body's telescope.
fn labels(row: &Row) -> Vec<Option<Label>> {
    let Subterm::Func(func) = &*row.body else {
        unreachable!("a row's body is a function over its binders");
    };

    let mut labels = Vec::with_capacity(func.telescope.len());
    let mut cursor = func.telescope.cursor();
    while let Some((_, domain)) = cursor.entry() {
        labels.push(cursor.label().copied());
        // A row's binder has no type written, so nothing after it reads what stands in its place here.
        cursor.advance(domain);
    }
    debug_assert_eq!(labels.len(), row.binders());

    labels
}

/// `function`, a row's guard or body — a function over the binders written before it — opened at `values`.
fn opened(function: &Term, values: &[Term]) -> Term {
    let Subterm::Func(func) = &**function else {
        unreachable!("a row's guards and body are functions over its binders");
    };

    func.telescope.open(&values.iter().collect::<Vec<_>>())
}

/// The type of a row with guards read as the function of what its failures take: from `type_`, what a failure takes, to `type_`.
fn failing_type(context: &mut Context, type_: &Term) -> Term {
    Term::func_type(vec![(context.fresh(None), type_.clone())], type_.clone())
}

/// Hold a struct pattern's head, written `head` and resolved to `structure`, to the structure the type of `column` is.
fn held_to_structure(
    context: &mut Context,
    column: &Term,
    head: &str,
    structure: &Mention,
) -> Result<(), Error> {
    // A head that names nothing is reported as the name it is, by the reason the lowering filed for it.
    let structure = match structure {
        Mention::Declaration(structure) => structure,
        Mention::Local(unbound) => return Err(context.unbound_variable(Term::free_var(unbound))),
    };

    let (_, type_) = elaborate(context, column, Mode::Infer)?;
    let type_ = reduce_with(context, &type_)?;
    match &*type_ {
        Subterm::StructType(StructType { name, .. }) if name == structure => Ok(()),
        _ => {
            let head = head.to_string();
            Err(pending(context, [&type_], move |reporting, [type_]| {
                Error::MatrixStructHead {
                    head,
                    scrutinee: Box::new(resolved_for_display(reporting, &type_)),
                }
            }))
        }
    }
}

/// Hold a tuple pattern of `written` fields to the tuple the type of `column` is, which has as many: a tuple's arity is part of its type, so a pattern narrower than its tuple would drop a field no reader sees dropped, and `()` matches the value of `{}` alone.
fn held_to_arity(context: &mut Context, column: &Term, written: usize) -> Result<(), Error> {
    let (_, type_) = elaborate(context, column, Mode::Infer)?;
    let type_ = reduce_with(context, &type_)?;
    match &*type_ {
        Subterm::TupleType(TupleType { telescope }) if telescope.len() == written => Ok(()),
        _ => Err(pending(context, [&type_], move |reporting, [type_]| {
            Error::MatrixTupleArity {
                written,
                scrutinee: Box::new(resolved_for_display(reporting, &type_)),
            }
        })),
    }
}

/// The field of `value` a pattern reads: by its label where it is written with one, and by its position.
///
/// A tuple literal's field is the term written there, so a match over `(a, b)` reads `a` and `b` themselves: an arm that states a case of one refines it as an arm of a match on it alone does, where a projection of the literal is a term no variable stands for.
/// How a field a row's tuple or struct pattern writes is read out of a value of its shape: a tuple's by the label it is written with, or its place among the fields written, and a structure's by the slot of its fields it reads.
#[derive(Clone, Copy, PartialEq)]
enum Read<'m> {
    Tuple(Option<&'m str>),
    Slot(Global, usize),
}

impl Read<'_> {
    /// The field this reads out of `value`, at `index` among the fields written.
    fn of(self, value: &Term, index: usize) -> Term {
        match self {
            Read::Tuple(label) => projection(value, label, index),
            Read::Slot(structure, slot) => Term::proj_struct(value.clone(), structure, slot),
        }
    }
}

/// Each field the tuple or struct `pattern` writes, beside how it is read out of a value of its shape and how a report spells it: a tuple's by its label or its place, and a structure's by the slot of its fields it reads ([`read_fields`]), or as written where its head names no structure, which [`held_to_structure`] refuses where its column is read.
fn parts<'m>(
    context: &Context,
    pattern: &'m Pattern,
) -> Result<Vec<(Read<'m>, String, &'m Pattern)>, Error> {
    let spelled = |mark: Plicity, label: Option<&str>| match (mark, label) {
        (Plicity::Explicit, None) => String::new(),
        (Plicity::Explicit, Some(label)) => format!("{label} = "),
        (_, None) => "@".to_string(),
        (_, Some(label)) => format!("@{label} = "),
    };
    match pattern {
        Pattern::Tuple(fields) => Ok(fields
            .iter()
            .map(|(label, field)| {
                let label = label.as_deref();
                (Read::Tuple(label), spelled(Plicity::Explicit, label), field)
            })
            .collect()),
        Pattern::Struct {
            structure, fields, ..
        } => {
            let declared = match structure {
                Mention::Declaration(name) => context
                    .struct_decl(name)
                    .map(|declaration| (*name, declaration)),
                Mention::Local(_) => None,
            };
            let reads = match declared {
                Some((name, declaration)) => {
                    let written = fields
                        .iter()
                        .map(|(mark, label, _)| (*mark, label.as_deref(), None))
                        .collect::<Vec<_>>();
                    read_fields(
                        &name,
                        record_keyword(context, &name),
                        declaration.fields(),
                        &written,
                    )?
                    .into_iter()
                    .map(|slot| Read::Slot(name, slot))
                    .collect()
                }
                None => fields
                    .iter()
                    .map(|(_, label, _)| Read::Tuple(label.as_deref()))
                    .collect::<Vec<_>>(),
            };
            Ok(reads
                .into_iter()
                .zip(fields)
                .map(|(read, (mark, label, field))| (read, spelled(*mark, label.as_deref()), field))
                .collect())
        }
        _ => unreachable!("only a tuple or a struct is read by its fields"),
    }
}

fn projection(value: &Term, label: Option<&str>, index: usize) -> Term {
    if let Subterm::Tuple(Tuple { fields, names }) = &**value {
        let at = match label {
            Some(label) => names.iter().position(|name| name.as_deref() == Some(label)),
            None => Some(index),
        };
        if let Some(field) = at.and_then(|at| fields.get(at)) {
            return field.clone();
        }
    }

    match label {
        Some(label) => Term::proj_label(value.clone(), label.to_string()),
        None => Term::proj_position(value.clone(), index),
    }
}

/// What a row writes for each payload of `tag`'s case, whose patterns `args` begin at the place `first` among its binders: its patterns met with the payloads by its own marks.
fn payload_cells<'m>(
    tag: &Atom,
    payload: &Telescope<Vec<Term>>,
    args: &'m [(Plicity, Pattern)],
    first: usize,
) -> Result<Vec<Cell<'m>>, Error> {
    let marks = args.iter().map(|(plicity, _)| *plicity).collect::<Vec<_>>();
    let fills = payload_fills(tag, payload, &marks)?;

    let mut places = Vec::with_capacity(args.len());
    let mut place = first;
    for (_, pattern) in args {
        places.push(place);
        place += pattern.binders();
    }

    Ok(fills
        .into_iter()
        .map(|fill| fill.map(|member| (Shape::Pattern(&args[member].1), places[member])))
        .collect())
}

/// The empty proposition a contradiction proves, as it is referenced and as it reduces.
fn empty(context: &mut Context) -> Result<(Term, Term), Error> {
    let empty = Global::Authored(context.syntax().proof.false_type.qualifier());
    let (stated, _) = reference(context, empty, None)?
        .ok_or_else(|| context.unbound_variable(Term::written(empty)))?;
    let reduced = reduce_with(context, &stated)?;

    Ok((stated, reduced))
}

/// How many dispatches a pattern is refuted at: one for each constructor and each carrier leaf it writes, at any depth. A pattern refuted at none takes nothing apart that could fail.
fn refutations(pattern: &Pattern) -> usize {
    match pattern {
        Pattern::Binder | Pattern::Wildcard(_) => 0,
        Pattern::Variant { args, .. } => {
            1 + args.iter().map(|(_, arg)| refutations(arg)).sum::<usize>()
        }
        Pattern::Tuple(fields) => fields.iter().map(|(_, field)| refutations(field)).sum(),
        Pattern::Struct { fields, .. } => {
            fields.iter().map(|(_, _, field)| refutations(field)).sum()
        }
        Pattern::Bool(_) | Pattern::Succ { .. } | Pattern::Literal(_) => 1,
        // A split for each byte, and one for the end.
        Pattern::Str(value) => value.len() + 1,
        // A split for each element, and one for a closed run's end.
        Pattern::List(elements) | Pattern::Bytes(elements) => {
            let ends = match elements {
                Elements::Closed(_) => 1,
                Elements::Open { .. } => 0,
            };

            ends + elements
                .elements()
                .map(|element| 1 + refutations(element))
                .sum::<usize>()
        }
    }
}

/// Each payload a constructor pattern writes, beside where it stands: a plain one by how many plain ones are written ahead of it, and a hidden one by that and by how many hidden ones are written ahead of it since the last plain one. A payload two rows write is at one place in both, whichever hidden payloads either leaves out.
fn payload_places(
    args: &[(Plicity, Pattern)],
) -> impl Iterator<Item = ((usize, Option<usize>), &Pattern)> {
    let mut plain = 0;
    let mut hidden = 0;

    args.iter().map(move |(plicity, pattern)| {
        let place = match plicity {
            Plicity::Explicit => {
                plain += 1;
                hidden = 0;

                (plain - 1, None)
            }
            Plicity::Implicit | Plicity::Witness => {
                hidden += 1;

                (plain, Some(hidden - 1))
            }
        };

        (place, pattern)
    })
}

/// Whether every value `this` matches is one `that` matches: `this` is a case of `that`. Two rows that reach one leaf agree wherever both write a case, so what is asked is whether `that` writes a case anywhere `this` writes a binder.
fn instance(context: &Context, this: &Pattern, that: &Pattern) -> bool {
    match (this, that) {
        (_, Pattern::Binder | Pattern::Wildcard(_)) => true,
        (Pattern::Binder | Pattern::Wildcard(_), that) => refutations(that) == 0,
        (
            Pattern::Variant { tag, args, .. },
            Pattern::Variant {
                tag: other_tag,
                args: others,
                ..
            },
        ) => {
            // A hidden payload one row writes and another leaves out is a binder where it is left out.
            let written = payload_places(args).collect::<BTreeMap<_, _>>();

            tag == other_tag
                && payload_places(others).all(|(place, other)| match written.get(&place) {
                    Some(pattern) => instance(context, pattern, other),
                    None => instance(context, &Pattern::WILDCARD, other),
                })
        }
        (Pattern::Tuple(fields), Pattern::Tuple(others)) => fields
            .iter()
            .zip(others)
            .all(|((_, field), (_, other))| instance(context, field, other)),
        // A field one row reads and another leaves out is a binder where it is left out: the two read the value's fields by slot.
        (Pattern::Struct { .. }, Pattern::Struct { .. }) => {
            let (Ok(these), Ok(those)) = (parts(context, this), parts(context, that)) else {
                return false;
            };
            those.iter().all(|(read, _, other)| {
                let field = these
                    .iter()
                    .find(|(this, ..)| this == read)
                    .map_or(&Pattern::WILDCARD, |(_, _, field)| *field);
                instance(context, field, other)
            })
        }
        (Pattern::Tuple(_) | Pattern::Struct { .. }, _) => false,
        (
            Pattern::List(elements) | Pattern::Bytes(elements),
            Pattern::List(others) | Pattern::Bytes(others),
        ) => {
            // Element by element where both write one; past the end of either, `that` takes what is left only through an open rest of its own.
            let (mut these, mut those) = (elements.elements(), others.elements());
            loop {
                match (these.next(), those.next()) {
                    (Some(element), Some(other)) => {
                        if !instance(context, element, other) {
                            return false;
                        }
                    }
                    (Some(_), None) => return others.rest().is_some(),
                    (None, Some(_)) => return false,
                    (None, None) => return elements.rest().is_none() || others.rest().is_some(),
                }
            }
        }
        _ => true,
    }
}

/// How a run under `sequence` opens.
fn opening(sequence: Sequence) -> &'static str {
    match sequence {
        Sequence::List => "[",
        Sequence::Bytes => "0x[",
    }
}

/// The scrutinee a dispatch on literals reads: a character as its code point, and any other as it stands.
fn dispatched(context: &Context, scrutinee: Scrutinee) -> Scrutinee {
    match scrutinee.is_char(context) {
        true => scrutinee.of_char(context),
        false => scrutinee,
    }
}
