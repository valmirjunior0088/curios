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
//! **Arm order decides nothing between patterns.** The rows that reach a leaf are its candidates, and the first written is taken. Each of the others either has it as a case, a general arm standing after its exception, or is a case of it, which no value reaches there; two that are neither would mean one thing in one order and another in the other, and the match is refused. A case no row reaches is a value no arm matches, reported as its pattern, and a row no leaf takes is an arm no value reaches.
//!
//! **A row with guards takes nothing for itself** ([`Compiler::guarded`]). Where it is the first candidate, its guards run, and where one fails the candidates after it are what the value takes. A row whose one guard is a `Bool` is a `Bool` split at its leaf, its own body the true case and the candidates after it the false one, so they are read under the condition's failure; any other row with guards fails in more ways than one, so the candidates after it are read once, at the leaf, and stand wherever one of its guards fails.
//!
//! **An arm of several alternatives is a row for each**, typed apart as arms of their own are, so two of them that reach one leaf are refused, whichever is a case of the other: an arm's alternatives match no value in common, and no order among them says which typing a value takes.

use {
    super::{
        BoolSplit, Expectation, Mode, NatSplit, NominalSplit, Scrutinee, Sequence, SequenceSplit,
        SwitchSplit, elaborate, payload_fills,
    },
    crate::{
        Context, Error, HeldFrames, WrittenLiteral, check, expect_as, pending, reduce_with,
        resolved_for_display, stuck_on_metavar,
    },
    curios_common::{Plicity, recurse},
    curios_core::{
        Arm, Atom, Elements, Ending, Free, Guard, InductArm, Matrix, Mention, Pattern, Row, Slot,
        StructType, Subterm, Telescope, Term, Tuple, TupleType, WrittenPlace,
    },
    curios_num::Natural,
    curios_surface::written_pattern,
    std::{
        collections::{BTreeMap, BTreeSet},
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
    };

    compiler.whole(context, mode)
}

/// One of a row's binders as its body's telescope labels it: the name it is written with, and where the lowering wrote it among its declaration's binders.
type Label = (Option<String>, Option<u32>);

/// What a row writes at one column beside the place of the first binder it writes there among its row's, or nothing where the compiler put the column under a pattern another row wrote, which the row reads as a binder that binds nothing.
type Cell<'m> = Option<(Shape<'m>, usize)>;

/// What a row writes at a column: a pattern, or what is left of a run once a split has read elements off its front, which is a run of its own.
#[derive(Clone, Copy)]
enum Shape<'m> {
    Pattern(&'m Pattern),
    /// The elements of a run after the ones read, and how it ends, over the sequence it is a run of.
    Remains(Sequence, &'m [Pattern], Ending),
    /// What a string pattern spells past the bytes read: a closed run of those bytes.
    Text(&'m [u8]),
}

impl<'m> Shape<'m> {
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
            Shape::Pattern(_) => None,
            Shape::Remains(sequence, elements, ending) => Some((
                sequence,
                elements
                    .split_first()
                    .map(|(first, then)| (first, Shape::after(sequence, then, ending))),
            )),
        }
    }

    /// The pattern this is, where it is no run's remainder.
    fn pattern(self) -> Option<&'m Pattern> {
        match self {
            Shape::Pattern(pattern) => Some(pattern),
            Shape::Remains(..) | Shape::Text(_) => None,
        }
    }
}

/// The pattern of a byte's literal `byte`, as a string pattern's run reads each of its bytes.
fn byte_pattern(byte: u8) -> &'static Pattern {
    static BYTES: OnceLock<Vec<Pattern>> = OnceLock::new();

    &BYTES.get_or_init(|| {
        (0..=u8::MAX)
            .map(|byte| Pattern::Literal(Natural::from(u32::from(byte))))
            .collect()
    })[usize::from(byte)]
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
    /// A constructor over its plain payloads.
    Variant(String, Vec<usize>),
    /// A tuple, or a struct under its head, over its fields.
    Fields(Option<String>, Vec<(Option<String>, usize)>),
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
    /// Each row's binders, as its body labels them.
    labels: Vec<Vec<Label>>,
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
            if !written
                .iter()
                .all(|other| other.run().is_some_and(|(other, _)| other == sequence))
            {
                return Err(Error::MatrixInconsistentShape);
            }

            return self.sequence(context, sequence, columns, lines, top, mode);
        }
        // What is left of a run is a run, so every other shape is a pattern as written.
        let Some(written) = written
            .iter()
            .map(|shape| shape.pattern())
            .collect::<Option<Vec<_>>>()
        else {
            return Err(Error::MatrixInconsistentShape);
        };
        let first = written[0];

        let every =
            |alike: fn(&Pattern, &Pattern) -> bool| written.iter().all(|other| alike(first, other));
        match first {
            Pattern::Variant { .. } => {
                if !every(|_, other| matches!(other, Pattern::Variant { .. })) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.variants(context, columns, lines, top, mode)
            }
            Pattern::Tuple(_) => {
                if !every(
                    |first, other| matches!((first, other), (Pattern::Tuple(first), Pattern::Tuple(other)) if first.len() == other.len()),
                ) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.fields(context, columns, lines, top, mode)
            }
            Pattern::Struct { .. } => {
                if !every(|first, other| {
                    matches!(
                        (first, other),
                        (
                            Pattern::Struct { structure: first_structure, fields: first, .. },
                            Pattern::Struct { structure: other_structure, fields: other, .. },
                        ) if first_structure == other_structure && first.len() == other.len()
                    )
                }) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.fields(context, columns, lines, top, mode)
            }
            Pattern::Bool(_) => {
                if !every(|_, other| matches!(other, Pattern::Bool(_))) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.booleans(context, columns, lines, top, mode)
            }
            // A character is a dispatch case spelled by its scalar value, so it shares a column with the numerals.
            Pattern::Zero | Pattern::Succ { .. } | Pattern::Literal(_) | Pattern::Character(_) => {
                if !every(|_, other| {
                    matches!(
                        other,
                        Pattern::Zero
                            | Pattern::Succ { .. }
                            | Pattern::Literal(_)
                            | Pattern::Character(_)
                    )
                }) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.naturals(context, columns, lines, top, mode)
            }
            Pattern::List(_) | Pattern::Bytes(_) | Pattern::Str(_) => {
                unreachable!("a run is read above")
            }
            Pattern::Binder | Pattern::Wildcard(_) => {
                unreachable!("a binder or a wildcard is no pattern a column is split by")
            }
        }
    }

    /// The arm of one case: its rows over the columns left, read against the type the case is typed at. Guarded by [`recurse`]: a row is read a pattern at a time, so the depth is that of the patterns a reader wrote.
    fn case(
        &mut self,
        context: &mut Context,
        columns: Vec<Column>,
        lines: Vec<Line<'m>>,
        expected: Term,
    ) -> Result<Term, Error> {
        let mode = Mode::Check(self.mode.at(expected));
        recurse(|| self.columns(context, columns, lines, false, mode)).map(|(arm, _)| arm)
    }

    /// The rows of a column a dispatch is about to split, with the column read by each row that writes a binder there: its name is bound to the column, and the row is under whichever case of the column the compiler puts it, none of which it states.
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
            if line.cells[0].is_none() {
                self.strays[line.row] = true;
            }
        }
    }

    /// The refusal of a match no arm of which matches a value of the case `missing` of the column at `column`: the value as a pattern, with `_` wherever no case was read. `indexed` says the case is a constructor of an indexed family.
    fn uncovered(&mut self, column: &Column, missing: Seen, indexed: bool) -> Error {
        self.seen[column.seen] = missing;

        Error::MatrixUncovered {
            value: self.value(0),
            indexed,
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
    ) -> Error {
        let pattern = self.written(row);

        pending(context, [scrutinee.type_()], move |reporting, [type_]| {
            Error::MatrixLiteralColumn {
                pattern,
                literal,
                scrutinee: Box::new(resolved_for_display(reporting, &type_)),
            }
        })
        .at_opt(self.row(row).body.extent())
    }

    /// The value at `place` as a pattern.
    fn value(&self, place: usize) -> String {
        let fields = |fields: &[(Option<String>, usize)]| {
            fields
                .iter()
                .map(|(label, field)| match label {
                    Some(label) => format!("{label} = {}", self.value(*field)),
                    None => self.value(*field),
                })
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
        let mut names = self.labels[row].iter().map(|(hint, _)| {
            curios_surface::Label::from(hint.clone().unwrap_or_else(|| "_".to_string())).into()
        });

        written_pattern(&self.row(row).pattern, &mut names, std::convert::identity).to_string()
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
                let (case, general) = match (instance(first, later), instance(later, first)) {
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
            if !instance(first, later) && !instance(later, first) {
                return Err(Error::MatrixArmsMeet {
                    value: self.value(0),
                    first: self.written(line.row),
                    second: self.written(other.row),
                }
                .at_opt(self.row(other.row).body.extent()));
            }
            // A guard orders no pattern: a guarded arm before one that is a case of it would hand the case on where its guards fail, where with no guard the case would be reached by nothing — so it is held to the order an arm with no guard is.
            if line.guarded && !instance(first, later) {
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
            return self.fallen(context, mode);
        }
        if !self.strays[line.row] {
            let row = line.row;
            let leaf = self.body_of(context, line);

            return elaborate(context, &leaf, mode)
                .map_err(|error| self.in_alternative(row, error));
        }

        let Mode::Check(expected) = mode else {
            unreachable!("a row is put under a case beneath a dispatch");
        };

        Ok((self.walked(context, &line)?, expected.type_))
    }

    /// The first candidate at a leaf when it has guards to run, and `rest`, the candidates after it: what a value there takes where one of its guards fails.
    ///
    /// A row whose one guard is a `Bool`, typed at this leaf, is a `Bool` split on that condition: its body the true case and `rest` the false one, so each row of `rest` typed at this leaf is read where the condition is false. Any other fails in more ways than one: `rest` is read once, here, and the row is read as the function of it ([`Compiler::failing`]), applied where it stands — `rest` in place where the row has one guard, and a function of no argument, called at each failure, where it has more. A row typed once by its own walk is read so too, its walk the function. A row with guards and no candidate after it leaves what they refuse with no arm.
    fn guarded(
        &mut self,
        context: &mut Context,
        mut line: Line<'m>,
        rest: Vec<Line<'m>>,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let row = line.row;
        if rest.is_empty() {
            return Err(Error::MatrixGuarded {
                value: self.value(0),
                guarded: self.written(row),
            }
            .at_opt(self.row(row).body.extent()));
        }

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
                    recurse(|| self.leaf(context, rest, mode)).map(|(arm, _)| arm)
                })?;
                let true_case = split.arm(context, true, |context, expected| {
                    let mode = Mode::Check(self.mode.at(expected));
                    recurse(|| self.leaf(context, vec![line], mode)).map(|(arm, _)| arm)
                })?;

                Ok(split.rebuild(false_case, true_case))
            });
        }

        let (rest, type_) = recurse(|| self.leaf(context, rest, mode))?;
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

        let body = self.unfailing(context, fallthrough.row);
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
    fn unfailing(&mut self, context: &mut Context, row: usize) -> Term {
        let head = &self.matrix.head;
        let pattern = &self.row(row).pattern;
        let mut line = self.line(row);
        line.cells.clear();
        self.unfailing_parts(context, &mut line, pattern, 0, head.clone());

        self.body_of(context, line)
    }

    /// Bind each name `pattern`, which takes nothing apart, writes to the part of `value` it stands at.
    fn unfailing_parts(
        &mut self,
        context: &mut Context,
        line: &mut Line<'m>,
        pattern: &Pattern,
        mut place: usize,
        value: Term,
    ) {
        match pattern {
            Pattern::Binder => self.bind(context, line, place, value),
            Pattern::Wildcard(_) => {}
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                for (index, (label, field)) in fields.iter().enumerate() {
                    let projected = projection(&value, label.as_deref(), index);
                    self.unfailing_parts(context, line, field, place, projected);
                    place += field.binders();
                }
            }
            _ => unreachable!("the row that cannot fail takes nothing apart"),
        }
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

            // Whether a constructor no row names is left possible, which the rows that write a binder are the one arm of. With no such row each is asked as its turn comes, as a match written one level deep asks.
            let defaulted = !binders.is_empty() && split.leaves_unnamed(context, tags.values())?;
            let undecided = split.admit(context, groups.is_empty(), defaulted)?;
            split.declared(context, tags.values())?;

            // Where a row writes a binder it takes every case no row names, so only the named ones are walked.
            let mut arms = Vec::with_capacity(groups.len());
            for tag in split.cases(context, tags.values(), !binders.is_empty()) {
                let tag = &tag;
                let Some(group) = groups.remove(&tag.as_string()) else {
                    // An arm may be left out where inversion rules the case out.
                    if split.ruled_out(context, tag)? {
                        continue;
                    }

                    let plain = split
                        .payload(context, tag)?
                        .marks()
                        .iter()
                        .filter(|mark| **mark == Plicity::Explicit)
                        .count();
                    let payloads = self.parts(plain);

                    let missing = Seen::Variant(tag.as_string(), payloads);

                    return Err(self.uncovered(&column, missing, split.indexed()));
                };

                arms.push(self.variant(context, &split, tag, group, &column, &rest)?);
            }

            let default = match defaulted {
                true => Some(split.default(context, |context, expected| {
                    self.case(context, rest, binders, expected)
                })?),
                false => None,
            };
            let (rebuilt, type_) = split.rebuild(context, arms, default, undecided, term)?;

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
                Some((Shape::Pattern(Pattern::Variant { args, .. }), first)) => {
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
                    line.cases.extend(binders.iter().copied());

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
        line.cases.extend(binders.iter().copied());

        (binders, slots)
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
        for (value, lines) in [("false", &falses), ("true", &trues)] {
            if lines.is_empty() {
                return Err(self.uncovered(&column, Seen::Leaf(value.to_string()), false));
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

    /// Rows over a column of naturals or of bytes, in one of two forms the rows that write a case choose: dispatch where a row writes a literal past `0`, or the column is a byte, over the literals written, with the rows that write a binder as what any other value takes, and a case split otherwise, `0` and `n + 1`, a row that writes a binder under both. A literal beside a successor is refused: no one form peels a successor and dispatches on a value. A byte is dispatched as the natural it is, and a column of any other type takes no literal.
    fn naturals(
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
        let dispatches =
            first(|pattern| matches!(pattern, Pattern::Literal(_) | Pattern::Character(_)));
        let place = |row: usize| self.row(row).body.extent();

        // What a column's literals are is its type's: a byte's are dispatched as the natural it is, a character's as its code point, which only a character's literal names, and a type that is none of these takes none.
        let scrutinee = Scrutinee::read(context, &column.term)?;
        let byte = scrutinee.is_byte();
        let character = scrutinee.is_char(context);
        let refused = lines.iter().find_map(|line| {
            let (shape, _) = line.cells[0]?;
            let written = match shape {
                Shape::Pattern(Pattern::Character(_)) => WrittenLiteral::Character,
                _ => WrittenLiteral::Numeral,
            };
            let taken = match written {
                WrittenLiteral::Character => character || byte || scrutinee.takes_naturals(context),
                _ => !character && (byte || scrutinee.takes_naturals(context)),
            };

            (!taken).then_some((line.row, written))
        });
        if let Some((row, literal)) = refused {
            return Err(self.literal_refused(context, &scrutinee, row, literal));
        }
        if byte && let Some(successor) = peels {
            return Err(Error::MatrixByteSuccessor {
                successor: self.written(successor),
            }
            .at_opt(place(successor)));
        }
        if let (Some(successor), Some(literal)) = (peels, dispatches) {
            return Err(Error::MatrixMixedNatDispatch {
                successor: self.written(successor),
                literal: self.written(literal),
                successor_at: place(successor),
                literal_at: place(literal),
            });
        }

        // `0` and a binder are induction's two cases, so `| 0 => … | _ => …` is the term `| 0 => … | _ + 1 => …` is, and takes its successor arm at a successor whose predecessor is unknown, where a dispatch takes an arm only at a literal. A byte has no successor, so its `0` is a literal.
        if byte || character || dispatches.is_some() {
            let literal = |shape: Shape<'m>| match shape {
                Shape::Pattern(Pattern::Zero) => Natural::zero(),
                Shape::Pattern(Pattern::Literal(value)) => value.clone(),
                Shape::Pattern(Pattern::Character(character)) => Natural::from(*character as u32),
                _ => unreachable!("every row of the column writes a `Nat`"),
            };
            // A byte's literals are its values, as a packed literal's constant entries are.
            if byte {
                let past = lines.iter().find_map(|line| {
                    let (shape, _) = line.cells[0]?;
                    (literal(shape) > Natural::from(u32::from(u8::MAX)))
                        .then_some((shape, line.row))
                });
                if let Some((shape, row)) = past {
                    let value = match shape {
                        Shape::Pattern(Pattern::Character(character)) => format!("{character:?}"),
                        _ => literal(shape).to_string(),
                    };

                    return Err(Error::ByteLiteralOutOfRange { value }.at_opt(place(row)));
                }
            }
            let mut groups: BTreeMap<Natural, Vec<Line<'m>>> = lines
                .iter()
                .filter_map(|line| line.cells[0].map(|(shape, _)| (literal(shape), Vec::new())))
                .collect();
            // Each case as its first row writes it, a character as a character.
            let mut spelled = BTreeMap::new();
            for line in &lines {
                if let Some((shape, _)) = line.cells[0] {
                    spelled
                        .entry(literal(shape))
                        .or_insert_with(|| match shape {
                            Shape::Pattern(Pattern::Character(character)) => {
                                format!("{character:?}")
                            }
                            _ => literal(shape).to_string(),
                        });
                }
            }
            let mut others = Vec::new();
            for mut line in lines {
                match line.cells.remove(0) {
                    Some((shape, _)) => groups
                        .get_mut(&literal(shape))
                        .expect("a group stands for each literal a row writes")
                        .push(line),
                    None => {
                        for group in groups.values_mut() {
                            group.push(line.clone());
                        }
                        others.push(line);
                    }
                }
            }
            // No set of literals covers every natural: the least one no row writes is a value no arm matches. A byte's dispatch is a natural's, so it too takes its other values at a binder, written `_` where every byte has an arm.
            if others.is_empty() {
                let missing = match (byte, character) {
                    (true, _) => (0..=u32::from(u8::MAX))
                        .map(Natural::from)
                        .find(|literal| !groups.contains_key(literal))
                        .map_or(Seen::Any, |least| Seen::Leaf(least.to_string())),
                    // A character no row writes is any of them.
                    (false, true) => Seen::Any,
                    (false, false) => {
                        let least = (0u32..)
                            .map(Natural::from)
                            .find(|literal| !groups.contains_key(literal))
                            .expect("a match writes finitely many literals");

                        Seen::Leaf(least.to_string())
                    }
                };

                return Err(self.uncovered(&column, missing, false));
            }

            let scrutinee = dispatched(context, scrutinee);
            let goal = Term::hole(context.mint_metavar());

            return context.awaiting(|context| {
                let split = Rc::new(SwitchSplit::resolve(context, scrutinee, &goal, &mode)?);
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
                    let arm = split.arm(context, &value, |context, expected| {
                        self.case(context, rest.clone(), group, expected)
                    })?;
                    cases.push((value, arm));
                }
                self.seen[column.seen] = Seen::Any;
                let default = split.default(context, |context, expected| {
                    self.case(context, rest, others, expected)
                })?;
                let (rebuilt, type_) = split.rebuild(cases, default);

                self.settled(context, top, rebuilt, type_)
            });
        }

        let mut zeros = Vec::new();
        let mut successors = Vec::new();
        for mut line in lines {
            match line.cells.remove(0) {
                Some((Shape::Pattern(Pattern::Zero), _)) => zeros.push(line),
                Some((Shape::Pattern(Pattern::Succ { predecessor }), place)) => {
                    let predecessor = Some((Shape::Pattern(slot_pattern(*predecessor)), place));
                    successors.push((Some(vec![predecessor]), line));
                }
                _ => {
                    zeros.push(line.clone());
                    successors.push((None, line));
                }
            }
        }
        // Each case of the split is covered or the match is refused: a row that writes `0` alone leaves every successor to no arm, the least of them `1`.
        if zeros.is_empty() {
            return Err(self.uncovered(&column, Seen::Leaf("0".to_string()), false));
        }
        if successors.is_empty() {
            return Err(self.uncovered(&column, Seen::Leaf("1".to_string()), false));
        }

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
            self.seen[column.seen] = Seen::Leaf("_ + 1".to_string());
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

        // A string pattern takes a `Str` apart by its bytes, and a column a row writes one at holds strings alone: the row that writes the first, where one does.
        let whole =
            |line: &Line<'m>| matches!(line.cells[0], Some((Shape::Pattern(Pattern::Str(_)), _)));
        let texts = lines.iter().find(|line| whole(line)).map(|line| line.row);
        if texts.is_some()
            && lines
                .iter()
                .any(|line| line.cells[0].is_some() && !whole(line))
        {
            return Err(Error::MatrixInconsistentShape);
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
        if empties.is_empty() {
            return Err(self.uncovered(&column, Seen::Empty(sequence), false));
        }
        if conses.is_empty() {
            let parts = self.parts(2);
            let missing = Seen::Cons(sequence, parts[0], parts[1]);

            return Err(self.uncovered(&column, missing, false));
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

        let written = |shape: Shape<'m>| match shape {
            Shape::Pattern(Pattern::Tuple(fields) | Pattern::Struct { fields, .. }) => fields,
            _ => unreachable!("every row that takes the column apart writes a tuple or a struct"),
        };
        let labelled = |fields: &'m [(Option<String>, Pattern)]| {
            fields
                .iter()
                .map(|(label, _)| label.as_deref())
                .collect::<Vec<_>>()
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
        let canonical = labelled(written(shape));
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

        let mut flattened = Vec::with_capacity(lines.len());
        for mut line in lines {
            let cells = match line.cells.remove(0) {
                Some((Shape::Pattern(Pattern::Binder), place)) => {
                    self.named(context, &mut line, place, column.term.clone());

                    vec![None; canonical.len()]
                }
                Some((Shape::Pattern(Pattern::Wildcard(_)), _)) => vec![None; canonical.len()],
                Some((shape, mut place)) => {
                    let fields = written(shape);
                    if labelled(fields) != canonical {
                        return Err(Error::MatrixInconsistentShape);
                    }

                    fields
                        .iter()
                        .map(|(_, field)| {
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
            .map(|(index, label)| Column {
                term: projection(&column.term, *label, index),
                seen: places[index],
            })
            .collect::<Vec<_>>();
        projections.extend(rest);
        self.seen[column.seen] = Seen::Fields(
            head,
            canonical
                .iter()
                .map(|label| label.map(str::to_string))
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
                    .map(|place| {
                        let (hint, written) = &self.labels[row][place];
                        context.fresh_written(hint.as_deref(), *written)
                    })
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
            Shape::Pattern(Pattern::Tuple(fields) | Pattern::Struct { fields, .. }) => {
                let mut cells = Vec::with_capacity(fields.len() + line.cells.len());
                let mut projections = Vec::with_capacity(fields.len() + columns.len());
                let mut place = place;
                for (index, (label, field)) in fields.iter().enumerate() {
                    cells.push(Some((Shape::Pattern(field), place)));
                    place += field.binders();
                    projections.push(projection(&column, label.as_deref(), index));
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
        let Shape::Pattern(pattern) = shape else {
            unreachable!("what is left of a run is a run");
        };

        match pattern {
            Pattern::Binder | Pattern::Wildcard(_) | Pattern::Tuple(_) | Pattern::Struct { .. } => {
                unreachable!("a pattern that states no case is read above")
            }
            Pattern::List(_) | Pattern::Bytes(_) | Pattern::Str(_) => {
                unreachable!("a run is read above")
            }
            Pattern::Variant { tag, args } => {
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
            Pattern::Literal(_) | Pattern::Character(_) => {
                let value = match pattern {
                    Pattern::Literal(value) => value.clone(),
                    Pattern::Character(character) => Natural::from(*character as u32),
                    _ => unreachable!("a literal is a numeral or a character"),
                };
                let split = match shared {
                    Some(Outermost::Switch(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());

                        let scrutinee = dispatched(context, scrutinee);

                        Rc::new(SwitchSplit::resolve(context, scrutinee, &goal, &mode)?)
                    }
                };

                walk_awaited(context, awaits, |context| {
                    split.arm(context, &value, |context, expected| {
                        arm(self, context, columns, line, expected)
                    })
                })?;
            }
            Pattern::Zero | Pattern::Succ { .. } => {
                let split = match shared {
                    // The tree's dispatch is in the form its rows chose: a `0` is a literal's case where a row writes a literal past it.
                    Some(Outermost::Switch(split)) => {
                        walk_awaited(context, awaits, |context| {
                            split.arm(context, &Natural::zero(), |context, expected| {
                                arm(self, context, columns, line, expected)
                            })
                        })?;

                        return Ok(walked.expect("the arm of a row's case is its walk"));
                    }
                    Some(Outermost::Nat(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let goal = Term::hole(context.mint_metavar());
                        // A byte has no successor, so its `0` is a literal's case, as the tree read it.
                        if scrutinee.is_byte() {
                            let split =
                                SwitchSplit::resolve(context, scrutinee.of_byte(), &goal, &mode)?;
                            walk_awaited(context, awaits, |context| {
                                split.arm(context, &Natural::zero(), |context, expected| {
                                    arm(self, context, columns, line, expected)
                                })
                            })?;

                            return Ok(walked.expect("the arm of a row's case is its walk"));
                        }

                        Rc::new(NatSplit::resolve(context, scrutinee, &goal, &mode)?)
                    }
                };

                match pattern {
                    Pattern::Succ { predecessor } => {
                        let cells = vec![Some((Shape::Pattern(slot_pattern(*predecessor)), place))];
                        let (binders, _) = self.alone(context, &mut line, cells);
                        let predecessor = binders[0];

                        walk_awaited(context, awaits, |context| {
                            split.successor(context, &predecessor, |context, expected| {
                                arm(self, context, columns, line, expected)
                            })
                        })?;
                    }
                    _ => {
                        walk_awaited(context, awaits, |context| {
                            split.zero(context, |context, expected| {
                                arm(self, context, columns, line, expected)
                            })
                        })?;
                    }
                }
            }
        }

        Ok(walked.expect("the arm of a row's case is its walk"))
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
        let (hint, written) = &self.labels[line.row][place];

        context.fresh_for(hint.as_deref(), *written)
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
        let (hint, written) = &self.labels[line.row][place];
        let name = context.fresh_written(hint.as_deref(), *written);
        // The `let` it is bound by is the matrix's, and a pattern's binder a slot, which a lint asks only whether it binds `{}`.
        context.note_pattern_site(*written);
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
fn labels(row: &Row) -> Vec<Label> {
    let Subterm::Func(func) = &*row.body else {
        unreachable!("a row's body is a function over its binders");
    };

    let mut labels = Vec::with_capacity(func.telescope.len());
    let mut cursor = func.telescope.cursor();
    while let Some((hint, domain)) = cursor.entry() {
        labels.push((hint.map(str::to_string), cursor.written()));
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

/// How many dispatches a pattern is refuted at: one for each constructor and each carrier leaf it writes, at any depth. A pattern refuted at none takes nothing apart that could fail.
fn refutations(pattern: &Pattern) -> usize {
    match pattern {
        Pattern::Binder | Pattern::Wildcard(_) => 0,
        Pattern::Variant { args, .. } => {
            1 + args.iter().map(|(_, arg)| refutations(arg)).sum::<usize>()
        }
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
            fields.iter().map(|(_, field)| refutations(field)).sum()
        }
        Pattern::Bool(_)
        | Pattern::Zero
        | Pattern::Succ { .. }
        | Pattern::Literal(_)
        | Pattern::Character(_) => 1,
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
fn instance(this: &Pattern, that: &Pattern) -> bool {
    match (this, that) {
        (_, Pattern::Binder | Pattern::Wildcard(_)) => true,
        (Pattern::Binder | Pattern::Wildcard(_), that) => refutations(that) == 0,
        (
            Pattern::Variant { tag, args },
            Pattern::Variant {
                tag: other_tag,
                args: others,
            },
        ) => {
            // A hidden payload one row writes and another leaves out is a binder where it is left out.
            let written = payload_places(args).collect::<BTreeMap<_, _>>();

            tag == other_tag
                && payload_places(others).all(|(place, other)| match written.get(&place) {
                    Some(pattern) => instance(pattern, other),
                    None => instance(&Pattern::WILDCARD, other),
                })
        }
        (Pattern::Tuple(fields), Pattern::Tuple(others))
        | (Pattern::Struct { fields, .. }, Pattern::Struct { fields: others, .. }) => fields
            .iter()
            .zip(others)
            .all(|((_, field), (_, other))| instance(field, other)),
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
                        if !instance(element, other) {
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
        Sequence::Bytes => "x[",
    }
}

/// The scrutinee a dispatch on literals reads: a byte as the natural it is, a character as its code point, and any other as it stands.
fn dispatched(context: &Context, scrutinee: Scrutinee) -> Scrutinee {
    match (scrutinee.is_byte(), scrutinee.is_char(context)) {
        (true, _) => scrutinee.of_byte(),
        (false, true) => scrutinee.of_char(context),
        (false, false) => scrutinee,
    }
}
