//! The matrix compiler: a match as it is written, compiled into the single-level matches the eliminators rebuild.
//!
//! [`elaborate_matrix`] takes the [`Matrix`] lowering wrote and builds the decision tree its rows come to (Maranget, "Compiling Pattern Matching to Good Decision Trees"), with each column's cases read off its type. Columns are taken left to right. A column no row takes apart is retired, each name written there bound to it. A tuple or struct column is flattened into its fields' projections, and into the fields themselves where the column is a tuple written in place. Any other is a dispatch: the rows that name a case are its arm, a row that writes a binder there joins the arm of every case some row names, and where the type has a case no row names, the rows that write a binder are what any other value takes.
//!
//! Each dispatch is an eliminator's own, taken a step at a time ([`NominalSplit`] and its kin): the column is resolved as a scrutinee, each case is entered as an arm of it is, with the rows of that case over the columns left as the arm's body, and the match is rebuilt from the arms. So what an arm assumes is stated once, for the tree and for a match written one level deep.
//!
//! **A row is typed once, where its own pattern states it.** A row whose every case on the way to its leaf is one it states is elaborated at that leaf, whose frames are the row's own. A row that a binder of its own put under a case another row wrote reaches a leaf whose frames hold more than it states, and may reach several. It is elaborated once, by its own walk ([`Compiler::walk`]): its cases entered alone, standing on the frames held where the match is entered. Each leaf it reaches holds that body with the leaf's binders in the places of the walk's ([`Term::substitute`]), which the elaborator does not type again and the kernel does.
//!
//! **The row that cannot fail**, a final row that takes nothing apart, states no case, so it is read where the match stands and at the match's own type, once, after the rows before it: written in place where one leaf reaches it, and bound once as a function of no argument, called from each, where more do.
//!
//! **Arm order decides nothing.** The rows that reach a leaf are its candidates, and the first written is taken. Each of the others either has it as a case, a general arm standing after its exception, or is a case of it, which no value reaches there; two that are neither would mean one thing in one order and another in the other, and the match is refused. A case no row reaches is a value no arm matches, reported as its pattern, and a row no leaf takes is an arm no value reaches.

use {
    super::{
        BoolSplit, Mode, NatSplit, NominalSplit, Scrutinee, Sequence, SequenceSplit, SwitchSplit,
        check, elaborate, expect, holds_goal, payload_fills,
    },
    crate::{Context, Error, HeldFrames, reduce_with, resolved_for_display},
    curios_core::{
        Atom, Bind, Free, InductArm, Many, Matrix, Pattern, Row, Scope, StructType, Subterm,
        Telescope, Term, Tuple,
    },
    curios_num::{Grain, Natural},
    curios_utilities::{Plicity, recurse},
    std::{
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// Compile `matrix`, which `term` holds, into the tree of matches it comes to, each elaborated as it is built.
pub(super) fn elaborate_matrix(
    context: &mut Context,
    matrix: &Matrix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let rows = matrix.rows.len();
    let compiler = Compiler {
        matrix,
        term,
        labels: matrix.rows.iter().map(labels).collect(),
        mode: mode.clone(),
        held: context.hold_frames(),
        fallthrough: None,
        outermost: None,
        strays: vec![false; rows],
        walks: vec![None; rows],
        folds: None,
        taken: vec![false; rows],
        covers: vec![None; rows],
        seen: vec![Seen::Any],
    };

    compiler.whole(context, mode)
}

/// One of a row's binders as its body's telescope labels it: the name it is written with, and where the lowering wrote it among its declaration's binders.
type Label = (Option<String>, Option<u32>);

/// What a row writes at one column: a pattern beside the place of the first binder it writes among its row's, or nothing where the compiler put the column under a pattern another row wrote, which the row reads as a binder that binds nothing.
type Cell<'m> = Option<(&'m Pattern, usize)>;

/// A row under a successor or a cons: the place of its first binder and the hypothesis it writes, where it writes the case, and the row.
type Peeled<'m> = (Option<(usize, &'m Option<Bind>)>, Line<'m>);

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
    /// What each of the row's binders stands for, once its pattern is read.
    taken: Vec<Option<Term>>,
    /// The binders bound ahead of the body, each to what it stands for, in the order they were read: the first is outermost.
    lets: Vec<(Free, Term)>,
    /// Each hypothesis the row writes under a structure's name: what it stands for, the name as written and what it resolves to. Held where the row's body stands ([`Compiler::body_of`]), a hypothesis having no type before its case is entered.
    held: Vec<(Term, String, Free)>,
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
    /// For each row, whether each of the cases a fold splits that its pattern writes is one of a fold ([`Compiler::folds`]): read where a row is first walked.
    folds: Option<Vec<Vec<bool>>>,
    /// The rows some leaf takes.
    taken: Vec<bool>,
    /// For a row that reached a leaf another row took, that row: the arm written before it that covers it there.
    covers: Vec<Option<usize>>,
    /// The value the scrutinee holds at the place the compiler stands, as a pattern: the first is the scrutinee's, and a part read apart names the places of its parts.
    seen: Vec<Seen>,
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
        let rows = matrix.rows.len();

        // A final row that takes nothing apart states no case. A lone one is a binding, read where its one leaf stands.
        if rows > 1 && refutations(&matrix.rows[rows - 1].pattern) == 0 {
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
            .at_opt(matrix.rows[row].body.extent())),
            None => Ok(compiled),
        }
    }

    /// The row at `row` with nothing of it read: its whole pattern over the scrutinee.
    fn line(&self, row: usize) -> Line<'m> {
        Line {
            row,
            cells: vec![Some((&self.matrix.rows[row].pattern, 0))],
            taken: vec![None; self.labels[row].len()],
            lets: Vec::new(),
            held: Vec::new(),
            cases: Vec::new(),
        }
    }

    /// One step: the leftmost column is retired where no row takes it apart, flattened where the rows that do write a tuple or a struct, and otherwise split by what they write. `top` says no dispatch stands above the column: its dispatch is where a written motive attaches, the scrutinee being what a match that writes one dispatches on, and where the row that cannot fail is read, and `mode` is how what the rows come to is read: as the match is at its scrutinee, and against the type of its case beneath a dispatch.
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

        let written = lines
            .iter()
            .filter_map(|line| line.cells[0].map(|(pattern, _)| pattern))
            .filter(|pattern| !matches!(pattern, Pattern::Binder))
            .collect::<Vec<_>>();
        let Some(first) = written.first().copied() else {
            let column = columns.remove(0);
            for line in &mut lines {
                if let Some((_, place)) = line.cells.remove(0) {
                    self.named(context, line, place, column.term.clone());
                }
            }

            return recurse(|| self.columns(context, columns, lines, top, mode));
        };

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
            Pattern::Zero | Pattern::Succ(_) | Pattern::Literal(_) | Pattern::Character(_) => {
                if !every(|_, other| {
                    matches!(
                        other,
                        Pattern::Zero
                            | Pattern::Succ(_)
                            | Pattern::Literal(_)
                            | Pattern::Character(_)
                    )
                }) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.naturals(context, columns, lines, top, mode)
            }
            Pattern::Nil | Pattern::Cons(_) => {
                if !every(|_, other| matches!(other, Pattern::Nil | Pattern::Cons(_))) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.lists(context, columns, lines, top, mode)
            }
            Pattern::End(_) | Pattern::Atom(..) => {
                if !every(|_, other| matches!(other, Pattern::End(_) | Pattern::Atom(..))) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.packed(context, columns, lines, top, mode)
            }
            Pattern::Binder => unreachable!("a binder is no pattern a column is split by"),
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
        recurse(|| self.columns(context, columns, lines, false, Mode::Check(expected)))
            .map(|(arm, _)| arm)
    }

    /// The rows of a column a dispatch is about to split, with the column read by each row that writes a binder there: its name is bound to the column, and the row is under whichever case of the column the compiler puts it, none of which it states.
    fn joined(&mut self, context: &mut Context, lines: &mut [Line<'m>], column: &Column) {
        for line in lines {
            if let Some((Pattern::Binder, place)) = line.cells[0] {
                self.named(context, line, place, column.term.clone());
                line.cells[0] = None;
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
        let mut names = self.labels[row]
            .iter()
            .map(|(hint, _)| hint.clone().unwrap_or_else(|| "_".to_string()));

        self.matrix.rows[row].pattern.written(&mut names)
    }

    /// The rows that reach a place where every column is read: the first written is the arm a value there takes, and it is a case of every other, since nothing else orders two arms that match one value.
    fn leaf(
        &mut self,
        context: &mut Context,
        mut lines: Vec<Line<'m>>,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let line = lines.remove(0);
        let matrix = self.matrix;
        let patterns = |row: usize| &matrix.rows[row].pattern;
        for other in &lines {
            // An arm written after one it is a case of is no second claim on the value: the first takes it, and the later one is reached by nothing here.
            let (first, later) = (patterns(line.row), patterns(other.row));
            if !instance(first, later) && !instance(later, first) {
                return Err(Error::MatrixArmsMeet {
                    value: self.value(0),
                    first: self.written(line.row),
                    second: self.written(other.row),
                }
                .at_opt(self.matrix.rows[other.row].body.extent()));
            }
            self.covers[other.row].get_or_insert(line.row);
        }
        self.taken[line.row] = true;

        if self
            .fallthrough
            .as_ref()
            .is_some_and(|fallthrough| fallthrough.row == line.row)
        {
            return self.fallen(context, mode);
        }
        if !self.strays[line.row] {
            let leaf = self.body_of(context, line);

            return elaborate(context, &leaf, mode);
        }

        let Mode::Check(expected) = mode else {
            unreachable!("a row is put under a case beneath a dispatch");
        };
        let walked = match &self.walks[line.row] {
            Some(walked) => Rc::clone(walked),
            None => {
                let walked = Rc::new(self.walk(context, line.row)?);
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

        Ok((walked.body.substitute(&renamed), expected))
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
        expect(context, self.term, &stands, &expected)?;

        Ok((stand_in, expected))
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

    /// The row that cannot fail as a term over the scrutinee: its body under each name its pattern writes, bound to the part of the scrutinee it stands at. A lone `_` binds nothing its body can name, so its one binder stands for anything.
    fn unfailing(&mut self, context: &mut Context, row: usize) -> Term {
        let head = &self.matrix.head;
        let pattern = &self.matrix.rows[row].pattern;
        if matches!(pattern, Pattern::Binder) && self.labels[row][0].0.is_none() {
            return self.body(row).open(&[head]);
        }

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
            if let Some((Pattern::Variant { tag, .. }, _)) = line.cells[0] {
                tags.entry(tag.as_string()).or_insert_with(|| tag.clone());
            }
        }
        // Each group in the order its rows are written, which is the order a leaf reads its candidates in.
        let mut groups: BTreeMap<String, Vec<Line<'m>>> =
            tags.keys().map(|tag| (tag.clone(), Vec::new())).collect();
        let mut binders = Vec::new();
        for mut line in lines {
            match line.cells[0] {
                Some((Pattern::Variant { tag, .. }, _)) => groups
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
        let motive = self.motive(context, top);
        let term = self.term;

        context.awaiting(|context| {
            let vacuous = groups.is_empty() && binders.is_empty();
            let split = Rc::new(NominalSplit::resolve(
                context,
                scrutinee,
                &motive,
                (term, &mode),
                vacuous,
            )?);
            self.resolved(top, || {
                (
                    Outermost::Nominal(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });

            // The constructors no row names that inversion leaves possible: what the rows that write a binder are the one arm of. With no such row each is asked as its turn comes, as a match written one level deep asks.
            let mut unnamed = Vec::new();
            if !binders.is_empty() {
                for tag in split.constructors() {
                    if !groups.contains_key(&tag.as_string()) && !split.ruled_out(context, tag)? {
                        unnamed.push(tag.clone());
                    }
                }
            }
            let defaulted = !unnamed.is_empty();
            let undecided = split.admit(context, groups.is_empty(), defaulted)?;
            split.declared(tags.values())?;

            let mut arms = Vec::with_capacity(groups.len());
            for tag in split.constructors() {
                let Some(group) = groups.remove(&tag.as_string()) else {
                    // An arm may be left out where a row that writes a binder takes its case, or where inversion rules the case out.
                    if !binders.is_empty() || split.ruled_out(context, tag)? {
                        continue;
                    }

                    let plain = split
                        .payload(tag)
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
        mut group: Vec<Line<'m>>,
        column: &Column,
        rest: &[Column],
    ) -> Result<(Atom, InductArm), Error> {
        let payload = split.payload(tag);
        let marks = payload.marks();
        let places = self.parts(marks.len());

        let naming = group
            .iter()
            .enumerate()
            .filter(|(_, line)| line.cells[0].is_some())
            .map(|(at, _)| at)
            .collect::<Vec<_>>();
        let (binders, mut columns, lines) = match naming.as_slice() {
            &[at] => {
                let Some((Pattern::Variant { args, .. }, first)) = group[at].cells.remove(0) else {
                    unreachable!("the row that names a constructor's case writes it");
                };
                let cells = payload_cells(tag, &payload, args, first)?;
                let (binders, slots) = self.alone(context, &mut group[at], cells);
                // A row that wrote a binder at the column reads no payload.
                for (index, line) in group.iter_mut().enumerate() {
                    if index != at {
                        line.cells.remove(0);
                        line.cells
                            .splice(0..0, std::iter::repeat_n(None, slots.len()));
                    }
                }
                let columns = slots
                    .into_iter()
                    .map(|slot| Column {
                        term: Term::free_var(&binders[slot]),
                        seen: places[slot],
                    })
                    .collect::<Vec<_>>();

                (binders, columns, group)
            }
            _ => {
                let binders = marks
                    .iter()
                    .map(|_| context.fresh(None))
                    .collect::<Vec<_>>();
                let mut read = vec![false; marks.len()];
                let mut rows = Vec::with_capacity(group.len());
                for mut line in group {
                    let cells = match line.cells.remove(0) {
                        Some((Pattern::Variant { args, .. }, first)) => {
                            let cells = payload_cells(tag, &payload, args, first)?;
                            line.cases.extend(binders.iter().copied());

                            cells
                        }
                        _ => vec![None; marks.len()],
                    };
                    for (slot, cell) in cells.iter().enumerate() {
                        read[slot] |= cell.is_some();
                    }
                    rows.push((cells, line));
                }

                // A column for each payload some row writes: what no row writes, no row reads.
                let slots = (0..marks.len())
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
                    .collect::<Vec<_>>();
                let columns = slots
                    .into_iter()
                    .map(|slot| Column {
                        term: Term::free_var(&binders[slot]),
                        seen: places[slot],
                    })
                    .collect::<Vec<_>>();

                (binders, columns, lines)
            }
        };
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

    /// A constructor's case as `line`, the one row that names it, reads it, over the `cells` it writes for the case's payloads: the binder of each payload — the row's own where it writes a name, and one of the compiler's where it writes a pattern or nothing — and the payloads whose pattern is left to read, which are the columns the row's cells are put ahead for.
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
                Some((Pattern::Binder, place)) => {
                    let name = self.name(context, line, place);
                    line.taken[place] = Some(Term::free_var(&name));

                    name
                }
                Some(cell) => {
                    slots.push(slot);
                    front.push(Some(cell));

                    context.fresh(None)
                }
                None => context.fresh(None),
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
                Some((Pattern::Bool(false), _)) => falses.push(line),
                Some((Pattern::Bool(true), _)) => trues.push(line),
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
        let motive = self.motive(context, top);
        let term = self.term;

        context.awaiting(|context| {
            let split = Rc::new(BoolSplit::resolve(
                context, scrutinee, &motive, term, &mode,
            )?);
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

    /// Rows over a `Nat` column, in one of two forms the rows that write a case choose: induction where a row peels a successor, `0` and `n + 1` with a hypothesis, and dispatch otherwise, over the literals written, with the rows that write a binder as what any other value takes. A literal beside a successor is refused: no one form peels a successor and dispatches on a value.
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

        let writes = |alike: fn(&Pattern) -> bool| {
            lines
                .iter()
                .any(|line| line.cells[0].is_some_and(|(pattern, _)| alike(pattern)))
        };
        let peels = writes(|pattern| matches!(pattern, Pattern::Succ(_)));
        let dispatches =
            writes(|pattern| matches!(pattern, Pattern::Literal(_) | Pattern::Character(_)));
        if peels && dispatches {
            return Err(Error::MatrixMixedNatDispatch);
        }

        let term = self.term;

        if !peels {
            let literal = |pattern: &Pattern| match pattern {
                Pattern::Zero => Natural::zero(),
                Pattern::Literal(value) => value.clone(),
                Pattern::Character(character) => Natural::from(*character as u32),
                _ => unreachable!("every row of the column writes a `Nat`"),
            };
            let mut groups: BTreeMap<Natural, Vec<Line<'m>>> = lines
                .iter()
                .filter_map(|line| line.cells[0].map(|(pattern, _)| (literal(pattern), Vec::new())))
                .collect();
            let mut others = Vec::new();
            for mut line in lines {
                match line.cells.remove(0) {
                    Some((pattern, _)) => groups
                        .get_mut(&literal(pattern))
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
            // No set of literals covers every natural: the least one no row writes is a value no arm matches.
            if others.is_empty() {
                let least = (0u32..)
                    .map(Natural::from)
                    .find(|literal| !groups.contains_key(literal))
                    .expect("a match writes finitely many literals");

                return Err(self.uncovered(&column, Seen::Leaf(least.to_string()), false));
            }

            let scrutinee = Scrutinee::read(context, &column.term)?;
            let motive = self.motive(context, top);

            return context.awaiting(|context| {
                let split = Rc::new(SwitchSplit::resolve(
                    context, scrutinee, &motive, term, &mode,
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
                    self.seen[column.seen] = Seen::Leaf(value.to_string());
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
        let mut successors: Vec<Peeled<'m>> = Vec::new();
        for mut line in lines {
            match line.cells.remove(0) {
                Some((Pattern::Zero, _)) => zeros.push(line),
                Some((Pattern::Succ(hypothesis), place)) => {
                    successors.push((Some((place, hypothesis)), line))
                }
                _ => {
                    zeros.push(line.clone());
                    successors.push((None, line));
                }
            }
        }
        if zeros.is_empty() {
            return Err(self.uncovered(&column, Seen::Leaf("0".to_string()), false));
        }

        let reads_hypothesis = self.reads_hypothesis(&successors, 1);
        let scrutinee = Scrutinee::read(context, &column.term)?;
        let motive = self.motive(context, top);

        context.awaiting(|context| {
            let split = Rc::new(NatSplit::resolve(
                context,
                scrutinee,
                &motive,
                (term, &mode),
                reads_hypothesis,
            )?);
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

            let ([predecessor, hypothesis], group) = self.peel(context, successors);
            self.seen[column.seen] = Seen::Leaf("_ + 1".to_string());
            let successor =
                split.successor(context, &predecessor, &hypothesis, |context, expected| {
                    self.case(context, rest, group, expected)
                })?;
            self.seen[column.seen] = Seen::Any;
            let (rebuilt, type_) = split.rebuild(zero, &predecessor, &hypothesis, successor);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// Rows split into `[]` and `[head, ..tail]`, a row that writes a binder under both.
    fn lists(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        self.joined(context, &mut lines, &column);

        let mut empties = Vec::new();
        let mut conses: Vec<Peeled<'m>> = Vec::new();
        for mut line in lines {
            match line.cells.remove(0) {
                Some((Pattern::Nil, _)) => empties.push(line),
                Some((Pattern::Cons(hypothesis), place)) => {
                    conses.push((Some((place, hypothesis)), line))
                }
                _ => {
                    empties.push(line.clone());
                    conses.push((None, line));
                }
            }
        }

        let cases = ["[]", "[_, .._]"];
        self.sequence(
            context,
            Sequence::List,
            (column, columns),
            (empties, conses),
            cases,
            (top, mode),
        )
    }

    /// Rows split into the empty packed sequence and one atom ahead of a tail, at the one grain every row that writes a case writes, a row that writes a binder under both.
    fn packed(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Column>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let column = columns.remove(0);
        self.joined(context, &mut lines, &column);

        let mut grain: Option<Grain> = None;
        let mut empties = Vec::new();
        let mut atoms: Vec<Peeled<'m>> = Vec::new();
        for mut line in lines {
            let written = match line.cells.remove(0) {
                Some((Pattern::End(written), _)) => {
                    empties.push(line);

                    *written
                }
                Some((Pattern::Atom(written, hypothesis), place)) => {
                    atoms.push((Some((place, hypothesis)), line));

                    *written
                }
                _ => {
                    empties.push(line.clone());
                    atoms.push((None, line));

                    continue;
                }
            };
            if grain.is_some_and(|grain| grain != written) {
                return Err(Error::MatrixMixedBinGrain);
            }
            grain = Some(written);
        }
        let grain = grain.expect("a column of packed sequences has a row that writes one");

        let cases = match grain {
            Grain::B => ["b[]", "b[_, .._]"],
            Grain::X => ["x[]", "x[_, .._]"],
        };
        self.sequence(
            context,
            Sequence::Bin(grain),
            (column, columns),
            (empties, atoms),
            cases,
            (top, mode),
        )
    }

    /// The fold over a list's or a packed sequence's column: the rows of its empty case and of its nonempty one, each the arm of its case over the columns left. `cases` is how a pattern writes the two.
    fn sequence(
        &mut self,
        context: &mut Context,
        sequence: Sequence,
        (column, rest): (Column, Vec<Column>),
        (empties, conses): (Vec<Line<'m>>, Vec<Peeled<'m>>),
        cases: [&'static str; 2],
        (top, mode): (bool, Mode),
    ) -> Result<(Term, Term), Error> {
        for (case, missing) in cases
            .into_iter()
            .zip([empties.is_empty(), conses.is_empty()])
        {
            if missing {
                return Err(self.uncovered(&column, Seen::Leaf(case.to_string()), false));
            }
        }

        let reads_hypothesis = self.reads_hypothesis(&conses, 2);
        let scrutinee = Scrutinee::read(context, &column.term)?;
        let motive = self.motive(context, top);
        let term = self.term;

        context.awaiting(|context| {
            let split = Rc::new(SequenceSplit::resolve(
                context,
                sequence,
                scrutinee,
                &motive,
                (term, &mode),
                reads_hypothesis,
            )?);
            self.resolved(top, || {
                (
                    Outermost::Sequence(Rc::clone(&split)),
                    column.term.clone(),
                    split.result_type(),
                )
            });

            self.seen[column.seen] = Seen::Leaf(cases[0].to_string());
            let empty = split.empty(context, |context, expected| {
                self.case(context, rest.clone(), empties, expected)
            })?;

            let (binders, group) = self.peel(context, conses);
            self.seen[column.seen] = Seen::Leaf(cases[1].to_string());
            let cons = split.cons(context, &binders, |context, expected| {
                self.case(context, rest, group, expected)
            })?;
            self.seen[column.seen] = Seen::Any;
            let (rebuilt, type_) = split.rebuild(empty, &binders, cons);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// The binders a fold's nonempty case is entered under — the atom ahead where the carrier has one, what is left, and the hypothesis — and the rows that reach the case with those binders read. The one row that writes the case names them by its own binders; where several write it they are the compiler's, each of those rows binding its names to them. A row that wrote a binder at the column reads none.
    fn peel<const N: usize>(
        &mut self,
        context: &mut Context,
        mut rows: Vec<Peeled<'m>>,
    ) -> ([Free; N], Vec<Line<'m>>) {
        let writing = rows
            .iter()
            .enumerate()
            .filter(|(_, (written, _))| written.is_some())
            .map(|(at, _)| at)
            .collect::<Vec<_>>();
        if let &[at] = writing.as_slice() {
            let (written, line) = &mut rows[at];
            let (place, bind) = written.expect("the row that writes the case");
            let binders = std::array::from_fn(|operand| match operand + 1 < N {
                true => {
                    let name = self.name(context, line, place + operand);
                    line.taken[place + operand] = Some(Term::free_var(&name));

                    name
                }
                false => self.hypothesis(context, line, bind, place + operand),
            });
            line.cases.extend(binders.iter().copied());

            return (binders, rows.into_iter().map(|(_, line)| line).collect());
        }

        let binders: [Free; N] = std::array::from_fn(|_| context.fresh(None));
        let mut group = Vec::with_capacity(rows.len());
        for (written, mut line) in rows {
            if let Some((place, bind)) = written {
                for (operand, binder) in binders[..N - 1].iter().enumerate() {
                    self.bind(context, &mut line, place + operand, Term::free_var(binder));
                }
                if let Some(bind) = bind {
                    let hypothesis = Term::free_var(&binders[N - 1]);
                    self.project(context, &mut line, bind, place + N - 1, hypothesis);
                }
                line.cases.extend(binders.iter().copied());
            }
            group.push(line);
        }

        (binders, group)
    }

    /// Whether the rows of a nonempty case may read its hypothesis, which is what makes their column a fold: `operands` is how many binders the case opens ahead of it. The one row that writes the case reads it where its body names the binder it writes for it, or where it takes it apart; rows that each write the case bind what they write to the one hypothesis, which reads it; and a body holding a written goal may name anything in scope. A row that wrote a binder at the column reads none.
    fn reads_hypothesis(&self, rows: &[Peeled<'m>], operands: usize) -> bool {
        let writing = rows
            .iter()
            .filter_map(|(written, line)| written.map(|written| (written, line)))
            .collect::<Vec<_>>();
        let written = match writing.as_slice() {
            [((place, bind), line)] => match bind {
                Some(Bind::Binder) => self.names(line.row, place + operands),
                Some(fields) => fields.binders() > 0,
                None => false,
            },
            writing => writing
                .iter()
                .any(|((_, bind), _)| bind.as_ref().is_some_and(|bind| bind.binders() > 0)),
        };

        written
            || writing
                .iter()
                .any(|(_, line)| holds_goal(&self.matrix.rows[line.row].body))
    }

    /// Whether the body of the row at `row` names its binder at `place`.
    fn names(&self, row: usize, place: usize) -> bool {
        let mut cursor = self.body(row).cursor();
        for _ in 0..place {
            let (_, domain) = cursor
                .entry()
                .expect("a row's body binds what its pattern writes");
            // A row's binder has no type written, so nothing after it reads what stands in its place here.
            cursor.advance(domain);
        }

        cursor.binder_used()
    }

    /// A tuple or struct column flattened into one column for each field, a projection of the scrutinee: a value of either has one shape, so matching one is reading its fields, and no row is put under a case by it. The rows that write the shape agree, field by field, on the label each is written with; a row that writes a binder binds its name to the column and reads no field. A struct pattern's head is the structure the column's type is.
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

        let written = |pattern: &'m Pattern| match pattern {
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields,
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
                line.cells[0].filter(|(pattern, _)| !matches!(pattern, Pattern::Binder))
            })
            .map(|(pattern, _)| pattern)
            .expect("a column is flattened for a row that writes its shape");
        let canonical = labelled(written(shape));
        let head = match shape {
            Pattern::Struct {
                head, structure, ..
            } => {
                held_to_structure(context, &column.term, head, structure)?;

                Some(head.clone())
            }
            _ => None,
        };

        let mut flattened = Vec::with_capacity(lines.len());
        for mut line in lines {
            let cells = match line.cells.remove(0) {
                Some((Pattern::Binder, place)) => {
                    self.named(context, &mut line, place, column.term.clone());

                    vec![None; canonical.len()]
                }
                Some((pattern, mut place)) => {
                    let fields = written(pattern);
                    if labelled(fields) != canonical {
                        return Err(Error::MatrixInconsistentShape);
                    }

                    fields
                        .iter()
                        .map(|(_, field)| {
                            let cell = Some((field, place));
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

    /// The binder a row alone writes for its hypothesis: the row's own where it writes a name, and one of the compiler's where it writes none or writes a pattern, whose leaves are bound to its projections.
    fn hypothesis(
        &mut self,
        context: &mut Context,
        line: &mut Line<'m>,
        bind: &Option<Bind>,
        place: usize,
    ) -> Free {
        match bind {
            Some(Bind::Binder) => {
                let name = self.name(context, line, place);
                line.taken[place] = Some(Term::free_var(&name));

                name
            }
            None => context.fresh(None),
            Some(fields) => {
                let hypothesis = context.fresh(None);
                self.project(context, line, fields, place, Term::free_var(&hypothesis));

                hypothesis
            }
        }
    }

    /// Bind what `bind` writes to `value`: a name to the value, and a pattern's leaves to its projections, by position or by label as each is written.
    fn project(
        &mut self,
        context: &mut Context,
        line: &mut Line<'m>,
        bind: &Bind,
        mut place: usize,
        value: Term,
    ) {
        match bind {
            Bind::Binder => self.bind(context, line, place, value),
            Bind::Fields(fields) | Bind::Struct { fields, .. } => {
                if let Bind::Struct {
                    head, structure, ..
                } = bind
                {
                    line.held.push((value.clone(), head.clone(), *structure));
                }
                for (index, (label, field)) in fields.iter().enumerate() {
                    let projected = projection(&value, label.as_deref(), index);
                    self.project(context, line, field, place, projected);
                    place += field.binders();
                }
            }
        }
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

        let body = line
            .lets
            .into_iter()
            .rev()
            .fold(body, |tail, (name, value)| {
                let hole = Term::hole(context.mint_metavar());

                Term::let_(&name, hole, value, tail)
            });

        // Each hypothesis written under a structure's name, as the one-row match over it whose row is the head over no field: the row takes nothing apart, and compiling it reads the head against the hypothesis's type, as an arm's pattern is read.
        line.held
            .into_iter()
            .rev()
            .fold(body, |tail, (value, head, structure)| {
                Term::matrix(Matrix {
                    head: value,
                    motive: None,
                    rows: vec![Row {
                        pattern: Pattern::Struct {
                            head,
                            structure,
                            fields: Vec::new(),
                        },
                        body: Term::func(Vec::<(Free, Term)>::new(), tail),
                    }],
                })
            })
    }

    /// The row at `row` elaborated by its own walk: its cases entered alone, standing on the frames held where the match is entered.
    fn walk(&mut self, context: &mut Context, row: usize) -> Result<Walked, Error> {
        if self.folds.is_none() {
            self.folds = Some(self.folds());
        }

        let held = self.held.clone();
        let line = self.line(row);
        let columns = vec![self.matrix.head.clone()];
        let mode = self.mode.clone();

        context.standing_on(&held, |context| {
            self.alone_over(context, columns, line, 0, true, mode)
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
            false => (None, Mode::Check(stands.clone()), true),
        }
    }

    /// One step of a row's own walk: `line` alone over `columns`. A name it writes is bound to its column, a tuple or a struct it writes is flattened, and a case it states is entered by the eliminator's own step, with the columns left as the arm; nothing is rebuilt. `fold` counts the cases a fold splits that the row has stated, which is where [`Compiler::folds`] says whether the next is a fold's, and `top` says it has stated no case yet. Answers the row's body elaborated where every column is read, with the binders of the cases it stated.
    fn alone_over(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        mut line: Line<'m>,
        fold: usize,
        top: bool,
        mode: Mode,
    ) -> Result<Walked, Error> {
        if columns.is_empty() {
            // A row that states no case at all is read as the row that cannot fail is: against the type the match stands at.
            let mode = match (top, &self.outermost) {
                (true, Some((_, _, stands))) => Mode::Check(stands.clone()),
                _ => mode,
            };
            let cases = line.cases.clone();
            let leaf = self.body_of(context, line);
            let (body, _) = elaborate(context, &leaf, mode)?;

            return Ok(Walked { cases, body });
        }

        let column = columns.remove(0);
        let (pattern, place) = line
            .cells
            .remove(0)
            .expect("a row alone writes every column it reads");
        let term = self.term;
        let row = line.row;

        match pattern {
            Pattern::Binder => {
                self.bind(context, &mut line, place, column);

                return recurse(|| self.alone_over(context, columns, line, fold, top, mode));
            }
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
                let mut cells = Vec::with_capacity(fields.len() + line.cells.len());
                let mut projections = Vec::with_capacity(fields.len() + columns.len());
                let mut place = place;
                for (index, (label, field)) in fields.iter().enumerate() {
                    cells.push(Some((field, place)));
                    place += field.binders();
                    projections.push(projection(&column, label.as_deref(), index));
                }
                cells.append(&mut line.cells);
                line.cells = cells;
                projections.extend(columns);

                return recurse(|| self.alone_over(context, projections, line, fold, top, mode));
            }
            _ => {}
        }

        let (shared, mode, awaits) = self.entering(top, &column, mode);
        let folds = |compiler: &Self| {
            compiler
                .folds
                .as_ref()
                .expect("the folds are read where a row is first walked")[row][fold]
        };

        // What the arm of the row's case comes to: the walk of the columns left, kept apart from the arm the eliminator closes over it.
        let mut walked = None;
        let mut arm = |compiler: &mut Self,
                       context: &mut Context,
                       columns: Vec<Term>,
                       line: Line<'m>,
                       fold: usize,
                       expected: Term| {
            let reached = recurse(|| {
                compiler.alone_over(context, columns, line, fold, false, Mode::Check(expected))
            })?;
            let body = reached.body.clone();
            walked = Some(reached);

            Ok(body)
        };

        match pattern {
            Pattern::Binder | Pattern::Tuple(_) | Pattern::Struct { .. } => {
                unreachable!("a pattern that states no case is read above")
            }
            Pattern::Variant { tag, args } => {
                let split = match shared {
                    Some(Outermost::Nominal(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let motive = self.motive(context, false);

                        Rc::new(NominalSplit::resolve(
                            context,
                            scrutinee,
                            &motive,
                            (term, &mode),
                            false,
                        )?)
                    }
                };
                let payload = split.payload(tag);
                let cells = payload_cells(tag, &payload, args, place)?;
                let (binders, slots) = self.alone(context, &mut line, cells);
                let mut payloads = slots
                    .into_iter()
                    .map(|slot| Term::free_var(&binders[slot]))
                    .collect::<Vec<_>>();
                payloads.extend(columns);

                walk_awaited(context, awaits, |context| {
                    split.arm(context, tag, payload, &binders, |context, expected| {
                        arm(self, context, payloads, line, fold, expected)
                    })
                })?;
            }
            Pattern::Bool(value) => {
                let split = match shared {
                    Some(Outermost::Bool(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let motive = self.motive(context, false);

                        Rc::new(BoolSplit::resolve(
                            context, scrutinee, &motive, term, &mode,
                        )?)
                    }
                };

                walk_awaited(context, awaits, |context| {
                    split.arm(context, *value, |context, expected| {
                        arm(self, context, columns, line, fold, expected)
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
                        let motive = self.motive(context, false);

                        Rc::new(SwitchSplit::resolve(
                            context, scrutinee, &motive, term, &mode,
                        )?)
                    }
                };

                walk_awaited(context, awaits, |context| {
                    split.arm(context, &value, |context, expected| {
                        arm(self, context, columns, line, fold, expected)
                    })
                })?;
            }
            Pattern::Zero | Pattern::Succ(_) => {
                let split = match shared {
                    // The tree's dispatch is in the form its rows chose: a `0` is a literal's case where no row peels a successor.
                    Some(Outermost::Switch(split)) => {
                        walk_awaited(context, awaits, |context| {
                            split.arm(context, &Natural::zero(), |context, expected| {
                                arm(self, context, columns, line, fold + 1, expected)
                            })
                        })?;

                        return Ok(walked.expect("the arm of a row's case is its walk"));
                    }
                    Some(Outermost::Nat(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let reads_hypothesis = folds(self);
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let motive = self.motive(context, false);

                        Rc::new(NatSplit::resolve(
                            context,
                            scrutinee,
                            &motive,
                            (term, &mode),
                            reads_hypothesis,
                        )?)
                    }
                };

                match pattern {
                    Pattern::Succ(bind) => {
                        let predecessor = self.name(context, &line, place);
                        line.taken[place] = Some(Term::free_var(&predecessor));
                        let hypothesis = self.hypothesis(context, &mut line, bind, place + 1);
                        line.cases.extend([predecessor, hypothesis]);

                        walk_awaited(context, awaits, |context| {
                            split.successor(
                                context,
                                &predecessor,
                                &hypothesis,
                                |context, expected| {
                                    arm(self, context, columns, line, fold + 1, expected)
                                },
                            )
                        })?;
                    }
                    _ => {
                        walk_awaited(context, awaits, |context| {
                            split.zero(context, |context, expected| {
                                arm(self, context, columns, line, fold + 1, expected)
                            })
                        })?;
                    }
                }
            }
            Pattern::Nil | Pattern::Cons(_) | Pattern::End(_) | Pattern::Atom(..) => {
                let sequence = match pattern {
                    Pattern::End(grain) | Pattern::Atom(grain, _) => Sequence::Bin(*grain),
                    _ => Sequence::List,
                };
                let split = match shared {
                    Some(Outermost::Sequence(split)) => split,
                    Some(_) => unreachable!("a column is split by one shape"),
                    None => {
                        let reads_hypothesis = folds(self);
                        let scrutinee = Scrutinee::read(context, &column)?;
                        let motive = self.motive(context, false);

                        Rc::new(SequenceSplit::resolve(
                            context,
                            sequence,
                            scrutinee,
                            &motive,
                            (term, &mode),
                            reads_hypothesis,
                        )?)
                    }
                };

                match pattern {
                    Pattern::Cons(bind) | Pattern::Atom(_, bind) => {
                        let head = self.name(context, &line, place);
                        line.taken[place] = Some(Term::free_var(&head));
                        let tail = self.name(context, &line, place + 1);
                        line.taken[place + 1] = Some(Term::free_var(&tail));
                        let hypothesis = self.hypothesis(context, &mut line, bind, place + 2);
                        let binders = [head, tail, hypothesis];
                        line.cases.extend(binders);

                        walk_awaited(context, awaits, |context| {
                            split.cons(context, &binders, |context, expected| {
                                arm(self, context, columns, line, fold + 1, expected)
                            })
                        })?;
                    }
                    _ => {
                        walk_awaited(context, awaits, |context| {
                            split.empty(context, |context, expected| {
                                arm(self, context, columns, line, fold + 1, expected)
                            })
                        })?;
                    }
                }
            }
        }

        Ok(walked.expect("the arm of a row's case is its walk"))
    }

    /// For each row, for each case a fold splits that its pattern writes — a `Nat`'s `0` and successor, a list's and a packed sequence's two — in the order the pattern writes them, whether some row may read a hypothesis at that place of the scrutinee: it writes one there, or its body holds a written goal.
    ///
    /// A row's own walk enters such a case as a fold's arm where this holds, and as a case split's where it does not. The tree makes a column a fold where a row of the group it splits reads its hypothesis ([`Compiler::reads_hypothesis`]), and every such group is of rows that write that place, so a row walked as a case split is under no fold, and one walked as a fold's arm is typed under less than a case split gives it: its body stands wherever the tree puts it.
    fn folds(&self) -> Vec<Vec<bool>> {
        let rows = &self.matrix.rows;

        let mut places = Vec::with_capacity(rows.len());
        let mut readers = BTreeSet::new();
        for row in rows {
            let goal = holds_goal(&row.body);
            let mut written = Vec::new();
            fold_places(&row.pattern, &mut Vec::new(), &mut |path, peeled| {
                let reads = peeled.is_some_and(|hypothesis| {
                    goal || hypothesis.is_some_and(|bind| bind.binders() > 0)
                });
                if reads {
                    readers.insert(path.to_vec());
                }
                written.push(path.to_vec());
            });
            places.push(written);
        }

        places
            .into_iter()
            .map(|written| written.iter().map(|path| readers.contains(path)).collect())
            .collect()
    }

    /// The body of the row at `row`, as the function over its binders it is.
    fn body(&self, row: usize) -> &'m Telescope<Term> {
        match &*self.matrix.rows[row].body {
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
        line.taken[place] = Some(Term::free_var(&name));
        line.lets.push((name, value));
    }

    /// The motive of one split: the written one at the outermost, and elsewhere, or where none is written, a hole the arms settle.
    fn motive(&self, context: &mut Context, top: bool) -> Scope<Many> {
        Term::match_motive_written(match (top, &self.matrix.motive) {
            (true, Some(motive)) => motive.clone(),
            _ => Term::hole(context.mint_metavar()),
        })
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
    debug_assert_eq!(labels.len(), row.pattern.binders());

    labels
}

/// Hold a struct pattern's head, written `head` and resolved to `structure`, to the structure the type of `column` is.
fn held_to_structure(
    context: &mut Context,
    column: &Term,
    head: &str,
    structure: &Free,
) -> Result<(), Error> {
    // A head that names nothing is reported as the name it is, by the reason the lowering filed for it.
    let Free::Global(structure) = structure else {
        return Err(context.unbound_variable(Term::free_var(structure)));
    };

    let (_, type_) = elaborate(context, column, Mode::Infer)?;
    let type_ = reduce_with(context, &type_)?;
    match &*type_ {
        Subterm::StructType(StructType { name, .. }) if name == structure => Ok(()),
        _ => Err(Error::MatrixStructHead {
            head: head.to_string(),
            scrutinee: Box::new(resolved_for_display(context, &type_)),
        }),
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
        .map(|fill| fill.map(|member| (&args[member].1, places[member])))
        .collect())
}

/// How many dispatches a pattern is refuted at: one for each constructor and each carrier leaf it writes, at any depth. A pattern refuted at none takes nothing apart that could fail.
fn refutations(pattern: &Pattern) -> usize {
    match pattern {
        Pattern::Binder => 0,
        Pattern::Variant { args, .. } => {
            1 + args.iter().map(|(_, arg)| refutations(arg)).sum::<usize>()
        }
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
            fields.iter().map(|(_, field)| refutations(field)).sum()
        }
        Pattern::Bool(_)
        | Pattern::Zero
        | Pattern::Succ(_)
        | Pattern::Literal(_)
        | Pattern::Character(_)
        | Pattern::Nil
        | Pattern::Cons(_)
        | Pattern::End(_)
        | Pattern::Atom(..) => 1,
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
        (_, Pattern::Binder) => true,
        (Pattern::Binder, that) => refutations(that) == 0,
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
                    None => instance(&Pattern::Binder, other),
                })
        }
        (Pattern::Tuple(fields), Pattern::Tuple(others))
        | (Pattern::Struct { fields, .. }, Pattern::Struct { fields: others, .. }) => fields
            .iter()
            .zip(others)
            .all(|((_, field), (_, other))| instance(field, other)),
        (Pattern::Tuple(_) | Pattern::Struct { .. }, _) => false,
        _ => true,
    }
}

/// The cases a fold splits that `pattern` writes, in the order it writes them: each with the path that reaches it from the scrutinee, and, for a nonempty one, the hypothesis it is written with.
fn fold_places<'m>(
    pattern: &'m Pattern,
    path: &mut Vec<Step>,
    visit: &mut impl FnMut(&[Step], Option<Option<&'m Bind>>),
) {
    match pattern {
        Pattern::Binder | Pattern::Bool(_) | Pattern::Literal(_) | Pattern::Character(_) => {}
        Pattern::Variant { tag, args } => {
            for ((plain, hidden), arg) in payload_places(args) {
                path.push(Step::Payload(tag.as_string(), plain, hidden));
                fold_places(arg, path, visit);
                path.pop();
            }
        }
        Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => {
            for (index, (_, field)) in fields.iter().enumerate() {
                path.push(Step::Field(index));
                fold_places(field, path, visit);
                path.pop();
            }
        }
        Pattern::Zero | Pattern::Nil | Pattern::End(_) => visit(path, None),
        Pattern::Succ(bind) | Pattern::Cons(bind) | Pattern::Atom(_, bind) => {
            visit(path, Some(bind.as_ref()))
        }
    }
}

/// One step from a value to a part of it a pattern reads: a payload of a constructor, by where it stands among the plain ones and, for a hidden one, among the hidden ones written ahead of the next plain one, or a field of a tuple or a struct.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Step {
    Payload(String, usize, Option<usize>),
    Field(usize),
}
