//! The matrix compiler: a match as it is written, compiled into the single-level matches the eliminators rebuild.
//!
//! [`elaborate_matrix`] takes the [`Matrix`] lowering wrote and builds what a person would get from nesting matches by hand: rows are grouped by the constructor in the leftmost column and the constructor's arguments become new columns, a column in which every row is a binder is retired with each name bound to it, a tuple or struct column is flattened into projections, and a final top-level `_`, or what a bind arm falls through to, is the default of every dispatch it reaches.
//!
//! Each dispatch is an eliminator's own, taken a step at a time ([`NominalSplit`] and its kin): the column is resolved as a scrutinee, each case is entered as an arm of it is, with the rows of that case over the columns left as the arm's body, and the match is rebuilt from the arms. So what an arm assumes is stated once, for the tree and for a match written one level deep, and a row's body is elaborated where its last column is read, in the frames its cases opened.
//!
//! The row that cannot fail states no case, so it is read where the match stands and at the match's own type, once, after the rows before it: written in place where one leaf reaches it, and bound once as a function of no argument, called from each, where more do.
//!
//! Arms have no priority: a binder beside a concrete pattern in one column of one group is refused, and so are two rows alike in every column.

use {
    super::{
        BoolSplit, Mode, NatSplit, NominalSplit, Scrutinee, Sequence, SequenceSplit, SwitchSplit,
        check, elaborate, expect, holds_goal, payload_fills,
    },
    crate::{Context, Error},
    curios_core::{
        Atom, Bind, Free, InductArm, Many, Matrix, Pattern, Row, Scope, Subterm, Telescope, Term,
    },
    curios_num::{Grain, Natural},
    curios_utilities::{Plicity, recurse},
    std::collections::BTreeMap,
};

/// Compile `matrix`, which `term` holds, into the tree of matches it comes to, each elaborated as it is built.
pub(super) fn elaborate_matrix(
    context: &mut Context,
    matrix: &Matrix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let compiler = Compiler {
        matrix,
        term,
        labels: matrix.rows.iter().map(labels).collect(),
        fallthrough: None,
    };

    compiler.whole(context, mode)
}

/// One of a row's binders as its body's telescope labels it: the name it is written with, and where the lowering wrote it among its declaration's binders.
type Label = (Option<String>, Option<u32>);

/// A group of rows under one constructor: each with the patterns it writes for the payload and the place of the first binder among them.
type Group<'m> = Vec<(&'m [(Plicity, Pattern)], usize, Line<'m>)>;

/// A row under a successor or a cons: the place of its first binder, the hypothesis it writes, and the row.
type Peeled<'m> = (usize, &'m Option<Bind>, Line<'m>);

/// One row while its columns are compiled.
struct Line<'m> {
    /// Which of the matrix's rows it is.
    row: usize,
    /// The patterns still to be read, one for each column left, each beside the place of the first binder it writes among its row's.
    patterns: Vec<(&'m Pattern, usize)>,
    /// What each of the row's binders stands for, once its pattern is read.
    taken: Vec<Option<Term>>,
    /// The binders bound ahead of the body, each to what it stands for, in the order they were read: the first is outermost.
    lets: Vec<(Free, Term)>,
}

struct Compiler<'m> {
    matrix: &'m Matrix,
    /// The match as it is written, which a report of one of its dispatches is placed at.
    term: &'m Term,
    /// Each row's binders, as its body labels them.
    labels: Vec<Vec<Label>>,
    /// What a case no row covers falls through to, the same at every dispatch of one matrix: a headed match's final `_`, or the rest of a bind arm's ladder. `None` is full enumeration, where a nominal match leans on index inversion for a constructor no row names and a carrier requires both of its cases.
    fallthrough: Option<Fallthrough>,
}

/// The row that cannot fail, while the rows before it are read.
struct Fallthrough {
    /// Its body, as lowering wrote it.
    body: Term,
    /// What stands at each leaf that reaches it until it is read.
    stand_in: Free,
    /// How many leaves reach it.
    reached: usize,
    /// The match's own type, which it is read at, once the outermost dispatch is resolved.
    stands: Option<Term>,
}

impl<'m> Compiler<'m> {
    /// The whole matrix: its rows over its scrutinee, with what it falls through to where it has one. A bind arm falls through to the rest of its ladder, and a headed match to its final `_` where its other arms dispatch.
    fn whole(mut self, context: &mut Context, mode: Mode) -> Result<(Term, Term), Error> {
        let matrix = self.matrix;
        let rows = matrix.rows.len();

        let (arms, body) = match &matrix.fallthrough {
            Some(rest) => (rows, Some(rest.clone())),
            // The final `_` binds nothing its body can name, so its one binder stands for anything.
            None if catch_all(&matrix.rows) => {
                (rows - 1, Some(self.body(rows - 1).open(&[&matrix.head])))
            }
            None => (rows, None),
        };
        self.fallthrough = body.map(|body| Fallthrough {
            body,
            stand_in: context.fresh(None),
            reached: 0,
            stands: None,
        });

        self.arms(context, arms, mode)
    }

    /// The first `arms` rows over the scrutinee. With no row the match is a vacuous elimination, which has no column to read a dispatch off and is the nominal one.
    fn arms(
        &mut self,
        context: &mut Context,
        arms: usize,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let head = self.matrix.head.clone();
        if arms == 0 {
            return self.variants(context, vec![head], Vec::new(), true, mode);
        }

        let lines = (0..arms)
            .map(|row| Line {
                row,
                patterns: vec![(&self.matrix.rows[row].pattern, 0)],
                taken: vec![None; self.labels[row].len()],
                lets: Vec::new(),
            })
            .collect();

        self.columns(context, vec![head], lines, true, mode)
    }

    /// One step: the leftmost column is retired where every row writes a binder, flattened where every row writes a tuple or a struct, and otherwise split by what the rows write. `top` says no dispatch stands above the column: its dispatch is where a written motive attaches, the scrutinee being what a match that writes one dispatches on, and where what the matrix falls through to is read, and `mode` is how what the rows come to is read: as the match is at its scrutinee, and against the type of its case beneath a dispatch.
    fn columns(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        mut lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        if columns.is_empty() {
            // The rows keep the order of their arms, so the first is the arm a value reaches and the one after it the first that none does.
            return match <[Line<'m>; 1]>::try_from(lines) {
                Ok([line]) => self.leaf(context, line, mode),
                Err(lines) => Err(Error::MatrixDuplicateRow.at_opt(
                    lines
                        .get(1)
                        .and_then(|line| self.matrix.rows[line.row].body.extent()),
                )),
            };
        }

        if lines
            .iter()
            .all(|line| matches!(line.patterns[0].0, Pattern::Binder))
        {
            let scrutinee = columns.remove(0);
            for line in &mut lines {
                let (_, place) = line.patterns.remove(0);
                self.bind(context, line, place, scrutinee.clone());
            }

            return recurse(|| self.columns(context, columns, lines, top, mode));
        }

        if lines
            .iter()
            .any(|line| matches!(line.patterns[0].0, Pattern::Binder))
        {
            return Err(Error::MatrixInconsistentShape);
        }

        let every = |alike: fn(&Pattern, &Pattern) -> bool| {
            lines
                .iter()
                .all(|line| alike(lines[0].patterns[0].0, line.patterns[0].0))
        };
        match lines[0].patterns[0].0 {
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
                            Pattern::Struct { head: first_head, fields: first },
                            Pattern::Struct { head: other_head, fields: other },
                        ) if first_head == other_head && first.len() == other.len()
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
            Pattern::Binder => unreachable!("a binder beside a concrete pattern is refused above"),
        }
    }

    /// The arm of one case: its rows over the columns left, read against the type the case is typed at, or what the matrix falls through to where no row is of the case.
    fn case(
        &mut self,
        context: &mut Context,
        columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        expected: Term,
    ) -> Result<Term, Error> {
        match lines.is_empty() {
            true => self.fallen(context, expected),
            // Guarded by [`recurse`]: a row is read a pattern at a time, so the depth is that of the patterns a reader wrote.
            false => {
                recurse(|| self.columns(context, columns, lines, false, Mode::Check(expected)))
                    .map(|(arm, _)| arm)
            }
        }
    }

    /// What a case with no row falls through to: a stand-in for the row that cannot fail, which is read once the rows before it are ([`Compiler::settled`]). The match's own type, which that row is read at, is held to the type of the case here, where the cases above are in view.
    fn fallen(&mut self, context: &mut Context, expected: Term) -> Result<Term, Error> {
        let fallthrough = self
            .fallthrough
            .as_mut()
            .expect("a case with no row is refused where nothing stands in for it");
        fallthrough.reached += 1;
        let stand_in = Term::free_var(&fallthrough.stand_in);
        let stands = fallthrough
            .stands
            .clone()
            .expect("a case is entered beneath the outermost dispatch");
        expect(context, self.term, &stands, &expected)?;

        Ok(stand_in)
    }

    /// Note the type the match stands at, where its outermost dispatch is resolved.
    fn stand(&mut self, top: bool, type_: impl FnOnce() -> Term) {
        if let Some(fallthrough) = self.fallthrough.as_mut().filter(|_| top) {
            fallthrough.stands = Some(type_());
        }
    }

    /// The match a dispatch rebuilt, at the type `type_` it stands at. Beneath another dispatch, and where the matrix falls through to nothing, it is the match as rebuilt. The outermost one takes what the matrix falls through to: the row that cannot fail states no case, so it is typed where the match stands and against the match's own type, once, after the rows before it. It is written in place where one leaf reaches it; where more do, it is bound once as a function of no argument and called from each, so it is written once and evaluated only where a value falls through to it.
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
        // No leaf reaches it, so nothing holds it.
        if fallthrough.reached == 0 {
            return Ok((rebuilt, type_));
        }

        let body = check(context, &fallthrough.body, type_.clone())?;
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

    /// Rows grouped by the constructor they name, each group the arm of its case, over the payload's columns and then the columns left. The arms are elaborated in the order the family declares its constructors, as a match written one level deep has them.
    fn variants(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut groups: BTreeMap<String, (Atom, Group<'m>)> = BTreeMap::new();
        for mut line in lines {
            let (Pattern::Variant { tag, args }, place) = line.patterns.remove(0) else {
                unreachable!("every row of the column writes a constructor");
            };
            groups
                .entry(tag.as_string())
                .or_insert_with(|| (tag.clone(), Vec::new()))
                .1
                .push((args.as_slice(), place, line));
        }

        // Rows under one constructor agree on how many payloads they write and on the mark of each.
        for (_, group) in groups.values() {
            let marks = |args: &[(Plicity, Pattern)]| {
                args.iter().map(|(plicity, _)| *plicity).collect::<Vec<_>>()
            };
            let written = marks(group[0].0);
            if group.iter().any(|(args, ..)| marks(args) != written) {
                return Err(Error::MatrixInconsistentShape);
            }
        }

        let scrutinee = Scrutinee::read(context, &scrutinee)?;
        let motive = self.motive(context, top);
        let term = self.term;
        let defaulted = self.fallthrough.is_some();

        context.awaiting(|context| {
            let vacuous = groups.is_empty() && !defaulted;
            let split = NominalSplit::resolve(context, scrutinee, &motive, (term, &mode), vacuous)?;
            self.stand(top, || split.result_type());
            let undecided = split.admit(context, groups.is_empty(), defaulted)?;
            split.declared(groups.values().map(|(tag, _)| tag))?;

            let mut arms = Vec::with_capacity(groups.len());
            for tag in split.constructors() {
                let Some((_, group)) = groups.remove(&tag.as_string()) else {
                    // A default covers every constructor no row names; with none, an arm may be left out only where inversion rules its case out.
                    if defaulted || split.ruled_out(context, tag)? {
                        continue;
                    }

                    return Err(split.missing(tag));
                };

                arms.push(self.variant(context, &split, tag, group, &rest)?);
            }

            let default = match defaulted {
                true => Some(
                    split.default(context, |context, expected| self.fallen(context, expected))?,
                ),
                false => None,
            };

            let (rebuilt, type_) = split.rebuild(context, arms, default, undecided, term)?;

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// The arm of `tag`'s case over the rows that name it.
    ///
    /// A group of one row names a payload it binds by the row's own binder, as a match written one level deep does; a payload a pattern takes apart, a payload no row writes, and every payload of a group of several rows, is a binder of the compiler's.
    fn variant(
        &mut self,
        context: &mut Context,
        split: &NominalSplit,
        tag: &Atom,
        mut group: Group<'m>,
        rest: &[Term],
    ) -> Result<(Atom, InductArm), Error> {
        let written = group[0]
            .0
            .iter()
            .map(|(plicity, _)| *plicity)
            .collect::<Vec<_>>();
        let payload = split.payload(tag);
        let fills = payload_fills(tag, &payload, &written)?;

        let mut binders = Vec::with_capacity(fills.len());
        let mut columns = Vec::new();
        let lines = if group.len() == 1 {
            let (args, first, mut line) = group.pop().expect("a group of one row");
            let mut places = Vec::with_capacity(args.len());
            let mut place = first;
            for (_, pattern) in args {
                places.push(place);
                place += pattern.binders();
            }

            let mut patterns = Vec::new();
            for fill in &fills {
                let binder = match fill.map(|member| (&args[member].1, places[member])) {
                    Some((Pattern::Binder, place)) => {
                        let name = self.name(context, &line, place);
                        line.taken[place] = Some(Term::free_var(&name));

                        name
                    }
                    Some((pattern, place)) => {
                        let payload = context.fresh(None);
                        columns.push(Term::free_var(&payload));
                        patterns.push((pattern, place));

                        payload
                    }
                    None => context.fresh(None),
                };
                binders.push(binder);
            }
            patterns.append(&mut line.patterns);
            line.patterns = patterns;

            vec![line]
        } else {
            for fill in &fills {
                let payload = context.fresh(None);
                if fill.is_some() {
                    columns.push(Term::free_var(&payload));
                }
                binders.push(payload);
            }

            group
                .into_iter()
                .map(|(args, mut place, mut line)| {
                    let mut patterns = Vec::with_capacity(args.len() + line.patterns.len());
                    for (_, pattern) in args {
                        patterns.push((pattern, place));
                        place += pattern.binders();
                    }
                    patterns.append(&mut line.patterns);
                    line.patterns = patterns;

                    line
                })
                .collect()
        };
        columns.extend(rest.iter().cloned());

        split.arm(context, tag, payload, &binders, |context, expected| {
            self.case(context, columns, lines, expected)
        })
    }

    /// Rows split by the `Bool` they write. `Bool` has no vacuity to excuse a case, so both are required where nothing stands in for one.
    fn booleans(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut falses = Vec::new();
        let mut trues = Vec::new();
        for mut line in lines {
            let (Pattern::Bool(value), _) = line.patterns.remove(0) else {
                unreachable!("every row of the column writes a `Bool`");
            };
            match value {
                false => falses.push(line),
                true => trues.push(line),
            }
        }

        if self.fallthrough.is_none() {
            let missing = match (falses.is_empty(), trues.is_empty()) {
                (true, _) => Some("false"),
                (_, true) => Some("true"),
                _ => None,
            };
            if let Some(missing) = missing {
                return Err(Error::MatrixIncompleteCarrierMatch {
                    carrier: "Bool",
                    missing,
                });
            }
        }

        let scrutinee = Scrutinee::read(context, &scrutinee)?;
        let motive = self.motive(context, top);
        let term = self.term;

        context.awaiting(|context| {
            let split = BoolSplit::resolve(context, scrutinee, &motive, term, &mode)?;
            self.stand(top, || split.result_type());
            let false_case = split.arm(context, false, |context, expected| {
                self.case(context, rest.clone(), falses, expected)
            })?;
            let true_case = split.arm(context, true, |context, expected| {
                self.case(context, rest, trues, expected)
            })?;

            let (rebuilt, type_) = split.rebuild(false_case, true_case);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// Rows over a `Nat` column, in one of two forms its rows choose: induction where a row peels a successor, `0` and `n + 1` with a hypothesis, and dispatch otherwise, over the literals written with the default as what any other value takes. A literal beside a successor is refused: no one form peels a successor and dispatches on a value.
    fn naturals(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut zeros = Vec::new();
        let mut successors: Vec<Peeled<'m>> = Vec::new();
        let mut literals: Vec<(Natural, Line<'m>)> = Vec::new();
        for mut line in lines {
            match line.patterns.remove(0) {
                (Pattern::Zero, _) => zeros.push(line),
                (Pattern::Succ(hypothesis), place) => successors.push((place, hypothesis, line)),
                (Pattern::Literal(value), _) => literals.push((value.clone(), line)),
                (Pattern::Character(character), _) => {
                    literals.push((Natural::from(*character as u32), line))
                }
                _ => unreachable!("every row of the column writes a `Nat`"),
            }
        }

        let term = self.term;

        if successors.is_empty() {
            // No set of literals covers every natural, so a dispatch has a default.
            if self.fallthrough.is_none() {
                return Err(Error::MatrixNatDispatchNeedsDefault);
            }
            let mut groups: BTreeMap<Natural, Vec<Line<'m>>> = BTreeMap::new();
            for line in zeros {
                groups.entry(Natural::zero()).or_default().push(line);
            }
            for (value, line) in literals {
                groups.entry(value).or_default().push(line);
            }

            let scrutinee = Scrutinee::read(context, &scrutinee)?;
            let motive = self.motive(context, top);

            return context.awaiting(|context| {
                let split = SwitchSplit::resolve(context, scrutinee, &motive, term, &mode)?;
                self.stand(top, || split.result_type());
                let mut cases = Vec::with_capacity(groups.len());
                for (value, group) in groups {
                    let arm = split.arm(context, &value, |context, expected| {
                        self.case(context, rest.clone(), group, expected)
                    })?;
                    cases.push((value, arm));
                }
                let default =
                    split.default(context, |context, expected| self.fallen(context, expected))?;

                let (rebuilt, type_) = split.rebuild(cases, default);

                self.settled(context, top, rebuilt, type_)
            });
        }

        if !literals.is_empty() {
            return Err(Error::MatrixMixedNatDispatch);
        }
        if zeros.is_empty() && self.fallthrough.is_none() {
            return Err(Error::MatrixIncompleteCarrierMatch {
                carrier: "Nat",
                missing: "0",
            });
        }

        let reads_hypothesis = self.reads_hypothesis(&successors, 1);
        let scrutinee = Scrutinee::read(context, &scrutinee)?;
        let motive = self.motive(context, top);

        context.awaiting(|context| {
            let split =
                NatSplit::resolve(context, scrutinee, &motive, (term, &mode), reads_hypothesis)?;
            self.stand(top, || split.result_type());
            let zero = split.zero(context, |context, expected| {
                self.case(context, rest.clone(), zeros, expected)
            })?;

            let (predecessor, hypothesis, group) = if successors.len() == 1 {
                let (place, bind, mut line) = successors.pop().expect("one row peels a successor");
                let predecessor = self.name(context, &line, place);
                line.taken[place] = Some(Term::free_var(&predecessor));
                let hypothesis = self.hypothesis(context, &mut line, bind, place + 1);

                (predecessor, hypothesis, vec![line])
            } else {
                let predecessor = context.fresh(None);
                let hypothesis = context.fresh(None);
                let mut group = Vec::with_capacity(successors.len());
                for (place, bind, mut line) in successors {
                    self.bind(context, &mut line, place, Term::free_var(&predecessor));
                    self.shared_hypothesis(context, &mut line, bind, place + 1, &hypothesis);
                    group.push(line);
                }

                (predecessor, hypothesis, group)
            };
            let successor =
                split.successor(context, &predecessor, &hypothesis, |context, expected| {
                    self.case(context, rest, group, expected)
                })?;

            let (rebuilt, type_) = split.rebuild(zero, &predecessor, &hypothesis, successor);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// Rows split into `[]` and `[head, ..tail]`, both required where nothing stands in for one.
    fn lists(
        &mut self,
        context: &mut Context,
        columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let mut empties = Vec::new();
        let mut conses: Vec<Peeled<'m>> = Vec::new();
        for mut line in lines {
            match line.patterns.remove(0) {
                (Pattern::Nil, _) => empties.push(line),
                (Pattern::Cons(hypothesis), place) => conses.push((place, hypothesis, line)),
                _ => unreachable!("every row of the column writes a list"),
            }
        }

        if self.fallthrough.is_none() {
            let missing = match (empties.is_empty(), conses.is_empty()) {
                (true, _) => Some("[]"),
                (_, true) => Some("[head, ..tail]"),
                _ => None,
            };
            if let Some(missing) = missing {
                return Err(Error::MatrixIncompleteCarrierMatch {
                    carrier: "List",
                    missing,
                });
            }
        }

        self.sequence(
            context,
            Sequence::List,
            columns,
            (empties, conses),
            top,
            mode,
        )
    }

    /// Rows split into the empty packed sequence and one atom ahead of a tail, at the one grain every row writes, both required where nothing stands in for one.
    fn packed(
        &mut self,
        context: &mut Context,
        columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let mut grain: Option<Grain> = None;
        let mut empties = Vec::new();
        let mut atoms: Vec<Peeled<'m>> = Vec::new();
        for mut line in lines {
            let (pattern, place) = line.patterns.remove(0);
            let written = match pattern {
                Pattern::End(written) | Pattern::Atom(written, _) => *written,
                _ => unreachable!("every row of the column writes a packed sequence"),
            };
            if grain.is_some_and(|grain| grain != written) {
                return Err(Error::MatrixMixedBinGrain);
            }
            grain = Some(written);
            match pattern {
                Pattern::Atom(_, hypothesis) => atoms.push((place, hypothesis, line)),
                _ => empties.push(line),
            }
        }
        let grain = grain.expect("a column of packed sequences has a row");

        if self.fallthrough.is_none() {
            // The grain the rows wrote names the carrier, since no surface type is the family of both.
            let (carrier, empty, atom) = match grain {
                Grain::B => ("Bits", "b[]", "b[head, ..tail]"),
                Grain::X => ("Bytes", "x[]", "x[head, ..tail]"),
            };
            let missing = match (empties.is_empty(), atoms.is_empty()) {
                (true, _) => Some(empty),
                (_, true) => Some(atom),
                _ => None,
            };
            if let Some(missing) = missing {
                return Err(Error::MatrixIncompleteCarrierMatch { carrier, missing });
            }
        }

        self.sequence(
            context,
            Sequence::Bin(grain),
            columns,
            (empties, atoms),
            top,
            mode,
        )
    }

    /// The fold over a list's or a packed sequence's column: the rows of its empty case and of its nonempty one, each the arm of its case over the columns left.
    fn sequence(
        &mut self,
        context: &mut Context,
        sequence: Sequence,
        mut columns: Vec<Term>,
        (empties, conses): (Vec<Line<'m>>, Vec<Peeled<'m>>),
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let reads_hypothesis = self.reads_hypothesis(&conses, 2);
        let scrutinee = Scrutinee::read(context, &scrutinee)?;
        let motive = self.motive(context, top);
        let term = self.term;

        context.awaiting(|context| {
            let split = SequenceSplit::resolve(
                context,
                sequence,
                scrutinee,
                &motive,
                (term, &mode),
                reads_hypothesis,
            )?;
            self.stand(top, || split.result_type());
            let empty = split.empty(context, |context, expected| {
                self.case(context, rest.clone(), empties, expected)
            })?;

            let (binders, group) = self.peel(context, conses);
            let cons = split.cons(context, &binders, |context, expected| {
                self.case(context, rest, group, expected)
            })?;

            let (rebuilt, type_) = split.rebuild(empty, &binders, cons);

            self.settled(context, top, rebuilt, type_)
        })
    }

    /// The binders a list's or a packed sequence's nonempty case is entered under — the head, the tail and the hypothesis — and its rows with those binders read. No row leaves the case to the default, whose binders nothing names; one row names the three by its own binders; and several share three of the compiler's, each row binding its names to them.
    fn peel(
        &mut self,
        context: &mut Context,
        mut rows: Vec<Peeled<'m>>,
    ) -> ([Free; 3], Vec<Line<'m>>) {
        if rows.len() == 1 {
            let (place, bind, mut line) = rows.pop().expect("one row writes the case");
            let head = self.name(context, &line, place);
            line.taken[place] = Some(Term::free_var(&head));
            let tail = self.name(context, &line, place + 1);
            line.taken[place + 1] = Some(Term::free_var(&tail));
            let hypothesis = self.hypothesis(context, &mut line, bind, place + 2);

            return ([head, tail, hypothesis], vec![line]);
        }

        let head = context.fresh(None);
        let tail = context.fresh(None);
        let hypothesis = context.fresh(None);
        let mut group = Vec::with_capacity(rows.len());
        for (place, bind, mut line) in rows {
            self.bind(context, &mut line, place, Term::free_var(&head));
            self.bind(context, &mut line, place + 1, Term::free_var(&tail));
            self.shared_hypothesis(context, &mut line, bind, place + 2, &hypothesis);
            group.push(line);
        }

        ([head, tail, hypothesis], group)
    }

    /// Whether the rows of a nonempty case may read its hypothesis, which is what makes their column a fold: `operands` is how many binders the case opens ahead of it. A lone row reads it where its body names the binder it writes for it, or where it takes it apart; rows that share the case each bind what they write to the one hypothesis, which reads it; and a body holding a written goal may name anything in scope.
    fn reads_hypothesis(&self, rows: &[Peeled<'m>], operands: usize) -> bool {
        let written = match rows {
            [(place, bind, line)] => match bind {
                Some(Bind::Binder) => self.names(line.row, place + operands),
                Some(fields) => fields.binders() > 0,
                None => false,
            },
            rows => rows
                .iter()
                .any(|(_, bind, _)| bind.as_ref().is_some_and(|bind| bind.binders() > 0)),
        };

        written
            || rows
                .iter()
                .any(|(.., line)| holds_goal(&self.matrix.rows[line.row].body))
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

    /// A tuple or struct column flattened into one column for each field, a projection of the scrutinee: a value of either has one shape, so matching one is reading its fields. Rows agree, field by field, on the label each is written with.
    fn fields(
        &mut self,
        context: &mut Context,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let written = |pattern: &'m Pattern| match pattern {
            Pattern::Tuple(fields) | Pattern::Struct { fields, .. } => fields,
            _ => unreachable!("every row of the column writes a tuple or a struct"),
        };
        let labelled = |fields: &'m [(Option<String>, Pattern)]| {
            fields
                .iter()
                .map(|(label, _)| label.as_deref())
                .collect::<Vec<_>>()
        };
        let canonical = labelled(written(lines[0].patterns[0].0));

        let mut flattened = Vec::with_capacity(lines.len());
        for mut line in lines {
            let (pattern, mut place) = line.patterns.remove(0);
            let fields = written(pattern);
            if labelled(fields) != canonical {
                return Err(Error::MatrixInconsistentShape);
            }

            let mut patterns = Vec::with_capacity(fields.len() + line.patterns.len());
            for (_, field) in fields {
                patterns.push((field, place));
                place += field.binders();
            }
            patterns.append(&mut line.patterns);
            line.patterns = patterns;
            flattened.push(line);
        }

        let mut projections = canonical
            .into_iter()
            .enumerate()
            .map(|(index, label)| match label {
                Some(label) => Term::proj_label(scrutinee.clone(), label.to_string()),
                None => Term::proj_position(scrutinee.clone(), index),
            })
            .collect::<Vec<_>>();
        projections.extend(rest);

        recurse(|| self.columns(context, projections, flattened, top, mode))
    }

    /// The binder a single row's hypothesis is: the row's own where it writes a name, and one of the compiler's where it writes none or writes a pattern, whose leaves are bound to its projections.
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

    /// What a row among several binds of the hypothesis they share: nothing where it writes none, its name, or the leaves of its pattern.
    fn shared_hypothesis(
        &mut self,
        context: &mut Context,
        line: &mut Line<'m>,
        bind: &Option<Bind>,
        place: usize,
        shared: &Free,
    ) {
        if let Some(bind) = bind {
            self.project(context, line, bind, place, Term::free_var(shared));
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
            Bind::Fields(fields) => {
                for (index, (label, field)) in fields.iter().enumerate() {
                    let projected = match label {
                        Some(label) => Term::proj_label(value.clone(), label.clone()),
                        None => Term::proj_position(value.clone(), index),
                    };
                    self.project(context, line, field, place, projected);
                    place += field.binders();
                }
            }
        }
    }

    /// A row with every column read: its body over what its binders stand for, under the binders bound ahead of it, elaborated where it stands.
    fn leaf(
        &mut self,
        context: &mut Context,
        line: Line<'m>,
        mode: Mode,
    ) -> Result<(Term, Term), Error> {
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
        let leaf = line
            .lets
            .into_iter()
            .rev()
            .fold(body, |tail, (name, value)| {
                let hole = Term::hole(context.mint_metavar());

                Term::let_(&name, hole, value, tail)
            });

        elaborate(context, &leaf, mode)
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

    /// Bind the binder at `place` to `value` ahead of the row's body.
    fn bind(&self, context: &mut Context, line: &mut Line<'m>, place: usize, value: Term) {
        let (hint, written) = &self.labels[line.row][place];
        let name = context.fresh_written(hint.as_deref(), *written);
        line.taken[place] = Some(Term::free_var(&name));
        line.lets.push((name, value));
    }

    /// The motive of one split: the written one at the scrutinee's, and elsewhere, or where none is written, a hole the arms settle.
    fn motive(&self, context: &mut Context, top: bool) -> Scope<Many> {
        Term::match_motive_written(match (top, &self.matrix.motive) {
            (true, Some(motive)) => motive.clone(),
            _ => Term::hole(context.mint_metavar()),
        })
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

/// Whether the rows end in a default: a final binder after arms that each dispatch. A lone binder is a binding and no default, and so is one after tuple or struct arms, which project and leave no case uncovered.
fn catch_all(rows: &[Row]) -> bool {
    match rows.split_last() {
        Some((last, arms)) => {
            !arms.is_empty()
                && arms.iter().all(|row| row.pattern.dispatches())
                && matches!(last.pattern, Pattern::Binder)
        }
        None => false,
    }
}
