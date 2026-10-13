//! The matrix compiler: a match as it is written, compiled into the single-level matches the eliminators type.
//!
//! [`elaborate_matrix`] takes the [`Matrix`] lowering wrote and builds what a person would get from nesting matches by hand: rows are grouped by the constructor in the leftmost column and the constructor's arguments become new columns, a column in which every row is a binder is retired with each name bound to it, a tuple or struct column is flattened into projections, and a final top-level `_`, or what a bind arm falls through to, is the default of every dispatch it reaches. The tree is then elaborated as any term is.
//!
//! Arms have no priority: a binder beside a concrete pattern in one column of one group is refused, and so are two rows alike in every column.

use {
    super::{Mode, elaborate},
    crate::{Context, Error},
    curios_core::{Atom, Bind, Free, Many, Matrix, Pattern, Row, Scope, Subterm, Term, Var},
    curios_num::{Grain, Natural},
    curios_utilities::Plicity,
    std::collections::BTreeMap,
};

/// Compile `matrix` and elaborate the tree it comes to, which stands where the match is written.
pub(super) fn elaborate_matrix(
    context: &mut Context,
    matrix: &Matrix,
    term: &Term,
    mode: Mode,
) -> Result<(Term, Term), Error> {
    let compiled = Compiler::new(context, matrix).whole()?;
    let compiled = match term.extent() {
        Some(extent) => compiled.with_extent(extent),
        None => compiled,
    };

    elaborate(context, &compiled, mode)
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

struct Compiler<'c, 'k, 'm> {
    context: &'c mut Context<'k>,
    matrix: &'m Matrix,
    /// Each row's binders, as its body labels them.
    labels: Vec<Vec<Label>>,
    /// What a case no row covers falls through to, the same at every dispatch of one matrix: a headed match's final `_`, or the rest of a bind arm's ladder. `None` is full enumeration, where a nominal match leans on index inversion for a constructor no row names and a carrier requires both of its cases.
    default: Option<Term>,
}

impl<'c, 'k, 'm> Compiler<'c, 'k, 'm> {
    fn new(context: &'c mut Context<'k>, matrix: &'m Matrix) -> Self {
        Self {
            context,
            matrix,
            labels: matrix.rows.iter().map(labels).collect(),
            default: None,
        }
    }

    /// The whole matrix: its rows over its scrutinee, with what it falls through to where it has one. A bind arm falls through to the rest of its ladder, and a headed match to its final `_` where its other arms dispatch.
    fn whole(mut self) -> Result<Term, Error> {
        let matrix = self.matrix;
        let rows = matrix.rows.len();

        if let Some(rest) = &matrix.fallthrough {
            let refuted = matrix
                .rows
                .iter()
                .map(|row| refutations(&row.pattern))
                .sum::<usize>();

            return self.defaulted(rows, rest.clone(), refuted > 1);
        }

        match catch_all(&matrix.rows) {
            true => {
                // The final `_` binds nothing its body can name, so its one binder stands for anything.
                let default = self.body(rows - 1).open(&[&matrix.head]);
                let shared = !flat(&matrix.rows[..rows - 1]);

                self.defaulted(rows - 1, default, shared)
            }
            false => self.arms(rows),
        }
    }

    /// The first `arms` rows with `default` where no row covers a case. Where `shared`, the default is bound once as a function of no argument and called from each dispatch it reaches, so it is written once and evaluated only where a value falls through to it.
    fn defaulted(&mut self, arms: usize, default: Term, shared: bool) -> Result<Term, Error> {
        if !shared {
            self.default = Some(default);

            return self.arms(arms);
        }

        let fallthrough = self.context.fresh(None);
        self.default = Some(Term::apply(
            Term::var(Var::free(fallthrough)),
            Vec::<Term>::new(),
        ));
        let compiled = self.arms(arms)?;
        let thunk = Term::func(Vec::<(Free, Term)>::new(), default);
        let hole = Term::hole(self.context.mint_metavar());

        Ok(Term::let_(&fallthrough, hole, thunk, compiled))
    }

    /// The first `arms` rows over the scrutinee. With no row the match is a vacuous elimination, which has no column to read a dispatch off and is the nominal one.
    fn arms(&mut self, arms: usize) -> Result<Term, Error> {
        let head = self.matrix.head.clone();
        if arms == 0 {
            let motive = self.motive(true);
            let cases = Vec::<(Atom, _, Term)>::new();

            return Ok(Term::induct_match_scoped_marked(
                head,
                motive,
                cases,
                self.default.clone(),
            ));
        }

        let lines = (0..arms)
            .map(|row| Line {
                row,
                patterns: vec![(&self.matrix.rows[row].pattern, 0)],
                taken: vec![None; self.labels[row].len()],
                lets: Vec::new(),
            })
            .collect();

        self.columns(vec![head], lines, true)
    }

    /// One step: the leftmost column is retired where every row writes a binder, flattened where every row writes a tuple or a struct, and otherwise split by what the rows write. `top` says the column is the scrutinee itself, which is where a written motive attaches.
    fn columns(
        &mut self,
        mut columns: Vec<Term>,
        mut lines: Vec<Line<'m>>,
        top: bool,
    ) -> Result<Term, Error> {
        if columns.is_empty() {
            // The rows keep the order of their arms, so the first is the arm a value reaches and the one after it the first that none does.
            return match <[Line<'m>; 1]>::try_from(lines) {
                Ok([line]) => Ok(self.leaf(line)),
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
                self.bind(line, place, scrutinee.clone());
            }

            return self.columns(columns, lines, false);
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

                self.variants(columns, lines, top)
            }
            Pattern::Tuple(_) => {
                if !every(
                    |first, other| matches!((first, other), (Pattern::Tuple(first), Pattern::Tuple(other)) if first.len() == other.len()),
                ) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.fields(columns, lines)
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

                self.fields(columns, lines)
            }
            Pattern::Bool(_) => {
                if !every(|_, other| matches!(other, Pattern::Bool(_))) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.booleans(columns, lines, top)
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

                self.naturals(columns, lines, top)
            }
            Pattern::Nil | Pattern::Cons(_) => {
                if !every(|_, other| matches!(other, Pattern::Nil | Pattern::Cons(_))) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.lists(columns, lines, top)
            }
            Pattern::End(_) | Pattern::Atom(..) => {
                if !every(|_, other| matches!(other, Pattern::End(_) | Pattern::Atom(..))) {
                    return Err(Error::MatrixInconsistentShape);
                }

                self.packed(columns, lines, top)
            }
            Pattern::Binder => unreachable!("a binder beside a concrete pattern is refused above"),
        }
    }

    /// Rows grouped by the constructor they name, each group over the payload's columns and then the columns left.
    ///
    /// A group of one row names a payload it binds by the row's own binder, as a match written one level deep does; a payload a pattern takes apart, and every payload of a group of several rows, is a binder of the compiler's.
    fn variants(
        &mut self,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
    ) -> Result<Term, Error> {
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

        let mut cases = Vec::with_capacity(groups.len());
        for (_, (tag, mut group)) in groups {
            // Rows under one constructor agree on how many payloads they write and on the mark of each.
            let plicities = group[0]
                .0
                .iter()
                .map(|(plicity, _)| *plicity)
                .collect::<Vec<_>>();
            if group.iter().any(|(args, ..)| {
                args.iter()
                    .map(|(plicity, _)| *plicity)
                    .ne(plicities.iter().copied())
            }) {
                return Err(Error::MatrixInconsistentShape);
            }

            if group.len() == 1 {
                let (args, mut place, mut line) = group.pop().expect("a group of one row");
                let mut binders = Vec::with_capacity(args.len());
                let mut payload_columns = Vec::new();
                let mut payload_patterns = Vec::new();
                for (plicity, pattern) in args {
                    match pattern {
                        Pattern::Binder => {
                            let name = self.name(&line, place);
                            line.taken[place] = Some(Term::free_var(&name));
                            binders.push((*plicity, name));
                        }
                        other => {
                            let payload = self.context.fresh(None);
                            payload_columns.push(Term::free_var(&payload));
                            payload_patterns.push((other, place));
                            binders.push((*plicity, payload));
                        }
                    }
                    place += pattern.binders();
                }
                payload_patterns.append(&mut line.patterns);
                line.patterns = payload_patterns;
                payload_columns.extend(rest.iter().cloned());

                let body = self.columns(payload_columns, vec![line], false)?;
                cases.push((tag, binders, body));
                continue;
            }

            let payloads = plicities
                .iter()
                .map(|_| self.context.fresh(None))
                .collect::<Vec<_>>();
            let group = group
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
                .collect();
            let mut payload_columns = payloads.iter().map(Term::free_var).collect::<Vec<_>>();
            payload_columns.extend(rest.iter().cloned());

            let body = self.columns(payload_columns, group, false)?;
            cases.push((tag, plicities.into_iter().zip(payloads).collect(), body));
        }

        let motive = self.motive(top);

        Ok(Term::induct_match_scoped_marked(
            scrutinee,
            motive,
            cases,
            self.default.clone(),
        ))
    }

    /// Rows split by the `Bool` they write. `Bool` has no vacuity to excuse a case, so both are required where nothing stands in for one.
    fn booleans(
        &mut self,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
    ) -> Result<Term, Error> {
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

        if self.default.is_none() {
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

        let motive = self.motive(top);
        let false_case = match falses.is_empty() {
            true => self.fallen(),
            false => self.columns(rest.clone(), falses, false)?,
        };
        let true_case = match trues.is_empty() {
            true => self.fallen(),
            false => self.columns(rest, trues, false)?,
        };

        Ok(Term::bool_match_scoped(
            scrutinee, motive, false_case, true_case,
        ))
    }

    /// Rows over a `Nat` column, in one of two forms its rows choose: induction where a row peels a successor, `0` and `n + 1` with a hypothesis, and dispatch otherwise, over the literals written with the default as what any other value takes. A literal beside a successor is refused: no one form peels a successor and dispatches on a value.
    fn naturals(
        &mut self,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
    ) -> Result<Term, Error> {
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

        let motive = self.motive(top);

        if successors.is_empty() {
            // No set of literals covers every natural, so a dispatch has a default.
            let Some(default) = self.default.clone() else {
                return Err(Error::MatrixNatDispatchNeedsDefault);
            };
            let mut groups: BTreeMap<Natural, Vec<Line<'m>>> = BTreeMap::new();
            for line in zeros {
                groups.entry(Natural::zero()).or_default().push(line);
            }
            for (value, line) in literals {
                groups.entry(value).or_default().push(line);
            }
            let mut cases = Vec::with_capacity(groups.len());
            for (value, group) in groups {
                cases.push((value, self.columns(rest.clone(), group, false)?));
            }

            return Ok(Term::switch_scoped(scrutinee, motive, cases, default));
        }

        if !literals.is_empty() {
            return Err(Error::MatrixMixedNatDispatch);
        }
        if zeros.is_empty() && self.default.is_none() {
            return Err(Error::MatrixIncompleteCarrierMatch {
                carrier: "Nat",
                missing: "0",
            });
        }

        let zero_case = match zeros.is_empty() {
            true => self.fallen(),
            false => self.columns(rest.clone(), zeros, false)?,
        };

        let (predecessor, hypothesis, successor_case) = if successors.len() == 1 {
            let (place, bind, mut line) = successors.pop().expect("one row peels a successor");
            let predecessor = self.name(&line, place);
            line.taken[place] = Some(Term::free_var(&predecessor));
            let hypothesis = self.hypothesis(&mut line, bind, place + 1);
            let successor_case = self.columns(rest, vec![line], false)?;

            (predecessor, hypothesis, successor_case)
        } else {
            let predecessor = self.context.fresh(None);
            let hypothesis = self.context.fresh(None);
            let mut group = Vec::with_capacity(successors.len());
            for (place, bind, mut line) in successors {
                self.bind(&mut line, place, Term::free_var(&predecessor));
                self.shared_hypothesis(&mut line, bind, place + 1, &hypothesis);
                group.push(line);
            }
            let successor_case = self.columns(rest, group, false)?;

            (predecessor, hypothesis, successor_case)
        };

        Ok(Term::nat_match_scoped(
            scrutinee,
            motive,
            zero_case,
            &predecessor,
            &hypothesis,
            successor_case,
        ))
    }

    /// Rows split into `[]` and `[head, ..tail]`, both required where nothing stands in for one.
    fn lists(
        &mut self,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
    ) -> Result<Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

        let mut empties = Vec::new();
        let mut conses: Vec<Peeled<'m>> = Vec::new();
        for mut line in lines {
            match line.patterns.remove(0) {
                (Pattern::Nil, _) => empties.push(line),
                (Pattern::Cons(hypothesis), place) => conses.push((place, hypothesis, line)),
                _ => unreachable!("every row of the column writes a list"),
            }
        }

        if self.default.is_none() {
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

        let motive = self.motive(top);
        let empty_case = match empties.is_empty() {
            true => self.fallen(),
            false => self.columns(rest.clone(), empties, false)?,
        };
        let (head, tail, hypothesis, cons_case) = self.cons(conses, rest)?;

        Ok(Term::list_match_scoped(
            scrutinee,
            Term::hole(self.context.mint_metavar()),
            motive,
            empty_case,
            &[&head, &tail, &hypothesis],
            cons_case,
        ))
    }

    /// Rows split into the empty packed sequence and one atom ahead of a tail, at the one grain every row writes, both required where nothing stands in for one.
    fn packed(
        &mut self,
        mut columns: Vec<Term>,
        lines: Vec<Line<'m>>,
        top: bool,
    ) -> Result<Term, Error> {
        let scrutinee = columns.remove(0);
        let rest = columns;

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

        if self.default.is_none() {
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

        let motive = self.motive(top);
        let empty_case = match empties.is_empty() {
            true => self.fallen(),
            false => self.columns(rest.clone(), empties, false)?,
        };
        let (head, tail, hypothesis, atom_case) = self.cons(atoms, rest)?;

        Ok(Term::bin_match_scoped(
            grain,
            scrutinee,
            motive,
            empty_case,
            &[&head, &tail, &hypothesis],
            atom_case,
        ))
    }

    /// The arm of a list's or a packed sequence's nonempty case over `rows`, with the binders it is closed over: the head, the tail and the hypothesis. No row is the default, whose binders nothing names; one row names the three by its own binders; and several share three of the compiler's, each row binding its names to them.
    fn cons(
        &mut self,
        mut rows: Vec<Peeled<'m>>,
        rest: Vec<Term>,
    ) -> Result<(Free, Free, Free, Term), Error> {
        if rows.is_empty() {
            return Ok((
                self.context.fresh(None),
                self.context.fresh(None),
                self.context.fresh(None),
                self.fallen(),
            ));
        }

        if rows.len() == 1 {
            let (place, bind, mut line) = rows.pop().expect("one row writes the case");
            let head = self.name(&line, place);
            line.taken[place] = Some(Term::free_var(&head));
            let tail = self.name(&line, place + 1);
            line.taken[place + 1] = Some(Term::free_var(&tail));
            let hypothesis = self.hypothesis(&mut line, bind, place + 2);
            let case = self.columns(rest, vec![line], false)?;

            return Ok((head, tail, hypothesis, case));
        }

        let head = self.context.fresh(None);
        let tail = self.context.fresh(None);
        let hypothesis = self.context.fresh(None);
        let mut group = Vec::with_capacity(rows.len());
        for (place, bind, mut line) in rows {
            self.bind(&mut line, place, Term::free_var(&head));
            self.bind(&mut line, place + 1, Term::free_var(&tail));
            self.shared_hypothesis(&mut line, bind, place + 2, &hypothesis);
            group.push(line);
        }
        let case = self.columns(rest, group, false)?;

        Ok((head, tail, hypothesis, case))
    }

    /// A tuple or struct column flattened into one column for each field, a projection of the scrutinee: a value of either has one shape, so matching one is reading its fields. Rows agree, field by field, on the label each is written with.
    fn fields(&mut self, mut columns: Vec<Term>, lines: Vec<Line<'m>>) -> Result<Term, Error> {
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

        self.columns(projections, flattened, false)
    }

    /// The binder a single row's hypothesis is: the row's own where it writes a name, and one of the compiler's where it writes none or writes a pattern, whose leaves are bound to its projections.
    fn hypothesis(&mut self, line: &mut Line<'m>, bind: &Option<Bind>, place: usize) -> Free {
        match bind {
            Some(Bind::Binder) => {
                let name = self.name(line, place);
                line.taken[place] = Some(Term::free_var(&name));

                name
            }
            None => self.context.fresh(None),
            Some(fields) => {
                let hypothesis = self.context.fresh(None);
                self.project(line, fields, place, Term::free_var(&hypothesis));

                hypothesis
            }
        }
    }

    /// What a row among several binds of the hypothesis they share: nothing where it writes none, its name, or the leaves of its pattern.
    fn shared_hypothesis(
        &mut self,
        line: &mut Line<'m>,
        bind: &Option<Bind>,
        place: usize,
        shared: &Free,
    ) {
        if let Some(bind) = bind {
            self.project(line, bind, place, Term::free_var(shared));
        }
    }

    /// Bind what `bind` writes to `value`: a name to the value, and a pattern's leaves to its projections, by position or by label as each is written.
    fn project(&mut self, line: &mut Line<'m>, bind: &Bind, mut place: usize, value: Term) {
        match bind {
            Bind::Binder => self.bind(line, place, value),
            Bind::Fields(fields) => {
                for (index, (label, field)) in fields.iter().enumerate() {
                    let projected = match label {
                        Some(label) => Term::proj_label(value.clone(), label.clone()),
                        None => Term::proj_position(value.clone(), index),
                    };
                    self.project(line, field, place, projected);
                    place += field.binders();
                }
            }
        }
    }

    /// A row with every column read: its body over what its binders stand for, under the binders bound ahead of it.
    fn leaf(&mut self, line: Line<'m>) -> Term {
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

        line.lets
            .into_iter()
            .rev()
            .fold(body, |tail, (name, value)| {
                let hole = Term::hole(self.context.mint_metavar());

                Term::let_(&name, hole, value, tail)
            })
    }

    /// The body of the row at `row`, as the function over its binders it is.
    fn body(&self, row: usize) -> &'m curios_core::Telescope<Term> {
        match &*self.matrix.rows[row].body {
            Subterm::Func(func) => &func.telescope,
            _ => unreachable!("a row's body is a function over its binders"),
        }
    }

    /// A fresh local for the binder at `place` of the row `line` is, under the label the row wrote it with.
    fn name(&mut self, line: &Line<'m>, place: usize) -> Free {
        let (hint, written) = &self.labels[line.row][place];

        self.context.fresh_written(hint.as_deref(), *written)
    }

    /// Bind the binder at `place` to `value` ahead of the row's body.
    fn bind(&mut self, line: &mut Line<'m>, place: usize, value: Term) {
        let name = self.name(line, place);
        line.taken[place] = Some(Term::free_var(&name));
        line.lets.push((name, value));
    }

    /// The motive of one split: the written one at the scrutinee's, and elsewhere, or where none is written, a hole the arms settle.
    fn motive(&mut self, top: bool) -> Scope<Many> {
        Term::match_motive_written(match (top, &self.matrix.motive) {
            (true, Some(motive)) => motive.clone(),
            _ => Term::hole(self.context.mint_metavar()),
        })
    }

    /// What a case with no row falls through to.
    fn fallen(&self) -> Term {
        self.default
            .clone()
            .expect("a case with no row is refused where nothing stands in for it")
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

/// Whether every constructor arm binds each payload by a name: such arms reach a default at the scrutinee's dispatch alone.
fn flat(rows: &[Row]) -> bool {
    rows.iter().all(|row| match &row.pattern {
        Pattern::Variant { args, .. } => args.iter().all(|(_, arg)| matches!(arg, Pattern::Binder)),
        _ => true,
    })
}

/// How many dispatches a pattern is refuted at: one for each constructor and each carrier leaf it writes, at any depth.
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
