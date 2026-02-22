use {
    super::{Name, Term},
    std::collections::{HashSet, VecDeque},
};

pub struct Comparator {
    entropy: usize,
    history: HashSet<(Term, Term)>,
    pending: VecDeque<(Term, Term)>,
}

impl Comparator {
    pub fn new(this: Term, that: Term) -> Self {
        Self {
            entropy: 0,
            history: HashSet::new(),
            pending: VecDeque::from([(this, that)]),
        }
    }

    fn fresh_name(&mut self) -> Name {
        let fresh = self.entropy.to_string();

        self.entropy += 1;

        Name::free(fresh)
    }

    fn in_history(&mut self, this: &Term, that: &Term) -> bool {
        !self.history.insert((this.clone(), that.clone()))
    }

    fn enqueue(&mut self, this: &Term, that: &Term) {
        self.pending.push_front((this.clone(), that.clone()))
    }

    fn dequeue(&mut self) -> Option<(Term, Term)> {
        self.pending.pop_back()
    }

    fn compare(&mut self) -> bool {
        while let Some((this, that)) = self.dequeue() {
            match (this.reduce(), that.reduce()) {
                (Term::Type, Term::Type) => {}
                (
                    ref this @ Term::FuncType(ref this_term, ref this_scope),
                    ref that @ Term::FuncType(ref that_term, ref that_scope),
                ) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    self.enqueue(this_term, that_term);

                    let name = Term::Name(self.fresh_name());

                    self.enqueue(
                        &this_scope.clone().open(&[&name]),
                        &that_scope.clone().open(&[&name]),
                    );
                }
                (ref this @ Term::Func(ref this_scope), ref that @ Term::Func(ref that_scope)) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    let name = Term::Name(self.fresh_name());

                    self.enqueue(
                        &this_scope.clone().open(&[&name]),
                        &that_scope.clone().open(&[&name]),
                    );
                }
                (
                    ref this @ Term::Apply(ref this_head, ref this_param),
                    ref that @ Term::Apply(ref that_head, ref that_param),
                ) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    self.enqueue(this_head, that_head);
                    self.enqueue(this_param, that_param);
                }
                (
                    ref this @ Term::PairType(ref this_term, ref this_scope),
                    ref that @ Term::PairType(ref that_term, ref that_scope),
                ) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    self.enqueue(this_term, that_term);

                    let name = Term::Name(self.fresh_name());

                    self.enqueue(
                        &this_scope.clone().open(&[&name]),
                        &that_scope.clone().open(&[&name]),
                    );
                }
                (
                    ref this @ Term::Pair(ref this_left, ref this_right),
                    ref that @ Term::Pair(ref that_left, ref that_right),
                ) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    self.enqueue(this_left, that_left);
                    self.enqueue(this_right, that_right);
                }
                (
                    ref this @ Term::Split(ref this_head, ref this_scope),
                    ref that @ Term::Split(ref that_head, ref that_scope),
                ) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    self.enqueue(this_head, that_head);

                    let left_name = Term::Name(self.fresh_name());
                    let right_name = Term::Name(self.fresh_name());
                    let pair = Term::Pair(left_name.clone().into(), right_name.clone().into());

                    let this_term = this_scope
                        .clone()
                        .open(&[&left_name.clone(), &right_name.clone()])
                        .rewrite(|term| (term == this_head.as_ref()).then(|| pair.clone()));

                    let that_term = that_scope
                        .clone()
                        .open(&[&left_name.clone(), &right_name.clone()])
                        .rewrite(|term| (term == that_head.as_ref()).then(|| pair.clone()));

                    self.enqueue(&this_term, &that_term);
                }
                (Term::AtomType(this_atoms), Term::AtomType(that_atoms)) => {
                    if this_atoms != that_atoms {
                        return false;
                    }
                }
                (Term::Atom(this_atom), Term::Atom(that_atom)) => {
                    if this_atom != that_atom {
                        return false;
                    }
                }
                (
                    ref this @ Term::Match(ref this_head, ref this_cases),
                    ref that @ Term::Match(ref that_head, ref that_cases),
                ) => {
                    if self.in_history(this, that) {
                        continue;
                    }

                    if this_cases.len() != that_cases.len() {
                        return false;
                    }

                    self.enqueue(this_head, that_head);

                    for ((this_atom, this_body), (that_atom, that_body)) in
                        this_cases.iter().zip(that_cases.iter())
                    {
                        if this_atom != that_atom {
                            return false;
                        }

                        let atom = Term::Atom(this_atom.clone());

                        let this_term = this_body
                            .as_ref()
                            .clone()
                            .rewrite(|term| (term == this_head.as_ref()).then(|| atom.clone()));

                        let that_term = that_body
                            .as_ref()
                            .clone()
                            .rewrite(|term| (term == that_head.as_ref()).then(|| atom.clone()));

                        self.enqueue(&this_term, &that_term);
                    }
                }
                (Term::Bind(this_bindings, this_body), that_term) => {
                    let projections = this_bindings
                        .iter()
                        .map(|(_, body)| Term::Bind(this_bindings.clone(), body.clone()))
                        .collect::<Vec<_>>();

                    self.enqueue(
                        &this_body.open(&projections.iter().collect::<Vec<_>>()),
                        &that_term,
                    );
                }
                (this_term, Term::Bind(that_bindings, that_body)) => {
                    let projections = that_bindings
                        .iter()
                        .map(|(_, body)| Term::Bind(that_bindings.clone(), body.clone()))
                        .collect::<Vec<_>>();

                    self.enqueue(
                        &this_term,
                        &that_body.open(&projections.iter().collect::<Vec<_>>()),
                    );
                }
                (Term::Name(this_name), Term::Name(that_name)) => {
                    if this_name != that_name {
                        return false;
                    }
                }
                (_, _) => return false,
            }
        }

        true
    }
}

pub fn compare(this: Term, that: Term) -> bool {
    Comparator::new(this, that).compare()
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::core::Atom,
        std::{
            env,
            process::{Command, Stdio},
            thread,
            time::Duration,
        },
    };

    const PATHOLOGIC_RECURSION_TIMES_OUT_TIMEOUT: Duration = Duration::from_secs(1);

    #[test]
    #[ignore]
    fn pathologic_recursion_times_out_unsafe() {
        let this = Term::bind(vec![("x", Term::Type, Term::free("x"))], Term::free("x"));
        let that = Term::bind(vec![("y", Term::Type, Term::free("y"))], Term::free("y"));
        assert!(compare(this, that));
    }

    #[test]
    fn pathologic_recursion_times_out() {
        let test_name = concat!(module_path!(), "::pathologic_recursion_times_out_unsafe");

        let test_name = test_name
            .strip_prefix(concat!(env!("CARGO_PKG_NAME"), "::"))
            .unwrap_or(test_name);

        let mut child = Command::new(env::current_exe().unwrap())
            .arg("--quiet")
            .arg("--exact")
            .arg("--ignored")
            .arg(test_name)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();

        thread::sleep(PATHOLOGIC_RECURSION_TIMES_OUT_TIMEOUT);
        child.kill().unwrap();

        assert!(!child.wait().unwrap().success());
    }

    #[test]
    fn split_rewrites_head_in_body_before_comparing() {
        let this = Term::func(
            "p",
            Term::split(
                Term::free("p"),
                "x",
                "y",
                Term::r#match(
                    Term::free("p"),
                    [
                        (Atom::from("left"), Term::free("x")),
                        (Atom::from("right"), Term::free("y")),
                    ],
                ),
            ),
        );

        let that = Term::func(
            "p",
            Term::split(
                Term::free("p"),
                "x",
                "y",
                Term::r#match(
                    Term::pair(Term::free("x"), Term::free("y")),
                    [
                        (Atom::from("left"), Term::free("x")),
                        (Atom::from("right"), Term::free("y")),
                    ],
                ),
            ),
        );

        assert!(compare(this, that));
    }

    #[test]
    fn match_rewrites_branch_bodies_with_case_atom() {
        let this = Term::func(
            "p",
            Term::r#match(
                Term::free("p"),
                [
                    (Atom::from("left"), Term::free("p")),
                    (Atom::from("right"), Term::free("p")),
                ],
            ),
        );

        let that = Term::func(
            "p",
            Term::r#match(
                Term::free("p"),
                [
                    (Atom::from("left"), Term::atom("left")),
                    (Atom::from("right"), Term::atom("right")),
                ],
            ),
        );

        assert!(compare(this, that));
    }

    #[test]
    fn match_rewrite_detects_wrong_branch() {
        let this = Term::func(
            "p",
            Term::r#match(
                Term::free("p"),
                [
                    (Atom::from("left"), Term::free("p")),
                    (Atom::from("right"), Term::free("p")),
                ],
            ),
        );

        let that = Term::func(
            "p",
            Term::r#match(
                Term::free("p"),
                [
                    (Atom::from("left"), Term::atom("left")),
                    (Atom::from("right"), Term::atom("left")),
                ],
            ),
        );

        assert!(!compare(this, that));
    }

    #[test]
    fn bind_unfolds_against_non_bind_in_both_directions() {
        let this = Term::bind(vec![("x", Term::Type, Term::atom("left"))], Term::free("x"));
        let that = Term::atom("left");
        assert!(compare(this.clone(), that.clone()));
        assert!(compare(that, this));
    }
}
