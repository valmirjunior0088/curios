use {
    super::{Name, Term},
    std::collections::{HashMap, HashSet},
};

pub struct Converts {
    entropy: usize,
    history: HashSet<(Term, Term)>,
}

impl Converts {
    pub fn new() -> Self {
        Self {
            entropy: 0,
            history: HashSet::new(),
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

    fn converts(&mut self, this: Term, that: Term) -> bool {
        if self.in_history(&this, &that) {
            return true;
        }

        match (this.reduce(), that.reduce()) {
            (Term::Type, Term::Type) => true,
            (Term::FuncType(this_term, this_scope), Term::FuncType(that_term, that_scope)) => {
                self.converts(*this_term, *that_term) && {
                    let name = Term::Name(self.fresh_name());

                    self.converts(this_scope.open(&[&name]), that_scope.open(&[&name]))
                }
            }
            (Term::Func(this_scope), Term::Func(that_scope)) => {
                let name = Term::Name(self.fresh_name());

                self.converts(this_scope.open(&[&name]), that_scope.open(&[&name]))
            }
            (Term::Apply(this_head, this_param), Term::Apply(that_head, that_param)) => {
                self.converts(*this_head, *that_head) && self.converts(*this_param, *that_param)
            }
            (Term::PairType(this_term, this_scope), Term::PairType(that_term, that_scope)) => {
                self.converts(*this_term, *that_term) && {
                    let name = Term::Name(self.fresh_name());

                    self.converts(this_scope.open(&[&name]), that_scope.open(&[&name]))
                }
            }
            (Term::Pair(this_left, this_right), Term::Pair(that_left, that_right)) => {
                self.converts(*this_left, *that_left) && self.converts(*this_right, *that_right)
            }
            (Term::Split(this_head, this_scope), Term::Split(that_head, that_scope)) => {
                if !self.converts(*this_head.clone(), *that_head.clone()) {
                    return false;
                }

                let left_name = Term::Name(self.fresh_name());
                let right_name = Term::Name(self.fresh_name());

                let this_term = this_scope
                    .open(&[&left_name.clone(), &right_name.clone()])
                    .rewrite(|term| {
                        (term == this_head.as_ref()).then(|| {
                            Term::Pair(left_name.clone().into(), right_name.clone().into())
                        })
                    });

                let that_term = that_scope
                    .open(&[&left_name.clone(), &right_name.clone()])
                    .rewrite(|term| {
                        (term == that_head.as_ref()).then(|| {
                            Term::Pair(left_name.clone().into(), right_name.clone().into())
                        })
                    });

                self.converts(this_term, that_term)
            }
            (Term::AtomType(this_atoms), Term::AtomType(that_atoms)) => this_atoms == that_atoms,
            (Term::Atom(this_atom), Term::Atom(that_atom)) => this_atom == that_atom,
            (Term::Match(this_head, this_cases), Term::Match(that_head, that_cases)) => {
                if !self.converts(*this_head.clone(), *that_head.clone()) {
                    return false;
                }

                if this_cases.len() != that_cases.len() {
                    return false;
                }

                for (atom, this_body) in this_cases {
                    let Some(that_body) = that_cases.get(&atom) else {
                        return false;
                    };

                    let atom = Term::Atom(atom.clone());

                    let this_term = this_body
                        .rewrite(|term| (term == this_head.as_ref()).then(|| atom.clone()));

                    let that_term = that_body
                        .clone()
                        .rewrite(|term| (term == that_head.as_ref()).then(|| atom.clone()));

                    if !self.converts(this_term, that_term) {
                        return false;
                    }
                }

                true
            }
            (Term::Bind(this_bindings, this_body), Term::Bind(that_bindings, that_body)) => {
                if this_bindings.len() != that_bindings.len() {
                    return false;
                }

                let names = (0..this_bindings.len())
                    .map(|_| Term::Name(self.fresh_name()))
                    .collect::<Vec<_>>();

                let names = names.iter().collect::<Vec<_>>();

                for ((this_kind, this_value), (that_kind, that_value)) in
                    this_bindings.into_iter().zip(that_bindings)
                {
                    if !self.converts(*this_kind, *that_kind) {
                        return false;
                    }

                    if !self.converts(this_value.open(&names), that_value.open(&names)) {
                        return false;
                    }
                }

                self.converts(this_body.open(&names), that_body.open(&names))
            }
            (Term::Bind(this_bindings, this_body), that_term) => {
                let names = (0..this_bindings.len())
                    .map(|_| Term::Name(self.fresh_name()))
                    .collect::<Vec<_>>();

                let names = names.iter().collect::<Vec<_>>();

                let values = this_bindings
                    .into_iter()
                    .map(|(_, this_value)| this_value.open(&names))
                    .collect::<Vec<_>>();

                let this_term = this_body.open(&names);

                let substitutions = names.into_iter().zip(values).collect::<HashMap<_, _>>();

                self.converts(
                    this_term.rewrite(|term| substitutions.get(term).cloned()),
                    that_term,
                )
            }
            (this_term, Term::Bind(that_bindings, that_body)) => {
                let names = (0..that_bindings.len())
                    .map(|_| Term::Name(self.fresh_name()))
                    .collect::<Vec<_>>();

                let names = names.iter().collect::<Vec<_>>();

                let values = that_bindings
                    .into_iter()
                    .map(|(_, that_value)| that_value.open(&names))
                    .collect::<Vec<_>>();

                let that_term = that_body.open(&names);

                let substitutions = names.into_iter().zip(values).collect::<HashMap<_, _>>();

                self.converts(
                    this_term,
                    that_term.rewrite(|term| substitutions.get(term).cloned()),
                )
            }
            (Term::Name(this_name), Term::Name(that_name)) => this_name == that_name,
            (_, _) => false,
        }
    }
}

pub fn converts(this: Term, that: Term) -> bool {
    Converts::new().converts(this, that)
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::core::{Atom, Many, One, Scope, Two},
        std::collections::BTreeMap,
    };

    #[test]
    fn alpha_equivalence() {
        let this = Term::Bind(
            vec![(
                Term::Type.into(),
                Scope::close(Many(1), &["x"], Term::from("x")),
            )],
            Scope::close(Many(1), &["x"], Term::from("x")),
        );

        let that = Term::Bind(
            vec![(
                Term::Type.into(),
                Scope::close(Many(1), &["y"], Term::from("y")),
            )],
            Scope::close(Many(1), &["y"], Term::from("y")),
        );

        assert!(converts(this, that));
    }

    #[test]
    fn split_rewrites_head_in_body_before_comparing() {
        let this = Term::Func(Scope::close(
            One,
            &["p"],
            Term::Split(
                Term::from("p").into(),
                Scope::close(
                    Two,
                    &["x", "y"],
                    Term::Match(
                        Term::from("p").into(),
                        BTreeMap::from([
                            (Atom::from("left"), Term::from("x").into()),
                            (Atom::from("right"), Term::from("y").into()),
                        ]),
                    ),
                ),
            ),
        ));

        let that = Term::Func(Scope::close(
            One,
            &["p"],
            Term::Split(
                Term::from("p").into(),
                Scope::close(
                    Two,
                    &["x", "y"],
                    Term::Match(
                        Term::Pair(Term::from("x").into(), Term::from("y").into()).into(),
                        BTreeMap::from([
                            (Atom::from("left"), Term::from("x").into()),
                            (Atom::from("right"), Term::from("y").into()),
                        ]),
                    ),
                ),
            ),
        ));

        assert!(converts(this, that));
    }

    #[test]
    fn match_rewrites_branch_bodies_with_case_atom() {
        let this = Term::Func(Scope::close(
            One,
            &["p"],
            Term::Match(
                Term::from("p").into(),
                BTreeMap::from([
                    (Atom::from("left"), Term::from("p").into()),
                    (Atom::from("right"), Term::from("p").into()),
                ]),
            ),
        ));

        let that = Term::Func(Scope::close(
            One,
            &["p"],
            Term::Match(
                Term::from("p").into(),
                BTreeMap::from([
                    (Atom::from("left"), Term::Atom(Atom::from("left")).into()),
                    (Atom::from("right"), Term::Atom(Atom::from("right")).into()),
                ]),
            ),
        ));

        assert!(converts(this, that));
    }

    #[test]
    fn match_rewrite_detects_wrong_branch() {
        let this = Term::Func(Scope::close(
            One,
            &["p"],
            Term::Match(
                Term::from("p").into(),
                BTreeMap::from([
                    (Atom::from("left"), Term::from("p").into()),
                    (Atom::from("right"), Term::from("p").into()),
                ]),
            ),
        ));

        let that = Term::Func(Scope::close(
            One,
            &["p"],
            Term::Match(
                Term::from("p").into(),
                BTreeMap::from([
                    (Atom::from("left"), Term::Atom(Atom::from("left")).into()),
                    (Atom::from("right"), Term::Atom(Atom::from("left")).into()),
                ]),
            ),
        ));

        assert!(!converts(this, that));
    }

    #[test]
    fn bind_unfolds_against_non_bind_in_both_directions() {
        let bind = Term::Bind(
            vec![(
                Term::Type.into(),
                Scope::close(Many(1), &["x"], Term::Atom(Atom::from("left"))),
            )],
            Scope::close(Many(1), &["x"], Term::from("x")),
        );

        let atom = Term::Atom(Atom::from("left"));

        assert!(converts(bind.clone(), atom.clone()));

        assert!(converts(atom, bind));
    }
}
