//! What a case's solution re-types, read through local definitions.

use {
    super::*,
    curios_core::{Global, InductDecl, Intrinsic, Nat, StructDecl},
    std::convert::Infallible,
};

/// The two facts these rules read from a checker's scope: which names are locals, and what each local definition is.
///
/// Not a checker and not a mock of one, for the totality tests' `Probe`'s reason: the rules ask no judgment, only these two environment queries, and the kernel — whose locals carry no definitions — cannot pose the question the reading through them answers. What the elaborator does with the answer is `curios/src/tests/`'s to show.
#[derive(Default)]
struct Scope {
    assumed: Vec<Free>,
    defined: BTreeMap<Free, Term>,
}

impl Scope {
    fn assume(&mut self, name: &Free) {
        self.assumed.push(name.clone());
    }

    fn define(&mut self, name: &Free, definition: Term) {
        self.defined.insert(name.clone(), definition);
    }
}

impl Env for Scope {
    type Error = Infallible;

    fn force(&mut self, term: &Term) -> Result<Term, Self::Error> {
        Ok(term.clone())
    }

    fn assumption(&self, _: &Free) -> Option<&Term> {
        None
    }

    fn fresh(&mut self, hint: Option<&str>) -> Free {
        Free::local(9_000, hint)
    }

    fn is_local(&self, name: &Free) -> bool {
        self.assumed.contains(name) || self.defined.contains_key(name)
    }

    fn unfold(&self, name: &Free) -> Option<&Term> {
        self.defined.get(name)
    }

    fn induct_decl(&self, _: &Global) -> Option<&InductDecl> {
        None
    }

    fn struct_decl(&self, _: &Global) -> Option<&StructDecl> {
        None
    }
}

fn nat(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A term mentioning exactly `name`, standing for a type that does.
fn over(name: &Free) -> Term {
    Term::intrinsic(Intrinsic::nat_add(Term::free_var(name), nat(1)))
}

/// `let t = s; match t | …` solves `s`, the variable the kernel's arm meets once the `let` is substituted. Controls: a top-level name is no local and solves nothing, and neither does an expression.
#[test]
fn a_let_of_a_variable_solves_the_variable_beneath_it() {
    let s = Free::local(1, Some("s"));
    let t = Free::local(2, Some("t"));
    let top = Free::local(3, Some("top"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.define(&t, Term::free_var(&s));

    let value = nat(7);
    for (scrutinee, expected) in [
        (Term::free_var(&s), Some((s.clone(), value.clone()))),
        (Term::free_var(&t), Some((s.clone(), value.clone()))),
        (Term::free_var(&top), None),
        (over(&s), None),
    ] {
        assert_eq!(
            scrutinee_solution(&scope, &scrutinee, &value),
            expected,
            "{scrutinee:?}"
        );
    }
}

/// With `s` solved, a local typed over `s` is re-typed, and so is one typed over `let t = s`, with `t` inlined — both are typed over `s` once the kernel has substituted the `let`. A local mentioning neither keeps its entry, and so does `s`, whose occurrences the arm substitutes.
#[test]
fn a_local_typed_through_a_let_is_retyped_with_the_let_inlined() {
    let s = Free::local(1, Some("s"));
    let t = Free::local(2, Some("t"));
    let z = Free::local(3, Some("z"));
    let w = Free::local(4, Some("w"));
    let u = Free::local(5, Some("u"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.define(&t, Term::free_var(&s));

    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let locals = [
        (s.clone(), nat_type.clone()),
        (z.clone(), over(&s)),
        (w.clone(), over(&t)),
        (u.clone(), nat_type),
    ];
    let solutions = [(s.clone(), nat(7))];

    let over_seven = Term::intrinsic(Intrinsic::nat_add(nat(7), nat(1)));
    assert_eq!(
        retyped(&scope, &locals, &solutions),
        vec![(z, over_seven.clone()), (w, over_seven)]
    );
}

/// A definition that mentions itself is read once: the local typed over it is re-typed with it inlined a single level, and the walk ends.
#[test]
fn a_definition_that_mentions_itself_is_read_once() {
    let s = Free::local(1, Some("s"));
    let f = Free::local(2, Some("f"));
    let h = Free::local(3, Some("h"));
    let mut scope = Scope::default();
    scope.assume(&s);
    let definition = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&f), Term::free_var(&s)));
    scope.define(&f, definition);

    let retyped = retyped(&scope, &[(h.clone(), over(&f))], &[(s, nat(7))]);

    let expected = Term::intrinsic(Intrinsic::nat_add(
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&f), nat(7))),
        nat(1),
    ));
    assert_eq!(retyped, vec![(h, expected)]);
}
