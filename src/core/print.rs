use {
    super::{Atom, Intrinsic, Name, One, Scope, Term, Two},
    crate::monads::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn label_at(depth: usize) -> String {
    format!("#{depth}")
}

fn labels_from(depth: usize, arity: usize) -> Vec<String> {
    (0..arity).map(|offset| label_at(depth + offset)).collect()
}

fn label_terms(labels: &[String]) -> Vec<Term> {
    labels.iter().map(Term::label).collect()
}

fn open_scope_one(scope: Scope<One>, depth: usize) -> (String, Term) {
    let label = label_at(depth);
    let term = Term::label(&label);
    let body = scope.open(&[&term]);

    (label, body)
}

fn open_scope_two(scope: Scope<Two>, depth: usize) -> ((String, String), Term) {
    let first = label_at(depth);
    let second = label_at(depth + 1);
    let first_term = Term::label(&first);
    let second_term = Term::label(&second);
    let body = scope.open(&[&first_term, &second_term]);

    ((first, second), body)
}

fn print_name(name: Name) -> Printer<'static> {
    pure(name.unwrap().to_string())
}

fn print_atom(atom: Atom) -> Printer<'static> {
    flat([pure(":"), pure(atom.string)])
}

fn print_flt(bits: u32) -> Printer<'static> {
    let value = f32::from_bits(bits);
    let mut string = value.to_string();

    if let Some(index) = string.find(['e', 'E']) {
        if !string[..index].contains('.') {
            string.insert_str(index, ".0");
        }
    } else if !string.contains('.') {
        string.push_str(".0");
    }

    pure(string)
}

fn print_term(term: Term, depth: usize) -> Printer<'static> {
    match term {
        Term::Type => pure("Type"),
        Term::Intrinsic { intrinsic } => match intrinsic {
            Intrinsic::IntType => pure("Int"),
            Intrinsic::Int(value) => pure(value.to_string()),
            Intrinsic::IntEql(first, second) => flat([
                pure("Int.eql "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
            Intrinsic::IntAdd(first, second) => flat([
                pure("Int.add "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
            Intrinsic::IntSub(first, second) => flat([
                pure("Int.sub "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
            Intrinsic::IntMul(first, second) => flat([
                pure("Int.mul "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
            Intrinsic::FltType => pure("Flt"),
            Intrinsic::Flt(bits) => print_flt(bits),
            Intrinsic::FltAdd(first, second) => flat([
                pure("Flt.add "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
            Intrinsic::FltSub(first, second) => flat([
                pure("Flt.sub "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
            Intrinsic::FltMul(first, second) => flat([
                pure("Flt.mul "),
                print_term(*first, depth),
                pure(" "),
                print_term(*second, depth),
            ]),
        },
        Term::FuncType { input, output } => {
            let (label, output) = open_scope_one(output, depth);

            flat([
                pure("("),
                pure(label),
                pure(" : "),
                print_term(*input, depth),
                pure(") -> "),
                print_term(output, depth + 1),
            ])
        }
        Term::Func { body } => {
            let (label, body) = open_scope_one(body, depth);

            flat([pure(label), pure(" => "), print_term(body, depth + 1)])
        }
        Term::Apply { head, param } => flat([print_term(*head, depth), pure(" "), print_term(*param, depth)]),
        Term::PairType { input, output } => {
            let (label, output) = open_scope_one(output, depth);

            flat([
                pure("("),
                pure(label),
                pure(" : "),
                print_term(*input, depth),
                pure(", "),
                print_term(output, depth + 1),
                pure(")"),
            ])
        }
        Term::Pair { first, second } => flat([
            pure("("),
            print_term(*first, depth),
            pure(", "),
            print_term(*second, depth),
            pure(")"),
        ]),
        Term::Split { head, motive, tail } => {
            let motive_label = label_at(depth + 2);
            let motive_label_term = Term::label(&motive_label);
            let motive = motive.open(&[&motive_label_term]);
            let ((first_label, second_label), tail) = open_scope_two(tail, depth);

            flat([
                pure("let ("),
                pure(first_label),
                pure(", "),
                pure(second_label),
                pure(")"),
                pure("\n"),
                indent(flat([
                    pure("with "),
                    pure(motive_label),
                    pure(" => "),
                    print_term(motive, depth + 3),
                    pure("\n"),
                    pure("= "),
                    print_term(*head, depth),
                    pure(";"),
                ])),
                pure("\n"),
                print_term(tail, depth + 2),
            ])
        }
        Term::AtomType { atoms } => flat([
            pure("{"),
            sep_flat(atoms.into_iter().map(print_atom), || pure(", ")),
            pure("}"),
        ]),
        Term::Atom { atom } => print_atom(atom),
        Term::Match {
            head,
            motive,
            cases,
        } => {
            let (motive_label, motive) = open_scope_one(motive, depth);

            let cases = flat(
                cases
                    .into_iter()
                    .map(|(atom, body)| {
                        flat([
                            pure("\ncase "),
                            print_atom(atom),
                            pure(" => "),
                            print_term(*body, depth),
                            pure(";"),
                        ])
                    })
                    .collect::<Vec<_>>(),
            );

            flat([
                pure("match "),
                print_term(*head, depth),
                pure("\n"),
                indent(flat([
                    pure("with "),
                    pure(motive_label),
                    pure(" => "),
                    print_term(motive, depth + 1),
                    pure(";"),
                    cases,
                ])),
            ])
        }
        Term::Let { type_, body, tail } => {
            let (label, tail) = open_scope_one(tail, depth);

            flat([
                pure("let "),
                pure(label),
                pure(" : "),
                print_term(*type_, depth),
                pure(" = "),
                print_term(*body, depth),
                pure(";\n"),
                print_term(tail, depth + 1),
            ])
        }
        Term::LetRec { items, tail } => {
            let labels = labels_from(depth, items.len());
            let label_terms = label_terms(&labels);
            let label_terms = label_terms.iter().collect::<Vec<_>>();
            let inner_depth = depth + labels.len();

            let bindings = items
                .into_iter()
                .enumerate()
                .map(|(index, (type_, body))| {
                    let type_ = type_.open(&label_terms);
                    let body = body.open(&label_terms);

                    flat([
                        pure(labels[index].clone()),
                        pure(" : "),
                        print_term(type_, inner_depth),
                        pure(" = "),
                        print_term(body, inner_depth),
                    ])
                })
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            flat([
                pure("let {"),
                pure("\n"),
                indent(sep_flat(bindings, || pure(";\n"))),
                pure("\n};\n"),
                print_term(tail, inner_depth),
            ])
        }
        Term::Name { name } => print_name(name),
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.into(), 0), formatter, 2)
    }
}

#[cfg(test)]
mod tests {
    use {super::*, crate::core::parse};

    #[test]
    fn print_parse_roundtrip_closed_terms() {
        let terms = [
            Term::func_type("x", Term::Type, Term::Type),
            Term::func("x", Term::label("x")),
            Term::apply(
                Term::label("f"),
                [Term::pair(Term::atom("a"), Term::atom("b"))],
            ),
            Term::let_(
                "x",
                Term::atom_type(["a", "b"]),
                Term::atom("a"),
                Term::match_(
                    Term::label("x"),
                    "m",
                    Term::Type,
                    [("a", Term::Type), ("b", Term::Type)],
                ),
            ),
            Term::let_rec(
                vec![(
                    "id",
                    Term::func_type("x", Term::Type, Term::Type),
                    Term::func("x", Term::label("x")),
                )],
                Term::apply(Term::label("id"), [Term::Type]),
            ),
        ];

        for term in terms {
            let printed = term.to_string();
            let reparsed = parse(&printed).unwrap();
            assert_eq!(reparsed, term);
        }
    }
}
