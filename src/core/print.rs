use {
    super::{
        Apply, Atom, AtomType, Func, FuncType, Let, LetRec, Match, One, Pair, PairType, Prim,
        Scope, Split, Term, Two, Var,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn label_at(depth: usize) -> String {
    format!("#{depth}")
}

fn labels_from(depth: usize, arity: usize) -> Vec<String> {
    (0..arity).map(|offset| label_at(depth + offset)).collect()
}

fn label_terms(labels: &[String]) -> Vec<Term> {
    labels.iter().map(Var::free).map(Into::into).collect()
}

fn open_scope_one(scope: Scope<One>, depth: usize) -> (String, Term) {
    let label = label_at(depth);
    let term = Var::free(&label).into();
    let body = scope.open(&[&term]);

    (label, body)
}

fn open_scope_two(scope: Scope<Two>, depth: usize) -> ((String, String), Term) {
    let fst = label_at(depth);
    let snd = label_at(depth + 1);
    let fst_term = Var::free(&fst).into();
    let snd_term = Var::free(&snd).into();
    let body = scope.open(&[&fst_term, &snd_term]);

    ((fst, snd), body)
}

fn print_var(var: Var) -> Printer<'static> {
    pure(var.unwrap().to_string())
}

fn print_atom(atom: Atom) -> Printer<'static> {
    flat([pure("'"), pure(atom.string)])
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

fn print_prim(prim: Prim, depth: usize) -> Printer<'static> {
    match prim {
        Prim::NatType => pure("Nat"),
        Prim::Nat(value) => pure(format!("{value}n")),
        Prim::NatEql(left, right) => flat([
            pure("Nat.eql "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatAdd(left, right) => flat([
            pure("Nat.add "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatSub(left, right) => flat([
            pure("Nat.sub "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatMul(left, right) => flat([
            pure("Nat.mul "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatNeq(left, right) => flat([
            pure("Nat.neq "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatDiv(left, right) => flat([
            pure("Nat.div "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatRem(left, right) => flat([
            pure("Nat.rem "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatLt(left, right) => flat([
            pure("Nat.lt "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatGt(left, right) => flat([
            pure("Nat.gt "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatLte(left, right) => flat([
            pure("Nat.lte "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatGte(left, right) => flat([
            pure("Nat.gte "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{value}i")),
        Prim::IntEql(left, right) => flat([
            pure("Int.eql "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntAdd(left, right) => flat([
            pure("Int.add "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntSub(left, right) => flat([
            pure("Int.sub "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntMul(left, right) => flat([
            pure("Int.mul "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(bits) => print_flt(bits),
        Prim::FltAdd(left, right) => flat([
            pure("Flt.add "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltSub(left, right) => flat([
            pure("Flt.sub "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltMul(left, right) => flat([
            pure("Flt.mul "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntNeg(inner) => flat([pure("Int.neg "), print_term(*inner, depth)]),
        Prim::IntNeq(left, right) => flat([
            pure("Int.neq "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntDiv(left, right) => flat([
            pure("Int.div "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntRem(left, right) => flat([
            pure("Int.rem "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntLt(left, right) => flat([
            pure("Int.lt "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntGt(left, right) => flat([
            pure("Int.gt "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntLte(left, right) => flat([
            pure("Int.lte "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntGte(left, right) => flat([
            pure("Int.gte "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltNeg(inner) => flat([pure("Flt.neg "), print_term(*inner, depth)]),
        Prim::FltAbs(inner) => flat([pure("Flt.abs "), print_term(*inner, depth)]),
        Prim::FltSqrt(inner) => flat([pure("Flt.sqrt "), print_term(*inner, depth)]),
        Prim::FltFloor(inner) => flat([pure("Flt.floor "), print_term(*inner, depth)]),
        Prim::FltCeil(inner) => flat([pure("Flt.ceil "), print_term(*inner, depth)]),
        Prim::FltTrunc(inner) => flat([pure("Flt.trunc "), print_term(*inner, depth)]),
        Prim::FltNearest(inner) => flat([pure("Flt.nearest "), print_term(*inner, depth)]),
        Prim::FltDiv(left, right) => flat([
            pure("Flt.div "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltMin(left, right) => flat([
            pure("Flt.min "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltMax(left, right) => flat([
            pure("Flt.max "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltEql(left, right) => flat([
            pure("Flt.eql "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltNeq(left, right) => flat([
            pure("Flt.neq "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltLt(left, right) => flat([
            pure("Flt.lt "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltGt(left, right) => flat([
            pure("Flt.gt "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltLte(left, right) => flat([
            pure("Flt.lte "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::FltGte(left, right) => flat([
            pure("Flt.gte "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::NatToInt(inner) => flat([pure("Nat.to-int "), print_term(*inner, depth)]),
        Prim::IntToNat(inner) => flat([pure("Int.to-nat "), print_term(*inner, depth)]),
        Prim::IntToFlt(inner) => flat([pure("Int.to-flt "), print_term(*inner, depth)]),
        Prim::NatToFlt(inner) => flat([pure("Nat.to-flt "), print_term(*inner, depth)]),
        Prim::FltToInt(inner) => flat([pure("Flt.to-int "), print_term(*inner, depth)]),
        Prim::FltToNat(inner) => flat([pure("Flt.to-nat "), print_term(*inner, depth)]),
        Prim::LstType(elem) => flat([pure("Lst "), print_term(*elem, depth)]),
        Prim::Lst(elems) => flat([
            pure("["),
            sep_flat(elems.into_iter().map(move |e| print_term(*e, depth)), || pure(", ")),
            pure("]"),
        ]),
        Prim::LstLen(list) => flat([pure("Lst.len "), print_term(*list, depth)]),
        Prim::LstGet(index, list) => flat([
            pure("Lst.get "),
            print_term(*index, depth),
            pure(" "),
            print_term(*list, depth),
        ]),
        Prim::LstSlice(start, end, list) => flat([
            pure("Lst.slice "),
            print_term(*start, depth),
            pure(" "),
            print_term(*end, depth),
            pure(" "),
            print_term(*list, depth),
        ]),
        Prim::LstConcat(left, right) => flat([
            pure("Lst.concat "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
    }
}

fn print_term(term: Term, depth: usize) -> Printer<'static> {
    match term {
        Term::Type => pure("Type"),
        Term::Prim(prim) => print_prim(prim, depth),
        Term::FuncType(FuncType { input, output }) => {
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
        Term::Func(Func { body }) => {
            let (label, body) = open_scope_one(body, depth);

            flat([pure(label), pure(" => "), print_term(body, depth + 1)])
        }
        Term::Apply(Apply { head, param }) => flat([
            print_term(*head, depth),
            pure(" "),
            print_term(*param, depth),
        ]),
        Term::PairType(PairType { input, output }) => {
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
        Term::Pair(Pair { fst, snd }) => flat([
            pure("("),
            print_term(*fst, depth),
            pure(", "),
            print_term(*snd, depth),
            pure(")"),
        ]),
        Term::Split(Split { head, motive, tail }) => {
            let motive_label = label_at(depth + 2);
            let motive_label_term = Var::free(&motive_label).into();
            let motive = motive.open(&[&motive_label_term]);
            let ((fst_label, snd_label), tail) = open_scope_two(tail, depth);

            flat([
                pure("let ("),
                pure(fst_label),
                pure(", "),
                pure(snd_label),
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
        Term::AtomType(AtomType { atoms }) => flat([
            pure("'["),
            sep_flat(atoms.into_iter().map(|atom| pure(atom.string)), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        Term::Atom(atom) => print_atom(atom),
        Term::Match(Match {
            head,
            motive,
            cases,
        }) => {
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
        Term::Let(Let { type_, body, tail }) => {
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
        Term::LetRec(LetRec { items, tail }) => {
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
        Term::Var(var) => print_var(var),
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone(), 0), formatter, 2)
    }
}

#[cfg(test)]
mod tests {
    use {super::*, crate::core::Type};

    #[test]
    fn print_parse_roundtrip_closed_terms() {
        let terms = [
            FuncType::new("x", Type, Type).into(),
            Func::new("x", Var::free("x")).into(),
            Apply::many(
                Var::free("f"),
                [Pair::new(Atom::from("a"), Atom::from("b"))],
            ),
            Let::new(
                "x",
                AtomType::new(["a", "b"]),
                Atom::from("a"),
                Match::new(Var::free("x"), "m", Type, [("a", Type), ("b", Type)]),
            )
            .into(),
            LetRec::new(
                vec![(
                    "id",
                    FuncType::new("x", Type, Type),
                    Func::new("x", Var::free("x")),
                )],
                Apply::many(Var::free("id"), [Type]),
            )
            .into(),
        ];

        for term in terms {
            let printed = term.to_string();
            let reparsed = printed.parse::<Term>().unwrap();
            assert_eq!(reparsed, term);
        }
    }
}
