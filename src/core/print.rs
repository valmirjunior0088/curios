use {
    super::{
        Apply, Atom, AtomType, Flt, Func, FuncType, Let, Match, NatFold, NatMatch, One, Prim,
        Proj, Rec, Scope, Seal, Sealed, Term, Tuple, TupleType, Two, Unseal, Var,
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
    flat([pure("'"), pure(atom.as_string())])
}

fn print_flt(flt: Flt) -> Printer<'static> {
    let mut string = flt.to_f32().to_string();

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
        Prim::NatNeq(left, right) => flat([
            pure("Nat.neq "),
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
        Prim::NatLt(left, right) => flat([
            pure("Nat.lt "),
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
        Prim::NatToStr(inner) => flat([pure("Nat.to_str "), print_term(*inner, depth)]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{value}i")),
        Prim::IntEql(left, right) => flat([
            pure("Int.eql "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::IntNeq(left, right) => flat([
            pure("Int.neq "),
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
        Prim::IntToStr(inner) => flat([pure("Int.to_str "), print_term(*inner, depth)]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(flt) => print_flt(flt),
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
        Prim::FltDiv(left, right) => flat([
            pure("Flt.div "),
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
        Prim::FltNeg(inner) => flat([pure("Flt.neg "), print_term(*inner, depth)]),
        Prim::FltAbs(inner) => flat([pure("Flt.abs "), print_term(*inner, depth)]),
        Prim::FltSqrt(inner) => flat([pure("Flt.sqrt "), print_term(*inner, depth)]),
        Prim::FltFloor(inner) => flat([pure("Flt.floor "), print_term(*inner, depth)]),
        Prim::FltCeil(inner) => flat([pure("Flt.ceil "), print_term(*inner, depth)]),
        Prim::FltTrunc(inner) => flat([pure("Flt.trunc "), print_term(*inner, depth)]),
        Prim::FltNearest(inner) => flat([pure("Flt.nearest "), print_term(*inner, depth)]),
        Prim::FltToStr(inner) => flat([pure("Flt.to_str "), print_term(*inner, depth)]),
        Prim::NatToInt(inner) => flat([pure("Nat.to_int "), print_term(*inner, depth)]),
        Prim::NatToFlt(inner) => flat([pure("Nat.to_flt "), print_term(*inner, depth)]),
        Prim::IntToNat(inner) => flat([pure("Int.to_nat "), print_term(*inner, depth)]),
        Prim::IntToFlt(inner) => flat([pure("Int.to_flt "), print_term(*inner, depth)]),
        Prim::FltToNat(inner) => flat([pure("Flt.to_nat "), print_term(*inner, depth)]),
        Prim::FltToInt(inner) => flat([pure("Flt.to_int "), print_term(*inner, depth)]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        Prim::BinLen(bin) => flat([pure("Bin.len "), print_term(*bin, depth)]),
        Prim::BinEql(left, right) => flat([
            pure("Bin.eql "),
            print_term(*left, depth),
            pure(" "),
            print_term(*right, depth),
        ]),
        Prim::BinGet(bin, index) => flat([
            pure("Bin.get "),
            print_term(*bin, depth),
            pure(" "),
            print_term(*index, depth),
        ]),
        Prim::BinSlice(bin, start, end) => flat([
            pure("Bin.slice "),
            print_term(*bin, depth),
            pure(" "),
            print_term(*start, depth),
            pure(" "),
            print_term(*end, depth),
        ]),
        Prim::BinAppend(bin, byte) => flat([
            pure("Bin.append "),
            print_term(*bin, depth),
            pure(" "),
            print_term(*byte, depth),
        ]),
        Prim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(
                operands.into_iter().map(move |e| print_term(*e, depth)),
                || pure(", "),
            ),
        ]),
        Prim::ArrType(elem) => flat([pure("Arr "), print_term(*elem, depth)]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(
                elems.into_iter().map(move |e| print_term(*e, depth)),
                || pure(", "),
            ),
            pure("]"),
        ]),
        Prim::ArrLen(list) => flat([pure("Arr.len "), print_term(*list, depth)]),
        Prim::ArrGet(list, index) => flat([
            pure("Arr.get "),
            print_term(*list, depth),
            pure(" "),
            print_term(*index, depth),
        ]),
        Prim::ArrSlice(list, start, end) => flat([
            pure("Arr.slice "),
            print_term(*list, depth),
            pure(" "),
            print_term(*start, depth),
            pure(" "),
            print_term(*end, depth),
        ]),
        Prim::ArrAppend(list, elem) => flat([
            pure("Arr.append "),
            print_term(*list, depth),
            pure(" "),
            print_term(*elem, depth),
        ]),
        Prim::ArrConcat(operands) => flat([
            pure("Arr.concat "),
            sep_flat(
                operands.into_iter().map(move |e| print_term(*e, depth)),
                || pure(", "),
            ),
        ]),
    }
}

fn print_term(term: Term, depth: usize) -> Printer<'static> {
    match term {
        Term::Type => pure("Type"),
        Term::Prim(prim) => print_prim(prim, depth),
        Term::NatFold(NatFold {
            head,
            motive,
            zero_case,
            succ_case,
        }) => {
            let (motive_label, motive) = open_scope_one(motive, depth);
            let ((pred_label, ih_label), succ_case) = open_scope_two(succ_case, depth);

            flat([
                pure("Nat.fold "),
                print_term(*head, depth),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(motive, depth + 1),
                pure(";"),
                pure("\n| 0n =>\n"),
                indent(flat([print_term(*zero_case, depth), pure(";")])),
                pure("\n| "),
                pure(pred_label),
                pure(" "),
                pure(ih_label),
                pure(" =>\n"),
                indent(flat([print_term(succ_case, depth), pure(";")])),
            ])
        }
        Term::NatMatch(NatMatch {
            head,
            motive,
            cases,
            default,
        }) => {
            let (motive_label, motive) = open_scope_one(motive, depth);
            let case_printers = flat(
                cases
                    .into_iter()
                    .map(|(n, body)| {
                        flat([
                            pure(format!("\n| {n}n =>\n")),
                            indent(flat([print_term(*body, depth), pure(";")])),
                        ])
                    })
                    .collect::<Vec<_>>(),
            );
            flat([
                pure("Nat.match "),
                print_term(*head, depth),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(motive, depth + 1),
                pure(";"),
                case_printers,
                pure("\n| _ =>\n"),
                indent(flat([print_term(*default, depth), pure(";")])),
            ])
        }
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

            flat([
                pure(label),
                pure(" =>\n"),
                indent(print_term(body, depth + 1)),
            ])
        }
        Term::Apply(Apply { head, param }) => flat([
            print_term(*head, depth),
            pure(" "),
            print_term(*param, depth),
        ]),
        Term::TupleType(TupleType { fields }) => {
            let n = fields.len();
            let labels = labels_from(depth, n);
            let label_terms_vec = label_terms(&labels);
            let label_refs = label_terms_vec.iter().collect::<Vec<_>>();

            let items = fields
                .into_iter()
                .enumerate()
                .map(|(i, scope)| {
                    let ty = scope.open(&label_refs[..i]);
                    indent(flat([
                        pure(labels[i].clone()),
                        pure(" : "),
                        print_term(ty, depth + n),
                    ]))
                })
                .collect::<Vec<_>>();

            flat([pure("{ "), sep_flat(items, || pure("\n, ")), pure("\n}")])
        }
        Term::Tuple(Tuple { fields }) => flat([
            pure("("),
            sep_flat(
                fields.into_iter().map(move |f| print_term(*f, depth)),
                || pure(", "),
            ),
            pure(")"),
        ]),
        Term::Proj(Proj { head, index }) => flat([
            pure("("),
            print_term(*head, depth),
            pure(format!(").{index}")),
        ]),
        Term::AtomType(AtomType { atoms }) => flat([
            pure("'["),
            sep_flat(atoms.into_iter().map(|atom| pure(atom.as_string())), || {
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
                            pure("\n| "),
                            print_atom(atom),
                            pure(" =>\n"),
                            indent(flat([print_term(*body, depth), pure(";")])),
                        ])
                    })
                    .collect::<Vec<_>>(),
            );

            flat([
                pure("match "),
                print_term(*head, depth),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(motive, depth + 1),
                pure(";"),
                cases,
            ])
        }
        Term::Let(Let { type_, body, tail }) => {
            let (label, tail) = open_scope_one(tail, depth);

            flat([
                pure("let "),
                pure(label),
                pure(" : "),
                print_term(*type_, depth),
                pure(" =\n"),
                indent(flat([print_term(*body, depth), pure(";")])),
                pure("\n"),
                print_term(tail, depth + 1),
            ])
        }
        Term::Rec(Rec { items, tail }) => {
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
                        pure(" =\n"),
                        indent(print_term(body, inner_depth)),
                    ])
                })
                .collect::<Vec<_>>();

            let tail = tail.open(&label_terms);

            flat([
                pure("rec "),
                sep_flat(bindings, || pure("\nand ")),
                pure(";\n"),
                print_term(tail, inner_depth),
            ])
        }
        Term::Sealed(Sealed { witness, tail }) => {
            let (label, tail) = open_scope_one(tail, depth);

            flat([
                pure("sealed "),
                pure(label),
                pure(" = "),
                print_term(*witness, depth),
                pure(";\n"),
                print_term(tail, depth + 1),
            ])
        }
        Term::Seal(Seal { witness, value }) => flat([
            pure("seal "),
            print_term(*witness, depth),
            pure(" "),
            print_term(*value, depth),
        ]),
        Term::Unseal(Unseal { witness, value }) => flat([
            pure("unseal "),
            print_term(*witness, depth),
            pure(" "),
            print_term(*value, depth),
        ]),
        Term::Var(var) => print_var(var),
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone(), 0), formatter, 2)
    }
}
