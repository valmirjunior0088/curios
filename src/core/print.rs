use {
    super::{
        Apply, Atom, Cases, Definition, Field, Flt, Func, FuncType, Item, Let, Match, Module, Nat,
        One, Plicity, Prim, Proj, Rec, Scope, Subterm, Telescope, Term, Tuple, TupleType, Two,
        UnionType, Var, Variant,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn label_at(depth: usize) -> String {
    format!("#{depth}")
}

fn label_terms(labels: &[String]) -> Vec<Term> {
    labels.iter().map(Var::free).map(Term::var).collect()
}

fn open_scope_one(scope: Scope<One>, depth: usize) -> (String, Term) {
    let label = scope
        .first_label()
        .map(str::to_string)
        .unwrap_or_else(|| label_at(depth));
    let body = scope.open(&[&Term::var(Var::free(&label))]);

    (label, body)
}

fn open_telescope(telescope: Telescope<Term>, depth: usize) -> (Vec<String>, Term) {
    fn walk(cur: Telescope<Term>, depth: usize, idx: usize, labels: &mut Vec<String>) -> Term {
        match cur {
            Telescope::Done(body) => *body,
            Telescope::Cons(_ty, rest) => {
                let label = rest
                    .first_label()
                    .map(str::to_string)
                    .unwrap_or_else(|| label_at(depth + idx));
                let next = rest.open(&[&Term::var(Var::free(&label))]);
                labels.push(label);
                walk(next, depth, idx + 1, labels)
            }
        }
    }

    let mut labels = Vec::new();
    let body = walk(telescope, depth, 0, &mut labels);
    (labels, body)
}

fn open_scope_two(scope: Scope<Two>, depth: usize) -> ((String, String), Term) {
    let fst = scope
        .first_label()
        .map(str::to_string)
        .unwrap_or_else(|| label_at(depth));
    let snd = scope
        .second_label()
        .map(str::to_string)
        .unwrap_or_else(|| label_at(depth + 1));
    let body = scope.open(&[&Term::var(Var::free(&fst)), &Term::var(Var::free(&snd))]);

    ((fst, snd), body)
}

fn print_var(var: Var) -> Printer<'static> {
    pure(var.unwrap().to_string())
}

fn print_atom(atom: Atom) -> Printer<'static> {
    flat([pure("'"), pure(atom.as_string())])
}

fn print_flt(flt: Flt) -> Printer<'static> {
    let mut string = format!("{:+}", flt.to_f32());

    // string always starts with '+' or '-'; work on the digits after the sign
    let after_sign = &string[1..];

    if let Some(exp) = after_sign.find(['e', 'E']) {
        if !after_sign[..exp].contains('.') {
            string.insert_str(1 + exp, ".0");
        }
    } else if !after_sign.contains('.') {
        string.push_str(".0");
    }

    pure(string)
}

fn print_prim(prim: Prim, depth: usize) -> Printer<'static> {
    match prim {
        Prim::BlnType => pure("Bln"),
        Prim::Bln(false) => pure("false"),
        Prim::Bln(true) => pure("true"),
        Prim::NatType => pure("Nat"),
        Prim::Nat(Nat::Zero) => pure("0"),
        Prim::Nat(Nat::Succ(spine, inner)) => match inner.as_ref() {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => pure(format!("{spine}")),
            inner => {
                if spine == num_bigint::BigUint::from(1usize) {
                    flat([
                        pure("Nat.succ("),
                        print_term(inner.clone().into(), depth),
                        pure(")"),
                    ])
                } else {
                    flat([
                        pure(format!("Nat.succ({spine}, ")),
                        print_term(inner.clone().into(), depth),
                        pure(")"),
                    ])
                }
            }
        },
        Prim::NatEql(left, right) => flat([
            pure("Nat.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatNeq(left, right) => flat([
            pure("Nat.neq "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatAdd(left, right) => flat([
            pure("Nat.add "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatSub(left, right) => flat([
            pure("Nat.sub "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatMul(left, right) => flat([
            pure("Nat.mul "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatLt(left, right) => flat([
            pure("Nat.lt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatDiv(left, right) => flat([
            pure("Nat.div "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatRem(left, right) => flat([
            pure("Nat.rem "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatGt(left, right) => flat([
            pure("Nat.gt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatLte(left, right) => flat([
            pure("Nat.lte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatGte(left, right) => flat([
            pure("Nat.gte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::NatToStr(inner) => flat([pure("Nat.to_str "), print_term(inner, depth)]),
        Prim::IntType => pure("Int"),
        Prim::Int(value) => pure(format!("{:+}", value.to_i32())),
        Prim::IntEql(left, right) => flat([
            pure("Int.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntNeq(left, right) => flat([
            pure("Int.neq "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntAdd(left, right) => flat([
            pure("Int.add "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntSub(left, right) => flat([
            pure("Int.sub "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntMul(left, right) => flat([
            pure("Int.mul "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntDiv(left, right) => flat([
            pure("Int.div "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntRem(left, right) => flat([
            pure("Int.rem "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntLt(left, right) => flat([
            pure("Int.lt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntGt(left, right) => flat([
            pure("Int.gt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntLte(left, right) => flat([
            pure("Int.lte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntGte(left, right) => flat([
            pure("Int.gte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::IntToStr(inner) => flat([pure("Int.to_str "), print_term(inner, depth)]),
        Prim::FltType => pure("Flt"),
        Prim::Flt(flt) => print_flt(flt),
        Prim::FltAdd(left, right) => flat([
            pure("Flt.add "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltSub(left, right) => flat([
            pure("Flt.sub "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltMul(left, right) => flat([
            pure("Flt.mul "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltDiv(left, right) => flat([
            pure("Flt.div "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltEql(left, right) => flat([
            pure("Flt.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltNeq(left, right) => flat([
            pure("Flt.neq "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltLt(left, right) => flat([
            pure("Flt.lt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltGt(left, right) => flat([
            pure("Flt.gt "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltLte(left, right) => flat([
            pure("Flt.lte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltGte(left, right) => flat([
            pure("Flt.gte "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltMin(left, right) => flat([
            pure("Flt.min "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltMax(left, right) => flat([
            pure("Flt.max "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::FltNeg(inner) => flat([pure("Flt.neg "), print_term(inner, depth)]),
        Prim::FltAbs(inner) => flat([pure("Flt.abs "), print_term(inner, depth)]),
        Prim::FltSqrt(inner) => flat([pure("Flt.sqrt "), print_term(inner, depth)]),
        Prim::FltFloor(inner) => flat([pure("Flt.floor "), print_term(inner, depth)]),
        Prim::FltCeil(inner) => flat([pure("Flt.ceil "), print_term(inner, depth)]),
        Prim::FltTrunc(inner) => flat([pure("Flt.trunc "), print_term(inner, depth)]),
        Prim::FltNearest(inner) => flat([pure("Flt.nearest "), print_term(inner, depth)]),
        Prim::FltToStr(inner) => flat([pure("Flt.to_str "), print_term(inner, depth)]),
        Prim::FltToLeBin(inner) => flat([pure("Flt.to_le_bin "), print_term(inner, depth)]),
        Prim::NatToInt(inner) => flat([pure("Nat.to_int "), print_term(inner, depth)]),
        Prim::NatToFlt(inner) => flat([pure("Nat.to_flt "), print_term(inner, depth)]),
        Prim::IntToNat(inner) => flat([pure("Int.to_nat "), print_term(inner, depth)]),
        Prim::IntToFlt(inner) => flat([pure("Int.to_flt "), print_term(inner, depth)]),
        Prim::FltToNat(inner) => flat([pure("Flt.to_nat "), print_term(inner, depth)]),
        Prim::FltToInt(inner) => flat([pure("Flt.to_int "), print_term(inner, depth)]),
        Prim::BinType => pure("Bin"),
        Prim::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        Prim::BinLen(bin) => flat([pure("Bin.len "), print_term(bin, depth)]),
        Prim::BinEql(left, right) => flat([
            pure("Bin.eql "),
            print_term(left, depth),
            pure(" "),
            print_term(right, depth),
        ]),
        Prim::BinGet(bin, index) => flat([
            pure("Bin.get "),
            print_term(bin, depth),
            pure(" "),
            print_term(index, depth),
        ]),
        Prim::BinSlice(bin, start, end) => flat([
            pure("Bin.slice "),
            print_term(bin, depth),
            pure(" "),
            print_term(start, depth),
            pure(" "),
            print_term(end, depth),
        ]),
        Prim::BinAppend(bin, byte) => flat([
            pure("Bin.append "),
            print_term(bin, depth),
            pure(" "),
            print_term(byte, depth),
        ]),
        Prim::BinConcat(operands) => flat([
            pure("Bin.concat "),
            sep_flat(
                operands.into_iter().map(move |e| print_term(e, depth)),
                || pure(", "),
            ),
        ]),
        Prim::ArrType(elem) => flat([pure("Arr "), print_term(elem, depth)]),
        Prim::Arr(elems) => flat([
            pure("["),
            sep_flat(elems.into_iter().map(move |e| print_term(e, depth)), || {
                pure(", ")
            }),
            pure("]"),
        ]),
        Prim::ArrLen(ty, list) => flat([
            pure("Arr.len "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
        ]),
        Prim::ArrGet(ty, list, index) => flat([
            pure("Arr.get "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
            pure(" "),
            print_term(index, depth),
        ]),
        Prim::ArrSlice(ty, list, start, end) => flat([
            pure("Arr.slice "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
            pure(" "),
            print_term(start, depth),
            pure(" "),
            print_term(end, depth),
        ]),
        Prim::ArrAppend(ty, list, elem) => flat([
            pure("Arr.append "),
            print_term(ty, depth),
            pure(" "),
            print_term(list, depth),
            pure(" "),
            print_term(elem, depth),
        ]),
        Prim::ArrConcat(ty, operands) => flat([
            pure("Arr.concat "),
            print_term(ty, depth),
            pure(" "),
            sep_flat(
                operands.into_iter().map(move |e| print_term(e, depth)),
                || pure(", "),
            ),
        ]),
        Prim::IoType => pure("Io"),
        Prim::Io(token) => pure(format!("Io({token})")),
        Prim::IoRead(handle, count) => flat([
            pure("Io.read "),
            print_term(handle, depth),
            pure(" "),
            print_term(count, depth),
        ]),
        Prim::IoWrite(handle, bytes) => flat([
            pure("Io.write "),
            print_term(handle, depth),
            pure(" "),
            print_term(bytes, depth),
        ]),
    }
}

fn print_term(term: Term, depth: usize) -> Printer<'static> {
    match Term::unwrap_or_clone(term) {
        Subterm::Type => pure("Type"),
        Subterm::Prim(prim) => print_prim(prim, depth),
        Subterm::FuncType(FuncType {
            telescope,
            plicities,
        }) => {
            fn walk(
                cur: Telescope<Term>,
                plicities: &[Plicity],
                depth: usize,
                total: usize,
                idx: usize,
                printers: &mut Vec<Printer<'static>>,
            ) -> Term {
                match cur {
                    Telescope::Done(body) => *body,
                    Telescope::Cons(ty, rest) => {
                        let raw = rest.first_label();
                        let label = raw
                            .map(str::to_string)
                            .unwrap_or_else(|| label_at(depth + idx));
                        let mark = match plicities.get(idx) {
                            Some(Plicity::Implicit) => "@",
                            _ => "",
                        };
                        let printer = match raw {
                            Some(_) => flat([
                                pure(mark),
                                pure(label.clone()),
                                pure(" : "),
                                print_term(ty, depth + total),
                            ]),
                            None => flat([pure(mark), print_term(ty, depth + total)]),
                        };
                        printers.push(printer);
                        let next = rest.open(&[&Term::var(Var::free(&label))]);
                        walk(next, plicities, depth, total, idx + 1, printers)
                    }
                }
            }

            let n = telescope.len();
            let mut printers = Vec::with_capacity(n);
            let output = walk(telescope, &plicities, depth, n, 0, &mut printers);
            flat([
                pure("("),
                sep_flat(printers, || pure(", ")),
                pure(") -> "),
                print_term(output, depth + n),
            ])
        }
        Subterm::Func(Func { telescope }) => {
            let n = telescope.len();
            let (labels, body) = open_telescope(telescope, depth);
            let param_str = if labels.len() == 1 {
                labels.into_iter().next().unwrap()
            } else {
                format!("({})", labels.join(", "))
            };
            flat([
                pure(param_str),
                pure(" =>\n"),
                indent(print_term(body, depth + n)),
            ])
        }
        Subterm::Apply(Apply {
            head,
            params,
            plicities,
        }) => flat([
            print_term(head, depth),
            pure("("),
            sep_flat(
                params
                    .into_iter()
                    .zip(plicities)
                    .map(|(p, plicity)| match plicity {
                        Plicity::Implicit => flat([pure("@"), print_term(p, depth)]),
                        Plicity::Explicit => print_term(p, depth),
                    })
                    .collect::<Vec<_>>(),
                || pure(", "),
            ),
            pure(")"),
        ]),
        Subterm::TupleType(TupleType { telescope }) => {
            fn walk(
                cur: Telescope<()>,
                depth: usize,
                total: usize,
                idx: usize,
                items: &mut Vec<Printer<'static>>,
            ) {
                match cur {
                    Telescope::Done(_) => {}
                    Telescope::Cons(ty, rest) => {
                        let label = rest
                            .first_label()
                            .map(str::to_string)
                            .unwrap_or_else(|| label_at(depth + idx));
                        items.push(indent(flat([
                            pure(label.clone()),
                            pure(" : "),
                            print_term(ty, depth + total),
                        ])));
                        let next = rest.open(&[&Term::var(Var::free(&label))]);
                        walk(next, depth, total, idx + 1, items);
                    }
                }
            }

            let n = telescope.len();
            let mut items = Vec::with_capacity(n);
            walk(telescope, depth, n, 0, &mut items);

            flat([pure("{ "), sep_flat(items, || pure("\n, ")), pure("\n}")])
        }
        Subterm::Tuple(Tuple { fields, names }) => {
            let mut names = names.into_iter().chain(std::iter::repeat(None));
            flat([
                pure("("),
                sep_flat(
                    fields.into_iter().map(move |f| match names.next().flatten() {
                        Some(name) => flat([pure(name), pure(" = "), print_term(f, depth)]),
                        None => print_term(f, depth),
                    }),
                    || pure(", "),
                ),
                pure(")"),
            ])
        }
        Subterm::Proj(Proj { head, field }) => {
            let field = match field {
                Field::Index(index) => format!(").{index}"),
                Field::Label(label) => format!(").{label}"),
            };
            flat([pure("("), print_term(head, depth), pure(field)])
        }
        // Params then indices, one flat argument list — exactly how the
        // type-constructor function is applied at use sites.
        Subterm::UnionType(UnionType {
            name,
            params,
            indices,
        }) => {
            if params.is_empty() && indices.is_empty() {
                pure(name)
            } else {
                flat([
                    pure(name),
                    pure("("),
                    sep_flat(
                        params
                            .into_iter()
                            .chain(indices)
                            .map(|p| print_term(p, depth))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure(")"),
                ])
            }
        }
        // Prints as the constructor-function call, instantiated type params
        // hidden — `Result/success(42)`.
        Subterm::Variant(Variant {
            name, tag, payload, ..
        }) => {
            if payload.is_empty() {
                pure(format!("{name}/{tag}"))
            } else {
                flat([
                    pure(format!("{name}/{tag}")),
                    pure("("),
                    sep_flat(
                        payload
                            .into_iter()
                            .map(|p| print_term(p, depth))
                            .collect::<Vec<_>>(),
                        || pure(", "),
                    ),
                    pure(")"),
                ])
            }
        }
        Subterm::Match(Match {
            head,
            motive,
            cases,
        }) => {
            // Arity 1 everywhere except an annotated union-match motive,
            // whose pattern binders precede the scrutinee binder.
            let motive_labels = motive
                .label_iter()
                .enumerate()
                .map(|(i, l)| l.map(str::to_string).unwrap_or_else(|| label_at(depth + i)))
                .collect::<Vec<_>>();
            let motive_terms = label_terms(&motive_labels);
            let motive_refs = motive_terms.iter().collect::<Vec<_>>();
            let motive_arity = motive_labels.len();
            let motive_label = motive_labels.join(", ");
            let motive = motive.open(&motive_refs);

            // Shared `<keyword> head : label => motive;` prefix; the keyword
            // and arm bodies depend on the case kind.
            let keyword = match &cases {
                Cases::Bln { .. } => "Bln.match ",
                Cases::Nat { .. } => "Nat.fold ",
                Cases::Switch { .. } => "Nat.match ",
                Cases::Union { .. } => "match ",
            };

            let prefix = flat([
                pure(keyword),
                print_term(head, depth),
                pure(" : "),
                pure(motive_label),
                pure(" => "),
                print_term(motive, depth + motive_arity),
                pure(";"),
            ]);

            let arms = match cases {
                Cases::Bln {
                    false_case,
                    true_case,
                } => flat([
                    pure("\n| false =>\n"),
                    indent(flat([print_term(false_case, depth), pure(";")])),
                    pure("\n| true =>\n"),
                    indent(flat([print_term(true_case, depth), pure(";")])),
                ]),
                Cases::Nat {
                    zero_case,
                    succ_case,
                } => {
                    let ((pred_label, ih_label), succ_case) = open_scope_two(succ_case, depth);
                    flat([
                        pure("\n| 0n =>\n"),
                        indent(flat([print_term(zero_case, depth), pure(";")])),
                        pure("\n| "),
                        pure(pred_label),
                        pure(" "),
                        pure(ih_label),
                        pure(" =>\n"),
                        indent(flat([print_term(succ_case, depth), pure(";")])),
                    ])
                }
                Cases::Switch { cases, default } => {
                    let case_printers = flat(
                        cases
                            .into_iter()
                            .map(|(n, body)| {
                                flat([
                                    pure(format!("\n| {n}n =>\n")),
                                    indent(flat([print_term(body, depth), pure(";")])),
                                ])
                            })
                            .collect::<Vec<_>>(),
                    );
                    flat([
                        case_printers,
                        pure("\n| _ =>\n"),
                        indent(flat([print_term(default, depth), pure(";")])),
                    ])
                }
                Cases::Union { cases, .. } => flat(
                    cases
                        .into_iter()
                        .map(|(atom, scope)| {
                            let labels = scope
                                .label_iter()
                                .enumerate()
                                .map(|(i, l)| {
                                    l.map(str::to_string).unwrap_or_else(|| label_at(depth + i))
                                })
                                .collect::<Vec<_>>();
                            let label_terms = label_terms(&labels);
                            let label_terms = label_terms.iter().collect::<Vec<_>>();
                            let body = scope.open(&label_terms);

                            let binders = if labels.is_empty() {
                                pure("")
                            } else {
                                pure(format!("({})", labels.join(", ")))
                            };

                            flat([
                                pure("\n| "),
                                print_atom(atom),
                                binders,
                                pure(" =>\n"),
                                indent(flat([print_term(body, depth + labels.len()), pure(";")])),
                            ])
                        })
                        .collect::<Vec<_>>(),
                ),
            };

            flat([prefix, arms])
        }
        Subterm::Let(Let { type_, body, tail }) => {
            let (label, tail) = open_scope_one(tail, depth);

            flat([
                pure("let "),
                pure(label),
                pure(" : "),
                print_term(type_, depth),
                pure(" =\n"),
                indent(flat([print_term(body, depth), pure(";")])),
                pure("\n"),
                print_term(tail, depth + 1),
            ])
        }
        Subterm::Rec(Rec { items, tail }) => {
            let labels = tail
                .label_iter()
                .enumerate()
                .map(|(i, l)| l.map(str::to_string).unwrap_or_else(|| label_at(depth + i)))
                .collect::<Vec<_>>();
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
        Subterm::Var(var) => print_var(var),
        Subterm::Metavar(metavar) => pure(format!("?{}", metavar.id)),
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone(), 0), formatter, 2)
    }
}

impl Display for Subterm {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_term(self.clone().into(), 0), formatter, 2)
    }
}

fn print_definition(formatter: &mut Formatter<'_>, def: &Definition) -> Result {
    write!(formatter, "{} : {} = {}", def.name, def.type_, def.body)
}

impl Display for Module {
    // Printed by *iterating* the flat items (never re-folding into a nested term),
    // so `--print core` stays O(N) and cannot re-trigger the prelude-depth
    // overflow this representation removed.
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        for item in &self.items {
            match item {
                Item::Let(def) => {
                    write!(formatter, "let ")?;
                    print_definition(formatter, def)?;
                    writeln!(formatter, ";")?;
                }
                Item::Rec(defs) => {
                    write!(formatter, "rec ")?;
                    for (index, def) in defs.iter().enumerate() {
                        if index > 0 {
                            write!(formatter, "and ")?;
                        }
                        print_definition(formatter, def)?;
                        write!(formatter, " ")?;
                    }
                    writeln!(formatter, ";")?;
                }
            }
        }

        write!(formatter, "{}", self.body)?;

        if let Some(type_) = &self.type_ {
            write!(formatter, "\n: {type_}")?;
        }

        Ok(())
    }
}
