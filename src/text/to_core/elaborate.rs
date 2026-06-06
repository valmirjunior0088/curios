use {
    super::Context,
    crate::{
        core,
        text::{BinLiteral, Error, Match, Nat, NatLiteral, NatMatch, Prim, Subterm, Term},
    },
    num_bigint::BigUint,
    std::collections::BTreeMap,
};

pub struct Elaborate<'a, 'b> {
    context: &'a Context<'b>,
}

impl<'a, 'b> Elaborate<'a, 'b> {
    pub fn new(context: &'a Context<'b>) -> Self {
        Self { context }
    }

    pub fn term(&self, term: &Term) -> Result<core::Term, Error> {
        let span = term.span().cloned();
        let elaborated = match span.as_ref() {
            Some(s) => self
                .subterm(term.as_subterm())
                .map_err(|error| error.at(s.clone()))?,
            None => self.subterm(term.as_subterm())?,
        };
        Ok(match span {
            Some(s) => core::Term::spanned(s, elaborated),
            None => elaborated,
        })
    }

    fn subterm(&self, term: &Subterm) -> Result<core::Term, Error> {
        Ok(match term {
            Subterm::Type => core::Term::type_(),
            Subterm::Hole => core::Term::metavar(self.context.fresh_metavar()),
            Subterm::Prim(prim) => core::Term::prim(self.prim(prim)?),
            Subterm::Name(name) => {
                let resolved = if name.is_abs() || !name.is_single() {
                    self.context.resolve_term_name(name)?.join()
                } else {
                    let label = name.head();

                    match self.context.bindings().get(label) {
                        Some(full) => full.join(),
                        None => label.to_string(),
                    }
                };

                core::Term::var(core::Var::free(resolved))
            }
            Subterm::Atom(atom) => core::Term::atom(core::Atom::from(atom.as_str())),
            Subterm::AtomType(at) => {
                core::Term::atom_type(at.atoms.iter().map(|atom| core::Atom::from(atom.as_str())))
            }
            Subterm::FuncType(ft) => core::Term::func_type(
                ft.params
                    .iter()
                    .map(|(label, ty)| Ok((label.clone().unwrap_or_default(), self.term(ty)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
                self.term(&ft.output)?,
            ),
            Subterm::Func(func) => {
                // An omitted argument annotation lowers to a fresh hole, exactly
                // like a `Subterm::Hole`; the domain is then solved (or, once the
                // surface carries annotations, checked) when elaborating the
                // lambda against its expected function type.
                let params = func
                    .params
                    .iter()
                    .map(|name| (name.clone(), core::Term::metavar(self.context.fresh_metavar())))
                    .collect::<Vec<_>>();
                core::Term::func(params, self.term(&func.body)?)
            }
            Subterm::Apply(apply) => core::Term::apply(
                self.term(&apply.head)?,
                apply
                    .params
                    .iter()
                    .map(|p| self.term(p))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::TupleType(tt) => core::Term::tuple_type(
                tt.fields
                    .iter()
                    .map(|(label, type_)| {
                        Ok((label.clone().unwrap_or_default(), self.term(type_)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Tuple(tuple) => core::Term::tuple(
                tuple
                    .fields
                    .iter()
                    .map(|field| self.term(field))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Subterm::Proj(proj) => core::Term::proj(self.term(&proj.head)?, proj.index),
            Subterm::Match(match_) => match match_ {
                Match::Bln(bm) => core::Term::bln_match(
                    self.term(&bm.head)?,
                    bm.motive.label.as_deref(),
                    self.term(&bm.motive.body)?,
                    self.term(&bm.false_case)?,
                    self.term(&bm.true_case)?,
                ),
                Match::Nat(NatMatch::Induction {
                    head,
                    motive,
                    zero_case,
                    pred_label,
                    ih_label,
                    succ_case,
                }) => core::Term::nat_induction(
                    self.term(head)?,
                    motive.label.as_deref(),
                    self.term(&motive.body)?,
                    self.term(zero_case)?,
                    pred_label.clone(),
                    ih_label.clone(),
                    self.term(succ_case)?,
                ),
                Match::Nat(NatMatch::Dispatch {
                    head,
                    motive,
                    cases,
                    default,
                }) => core::Term::nat_dispatch(
                    self.term(head)?,
                    motive.label.as_deref(),
                    self.term(&motive.body)?,
                    cases
                        .iter()
                        .map(|(&nat, body)| Ok((nat, self.term(body)?)))
                        .collect::<Result<Vec<_>, Error>>()?,
                    self.term(default)?,
                ),
                Match::Atom(am) => core::Term::match_(
                    self.term(&am.head)?,
                    am.motive.label.as_deref(),
                    self.term(&am.motive.body)?,
                    am.cases
                        .iter()
                        .map(|(atom, body)| Ok((core::Atom::from(atom.as_str()), self.term(body)?)))
                        .collect::<Result<Vec<_>, Error>>()?,
                ),
                Match::Union(um) => {
                    // A union match desugars to an atom match on the projected tag:
                    // the scrutinee's tag (field 0) selects the arm, and each arm's
                    // binders are bound to the payload's (field 1) projections.
                    let head = self.term(&um.head)?;
                    let tag = core::Term::proj(head.clone(), 0);
                    let payload = core::Term::proj(head, 1);

                    let motive_body = self.term(&um.motive.body)?;
                    let motive = match um.motive.label.as_deref() {
                        Some(label) => core::Scope::close(core::One, &[label], motive_body),
                        None => core::Scope::constant(core::One, motive_body),
                    };

                    let cases = um
                        .cases
                        .iter()
                        .map(|(label, case)| {
                            let body = self.term(&case.body)?;
                            let binder_strs =
                                case.binders.iter().map(String::as_str).collect::<Vec<_>>();
                            let scope = core::Scope::close(
                                core::Many(case.binders.len()),
                                &binder_strs,
                                body,
                            );
                            let projections = (0..case.binders.len())
                                .map(|i| core::Term::proj(payload.clone(), i))
                                .collect::<Vec<_>>();
                            let refs = projections.iter().collect::<Vec<_>>();
                            Ok((core::Atom::from(label.as_str()), scope.open(&refs)))
                        })
                        .collect::<Result<BTreeMap<_, _>, Error>>()?;

                    core::Subterm::Match(core::Match {
                        head: tag,
                        motive,
                        cases,
                    })
                    .into()
                }
            },
            Subterm::Let(let_) => core::Term::let_(
                let_.label.clone(),
                self.term(&let_.signature.type_())?,
                self.term(&let_.signature.body())?,
                self.term(&let_.tail)?,
            ),
            Subterm::Rec(rec) => core::Term::rec(
                rec.items
                    .iter()
                    .map(|it| {
                        Ok((
                            it.label.clone(),
                            self.term(&it.signature.type_())?,
                            self.term(&it.signature.body())?,
                        ))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
                self.term(&rec.tail)?,
            ),
        })
    }

    pub fn prim(&self, prim: &Prim) -> Result<core::Prim, Error> {
        Ok(match prim {
            Prim::BlnType => core::Prim::BlnType,
            Prim::Bln(b) => core::Prim::Bln(*b),
            Prim::NatType => core::Prim::NatType,
            Prim::Nat(Nat::Zero) => core::Prim::Nat(core::Nat::Zero),
            Prim::Nat(Nat::Succ(NatLiteral::Number(spine), inner)) => {
                core::Prim::Nat(core::Nat::Succ(spine.clone(), self.term(inner)?))
            }
            Prim::Nat(Nat::Succ(NatLiteral::Char(c), inner)) => core::Prim::Nat(core::Nat::Succ(
                BigUint::from(*c as usize),
                self.term(inner)?,
            )),
            Prim::NatEql(left, right) => core::Prim::nat_eql(self.term(left)?, self.term(right)?),
            Prim::NatNeq(left, right) => core::Prim::nat_neq(self.term(left)?, self.term(right)?),
            Prim::NatAdd(left, right) => core::Prim::nat_add(self.term(left)?, self.term(right)?),
            Prim::NatSub(left, right) => core::Prim::nat_sub(self.term(left)?, self.term(right)?),
            Prim::NatMul(left, right) => core::Prim::nat_mul(self.term(left)?, self.term(right)?),
            Prim::NatLt(left, right) => core::Prim::nat_lt(self.term(left)?, self.term(right)?),
            Prim::NatDiv(left, right) => core::Prim::nat_div(self.term(left)?, self.term(right)?),
            Prim::NatRem(left, right) => core::Prim::nat_rem(self.term(left)?, self.term(right)?),
            Prim::NatGt(left, right) => core::Prim::nat_gt(self.term(left)?, self.term(right)?),
            Prim::NatLte(left, right) => core::Prim::nat_lte(self.term(left)?, self.term(right)?),
            Prim::NatGte(left, right) => core::Prim::nat_gte(self.term(left)?, self.term(right)?),
            Prim::IntType => core::Prim::IntType,
            Prim::Int(value) => core::Prim::Int(core::Int::new(*value as i64)),
            Prim::IntEql(left, right) => core::Prim::int_eql(self.term(left)?, self.term(right)?),
            Prim::IntNeq(left, right) => core::Prim::int_neq(self.term(left)?, self.term(right)?),
            Prim::IntAdd(left, right) => core::Prim::int_add(self.term(left)?, self.term(right)?),
            Prim::IntSub(left, right) => core::Prim::int_sub(self.term(left)?, self.term(right)?),
            Prim::IntMul(left, right) => core::Prim::int_mul(self.term(left)?, self.term(right)?),
            Prim::IntDiv(left, right) => core::Prim::int_div(self.term(left)?, self.term(right)?),
            Prim::IntRem(left, right) => core::Prim::int_rem(self.term(left)?, self.term(right)?),
            Prim::IntLt(left, right) => core::Prim::int_lt(self.term(left)?, self.term(right)?),
            Prim::IntGt(left, right) => core::Prim::int_gt(self.term(left)?, self.term(right)?),
            Prim::IntLte(left, right) => core::Prim::int_lte(self.term(left)?, self.term(right)?),
            Prim::IntGte(left, right) => core::Prim::int_gte(self.term(left)?, self.term(right)?),
            Prim::FltType => core::Prim::FltType,
            Prim::Flt(flt) => core::Prim::Flt(core::Flt::from_f32(*flt)),
            Prim::FltAdd(left, right) => core::Prim::flt_add(self.term(left)?, self.term(right)?),
            Prim::FltSub(left, right) => core::Prim::flt_sub(self.term(left)?, self.term(right)?),
            Prim::FltMul(left, right) => core::Prim::flt_mul(self.term(left)?, self.term(right)?),
            Prim::FltDiv(left, right) => core::Prim::flt_div(self.term(left)?, self.term(right)?),
            Prim::FltEql(left, right) => core::Prim::flt_eql(self.term(left)?, self.term(right)?),
            Prim::FltNeq(left, right) => core::Prim::flt_neq(self.term(left)?, self.term(right)?),
            Prim::FltLt(left, right) => core::Prim::flt_lt(self.term(left)?, self.term(right)?),
            Prim::FltGt(left, right) => core::Prim::flt_gt(self.term(left)?, self.term(right)?),
            Prim::FltLte(left, right) => core::Prim::flt_lte(self.term(left)?, self.term(right)?),
            Prim::FltGte(left, right) => core::Prim::flt_gte(self.term(left)?, self.term(right)?),
            Prim::FltMin(left, right) => core::Prim::flt_min(self.term(left)?, self.term(right)?),
            Prim::FltMax(left, right) => core::Prim::flt_max(self.term(left)?, self.term(right)?),
            Prim::FltNeg(inner) => core::Prim::flt_neg(self.term(inner)?),
            Prim::FltAbs(inner) => core::Prim::flt_abs(self.term(inner)?),
            Prim::FltSqrt(inner) => core::Prim::flt_sqrt(self.term(inner)?),
            Prim::FltFloor(inner) => core::Prim::flt_floor(self.term(inner)?),
            Prim::FltCeil(inner) => core::Prim::flt_ceil(self.term(inner)?),
            Prim::FltTrunc(inner) => core::Prim::flt_trunc(self.term(inner)?),
            Prim::FltNearest(inner) => core::Prim::flt_nearest(self.term(inner)?),
            Prim::NatToStr(inner) => core::Prim::nat_to_str(self.term(inner)?),
            Prim::IoPrint(inner) => core::Prim::io_print(self.term(inner)?),
            Prim::IoRead => core::Prim::IoRead,
            Prim::IntToStr(inner) => core::Prim::int_to_str(self.term(inner)?),
            Prim::FltToStr(inner) => core::Prim::flt_to_str(self.term(inner)?),
            Prim::NatToInt(inner) => core::Prim::nat_to_int(self.term(inner)?),
            Prim::NatToFlt(inner) => core::Prim::nat_to_flt(self.term(inner)?),
            Prim::IntToNat(inner) => core::Prim::int_to_nat(self.term(inner)?),
            Prim::IntToFlt(inner) => core::Prim::int_to_flt(self.term(inner)?),
            Prim::FltToNat(inner) => core::Prim::flt_to_nat(self.term(inner)?),
            Prim::FltToInt(inner) => core::Prim::flt_to_int(self.term(inner)?),
            Prim::BinType => core::Prim::BinType,
            Prim::Bin(BinLiteral::Bytes(bytes)) => core::Prim::Bin(bytes.clone()),
            Prim::Bin(BinLiteral::String(string)) => core::Prim::Bin(string.as_bytes().to_vec()),
            Prim::BinLen(inner) => core::Prim::bin_len(self.term(inner)?),
            Prim::BinEql(left, right) => core::Prim::bin_eql(self.term(left)?, self.term(right)?),
            Prim::BinGet(bin, index) => core::Prim::bin_get(self.term(bin)?, self.term(index)?),
            Prim::BinSlice(bin, start, end) => {
                core::Prim::bin_slice(self.term(bin)?, self.term(start)?, self.term(end)?)
            }
            Prim::BinAppend(bin, byte) => core::Prim::bin_append(self.term(bin)?, self.term(byte)?),
            Prim::BinConcat(left, right) => {
                core::Prim::bin_concat([self.term(left)?, self.term(right)?])
            }
            Prim::ArrType(inner) => core::Prim::arr_type(self.term(inner)?),
            Prim::Arr(elems) => core::Prim::Arr(
                elems
                    .iter()
                    .map(|elem| self.term(elem))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Prim::ArrLen(ty, inner) => core::Prim::arr_len(self.term(ty)?, self.term(inner)?),
            Prim::ArrGet(ty, list, index) => {
                core::Prim::arr_get(self.term(ty)?, self.term(list)?, self.term(index)?)
            }
            Prim::ArrSlice(ty, list, start, end) => core::Prim::arr_slice(
                self.term(ty)?,
                self.term(list)?,
                self.term(start)?,
                self.term(end)?,
            ),
            Prim::ArrAppend(ty, list, elem) => {
                core::Prim::arr_append(self.term(ty)?, self.term(list)?, self.term(elem)?)
            }
            Prim::ArrConcat(ty, left, right) => {
                core::Prim::arr_concat(self.term(ty)?, [self.term(left)?, self.term(right)?])
            }
        })
    }
}
