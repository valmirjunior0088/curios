use {
    super::Context,
    crate::{
        core,
        text::{BinLiteral, Error, Match, Name, Nat, NatLiteral, NatMatch, Prim, Term},
    },
};

pub struct Elaborate<'a, 'b> {
    context: &'a Context<'b>,
}

impl<'a, 'b> Elaborate<'a, 'b> {
    pub fn new(context: &'a Context<'b>) -> Self {
        Self { context }
    }

    pub fn term(&self, term: &Term) -> Result<core::Term, Error> {
        Ok(match term {
            Term::Type => core::Subterm::Type.into(),
            Term::Prim(prim) => self.prim(prim)?.into(),
            Term::Name(name) => core::Var::free(match name.is_single() {
                true => {
                    let label = name.head();

                    if let Some(full) = self.context.bindings().get(label) {
                        full.join()
                    } else if let Some(full) = self.context.definitions().get(label) {
                        full.join()
                    } else {
                        label.to_string()
                    }
                }
                false => self.resolve_name(name)?.join(),
            })
            .into(),
            Term::Atom(atom) => core::Atom::from(atom.as_str()).into(),
            Term::AtomType(at) => {
                core::AtomType::new(at.atoms.iter().map(|atom| core::Atom::from(atom.as_str())))
                    .into()
            }
            Term::FuncType(ft) => core::FuncType::new(
                ft.params
                    .iter()
                    .map(|(label, ty)| Ok((label.clone().unwrap_or_default(), self.term(ty)?)))
                    .collect::<Result<Vec<_>, Error>>()?,
                self.term(&ft.output)?,
            )
            .into(),
            Term::Func(func) => core::Func::new(func.params.clone(), self.term(&func.body)?).into(),
            Term::Apply(apply) => core::Apply::new(
                self.term(&apply.head)?,
                apply
                    .params
                    .iter()
                    .map(|p| self.term(p))
                    .collect::<Result<Vec<_>, Error>>()?,
            )
            .into(),
            Term::TupleType(tt) => core::TupleType::new(
                tt.fields
                    .iter()
                    .map(|(label, type_)| {
                        Ok((label.clone().unwrap_or_default(), self.term(type_)?))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
            )
            .into(),
            Term::Tuple(tuple) => core::Tuple::new(
                tuple
                    .fields
                    .iter()
                    .map(|field| self.term(field))
                    .collect::<Result<Vec<_>, Error>>()?,
            )
            .into(),
            Term::Proj(proj) => core::Proj::new(self.term(&proj.head)?, proj.index).into(),
            Term::Match(match_) => match match_ {
                Match::Bln(bm) => core::BlnMatch::new(
                    self.term(&bm.head)?,
                    bm.motive.label.as_deref(),
                    self.term(&bm.motive.body)?,
                    self.term(&bm.false_case)?,
                    self.term(&bm.true_case)?,
                )
                .into(),
                Match::Nat(NatMatch::Induction {
                    head,
                    motive,
                    zero_case,
                    pred_label,
                    ih_label,
                    succ_case,
                }) => core::NatMatch::induction(
                    self.term(head)?,
                    motive.label.as_deref(),
                    self.term(&motive.body)?,
                    self.term(zero_case)?,
                    pred_label.clone(),
                    ih_label.clone(),
                    self.term(succ_case)?,
                )
                .into(),
                Match::Nat(NatMatch::Dispatch {
                    head,
                    motive,
                    cases,
                    default,
                }) => core::NatMatch::dispatch(
                    self.term(head)?,
                    motive.label.as_deref(),
                    self.term(&motive.body)?,
                    cases
                        .iter()
                        .map(|(&nat, body)| Ok((nat, self.term(body)?)))
                        .collect::<Result<Vec<_>, Error>>()?,
                    self.term(default)?,
                )
                .into(),
                Match::Atom(am) => core::Match::new(
                    self.term(&am.head)?,
                    am.motive.label.as_deref(),
                    self.term(&am.motive.body)?,
                    am.cases
                        .iter()
                        .map(|(atom, body)| Ok((core::Atom::from(atom.as_str()), self.term(body)?)))
                        .collect::<Result<Vec<_>, Error>>()?,
                )
                .into(),
            },
            Term::DefFrom(from) => core::Seal::new(
                core::Var::free(
                    self.context
                        .definitions()
                        .get(&from.label)
                        .ok_or_else(|| Error::CoercionOutsideDefBlock {
                            label: from.label.clone(),
                        })?
                        .join(),
                ),
                self.term(&from.body)?,
            )
            .into(),
            Term::DefInto(into) => core::Unseal::new(
                core::Var::free(
                    self.context
                        .definitions()
                        .get(&into.label)
                        .ok_or_else(|| Error::CoercionOutsideDefBlock {
                            label: into.label.clone(),
                        })?
                        .join(),
                ),
                self.term(&into.body)?,
            )
            .into(),
            Term::Let(let_) => core::Let::new(
                let_.label.clone(),
                self.term(&let_.type_)?,
                self.term(&let_.body)?,
                self.term(&let_.tail)?,
            )
            .into(),
            Term::Rec(rec) => core::Rec::new(
                rec.items
                    .iter()
                    .map(|it| {
                        Ok((
                            it.label.clone(),
                            self.term(&it.type_)?,
                            self.term(&it.value)?,
                        ))
                    })
                    .collect::<Result<Vec<_>, Error>>()?,
                self.term(&rec.tail)?,
            )
            .into(),
            Term::Spanned(span, inner) => core::Term::new(core::Subterm::Spanned(
                *span,
                self.term(inner).map_err(|error| error.at(*span))?,
            )),
        })
    }

    pub fn prim(&self, prim: &Prim) -> Result<core::Prim, Error> {
        Ok(match prim {
            Prim::BlnType => core::Prim::BlnType,
            Prim::Bln(b) => core::Prim::Bln(*b),
            Prim::NatType => core::Prim::NatType,
            Prim::Nat(Nat::Zero) => core::Prim::Nat(core::Nat::Zero),
            Prim::Nat(Nat::Succ(NatLiteral::Number(spine), inner)) => {
                core::Prim::Nat(core::Nat::Succ(*spine, self.term(inner)?))
            }
            Prim::Nat(Nat::Succ(NatLiteral::Char(c), inner)) => {
                core::Prim::Nat(core::Nat::Succ(*c as u32, self.term(inner)?))
            }
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
            Prim::Int(value) => core::Prim::Int(*value),
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
            Prim::SysPrint(inner) => core::Prim::sys_print(self.term(inner)?),
            Prim::SysRead => core::Prim::SysRead,
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
            Prim::BinConcat(operands) => core::Prim::bin_concat(
                operands
                    .iter()
                    .map(|operand| self.term(operand))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Prim::ArrType(inner) => core::Prim::arr_type(self.term(inner)?),
            Prim::Arr(elems) => core::Prim::Arr(
                elems
                    .iter()
                    .map(|elem| self.term(elem))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
            Prim::ArrLen(inner) => core::Prim::arr_len(self.term(inner)?),
            Prim::ArrGet(list, index) => core::Prim::arr_get(self.term(list)?, self.term(index)?),
            Prim::ArrSlice(list, start, end) => {
                core::Prim::arr_slice(self.term(list)?, self.term(start)?, self.term(end)?)
            }
            Prim::ArrAppend(list, elem) => {
                core::Prim::arr_append(self.term(list)?, self.term(elem)?)
            }
            Prim::ArrConcat(operands) => core::Prim::arr_concat(
                operands
                    .iter()
                    .map(|operand| self.term(operand))
                    .collect::<Result<Vec<_>, Error>>()?,
            ),
        })
    }

    fn resolve_name(&self, name: &Name) -> Result<Name, Error> {
        let qualifier = name.head();

        let mut current = self
            .context
            .qualifiers()
            .get(qualifier)
            .ok_or_else(|| Error::UnresolvedQualifier {
                qualifier: qualifier.to_string(),
            })?
            .clone();

        for segment in name.interior() {
            let info = self
                .context
                .table()
                .get(&current)
                .ok_or_else(|| Error::ModuleNotFound {
                    path: current.join(),
                })?;

            let is_pub = info
                .get_child(segment)
                .ok_or_else(|| Error::ChildModuleNotFound {
                    segment: segment.to_string(),
                })?;

            if !is_pub {
                return Err(Error::PrivateChildModule {
                    segment: segment.to_string(),
                });
            }

            current = current.with(segment);

            if let Some(canonical) = self.context.module_aliases().get(&current) {
                current = canonical.clone();
            }
        }

        let last = name.last();

        let info = self
            .context
            .table()
            .get(&current)
            .ok_or_else(|| Error::ModuleNotFound {
                path: current.join(),
            })?;

        let is_pub = info
            .get_binding(last)
            .ok_or_else(|| Error::BindingNotFound {
                binding: last.to_string(),
            })?;

        if !is_pub {
            return Err(Error::PrivateBinding {
                binding: last.to_string(),
            });
        }

        let mut resolved = current.with(last);

        if let Some(canonical) = self.context.binding_aliases().get(&resolved) {
            resolved = canonical.clone();
        }

        Ok(resolved)
    }
}
