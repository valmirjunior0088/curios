use {
    super::{
        Apply, BlnMatch, Context, Flt, Func, Let, Match, NatFold, NatMatch, Preempted, Prim, Proj,
        Seal, Term, Tuple, Unseal, Var,
    },
    std::time::{Duration, Instant},
};

pub fn reduce(context: &mut Context, term: &Term) -> Result<Term, Preempted> {
    Reduce::new(context.timeout()).reduce(context, term.clone())
}

enum Step {
    Continue(Term),
    Break(Term),
}

#[derive(Debug)]
struct Reduce {
    deadline: Instant,
}

impl Reduce {
    fn new(timeout: Duration) -> Self {
        Self {
            deadline: Instant::now() + timeout,
        }
    }

    fn reduce_prim(&mut self, context: &mut Context, prim: &Prim) -> Result<Term, Preempted> {
        match prim {
            Prim::BlnType => Ok(Term::Prim(Prim::BlnType)),
            Prim::Bln(value) => Ok(Term::Prim(Prim::Bln(*value))),
            Prim::NatType => Ok(Term::Prim(Prim::NatType)),
            Prim::Nat(value) => Ok(Term::Prim(Prim::Nat(*value))),
            Prim::NatEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Bln(left == right))
                    }
                    (left, right) => Term::Prim(Prim::nat_eql(left, right)),
                })
            }
            Prim::NatNeq(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Bln(left != right))
                    }
                    (left, right) => Term::Prim(Prim::nat_neq(left, right)),
                })
            }
            Prim::NatAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(left.wrapping_add(right)))
                    }
                    (left, right) => Term::Prim(Prim::nat_add(left, right)),
                })
            }
            Prim::NatSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(left.wrapping_sub(right)))
                    }
                    (left, right) => Term::Prim(Prim::nat_sub(left, right)),
                })
            }
            Prim::NatMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(left.wrapping_mul(right)))
                    }
                    (left, right) => Term::Prim(Prim::nat_mul(left, right)),
                })
            }
            Prim::NatLt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Bln(left < right))
                    }
                    (left, right) => Term::Prim(Prim::nat_lt(left, right)),
                })
            }
            Prim::NatDiv(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(left.wrapping_div(right)))
                    }
                    (left, right) => Term::Prim(Prim::nat_div(left, right)),
                })
            }
            Prim::NatRem(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(left.wrapping_rem(right)))
                    }
                    (left, right) => Term::Prim(Prim::nat_rem(left, right)),
                })
            }
            Prim::NatGt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Bln(left > right))
                    }
                    (left, right) => Term::Prim(Prim::nat_gt(left, right)),
                })
            }
            Prim::NatLte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Bln(left <= right))
                    }
                    (left, right) => Term::Prim(Prim::nat_lte(left, right)),
                })
            }
            Prim::NatGte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Bln(left >= right))
                    }
                    (left, right) => Term::Prim(Prim::nat_gte(left, right)),
                })
            }
            Prim::IntType => Ok(Term::Prim(Prim::IntType)),
            Prim::Int(value) => Ok(Term::Prim(Prim::Int(*value))),
            Prim::IntEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Bln(left == right))
                    }
                    (left, right) => Term::Prim(Prim::int_eql(left, right)),
                })
            }
            Prim::IntNeq(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Bln(left != right))
                    }
                    (left, right) => Term::Prim(Prim::int_neq(left, right)),
                })
            }
            Prim::IntAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Int(left.wrapping_add(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_add(left, right)),
                })
            }
            Prim::IntSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Int(left.wrapping_sub(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_sub(left, right)),
                })
            }
            Prim::IntMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Int(left.wrapping_mul(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_mul(left, right)),
                })
            }
            Prim::IntDiv(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Int(left.wrapping_div(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_div(left, right)),
                })
            }
            Prim::IntRem(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Int(left.wrapping_rem(right)))
                    }
                    (left, right) => Term::Prim(Prim::int_rem(left, right)),
                })
            }
            Prim::IntLt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Bln(left < right))
                    }
                    (left, right) => Term::Prim(Prim::int_lt(left, right)),
                })
            }
            Prim::IntGt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Bln(left > right))
                    }
                    (left, right) => Term::Prim(Prim::int_gt(left, right)),
                })
            }
            Prim::IntLte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Bln(left <= right))
                    }
                    (left, right) => Term::Prim(Prim::int_lte(left, right)),
                })
            }
            Prim::IntGte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Bln(left >= right))
                    }
                    (left, right) => Term::Prim(Prim::int_gte(left, right)),
                })
            }
            Prim::FltType => Ok(Term::Prim(Prim::FltType)),
            Prim::Flt(flt) => Ok(Term::Prim(Prim::Flt(*flt))),
            Prim::FltAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Flt(left + right))
                    }
                    (left, right) => Term::Prim(Prim::flt_add(left, right)),
                })
            }
            Prim::FltSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Flt(left - right))
                    }
                    (left, right) => Term::Prim(Prim::flt_sub(left, right)),
                })
            }
            Prim::FltMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Flt(left * right))
                    }
                    (left, right) => Term::Prim(Prim::flt_mul(left, right)),
                })
            }
            Prim::FltDiv(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Flt(left / right))
                    }
                    (left, right) => Term::Prim(Prim::flt_div(left, right)),
                })
            }
            Prim::FltMin(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Flt(left.min(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_min(left, right)),
                })
            }
            Prim::FltMax(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Flt(left.max(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_max(left, right)),
                })
            }
            Prim::FltEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Bln(left.eql(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_eql(left, right)),
                })
            }
            Prim::FltNeq(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Bln(left.neq(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_neq(left, right)),
                })
            }
            Prim::FltLt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Bln(left.lt(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_lt(left, right)),
                })
            }
            Prim::FltGt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Bln(left.gt(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_gt(left, right)),
                })
            }
            Prim::FltLte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Bln(left.lte(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_lte(left, right)),
                })
            }
            Prim::FltGte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Bln(left.gte(right)))
                    }
                    (left, right) => Term::Prim(Prim::flt_gte(left, right)),
                })
            }
            Prim::FltNeg(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(-flt)),
                    inner => Term::Prim(Prim::flt_neg(inner)),
                })
            }
            Prim::FltAbs(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(flt.abs())),
                    inner => Term::Prim(Prim::flt_abs(inner)),
                })
            }
            Prim::FltSqrt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(flt.sqrt())),
                    inner => Term::Prim(Prim::flt_sqrt(inner)),
                })
            }
            Prim::FltFloor(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(flt.floor())),
                    inner => Term::Prim(Prim::flt_floor(inner)),
                })
            }
            Prim::FltCeil(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(flt.ceil())),
                    inner => Term::Prim(Prim::flt_ceil(inner)),
                })
            }
            Prim::FltTrunc(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(flt.trunc())),
                    inner => Term::Prim(Prim::flt_trunc(inner)),
                })
            }
            Prim::FltNearest(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Flt(flt.nearest())),
                    inner => Term::Prim(Prim::flt_nearest(inner)),
                })
            }
            Prim::NatToStr(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Nat(v)) => Term::Prim(Prim::Bin(format!("{v}").into_bytes())),
                    inner => Term::Prim(Prim::nat_to_str(inner)),
                })
            }
            Prim::IntToStr(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Int(v)) => Term::Prim(Prim::Bin(format!("{v}").into_bytes())),
                    inner => Term::Prim(Prim::int_to_str(inner)),
                })
            }
            Prim::FltToStr(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(v)) => {
                        Term::Prim(Prim::Bin(format!("{}", v.to_f32()).into_bytes()))
                    }
                    inner => Term::Prim(Prim::flt_to_str(inner)),
                })
            }
            Prim::NatToInt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Nat(v)) => Term::Prim(Prim::Int(v as i32)),
                    inner => Term::Prim(Prim::nat_to_int(inner)),
                })
            }
            Prim::NatToFlt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Nat(v)) => Term::Prim(Prim::Flt(Flt::from_f32(v as f32))),
                    inner => Term::Prim(Prim::nat_to_flt(inner)),
                })
            }
            Prim::IntToNat(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Int(v)) => Term::Prim(Prim::Nat(v as u32)),
                    inner => Term::Prim(Prim::int_to_nat(inner)),
                })
            }
            Prim::IntToFlt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Int(v)) => Term::Prim(Prim::Flt(Flt::from_f32(v as f32))),
                    inner => Term::Prim(Prim::int_to_flt(inner)),
                })
            }
            Prim::FltToNat(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Nat(flt.to_f32() as u32)),
                    inner => Term::Prim(Prim::flt_to_nat(inner)),
                })
            }
            Prim::FltToInt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(flt)) => Term::Prim(Prim::Int(flt.to_f32() as i32)),
                    inner => Term::Prim(Prim::flt_to_int(inner)),
                })
            }
            Prim::BinType => Ok(Term::Prim(Prim::BinType)),
            Prim::Bin(bytes) => Ok(Term::Prim(Prim::Bin(bytes.clone()))),
            Prim::BinLen(bin) => {
                let bin = self.reduce(context, bin.as_ref().clone())?;
                Ok(match bin {
                    Term::Prim(Prim::Bin(bytes)) => Term::Prim(Prim::Nat(bytes.len() as u32)),
                    bin => Term::Prim(Prim::bin_len(bin)),
                })
            }
            Prim::BinEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Bin(left)), Term::Prim(Prim::Bin(right))) => {
                        Term::Prim(Prim::Bln(left == right))
                    }
                    (left, right) => Term::Prim(Prim::bin_eql(left, right)),
                })
            }
            Prim::BinGet(bin, index) => {
                let bin = self.reduce(context, bin.as_ref().clone())?;
                let index = self.reduce(context, index.as_ref().clone())?;
                Ok(match (bin, index) {
                    (Term::Prim(Prim::Bin(bytes)), Term::Prim(Prim::Nat(i))) => {
                        Term::Prim(Prim::Nat(bytes[i as usize] as u32))
                    }
                    (bin, index) => Term::Prim(Prim::bin_get(bin, index)),
                })
            }
            Prim::BinSlice(bin, start, end) => {
                let bin = self.reduce(context, bin.as_ref().clone())?;
                let start = self.reduce(context, start.as_ref().clone())?;
                let end = self.reduce(context, end.as_ref().clone())?;
                Ok(match (bin, start, end) {
                    (
                        Term::Prim(Prim::Bin(bytes)),
                        Term::Prim(Prim::Nat(s)),
                        Term::Prim(Prim::Nat(e)),
                    ) => Term::Prim(Prim::Bin(bytes[s as usize..e as usize].to_vec())),
                    (bin, start, end) => Term::Prim(Prim::bin_slice(bin, start, end)),
                })
            }
            Prim::BinAppend(bin, byte) => {
                let bin = self.reduce(context, bin.as_ref().clone())?;
                let byte = self.reduce(context, byte.as_ref().clone())?;
                Ok(match (bin, byte) {
                    (Term::Prim(Prim::Bin(mut bytes)), Term::Prim(Prim::Nat(n))) => {
                        bytes.push(n as u8);
                        Term::Prim(Prim::Bin(bytes))
                    }
                    (bin, byte) => Term::Prim(Prim::bin_append(bin, byte)),
                })
            }
            Prim::BinConcat(operands) => {
                let reduced: Vec<Term> = operands
                    .iter()
                    .map(|e| self.reduce(context, e.as_ref().clone()))
                    .collect::<Result<_, _>>()?;
                let merged = reduced.iter().try_fold(Vec::new(), |mut acc, t| {
                    if let Term::Prim(Prim::Bin(b)) = t {
                        acc.extend(b);
                        Some(acc)
                    } else {
                        None
                    }
                });
                Ok(match merged {
                    Some(bytes) => Term::Prim(Prim::Bin(bytes)),
                    None => Term::Prim(Prim::BinConcat(
                        reduced.into_iter().map(|t| t.into()).collect(),
                    )),
                })
            }
            Prim::ArrType(elem) => {
                let elem = self.reduce(context, elem.as_ref().clone())?;
                Ok(Term::Prim(Prim::arr_type(elem)))
            }
            Prim::Arr(elems) => {
                let elems = elems
                    .iter()
                    .map(|e| self.reduce(context, e.as_ref().clone()).map(|t| t.into()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Term::Prim(Prim::Arr(elems)))
            }
            Prim::ArrLen(list) => {
                let list = self.reduce(context, list.as_ref().clone())?;
                Ok(match list {
                    Term::Prim(Prim::Arr(elems)) => Term::Prim(Prim::Nat(elems.len() as u32)),
                    list => Term::Prim(Prim::arr_len(list)),
                })
            }
            Prim::ArrGet(list, index) => {
                let list = self.reduce(context, list.as_ref().clone())?;
                let index = self.reduce(context, index.as_ref().clone())?;
                Ok(match (list, index) {
                    (Term::Prim(Prim::Arr(elems)), Term::Prim(Prim::Nat(index))) => *elems
                        .into_iter()
                        .nth(index as usize)
                        .expect("Arr.get: index out of bounds"),
                    (list, index) => Term::Prim(Prim::arr_get(list, index)),
                })
            }
            Prim::ArrSlice(list, start, end) => {
                let list = self.reduce(context, list.as_ref().clone())?;
                let start = self.reduce(context, start.as_ref().clone())?;
                let end = self.reduce(context, end.as_ref().clone())?;
                Ok(match (list, start, end) {
                    (
                        Term::Prim(Prim::Arr(elems)),
                        Term::Prim(Prim::Nat(start)),
                        Term::Prim(Prim::Nat(end)),
                    ) => Term::Prim(Prim::Arr(elems[start as usize..end as usize].to_vec())),
                    (list, start, end) => Term::Prim(Prim::arr_slice(list, start, end)),
                })
            }
            Prim::ArrAppend(list, elem) => {
                let list = self.reduce(context, list.as_ref().clone())?;
                let elem = self.reduce(context, elem.as_ref().clone())?;
                Ok(match list {
                    Term::Prim(Prim::Arr(mut elems)) => {
                        elems.push(elem.into());
                        Term::Prim(Prim::Arr(elems))
                    }
                    list => Term::Prim(Prim::arr_append(list, elem)),
                })
            }
            Prim::ArrConcat(operands) => {
                let reduced: Vec<Term> = operands
                    .iter()
                    .map(|e| self.reduce(context, e.as_ref().clone()))
                    .collect::<Result<_, _>>()?;
                let merged = reduced.iter().try_fold(Vec::new(), |mut acc, t| {
                    if let Term::Prim(Prim::Arr(elems)) = t {
                        acc.extend(elems.iter().cloned());
                        Some(acc)
                    } else {
                        None
                    }
                });
                Ok(match merged {
                    Some(elems) => Term::Prim(Prim::Arr(elems)),
                    None => Term::Prim(Prim::ArrConcat(
                        reduced.into_iter().map(|t| t.into()).collect(),
                    )),
                })
            }
            Prim::SysPrint(_) => panic!("SysPrint cannot appear at the type level"),
            Prim::SysRead => panic!("SysRead cannot appear at the type level"),
        }
    }

    fn reduce_apply(&mut self, context: &mut Context, apply: Apply) -> Result<Step, Preempted> {
        let Apply { head, param } = apply;
        match self.reduce(context, *head)? {
            Term::Func(Func { body }) => Ok(Step::Continue(body.open(&[param.as_ref()]))),
            head => Ok(Step::Break(
                Apply {
                    head: head.into(),
                    param,
                }
                .into(),
            )),
        }
    }

    fn reduce_proj(&mut self, context: &mut Context, proj: Proj) -> Result<Step, Preempted> {
        let Proj { head, index } = proj;
        if let Some(v) = context.projection(&head, index) {
            return Ok(Step::Continue(v.clone()));
        }
        match self.reduce(context, *head)? {
            Term::Tuple(Tuple { fields }) => Ok(Step::Continue(
                *fields
                    .into_iter()
                    .nth(index)
                    .expect("Proj: index out of bounds"),
            )),
            head => match context.projection(&head, index) {
                Some(v) => Ok(Step::Continue(v.clone())),
                None => Ok(Step::Break(
                    Proj {
                        head: head.into(),
                        index,
                    }
                    .into(),
                )),
            },
        }
    }

    fn reduce_func_eta(&mut self, context: &mut Context, func: Func) -> Result<Step, Preempted> {
        let fresh = context.fresh();
        let y: Term = Var::free(&fresh).into();
        match func.body.open(&[&y]) {
            Term::Apply(Apply { head, param })
                if matches!(param.as_ref(), Term::Var(v) if v.unwrap() == fresh.as_str())
                    && !head.free_vars().contains(&fresh) =>
            {
                Ok(Step::Continue(*head))
            }
            _ => Ok(Step::Break(func.into())),
        }
    }

    fn eta_reduce_tuple(tuple: Tuple) -> Term {
        let n = tuple.fields.len();
        if n == 0 {
            return tuple.into();
        }
        let mut base: Option<Term> = None;
        for (i, f) in tuple.fields.iter().enumerate() {
            match f.as_ref() {
                Term::Proj(Proj { head, index }) if *index == i => {
                    let h = (**head).clone();
                    match &base {
                        None => base = Some(h),
                        Some(b) if b == &h => {}
                        _ => return tuple.into(),
                    }
                }
                _ => return tuple.into(),
            }
        }
        base.unwrap()
    }

    fn reduce_nat_fold(
        &mut self,
        context: &mut Context,
        nat_fold: NatFold,
    ) -> Result<Step, Preempted> {
        let NatFold {
            head,
            motive,
            zero_case,
            succ_case,
        } = nat_fold;

        match self.reduce(context, *head)? {
            Term::Prim(Prim::Nat(0)) => Ok(Step::Continue(*zero_case)),
            Term::Prim(Prim::Nat(n)) => {
                let pred = Term::Prim(Prim::Nat(n - 1));
                let ih = Term::NatFold(NatFold {
                    head: pred.clone().into(),
                    motive: motive.clone(),
                    zero_case: zero_case.clone(),
                    succ_case: succ_case.clone(),
                });
                Ok(Step::Continue(succ_case.open(&[&pred, &ih])))
            }
            head => Ok(Step::Break(
                NatFold {
                    head: head.into(),
                    motive,
                    zero_case,
                    succ_case,
                }
                .into(),
            )),
        }
    }

    fn reduce_bln_match(&mut self, context: &mut Context, bm: BlnMatch) -> Result<Step, Preempted> {
        let BlnMatch {
            head,
            motive,
            false_case,
            true_case,
        } = bm;
        match self.reduce(context, *head)? {
            Term::Prim(Prim::Bln(false)) => Ok(Step::Continue(*false_case)),
            Term::Prim(Prim::Bln(true)) => Ok(Step::Continue(*true_case)),
            head => Ok(Step::Break(
                BlnMatch {
                    head: head.into(),
                    motive,
                    false_case,
                    true_case,
                }
                .into(),
            )),
        }
    }

    fn reduce_nat_match(&mut self, context: &mut Context, nm: NatMatch) -> Result<Step, Preempted> {
        let NatMatch {
            head,
            motive,
            cases,
            default,
        } = nm;
        match self.reduce(context, *head)? {
            Term::Prim(Prim::Nat(n)) => match cases.get(&n) {
                Some(body) => Ok(Step::Continue(body.as_ref().clone())),
                None => Ok(Step::Continue(*default)),
            },
            head => Ok(Step::Break(
                NatMatch {
                    head: head.into(),
                    motive,
                    cases,
                    default,
                }
                .into(),
            )),
        }
    }

    fn reduce_match(&mut self, context: &mut Context, m: Match) -> Result<Step, Preempted> {
        let Match {
            head,
            motive,
            cases,
        } = m;
        let atom = match self.reduce(context, *head)? {
            Term::Atom(atom) => atom,
            head => {
                return Ok(Step::Break(
                    Match {
                        head: head.into(),
                        motive,
                        cases,
                    }
                    .into(),
                ));
            }
        };

        match cases.get(&atom) {
            Some(body) => Ok(Step::Continue(body.as_ref().clone())),
            None => Ok(Step::Break(
                Match {
                    head: Term::from(atom).into(),
                    motive,
                    cases,
                }
                .into(),
            )),
        }
    }

    fn reduce_unseal(&mut self, context: &mut Context, unseal: Unseal) -> Result<Step, Preempted> {
        let Unseal { witness, value } = unseal;
        match self.reduce(context, *value)? {
            Term::Seal(Seal {
                value: sealed_value,
                ..
            }) => Ok(Step::Continue(*sealed_value)),
            value => Ok(Step::Break(
                Unseal {
                    witness,
                    value: value.into(),
                }
                .into(),
            )),
        }
    }

    fn reduce_let(&self, let_: Let) -> Step {
        Step::Continue(let_.tail.open(&[let_.body.as_ref()]))
    }

    fn reduce_var(&self, context: &Context, var: Var) -> Step {
        match context.definition(var.unwrap()) {
            Some(next) => Step::Continue(next.clone()),
            None => Step::Break(var.into()),
        }
    }

    fn reduce(&mut self, context: &mut Context, mut term: Term) -> Result<Term, Preempted> {
        loop {
            if Instant::now() > self.deadline {
                break Err(Preempted);
            }

            let step = match term {
                Term::Prim(prim) => Step::Break(self.reduce_prim(context, &prim)?),
                Term::BlnMatch(bm) => self.reduce_bln_match(context, bm)?,
                Term::NatFold(nat_fold) => self.reduce_nat_fold(context, nat_fold)?,
                Term::NatMatch(nm) => self.reduce_nat_match(context, nm)?,
                Term::Apply(apply) => self.reduce_apply(context, apply)?,
                Term::Proj(proj) => self.reduce_proj(context, proj)?,
                Term::Func(func) => self.reduce_func_eta(context, func)?,
                Term::Match(m) => self.reduce_match(context, m)?,
                Term::Let(let_) => self.reduce_let(let_),
                Term::Unseal(unseal) => self.reduce_unseal(context, unseal)?,
                Term::Var(var) => self.reduce_var(context, var),
                Term::Tuple(t) => Step::Break(Self::eta_reduce_tuple(t)),
                term => Step::Break(term),
            };

            match step {
                Step::Continue(next) => term = next,
                Step::Break(result) => break Ok(result),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::core::{
            Atom, AtomType, Let, Match, NatFold, Prim, Seal, Sealed, Tuple, Type, Unseal, Var,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn reduce_apply_beta_reduces() {
        let mut context = context();

        let term = Apply::many(Func::new("x", Var::free("x")), [Atom::from("ok")]);

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("ok").into()));
    }

    #[test]
    fn reduce_match_selects_match() {
        let mut context = context();

        let term = Match::new(
            Atom::from("a"),
            "m",
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        )
        .into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("yes").into()));
    }

    #[test]
    fn reduce_nat_fold_zero_is_not_true() {
        let mut context = context();

        let term = NatFold::new(
            Prim::Nat(0),
            "m",
            AtomType::new(["false", "true"]),
            Atom::from("false"),
            "pred",
            "ih",
            Atom::from("true"),
        )
        .into();

        assert_ne!(reduce(&mut context, &term), Ok(Atom::from("true").into()));
    }

    #[test]
    fn reduce_let_then_var_unfolds_definition() {
        let mut context = context();

        context.define("y", &Atom::from("done").into());

        let term = Let::new("x", Type, Var::free("y"), Var::free("x")).into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("done").into()));
    }

    #[test]
    fn reduce_var_cycle_times_out() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        assert_eq!(
            reduce(&mut context, &Var::free("loop").into()),
            Err(Preempted)
        );
    }

    #[test]
    fn reduce_int_add_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_add(
                    Term::Prim(Prim::Int(1)),
                    Term::Prim(Prim::Int(2))
                ))
            ),
            Ok(Term::Prim(Prim::Int(3)))
        );
    }

    #[test]
    fn reduce_int_eql_returns_true_or_false_atom() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::Int(4)),
                    Term::Prim(Prim::Int(4))
                ))
            ),
            Ok(Term::Prim(Prim::Bln(true)))
        );
        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::Int(4)),
                    Term::Prim(Prim::Int(5))
                ))
            ),
            Ok(Term::Prim(Prim::Bln(false)))
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::flt_mul(
                    Term::Prim(Prim::Flt(Flt::from_f32(1.5))),
                    Term::Prim(Prim::Flt(Flt::from_f32(2.0)))
                ))
            ),
            Ok(Term::Prim(Prim::Flt(Flt::from_f32(3.0))))
        );
    }

    #[test]
    fn reduce_lst_get_returns_element_at_index() {
        let mut context = context();

        let list = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(10)),
            Term::Prim(Prim::Nat(20)),
            Term::Prim(Prim::Nat(30)),
        ]));

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::arr_get(list.clone(), Term::Prim(Prim::Nat(0))))
            ),
            Ok(Term::Prim(Prim::Nat(10)))
        );
        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::arr_get(list, Term::Prim(Prim::Nat(2))))
            ),
            Ok(Term::Prim(Prim::Nat(30)))
        );
    }

    #[test]
    fn reduce_unseal_fires_on_sealed_value() {
        let mut context = context();

        let term = Unseal::new(Var::free("x"), Seal::new(Var::free("x"), Atom::from("ok"))).into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("ok").into()));
    }

    #[test]
    fn reduce_unseal_stuck_on_free_var() {
        let mut context = context();

        let term: Term = Unseal::new(Var::free("x"), Var::free("v")).into();

        assert_eq!(reduce(&mut context, &term), Ok(term));
    }

    #[test]
    fn reduce_sealed_is_stuck() {
        let mut context = context();

        let term: Term = Sealed::new("x", Type, Var::free("x")).into();

        assert_eq!(reduce(&mut context, &term), Ok(term));
    }

    #[test]
    #[should_panic(expected = "Arr.get: index out of bounds")]
    fn reduce_lst_get_panics_on_out_of_bounds() {
        let mut context = context();

        let list = Term::Prim(Prim::from(vec![Term::Prim(Prim::Nat(1))]));

        reduce(
            &mut context,
            &Term::Prim(Prim::arr_get(list, Term::Prim(Prim::Nat(1)))),
        )
        .ok();
    }

    #[test]
    fn reduce_bin_append_adds_byte() {
        let mut context = context();

        let bin = Term::Prim(Prim::Bin(vec![1, 2]));
        let byte = Term::Prim(Prim::Nat(3));

        assert_eq!(
            reduce(&mut context, &Term::Prim(Prim::bin_append(bin, byte))),
            Ok(Term::Prim(Prim::Bin(vec![1, 2, 3])))
        );
    }

    #[test]
    fn reduce_lst_append_adds_element() {
        let mut context = context();

        let list = Term::Prim(Prim::from(vec![
            Term::Prim(Prim::Nat(10)),
            Term::Prim(Prim::Nat(20)),
        ]));

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::arr_append(list, Term::Prim(Prim::Nat(30))))
            ),
            Ok(Term::Prim(Prim::from(vec![
                Term::Prim(Prim::Nat(10)),
                Term::Prim(Prim::Nat(20)),
                Term::Prim(Prim::Nat(30)),
            ])))
        );
    }

    #[test]
    fn reduce_proj_beta_reduces() {
        let mut context = context();

        let term: Term = Proj::new(Tuple::new([Atom::from("a"), Atom::from("b")]), 1).into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("b").into()));
    }

    #[test]
    fn reduce_proj_table_lookup() {
        let mut context = context();

        context.define_proj(Var::free("r").into(), 0, &Atom::from("ok").into());

        let term: Term = Proj::new(Var::free("r"), 0).into();

        assert_eq!(reduce(&mut context, &term), Ok(Atom::from("ok").into()));
    }

    #[test]
    fn eta_reduce_tuple_fires() {
        let mut context = context();

        let term: Term =
            Tuple::new([Proj::new(Var::free("r"), 0), Proj::new(Var::free("r"), 1)]).into();

        assert_eq!(reduce(&mut context, &term), Ok(Var::free("r").into()));
    }

    #[test]
    fn eta_reduce_func_fires() {
        let mut context = context();

        let term: Term = Func::new("y", Apply::new(Var::free("f"), Var::free("y"))).into();

        assert_eq!(reduce(&mut context, &term), Ok(Var::free("f").into()));
    }
}
