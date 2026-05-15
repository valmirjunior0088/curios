use {
    super::{Apply, Context, Func, Let, Match, Pair, Preempted, Prim, Split, Term, Var},
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
            Prim::NatType => Ok(Term::Prim(Prim::NatType)),
            Prim::Nat(value) => Ok(Term::Prim(Prim::Nat(*value))),
            Prim::NatEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(if left == right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::nat_eql(left, right)),
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
            Prim::NatNeq(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(if left != right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::nat_neq(left, right)),
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
            Prim::NatLt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(if left < right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::nat_lt(left, right)),
                })
            }
            Prim::NatGt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(if left > right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::nat_gt(left, right)),
                })
            }
            Prim::NatLte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(if left <= right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::nat_lte(left, right)),
                })
            }
            Prim::NatGte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Nat(left)), Term::Prim(Prim::Nat(right))) => {
                        Term::Prim(Prim::Nat(if left >= right { 1 } else { 0 }))
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
                        Term::Prim(Prim::Nat(if left == right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_eql(left, right)),
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
            Prim::FltType => Ok(Term::Prim(Prim::FltType)),
            Prim::Flt(bits) => Ok(Term::Prim(Prim::Flt(*bits))),
            Prim::FltAdd(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Flt((f32::from_bits(left) + f32::from_bits(right)).to_bits()),
                    ),
                    (left, right) => Term::Prim(Prim::flt_add(left, right)),
                })
            }
            Prim::FltSub(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Flt((f32::from_bits(left) - f32::from_bits(right)).to_bits()),
                    ),
                    (left, right) => Term::Prim(Prim::flt_sub(left, right)),
                })
            }
            Prim::FltMul(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Flt((f32::from_bits(left) * f32::from_bits(right)).to_bits()),
                    ),
                    (left, right) => Term::Prim(Prim::flt_mul(left, right)),
                })
            }
            Prim::IntNeq(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Nat(if left != right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_neq(left, right)),
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
                        Term::Prim(Prim::Nat(if left < right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_lt(left, right)),
                })
            }
            Prim::IntGt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Nat(if left > right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_gt(left, right)),
                })
            }
            Prim::IntLte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Nat(if left <= right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_lte(left, right)),
                })
            }
            Prim::IntGte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Int(left)), Term::Prim(Prim::Int(right))) => {
                        Term::Prim(Prim::Nat(if left >= right { 1 } else { 0 }))
                    }
                    (left, right) => Term::Prim(Prim::int_gte(left, right)),
                })
            }
            Prim::FltNeg(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt((-f32::from_bits(bits)).to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_neg(inner)),
                })
            }
            Prim::FltAbs(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt(f32::from_bits(bits).abs().to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_abs(inner)),
                })
            }
            Prim::FltSqrt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt(f32::from_bits(bits).sqrt().to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_sqrt(inner)),
                })
            }
            Prim::FltFloor(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt(f32::from_bits(bits).floor().to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_floor(inner)),
                })
            }
            Prim::FltCeil(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt(f32::from_bits(bits).ceil().to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_ceil(inner)),
                })
            }
            Prim::FltTrunc(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt(f32::from_bits(bits).trunc().to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_trunc(inner)),
                })
            }
            Prim::FltNearest(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Flt(f32::from_bits(bits).round_ties_even().to_bits()))
                    }
                    inner => Term::Prim(Prim::flt_nearest(inner)),
                })
            }
            Prim::FltDiv(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Flt((f32::from_bits(left) / f32::from_bits(right)).to_bits()),
                    ),
                    (left, right) => Term::Prim(Prim::flt_div(left, right)),
                })
            }
            Prim::FltMin(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Flt(f32::from_bits(left).min(f32::from_bits(right)).to_bits()),
                    ),
                    (left, right) => Term::Prim(Prim::flt_min(left, right)),
                })
            }
            Prim::FltMax(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Flt(f32::from_bits(left).max(f32::from_bits(right)).to_bits()),
                    ),
                    (left, right) => Term::Prim(Prim::flt_max(left, right)),
                })
            }
            Prim::FltEql(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Nat(if f32::from_bits(left) == f32::from_bits(right) {
                            1
                        } else {
                            0
                        }),
                    ),
                    (left, right) => Term::Prim(Prim::flt_eql(left, right)),
                })
            }
            Prim::FltNeq(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Nat(if f32::from_bits(left) != f32::from_bits(right) {
                            1
                        } else {
                            0
                        }),
                    ),
                    (left, right) => Term::Prim(Prim::flt_neq(left, right)),
                })
            }
            Prim::FltLt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Nat(if f32::from_bits(left) < f32::from_bits(right) {
                            1
                        } else {
                            0
                        }))
                    }
                    (left, right) => Term::Prim(Prim::flt_lt(left, right)),
                })
            }
            Prim::FltGt(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => {
                        Term::Prim(Prim::Nat(if f32::from_bits(left) > f32::from_bits(right) {
                            1
                        } else {
                            0
                        }))
                    }
                    (left, right) => Term::Prim(Prim::flt_gt(left, right)),
                })
            }
            Prim::FltLte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Nat(if f32::from_bits(left) <= f32::from_bits(right) {
                            1
                        } else {
                            0
                        }),
                    ),
                    (left, right) => Term::Prim(Prim::flt_lte(left, right)),
                })
            }
            Prim::FltGte(left, right) => {
                let left = self.reduce(context, left.as_ref().clone())?;
                let right = self.reduce(context, right.as_ref().clone())?;

                Ok(match (left, right) {
                    (Term::Prim(Prim::Flt(left)), Term::Prim(Prim::Flt(right))) => Term::Prim(
                        Prim::Nat(if f32::from_bits(left) >= f32::from_bits(right) {
                            1
                        } else {
                            0
                        }),
                    ),
                    (left, right) => Term::Prim(Prim::flt_gte(left, right)),
                })
            }
            Prim::NatToInt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Nat(v)) => Term::Prim(Prim::Int(v as i32)),
                    inner => Term::Prim(Prim::nat_to_int(inner)),
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
                    Term::Prim(Prim::Int(v)) => Term::Prim(Prim::Flt((v as f32).to_bits())),
                    inner => Term::Prim(Prim::int_to_flt(inner)),
                })
            }
            Prim::NatToFlt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Nat(v)) => Term::Prim(Prim::Flt((v as f32).to_bits())),
                    inner => Term::Prim(Prim::nat_to_flt(inner)),
                })
            }
            Prim::FltToInt(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Int(f32::from_bits(bits) as i32))
                    }
                    inner => Term::Prim(Prim::flt_to_int(inner)),
                })
            }
            Prim::FltToNat(inner) => {
                let inner = self.reduce(context, inner.as_ref().clone())?;

                Ok(match inner {
                    Term::Prim(Prim::Flt(bits)) => {
                        Term::Prim(Prim::Nat(f32::from_bits(bits) as u32))
                    }
                    inner => Term::Prim(Prim::flt_to_nat(inner)),
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

    fn reduce_split(&mut self, context: &mut Context, split: Split) -> Result<Step, Preempted> {
        let Split { head, motive, tail } = split;
        match self.reduce(context, *head)? {
            Term::Pair(Pair { fst, snd }) => {
                Ok(Step::Continue(tail.open(&[fst.as_ref(), snd.as_ref()])))
            }
            head => Ok(Step::Break(
                Split {
                    head: head.into(),
                    motive,
                    tail,
                }
                .into(),
            )),
        }
    }

    fn reduce_match(&mut self, context: &mut Context, match_: Match) -> Result<Step, Preempted> {
        let Match {
            head,
            motive,
            cases,
        } = match_;
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
                Term::Apply(apply) => self.reduce_apply(context, apply)?,
                Term::Split(split) => self.reduce_split(context, split)?,
                Term::Match(match_) => self.reduce_match(context, match_)?,
                Term::Let(let_) => self.reduce_let(let_),
                Term::Prim(prim) => Step::Break(self.reduce_prim(context, &prim)?),
                Term::Var(var) => self.reduce_var(context, var),
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
        crate::core::{Atom, Let, Type, Var},
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
    fn reduce_split_opens_pair_tail() {
        let mut context = context();

        let term = Split::new(
            Pair::new(Atom::from("left"), Atom::from("right")),
            "p",
            Type,
            "x",
            "y",
            Pair::new(Var::free("x"), Var::free("y")),
        )
        .into();

        assert_eq!(
            reduce(&mut context, &term),
            Ok(Pair::new(Atom::from("left"), Atom::from("right")).into())
        );
    }

    #[test]
    fn reduce_match_selects_case() {
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
    fn reduce_int_eql_returns_one_for_true_and_zero_for_false() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::Int(4)),
                    Term::Prim(Prim::Int(4))
                ))
            ),
            Ok(Term::Prim(Prim::Nat(1)))
        );
        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::int_eql(
                    Term::Prim(Prim::Int(4)),
                    Term::Prim(Prim::Int(5))
                ))
            ),
            Ok(Term::Prim(Prim::Nat(0)))
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                &Term::Prim(Prim::flt_mul(
                    Term::Prim(Prim::Flt(1.5_f32.to_bits())),
                    Term::Prim(Prim::Flt(2.0_f32.to_bits()))
                ))
            ),
            Ok(Term::Prim(Prim::Flt(3.0_f32.to_bits())))
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
            reduce(&mut context, &Term::Prim(Prim::arr_get(list.clone(), Term::Prim(Prim::Nat(0))))),
            Ok(Term::Prim(Prim::Nat(10)))
        );
        assert_eq!(
            reduce(&mut context, &Term::Prim(Prim::arr_get(list, Term::Prim(Prim::Nat(2))))),
            Ok(Term::Prim(Prim::Nat(30)))
        );
    }

    #[test]
    #[should_panic(expected = "Arr.get: index out of bounds")]
    fn reduce_lst_get_panics_on_out_of_bounds() {
        let mut context = context();

        let list = Term::Prim(Prim::from(vec![Term::Prim(Prim::Nat(1))]));

        reduce(&mut context, &Term::Prim(Prim::arr_get(list, Term::Prim(Prim::Nat(1))))).ok();
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
}
