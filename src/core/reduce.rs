use {
    super::{
        Apply, BlnMatch, Context, Flt, Func, Let, Match, Nat, NatMatch, One, Preempted, Prim, Proj,
        Scope, Seal, Subterm, Term, Tuple, Two, Unseal, Var,
    },
    std::{
        collections::BTreeMap,
        time::{Duration, Instant},
    },
};

pub fn reduce(context: &mut Context, term: Term) -> Result<Term, Preempted> {
    Reduce::new(context.timeout()).reduce(context, term)
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

    fn reduce_prim(&mut self, context: &mut Context, prim: &Prim) -> Result<Subterm, Preempted> {
        match prim {
            Prim::BlnType => Ok(Subterm::Prim(Prim::BlnType)),
            Prim::Bln(value) => Ok(Subterm::Prim(Prim::Bln(*value))),
            Prim::NatType => Ok(Subterm::Prim(Prim::NatType)),
            Prim::Nat(Nat::Zero) => Ok(Subterm::Prim(Prim::Nat(Nat::Zero))),
            Prim::Nat(Nat::Succ(spine, inner)) => {
                let inner = self.reduce(context, inner.clone())?;
                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Nat(Nat::Succ(j, tail))) => {
                        Prim::Nat(Nat::Succ(spine + j, tail)).into()
                    }
                    inner => Prim::Nat(Nat::Succ(*spine, Term::new(inner))).into(),
                })
            }
            Prim::NatEql(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Bln(l == r)),
                    _ => Subterm::Prim(Prim::nat_eql(left, right)),
                })
            }
            Prim::NatNeq(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Bln(l != r)),
                    _ => Subterm::Prim(Prim::nat_neq(left, right)),
                })
            }
            Prim::NatAdd(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Nat(Nat::new(l.wrapping_add(r)))),
                    _ => Subterm::Prim(Prim::nat_add(left, right)),
                })
            }
            Prim::NatSub(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Nat(Nat::new(l.wrapping_sub(r)))),
                    _ => Subterm::Prim(Prim::nat_sub(left, right)),
                })
            }
            Prim::NatMul(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Nat(Nat::new(l.wrapping_mul(r)))),
                    _ => Subterm::Prim(Prim::nat_mul(left, right)),
                })
            }
            Prim::NatLt(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Bln(l < r)),
                    _ => Subterm::Prim(Prim::nat_lt(left, right)),
                })
            }
            Prim::NatDiv(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Nat(Nat::new(l.wrapping_div(r)))),
                    _ => Subterm::Prim(Prim::nat_div(left, right)),
                })
            }
            Prim::NatRem(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Nat(Nat::new(l.wrapping_rem(r)))),
                    _ => Subterm::Prim(Prim::nat_rem(left, right)),
                })
            }
            Prim::NatGt(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Bln(l > r)),
                    _ => Subterm::Prim(Prim::nat_gt(left, right)),
                })
            }
            Prim::NatLte(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Bln(l <= r)),
                    _ => Subterm::Prim(Prim::nat_lte(left, right)),
                })
            }
            Prim::NatGte(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(match (left.as_nat(), right.as_nat()) {
                    (Some(l), Some(r)) => Subterm::Prim(Prim::Bln(l >= r)),
                    _ => Subterm::Prim(Prim::nat_gte(left, right)),
                })
            }
            Prim::IntType => Ok(Subterm::Prim(Prim::IntType)),
            Prim::Int(value) => Ok(Subterm::Prim(Prim::Int(*value))),
            Prim::IntEql(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Bln(left == right))
                        }
                        (left, right) => Subterm::Prim(Prim::int_eql(left, right)),
                    },
                )
            }
            Prim::IntNeq(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Bln(left != right))
                        }
                        (left, right) => Subterm::Prim(Prim::int_neq(left, right)),
                    },
                )
            }
            Prim::IntAdd(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Int(left.wrapping_add(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::int_add(left, right)),
                    },
                )
            }
            Prim::IntSub(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Int(left.wrapping_sub(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::int_sub(left, right)),
                    },
                )
            }
            Prim::IntMul(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Int(left.wrapping_mul(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::int_mul(left, right)),
                    },
                )
            }
            Prim::IntDiv(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Int(left.wrapping_div(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::int_div(left, right)),
                    },
                )
            }
            Prim::IntRem(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Int(left.wrapping_rem(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::int_rem(left, right)),
                    },
                )
            }
            Prim::IntLt(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Bln(left < right))
                        }
                        (left, right) => Subterm::Prim(Prim::int_lt(left, right)),
                    },
                )
            }
            Prim::IntGt(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Bln(left > right))
                        }
                        (left, right) => Subterm::Prim(Prim::int_gt(left, right)),
                    },
                )
            }
            Prim::IntLte(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Bln(left <= right))
                        }
                        (left, right) => Subterm::Prim(Prim::int_lte(left, right)),
                    },
                )
            }
            Prim::IntGte(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Int(left)), Subterm::Prim(Prim::Int(right))) => {
                            Subterm::Prim(Prim::Bln(left >= right))
                        }
                        (left, right) => Subterm::Prim(Prim::int_gte(left, right)),
                    },
                )
            }
            Prim::FltType => Ok(Subterm::Prim(Prim::FltType)),
            Prim::Flt(flt) => Ok(Subterm::Prim(Prim::Flt(*flt))),
            Prim::FltAdd(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Flt(left + right))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_add(left, right)),
                    },
                )
            }
            Prim::FltSub(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Flt(left - right))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_sub(left, right)),
                    },
                )
            }
            Prim::FltMul(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Flt(left * right))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_mul(left, right)),
                    },
                )
            }
            Prim::FltDiv(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Flt(left / right))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_div(left, right)),
                    },
                )
            }
            Prim::FltMin(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Flt(left.min(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_min(left, right)),
                    },
                )
            }
            Prim::FltMax(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Flt(left.max(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_max(left, right)),
                    },
                )
            }
            Prim::FltEql(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Bln(left.eql(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_eql(left, right)),
                    },
                )
            }
            Prim::FltNeq(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Bln(left.neq(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_neq(left, right)),
                    },
                )
            }
            Prim::FltLt(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Bln(left.lt(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_lt(left, right)),
                    },
                )
            }
            Prim::FltGt(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Bln(left.gt(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_gt(left, right)),
                    },
                )
            }
            Prim::FltLte(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Bln(left.lte(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_lte(left, right)),
                    },
                )
            }
            Prim::FltGte(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Flt(left)), Subterm::Prim(Prim::Flt(right))) => {
                            Subterm::Prim(Prim::Bln(left.gte(right)))
                        }
                        (left, right) => Subterm::Prim(Prim::flt_gte(left, right)),
                    },
                )
            }
            Prim::FltNeg(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(-flt)),
                    inner => Subterm::Prim(Prim::flt_neg(inner)),
                })
            }
            Prim::FltAbs(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(flt.abs())),
                    inner => Subterm::Prim(Prim::flt_abs(inner)),
                })
            }
            Prim::FltSqrt(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(flt.sqrt())),
                    inner => Subterm::Prim(Prim::flt_sqrt(inner)),
                })
            }
            Prim::FltFloor(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(flt.floor())),
                    inner => Subterm::Prim(Prim::flt_floor(inner)),
                })
            }
            Prim::FltCeil(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(flt.ceil())),
                    inner => Subterm::Prim(Prim::flt_ceil(inner)),
                })
            }
            Prim::FltTrunc(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(flt.trunc())),
                    inner => Subterm::Prim(Prim::flt_trunc(inner)),
                })
            }
            Prim::FltNearest(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Flt(flt.nearest())),
                    inner => Subterm::Prim(Prim::flt_nearest(inner)),
                })
            }
            Prim::NatToStr(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match inner.as_nat() {
                    Some(v) => Subterm::Prim(Prim::Bin(format!("{v}").into_bytes())),
                    None => Subterm::Prim(Prim::nat_to_str(inner)),
                })
            }
            Prim::IntToStr(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Int(v)) => {
                        Subterm::Prim(Prim::Bin(format!("{v}").into_bytes()))
                    }
                    inner => Subterm::Prim(Prim::int_to_str(inner)),
                })
            }
            Prim::FltToStr(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(v)) => {
                        Subterm::Prim(Prim::Bin(format!("{}", v.to_f32()).into_bytes()))
                    }
                    inner => Subterm::Prim(Prim::flt_to_str(inner)),
                })
            }
            Prim::NatToInt(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match inner.as_nat() {
                    Some(v) => Subterm::Prim(Prim::Int(v as i32)),
                    None => Subterm::Prim(Prim::nat_to_int(inner)),
                })
            }
            Prim::NatToFlt(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match inner.as_nat() {
                    Some(v) => Subterm::Prim(Prim::Flt(Flt::from_f32(v as f32))),
                    None => Subterm::Prim(Prim::nat_to_flt(inner)),
                })
            }
            Prim::IntToNat(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Int(v)) => Subterm::Prim(Prim::Nat(Nat::new(v as u32))),
                    inner => Subterm::Prim(Prim::int_to_nat(inner)),
                })
            }
            Prim::IntToFlt(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Int(v)) => {
                        Subterm::Prim(Prim::Flt(Flt::from_f32(v as f32)))
                    }
                    inner => Subterm::Prim(Prim::int_to_flt(inner)),
                })
            }
            Prim::FltToNat(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => {
                        Subterm::Prim(Prim::Nat(Nat::new(flt.to_f32() as u32)))
                    }
                    inner => Subterm::Prim(Prim::flt_to_nat(inner)),
                })
            }
            Prim::FltToInt(inner) => {
                let inner = self.reduce(context, inner.clone())?;

                Ok(match Term::unwrap_or_clone(inner) {
                    Subterm::Prim(Prim::Flt(flt)) => Subterm::Prim(Prim::Int(flt.to_f32() as i32)),
                    inner => Subterm::Prim(Prim::flt_to_int(inner)),
                })
            }
            Prim::BinType => Ok(Subterm::Prim(Prim::BinType)),
            Prim::Bin(bytes) => Ok(Subterm::Prim(Prim::Bin(bytes.clone()))),
            Prim::BinLen(bin) => {
                let bin = self.reduce(context, bin.clone())?;
                Ok(match Term::unwrap_or_clone(bin) {
                    Subterm::Prim(Prim::Bin(bytes)) => {
                        Subterm::Prim(Prim::Nat(Nat::new(bytes.len() as u32)))
                    }
                    bin => Subterm::Prim(Prim::bin_len(bin)),
                })
            }
            Prim::BinEql(left, right) => {
                let left = self.reduce(context, left.clone())?;
                let right = self.reduce(context, right.clone())?;

                Ok(
                    match (Term::unwrap_or_clone(left), Term::unwrap_or_clone(right)) {
                        (Subterm::Prim(Prim::Bin(left)), Subterm::Prim(Prim::Bin(right))) => {
                            Subterm::Prim(Prim::Bln(left == right))
                        }
                        (left, right) => Subterm::Prim(Prim::bin_eql(left, right)),
                    },
                )
            }
            Prim::BinGet(bin, index) => {
                let bin = self.reduce(context, bin.clone())?;
                let index = self.reduce(context, index.clone())?;
                let i = index.as_nat();
                Ok(match (Term::unwrap_or_clone(bin), i) {
                    (Subterm::Prim(Prim::Bin(bytes)), Some(i)) => {
                        Subterm::Prim(Prim::Nat(Nat::new(
                            bytes
                                .get(i as usize)
                                .copied()
                                .expect("Bin.get: index out of bounds")
                                as u32,
                        )))
                    }
                    (bin, _) => Subterm::Prim(Prim::bin_get(bin, index)),
                })
            }
            Prim::BinSlice(bin, start, end) => {
                let bin = self.reduce(context, bin.clone())?;
                let start = self.reduce(context, start.clone())?;
                let end = self.reduce(context, end.clone())?;
                let s = start.as_nat();
                let e = end.as_nat();
                Ok(match (Term::unwrap_or_clone(bin), s, e) {
                    (Subterm::Prim(Prim::Bin(bytes)), Some(s), Some(e)) => {
                        Subterm::Prim(Prim::Bin(
                            bytes
                                .get(s as usize..e as usize)
                                .expect("Bin.slice: range out of bounds")
                                .to_vec(),
                        ))
                    }
                    (bin, _, _) => Subterm::Prim(Prim::bin_slice(bin, start, end)),
                })
            }
            Prim::BinAppend(bin, byte) => {
                let bin = self.reduce(context, bin.clone())?;
                let byte = self.reduce(context, byte.clone())?;
                let n = byte.as_nat();
                Ok(match (Term::unwrap_or_clone(bin), n) {
                    (Subterm::Prim(Prim::Bin(mut bytes)), Some(n)) => {
                        bytes.push(n as u8);
                        Subterm::Prim(Prim::Bin(bytes))
                    }
                    (bin, _) => Subterm::Prim(Prim::bin_append(bin, byte)),
                })
            }
            Prim::BinConcat(operands) => {
                let reduced: Vec<Term> = operands
                    .iter()
                    .map(|e| self.reduce(context, e.clone()))
                    .collect::<Result<_, _>>()?;
                let merged = reduced.iter().try_fold(Vec::new(), |mut acc, t| {
                    if let Subterm::Prim(Prim::Bin(b)) = &**t {
                        acc.extend(b);
                        Some(acc)
                    } else {
                        None
                    }
                });
                Ok(match merged {
                    Some(bytes) => Subterm::Prim(Prim::Bin(bytes)),
                    None => Subterm::Prim(Prim::BinConcat(
                        reduced.into_iter().map(|t| t.into()).collect(),
                    )),
                })
            }
            Prim::ArrType(elem) => {
                let elem = self.reduce(context, elem.clone())?;
                Ok(Subterm::Prim(Prim::arr_type(elem)))
            }
            Prim::Arr(elems) => {
                let elems = elems
                    .iter()
                    .map(|e| self.reduce(context, e.clone()).map(|t| t.into()))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Subterm::Prim(Prim::Arr(elems)))
            }
            Prim::ArrLen(list) => {
                let list = self.reduce(context, list.clone())?;
                Ok(match Term::unwrap_or_clone(list) {
                    Subterm::Prim(Prim::Arr(elems)) => {
                        Subterm::Prim(Prim::Nat(Nat::new(elems.len() as u32)))
                    }
                    list => Subterm::Prim(Prim::arr_len(list)),
                })
            }
            Prim::ArrGet(list, index) => {
                let list = self.reduce(context, list.clone())?;
                let index = self.reduce(context, index.clone())?;
                let i = index.as_nat();
                Ok(match (Term::unwrap_or_clone(list), i) {
                    (Subterm::Prim(Prim::Arr(elems)), Some(i)) => elems
                        .into_iter()
                        .nth(i as usize)
                        .map(Term::unwrap_or_clone)
                        .expect("Arr.get: index out of bounds"),
                    (list, _) => Subterm::Prim(Prim::arr_get(list, index)),
                })
            }
            Prim::ArrSlice(list, start, end) => {
                let list = self.reduce(context, list.clone())?;
                let start = self.reduce(context, start.clone())?;
                let end = self.reduce(context, end.clone())?;
                let s = start.as_nat();
                let e = end.as_nat();
                Ok(match (Term::unwrap_or_clone(list), s, e) {
                    (Subterm::Prim(Prim::Arr(elems)), Some(s), Some(e)) => {
                        Subterm::Prim(Prim::Arr(
                            elems
                                .get(s as usize..e as usize)
                                .expect("Arr.slice: range out of bounds")
                                .to_vec(),
                        ))
                    }
                    (list, _, _) => Subterm::Prim(Prim::arr_slice(list, start, end)),
                })
            }
            Prim::ArrAppend(list, elem) => {
                let list = self.reduce(context, list.clone())?;
                let elem = self.reduce(context, elem.clone())?;
                Ok(match Term::unwrap_or_clone(list) {
                    Subterm::Prim(Prim::Arr(mut elems)) => {
                        elems.push(elem.into());
                        Subterm::Prim(Prim::Arr(elems))
                    }
                    list => Subterm::Prim(Prim::arr_append(list, elem)),
                })
            }
            Prim::ArrConcat(operands) => {
                let reduced: Vec<Term> = operands
                    .iter()
                    .map(|e| self.reduce(context, e.clone()))
                    .collect::<Result<_, _>>()?;
                let merged = reduced.iter().try_fold(Vec::new(), |mut acc, t| {
                    if let Subterm::Prim(Prim::Arr(elems)) = &**t {
                        acc.extend(elems.iter().cloned());
                        Some(acc)
                    } else {
                        None
                    }
                });
                Ok(match merged {
                    Some(elems) => Subterm::Prim(Prim::Arr(elems)),
                    None => Subterm::Prim(Prim::ArrConcat(
                        reduced.into_iter().map(|t| t.into()).collect(),
                    )),
                })
            }
            Prim::SysPrint(_) => panic!("SysPrint cannot appear at the type level"),
            Prim::SysRead => panic!("SysRead cannot appear at the type level"),
        }
    }

    fn reduce_apply(&mut self, context: &mut Context, apply: Apply) -> Result<Step, Preempted> {
        let Apply { head, params } = apply;
        let param_refs = params.iter().collect::<Vec<_>>();
        match Term::unwrap_or_clone(self.reduce(context, head)?) {
            Subterm::Func(Func { body }) => Ok(Step::Continue(body.open(&param_refs))),
            head => Ok(Step::Break(
                Apply {
                    head: head.into(),
                    params,
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
        match Term::unwrap_or_clone(self.reduce(context, head)?) {
            Subterm::Tuple(Tuple { fields }) => Ok(Step::Continue(
                fields
                    .into_iter()
                    .nth(index)
                    .expect("Proj: index out of bounds"),
            )),
            head => {
                let head: Term = head.into();
                match context.projection(&head, index) {
                    Some(v) => Ok(Step::Continue(v.clone())),
                    None => Ok(Step::Break(Proj { head, index }.into())),
                }
            }
        }
    }

    fn reduce_func_eta(&mut self, context: &mut Context, func: Func) -> Result<Step, Preempted> {
        let n = func.body.arity();
        let freshs = (0..n).map(|_| context.fresh(None)).collect::<Vec<_>>();
        let ys = freshs
            .iter()
            .map(|f| Term::from(Var::free(f)))
            .collect::<Vec<_>>();
        let y_refs = ys.iter().collect::<Vec<_>>();
        match Term::unwrap_or_clone(func.body.open(&y_refs)) {
            Subterm::Apply(Apply { head, params })
                if params.len() == n
                    && params.iter().enumerate().all(|(i, p)| {
                        matches!(p.as_ref(), Subterm::Var(v) if v.unwrap() == freshs[i].as_str())
                    })
                    && freshs.iter().all(|f| !head.free_vars().contains(f)) =>
            {
                Ok(Step::Continue(head))
            }
            _ => Ok(Step::Break(func.into())),
        }
    }

    fn eta_reduce_tuple(tuple: Tuple) -> Subterm {
        let n = tuple.fields.len();
        if n == 0 {
            return tuple.into();
        }
        let mut base: Option<Subterm> = None;
        for (i, f) in tuple.fields.iter().enumerate() {
            match f.as_ref() {
                Subterm::Proj(Proj { head, index }) if *index == i => {
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

    fn reduce_nat_induction(
        &mut self,
        context: &mut Context,
        head: Subterm,
        motive: Scope<One>,
        zero_case: Term,
        succ_case: Scope<Two>,
    ) -> Result<Step, Preempted> {
        match Term::unwrap_or_clone(self.reduce(context, head.into())?) {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => Ok(Step::Continue(zero_case)),
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) => {
                let pred = if spine == 1 {
                    inner
                } else {
                    Prim::Nat(Nat::Succ(spine - 1, inner)).into()
                };
                let ih: Term = Subterm::NatMatch(NatMatch::Induction {
                    head: pred.clone(),
                    motive: motive.clone(),
                    zero_case: zero_case.clone(),
                    succ_case: succ_case.clone(),
                })
                .into();
                Ok(Step::Continue(succ_case.open(&[&pred, &ih])))
            }
            head => Ok(Step::Break(
                NatMatch::Induction {
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
        match Term::unwrap_or_clone(self.reduce(context, head)?) {
            Subterm::Prim(Prim::Bln(false)) => Ok(Step::Continue(false_case)),
            Subterm::Prim(Prim::Bln(true)) => Ok(Step::Continue(true_case)),
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

    fn reduce_nat_dispatch(
        &mut self,
        context: &mut Context,
        head: Subterm,
        motive: Scope<One>,
        cases: BTreeMap<u32, Term>,
        default: Term,
    ) -> Result<Step, Preempted> {
        match Term::unwrap_or_clone(self.reduce(context, head.into())?) {
            Subterm::Prim(Prim::Nat(Nat::Zero)) => match cases.get(&0) {
                Some(body) => Ok(Step::Continue(body.clone())),
                None => Ok(Step::Continue(default.clone())),
            },
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner)))
                if matches!(inner.as_ref(), Subterm::Prim(Prim::Nat(Nat::Zero))) =>
            {
                match cases.get(&spine) {
                    Some(body) => Ok(Step::Continue(body.clone())),
                    None => Ok(Step::Continue(default.clone())),
                }
            }
            head => Ok(Step::Break(
                NatMatch::Dispatch {
                    head: head.into(),
                    motive,
                    cases,
                    default,
                }
                .into(),
            )),
        }
    }

    fn reduce_nat_match(&mut self, context: &mut Context, nm: NatMatch) -> Result<Step, Preempted> {
        match nm {
            NatMatch::Induction {
                head,
                motive,
                zero_case,
                succ_case,
            } => self.reduce_nat_induction(
                context,
                Term::unwrap_or_clone(head),
                motive,
                zero_case,
                succ_case,
            ),
            NatMatch::Dispatch {
                head,
                motive,
                cases,
                default,
            } => self.reduce_nat_dispatch(
                context,
                Term::unwrap_or_clone(head),
                motive,
                cases,
                default,
            ),
        }
    }

    fn reduce_match(&mut self, context: &mut Context, m: Match) -> Result<Step, Preempted> {
        let Match {
            head,
            motive,
            cases,
        } = m;
        let atom = match Term::unwrap_or_clone(self.reduce(context, head)?) {
            Subterm::Atom(atom) => atom,
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
            Some(body) => Ok(Step::Continue(body.clone())),
            None => Ok(Step::Break(
                Match {
                    head: Subterm::from(atom).into(),
                    motive,
                    cases,
                }
                .into(),
            )),
        }
    }

    fn reduce_unseal(&mut self, context: &mut Context, unseal: Unseal) -> Result<Step, Preempted> {
        let Unseal { witness, value } = unseal;
        match Term::unwrap_or_clone(self.reduce(context, value)?) {
            Subterm::Seal(Seal {
                value: sealed_value,
                ..
            }) => Ok(Step::Continue(sealed_value)),
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
        Step::Continue(let_.tail.open(&[&let_.body]))
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

            let step = match Term::unwrap_or_clone(term) {
                Subterm::Prim(prim) => Step::Break(self.reduce_prim(context, &prim)?.into()),
                Subterm::BlnMatch(bm) => self.reduce_bln_match(context, bm)?,
                Subterm::NatMatch(nm) => self.reduce_nat_match(context, nm)?,
                Subterm::Apply(apply) => self.reduce_apply(context, apply)?,
                Subterm::Proj(proj) => self.reduce_proj(context, proj)?,
                Subterm::Func(func) => self.reduce_func_eta(context, func)?,
                Subterm::Match(m) => self.reduce_match(context, m)?,
                Subterm::Let(let_) => self.reduce_let(let_),
                Subterm::Unseal(unseal) => self.reduce_unseal(context, unseal)?,
                Subterm::Var(var) => self.reduce_var(context, var),
                Subterm::Tuple(t) => Step::Break(Self::eta_reduce_tuple(t).into()),
                Subterm::Spanned(_, inner) => Step::Continue(inner),
                term => Step::Break(term.into()),
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
            Atom, AtomType, Let, Match, Nat, NatMatch, Prim, Seal, Sealed, Tuple, Type, Unseal, Var,
        },
        std::time::Duration,
    };

    fn context() -> Context {
        Context::new(Duration::from_millis(10))
    }

    #[test]
    fn reduce_apply_beta_reduces() {
        let mut context = context();

        let term: Term = Apply::new(Func::new(["x"], Var::free("x")), [Atom::from("ok")]).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("ok").into())
        );
    }

    #[test]
    fn reduce_match_selects_match() {
        let mut context = context();

        let term: Term = Match::new(
            Atom::from("a"),
            Some("m"),
            Type,
            vec![("a", Atom::from("yes")), ("b", Atom::from("no"))],
        )
        .into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("yes").into())
        );
    }

    #[test]
    fn reduce_nat_fold_zero_is_not_true() {
        let mut context = context();

        let term: Term = NatMatch::induction(
            Subterm::Prim(Prim::Nat(Nat::new(0))),
            Some("m"),
            AtomType::new(["false", "true"]),
            Atom::from("false"),
            "pred",
            "ih",
            Atom::from("true"),
        )
        .into();

        assert_ne!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("true").into())
        );
    }

    #[test]
    fn reduce_let_then_var_unfolds_definition() {
        let mut context = context();

        context.define("y", &Atom::from("done").into());

        let term: Term = Let::new("x", Type, Var::free("y"), Var::free("x")).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("done").into())
        );
    }

    #[test]
    fn reduce_var_cycle_times_out() {
        let mut context = context();

        context.define("loop", &Var::free("loop").into());

        assert_eq!(
            reduce(&mut context, Var::free("loop").into()),
            Err(Preempted)
        );
    }

    #[test]
    fn reduce_int_add_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::int_add(
                    Subterm::Prim(Prim::Int(1)),
                    Subterm::Prim(Prim::Int(2))
                ))
                .into()
            ),
            Ok(Subterm::Prim(Prim::Int(3)).into())
        );
    }

    #[test]
    fn reduce_int_eql_returns_true_or_false_atom() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::int_eql(
                    Subterm::Prim(Prim::Int(4)),
                    Subterm::Prim(Prim::Int(4))
                ))
                .into()
            ),
            Ok(Subterm::Prim(Prim::Bln(true)).into())
        );
        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::int_eql(
                    Subterm::Prim(Prim::Int(4)),
                    Subterm::Prim(Prim::Int(5))
                ))
                .into()
            ),
            Ok(Subterm::Prim(Prim::Bln(false)).into())
        );
    }

    #[test]
    fn reduce_flt_mul_computes() {
        let mut context = context();

        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::flt_mul(
                    Subterm::Prim(Prim::Flt(Flt::from_f32(1.5))),
                    Subterm::Prim(Prim::Flt(Flt::from_f32(2.0)))
                ))
                .into()
            ),
            Ok(Subterm::Prim(Prim::Flt(Flt::from_f32(3.0))).into())
        );
    }

    #[test]
    fn reduce_lst_get_returns_element_at_index() {
        let mut context = context();

        let list = Subterm::Prim(Prim::from(vec![
            Subterm::Prim(Prim::Nat(Nat::new(10))),
            Subterm::Prim(Prim::Nat(Nat::new(20))),
            Subterm::Prim(Prim::Nat(Nat::new(30))),
        ]));

        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::arr_get(
                    list.clone(),
                    Subterm::Prim(Prim::Nat(Nat::new(0)))
                ))
                .into()
            ),
            Ok(Subterm::Prim(Prim::Nat(Nat::new(10))).into())
        );
        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::arr_get(list, Subterm::Prim(Prim::Nat(Nat::new(2))))).into()
            ),
            Ok(Subterm::Prim(Prim::Nat(Nat::new(30))).into())
        );
    }

    #[test]
    fn reduce_unseal_fires_on_sealed_value() {
        let mut context = context();

        let term: Term =
            Unseal::new(Var::free("x"), Seal::new(Var::free("x"), Atom::from("ok"))).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("ok").into())
        );
    }

    #[test]
    fn reduce_unseal_stuck_on_free_var() {
        let mut context = context();

        let term: Term = Unseal::new(Var::free("x"), Var::free("v")).into();

        assert_eq!(reduce(&mut context, term.clone()), Ok(term));
    }

    #[test]
    fn reduce_sealed_is_stuck() {
        let mut context = context();

        let term: Term = Sealed::new("x", Type, Var::free("x")).into();

        assert_eq!(reduce(&mut context, term.clone()), Ok(term));
    }

    #[test]
    #[should_panic(expected = "Arr.get: index out of bounds")]
    fn reduce_lst_get_panics_on_out_of_bounds() {
        let mut context = context();

        let list = Subterm::Prim(Prim::from(vec![Subterm::Prim(Prim::Nat(Nat::new(1)))]));

        reduce(
            &mut context,
            Subterm::Prim(Prim::arr_get(list, Subterm::Prim(Prim::Nat(Nat::new(1))))).into(),
        )
        .ok();
    }

    #[test]
    fn reduce_bin_append_adds_byte() {
        let mut context = context();

        let bin = Subterm::Prim(Prim::Bin(vec![1, 2]));
        let byte: Subterm = Subterm::Prim(Prim::Nat(Nat::new(3)));

        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::bin_append(bin, byte)).into()
            ),
            Ok(Subterm::Prim(Prim::Bin(vec![1, 2, 3])).into())
        );
    }

    #[test]
    fn reduce_lst_append_adds_element() {
        let mut context = context();

        let list = Subterm::Prim(Prim::from(vec![
            Subterm::Prim(Prim::Nat(Nat::new(10))),
            Subterm::Prim(Prim::Nat(Nat::new(20))),
        ]));

        assert_eq!(
            reduce(
                &mut context,
                Subterm::Prim(Prim::arr_append(
                    list,
                    Subterm::Prim(Prim::Nat(Nat::new(30)))
                ))
                .into()
            ),
            Ok(Subterm::Prim(Prim::from(vec![
                Subterm::Prim(Prim::Nat(Nat::new(10))),
                Subterm::Prim(Prim::Nat(Nat::new(20))),
                Subterm::Prim(Prim::Nat(Nat::new(30))),
            ]))
            .into())
        );
    }

    #[test]
    fn reduce_proj_beta_reduces() {
        let mut context = context();

        let term: Term = Proj::new(Tuple::new([Atom::from("a"), Atom::from("b")]), 1).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("b").into())
        );
    }

    #[test]
    fn reduce_proj_table_lookup() {
        let mut context = context();

        context.define_proj(Var::free("r").into(), 0, Atom::from("ok").into());

        let term: Term = Proj::new(Var::free("r"), 0).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Atom::from("ok").into())
        );
    }

    #[test]
    fn eta_reduce_tuple_fires() {
        let mut context = context();

        let term: Term =
            Tuple::new([Proj::new(Var::free("r"), 0), Proj::new(Var::free("r"), 1)]).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Var::free("r").into())
        );
    }

    #[test]
    fn eta_reduce_func_fires() {
        let mut context = context();

        let term: Term = Func::new(["y"], Apply::new(Var::free("f"), [Var::free("y")])).into();

        assert_eq!(
            reduce(&mut context, term.clone()),
            Ok(Var::free("f").into())
        );
    }
}
