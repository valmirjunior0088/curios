//! What every intrinsic demands of its operands, and what it produces.
//!
//! An intrinsic's signature is fixed by the language, so it is a table, and the table is the specification. A table written as a checking procedure can be *executed* by one caller and read by none, which would leave each consumer its own copy — `/sys`'s declarations (`curios-text`'s `sys_module`), the kernel's rules, elaboration's — in three crates, with nothing checking the three agree.
//!
//! This is the one statement. The kernel walks it to check, elaboration walks it to elaborate, and congruence walks it to compare each operand *at its own type* rather than at a flat `Type` — which is what lets proof irrelevance fire on a bound through the ordinary gate instead of through a rule about bounds.
//!
//! A host call is stated in the same vocabulary. `foreign_signature` reads its demands off the row — the roster's for a builtin, the declaration's own otherwise — so both checkers type a foreign call with the walk they run for an intrinsic, and neither keeps a rule of its own for one.
//!
//! **`/sys`'s copy is checked rather than removed, and that is enough.** `/sys` states every one of these types a second time, as the declarations a user actually calls — and it cannot drift, because elaborating a `/sys` body checks its operands against this table and unifies its result with the declared one. A declaration disagreeing with the operation its body constructs does not compile, and the prelude build is where that is enforced.
//!
//! Checked rather than argued: declaring `Nat/div`'s operands `Int` while its body still builds `NatDiv` fails the build with `while elaborating /sys/Nat/div: type mismatch, inferred: Int, expected: Nat`. Reproduce by changing the first `nat()` in `sys_module`'s `guarded_binary("div", …)` to `int()`.
//!
//! **Preconditions belong here.** A window is a start and a *count*, so `spine.rs`'s window fusion has no ordering bound to compose — building a fused window's `Le(s, e)` from its two halves would be transitivity of `<=`, an implication no equality procedure supplies, and a reducer that constructs proofs is a defect. The one bound a window keeps is carried from window₂ to the fused window unchanged: `spine`'s `push` moves a proof it was handed, and the reassociation that makes the two propositions one is `peel_nat_terms`'s to decide.
//!
//! A narrowing carries its bound as an operand for the same reason, as [`Intrinsic::NatToByte`] and [`Intrinsic::IntToNat`] state: a bound stated only on `/sys`'s wrapper stops constraining anything the moment that wrapper unfolds, leaving a bare narrowing this table would type from its operand alone, and one that masked instead would change a value, which `documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md` forbids. The stated domain buys the inverse besides: `ByteToNat` reduces back through `NatToByte`, which is what lets a bound established in `Nat` survive a trip through `Byte`.
//!
//! Channel creation carries its positive-capacity evidence as an operand. Coordination outcomes are ordinary registered inductives: every `Produced::Fixed` result goes through the ordinary type-formation judgment in both checkers, and elaboration retains the resulting universe instance on `CellPoll` and `ChannelTake`. The registry supplies identities, not a separate declaration validator.
//!
//! **These rows cover what a program writes, and nothing below erasure.** The lowerings emit sequence reads of their own, where a proposition cannot be stated at all — so what holds those is that they name no extent to get wrong rather than a bound anything re-checks; see [A lowering names the elimination it performs](../../../documentation/design/compilation/a-lowering-names-the-elimination-it-performs.md).
//!
//! **Totality is the point, not the coverage.** A signature every operation states is one no operation can be forgotten from, which is a stronger property than any individual entry. `Nat`'s successor payload is the standing example: `Nat::Succ` carries a `Term` — that is how `x + 3` is represented — and a rule `Intrinsic::Nat(_) => Ok(nat_type())` would type a successor over a `Bool` as a `Nat`. The elaborator never builds one; catching an elaborator that did is the entire reason a second checker exists, and here that check is a consequence of the table being total rather than an arm someone remembered.

use {
    super::Intrinsic,
    crate::{Free, Global, Level, Nat, Term, Var},
    curios_num::{Floating, Grain, Integer},
    curios_utilities::{SyntaxName, SyntaxRegistry},
};

/// What one operand must be, in [`Intrinsic::traverse`] order.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Check the operand against this type.
    At(Term),
    /// The operand *is* a type; check that it is one. An intrinsic carrying its element type carries a type, and taking that on trust is how a container of nonsense would be admitted.
    IsType,
    /// Check the operand against `(x₁: domain₁, …, xₙ: domainₙ) -> codomain`.
    ///
    /// Spelled as its halves rather than as the function type itself because the binders have to be minted, and a signature is a pure function of the node with no name source of its own. The three operations that need it — `ListMap`, `IoBind` and `ListFold` — are all non-dependent, so which names the walker picks cannot matter.
    Function { domains: Vec<Term>, codomain: Term },
}

/// What an intrinsic produces.
#[derive(Debug, Clone)]
pub enum Produced {
    /// Exactly this type, established by each checker through its ordinary type-formation judgment.
    Fixed(Term),
    /// The sort the parameterized former lands in, which only the sort judgment answers. The element's own sort is *not* it: a list or a cell of proofs has a length or an identity, and a description of proofs has an effect, so none of them is itself a proposition.
    Sort,
}

/// One operation's operand demands and result.
#[derive(Debug, Clone)]
pub struct Signature {
    pub operands: Vec<Operand>,
    pub produced: Produced,
}

impl Intrinsic {
    /// The operand types and result of this operation, with operands in [`traverse`](Intrinsic::traverse) order.
    ///
    /// Order is the contract: a walker zips this against the operands `traverse` yields, so the two are written to be read together and a mismatch in length is a bug this crate can assert on rather than a silent misalignment downstream.
    pub fn signature(&self, syntax: &SyntaxRegistry) -> Signature {
        let bool_type = || Term::intrinsic(Intrinsic::BoolType);
        let nat_type = || Term::intrinsic(Intrinsic::NatType);
        let byte_type = || Term::intrinsic(Intrinsic::ByteType);
        let int_type = || Term::intrinsic(Intrinsic::IntType);
        let flt_type = || Term::intrinsic(Intrinsic::FltType);
        let handle_type = || Term::intrinsic(Intrinsic::HandleType);
        let bin_type = |grain| Term::intrinsic(Intrinsic::BinType(grain));
        let list_type = |element: Term| Term::intrinsic(Intrinsic::ListType(element));
        // A bound is stated over the *intrinsic* measure, not `/sys`'s wrapper: this table sits below `/sys`, and it is the shape a reduced occurrence carries anyway.
        let bin_len = |grain, bin| Term::intrinsic(Intrinsic::BinLen(grain, bin));
        let list_len = |element, list| Term::intrinsic(Intrinsic::ListLen { element, list });
        let cell_type = |element: Term| Term::intrinsic(Intrinsic::CellType(element));
        let channel_type = |element: Term| Term::intrinsic(Intrinsic::ChannelType(element));
        let nominal = |family: SyntaxName, universes: Vec<Level>, params: Vec<Term>| {
            Term::induct_type_at(
                Global::Authored(family.qualifier()),
                universes,
                params,
                Vec::<Term>::new(),
            )
        };
        let io_type = |result: Term| Term::intrinsic(Intrinsic::IoType(result));
        let unit = Term::tuple_type_unit;

        // A grain says what a `Bin` is a sequence *of*: bytes at `X`, bits at `B`. Every `Bin` operation is the same rule at two element types.
        let grain_element = |grain| match grain {
            Grain::X => byte_type(),
            Grain::B => bool_type(),
        };

        let decided = |slot: SyntaxName, args: Vec<Term>| {
            Term::apply(Term::var(Var::free(Free::global(slot.qualifier()))), args)
        };

        // A bound stated over a comparison this table can build: `Holds` applied to the decision itself, rather than a proposition named per operand shape. Named propositions — `Lt`, `Le`, `NonZero`, `NonNeg`, `EightBytes`, and `Flt`'s `Finite` and `NonNeg` — would each be a comparison or a conjunction of two under the same reflection, and naming them would make the `/sys` roster reference a root above it.
        let holds =
            |decision: Intrinsic| decided(syntax.proof.holds, vec![Term::intrinsic(decision)]);

        let sig = |operands: Vec<Operand>, produced: Term| Signature {
            operands,
            produced: Produced::Fixed(produced),
        };
        let nullary = |produced: Term| sig(Vec::new(), produced);
        let un = |operand: Term, produced: Term| sig(vec![Operand::At(operand)], produced);
        let bin_op = |operand: Term, produced: Term| {
            sig(
                vec![Operand::At(operand.clone()), Operand::At(operand)],
                produced,
            )
        };
        // A parameterized former: one type operand, and a sort only the sort judgment can answer.
        let former = || Signature {
            operands: vec![Operand::IsType],
            produced: Produced::Sort,
        };

        match self {
            // Type formers. Every closed one is small; the parameterized ones defer their sort.
            Self::BoolType
            | Self::NatType
            | Self::ByteType
            | Self::IntType
            | Self::FltType
            | Self::BinType(_)
            | Self::HandleType => nullary(Term::type_ground()),
            Self::ListType(_) | Self::CellType(_) | Self::ChannelType(_) | Self::IoType(_) => {
                former()
            }

            // Literals. A `Nat` successor is the one literal carrying a term: `Succ(3, x)` is `x + 3`, and its base is a `Nat` like any other.
            Self::Bool(_) => nullary(bool_type()),
            Self::Nat(self::Nat::Zero) => nullary(nat_type()),
            Self::Nat(self::Nat::Succ(..)) => un(nat_type(), nat_type()),
            Self::Byte(_) => nullary(byte_type()),
            Self::Int(_) => nullary(int_type()),
            Self::Flt(_) => nullary(flt_type()),
            Self::Bin(grain, _) => nullary(bin_type(*grain)),
            Self::Handle(_) => nullary(handle_type()),

            // Comparisons: same-typed operands in, a boolean out.
            Self::BoolEql(..) | Self::BoolNeq(..) => bin_op(bool_type(), bool_type()),
            Self::NatEql(..) | Self::NatNeq(..) | Self::NatLt(..) | Self::NatLe(..) => {
                bin_op(nat_type(), bool_type())
            }
            Self::IntEql(..) | Self::IntNeq(..) | Self::IntLt(..) | Self::IntLe(..) => {
                bin_op(int_type(), bool_type())
            }
            Self::FltEql(..) | Self::FltNeq(..) | Self::FltLt(..) | Self::FltLe(..) => {
                bin_op(flt_type(), bool_type())
            }

            // Arithmetic and bitwise: closed on their carrier.
            Self::BoolAnd(..) | Self::BoolOr(..) | Self::BoolXor(..) => {
                bin_op(bool_type(), bool_type())
            }
            Self::NatAdd(..)
            | Self::NatSub(..)
            | Self::NatMul(..)
            | Self::NatAnd(..)
            | Self::NatOr(..)
            | Self::NatXor(..)
            | Self::NatShl(..)
            | Self::NatShr(..) => bin_op(nat_type(), nat_type()),
            Self::IntAdd(..)
            | Self::IntSub(..)
            | Self::IntMul(..)
            | Self::IntAnd(..)
            | Self::IntOr(..)
            | Self::IntXor(..) => bin_op(int_type(), int_type()),
            // A shift count is a natural on both carriers: a signed count would leave `Int/shr(v, -1)` — `⌊v / 2^-1⌋` — for the theory to define, which it does not.
            Self::IntShl(..) | Self::IntShr(..) => sig(
                vec![Operand::At(int_type()), Operand::At(nat_type())],
                int_type(),
            ),
            Self::FltAdd(..)
            | Self::FltSub(..)
            | Self::FltMul(..)
            | Self::FltDiv(..)
            | Self::FltRem(..)
            | Self::FltMin(..)
            | Self::FltMax(..)
            | Self::FltCopysign(..) => bin_op(flt_type(), flt_type()),
            Self::FltNeg(..)
            | Self::FltAbs(..)
            | Self::FltSqrt(..)
            | Self::FltRoundIntegral(..) => un(flt_type(), flt_type()),
            Self::FltFma(..) => sig(
                vec![
                    Operand::At(flt_type()),
                    Operand::At(flt_type()),
                    Operand::At(flt_type()),
                ],
                flt_type(),
            ),

            // The guarded divisions. A natural is nonzero exactly when zero is below it, which is why `Nat` needs no `NonZero` of its own.
            Self::NatDiv { divisor, .. } | Self::NatRem { divisor, .. } => sig(
                vec![
                    Operand::At(nat_type()),
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLt(
                        Term::intrinsic(Self::Nat(self::Nat::Zero)),
                        divisor.clone(),
                    ))),
                ],
                nat_type(),
            ),
            Self::IntDiv { divisor, .. } | Self::IntRem { divisor, .. } => sig(
                vec![
                    Operand::At(int_type()),
                    Operand::At(int_type()),
                    Operand::At(holds(Self::IntNeq(
                        divisor.clone(),
                        Term::intrinsic(Self::Int(Integer::from(0))),
                    ))),
                ],
                int_type(),
            ),

            // Conversions preserve the number, never the bits — a bit view belongs to the explicit `Bin` casts below.
            Self::ByteToNat(..) => un(byte_type(), nat_type()),
            Self::NatToByte { nat, .. } => sig(
                vec![
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLt(
                        nat.clone(),
                        Term::intrinsic(Self::Nat(self::Nat::new(256u32))),
                    ))),
                ],
                byte_type(),
            ),
            Self::NatToInt(..) => un(nat_type(), int_type()),
            Self::NatToFlt(..) => un(nat_type(), flt_type()),
            Self::IntToNat { int, .. } => sig(
                vec![
                    Operand::At(int_type()),
                    Operand::At(holds(Self::IntLe(
                        Term::intrinsic(Self::Int(Integer::from(0))),
                        int.clone(),
                    ))),
                ],
                nat_type(),
            ),
            Self::IntToFlt(..) => un(int_type(), flt_type()),
            Self::FltToNat { flt, .. } => sig(
                vec![
                    Operand::At(flt_type()),
                    Operand::At(holds(Self::BoolAnd(
                        Term::intrinsic(Self::FltLe(
                            Term::intrinsic(Self::Flt(Floating::zero(false))),
                            flt.clone(),
                        )),
                        Term::intrinsic(Self::FltLt(
                            flt.clone(),
                            Term::intrinsic(Self::Flt(Floating::infinite(false))),
                        )),
                    ))),
                ],
                nat_type(),
            ),
            Self::FltToInt { flt, .. }
            | Self::FltMantissa { flt, .. }
            | Self::FltExponent { flt, .. } => sig(
                vec![
                    Operand::At(flt_type()),
                    Operand::At(holds(Self::BoolAnd(
                        Term::intrinsic(Self::FltLt(
                            Term::intrinsic(Self::Flt(Floating::infinite(true))),
                            flt.clone(),
                        )),
                        Term::intrinsic(Self::FltLt(
                            flt.clone(),
                            Term::intrinsic(Self::Flt(Floating::infinite(false))),
                        )),
                    ))),
                ],
                int_type(),
            ),
            Self::FltToLeBytes(..) => un(flt_type(), bin_type(Grain::X)),
            Self::FltOfLeBytes { bin, .. } => sig(
                vec![
                    Operand::At(bin_type(Grain::X)),
                    Operand::At(holds(Self::NatEql(
                        bin_len(Grain::X, bin.clone()),
                        Term::intrinsic(Self::Nat(self::Nat::new(8u32))),
                    ))),
                ],
                flt_type(),
            ),

            // `Bin`: a sequence of bytes or of bits, depending on the grain.
            Self::BinLen(grain, _) => un(bin_type(*grain), nat_type()),
            Self::BinEql(grain, ..) => bin_op(bin_type(*grain), bool_type()),
            // The two bounded `Bin` accessors. `Bin/len` is the *intrinsic* here rather than `/sys`'s wrapper, because a signature states what the operand must be and this table is below `/sys`.
            Self::BinGet {
                grain, bin, index, ..
            } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLt(
                        index.clone(),
                        bin_len(*grain, bin.clone()),
                    ))),
                ],
                grain_element(*grain),
            ),
            Self::BinSlice {
                grain,
                bin,
                start,
                length,
                ..
            } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(nat_type()),
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLe(
                        Term::intrinsic(Self::NatAdd(start.clone(), length.clone())),
                        bin_len(*grain, bin.clone()),
                    ))),
                ],
                bin_type(*grain),
            ),
            Self::BinAppend { grain, .. } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(grain_element(*grain)),
                ],
                bin_type(*grain),
            ),
            Self::BinConcat { grain, operands } => sig(
                operands
                    .iter()
                    .map(|_| Operand::At(bin_type(*grain)))
                    .collect(),
                bin_type(*grain),
            ),
            // Count first, then the generator, as `/std/List/replicate` reads. No bound: every count names a run, and the length of the one it names is what `BinLen` reduces over this node.
            Self::BinReplicate { grain, .. } => sig(
                vec![Operand::At(nat_type()), Operand::At(grain_element(*grain))],
                bin_type(*grain),
            ),
            // The three pointwise rows are one rule at three operators. See [`Intrinsic::BinAnd`] for why the equal length is stated rather than extended away.
            Self::BinAnd {
                grain, left, right, ..
            }
            | Self::BinOr {
                grain, left, right, ..
            }
            | Self::BinXor {
                grain, left, right, ..
            } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(bin_type(*grain)),
                    Operand::At(holds(Self::NatEql(
                        bin_len(*grain, left.clone()),
                        bin_len(*grain, right.clone()),
                    ))),
                ],
                bin_type(*grain),
            ),
            // One row for both directions, its bound uniform in position and decided per grain — `NatDiv`'s arrangement, which is what keeps this from being two rows that differ only in which way they read.
            Self::BinReinterp { grain, bin, .. } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(match grain {
                        // `n` bytes are `8n` bits, so no count of them leaves a remainder.
                        Grain::X => holds(Self::Bool(true)),
                        // The run holds a whole number of bytes. Spelled with the remainder rather than a mask of the low three bits: the two decide the same thing, and this is the one a caller can read.
                        Grain::B => holds(Self::NatEql(
                            Term::intrinsic(Self::NatRem {
                                dividend: bin_len(Grain::B, bin.clone()),
                                divisor: Term::intrinsic(Self::Nat(self::Nat::new(8u32))),
                                // `0 < 8` over the literal, which reduces to the proposition the registry names an inhabitant for.
                                non_zero: decided(syntax.proof.true_qed, vec![]),
                            }),
                            Term::intrinsic(Self::Nat(self::Nat::new(0u32))),
                        )),
                    }),
                ],
                bin_type(grain.other()),
            ),

            // `List`. Every operation carries its element type as an operand, which is what lets it be typed without inventing anything — `[]` included, which has no element to read a type from.
            Self::List { element, items } => sig(
                std::iter::once(Operand::IsType)
                    .chain(items.iter().map(|_| Operand::At(element.clone())))
                    .collect(),
                list_type(element.clone()),
            ),
            Self::ListLen { element, .. } => sig(
                vec![Operand::IsType, Operand::At(list_type(element.clone()))],
                nat_type(),
            ),
            Self::ListGet {
                element,
                list,
                index,
                ..
            } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLt(
                        index.clone(),
                        list_len(element.clone(), list.clone()),
                    ))),
                ],
                element.clone(),
            ),
            Self::ListSlice {
                element,
                list,
                start,
                length,
                ..
            } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(nat_type()),
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLe(
                        Term::intrinsic(Self::NatAdd(start.clone(), length.clone())),
                        list_len(element.clone(), list.clone()),
                    ))),
                ],
                list_type(element.clone()),
            ),
            Self::ListAppend { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(element.clone()),
                ],
                list_type(element.clone()),
            ),
            Self::ListConcat { element, operands } => sig(
                std::iter::once(Operand::IsType)
                    .chain(
                        operands
                            .iter()
                            .map(|_| Operand::At(list_type(element.clone()))),
                    )
                    .collect(),
                list_type(element.clone()),
            ),
            Self::ListMap { from, to, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::IsType,
                    Operand::At(list_type(from.clone())),
                    Operand::Function {
                        domains: vec![from.clone()],
                        codomain: to.clone(),
                    },
                ],
                list_type(to.clone()),
            ),

            // Guest coordination operations describe effects; their outcomes are ordinary declared inductives.
            Self::Cell { element } => {
                sig(vec![Operand::IsType], io_type(cell_type(element.clone())))
            }
            Self::CellPoll {
                element, universes, ..
            } => sig(
                vec![Operand::IsType, Operand::At(cell_type(element.clone()))],
                io_type(nominal(
                    syntax.option.family,
                    universes.clone(),
                    vec![element.clone()],
                )),
            ),
            Self::CellFill { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(cell_type(element.clone())),
                    Operand::At(element.clone()),
                ],
                io_type(bool_type()),
            ),
            Self::Channel {
                element, capacity, ..
            } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(nat_type()),
                    Operand::At(holds(Self::NatLt(
                        Term::intrinsic(Self::Nat(self::Nat::Zero)),
                        capacity.clone(),
                    ))),
                ],
                io_type(channel_type(element.clone())),
            ),
            Self::ChannelPush { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(channel_type(element.clone())),
                    Operand::At(element.clone()),
                ],
                io_type(nominal(syntax.channel.push, Vec::new(), Vec::new())),
            ),
            Self::ChannelTake {
                element, universes, ..
            } => sig(
                vec![Operand::IsType, Operand::At(channel_type(element.clone()))],
                io_type(nominal(
                    syntax.channel.take,
                    universes.clone(),
                    vec![element.clone()],
                )),
            ),
            Self::ChannelClose { element, .. } => sig(
                vec![Operand::IsType, Operand::At(channel_type(element.clone()))],
                io_type(unit()),
            ),
            Self::ChannelClosed { element, .. } => sig(
                vec![Operand::IsType, Operand::At(channel_type(element.clone()))],
                io_type(bool_type()),
            ),
            Self::ChannelCount { element, .. } | Self::ChannelCapacity { element, .. } => sig(
                vec![Operand::IsType, Operand::At(channel_type(element.clone()))],
                io_type(nat_type()),
            ),

            // The two constructors of the opaque effect carrier. There is no third: nothing anywhere lowers an `Io(T)` to its `T`, which is what makes every term of non-`Io` type pure by typing.
            Self::IoPure { result, .. } => sig(
                vec![Operand::IsType, Operand::At(result.clone())],
                io_type(result.clone()),
            ),
            Self::ListFold {
                element, result, ..
            } => sig(
                vec![
                    Operand::IsType,
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(result.clone()),
                    Operand::Function {
                        domains: vec![element.clone(), result.clone()],
                        codomain: result.clone(),
                    },
                ],
                result.clone(),
            ),
            Self::IoBind { from, to, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::IsType,
                    Operand::At(io_type(from.clone())),
                    Operand::Function {
                        domains: vec![from.clone()],
                        codomain: io_type(to.clone()),
                    },
                ],
                io_type(to.clone()),
            ),
        }
    }
}
