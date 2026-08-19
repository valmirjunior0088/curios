//! What every intrinsic demands of its operands, and what it produces.
//!
//! `curios-cert`'s `infer_intrinsic` opens by calling itself a table — "an intrinsic's signature is fixed by the language, so this module is a table, and the table is the specification". It was right about what it wanted to be and wrong about what it was: a table written as a checking procedure can be *executed* by one caller and read by none. So the same signatures were written three times — here in `/sys`'s declarations (`curios-text`'s `prelude`), again as the kernel's rules, and a third time as elaboration's — in three crates, with nothing checking the three agreed.
//!
//! This is the one statement. The kernel walks it to check, elaboration walks it to elaborate, and congruence walks it to compare each operand *at its own type* rather than at a flat `Type` — which is what lets proof irrelevance fire on a bound through the ordinary gate instead of through a rule about bounds.
//!
//! **The third copy is checked rather than removed, and that is enough.** `/sys` still states every one of these types a second time, as the declarations a user actually calls — and it cannot drift, because elaborating a `/sys` body checks its operands against this table and unifies its result with the declared one. A declaration disagreeing with the operation its body constructs does not compile, and the prelude build is where that is enforced.
//!
//! Measured 2026-08-19 rather than argued: declaring `Nat/div`'s operands `Int` while its body still builds `NatDiv` fails the build with `while elaborating /sys/Nat/div: type mismatch, inferred: Int, expected: Nat`. Reproduce by changing the first `nat()` in `prelude`'s `guarded_binary("div", …)` to `int()`.
//!
//! **Totality is the point, not the coverage.** A signature every operation states is one no operation can be forgotten from, which is a stronger property than any individual entry. `Nat`'s successor payload is the standing example: `Nat::Succ` carries a `Term` — that is how `x + 3` is represented — and the kernel's `Intrinsic::Nat(_) => Ok(nat_type())` never checked it, so a successor over a `Bool` typed as a `Nat`. Nothing constructs one today, and the elaborator never would; catching an elaborator that did is the entire reason a second checker exists. Here that check is a consequence of the table being total rather than an arm someone remembered.

use {
    super::Intrinsic,
    crate::{Nat, Term},
    curios_utilities::{Grain, SyntaxName, SyntaxRegistry},
};

/// What one operand must be, in [`Intrinsic::traverse`] order.
#[derive(Debug, Clone)]
pub enum Operand {
    /// Check the operand against this type.
    At(Term),
    /// The operand *is* a type; check that it is one. An intrinsic carrying its element type carries a type, and taking that on trust is how a container of nonsense would be admitted.
    IsType,
    /// Check the operand against `(x: domain) -> codomain`.
    ///
    /// Spelled as its two halves rather than as the function type itself because the binder has to be minted, and a signature is a pure function of the node with no name source of its own. The two operations that need it — `ListMap` and `IoBind` — are both non-dependent, so which name the walker picks cannot matter.
    Function { domain: Term, codomain: Term },
}

/// What an intrinsic produces.
#[derive(Debug, Clone)]
pub enum Produced {
    /// Exactly this type.
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
        let cell_type = |element: Term| Term::intrinsic(Intrinsic::CellType(element));
        let io_type = |result: Term| Term::intrinsic(Intrinsic::IoType(result));
        let unit = Term::tuple_type_unit;

        // A grain says what a `Bin` is a sequence *of*: bytes at `X`, bits at `B`. Every `Bin` operation is the same rule at two element types.
        let grain_element = |grain| match grain {
            Grain::X => byte_type(),
            Grain::B => bool_type(),
        };

        let decided = |slot: SyntaxName, args: Vec<Term>| {
            Term::apply(
                Term::var(crate::Var::free(crate::Free::global(slot.qualifier()))),
                args,
            )
        };

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

        use Intrinsic::*;

        match self {
            // Type formers. Every closed one is small; the parameterized ones defer their sort.
            BoolType | NatType | ByteType | IntType | FltType | BinType(_) | HandleType => {
                nullary(Term::type_ground())
            }
            ListType(_) | CellType(_) | IoType(_) => former(),

            // Literals. A `Nat` successor is the one literal carrying a term: `Succ(3, x)` is `x + 3`, and its base is a `Nat` like any other.
            Bool(_) => nullary(bool_type()),
            Nat(self::Nat::Zero) => nullary(nat_type()),
            Nat(self::Nat::Succ(..)) => un(nat_type(), nat_type()),
            Byte(_) => nullary(byte_type()),
            Int(_) => nullary(int_type()),
            Flt(_) => nullary(flt_type()),
            Bin(grain, _) => nullary(bin_type(*grain)),
            Handle(_) => nullary(handle_type()),

            // Comparisons: same-typed operands in, a boolean out.
            BoolEql(..) | BoolNeq(..) => bin_op(bool_type(), bool_type()),
            NatEql(..) | NatNeq(..) | NatLt(..) | NatGt(..) | NatLe(..) | NatGe(..) => {
                bin_op(nat_type(), bool_type())
            }
            ByteEql(..) | ByteLt(..) | ByteLe(..) | ByteGt(..) | ByteGe(..) => {
                bin_op(byte_type(), bool_type())
            }
            IntEql(..) | IntNeq(..) | IntLt(..) | IntGt(..) | IntLe(..) | IntGe(..) => {
                bin_op(int_type(), bool_type())
            }
            FltEql(..) | FltNeq(..) | FltLt(..) | FltGt(..) | FltLe(..) | FltGe(..) => {
                bin_op(flt_type(), bool_type())
            }
            HandleEql(..) => bin_op(handle_type(), bool_type()),

            // Arithmetic and bitwise: closed on their carrier.
            BoolAnd(..) | BoolOr(..) | BoolXor(..) => bin_op(bool_type(), bool_type()),
            NatAdd(..) | NatSub(..) | NatMul(..) | NatAnd(..) | NatOr(..) | NatXor(..)
            | NatShl(..) | NatShr(..) | NatRotl(..) | NatRotr(..) => bin_op(nat_type(), nat_type()),
            IntAdd(..) | IntSub(..) | IntMul(..) | IntAnd(..) | IntOr(..) | IntXor(..)
            | IntShl(..) | IntShr(..) | IntRotl(..) | IntRotr(..) => bin_op(int_type(), int_type()),
            FltAdd(..) | FltSub(..) | FltMul(..) | FltDiv(..) | FltRem(..) | FltMin(..)
            | FltMax(..) | FltCopysign(..) => bin_op(flt_type(), flt_type()),
            NatClz(..) | NatCtz(..) | NatPopcnt(..) => un(nat_type(), nat_type()),
            IntClz(..) | IntCtz(..) | IntPopcnt(..) => un(int_type(), int_type()),
            FltNeg(..) | FltAbs(..) | FltSqrt(..) | FltFloor(..) | FltCeil(..) | FltTrunc(..)
            | FltNearest(..) => un(flt_type(), flt_type()),

            // The guarded divisions. A natural is nonzero exactly when zero is below it, which is why `Nat` needs no `NonZero` of its own.
            NatDiv { divisor, .. } | NatRem { divisor, .. } => sig(
                vec![
                    Operand::At(nat_type()),
                    Operand::At(nat_type()),
                    Operand::At(decided(
                        syntax.proof.lt,
                        vec![Term::intrinsic(Nat(self::Nat::Zero)), divisor.clone()],
                    )),
                ],
                nat_type(),
            ),
            IntDiv { divisor, .. } | IntRem { divisor, .. } => sig(
                vec![
                    Operand::At(int_type()),
                    Operand::At(int_type()),
                    Operand::At(decided(syntax.proof.int_non_zero, vec![divisor.clone()])),
                ],
                int_type(),
            ),

            // Conversions preserve the number, never the bits — a bit view belongs to the explicit `Bin` casts below.
            ByteToNat(..) => un(byte_type(), nat_type()),
            NatToByte(..) => un(nat_type(), byte_type()),
            NatToInt(..) => un(nat_type(), int_type()),
            NatToFlt(..) => un(nat_type(), flt_type()),
            IntToNat(..) => un(int_type(), nat_type()),
            IntToFlt(..) => un(int_type(), flt_type()),
            FltToNat(..) => un(flt_type(), nat_type()),
            FltToInt(..) => un(flt_type(), int_type()),
            FltToLeBytes(..) => un(flt_type(), bin_type(Grain::X)),
            FltOfLeBytes { bin, .. } => sig(
                vec![
                    Operand::At(bin_type(Grain::X)),
                    Operand::At(decided(syntax.proof.bytes_four, vec![bin.clone()])),
                ],
                flt_type(),
            ),

            // `Bin`: a sequence of bytes or of bits, depending on the grain.
            BinLen(grain, _) => un(bin_type(*grain), nat_type()),
            BinEql(grain, ..) => bin_op(bin_type(*grain), bool_type()),
            BinGet { grain, .. } => sig(
                vec![Operand::At(bin_type(*grain)), Operand::At(nat_type())],
                grain_element(*grain),
            ),
            BinSlice { grain, .. } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(nat_type()),
                    Operand::At(nat_type()),
                ],
                bin_type(*grain),
            ),
            BinAppend { grain, .. } => sig(
                vec![
                    Operand::At(bin_type(*grain)),
                    Operand::At(grain_element(*grain)),
                ],
                bin_type(*grain),
            ),
            BinConcat { grain, operands } => sig(
                operands
                    .iter()
                    .map(|_| Operand::At(bin_type(*grain)))
                    .collect(),
                bin_type(*grain),
            ),

            // `List`. Every operation carries its element type as an operand, which is what lets it be typed without inventing anything — `[]` included, the case that used to be refused for having no element to read a type from.
            List { element, items } => sig(
                std::iter::once(Operand::IsType)
                    .chain(items.iter().map(|_| Operand::At(element.clone())))
                    .collect(),
                list_type(element.clone()),
            ),
            ListLen { element, .. } => sig(
                vec![Operand::IsType, Operand::At(list_type(element.clone()))],
                nat_type(),
            ),
            ListGet { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(nat_type()),
                ],
                element.clone(),
            ),
            ListSlice { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(nat_type()),
                    Operand::At(nat_type()),
                ],
                list_type(element.clone()),
            ),
            ListAppend { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(list_type(element.clone())),
                    Operand::At(element.clone()),
                ],
                list_type(element.clone()),
            ),
            ListConcat { element, operands } => sig(
                std::iter::once(Operand::IsType)
                    .chain(
                        operands
                            .iter()
                            .map(|_| Operand::At(list_type(element.clone()))),
                    )
                    .collect(),
                list_type(element.clone()),
            ),
            ListMap { from, to, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::IsType,
                    Operand::At(list_type(from.clone())),
                    Operand::Function {
                        domain: from.clone(),
                        codomain: to.clone(),
                    },
                ],
                list_type(to.clone()),
            ),

            // A mutable cell, and the process exit. All of these are host effects and so describe rather than do: `CellGet` returning `Io(T)` rather than `T` is what makes `match Cell/get(c)` ill-typed.
            ProcExit(..) => un(nat_type(), io_type(unit())),
            Cell { element, .. } => sig(
                vec![Operand::IsType, Operand::At(element.clone())],
                io_type(cell_type(element.clone())),
            ),
            CellGet { element, .. } => sig(
                vec![Operand::IsType, Operand::At(cell_type(element.clone()))],
                io_type(element.clone()),
            ),
            CellSet { element, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::At(cell_type(element.clone())),
                    Operand::At(element.clone()),
                ],
                io_type(unit()),
            ),

            // The two constructors of the opaque effect carrier. There is no third: nothing anywhere lowers an `Io(T)` to its `T`, which is what makes every term of non-`Io` type pure by typing.
            IoPure { result, .. } => sig(
                vec![Operand::IsType, Operand::At(result.clone())],
                io_type(result.clone()),
            ),
            IoBind { from, to, .. } => sig(
                vec![
                    Operand::IsType,
                    Operand::IsType,
                    Operand::At(io_type(from.clone())),
                    Operand::Function {
                        domain: from.clone(),
                        codomain: io_type(to.clone()),
                    },
                ],
                io_type(to.clone()),
            ),
        }
    }
}
