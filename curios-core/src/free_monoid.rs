//! The free-monoid destructors. `Nat`, `Bin`, and `Lst` are the native intrinsics that are free monoids on their generators (the unary unit, bytes, elements). Their structural eliminator (`Cases::FreeMonoid`) reduces by a one-step decode — peel the empty identity or a single leading generator off the scrutinee — and that decode is `uncons`, one match arm per carrier; the catamorphism driver in `reduce` consumes the `Layer` it returns and never inspects the carrier's representation again. The eliminator-side analogue of `spine::peel_intrinsic`: a new free-monoid carrier is one `FreeMonoid` variant and its `uncons` arm.
//!
//! For `Bin` and `Lst`, the same front decode also feeds the operation level: `Bin/get`/`Bin/slice` and `Lst/get`/`Lst/slice` peel one generator at a time via `peel_first_byte`/`peel_first_elem`. Each carrier's two destructors share one structural traversal (`peel_front`, `peel_front_lst`); `Bin` reflects the peeled head two ways — a `Nat` byte for the eliminator, a one-byte `Bin` chunk for the operations — while `Lst`'s head is the element term for both.

use {
    super::{Intrinsic, Nat, Subterm, Term},
    curios_base::{Grain, PackedBin},
    num_bigint::BigUint,
};

/// The one-step decode of a free-monoid scrutinee — a carrier's signature functor made concrete. `Empty` is the identity form (`x[]`, `[]`, `0`); `Cons` peels a generator: its `head` is the payload reflected into the element type — a `Nat` byte for `Bin`, the element term itself for `Lst`, and `None` for `Nat`, whose unary successor carries no payload — over the symbolic tail the induction hypothesis recurses on; `Stuck` is a scrutinee exposing neither form (a variable, a non-cons symbolic concatenation), where the eliminator rebuilds.
pub enum Layer {
    Empty,
    Cons { head: Option<Term>, tail: Term },
    Stuck(Subterm),
}

/// A native intrinsic that is the free monoid on a generator set, named by its generator: `Unary` is `Nat` on the unit, `Bin` is `Bin` on its bytes, `Lst` is `Lst` on its elements. A closed set — the value-level twin of the `Carrier` the eliminator already carries (`reduce` maps one to the other). `uncons` is the only seam the structural eliminator needs; the recursion scheme around it is carrier-generic and lives in `reduce`.
pub enum FreeMonoid {
    /// The free monoid on one payload-less generator: the unary naturals. `succ` peels to its predecessor with no head.
    Unary,
    /// The free monoid on bytes. Its generator is a byte, reflected back into the eliminator as a `Nat`.
    Bin(Grain),
    /// The free monoid on its elements. Its generator is the element term itself.
    Lst,
}

impl FreeMonoid {
    /// Decode one constructor layer off an already-reduced scrutinee — the values live as `Subterm`s, so the carrier is the only thing dispatched on.
    pub fn uncons(self, scrutinee: Subterm) -> Layer {
        match self {
            FreeMonoid::Unary => Self::uncons_unary(scrutinee),
            FreeMonoid::Bin(grain) => Self::uncons_bin(grain, scrutinee),
            FreeMonoid::Lst => Self::uncons_lst(scrutinee),
        }
    }

    fn uncons_unary(scrutinee: Subterm) -> Layer {
        match scrutinee {
            // The identity: zero.
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => Layer::Empty,
            // Peel one successor off the spine; the predecessor is the tail, and the unary generator carries no head. A spine count `> 1` keeps the rest as a shorter `Succ`, exactly as the old bespoke `Nat` eliminator did.
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine, inner))) => {
                let one = BigUint::from(1usize);

                Layer::Cons {
                    head: None,
                    tail: match spine == one {
                        true => inner,
                        false => Term::intrinsic(Intrinsic::Nat(Nat::Succ(spine - one, inner))),
                    },
                }
            }
            stuck => Layer::Stuck(stuck),
        }
    }

    fn uncons_bin(grain: Grain, scrutinee: Subterm) -> Layer {
        let term = scrutinee.into();

        match peel_front(grain, &term) {
            Front::Empty => Layer::Empty,
            // The peeled byte is the eliminator's `Nat`-typed head generator.
            Front::Cons { head, tail } => Layer::Cons {
                head: Some(head.into_atom()),
                tail,
            },
            Front::Opaque => Layer::Stuck(Term::unwrap_or_clone(term)),
        }
    }

    fn uncons_lst(scrutinee: Subterm) -> Layer {
        let term = scrutinee.into();

        match peel_front_lst(&term) {
            LstFront::Empty => Layer::Empty,
            // The peeled element is the eliminator's head generator directly.
            LstFront::Cons { head, tail } => Layer::Cons {
                head: Some(head),
                tail,
            },
            LstFront::Opaque => Layer::Stuck(Term::unwrap_or_clone(term)),
        }
    }
}

/// A leading generator peeled off a `Bin` value: a concrete byte (`Literal`) or the symbolic byte of a `Utf8` cons `append(x[], c)` (`Symbolic`). Kept abstract so each destructor reflects it into the shape its consumer wants — the eliminator into a `Nat`, `Bin/get`/`Bin/slice` into a one-byte `Bin` chunk.
enum Head {
    LiteralBit(bool),
    LiteralByte(u8),
    Symbolic(Term),
}

impl Head {
    /// Reflect into the eliminator's grain-specific element type.
    fn into_atom(self) -> Term {
        match self {
            Head::LiteralBit(bit) => Subterm::Intrinsic(Intrinsic::Bool(bit)).into(),
            Head::LiteralByte(byte) => Subterm::Intrinsic(Intrinsic::Byte(byte)).into(),
            Head::Symbolic(atom) => atom,
        }
    }

    /// Reflect into a length-1 `Bin` chunk — the cons head `Bin/get`/`Bin/slice` rebuild as `head ++ tail` (`get(head, 0)` is the byte; a `Utf8` cons head stays the symbolic `append(x[], c)`).
    fn into_chunk(self, grain: Grain) -> Term {
        match self {
            Head::LiteralBit(bit) => {
                Subterm::Intrinsic(Intrinsic::Bin(Grain::B, PackedBin::from_bits([bit]))).into()
            }
            Head::LiteralByte(byte) => {
                Subterm::Intrinsic(Intrinsic::Bin(Grain::X, PackedBin::from_bytes(vec![byte])))
                    .into()
            }
            Head::Symbolic(atom) => Term::intrinsic(Intrinsic::bin_append(
                grain,
                Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())),
                atom,
            )),
        }
    }
}

/// One step of the `Bin` front decode. `Empty` is the identity (`x[]`); `Cons` peels a leading generator and the residual tail; `Opaque` is a value exposing no leading generator (a variable, a slice, a non-`x[]`-based append).
enum Front {
    Empty,
    Cons { head: Head, tail: Term },
    Opaque,
}

/// The structural traversal shared by both `Bin` destructors ([`FreeMonoid::uncons`] for the eliminator, [`peel_first_atom`] for `Bin/get`/`Bin/slice`): peel the leading generator off an already-reduced value. A literal run yields its first byte; a `Utf8` cons `append(x[], c)` yields its symbolic byte; a concatenation recurses into its first operand so a literal- or cons-led `BinConcat` decodes too, the residual first-operand tail rejoining the rest — normalised (an empty first-operand tail drops, a lone survivor collapses) so a cons-led concat decodes to the same tail the bare cons would. The empty bytestring is `Empty`; anything else (a variable, a slice, a non-`x[]`-based append) is `Opaque`.
fn peel_front(grain: Grain, bin: &Term) -> Front {
    match &**bin {
        Subterm::Intrinsic(Intrinsic::Bin(found, bytes)) if *found == grain => match grain {
            Grain::B => match bytes.bit(0) {
                None => Front::Empty,
                Some(bit) => Front::Cons {
                    head: Head::LiteralBit(bit),
                    tail: Subterm::Intrinsic(Intrinsic::Bin(
                        grain,
                        bytes.slice(grain, 1, bytes.len(grain)).unwrap(),
                    ))
                    .into(),
                },
            },
            Grain::X => match bytes.byte(0) {
                None => Front::Empty,
                Some(byte) => Front::Cons {
                    head: Head::LiteralByte(byte),
                    tail: Subterm::Intrinsic(Intrinsic::Bin(
                        grain,
                        bytes.slice(grain, 1, bytes.len(grain)).unwrap(),
                    ))
                    .into(),
                },
            },
        },
        // `append(base, atom) = base ++ [atom]`: peel the base's leading generator, and the appended atom rejoins the residual. An empty base is the canonical one-atom chunk, which is where the symbolic head comes from; any other base decodes as far as it can, so a chained `append(append(x[], a), b)` and a run-based `append(x[\48], b)` both expose their first generator instead of going opaque. `core::spine`'s two-value peel has always decoded an append this way, so without this arm the same term was transparent to conversion and opaque to reduction.
        Subterm::Intrinsic(Intrinsic::BinAppend(found, base, atom)) if *found == grain => {
            match peel_front(grain, base) {
                Front::Empty => Front::Cons {
                    head: Head::Symbolic(atom.clone()),
                    tail: Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())).into(),
                },
                Front::Cons { head, tail } => Front::Cons {
                    head,
                    tail: Subterm::Intrinsic(Intrinsic::BinAppend(grain, tail, atom.clone()))
                        .into(),
                },
                Front::Opaque => Front::Opaque,
            }
        }
        // A concatenation: peel the leading generator off its first operand; the residual first-operand tail rejoins the rest.
        Subterm::Intrinsic(Intrinsic::BinConcat(found, operands)) if *found == grain => {
            match operands.split_first() {
                Some((first, rest)) => match peel_front(grain, first) {
                    Front::Cons {
                        head,
                        tail: first_tail,
                    } => {
                        let mut segments = Vec::with_capacity(operands.len());
                        if !is_empty_bin(grain, &first_tail) {
                            segments.push(first_tail);
                        }
                        segments.extend(rest.iter().cloned());
                        let tail = match segments.len() {
                            0 => {
                                Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())).into()
                            }
                            1 => segments.into_iter().next().unwrap(),
                            _ => Subterm::Intrinsic(Intrinsic::BinConcat(grain, segments)).into(),
                        };
                        Front::Cons { head, tail }
                    }
                    // A reduced `BinConcat` has no empty operands, so a first operand that exposes no byte is opaque (a leading variable/slice).
                    Front::Empty | Front::Opaque => Front::Opaque,
                },
                None => Front::Empty,
            }
        }
        _ => Front::Opaque,
    }
}

/// Split the first byte off a reduced `Bin` value, returning a length-1 head chunk and the residual tail. Where `peel_bin` (`core::spine`) strips a common prefix of *two* values, this decomposes *one* — the operation-level destructor `Bin/get` and `Bin/slice` walk a codepoint at a time, exposing the cons structure the `Utf8` relation builds (`concat(append(x[], h), t)`) along with literal runs and concatenations. `None` for the empty bytestring or an opaque symbolic value, where no first byte is statically exposed.
pub(crate) fn peel_first_atom(grain: Grain, bin: &Term) -> Option<(Term, Term)> {
    match peel_front(grain, bin) {
        Front::Cons { head, tail } => Some((head.into_chunk(grain), tail)),
        Front::Empty | Front::Opaque => None,
    }
}

/// One step of the `Lst` front decode — the element-typed analogue of [`Front`]. `Empty` is the identity (`[]`); `Cons` peels the leading element and the residual tail; `Opaque` is a value exposing no leading element (a variable, a slice). Unlike `Bin`, the head needs no reflection — an `Lst` generator IS its element term, used directly by both the eliminator and the operations.
enum LstFront {
    Empty,
    Cons { head: Term, tail: Term },
    Opaque,
}

/// The structural traversal shared by both `Lst` destructors ([`FreeMonoid::uncons`] for the eliminator, [`peel_first_elem`] for `Lst/get`/`Lst/slice`) — the element-typed twin of [`peel_front`]. Peel the leading element off an already-reduced value: a literal run yields its first element; `append(base, e)` decodes through its base, the appended element rejoining the residual; a concatenation recurses into its first segment — so the symbolic cons `concat([h], t)`, a cons-led concat, and an append over a decodable base all expose their first element, the residual renormalised (an empty first-segment tail drops, a lone survivor collapses) so each decodes to the same tail its flattened form would. The empty list is `Empty`; anything else (a variable, a slice) is `Opaque`.
fn peel_front_lst(lst: &Term) -> LstFront {
    match &**lst {
        Subterm::Intrinsic(Intrinsic::Lst(elem, elems)) => match elems.split_first() {
            None => LstFront::Empty,
            Some((head, rest)) => LstFront::Cons {
                head: head.clone(),
                tail: Subterm::Intrinsic(Intrinsic::Lst(elem.clone(), rest.to_vec())).into(),
            },
        },
        // `append(base, e) = base ++ [e]`: peel the base's leading element, and the appended element rejoins the residual. An empty base's front is the appended element itself; a decodable base exposes its own head instead. The element-typed twin of the `BinAppend` arm above, closing the same gap: `core::spine`'s two-value peel decodes an append as `concat(base, [e])`, so without this arm the same term was transparent to conversion and opaque to reduction.
        Subterm::Intrinsic(Intrinsic::LstAppend(elem, base, appended)) => {
            match peel_front_lst(base) {
                LstFront::Empty => LstFront::Cons {
                    head: appended.clone(),
                    tail: Subterm::Intrinsic(Intrinsic::Lst(elem.clone(), vec![])).into(),
                },
                LstFront::Cons { head, tail } => LstFront::Cons {
                    head,
                    tail: Subterm::Intrinsic(Intrinsic::LstAppend(
                        elem.clone(),
                        tail,
                        appended.clone(),
                    ))
                    .into(),
                },
                LstFront::Opaque => LstFront::Opaque,
            }
        }
        // A concatenation: peel the leading element off its first segment; the residual first-segment tail rejoins the rest.
        Subterm::Intrinsic(Intrinsic::LstConcat(elem, segments)) => {
            match segments.split_first() {
                Some((first, rest)) => match peel_front_lst(first) {
                    LstFront::Cons {
                        head,
                        tail: first_tail,
                    } => {
                        let mut kept = Vec::with_capacity(segments.len());
                        if !is_empty_lst(&first_tail) {
                            kept.push(first_tail);
                        }
                        kept.extend(rest.iter().cloned());
                        let tail = match kept.len() {
                            0 => Subterm::Intrinsic(Intrinsic::Lst(elem.clone(), vec![])).into(),
                            1 => kept.into_iter().next().unwrap(),
                            _ => {
                                Subterm::Intrinsic(Intrinsic::LstConcat(elem.clone(), kept)).into()
                            }
                        };
                        LstFront::Cons { head, tail }
                    }
                    // A reduced `LstConcat` has no empty segments, so a first segment that exposes no element is opaque (a leading variable/slice).
                    LstFront::Empty | LstFront::Opaque => LstFront::Opaque,
                },
                None => LstFront::Empty,
            }
        }
        _ => LstFront::Opaque,
    }
}

/// Split the first element off a reduced `Lst` value, returning the head element and the residual tail — the element-typed twin of [`peel_first_atom`]. Lets `Lst/get` and `Lst/slice` peel a symbolic cons one element at a time, exactly as `Bin/get`/ `Bin/slice` walk a byte at a time. `None` for the empty array or an opaque symbolic value, where no first element is statically exposed.
pub(crate) fn peel_first_elem(lst: &Term) -> Option<(Term, Term)> {
    match peel_front_lst(lst) {
        LstFront::Cons { head, tail } => Some((head, tail)),
        LstFront::Empty | LstFront::Opaque => None,
    }
}

// `cons` injects a byte at the front as `append(x[], c)` (a one-byte `Bin`); `peel_front` recognises that encoding to decode a symbolic cons head.
fn is_empty_bin(grain: Grain, term: &Term) -> bool {
    matches!(&**term, Subterm::Intrinsic(Intrinsic::Bin(found, bytes)) if *found == grain && bytes.is_empty())
}

// `cons` injects an element at the front as the singleton literal `[h]`; `peel_front_lst`'s concat recursion decodes that encoding through the literal case.
fn is_empty_lst(term: &Term) -> bool {
    matches!(&**term, Subterm::Intrinsic(Intrinsic::Lst(_, elems)) if elems.is_empty())
}

/// The free monoid's normalising *product* — the constructor dual of [`FreeMonoid::uncons`] (the destructor) — shared verbatim by `BinConcat` and `LstConcat` reduction. Collapse a concatenation's already-reduced `operands` to a normal form under the unit and associativity laws: drop the empty identity (`x[]`, `[]`), merge adjacent literal runs into one literal, and collapse a lone surviving operand to itself (an `n`-ary concat of one *is* that one). `literal` borrows an operand's run when it is a literal (`None` for a symbolic chunk); `into_literal`/`into_concat` rebuild the result in the carrier's intrinsics.
///
/// Window fusion (adjacent `Bin/slice`s of one base) is deliberately NOT done here: that is the spine peel's job when *deciding equality* (`spine::push`); reduction only needs a normal form, and conversion closes any residual gap.
pub(crate) fn normalize_concat<E: Clone>(
    operands: Vec<Term>,
    literal: fn(&Term) -> Option<&[E]>,
    into_literal: impl FnOnce(Vec<E>) -> Subterm,
    into_concat: impl FnOnce(Vec<Term>) -> Subterm,
) -> Subterm {
    let mut kept: Vec<Term> = operands
        .into_iter()
        .filter(|operand| !matches!(literal(operand), Some(run) if run.is_empty()))
        .collect();

    // Every surviving operand literal ⇒ one merged literal; the first symbolic chunk stops the fold, leaving the concatenation (a lone operand collapses to itself).
    let merged = kept.iter().try_fold(Vec::new(), |mut run, operand| {
        run.extend(literal(operand)?.iter().cloned());

        Some(run)
    });

    match merged {
        Some(run) => into_literal(run),
        None if kept.len() == 1 => Term::unwrap_or_clone(kept.pop().unwrap()),
        None => into_concat(kept),
    }
}
