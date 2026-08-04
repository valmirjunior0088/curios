//! The free-monoid destructors. `Nat`, `Bin`, and `Lst` are the native primitives that are free monoids on their generators (the unary unit, bytes, elements). Their structural eliminator (`Cases::FreeMonoid`) reduces by a one-step decode — peel the empty identity or a single leading generator off the scrutinee — and that decode is `uncons`, one match arm per carrier; the catamorphism driver in `reduce` consumes the `Layer` it returns and never inspects the carrier's representation again. The eliminator-side analogue of `spine::peel_prim`: a new free-monoid carrier is one `FreeMonoid` variant and its `uncons` arm.
//!
//! For `Bin` and `Lst`, the same front decode also feeds the operation level: `Bin/get`/`Bin/slice` and `Lst/get`/`Lst/slice` peel one generator at a time via `peel_first_byte`/`peel_first_elem`. Each carrier's two destructors share one structural traversal (`peel_front`, `peel_front_lst`); `Bin` reflects the peeled head two ways — a `Nat` byte for the eliminator, a one-byte `Bin` chunk for the operations — while `Lst`'s head is the element term for both.

use {
    super::{Nat, Prim, Subterm, Term},
    curios_base::{Grain, PackedBin},
    num_bigint::BigUint,
};

/// The one-step decode of a free-monoid scrutinee — a carrier's signature functor made concrete. `Empty` is the identity form (`x[]`, `[]`, `0`); `Cons` peels a generator: its `head` is the payload reflected into the element type — a `Nat` byte for `Bin`, the element term itself for `Lst`, and `None` for `Nat`, whose unary successor carries no payload — over the symbolic tail the induction hypothesis recurses on; `Stuck` is a scrutinee exposing neither form (a variable, a non-cons symbolic concatenation), where the eliminator rebuilds.
pub enum Layer {
    Empty,
    Cons { head: Option<Term>, tail: Term },
    Stuck(Subterm),
}

/// A native primitive that is the free monoid on a generator set, named by its generator: `Unary` is `Nat` on the unit, `Bin` is `Bin` on its bytes, `Lst` is `Lst` on its elements. A closed set — the value-level twin of the `Carrier` the eliminator already carries (`reduce` maps one to the other). `uncons` is the only seam the structural eliminator needs; the recursion scheme around it is carrier-generic and lives in `reduce`.
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
            Subterm::Prim(Prim::Nat(Nat::Zero)) => Layer::Empty,
            // Peel one successor off the spine; the predecessor is the tail, and the unary generator carries no head. A spine count `> 1` keeps the rest as a shorter `Succ`, exactly as the old bespoke `Nat` eliminator did.
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) => {
                let one = BigUint::from(1usize);

                Layer::Cons {
                    head: None,
                    tail: match spine == one {
                        true => inner,
                        false => Term::prim(Prim::Nat(Nat::Succ(spine - one, inner))),
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
            Head::LiteralBit(bit) => Subterm::Prim(Prim::Bool(bit)).into(),
            Head::LiteralByte(byte) => Subterm::Prim(Prim::Byte(byte)).into(),
            Head::Symbolic(atom) => atom,
        }
    }

    /// Reflect into a length-1 `Bin` chunk — the cons head `Bin/get`/`Bin/slice` rebuild as `head ++ tail` (`get(head, 0)` is the byte; a `Utf8` cons head stays the symbolic `append(x[], c)`).
    fn into_chunk(self, grain: Grain) -> Term {
        match self {
            Head::LiteralBit(bit) => {
                Subterm::Prim(Prim::Bin(Grain::B, PackedBin::from_bits([bit]))).into()
            }
            Head::LiteralByte(byte) => {
                Subterm::Prim(Prim::Bin(Grain::X, PackedBin::from_bytes(vec![byte]))).into()
            }
            Head::Symbolic(atom) => Term::prim(Prim::bin_append(
                grain,
                Subterm::Prim(Prim::Bin(grain, PackedBin::empty())),
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
        Subterm::Prim(Prim::Bin(found, bytes)) if *found == grain => match grain {
            Grain::B => match bytes.bit(0) {
                None => Front::Empty,
                Some(bit) => Front::Cons {
                    head: Head::LiteralBit(bit),
                    tail: Subterm::Prim(Prim::Bin(
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
                    tail: Subterm::Prim(Prim::Bin(
                        grain,
                        bytes.slice(grain, 1, bytes.len(grain)).unwrap(),
                    ))
                    .into(),
                },
            },
        },
        // `append(base, atom) = base ++ [atom]`: peel the base's leading generator, and the appended atom rejoins the residual. An empty base is the canonical one-atom chunk, which is where the symbolic head comes from; any other base decodes as far as it can, so a chained `append(append(x[], a), b)` and a run-based `append(x[\48], b)` both expose their first generator instead of going opaque. `core::spine`'s two-value peel has always decoded an append this way, so without this arm the same term was transparent to conversion and opaque to reduction.
        Subterm::Prim(Prim::BinAppend(found, base, atom)) if *found == grain => {
            match peel_front(grain, base) {
                Front::Empty => Front::Cons {
                    head: Head::Symbolic(atom.clone()),
                    tail: Subterm::Prim(Prim::Bin(grain, PackedBin::empty())).into(),
                },
                Front::Cons { head, tail } => Front::Cons {
                    head,
                    tail: Subterm::Prim(Prim::BinAppend(grain, tail, atom.clone())).into(),
                },
                Front::Opaque => Front::Opaque,
            }
        }
        // A concatenation: peel the leading generator off its first operand; the residual first-operand tail rejoins the rest.
        Subterm::Prim(Prim::BinConcat(found, operands)) if *found == grain => {
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
                            0 => Subterm::Prim(Prim::Bin(grain, PackedBin::empty())).into(),
                            1 => segments.into_iter().next().unwrap(),
                            _ => Subterm::Prim(Prim::BinConcat(grain, segments)).into(),
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

/// One step of the `Lst` front decode — the element-typed analogue of [`Front`]. `Empty` is the identity (`[]`); `Cons` peels the leading element and the residual tail; `Opaque` is a value exposing no leading element (a variable, a slice, an append). Unlike `Bin`, the head needs no reflection — an `Lst` generator IS its element term, used directly by both the eliminator and the operations.
enum LstFront {
    Empty,
    Cons { head: Term, tail: Term },
    Opaque,
}

/// The structural traversal shared by both `Lst` destructors ([`FreeMonoid::uncons`] for the eliminator, [`peel_first_elem`] for `Lst/get`/`Lst/slice`) — the element-typed twin of [`peel_front`]. Peel the leading element off an already-reduced value: a literal run yields its first element; a symbolic cons `concat([h], t)` yields `h` off its leading non-empty literal segment, the residual elements rejoining the rest exactly as `LstConcat` collapses them. The empty list is `Empty`; anything else (a variable, a slice, an append) is `Opaque`.
fn peel_front_lst(lst: &Term) -> LstFront {
    match &**lst {
        Subterm::Prim(Prim::Lst(elem, elems)) => match elems.split_first() {
            None => LstFront::Empty,
            Some((head, rest)) => LstFront::Cons {
                head: head.clone(),
                tail: Subterm::Prim(Prim::Lst(elem.clone(), rest.to_vec())).into(),
            },
        },
        // A symbolic cons: decode the head off the leading non-empty literal segment; its remaining elements stay ahead of the rest.
        Subterm::Prim(Prim::LstConcat(elem, segments)) => match segments.split_first() {
            Some((first, rest)) if is_nonempty_lst_literal(first) => {
                let mut lead = match &**first {
                    Subterm::Prim(Prim::Lst(_, elems)) => elems.clone(),
                    _ => unreachable!("guard checked a non-empty `Lst` literal lead segment"),
                };

                let head = lead.remove(0);

                let mut segments = Vec::with_capacity(rest.len() + 1);

                if !lead.is_empty() {
                    segments.push(Subterm::Prim(Prim::Lst(elem.clone(), lead)).into());
                }

                segments.extend(rest.iter().cloned());

                let tail = match segments.len() {
                    0 => Subterm::Prim(Prim::Lst(elem.clone(), vec![])).into(),
                    1 => segments.into_iter().next().unwrap(),
                    _ => Subterm::Prim(Prim::LstConcat(elem.clone(), segments)).into(),
                };
                LstFront::Cons { head, tail }
            }
            _ => LstFront::Opaque,
        },
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
    matches!(&**term, Subterm::Prim(Prim::Bin(found, bytes)) if *found == grain && bytes.is_empty())
}

// `cons` injects an element at the front as the singleton literal `[h]`; the `Lst` eliminator recognizes a non-empty literal lead segment to decode a symbolic cons.
fn is_nonempty_lst_literal(term: &Term) -> bool {
    matches!(&**term, Subterm::Prim(Prim::Lst(_, elems)) if !elems.is_empty())
}

/// The free monoid's normalising *product* — the constructor dual of [`FreeMonoid::uncons`] (the destructor) — shared verbatim by `BinConcat` and `LstConcat` reduction. Collapse a concatenation's already-reduced `operands` to a normal form under the unit and associativity laws: drop the empty identity (`x[]`, `[]`), merge adjacent literal runs into one literal, and collapse a lone surviving operand to itself (an `n`-ary concat of one *is* that one). `literal` borrows an operand's run when it is a literal (`None` for a symbolic chunk); `into_literal`/`into_concat` rebuild the result in the carrier's primitives.
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
