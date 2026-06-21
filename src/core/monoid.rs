//! The free-monoid destructors. `Nat`, `Bin`, and `Arr` are the native primitives
//! that are free monoids on their generators (the unary unit, bytes, elements).
//! Their structural eliminator (`Cases::Inductive`) reduces by a one-step decode —
//! peel the empty identity or a single leading generator off the scrutinee — and
//! that decode is `uncons`, one method per carrier; the catamorphism driver in
//! `reduce` consumes the `Layer` it returns and never inspects the carrier's
//! representation again. The eliminator-side analogue of `spine::peel_prim`: a new
//! free-monoid carrier is one `uncons` and nothing else.
//!
//! For `Bin`, the same front decode also feeds the operation level: `Bin/get` and
//! `Bin/slice` peel one byte at a time along a codepoint walk via `peel_first_byte`.
//! Both destructors share one structural traversal, `peel_front`, differing only in
//! how they reflect the peeled head — a `Nat` byte for the eliminator, a one-byte
//! `Bin` chunk for the operations.

use {
    super::{Nat, Prim, Subterm, Term},
    num_bigint::BigUint,
};

/// The one-step decode of a free-monoid scrutinee — a carrier's signature functor
/// made concrete. `Empty` is the identity form (`\\`, `[]`, `0`); `Cons` peels a
/// generator: its `head` is the payload reflected into the element type — a `Nat`
/// byte for `Bin`, the element term itself for `Arr`, and `None` for `Nat`, whose
/// unary successor carries no payload — over the symbolic tail the induction
/// hypothesis recurses on; `Stuck` is a scrutinee exposing neither form (a variable,
/// a non-cons symbolic concatenation), where the eliminator rebuilds.
pub enum Layer {
    Empty,
    Cons { head: Option<Term>, tail: Term },
    Stuck(Subterm),
}

/// A native primitive that is the free monoid on a generator set: `Nat` on the unit,
/// `Bin` on its bytes, `Arr` on its elements. Implemented by witness types — the
/// values live as `Subterm`s, so the method is static. `uncons` is the only seam the
/// structural eliminator needs; the recursion scheme around it is carrier-generic
/// and lives in `reduce`.
pub trait FreeMonoid {
    /// Decode one constructor layer off an already-reduced scrutinee.
    fn uncons(scrutinee: Subterm) -> Layer;
}

/// The free monoid on one payload-less generator: the unary naturals. `succ` peels
/// to its predecessor with no head.
pub struct Unary;

/// The free monoid on bytes. Its generator is a byte, reflected back into the
/// eliminator as a `Nat`.
pub struct Bin;

/// The free monoid on its elements. Its generator is the element term itself.
pub struct Arr;

impl FreeMonoid for Unary {
    fn uncons(scrutinee: Subterm) -> Layer {
        match scrutinee {
            // The identity: zero.
            Subterm::Prim(Prim::Nat(Nat::Zero)) => Layer::Empty,
            // Peel one successor off the spine; the predecessor is the tail, and the
            // unary generator carries no head. A spine count `> 1` keeps the rest as
            // a shorter `Succ`, exactly as the old bespoke `Nat` eliminator did.
            Subterm::Prim(Prim::Nat(Nat::Succ(spine, inner))) => {
                let one = BigUint::from(1usize);
                let tail = match spine == one {
                    true => inner,
                    false => Term::prim(Prim::Nat(Nat::Succ(spine - one, inner))),
                };
                Layer::Cons { head: None, tail }
            }
            stuck => Layer::Stuck(stuck),
        }
    }
}

impl FreeMonoid for Bin {
    fn uncons(scrutinee: Subterm) -> Layer {
        let term: Term = scrutinee.into();
        match peel_front(&term) {
            Front::Empty => Layer::Empty,
            // The peeled byte is the eliminator's `Nat`-typed head generator.
            Front::Cons { head, tail } => Layer::Cons { head: Some(head.into_nat()), tail },
            Front::Opaque => Layer::Stuck(Term::unwrap_or_clone(term)),
        }
    }
}

impl FreeMonoid for Arr {
    fn uncons(scrutinee: Subterm) -> Layer {
        match scrutinee {
            // The identity: the empty array.
            Subterm::Prim(Prim::Arr(elems)) if elems.is_empty() => Layer::Empty,
            // A literal run: peel the leading element directly.
            Subterm::Prim(Prim::Arr(mut elems)) => {
                let head = elems.remove(0);
                let tail: Term = Subterm::Prim(Prim::Arr(elems)).into();
                Layer::Cons { head: Some(head), tail }
            }
            // A symbolic cons `cons(x, xs) = concat([x], xs)`: decode `x` off the
            // leading non-empty literal segment; its remaining elements stay ahead
            // of the rest exactly as `ArrConcat` collapses them.
            Subterm::Prim(Prim::ArrConcat(concat_elem, mut segments))
                if segments.first().is_some_and(is_nonempty_arr_literal) =>
            {
                let mut lead = match Term::unwrap_or_clone(segments.remove(0)) {
                    Subterm::Prim(Prim::Arr(elems)) => elems,
                    _ => unreachable!("guard checked a non-empty `Arr` literal lead segment"),
                };
                let head = lead.remove(0);
                if !lead.is_empty() {
                    segments.insert(0, Subterm::Prim(Prim::Arr(lead)).into());
                }
                let tail: Term = match segments.len() {
                    0 => Subterm::Prim(Prim::Arr(vec![])).into(),
                    1 => segments.into_iter().next().unwrap(),
                    _ => Subterm::Prim(Prim::ArrConcat(concat_elem, segments)).into(),
                };
                Layer::Cons { head: Some(head), tail }
            }
            stuck => Layer::Stuck(stuck),
        }
    }
}

/// A leading generator peeled off a `Bin` value: a concrete byte (`Literal`) or the
/// symbolic byte of a `Utf8` cons `append(\\, c)` (`Symbolic`). Kept abstract so
/// each destructor reflects it into the shape its consumer wants — the eliminator
/// into a `Nat`, `Bin/get`/`Bin/slice` into a one-byte `Bin` chunk.
enum Head {
    Literal(u8),
    Symbolic(Term),
}

impl Head {
    /// Reflect into the eliminator's element type: a `Bin` generator IS a `Nat` byte.
    fn into_nat(self) -> Term {
        match self {
            Head::Literal(byte) => Subterm::Prim(Prim::Nat(Nat::new(byte as usize))).into(),
            Head::Symbolic(byte) => byte,
        }
    }

    /// Reflect into a length-1 `Bin` chunk — the cons head `Bin/get`/`Bin/slice`
    /// rebuild as `head ++ tail` (`get(head, 0)` is the byte; a `Utf8` cons head
    /// stays the symbolic `append(\\, c)`).
    fn into_chunk(self) -> Term {
        match self {
            Head::Literal(byte) => Subterm::Prim(Prim::Bin(vec![byte])).into(),
            Head::Symbolic(byte) => {
                Term::prim(Prim::bin_append(Subterm::Prim(Prim::Bin(Vec::new())), byte))
            }
        }
    }
}

/// One step of the `Bin` front decode. `Empty` is the identity (`\\`); `Cons` peels
/// a leading generator and the residual tail; `Opaque` is a value exposing no
/// leading generator (a variable, a slice, a non-`\\`-based append).
enum Front {
    Empty,
    Cons { head: Head, tail: Term },
    Opaque,
}

/// The structural traversal shared by both `Bin` destructors ([`FreeMonoid::uncons`]
/// for the eliminator, [`peel_first_byte`] for `Bin/get`/`Bin/slice`): peel the
/// leading generator off an already-reduced value. A literal run yields its first
/// byte; a `Utf8` cons `append(\\, c)` yields its symbolic byte; a concatenation
/// recurses into its first operand so a literal- or cons-led `BinConcat` decodes
/// too, the residual first-operand tail rejoining the rest — normalised (an empty
/// first-operand tail drops, a lone survivor collapses) so a cons-led concat decodes
/// to the same tail the bare cons would. The empty bytestring is `Empty`; anything
/// else (a variable, a slice, a non-`\\`-based append) is `Opaque`.
fn peel_front(bin: &Term) -> Front {
    match &**bin {
        Subterm::Prim(Prim::Bin(bytes)) => match bytes.split_first() {
            None => Front::Empty,
            Some((&byte, rest)) => Front::Cons {
                head: Head::Literal(byte),
                tail: Subterm::Prim(Prim::Bin(rest.to_vec())).into(),
            },
        },
        // `append(\\, c)`: a single (symbolic) byte — the `Utf8` cons head.
        Subterm::Prim(Prim::BinAppend(base, c)) if is_empty_bin(base) => Front::Cons {
            head: Head::Symbolic(c.clone()),
            tail: Subterm::Prim(Prim::Bin(Vec::new())).into(),
        },
        // A concatenation: peel the leading generator off its first operand; the
        // residual first-operand tail rejoins the rest.
        Subterm::Prim(Prim::BinConcat(operands)) => match operands.split_first() {
            Some((first, rest)) => match peel_front(first) {
                Front::Cons { head, tail: first_tail } => {
                    let mut segments = Vec::with_capacity(operands.len());
                    if !is_empty_bin(&first_tail) {
                        segments.push(first_tail);
                    }
                    segments.extend(rest.iter().cloned());
                    let tail = match segments.len() {
                        0 => Subterm::Prim(Prim::Bin(Vec::new())).into(),
                        1 => segments.into_iter().next().unwrap(),
                        _ => Subterm::Prim(Prim::BinConcat(segments)).into(),
                    };
                    Front::Cons { head, tail }
                }
                // A reduced `BinConcat` has no empty operands, so a first operand
                // that exposes no byte is opaque (a leading variable/slice).
                Front::Empty | Front::Opaque => Front::Opaque,
            },
            None => Front::Empty,
        },
        _ => Front::Opaque,
    }
}

/// Split the first byte off a reduced `Bin` value, returning a length-1 head chunk
/// and the residual tail. Where `peel_bin` (`core::spine`) strips a common prefix of
/// *two* values, this decomposes *one* — the operation-level destructor `Bin/get`
/// and `Bin/slice` walk a codepoint at a time, exposing the cons structure the
/// `Utf8` relation builds (`concat(append(\\, h), t)`) along with literal runs and
/// concatenations. `None` for the empty bytestring or an opaque symbolic value,
/// where no first byte is statically exposed.
pub fn peel_first_byte(bin: &Term) -> Option<(Term, Term)> {
    match peel_front(bin) {
        Front::Cons { head, tail } => Some((head.into_chunk(), tail)),
        Front::Empty | Front::Opaque => None,
    }
}

// `cons` injects a byte at the front as `append(\\, c)` (a one-byte `Bin`);
// `peel_front` recognises that encoding to decode a symbolic cons head.
fn is_empty_bin(term: &Term) -> bool {
    matches!(&**term, Subterm::Prim(Prim::Bin(bytes)) if bytes.is_empty())
}

// `cons` injects an element at the front as the singleton literal `[h]`; the `Arr`
// eliminator recognizes a non-empty literal lead segment to decode a symbolic cons.
fn is_nonempty_arr_literal(term: &Term) -> bool {
    matches!(&**term, Subterm::Prim(Prim::Arr(elems)) if !elems.is_empty())
}

/// The free monoid's normalising *product* — the constructor dual of
/// [`FreeMonoid::uncons`] (the destructor) — shared verbatim by `BinConcat` and
/// `ArrConcat` reduction. Collapse a concatenation's already-reduced `operands` to
/// a normal form under the unit and associativity laws: drop the empty identity
/// (`\\`, `[]`), merge adjacent literal runs into one literal, and collapse a lone
/// surviving operand to itself (an `n`-ary concat of one *is* that one). `literal`
/// borrows an operand's run when it is a literal (`None` for a symbolic chunk);
/// `into_literal`/`into_concat` rebuild the result in the carrier's primitives.
///
/// Window fusion (adjacent `Bin/slice`s of one base) is deliberately NOT done here:
/// that is the spine peel's job when *deciding equality* (`spine::push`); reduction
/// only needs a normal form, and conversion closes any residual gap.
pub fn normalize_concat<E: Clone>(
    operands: Vec<Term>,
    literal: fn(&Term) -> Option<&[E]>,
    into_literal: impl FnOnce(Vec<E>) -> Subterm,
    into_concat: impl FnOnce(Vec<Term>) -> Subterm,
) -> Subterm {
    let mut kept: Vec<Term> = operands
        .into_iter()
        .filter(|operand| !matches!(literal(operand), Some(run) if run.is_empty()))
        .collect();

    // Every surviving operand literal ⇒ one merged literal; the first symbolic chunk
    // stops the fold, leaving the concatenation (a lone operand collapses to itself).
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
