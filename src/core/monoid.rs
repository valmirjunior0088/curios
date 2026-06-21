//! The free-monoid eliminator seam. `Bin` and `Arr` are the native primitives
//! that are free monoids on their generators (bytes, elements); their structural
//! eliminator (`Cases::Inductive`) reduces by a one-step decode — peel the empty
//! identity or a single leading generator off the scrutinee. `uncons` is that
//! decode, one method per carrier; the catamorphism driver in `reduce` consumes
//! the `Layer` it returns and never inspects the carrier's representation again.
//! This is the eliminator-side analogue of `spine::peel_prim`: a new free-monoid
//! carrier is one `uncons` implementation and nothing else.

use super::{Nat, Prim, Subterm, Term};

/// The one-step decode of a free-monoid scrutinee — a carrier's signature functor
/// made concrete. `Empty` is the identity form (`\\`, `[]`); `Cons` is a peeled
/// head generator (already reflected into its element type — a `Nat` byte for
/// `Bin`, the element term itself for `Arr`) over the symbolic tail the induction
/// hypothesis recurses on; `Stuck` is a scrutinee exposing neither form (a
/// variable, a non-cons symbolic concatenation), where the eliminator rebuilds.
pub enum Layer {
    Empty,
    Cons { head: Term, tail: Term },
    Stuck(Subterm),
}

/// A native primitive that is the free monoid on a generator set: `Bin` on its
/// bytes, `Arr` on its elements. Implemented by witness types — the values live
/// as `Subterm`s, so the method is static. `uncons` is the only seam the
/// structural eliminator needs; the recursion scheme around it is carrier-generic
/// and lives in `reduce`.
pub trait FreeMonoid {
    /// Decode one constructor layer off an already-reduced scrutinee.
    fn uncons(scrutinee: Subterm) -> Layer;
}

/// The free monoid on bytes. Its generator is a byte, reflected back into the
/// eliminator as a `Nat`.
pub struct Bin;

/// The free monoid on its elements. Its generator is the element term itself.
pub struct Arr;

impl FreeMonoid for Bin {
    fn uncons(scrutinee: Subterm) -> Layer {
        match scrutinee {
            // The identity: the empty bytestring.
            Subterm::Prim(Prim::Bin(bytes)) if bytes.is_empty() => Layer::Empty,
            // A literal run: peel the leading byte, reflected as a `Nat` generator.
            Subterm::Prim(Prim::Bin(mut bytes)) => {
                let head_byte = bytes.remove(0);
                let head: Term = Subterm::Prim(Prim::Nat(Nat::new(head_byte as usize))).into();
                let tail: Term = Subterm::Prim(Prim::Bin(bytes)).into();
                Layer::Cons { head, tail }
            }
            // A symbolic cons `cons(c, \\) = append(\\, c)`: the byte stays symbolic.
            Subterm::Prim(Prim::BinAppend(base, c)) if is_empty_bin(&base) => {
                let tail: Term = Subterm::Prim(Prim::Bin(vec![])).into();
                Layer::Cons { head: c, tail }
            }
            // A symbolic cons `cons(c, t) = concat(append(\\, c), t)`: decode `c`
            // off the leading single-byte append; the rest rides on the tail.
            Subterm::Prim(Prim::BinConcat(mut segments))
                if segments.first().is_some_and(is_single_byte_append) =>
            {
                let c = match Term::unwrap_or_clone(segments.remove(0)) {
                    Subterm::Prim(Prim::BinAppend(_, c)) => c,
                    _ => unreachable!("guard checked a leading single-byte append"),
                };
                let tail: Term = match segments.len() {
                    0 => Subterm::Prim(Prim::Bin(vec![])).into(),
                    1 => segments.into_iter().next().unwrap(),
                    _ => Subterm::Prim(Prim::BinConcat(segments)).into(),
                };
                Layer::Cons { head: c, tail }
            }
            stuck => Layer::Stuck(stuck),
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
                Layer::Cons { head, tail }
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
                Layer::Cons { head, tail }
            }
            stuck => Layer::Stuck(stuck),
        }
    }
}

// `cons` injects a byte at the front as `append(\\, c)` (a one-byte `Bin`); these
// recognize that encoding so the `Bin` eliminator can decode a symbolic cons.
fn is_empty_bin(term: &Term) -> bool {
    matches!(&**term, Subterm::Prim(Prim::Bin(bytes)) if bytes.is_empty())
}

fn is_single_byte_append(term: &Term) -> bool {
    matches!(&**term, Subterm::Prim(Prim::BinAppend(base, _)) if is_empty_bin(base))
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
