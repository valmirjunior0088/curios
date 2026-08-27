//! The free-monoid destructors. `Nat`, `Bin`, and `List` are the native intrinsics that are free monoids on their generators (the unary unit, bytes, elements). Their structural eliminator (`Cases::FreeMonoid`) reduces by a one-step decode — peel the empty identity or a single leading generator off the scrutinee — and that decode is `uncons`, one match arm per carrier; the catamorphism driver in `reduce` consumes the `Layer` it returns and never inspects the carrier's representation again. The eliminator-side analogue of `spine::peel_intrinsic`: a new free-monoid carrier is one `FreeMonoid` variant and its `uncons` arm.
//!
//! For `Bin` and `List`, the same front decode also feeds the operation level: `Bin/get`/`Bin/slice` and `List/get`/`List/slice` peel one generator at a time via `peel_first_atom`/`peel_first_elem`. Each carrier's two destructors share one structural traversal (`peel_front`, `peel_front_list`); `Bin` reflects the peeled head two ways — a `Nat` byte for the eliminator, a one-byte `Bin` chunk for the operations — while `List`'s head is the element term for both.
//!
//! Beside the destructors, this module owns the free monoid's *product* and its *measure*, because both are walks over the same spine. [`normalize_concat`] is the product: it collapses a concatenation under the unit and associativity laws and fuses literal operands up to [`FUSION_CAP`], past which a value keeps its `Concat` node rather than being recopied into a longer run. The measure is how many generators a value carries, folded off the spine, and it is read two ways: `Bin/get` and `Bin/slice` locate an index or a window through [`bin_segments`], which yields the operands they will read *into*, while `Bin/len` goes through [`bin_measure`], which answers for strictly more values because a length can be known where a generator cannot. Neither the measure nor the walks take a [`crate::Reducer`]: measuring or peeling cannot re-enter reduction, and that is enforced by the signatures rather than asserted in a comment.

use {
    super::{Intrinsic, Nat, Subterm, Term},
    curios_num::Natural,
    curios_utilities::{Grain, PackedBin},
};

/// The one-step decode of a free-monoid scrutinee — a carrier's signature functor made concrete. `Empty` is the identity form (`x[]`, `[]`, `0`); `Cons` peels a generator: its `head` is the payload reflected into the element type — a `Nat` byte for `Bin`, the element term itself for `List`, and `None` for `Nat`, whose unary successor carries no payload — over the symbolic tail the induction hypothesis recurses on; `Stuck` is a scrutinee exposing neither form (a variable, a non-cons symbolic concatenation), where the eliminator rebuilds.
pub enum Layer {
    Empty,
    Cons { head: Option<Term>, tail: Term },
    Stuck(Subterm),
}

/// A native intrinsic that is the free monoid on a generator set, named by its generator: `Unary` is `Nat` on the unit, `Bin` is `Bin` on its bytes, `List` is `List` on its elements. A closed set — the value-level twin of the `Carrier` the eliminator already carries (`reduce` maps one to the other). `uncons` is the only seam the structural eliminator needs; the recursion scheme around it is carrier-generic and lives in `reduce`.
#[derive(Clone, Copy)]
pub enum FreeMonoid {
    /// The free monoid on one payload-less generator: the unary naturals. `succ` peels to its predecessor with no head.
    Unary,
    /// The free monoid on bytes. Its generator is a byte, reflected back into the eliminator as a `Nat`.
    Bin(Grain),
    /// The free monoid on its elements. Its generator is the element term itself.
    List,
}

impl FreeMonoid {
    /// Decode one constructor layer off an already-reduced scrutinee — the values live as `Subterm`s, so the carrier is the only thing dispatched on.
    pub fn uncons(self, scrutinee: Subterm) -> Layer {
        match self {
            FreeMonoid::Unary => Self::uncons_unary(scrutinee),
            FreeMonoid::Bin(grain) => Self::uncons_bin(grain, scrutinee),
            FreeMonoid::List => Self::uncons_list(scrutinee),
        }
    }

    fn uncons_unary(scrutinee: Subterm) -> Layer {
        match scrutinee {
            // The identity: zero.
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Zero)) => Layer::Empty,
            // Peel one successor off the spine; the predecessor is the tail, and the unary generator carries no head. A spine count `> 1` keeps the rest as a shorter `Succ`, exactly as the old bespoke `Nat` eliminator did.
            Subterm::Intrinsic(Intrinsic::Nat(Nat::Succ(spine, inner))) => {
                let one = Natural::from(1usize);

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

    fn uncons_list(scrutinee: Subterm) -> Layer {
        let term = scrutinee.into();

        match peel_front_list(&term) {
            ListFront::Empty => Layer::Empty,
            // The peeled element is the eliminator's head generator directly.
            ListFront::Cons { head, tail } => Layer::Cons {
                head: Some(head),
                tail,
            },
            ListFront::Opaque => Layer::Stuck(Term::unwrap_or_clone(term)),
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

/// One pending level of a leading-edge descent — what that level has to put back once the generator underneath it has been peeled. `Appended` is a `BinAppend`'s trailing atom; `Concat` is the operands a `BinConcat` carried after its first.
///
/// **The descent is explicit rather than recursive because this depth is *data*-shaped.** `FUSION_CAP` stops an accumulation fusing, so what it builds is a concatenation nested as deeply as the loop is long, and the leading edge of that is what these walks descend. [`curios_utilities::recurse`] is taken at the *entry point* of each checker's reduction and checked between its frames, never per level of a helper it calls, so a hundred thousand native frames here would spend one granted segment without ever reaching a check. `curios-cont`'s `$<carrier>/force` walks the same shape on the same kind of explicit worklist, for the same reason — and `documentation/design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md` does not reach here: what that entry defends is the two reduction and conversion strategies written deliberately *twice*, so that a person can diff them, and these are single-implementation helpers both of them share.
enum BinLevel<'a> {
    Appended(&'a Term),
    Concat(&'a [Term]),
}

/// The structural traversal shared by both `Bin` destructors ([`FreeMonoid::uncons`] for the eliminator, [`peel_first_atom`] for `Bin/get`/`Bin/slice`): peel the leading generator off an already-reduced value. A literal run yields its first byte; a `Utf8` cons `append(x[], c)` yields its symbolic byte; a concatenation descends into its first operand so a literal- or cons-led `BinConcat` decodes too, the residual first-operand tail rejoining the rest — normalised (an empty first-operand tail drops, a lone survivor collapses) so a cons-led concat decodes to the same tail the bare cons would. The empty bytestring is `Empty`; anything else (a variable, a slice, a non-`x[]`-based append) is `Opaque`.
///
/// Two phases over a [`BinLevel`] stack: descend the leading edge to the value that actually carries the first generator, then rebuild outward, each level rejoining what it was holding. `Opaque` propagates through both level kinds, and a concatenation whose first operand exposes no generator is itself opaque — a reduced `BinConcat` has no empty operands, so that case is a leading variable or slice.
fn peel_front(grain: Grain, bin: &Term) -> Front {
    let mut levels: Vec<BinLevel<'_>> = Vec::new();
    let mut current = bin;

    let mut front = loop {
        match &**current {
            Subterm::Intrinsic(Intrinsic::Bin(found, bytes)) if *found == grain => {
                let tail = || {
                    Subterm::Intrinsic(Intrinsic::Bin(
                        grain,
                        bytes.slice(grain, 1, bytes.len(grain)).unwrap(),
                    ))
                    .into()
                };

                break match grain {
                    Grain::B => match bytes.bit(0) {
                        None => Front::Empty,
                        Some(bit) => Front::Cons {
                            head: Head::LiteralBit(bit),
                            tail: tail(),
                        },
                    },
                    Grain::X => match bytes.byte(0) {
                        None => Front::Empty,
                        Some(byte) => Front::Cons {
                            head: Head::LiteralByte(byte),
                            tail: tail(),
                        },
                    },
                };
            }
            // `append(base, atom) = base ++ [atom]`: peel the base's leading generator, and the appended atom rejoins the residual. An empty base is the canonical one-atom chunk, which is where the symbolic head comes from; any other base decodes as far as it can, so a chained `append(append(x[], a), b)` and a run-based `append(x[0x48], b)` both expose their first generator instead of going opaque. `core::spine`'s two-value peel has always decoded an append this way, so without this level the same term was transparent to conversion and opaque to reduction.
            Subterm::Intrinsic(Intrinsic::BinAppend {
                grain: found,
                bin: base,
                element: atom,
            }) if *found == grain => {
                levels.push(BinLevel::Appended(atom));
                current = base;
            }
            Subterm::Intrinsic(Intrinsic::BinConcat {
                grain: found,
                operands,
            }) if *found == grain => match operands.split_first() {
                Some((first, rest)) => {
                    levels.push(BinLevel::Concat(rest));
                    current = first;
                }
                None => break Front::Empty,
            },
            _ => break Front::Opaque,
        }
    };

    while let Some(level) = levels.pop() {
        front = match (level, front) {
            (BinLevel::Appended(atom), Front::Empty) => Front::Cons {
                head: Head::Symbolic(atom.clone()),
                tail: Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())).into(),
            },
            (BinLevel::Appended(atom), Front::Cons { head, tail }) => Front::Cons {
                head,
                tail: Subterm::Intrinsic(Intrinsic::BinAppend {
                    grain,
                    bin: tail,
                    element: atom.clone(),
                })
                .into(),
            },
            (
                BinLevel::Concat(rest),
                Front::Cons {
                    head,
                    tail: first_tail,
                },
            ) => {
                let mut segments = Vec::with_capacity(rest.len() + 1);
                if !is_empty_bin(grain, &first_tail) {
                    segments.push(first_tail);
                }
                segments.extend(rest.iter().cloned());
                let tail = match segments.len() {
                    0 => Subterm::Intrinsic(Intrinsic::Bin(grain, PackedBin::empty())).into(),
                    1 => segments.into_iter().next().unwrap(),
                    _ => Subterm::Intrinsic(Intrinsic::BinConcat {
                        grain,
                        operands: segments,
                    })
                    .into(),
                };
                Front::Cons { head, tail }
            }
            (BinLevel::Appended(_), Front::Opaque)
            | (BinLevel::Concat(_), Front::Empty | Front::Opaque) => Front::Opaque,
        };
    }

    front
}

/// Split the first byte off a reduced `Bin` value, returning a length-1 head chunk and the residual tail. Where `peel_bin` (`core::spine`) strips a common prefix of *two* values, this decomposes *one* — the operation-level destructors `Bin/get` and `Bin/slice` walk an atom at a time, exposing the cons structure the `Utf8` relation builds (`concat(append(x[], h), t)`) along with literal runs and concatenations. `None` for the empty bytestring or an opaque symbolic value, where no first byte is statically exposed.
pub(crate) fn peel_first_atom(grain: Grain, bin: &Term) -> Option<(Term, Term)> {
    match peel_front(grain, bin) {
        Front::Cons { head, tail } => Some((head.into_chunk(grain), tail)),
        Front::Empty | Front::Opaque => None,
    }
}

/// One step of the `List` front decode — the element-typed analogue of [`Front`]. `Empty` is the identity (`[]`); `Cons` peels the leading element and the residual tail; `Opaque` is a value exposing no leading element (a variable, a slice). Unlike `Bin`, the head needs no reflection — a `List` generator IS its element term, used directly by both the eliminator and the operations.
enum ListFront {
    Empty,
    Cons { head: Term, tail: Term },
    Opaque,
}

/// [`BinLevel`] over the element carrier: `Appended` is a `ListAppend`'s trailing element, `Concat` the segments a `ListConcat` carried after its first. Each level also carries the element type, since every `List`-valued producer holds one and the residual has to be rebuilt with it.
enum ListLevel<'a> {
    Appended { elem: &'a Term, appended: &'a Term },
    Concat { elem: &'a Term, rest: &'a [Term] },
}

/// The structural traversal shared by both `List` destructors ([`FreeMonoid::uncons`] for the eliminator, [`peel_first_elem`] for `List/get`/`List/slice`) — the element-typed twin of [`peel_front`], iterative for the same reason and in the same two phases. Peel the leading element off an already-reduced value: a literal run yields its first element; `append(base, e)` decodes through its base, the appended element rejoining the residual; a concatenation descends into its first segment — so the symbolic cons `concat([h], t)`, a cons-led concat, and an append over a decodable base all expose their first element, the residual renormalised (an empty first-segment tail drops, a lone survivor collapses) so each decodes to the same tail its flattened form would. The empty list is `Empty`; anything else (a variable, a slice) is `Opaque`.
fn peel_front_list(list: &Term) -> ListFront {
    let mut levels: Vec<ListLevel<'_>> = Vec::new();
    let mut current = list;

    let mut front = loop {
        match &**current {
            Subterm::Intrinsic(Intrinsic::List {
                element: elem,
                items: elems,
            }) => {
                break match elems.split_first() {
                    None => ListFront::Empty,
                    Some((head, rest)) => ListFront::Cons {
                        head: head.clone(),
                        tail: Subterm::Intrinsic(Intrinsic::List {
                            element: elem.clone(),
                            items: rest.to_vec(),
                        })
                        .into(),
                    },
                };
            }
            // `append(base, e) = base ++ [e]`: peel the base's leading element, and the appended element rejoins the residual. An empty base's front is the appended element itself; a decodable base exposes its own head instead. The element-typed twin of the `BinAppend` level above, closing the same gap: `core::spine`'s two-value peel decodes an append as `concat(base, [e])`, so without this level the same term was transparent to conversion and opaque to reduction.
            Subterm::Intrinsic(Intrinsic::ListAppend {
                element: elem,
                list: base,
                item: appended,
            }) => {
                levels.push(ListLevel::Appended { elem, appended });
                current = base;
            }
            Subterm::Intrinsic(Intrinsic::ListConcat {
                element: elem,
                operands: segments,
            }) => match segments.split_first() {
                Some((first, rest)) => {
                    levels.push(ListLevel::Concat { elem, rest });
                    current = first;
                }
                None => break ListFront::Empty,
            },
            _ => break ListFront::Opaque,
        }
    };

    while let Some(level) = levels.pop() {
        front = match (level, front) {
            (ListLevel::Appended { elem, appended }, ListFront::Empty) => ListFront::Cons {
                head: appended.clone(),
                tail: Subterm::Intrinsic(Intrinsic::List {
                    element: elem.clone(),
                    items: vec![],
                })
                .into(),
            },
            (ListLevel::Appended { elem, appended }, ListFront::Cons { head, tail }) => {
                ListFront::Cons {
                    head,
                    tail: Subterm::Intrinsic(Intrinsic::ListAppend {
                        element: elem.clone(),
                        list: tail,
                        item: appended.clone(),
                    })
                    .into(),
                }
            }
            (
                ListLevel::Concat { elem, rest },
                ListFront::Cons {
                    head,
                    tail: first_tail,
                },
            ) => {
                let mut kept = Vec::with_capacity(rest.len() + 1);
                if !is_empty_list(&first_tail) {
                    kept.push(first_tail);
                }
                kept.extend(rest.iter().cloned());
                let tail = match kept.len() {
                    0 => Subterm::Intrinsic(Intrinsic::List {
                        element: elem.clone(),
                        items: vec![],
                    })
                    .into(),
                    1 => kept.into_iter().next().unwrap(),
                    _ => Subterm::Intrinsic(Intrinsic::ListConcat {
                        element: elem.clone(),
                        operands: kept,
                    })
                    .into(),
                };
                ListFront::Cons { head, tail }
            }
            // A reduced `ListConcat` has no empty segments, so a first segment that exposes no element is opaque (a leading variable/slice) — and an `Opaque` from any level below stays opaque all the way out.
            (ListLevel::Appended { .. }, ListFront::Opaque)
            | (ListLevel::Concat { .. }, ListFront::Empty | ListFront::Opaque) => ListFront::Opaque,
        };
    }

    front
}

/// Split the first element off a reduced `List` value, returning the head element and the residual tail — the element-typed twin of [`peel_first_atom`]. Lets `List/get` and `List/slice` peel a symbolic cons one element at a time, exactly as `Bin/get`/`Bin/slice` walk an atom at a time. `None` for the empty array or an opaque symbolic value, where no first element is statically exposed.
pub(crate) fn peel_first_elem(list: &Term) -> Option<(Term, Term)> {
    match peel_front_list(list) {
        ListFront::Cons { head, tail } => Some((head, tail)),
        ListFront::Empty | ListFront::Opaque => None,
    }
}

// `cons` injects a byte at the front as `append(x[], c)` (a one-byte `Bin`); `peel_front` recognises that encoding to decode a symbolic cons head.
fn is_empty_bin(grain: Grain, term: &Term) -> bool {
    matches!(&**term, Subterm::Intrinsic(Intrinsic::Bin(found, bytes)) if *found == grain && bytes.is_empty())
}

// `cons` injects an element at the front as the singleton literal `[h]`; `peel_front_list`'s concat recursion decodes that encoding through the literal case.
fn is_empty_list(term: &Term) -> bool {
    matches!(&**term, Subterm::Intrinsic(Intrinsic::List { element: _, items: elems }) if elems.is_empty())
}

/// The operands of an already-reduced value, left to right, each with the number of generators it carries — `None` where any operand's length is not statically known.
///
/// **The free monoid's measure, and the one thing `len`, `get` and `slice` all actually want.** A length is a homomorphism into `(ℕ, +, 0)`, and over a value already in normal form it is a *fold over the spine*: no reduction, no rebuilding, no budget. That is what this is, and it replaces computing one by building `Bin/len(operand)` terms and handing them back to the reducer — which costs a full re-walk of each sub-spine, so a spine of depth n costs Σk = O(n²) where the answer is one linear pass.
///
/// **It takes no [`crate::Reducer`], and that is the enforcement rather than a comment.** A function that cannot reach the reducer cannot re-enter reduction, cannot spend budget, and cannot rebuild a term to ask about it. The audit is the signature.
///
/// **The notion is not new here — conversion has had it all along.** `crate::spine`'s `Atom::Window` is documented as a chunk whose contents are symbolic but whose length it carries outright; deciding equality has always been able to measure what it cannot read. Only reduction lacked the same view.
///
/// **Deliberately narrow: literal runs and their concatenations, nothing else.** An append, a window, a variable — anything whose length this cannot read off — answers `None`, and every caller then falls back to exactly the rule it uses today. That keeps this from asserting a single equation that is not already decided: measuring a window by the count it carries would be sound on `/sys`'s `s + l <= len(b)` precondition, but nothing decides `len(slice(b, s, l)) = l` today — conversion holds a window's very emptiness undecidable (`against_identity` answers `Stuck`) — so admitting it would be a *new* definitional equation on the perimeter's weakest row. It buys nothing here, since an accumulation's spine is literals, and it can be taken later on its own evidence.
fn bin_segments(grain: Grain, value: &Term) -> Option<Vec<(&Term, usize)>> {
    let mut segments = Vec::new();
    let mut pending = vec![value];

    while let Some(term) = pending.pop() {
        match &**term {
            Subterm::Intrinsic(Intrinsic::Bin(found, run)) if *found == grain => {
                segments.push((term, run.len(grain)));
            }
            // Pushed in reverse so they come back off left to right: a segment list is ordered, unlike the sum taken over it.
            Subterm::Intrinsic(Intrinsic::BinConcat {
                grain: found,
                operands,
            }) if *found == grain => {
                pending.extend(operands.iter().rev());
            }
            _ => return None,
        }
    }

    Some(segments)
}

/// [`bin_segments`] over the element carrier.
fn list_segments(value: &Term) -> Option<Vec<(&Term, usize)>> {
    let mut segments = Vec::new();
    let mut pending = vec![value];

    while let Some(term) = pending.pop() {
        match &**term {
            Subterm::Intrinsic(Intrinsic::List {
                element: _,
                items: elems,
            }) => segments.push((term, elems.len())),
            Subterm::Intrinsic(Intrinsic::ListConcat {
                element: _,
                operands,
            }) => {
                pending.extend(operands.iter().rev());
            }
            _ => return None,
        }
    }

    Some(segments)
}

/// How many generators an already-reduced `Bin` value carries. `None` where the value is not wholly measurable, or where the total does not fit a `usize`.
///
/// **Deliberately a superset of [`bin_segments`] rather than a fold over it.** A *segment* is something `Bin/get` and `Bin/slice` may read *into*; this walk additionally credits an operation whose result arity is fixed by the operation rather than by any operand. Today that is `Flt/to_le_bytes` alone, which writes four bytes for every float, NaN and both zeros included.
///
/// Crediting it as a segment instead would be wrong twice over: `bin_locate` would hand the node back to `Bin/get` as the operand holding the index, which re-enters this same node and never terminates, and `bin_window` would try to narrow a value it cannot read.
///
/// The length is the one thing about such a node knowable without observing the float, and that is what this reads. `Flt` folds through the binary32 model now, so a *literal* operand is answered by `reduce::intrinsic` before it ever reaches here; what survives is the case this was written for — a **symbolic** operand, where `Bin/len(Flt/to_le_bytes(x))` is still `4` because it is the arity of the result rather than anything about the float. Without it `Flt/of_le_bytes`'s length precondition is undischargeable over the very operation it inverts, so the pair's round trip could not be written at all.
pub(crate) fn bin_measure(grain: Grain, value: &Term) -> Option<usize> {
    let mut total = 0usize;
    let mut pending = vec![value];

    while let Some(term) = pending.pop() {
        let length = match &**term {
            Subterm::Intrinsic(Intrinsic::Bin(found, run)) if *found == grain => run.len(grain),
            Subterm::Intrinsic(Intrinsic::BinConcat {
                grain: found,
                operands,
            }) if *found == grain => {
                // Order is irrelevant to a sum, so this walk does not reverse the way `bin_segments` must.
                pending.extend(operands.iter());
                continue;
            }
            Subterm::Intrinsic(Intrinsic::FltToLeBytes(_)) if grain == Grain::X => 4,
            _ => return None,
        };
        total = total.checked_add(length)?;
    }

    Some(total)
}

/// [`bin_measure`] over the element carrier.
pub(crate) fn list_measure(value: &Term) -> Option<usize> {
    measure(&list_segments(value)?)
}

fn measure(segments: &[(&Term, usize)]) -> Option<usize> {
    segments
        .iter()
        .try_fold(0usize, |total, (_, length)| total.checked_add(*length))
}

/// Where an index lands: the operand holding it, and the index *within* that operand. `None` when the value is not wholly measurable, or when the index is past its end — which the caller reports as the out-of-bounds it is.
pub(crate) fn bin_locate(grain: Grain, value: &Term, index: usize) -> Option<Located<'_>> {
    locate(bin_segments(grain, value)?, index)
}

/// [`bin_locate`] over the element carrier.
pub(crate) fn list_locate(value: &Term, index: usize) -> Option<Located<'_>> {
    locate(list_segments(value)?, index)
}

/// The outcome of locating an index in a measured value.
pub(crate) enum Located<'a> {
    /// The operand holding the index, and the index within it.
    At(&'a Term, usize),
    /// The index is past the value's end, which measured this many generators.
    Past(usize),
}

fn locate(segments: Vec<(&Term, usize)>, index: usize) -> Option<Located<'_>> {
    let mut offset = 0usize;

    for (operand, length) in segments {
        match index.checked_sub(offset) {
            Some(local) if local < length => return Some(Located::At(operand, local)),
            _ => offset = offset.checked_add(length)?,
        }
    }

    Some(Located::Past(offset))
}

/// The operands a `count`-long window at `start` spans, each already narrowed to its overlap — the pieces whose concatenation *is* the window. `None` when the value is not wholly measurable; `Err` carries the measured total when the window runs past the end, which the caller reports.
///
/// The point of returning pieces rather than a value: an operand the window covers whole is handed back untouched and shares its payload, and only the two at the edges are narrowed. A window over a spine therefore costs one pass and two slices, where peeling one generator at a time costs a walk of the whole spine per generator read.
pub(crate) fn bin_window(
    grain: Grain,
    value: &Term,
    start: usize,
    count: usize,
) -> Option<Result<Vec<Piece<'_>>, usize>> {
    window(bin_segments(grain, value)?, start, count)
}

/// [`bin_window`] over the element carrier.
pub(crate) fn list_window(
    value: &Term,
    start: usize,
    count: usize,
) -> Option<Result<Vec<Piece<'_>>, usize>> {
    window(list_segments(value)?, start, count)
}

/// One operand a window spans: whole, or narrowed to the half-open range within it.
pub(crate) enum Piece<'a> {
    Whole(&'a Term),
    Part(&'a Term, usize, usize),
}

fn window(
    segments: Vec<(&Term, usize)>,
    start: usize,
    count: usize,
) -> Option<Result<Vec<Piece<'_>>, usize>> {
    let total = measure(&segments)?;

    // No reversed-range case to reject: a window is a start and a *count*, so `start > end` is not a shape this can be handed. Only running past the end remains, and an end that overflows `usize` has certainly done so.
    let Some(end) = start.checked_add(count).filter(|end| *end <= total) else {
        return Some(Err(total));
    };

    let mut pieces = Vec::new();
    let mut offset = 0usize;

    for (operand, length) in segments {
        let (from, to) = (offset, offset + length);
        offset = to;

        // Half-open overlap: an operand entirely before the window or entirely after it contributes nothing, and an empty overlap is not a piece.
        let (lo, hi) = (start.max(from), end.min(to));
        if lo >= hi {
            continue;
        }

        pieces.push(match (lo - from, hi - from) {
            (0, upper) if upper == length => Piece::Whole(operand),
            (lower, upper) => Piece::Part(operand, lower, upper),
        });
    }

    Some(Ok(pieces))
}

/// The largest literal operand reduction will fuse, in the carrier's own generators — bits at [`Grain::B`], bytes at [`Grain::X`], elements for `List`.
///
/// **Fusion is an optimization for small runs, not a normalization obligation.** [`normalize_concat`] copies every operand it fuses, so an accumulation that appends to its own result recopies everything accumulated so far on every step: a linear number of transitions performing a quadratic volume of construction. Declining to fuse past this size leaves the `Concat` node standing instead, and each step is then one node over operands it shares rather than copies. `curios`'s `tests::reduction` measures both halves; `tests::runtime`'s `accumulation_loops_are_linear_by_construction` is the same loop at runtime, where a rope has always made it linear.
///
/// **The cap is by *operand*, and that is the load-bearing half.** Under a cap on the *result* an accumulator is re-copied within every chunk, so the cost stays quadratic in the chunk size and is merely divided by the number of chunks. Under an operand cap the accumulator stops being fusible after a handful of steps whatever its chunk size, every later step is one node, and the leaves of a loop appending the same run share one payload.
///
/// **Chosen from a census, not picked.** Instrumenting the two `merge` closures over a fixed-prelude build (elaboration *and* kernel certification of every `/std` and `/syn` module) and over `curios`'s 557-test corpus, taken **2026-08-14**:
///
/// | Corpus | Fusing concatenations | Largest operand |
/// | --- | --- | --- |
/// | Fixed prelude | 207, all bit-grain | 1 generator |
/// | Cross-stage corpus | 210 — 207 bit-grain, 3 `List`, **0 byte-grain** | 3 generators |
/// | The accumulation reproducer alone, at n = 800 | 1600 | 7990 generators |
///
/// So the corpus fuses almost nothing, and what it does fuse is one to three generators; the pathological case is unbounded. Any cap of four or more leaves every normal form the corpus reaches untouched, and this one clears the observed maximum by more than an order of magnitude while stopping the accumulation within a handful of steps. The third row is the whole defect in one line: operands of 10, 20, 30 … 7990, the accumulator recopied every step.
///
/// **What declining to fuse costs is completeness, not soundness.** It removes a normalization step rather than adding an equation, so the risk is a proof that used to close by literal equality failing to close through the peel. It does not: `bin_atoms`/`list_atoms` flatten a concatenation into segments and `push` merges adjacent literal runs (`crate::spine`), so a capped spelling and the literal it would have fused to decompose identically. `curios-core`'s `spine` tests state that per grain and per carrier, `curios`'s `tests::aggregates::a_literal_run_is_the_same_value_however_it_is_grouped` states it where both checkers see it, and a workspace build — which elaborates, erases and certifies the whole standard library — is the detector for anything they miss.
pub(crate) const FUSION_CAP: usize = 64;

/// The literal run a normalized concatenation inspects — each carrier's own representation of a generator sequence (`PackedBin` for both `Bin` grains, the element vector for `List`), exposing only the emptiness [`normalize_concat`] drops.
pub(crate) trait Run {
    fn is_empty(&self) -> bool;
}

impl Run for PackedBin {
    fn is_empty(&self) -> bool {
        PackedBin::is_empty(self)
    }
}

impl Run for Vec<Term> {
    fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }
}

/// The free monoid's normalising *product* — the constructor dual of [`FreeMonoid::uncons`] (the destructor) — shared by `BinConcat` (both grains) and `ListConcat` reduction. Collapse a concatenation's already-reduced `operands` to a normal form under the unit and associativity laws: drop the empty identity (`x[]`, `b[]`, `[]`), fuse an all-*fusible*-literal survivor set into one literal, and collapse a lone surviving operand to itself (an `n`-ary concat of one *is* that one). `literal` lends an operand's run when it is a literal this may fuse (`None` for a symbolic chunk *or* for a literal past [`FUSION_CAP`], which is how the cap is applied without this function knowing the carrier's size unit); `merge` fuses the runs in the carrier's own representation — `PackedBin::concat`'s bulk copy, `List`'s flatten — and must fuse zero runs to the empty literal; `into_concat` rebuilds the surviving mixed operands.
///
/// An over-cap operand therefore reads exactly like a symbolic one here: not droppable as empty, not fusible, and left standing by `into_concat`. That is the whole of the cap's implementation, and it is why the emptiness filter below is unaffected — an over-cap run is never empty.
///
/// **`merge` is fallible and the other two are not**, and the asymmetry is the price list's rather than this function's: fusing is the only branch that *constructs* anything, so it is the only one that has to charge before it does. Dropping an empty operand and rebuilding the survivors both hand back storage that already exists. That makes this function fallible for its caller, which is the shape the audit says to expect at every fusing seam rather than only at this one.
///
/// Window fusion (adjacent `Bin/slice`s of one base) is deliberately NOT done here: that is the spine peel's job when *deciding equality* (`spine::push`); reduction only needs a normal form, and conversion closes any residual gap.
pub(crate) fn normalize_concat<C: Run, E>(
    operands: Vec<Term>,
    literal: impl Fn(&Term) -> Option<&C>,
    merge: impl FnOnce(Vec<&C>) -> Result<Subterm, E>,
    into_concat: impl FnOnce(Vec<Term>) -> Subterm,
) -> Result<Subterm, E> {
    let mut kept: Vec<Term> = operands
        .into_iter()
        .filter(|operand| !matches!(literal(operand), Some(run) if run.is_empty()))
        .collect();

    // Every surviving operand literal ⇒ the runs fuse into one; the first symbolic chunk stops the collection, leaving the concatenation (a lone operand collapses to itself).
    match kept.iter().map(&literal).collect::<Option<Vec<&C>>>() {
        Some(runs) => merge(runs),
        None if kept.len() == 1 => Ok(Term::unwrap_or_clone(kept.pop().unwrap())),
        None => Ok(into_concat(kept)),
    }
}

#[cfg(test)]
mod tests;
