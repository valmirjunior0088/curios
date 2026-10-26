//! The free-monoid destructors. `Nat`, `Bytes`, and `List` are the native intrinsics that are free monoids on their generators (the unary unit, bytes, elements). Their structural eliminator (`Cases::FreeMonoid`) reduces by a one-step decode — peel the empty identity or a single leading generator off the scrutinee — and that decode is `uncons`, one match arm per carrier; the catamorphism driver in `reduce` consumes the `Layer` it returns and never inspects the carrier's representation again. The eliminator-side analogue of `spine::peel_intrinsic`: a new free-monoid carrier is one `FreeMonoid` variant and its `uncons` arm.
//!
//! For `Bytes` and `List`, the same front decode also feeds the operation level: `Bytes/get`/`Bytes/slice` and `List/get`/`List/slice` peel one generator at a time via `peel_first_atom`/`peel_first_elem`. Each carrier's two destructors share one structural traversal (`peel_front`, `peel_front_list`); `Bytes` reflects the peeled head two ways — a `Byte` for the eliminator, a one-byte `Bytes` chunk for the operations — while `List`'s head is the element term for both.
//!
//! Beside the destructors, this module owns the free monoid's *product* and its *measure*, because both are walks over the same spine. [`normalize_concat`] is the product: it collapses a concatenation under the unit and associativity laws and fuses literal operands up to [`FUSION_CAP`], past which a value keeps its `Concat` node rather than being recopied into a longer run. The measure is how many generators a value carries, folded off the spine, and it is read two ways: `get` and `slice` locate a window through [`FreeMonoid::segments`], which yields the operands they will read *into*, while `len` goes through `bytes_measure`, which answers for strictly more values because a length can be known where a generator cannot. Neither takes a [`crate::Reducer`]: measuring or peeling cannot re-enter reduction, and that is enforced by the signatures rather than asserted in a comment. The two-strategy entry that *does* take one — `reduce::intrinsic`'s `FreeMonoid::window` — reaches its symbolic half only once these have declined, so a measurable value still costs no budget.
//!
//! **What belongs on the carrier and what does not.** A reader folds into [`FreeMonoid`] exactly when it needs nothing the carrier does not already hold. The spine walk, the joined walk and the one-generator read all qualify, and are methods here. The two that do not are `reduce::intrinsic`'s `bytes_shape`/`list_shape`, which answer different types and differ in fallibility, and its `bytes_piece`/`list_piece`, which must restate a `List`'s element type — a term the `List` variant does not carry. The criterion settles the question in advance rather than per pair.

use {
    super::{Intrinsic, Nat, Subterm, Term},
    curios_algebra::{Concatenated, Joining, Span, join, locate, total},
    curios_num::Binary,
};

/// The one-step decode of a free-monoid scrutinee — a carrier's signature functor made concrete. `Empty` is the identity form (`x[]`, `[]`, `0`); `Cons` peels a generator: its `head` is the payload reflected into the element type — a `Byte` for `Bytes`, the element term itself for `List`, and `None` for `Nat`, whose unary successor carries no payload — over the symbolic tail the induction hypothesis recurses on; `Stuck` is a scrutinee exposing neither form (a variable, a non-cons symbolic concatenation), where the eliminator rebuilds.
pub enum Layer {
    Empty,
    Cons { head: Option<Term>, tail: Term },
    Stuck(Subterm),
}

/// A native intrinsic that is the free monoid on a generator set, named by its generator: `Unary` is `Nat` on the unit, `Bytes` is `Bytes` on its bytes, `List` is `List` on its elements. A closed set — the value-level twin of the `Carrier` the eliminator already carries (`reduce` maps one to the other). `uncons` is the only seam the structural eliminator needs; the recursion scheme around it is carrier-generic and lives in `reduce`.
#[derive(Clone, Copy)]
pub enum FreeMonoid {
    /// The free monoid on one payload-less generator: the unary naturals. `succ` peels to its predecessor with no head.
    Unary,
    /// The free monoid on bytes. Its generator is a byte, reflected back into the eliminator as a `Byte`.
    Bytes,
    /// The free monoid on its elements. Its generator is the element term itself.
    List,
}

impl FreeMonoid {
    /// Decode one constructor layer off an already-reduced scrutinee — the values live as `Subterm`s, so the carrier is the only thing dispatched on.
    pub fn uncons(self, scrutinee: Subterm) -> Layer {
        match self {
            FreeMonoid::Unary => Self::uncons_unary(scrutinee),
            FreeMonoid::Bytes => Self::uncons_bytes(scrutinee),
            FreeMonoid::List => Self::uncons_list(scrutinee),
        }
    }

    fn uncons_unary(scrutinee: Subterm) -> Layer {
        let term = scrutinee.into();

        match Nat::peel_succ(&term) {
            // Peel one successor off the spine; the predecessor is the tail, and the unary generator carries no head. A spine count `> 1` keeps the rest as a shorter `Succ`, which is [`Nat::rebuild`]'s job rather than this one's.
            Some(tail) => Layer::Cons { head: None, tail },
            // The identity is zero; a count carrying no floor at all is a scrutinee with no layer to give.
            None => match Nat::is_zero(&term) {
                true => Layer::Empty,
                false => Layer::Stuck(Term::unwrap_or_clone(term)),
            },
        }
    }

    fn uncons_bytes(scrutinee: Subterm) -> Layer {
        let term = scrutinee.into();

        match peel_front(&term) {
            Front::Empty => Layer::Empty,
            // The peeled byte is the eliminator's `Byte`-typed head generator.
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

/// A leading generator peeled off a `Bytes` value: a concrete byte (`LiteralByte`) or the symbolic byte of a cons `append(x[], c)`, the head `x[c, ..t]` lowers to (`Symbolic`). Kept abstract so each destructor reflects it into the shape its consumer wants — the eliminator into a `Byte`, `Bytes/get`/`Bytes/slice` into a one-byte `Bytes` chunk.
enum Head {
    LiteralByte(u8),
    Symbolic(Term),
}

impl Head {
    /// Reflect into the eliminator's carrier-specific element type.
    fn into_atom(self) -> Term {
        match self {
            Head::LiteralByte(byte) => Subterm::Intrinsic(Intrinsic::Byte(byte)).into(),
            Head::Symbolic(atom) => atom,
        }
    }

    /// Reflect into a length-1 `Bytes` chunk — the cons head `Bytes/get`/`Bytes/slice` rebuild as `head ++ tail` (`get(head, 0)` is the byte; a symbolic cons head stays `append(x[], c)`).
    fn into_chunk(self) -> Term {
        match self {
            Head::LiteralByte(byte) => {
                Subterm::Intrinsic(Intrinsic::Bytes(Binary::from_bytes(vec![byte]))).into()
            }
            Head::Symbolic(atom) => Term::intrinsic(Intrinsic::bytes_append(
                Subterm::Intrinsic(Intrinsic::Bytes(Binary::empty())),
                atom,
            )),
        }
    }
}

/// One step of the `Bytes` front decode. `Empty` is the identity (`x[]`); `Cons` peels a leading generator and the residual tail; `Opaque` is a value exposing no leading generator (a variable, a slice, a non-`x[]`-based append, a fill whose count is neither zero nor a successor).
enum Front {
    Empty,
    Cons { head: Head, tail: Term },
    Opaque,
}

/// One pending level of a leading-edge descent — what that level has to put back once the generator underneath it has been peeled. `Appended` is a `BytesAppend`'s trailing atom; `Concat` is the operands a `BytesConcat` carried after its first.
///
/// **The descent is explicit rather than recursive because this depth is *data*-shaped.** `FUSION_CAP` stops an accumulation fusing, so what it builds is a concatenation nested as deeply as the loop is long, and the leading edge of that is what these walks descend. [`curios_utilities::recurse`] is taken at the *entry point* of each checker's reduction and checked between its frames, never per level of a helper it calls, so a hundred thousand native frames here would spend one granted segment without ever reaching a check. `curios-emit`'s `$<carrier>/force` walks the same shape on the same kind of explicit worklist, for the same reason — and `documentation/design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md` does not reach here: what that entry defends is the two reduction and conversion strategies written deliberately *twice*, so that a person can diff them, and these are single-implementation helpers both of them share.
enum BytesLevel<'a> {
    Appended(&'a Term),
    Concat(&'a [Term]),
}

/// The structural traversal shared by both `Bytes` destructors ([`FreeMonoid::uncons`] for the eliminator, [`peel_first_atom`] for `Bytes/get`/`Bytes/slice`): peel the leading generator off an already-reduced value. A literal run yields its first byte; a cons `append(x[], c)` yields its symbolic byte; a fill yields its atom over a fill one shorter; a concatenation descends into its first operand so a literal- or cons-led `BytesConcat` decodes too, the residual first-operand tail rejoining the rest — normalised (an empty first-operand tail drops, a lone survivor collapses) so a cons-led concat decodes to the same tail the bare cons would. The empty bytestring is `Empty`; anything else (a variable, a slice, a non-`x[]`-based append, a fill of unknown count) is `Opaque`.
///
/// Two phases over a [`BytesLevel`] stack: descend the leading edge to the value that actually carries the first generator, then rebuild outward, each level rejoining what it was holding. `Opaque` propagates through both level kinds, and a concatenation whose first operand exposes no generator is itself opaque — a reduced `BytesConcat` has no empty operands, so that case is a leading variable or slice.
fn peel_front(bytes: &Term) -> Front {
    let mut levels: Vec<BytesLevel<'_>> = Vec::new();
    let mut current = bytes;

    let mut front = loop {
        match &**current {
            Subterm::Intrinsic(Intrinsic::Bytes(run)) => {
                let tail = || {
                    Subterm::Intrinsic(Intrinsic::Bytes(run.slice(1, run.len()).unwrap())).into()
                };

                break match run.byte(0) {
                    None => Front::Empty,
                    Some(byte) => Front::Cons {
                        head: Head::LiteralByte(byte),
                        tail: tail(),
                    },
                };
            }
            // `append(base, atom) = base ++ [atom]`: peel the base's leading generator, and the appended atom rejoins the residual. An empty base is the canonical one-atom chunk, which is where the symbolic head comes from; any other base decodes as far as it can, so a chained `append(append(x[], a), b)` and a run-based `append(x[0x48], b)` both expose their first generator instead of going opaque. `core::spine`'s two-value peel has always decoded an append this way, so without this level the same term was transparent to conversion and opaque to reduction.
            Subterm::Intrinsic(Intrinsic::BytesAppend {
                bytes: base,
                element: atom,
            }) => {
                levels.push(BytesLevel::Appended(atom));
                current = base;
            }
            Subterm::Intrinsic(Intrinsic::BytesConcat { operands }) => {
                match operands.split_first() {
                    Some((first, rest)) => {
                        levels.push(BytesLevel::Concat(rest));
                        current = first;
                    }
                    None => break Front::Empty,
                }
            }
            // `replicate(n, a)` is `n` copies of one generator, so the leading one is `a` and what remains is a shorter fill of the same atom — no descent, because a fill carries its generator directly rather than under a spine. The count is read as [`FreeMonoid::uncons_unary`] reads a scrutinee, which is what makes a *symbolic* successor peel: a closed count folded to a run long before it reached here, so what arrives is the fill `/std`'s `not`, `shl` and `shr` build over an operand's own length, and leaving it opaque made a fill the one packed shape no eliminator could take apart.
            Subterm::Intrinsic(Intrinsic::BytesReplicate { count, atom }) => {
                break match Nat::peel_succ(count) {
                    Some(rest) => Front::Cons {
                        head: Head::Symbolic(atom.clone()),
                        tail: Term::intrinsic(Intrinsic::BytesReplicate {
                            count: rest,
                            atom: atom.clone(),
                        }),
                    },
                    None => match Nat::is_zero(count) {
                        true => Front::Empty,
                        false => Front::Opaque,
                    },
                };
            }
            _ => break Front::Opaque,
        }
    };

    while let Some(level) = levels.pop() {
        front = match (level, front) {
            (BytesLevel::Appended(atom), Front::Empty) => Front::Cons {
                head: Head::Symbolic(atom.clone()),
                tail: Subterm::Intrinsic(Intrinsic::Bytes(Binary::empty())).into(),
            },
            (BytesLevel::Appended(atom), Front::Cons { head, tail }) => Front::Cons {
                head,
                tail: Subterm::Intrinsic(Intrinsic::BytesAppend {
                    bytes: tail,
                    element: atom.clone(),
                })
                .into(),
            },
            (
                BytesLevel::Concat(rest),
                Front::Cons {
                    head,
                    tail: first_tail,
                },
            ) => {
                let mut segments = Vec::with_capacity(rest.len() + 1);
                if !is_empty_bytes(&first_tail) {
                    segments.push(first_tail);
                }
                segments.extend(rest.iter().cloned());
                let tail = match segments.len() {
                    0 => Subterm::Intrinsic(Intrinsic::Bytes(Binary::empty())).into(),
                    1 => segments.into_iter().next().unwrap(),
                    _ => Subterm::Intrinsic(Intrinsic::BytesConcat { operands: segments }).into(),
                };
                Front::Cons { head, tail }
            }
            (BytesLevel::Appended(_), Front::Opaque)
            | (BytesLevel::Concat(_), Front::Empty | Front::Opaque) => Front::Opaque,
        };
    }

    front
}

/// Split the first byte off a reduced `Bytes` value, returning a length-1 head chunk and the residual tail. Where `peel_bytes` (`core::spine`) strips a common prefix of *two* values, this decomposes *one* — the operation-level destructors `Bytes/get` and `Bytes/slice` walk an atom at a time, exposing the cons structure a symbolic `x[h, ..t]` builds (`concat(append(x[], h), t)`) along with literal runs and concatenations. `None` for the empty bytestring or an opaque symbolic value, where no first byte is statically exposed.
pub(crate) fn peel_first_atom(bytes: &Term) -> Option<(Term, Term)> {
    match peel_front(bytes) {
        Front::Cons { head, tail } => Some((head.into_chunk(), tail)),
        Front::Empty | Front::Opaque => None,
    }
}

/// One step of the `List` front decode — the element-typed analogue of [`Front`]. `Empty` is the identity (`[]`); `Cons` peels the leading element and the residual tail; `Opaque` is a value exposing no leading element (a variable, a slice). Unlike `Bytes`, the head needs no reflection — a `List` generator IS its element term, used directly by both the eliminator and the operations.
enum ListFront {
    Empty,
    Cons { head: Term, tail: Term },
    Opaque,
}

/// [`BytesLevel`] over the element carrier: `Appended` is a `ListAppend`'s trailing element, `Concat` the segments a `ListConcat` carried after its first. Each level also carries the element type, since every `List`-valued producer holds one and the residual has to be rebuilt with it.
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
            // `append(base, e) = base ++ [e]`: peel the base's leading element, and the appended element rejoins the residual. An empty base's front is the appended element itself; a decodable base exposes its own head instead. The element-typed twin of the `BytesAppend` level above, closing the same gap: `core::spine`'s two-value peel decodes an append as `concat(base, [e])`, so without this level the same term was transparent to conversion and opaque to reduction.
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

/// Split the first element off a reduced `List` value, returning the head element and the residual tail — the element-typed twin of [`peel_first_atom`]. Lets `List/get` and `List/slice` peel a symbolic cons one element at a time, exactly as `Bytes/get`/`Bytes/slice` walk an atom at a time. `None` for the empty array or an opaque symbolic value, where no first element is statically exposed.
pub(crate) fn peel_first_elem(list: &Term) -> Option<(Term, Term)> {
    match peel_front_list(list) {
        ListFront::Cons { head, tail } => Some((head, tail)),
        ListFront::Empty | ListFront::Opaque => None,
    }
}

// `cons` injects a byte at the front as `append(x[], c)` (a one-byte `Bytes`); `peel_front` recognises that encoding to decode a symbolic cons head.
fn is_empty_bytes(term: &Term) -> bool {
    matches!(&**term, Subterm::Intrinsic(Intrinsic::Bytes( bytes)) if bytes.is_empty())
}

// `cons` injects an element at the front as the singleton literal `[h]`; `peel_front_list`'s concat recursion decodes that encoding through the literal case.
fn is_empty_list(term: &Term) -> bool {
    matches!(&**term, Subterm::Intrinsic(Intrinsic::List { element: _, items: elems }) if elems.is_empty())
}

/// One node of a measured spine: a literal run of that many generators, or a concatenation's operands to walk into.
enum Spine<'a> {
    Run(usize),
    Operands(&'a [Term]),
}

/// One node of a juxtaposition: a concatenation's operands, or an append's base beside the one-generator value it adds, already built.
enum Joined<'a> {
    Operands(&'a [Term]),
    Append { base: &'a Term, singleton: Term },
}

impl FreeMonoid {
    /// How this carrier reads one node of a measured spine — `None` for a value whose length it cannot read off, which is a variable, an append, a window, or a node of another carrier.
    ///
    /// **The one place a carrier says what a measured node is.** The walks below are one walk over this, as the structural eliminator is one recursion over [`FreeMonoid::uncons`]: a carrier that could be measured differently by `get` than by `slice` is the shape this removes. `Unary` answers `None` throughout — its spine carries the value rather than operands, and nothing indexes into it.
    ///
    /// **Deliberately narrow: literal runs and their concatenations, nothing else.** A segment is what `get` and `slice` read *into*, and only a literal run can be narrowed where a window's edge falls inside it — `reduce::intrinsic`'s `bytes_piece` and `list_piece` hold every located segment to that — so anything else answers `None`, and every caller then falls back to the rule it uses otherwise. A window is the near miss: its length is the count it was cut to, which `len`'s homomorphism reads off the window on `/sys`'s `s + l <= len(b)` precondition, but its contents are not in hand to be narrowed. It costs nothing here, since an accumulation's spine is literals.
    fn spine<'a>(self, value: &'a Term) -> Option<Spine<'a>> {
        match (self, &**value) {
            (FreeMonoid::Bytes, Subterm::Intrinsic(Intrinsic::Bytes(run))) => {
                Some(Spine::Run(run.len()))
            }
            (FreeMonoid::Bytes, Subterm::Intrinsic(Intrinsic::BytesConcat { operands })) => {
                Some(Spine::Operands(operands))
            }
            (
                FreeMonoid::List,
                Subterm::Intrinsic(Intrinsic::List {
                    element: _,
                    items: elems,
                }),
            ) => Some(Spine::Run(elems.len())),
            (
                FreeMonoid::List,
                Subterm::Intrinsic(Intrinsic::ListConcat {
                    element: _,
                    operands,
                }),
            ) => Some(Spine::Operands(operands)),
            _ => None,
        }
    }

    /// How this carrier reads one *joined* node — a concatenation's operands, or an append's base beside the one-generator value it adds. `None` for anything that is neither.
    ///
    /// The seam [`FreeMonoid::concatenated`] walks, beside the one [`FreeMonoid::spine`] walks. They differ in what they admit rather than in how they recurse: a spine node must be *measurable*, so an append is not one, while a joined node need only be a juxtaposition, which an append is by the peel's own law.
    fn joined<'a>(self, value: &'a Term) -> Option<Joined<'a>> {
        match (self, &**value) {
            (FreeMonoid::Bytes, Subterm::Intrinsic(Intrinsic::BytesConcat { operands })) => {
                Some(Joined::Operands(operands))
            }
            (
                FreeMonoid::Bytes,
                Subterm::Intrinsic(Intrinsic::BytesAppend {
                    bytes: base,
                    element,
                }),
            ) => Some(Joined::Append {
                base,
                singleton: Term::intrinsic(Intrinsic::bytes_append(
                    Term::intrinsic(Intrinsic::Bytes(Binary::empty())),
                    element.clone(),
                )),
            }),
            (
                FreeMonoid::List,
                Subterm::Intrinsic(Intrinsic::ListConcat {
                    element: _,
                    operands,
                }),
            ) => Some(Joined::Operands(operands)),
            (
                FreeMonoid::List,
                Subterm::Intrinsic(Intrinsic::ListAppend {
                    element,
                    list: base,
                    item,
                }),
            ) => Some(Joined::Append {
                base,
                singleton: Term::intrinsic(Intrinsic::list_append(
                    element.clone(),
                    Term::intrinsic(Intrinsic::List {
                        element: element.clone(),
                        items: Vec::new(),
                    }),
                    item.clone(),
                )),
            }),
            _ => None,
        }
    }

    /// A value read as a concatenation's operands, flattened: a concatenation's own, and an append's base beside the one-generator run it adds. `None` for anything that is neither.
    ///
    /// **An append *is* a concatenation, and the locator agrees.** `append(b, k) = b ++ append(x[], k)` is the peel's own law, conversion's spine flattens both spellings to one atom list, and `x[..p, k]` and `x[..p, ..x[k]]` are definitionally equal terms. A locator gated on the concatenation node alone would decline a window it had already decided for the other spelling of the same value — incompleteness with nothing on the refusing side to justify it. The law is the peel's, not one caller's, so a snoc has to locate.
    ///
    /// **The reading flattens, so a seam is a seam however it is nested.** A carrier that flattened only its outermost node would present an inner seam as an operand's interior, which is a window it can already decide declined for the way it happens to be spelled.
    pub(crate) fn concatenated(self, value: &Term) -> Option<Vec<Term>> {
        fn flatten(carrier: FreeMonoid, value: &Term, into: &mut Vec<Term>) {
            match carrier.joined(value) {
                Some(Joined::Operands(operands)) => {
                    for operand in operands {
                        flatten(carrier, operand, into);
                    }
                }
                Some(Joined::Append { base, singleton }) => {
                    flatten(carrier, base, into);
                    into.push(singleton);
                }
                None => into.push(value.clone()),
            }
        }

        self.joined(value).is_some().then(|| {
            let mut operands = Vec::new();
            flatten(self, value, &mut operands);
            operands
        })
    }

    /// The operands of an already-reduced value, left to right, each with the number of generators it carries — `None` where any operand's length is not statically known.
    ///
    /// **The free monoid's measure, and the one thing `len`, `get` and `slice` all actually want.** A length is a homomorphism into `(ℕ, +, 0)`, and over a value already in normal form it is a *fold over the spine*: no reduction, no rebuilding, no budget. That is what this is, where building `Bytes/len(operand)` terms and handing them back to the reducer would cost a full re-walk of each sub-spine, so a spine of depth n would cost Σk = O(n²) where the answer is one linear pass.
    ///
    /// **It takes no [`crate::Reducer`], and that is the enforcement rather than a comment.** A function that cannot reach the reducer cannot re-enter reduction, cannot spend budget, and cannot rebuild a term to ask about it. The audit is the signature.
    ///
    /// **Conversion has the same notion.** `curios-algebra`'s `Segment::Window` is a stretch whose contents are unknown and whose length is known, so deciding equality measures what it cannot read, as reduction does here.
    fn segments(self, value: &Term) -> Option<Vec<(&Term, usize)>> {
        let mut segments = Vec::new();
        let mut pending = vec![value];

        while let Some(term) = pending.pop() {
            match self.spine(term)? {
                Spine::Run(length) => segments.push((term, length)),
                // Pushed in reverse so they come back off left to right: a segment list is ordered, unlike the sum taken over it.
                Spine::Operands(operands) => pending.extend(operands.iter().rev()),
            }
        }

        Some(segments)
    }

    /// How many generators an already-reduced value carries. `None` where it is not wholly measurable, or where the total does not fit a `usize`.
    ///
    /// **`Bytes`'s answer is deliberately a superset of its segments**, which is why this dispatches rather than folding [`FreeMonoid::segments`] for every carrier — see [`bytes_measure`].
    pub(crate) fn measure(self, value: &Term) -> Option<usize> {
        match self {
            FreeMonoid::Bytes => bytes_measure(value),
            _ => total(&lengths(&self.segments(value)?)),
        }
    }

    /// The operands a `count`-long window at `start` spans, each already narrowed to its overlap — the pieces whose concatenation *is* the window. `None` when the value is not wholly measurable; `Err` carries the measured total when the window runs past the end, which the caller reports.
    ///
    /// The point of returning pieces rather than a value: an operand the window covers whole is handed back untouched and shares its payload, and only the two at the edges are narrowed. A window over a spine therefore costs one pass and two slices, where peeling one generator at a time costs a walk of the whole spine per generator read.
    pub(crate) fn measured_window<'a>(
        self,
        value: &'a Term,
        start: usize,
        count: usize,
    ) -> Option<Result<Vec<Piece<'a>>, usize>> {
        window(self.segments(value)?, start, count)
    }
}

/// How many generators an already-reduced `Bytes` value carries. `None` where the value is not wholly measurable, or where the total does not fit a `usize`.
///
/// **Deliberately a superset of [`FreeMonoid::segments`] rather than a fold over it.** A *segment* is something `Bytes/get` and `Bytes/slice` may read *into*; this walk additionally credits an operation whose result arity is fixed by the operation rather than by any operand. That is `Flt/to_le_bytes` alone, which writes eight bytes for every float, NaN and both zeros included.
///
/// Crediting it as a segment instead would be wrong twice over: locating an index would hand the node back to `Bytes/get` as the operand holding it, which re-enters this same node and never terminates, and [`FreeMonoid::measured_window`] would try to narrow a value it cannot read.
///
/// The length is the one thing about such a node knowable without observing the float, and that is what this reads. `Flt` folds through the binary64 model, so a *literal* operand is answered by `reduce::intrinsic` before it ever reaches here; what reaches it is a **symbolic** operand, where `Bytes/len(Flt/to_le_bytes(x))` is still `8` because it is the arity of the result rather than anything about the float. Without it `Flt/of_le_bytes`'s length precondition is undischargeable over the very operation it inverts, so the pair's round trip could not be written at all.
fn bytes_measure(value: &Term) -> Option<usize> {
    let mut total = 0usize;
    let mut pending = vec![value];

    while let Some(term) = pending.pop() {
        let length = match &**term {
            Subterm::Intrinsic(Intrinsic::Bytes(run)) => run.len(),
            Subterm::Intrinsic(Intrinsic::BytesConcat { operands }) => {
                // Order is irrelevant to a sum, so this walk does not reverse the way [`FreeMonoid::segments`] must.
                pending.extend(operands.iter());
                continue;
            }
            Subterm::Intrinsic(Intrinsic::FltToLeBytes(_)) => 8,
            _ => return None,
        };
        total = total.checked_add(length)?;
    }

    Some(total)
}

/// The lengths of `segments`, in order, as `curios-algebra`'s measures read them.
fn lengths(segments: &[(&Term, usize)]) -> Vec<usize> {
    segments.iter().map(|(_, length)| *length).collect()
}

/// The operands a `count`-long window at `start` spans, each already narrowed to its overlap — the pieces whose concatenation *is* the window. `None` when the value is not wholly measurable; `Err` carries the measured total when the window runs past the end, which the caller reports.
///
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
    locate(&lengths(&segments), start, count).map(|located| {
        located.map(|spans| {
            spans
                .into_iter()
                .map(|span| match span {
                    Span::Whole(at) => Piece::Whole(segments[at].0),
                    Span::Part { operand, range } => {
                        Piece::Part(segments[operand].0, range.start, range.end)
                    }
                })
                .collect::<Vec<_>>()
        })
    })
}

/// The largest literal operand reduction will fuse, in the carrier's own generators — bytes for `Bytes`, elements for `List`.
///
/// **Fusion is an optimization for small runs, not a normalization obligation.** [`normalize_concat`] copies every operand it fuses, so an accumulation that appends to its own result recopies everything accumulated so far on every step: a linear number of transitions performing a quadratic volume of construction. Declining to fuse past this size leaves the `Concat` node standing instead, and each step is then one node over operands it shares rather than copies. `curios`'s `tests::reduction` measures both halves; `tests::runtime`'s `accumulation_loops_are_linear_by_construction` is the same loop at runtime, where a rope has always made it linear.
///
/// **The cap is by *operand*, and that is the load-bearing half.** Under a cap on the *result* an accumulator is re-copied within every chunk, so the cost stays quadratic in the chunk size and is merely divided by the number of chunks. Under an operand cap the accumulator stops being fusible after a handful of steps whatever its chunk size, every later step is one node, and the leaves of a loop appending the same run share one payload.
///
/// **Chosen from a census, not picked.** Counted at `9c543476a`, in a scratch copy where each of the two `merge` closures appends a line to a file a variable names at every call — its carrier, its operand count, its largest operand and their total — and a second variable lifts the cap. Run over a prelude build (its two build scripts: the elaboration, then the kernel certification, of every `/sys` and `/std` module), over each of `curios`'s tests run alone, and over `curios run` of `tests::reduction`'s `bytes_growing(800)`, which appends ten bytes to its own result 800 times under a bound that evaluates it:
///
/// | Corpus | Fusing concatenations | Largest operand |
/// | --- | --- | --- |
/// | Prelude build, elaboration | 18 — 10 `Bytes`, 8 `List` | 1 generator |
/// | Prelude build, certification | 26 — 13 `Bytes`, 13 `List` | 1 generator |
/// | `curios`'s tests, but the five that accumulate to the cap | 1146 — 334 `Bytes`, 812 `List` | 7 generators |
/// | The five that accumulate to the cap | 119 — 98 `Bytes`, 21 `List` | 60 generators |
/// | `bytes_growing(800)`, the cap lifted | 2400 — 800 for each of the elaborator, the kernel and erasure | 7990 generators |
///
/// So the corpus fuses little, and what it does fuse is one to seven generators; the pathological case is unbounded. Any cap of eight or more leaves every normal form the rest of the corpus reaches untouched, and this one clears the observed maximum by nine times while stopping the accumulation within a handful of steps: the five tests that accumulate (`tests::reduction`'s two, `tests::numeric::bound_tests`' two and `tests::runtime`'s `accumulation_loops_are_linear_by_construction`) fuse no operand past 60, and `bytes_growing(800)` at the cap fuses seven times a walk. The last row is the whole defect in one line: operands of 10, 20, 30 … 7990, the accumulator recopied every step.
///
/// **What declining to fuse costs is completeness, not soundness.** It removes a normalization step rather than adding an equation, so the risk is a proof that would close by literal equality failing to close through the peel. It does not: `crate::words` flattens a concatenation into segments and `curios-algebra`'s `Word::push` merges adjacent runs, so a capped spelling and the literal it would have fused to decompose identically. `curios-core`'s `spine` tests state that per carrier, `curios`'s `tests::aggregates::monoid_tests::a_literal_run_is_the_same_value_however_it_is_grouped` states it where both checkers see it, and a workspace build — which elaborates, erases and certifies the whole standard library — is the detector for anything they miss.
pub(crate) const FUSION_CAP: usize = 64;

/// The literal run a normalized concatenation inspects — each carrier's own representation of a generator sequence (`Binary` for `Bytes`, the element vector for `List`), exposing only the emptiness [`normalize_concat`] drops.
pub(crate) trait Run {
    fn is_empty(&self) -> bool;
}

impl Run for Binary {
    fn is_empty(&self) -> bool {
        Binary::is_empty(self)
    }
}

impl Run for Vec<Term> {
    fn is_empty(&self) -> bool {
        self.as_slice().is_empty()
    }
}

/// The free monoid's normalising *product* — the constructor dual of [`FreeMonoid::uncons`] (the destructor) — shared by `BytesConcat` and `ListConcat` reduction. Collapse a concatenation's already-reduced `operands` to a normal form under the unit and associativity laws: drop the empty identity (`x[]`, `[]`), fuse an all-*fusible*-literal survivor set into one literal, and collapse a lone surviving operand to itself (an `n`-ary concat of one *is* that one). `literal` lends an operand's run when it is a literal this may fuse (`None` for a symbolic chunk *or* for a literal past [`FUSION_CAP`], which is how the cap is applied without this function knowing the carrier's size unit); `merge` fuses the runs in the carrier's own representation — `Binary::concat`'s bulk copy, `List`'s flatten — and must fuse zero runs to the empty literal; `into_concat` rebuilds the surviving mixed operands. Which operands are dropped, fused or kept is `curios-algebra`'s `join`, over what `literal` says each one is; this reads them for it and builds what it decides.
///
/// An over-cap operand therefore reads exactly like a symbolic one here: not droppable as empty, not fusible, and left standing by `into_concat`. That is the whole of the cap's implementation, and it is why the emptiness filter below is unaffected — an over-cap run is never empty.
///
/// **`merge` is fallible and the other two are not**, and the asymmetry is the price list's rather than this function's: fusing is the only branch that *constructs* anything, so it is the only one that has to charge before it does. Dropping an empty operand and rebuilding the survivors both hand back storage that already exists. That makes this function fallible for its caller, which is the shape the audit says to expect at every fusing seam rather than only at this one.
///
/// Window fusion (adjacent `Bytes/slice`s of one base) is deliberately NOT done here: that is the word strip's job when *deciding equality* (`curios-algebra`'s `Word::push`); reduction only needs a normal form, and conversion closes any residual gap.
pub(crate) fn normalize_concat<C: Run, E>(
    mut operands: Vec<Term>,
    literal: impl Fn(&Term) -> Option<&C>,
    merge: impl FnOnce(Vec<&C>) -> Result<Subterm, E>,
    into_concat: impl FnOnce(Vec<Term>) -> Subterm,
) -> Result<Subterm, E> {
    let joining = operands
        .iter()
        .map(|operand| match literal(operand) {
            Some(run) if run.is_empty() => Joining::Empty,
            Some(_) => Joining::Fusible,
            None => Joining::Standing,
        })
        .collect::<Vec<_>>();
    match join(&joining) {
        Concatenated::Fused(kept) => merge(
            kept.iter()
                .map(|&at| literal(&operands[at]).expect("a fused operand is a literal"))
                .collect(),
        ),
        Concatenated::Lone(at) => Ok(Term::unwrap_or_clone(operands.swap_remove(at))),
        Concatenated::Kept(kept) => {
            let mut operands = operands.into_iter().map(Some).collect::<Vec<_>>();
            Ok(into_concat(
                kept.iter()
                    .map(|&at| operands[at].take().expect("each operand is kept once"))
                    .collect(),
            ))
        }
    }
}

#[cfg(test)]
mod tests;
