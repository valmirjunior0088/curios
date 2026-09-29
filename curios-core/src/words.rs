//! The words `curios-algebra` compares, read off `Bin` and `List` terms and rebuilt from what it leaves.
//!
//! A value is read as its segments — a literal as a run, a `Bin/append`'s symbolic byte as a single element, a slice as a window of its base, anything else as an opaque chunk — into `curios-algebra`'s `Word`, which keeps the free monoid's normal form and decides what two words share. What is decided here is which terms are one symbol and which are one number, where a position lies inside the windows it is read through, and how a residual is rebuilt.
//!
//! **Symbols are compared as written.** A list element may be a type, where a level is part of the value, so an element, a symbolic byte, a chunk and a window's base are one symbol only where they are one term. **Numbers take the numeric identity:** a window's offset and length and a position are `Nat`s, compared by the carrier's cancellation up to universe instances ([`Nat::same`]) and summed as the fold sums them ([`Nat::sum`]), Euclid's recombination included.

use {
    super::{Intrinsic, Nat, Subterm, Term},
    curios_algebra::{Alphabet, Packed, Run, Segment, Window, Word},
    curios_num::{Binary, Grain},
};

/// A sequence carrier's words: one alphabet over the `Nat` measures every sequence shares, and the sequence that says what its runs are, how a chunk is measured, and how a value is read and rebuilt.
pub(crate) struct Words<S> {
    sequence: S,
}

/// The element type a `List`'s words are over, which every residual is rebuilt with.
pub(crate) struct Element(pub(crate) Term);

/// What differs between `Bin`'s words and `List`'s.
pub(crate) trait Sequence: Sized {
    type Run: Run;
    /// `len` of an opaque chunk.
    fn measure(&self, chunk: &Term) -> Term;
    /// A value flattened to its segments.
    fn read(words: &Words<Self>, intrinsic: &Intrinsic) -> Word<Words<Self>>;
    /// A residual rebuilt as a term: a lone segment as itself, so the inverter sees the bare binder it must solve, and several as their concatenation.
    fn rebuild(&self, word: Word<Words<Self>>) -> Term;
}

impl<S: Sequence> Words<S> {
    pub(crate) fn new(sequence: S) -> Self {
        Words { sequence }
    }

    /// `intrinsic` as its word.
    pub(crate) fn read(&self, intrinsic: &Intrinsic) -> Word<Self> {
        S::read(self, intrinsic)
    }

    /// `word` as the term it spells.
    pub(crate) fn rebuild(&self, word: Word<Self>) -> Term {
        self.sequence.rebuild(word)
    }
}

impl<S: Sequence> Alphabet for Words<S> {
    type Run = S::Run;
    type Symbol = Term;
    type Number = Term;
    type Proof = Term;

    fn count(&self, count: usize) -> Term {
        Term::intrinsic(Intrinsic::Nat(Nat::new(count)))
    }

    fn is_zero(&self, number: &Term) -> bool {
        Nat::is_zero(number)
    }

    fn sum(&self, left: &Term, right: &Term) -> Term {
        Nat::sum(left, right)
    }

    fn same(&self, left: &Term, right: &Term) -> bool {
        Nat::same(left, right)
    }

    /// [`Nat::cancel_common`] is the whole of it, because a distance and a measure are two `Nat`s in one cancellative monoid: what it leaves on the left is the distance still to cover, and anything it leaves on the right is what the measure had and the distance did not. Clamping each shared coefficient is what keeps the subtraction total, so a measure the distance cannot absorb arrives as a residual to read rather than a difference to guard.
    fn difference(&self, minuend: &Term, subtrahend: &Term) -> Option<Term> {
        let (rest, unmatched) = Nat::cancel_common(minuend, subtrahend);
        Nat::is_zero(&unmatched).then_some(rest)
    }

    fn measure(&self, chunk: &Term) -> Term {
        self.sequence.measure(chunk)
    }

    /// A slice `slice(b, s, l)` begins at `s`, so its position `i` is `b`'s position `s + i`, and a window of a window nests the same way. The windows' own lengths and proofs are not read: what makes every window on the way well placed is the typing of the term in hand, and this states where a position *is*, never that it is in range.
    fn rooted(&self, base: &Term, position: &Term) -> (Term, Term) {
        let (mut base, mut position) = (base.clone(), position.clone());
        loop {
            let (inner, start) = match &*base {
                Subterm::Intrinsic(Intrinsic::BinSlice { bin, start, .. }) => {
                    (bin.clone(), start.clone())
                }
                Subterm::Intrinsic(Intrinsic::ListSlice { list, start, .. }) => {
                    (list.clone(), start.clone())
                }
                _ => return (base, position),
            };
            position = Nat::sum(&start, &position);
            base = inner;
        }
    }

    /// `root` is read as the word of its own carrier, whatever this alphabet's is.
    fn offsets(&self, root: &Term, operand: &Term) -> Vec<Term> {
        let Subterm::Intrinsic(intrinsic) = &**root else {
            return Vec::new();
        };

        if let Some(grain) = bin_grain(intrinsic) {
            let words = Words::new(grain);
            return words.read(intrinsic).offsets_of(&words, operand);
        }

        match list_element(intrinsic) {
            Some(element) => {
                let words = Words::new(Element(element));
                words.read(intrinsic).offsets_of(&words, operand)
            }
            None => Vec::new(),
        }
    }
}

/// The `Bin`-valued intrinsics a word reads at `grain`. `Bin` and `BinConcat` carry the monoid's literals and juxtaposition; `BinSlice` rides in as a window, so adjacent slices of one base fuse and equal slices cancel; `BinAppend` rides in as its base followed by the appended byte; `BinReplicate` rides in as its leading generator followed by a fill one shorter, which is what lets a fill compare against the cons it equals. Any other producer is an opaque chunk left to the caller's own comparison.
pub(crate) fn bin_grain(intrinsic: &Intrinsic) -> Option<Grain> {
    match intrinsic {
        Intrinsic::Bin(grain, _)
        | Intrinsic::BinConcat { grain, operands: _ }
        | Intrinsic::BinSlice { grain, .. }
        | Intrinsic::BinAppend { grain, .. }
        | Intrinsic::BinReplicate { grain, .. } => Some(*grain),
        _ => None,
    }
}

/// The `List` analogue of [`bin_grain`], doubling as the element type residuals are rebuilt with: every element of a `List(T)` value is a `T`, so one suffices for the whole list. `List` and `ListConcat` carry the monoid's literals and juxtaposition, `ListSlice` rides in as a window, and `ListAppend` as its base followed by a one-element run, so `append(xs, e)` is `xs ++ [e]`.
pub(crate) fn list_element(intrinsic: &Intrinsic) -> Option<Term> {
    match intrinsic {
        Intrinsic::List { element, items: _ }
        | Intrinsic::ListConcat {
            element,
            operands: _,
        }
        | Intrinsic::ListSlice { element, .. }
        | Intrinsic::ListAppend { element, .. } => Some(element.clone()),
        _ => None,
    }
}

/// One item of a flattening walk's worklist. `Appended` is an append's trailing element, held back so it lands after everything its base contributes — the one place order is not simply left to right, and the reason a plain stack of operands would not do.
///
/// Explicit rather than recursive because a concatenation's depth is data-shaped once [`crate::FUSION_CAP`] stops an accumulation fusing; see [`crate::free_monoid`]'s `BinLevel`, which states the argument in full for the destructor side.
enum Pending<'a> {
    Term(&'a Term),
    Intrinsic(&'a Intrinsic),
    Appended(&'a Term),
}

impl Sequence for Grain {
    type Run = Packed;

    fn measure(&self, chunk: &Term) -> Term {
        Term::intrinsic(Intrinsic::BinLen(*self, chunk.clone()))
    }

    fn read(words: &Words<Grain>, intrinsic: &Intrinsic) -> Word<Words<Grain>> {
        let grain = words.sequence;
        let mut word = Word::default();
        let mut push = |segment| word.push(words, segment);
        // A stack, so operands are pushed in reverse to come back off in order: a run merges only with the run just before it.
        let mut pending = vec![Pending::Intrinsic(intrinsic)];

        while let Some(item) = pending.pop() {
            match item {
                Pending::Term(term) => match &**term {
                    Subterm::Intrinsic(intrinsic) => pending.push(Pending::Intrinsic(intrinsic)),
                    _ => push(Segment::Chunk(term.clone())),
                },
                // A concrete byte is a one-element run, so it merges with an abutting run and meets `concat(base, x[b])`; a symbolic one is a single element, whose contents are unknown and whose length is one.
                Pending::Appended(element) => push(generator(grain, element)),
                Pending::Intrinsic(intrinsic) => match intrinsic {
                    Intrinsic::Bin(found, value) if *found == grain => push(Segment::Run(Packed {
                        grain,
                        bits: value.clone(),
                    })),
                    Intrinsic::BinConcat {
                        grain: found,
                        operands,
                    } if *found == grain => {
                        pending.extend(operands.iter().rev().map(Pending::Term));
                    }
                    Intrinsic::BinSlice {
                        grain: found,
                        bin,
                        start,
                        length,
                        within,
                    } if *found == grain => push(Segment::Window(Window {
                        base: bin.clone(),
                        offset: start.clone(),
                        length: length.clone(),
                        proof: within.clone(),
                    })),
                    // `replicate(n + 1, a) = [a] ++ replicate(n, a)`: the leading generator, and the shorter fill as one chunk, which is all conversion needs — the other side's own residual fill is that same chunk, so the two meet without either being unrolled. A count of zero contributes nothing, and a count of unknown size stays opaque, since a fill that might be empty exposes no generator.
                    Intrinsic::BinReplicate {
                        grain: found,
                        count,
                        atom,
                    } if *found == grain => match Nat::peel_succ(count) {
                        Some(rest) => {
                            push(generator(grain, atom));
                            push(Segment::Chunk(Term::intrinsic(Intrinsic::BinReplicate {
                                grain,
                                count: rest,
                                atom: atom.clone(),
                            })));
                        }
                        None => {
                            if !Nat::is_zero(count) {
                                push(Segment::Chunk(Term::intrinsic(intrinsic.clone())));
                            }
                        }
                    },
                    Intrinsic::BinAppend {
                        grain: found,
                        bin,
                        element,
                    } if *found == grain => {
                        pending.push(Pending::Appended(element));
                        pending.push(Pending::Term(bin));
                    }
                    other => push(Segment::Chunk(Term::intrinsic(other.clone()))),
                },
            }
        }

        word
    }

    /// A run is a `Bin` literal, a single element the one-byte `append(x[], b)`, and a window its `BinSlice`.
    fn rebuild(&self, word: Word<Words<Grain>>) -> Term {
        let grain = *self;
        let into_term = |segment| match segment {
            Segment::Run(Packed { bits, .. }) => Term::intrinsic(Intrinsic::Bin(grain, bits)),
            Segment::Single(element) => Term::intrinsic(Intrinsic::bin_append(
                grain,
                Term::intrinsic(Intrinsic::Bin(grain, Binary::empty())),
                element,
            )),
            Segment::Window(window) => Term::intrinsic(Intrinsic::bin_slice(
                grain,
                window.base,
                window.offset,
                window.length,
                window.proof,
            )),
            Segment::Chunk(term) => term,
        };

        let mut segments = word.into_segments();
        match segments.len() {
            1 => into_term(segments.pop_front().unwrap()),
            _ => Term::intrinsic(Intrinsic::BinConcat {
                grain,
                operands: segments.into_iter().map(into_term).collect(),
            }),
        }
    }
}

/// An appended or replicated `Bin` element as its segment: a decided bit or byte — taken mod 256, as the runtime's packed store takes it — is a one-element run, and anything else a single element.
fn generator(grain: Grain, element: &Term) -> Segment<Words<Grain>> {
    let bits = match (grain, &**element) {
        (Grain::B, Subterm::Intrinsic(Intrinsic::Bool(bit))) => Some(Binary::from_bits([*bit])),
        (Grain::X, Subterm::Intrinsic(Intrinsic::Byte(byte))) => {
            Some(Binary::from_bytes(vec![*byte]))
        }
        _ => None,
    };
    match bits {
        Some(bits) => Segment::Run(Packed { grain, bits }),
        None => Segment::Single(element.clone()),
    }
}

impl Sequence for Element {
    type Run = Vec<Term>;

    fn measure(&self, chunk: &Term) -> Term {
        Term::intrinsic(Intrinsic::ListLen {
            element: self.0.clone(),
            list: chunk.clone(),
        })
    }

    /// A `List` never reads a single element: its runs hold terms, so an appended element is a one-element run.
    fn read(words: &Words<Element>, intrinsic: &Intrinsic) -> Word<Words<Element>> {
        let mut word = Word::default();
        let mut push = |segment| word.push(words, segment);
        let mut pending = vec![Pending::Intrinsic(intrinsic)];

        while let Some(item) = pending.pop() {
            match item {
                Pending::Term(term) => match &**term {
                    Subterm::Intrinsic(intrinsic) => pending.push(Pending::Intrinsic(intrinsic)),
                    _ => push(Segment::Chunk(term.clone())),
                },
                Pending::Appended(item) => push(Segment::Run(vec![item.clone()])),
                Pending::Intrinsic(intrinsic) => match intrinsic {
                    Intrinsic::List { element: _, items } => push(Segment::Run(items.clone())),
                    Intrinsic::ListConcat {
                        element: _,
                        operands,
                    } => {
                        pending.extend(operands.iter().rev().map(Pending::Term));
                    }
                    Intrinsic::ListSlice {
                        element: _,
                        list,
                        start,
                        length,
                        within,
                    } => push(Segment::Window(Window {
                        base: list.clone(),
                        offset: start.clone(),
                        length: length.clone(),
                        proof: within.clone(),
                    })),
                    Intrinsic::ListAppend {
                        element: _,
                        list,
                        item,
                    } => {
                        pending.push(Pending::Appended(item));
                        pending.push(Pending::Term(list));
                    }
                    other => push(Segment::Chunk(Term::intrinsic(other.clone()))),
                },
            }
        }

        word
    }

    /// [`Grain`]'s rebuild over element runs, restoring the element type.
    fn rebuild(&self, word: Word<Words<Element>>) -> Term {
        let into_term = |segment| match segment {
            Segment::Run(items) => Term::intrinsic(Intrinsic::List {
                element: self.0.clone(),
                items,
            }),
            Segment::Single(item) => Term::intrinsic(Intrinsic::List {
                element: self.0.clone(),
                items: vec![item],
            }),
            Segment::Window(window) => Term::intrinsic(Intrinsic::list_slice(
                self.0.clone(),
                window.base,
                window.offset,
                window.length,
                window.proof,
            )),
            Segment::Chunk(term) => term,
        };

        let mut segments = word.into_segments();
        match segments.len() {
            1 => into_term(segments.pop_front().unwrap()),
            _ => Term::intrinsic(Intrinsic::ListConcat {
                element: self.0.clone(),
                operands: segments.into_iter().map(into_term).collect(),
            }),
        }
    }
}
