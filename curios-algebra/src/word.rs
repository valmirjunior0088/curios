//! Words: the free monoid a sequence carrier is, read as segments — runs of known elements beside stretches whose contents are unknown.
//!
//! A [`Word`] is a list of [`Segment`]s kept in one normal form as it is built: no empty run and no empty window, no two runs adjacent, and no window beginning where the window before it on the same base ends. Two values spelled apart by the monoid's laws — a nesting, an append, a run split across operands, two touching slices — therefore read as one list, and [`Word::strip_common_prefix`] compares two words by taking off the longest prefix they certainly share.
//!
//! **What a word is spelled over is its caller's [`Alphabet`].** A symbol — a single element, an opaque chunk, the base a window is cut from — is one symbol where the caller's equality says so, and two numbers are one number where [`Alphabet::same`] says so. The measures stay the caller's because their normal form is: a sum is folded as the carrier folds it, Euclid's recombination included, which reads the terms the numbers are; and a number is read only where it is compared, so a word costs what its comparisons cost. What is stated here is everything the monoid decides from those answers: the normal form, window fusion, the prefix strip and its verdicts, where an operand begins, and when two positions are one.

use {
    curios_num::{Binary, Grain},
    std::collections::VecDeque,
};

/// What a word is spelled over, and the arithmetic its measures are kept in.
///
/// Nothing here converts or reduces: [`Alphabet::sum`] is the carrier's normal form of a sum, [`Alphabet::same`] is its identity, and [`Alphabet::rooted`] and [`Alphabet::offsets`] read how a term is built. Every decision taken from those answers is this module's.
pub trait Alphabet {
    /// A run of elements a literal spells out.
    type Run: Run;
    /// A single element, an opaque chunk, or the base a window is cut from, compared by its own equality — the caller's identity for it.
    type Symbol: PartialEq;
    /// A length, an offset or a position.
    type Number: Clone;
    /// What a window carries and is never compared by: the evidence that it lies inside its base.
    type Proof;

    /// The number `count`.
    fn count(&self, count: usize) -> Self::Number;
    /// Whether `number` is certainly zero.
    fn is_zero(&self, number: &Self::Number) -> bool;
    /// `left + right`, in the carrier's normal form.
    fn sum(&self, left: &Self::Number, right: &Self::Number) -> Self::Number;
    /// Whether two numbers are certainly one. `false` declines; it never claims they differ.
    fn same(&self, left: &Self::Number, right: &Self::Number) -> bool;
    /// `minuend - subtrahend` where the subtrahend is certainly part of the minuend, or `None` where it is not certainly: the distance still to cover once a measure is taken off it.
    fn difference(&self, minuend: &Self::Number, subtrahend: &Self::Number)
    -> Option<Self::Number>;
    /// The length of an opaque chunk.
    fn measure(&self, chunk: &Self::Symbol) -> Self::Number;
    /// `position` in `base`, read through every window `base` is itself cut from: the root those windows were cut from, and the position counted from the root's start.
    fn rooted(&self, base: &Self::Symbol, position: &Self::Number) -> (Self::Symbol, Self::Number);
    /// Where `operand` begins inside `root` read as a word — [`Word::offsets_of`] over `root`'s own segments, empty when `root` is no word.
    fn offsets(&self, root: &Self::Symbol, operand: &Self::Symbol) -> Vec<Self::Number>;
}

/// A run of elements a literal spells out.
pub trait Run {
    /// Whether elements are decided values, so that two runs whose first elements differ are two values. Packed bits and bytes are; element terms are not, since two of them may still convert.
    const DECIDED: bool;
    /// How many elements the run holds.
    fn len(&self) -> usize;
    /// Whether the run holds no element.
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    /// How many leading elements `self` and `other` certainly share.
    fn shared_prefix(&self, other: &Self) -> usize;
    /// The run without its first `count` elements; `count` never exceeds its length.
    fn skip(&mut self, count: usize);
    /// The run followed by `other`'s elements.
    fn extend(&mut self, other: Self);
}

/// A packed run: bits or bytes as `curios-num` holds them, never an element per bit.
#[derive(Clone, Debug, PartialEq)]
pub struct Packed {
    pub grain: Grain,
    pub bits: Binary,
}

impl Run for Packed {
    const DECIDED: bool = true;

    fn len(&self) -> usize {
        self.bits.len(self.grain)
    }

    fn shared_prefix(&self, other: &Self) -> usize {
        let bound = self.len().min(other.len());
        match self.grain {
            Grain::B => (0..bound)
                .take_while(|&index| self.bits.bit(index) == other.bits.bit(index))
                .count(),
            Grain::X => (0..bound)
                .take_while(|&index| self.bits.byte(index) == other.bits.byte(index))
                .count(),
        }
    }

    fn skip(&mut self, count: usize) {
        let skipped = count * self.grain.bits();
        self.bits = self
            .bits
            .window(skipped, self.bits.bit_length() - skipped)
            .expect("a run is skipped by at most its length");
    }

    fn extend(&mut self, other: Self) {
        self.bits = Binary::concat([&self.bits, &other.bits]);
    }
}

impl<E: PartialEq> Run for Vec<E> {
    const DECIDED: bool = false;

    fn len(&self) -> usize {
        Vec::len(self)
    }

    fn shared_prefix(&self, other: &Self) -> usize {
        self.iter()
            .zip(other)
            .take_while(|(left, right)| left == right)
            .count()
    }

    fn skip(&mut self, count: usize) {
        self.drain(0..count);
    }

    fn extend(&mut self, other: Self) {
        Extend::extend(self, other);
    }
}

/// One stretch of a word.
pub enum Segment<A: Alphabet> {
    /// Elements spelled out.
    Run(A::Run),
    /// One element whose value is unknown: its length is one, which is what lets it clash against the empty word without being read.
    Single(A::Symbol),
    /// A stretch of a base whose contents are unknown and whose length is known.
    Window(Window<A>),
    /// A stretch whose contents and length are both unknown.
    Chunk(A::Symbol),
}

/// `length` elements of `base`, starting `offset` elements in.
pub struct Window<A: Alphabet> {
    pub base: A::Symbol,
    pub offset: A::Number,
    pub length: A::Number,
    /// Carried so a residual is rebuilt as the bounded window it came from, and so fusion hands one on rather than derives one. Never compared: two windows over one span are one window whatever proved them.
    pub proof: A::Proof,
}

/// A value as its segments, in the monoid's normal form.
pub struct Word<A: Alphabet> {
    segments: VecDeque<Segment<A>>,
}

impl<A: Alphabet> Default for Word<A> {
    fn default() -> Self {
        Word {
            segments: VecDeque::new(),
        }
    }
}

/// What stripping two words' longest common prefix concluded.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stripped {
    /// Both words were stripped to nothing.
    Equal,
    /// The words are two values: their heads are decided runs that disagree, or one is empty and the other holds a run or a single element, and so has a positive length.
    Impossible,
    /// One word is empty and the other holds only windows and chunks, whose lengths may all be zero.
    Undecided,
    /// Both words keep a residual whose heads the strip could not match, and `peeled` says whether anything came off first.
    Residual { peeled: bool },
}

impl<A: Alphabet> Word<A> {
    /// The segments, in order.
    pub fn segments(&self) -> &VecDeque<Segment<A>> {
        &self.segments
    }

    /// The segments, in order, for the caller to rebuild a value from.
    pub fn into_segments(self) -> VecDeque<Segment<A>> {
        self.segments
    }

    /// The word followed by `segment`, kept in normal form: an empty run or window is the identity and vanishes, a run abutting a run merges into it, and a window beginning where the window before it on one base ends fuses with it — `slice(b, s, m) ++ slice(b, s + m, n)` is `slice(b, s, m + n)`.
    ///
    /// **A fused window takes the later window's proof, unchanged.** The later window proved `(s + m) + n <= len(b)` and the fused one needs `s + (m + n) <= len(b)`, which is the same proposition up to a reassociation conversion decides; so the term that proved one proves the other, and nothing here derives anything. The seam is an arithmetic fact, `offset = prev.offset + prev.length`, decided by [`Alphabet::same`]; a run of touching windows collapses left to right into one.
    pub fn push(&mut self, alphabet: &A, segment: Segment<A>) {
        match segment {
            Segment::Run(run) if run.is_empty() => {}
            Segment::Run(run) => match self.segments.back_mut() {
                Some(Segment::Run(head)) => head.extend(run),
                _ => self.segments.push_back(Segment::Run(run)),
            },
            Segment::Window(window) if alphabet.is_zero(&window.length) => {}
            Segment::Window(window) => match self.segments.back_mut() {
                Some(Segment::Window(previous))
                    if previous.base == window.base
                        && alphabet.same(
                            &window.offset,
                            &alphabet.sum(&previous.offset, &previous.length),
                        ) =>
                {
                    previous.length = alphabet.sum(&previous.length, &window.length);
                    previous.proof = window.proof;
                }
                _ => self.segments.push_back(Segment::Window(window)),
            },
            other @ (Segment::Single(_) | Segment::Chunk(_)) => self.segments.push_back(other),
        }
    }

    /// Strip the longest prefix `self` and `other` certainly share, leaving each at its residual, and say what that concludes.
    ///
    /// Runs are matched element by element; singles and chunks whole, by their symbol; and two windows whole where their lengths are one number and their starts one position of one root ([`same_position`]) — so a window of a window, `slice(slice(b, s, l), t, n)`, meets `slice(b, s + t, n)`, and a window of a concatenation meets the window of the operand it lies inside. Two windows sharing a start and differing in length could peel too, but that needs the two lengths ordered, so it is left undecided.
    ///
    /// A strip that stops at two runs whose heads differ is [`Stripped::Impossible`] where the runs are decided and a residual where they are not; one that empties one word decides by whether what the other keeps has a positive length anywhere in it — `x ++ [5]` against the empty word clashes as `[5] ++ x` does.
    pub fn strip_common_prefix(&mut self, alphabet: &A, other: &mut Self) -> Stripped {
        let mut peeled = false;

        loop {
            let common = match (self.segments.front(), other.segments.front()) {
                (Some(Segment::Run(this)), Some(Segment::Run(that))) => this.shared_prefix(that),
                (Some(Segment::Single(this)), Some(Segment::Single(that)))
                | (Some(Segment::Chunk(this)), Some(Segment::Chunk(that)))
                    if this == that =>
                {
                    1
                }
                (Some(Segment::Window(this)), Some(Segment::Window(that)))
                    if alphabet.same(&this.length, &that.length)
                        && same_position(
                            alphabet,
                            (&this.base, &this.offset),
                            (&that.base, &that.offset),
                        ) =>
                {
                    1
                }
                _ => 0,
            };
            if common == 0 {
                break;
            }
            peeled = true;
            self.skip_front(common);
            other.skip_front(common);
        }

        match (self.segments.front(), other.segments.front()) {
            (None, None) => Stripped::Equal,
            (None, Some(_)) => other.against_identity(),
            (Some(_), None) => self.against_identity(),
            (Some(Segment::Run(_)), Some(Segment::Run(_))) if A::Run::DECIDED => {
                Stripped::Impossible
            }
            _ => Stripped::Residual { peeled },
        }
    }

    /// The word without the first `count` elements of its head: the whole head where it is not a run, which the strip takes off as one element, and otherwise that many of the run's, the run going once it is exhausted.
    fn skip_front(&mut self, count: usize) {
        match self.segments.front_mut() {
            Some(Segment::Run(run)) if run.len() > count => run.skip(count),
            _ => {
                self.segments.pop_front();
            }
        }
    }

    /// What this residual concludes against the empty word: a run (never empty, [`Word::push`] drops those) or a single element anywhere in it gives it a positive length, and windows and chunks alone might all be empty.
    fn against_identity(&self) -> Stripped {
        let positive = self
            .segments
            .iter()
            .any(|segment| matches!(segment, Segment::Run(_) | Segment::Single(_)));
        match positive {
            true => Stripped::Impossible,
            false => Stripped::Undecided,
        }
    }

    /// Where `operand` begins in this word: for each chunk that is `operand` itself, the sum of the measures before it — a run's length, a single element's one, a window's length, and [`Alphabet::measure`] of any other chunk.
    ///
    /// An operand is matched whole, by its symbol, as the strip matches a chunk: an operand that is itself several segments is not searched for as a sub-run, and one that differs in spelling declines, the refusing direction.
    pub fn offsets_of(&self, alphabet: &A, operand: &A::Symbol) -> Vec<A::Number> {
        let mut found = Vec::new();
        let mut at = alphabet.count(0);

        for segment in &self.segments {
            at = match segment {
                Segment::Run(run) => alphabet.sum(&at, &alphabet.count(run.len())),
                Segment::Single(_) => alphabet.sum(&at, &alphabet.count(1)),
                Segment::Window(window) => alphabet.sum(&at, &window.length),
                Segment::Chunk(chunk) => {
                    if chunk == operand {
                        found.push(at.clone());
                    }
                    alphabet.sum(&at, &alphabet.measure(chunk))
                }
            };
        }

        found
    }
}

/// Whether reading `this` at `here` and `that` at `there` reads one position: each is rooted through the windows its value is cut from ([`Alphabet::rooted`]), and the two are one where they are one number of one root, or where one root is an operand of the other's concatenation and the position inside it is that number past where the operand begins — `(a ++ b ++ c)` at `len(a) + i` is `b` at `i`.
///
/// **Decided without reading a bound.** A read inside an operand is typed by its own bound, so at every well-typed instantiation the position lies inside the operand, where the concatenation holds the operand's elements unchanged. That is why this is a comparison and never a rewrite: rewriting `(a ++ b)` at `i` into `a` at `i` would owe `i < len(a)`, a proof nothing in hand is.
pub fn same_position<A: Alphabet>(
    alphabet: &A,
    (this, here): (&A::Symbol, &A::Number),
    (that, there): (&A::Symbol, &A::Number),
) -> bool {
    let (this_root, here) = alphabet.rooted(this, here);
    let (that_root, there) = alphabet.rooted(that, there);

    if this_root == that_root {
        return alphabet.same(&here, &there);
    }

    let inside = |outer: &A::Symbol, at: &A::Number, operand: &A::Symbol, within: &A::Number| {
        alphabet
            .offsets(outer, operand)
            .iter()
            .any(|offset| alphabet.same(&alphabet.sum(offset, within), at))
    };

    inside(&this_root, &here, &that_root, &there) || inside(&that_root, &there, &this_root, &here)
}

#[cfg(test)]
mod tests;
