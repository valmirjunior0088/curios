//! A toy alphabet the word and measure suites share: symbols are names, a number is a constant beside a multiset of names, and a base may be declared a window of another or a concatenation of chunks.

use {
    crate::{Alphabet, Segment, Word},
    std::collections::HashMap,
};

/// A constant beside a sorted multiset of symbolic names.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Number {
    pub(crate) constant: usize,
    pub(crate) names: Vec<&'static str>,
}

pub(crate) fn n(constant: usize, names: &[&'static str]) -> Number {
    let mut names = names.to_vec();
    names.sort();
    Number { constant, names }
}

/// Bases that are windows of another base, and roots that are concatenations of named chunks.
#[derive(Default)]
pub(crate) struct Toy {
    pub(crate) windows: HashMap<&'static str, (&'static str, Number)>,
    pub(crate) concatenations: HashMap<&'static str, Vec<&'static str>>,
}

impl Alphabet for Toy {
    type Run = Vec<u32>;
    type Symbol = &'static str;
    type Number = Number;
    type Proof = &'static str;

    fn count(&self, count: usize) -> Number {
        n(count, &[])
    }

    fn is_zero(&self, number: &Number) -> bool {
        *number == n(0, &[])
    }

    fn sum(&self, left: &Number, right: &Number) -> Number {
        let names = [left.names.as_slice(), right.names.as_slice()].concat();
        n(left.constant + right.constant, &names)
    }

    fn same(&self, left: &Number, right: &Number) -> bool {
        left == right
    }

    fn difference(&self, minuend: &Number, subtrahend: &Number) -> Option<Number> {
        let constant = minuend.constant.checked_sub(subtrahend.constant)?;
        let mut names = minuend.names.clone();
        for name in &subtrahend.names {
            let at = names.iter().position(|held| held == name)?;
            names.remove(at);
        }
        Some(n(constant, &names))
    }

    fn measure(&self, chunk: &&'static str) -> Number {
        n(0, &[chunk])
    }

    fn rooted(&self, base: &&'static str, position: &Number) -> (&'static str, Number) {
        let (mut base, mut position) = (*base, position.clone());
        while let Some((inner, start)) = self.windows.get(base) {
            position = self.sum(start, &position);
            base = inner;
        }
        (base, position)
    }

    fn offsets(&self, root: &&'static str, operand: &&'static str) -> Vec<Number> {
        let Some(chunks) = self.concatenations.get(root) else {
            return Vec::new();
        };
        let mut word = Word::default();
        for chunk in chunks {
            word.push(self, Segment::Chunk(*chunk));
        }
        word.offsets_of(self, operand)
    }
}
