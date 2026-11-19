//! A value whose every write is counted, so that what is read off it can be filed with the generation it was read at and told from what a later write changed.

/// A value reached for writing through one door, [`Written::write`], which counts each write; reading it through [`Deref`](std::ops::Deref) counts nothing.
#[derive(Debug, Clone, Default)]
pub struct Written<T> {
    generation: u64,
    value: T,
}

impl<T> Written<T> {
    /// How many writes the value has taken: two readings at one generation read one value.
    pub fn generation(&self) -> u64 {
        self.generation
    }

    /// The value for writing, counted as a write whether or not the caller changes it.
    pub fn write(&mut self) -> &mut T {
        self.generation += 1;
        &mut self.value
    }
}

impl<T> std::ops::Deref for Written<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}
