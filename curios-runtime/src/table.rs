use {super::Handle, curios_abi::TokenMint, std::collections::HashMap};

/// A host's handle table: a [`TokenMint`] paired with the live-handle map keyed by minted token bytes, generic over the host's resource type `T`. Each host wraps one in a `Mutex` so a mint (take the next token, file the resource) is atomic. The mint's counter never wraps, so a token is never reused — a closed handle's bytes are removed and never minted again, making use-after-close a loud miss rather than a silent alias. The map tracks live handles only, so it is sized by what is currently open, not by how many were ever opened.
pub(crate) struct Table<T> {
    tokens: TokenMint,
    map: HashMap<Vec<u8>, T>,
}

impl<T> Table<T> {
    /// A fresh table: a mint seeded one past the stdio tokens, no live handles.
    pub(crate) fn new() -> Self {
        Self {
            tokens: TokenMint::new(),
            map: HashMap::new(),
        }
    }

    /// Mint a fresh handle for `resource` and file the resource under its token bytes. The bytes are the handle the guest shuttles back; `close` removes them and the mint never reproduces them.
    pub(crate) fn mint(&mut self, resource: T) -> Handle {
        let bytes = self.tokens.mint();
        self.map.insert(bytes.clone(), resource);

        Handle::Other(bytes)
    }

    pub(crate) fn get(&self, handle: &Handle) -> Option<&T> {
        self.map.get(&handle.bytes())
    }

    pub(crate) fn get_mut(&mut self, handle: &Handle) -> Option<&mut T> {
        self.map.get_mut(&handle.bytes())
    }

    /// File `resource` under `handle`, keeping the exact token the guest already holds — used to re-file a handle whose state changed in place (e.g. `connect` turning a socket into a stream).
    pub(crate) fn insert(&mut self, handle: &Handle, resource: T) {
        self.map.insert(handle.bytes(), resource);
    }

    pub(crate) fn remove(&mut self, handle: &Handle) -> Option<T> {
        self.map.remove(&handle.bytes())
    }

    pub(crate) fn contains(&self, handle: &Handle) -> bool {
        self.map.contains_key(&handle.bytes())
    }
}

impl<T> Default for Table<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokens_are_unique_never_reused_and_clear_of_the_stdio_band() {
        let mut table: Table<u32> = Table::new();

        let a = table.mint(10);
        let b = table.mint(20);

        // Distinct live handles get distinct tokens...
        assert_ne!(a.bytes(), b.bytes());
        // ...and minted tokens never collide with stdin/stdout/stderr.
        assert_ne!(a.bytes(), Handle::Stdin.bytes());
        assert_ne!(a.bytes(), Handle::Stdout.bytes());
        assert_ne!(a.bytes(), Handle::Stderr.bytes());

        assert_eq!(table.get(&a), Some(&10));
        assert_eq!(table.get(&b), Some(&20));
    }

    #[test]
    fn use_after_close_is_a_loud_miss_never_an_alias() {
        let mut table: Table<u32> = Table::new();

        let a = table.mint(10);
        // Closing removes the entry and hands the resource back.
        assert_eq!(table.remove(&a), Some(10));
        // Use-after-close misses; double-close is a clean miss, not an alias.
        assert_eq!(table.get(&a), None);
        assert_eq!(table.remove(&a), None);

        // A later mint never reuses the closed token (the counter never wraps), and the stale handle keeps missing rather than aliasing the new entry.
        let b = table.mint(99);
        assert_ne!(b.bytes(), a.bytes());
        assert_eq!(table.get(&a), None);
        assert_eq!(table.get(&b), Some(&99));
    }

    #[test]
    fn stdio_handles_are_never_in_the_table() {
        let table: Table<u32> = Table::new();

        assert_eq!(table.get(&Handle::Stdin), None);
        assert_eq!(table.get(&Handle::Stdout), None);
        assert_eq!(table.get(&Handle::Stderr), None);
    }
}
