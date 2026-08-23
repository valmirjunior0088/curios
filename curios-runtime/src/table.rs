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

    /// Take the resource filed under `handle` if `select` claims it — `Ok(value)` hands the value out and leaves the handle free, `Err(resource)` re-files what it declined and answers `None`. A handle that is not filed answers `None` without consulting `select`.
    ///
    /// **The restore is why this is one operation rather than a `remove` and an `insert` at the caller.** A caller that removes before it can tell whether it wants what came out has to put a declined resource back, and a caller that forgets makes a handle the guest still holds vanish: a live socket answering `NotFound` on its next use, with the resource dropped. Stating it here keeps that obligation with the map it is about, instead of once per caller that transitions a handle in place.
    pub(crate) fn take_if<U>(
        &mut self,
        handle: &Handle,
        select: impl FnOnce(T) -> Result<U, T>,
    ) -> Option<U> {
        match select(self.remove(handle)?) {
            Ok(value) => Some(value),
            Err(declined) => {
                self.insert(handle, declined);

                None
            }
        }
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
    fn a_declined_resource_stays_filed_under_its_handle() {
        let mut table: Table<u32> = Table::new();

        let a = table.mint(10);

        // Declining hands the resource back to the table rather than to the caller: the handle still resolves, and to the same value.
        let declined = table.take_if(&a, |value| match value {
            10 => Err(10),
            other => Ok(other),
        });
        assert_eq!(declined, None);
        assert_eq!(table.get(&a), Some(&10));

        // Claiming takes it out, so the handle misses afterwards — the transition `connect`/`start_tls` perform.
        assert_eq!(
            table.take_if(&a, |value| Ok::<u32, u32>(value + 1)),
            Some(11)
        );
        assert_eq!(table.get(&a), None);

        // A handle that is not filed never reaches `select`.
        let missing = table.take_if(&a, |_: u32| -> Result<u32, u32> {
            panic!("select must not run for a handle the table does not hold")
        });
        assert_eq!(missing, None);
    }

    #[test]
    fn stdio_handles_are_never_in_the_table() {
        let table: Table<u32> = Table::new();

        assert_eq!(table.get(&Handle::Stdin), None);
        assert_eq!(table.get(&Handle::Stdout), None);
        assert_eq!(table.get(&Handle::Stderr), None);
    }
}
