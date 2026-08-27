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
mod tests;
