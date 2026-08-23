/// Declares a string-newtype name type: `name!(Foo)` generates `pub struct Foo` wrapping a `String` with `as_str`/`as_string` accessors, `From<impl Into<String>>`, and `Display` — so names from different namespaces cannot be confused however identical their text. The two-argument form `name!(Foo, "f")` additionally implements `Mint` by spelling the counter after the prefix, making `Entropy<Foo>` a gensym source for `f0`, `f1`, ... Every IR crate declares its namespaces this way (see the `names.rs` in curios-elab, -ersd, -cont, -wasm).
///
/// No form derives `PartialOrd`/`Ord`. Ordering a name is ordering its *spelling*, and a name's spelling is identity and rendering only — never a source of behavior. Deriving it once let a `BTreeMap<Atom, _>` make constructor collation order the emitted runtime tag order, so renaming a case silently renumbered the tags of every case it sorted past. Without the derive, a `BTreeMap`/`BTreeSet` keyed on a name is a compile error rather than a convention someone has to remember; reach for the hash collections where a name is genuinely just a key, and carry an explicit sequence where the order is load-bearing.
#[macro_export]
macro_rules! name {
    // The shared impl surface. Both declaration forms delegate here, so the impls cannot drift between them.
    (@impls $name:ident) => {
        impl $name {
            /// The name's spelling, borrowed.
            pub fn as_str(&self) -> &str {
                &self.string
            }

            /// The name's spelling, as an owned `String`.
            pub fn as_string(&self) -> String {
                self.string.clone()
            }
        }

        impl<A: Into<String>> From<A> for $name {
            fn from(string: A) -> Self {
                Self {
                    string: string.into(),
                }
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.write_str(&self.string)
            }
        }
    };

    ($name:ident; archive) => {
        #[doc = concat!("A `", stringify!($name), "`: a plain string spelling behind a dedicated type, so names from different namespaces cannot be confused however identical their text.")]
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        #[curios_archive::archived(derive(PartialEq, Eq, Hash))]
        pub struct $name {
            string: String,
        }

        $crate::name!(@impls $name);
    };

    ($name:ident) => {
        #[doc = concat!("A `", stringify!($name), "`: a plain string spelling behind a dedicated type, so names from different namespaces cannot be confused however identical their text.")]
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name {
            string: String,
        }

        $crate::name!(@impls $name);
    };

    ($name:ident, $prefix:literal) => {
        $crate::name!($name);

        impl $crate::Mint for $name {
            fn mint(entropy: usize) -> Self {
                Self::from(format!(concat!($prefix, "{}"), entropy))
            }
        }
    };
}

/// Declares a `u32`-newtype identity type: `id!(Foo, "f")` generates a `pub struct Foo(pub(crate) u32)` with the standard identity derives, an `index()` accessor, and a `Display` that spells the prefix before the raw index (`f0`, `f1`, ...). The three-argument `id!(Foo, "f", mint)` form additionally implements `Mint`, making `Entropy<Foo>` a gensym source. IR crates that key arenas or gensym stable `u32` identities declare them this way, the numeric twin of the string-spelled `name!`.
#[macro_export]
macro_rules! id {
    // The shared impl surface. Both declaration forms delegate here, so the impls cannot drift between them.
    (@impls $name:ident, $prefix:literal) => {
        impl $name {
            /// The identity's raw arena index.
            ///
            /// Forwards to the `ArenaId` method rather than repeating it. An inherent method shadows a trait one at every concrete call site, so two bodies here would let a call on `Foo` and a call through `I: ArenaId` answer differently with nothing to catch it.
            pub fn index(self) -> usize {
                <Self as $crate::ArenaId>::index(self)
            }
        }

        impl $crate::ArenaId for $name {
            fn from_index(index: usize) -> Self {
                Self(u32::try_from(index).unwrap_or_else(|_| {
                    panic!(concat!(stringify!($name), " identity space exhausted"))
                }))
            }

            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, concat!($prefix, "{}"), self.0)
            }
        }
    };

    ($name:ident, $prefix:literal) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub(crate) u32);

        $crate::id!(@impls $name, $prefix);
    };

    ($name:ident, $prefix:literal; archive) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
        pub struct $name(pub(crate) u32);

        $crate::id!(@impls $name, $prefix);
    };

    ($name:ident, $prefix:literal, mint) => {
        $crate::id!($name, $prefix);

        impl $crate::Mint for $name {
            // Through the minting intrinsic rather than building the newtype again: one narrowing, and the exhaustion panic names the identity type instead of saying only that some space ran out.
            fn mint(entropy: usize) -> Self {
                <Self as $crate::ArenaId>::from_index(entropy)
            }
        }
    };
}
