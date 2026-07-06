/// Declares a string-newtype name type: `name!(Foo)` generates `pub struct Foo` wrapping a `String` with `as_str`/`as_string` accessors, `From<impl Into<String>>`, and `Display` — so names from different namespaces cannot be confused however identical their text. The two-argument form `name!(Foo, "f")` additionally implements `Mint` by spelling the counter after the prefix, making `Entropy<Foo>` a gensym source for `f0`, `f1`, ... Every IR crate declares its namespaces this way (see the `names.rs` in curios-core, -ersd, -cont, -wasm).
#[macro_export]
macro_rules! name {
    ($name:ident) => {
        #[doc = concat!("A `", stringify!($name), "`: a plain string spelling behind a dedicated type, so names from different namespaces cannot be confused however identical their text.")]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            string: String,
        }

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

    ($name:ident, $prefix:literal) => {
        $crate::name!($name);

        impl $crate::Mint for $name {
            fn mint(entropy: usize) -> Self {
                Self::from(format!(concat!($prefix, "{}"), entropy))
            }
        }
    };
}
