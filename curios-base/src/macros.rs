#[macro_export]
macro_rules! name {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            string: String,
        }

        impl $name {
            pub fn as_str(&self) -> &str {
                &self.string
            }

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
