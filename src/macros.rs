macro_rules! name {
    ($name:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            pub string: String,
        }

        impl<A: Into<String>> From<A> for $name {
            fn from(string: A) -> Self {
                Self {
                    string: string.into(),
                }
            }
        }
    };
}

pub(super) use name;
