macro_rules! name {
    ($name:ident) => {
        #[derive(Debug, PartialEq, Eq, Clone, Hash)]
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

name!(ValueName);
name!(BlockName);
name!(ClsrName);
name!(FuncName);
