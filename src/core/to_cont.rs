use {
    crate::{cont, core},
    std::marker::PhantomData,
};

struct Entropy<T> {
    entropy: usize,
    prefix: &'static str,
    marker: PhantomData<T>,
}

impl Entropy<cont::ValueName> {
    fn new() -> Self {
        Self {
            entropy: 0,
            prefix: "v",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::BlockName> {
    fn new() -> Self {
        Self {
            entropy: 0,
            prefix: "b",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::FuncName> {
    fn new() -> Self {
        Self {
            entropy: 0,
            prefix: "f",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::ClsrName> {
    fn new() -> Self {
        Self {
            entropy: 0,
            prefix: "c",
            marker: PhantomData,
        }
    }
}

impl<T> Entropy<T>
where
    T: From<String>,
{
    pub fn fresh(&mut self) -> T {
        let entropy = self.entropy;
        self.entropy += 1;

        T::from(format!("{}{entropy}", self.prefix))
    }
}

pub fn to_cont(core_term: &core::ErasedTerm) -> cont::Module {
    todo!()
}
