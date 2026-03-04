use {crate::cont, std::marker::PhantomData};

#[derive(Debug)]
pub struct Entropy<T> {
    entropy: usize,
    prefix: &'static str,
    marker: PhantomData<T>,
}

impl Entropy<cont::ValueName> {
    pub fn new() -> Self {
        Self {
            entropy: 0,
            prefix: "v",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::BlockName> {
    pub fn new() -> Self {
        Self {
            entropy: 0,
            prefix: "b",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::ClsrName> {
    pub fn new() -> Self {
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
