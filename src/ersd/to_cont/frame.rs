use {
    crate::{Entropy, cont},
    std::collections::HashMap,
};

#[derive(Debug)]
pub struct FrameEntropy {
    values: Entropy<cont::ValueName>,
    blocks: Entropy<cont::BlockName>,
}

impl FrameEntropy {
    pub fn new() -> (Self, cont::BlockName) {
        let blocks = Entropy::<cont::BlockName>::new();
        let resume = blocks.fresh();

        (
            Self {
                values: Entropy::<cont::ValueName>::new(),
                blocks,
            },
            resume,
        )
    }

    pub fn fresh_value(&mut self) -> cont::ValueName {
        self.values.fresh()
    }

    pub fn fresh_block(&mut self) -> cont::BlockName {
        self.blocks.fresh()
    }
}

#[derive(Debug, Default, Clone)]
pub struct Frame {
    bindings: HashMap<String, cont::ValueName>,
}

impl Frame {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn find(&self, name: &str) -> cont::ValueName {
        self.bindings
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("`to_cont` lacks value `{name}`"))
    }

    pub fn push(&mut self, name: String, value: cont::ValueName) {
        self.bindings.insert(name, value);
    }

    pub fn extended<I>(&self, bindings: I) -> Self
    where
        I: IntoIterator<Item = (String, cont::ValueName)>,
    {
        let mut frame = self.clone();

        bindings
            .into_iter()
            .for_each(|(name, value)| frame.push(name, value));

        frame
    }
}
