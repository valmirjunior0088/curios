use {
    curios_base::Entropy,
    curios_cont::{BlockName, ValueName},
    std::collections::HashMap,
};

#[derive(Debug)]
pub(super) struct FrameEntropy {
    values: Entropy<ValueName>,
    blocks: Entropy<BlockName>,
}

impl FrameEntropy {
    pub(super) fn new() -> (Self, BlockName) {
        let blocks = Entropy::<BlockName>::new();
        let resume = blocks.fresh();

        (
            Self {
                values: Entropy::<ValueName>::new(),
                blocks,
            },
            resume,
        )
    }

    pub(super) fn fresh_value(&mut self) -> ValueName {
        self.values.fresh()
    }

    pub(super) fn fresh_block(&mut self) -> BlockName {
        self.blocks.fresh()
    }
}

#[derive(Debug, Default, Clone)]
pub(super) struct Frame {
    bindings: HashMap<String, ValueName>,
}

impl Frame {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn find(&self, name: &str) -> ValueName {
        self.bindings
            .get(name)
            .cloned()
            .unwrap_or_else(|| panic!("`to_cont` lacks value `{name}`"))
    }

    pub(super) fn push(&mut self, name: String, value: ValueName) {
        self.bindings.insert(name, value);
    }

    pub(super) fn extended<I>(&self, bindings: I) -> Self
    where
        I: IntoIterator<Item = (String, ValueName)>,
    {
        let mut frame = self.clone();

        bindings
            .into_iter()
            .for_each(|(name, value)| frame.push(name, value));

        frame
    }
}
