use {super::FrameEntropy, crate::cont};

/// A region under construction together with access to the shared name supply.
///
/// `state` is the per-function fresh-name counter (borrowed, long-lived); `builder`
/// is the region being assembled (owned). Sub-regions reborrow the same name supply
/// via [`Emit::subregion`], so freshly-minted blocks share one monotonic namespace.
pub struct Emit<'s> {
    state: &'s mut FrameEntropy,
    builder: RegionBuilder,
}

impl<'s> Emit<'s> {
    pub fn new(state: &'s mut FrameEntropy) -> Self {
        Self {
            state,
            builder: RegionBuilder::new(),
        }
    }

    /// A nested region that draws fresh names from the same supply as `self`.
    pub fn subregion(&mut self) -> Emit<'_> {
        Emit::new(&mut *self.state)
    }

    pub fn fresh_value(&mut self) -> cont::ValueName {
        self.state.fresh_value()
    }

    pub fn fresh_block(&mut self) -> cont::BlockName {
        self.state.fresh_block()
    }

    /// Bind `value` to a fresh name in this region and return that name.
    pub fn fresh(&mut self, value: cont::Value) -> cont::ValueName {
        let name = self.state.fresh_value();
        self.builder.add_value(name.clone(), value);

        name
    }

    pub fn add_prealloc(&mut self, name: cont::ValueName, clsr: cont::ClsrName) {
        self.builder.add_prealloc(name, clsr);
    }

    pub fn add_value(&mut self, name: cont::ValueName, value: cont::Value) {
        self.builder.add_value(name, value);
    }

    pub fn add_block(&mut self, name: cont::BlockName, block: cont::Block) {
        self.builder.add_block(name, block);
    }

    pub fn finish(self, tail: cont::Tail) -> cont::Region {
        self.builder.finish(tail)
    }
}

pub struct RegionBuilder {
    preallocs: Vec<(cont::ValueName, cont::ClsrName)>,
    values: Vec<(cont::ValueName, cont::Value)>,
    blocks: Vec<(cont::BlockName, cont::Block)>,
}

impl RegionBuilder {
    pub fn new() -> Self {
        Self {
            preallocs: vec![],
            values: vec![],
            blocks: vec![],
        }
    }

    pub fn add_prealloc(&mut self, name: cont::ValueName, clsr: cont::ClsrName) {
        self.preallocs.push((name, clsr));
    }

    pub fn add_value(&mut self, name: cont::ValueName, value: cont::Value) {
        self.values.push((name, value));
    }

    pub fn add_block(&mut self, name: cont::BlockName, block: cont::Block) {
        self.blocks.push((name, block));
    }

    pub fn finish(self, tail: cont::Tail) -> cont::Region {
        cont::Region {
            preallocs: self.preallocs,
            values: self.values,
            blocks: self.blocks,
            tail,
        }
    }
}
