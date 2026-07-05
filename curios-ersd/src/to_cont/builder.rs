use {
    super::FrameEntropy,
    curios_cont::{Block, BlockName, ClsrName, Region, Tail, Value, ValueName},
};

/// A region under construction together with access to the shared name supply.
///
/// `state` is the per-function fresh-name counter (borrowed, long-lived); `builder`
/// is the region being assembled (owned). Sub-regions reborrow the same name supply
/// via [`Emit::subregion`], so freshly-minted blocks share one monotonic namespace.
pub(super) struct Emit<'s> {
    state: &'s mut FrameEntropy,
    builder: RegionBuilder,
}

impl<'s> Emit<'s> {
    pub(super) fn new(state: &'s mut FrameEntropy) -> Self {
        Self {
            state,
            builder: RegionBuilder::new(),
        }
    }

    /// A nested region that draws fresh names from the same supply as `self`.
    pub(super) fn subregion(&mut self) -> Emit<'_> {
        Emit::new(&mut *self.state)
    }

    pub(super) fn fresh_value(&mut self) -> ValueName {
        self.state.fresh_value()
    }

    pub(super) fn fresh_block(&mut self) -> BlockName {
        self.state.fresh_block()
    }

    /// Bind `value` to a fresh name in this region and return that name.
    pub(super) fn fresh(&mut self, value: Value) -> ValueName {
        let name = self.state.fresh_value();
        self.builder.add_value(name.clone(), value);

        name
    }

    pub(super) fn add_prealloc(&mut self, name: ValueName, clsr: ClsrName) {
        self.builder.add_prealloc(name, clsr);
    }

    pub(super) fn add_value(&mut self, name: ValueName, value: Value) {
        self.builder.add_value(name, value);
    }

    pub(super) fn add_block(&mut self, name: BlockName, block: Block) {
        self.builder.add_block(name, block);
    }

    pub(super) fn finish(self, tail: Tail) -> Region {
        self.builder.finish(tail)
    }
}

pub(super) struct RegionBuilder {
    preallocs: Vec<(ValueName, ClsrName)>,
    values: Vec<(ValueName, Value)>,
    blocks: Vec<(BlockName, Block)>,
}

impl RegionBuilder {
    pub(super) fn new() -> Self {
        Self {
            preallocs: vec![],
            values: vec![],
            blocks: vec![],
        }
    }

    pub(super) fn add_prealloc(&mut self, name: ValueName, clsr: ClsrName) {
        self.preallocs.push((name, clsr));
    }

    pub(super) fn add_value(&mut self, name: ValueName, value: Value) {
        self.values.push((name, value));
    }

    pub(super) fn add_block(&mut self, name: BlockName, block: Block) {
        self.blocks.push((name, block));
    }

    pub(super) fn finish(self, tail: Tail) -> Region {
        Region {
            preallocs: self.preallocs,
            values: self.values,
            blocks: self.blocks,
            tail,
        }
    }
}
