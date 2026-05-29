use {super::FrameEntropy, crate::cont};

pub fn emit_fresh_value(
    state: &mut FrameEntropy,
    builder: &mut RegionBuilder,
    value: cont::Value,
) -> cont::ValueName {
    let name = state.fresh_value();
    builder.add_value(name.clone(), value);

    name
}

pub struct RegionBuilder {
    preallocs: Vec<(cont::ValueName, cont::Prealloc)>,
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

    pub fn add_prealloc(&mut self, name: cont::ValueName, prealloc: cont::Prealloc) {
        self.preallocs.push((name, prealloc));
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
