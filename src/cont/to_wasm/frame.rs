use {
    crate::{cont, wasm},
    std::collections::HashMap,
};

#[derive(Clone)]
pub struct BlockData<'a> {
    pub dispatcher_label: wasm::LabelName,
    pub dispatcher_local: wasm::LocalName,
    pub index: usize,
    pub label_name: wasm::LabelName,
    pub params: Vec<(&'a cont::ValueName, (wasm::LocalName, bool))>,
    pub region: &'a cont::Region,
}

impl<'a> BlockData<'a> {
    pub fn new(
        dispatcher_label: wasm::LabelName,
        dispatcher_local: wasm::LocalName,
        index: usize,
        block_name: &'a cont::BlockName,
        params: Vec<(&'a cont::ValueName, (wasm::LocalName, bool))>,
        region: &'a cont::Region,
    ) -> Self {
        Self {
            dispatcher_label,
            dispatcher_local,
            index,
            label_name: wasm::LabelName::from(format!("${}", block_name.string)),
            params,
            region,
        }
    }
}

pub struct Frame<'a> {
    pub params: HashMap<&'a cont::ValueName, (wasm::LocalName, bool)>,
    pub values: HashMap<&'a cont::ValueName, wasm::LocalName>,
    pub blocks: Vec<(&'a cont::BlockName, BlockData<'a>)>,
    pub instrs: Vec<wasm::Instr>,
}

impl<'a> Frame<'a> {
    pub fn new(
        params: HashMap<&'a cont::ValueName, (wasm::LocalName, bool)>,
        values: HashMap<&'a cont::ValueName, wasm::LocalName>,
        blocks: Vec<(&'a cont::BlockName, BlockData<'a>)>,
    ) -> Self {
        Self {
            params,
            values,
            blocks,
            instrs: Default::default(),
        }
    }
}
