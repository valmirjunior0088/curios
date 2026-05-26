use {
    crate::{cont, wasm},
    std::collections::{HashMap, HashSet},
};

#[derive(Debug, Clone)]
pub struct LocalData {
    pub local_name: wasm::LocalName,
    pub is_nullable: bool,
}

impl LocalData {
    pub fn new(local_name: wasm::LocalName, is_nullable: bool) -> Self {
        Self {
            local_name,
            is_nullable,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockData<'a> {
    pub dispatcher_label: wasm::LabelName,
    pub dispatcher_local: wasm::LocalName,
    pub index: usize,
    pub label_name: wasm::LabelName,
    pub params: Vec<(&'a cont::ValueName, LocalData)>,
    pub region: &'a cont::Region,
}

impl<'a> BlockData<'a> {
    pub fn new(
        dispatcher_label: wasm::LabelName,
        dispatcher_local: wasm::LocalName,
        index: usize,
        block_name: &'a cont::BlockName,
        params: Vec<(&'a cont::ValueName, LocalData)>,
        region: &'a cont::Region,
    ) -> Self {
        Self {
            dispatcher_label,
            dispatcher_local,
            index,
            label_name: wasm::LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }
}

#[derive(Debug)]
pub struct Frame<'a> {
    pub params: HashMap<&'a cont::ValueName, LocalData>,
    pub values: HashMap<&'a cont::ValueName, wasm::LocalName>,
    pub preallocs: HashSet<&'a cont::ValueName>,
    pub blocks: Vec<(&'a cont::BlockName, BlockData<'a>)>,
    pub instrs: Vec<wasm::Instr>,
}

impl<'a> Frame<'a> {
    pub fn new(
        params: HashMap<&'a cont::ValueName, LocalData>,
        preallocs: HashSet<&'a cont::ValueName>,
        blocks: Vec<(&'a cont::BlockName, BlockData<'a>)>,
    ) -> Self {
        Self {
            params,
            values: HashMap::new(),
            preallocs,
            blocks,
            instrs: Default::default(),
        }
    }
}
