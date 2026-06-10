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
    bloink_label: wasm::LabelName,
    bloink_local: wasm::LocalName,
    index: usize,
    pub label_name: wasm::LabelName,
    params: Vec<(&'a cont::ValueName, LocalData)>,
    pub region: &'a cont::Region,
}

impl<'a> BlockData<'a> {
    pub fn new(
        bloink_label: wasm::LabelName,
        bloink_local: wasm::LocalName,
        index: usize,
        block_name: &'a cont::BlockName,
        params: Vec<(&'a cont::ValueName, LocalData)>,
        region: &'a cont::Region,
    ) -> Self {
        Self {
            bloink_label,
            bloink_local,
            index,
            label_name: wasm::LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }

    pub fn enter(&self, arity: usize) -> Vec<wasm::Instr> {
        assert_eq!(
            self.params.len(),
            arity,
            "block `{}` expects {} params, got {}",
            self.label_name,
            self.params.len(),
            arity,
        );

        self.params
            .iter()
            .rev()
            .map(|(_, local_data)| wasm::Instr::LocalSet {
                local_name: local_data.local_name.clone(),
            })
            .chain([
                wasm::Instr::I32Const {
                    value: self.index as i32,
                },
                wasm::Instr::LocalSet {
                    local_name: self.bloink_local.clone(),
                },
                wasm::Instr::Br {
                    label_name: self.bloink_label.clone(),
                },
            ])
            .collect()
    }

    pub fn params_map(&self) -> HashMap<&'a cont::ValueName, LocalData> {
        self.params.iter().cloned().collect()
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
