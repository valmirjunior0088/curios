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

/// How entering a block branches to its body. A region with multiple blocks (or a
/// single block with a back-edge) trampolines through a `loop`: `enter` sets the
/// dispatcher index and branches to the loop, whose `br_table` then selects the
/// block. A single block with no back-edge instead branches straight out of its own
/// label — a plain forward `br` — so it needs neither index nor loop.
#[derive(Debug, Clone)]
enum Dispatch {
    Loop {
        bloink_label: wasm::LabelName,
        bloink_local: wasm::LocalName,
        index: usize,
    },
    Direct,
}

#[derive(Debug, Clone)]
pub struct BlockData<'a> {
    dispatch: Dispatch,
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
            dispatch: Dispatch::Loop {
                bloink_label,
                bloink_local,
                index,
            },
            label_name: wasm::LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }

    /// A single-target block reached only by forward branches — no dispatcher,
    /// no loop. `enter` branches straight out of `label_name` into the body.
    pub fn new_direct(
        block_name: &'a cont::BlockName,
        params: Vec<(&'a cont::ValueName, LocalData)>,
        region: &'a cont::Region,
    ) -> Self {
        Self {
            dispatch: Dispatch::Direct,
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

        let bind = self
            .params
            .iter()
            .rev()
            .map(|(_, local_data)| wasm::Instr::LocalSet {
                local_name: local_data.local_name.clone(),
            });

        let branch = match &self.dispatch {
            Dispatch::Loop {
                bloink_label,
                bloink_local,
                index,
            } => vec![
                wasm::Instr::I32Const {
                    value: *index as i32,
                },
                wasm::Instr::LocalSet {
                    local_name: bloink_local.clone(),
                },
                wasm::Instr::Br {
                    label_name: bloink_label.clone(),
                },
            ],
            Dispatch::Direct => vec![wasm::Instr::Br {
                label_name: self.label_name.clone(),
            }],
        };

        bind.chain(branch).collect()
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
