use {
    curios_wasm::{Instr, LabelName, LocalName},
    std::collections::{HashMap, HashSet},
};

#[derive(Debug, Clone)]
pub(super) struct LocalData {
    pub local_name: LocalName,
    pub is_nullable: bool,
}

impl LocalData {
    pub(super) fn new(local_name: LocalName, is_nullable: bool) -> Self {
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
        bloink_label: LabelName,
        bloink_local: LocalName,
        index: usize,
    },
    Direct,
}

#[derive(Debug, Clone)]
pub(super) struct BlockData<'a> {
    dispatch: Dispatch,
    pub label_name: LabelName,
    params: Vec<(&'a crate::ValueName, LocalData)>,
    pub region: &'a crate::Region,
}

impl<'a> BlockData<'a> {
    pub(super) fn new(
        bloink_label: LabelName,
        bloink_local: LocalName,
        index: usize,
        block_name: &'a crate::BlockName,
        params: Vec<(&'a crate::ValueName, LocalData)>,
        region: &'a crate::Region,
    ) -> Self {
        Self {
            dispatch: Dispatch::Loop {
                bloink_label,
                bloink_local,
                index,
            },
            label_name: LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }

    /// A single-target block reached only by forward branches — no dispatcher,
    /// no loop. `enter` branches straight out of `label_name` into the body.
    pub(super) fn new_direct(
        block_name: &'a crate::BlockName,
        params: Vec<(&'a crate::ValueName, LocalData)>,
        region: &'a crate::Region,
    ) -> Self {
        Self {
            dispatch: Dispatch::Direct,
            label_name: LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }

    pub(super) fn enter(&self, arity: usize) -> Vec<Instr> {
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
            .map(|(_, local_data)| Instr::LocalSet {
                local_name: local_data.local_name.clone(),
            });

        let branch = match &self.dispatch {
            Dispatch::Loop {
                bloink_label,
                bloink_local,
                index,
            } => vec![
                Instr::I32Const {
                    value: *index as i32,
                },
                Instr::LocalSet {
                    local_name: bloink_local.clone(),
                },
                Instr::Br {
                    label_name: bloink_label.clone(),
                },
            ],
            Dispatch::Direct => vec![Instr::Br {
                label_name: self.label_name.clone(),
            }],
        };

        bind.chain(branch).collect()
    }

    pub(super) fn params_map(&self) -> HashMap<&'a crate::ValueName, LocalData> {
        self.params.iter().cloned().collect()
    }
}

#[derive(Debug)]
pub(super) struct Frame<'a> {
    pub params: HashMap<&'a crate::ValueName, LocalData>,
    pub values: HashMap<&'a crate::ValueName, LocalName>,
    pub preallocs: HashSet<&'a crate::ValueName>,
    pub blocks: Vec<(&'a crate::BlockName, BlockData<'a>)>,
    pub instrs: Vec<Instr>,
}

impl<'a> Frame<'a> {
    pub(super) fn new(
        params: HashMap<&'a crate::ValueName, LocalData>,
        preallocs: HashSet<&'a crate::ValueName>,
        blocks: Vec<(&'a crate::BlockName, BlockData<'a>)>,
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
