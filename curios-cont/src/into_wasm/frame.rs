use {
    crate::{EmissionBlockName, EmissionBody, EmissionValueName},
    std::collections::{HashMap, HashSet},
};

#[derive(Debug, Clone)]
pub(crate) struct LocalData {
    pub local_name: curios_wasm::LocalName,
    pub is_nullable: bool,
}

impl LocalData {
    pub(crate) fn new(local_name: curios_wasm::LocalName, is_nullable: bool) -> Self {
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
        bloink_label: curios_wasm::LabelName,
        bloink_local: curios_wasm::LocalName,
        index: usize,
    },
    NaturalLoop {
        loop_label: curios_wasm::LabelName,
    },
    Direct,
}

#[derive(Debug, Clone)]
pub(crate) struct BlockData<'a> {
    dispatch: Dispatch,
    pub label_name: curios_wasm::LabelName,
    params: Vec<(&'a EmissionValueName, LocalData)>,
    pub region: &'a EmissionBody,
}

impl<'a> BlockData<'a> {
    pub(crate) fn new(
        bloink_label: curios_wasm::LabelName,
        bloink_local: curios_wasm::LocalName,
        index: usize,
        block_name: &'a EmissionBlockName,
        params: Vec<(&'a EmissionValueName, LocalData)>,
        region: &'a EmissionBody,
    ) -> Self {
        Self {
            dispatch: Dispatch::Loop {
                bloink_label,
                bloink_local,
                index,
            },
            label_name: curios_wasm::LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }

    /// A single-target block reached only by forward branches — no dispatcher,
    /// no loop. `enter` branches straight out of `label_name` into the body.
    pub(crate) fn new_direct(
        block_name: &'a EmissionBlockName,
        params: Vec<(&'a EmissionValueName, LocalData)>,
        region: &'a EmissionBody,
    ) -> Self {
        Self {
            dispatch: Dispatch::Direct,
            label_name: curios_wasm::LabelName::from(format!("${}", block_name)),
            params,
            region,
        }
    }

    pub(crate) fn new_natural_loop(
        block_name: &'a EmissionBlockName,
        params: Vec<(&'a EmissionValueName, LocalData)>,
        region: &'a EmissionBody,
    ) -> Self {
        let label_name = curios_wasm::LabelName::from(format!("${}", block_name));
        Self {
            dispatch: Dispatch::NaturalLoop {
                loop_label: label_name.clone(),
            },
            label_name,
            params,
            region,
        }
    }

    pub(crate) fn bind(&self, arity: usize) -> Vec<curios_wasm::Instr> {
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
            .map(|(_, local_data)| curios_wasm::Instr::LocalSet {
                local_name: local_data.local_name.clone(),
            })
            .collect()
    }

    pub(crate) fn enter(&self, arity: usize) -> Vec<curios_wasm::Instr> {
        let bind = self.bind(arity);

        let branch = match &self.dispatch {
            Dispatch::Loop {
                bloink_label,
                bloink_local,
                index,
            } => vec![
                curios_wasm::Instr::I32Const {
                    value: *index as i32,
                },
                curios_wasm::Instr::LocalSet {
                    local_name: bloink_local.clone(),
                },
                curios_wasm::Instr::Br {
                    label_name: bloink_label.clone(),
                },
            ],
            Dispatch::NaturalLoop { loop_label } => vec![curios_wasm::Instr::Br {
                label_name: loop_label.clone(),
            }],
            Dispatch::Direct => vec![curios_wasm::Instr::Br {
                label_name: self.label_name.clone(),
            }],
        };

        bind.into_iter().chain(branch).collect()
    }

    pub(crate) fn params_map(&self) -> HashMap<&'a EmissionValueName, LocalData> {
        self.params.iter().cloned().collect()
    }
}

#[derive(Debug)]
pub(crate) struct Frame<'a> {
    pub params: HashMap<&'a EmissionValueName, LocalData>,
    pub values: HashMap<&'a EmissionValueName, curios_wasm::LocalName>,
    pub shells: HashSet<&'a EmissionValueName>,
    pub blocks: Vec<(&'a EmissionBlockName, BlockData<'a>)>,
    pub instrs: Vec<curios_wasm::Instr>,
}

impl<'a> Frame<'a> {
    pub(crate) fn new(
        params: HashMap<&'a EmissionValueName, LocalData>,
        shells: HashSet<&'a EmissionValueName>,
        blocks: Vec<(&'a EmissionBlockName, BlockData<'a>)>,
    ) -> Self {
        Self {
            params,
            values: HashMap::new(),
            shells,
            blocks,
            instrs: Default::default(),
        }
    }
}
