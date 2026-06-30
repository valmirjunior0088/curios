use {
    crate::wasm::{FuncName, LabelName},
    std::io::Result,
};

#[derive(Debug)]
pub enum State<'f, 'l> {
    Const,
    Func {
        func_name: &'f FuncName,
        label_names: Vec<&'l LabelName>,
    },
}

impl<'f, 'l> State<'f, 'l> {
    pub fn new_const() -> Self {
        Self::Const
    }

    pub fn new_func(func_name: &'f FuncName, label_name: &'l LabelName) -> Self {
        Self::Func {
            func_name,
            label_names: vec![label_name],
        }
    }

    pub fn owner(&self) -> &'f FuncName {
        match self {
            Self::Const => panic!("`State` is const"),
            Self::Func { func_name, .. } => func_name,
        }
    }

    pub fn enter_scope(&mut self, label_name: &'l LabelName) {
        match self {
            Self::Const => {
                panic!("`State` is const");
            }
            Self::Func { label_names, .. } => {
                label_names.push(label_name);
            }
        }
    }

    pub fn leave_scope(&mut self) {
        match self {
            Self::Const => {
                panic!("`State` is const");
            }
            Self::Func { label_names, .. } => {
                label_names.pop();
            }
        }
    }

    pub fn scoped<T, F>(&mut self, label_name: &'l LabelName, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.enter_scope(label_name);
        let result = f(self);
        self.leave_scope();

        result
    }

    pub fn resolve(&self, target_name: &LabelName) -> usize {
        match self {
            Self::Const => panic!("`State` is const"),
            Self::Func { label_names, .. } => label_names
                .iter()
                .rev()
                .position(|&label_name| target_name == label_name)
                .unwrap_or_else(|| panic!("`State` lacks label `{}`", target_name)),
        }
    }
}
