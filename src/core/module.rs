use super::Term;

pub struct Item {
    pub label: String,
    pub type_: Term,
    pub body: Term,
}

pub struct Module {
    items: Vec<Item>,
}

impl Module {
    pub fn items(&self) -> &[Item] {
        &self.items
    }
}
