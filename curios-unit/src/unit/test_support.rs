//! What a suite asks of a unit that no product does: the unit holding another record.

use super::*;

impl Unit {
    /// This unit holding `certification` as the certifier's record of it: how a history of the soundness board withholds part or all of a filed unit's record, every verdict the record still states being one a walk concluded.
    pub fn with_certification(mut self, certification: Certification) -> Self {
        self.certification = Rc::new(certification);
        self
    }
}
