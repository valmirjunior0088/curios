//! What a census asks of a context and a module's table that no lowering does: a body's scope bound one label at a time, as its declarations and its `use` declarations left it, and a table holding the labels a record names and no others.

use super::*;

impl Context<'_> {
    /// Bind `label`, a binding where `binding` and a module otherwise, to `target` in this body's scope, as a `use` brings one in where `imported`: a label a `use` brought in marks the site it came through as used, and no guard answers for it at a path's head (`Context::guarded`).
    pub fn bound_as(
        &mut self,
        label: &str,
        binding: bool,
        target: Qualifier,
        imported: bool,
    ) -> Result<(), Error> {
        if imported {
            self.open_site(UseSite {
                body: self.prefix,
                line: None,
                span: None,
                what: UseSiteKind::Glob(String::new()),
                exported: false,
                used: false,
            });
            let site = self.current_site.expect("a site just opened");
            self.close_site();
            match binding {
                true => self.binding_sites.insert(label.to_string(), site),
                false => self.qualifier_sites.insert(label.to_string(), site),
            };
        }

        match binding {
            true => self.insert_binding(label.to_string(), None, target),
            false => self.insert_scope(label.to_string(), None, target),
        }
    }
}

impl ModuleInfo {
    /// Declare `label` as `declares` says, beside what this module holds already.
    pub(crate) fn holding(&mut self, label: &str, declares: Declares) {
        match declares {
            Declares::Child(info) => {
                self.children.insert(label.to_string(), info);
            }
            Declares::Binding(vis_pub) => {
                self.bindings.insert(label.to_string(), vis_pub);
            }
        }
    }
}
