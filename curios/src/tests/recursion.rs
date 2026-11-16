//! Recursion is implicit and a group is declared: a definition may mention itself, at the top level and locally, and definitions that mention one another are declared as one `let … and …;` group.
//!
//! The refusals carry the rule's weight. A cycle the source did not declare is refused by name with the way out, by the elaborator, which asks for each declaration when it is read and so is the one place that sees a cycle through a name, a registry entry or a witness alike; a value that reads itself while it is being computed is refused where the erased program is verified, since only there can a read be told from a knot forced by need; and a local binding that names itself must be a plain, typed name — which is what turns the shadowing idiom `let n = n + 1;` into an error that says so, since the new binding is in scope of its own value.

mod cycle;
mod declaration;
mod descent;
mod group;
