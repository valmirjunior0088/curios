//! The binder lints' suite, one file per lint, and `unused-dependency`'s: each lowers a fixture and reads the lints decided of it.

mod test_support;

mod shadowed_parameter;
mod unused_binder;
mod unused_dependency;
mod unused_let;
