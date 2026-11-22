//! What the cabinet does across two compilations of the same project.
//!
//! Every test here compiles for real, twice, and reads the answer off the fold's own progress events rather than off the cabinet's internals: whether a unit was reused is exactly the question a caller asks, and asserting on an object's address would pass just as happily with verification broken.

mod test_support;

mod by_name;
mod compilers;
mod object;
mod reuse;
