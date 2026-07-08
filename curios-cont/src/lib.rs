//! The continuation IR — the compiler's last stop before wasm. `curios_ersd::to_cont` lowers the erased first-order IR into a [`Module`] of functions, closures, and consts whose bodies are region trees: straight-line bindings of pure [`Value`]s ended by a single [`Tail`] transfer, with all control flow — jumps, matches, calls, host ops — explicit. Every call names the block that receives its result, and returning is a jump to the body's `resume` sentinel, so a tail call is a syntactic condition (`resume` == the sentinel) rather than an analysis; effects exist only as `Tail::Host`/`Tail::Cell` transfers, never inside a binding.
//!
//! The crate has three faces: the IR itself (`module`, with the `ValueName`/`BlockName`/`ClsrName`/`FuncName` namespaces minted by curios-base's `name!` in `names`), the in-place optimization pipeline [`optimize()`](optimize), and [`to_wasm()`], which emits the optimized module as a `curios_wasm::Module`. The private `print` module supplies the `Display` rendering the pipeline's stage dumps and the snapshot tests read.

mod names;
pub use names::*;

mod module;
pub use module::*;

mod to_wasm;
pub use to_wasm::*;

mod optm;
pub use optm::*;

mod print;
use print::*;
