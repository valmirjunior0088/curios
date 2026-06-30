//! Wall-clock scaling of *type checking* over metavariable-heavy programs —
//! the spine-pruning gate (memory: contextual metavariables carry the full
//! local Γ per occurrence; prune only if this benchmark says elaboration
//! hurts). Two shapes: a deep chain of implicit-heavy `Eq` proofs (every step
//! mints inserted metavariables under a growing context), and a wide module
//! of independent indexed-vector definitions (many telescopes, many drains).
//! Per-definition cost is the slope of time vs. size, cancelling fixed
//! startup overhead.

use {
    curios::{core, text},
    std::time::{Duration, Instant},
};

/// `n` chained `Eq` transitivity steps: each line births implicit metas for
/// `trans`/`refl` under a context that grows by one binding per line.
fn deep_eq_chain(n: usize) -> String {
    let mut s = String::from("use /std/{Nat, Eq, Io};\n");
    s.push_str("let e0 : Eq(0, 0) = Eq/refl();\n");
    for i in 1..=n {
        s.push_str(&format!(
            "let e{i} : Eq(0, 0) = Eq/trans(e{}, e0);\n",
            i - 1
        ));
    }
    s.push_str("Io/print(\"ok\")\n");
    s
}

/// `n` independent length-indexed vector definitions: each elaborates its own
/// telescope of implicits and drains its own parked constraints.
fn wide_vectors(n: usize) -> String {
    let mut s = String::from("use /std/{Nat, Vec, Io};\n");
    for i in 0..n {
        s.push_str(&format!(
            "let v{i} : Vec(Nat, 3) = Vec/cons({i}, Vec/cons({i}, Vec/cons({i}, Vec/nil())));\n"
        ));
    }
    s.push_str("Io/print(\"ok\")\n");
    s
}

fn time_check(label: &str, source: &str) {
    let entrypoint = source
        .parse::<text::Entrypoint>()
        .expect("benchmark source parses");

    let start = Instant::now();
    let (module, metavars) = text::to_core(&entrypoint, &text::prelude(&text::NullLoader))
        .expect("benchmark source lowers");
    let lowered = start.elapsed();

    let start = Instant::now();
    core::elaborate_module(
        &mut core::Context::new(Duration::from_secs(60)),
        &module,
        metavars,
        core::Mode::Infer,
    )
    .expect("benchmark source elaborates");
    let elaborated = start.elapsed();

    println!("{label}: to_core {lowered:?}, elaborate {elaborated:?}");
}

fn main() {
    for n in [16, 64, 256] {
        time_check(&format!("deep eq chain n={n}"), &deep_eq_chain(n));
    }
    for n in [16, 64, 256] {
        time_check(&format!("wide vectors n={n}"), &wide_vectors(n));
    }
}
