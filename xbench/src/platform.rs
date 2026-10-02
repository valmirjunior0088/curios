//! Where a reading was taken, in three lines the host answers for itself.
//!
//! The division is what the report does with each. [`Platform::machine`] and [`Platform::state`] gate a comparison, because a different machine or a different arrangement of the same one makes two readings incomparable and nothing else would notice. [`Platform::software`] is recorded and not gated: a kernel or microcode update rarely moves a figure past its span, and where it does the control moves with it and the comparison is disqualified on its own. Gating on it would strand the record at every patch.
//!
//! Each line is read from `/proc` and `/sys` rather than asked of a program, so a capture spawns nothing and cannot be told a different story by a tool on the path. Reading a file and reading what it says are separate here, so what the parsing decides is held by tests over text rather than over this machine.

#[cfg(test)]
mod tests;

use std::{env, fs, thread};

/// What stands where the host exposes nothing. A missing value is spelled rather than dropped, so two readings taken where different things are knowable still compare as the different arrangements they are.
const UNKNOWN: &str = "unknown";

/// Where a reading was taken, as the reading records it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Platform {
    /// The machine itself. Differs between two readings, and they are of different machines.
    pub machine: &'static str,
    /// How it was arranged when the reading was taken. Differs, and the same machine was running under different rules.
    pub state: &'static str,
    /// What was running on it. Recorded, never gated.
    pub software: &'static str,
}

/// The same three lines as the host answers them, which is what `collect` captures and writes into the module it prints.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Captured {
    pub machine: String,
    pub state: String,
    pub software: String,
}

/// The value `told` gives for `name`, where it is a file of `name: value` lines — `/proc/cpuinfo`, which states each field once per processor and identically.
fn field(told: &str, name: &str) -> String {
    told.lines()
        .find_map(|line| {
            let (said, value) = line.split_once(':')?;
            (said.trim() == name).then(|| value.trim().to_string())
        })
        .unwrap_or_else(|| UNKNOWN.to_string())
}

/// `MemTotal` as gibibytes, which is what the figure is for: an identity stable across every reading on one machine, not a measurement.
fn gibibytes(told: &str) -> String {
    told.lines()
        .find_map(|line| {
            let rest = line.strip_prefix("MemTotal:")?;
            let kibibytes: f64 = rest.trim().strip_suffix(" kB")?.parse().ok()?;
            Some(format!("{:.1} GiB", kibibytes / (1024.0 * 1024.0)))
        })
        .unwrap_or_else(|| UNKNOWN.to_string())
}

/// Whether the processor may clock above its base, from whichever driver the host has.
///
/// The two spell it oppositely — `cpufreq/boost` is on when it is `1`, `intel_pstate/no_turbo` when it is `0` — so a reading that recorded the raw file would compare two machines by a number that means the reverse on each.
fn boosting(cpufreq: Option<&str>, pstate: Option<&str>) -> String {
    let said = match (cpufreq, pstate) {
        (Some(boost), _) => match boost.trim() {
            "1" => "on",
            "0" => "off",
            other => other,
        },
        (None, Some(no_turbo)) => match no_turbo.trim() {
            "0" => "on",
            "1" => "off",
            other => other,
        },
        (None, None) => UNKNOWN,
    };

    format!("boost {said}")
}

/// What a file says, trimmed, or [`UNKNOWN`].
fn told(path: &str) -> String {
    fs::read_to_string(path).map_or_else(|_| UNKNOWN.to_string(), |told| told.trim().to_string())
}

/// What the host answers about itself, as a reading records it.
pub fn capture() -> Captured {
    let cpuinfo = fs::read_to_string("/proc/cpuinfo").unwrap_or_default();
    let meminfo = fs::read_to_string("/proc/meminfo").unwrap_or_default();
    let cpus = thread::available_parallelism()
        .map_or_else(|_| UNKNOWN.to_string(), |cpus| cpus.to_string());

    Captured {
        machine: format!(
            "{}, {}, {cpus} cpus, {}",
            field(&cpuinfo, "model name"),
            env::consts::ARCH,
            gibibytes(&meminfo)
        ),
        state: format!(
            "governor {}, {}, smt {}",
            told("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor"),
            boosting(
                fs::read_to_string("/sys/devices/system/cpu/cpufreq/boost")
                    .ok()
                    .as_deref(),
                fs::read_to_string("/sys/devices/system/cpu/intel_pstate/no_turbo")
                    .ok()
                    .as_deref(),
            ),
            told("/sys/devices/system/cpu/smt/control")
        ),
        software: format!(
            "kernel {}, microcode {}",
            told("/proc/sys/kernel/osrelease"),
            field(&cpuinfo, "microcode")
        ),
    }
}
