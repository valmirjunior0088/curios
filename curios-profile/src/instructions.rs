//! Instruction accounting: the user-space instructions a thread retires, alone or with every thread it starts once its count is open.
//!
//! A node look counts what a term walk reads and the allocator what a stage takes, and neither sees a walk that reads without allocating or code that is not Rust: a frame stack searched at every step, Binaryen's passes. A retired instruction is what all of them cost, counted by the processor rather than timed, so a reading on a loaded machine is the reading on a quiet one. `README.md` states why a measurement reads it and no span records it.

#[cfg(test)]
mod tests;

use std::{fs::File, io::Read};

/// An open count of the user-space instructions retired by the thread that opened it, and, where it was opened to, by every thread that thread starts afterwards.
///
/// A thread that already stood when the count opened is not counted, so a measurement opens its count before anything it measures starts a pool: one measurement a process, as `curios`'s instrumented tests are run. A pool's thread also retires what it does while it waits for work, inside whatever the opening thread does meanwhile, so work the opening thread does alone is read on a count of it alone.
pub struct Instructions {
    counter: File,
}

impl Instructions {
    /// A count opened on this thread, with every thread it starts, or `None` where the kernel opens none — a `perf_event_paranoid` above 2, a container that hides the processor's counters, a system other than Linux. A measurement reads `None` as absent evidence, as a binary without this crate's allocator reads zero bytes, and never as zero instructions.
    pub fn open() -> Option<Self> {
        counter(true).map(|counter| Self { counter })
    }

    /// A count opened on this thread alone, or `None` as for [`Instructions::open`]: what the thread retires, and nothing a thread it starts does.
    pub fn open_alone() -> Option<Self> {
        counter(false).map(|counter| Self { counter })
    }

    /// The instructions retired since the count opened, by this thread and, where it counts them, the threads it has started, those still running included.
    pub fn read(&self) -> u64 {
        let mut bytes = [0; 8];
        (&self.counter)
            .read_exact(&mut bytes)
            .expect("an open instruction count reads as one 64-bit value");

        u64::from_ne_bytes(bytes)
    }
}

/// The attributes of a count, as `perf_event_open(2)` reads them: the first version of the structure, 64 bytes, which every kernel since takes.
#[cfg(target_os = "linux")]
#[repr(C)]
struct Attributes {
    kind: u32,
    size: u32,
    config: u64,
    sample_period: u64,
    sample_type: u64,
    read_format: u64,
    flags: u64,
    wakeup_events: u32,
    breakpoint_type: u32,
    config1: u64,
}

/// A count of this thread's retired instructions, with every thread it starts where `inherited`.
#[cfg(target_os = "linux")]
fn counter(inherited: bool) -> Option<File> {
    use std::os::fd::{FromRawFd, OwnedFd, RawFd};

    const HARDWARE: u32 = 0;
    const INSTRUCTIONS: u64 = 1;
    const INHERIT: u64 = 1 << 1;
    const EXCLUDE_KERNEL: u64 = 1 << 5;
    const EXCLUDE_HYPERVISOR: u64 = 1 << 6;
    const CLOSE_ON_EXEC: libc::c_ulong = 1 << 3;

    let inherit = match inherited {
        true => INHERIT,
        false => 0,
    };
    let attributes = Attributes {
        kind: HARDWARE,
        size: size_of::<Attributes>() as u32,
        config: INSTRUCTIONS,
        sample_period: 0,
        sample_type: 0,
        read_format: 0,
        flags: inherit | EXCLUDE_KERNEL | EXCLUDE_HYPERVISOR,
        wakeup_events: 0,
        breakpoint_type: 0,
        config1: 0,
    };

    // SAFETY: `perf_event_open` reads `size` bytes of attributes through the pointer, which is `Attributes` whole and lives across the call; the other arguments are plain integers naming this thread (0), any processor (-1) and no group (-1). It writes nothing of ours.
    let descriptor = unsafe {
        libc::syscall(
            libc::SYS_perf_event_open,
            &raw const attributes,
            0 as libc::pid_t,
            -1 as libc::c_int,
            -1 as libc::c_int,
            CLOSE_ON_EXEC,
        )
    };
    let descriptor = RawFd::try_from(descriptor).ok().filter(|fd| *fd >= 0)?;

    // SAFETY: the kernel has just returned this descriptor open, and nothing else holds it: ownership passes to the `OwnedFd`, which closes it once.
    let owned = unsafe { OwnedFd::from_raw_fd(descriptor) };

    Some(File::from(owned))
}

#[cfg(not(target_os = "linux"))]
fn counter(_: bool) -> Option<File> {
    None
}
