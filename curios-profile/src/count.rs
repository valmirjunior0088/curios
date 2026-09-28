//! Allocation accounting for [`trace`](crate::trace()): a `GlobalAlloc` wrapper maintaining process-wide live, cumulative, and high-water byte counters that span timing samples at each boundary.
//!
//! This crate installs `CountingAllocator` as the `#[global_allocator]` of every binary it is linked into with `enabled` on, so a profile build counts wherever it measures and no binary has to remember to opt in.
//!
//! The counters are process-wide, so a span measures whatever the whole process did while it was entered — precise for the single-threaded stage pipelines the workspace profiles and an overcount anywhere else; `README.md` states why.

use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicUsize, Ordering},
};

static LIVE: AtomicUsize = AtomicUsize::new(0);
static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

/// A `GlobalAlloc` forwarding every request to the system allocator and counting the bytes that pass through it. Installed below, by this crate, wherever `enabled` is on.
struct CountingAllocator;

// Installed here rather than by each binary, because a binary that had to opt in could forget: the columns would then read zero, which is absent evidence rather than a failure, and nothing would say so. It is also what keeps the accounting tests falsifiable, since this crate's own test binary is one of the binaries it is linked into — inverting the sign of `retained` was observed to pass the suite under the system allocator and to fail `capture_accounts_retained_and_allocated_bytes` under this one.
#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

impl CountingAllocator {
    /// One trip through the allocator, whatever it was for. Counted separately from the bytes because the two choose different fixes: many small requests want fewer allocations, one large request wants a smaller structure, and a byte total alone cannot tell them apart.
    fn requested() {
        ALLOCATIONS.fetch_add(1, Ordering::Relaxed);
    }

    fn grew(size: usize) {
        ALLOCATED.fetch_add(size, Ordering::Relaxed);
        let live = LIVE.fetch_add(size, Ordering::Relaxed) + size;
        PEAK.fetch_max(live, Ordering::Relaxed);
    }

    fn shrank(size: usize) {
        LIVE.fetch_sub(size, Ordering::Relaxed);
    }
}

// SAFETY: every method forwards its request unchanged to `System` and returns exactly what it returned, so the allocator contract is `System`'s. The counters are plain atomics touched after the underlying call and never allocate, so no method re-enters this allocator.
unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            Self::requested();
            Self::grew(layout.size());
        }
        pointer
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() {
            Self::requested();
            Self::grew(layout.size());
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
        Self::shrank(layout.size());
    }

    // A reallocation counts its growth, not the whole new block: the bytes below the old size were already counted when the block was first taken, and counting them again would report a growing vector as having allocated its every intermediate capacity.
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let moved = unsafe { System.realloc(pointer, layout, size) };
        if !moved.is_null() {
            Self::requested();
            match size.checked_sub(layout.size()) {
                Some(growth) => Self::grew(growth),
                None => Self::shrank(layout.size() - size),
            }
        }
        moved
    }
}

/// Bytes taken and not yet returned.
pub fn live_bytes() -> usize {
    LIVE.load(Ordering::Relaxed)
}

/// Bytes taken since the process started, counting each reallocation's growth once and never decreasing.
pub fn allocated_bytes() -> usize {
    ALLOCATED.load(Ordering::Relaxed)
}

/// Trips through the allocator since the process started, never decreasing. Read against [`allocated_bytes`] for the average request size.
pub fn allocation_count() -> usize {
    ALLOCATIONS.load(Ordering::Relaxed)
}

/// The greatest [`live_bytes`] the process ever reached.
pub fn peak_bytes() -> usize {
    PEAK.load(Ordering::Relaxed)
}
