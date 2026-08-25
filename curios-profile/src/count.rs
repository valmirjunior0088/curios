//! Allocation accounting for [`capture`](crate::capture): a `GlobalAlloc` wrapper maintaining process-wide live, cumulative, and high-water byte counters that span timing samples at each boundary.
//!
//! A binary opts in by installing [`CountingAllocator`] as its `#[global_allocator]` under its own `profile` feature. Nothing else observes the counters, so a build that installs no allocator reports every memory column as zero while its timings stay exactly as they were — the columns are absent evidence, never a claim that nothing allocated.
//!
//! The counters are process-wide rather than per-thread because a `GlobalAlloc` cannot allocate the thread-local state per-thread attribution would need. Spans therefore measure whatever the whole process did while they were entered, which is precise for the single-threaded stage pipelines the workspace profiles and an overcount anywhere else.

use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicUsize, Ordering},
};

static LIVE: AtomicUsize = AtomicUsize::new(0);
static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

/// A `GlobalAlloc` forwarding every request to the system allocator and counting the bytes that pass through it.
///
/// ```text
/// #[cfg(feature = "profile")]
/// #[global_allocator]
/// static ALLOCATOR: curios_profile::CountingAllocator = curios_profile::CountingAllocator;
/// ```
pub struct CountingAllocator;

// This crate's own test binary installs it, because the accounting tests are otherwise unfalsifiable: nothing but this allocator writes the counters, so under the system allocator every memory column reads zero and an assertion over them holds whatever the accounting does. Inverting the sign of `retained` was observed to pass the suite without this line and to fail `capture_accounts_retained_and_allocated_bytes` with it.
#[cfg(test)]
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
