//! Allocation accounting for [`trace`](crate::trace()): a `GlobalAlloc` wrapper maintaining process-wide live, cumulative, and high-water byte counters, which the recorder reads at each span boundary.
//!
//! This crate installs `CountingAllocator` as the `#[global_allocator]` of every binary it is linked into, so every build carries it and no binary has to remember to opt in — one that forgot would read zero in every memory column, which is absent evidence rather than a failure.
//!
//! **It counts only while someone reads the counts**: a recorder, [`install`](crate::install)'s for the rest of the process and [`trace`](crate::trace())'s for its closure, or a measurement that asked with [`count_allocations`]. Otherwise a trip through it costs one relaxed load: counting is four atomic read-modify-writes, each a locked instruction on x86, which every compile would pay for a count nobody reads. So the live bytes are counted from where counting began, and a block taken before it and returned since lowers them, below zero where more was returned than taken.
//!
//! The counters are process-wide, so a span measures whatever the whole process did while it was entered — precise for the single-threaded stage pipelines the workspace profiles and an overcount anywhere else; `README.md` states why.

#[cfg(test)]
mod tests;

use std::{
    alloc::{GlobalAlloc, Layout, System},
    sync::atomic::{AtomicIsize, AtomicUsize, Ordering},
};

/// How many recorders and measurements the allocator counts for: it counts while this is above zero.
static LISTENERS: AtomicUsize = AtomicUsize::new(0);
static LIVE: AtomicIsize = AtomicIsize::new(0);
static ALLOCATED: AtomicUsize = AtomicUsize::new(0);
static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicIsize = AtomicIsize::new(0);

/// A `GlobalAlloc` forwarding every request to the system allocator and counting the bytes that pass through it while anyone reads them. Installed below, by this crate, in every binary it is linked into.
struct CountingAllocator;

#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

impl CountingAllocator {
    /// Whether anyone reads the counts: the one load every trip through the allocator pays.
    fn counting() -> bool {
        LISTENERS.load(Ordering::Relaxed) > 0
    }

    /// One trip through the allocator, whatever it was for. Counted separately from the bytes because the two choose different fixes: many small requests want fewer allocations, one large request wants a smaller structure, and a byte total alone cannot tell them apart.
    fn requested() {
        ALLOCATIONS.fetch_add(1, Ordering::Relaxed);
    }

    // A layout's size is at most `isize::MAX`, so each conversion below is exact.
    fn grew(size: usize) {
        ALLOCATED.fetch_add(size, Ordering::Relaxed);
        let live = LIVE.fetch_add(size as isize, Ordering::Relaxed) + size as isize;
        PEAK.fetch_max(live, Ordering::Relaxed);
    }

    fn shrank(size: usize) {
        LIVE.fetch_sub(size as isize, Ordering::Relaxed);
    }
}

// SAFETY: every method forwards its request unchanged to `System` and returns exactly what it returned, so the allocator contract is `System`'s. The counters are plain atomics touched after the underlying call and never allocate, so no method re-enters this allocator.
unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() && Self::counting() {
            Self::requested();
            Self::grew(layout.size());
        }
        pointer
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() && Self::counting() {
            Self::requested();
            Self::grew(layout.size());
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
        if Self::counting() {
            Self::shrank(layout.size());
        }
    }

    // A reallocation counts its growth, not the whole new block: the bytes below the old size were already counted when the block was first taken, and counting them again would report a growing vector as having allocated its every intermediate capacity.
    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let moved = unsafe { System.realloc(pointer, layout, size) };
        if !moved.is_null() && Self::counting() {
            Self::requested();
            match size.checked_sub(layout.size()) {
                Some(growth) => Self::grew(growth),
                None => Self::shrank(layout.size() - size),
            }
        }
        moved
    }
}

/// One more reader of the counts: the allocator counts until each has ended.
pub fn begin_counting() {
    LISTENERS.fetch_add(1, Ordering::Relaxed);
}

/// One reader of the counts fewer.
pub fn end_counting() {
    LISTENERS.fetch_sub(1, Ordering::Relaxed);
}

/// Count every allocation from here to the end of the process, for a measurement that reads [`allocation_count`] and [`allocated_bytes`] with no recorder listening. Nothing ends it, so a process that asks pays the count for the rest of its life, which a measurement, run alone in its process, does.
pub fn count_allocations() {
    begin_counting();
}

/// Bytes taken and not yet returned since counting began, below zero where more was returned than taken since.
pub fn live_bytes() -> isize {
    LIVE.load(Ordering::Relaxed)
}

/// Bytes taken while counting, counting each reallocation's growth once and never decreasing.
pub fn allocated_bytes() -> usize {
    ALLOCATED.load(Ordering::Relaxed)
}

/// Trips through the allocator while counting, never decreasing. Read against [`allocated_bytes`] for the average request size.
pub fn allocation_count() -> usize {
    ALLOCATIONS.load(Ordering::Relaxed)
}

/// The greatest [`live_bytes`] counting ever reached, zero where it never rose above where it began.
pub fn peak_bytes() -> usize {
    // Never below zero, where it starts and which it only rises from.
    PEAK.load(Ordering::Relaxed) as usize
}
