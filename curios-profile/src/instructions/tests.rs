use {super::Instructions, std::hint::black_box};

/// A loop of `rounds` additions, each at least one retired instruction however it is compiled: `black_box` keeps the sum from being folded away.
fn work(rounds: u64) -> u64 {
    let mut sum = 0u64;
    for round in 0..rounds {
        sum = black_box(sum.wrapping_add(round));
    }
    sum
}

/// A count grows by at least the instructions its own thread retires. Where the kernel opens no count, there is nothing to read and nothing to hold.
#[test]
fn a_count_grows_by_the_work_its_thread_does() {
    let Some(count) = Instructions::open() else {
        return;
    };

    let before = count.read();
    black_box(work(1_000_000));
    let after = count.read();

    assert!(
        after - before >= 1_000_000,
        "{} instructions over a million additions",
        after - before
    );
}

/// A thread started after the count opened is counted with the thread that opened it, as a pool a measured stage starts is.
#[test]
fn a_count_includes_a_thread_started_after_it_opened() {
    let Some(count) = Instructions::open() else {
        return;
    };

    let before = count.read();
    std::thread::spawn(|| black_box(work(10_000_000)))
        .join()
        .expect("the counted thread finishes");
    let after = count.read();

    assert!(
        after - before >= 10_000_000,
        "{} instructions over ten million additions on a started thread",
        after - before
    );
}

/// A count opened alone leaves out a thread started after it opened, as the work a pool's threads do while the opening thread runs alone.
#[test]
fn a_count_opened_alone_leaves_out_a_thread_it_starts() {
    let Some(count) = Instructions::open_alone() else {
        return;
    };

    let before = count.read();
    std::thread::spawn(|| black_box(work(10_000_000)))
        .join()
        .expect("the uncounted thread finishes");
    let after = count.read();

    assert!(
        after - before < 10_000_000,
        "{} instructions over ten million additions on a started thread",
        after - before
    );
}
