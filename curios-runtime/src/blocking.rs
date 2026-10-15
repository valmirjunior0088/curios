//! Blocking work run off the thread that asks for it: [`blocking`], and the pool of threads it runs on.
//!
//! **A thread is started where none is idle, up to a bound, and ends once it has been idle a while.** A blocking call cannot be stopped, so one that never returns — a serial port's drain on a device that takes no output, a name lookup the system resolver never answers — holds its thread for good. A pool of a fixed few would let such calls hold all of it, and every other piece of blocking work would wait behind them; a pool that starts a thread where none is idle keeps them apart, and the bound keeps a flood of them from costing more threads than [`THREADS`].

use std::{
    collections::VecDeque,
    panic::{self, AssertUnwindSafe},
    pin::Pin,
    sync::{Arc, Condvar, LazyLock, Mutex},
    task::{Context, Poll, Waker},
    thread,
    time::Duration,
};

/// The most threads the pool runs at once: past it, work waits for a thread to come free.
const THREADS: usize = 64;

/// How long a thread waits for work before it ends.
const KEEP_ALIVE: Duration = Duration::from_secs(10);

/// Run `work` on the runtime's pool of threads, off the thread that asks, and answer what it returns once it has: what a host or an embedder answers a row later with where the answer is a blocking call. The future only learns that `work` is done, so any executor may await it, the run's own among them; dropping it leaves `work` to finish, its answer unread. A panic in `work` is raised again where the future is polled.
pub fn blocking<T: Send + 'static>(work: impl FnOnce() -> T + Send + 'static) -> Blocking<T> {
    let outcome = Arc::new(Mutex::new(Outcome {
        value: None,
        waker: None,
    }));
    let delivered = Arc::clone(&outcome);

    POOL.run(Box::new(move || {
        let value = panic::catch_unwind(AssertUnwindSafe(work));
        let waker = {
            let mut outcome = delivered.lock().unwrap();
            outcome.value = Some(value);
            outcome.waker.take()
        };

        if let Some(waker) = waker {
            waker.wake();
        }
    }));

    Blocking(outcome)
}

/// What [`blocking`]'s work returns, once it has.
pub struct Blocking<T>(Arc<Mutex<Outcome<T>>>);

/// What a piece of blocking work came to, if it has, and the waker of whoever waits for it.
struct Outcome<T> {
    value: Option<thread::Result<T>>,
    waker: Option<Waker>,
}

impl<T> Future for Blocking<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<T> {
        let mut outcome = self.0.lock().unwrap();

        match outcome.value.take() {
            Some(Ok(value)) => Poll::Ready(value),
            Some(Err(payload)) => panic::resume_unwind(payload),
            None => {
                outcome.waker = Some(context.waker().clone());

                Poll::Pending
            }
        }
    }
}

/// One piece of work a thread of the pool runs.
type Job = Box<dyn FnOnce() + Send>;

/// The pool's threads and the work waiting for one.
struct Pool {
    state: Mutex<State>,
    /// Signalled where work arrives for a thread that is idle.
    arrived: Condvar,
}

struct State {
    queue: VecDeque<Job>,
    /// The threads waiting for work.
    idle: usize,
    /// The threads running, idle ones among them.
    threads: usize,
}

static POOL: LazyLock<Pool> = LazyLock::new(|| Pool {
    state: Mutex::new(State {
        queue: VecDeque::new(),
        idle: 0,
        threads: 0,
    }),
    arrived: Condvar::new(),
});

impl Pool {
    /// Queue `job` for a thread: an idle one where the queue leaves one free, a new one where none is and the bound allows, and otherwise the next to come free. A system that refuses a new thread where the pool has none leaves the job to the thread that asked, which waits for it rather than leave it unrun.
    fn run(&'static self, job: Job) {
        let mut state = self.state.lock().unwrap();
        state.queue.push_back(job);

        if state.queue.len() <= state.idle {
            self.arrived.notify_one();
            return;
        }
        if state.threads >= THREADS {
            return;
        }

        state.threads += 1;
        drop(state);

        let started = thread::Builder::new()
            .name("curios-blocking".to_string())
            .spawn(move || self.serve());
        if started.is_err() {
            let mut state = self.state.lock().unwrap();
            state.threads -= 1;

            if state.threads == 0
                && let Some(job) = state.queue.pop_front()
            {
                drop(state);
                job();
            }
        }
    }

    /// A thread's life: run the work it is handed until it has waited [`KEEP_ALIVE`] for none.
    fn serve(&self) {
        loop {
            let job = {
                let mut state = self.state.lock().unwrap();

                loop {
                    if let Some(job) = state.queue.pop_front() {
                        break job;
                    }

                    state.idle += 1;
                    let (waited, timeout) = self.arrived.wait_timeout(state, KEEP_ALIVE).unwrap();
                    state = waited;
                    state.idle -= 1;

                    if timeout.timed_out() && state.queue.is_empty() {
                        state.threads -= 1;
                        return;
                    }
                }
            };

            job();
        }
    }
}
