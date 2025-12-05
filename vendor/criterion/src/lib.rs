use std::hint;
use std::time::{Duration, Instant};

#[derive(Clone, Debug)]
pub struct BenchmarkId {
    name: String,
    id: String,
}

impl BenchmarkId {
    pub fn new(name: impl Into<String>, id: impl Into<String>) -> Self {
        Self { name: name.into(), id: id.into() }
    }
}

#[derive(Default)]
pub struct Criterion {
    last_run: Option<Duration>,
}

pub struct Bencher {
    last_run: Option<Duration>,
}

impl Criterion {
    pub fn bench_with_input<I, F>(&mut self, id: BenchmarkId, input: I, mut f: F)
    where
        F: FnMut(&mut Bencher, &I),
    {
        let mut bencher = Bencher { last_run: None };
        f(&mut bencher, &input);
        self.last_run = bencher.last_run;
        log_run(&id.name, &id.id, self.last_run);
    }

    pub fn bench_function<F>(&mut self, name: &str, mut f: F)
    where
        F: FnMut(&mut Bencher),
    {
        let mut bencher = Bencher { last_run: None };
        f(&mut bencher);
        self.last_run = bencher.last_run;
        log_run(name, "", self.last_run);
    }
}

impl Bencher {
    pub fn iter<R, F: FnMut() -> R>(&mut self, mut f: F) {
        let start = Instant::now();
        f();
        self.last_run = Some(start.elapsed());
    }
}

pub fn black_box<T>(value: T) -> T {
    hint::black_box(value)
}

fn log_run(name: &str, id: &str, duration: Option<Duration>) {
    if let Some(elapsed) = duration {
        if id.is_empty() {
            println!("benchmark `{}` completed in {:?}", name, elapsed);
        } else {
            println!("benchmark `{}` ({}) completed in {:?}", name, id, elapsed);
        }
    }
}

#[macro_export]
macro_rules! criterion_group {
    ($name:ident, $($func:ident),+ $(,)?) => {
        pub fn $name() -> Vec<fn(&mut $crate::Criterion)> {
            vec![$($func),+]
        }
    };
}

#[macro_export]
macro_rules! criterion_main {
    ($($group:ident),+ $(,)?) => {
        pub fn main() {
            let mut crit = $crate::Criterion::default();
            $(for func in $group() { func(&mut crit); })+
        }
    };
}
