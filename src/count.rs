use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::io::Write;
use std::ops::AddAssign;

pub struct Counter<T> {
    counts: HashMap<T, i64>,
}

impl<T: Hash + Eq> Counter<T> {
    pub fn new() -> Counter<T> {
        Counter {
            counts: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: T) {
        self.counts.entry(key).or_default().add_assign(1);
    }

    /// Consume counter and return an owned iterator to all the items, sorted by count
    pub fn sorted_entries(self) -> impl Iterator<Item = (T, i64)> {
        let mut entries = self.counts.into_iter().collect::<Vec<_>>();
        entries.sort_by_key(|(_, v)| -*v);
        entries.into_iter()
    }
}

impl<T: Hash + Eq + Display> Counter<T> {
    /// Pretty print the counter as `uniq -c | sort -nr` would.
    pub fn print(&self, writer: impl Write) {
        let mut w = writer;
        let mut entries = self.counts.iter().collect::<Vec<_>>();
        entries.sort_by_key(|(_, v)| -**v);

        // figure out how many characters we need to pad with
        let width = format!("{}", entries[0].1).len() + 1;

        for (k, v) in entries {
            writeln!(w, "{:width$}  {}", v, k, width = width).unwrap();
        }
    }
}
