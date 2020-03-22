use std::collections::HashMap;
use std::io::Write;
use std::ops::AddAssign;

pub struct Counter {
    counts: HashMap<String, i64>,
}

impl Counter {
    pub fn new() -> Counter {
        Counter {
            counts: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String) {
        self.counts.entry(key).or_default().add_assign(1);
    }

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
