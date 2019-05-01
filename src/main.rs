use std::io::{stdin, BufRead};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "aline",
    author = "Brandon Holt",
    version = "0.1.0",
    about = "Replacement for awk/cut/sed. Never again struggle to remember which one does what you want or splits fields how you want, just switch to aline for all your line-parsing needs"
)]
struct Config {
    /// Delimiter to use for separating
    #[structopt(short = "d", long = "delimiter", default_value = "\\s+")]
    delimiter: String,

    #[structopt(short = "f", long = "field")]
    field: Option<i32>,

    #[structopt(short = "v", long = "verbose")]
    verbose: bool,
}

impl Config {
    fn handle_line(&self, l: String) -> String {
        if let Some(f) = self.field {
            let delim = self.delimiter;
            return l.split(delim).nth(f);
        }
        return "".to_string();
    }
}

fn main() {
    let config = Config::from_args();
    println!("{:#?}", config);

    for line in stdin().lock().lines() {
        println!(">>> {}", line.unwrap());
    }
}
