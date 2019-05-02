use std::io::BufRead;
use std::io::Result;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "aline",
    author = "Brandon Holt",
    version = "0.1.0",
    about = "Replacement for awk/cut/sed. Never again struggle to remember which one does what you want or splits fields how you want, just switch to aline for all your line-parsing needs"
)]
pub struct Config {
    /// Delimiter to use for separating
    #[structopt(short = "d", long = "delimiter", default_value = "\\s+")]
    pub delimiter: String,

    #[structopt(short = "f", long = "field")]
    pub field: Option<usize>,

    #[structopt(short = "v", long = "verbose")]
    pub verbose: bool,

    #[structopt(parse(from_os_str))]
    pub inputs: Vec<PathBuf>,
}

impl Config {
    pub fn handle_line<'a>(&self, l: &'a str) -> Option<&'a str> {
        if let Some(f) = self.field {
            let mut parts = l.split(&self.delimiter);
            return parts.nth(f);
        }
        return Some(l);
    }
}

pub fn process_reader<R: BufRead>(
    config: &Config,
    r: R,
) -> impl Iterator<Item = Option<String>> + '_ {
    return r.lines().map(move |rl| {
        let l = rl.unwrap();
        println!(">>> {}", &l);
        if let Some(f) = config.field {
            let mut parts = l.split(&config.delimiter);
            return parts.nth(f).map(String::from);
        }
        return Some(l);
    });
}
